//! Decides how the environments of the program's functions are laid out, for the whole program
//! at once rather than one function at a time.
//!
//! A function's captures, parameters and each run of let statements are laid out in a tree, with
//! the most referenced symbols nearest the root (see [`Environment::tree`]). Reading a symbol
//! costs a path whose size depends on its depth, which the balancing by reference count mostly
//! settles. But each call also builds the callee's tree from the caller's, and a subtree that's
//! laid out the same way in both is passed on as one path, where the rest is rebuilt with a `c`
//! for each pair. That depends on how the trees of the two functions line up, which isn't
//! something either one can decide alone.
//!
//! So the program is lowered with each of a few layouts (see [`Layout::CANDIDATES`]), each
//! with its own decisions about what to inline (see [`super::inline`]), and the smallest program
//! is kept, so it's no larger than with any one of them. They're tried in this order, and the
//! earlier one is kept when two are the same size:
//!
//! - **Reached:** a function's captures that are referenced equally often are in the order the
//!   program reaches them in from its entrypoint, so that functions that capture the same symbols
//!   have the same tree (#593).
//! - **Written:** they stay in the order each function found them in.
//! - **Nested:** as reached, but the captures of a function that are inside another tree, such
//!   as the captures of a function that calls it, are laid out there as its own tree, so that
//!   the call passes them on as one path (see [`Nesting`]).
//! - **Everything reached:** as reached, but a function's parameters and each run of let
//!   statements are in that order too.
//!
//! Written and everything reached are only tried if lowering with reached put something in
//! another order than they would (see [`Reordered`]). Otherwise they'd lower the program the
//! same way, since each trial is decided from the same state.
//!
//! Whatever the layout, a function's body and every call to it get their trees from
//! [`Lowerer::function_groups`], which only depends on the layout and the symbols being laid
//! out, so the two always agree.

use std::collections::{HashMap, HashSet};

use indexmap::IndexSet;
use rue_lir::LirId;

use crate::{Environment, Symbol, SymbolId};

use super::{Lowerer, measure::Trial};

/// How the environments of the program's functions are laid out (see the [module](self)).
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(super) enum Layout {
    Written,
    Reached,
    Nested,
    EverythingReached,
}

impl Layout {
    /// The layouts that a program that's run on chain is lowered with, in the order they're
    /// preferred in when they're the same size. The first is the one anything else is lowered
    /// with, such as a const expression.
    pub const CANDIDATES: [Self; 4] = [
        Self::Reached,
        Self::Written,
        Self::Nested,
        Self::EverythingReached,
    ];

    /// Whether a function's captures are in the order the program reaches them in.
    fn orders_captures(self) -> bool {
        self != Self::Written
    }
}

/// What lowering with [`Layout::Reached`] put in another order than the other layouts would, so
/// that they're only tried if they could lower the program differently.
#[derive(Debug, Default, Clone, Copy)]
pub(super) struct Reordered {
    /// A function's captures, which [`Layout::Written`] leaves in the order it found them in.
    captures: bool,
    /// A function's parameters or a run of let statements, which [`Layout::EverythingReached`]
    /// puts in the order the program reaches them in.
    others: bool,
}

/// The captures of the program's functions that are laid out as subtrees of the trees they're
/// in, for [`Layout::Nested`].
///
/// Each function's set of captures is a block, unless it overlaps one that's already a block
/// without either containing the other, so that any two blocks are nested or disjoint. They're
/// taken from the smallest, so that the most of them fit. A tree of symbols is then built from
/// the largest blocks inside of it, each laid out as its own tree, and the symbols that aren't in
/// one. The tree of a block is the tree its function has, so a call to that function from one
/// whose tree contains the block passes the block on as it is.
#[derive(Debug, Default)]
pub(super) struct Nesting {
    blocks: Vec<Block>,
    /// The index of the block with each set of symbols, keyed by its symbols in order of id.
    by_symbols: HashMap<Vec<SymbolId>, usize>,
    /// The smallest block that each symbol is in.
    innermost: HashMap<SymbolId, usize>,
}

#[derive(Debug)]
struct Block {
    len: usize,
    /// The smallest block that contains this one.
    parent: Option<usize>,
    tree: Environment,
    /// How many references the block stands for, in the tree it's part of.
    references: usize,
}

impl Lowerer<'_, '_, '_> {
    /// Lowers the entrypoint, after deciding how the environments are laid out and which of the
    /// functions that are measured are inlined (see [`Self::is_measured_function`]). This is how
    /// a program that's run on chain is lowered. A program that's only run while compiling, such
    /// as a const expression, is lowered with [`lower_symbol_value`](Self::lower_symbol_value),
    /// which doesn't measure anything, since its size doesn't matter.
    pub fn lower_main(&mut self, env: &Environment) -> LirId {
        let mut best: Option<(Trial, Layout, HashSet<SymbolId>)> = None;

        for layout in Layout::CANDIDATES {
            if !self.is_layout_needed(layout) {
                continue;
            }

            self.layout = layout;
            self.inlined_functions.clear();

            let trial = self.decide_inlining(env);

            if best
                .as_ref()
                .is_none_or(|(best, ..)| trial.size < best.size)
            {
                best = Some((trial, layout, self.inlined_functions.clone()));
            }
        }

        let (trial, layout, inlined_functions) = best.expect("the first layout is always lowered");

        // Anything that's lowered after this is laid out the same way, and inlines the same
        // functions
        self.layout = layout;
        self.inlined_functions = inlined_functions;
        self.prepare_trial();

        self.keep(trial)
    }

    /// Decides again what depends on which functions are inlined, before the program is lowered
    /// that way: which values are constants, since a value can be one with a function inlined,
    /// and what the layout nests, since that depends on what the functions capture.
    pub(super) fn prepare_trial(&mut self) {
        // Nothing is nested while the constants are found
        self.nesting = None;
        self.decide_repeats();
        self.prepare_layout();
    }

    /// Whether the program is lowered with the layout, which it isn't if it would be lowered the
    /// same way as with one that's already been tried. The first layout is always tried, and
    /// [`Layout::Written`] and [`Layout::EverythingReached`] only differ from it if lowering with
    /// it put something in another order than they would.
    pub(super) fn is_layout_needed(&self, layout: Layout) -> bool {
        let reordered = self.reordered.get();

        match layout {
            Layout::Reached | Layout::Nested => true,
            Layout::Written => reordered.captures,
            Layout::EverythingReached => reordered.others,
        }
    }

    /// Finds what the layout nests, before the program is lowered with it, outside of any
    /// inlined call. Until then, as when a value is lowered to find out whether it's a constant,
    /// nothing is nested.
    fn prepare_layout(&mut self) {
        self.nesting = (self.layout == Layout::Nested).then(|| self.find_nesting());
    }

    /// What the function captures where it's lowered here (see [`Self::lowered_captures`]), in
    /// the order the layout puts them in, before they're grouped.
    pub(super) fn ordered_captures(&self, symbol: SymbolId) -> IndexSet<SymbolId> {
        let mut captures = self.lowered_captures(symbol);
        // The captures of a function that are referenced equally often are laid out in the order
        // the program reaches them in from its entrypoint, whatever order the function found them
        // in, so that functions that capture the same symbols, such as a recursive function and a
        // loop that call each other, have the same tree, which each passes on to the other as it
        // is. That order follows where a symbol is used, not where it's defined, so the default
        // value of a struct's field is still laid out as the same value written where it's used.
        // A closure's captures are a list in the order it's built with
        if self.graph.is_closure(symbol) {
            return captures;
        }

        if self.layout.orders_captures()
            && self.order_by_reach(&mut captures)
            && self.layout == Layout::Reached
        {
            self.note_reordered(|reordered| reordered.captures = true);
        }

        captures
    }

    fn note_reordered(&self, f: impl FnOnce(&mut Reordered)) {
        let mut reordered = self.reordered.get();
        f(&mut reordered);
        self.reordered.set(reordered);
    }

    /// Orders the symbols by when the program reaches them from its entrypoint, then by id, and
    /// returns whether that changed their order.
    fn order_by_reach(&self, symbols: &mut IndexSet<SymbolId>) -> bool {
        let order = self.dependencies(self.main, false);
        let key = |symbol: &SymbolId| (order.get_index_of(symbol).unwrap_or(usize::MAX), *symbol);

        if symbols.iter().is_sorted_by_key(key) {
            return false;
        }

        symbols.sort_by_key(key);
        true
    }

    /// The tree of the symbols for the layout, by how often each one is referenced. The symbols
    /// that are referenced equally often keep the order they're given in, unless every tree is in
    /// the order the program reaches its symbols in.
    pub(super) fn layout_tree(&self, symbols: Vec<SymbolId>) -> Environment {
        let mut symbols: IndexSet<SymbolId> = symbols.into_iter().collect();

        if self.layout == Layout::Reached {
            // Only to find out whether everything reached would lay them out differently
            if self.order_by_reach(&mut symbols.clone()) {
                self.note_reordered(|reordered| reordered.others = true);
            }
        } else if self.layout == Layout::EverythingReached {
            self.order_by_reach(&mut symbols);
        }

        let Some(nesting) = &self.nesting else {
            return Environment::tree(
                symbols
                    .into_iter()
                    .map(|symbol| (symbol, self.graph.layout_references(symbol)))
                    .collect(),
            );
        };

        let mut key: Vec<SymbolId> = symbols.iter().copied().collect();
        key.sort();

        if let Some(&block) = nesting.by_symbols.get(&key) {
            return nesting.blocks[block].tree.clone();
        }

        // The largest block that each symbol is in that's inside of these symbols. If a block is
        // inside of them, so is every block inside of it
        let mut inside = HashMap::new();
        let mut largest = HashMap::new();

        for &symbol in &symbols {
            let mut block = nesting.innermost.get(&symbol).copied();

            while let Some(index) = block
                && nesting.blocks[index].len < symbols.len()
            {
                *inside.entry(index).or_insert(0) += 1;
                block = nesting.blocks[index].parent;
            }
        }

        for &symbol in &symbols {
            let mut block = nesting.innermost.get(&symbol).copied();

            while let Some(index) = block
                && inside.get(&index) == Some(&nesting.blocks[index].len)
            {
                largest.insert(symbol, index);
                block = nesting.blocks[index].parent;
            }
        }

        self.nested_tree(nesting, &symbols, &largest)
    }

    /// The tree of the symbols, with each one that's in a block given by `largest` laid out as
    /// part of that block's tree.
    fn nested_tree(
        &self,
        nesting: &Nesting,
        symbols: &IndexSet<SymbolId>,
        largest: &HashMap<SymbolId, usize>,
    ) -> Environment {
        let mut placed = IndexSet::new();
        let mut parts = Vec::new();

        for &symbol in symbols {
            match largest.get(&symbol) {
                Some(&index) => {
                    if placed.insert(index) {
                        let block = &nesting.blocks[index];
                        parts.push((block.tree.clone(), block.references));
                    }
                }
                None => parts.push((
                    Environment::Leaf(symbol),
                    self.graph.layout_references(symbol),
                )),
            }
        }

        Environment::tree_of(parts)
    }

    fn find_nesting(&self) -> Nesting {
        // The captures of each function that has a tree of them, as it's lowered outside of any
        // inlined call. Elsewhere, it can capture fewer, and that tree isn't nested
        let mut sets = Vec::new();

        for &symbol in self.dependencies(self.main, false).iter() {
            if symbol == self.main
                || self.graph.is_closure(symbol)
                || !matches!(self.db.symbol(symbol), Symbol::Function(_))
            {
                continue;
            }

            let mut captures = self.ordered_captures(symbol);

            let for_loop = self.graph.is_loop(symbol);
            captures.retain(|&capture| self.is_grouped_capture(capture, for_loop));

            if captures.len() < 2 {
                continue;
            }

            let mut key: Vec<SymbolId> = captures.iter().copied().collect();
            key.sort();
            sets.push((key, captures));
        }

        // The sort is stable, so sets of the same size stay in the order the program reaches
        // their functions in
        sets.sort_by_key(|(key, _)| key.len());

        let mut nesting = Nesting::default();

        // The largest block that each symbol is in so far, which is the last one, since they're
        // taken from the smallest
        let mut outermost: HashMap<SymbolId, usize> = HashMap::new();

        for (key, symbols) in sets {
            // Functions that capture the same symbols have the same block
            if nesting.by_symbols.contains_key(&key) {
                continue;
            }

            // The blocks it overlaps must all be inside of it. They're disjoint, so that holds if
            // each one has as many of its symbols as it has
            let mut overlaps: HashMap<usize, usize> = HashMap::new();

            for symbol in &symbols {
                if let Some(&block) = outermost.get(symbol) {
                    *overlaps.entry(block).or_insert(0) += 1;
                }
            }

            if overlaps
                .iter()
                .any(|(&block, &count)| nesting.blocks[block].len != count)
            {
                continue;
            }

            let index = nesting.blocks.len();
            let largest: HashMap<SymbolId, usize> = symbols
                .iter()
                .filter_map(|symbol| Some((*symbol, *outermost.get(symbol)?)))
                .collect();
            let tree = self.nested_tree(&nesting, &symbols, &largest);

            for &block in overlaps.keys() {
                nesting.blocks[block].parent = Some(index);
            }

            for &symbol in &symbols {
                outermost.insert(symbol, index);
                nesting.innermost.entry(symbol).or_insert(index);
            }

            nesting.by_symbols.insert(key, index);

            nesting.blocks.push(Block {
                len: symbols.len(),
                parent: None,
                tree,
                references: symbols
                    .iter()
                    .map(|&symbol| self.graph.layout_references(symbol))
                    .sum(),
            });
        }

        nesting
    }
}
