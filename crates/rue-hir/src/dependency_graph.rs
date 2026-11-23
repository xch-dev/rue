use std::{
    collections::{HashMap, HashSet},
    iter,
};

use indexmap::{IndexSet, set::Slice};
use rue_options::CompilerOptions;

use crate::{
    BindingSymbol, ConstantSymbol, Database, FailureAnalysis, FunctionKind, Hir, HirId, Statement,
    Symbol, SymbolId,
    analysis::{Callee, Effect, resolve_callee, resolve_function},
    lower::MAX_REPEATED_STATIC_SIZE,
    repeat_cost::is_cheap_to_repeat,
    small_static::{StaticContext, is_small_static, take},
};

/// The most reads that are counted for a symbol. Each inline binding that reads another one more
/// than once multiplies the reads of what's below it, so a chain of them would overflow otherwise.
/// Nothing tells counts this high apart, and they're added up when captures are put in a tree.
const MAX_READS: usize = 1 << 16;

#[derive(Debug, Clone)]
pub struct DependencyGraph {
    options: CompilerOptions,

    // Keeps track of the direct dependencies of a symbol
    dependencies: HashMap<SymbolId, IndexSet<SymbolId>>,

    // Keeps track of the symbol that each local is defined in
    owners: HashMap<SymbolId, SymbolId>,

    // Keeps track of the number of times a symbol is read. While the graph is being built, this
    // is how many references to it have been visited, which is only compared with zero. Once
    // it's built, each of those is counted as many times as it's lowered (see `count_reads`)
    references: HashMap<SymbolId, usize>,

    // Keeps track of the number of references to each symbol in each place that's lowered a
    // different number of times, which is `None` for a place that's lowered once
    reads: HashMap<(Option<RepeatId>, SymbolId), usize>,

    // The places that are lowered once for each read of something else, and the place that each
    // one is in, if that's repeated too
    repeats: IndexSet<(Repeat, Option<RepeatId>)>,

    // Keeps track of the place being visited, if it's repeated
    repeat: Option<RepeatId>,

    // Keeps track of the place that each symbol is defined in, if it's repeated: the one that
    // the let statement of a binding or the expression of a lambda is in, and the body of a
    // lambda for its parameters. There's a copy of the symbol in each copy of that place
    definitions: HashMap<SymbolId, RepeatId>,

    // Keeps track of where each symbol was last referenced, which is its only reference if it's
    // only read once. For a lambda, that's the lambda expression
    sites: HashMap<SymbolId, Site>,

    // Keeps track of how the value of an expression is used, if it's the whole value of a binding
    // or constant, or an argument of a call to a function that's known where it's called
    usages: HashMap<HirId, Usage>,

    // Keeps track of the bindings and constants whose only reference can be evaluated more than
    // once for each time their value would be (see `Self::is_repeated`)
    repeated: HashSet<SymbolId>,

    // Keeps track of the `inline` bindings and constants, whose values are lowered where they're
    // referenced
    inline_values: HashSet<SymbolId>,

    // Keeps track of the reads that a symbol would have on top of those in each place, if each
    // struct that reuses the rest of it from its first or second field on copied the fields one
    // at a time instead. These only count for where the symbol is in the environment
    tail_reads: HashMap<(Option<RepeatId>, SymbolId), usize>,

    // The number of those reads of each symbol, once the graph is built, counted as many times
    // as they're lowered, like `references`
    tail_references: HashMap<SymbolId, usize>,

    // Keeps track of whether a symbol is ever used as a closure
    closures: HashSet<SymbolId>,
    /// The functions that are used as values and lowered to the program they're compiled to on
    /// their own (see [`crate::program_value`]). They're constants, so they aren't dependencies.
    program_values: IndexSet<SymbolId>,
    /// The functions that `for` expressions are compiled to (see `FunctionKind::Loop`).
    loops: HashSet<SymbolId>,
    /// The symbols that are referenced inside of a loop they're defined outside of, which
    /// evaluates what references them once for each item.
    loop_captures: HashSet<SymbolId>,
    /// What each loop captures (see [`Self::loop_captures`]).
    loop_dependencies: HashMap<SymbolId, IndexSet<SymbolId>>,

    /// The loop that each loop capture is only referenced inside of, if there's one (see
    /// `DependencyGraph::sole_loop`).
    sole_loops: HashMap<SymbolId, SymbolId>,

    // Keeps track of the const expressions that are reachable
    const_exprs: IndexSet<HirId>,

    // Keeps track of the symbol whose body is being visited
    current: Option<SymbolId>,

    // Keeps track of the symbols that have been referenced but not visited yet. They're visited
    // from here rather than recursively, so that a long chain of symbols that each reference the
    // next doesn't overflow the stack
    pending: Vec<SymbolId>,

    // Keeps track of the symbols referenced by the body being visited, in order
    referenced: Vec<SymbolId>,

    // Keeps track of the symbols that have been visited to avoid infinite loops
    visited: HashSet<SymbolId>,

    // Keeps track of the block and index of the let statement of each binding that has one
    lets: HashMap<SymbolId, (HirId, usize)>,

    // Keeps track of the bindings that are evaluated for their effect at their let statements,
    // since their values may fail, if they're never referenced
    effects: HashSet<SymbolId>,

    // Keeps track of the non-inline bindings that haven't been decided yet, along with the symbol
    // whose body their let statement is in. Whether each one is evaluated for its effect is
    // decided in `visit_effects`, once the rest of the program has been visited
    unvisited_effects: Vec<(Option<SymbolId>, SymbolId)>,

    // Keeps track of the type guards whose checks don't use their values, but whose values are
    // evaluated for their effect, since they may fail
    guard_effects: HashSet<HirId>,

    // Keeps track of the type guards whose checks don't use their values that haven't been
    // decided yet, along with the symbol whose body they're in. They're decided in
    // `visit_effects`, the same way as unreferenced bindings
    unvisited_guards: Vec<UnvisitedGuard>,

    // Keeps track of the direct calls to each function, so that a binding that calls the value of
    // one of its parameters can find the function values it's passed
    calls: HashMap<SymbolId, Vec<HirId>>,

    // The entrypoint, whose parameters come from the solution
    main: SymbolId,

    // Finds whether the values of let statements may fail
    failures: FailureAnalysis,
}

/// An index into the places that are lowered more than once (see [`Repeat`]).
type RepeatId = usize;

/// Somewhere that's lowered once for each read of a symbol, rather than once, since its value is
/// substituted for every reference to it. A reference in one of these is as many reads of what it
/// refers to.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
enum Repeat {
    /// The value of an inline binding or constant.
    Value(SymbolId),
    /// An argument for the parameter of an `inline fn`, which substitutes its arguments wherever
    /// the body references its parameters.
    Argument(SymbolId),
    /// The body of the lambda, which is lowered wherever the lambda expression is, and so only
    /// as often as the place that it's in.
    Lambda(SymbolId),
}

/// Where a symbol is referenced.
#[derive(Debug, Clone, Copy)]
struct Site {
    /// The symbol whose body or value the reference is in, which it's evaluated with.
    place: SymbolId,
    /// What's done with the value that's referenced there.
    usage: Usage,
}

/// The state of the walk that finds how often places are evaluated (see
/// [`DependencyGraph::is_evaluated_once`]).
#[derive(Debug, Default)]
struct Walk {
    /// The places and bindings on the way to the one being followed.
    path: HashSet<SymbolId>,
    /// Whether each place is evaluated at most once each time the scope is, once it's known.
    results: HashMap<(SymbolId, Option<SymbolId>), bool>,
}

/// What's done with the value of a reference or a lambda expression, which decides how many
/// times a function that it's the value of can be called.
#[derive(Debug, Clone, Copy)]
enum Usage {
    /// It's called where it's written.
    Called,
    /// It's the whole value of the binding or constant.
    Bound(SymbolId),
    /// It's the argument for the parameter, in a call to a function that's known there.
    Argument(SymbolId),
    /// Anything else, after which it could be called any number of times.
    Other,
}

/// A type guard whose check doesn't use its value, along with the symbol whose body it's in, which
/// hasn't been decided by [`visit_effects`] yet.
#[derive(Debug, Clone, Copy)]
struct UnvisitedGuard {
    current: Option<SymbolId>,
    repeat: Option<RepeatId>,
    guard: HirId,
    value: HirId,
}

/// What [`visit_effects`] does with a value that's only evaluated for its effect.
enum Decision {
    /// It can't fail, so it's never evaluated.
    Never,
    /// It's decided again once more calls are known.
    Wait,
    /// It's evaluated for its effect.
    Evaluate,
}

impl DependencyGraph {
    pub fn build(db: &Database, main: SymbolId, options: CompilerOptions) -> Self {
        let mut graph = Self {
            options,
            dependencies: HashMap::new(),
            owners: HashMap::new(),
            references: HashMap::new(),
            reads: HashMap::new(),
            tail_reads: HashMap::new(),
            tail_references: HashMap::new(),
            repeats: IndexSet::new(),
            repeat: None,
            definitions: HashMap::new(),
            sites: HashMap::new(),
            usages: HashMap::new(),
            repeated: HashSet::new(),
            inline_values: HashSet::new(),
            closures: HashSet::new(),
            program_values: IndexSet::new(),
            loops: HashSet::new(),
            loop_captures: HashSet::new(),
            loop_dependencies: HashMap::new(),
            sole_loops: HashMap::new(),
            const_exprs: IndexSet::new(),
            current: None,
            pending: vec![main],
            referenced: Vec::new(),
            visited: HashSet::new(),
            lets: HashMap::new(),
            effects: HashSet::new(),
            unvisited_effects: Vec::new(),
            guard_effects: HashSet::new(),
            unvisited_guards: Vec::new(),
            calls: HashMap::new(),
            main,
            failures: FailureAnalysis::new(options.enforces_operand_limits()),
        };

        // The symbols are visited depth first, in the order they're referenced, like a recursive
        // traversal would, except that each symbol's whole body is visited before the symbols it
        // references. The bindings and guard values that are only evaluated for their effect are
        // visited last, until there are no more of them.
        loop {
            while let Some(symbol) = graph.pending.pop() {
                // We've already visited this symbol, so we don't need to track its dependencies
                // again
                if !graph.visited.insert(symbol) {
                    continue;
                }

                visit_symbol(db, &mut graph, symbol);

                let referenced = std::mem::take(&mut graph.referenced);
                graph.pending.extend(referenced.into_iter().rev());
            }

            if !visit_effects(db, &mut graph) {
                break;
            }
        }

        // This happens once everything has been visited, since the symbol a binding destructures
        // might not have been visited before the binding itself
        for symbol in std::mem::take(&mut graph.visited) {
            assign_destructured_owner(db, &mut graph, symbol);
        }

        graph.count_reads(db);
        graph.find_repeated(db);
        graph.find_loop_captures(db);
        graph.find_loop_dependencies(db);
        graph.find_sole_loops();

        graph
    }

    /// Enters a place that's inside the one being visited.
    fn enter(&mut self, repeat: Repeat) -> RepeatId {
        let (id, _) = self.repeats.insert_full((repeat, self.repeat));
        self.repeat = Some(id);
        id
    }

    /// The place that the symbol is defined in, if it's repeated. The parts of a destructured
    /// value don't have a let statement of their own, so they're defined where it is.
    fn definition(&self, db: &Database, mut symbol: SymbolId) -> Option<RepeatId> {
        loop {
            if let Some(&repeat) = self.definitions.get(&symbol) {
                return Some(repeat);
            }

            // A binding with a let statement is defined there, whatever its value refers to
            if self.lets.contains_key(&symbol) {
                return None;
            }

            let Symbol::Binding(binding) = db.symbol(symbol) else {
                return None;
            };

            symbol = binding.value.reference.as_ref()?.symbol;
        }
    }

    /// Counts how many times each symbol is read, now that every reference has been visited.
    ///
    /// A reference is lowered once for each time the code it's in is. That's once for the body
    /// of a function, or the value of a binding or constant that's bound. But the value of an
    /// inline binding or constant is lowered again for every reference to it, and so is the
    /// argument of an `inline fn` for every reference to its parameter. So a reference in one of
    /// those counts as many times as the binding, constant or parameter is read itself. The parts
    /// of a destructured value are inline bindings that read it, so a part that's used three
    /// times reads the value three times, which must not be taken for a single reference to a
    /// value that can be substituted there.
    ///
    /// A let statement or a lambda expression in one of those places is lowered as often too,
    /// along with the value of the binding or the body of the lambda. There's a copy of the
    /// binding, or of the lambda and its parameters, in each copy of the place, so their reads
    /// are counted for one copy: the references outside the place they're defined in don't
    /// exist, and the ones inside aren't multiplied by how often that place is lowered.
    ///
    /// The default value of a struct's field is one of these values: it's an inline constant
    /// that every initializer that leaves the field out refers to, so what it references is read
    /// in each of them.
    ///
    /// The counts are for one copy of each function's body. A function that's inlined into more
    /// than one call has a copy of its body in each, but they each read what's in them as often.
    ///
    /// A binding or constant whose value is small and static is the exception: its reads aren't
    /// multiplied, so one that's referenced once is still substituted, and folded with what uses
    /// it. Repeating such a value costs no more than binding it would.
    fn count_reads(&mut self, db: &Database) {
        let mut sites: HashMap<SymbolId, Vec<(Option<RepeatId>, usize)>> = HashMap::new();

        for (&(repeat, symbol), &count) in &self.reads {
            sites.entry(symbol).or_default().push((repeat, count));
        }

        // The counts don't depend on the order, unless a value is substituted into itself, which
        // it can't be. If it ever is, they're still the same from run to run
        let mut symbols: Vec<SymbolId> = sites.keys().copied().collect();
        symbols.sort_unstable();

        let mut counter = ReadCounter {
            db,
            graph: self,
            sites,
            counts: HashMap::new(),
            counting: HashSet::new(),
            static_sizes: HashMap::new(),
        };

        for symbol in symbols {
            counter.reads(symbol);
        }

        // The reads that only count for the environment are multiplied the same way
        let mut tail_reads: Vec<_> = self
            .tail_reads
            .iter()
            .map(|(&key, &count)| (key, count))
            .collect();
        tail_reads.sort_unstable();

        let mut tail_references: HashMap<SymbolId, usize> = HashMap::new();

        for ((repeat, symbol), count) in tail_reads {
            let times = if counter.static_size(symbol).is_none() {
                let definition = self.definition(db, symbol);
                counter.times_lowered(repeat, definition)
            } else {
                1
            };

            let total = tail_references.entry(symbol).or_insert(0);
            *total = total
                .saturating_add(count.saturating_mul(times))
                .min(MAX_READS);
        }

        self.references = counter.counts;
        self.tail_references = tail_references;
    }

    /// Whether the binding or constant is only referenced once, but the reference can be evaluated
    /// more than once for each time the value would be if it were bound: at its let statement for
    /// a binding, and when the program starts for a constant. That's the case for a reference in
    /// a lambda that can be called more than once, such as `k` in
    /// `let k = total(xs); map(xs, fn(x) => x + k)`, or for a constant in a function that calls
    /// itself. Substituting the value there would evaluate it again each time, so it's bound
    /// instead, unless it's compiled to a constant.
    ///
    /// A value that's cheap to evaluate isn't one of these, since evaluating it again costs less
    /// than the bytes that binding it takes (see [`is_cheap_to_repeat`]).
    pub fn is_repeated(&self, symbol: SymbolId) -> bool {
        self.repeated.contains(&symbol)
    }

    /// Finds the bindings and constants that are only referenced once, somewhere that can be
    /// evaluated more than once, and whose values aren't cheap to evaluate (see
    /// [`Self::is_repeated`]). An `inline` one is always substituted, and one that's referenced
    /// more than once is bound unless it's only a path.
    fn find_repeated(&mut self, db: &Database) {
        let mut walk = Walk::default();
        let mut repeated = HashSet::new();

        // The answers don't depend on the order, but they're found in the same one each run
        let mut sites: Vec<(SymbolId, Site)> = self
            .sites
            .iter()
            .map(|(&symbol, &site)| (symbol, site))
            .collect();
        sites.sort_unstable_by_key(|&(symbol, _)| symbol);

        for (symbol, site) in sites {
            if self.references(symbol) != 1 {
                continue;
            }

            // What the value is evaluated with if it's bound: the function or value that a
            // binding's let statement is in, or the program for a constant
            let (scope, value) = match db.symbol(symbol) {
                Symbol::Binding(binding) if !binding.inline => {
                    let Some(&owner) = self.owners.get(&symbol) else {
                        continue;
                    };
                    (Some(owner), binding.value.hir)
                }
                Symbol::Constant(constant) if !constant.inline => (None, constant.value.hir),
                Symbol::Binding(_)
                | Symbol::Constant(_)
                | Symbol::Unresolved
                | Symbol::Module(_)
                | Symbol::Function(_)
                | Symbol::Parameter(_)
                | Symbol::Builtin(_) => continue,
            };

            if !self.is_evaluated_once(db, site.place, scope, &mut walk)
                && !is_cheap_to_repeat(db, self, value)
            {
                repeated.insert(symbol);
            }
        }

        self.repeated = repeated;
    }

    /// Whether the body or value of `place` is evaluated at most once each time `scope` is, or
    /// each time the program is run if that's `None`. This is found by following each place to
    /// the one that evaluates it, which has to end at the scope:
    ///
    /// - A function's body is evaluated when it's called. That's once if its only reference is
    ///   called once (see [`Self::call_site`]), each time the place that the call is in is
    ///   evaluated. A function that calls itself is referenced more than once, and a loop's body
    ///   is evaluated for each item.
    /// - The value of an `inline` binding or constant is evaluated wherever it's referenced.
    /// - The value of any other binding is evaluated once at its let statement, or once where
    ///   it's referenced if it's substituted there. If it's substituted somewhere that's
    ///   evaluated more often, it's cheap along with everything that's substituted into it,
    ///   which is all that's evaluated again. Any other constant is evaluated once when the
    ///   program starts, or substituted the same way.
    ///
    /// An argument of a call that's inlined is evaluated where its parameter is referenced, but
    /// a loop or a lambda that isn't inlined captures the argument's value, so that's still once
    /// for each time the call is evaluated.
    fn is_evaluated_once(
        &self,
        db: &Database,
        place: SymbolId,
        scope: Option<SymbolId>,
        walk: &mut Walk,
    ) -> bool {
        if scope == Some(place) {
            return true;
        }

        // The entrypoint is run once, unless the program calls it again
        if place == self.main {
            return scope.is_none() && self.references(place) == 0;
        }

        if let Some(&once) = walk.results.get(&(place, scope)) {
            return once;
        }

        // The places on the way here, which nothing is evaluated by. This keeps the walk finite
        // if that changes. A place can be reached again by another way, such as two calls of the
        // same function that each pass it a lambda, so it's taken off once it's been followed
        if !walk.path.insert(place) {
            return false;
        }

        // A chain of inline constants, or of calls, is as deep as it's long
        let once = rue_types::grow_stack(|| match db.symbol(place) {
            Symbol::Function(_) => {
                !self.loops.contains(&place)
                    && self.references(place) == 1
                    && self.sites.get(&place).is_some_and(|&site| {
                        self.call_site(db, site, walk)
                            .is_some_and(|call| self.is_evaluated_once(db, call, scope, walk))
                    })
            }
            Symbol::Binding(BindingSymbol { inline: true, .. })
            | Symbol::Constant(ConstantSymbol { inline: true, .. }) => {
                self.references(place) == 1
                    && self
                        .sites
                        .get(&place)
                        .is_some_and(|site| self.is_evaluated_once(db, site.place, scope, walk))
            }
            Symbol::Binding(_) | Symbol::Constant(_) => match self.owners.get(&place) {
                Some(&owner) => self.is_evaluated_once(db, owner, scope, walk),
                // A constant of the program
                None => scope.is_none(),
            },
            Symbol::Unresolved | Symbol::Module(_) | Symbol::Parameter(_) | Symbol::Builtin(_) => {
                false
            }
        });
        walk.path.remove(&place);

        // A place that's found not to be evaluated once because the walk came back to it is on
        // the way back to itself, so that's the answer from anywhere. The answers are kept, so
        // that a place that many lambdas are passed through is only followed once
        walk.results.insert((place, scope), once);
        once
    }

    /// The place that calls a function once each time it's evaluated, if the function is only
    /// called there, given the only reference to it (or the lambda expression, for a lambda).
    ///
    /// The function's value can go through bindings that are only referenced once, as in
    /// `let g = fn(x) => x + k; g(1)`, and through the parameters of the functions it's passed
    /// to, if they're known where they're called, as long as each parameter is only referenced
    /// once, and the call it ends up in is evaluated once each time that function is. Anything
    /// else could call it any number of times, such as `map(xs, g)`, whose parameter is called
    /// and passed on.
    fn call_site(&self, db: &Database, site: Site, walk: &mut Walk) -> Option<SymbolId> {
        let (symbol, function) = match site.usage {
            Usage::Called => return Some(site.place),
            Usage::Bound(binding) => (binding, None),
            Usage::Argument(parameter) => (parameter, Some(*self.owners.get(&parameter)?)),
            Usage::Other => return None,
        };

        let next = *self.sites.get(&symbol)?;

        if self.references(symbol) != 1 || !walk.path.insert(symbol) {
            return None;
        }

        // A chain of bindings that are each the one before is as deep as it's long
        let call = rue_types::grow_stack(|| self.call_site(db, next, walk));
        walk.path.remove(&symbol);
        let call = call?;

        let Some(function) = function else {
            return Some(call);
        };

        // The function calls its parameter once, each time it's called from where the argument is
        self.is_evaluated_once(db, call, Some(function), walk)
            .then_some(site.place)
    }

    /// The block that the binding's let statement is in, and its index in the block's statements.
    pub fn let_statement(&self, symbol: SymbolId) -> Option<(HirId, usize)> {
        self.lets.get(&symbol).copied()
    }

    /// Whether the binding is never referenced, but its value may fail, so it's evaluated for its
    /// effect at its let statement. Unreferenced bindings are otherwise not evaluated at all.
    pub fn is_effect(&self, symbol: SymbolId) -> bool {
        self.effects.contains(&symbol) && self.references(symbol) == 0
    }

    /// Whether the type guard's check doesn't use its value, but the value may fail, so it's
    /// evaluated for its effect. Such a guard's value is otherwise not evaluated at all.
    pub fn is_guard_effect(&self, guard: HirId) -> bool {
        self.guard_effects.contains(&guard)
    }

    /// What [`visit_effects`] does with a value that's only evaluated for its effect. A value that
    /// only fails if a function value that a parameter is passed fails is decided once the calls
    /// that pass them have been visited.
    fn decide(&self, db: &Database, effect: Effect) -> Decision {
        match effect {
            Effect::Never => Decision::Never,
            Effect::IfCallsMayFail(calls) if !self.calls_may_fail(db, &calls) => Decision::Wait,
            Effect::Always | Effect::IfCallsMayFail(_) => Decision::Evaluate,
        }
    }

    /// Whether calling the function value of the parameter may fail, given every call of its
    /// function (see [`Self::calls_may_fail`]).
    pub(crate) fn parameter_values_may_fail(&self, db: &Database, parameter: SymbolId) -> bool {
        self.calls_may_fail(db, &IndexSet::from([parameter]))
    }

    /// Whether calling the function values of any of the parameters may fail, given the calls
    /// that have been visited so far. The parameters of `main` come from the solution, and a
    /// function that's used as a value, such as a lambda, could be called from anywhere, so
    /// calling their values may fail.
    fn calls_may_fail(&self, db: &Database, parameters: &IndexSet<SymbolId>) -> bool {
        let mut visited = HashSet::new();

        parameters
            .iter()
            .any(|&parameter| self.parameter_calls_may_fail(db, parameter, &mut visited))
    }

    fn parameter_calls_may_fail(
        &self,
        db: &Database,
        parameter: SymbolId,
        visited: &mut HashSet<SymbolId>,
    ) -> bool {
        // A parameter that's already being checked only passes on the values that are passed to
        // it elsewhere, which are checked there
        if !visited.insert(parameter) {
            return false;
        }

        let Some(&function) = self.owners.get(&parameter) else {
            return true;
        };

        if function == self.main || self.closures.contains(&function) {
            return true;
        }

        let Symbol::Function(symbol) = db.symbol(function) else {
            return true;
        };

        let Some(index) = symbol
            .fixed_parameters()
            .iter()
            .position(|&fixed| fixed == parameter)
        else {
            return true;
        };

        self.calls
            .get(&function)
            .into_iter()
            .flatten()
            .any(|&call| {
                let Hir::FunctionCall(call) = db.hir(call) else {
                    return true;
                };

                // A spread argument is a list of the rest of the arguments
                let spread = !call.nil_terminated && index + 1 >= call.args.len();

                spread
                    || call.args.get(index).is_none_or(|&arg| {
                        // A parameter that's passed on is called with what's passed for it
                        if let Callee::Parameter(forwarded) = resolve_callee(db, arg) {
                            self.parameter_calls_may_fail(db, forwarded, visited)
                        } else {
                            self.failures.value_call_may_fail(db, arg)
                        }
                    })
            })
    }

    pub fn references(&self, symbol: SymbolId) -> usize {
        *self.references.get(&symbol).unwrap_or(&0)
    }

    /// The number of times a symbol is referenced for deciding where it goes in the environment,
    /// where the most referenced symbols have the shortest paths. A struct that's updated from a
    /// value reuses the rest of it with one reference, where a struct that's rebuilt by hand has
    /// one for each field, so the value counts as being referenced that often here. Otherwise
    /// updating a struct could move its value further from the root than rebuilding it would,
    /// which makes every path to it longer.
    pub fn layout_references(&self, symbol: SymbolId) -> usize {
        self.references(symbol)
            .saturating_add(*self.tail_references.get(&symbol).unwrap_or(&0))
    }

    pub fn is_closure(&self, symbol: SymbolId) -> bool {
        self.closures.contains(&symbol)
    }

    /// Whether the function is lowered to the program it's compiled to on its own where it's
    /// used as a value, rather than to a closure (see [`crate::program_value`]).
    pub fn is_program_value(&self, symbol: SymbolId) -> bool {
        self.program_values.contains(&symbol)
    }

    /// The functions that are lowered to their own programs where they're used as values, in the
    /// order they were found in.
    pub fn program_values(&self) -> impl Iterator<Item = SymbolId> + '_ {
        self.program_values.iter().copied()
    }

    /// Whether the symbol is the function a `for` expression is compiled to.
    pub fn is_loop(&self, symbol: SymbolId) -> bool {
        self.loops.contains(&symbol)
    }

    /// Whether what references the symbol can be evaluated once for each item of a loop, or more,
    /// though the symbol is only evaluated once: it's referenced inside of a loop that it's
    /// defined outside of, such as `k` in `for x in xs { x * k }`, or it's defined in a loop and
    /// referenced inside of a lambda in it, which can be called any number of times. Its value
    /// isn't substituted there unless that's as cheap as referring to it.
    pub fn is_loop_capture(&self, symbol: SymbolId) -> bool {
        self.loop_captures.contains(&symbol)
    }

    fn find_loop_captures(&mut self, db: &Database) {
        if self.loops.is_empty() {
            return;
        }

        let mut captures = HashSet::new();

        for (&from, dependencies) in &self.dependencies {
            // The loops and lambdas that the reference is inside of. Any symbol that's used as a
            // value is among the closures, but only a function is one that's called again: a
            // binding that's destructured is referenced by the bindings for its parts, which
            // are evaluated where it's defined
            let functions: Vec<SymbolId> = iter::once(from)
                .chain(self.owners(from))
                .filter(|&symbol| {
                    self.loops.contains(&symbol)
                        || (self.closures.contains(&symbol)
                            && matches!(db.symbol(symbol), Symbol::Function(_)))
                })
                .collect();

            if !functions.iter().any(|symbol| self.loops.contains(symbol)) {
                continue;
            }

            // What an `inline` binding or constant refers to is evaluated wherever it's
            // referred to, so it's referred to there too
            let mut pending: Vec<SymbolId> = dependencies.iter().copied().collect();
            let mut seen = HashSet::new();
            let mut inlined = HashSet::new();

            while let Some(dependency) = pending.pop() {
                if !seen.insert(dependency) {
                    continue;
                }

                // What's defined inside of a value that's evaluated here is defined here too,
                // such as a let statement in a block, so only what it refers to is from outside
                if self
                    .owners(dependency)
                    .any(|owner| inlined.contains(&owner))
                {
                    pending.extend(self.dependencies.get(&dependency).into_iter().flatten());
                    continue;
                }

                // Only values are evaluated where they're substituted
                let inline = match db.symbol(dependency) {
                    Symbol::Binding(binding) => binding.inline,
                    Symbol::Constant(constant) => constant.inline,
                    Symbol::Parameter(_) => false,
                    _ => continue,
                };

                let in_loop = self
                    .owners(dependency)
                    .any(|owner| self.loops.contains(&owner));

                if !functions.iter().any(|&function| {
                    !self.is_defined_in(dependency, function)
                        && (in_loop || self.loops.contains(&function))
                }) {
                    continue;
                }

                if inline {
                    inlined.insert(dependency);
                    pending.extend(self.dependencies.get(&dependency).into_iter().flatten());
                } else {
                    captures.insert(dependency);
                }
            }
        }

        self.loop_captures = captures;
    }

    /// What the loop captures, if the symbol is one: itself and the other functions and constants
    /// of the program that it needs, and the values that it refers to from the functions it's
    /// defined in.
    ///
    /// A loop is lowered once, where the program's functions are, but it's called where its
    /// `for` expression is, which can be inside a call that's inlined, or a run of let statements
    /// that are substituted. So what it captures can't depend on any of that, unlike what a
    /// lambda captures, which is lowered where it's written. A value from outside of the loop
    /// that it refers to is always captured, and never substituted (see
    /// [`Self::is_loop_capture`]), so the loop doesn't need what the value depends on. Only an
    /// `inline` value, or one that's made of literals, is lowered in the loop, which then
    /// captures what that refers to.
    pub fn loop_captures(&self, symbol: SymbolId) -> Option<&IndexSet<SymbolId>> {
        self.loop_dependencies.get(&symbol)
    }

    fn find_loop_dependencies(&mut self, db: &Database) {
        let mut loops: Vec<SymbolId> = self.loops.iter().copied().collect();
        loops.sort();

        for function in loops {
            // The functions that the loop is defined in, whose values it can refer to
            let enclosing: Vec<SymbolId> = self.owners(function).collect();

            let mut result = IndexSet::new();
            let mut visited = HashSet::new();
            // Whether the path to each symbol goes through a function the loop is defined in,
            // such as a call to it in the body. That function's values are its own from there
            let mut stack = vec![(function, false)];

            while let Some((current, entered)) = stack.pop() {
                if !visited.insert((current, entered)) {
                    continue;
                }

                // What's referenced, from the last to the first, so that it's taken in order
                let mut pending: Vec<(SymbolId, bool)> = Vec::new();
                let dependencies = &self.dependencies;
                let inline_values = &self.inline_values;
                let refer = |pending: &mut Vec<(SymbolId, bool)>, symbol, entered| {
                    let references = dependencies.get(&symbol).into_iter().flatten();
                    pending.extend(references.rev().map(|&reference| (reference, entered)));
                };

                refer(&mut pending, current, entered);

                while let Some((dependency, entered)) = pending.pop() {
                    let entered = entered || enclosing.contains(&dependency);

                    // An `inline` value is lowered where it's referenced, so what it refers to
                    // is referenced there too, in place of the value, rather than after
                    // everything else that's referenced. The default value of a struct's field
                    // is one of these, and the loop captures the same symbols in the same order
                    // as if the value were written in the initializer
                    let mut visit =
                        |stack: &mut Vec<(SymbolId, bool)>, pending: &mut Vec<(SymbolId, bool)>| {
                            if !inline_values.contains(&dependency) {
                                stack.push((dependency, entered));
                            } else if visited.insert((dependency, entered)) {
                                refer(pending, dependency, entered);
                            }
                        };

                    // A loop is a function of the program wherever it's defined, including the
                    // loop itself and the loops inside of it
                    if self.loops.contains(&dependency) {
                        result.insert(dependency);
                        stack.push((dependency, entered));
                        continue;
                    }

                    // A value of a function the loop is in is one that the function itself
                    // defines. Anything else that's defined somewhere is defined in the loop, or
                    // inside of something that's lowered in it, such as the block of an `inline
                    // let` outside of the loop, so it's lowered with the loop
                    let enclosed = self
                        .owners
                        .get(&dependency)
                        .is_some_and(|owner| enclosing.contains(owner));

                    // A lambda that's lowered in the loop is lowered where it's written, and
                    // captures everything it can reach, including what the values it refers to
                    // depend on (see `Self::dependencies`). The loop has to have all of that for
                    // it, so those values aren't substituted either
                    if self.is_defined_in(dependency, function)
                        || (self.owners.contains_key(&dependency) && !enclosed)
                    {
                        // Only a lambda is one, though any symbol that's used as a value is
                        // among the closures. One that's reached through a call to a function the
                        // loop is in is that call's own, as its values are
                        if !entered
                            && self.closures.contains(&dependency)
                            && matches!(db.symbol(dependency), Symbol::Function(_))
                        {
                            for capture in self.dependencies(dependency, true) {
                                // A loop is a function of the program wherever it's defined:
                                // in the loop, or in a value that's lowered in it, such as the
                                // default value of a struct's field, which captures the same
                                // as if it were written in the initializer
                                if !self.loops.contains(&capture)
                                    && (self.is_defined_in(capture, function)
                                        || self
                                            .owners
                                            .get(&capture)
                                            .is_some_and(|owner| !enclosing.contains(owner)))
                                {
                                    continue;
                                }

                                result.insert(capture);

                                let value = match db.symbol(capture) {
                                    Symbol::Binding(binding) => {
                                        Some((binding.inline, binding.value.hir))
                                    }
                                    Symbol::Constant(constant) => {
                                        Some((constant.inline, constant.value.hir))
                                    }
                                    _ => None,
                                };

                                if let Some((inline, hir)) = value
                                    && !inline
                                    && !db.is_literal_hir(hir)
                                    && self.owners.contains_key(&capture)
                                {
                                    self.loop_captures.insert(capture);
                                }
                            }
                        }

                        visit(&mut stack, &mut pending);
                        continue;
                    }

                    // A function or constant of the program, which the loop needs what it needs
                    // of too
                    if !enclosed {
                        result.insert(dependency);
                        visit(&mut stack, &mut pending);
                        continue;
                    }

                    // A value of a function the loop is in, reached through a call to it, is
                    // that call's own
                    if entered {
                        stack.push((dependency, entered));
                        continue;
                    }

                    result.insert(dependency);

                    // A value that's lowered where it's referred to is lowered in the loop
                    let substituted = match db.symbol(dependency) {
                        Symbol::Parameter(_) => false,
                        Symbol::Binding(binding) => {
                            binding.inline || db.is_literal_hir(binding.value.hir)
                        }
                        Symbol::Constant(constant) => {
                            constant.inline || db.is_literal_hir(constant.value.hir)
                        }
                        _ => true,
                    };

                    if substituted {
                        visit(&mut stack, &mut pending);
                    } else {
                        self.loop_captures.insert(dependency);
                    }
                }
            }

            self.loop_dependencies.insert(function, result);
        }
    }

    pub fn const_exprs(&self) -> impl Iterator<Item = HirId> + '_ {
        self.const_exprs.iter().copied()
    }

    /// What the body or value of the symbol references, in order.
    fn references_of(&self, symbol: SymbolId) -> impl DoubleEndedIterator<Item = SymbolId> + '_ {
        self.dependencies
            .get(&symbol)
            .into_iter()
            .flatten()
            .copied()
    }

    /// The functions that the symbol is defined in, from the innermost to the outermost.
    fn owners(&self, symbol: SymbolId) -> impl Iterator<Item = SymbolId> + '_ {
        iter::successors(self.owners.get(&symbol).copied(), |owner| {
            self.owners.get(owner).copied()
        })
    }

    /// The function that the symbol is defined in, such as the one a parameter belongs to.
    pub fn owner(&self, symbol: SymbolId) -> Option<SymbolId> {
        self.owners.get(&symbol).copied()
    }

    /// Whether the symbol is defined in the function, or in a function defined in it, such as a
    /// parameter or local of the function or of a lambda inside it.
    pub fn is_defined_in(&self, symbol: SymbolId, function: SymbolId) -> bool {
        self.owners(symbol).any(|owner| owner == function)
    }

    /// The loop that the loop capture is only referenced inside of, if there's one: in its body,
    /// or in the value or body of something that's defined in it. Of the loops inside of each
    /// other that capture it, that's the outermost one, which is called from where the value is
    /// defined. The loop can refer to it any number of times, but nothing outside of it does.
    pub fn sole_loop(&self, symbol: SymbolId) -> Option<SymbolId> {
        self.sole_loops.get(&symbol).copied()
    }

    fn find_sole_loops(&mut self) {
        if self.loop_captures.is_empty() {
            return;
        }

        // What refers to each capture
        let mut referrers: HashMap<SymbolId, Vec<SymbolId>> = HashMap::new();

        for (&from, dependencies) in &self.dependencies {
            for &dependency in dependencies {
                if self.loop_captures.contains(&dependency) {
                    referrers.entry(dependency).or_default().push(from);
                }
            }
        }

        let mut sole_loops = HashMap::new();

        for (capture, referrers) in referrers {
            // A loop that the capture is defined in doesn't capture it
            let contains_all = |function: SymbolId| {
                self.loop_dependencies
                    .get(&function)
                    .is_some_and(|captures| captures.contains(&capture))
                    && referrers
                        .iter()
                        .all(|&from| from == function || self.is_defined_in(from, function))
            };

            // The loops around the first reference, from the outermost, which contain the others
            // too if any loop does
            let outermost = iter::once(referrers[0])
                .chain(self.owners(referrers[0]))
                .filter(|symbol| self.loops.contains(symbol))
                .filter(|&function| contains_all(function))
                .last();

            if let Some(function) = outermost {
                sole_loops.insert(capture, function);
            }
        }

        self.sole_loops = sole_loops;
    }

    /// Whether the function can call itself other than through a loop, so that it can't be
    /// inlined. A loop on the way back to the function is in a cycle, so it calls itself and is
    /// never inlined. A function that only gets back to itself through loops, such as one whose
    /// body is a `for` that calls it for each item, can be inlined where it's called without
    /// inlining itself again.
    ///
    /// A loop that captures one of the function's own values, such as a parameter it passes back
    /// to the function, doesn't count. Inlined into that loop, the function is given its own
    /// parameter as an argument, which stands for itself, so measuring it goes to the depth limit
    /// and is dropped, and a function that calls itself after such a loop is measured as a
    /// candidate that comes out larger (as in `tests/warnings/for_loop_ends.rue`).
    pub fn calls_itself_outside_loops(&self, symbol: SymbolId) -> bool {
        let mut visited = HashSet::new();
        let mut stack: Vec<SymbolId> = self
            .dependencies
            .get(&symbol)
            .into_iter()
            .flatten()
            .copied()
            .collect();

        while let Some(current) = stack.pop() {
            if current == symbol {
                return true;
            }

            if !visited.insert(current) {
                continue;
            }

            if self.loops.contains(&current)
                && !self
                    .loop_dependencies
                    .get(&current)
                    .is_some_and(|captures| {
                        captures.iter().any(|&capture| {
                            !self.loops.contains(&capture) && self.is_defined_in(capture, symbol)
                        })
                    })
            {
                continue;
            }

            stack.extend(
                self.dependencies
                    .get(&current)
                    .into_iter()
                    .flatten()
                    .copied(),
            );
        }

        false
    }

    pub fn dependencies(&self, symbol: SymbolId, captures_only: bool) -> IndexSet<SymbolId> {
        self.dependencies_with(symbol, captures_only, |_| false)
    }

    /// What the function captures, like [`Self::dependencies`] with only captures, but without
    /// what the values that it captures depend on, if they're bound outside of it (`is_bound`).
    /// A function only needs the value of a binding that's bound before it, as in
    /// `let k = total(xs); map(xs, fn(x) => x + k + k)`, where the lambda captures `k`, but
    /// neither `total` nor `xs`. Whatever binds the value has what it depends on: a binding is
    /// defined in a function that it's local to, and a constant is bound by the entrypoint, for
    /// which `is_bound` must not hold.
    pub fn captures(
        &self,
        symbol: SymbolId,
        is_bound: impl Fn(SymbolId) -> bool,
    ) -> IndexSet<SymbolId> {
        self.dependencies_with(symbol, true, is_bound)
    }

    /// Walks what the symbol depends on, and collects all of it, or only what it captures. What
    /// a value that's captured depends on is left out if the value `is_bound` outside of the
    /// symbol, since only the value is needed then (see [`Self::captures`]).
    fn dependencies_with(
        &self,
        symbol: SymbolId,
        captures_only: bool,
        is_bound: impl Fn(SymbolId) -> bool,
    ) -> IndexSet<SymbolId> {
        // The locals of the scopes enclosing the symbol are captured, unless they are reached
        // through the scope they're defined in (such as by a recursive call), so each path
        // keeps track of which of these scopes it has entered
        let scopes: Vec<SymbolId> = self.owners(symbol).collect();

        let mut visited = HashSet::new();
        let mut result = IndexSet::new();
        let mut stack = vec![(symbol, false, false, vec![false; scopes.len()])];

        while let Some((current, is_result, is_value, mut entered)) = stack.pop() {
            if !visited.insert((current, is_result, is_value, entered.clone())) {
                continue;
            }

            if is_result {
                result.insert(current);
            }

            // Only the value is needed, rather than what it's evaluated from
            if is_value {
                continue;
            }

            if let Some(index) = scopes.iter().position(|&scope| scope == current) {
                entered[index] = true;
            }

            // What's referenced, from the last to the first, so that it's taken in order
            let mut pending: Vec<SymbolId> = self.references_of(current).rev().collect();
            let mut referenced = HashSet::new();

            while let Some(dependency) = pending.pop() {
                if !referenced.insert(dependency) {
                    continue;
                }

                // Any other local can only be reached through the scope it's defined in. A loop is
                // defined in a function, so that it captures the function's values, but it's a
                // function of the program like one that's declared, which anything can capture
                let is_local = !self.loops.contains(&dependency)
                    && self.owners.get(&dependency).is_some_and(|owner| {
                        scopes
                            .iter()
                            .position(|scope| scope == owner)
                            .is_none_or(|index| entered[index])
                    });

                let is_result = !captures_only || !is_local;

                // An `inline` value is lowered where it's referenced, so what it refers to is
                // referenced there too, in place of the value, rather than after everything
                // else. The default value of a struct's field is one of these, and the symbols
                // are found in the same order as if the value were written in the initializer
                if self.inline_values.contains(&dependency) && !scopes.contains(&dependency) {
                    if visited.insert((dependency, is_result, false, entered.clone())) {
                        if is_result {
                            result.insert(dependency);
                        }

                        pending.extend(self.references_of(dependency).rev());
                    }

                    continue;
                }

                let is_value = !is_local && is_bound(dependency);

                stack.push((dependency, is_result, is_value, entered.clone()));
            }
        }

        result
    }

    /// The round that each of the symbols is bound in, where a symbol is bound in a round after
    /// every symbol in the set that it depends on, as given by [`Self::dependencies`] with only
    /// captures. Its round is one after the latest round of those symbols, or the first round if
    /// there are none. Symbols that aren't `bound_after` are bound in the first round, without
    /// waiting for any of the others.
    ///
    /// A symbol that depends on itself, or on a symbol in the set that does, can't be bound after
    /// its dependencies, so its round is `None`.
    ///
    /// The symbols are expected to be defined in the same scope, as a run of let statements is, or
    /// in none, as the symbols that main captures are. Then this takes time linear in what they can
    /// reach. Otherwise, it falls back to following the dependencies of each symbol.
    pub fn rounds(
        &self,
        symbols: &IndexSet<SymbolId>,
        bound_after: impl Fn(SymbolId) -> bool,
    ) -> Vec<Option<usize>> {
        // A loop is defined in the function its `for` expression is in, but it's captured like
        // the functions that are declared (see `Self::dependencies`), so the symbols main captures
        // aren't in one scope if there's a loop among them
        let has_loop = symbols.iter().any(|symbol| self.loops.contains(symbol));

        let owner = symbols
            .first()
            .and_then(|symbol| self.owners.get(symbol).copied());

        let same_owner = !has_loop
            && symbols
                .iter()
                .all(|symbol| self.owners.get(symbol).copied() == owner);

        debug_assert!(
            same_owner || has_loop,
            "grouped symbols are defined in different scopes"
        );

        if same_owner {
            self.rounds_from_edges(symbols, &bound_after, owner)
        } else {
            self.rounds_from_dependencies(symbols, &bound_after)
        }
    }

    /// Finds the rounds from the direct dependencies, when each of the symbols is defined in the
    /// same scope, `owner`. A symbol then depends on another in the set if it can reach it without
    /// passing through the owner, since that enters the scope the other is defined in (see
    /// [`Self::dependencies`]). If the symbols aren't defined in a scope, any path counts.
    ///
    /// A symbol's round only depends on the symbols in the set that it can reach, so it's the same
    /// for every symbol in a strongly connected component. The components are found with Tarjan's
    /// algorithm, which finishes each one after every component that it can reach, and visits each
    /// symbol that the set can reach once. It's iterative, so that a long chain of bindings
    /// doesn't overflow the stack.
    fn rounds_from_edges(
        &self,
        symbols: &IndexSet<SymbolId>,
        bound_after: &impl Fn(SymbolId) -> bool,
        owner: Option<SymbolId>,
    ) -> Vec<Option<usize>> {
        // Paths stop at the owner, since anything they reach from there isn't a dependency
        let edges = |symbol: SymbolId| {
            if Some(symbol) == owner {
                return Slice::new();
            }

            self.dependencies
                .get(&symbol)
                .map_or(Slice::new(), IndexSet::as_slice)
        };

        let waits = |symbol| symbols.contains(&symbol).then(|| bound_after(symbol));

        let mut components = Components::default();

        for &root in symbols {
            if !bound_after(root) || components.indices.contains_key(&root) {
                continue;
            }

            // Each symbol being visited, along with its edges and how many have been followed
            let mut visits = vec![(components.enter(root, Some(true)), edges(root), 0)];

            while let Some((index, dependencies, next)) = visits.last_mut() {
                let index = *index;

                if let Some(&dependency) = dependencies.get_index(*next) {
                    *next += 1;

                    if let Some(&dependency_index) = components.indices.get(&dependency) {
                        if components.component[dependency_index].is_none() {
                            components.low[index] = components.low[index].min(dependency_index);
                        }
                    } else {
                        let entered = components.enter(dependency, waits(dependency));
                        visits.push((entered, edges(dependency), 0));
                    }

                    continue;
                }

                visits.pop();

                if let Some(&(outer, ..)) = visits.last() {
                    components.low[outer] = components.low[outer].min(components.low[index]);
                }

                if components.low[index] != index {
                    continue;
                }

                // The symbol is the first one visited in its component, so the rest of the
                // component is above it on the stack
                let position = components.stack.iter().rposition(|&i| i == index).unwrap();
                let members = components.stack.split_off(position);
                let component = components.reaches.len();

                for &member in &members {
                    components.component[member] = Some(component);
                }

                let symbol = components.symbols[index];
                let cyclic = members.len() > 1 || edges(symbol).iter().any(|&edge| edge == symbol);
                let mut reach = Reach::default();

                for &member in &members {
                    // Every symbol in a cycle can reach every symbol in it, including itself
                    match components.waits[member] {
                        Some(true) if cyclic => reach.waiting = true,
                        Some(false) if cyclic => reach.round = reach.round.max(1),
                        _ => {}
                    }

                    for dependency in edges(components.symbols[member]) {
                        let dependency = components.indices[dependency];
                        let other = components.component[dependency].unwrap();

                        if other == component {
                            continue;
                        }

                        let other = components.reaches[other];
                        reach.waiting |= other.waiting;

                        // A symbol in the set is bound after what it reaches, unless it's bound in
                        // the first round
                        let round = match components.waits[dependency] {
                            None => other.round,
                            Some(true) => other.round + 1,
                            Some(false) => other.round.max(1),
                        };

                        reach.round = reach.round.max(round);
                    }
                }

                components.reaches.push(reach);
            }
        }

        symbols
            .iter()
            .map(|&symbol| {
                if !bound_after(symbol) {
                    return Some(0);
                }

                let component = components.component[components.indices[&symbol]].unwrap();
                let reach = components.reaches[component];
                (!reach.waiting).then_some(reach.round)
            })
            .collect()
    }

    /// Finds the rounds from the dependencies of each symbol, which works for any set of symbols,
    /// but walks everything each symbol can reach. A symbol is visited once all of its
    /// dependencies in the set have been.
    fn rounds_from_dependencies(
        &self,
        symbols: &IndexSet<SymbolId>,
        bound_after: &impl Fn(SymbolId) -> bool,
    ) -> Vec<Option<usize>> {
        let mut rounds = vec![0; symbols.len()];
        let mut waiting = vec![0; symbols.len()];
        let mut dependents = vec![Vec::new(); symbols.len()];
        let mut ready = Vec::new();

        for (index, &symbol) in symbols.iter().enumerate() {
            if bound_after(symbol) {
                for dependency in &self.dependencies(symbol, true) {
                    if let Some(dependency) = symbols.get_index_of(dependency) {
                        waiting[index] += 1;
                        dependents[dependency].push(index);
                    }
                }
            }

            if waiting[index] == 0 {
                ready.push(index);
            }
        }

        while let Some(index) = ready.pop() {
            for &dependent in &dependents[index] {
                rounds[dependent] = rounds[dependent].max(rounds[index] + 1);
                waiting[dependent] -= 1;

                if waiting[dependent] == 0 {
                    ready.push(dependent);
                }
            }
        }

        // Symbols that are still waiting depend on a cycle, so they're never visited
        rounds
            .into_iter()
            .zip(waiting)
            .map(|(round, waiting)| (waiting == 0).then_some(round))
            .collect()
    }
}

/// The state of Tarjan's algorithm, over the dependency graph, in [`DependencyGraph::rounds`].
#[derive(Debug, Default)]
struct Components {
    /// The order in which each symbol was first visited.
    indices: HashMap<SymbolId, usize>,
    /// The symbol visited at each index.
    symbols: Vec<SymbolId>,
    /// Whether each symbol in the set waits for the ones it depends on, or `None` if it isn't in
    /// the set.
    waits: Vec<Option<bool>>,
    /// The lowest index reachable from each symbol through the ones still on the stack.
    low: Vec<usize>,
    stack: Vec<usize>,
    /// The component that each symbol is in, once it's finished, or `None` while it's still on
    /// the stack.
    component: Vec<Option<usize>>,
    /// What each finished component can reach.
    reaches: Vec<Reach>,
}

impl Components {
    fn enter(&mut self, symbol: SymbolId, waits: Option<bool>) -> usize {
        let index = self.symbols.len();
        self.indices.insert(symbol, index);
        self.symbols.push(symbol);
        self.waits.push(waits);
        self.low.push(index);
        self.stack.push(index);
        self.component.push(None);
        index
    }
}

/// What a strongly connected component of the dependency graph can reach in a set of symbols.
#[derive(Debug, Default, Clone, Copy)]
struct Reach {
    /// The round after every symbol in the set that it can reach.
    round: usize,
    /// Whether it can reach a symbol in the set that depends on itself.
    waiting: bool,
}

/// Counts the reads of each symbol from the references to it (see
/// [`DependencyGraph::count_reads`]).
struct ReadCounter<'a> {
    db: &'a Database,
    graph: &'a DependencyGraph,
    /// The number of references to each symbol in each place.
    sites: HashMap<SymbolId, Vec<(Option<RepeatId>, usize)>>,
    counts: HashMap<SymbolId, usize>,
    /// The symbols whose reads are being counted, so that a value that's substituted into itself
    /// doesn't count forever. That can't happen today, since a binding or constant can't refer
    /// to itself, and neither can an `inline fn`.
    counting: HashSet<SymbolId>,
    /// The size of the value of each binding and constant that's small and static, or `None` if
    /// it isn't, or while it's being found.
    static_sizes: HashMap<SymbolId, Option<usize>>,
}

impl ReadCounter<'_> {
    fn reads(&mut self, symbol: SymbolId) -> usize {
        if let Some(&count) = self.counts.get(&symbol) {
            return count;
        }

        if !self.counting.insert(symbol) {
            return 1;
        }

        // Each symbol is only counted once
        let sites = self.sites.remove(&symbol).unwrap_or_default();
        let multiplied = self.static_size(symbol).is_none();
        let definition = self.graph.definition(self.db, symbol);

        // A chain of inline bindings is as deep as it's long
        let count = rue_types::grow_stack(|| {
            sites.into_iter().fold(0usize, |total, (repeat, count)| {
                let times = if multiplied {
                    self.times_lowered(repeat, definition)
                } else {
                    1
                };

                total.saturating_add(count.saturating_mul(times))
            })
        })
        .min(MAX_READS);

        self.counting.remove(&symbol);
        self.counts.insert(symbol, count);
        count
    }

    /// How many times the place is lowered for each time that the place `definition` is, which
    /// it's inside of, or altogether if that's `None`.
    fn times_lowered(&mut self, repeat: Option<RepeatId>, definition: Option<RepeatId>) -> usize {
        let Some(id) = repeat else {
            return 1;
        };

        if repeat == definition {
            return 1;
        }

        let (repeat, outer) = self.graph.repeats[id];

        let times = match repeat {
            Repeat::Value(symbol) | Repeat::Argument(symbol) => self.reads(symbol),
            Repeat::Lambda(_) => 1,
        };

        // Something that's visited is lowered, apart from the argument for a parameter that an
        // `inline fn` never references. That still counts once, so that what's only referenced
        // there is treated the way it was when its reference was visited: as referenced, and so
        // not evaluated for its effect.
        times
            .max(1)
            .saturating_mul(self.times_lowered(outer, definition))
            .min(MAX_READS)
    }

    /// The size of the atoms of the symbol's value, if it's a binding or constant whose value is
    /// static and small (see [`MAX_REPEATED_STATIC_SIZE`]), so that repeating it doesn't evaluate
    /// anything again, or take more bytes than binding it.
    fn static_size(&mut self, symbol: SymbolId) -> Option<usize> {
        if let Some(&size) = self.static_sizes.get(&symbol) {
            return size;
        }

        let value = match self.db.symbol(symbol) {
            Symbol::Binding(BindingSymbol { value, .. })
            | Symbol::Constant(ConstantSymbol { value, .. }) => value.hir,
            Symbol::Unresolved
            | Symbol::Module(_)
            | Symbol::Function(_)
            | Symbol::Parameter(_)
            | Symbol::Builtin(_) => return None,
        };

        // A value isn't static if it's followed back into itself
        self.static_sizes.insert(symbol, None);

        let mut budget = MAX_REPEATED_STATIC_SIZE;

        // A chain of constants is as deep as it's long
        let size = rue_types::grow_stack(|| is_small_static(self, self.db, value, &mut budget))
            .then(|| MAX_REPEATED_STATIC_SIZE - budget);

        self.static_sizes.insert(symbol, size);
        size
    }
}

/// A binding or constant's value is small and static as it's checked where it's passed as an
/// argument (see `Lowerer::is_trivial_argument`), without the values that depend on what's being
/// inlined. This follows references to bindings and constants (see [`ReadCounter::static_size`]).
impl StaticContext for ReadCounter<'_> {
    fn is_small_static_here(&mut self, db: &Database, hir: HirId, budget: &mut usize) -> bool {
        match db.hir(hir) {
            Hir::Reference(symbol) => self
                .static_size(*symbol)
                .is_some_and(|size| take(budget, size)),
            // A guard that doesn't use its value at runtime is lowered to a boolean, unless its
            // value is evaluated for its effect. The lowerer still treats such a guard as static
            // if its value can't fail, which can depend on what's inlined, so it isn't here
            Hir::Guard(guard) => {
                guard.value_uses() == 0 && !self.graph.is_guard_effect(hir) && take(budget, 1)
            }
            _ => false,
        }
    }
}

/// Visits somewhere that's lowered once for each read of something else.
fn visit_repeated(db: &Database, graph: &mut DependencyGraph, repeat: Repeat, hir: HirId) {
    let outer = graph.repeat;
    graph.enter(repeat);
    visit_hir(db, graph, hir, false);
    graph.repeat = outer;
}

/// How many steps into a value that's only referenced once the tail of a struct update can be
/// before the value is bound instead of being used in place, when the value is one that's bound
/// on its own, such as a call that the compiler binds for the update. Each step in place is an
/// operation around the value, and a binding that nothing else shares costs about as much as
/// four of them.
const UNNAMED_TAIL_STEPS: usize = 3;

/// The value that the tail of a struct update is a part of.
struct TailBase {
    symbol: SymbolId,
    /// How many steps into the symbol's value the tail is.
    steps: usize,
    /// Whether the symbol is bound on its own, rather than next to the other bindings of a
    /// block: what the compiler binds a value that isn't a path to for the update, or the result
    /// of a fold for the rest of the list.
    bound_alone: bool,
}

/// The symbol that the tail of a struct update is read from, through the inline bindings and
/// constants whose values are paths themselves, such as the parts of a destructured value. Those
/// are substituted, so the tail is as many steps further into what they're paths into. There
/// isn't one if the tail isn't a path, or it's a path into an inline value that isn't, which is
/// evaluated where the tail is.
fn tail_base(db: &Database, tail: HirId) -> Option<TailBase> {
    let (mut symbol, mut steps) = db.path(tail)?;
    let mut substituted = false;

    loop {
        let value = match db.symbol(symbol) {
            Symbol::Binding(BindingSymbol {
                value,
                inline: true,
                ..
            })
            | Symbol::Constant(ConstantSymbol {
                value,
                inline: true,
                ..
            }) => value.hir,
            Symbol::Binding(binding) => {
                return Some(TailBase {
                    symbol,
                    steps,
                    bound_alone: (binding.name.is_none() && !substituted)
                        || db.is_loop_rest_call(binding.value.hir),
                });
            }
            _ => {
                return Some(TailBase {
                    symbol,
                    steps,
                    bound_alone: false,
                });
            }
        };

        let (inner, inner_steps) = db.path(value)?;
        symbol = inner;
        steps += inner_steps;
        substituted = true;
    }
}

/// Whether the symbol is a parameter of an `inline fn` that's being visited, whose arguments are
/// lowered wherever it's referenced.
fn is_inline_parameter(db: &Database, graph: &DependencyGraph, symbol: SymbolId) -> bool {
    matches!(db.symbol(symbol), Symbol::Parameter(_))
        && std::iter::successors(graph.current, |current| graph.owners.get(current).copied()).any(
            |function| {
                matches!(
                    db.symbol(function),
                    Symbol::Function(function)
                        if function.kind == FunctionKind::Inline
                            && function.parameters.contains(&symbol)
                )
            },
        )
}

fn visit_hir(db: &Database, graph: &mut DependencyGraph, hir: HirId, is_call: bool) {
    match db.hir(hir) {
        Hir::Unresolved
        | Hir::Nil
        | Hir::String(_)
        | Hir::Int(_)
        | Hir::Bytes(_)
        | Hir::Bool(_) => {}
        Hir::Pair(first, rest) => {
            visit_hir(db, graph, *first, false);
            visit_hir(db, graph, *rest, false);
        }
        Hir::List(list) => {
            for &item in &list.items {
                visit_hir(db, graph, item, false);
            }
        }
        Hir::Struct(initializer) => {
            for value in initializer.values() {
                visit_hir(db, graph, value, false);
            }

            // The tail is one reference to the value being updated, but a struct that's rebuilt
            // by hand has one for each field that's read from it, and the difference could make
            // the update larger than the rebuild
            if let Some(tail) = initializer.tail
                && tail.reads > 1
                && graph.current.is_some()
                && let Some(base) = tail_base(db, tail.value)
            {
                // A value that's only referenced once is used in place rather than being bound,
                // which is smaller for the whole value or one step into it, such as the rest
                // after its first field. Any further in, whether that's past more fields or
                // because the value being updated is a field itself, each step to the tail is an
                // operation around the value where a bound value would only have a longer path,
                // so it's bound like it is when the struct is rebuilt, by counting as many
                // reads. A value that the compiler binds for the update is bound on its own,
                // which costs more than a binding next to others, so it's used in place further
                // in.
                let limit = if base.bound_alone {
                    UNNAMED_TAIL_STEPS
                } else {
                    1
                };

                let extra = tail.reads - 1;
                let place = (graph.repeat, base.symbol);

                // How far in the argument for the parameter of an `inline fn` is isn't known
                // here, so it's read as often as it is when the struct is rebuilt. A constant
                // isn't bound where it's used, and reading it more often would only keep it
                // from being folded there
                let bound = !matches!(db.symbol(base.symbol), Symbol::Constant(_))
                    && (base.steps > limit || is_inline_parameter(db, graph, base.symbol));

                if bound {
                    *graph.references.entry(base.symbol).or_insert(0) += extra;
                    *graph.reads.entry(place).or_insert(0) += extra;
                } else {
                    *graph.tail_reads.entry(place).or_insert(0) += extra;
                }
            }
        }
        Hir::FieldAccess(access) => {
            visit_hir(db, graph, access.value, false);
        }
        // A cast has no effect, so a function that's cast and then called is still called directly
        Hir::Cast(value) => {
            visit_hir(db, graph, *value, is_call);
        }
        // The value is referenced as many times as the check it's lowered to uses it. If the
        // check doesn't use it, whether it's evaluated for its effect is decided once the rest of
        // the program has been visited, like a binding that's never referenced.
        Hir::Guard(guard) => {
            let uses = guard.value_uses();

            if uses == 0 {
                graph.unvisited_guards.push(UnvisitedGuard {
                    current: graph.current,
                    repeat: graph.repeat,
                    guard: hir,
                    value: guard.value,
                });
            }

            for _ in 0..uses {
                visit_hir(db, graph, guard.value, false);
            }
        }
        Hir::Const(expr) => {
            // Once it has been evaluated, only the result is used
            if let Some(result) = expr.result {
                visit_hir(db, graph, result, false);
            } else {
                graph.const_exprs.insert(hir);
                visit_hir(db, graph, expr.value, false);
            }
        }
        Hir::Reference(symbol) => {
            // A function whose value is its own program is the constant with that program as its
            // value, so the function itself isn't a dependency
            // The entrypoint refers to itself through the environment instead
            if !is_call
                && *symbol != graph.main
                && let Some(constant) = db.program_constant(*symbol)
            {
                visit_symbol_reference(graph, constant, usage(graph, hir, is_call));
                return;
            }

            if !is_call {
                graph.closures.insert(*symbol);
            }

            // A loop is defined where it's first called, which is in the function that the `for`
            // expression is in, like a lambda is. Its calls to itself come after that
            if let Symbol::Function(function) = db.symbol(*symbol)
                && function.kind == FunctionKind::Loop
                && let Some(current) = graph.current
                && current != *symbol
                && graph.loops.insert(*symbol)
            {
                graph.owners.insert(*symbol, current);
            }

            visit_symbol_reference(graph, *symbol, usage(graph, hir, is_call));
        }
        Hir::Block(block) => {
            // Nothing after a `return` or `raise` is lowered, so it doesn't reference anything
            for (index, stmt) in block.reachable_statements().iter().enumerate() {
                match stmt {
                    Statement::Let(stmt, _) => {
                        if let Some(current) = graph.current {
                            graph.owners.insert(*stmt, current);
                        }

                        graph.lets.insert(*stmt, (hir, index));

                        // The value is lowered with the let statement, which is visited before it
                        if let Some(repeat) = graph.repeat {
                            graph.definitions.insert(*stmt, repeat);
                        }

                        visit_let(db, graph, *stmt);
                    }
                    // A `debug` statement, the value of a `raise` and the message of an
                    // `assert` are only lowered with debug symbols
                    Statement::Assert(..)
                    | Statement::Raise(..)
                    | Statement::If(_)
                    | Statement::Return(..)
                    | Statement::Expr(_)
                    | Statement::Control(_)
                    | Statement::Debug(..) => {
                        for hir in stmt.lowered_hirs(graph.options.debug_symbols) {
                            visit_hir(db, graph, hir, false);
                        }
                    }
                }
            }

            if let Some(body) = block.reachable_body() {
                visit_hir(db, graph, body, false);
            }
        }
        Hir::Program(function) => {
            graph.program_values.insert(*function);
        }
        Hir::Lambda(lambda) => {
            if let Some(current) = graph.current {
                graph.owners.insert(*lambda, current);
            }

            // The body is lowered with the lambda expression, which is visited before it
            if let Some(repeat) = graph.repeat {
                graph.definitions.insert(*lambda, repeat);
            }

            graph.closures.insert(*lambda);

            visit_symbol_reference(graph, *lambda, usage(graph, hir, is_call));
        }
        Hir::If(condition, then, else_, _inline) => {
            visit_hir(db, graph, *condition, false);
            visit_hir(db, graph, *then, false);
            visit_hir(db, graph, *else_, false);
        }
        Hir::FunctionCall(call) => {
            let mut function = None;

            if let Hir::Reference(reference) = db.hir(db.without_casts(call.function))
                && let Symbol::Function(symbol) = db.symbol(*reference)
            {
                graph.calls.entry(*reference).or_default().push(hir);
                function = Some(symbol);
            }

            let inline = function.filter(|function| function.kind == FunctionKind::Inline);

            visit_hir(db, graph, call.function, true);

            // An argument that's passed for a parameter of its own is that parameter's value,
            // if the function that's called is known, which a lambda that's bound to a name is
            if let Some(callee) = resolve_function(db, call.function)
                && let Symbol::Function(symbol) = db.symbol(callee)
            {
                let spread = usize::from(!call.nil_terminated);
                let passed = call.args.len().saturating_sub(spread);

                for (&parameter, &arg) in symbol.fixed_parameters().iter().zip(&call.args[..passed])
                {
                    graph
                        .usages
                        .insert(db.without_casts(arg), Usage::Argument(parameter));
                }
            }

            for (index, &arg) in call.args.iter().enumerate() {
                // The arguments of an `inline fn` are lowered where its parameters are referenced
                let parameter = inline.and_then(|function| function.argument_parameter(index));

                if let Some(parameter) = parameter {
                    visit_repeated(db, graph, Repeat::Argument(parameter), arg);
                } else {
                    visit_hir(db, graph, arg, false);
                }
            }
        }
        Hir::Unary(_op, arg) => {
            visit_hir(db, graph, *arg, false);
        }
        Hir::Binary(_op, lhs, rhs) => {
            visit_hir(db, graph, *lhs, false);
            visit_hir(db, graph, *rhs, false);
        }
        Hir::CoinId(parent, puzzle, amount) => {
            visit_hir(db, graph, *parent, false);
            visit_hir(db, graph, *puzzle, false);
            visit_hir(db, graph, *amount, false);
        }
        Hir::Substr(hir, start, end) => {
            visit_hir(db, graph, *hir, false);
            visit_hir(db, graph, *start, false);
            if let Some(end) = end {
                visit_hir(db, graph, *end, false);
            }
        }
        Hir::G1Map(data, dst) => {
            visit_hir(db, graph, *data, false);
            if let Some(dst) = dst {
                visit_hir(db, graph, *dst, false);
            }
        }
        Hir::G2Map(data, dst) => {
            visit_hir(db, graph, *data, false);
            if let Some(dst) = dst {
                visit_hir(db, graph, *dst, false);
            }
        }
        Hir::Modpow(base, exponent, modulus) => {
            visit_hir(db, graph, *base, false);
            visit_hir(db, graph, *exponent, false);
            visit_hir(db, graph, *modulus, false);
        }
        Hir::BlsPairingIdentity(args) => {
            for arg in args {
                visit_hir(db, graph, *arg, false);
            }
        }
        Hir::BlsVerify(sig, args) => {
            visit_hir(db, graph, *sig, false);
            for arg in args {
                visit_hir(db, graph, *arg, false);
            }
        }
        Hir::Secp256K1Verify(sig, pk, msg) => {
            visit_hir(db, graph, *sig, false);
            visit_hir(db, graph, *pk, false);
            visit_hir(db, graph, *msg, false);
        }
        Hir::Secp256R1Verify(sig, pk, msg) => {
            visit_hir(db, graph, *sig, false);
            visit_hir(db, graph, *pk, false);
            visit_hir(db, graph, *msg, false);
        }
        Hir::Keccak256Verify(value, hash) => {
            visit_hir(db, graph, *value, false);
            visit_hir(db, graph, *hash, false);
        }
        Hir::InfinityG1 => {}
        Hir::InfinityG2 => {}
        Hir::ClvmOp(_op, args) => {
            visit_hir(db, graph, *args, false);
        }
    }
}

fn visit_symbol(db: &Database, graph: &mut DependencyGraph, symbol: SymbolId) {
    graph.current = Some(symbol);

    // Its value or body is lowered as often as the place it's defined in
    graph.repeat = graph.definition(db, symbol);

    // A function that's the whole value of a binding or constant is called through it
    if let Symbol::Constant(ConstantSymbol { value, .. })
    | Symbol::Binding(BindingSymbol { value, .. }) = db.symbol(symbol)
    {
        graph
            .usages
            .insert(db.without_casts(value.hir), Usage::Bound(symbol));
    }

    match db.symbol(symbol) {
        // The value of an inline binding or constant is lowered where it's referenced
        Symbol::Constant(ConstantSymbol { value, inline, .. })
        | Symbol::Binding(BindingSymbol { value, inline, .. })
            if *inline =>
        {
            graph.inline_values.insert(symbol);
            visit_repeated(db, graph, Repeat::Value(symbol), value.hir);
        }
        Symbol::Unresolved | Symbol::Module(_) | Symbol::Parameter(_) | Symbol::Builtin(_) => {}
        Symbol::Function(function) => {
            for &parameter in &function.parameters {
                graph.owners.insert(parameter, symbol);
            }

            // The body of a lambda in a place that's repeated is a place of its own, which its
            // parameters are defined in
            if graph.repeat.is_some() {
                let body = graph.enter(Repeat::Lambda(symbol));

                for &parameter in &function.parameters {
                    graph.definitions.insert(parameter, body);
                }
            }

            visit_hir(db, graph, function.body, false);
        }
        Symbol::Constant(constant) => {
            visit_hir(db, graph, constant.value.hir, false);
        }
        Symbol::Binding(binding) => {
            visit_hir(db, graph, binding.value.hir, false);
        }
    }

    graph.current = None;
    graph.repeat = None;
}

/// Destructured bindings don't have a let statement of their own, so they're defined in the same
/// scope as the symbol they destructure, which might be destructured from another symbol in turn.
/// Bindings from let statements already have an owner, which must be kept rather than replaced by
/// the owner of the symbol they reference.
fn assign_destructured_owner(db: &Database, graph: &mut DependencyGraph, symbol: SymbolId) {
    let mut chain = Vec::new();
    let mut current = symbol;

    let owner = loop {
        if let Some(&owner) = graph.owners.get(&current) {
            break owner;
        }

        let Symbol::Binding(binding) = db.symbol(current) else {
            return;
        };

        let Some(reference) = &binding.value.reference else {
            return;
        };

        chain.push(current);
        current = reference.symbol;
    };

    for symbol in chain {
        graph.owners.insert(symbol, owner);
    }
}

/// Records a non-inline binding, which is evaluated for its effect if it's never referenced and its
/// value may fail, so that the check isn't dropped. Whether it's referenced isn't known until the
/// rest of the program has been visited, so that's decided in [`visit_effects`]. A reference to
/// another non-inline binding can't fail there (see [`FailureAnalysis`]), so the order the
/// bindings are decided in doesn't matter.
fn visit_let(db: &Database, graph: &mut DependencyGraph, symbol: SymbolId) {
    let Symbol::Binding(binding) = db.symbol(symbol) else {
        return;
    };

    // Whether it's an effect is decided once everything that's referenced has been visited
    if !binding.inline {
        graph.unvisited_effects.push((graph.current, symbol));
    }
}

/// Once everything that's referenced has been visited, the bindings whose values may fail but
/// that were never referenced are visited, since they're evaluated for their effect. They don't
/// count as references, but the enclosing function still depends on them, so that it captures what
/// their values need. Visiting them can reach more of these, so this repeats until there are none.
///
/// A constant that's referenced is evaluated whenever the program runs, either at the start or
/// where it's referenced, so reading it again can't fail. That's decided again each time, since
/// visiting effects can reference more constants.
///
/// A binding that only calls the function values of parameters waits until a call passes one that
/// may fail. Visiting more of the program can find more calls, so it's checked again each time.
///
/// The values of type guards whose checks don't use them are decided the same way, and visited
/// where the guard is if they're evaluated for their effect, which counts as a reference.
///
/// These are only added at the end, so that the dependencies of a program without any are in the
/// same order as if they didn't exist. Returns whether any were added, in which case what they
/// reach is visited and this runs again.
fn visit_effects(db: &Database, graph: &mut DependencyGraph) -> bool {
    let evaluated_constants = graph
        .references
        .iter()
        .filter(|&(&symbol, &references)| {
            references > 0
                && matches!(db.symbol(symbol), Symbol::Constant(constant) if !constant.inline)
        })
        .map(|(&symbol, _)| symbol)
        .collect();

    graph.failures = FailureAnalysis::with_evaluated_constants(
        graph.options.enforces_operand_limits(),
        evaluated_constants,
    );

    let effects = std::mem::take(&mut graph.unvisited_effects);
    let mut added = false;

    for (current, symbol) in effects.into_iter().rev() {
        if graph.visited.contains(&symbol) || graph.references(symbol) > 0 {
            continue;
        }

        match graph.decide(db, graph.failures.effect(db, symbol)) {
            Decision::Never => continue,
            Decision::Wait => {
                graph.unvisited_effects.push((current, symbol));
                continue;
            }
            Decision::Evaluate => {}
        }

        graph.effects.insert(symbol);
        added = true;

        if let Some(current) = current {
            graph
                .dependencies
                .entry(current)
                .or_default()
                .insert(symbol);
        }

        graph.pending.push(symbol);
    }

    // The ones that are still waiting are kept in the order they were found
    graph.unvisited_effects.reverse();

    let guards = std::mem::take(&mut graph.unvisited_guards);
    let mut waiting = Vec::new();

    for unvisited in guards {
        match graph.decide(db, graph.failures.value_effect(db, unvisited.value)) {
            Decision::Never => continue,
            Decision::Wait => {
                waiting.push(unvisited);
                continue;
            }
            Decision::Evaluate => {}
        }

        graph.guard_effects.insert(unvisited.guard);
        added = true;

        let outer = std::mem::replace(&mut graph.current, unvisited.current);
        let outer_repeat = std::mem::replace(&mut graph.repeat, unvisited.repeat);
        visit_hir(db, graph, unvisited.value, false);
        graph.current = outer;
        graph.repeat = outer_repeat;

        let referenced = std::mem::take(&mut graph.referenced);
        graph.pending.extend(referenced.into_iter().rev());
    }

    // The ones that are still waiting are decided before any found while visiting the values
    waiting.append(&mut graph.unvisited_guards);
    graph.unvisited_guards = waiting;

    added
}

/// What's done with the value of a reference or a lambda expression where it's written.
fn usage(graph: &DependencyGraph, hir: HirId, is_call: bool) -> Usage {
    if is_call {
        Usage::Called
    } else {
        graph.usages.get(&hir).copied().unwrap_or(Usage::Other)
    }
}

fn visit_symbol_reference(graph: &mut DependencyGraph, symbol: SymbolId, usage: Usage) {
    // We should add the symbol to the dependencies of the symbol we're currently visiting
    if let Some(current) = graph.current {
        graph.sites.insert(
            symbol,
            Site {
                place: current,
                usage,
            },
        );
        graph
            .dependencies
            .entry(current)
            .or_default()
            .insert(symbol);
        *graph.references.entry(symbol).or_insert(0) += 1;
        *graph.reads.entry((graph.repeat, symbol)).or_insert(0) += 1;
    }

    if !graph.visited.contains(&symbol) {
        graph.referenced.push(symbol);
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    /// A small pseudorandom number generator (xorshift), so that the graphs are the same each run.
    struct Random(u64);

    impl Random {
        fn below(&mut self, n: usize) -> usize {
            self.0 ^= self.0 << 13;
            self.0 ^= self.0 >> 7;
            self.0 ^= self.0 << 17;
            (self.0 % n as u64) as usize
        }

        fn chance(&mut self, percent: usize) -> bool {
            self.below(100) < percent
        }
    }

    /// Groups a long chain of bindings, which each depend on the one after and on a parameter of
    /// the function they're in, on a 2 MiB stack. Following each binding's dependencies would
    /// take time quadratic in the length of the chain, which wouldn't finish in any reasonable
    /// time at this length. The walk from the first binding goes through the whole chain, so
    /// walking the graph recursively would overflow the stack.
    #[test]
    fn long_chains_are_grouped_in_linear_time() {
        const LENGTH: usize = 100_000;

        std::thread::Builder::new()
            .stack_size(2 * 1024 * 1024)
            .spawn(|| {
                let mut db = Database::new();
                let main = db.alloc_symbol(Symbol::Unresolved);
                let parameter = db.alloc_symbol(Symbol::Unresolved);
                let mut graph = DependencyGraph::build(&db, main, CompilerOptions::default());

                graph.owners.insert(parameter, main);

                let bindings: IndexSet<SymbolId> = (0..LENGTH)
                    .map(|_| db.alloc_symbol(Symbol::Unresolved))
                    .collect();

                for (index, &binding) in bindings.iter().enumerate() {
                    graph.owners.insert(binding, main);

                    let dependencies = graph.dependencies.entry(binding).or_default();
                    dependencies.insert(parameter);

                    if let Some(&next) = bindings.get_index(index + 1) {
                        dependencies.insert(next);
                    }
                }

                let rounds = graph.rounds(&bindings, |_| true);
                assert!(rounds.into_iter().eq((0..LENGTH).rev().map(Some)));
            })
            .unwrap()
            .join()
            .unwrap();
    }

    /// Checks that the rounds found from the direct dependencies are the same as the ones found
    /// from each symbol's dependencies, on random graphs with cycles, nested scopes and paths that
    /// pass through the scope that the symbols are defined in.
    #[test]
    fn rounds_from_edges_match_dependencies() {
        let mut random = Random(0x9e37_79b9_7f4a_7c15);
        let mut db = Database::new();
        let main = db.alloc_symbol(Symbol::Unresolved);

        for _ in 0..20_000 {
            let mut graph = DependencyGraph::build(&db, main, CompilerOptions::default());
            let count = 1 + random.below(14);
            let nodes: Vec<SymbolId> = (0..count)
                .map(|_| db.alloc_symbol(Symbol::Unresolved))
                .collect();

            // Each symbol is defined in an earlier one, if any, so that scopes don't nest forever
            for (index, &node) in nodes.iter().enumerate() {
                if index > 0 && random.chance(60) {
                    graph.owners.insert(node, nodes[random.below(index)]);
                }
            }

            let edges = random.below(count * 3);

            for _ in 0..edges {
                let from = nodes[random.below(count)];
                let to = nodes[random.below(count)];
                graph.dependencies.entry(from).or_default().insert(to);
            }

            // The symbols being grouped are defined in the same scope, as in a run of let
            // statements
            let owner = graph.owners.get(&nodes[random.below(count)]).copied();

            let symbols: IndexSet<SymbolId> = nodes
                .iter()
                .copied()
                .filter(|node| graph.owners.get(node).copied() == owner && random.chance(70))
                .collect();

            let bound_after: HashSet<SymbolId> = symbols
                .iter()
                .copied()
                .filter(|_| random.chance(80))
                .collect();
            let bound_after = |symbol| bound_after.contains(&symbol);

            assert_eq!(
                graph.rounds_from_edges(&symbols, &bound_after, owner),
                graph.rounds_from_dependencies(&symbols, &bound_after),
                "{:?} owned by {owner:?}, grouping {symbols:?}",
                graph.dependencies,
            );
        }
    }
}
