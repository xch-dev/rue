#![allow(clippy::needless_pass_by_value)]

mod debug;
mod eager;
mod inline;
mod layout;
mod measure;
mod path_binding;
mod substitution;

pub use debug::*;

use std::{
    cell::{Cell, OnceCell, RefCell},
    collections::{HashMap, HashSet},
    iter,
    path::{Path, PathBuf},
    rc::Rc,
};

use id_arena::Arena;
use indexmap::{IndexSet, set::Slice};
use rue_diagnostic::SrcLoc;
use rue_lir::{Lir, LirId, bigint_atom};
use rue_options::CompilerOptions;

use crate::{
    BinaryOp, BindingSymbol, Block, ConstantSymbol, ControlStatement, Database, DependencyGraph,
    Environment, ExprStatement, FieldAccess, FieldKind, FunctionCall, FunctionKind, FunctionSymbol,
    Hir, HirId, IfStatement, PathError, Statement, Symbol, SymbolId, TypePath, UnaryOp,
    generate_check_hir,
};

use crate::analysis::{FunctionFailure, may_fail};

use layout::{Layout, Nesting, Reordered};

#[derive(Debug, Clone)]
enum SymbolGroup {
    Sequential(Vec<SymbolId>),
    Tree(Environment),
}

impl SymbolGroup {
    pub fn is_empty(&self) -> bool {
        match self {
            SymbolGroup::Sequential(symbols) => symbols.is_empty(),
            SymbolGroup::Tree(Environment::Nil) => true,
            SymbolGroup::Tree(_) => false,
        }
    }
}

/// Something that's being lowered, which is one level of nesting.
#[derive(Debug, Clone)]
enum Frame {
    Hir(HirId),
    /// A statement, which nests the rest of the block inside of it.
    Statement(SrcLoc),
}

/// The symbols that a symbol depends on, which are shared rather than copied each time they're
/// needed.
type Dependencies = Rc<IndexSet<SymbolId>>;

#[derive(Debug)]
pub struct Lowerer<'d, 'a, 'g> {
    db: &'d mut Database,
    arena: &'a mut Arena<Lir>,
    graph: &'g DependencyGraph,
    /// The parameters of each call that's being inlined, from the outermost to the innermost,
    /// with the argument that's substituted for each one, or `None` if its argument is bound in
    /// the environment or evaluated for its effect instead. A parameter is looked up in the
    /// innermost call that has it, since the same function can be inlined inside the argument
    /// of another call to it, where an outer call's argument isn't this call's.
    inline_symbols: Vec<HashMap<SymbolId, Option<HirId>>>,
    /// The functions whose calls are being inlined, from the outermost to the innermost, which
    /// keeps a lambda from being inlined into a call of a function it depends on (see
    /// [`Self::known_lambda`]).
    inline_functions: Vec<SymbolId>,
    options: CompilerOptions,
    main: SymbolId,
    base_path: PathBuf,
    /// What's currently being lowered, from the outermost to the innermost.
    path: Vec<Frame>,
    /// Whether lowering went deeper than [`MAX_LOWERING_DEPTH`].
    too_deep: Option<TooDeep>,
    /// The dependencies of each symbol, keyed by whether only captures are included. The graph
    /// doesn't change while lowering, and finding them walks it, so they're only found once per
    /// symbol rather than each time a symbol is inlined or captured.
    dependencies: RefCell<HashMap<(SymbolId, bool), Dependencies>>,
    /// Whether each function that's in a cycle calls itself in a way that keeps it from being
    /// inlined (see [`Self::is_recursive`]), which walks the graph, so it's only found once.
    recursive: RefCell<HashMap<SymbolId, bool>>,
    /// The round that each run of symbols grouped by value is bound in. A run of let statements is
    /// lowered again each time the function it's in is inlined, and finding the rounds walks
    /// everything the run can reach, so they're only found once per run.
    rounds: HashMap<Box<Slice<SymbolId>>, Rc<Vec<Option<usize>>>>,
    /// Whether each binding or constant that's referenced once can be evaluated where it's
    /// referenced, which isn't the case if it may fail and the reference isn't always evaluated,
    /// or if the reference can be evaluated more than once and the value isn't cheap.
    substitutions: RefCell<HashMap<SymbolId, bool>>,
    /// Whether each binding that's referenced more than once is evaluated where it's referenced
    /// anyway, since its value is only a path into the environment, for the let statements being
    /// lowered (see [`Self::lower_path_bindings`]).
    path_bindings: RefCell<HashMap<SymbolId, bool>>,
    /// Whether evaluating each such binding where it's referenced was found to be no larger than
    /// binding it, by the size of the path it's lowered to, which is only measured the first
    /// time its let statement is lowered with a path of that size.
    path_choices: HashMap<(SymbolId, usize), bool>,
    /// How many runs of let statements with path bindings are being measured around what's
    /// being lowered.
    path_run_depth: usize,
    /// Whether no binding is read as a path, while a run is lowered the way it was before there
    /// were path bindings.
    paths_disabled: bool,
    /// The functions that are only inlined if the program is measured to be smaller that way
    /// (see [`Self::is_measured_function`]), and were (see [`Self::lower_main`]).
    inlined_functions: HashSet<SymbolId>,
    /// Whether calling each function may fail, which is shared between checks.
    function_failures: RefCell<HashMap<SymbolId, FunctionFailure>>,
    /// Whether the value of each constant may fail, for the ones that can't (see
    /// [`crate::analysis::AnalysisContext::shares_substitution`]).
    substitution_failures: RefCell<HashMap<(SymbolId, bool), bool>>,
    /// What's always evaluated when the program is run, which constants are checked against.
    constant_roots: OnceCell<Vec<HirId>>,
    /// The parameters of an `inline fn` that each closure in it only captures so that their
    /// arguments are evaluated when the closure is built, rather than because it refers to them
    /// (see [`Self::function_captures`]). They're found along with the dependencies.
    argument_captures: RefCell<HashMap<SymbolId, IndexSet<SymbolId>>>,
    /// Whether a symbol wasn't found in the environment, while a value is lowered on its own to
    /// find out whether it's a constant (see [`Self::folds_to_constant`]), or `None` otherwise.
    missing_paths: Option<bool>,
    /// The bindings and constants that are substituted although their only reference can be
    /// evaluated more than once, since they're compiled to constants (see
    /// [`Self::decide_repeats`]).
    folded_repeats: HashSet<SymbolId>,
    /// The call of a loop that each binding is evaluated at, rather than bound before, or `None`
    /// if it's bound, while the run of let statements it's in is lowered, innermost last (see
    /// [`Self::loop_call_substitution`]).
    loop_calls: HashMap<SymbolId, Vec<Option<HirId>>>,
    /// What's recorded for a debug sidecar, if one was requested.
    debug: Option<LoweringDebugInfo>,
    /// How the environments of the program's functions are laid out (see [`layout`]).
    layout: Layout,
    /// The blocks that [`Layout::Nested`] lays out as subtrees, which are found before each way
    /// of lowering the program is tried, or `None` when the layout doesn't nest anything.
    nesting: Option<Nesting>,
    /// Whether lowering with [`Layout::Reached`] put a function's captures in another order than
    /// it found them in, or a function's parameters or a run of let statements in another order
    /// than the program reaches them in, so that [`Layout::Written`] or
    /// [`Layout::EverythingReached`] could lower the program differently.
    reordered: Cell<Reordered>,
}

/// What a block's statements are followed by.
#[derive(Debug, Clone)]
enum Tail {
    /// The block's body, which is its value, or nil if it doesn't have one.
    Body(Option<HirId>),
    /// The rest of the block that a [`ControlStatement`] is in, which a branch of the statement
    /// that falls through continues with.
    Continue(Rc<Continuation>),
    /// The chain of `if` nodes of a `match` statement, which comes after the statement that binds
    /// its value, followed by what follows the statement.
    Control(HirId, Rc<Tail>),
}

/// The rest of the block after a [`ControlStatement`], for the branches that fall through to it.
#[derive(Debug)]
enum Continuation {
    /// The rest of the block, which is lowered where the only branch that falls through ends.
    Inline(Vec<Statement>, Tail),
    /// The rest of the block, which is lowered once and quoted, and passed to the branches along
    /// with the environment it runs in, so that each branch that falls through can run it.
    Shared { program: SymbolId, env: SymbolId },
}

/// Lowering went deeper than [`MAX_LOWERING_DEPTH`], so the LIR it produced is incomplete.
#[derive(Debug, Clone)]
pub struct TooDeep {
    /// The innermost location that lowering was at when it went too deep, if anything it was
    /// lowering has one.
    pub srcloc: Option<SrcLoc>,
}

/// How deeply HIR can be nested while it's lowered, which bounds how deeply the generated code is
/// nested.
///
/// This is separate from the parser's nesting limit, since lowering can go much deeper than the
/// source code. The body of an inline function is lowered again at each call, inside of its
/// arguments, and a function, constant or binding that's only referenced once is lowered where
/// it's referenced, so a chain of them that each reference the next nests once per link. Each
/// statement in a block also nests the rest of the block inside of it.
///
/// Lowering grows the stack as it goes, as do the optimizer and code generation, so this doesn't
/// depend on the size of the thread's stack.
///
/// Real programs stay far below this.
pub const MAX_LOWERING_DEPTH: usize = 512;

/// The most bytes of atoms that a static argument can have and still be substituted for each
/// reference to its parameter, rather than bound (see [`Lowerer::is_trivial_argument`]). Binding a
/// value costs about this much more than quoting it again. The reads of a binding or constant
/// whose value is no larger aren't multiplied either (see `DependencyGraph::count_reads`).
pub(crate) const MAX_REPEATED_STATIC_SIZE: usize = 8;

impl<'d, 'a, 'g> Lowerer<'d, 'a, 'g> {
    pub fn new(
        db: &'d mut Database,
        arena: &'a mut Arena<Lir>,
        graph: &'g DependencyGraph,
        options: CompilerOptions,
        main: SymbolId,
        base_path: PathBuf,
    ) -> Self {
        Self {
            db,
            arena,
            graph,
            inline_symbols: Vec::new(),
            inline_functions: Vec::new(),
            options,
            main,
            base_path,
            path: Vec::new(),
            too_deep: None,
            dependencies: RefCell::default(),
            recursive: RefCell::default(),
            rounds: HashMap::new(),
            substitutions: RefCell::default(),
            path_bindings: RefCell::default(),
            path_choices: HashMap::new(),
            path_run_depth: 0,
            paths_disabled: false,
            inlined_functions: HashSet::new(),
            function_failures: RefCell::default(),
            substitution_failures: RefCell::default(),
            constant_roots: OnceCell::new(),
            argument_captures: RefCell::default(),
            missing_paths: None,
            folded_repeats: HashSet::new(),
            loop_calls: HashMap::new(),
            debug: None,
            layout: Layout::CANDIDATES[0],
            nesting: None,
            reordered: Cell::default(),
        }
    }

    /// Records where each LIR node comes from in the source, and which nodes are the bodies of
    /// functions, for a debug sidecar (see [`take_debug_info`](Self::take_debug_info)). This
    /// doesn't change the LIR that's generated.
    #[must_use]
    pub fn with_debug_info(mut self) -> Self {
        self.debug = Some(LoweringDebugInfo::default());
        self
    }

    /// What was recorded for a debug sidecar, if it was requested with
    /// [`with_debug_info`](Self::with_debug_info). Each node is tagged with the location of the
    /// innermost HIR that it was lowered for, if any.
    pub fn take_debug_info(&mut self) -> Option<LoweringDebugInfo> {
        let mut debug = self.debug.take()?;
        debug.scopes.apply(self.arena, 0, &mut debug.tags.spans);
        Some(debug)
    }

    /// The argument that's substituted for the symbol, if it's the parameter of a function that's
    /// being inlined, along with the index of the inlined call it's for.
    fn inline_substitution(&self, symbol: SymbolId) -> Option<(usize, HirId)> {
        self.inline_symbols
            .iter()
            .enumerate()
            .rev()
            .find_map(|(frame, symbols)| Some((frame, *symbols.get(&symbol)?)))
            .and_then(|(frame, argument)| Some((frame, argument?)))
    }

    /// Lowers an argument where it's substituted into a function that's inlined. It's the caller's
    /// code, so it's named after the caller, rather than the function that it's substituted into.
    fn lower_substitution(&mut self, env: &Environment, frame: usize, hir: HirId) -> LirId {
        let owner = self
            .debug
            .as_ref()
            .and_then(|debug| debug.inline_owners.get(frame).copied().flatten());

        let lir = self.with_owner(owner, |this| this.lower_hir(env, hir));

        if let (Some(debug), Some(owner)) = (&mut self.debug, owner) {
            debug.tags.labels.entry(lir).or_insert(owner);
        }

        lir
    }

    /// Lowers code that the owner (a label) is running, if debug info is being recorded.
    fn with_owner(&mut self, owner: Option<usize>, f: impl FnOnce(&mut Self) -> LirId) -> LirId {
        let Some(debug) = &mut self.debug else {
            return f(self);
        };

        debug.owners.push(owner);
        let lir = f(self);
        self.debug
            .as_mut()
            .expect("debug info is kept while lowering")
            .owners
            .pop();
        lir
    }

    /// Lowers something with the nodes it allocates tagged with the location, unless something
    /// inside of it has a location of its own.
    fn with_span(&mut self, srcloc: SrcLoc, f: impl FnOnce(&mut Self) -> LirId) -> LirId {
        let span = self.debug.as_mut().map(|debug| debug.span(srcloc));
        self.in_span(span, f)
    }

    /// Lowers something with the nodes it allocates tagged with the span, if there is one, unless
    /// something inside of it has a span of its own.
    fn in_span(&mut self, span: Option<usize>, f: impl FnOnce(&mut Self) -> LirId) -> LirId {
        let Some(span) = span else {
            return f(self);
        };

        let debug = self
            .debug
            .as_mut()
            .expect("spans are only found with debug info");
        debug.scopes.enter(self.arena.len(), span);
        let lir = f(self);
        let debug = self
            .debug
            .as_mut()
            .expect("debug info is kept while lowering");
        debug.scopes.exit(self.arena.len());
        lir
    }

    /// Whether the function calls itself in a way that keeps it from being inlined. An `inline fn`
    /// can't be in a cycle at all, and neither can the entrypoint, which the program is. Another
    /// function can be inlined if it only gets back to itself through a loop (see
    /// [`DependencyGraph::calls_itself_outside_loops`]).
    pub(super) fn is_recursive(&self, symbol: SymbolId, function: &FunctionSymbol) -> bool {
        if !self.dependencies(symbol, false).contains(&symbol) {
            return false;
        }

        if function.kind == FunctionKind::Inline || symbol == self.main {
            return true;
        }

        if let Some(&recursive) = self.recursive.borrow().get(&symbol) {
            return recursive;
        }

        let recursive = self.graph.calls_itself_outside_loops(symbol);
        self.recursive.borrow_mut().insert(symbol, recursive);
        recursive
    }

    /// The symbols that the symbol depends on, as given by [`DependencyGraph::dependencies`].
    fn dependencies(&self, symbol: SymbolId, captures_only: bool) -> Dependencies {
        let key = (symbol, captures_only);

        if let Some(dependencies) = self.dependencies.borrow().get(&key) {
            return dependencies.clone();
        }

        // A loop captures what it's found to need when the graph is built, which doesn't depend
        // on where it's called from (see `DependencyGraph::loop_captures`)
        let dependencies = match self.graph.loop_captures(symbol) {
            Some(captures) if captures_only => Rc::new(captures.clone()),
            _ if captures_only => Rc::new(self.function_captures(symbol)),
            _ => Rc::new(self.graph.dependencies(symbol, false)),
        };
        self.dependencies
            .borrow_mut()
            .insert(key, dependencies.clone());
        dependencies
    }

    /// What the function captures: what it depends on from outside of it, apart from what the
    /// values that are bound outside of it depend on (see [`DependencyGraph::captures`]).
    fn function_captures(&self, symbol: SymbolId) -> IndexSet<SymbolId> {
        // The entrypoint binds the constants, so it needs what their values depend on
        let mut captures = self.graph.captures(symbol, |capture| {
            self.is_always_bound(capture)
                && !(symbol == self.main && matches!(self.db.symbol(capture), Symbol::Constant(_)))
        });

        // What the function would capture if nothing were left out, which is only found if
        // something below needs it
        let mut unpruned = None;

        // Two kinds of function capture more than that, and whatever calls one passes it all of
        // what it captures. A loop is lowered once, with what it was found to capture when the
        // graph was built, which includes what the values it captures depend on. And the
        // entrypoint captures what its constants depend on. So a function that can reach either
        // one captures the same, as far as it would without leaving anything out, which is
        // what tells its own values apart from the ones of a call that it makes. A loop captures
        // the loops inside of it, so they're all found here
        let mut index = 0;

        while let Some(&capture) = captures.get_index(index) {
            index += 1;

            let is_entrypoint = capture == self.main && symbol != self.main;

            if !is_entrypoint && self.graph.loop_captures(capture).is_none() {
                continue;
            }

            let needed = self.dependencies(capture, true);
            let unpruned = unpruned.get_or_insert_with(|| self.graph.dependencies(symbol, true));

            captures.extend(
                needed
                    .iter()
                    .copied()
                    .filter(|needed| unpruned.contains(needed)),
            );
        }

        // What's added back is in the order it would have without anything left out, so that
        // the functions are grouped together the same way
        if let Some(unpruned) = &unpruned {
            captures = unpruned
                .iter()
                .copied()
                .filter(|capture| captures.contains(capture))
                .chain(
                    captures
                        .iter()
                        .copied()
                        .filter(|capture| !unpruned.contains(capture)),
                )
                .collect();
        }

        // The argument of an `inline fn` is evaluated wherever its parameter is referenced, and
        // a closure that captures the parameter evaluates the argument when it's built. So a
        // closure in an `inline fn` still captures the parameters that the values it captures
        // depend on. Otherwise an argument that may fail, and that's only evaluated some of the
        // time by what refers to it, would no longer be evaluated where the closure is built
        let inline_fn = |symbol: SymbolId| {
            matches!(
                self.db.symbol(symbol),
                Symbol::Function(function) if function.kind == FunctionKind::Inline
            )
        };

        let mut owners =
            iter::successors(self.graph.owner(symbol), |&owner| self.graph.owner(owner));

        if owners.any(inline_fn) {
            let unpruned = unpruned.get_or_insert_with(|| self.graph.dependencies(symbol, true));

            // The closure doesn't refer to these itself, so they're only captured where the
            // argument may fail (see `Self::is_idle_argument`)
            let arguments: IndexSet<SymbolId> = unpruned
                .iter()
                .copied()
                .filter(|&capture| {
                    matches!(self.db.symbol(capture), Symbol::Parameter(_))
                        && self.graph.owner(capture).is_some_and(inline_fn)
                        && !captures.contains(&capture)
                })
                .collect();

            captures.extend(arguments.iter().copied());

            self.argument_captures
                .borrow_mut()
                .insert(symbol, arguments);
        }

        captures
    }

    /// What the function captures where it's lowered here, which is the same for the environment
    /// its body is lowered with and the closure that's built for it: what it depends on, apart
    /// from the parameters it doesn't need here (see [`Self::is_idle_argument`]).
    fn lowered_captures(&self, symbol: SymbolId) -> IndexSet<SymbolId> {
        self.dependencies(symbol, true)
            .iter()
            .copied()
            .filter(|&capture| !self.is_idle_argument(symbol, capture))
            .collect()
    }

    /// Whether the function only captures the parameter of an `inline fn` so that its argument
    /// is evaluated when the closure is built (see [`Self::function_captures`]), and there's
    /// nothing to evaluate it for here: the argument can't fail, or the call isn't being
    /// inlined. The closure doesn't refer to the parameter, so it isn't captured then.
    fn is_idle_argument(&self, function: SymbolId, parameter: SymbolId) -> bool {
        self.argument_captures
            .borrow()
            .get(&function)
            .is_some_and(|arguments| arguments.contains(&parameter))
            && self
                .inline_argument(parameter)
                .is_none_or(|argument| !may_fail(self, argument))
    }

    pub fn base_path(&self) -> &Path {
        &self.base_path
    }

    /// Whether lowering went deeper than [`MAX_LOWERING_DEPTH`], in which case the LIR it
    /// produced is incomplete and must not be used.
    pub fn too_deep(&self) -> Option<&TooDeep> {
        self.too_deep.as_ref()
    }

    /// Lowers the value of a symbol in the environment. The entrypoint of a program that's
    /// compiled to be run on chain is lowered with [`lower_main`](Self::lower_main) instead,
    /// which also decides how the environments are laid out and which functions to inline by
    /// measuring.
    pub fn lower_symbol_value(&mut self, env: &Environment, symbol: SymbolId) -> LirId {
        if let Some((frame, hir)) = self.inline_substitution(symbol) {
            return self.lower_substitution(env, frame, hir);
        }

        match self.db.symbol(symbol).clone() {
            Symbol::Unresolved | Symbol::Module(_) | Symbol::Parameter(_) | Symbol::Builtin(_) => {
                unreachable!()
            }
            Symbol::Function(function) => self.lower_function(env, symbol, function),
            Symbol::Constant(constant) => self.lower_constant(env, constant),
            Symbol::Binding(binding) => self.lower_binding(env, binding),
        }
    }

    fn function_groups(
        &mut self,
        symbol: SymbolId,
        function: &FunctionSymbol,
    ) -> (Vec<SymbolGroup>, SymbolGroup) {
        let captures = self.ordered_captures(symbol);

        let capture_groups = self.group_symbols(
            captures,
            symbol != self.main,
            !self.graph.is_closure(symbol),
            self.graph.is_loop(symbol),
        );

        let param_group = self.create_group(
            function.parameters.clone(),
            matches!(function.kind, FunctionKind::BinaryTree | FunctionKind::Loop)
                && !self.graph.is_closure(symbol)
                && symbol != self.main,
        );

        (capture_groups, param_group)
    }

    fn lower_function(
        &mut self,
        parent_env: &Environment,
        symbol: SymbolId,
        function: FunctionSymbol,
    ) -> LirId {
        let (capture_groups, param_group) = self.function_groups(symbol, &function);

        let mut function_env = Self::apply_group(
            Environment::Nil,
            &param_group,
            function.nil_terminated && matches!(param_group, SymbolGroup::Sequential(_)),
        );

        for group in &capture_groups {
            function_env = Self::apply_group(function_env, group, true);
        }

        // Inline arguments are lowered in the caller's environment, so the function body
        // can only reach them through its captures, unless they are static values. The ones for
        // the function's own parameters are from a call to it that's inlined around this one, so
        // they aren't its parameters' values here. Those of functions defined in it are left out
        // too, so that nothing inside the closure sees an inlined call of code from outside it.
        let static_arguments = self
            .inline_symbols
            .iter()
            .flat_map(HashMap::keys)
            .filter(|&&parameter| !self.graph.is_defined_in(parameter, symbol))
            .filter_map(|&symbol| {
                let hir = self.inline_argument(symbol)?;
                self.is_static_hir(hir).then_some((symbol, Some(hir)))
            })
            .collect();
        let inline_symbols = std::mem::replace(&mut self.inline_symbols, vec![static_arguments]);

        // The body is the function's own code, and the static arguments are only values
        let owner = self.debug.as_mut().map(|debug| {
            let name = match &function.name {
                Some(name) => name.text().to_string(),
                None => "<lambda>".to_string(),
            };
            let owner = debug.label(name);
            let inline_owners = std::mem::replace(&mut debug.inline_owners, vec![None]);
            (owner, inline_owners)
        });

        let mut expr = self.with_owner(owner.as_ref().map(|(owner, _)| *owner), |this| {
            this.lower_hir(&function_env, function.body)
        });

        if let (Some(debug), Some((_, inline_owners))) = (&mut self.debug, owner) {
            debug.inline_owners = inline_owners;
        }

        self.inline_symbols = inline_symbols;

        if let Some(debug) = &mut self.debug {
            debug.add_parameter_names(self.db, &function);
            let index = debug.function(symbol, &function_env);
            // If the body is the body of a function inside of it, the innermost one is kept, as
            // with locations
            debug.tags.functions.entry(expr).or_insert(index);
        }

        if symbol == self.main && function.kind == FunctionKind::External {
            let entire_env = self.arena.alloc(Lir::Path(1u8.into()));
            let wrapper = self.arena.alloc(Lir::Run(expr, entire_env));
            self.tag_main_wrapper(wrapper, function.body);
            return wrapper;
        }

        if symbol == self.main {
            let body = expr;
            let mut map = HashMap::new();

            if self.dependencies(symbol, true).contains(&symbol) {
                map.insert(symbol, self.arena.alloc(Lir::Quote(expr)));
                let reference = self.lower_symbol_reference(&function_env, symbol);
                let entire_env = self.arena.alloc(Lir::Path(1u8.into()));
                expr = self.arena.alloc(Lir::Run(reference, entire_env));
            }

            for (i, group) in capture_groups.iter().enumerate().rev() {
                expr = self.arena.alloc(Lir::Quote(expr));

                let mut bind_env = parent_env.clone();

                for existing_group in capture_groups.iter().take(i) {
                    bind_env = Self::apply_group(bind_env, existing_group, true);
                }

                let rest = if i == 0 && param_group.is_empty() {
                    self.arena.alloc(Lir::Atom(vec![]))
                } else {
                    self.arena.alloc(Lir::Path(1u8.into()))
                };

                let group_env =
                    self.lower_group_environment(&bind_env, group, rest, false, Some(&map), true);

                expr = self.arena.alloc(Lir::Run(expr, group_env));
            }

            if expr != body {
                self.tag_main_wrapper(expr, function.body);
            }

            expr
        } else if function.kind == FunctionKind::External {
            expr
        } else {
            self.arena.alloc(Lir::Quote(expr))
        }
    }

    /// Tags the setup that binds `main`'s captures with the location of its body, since nothing
    /// else would. It's evaluated with the environment that `main` is called with, rather than
    /// the one its body has once its captures are bound, so it isn't tagged as the function.
    fn tag_main_wrapper(&mut self, wrapper: LirId, body: HirId) {
        if let Some(debug) = &mut self.debug
            && let Some(span) = debug.hir_span(self.db, body)
        {
            debug.tags.spans.entry(wrapper).or_insert(span);
        }
    }

    fn lower_constant(&mut self, env: &Environment, constant: ConstantSymbol) -> LirId {
        self.lower_hir(env, constant.value.hir)
    }

    fn lower_binding(&mut self, env: &Environment, binding: BindingSymbol) -> LirId {
        self.lower_hir(env, binding.value.hir)
    }

    fn lower_hir(&mut self, env: &Environment, hir: HirId) -> LirId {
        self.nested(Frame::Hir(hir), |this| this.lower_hir_inner(env, hir))
    }

    /// Lowers something one level deeper, unless that's deeper than [`MAX_LOWERING_DEPTH`], in
    /// which case it records where and returns a placeholder instead.
    fn nested(&mut self, frame: Frame, f: impl FnOnce(&mut Self) -> LirId) -> LirId {
        // A value that needs the environment isn't a constant, whatever the rest of it is
        if self.too_deep.is_some() || self.missing_paths == Some(true) {
            return self.arena.alloc(Lir::Atom(vec![]));
        }

        if self.path.len() >= MAX_LOWERING_DEPTH {
            let srcloc = iter::once(&frame)
                .chain(self.path.iter().rev())
                .find_map(|frame| match frame {
                    Frame::Hir(hir) => self.db.hir_srcloc(*hir),
                    Frame::Statement(srcloc) => Some(srcloc),
                })
                .cloned();

            self.too_deep = Some(TooDeep { srcloc });
            return self.arena.alloc(Lir::Atom(vec![]));
        }

        // The nodes allocated for HIR are tagged with its location, unless the HIR inside of it
        // has one of its own
        let span = match (&mut self.debug, &frame) {
            (Some(debug), Frame::Hir(hir)) => debug.hir_span(self.db, *hir),
            _ => None,
        };

        self.in_span(span, |this| {
            this.path.push(frame);
            // Every cycle of recursion in lowering passes through here, and each level can use a
            // lot of stack in a debug build, so grow it rather than rely on the thread having
            // enough
            let lir = rue_types::grow_stack(|| f(this));
            this.path.pop();
            lir
        })
    }

    fn lower_hir_inner(&mut self, env: &Environment, hir: HirId) -> LirId {
        match self.db.hir(hir).clone() {
            Hir::Unresolved => unreachable!(),
            Hir::Nil => self.arena.alloc(Lir::Atom(vec![])),
            Hir::String(value) => self.arena.alloc(Lir::Atom(value.as_bytes().to_vec())),
            Hir::Int(value) => self.arena.alloc(Lir::Atom(bigint_atom(value.clone()))),
            Hir::Bool(value) => self
                .arena
                .alloc(Lir::Atom(if value { vec![1] } else { vec![] })),
            Hir::Bytes(atom) => self.arena.alloc(Lir::Atom(atom)),
            Hir::Pair(first, rest) => {
                let first = self.lower_hir(env, first);
                let rest = self.lower_hir(env, rest);
                self.arena.alloc(Lir::Cons(first, rest))
            }
            Hir::List(list) => self.lower_list(env, list.items, list.nil_terminated),
            Hir::Struct(initializer) => {
                let values = initializer.values().collect();
                self.lower_list(env, values, initializer.values_nil_terminated())
            }
            Hir::FieldAccess(access) => {
                let mut lir = self.lower_hir(env, access.value);

                match access.kind {
                    FieldKind::Path(path) => {
                        for path in path {
                            lir = match path {
                                TypePath::First => self.arena.alloc(Lir::First(lir)),
                                TypePath::Rest => self.arena.alloc(Lir::Rest(lir)),
                            };
                        }
                    }
                    FieldKind::Length => lir = self.arena.alloc(Lir::Strlen(lir)),
                }

                lir
            }
            Hir::Cast(value) => self.lower_hir(env, value),
            Hir::Guard(guard) => {
                let check = generate_check_hir(self.db, guard.runtime_check(), guard.value);
                let result = self.lower_hir(env, check);

                // A check that doesn't use the value is a constant, but the value is still
                // evaluated for its effect if it may fail, like a binding that's never referenced
                if self.is_lowered_guard_effect(hir, guard.value) {
                    let value = self.lower_hir(env, guard.value);
                    self.effect(value, result)
                } else {
                    result
                }
            }
            Hir::Const(expr) => {
                let result = expr
                    .result
                    .expect("unevaluated const expression reached lowering");
                self.lower_hir(env, result)
            }
            Hir::Reference(symbol) => self.lower_symbol(env, symbol, false),
            Hir::Block(block) => self.lower_block(env, block.statements, Tail::Body(block.body)),
            Hir::Lambda(lambda) => self.lower_symbol(env, lambda, true),
            Hir::If(condition, then, else_, inline) => {
                let condition = self.lower_hir(env, condition);
                let then = self.lower_hir(env, then);
                let else_ = self.lower_hir(env, else_);
                self.arena.alloc(Lir::If(condition, then, else_, inline))
            }
            Hir::FunctionCall(call) => self.lower_function_call(env, hir, call),
            Hir::Unary(op, hir) => {
                let lir = self.lower_hir(env, hir);
                match op {
                    UnaryOp::Listp => self.arena.alloc(Lir::Listp(lir)),
                    UnaryOp::First => self.arena.alloc(Lir::First(lir)),
                    UnaryOp::Rest => self.arena.alloc(Lir::Rest(lir)),
                    UnaryOp::Strlen => self.arena.alloc(Lir::Strlen(lir)),
                    UnaryOp::Not => self.arena.alloc(Lir::Not(lir)),
                    UnaryOp::Plus => lir,
                    UnaryOp::Neg => {
                        let zero = self.arena.alloc(Lir::Atom(vec![]));
                        self.arena.alloc(Lir::Sub(vec![zero, lir]))
                    }
                    UnaryOp::BitwiseNot => self.arena.alloc(Lir::Lognot(lir)),
                    UnaryOp::G1Negate => self.arena.alloc(Lir::G1Negate(lir)),
                    UnaryOp::G2Negate => self.arena.alloc(Lir::G2Negate(lir)),
                    UnaryOp::Sha256 => self.arena.alloc(Lir::Sha256(vec![lir])),
                    UnaryOp::Sha256Inline => self.arena.alloc(Lir::Sha256Inline(vec![lir])),
                    UnaryOp::Keccak256 => self.arena.alloc(Lir::Keccak256(vec![lir])),
                    UnaryOp::Keccak256Inline => self.arena.alloc(Lir::Keccak256Inline(vec![lir])),
                    UnaryOp::PubkeyForExp => self.arena.alloc(Lir::PubkeyForExp(lir)),
                }
            }
            Hir::Binary(op, left_hir, right_hir) => {
                let left = self.lower_hir(env, left_hir);
                let right = self.lower_hir(env, right_hir);
                match op {
                    BinaryOp::Add => self.arena.alloc(Lir::Add(vec![left, right])),
                    BinaryOp::Sub => self.arena.alloc(Lir::Sub(vec![left, right])),
                    BinaryOp::Mul => self.arena.alloc(Lir::Mul(vec![left, right])),
                    BinaryOp::Div => self.arena.alloc(Lir::Div(left, right)),
                    BinaryOp::Mod => self.arena.alloc(Lir::Mod(left, right)),
                    BinaryOp::Divmod => self.arena.alloc(Lir::Divmod(left, right)),
                    BinaryOp::Concat => self.arena.alloc(Lir::Concat(vec![left, right])),
                    BinaryOp::G1Add => self.arena.alloc(Lir::G1Add(vec![left, right])),
                    BinaryOp::G1Subtract => self.arena.alloc(Lir::G1Subtract(vec![left, right])),
                    BinaryOp::G1Multiply => self.arena.alloc(Lir::G1Multiply(left, right)),
                    BinaryOp::G2Add => self.arena.alloc(Lir::G2Add(vec![left, right])),
                    BinaryOp::G2Subtract => self.arena.alloc(Lir::G2Subtract(vec![left, right])),
                    BinaryOp::G2Multiply => self.arena.alloc(Lir::G2Multiply(left, right)),
                    BinaryOp::BitwiseAnd => self.arena.alloc(Lir::Logand(vec![left, right])),
                    BinaryOp::BitwiseOr => self.arena.alloc(Lir::Logior(vec![left, right])),
                    BinaryOp::BitwiseXor => self.arena.alloc(Lir::Logxor(vec![left, right])),
                    BinaryOp::RightLogicalShift => {
                        let zero = self.arena.alloc(Lir::Atom(vec![]));
                        let neg = self.arena.alloc(Lir::Sub(vec![zero, right]));
                        self.arena.alloc(Lir::Lsh(left, neg))
                    }
                    BinaryOp::RightArithmeticShift => {
                        let zero = self.arena.alloc(Lir::Atom(vec![]));
                        let neg = self.arena.alloc(Lir::Sub(vec![zero, right]));
                        self.arena.alloc(Lir::Ash(left, neg))
                    }
                    BinaryOp::LeftArithmeticShift => self.arena.alloc(Lir::Ash(left, right)),
                    BinaryOp::Gt => self.arena.alloc(Lir::Gt(left, right)),
                    BinaryOp::Lt => self.arena.alloc(Lir::Gt(right, left)),
                    BinaryOp::Gte => {
                        let lt = self.arena.alloc(Lir::Gt(right, left));
                        self.arena.alloc(Lir::Not(lt))
                    }
                    BinaryOp::Lte => {
                        let gt = self.arena.alloc(Lir::Gt(left, right));
                        self.arena.alloc(Lir::Not(gt))
                    }
                    BinaryOp::GtBytes => self.arena.alloc(Lir::GtBytes(left, right)),
                    BinaryOp::LtBytes => self.arena.alloc(Lir::GtBytes(right, left)),
                    BinaryOp::GteBytes => {
                        let lt = self.arena.alloc(Lir::GtBytes(right, left));
                        self.arena.alloc(Lir::Not(lt))
                    }
                    BinaryOp::LteBytes => {
                        let gt = self.arena.alloc(Lir::GtBytes(left, right));
                        self.arena.alloc(Lir::Not(gt))
                    }
                    BinaryOp::Eq => self.arena.alloc(Lir::Eq(left, right)),
                    BinaryOp::Ne => {
                        let eq = self.arena.alloc(Lir::Eq(left, right));
                        self.arena.alloc(Lir::Not(eq))
                    }
                    // The right operand is a `Bool`, so it's the result as it is, without
                    // being turned into `1` or `()` again. It's only evaluated if the left
                    // operand doesn't decide the result, unless evaluating it either way is
                    // smaller and can't be told apart (see [`Self::is_eager_operand`]).
                    BinaryOp::And => {
                        let false_atom = self.arena.alloc(Lir::Atom(vec![]));
                        let eager = self.is_eager_operand(hir, left_hir, right_hir);
                        self.arena.alloc(Lir::If(left, right, false_atom, eager))
                    }
                    BinaryOp::Or => {
                        let true_atom = self.arena.alloc(Lir::Atom(vec![1]));
                        let eager = self.is_eager_operand(hir, left_hir, right_hir);
                        self.arena.alloc(Lir::If(left, true_atom, right, eager))
                    }
                    BinaryOp::All => self.arena.alloc(Lir::All(vec![left, right])),
                    BinaryOp::Any => self.arena.alloc(Lir::Any(vec![left, right])),
                }
            }
            Hir::CoinId(parent, puzzle, amount) => {
                let parent = self.lower_hir(env, parent);
                let puzzle = self.lower_hir(env, puzzle);
                let amount = self.lower_hir(env, amount);
                self.arena.alloc(Lir::CoinId(parent, puzzle, amount))
            }
            Hir::Substr(hir, start, end) => {
                let hir = self.lower_hir(env, hir);
                let start = self.lower_hir(env, start);
                let end = end.map(|end| self.lower_hir(env, end));
                self.arena.alloc(Lir::Substr(hir, start, end))
            }
            Hir::G1Map(data, dst) => {
                let data = self.lower_hir(env, data);
                let dst = dst.map(|dst| self.lower_hir(env, dst));
                self.arena.alloc(Lir::G1Map(data, dst))
            }
            Hir::G2Map(data, dst) => {
                let data = self.lower_hir(env, data);
                let dst = dst.map(|dst| self.lower_hir(env, dst));
                self.arena.alloc(Lir::G2Map(data, dst))
            }
            Hir::Modpow(base, exponent, modulus) => {
                let base = self.lower_hir(env, base);
                let exponent = self.lower_hir(env, exponent);
                let modulus = self.lower_hir(env, modulus);
                self.arena.alloc(Lir::Modpow(base, exponent, modulus))
            }
            Hir::BlsPairingIdentity(args) => {
                let args = args
                    .into_iter()
                    .map(|arg| self.lower_hir(env, arg))
                    .collect();
                self.arena.alloc(Lir::BlsPairingIdentity(args))
            }
            Hir::BlsVerify(sig, args) => {
                let sig = self.lower_hir(env, sig);
                let args = args
                    .into_iter()
                    .map(|arg| self.lower_hir(env, arg))
                    .collect();
                self.arena.alloc(Lir::BlsVerify(sig, args))
            }
            Hir::Secp256K1Verify(sig, pk, msg) => {
                let sig = self.lower_hir(env, sig);
                let pk = self.lower_hir(env, pk);
                let msg = self.lower_hir(env, msg);
                self.arena.alloc(Lir::K1Verify(sig, pk, msg))
            }
            Hir::Secp256R1Verify(sig, pk, msg) => {
                let sig = self.lower_hir(env, sig);
                let pk = self.lower_hir(env, pk);
                let msg = self.lower_hir(env, msg);
                self.arena.alloc(Lir::R1Verify(sig, pk, msg))
            }
            Hir::Keccak256Verify(value, hash) => {
                let value = self.lower_hir(env, value);
                let hash = self.lower_hir(env, hash);
                self.arena.alloc(Lir::Keccak256Verify(value, hash))
            }
            Hir::InfinityG1 => self.arena.alloc(Lir::G1Add(vec![])),
            Hir::InfinityG2 => self.arena.alloc(Lir::G2Add(vec![])),
            Hir::ClvmOp(op, args) => {
                let args = self.lower_hir(env, args);
                self.arena.alloc(Lir::Op(op, args))
            }
        }
    }

    /// Lowers the items of a list (or the fields of a struct) to nested pairs. The items are
    /// lowered in order, and if the list isn't nil-terminated, the last item is the end of it.
    fn lower_list(&mut self, env: &Environment, items: Vec<HirId>, nil_terminated: bool) -> LirId {
        let mut items = items
            .into_iter()
            .map(|item| self.lower_hir(env, item))
            .collect::<Vec<_>>();

        let mut lir = if nil_terminated {
            self.arena.alloc(Lir::Atom(vec![]))
        } else if let Some(last) = items.pop() {
            last
        } else {
            unreachable!("a list that isn't nil-terminated ends in a spread item")
        };

        for item in items.into_iter().rev() {
            lir = self.arena.alloc(Lir::Cons(item, lir));
        }

        lir
    }

    fn lower_function_call(&mut self, env: &Environment, hir: HirId, call: FunctionCall) -> LirId {
        if let Some(callee) = self.inlined_callee(call.function)
            && let Symbol::Function(function) = self.db.symbol(callee).clone()
        {
            let args = self.call_arguments(&function, call);
            return self.lower_inline_call(env, callee, &function, args);
        }

        if let Hir::Reference(symbol) = self.db.hir(self.db.without_casts(call.function)).clone()
            && let Symbol::Function(function) = self.db.symbol(symbol).clone()
        {
            let args = self.call_arguments(&function, call);

            let (capture_groups, param_group) = self.function_groups(symbol, &function);

            let function_lir = self.lower_symbol_reference(env, symbol);

            let rest = self.arena.alloc(Lir::Atom(vec![]));

            // The arguments are lowered in the order of the parameters, rather than the map's,
            // which changes from run to run, since it decides where lowering first goes too deep
            let mut lir_args = HashMap::new();

            for parameter in &function.parameters {
                if let Some(&hir) = args.get(parameter) {
                    lir_args.insert(*parameter, self.lower_hir(env, hir));
                }
            }

            let mut arg_env = self.lower_group_environment(
                env,
                &param_group,
                rest,
                false,
                Some(&lir_args),
                function.nil_terminated && matches!(param_group, SymbolGroup::Sequential(_)),
            );

            // A `for` expression's call of a loop evaluates the values that are only bound for
            // the loop there, rather than reading them from where they're bound (see
            // `Self::loop_call_substitution`)
            let mut substituted = HashMap::new();

            if function.kind == FunctionKind::Loop {
                for capture in self.lowered_captures(symbol) {
                    if self
                        .loop_calls
                        .get(&capture)
                        .and_then(|calls| calls.last())
                        .is_some_and(|&call| call == Some(hir))
                    {
                        let value = self.lower_symbol_value(env, capture);
                        substituted.insert(capture, value);
                    }
                }
            }

            for group in &capture_groups {
                arg_env = self.lower_group_environment(
                    env,
                    group,
                    arg_env,
                    true,
                    Some(&substituted),
                    true,
                );
            }

            return self.arena.alloc(Lir::Run(function_lir, arg_env));
        }

        let function = self.lower_hir(env, call.function);

        let mut args = Vec::new();

        for arg in call.args {
            args.push(self.lower_hir(env, arg));
        }

        let mut env = self.arena.alloc(Lir::Atom(Vec::new()));

        for (i, &arg) in args.iter().rev().enumerate() {
            if i == 0 && !call.nil_terminated {
                env = arg;
            } else {
                env = self.arena.alloc(Lir::Cons(arg, env));
            }
        }

        self.arena.alloc(Lir::Run(function, env))
    }

    /// The argument for each of the function's parameters. Type checking ensures that the call
    /// passes what the parameters take, and the arguments for a spread parameter are put in a
    /// list.
    fn call_arguments(
        &mut self,
        function: &FunctionSymbol,
        call: FunctionCall,
    ) -> HashMap<SymbolId, HirId> {
        let mut args = HashMap::new();
        let mut arg_iter = call.args.into_iter();

        for (&parameter, arg) in function.fixed_parameters().iter().zip(&mut arg_iter) {
            args.insert(parameter, arg);
        }

        if !function.nil_terminated {
            let mut last_arg = self.db.alloc_hir(Hir::Nil);

            for (i, arg) in arg_iter.rev().enumerate() {
                if i == 0 && !call.nil_terminated {
                    last_arg = arg;
                } else {
                    last_arg = self.db.alloc_hir(Hir::Pair(arg, last_arg));
                }
            }

            if let Some(last_param) = function.parameters.last() {
                args.insert(*last_param, last_arg);
            }
        }

        args
    }

    /// The function that a call to the callee is inlined into: a function called by name that's
    /// inlined (see [`Self::should_inline`]), or a lambda that's known where it's called (see
    /// [`Self::known_lambda`]), rather than building the closure and running it.
    fn inlined_callee(&self, callee: HirId) -> Option<SymbolId> {
        if let Hir::Reference(symbol) = self.db.hir(self.db.without_casts(callee))
            && let Symbol::Function(_) = self.db.symbol(*symbol)
        {
            return self.should_inline(*symbol).then_some(*symbol);
        }

        self.known_lambda(callee)
    }

    /// The lambda that the callee is, if it's a lambda expression, either directly or through
    /// bindings that are inlined and the arguments of calls that are inlined, and its body can be
    /// lowered where it's called. Everything it captures is in scope there, since the arguments
    /// of inlined calls are lowered in the caller's environment, and a function that's lowered
    /// within an inlined call only keeps the arguments that are static (see
    /// [`Self::lower_function`]).
    ///
    /// Parameters are substituted with the innermost argument for them, so a function must not be
    /// inlined inside a call to itself while code written in the outer call can still be lowered,
    /// since the inner call's parameters would hide the outer call's from that code. Calls by name
    /// only nest through the arguments of the outer call, which are the caller's code, as in
    /// `twice(f, twice(f, x))`, where the inner call's frame has all of its own parameters (see
    /// `inline_symbols`). But a lambda's code can be reached from inside a call it makes:
    /// the lambda can call a function whose call it was passed to, or pass a lambda it creates to
    /// an argument that calls the same lambda again. So a lambda isn't inlined inside a call of
    /// itself or of a function it depends on, and is built as a closure instead.
    ///
    /// Like the automatic inlining of functions and bindings, this is only done when
    /// `auto_inline` is enabled.
    fn known_lambda(&self, mut hir: HirId) -> Option<SymbolId> {
        if !self.options.auto_inline {
            return None;
        }

        let mut seen = HashSet::new();

        let lambda = loop {
            match self.db.hir(self.db.without_casts(hir)) {
                Hir::Lambda(symbol) => break *symbol,
                Hir::Reference(symbol) if seen.insert(*symbol) => {
                    if let Some(arg) = self.inline_argument(*symbol) {
                        hir = arg;
                    } else if let Symbol::Binding(binding) = self.db.symbol(*symbol)
                        && self.should_inline(*symbol)
                    {
                        hir = binding.value.hir;
                    } else {
                        return None;
                    }
                }
                _ => return None,
            }
        };

        if self.inline_functions.is_empty() {
            return Some(lambda);
        }

        let dependencies = self.dependencies(lambda, false);

        let reenters = self
            .inline_functions
            .iter()
            .any(|function| *function == lambda || dependencies.contains(function));

        (!reenters).then_some(lambda)
    }

    /// Whether the argument is no more expensive to evaluate at each reference than to bind:
    /// it's a value that's in the environment, a path to a field of one (through any arguments
    /// and bindings that are substituted), or a small static value.
    fn is_trivial_argument(&self, hir: HirId) -> bool {
        if let Hir::Reference(symbol) = self.db.hir(self.db.without_casts(hir))
            && !self.should_inline(*symbol)
        {
            return true;
        }

        if self.is_path(hir) {
            return true;
        }

        let mut budget = MAX_REPEATED_STATIC_SIZE;
        self.is_small_static(hir, &mut budget, &mut HashSet::new(), &mut Vec::new())
    }

    /// Whether the value is static and its atoms take up no more than the budget, which is
    /// reduced by their size. Such a value is quoted where it's referenced, or folded with what
    /// uses it. This follows the values that are substituted (see [`Self::substituted_value`]),
    /// fields of static values, and calls that are inlined (see [`Self::inlined_callee`]) whose
    /// arguments are static and whose body is static with them, such as `fn() => (1, 2)` or
    /// `unchecked_cast((1, 2))`. Each argument of such a call is checked once, in the caller's
    /// context, and its size is kept in `substitutions` and counted at each reference to its
    /// parameter in the body. The symbols that are being followed are kept in `path`, so that a
    /// value is never followed back into itself. This can't happen today, since bindings don't
    /// refer to themselves and functions that call themselves aren't inlined, but it keeps the
    /// walk finite if that changes.
    fn is_small_static(
        &self,
        hir: HirId,
        budget: &mut usize,
        path: &mut HashSet<SymbolId>,
        substitutions: &mut Vec<(SymbolId, usize)>,
    ) -> bool {
        let mut take = |size: usize| match budget.checked_sub(size) {
            Some(rest) => {
                *budget = rest;
                true
            }
            None => false,
        };

        match self.db.hir(hir) {
            Hir::Nil | Hir::Bool(_) => take(1),
            Hir::Int(value) => take(value.bits().div_ceil(8) as usize + 1),
            Hir::Bytes(bytes) => take(bytes.len() + 1),
            Hir::String(string) => take(string.len() + 1),
            Hir::Pair(first, rest) => {
                self.is_small_static(*first, budget, path, substitutions)
                    && self.is_small_static(*rest, budget, path, substitutions)
            }
            Hir::List(list) => list
                .items
                .iter()
                .all(|&item| self.is_small_static(item, budget, path, substitutions)),
            Hir::Struct(initializer) => initializer
                .values()
                .all(|value| self.is_small_static(value, budget, path, substitutions)),
            Hir::Unary(UnaryOp::Plus, hir) | Hir::Cast(hir) => {
                self.is_small_static(*hir, budget, path, substitutions)
            }
            // A field of a static value is folded, and is no larger than the value
            Hir::FieldAccess(FieldAccess {
                value,
                kind: FieldKind::Path(_),
                ..
            }) => self.is_small_static(*value, budget, path, substitutions),
            Hir::Const(expr) => expr
                .result
                .is_some_and(|result| self.is_small_static(result, budget, path, substitutions)),
            Hir::Guard(_) => self.is_static_hir(hir) && take(1),
            Hir::Block(Block {
                statements,
                body: Some(body),
            }) if statements.is_empty() => self.is_small_static(*body, budget, path, substitutions),
            Hir::Reference(symbol) => {
                if let Some(&(_, size)) = substitutions
                    .iter()
                    .rev()
                    .find(|(parameter, _)| parameter == symbol)
                {
                    return take(size);
                }

                if !path.insert(*symbol) {
                    return false;
                }

                let small = self
                    .substituted_value(*symbol)
                    .is_some_and(|value| self.is_small_static(value, budget, path, substitutions));
                path.remove(symbol);
                small
            }
            Hir::FunctionCall(call) => {
                let Some(callee) = self.inlined_callee(call.function) else {
                    return false;
                };
                let Symbol::Function(function) = self.db.symbol(callee) else {
                    return false;
                };

                // Only calls that pass each parameter its own argument
                if !function.nil_terminated
                    || !call.nil_terminated
                    || function.parameters.len() != call.args.len()
                {
                    return false;
                }

                let mut sizes = Vec::with_capacity(call.args.len());

                for &arg in &call.args {
                    let mut remaining = *budget;

                    if !self.is_small_static(arg, &mut remaining, path, substitutions) {
                        return false;
                    }

                    sizes.push(*budget - remaining);
                }

                let frame = substitutions.len();
                substitutions.extend(function.parameters.iter().copied().zip(sizes));
                let small = self.is_small_static(function.body, budget, path, substitutions);
                substitutions.truncate(frame);
                small
            }
            _ => false,
        }
    }

    /// Lowers a call to a function that's inlined, by substituting the arguments for its
    /// parameters.
    ///
    /// The arguments of an `inline fn` are always substituted, so they're evaluated where they're
    /// used. Otherwise, an argument that may fail is only substituted if the body always
    /// evaluates its parameter. If the parameter is never referenced, the argument is evaluated
    /// for its effect instead, and if it's only referenced some of the time, it's bound before
    /// the body like a let statement, so that it's still evaluated at the call. An argument whose
    /// parameter is referenced more than once is bound too, so that it's only evaluated once,
    /// unless it's as cheap to evaluate each time (see [`Self::is_trivial_argument`]).
    fn lower_inline_call(
        &mut self,
        env: &Environment,
        symbol: SymbolId,
        function: &FunctionSymbol,
        args: HashMap<SymbolId, HirId>,
    ) -> LirId {
        let mut effects = Vec::new();
        let mut bound = Vec::new();
        let mut args: HashMap<SymbolId, Option<HirId>> = args
            .into_iter()
            .map(|(parameter, arg)| (parameter, Some(arg)))
            .collect();

        if function.kind != FunctionKind::Inline {
            for &parameter in &function.parameters {
                let Some(&Some(arg)) = args.get(&parameter) else {
                    continue;
                };

                if !self.binds_inline_argument(
                    function,
                    parameter,
                    || self.is_trivial_argument(arg),
                    || may_fail(self, arg),
                ) {
                    continue;
                }

                // The parameter stays in the call's frame, so that it isn't taken for the
                // parameter of an enclosing call to the same function
                args.insert(parameter, None);

                if self.graph.references(parameter) == 0 {
                    effects.push(arg);
                } else {
                    bound.push((parameter, arg));
                }
            }
        }

        if let Some(debug) = &mut self.debug {
            debug.add_parameter_names(self.db, function);
        }

        let group = SymbolGroup::Sequential(bound.iter().map(|&(symbol, _)| symbol).collect());
        let body_env = Self::apply_group(env.clone(), &group, true);

        // The body has no frame of its own, so it's named after the function, which tells a
        // debugger that it's running that function. The arguments are the caller's code, so
        // they're named after the caller where they're substituted (see `lower_substitution`).
        let owner = self.debug.as_mut().map(|debug| {
            let caller = debug.owner();
            debug.inline_owners.push(caller);
            let name = function
                .name
                .as_ref()
                .map_or("<lambda>", |name| name.text());
            debug.label(format!("{name} (inlined)"))
        });

        self.inline_symbols.push(args);
        self.inline_functions.push(symbol);
        let mut result = self.with_owner(owner, |this| this.lower_hir(&body_env, function.body));
        self.inline_functions.pop().unwrap();
        self.inline_symbols.pop().unwrap();

        if let Some(debug) = &mut self.debug {
            debug.inline_owners.pop();

            if let Some(owner) = owner {
                debug.tags.labels.entry(result).or_insert(owner);
            }
        }

        // The arguments that are referenced some of the time are bound like a let statement
        if !bound.is_empty() {
            let bound_args = bound
                .iter()
                .map(|&(parameter, arg)| (parameter, self.lower_hir(env, arg)))
                .collect();

            let body = self.arena.alloc(Lir::Quote(result));
            let rest = self.arena.alloc(Lir::Path(1u8.into()));
            let group_env =
                self.lower_group_environment(env, &group, rest, false, Some(&bound_args), true);

            result = self.arena.alloc(Lir::Run(body, group_env));
        }

        for arg in effects.into_iter().rev() {
            let value = self.lower_hir(env, arg);
            result = self.effect(value, result);
        }

        result
    }

    /// Evaluates the value for its effect, before returning the result. This is the form that
    /// bindings and arguments that are never referenced, but may fail, are lowered to, along with
    /// statements that always raise or fall through, since an inline `if` evaluates both branches.
    ///
    /// CLVM evaluates an operator's operands from the last to the first, so `(i () value result)`
    /// evaluates the result first. Which one is evaluated first only matters if both of them
    /// fail, since that's the failure that's reported, which in a debug build is the message and
    /// location of the raise. So in a debug build, a result that always raises, such as the raise
    /// of a `match` on a value that has no values (#580), comes first, so that the value is
    /// evaluated before it. A release build keeps the order, since every raise is the same there,
    /// and the optimizer can only drop a value that turns out not to fail, such as a call that's
    /// inlined, when it's the first branch. If the value always raises, the result is never
    /// reached, so it's left out.
    fn effect(&mut self, value: LirId, result: LirId) -> LirId {
        if self.always_raises(value) {
            return value;
        }

        let nil = self.arena.alloc(Lir::Atom(vec![]));

        if self.options.debug_symbols && self.always_raises(result) {
            self.arena.alloc(Lir::If(nil, result, value, true))
        } else {
            self.arena.alloc(Lir::If(nil, value, result, true))
        }
    }

    /// Whether evaluating the LIR always raises: a raise, an `if` whose condition raises, an
    /// inline `if` with a branch that raises (since it evaluates both), any other `if` whose
    /// branches both raise, or a run whose environment or quoted body raises. A call to a function
    /// that always raises can't be seen here, so it isn't one.
    fn always_raises(&self, lir: LirId) -> bool {
        match self.arena[lir] {
            Lir::Raise(_) => true,
            Lir::If(condition, then, otherwise, inline) => {
                self.always_raises(condition)
                    || if inline {
                        self.always_raises(then) || self.always_raises(otherwise)
                    } else {
                        self.always_raises(then) && self.always_raises(otherwise)
                    }
            }
            Lir::Run(body, env) => {
                self.always_raises(env)
                    || matches!(self.arena[body], Lir::Quote(body) if self.always_raises(body))
            }
            _ => false,
        }
    }

    fn lower_symbol_reference(&mut self, env: &Environment, symbol: SymbolId) -> LirId {
        if let Some((frame, hir)) = self.inline_substitution(symbol) {
            return self.lower_substitution(env, frame, hir);
        }

        self.lower_path(env, symbol)
    }

    fn lower_symbol(&mut self, env: &Environment, symbol: SymbolId, is_lambda: bool) -> LirId {
        let mut reference = if self.should_inline(symbol) || is_lambda {
            self.lower_symbol_value(env, symbol)
        } else {
            self.lower_symbol_reference(env, symbol)
        };

        if let Symbol::Function(function) = self.db.symbol(symbol).clone()
            && function.kind != FunctionKind::External
        {
            let mut refs = Vec::new();

            if symbol == self.main {
                // Main expects its captures in nested groups, with the last group first
                let (capture_groups, _) = self.function_groups(symbol, &function);

                for group in capture_groups.iter().rev() {
                    match group {
                        SymbolGroup::Sequential(symbols) => {
                            for &capture in symbols {
                                refs.push(self.lower_symbol_reference(env, capture));
                            }
                        }
                        SymbolGroup::Tree(tree) => {
                            refs.push(self.lower_tree(env, tree, true, None));
                        }
                    }
                }
            } else {
                let captures: Vec<SymbolId> = self
                    .lowered_captures(symbol)
                    .into_iter()
                    .filter(|&capture| self.is_capture(capture))
                    .collect();

                for capture in captures {
                    refs.push(self.lower_symbol_reference(env, capture));
                }
            }

            reference = self.arena.alloc(Lir::Closure(
                reference,
                refs,
                !function.parameters.is_empty(),
            ));
        }

        reference
    }

    fn lower_path(&mut self, env: &Environment, symbol: SymbolId) -> LirId {
        let path = match env.path(symbol) {
            Ok(path) => path,
            // A value that's lowered on its own isn't a constant if it needs the environment
            Err(PathError::SymbolNotFound) if self.missing_paths.is_some() => {
                self.missing_paths = Some(true);
                return self.arena.alloc(Lir::Atom(vec![]));
            }
            Err(PathError::SymbolNotFound) => panic!(
                "clvm path in environment not found for symbol {}",
                self.db.debug_symbol(symbol)
            ),
        };

        self.arena.alloc(Lir::Path(path))
    }

    fn lower_block(&mut self, env: &Environment, mut stmts: Vec<Statement>, body: Tail) -> LirId {
        if !self.options.debug_symbols {
            stmts.retain(|stmt| !matches!(stmt, Statement::Debug(..)));
        }

        let Some(stmt) = stmts.first().cloned() else {
            return match body {
                Tail::Body(Some(body)) => self.lower_hir(env, body),
                Tail::Body(None) => self.arena.alloc(Lir::Atom(vec![])),
                Tail::Continue(continuation) => self.lower_continuation(env, &continuation),
                Tail::Control(chain, tail) => {
                    self.lower_control_chain(env, chain, Tail::clone(&tail))
                }
            };
        };

        // Each statement nests the rest of the block inside of it
        self.nested(Frame::Statement(stmt.srcloc().clone()), |this| {
            this.lower_statement(env, stmt, stmts, body)
        })
    }

    fn lower_statement(
        &mut self,
        env: &Environment,
        stmt: Statement,
        stmts: Vec<Statement>,
        body: Tail,
    ) -> LirId {
        match stmt {
            Statement::Let(..) => self.lower_let_stmts(env, stmts, body),
            Statement::Return(hir, _) => self.lower_block(env, vec![], Tail::Body(Some(hir))),
            Statement::Assert(condition, message, srcloc) => {
                self.lower_assert(env, stmts, condition, message, srcloc, body)
            }
            Statement::Expr(stmt) => self.lower_expr_stmts(env, stmt, stmts, body),
            Statement::Raise(hir, srcloc) => self.lower_raise(env, hir, srcloc),
            Statement::If(stmt) => self.lower_if(env, stmts, stmt, body),
            Statement::Control(stmt) => self.lower_control(env, stmts, &stmt, body),
            Statement::Debug(hir, srcloc) => self.lower_debug(env, stmts, hir, srcloc, body),
        }
    }

    fn lower_let_stmts(
        &mut self,
        env: &Environment,
        mut stmts: Vec<Statement>,
        body: Tail,
    ) -> LirId {
        let lets = stmts
            .iter()
            .take_while(|stmt| matches!(stmt, Statement::Let(..)))
            .count();

        let symbols: IndexSet<SymbolId> = stmts
            .drain(..lets)
            .map(|stmt| match stmt {
                Statement::Let(symbol, _) => symbol,
                _ => unreachable!(),
            })
            .collect();

        let saved = self.forget_decisions(&symbols);

        let effects: Vec<SymbolId> = symbols
            .iter()
            .copied()
            .filter(|&symbol| self.is_lowered_effect(symbol))
            .collect();

        let expr = self.lower_path_bindings(env, &symbols, |this| {
            this.lower_let_run(env, &symbols, &effects, stmts.clone(), body.clone())
        });

        self.restore_decisions(saved);

        expr
    }

    /// Lowers a run of let statements and the rest of the block, with the bindings that aren't
    /// substituted bound in groups before it.
    fn lower_let_run(
        &mut self,
        env: &Environment,
        symbols: &IndexSet<SymbolId>,
        effects: &[SymbolId],
        stmts: Vec<Statement>,
        body: Tail,
    ) -> LirId {
        // A binding that's evaluated where the only loop that uses it is called isn't bound here,
        // but at that call, which is in the rest of the block. What's decided here is the
        // innermost decision for the binding while the block is lowered, since an `inline fn`
        // can be inlined into its own argument, which decides it again with other arguments
        let mut bound = IndexSet::new();
        let mut decided = Vec::new();

        for &symbol in symbols {
            if self.graph.sole_loop(symbol).is_none() {
                bound.insert(symbol);
                continue;
            }

            let call = self.loop_call_substitution(symbol);

            if call.is_none() {
                bound.insert(symbol);
            }

            decided.push(symbol);
            self.loop_calls.entry(symbol).or_default().push(call);
        }

        let binding_groups = self.group_symbols(bound, false, true, false);

        let mut body_env = env.clone();

        for group in &binding_groups {
            body_env = Self::apply_group(body_env, group, true);
        }

        let mut expr = self.lower_block(&body_env, stmts, body);

        for symbol in decided {
            if let Some(calls) = self.loop_calls.get_mut(&symbol) {
                calls.pop();

                if calls.is_empty() {
                    self.loop_calls.remove(&symbol);
                }
            }
        }

        // The bindings that are never referenced, but may fail, are evaluated for their effect.
        // Nothing references them, so they're evaluated after the others are bound.
        for &symbol in effects.iter().rev() {
            let value = self.lower_symbol_value(&body_env, symbol);
            expr = self.effect(value, expr);
        }

        for (i, group) in binding_groups.iter().enumerate().rev() {
            expr = self.arena.alloc(Lir::Quote(expr));

            let mut bind_env = env.clone();

            for group in binding_groups.iter().take(i) {
                bind_env = Self::apply_group(bind_env, group, true);
            }

            let rest = self.arena.alloc(Lir::Path(1u8.into()));
            let group_env = self.lower_group_environment(&bind_env, group, rest, false, None, true);

            expr = self.arena.alloc(Lir::Run(expr, group_env));
        }

        expr
    }

    fn lower_group_environment(
        &mut self,
        env: &Environment,
        group: &SymbolGroup,
        rest: LirId,
        by_reference: bool,
        map: Option<&HashMap<SymbolId, LirId>>,
        include_rest: bool,
    ) -> LirId {
        match group {
            SymbolGroup::Sequential(symbols) => {
                let mut result = rest;

                for (i, &symbol) in symbols.iter().rev().enumerate() {
                    let value = if let Some(lir) = map.and_then(|map| map.get(&symbol)) {
                        *lir
                    } else if by_reference {
                        self.lower_symbol_reference(env, symbol)
                    } else {
                        self.lower_symbol_value(env, symbol)
                    };
                    if !include_rest && i == 0 {
                        result = value;
                    } else {
                        result = self.arena.alloc(Lir::Cons(value, result));
                    }
                }

                result
            }
            SymbolGroup::Tree(tree) => {
                let tree = self.lower_tree(env, tree, by_reference, map);
                if include_rest {
                    self.arena.alloc(Lir::Cons(tree, rest))
                } else {
                    tree
                }
            }
        }
    }

    fn lower_tree(
        &mut self,
        env: &Environment,
        tree: &Environment,
        by_reference: bool,
        map: Option<&HashMap<SymbolId, LirId>>,
    ) -> LirId {
        enum Step<'a> {
            Lower(&'a Environment),
            Cons,
        }

        // A tree of parameters is as deep as the function has unreferenced parameters, so it's
        // lowered without recursing
        let mut steps = vec![Step::Lower(tree)];

        // Both sides of each pair are lowered before the pair itself
        let mut lowered = Vec::new();

        while let Some(step) = steps.pop() {
            match step {
                Step::Lower(Environment::Nil) => lowered.push(self.arena.alloc(Lir::Atom(vec![]))),
                Step::Lower(Environment::Leaf(symbol)) => {
                    lowered.push(if let Some(lir) = map.and_then(|map| map.get(symbol)) {
                        *lir
                    } else if by_reference {
                        self.lower_symbol_reference(env, *symbol)
                    } else {
                        self.lower_symbol_value(env, *symbol)
                    });
                }
                Step::Lower(Environment::Pair(first, rest)) => {
                    steps.push(Step::Cons);
                    steps.push(Step::Lower(rest));
                    steps.push(Step::Lower(first));
                }
                Step::Cons => {
                    let rest = lowered.pop().expect("rest is lowered");
                    let first = lowered.pop().expect("first is lowered");
                    lowered.push(self.arena.alloc(Lir::Cons(first, rest)));
                }
            }
        }

        lowered.pop().expect("tree is lowered")
    }

    fn lower_assert(
        &mut self,
        env: &Environment,
        mut stmts: Vec<Statement>,
        condition: HirId,
        message: Option<HirId>,
        srcloc: SrcLoc,
        body: Tail,
    ) -> LirId {
        stmts.remove(0);

        let condition = self.lower_hir(env, condition);
        let then_branch = self.lower_block(env, stmts, body);

        // The check and the failure are located at the statement, rather than the condition
        self.with_span(srcloc.clone(), |this| {
            let else_branch = this.lower_failure(env, "assertion failed at", message, &srcloc);
            this.arena
                .alloc(Lir::If(condition, then_branch, else_branch, false))
        })
    }

    fn lower_raise(&mut self, env: &Environment, hir: Option<HirId>, srcloc: SrcLoc) -> LirId {
        self.with_span(srcloc.clone(), |this| {
            this.lower_failure(env, "raise called at", hir, &srcloc)
        })
    }

    /// Raises the description and location of a failing statement, followed by its value or
    /// message if it has one. They're only lowered with debug symbols, so a release build is the
    /// same whatever they are, and never evaluates them.
    fn lower_failure(
        &mut self,
        env: &Environment,
        description: &str,
        value: Option<HirId>,
        srcloc: &SrcLoc,
    ) -> LirId {
        if !self.options.debug_symbols {
            return self.arena.alloc(Lir::Raise(vec![]));
        }

        let error = self.arena.alloc(Lir::Atom(
            format!("{description} {}", srcloc.display(&self.base_path)).into_bytes(),
        ));
        let value = value.map(|value| self.lower_hir(env, value));

        self.arena
            .alloc(Lir::Raise(std::iter::once(error).chain(value).collect()))
    }

    fn lower_debug(
        &mut self,
        env: &Environment,
        mut stmts: Vec<Statement>,
        value: HirId,
        srcloc: SrcLoc,
        body: Tail,
    ) -> LirId {
        stmts.remove(0);

        let rest = self.lower_block(env, stmts, body);

        let value = self.lower_hir(env, value);
        let print = self
            .arena
            .alloc(Lir::DebugPrint(srcloc.display(&self.base_path), value));
        let nil = self.arena.alloc(Lir::Atom(vec![]));
        self.arena.alloc(Lir::If(print, nil, rest, false))
    }

    fn lower_if(
        &mut self,
        env: &Environment,
        mut stmts: Vec<Statement>,
        stmt: IfStatement,
        body: Tail,
    ) -> LirId {
        stmts.remove(0);
        let condition = self.lower_hir(env, stmt.condition);
        let then_branch = self.lower_hir(env, stmt.then);
        let else_branch = self.lower_block(env, stmts, body);
        self.arena
            .alloc(Lir::If(condition, then_branch, else_branch, stmt.inline))
    }

    /// Lowers an `if` and `else` chain or a `match` used as a statement, along with the rest of the
    /// block, which each branch that falls through continues with (see [`Continuation`]).
    fn lower_control(
        &mut self,
        env: &Environment,
        mut stmts: Vec<Statement>,
        stmt: &ControlStatement,
        body: Tail,
    ) -> LirId {
        stmts.remove(0);

        // Nothing after the statement is evaluated
        if stmt.fallthroughs == 0 {
            return self.lower_hir(env, stmt.hir);
        }

        // Each branch either raises or falls through, so the statement can be evaluated for its
        // effect, like an expression statement, and then the rest of the block
        if !stmt.may_return {
            let value = self.lower_hir(env, stmt.hir);
            let rest = self.lower_block(env, stmts, body);
            return self.effect(value, rest);
        }

        // Only one branch continues with the rest of the block, so it's lowered there
        if stmt.fallthroughs == 1 {
            let continuation = Rc::new(Continuation::Inline(stmts, body));
            return self.lower_control_chain(env, stmt.hir, Tail::Continue(continuation));
        }

        // Several branches continue with the rest of the block, so it's lowered once and run by
        // each of them, rather than being generated again in each one
        let rest = self.lower_block(env, stmts, body);
        let program = self.db.alloc_symbol(Symbol::Unresolved);
        let rest_env = self.db.alloc_symbol(Symbol::Unresolved);

        let chain_env = Environment::Pair(
            Box::new(Environment::Leaf(program)),
            Box::new(Environment::Pair(
                Box::new(Environment::Leaf(rest_env)),
                Box::new(env.clone()),
            )),
        );

        let continuation = Rc::new(Continuation::Shared {
            program,
            env: rest_env,
        });
        let chain = self.lower_control_chain(&chain_env, stmt.hir, Tail::Continue(continuation));

        let chain = self.arena.alloc(Lir::Quote(chain));
        let rest = self.arena.alloc(Lir::Quote(rest));
        let current_env = self.arena.alloc(Lir::Path(1u8.into()));
        let envs = self.arena.alloc(Lir::Cons(current_env, current_env));
        let args = self.arena.alloc(Lir::Cons(rest, envs));
        self.arena.alloc(Lir::Run(chain, args))
    }

    /// Lowers the node of a [`ControlStatement`], whose branches end with the tail when they fall
    /// through, rather than with nil like a block without a body.
    fn lower_control_chain(&mut self, env: &Environment, hir: HirId, tail: Tail) -> LirId {
        self.nested(Frame::Hir(hir), |this| match this.db.hir(hir).clone() {
            Hir::If(condition, then, else_, inline) => {
                let condition = this.lower_hir(env, condition);
                let then = this.lower_control_chain(env, then, tail.clone());
                let else_ = this.lower_control_chain(env, else_, tail);
                this.arena.alloc(Lir::If(condition, then, else_, inline))
            }
            Hir::Block(block) => {
                let tail = match block.body {
                    Some(body) => Tail::Control(body, Rc::new(tail)),
                    None => tail,
                };
                this.lower_block(env, block.statements, tail)
            }
            _ => this.lower_hir_inner(env, hir),
        })
    }

    /// Lowers what a branch of a [`ControlStatement`] that falls through continues with.
    fn lower_continuation(&mut self, env: &Environment, continuation: &Continuation) -> LirId {
        match continuation {
            Continuation::Inline(stmts, tail) => self.lower_block(env, stmts.clone(), tail.clone()),
            Continuation::Shared {
                program,
                env: rest_env,
            } => {
                let program = self.lower_path(env, *program);
                let rest_env = self.lower_path(env, *rest_env);
                self.arena.alloc(Lir::Run(program, rest_env))
            }
        }
    }

    fn lower_expr_stmts(
        &mut self,
        env: &Environment,
        stmt: ExprStatement,
        mut stmts: Vec<Statement>,
        body: Tail,
    ) -> LirId {
        if self.options.debug_symbols {
            stmts.remove(0);

            let rest = self.lower_block(env, stmts, body);
            let value = self.lower_hir(env, stmt.hir);
            let one = self.arena.alloc(Lir::Atom(vec![1]));
            return self.arena.alloc(Lir::If(one, rest, value, true));
        }

        let mut ids = Vec::new();

        while let Some(stmt) = stmts.first().cloned()
            && let Statement::Expr(expr) = stmt
        {
            ids.push((self.lower_hir(env, expr.hir), expr.always_nil));
            stmts.remove(0);
        }

        let expr = self.lower_block(env, stmts, body);

        if ids.is_empty() {
            return expr;
        }

        let (condition, verifications) = if ids.len() == 1 {
            let condition = self.arena.alloc(Lir::Atom(vec![]));

            (condition, ids[0].0)
        } else if let Some(index) = ids.iter().position(|(_, always_nil)| *always_nil) {
            let (condition, _) = ids.remove(index);

            let id = if ids.len() == 1 {
                ids[0].0
            } else {
                self.arena
                    .alloc(Lir::All(ids.iter().map(|(id, _)| *id).collect()))
            };

            (condition, id)
        } else {
            let condition = self.arena.alloc(Lir::Atom(vec![]));

            let id = if ids.len() == 1 {
                ids[0].0
            } else {
                self.arena
                    .alloc(Lir::All(ids.iter().map(|(id, _)| *id).collect()))
            };

            (condition, id)
        };

        self.arena
            .alloc(Lir::If(condition, verifications, expr, true))
    }

    /// Groups the symbols into rounds, where each symbol is bound in a round after every symbol
    /// in the set that it depends on (see [`DependencyGraph::rounds`]). Functions, and symbols
    /// captured by reference, can be bound in the first round, since they don't need the values of
    /// the others yet.
    fn group_symbols(
        &mut self,
        symbols: IndexSet<SymbolId>,
        by_reference: bool,
        is_tree: bool,
        for_loop: bool,
    ) -> Vec<SymbolGroup> {
        let rounds = if by_reference {
            Rc::new(vec![Some(0); symbols.len()])
        } else {
            self.rounds(&symbols)
        };

        // Symbols that depend on each other can't be bound after one another, so they're bound in
        // a round after all of the others
        let last_round = rounds.iter().flatten().max().map_or(0, |round| round + 1);

        let mut groups = vec![Vec::new(); last_round + 1];

        for (&symbol, round) in symbols.iter().zip(rounds.iter()) {
            if self.is_grouped_capture(symbol, for_loop) {
                groups[round.unwrap_or(last_round)].push(symbol);
            }
        }

        groups
            .into_iter()
            .filter(|group| !group.is_empty())
            .map(|group| self.create_group(group, is_tree))
            .collect()
    }

    /// Whether the symbol is bound in a group, rather than inlined or left out because nothing
    /// references it.
    fn is_grouped_capture(&self, symbol: SymbolId, for_loop: bool) -> bool {
        // A loop is lowered once, where the program's functions are, but it's called where its
        // `for` expression is, which can be inside an inlined call. So what it captures can't
        // depend on the arguments of the calls being inlined: it always captures a parameter, and
        // a call inside an inlined call passes it the parameter's argument
        let is_capture = if for_loop && matches!(self.db.symbol(symbol), Symbol::Parameter(_)) {
            true
        } else {
            self.is_capture(symbol)
        };

        is_capture && self.graph.references(symbol) > 0
    }

    /// The rounds that the symbols are bound in when they're grouped by value, as given by
    /// [`DependencyGraph::rounds`]. Functions don't need the values of the others yet.
    fn rounds(&mut self, symbols: &IndexSet<SymbolId>) -> Rc<Vec<Option<usize>>> {
        if let Some(rounds) = self.rounds.get(symbols.as_slice()) {
            return rounds.clone();
        }

        let rounds = Rc::new(self.graph.rounds(symbols, |symbol| {
            !matches!(self.db.symbol(symbol), Symbol::Function(_))
        }));

        self.rounds
            .insert(symbols.as_slice().into(), rounds.clone());
        rounds
    }

    fn create_group(&self, symbols: Vec<SymbolId>, is_tree: bool) -> SymbolGroup {
        if is_tree {
            SymbolGroup::Tree(self.layout_tree(symbols))
        } else {
            SymbolGroup::Sequential(symbols)
        }
    }

    fn apply_group(mut env: Environment, group: &SymbolGroup, include_rest: bool) -> Environment {
        match group {
            SymbolGroup::Sequential(symbols) => {
                for (i, &symbol) in symbols.iter().rev().enumerate() {
                    if i == 0 && !include_rest {
                        env = Environment::Leaf(symbol);
                    } else {
                        env = Environment::Pair(Box::new(Environment::Leaf(symbol)), Box::new(env));
                    }
                }

                env
            }
            SymbolGroup::Tree(tree) => {
                if include_rest {
                    Environment::Pair(Box::new(tree.clone()), Box::new(env))
                } else {
                    tree.clone()
                }
            }
        }
    }

    fn inline_argument(&self, symbol: SymbolId) -> Option<HirId> {
        self.inline_substitution(symbol).map(|(_, hir)| hir)
    }

    /// Whether the value can be lowered in any environment, since it doesn't depend on it.
    fn is_static_hir(&self, hir: HirId) -> bool {
        // This follows references to the values that are inlined, which can go as deep as a chain
        // of them is long, without passing through [`Self::nested`]
        rue_types::grow_stack(|| self.is_static_hir_inner(hir))
    }

    fn is_static_hir_inner(&self, hir: HirId) -> bool {
        match self.db.hir(hir) {
            Hir::Nil | Hir::String(_) | Hir::Int(_) | Hir::Bool(_) | Hir::Bytes(_) => true,
            Hir::Pair(first, rest) => self.is_static_hir(*first) && self.is_static_hir(*rest),
            Hir::Unary(UnaryOp::Plus, hir) | Hir::Cast(hir) => self.is_static_hir(*hir),
            Hir::List(list) => list.items.iter().all(|&item| self.is_static_hir(item)),
            Hir::Struct(initializer) => initializer.values().all(|value| self.is_static_hir(value)),
            // Guards that don't use the value at runtime are lowered to a boolean, unless the value
            // may fail, in which case it's evaluated for its effect first
            Hir::Guard(guard) => {
                guard.value_uses() == 0 && !self.is_lowered_guard_effect(hir, guard.value)
            }
            Hir::Const(expr) => expr.result.is_some_and(|result| self.is_static_hir(result)),
            Hir::Reference(symbol) => self
                .substituted_value(*symbol)
                .is_some_and(|value| self.is_static_hir(value)),
            _ => false,
        }
    }

    /// The value that's lowered in place of a reference to the symbol, if it's substituted: the
    /// argument of an inlined call for its parameter, or the value of a constant or binding
    /// that's inlined.
    fn substituted_value(&self, symbol: SymbolId) -> Option<HirId> {
        if let Some(hir) = self.inline_argument(symbol) {
            return Some(hir);
        }

        if !self.should_inline(symbol) {
            return None;
        }

        match self.db.symbol(symbol) {
            Symbol::Constant(constant) => Some(constant.value.hir),
            Symbol::Binding(binding) => Some(binding.value.hir),
            _ => None,
        }
    }

    /// Whether a function needs to capture the symbol rather than inlining its value.
    /// Inline arguments are captured by value, unless they are static.
    fn is_capture(&self, symbol: SymbolId) -> bool {
        match self.inline_argument(symbol) {
            Some(hir) => !self.is_static_hir(hir),
            None => !self.should_inline(symbol),
        }
    }

    fn should_inline(&self, symbol: SymbolId) -> bool {
        if self.inline_argument(symbol).is_some() {
            return true;
        }

        let references = self.graph.references(symbol);

        match self.db.symbol(symbol) {
            Symbol::Unresolved | Symbol::Module(_) | Symbol::Parameter(_) | Symbol::Builtin(_) => {
                false
            }
            Symbol::Function(function) => {
                if function.kind == FunctionKind::External {
                    return false;
                }

                if self.is_recursive(symbol, function) {
                    return false;
                }

                if function.kind == FunctionKind::Inline {
                    return true;
                }

                if !self.options.auto_inline {
                    return false;
                }

                // A function that's called more than once, or that reads a parameter more than
                // once, is only inlined if the program was measured to be smaller that way
                !self.is_measured_function(symbol) || self.is_inlined_function(symbol)
            }
            Symbol::Constant(constant) => {
                constant.inline
                    || (self.is_substitution_candidate(symbol, constant.value.hir)
                        && references <= 1
                        && self.can_substitute(symbol))
            }
            Symbol::Binding(binding) => {
                if binding.inline {
                    return true;
                }

                if !self.is_substitution_candidate(symbol, binding.value.hir) {
                    return false;
                }

                if references <= 1 {
                    return self.can_substitute(symbol);
                }

                self.is_path_binding(symbol)
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use rue_types::BuiltinTypes;

    use crate::{ParameterSymbol, Scope};

    use super::*;

    fn function(
        db: &mut Database,
        builtins: &BuiltinTypes,
        parameters: Vec<SymbolId>,
        body: HirId,
        kind: FunctionKind,
    ) -> FunctionSymbol {
        FunctionSymbol {
            name: None,
            ty: builtins.unresolved,
            scope: db.alloc_scope(Scope::new(None)),
            vars: vec![],
            parameters,
            nil_terminated: true,
            return_type: builtins.unresolved,
            body,
            kind,
        }
    }

    fn parameter(db: &mut Database, builtins: &BuiltinTypes) -> SymbolId {
        db.alloc_symbol(Symbol::Parameter(ParameterSymbol {
            name: None,
            ty: builtins.int,
        }))
    }

    // fn main(x: Int) -> Int { f(<argument>)() }
    // inline fn f(y: Int) -> fn() -> Int { fn() => y }
    // Returns the captures of the closure created by the lambda in `f`
    fn lambda_captures(argument: impl FnOnce(&mut Database, SymbolId) -> HirId) -> Vec<Lir> {
        let mut db = Database::new();
        let builtins = BuiltinTypes::new(db.types_mut());

        let y = parameter(&mut db, &builtins);
        let lambda_body = db.alloc_hir(Hir::Reference(y));
        let lambda = function(
            &mut db,
            &builtins,
            vec![],
            lambda_body,
            FunctionKind::Sequential,
        );
        let lambda = db.alloc_symbol(Symbol::Function(lambda));

        let f_body = db.alloc_hir(Hir::Lambda(lambda));
        let f = function(&mut db, &builtins, vec![y], f_body, FunctionKind::Inline);
        let f = db.alloc_symbol(Symbol::Function(f));

        let x = parameter(&mut db, &builtins);
        let f_reference = db.alloc_hir(Hir::Reference(f));
        let argument = argument(&mut db, x);
        let f_call = db.alloc_hir(Hir::FunctionCall(FunctionCall {
            function: f_reference,
            args: vec![argument],
            nil_terminated: true,
        }));
        let main_body = db.alloc_hir(Hir::FunctionCall(FunctionCall {
            function: f_call,
            args: vec![],
            nil_terminated: true,
        }));
        let main = function(
            &mut db,
            &builtins,
            vec![x],
            main_body,
            FunctionKind::Sequential,
        );
        let main = db.alloc_symbol(Symbol::Function(main));

        let options = CompilerOptions::default();
        let graph = DependencyGraph::build(&db, main, options);
        let mut arena = Arena::new();
        let mut lowerer = Lowerer::new(&mut db, &mut arena, &graph, options, main, PathBuf::new());
        lowerer.lower_symbol_value(&Environment::default(), main);

        let mut closures = arena
            .iter()
            .filter_map(|(_, lir)| match lir {
                Lir::Closure(_, captures, _) => Some(captures.clone()),
                _ => None,
            })
            .collect::<Vec<_>>();
        assert_eq!(closures.len(), 1);

        closures
            .remove(0)
            .into_iter()
            .map(|capture| arena[capture].clone())
            .collect()
    }

    #[test]
    fn closure_captures_inline_argument() {
        let captures = lambda_captures(|db, x| db.alloc_hir(Hir::Reference(x)));
        assert!(matches!(captures.as_slice(), [Lir::Path(path)] if *path == 2u8.into()));
    }

    #[test]
    fn closure_inlines_static_argument() {
        let captures = lambda_captures(|db, _| db.alloc_hir(Hir::Int(5.into())));
        assert!(captures.is_empty());
    }
}
