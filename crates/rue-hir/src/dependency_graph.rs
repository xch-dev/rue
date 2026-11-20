use std::{
    collections::{HashMap, HashSet},
    iter,
};

use indexmap::{IndexSet, set::Slice};
use rue_options::CompilerOptions;

use crate::{
    Database, FailureAnalysis, Hir, HirId, Statement, Symbol, SymbolId,
    analysis::{Callee, Effect, resolve_callee},
};

#[derive(Debug, Clone)]
pub struct DependencyGraph {
    options: CompilerOptions,

    // Keeps track of the direct dependencies of a symbol
    dependencies: HashMap<SymbolId, IndexSet<SymbolId>>,

    // Keeps track of the symbol that each local is defined in
    owners: HashMap<SymbolId, SymbolId>,

    // Keeps track of the number of times a symbol is referenced
    references: HashMap<SymbolId, usize>,

    // Keeps track of whether a symbol is ever used as a closure
    closures: HashSet<SymbolId>,

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

/// A type guard whose check doesn't use its value, along with the symbol whose body it's in, which
/// hasn't been decided by [`visit_effects`] yet.
#[derive(Debug, Clone, Copy)]
struct UnvisitedGuard {
    current: Option<SymbolId>,
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
            closures: HashSet::new(),
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

        graph
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

    pub fn is_closure(&self, symbol: SymbolId) -> bool {
        self.closures.contains(&symbol)
    }

    pub fn const_exprs(&self) -> impl Iterator<Item = HirId> + '_ {
        self.const_exprs.iter().copied()
    }

    /// Whether the symbol is defined in the function, or in a function defined in it, such as a
    /// parameter or local of the function or of a lambda inside it.
    pub fn is_defined_in(&self, symbol: SymbolId, function: SymbolId) -> bool {
        iter::successors(self.owners.get(&symbol).copied(), |owner| {
            self.owners.get(owner).copied()
        })
        .any(|owner| owner == function)
    }

    pub fn dependencies(&self, symbol: SymbolId, captures_only: bool) -> IndexSet<SymbolId> {
        // The locals of the scopes enclosing the symbol are captured, unless they are reached
        // through the scope they're defined in (such as by a recursive call), so each path
        // keeps track of which of these scopes it has entered
        let scopes: Vec<SymbolId> = iter::successors(self.owners.get(&symbol).copied(), |owner| {
            self.owners.get(owner).copied()
        })
        .collect();

        let mut visited = HashSet::new();
        let mut result = IndexSet::new();
        let mut stack = vec![(symbol, false, vec![false; scopes.len()])];

        while let Some((current, is_result, mut entered)) = stack.pop() {
            if !visited.insert((current, is_result, entered.clone())) {
                continue;
            }

            if is_result {
                result.insert(current);
            }

            if let Some(index) = scopes.iter().position(|&scope| scope == current) {
                entered[index] = true;
            }

            for dependency in self.dependencies.get(&current).cloned().unwrap_or_default() {
                // Any other local can only be reached through the scope it's defined in
                let is_local = self.owners.get(&dependency).is_some_and(|owner| {
                    scopes
                        .iter()
                        .position(|scope| scope == owner)
                        .is_none_or(|index| entered[index])
                });

                stack.push((dependency, !captures_only || !is_local, entered.clone()));
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
        let owner = symbols
            .first()
            .and_then(|symbol| self.owners.get(symbol).copied());

        let same_owner = symbols
            .iter()
            .all(|symbol| self.owners.get(symbol).copied() == owner);

        debug_assert!(
            same_owner,
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
            for field in &initializer.fields {
                visit_hir(db, graph, field.value, false);
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
            if !is_call {
                graph.closures.insert(*symbol);
            }
            visit_symbol_reference(graph, *symbol);
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

                        visit_let(db, graph, *stmt);
                    }
                    Statement::Assert(stmt, _) => {
                        visit_hir(db, graph, *stmt, false);
                    }
                    Statement::Raise(stmt, _) => {
                        if graph.options.debug_symbols
                            && let Some(stmt) = stmt
                        {
                            visit_hir(db, graph, *stmt, false);
                        }
                    }
                    Statement::If(stmt) => {
                        visit_hir(db, graph, stmt.condition, false);
                        visit_hir(db, graph, stmt.then, false);
                    }
                    Statement::Return(expr, _) => {
                        visit_hir(db, graph, *expr, false);
                    }
                    Statement::Expr(expr) => {
                        visit_hir(db, graph, expr.hir, false);
                    }
                    Statement::Debug(expr, _) => {
                        if graph.options.debug_symbols {
                            visit_hir(db, graph, *expr, false);
                        }
                    }
                }
            }

            if let Some(body) = block.reachable_body() {
                visit_hir(db, graph, body, false);
            }
        }
        Hir::Lambda(lambda) => {
            if let Some(current) = graph.current {
                graph.owners.insert(*lambda, current);
            }

            graph.closures.insert(*lambda);

            visit_symbol_reference(graph, *lambda);
        }
        Hir::If(condition, then, else_, _inline) => {
            visit_hir(db, graph, *condition, false);
            visit_hir(db, graph, *then, false);
            visit_hir(db, graph, *else_, false);
        }
        Hir::FunctionCall(call) => {
            if let Hir::Reference(function) = db.hir(db.without_casts(call.function))
                && let Symbol::Function(_) = db.symbol(*function)
            {
                graph.calls.entry(*function).or_default().push(hir);
            }

            visit_hir(db, graph, call.function, true);
            for &arg in &call.args {
                visit_hir(db, graph, arg, false);
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

    match db.symbol(symbol) {
        Symbol::Unresolved | Symbol::Module(_) | Symbol::Parameter(_) | Symbol::Builtin(_) => {}
        Symbol::Function(function) => {
            for &parameter in &function.parameters {
                graph.owners.insert(parameter, symbol);
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
        visit_hir(db, graph, unvisited.value, false);
        graph.current = outer;

        let referenced = std::mem::take(&mut graph.referenced);
        graph.pending.extend(referenced.into_iter().rev());
    }

    // The ones that are still waiting are decided before any found while visiting the values
    waiting.append(&mut graph.unvisited_guards);
    graph.unvisited_guards = waiting;

    added
}

fn visit_symbol_reference(graph: &mut DependencyGraph, symbol: SymbolId) {
    // We should add the symbol to the dependencies of the symbol we're currently visiting
    if let Some(current) = graph.current {
        graph
            .dependencies
            .entry(current)
            .or_default()
            .insert(symbol);
        *graph.references.entry(symbol).or_insert(0) += 1;
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
