use std::{
    collections::{HashMap, HashSet},
    iter,
};

use indexmap::IndexSet;
use rue_options::CompilerOptions;

use crate::{Database, FailureAnalysis, Hir, HirId, Statement, Symbol, SymbolId};

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

    // Keeps track of the bindings whose values may fail that haven't been visited yet, along with
    // the symbol whose body their let statement is in, and the parameters whose function values
    // they call, if that's the only way they can fail
    unvisited_effects: Vec<(Option<SymbolId>, SymbolId, IndexSet<SymbolId>)>,

    // Keeps track of the direct calls to each function, so that a binding that calls the value of
    // one of its parameters can find the function values it's passed
    calls: HashMap<SymbolId, Vec<HirId>>,

    // The entrypoint, whose parameters come from the solution
    main: SymbolId,

    // Finds whether the values of let statements may fail
    failures: FailureAnalysis,
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
            calls: HashMap::new(),
            main,
            failures: FailureAnalysis::new(),
        };

        // The symbols are visited depth first, in the order they're referenced, like a recursive
        // traversal would, except that each symbol's whole body is visited before the symbols it
        // references. The bindings that are only evaluated for their effect are visited last.
        while !graph.pending.is_empty() {
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

            visit_effects(db, &mut graph);
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

    /// Whether calling the function values of any of the parameters may fail, given the calls
    /// that have been visited so far. The parameters of `main` come from the solution, and a
    /// function that's used as a value, such as a lambda, could be called from anywhere, so
    /// calling their values may fail.
    fn calls_may_fail(&self, db: &Database, parameters: &IndexSet<SymbolId>) -> bool {
        parameters.iter().any(|&parameter| {
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
                        || call
                            .args
                            .get(index)
                            .is_none_or(|&arg| self.failures.value_call_may_fail(db, arg))
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
        // The value is referenced as many times as the check it's lowered to uses it
        Hir::Guard(guard) => {
            for _ in 0..guard.value_uses() {
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

/// A binding whose value may fail is evaluated even if it's never referenced, so that the check
/// isn't dropped. Whether it's referenced isn't known until the rest of the program has been
/// visited, so it's only recorded here (see [`visit_effects`]).
///
/// A reference to another binding can't fail here (see [`FailureAnalysis`]), so whether another
/// binding is referenced doesn't change which values may fail.
fn visit_let(db: &Database, graph: &mut DependencyGraph, symbol: SymbolId) {
    let Symbol::Binding(binding) = db.symbol(symbol) else {
        return;
    };

    if binding.inline {
        return;
    }

    // A value that only fails if a function value that a parameter is passed fails is decided
    // once the calls that pass them have been visited
    let calls = match graph.failures.body_calls(db, binding.value.hir) {
        None => IndexSet::new(),
        Some(calls) if calls.is_empty() => return,
        Some(calls) => calls,
    };

    graph.unvisited_effects.push((graph.current, symbol, calls));
}

/// Once everything that's referenced has been visited, the bindings whose values may fail but
/// that were never referenced are visited, since they're evaluated for their effect. They don't
/// count as references, but the enclosing function still depends on them, so that it captures what
/// their values need. Visiting them can reach more of these, so this repeats until there are none.
///
/// A binding that only calls the function values of parameters waits until a call passes one that
/// may fail. Visiting more of the program can find more calls, so it's checked again each time.
///
/// These are only added at the end, so that the dependencies of a program without any are in the
/// same order as if they didn't exist.
fn visit_effects(db: &Database, graph: &mut DependencyGraph) {
    let effects = std::mem::take(&mut graph.unvisited_effects);

    for (current, symbol, calls) in effects.into_iter().rev() {
        if graph.visited.contains(&symbol) || graph.references(symbol) > 0 {
            continue;
        }

        if !calls.is_empty() && !graph.calls_may_fail(db, &calls) {
            graph.unvisited_effects.push((current, symbol, calls));
            continue;
        }

        graph.effects.insert(symbol);

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
