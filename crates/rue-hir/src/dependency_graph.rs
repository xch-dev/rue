use std::{
    collections::{HashMap, HashSet},
    iter,
};

use indexmap::IndexSet;
use rue_options::CompilerOptions;

use crate::{Database, Hir, HirId, Statement, Symbol, SymbolId};

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

    // Keeps track of the current stack of symbols being visited
    stack: IndexSet<SymbolId>,

    // Keeps track of the symbols that have been visited to avoid infinite loops
    visited: IndexSet<SymbolId>,
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
            stack: IndexSet::new(),
            visited: IndexSet::new(),
        };

        visit_symbol_reference(db, &mut graph, main);

        graph
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
        Hir::Const(expr) => {
            graph.const_exprs.insert(hir);
            visit_hir(db, graph, expr.value, false);
        }
        Hir::Reference(symbol) => {
            if !is_call {
                graph.closures.insert(*symbol);
            }
            visit_symbol_reference(db, graph, *symbol);
        }
        Hir::Block(block) => {
            for stmt in &block.statements {
                match stmt {
                    Statement::Let(stmt) => {
                        if let Some(last) = graph.stack.last() {
                            graph.owners.insert(*stmt, *last);
                        }
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
                    Statement::Return(expr) => {
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

            if let Some(body) = block.body {
                visit_hir(db, graph, body, false);
            }
        }
        Hir::Lambda(lambda) => {
            if let Some(last) = graph.stack.last() {
                graph.owners.insert(*lambda, *last);
            }

            graph.closures.insert(*lambda);

            visit_symbol_reference(db, graph, *lambda);
        }
        Hir::If(condition, then, else_, _inline) => {
            visit_hir(db, graph, *condition, false);
            visit_hir(db, graph, *then, false);
            visit_hir(db, graph, *else_, false);
        }
        Hir::FunctionCall(call) => {
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
    // We've already visited this symbol, so we don't need to track its dependencies again
    if !graph.visited.insert(symbol) {
        return;
    }

    graph.stack.insert(symbol);

    match db.symbol(symbol) {
        Symbol::Unresolved | Symbol::Module(_) | Symbol::Parameter(_) | Symbol::Builtin(_) => {}
        Symbol::Function(function) => {
            for &parameter in function.parameters.values() {
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

    graph.stack.pop().unwrap();
}

fn visit_symbol_reference(db: &Database, graph: &mut DependencyGraph, symbol: SymbolId) {
    // We should add the symbol to the dependencies of the symbol we're currently visiting
    if let Some(last) = graph.stack.last() {
        graph.dependencies.entry(*last).or_default().insert(symbol);
        *graph.references.entry(symbol).or_insert(0) += 1;
    }

    visit_symbol(db, graph, symbol);
}
