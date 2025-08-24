use std::collections::HashMap;

use indexmap::IndexSet;

use crate::{Database, Hir, HirId, Statement, Symbol, SymbolId};

#[derive(Debug, Default, Clone)]
pub struct DependencyGraph {
    // Keeps track of the direct dependencies of a symbol
    dependencies: HashMap<SymbolId, IndexSet<SymbolId>>,

    // Keeps track of the locals of a symbol
    locals: HashMap<SymbolId, IndexSet<SymbolId>>,

    // Keeps track of the number of times a symbol is referenced
    references: HashMap<SymbolId, usize>,

    // Keeps track of the current stack of symbols being visited
    stack: IndexSet<SymbolId>,

    // Keeps track of the symbols that have been visited to avoid infinite loops
    visited: IndexSet<SymbolId>,
}

impl DependencyGraph {
    pub fn build(db: &Database, main: SymbolId) -> Self {
        let mut graph = Self::default();
        visit_symbol_reference(db, &mut graph, main);
        graph
    }

    pub fn references(&self, symbol: SymbolId) -> usize {
        *self.references.get(&symbol).unwrap_or(&0)
    }

    pub fn dependencies(&self, symbol: SymbolId, captures_only: bool) -> IndexSet<SymbolId> {
        let mut visited = IndexSet::new();
        let mut result = IndexSet::new();
        let mut locals = IndexSet::new();
        let mut stack = vec![(symbol, false)];

        while let Some((current, is_result)) = stack.pop() {
            if !visited.insert((current, is_result)) {
                continue;
            }

            if is_result {
                result.insert(current);
            }

            locals.extend(self.locals.get(&current).cloned().unwrap_or_default());

            for dependency in self.dependencies.get(&current).cloned().unwrap_or_default() {
                stack.push((dependency, !captures_only || !locals.contains(&dependency)));
            }
        }

        result
    }
}

fn visit_hir(db: &Database, graph: &mut DependencyGraph, hir: HirId) {
    match db.hir(hir) {
        Hir::Unresolved
        | Hir::Nil
        | Hir::String(_)
        | Hir::Int(_)
        | Hir::Bytes(_)
        | Hir::Bool(_) => {}
        Hir::Pair(first, rest) => {
            visit_hir(db, graph, *first);
            visit_hir(db, graph, *rest);
        }
        Hir::Reference(symbol) => {
            visit_symbol_reference(db, graph, *symbol);
        }
        Hir::Block(block) => {
            for stmt in &block.statements {
                if let Statement::Let(stmt) = stmt {
                    visit_symbol(db, graph, *stmt);

                    if let Some(last) = graph.stack.last() {
                        graph.locals.entry(*last).or_default().insert(*stmt);
                    }
                }
            }

            if let Some(body) = block.body {
                visit_hir(db, graph, body);
            }
        }
        Hir::Lambda(lambda) => visit_symbol(db, graph, *lambda),
        Hir::If(condition, then, else_) => {
            visit_hir(db, graph, *condition);
            visit_hir(db, graph, *then);
            visit_hir(db, graph, *else_);
        }
        Hir::FunctionCall(call) => {
            visit_hir(db, graph, call.function);
            for &arg in &call.args {
                visit_hir(db, graph, arg);
            }
        }
        Hir::Unary(_op, arg) => {
            visit_hir(db, graph, *arg);
        }
        Hir::Binary(_op, lhs, rhs) => {
            visit_hir(db, graph, *lhs);
            visit_hir(db, graph, *rhs);
        }
        Hir::CoinId(parent, puzzle, amount) => {
            visit_hir(db, graph, *parent);
            visit_hir(db, graph, *puzzle);
            visit_hir(db, graph, *amount);
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
        Symbol::Function(function) => {
            graph
                .locals
                .entry(symbol)
                .or_default()
                .extend(function.parameters.iter().copied());

            visit_hir(db, graph, function.body);
        }
        Symbol::Parameter(_) => {}
        Symbol::Constant(constant) => {
            visit_hir(db, graph, constant.value);
        }
        Symbol::Binding(binding) => {
            visit_hir(db, graph, binding.value);
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
