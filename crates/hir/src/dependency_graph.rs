use std::collections::HashMap;

use indexmap::IndexSet;

use crate::{Database, Hir, HirId, Symbol, SymbolId};

#[derive(Debug, Default, Clone)]
pub struct DependencyGraph {
    dependencies: HashMap<SymbolId, IndexSet<SymbolId>>,
    references: HashMap<SymbolId, usize>,
    stack: IndexSet<SymbolId>,
    visited: IndexSet<SymbolId>,
}

impl DependencyGraph {
    pub fn build(db: &Database, main: SymbolId) -> Self {
        let mut graph = Self::default();
        // TODO: Is this the correct way to build the dependency graph from the entrypoint?
        visit_symbol_reference(db, &mut graph, main);
        graph
    }

    pub fn direct_dependencies(&self, symbol: SymbolId) -> impl Iterator<Item = SymbolId> {
        self.dependencies
            .get(&symbol)
            .into_iter()
            .flat_map(|deps| deps.iter().copied())
    }

    pub fn dependencies(&self, symbol: SymbolId) -> IndexSet<SymbolId> {
        let mut visited = IndexSet::new();
        let mut stack = vec![symbol];
        let mut has_self_reference = false;

        // First pass: collect all dependencies and check for self-reference
        while let Some(current) = stack.pop() {
            if !visited.insert(current) {
                continue;
            }

            // Check if this symbol directly references the original symbol
            if let Some(deps) = self.dependencies.get(&current) {
                if current == symbol && deps.contains(&symbol) {
                    has_self_reference = true;
                }
                stack.extend(deps.iter().copied());
            }
        }

        // If there's no self-reference, remove the original symbol
        if !has_self_reference {
            visited.swap_remove(&symbol);
        }

        visited
    }

    pub fn references(&self, symbol: SymbolId) -> usize {
        *self.references.get(&symbol).unwrap_or(&0)
    }
}

fn visit_hir(db: &Database, graph: &mut DependencyGraph, hir: HirId) {
    match db.hir(hir) {
        Hir::Unresolved | Hir::Atom(_) => {}
        Hir::Reference(symbol) => {
            visit_symbol_reference(db, graph, *symbol);
        }
        Hir::Block(block) => {
            // TODO: Properly implement blocks
            if let Some(body) = block.body {
                visit_hir(db, graph, body);
            }
        }
        Hir::Unary(_op, arg) => {
            visit_hir(db, graph, *arg);
        }
        Hir::Binary(_op, lhs, rhs) => {
            visit_hir(db, graph, *lhs);
            visit_hir(db, graph, *rhs);
        }
    }
}

fn visit_symbol_reference(db: &Database, graph: &mut DependencyGraph, symbol: SymbolId) {
    // We should add the symbol to the dependencies of the symbol we're currently
    if let Some(last) = graph.stack.last() {
        graph.dependencies.entry(*last).or_default().insert(symbol);
        *graph.references.entry(symbol).or_insert(0) += 1;
    }

    // We've already visited this symbol, so we don't need to track its dependencies again
    if !graph.visited.insert(symbol) {
        return;
    }

    graph.stack.insert(symbol);

    match db.symbol(symbol) {
        Symbol::Function(function) => {
            visit_hir(db, graph, function.body);
        }
        Symbol::Parameter(_) => {}
        Symbol::Binding(binding) => {
            visit_hir(db, graph, binding.value);
        }
    }

    graph.stack.pop().unwrap();
}
