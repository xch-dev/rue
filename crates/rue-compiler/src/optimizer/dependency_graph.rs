use std::collections::{HashMap, HashSet};

use indexmap::{IndexMap, IndexSet};

use crate::{
    hir::Hir,
    symbol::{Const, Function, Let, Symbol},
    ty::{FunctionType, Rest},
    Database, EnvironmentId, HirId, ScopeId, SymbolId,
};

use super::environment::Environment;

#[derive(Debug, Default)]
pub struct DependencyGraph {
    env: IndexMap<ScopeId, EnvironmentId>,
    symbol_usages: HashMap<SymbolId, usize>,
}

impl DependencyGraph {
    pub fn env(&self, scope_id: ScopeId) -> EnvironmentId {
        self.env[&scope_id]
    }

    pub fn visited_scopes(&self) -> Vec<ScopeId> {
        self.env.keys().copied().collect()
    }

    pub fn symbol_usages(&self, symbol_id: SymbolId) -> usize {
        *self.symbol_usages.get(&symbol_id).unwrap_or(&0)
    }
}

pub struct GraphTraversal<'a> {
    db: &'a mut Database,
    graph: DependencyGraph,
    edges: HashMap<ScopeId, IndexSet<ScopeId>>,
}

impl<'a> GraphTraversal<'a> {
    pub fn new(db: &'a mut Database) -> Self {
        Self {
            db,
            graph: DependencyGraph::default(),
            edges: HashMap::new(),
        }
    }

    pub fn build_graph(mut self, exported_symbols: &[SymbolId]) -> DependencyGraph {
        for &symbol_id in exported_symbols {
            self.compute_edges(symbol_id);
        }
        for &symbol_id in exported_symbols {
            self.visit_entrypoint(symbol_id);
        }
        self.graph
    }

    fn propagate_capture(
        &mut self,
        scope_id: ScopeId,
        symbol_id: SymbolId,
        visited: &mut HashSet<ScopeId>,
    ) {
        // Prevent infinite recursion.
        if !visited.insert(scope_id) {
            return;
        }

        let symbol = self.db.symbol(symbol_id).clone();
        let is_local = self.db.scope(scope_id).is_local(symbol_id);

        if is_local && symbol.is_definition() {
            self.db.env_mut(self.graph.env[&scope_id]).define(symbol_id);
        } else if !is_local && symbol.is_capturable() {
            self.db
                .env_mut(self.graph.env[&scope_id])
                .capture(symbol_id);

            for dependent in self.edges[&scope_id].clone() {
                self.propagate_capture(dependent, symbol_id, visited);
            }
        }
    }

    fn visit_entrypoint(&mut self, symbol_id: SymbolId) {
        let Symbol::Function(Function {
            scope_id, hir_id, ..
        }) = self.db.symbol(symbol_id).clone()
        else {
            unreachable!();
        };

        self.visit_hir(scope_id, hir_id, &mut HashSet::new());
    }

    fn visit_hir(
        &mut self,
        scope_id: ScopeId,
        hir_id: HirId,
        visited: &mut HashSet<(ScopeId, HirId)>,
    ) {
        if !visited.insert((scope_id, hir_id)) {
            return;
        }

        match self.db.hir(hir_id).clone() {
            Hir::Atom(_) | Hir::Unknown => {}
            Hir::PubkeyForExp(hir_id)
            | Hir::Sha256(hir_id)
            | Hir::Strlen(hir_id)
            | Hir::First(hir_id)
            | Hir::Rest(hir_id)
            | Hir::Not(hir_id)
            | Hir::IsCons(hir_id) => self.visit_hir(scope_id, hir_id, visited),
            Hir::Raise(hir_id) => {
                if let Some(hir_id) = hir_id {
                    self.visit_hir(scope_id, hir_id, visited);
                }
            }
            Hir::Pair(first, rest) => {
                self.visit_hir(scope_id, first, visited);
                self.visit_hir(scope_id, rest, visited);
            }
            Hir::FunctionCall { callee, args, .. } => {
                self.visit_hir(scope_id, callee, visited);
                for arg in args {
                    self.visit_hir(scope_id, arg, visited);
                }
            }
            Hir::If {
                condition,
                then_block,
                else_block,
            } => {
                self.visit_hir(scope_id, condition, visited);
                self.visit_hir(scope_id, then_block, visited);
                self.visit_hir(scope_id, else_block, visited);
            }
            Hir::BinaryOp { lhs, rhs, .. } => {
                self.visit_hir(scope_id, lhs, visited);
                self.visit_hir(scope_id, rhs, visited);
            }
            Hir::Definition {
                hir_id, scope_id, ..
            } => {
                self.visit_hir(scope_id, hir_id, visited);
            }
            Hir::Reference(symbol_id) => {
                self.graph
                    .symbol_usages
                    .entry(symbol_id)
                    .and_modify(|count| *count += 1)
                    .or_insert(1);

                self.propagate_capture(scope_id, symbol_id, &mut HashSet::new());

                match self.db.symbol(symbol_id).clone() {
                    Symbol::Unknown => unreachable!(),
                    Symbol::Function(fun) => self.visit_hir(fun.scope_id, fun.hir_id, visited),
                    Symbol::Parameter { .. } => {}
                    Symbol::Let(Let { hir_id, .. }) | Symbol::Const(Const { hir_id, .. }) => {
                        self.visit_hir(scope_id, hir_id, visited);
                    }
                }
            }
        }
    }

    fn compute_edges(&mut self, symbol_id: SymbolId) {
        match self.db.symbol(symbol_id).clone() {
            Symbol::Function(fun) => {
                // Add the function scope to the graph.
                self.edges.entry(fun.scope_id).or_default();

                // Compute the function's edges.
                self.compute_function_edges(fun.scope_id, fun.hir_id, fun.ty, &mut HashSet::new());
            }
            Symbol::Const(..) => {}
            Symbol::Let(..) | Symbol::Parameter(..) | Symbol::Unknown => {
                unreachable!()
            }
        }
    }

    fn compute_hir_edges(
        &mut self,
        scope_id: ScopeId,
        hir_id: HirId,
        visited: &mut HashSet<(ScopeId, HirId)>,
    ) {
        if !visited.insert((scope_id, hir_id)) {
            return;
        }

        match self.db.hir(hir_id).clone() {
            Hir::Atom(_) | Hir::Unknown => {}
            Hir::PubkeyForExp(hir_id)
            | Hir::Sha256(hir_id)
            | Hir::Strlen(hir_id)
            | Hir::First(hir_id)
            | Hir::Rest(hir_id)
            | Hir::Not(hir_id)
            | Hir::IsCons(hir_id) => self.compute_hir_edges(scope_id, hir_id, visited),
            Hir::Raise(hir_id) => {
                if let Some(hir_id) = hir_id {
                    self.compute_hir_edges(scope_id, hir_id, visited);
                }
            }
            Hir::Pair(first, rest) => {
                self.compute_hir_edges(scope_id, first, visited);
                self.compute_hir_edges(scope_id, rest, visited);
            }
            Hir::FunctionCall { callee, args, .. } => {
                self.compute_hir_edges(scope_id, callee, visited);

                for arg in args {
                    self.compute_hir_edges(scope_id, arg, visited);
                }
            }
            Hir::If {
                condition,
                then_block,
                else_block,
            } => {
                self.compute_hir_edges(scope_id, condition, visited);
                self.compute_hir_edges(scope_id, then_block, visited);
                self.compute_hir_edges(scope_id, else_block, visited);
            }
            Hir::BinaryOp { lhs, rhs, .. } => {
                self.compute_hir_edges(scope_id, lhs, visited);
                self.compute_hir_edges(scope_id, rhs, visited);
            }
            Hir::Definition {
                hir_id,
                scope_id: child_scope_id,
                ..
            } => {
                // Add the scope to the graph.
                if !self
                    .edges
                    .entry(child_scope_id)
                    .or_default()
                    .insert(scope_id)
                {
                    return;
                }

                let env_id = self.graph.env[&scope_id];

                let mut environment = Environment::binding(env_id);
                for symbol_id in self.db.scope(child_scope_id).local_symbols() {
                    if self.db.symbol(symbol_id).is_definition() {
                        environment.define(symbol_id);
                    }
                }

                let child_env_id = self.db.alloc_env(environment);
                self.graph.env.insert(child_scope_id, child_env_id);

                log::debug!("Scope id {:?} => {:?}", child_scope_id, child_env_id);

                self.compute_hir_edges(child_scope_id, hir_id, visited);

                log::debug!(
                    "Adding parent {:?} as edge to child {:?}",
                    self.graph.env[&scope_id],
                    self.graph.env[&child_scope_id]
                );
            }
            Hir::Reference(symbol_id) => {
                self.graph
                    .symbol_usages
                    .entry(symbol_id)
                    .and_modify(|count| *count += 1)
                    .or_insert(1);

                match self.db.symbol(symbol_id).clone() {
                    Symbol::Unknown => unreachable!(),
                    Symbol::Function(fun) => {
                        // Add the new scope to the graph.
                        // The parent scope depends on the new scope's captures.
                        self.edges.entry(fun.scope_id).or_default().insert(scope_id);

                        // Compute the function's edges.
                        self.compute_function_edges(fun.scope_id, fun.hir_id, fun.ty, visited);
                    }
                    Symbol::Parameter { .. } => {}
                    Symbol::Let(Let { hir_id, .. }) | Symbol::Const(Const { hir_id, .. }) => {
                        self.compute_hir_edges(scope_id, hir_id, visited);
                    }
                }
            }
        }
    }

    fn compute_function_edges(
        &mut self,
        scope_id: ScopeId,
        hir_id: HirId,
        ty: FunctionType,
        visited: &mut HashSet<(ScopeId, HirId)>,
    ) {
        // Initialize the new scope's environment.
        // We need to include the parameters and whether or not the scope ends in varargs.
        let parameters = self
            .db
            .scope(scope_id)
            .local_symbols()
            .into_iter()
            .filter(|symbol| self.db.symbol(*symbol).is_parameter())
            .collect();

        if !self.graph.env.contains_key(&scope_id) {
            let environment_id = self.db.alloc_env(Environment::function(
                parameters,
                ty.rest == Rest::Parameter,
            ));

            self.graph.env.insert(scope_id, environment_id);
        }

        // Compute the new scope's edges.
        self.compute_hir_edges(scope_id, hir_id, visited);
    }
}
