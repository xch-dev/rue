use std::collections::{HashMap, HashSet};

use indexmap::{IndexMap, IndexSet};

use crate::{
    hir::Hir,
    symbol::{Const, Function, Let, Symbol},
    ty::{FunctionType, Rest},
    Database, EnvironmentId, HirId, ScopeId, SymbolId,
};

use super::Environment;

/// Responsible for converting the compiler's `Scope` objects to the lower level `Environment`.
/// It does this by determining which scopes depend on each other,
/// then capturing symbols from higher scopes into lower ones.
#[derive(Debug, Default)]
pub struct DependencyGraph {
    env: IndexMap<ScopeId, EnvironmentId>,
    symbol_usages: HashMap<SymbolId, usize>,
}

impl DependencyGraph {
    pub fn build(db: &mut Database, exported_symbols: &[SymbolId]) -> Self {
        GraphTraversal::new(db).build(exported_symbols)
    }

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

struct GraphTraversal<'a> {
    db: &'a mut Database,
    graph: DependencyGraph,
    edges: HashMap<ScopeId, IndexSet<ScopeId>>,
}

impl<'a> GraphTraversal<'a> {
    fn new(db: &'a mut Database) -> Self {
        Self {
            db,
            graph: DependencyGraph::default(),
            edges: HashMap::new(),
        }
    }

    fn build(mut self, exported_symbols: &[SymbolId]) -> DependencyGraph {
        for &symbol_id in exported_symbols {
            self.compute_edges(symbol_id);
        }
        for &symbol_id in exported_symbols {
            self.visit_entrypoint(symbol_id);
        }
        self.graph
    }

    /// Resolves a reference by capturing the symbol if it's not defined in the current scope.
    /// If it's not capturable (for example, an inline constant), it does not get captured.
    /// When captured, it will be propogated to each of the scopes for which this scope depends on.
    /// This is done to make sure that somewhere in the environment chain, the symbol is actually defined.
    /// For inline functions, for example, it's possible that multiple scopes will need to capture the same symbol.
    fn propagate_capture(
        &mut self,
        scope_id: ScopeId,
        symbol_id: SymbolId,
        visited: &mut HashSet<ScopeId>,
    ) {
        // This is done to prevent stack overflow errors.
        // If a scope has already been visited, we don't need to visit it again.
        if !visited.insert(scope_id) {
            return;
        }

        let symbol = self.db.symbol(symbol_id).clone();
        let is_local = self.db.scope(scope_id).is_local(symbol_id);

        if is_local && symbol.is_definition() {
            // If the symbol was originally defined in the current scope,
            // we can mark it as defined in the current environment too.
            // Now we don't need to capture it any further, since we're at the origin.
            self.db.env_mut(self.graph.env[&scope_id]).define(symbol_id);
        } else if !is_local && symbol.is_capturable() {
            // Otherwise, the symbol must be captured if possible.
            self.db
                .env_mut(self.graph.env[&scope_id])
                .capture(symbol_id);

            // Propogate the capture to each of the scopes we depend on.
            for dependent in self.edges[&scope_id].clone() {
                self.propagate_capture(dependent, symbol_id, visited);
            }
        }
    }

    /// TODO: This function will eventually be phased out in favor of a more general solution.
    /// Everything that gets exported (or `main`) is considered an entrypoint.
    /// Entrypoints are the starting point for the traversal of the dependency graph.
    /// Everything else is considered unused if it isn't reached by an entrypoint.
    fn visit_entrypoint(&mut self, symbol_id: SymbolId) {
        // Currently this only supports functions and inline functions.
        // This is not ideal.
        let Symbol::Function(Function {
            scope_id, hir_id, ..
        }) = self.db.symbol(symbol_id).clone()
        else {
            unreachable!();
        };

        self.visit_hir(scope_id, hir_id, &mut HashSet::new());
    }

    /// Visits a HIR node and all of its children.
    /// We assume that all references are appropriately included within the HIR.
    /// Otherwise, symbols will be considered unused.
    /// Perhaps this can be improved later, but this is sufficient for most cases.
    fn visit_hir(
        &mut self,
        scope_id: ScopeId,
        hir_id: HirId,
        visited: &mut HashSet<(ScopeId, HirId)>,
    ) {
        // Don't revisit this specific combination of scope and HIR.
        // This prevents stack overflow errors for some edge cases.
        if !visited.insert((scope_id, hir_id)) {
            return;
        }

        match self.db.hir(hir_id).clone() {
            // We can't rely on files being valid, so we ignore unknown HIR.
            Hir::Unknown => {}

            // An atom doesn't depend on any other HIR nodes.
            Hir::Atom(_bytes) => {}

            // These operators each depend on a single child HIR node.
            Hir::PubkeyForExp(hir_id)
            | Hir::Sha256(hir_id)
            | Hir::Strlen(hir_id)
            | Hir::First(hir_id)
            | Hir::Rest(hir_id)
            | Hir::Not(hir_id)
            | Hir::IsCons(hir_id) => self.visit_hir(scope_id, hir_id, visited),

            // These depend on multiple HIR nodes, in order.
            // This ensures that every part of the HIR is visited.
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

            // A definition consists of a new scope and an HIR node for the rest of the body.
            // TODO: Should this add the scope id to the graph?
            Hir::Definition {
                hir_id, scope_id, ..
            } => self.visit_hir(scope_id, hir_id, visited),

            // Resolves a reference to a symbol. This doesn't have any direct child HIR nodes.
            Hir::Reference(symbol_id) => self.visit_reference(scope_id, symbol_id, visited),
        }
    }

    /// Resolves a reference to a symbol and increments a counter tracking usages.
    /// Because we don't want to define or capture symbols which are never used,
    /// this has been deferred until now.
    fn visit_reference(
        &mut self,
        scope_id: ScopeId,
        symbol_id: SymbolId,
        visited: &mut HashSet<(ScopeId, HirId)>,
    ) {
        self.graph
            .symbol_usages
            .entry(symbol_id)
            .and_modify(|count| *count += 1)
            .or_insert(1);

        self.propagate_capture(scope_id, symbol_id, &mut HashSet::new());

        match self.db.symbol(symbol_id).clone() {
            // It's not possible to visit unknown symbols,
            // this would be a compiler bug if it were reached.
            Symbol::Unknown => {
                unreachable!(
                    "Unknown symbol {} in scope {}",
                    self.db.dbg_symbol(symbol_id),
                    self.db.dbg_scope(scope_id)
                )
            }

            // TODO: Should these be visited in a different scope if they aren't inline?
            Symbol::Let(Let { hir_id, .. }) | Symbol::Const(Const { hir_id, .. }) => {
                self.visit_hir(scope_id, hir_id, visited);
            }

            // Functions are visited in the scope in which they are defined.
            // TODO: For inline functions, should this be visited in the current scope?
            Symbol::Function(fun) => self.visit_hir(fun.scope_id, fun.hir_id, visited),

            // Parameters don't need to be visited, since currently
            // they are just a reference to the environment.
            Symbol::Parameter(..) => {}
        }
    }

    fn compute_edges(&mut self, symbol_id: SymbolId) {
        match self.db.symbol(symbol_id).clone() {
            Symbol::Function(fun) => {
                // Add the function scope to the graph.
                self.edges.entry(fun.scope_id).or_default();

                // Compute the function's edges.
                self.compute_function_edges(fun.scope_id, fun.hir_id, &fun.ty, &mut HashSet::new());
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
                let child_env_id = self.db.alloc_env(Environment::binding(env_id));
                self.graph.env.insert(child_scope_id, child_env_id);

                self.compute_hir_edges(child_scope_id, hir_id, visited);
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
                        self.compute_function_edges(fun.scope_id, fun.hir_id, &fun.ty, visited);
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
        ty: &FunctionType,
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
