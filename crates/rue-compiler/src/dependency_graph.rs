use std::collections::{HashMap, HashSet};

use indexmap::{IndexMap, IndexSet};

use crate::{hir::Hir, symbol::Symbol, ty::FunctionType, Database, HirId, ScopeId, SymbolId};

#[derive(Debug, Default)]
pub struct Environment {
    defined_symbols: IndexSet<SymbolId>,
    captured_symbols: IndexSet<SymbolId>,
    parameters: IndexSet<SymbolId>,
    parent_scope: Option<ScopeId>,
    varargs: bool,
}

impl Environment {
    pub fn definitions(&self) -> Vec<SymbolId> {
        self.defined_symbols.iter().copied().collect()
    }

    pub fn captures(&self) -> Vec<SymbolId> {
        self.captured_symbols.iter().copied().collect()
    }

    pub fn varargs(&self) -> bool {
        self.varargs
    }

    pub fn parent_scope(&self) -> Option<ScopeId> {
        self.parent_scope
    }

    pub fn build(&self) -> Vec<SymbolId> {
        let mut symbol_ids = Vec::new();
        symbol_ids.extend(self.defined_symbols.iter().copied());

        if self.parent_scope.is_none() {
            symbol_ids.extend(self.captured_symbols.iter().copied());
        } else {
            assert!(self.parameters.is_empty());
        }

        symbol_ids.extend(self.parameters.iter().copied());
        symbol_ids
    }
}

#[derive(Default)]
pub struct DependencyGraph {
    env: IndexMap<ScopeId, Environment>,
    symbol_usages: HashMap<SymbolId, usize>,
}

impl DependencyGraph {
    pub fn env(&self, scope_id: ScopeId) -> &Environment {
        &self.env[&scope_id]
    }

    pub fn visited_scopes(&self) -> Vec<ScopeId> {
        self.env.keys().copied().collect()
    }

    pub fn symbol_usages(&self, symbol_id: SymbolId) -> usize {
        *self.symbol_usages.get(&symbol_id).unwrap_or(&0)
    }
}

pub struct GraphTraversal<'a> {
    db: &'a Database,
    graph: DependencyGraph,
    edges: HashMap<ScopeId, Vec<ScopeId>>,
}

impl<'a> GraphTraversal<'a> {
    pub fn new(db: &'a Database) -> Self {
        Self {
            db,
            graph: DependencyGraph::default(),
            edges: HashMap::new(),
        }
    }

    pub fn build_graph(mut self, main_function: SymbolId) -> DependencyGraph {
        self.compute_edges(main_function);
        self.visit_main(main_function);
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
            self.graph
                .env
                .get_mut(&scope_id)
                .unwrap()
                .defined_symbols
                .insert(symbol_id);
        } else if !is_local && symbol.is_capturable() {
            self.graph
                .env
                .get_mut(&scope_id)
                .unwrap()
                .captured_symbols
                .insert(symbol_id);

            for dependent in self.edges[&scope_id].clone() {
                self.propagate_capture(dependent, symbol_id, visited);
            }
        }
    }

    fn visit_main(&mut self, main_function: SymbolId) {
        let Symbol::Function {
            scope_id, hir_id, ..
        } = self.db.symbol(main_function).clone()
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
            Hir::FunctionCall { callee, args } => {
                self.visit_hir(scope_id, callee, visited);
                self.visit_hir(scope_id, args, visited);
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
            Hir::Scope {
                scope_id: new_scope_id,
                hir_id,
            } => {
                self.visit_hir(new_scope_id, hir_id, visited);
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
                    Symbol::Function {
                        scope_id: new_scope_id,
                        hir_id,
                        ..
                    } => self.visit_hir(new_scope_id, hir_id, visited),
                    Symbol::Parameter { .. } => {}
                    Symbol::LetBinding { hir_id, .. } | Symbol::ConstBinding { hir_id, .. } => {
                        self.visit_hir(scope_id, hir_id, visited);
                    }
                }
            }
        }
    }

    fn compute_edges(&mut self, main_function: SymbolId) {
        let Symbol::Function {
            scope_id,
            hir_id,
            ty,
            ..
        } = self.db.symbol(main_function).clone()
        else {
            unreachable!();
        };

        // Add the main function scope to the graph.
        self.edges.insert(scope_id, Vec::new());

        // Compute the main function's edges.
        self.compute_function_edges(scope_id, hir_id, ty, &mut HashSet::new());
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
            Hir::FunctionCall { callee, args } => {
                self.compute_hir_edges(scope_id, callee, visited);
                self.compute_hir_edges(scope_id, args, visited);
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
            Hir::Scope {
                scope_id: new_scope_id,
                hir_id,
            } => {
                // Add the new scope to the graph.
                // The parent scope depends on the new scope's captures.
                self.edges.entry(new_scope_id).or_default().push(scope_id);

                // Initialize the new scope's environment. We don't need to
                // build the captures for the new scope because the built
                // environment will use the parent scope's captures.
                self.graph.env.entry(new_scope_id).or_insert(Environment {
                    parent_scope: Some(scope_id),
                    ..Default::default()
                });

                // Compute the new scope's edges.
                self.compute_hir_edges(new_scope_id, hir_id, visited);
            }
            Hir::Reference(symbol_id) => match self.db.symbol(symbol_id).clone() {
                Symbol::Unknown => unreachable!(),
                Symbol::Function {
                    scope_id: new_scope_id,
                    hir_id,
                    ty,
                    ..
                } => {
                    // Add the new scope to the graph.
                    // The parent scope depends on the new scope's captures.
                    self.edges.entry(new_scope_id).or_default().push(scope_id);

                    // Compute the function's edges.
                    self.compute_function_edges(new_scope_id, hir_id, ty, visited);
                }
                Symbol::Parameter { .. } => {}
                Symbol::LetBinding { hir_id, .. } | Symbol::ConstBinding { hir_id, .. } => {
                    self.compute_hir_edges(scope_id, hir_id, visited);
                }
            },
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
        self.graph.env.insert(
            scope_id,
            Environment {
                varargs: ty.varargs(),
                parameters: self
                    .db
                    .scope(scope_id)
                    .local_symbols()
                    .into_iter()
                    .filter(|symbol| self.db.symbol(*symbol).is_parameter())
                    .collect(),
                ..Default::default()
            },
        );

        // Compute the new scope's edges.
        self.compute_hir_edges(scope_id, hir_id, visited);
    }
}
