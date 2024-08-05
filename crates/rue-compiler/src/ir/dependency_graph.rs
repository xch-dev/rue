use std::hash::{Hash, Hasher};

use ahash::AHasher;
use indexmap::{IndexMap, IndexSet};
use rowan::TextRange;

use crate::{
    Database, Environment, EnvironmentId, ErrorKind, Function, Hir, HirId, Module, ScopeId, Symbol,
    SymbolId,
};

#[derive(Debug, Default, Clone)]
pub struct DependencyGraph {
    environments: IndexMap<ScopeId, EnvironmentId>,
    other_visited_scopes: IndexSet<ScopeId>,
    parent_scopes: IndexMap<ScopeId, IndexSet<ScopeId>>,
    symbol_references: IndexMap<SymbolId, usize>,
    references: IndexMap<SymbolId, IndexSet<SymbolId>>,
}

impl DependencyGraph {
    pub fn scopes(&self) -> Vec<ScopeId> {
        self.environments
            .keys()
            .copied()
            .chain(self.other_visited_scopes.iter().copied())
            .collect()
    }

    pub fn symbol_references(&self, symbol_id: SymbolId) -> usize {
        self.symbol_references
            .get(&symbol_id)
            .copied()
            .unwrap_or_default()
    }

    pub fn all_references(&self, symbol_id: SymbolId) -> IndexSet<SymbolId> {
        let mut visited = IndexSet::new();
        let mut stack = vec![symbol_id];

        while let Some(symbol_id) = stack.pop() {
            if !visited.insert(symbol_id) {
                continue;
            }

            if let Some(references) = self.references.get(&symbol_id) {
                for &reference in references {
                    stack.push(reference);
                }
            }
        }

        visited.shift_remove(&symbol_id);
        visited
    }

    pub fn environment_id(&self, scope_id: ScopeId) -> EnvironmentId {
        self.environments[&scope_id]
    }
}

impl DependencyGraph {
    pub fn build(db: &mut Database, entrypoint: &Module) -> Self {
        let mut builder = GraphBuilder {
            db,
            graph: Self::default(),
            symbol_stack: IndexSet::new(),
            visited: IndexSet::new(),
            inline_parameter_stack: Vec::new(),
        };
        builder.walk_module(entrypoint);
        assert!(builder.symbol_stack.is_empty(), "symbol stack is not empty");
        builder.visited.clear();
        builder.ref_module(entrypoint);
        builder.graph
    }
}

struct GraphBuilder<'a> {
    db: &'a mut Database,
    graph: DependencyGraph,
    symbol_stack: IndexSet<SymbolId>,
    visited: IndexSet<(ScopeId, HirId, u64)>,
    inline_parameter_stack: Vec<IndexMap<SymbolId, Vec<HirId>>>,
}

impl<'a> GraphBuilder<'a> {
    fn hash_param_map(&self) -> u64 {
        let mut hasher = AHasher::default();
        let mut sum = IndexMap::new();
        for map in &self.inline_parameter_stack {
            for (symbol_id, args) in map {
                sum.insert(*symbol_id, args.clone());
            }
        }
        sum.sort_by_cached_key(|k, _| *k);
        for (symbol_id, mut args) in sum {
            symbol_id.hash(&mut hasher);
            args.sort();
            for arg in args {
                arg.hash(&mut hasher);
            }
        }
        hasher.finish()
    }

    fn walk_module(&mut self, module: &Module) {
        if self.graph.environments.contains_key(&module.scope_id) {
            log::debug!(
                "Skipping module {}, since it's already been visited.",
                self.db.dbg_scope(module.scope_id)
            );
            return;
        }

        let environment_id = self.db.alloc_env(Environment::default());
        self.graph
            .environments
            .insert(module.scope_id, environment_id);

        for &symbol_id in &module.exported_symbols {
            self.walk_export(module.scope_id, symbol_id);
        }
    }

    fn walk_export(&mut self, scope_id: ScopeId, symbol_id: SymbolId) {
        match self.db.symbol(symbol_id).clone() {
            Symbol::Unknown | Symbol::Let(..) | Symbol::Parameter(..) => unreachable!(),
            Symbol::Module(module) => {
                self.walk_module(&module);
            }
            Symbol::Function(function) => {
                self.graph
                    .parent_scopes
                    .entry(function.scope_id)
                    .or_default();

                self.walk_function(&function);
            }
            Symbol::InlineFunction(function) => {
                self.graph.other_visited_scopes.insert(function.scope_id);
            }
            Symbol::Const(constant) | Symbol::InlineConst(constant) => {
                self.walk_hir(scope_id, constant.hir_id);
            }
        }
    }

    fn walk_function(&mut self, function: &Function) {
        if self.graph.environments.contains_key(&function.scope_id) {
            log::debug!(
                "Skipping function {}, since it's already been visited.",
                self.db.dbg_scope(function.scope_id)
            );
            return;
        }

        let parameters = self
            .db
            .scope(function.scope_id)
            .local_symbols()
            .into_iter()
            .filter(|&symbol_id| matches!(self.db.symbol(symbol_id), Symbol::Parameter(_)))
            .collect();

        let environment_id = self
            .db
            .alloc_env(Environment::function(parameters, !function.nil_terminated));

        self.graph
            .environments
            .insert(function.scope_id, environment_id);

        self.walk_hir(function.scope_id, function.hir_id);
    }

    fn walk_hir(&mut self, scope_id: ScopeId, hir_id: HirId) {
        if !self
            .visited
            .insert((scope_id, hir_id, self.hash_param_map()))
        {
            return;
        }

        match self.db.hir(hir_id).clone() {
            Hir::Unknown | Hir::Atom(..) => {}
            Hir::Op(_op, hir_id) => {
                self.walk_hir(scope_id, hir_id);
            }
            Hir::Raise(hir_id) => {
                if let Some(hir_id) = hir_id {
                    self.walk_hir(scope_id, hir_id);
                }
            }
            Hir::Pair(first, rest) => {
                self.walk_hir(scope_id, first);
                self.walk_hir(scope_id, rest);
            }
            Hir::FunctionCall(callee, args, _varargs) => {
                self.walk_function_call(scope_id, callee, args);
            }
            Hir::If(condition, then_block, else_block) => {
                self.walk_hir(scope_id, condition);
                self.walk_hir(scope_id, then_block);
                self.walk_hir(scope_id, else_block);
            }
            Hir::BinaryOp(_op, lhs, rhs) => {
                self.walk_hir(scope_id, lhs);
                self.walk_hir(scope_id, rhs);
            }
            Hir::Substr(value, start, end) => {
                self.walk_hir(scope_id, value);
                self.walk_hir(scope_id, start);
                self.walk_hir(scope_id, end);
            }
            Hir::Definition(child_scope_id, hir_id) => {
                self.walk_definition(scope_id, child_scope_id, hir_id);
            }
            Hir::Reference(symbol_id, text_range) => {
                self.walk_reference(scope_id, symbol_id, text_range);
            }
        }
    }

    fn walk_function_call(&mut self, scope_id: ScopeId, callee: HirId, args: Vec<HirId>) {
        if let Hir::Reference(symbol_id, text_range) = self.db.hir(callee) {
            if let Symbol::InlineFunction(function) = self.db.symbol(*symbol_id).clone() {
                let symbol_id = *symbol_id;
                if !self.symbol_stack.insert(symbol_id) {
                    self.db
                        .error(ErrorKind::RecursiveInlineFunctionCall, *text_range);
                }
                self.walk_inline_function_call(scope_id, &function, &args);
                self.symbol_stack.shift_remove(&symbol_id);
                return;
            }
        }

        self.walk_hir(scope_id, callee);
        for arg in args {
            self.walk_hir(scope_id, arg);
        }
    }

    fn walk_inline_function_call(
        &mut self,
        scope_id: ScopeId,
        function: &Function,
        args: &[HirId],
    ) {
        self.graph.other_visited_scopes.insert(function.scope_id);

        let params: Vec<SymbolId> = self
            .db
            .scope(function.scope_id)
            .local_symbols()
            .iter()
            .copied()
            .filter(|symbol_id| matches!(self.db.symbol(*symbol_id), Symbol::Parameter(..)))
            .collect();

        let mut param_map = IndexMap::new();

        for (i, &symbol_id) in params.iter().enumerate() {
            if i + 1 != params.len() || function.nil_terminated {
                param_map.insert(symbol_id, vec![args[i]]);
                continue;
            }
            param_map.insert(symbol_id, args[i..].to_vec());
        }

        self.inline_parameter_stack.push(param_map);
        self.walk_hir(scope_id, function.hir_id);
        self.inline_parameter_stack.pop().unwrap();
    }

    fn walk_definition(
        &mut self,
        parent_scope_id: ScopeId,
        child_scope_id: ScopeId,
        hir_id: HirId,
    ) {
        if self.graph.environments.contains_key(&child_scope_id) {
            log::debug!(
                "Skipping definition {}, since it's already been visited.",
                self.db.dbg_scope(child_scope_id)
            );
            return;
        }

        self.graph
            .parent_scopes
            .entry(child_scope_id)
            .or_default()
            .insert(parent_scope_id);

        let parent_env_id = self.graph.environments[&parent_scope_id];
        let child_env_id = self.db.alloc_env(Environment::binding(parent_env_id));
        self.graph.environments.insert(child_scope_id, child_env_id);
        self.walk_hir(child_scope_id, hir_id);
    }

    fn walk_reference(&mut self, scope_id: ScopeId, symbol_id: SymbolId, text_range: TextRange) {
        for map in self.inline_parameter_stack.iter().rev() {
            if let Some(args) = map.get(&symbol_id).cloned() {
                for arg in args {
                    self.walk_hir(scope_id, arg);
                }
                return;
            }
        }

        let symbol = self.db.symbol(symbol_id).clone();

        for &key in &self.symbol_stack {
            self.graph
                .references
                .entry(key)
                .or_default()
                .insert(symbol_id);
        }

        if !self.symbol_stack.insert(symbol_id) {
            let error = match symbol {
                Symbol::Const(..) => Some(ErrorKind::RecursiveConstantReference),
                Symbol::InlineConst(..) => Some(ErrorKind::RecursiveInlineConstantReference),
                _ => None,
            };

            if let Some(error) = error {
                self.db.error(error, text_range);
                return;
            }
        }

        match symbol {
            Symbol::Unknown | Symbol::Module(..) | Symbol::InlineFunction(..) => unreachable!(),
            Symbol::Function(function) => {
                self.graph
                    .parent_scopes
                    .entry(function.scope_id)
                    .or_default()
                    .insert(scope_id);

                self.walk_function(&function);
            }
            Symbol::Parameter(..) => {}
            Symbol::Let(value) | Symbol::Const(value) | Symbol::InlineConst(value) => {
                self.walk_hir(scope_id, value.hir_id);
            }
        }

        self.symbol_stack.shift_remove(&symbol_id);

        if let Some(references) = self.graph.references.get(&symbol_id).cloned() {
            for reference in references {
                if !self
                    .graph
                    .references
                    .get(&reference)
                    .cloned()
                    .unwrap_or_default()
                    .contains(&symbol_id)
                {
                    continue;
                }

                if let Symbol::Function(..) = self.db.symbol(reference) {
                    let error = match self.db.symbol(symbol_id) {
                        Symbol::Const(..) => Some(ErrorKind::RecursiveConstantReference),
                        Symbol::InlineConst(..) => {
                            Some(ErrorKind::RecursiveInlineConstantReference)
                        }
                        Symbol::InlineFunction(..) => Some(ErrorKind::RecursiveInlineFunctionCall),
                        _ => None,
                    };

                    if let Some(error) = error {
                        self.db.error(error, text_range);
                        return;
                    }
                }
            }
        }
    }

    fn propagate_capture(
        &mut self,
        scope_id: ScopeId,
        symbol_id: SymbolId,
        visited_scopes: &mut IndexSet<ScopeId>,
    ) {
        if !visited_scopes.insert(scope_id) {
            return;
        }

        let capturable = self.db.symbol(symbol_id).is_capturable();
        let definable = self.db.symbol(symbol_id).is_definable();
        let is_local = self.db.scope(scope_id).is_local(symbol_id);

        if is_local && definable {
            self.db
                .env_mut(self.graph.environments[&scope_id])
                .define(symbol_id);
        } else if !is_local && capturable {
            self.db
                .env_mut(self.graph.environments[&scope_id])
                .capture(symbol_id);

            for parent_scope_id in self.graph.parent_scopes[&scope_id].clone() {
                self.propagate_capture(parent_scope_id, symbol_id, visited_scopes);
            }
        }

        visited_scopes.pop().unwrap();
    }

    fn ref_module(&mut self, module: &Module) {
        for &symbol_id in &module.exported_symbols {
            self.ref_symbol(module.scope_id, symbol_id);
        }
    }

    fn ref_symbol(&mut self, scope_id: ScopeId, symbol_id: SymbolId) {
        match self.db.symbol(symbol_id).clone() {
            Symbol::Unknown | Symbol::InlineFunction(..) => unreachable!(),
            Symbol::Module(module) => {
                self.ref_module(&module);
            }
            Symbol::Function(function) => {
                self.ref_hir(function.scope_id, function.hir_id);
            }
            Symbol::Parameter(..) => {}
            Symbol::Let(value) | Symbol::Const(value) | Symbol::InlineConst(value) => {
                self.ref_hir(scope_id, value.hir_id);
            }
        }
    }

    fn ref_hir(&mut self, scope_id: ScopeId, hir_id: HirId) {
        if !self
            .visited
            .insert((scope_id, hir_id, self.hash_param_map()))
        {
            return;
        }

        match self.db.hir(hir_id).clone() {
            Hir::Unknown | Hir::Atom(..) => {}
            Hir::Op(_op, hir_id) => {
                self.ref_hir(scope_id, hir_id);
            }
            Hir::Raise(hir_id) => {
                if let Some(hir_id) = hir_id {
                    self.ref_hir(scope_id, hir_id);
                }
            }
            Hir::Pair(first, rest) => {
                self.ref_hir(scope_id, first);
                self.ref_hir(scope_id, rest);
            }
            Hir::FunctionCall(callee, args, _varargs) => {
                self.ref_function_call(scope_id, callee, args);
            }
            Hir::If(condition, then_block, else_block) => {
                self.ref_hir(scope_id, condition);
                self.ref_hir(scope_id, then_block);
                self.ref_hir(scope_id, else_block);
            }
            Hir::BinaryOp(_op, lhs, rhs) => {
                self.ref_hir(scope_id, lhs);
                self.ref_hir(scope_id, rhs);
            }
            Hir::Substr(value, start, end) => {
                self.ref_hir(scope_id, value);
                self.ref_hir(scope_id, start);
                self.ref_hir(scope_id, end);
            }
            Hir::Definition(scope_id, hir_id) => {
                self.ref_hir(scope_id, hir_id);
            }
            Hir::Reference(symbol_id, ..) => {
                self.resolve_reference(scope_id, symbol_id);
            }
        }
    }

    fn resolve_reference(&mut self, scope_id: ScopeId, symbol_id: SymbolId) {
        for map in self.inline_parameter_stack.iter().rev() {
            if let Some(args) = map.get(&symbol_id).cloned() {
                self.graph
                    .symbol_references
                    .entry(symbol_id)
                    .and_modify(|usages| *usages += 1)
                    .or_insert(1);

                for arg in args {
                    self.ref_hir(scope_id, arg);
                }

                return;
            }
        }

        let symbol = self.db.symbol(symbol_id).clone();

        if self.db.symbol(symbol_id).is_constant() && !self.symbol_stack.insert(symbol_id) {
            return;
        }

        self.graph
            .symbol_references
            .entry(symbol_id)
            .and_modify(|usages| *usages += 1)
            .or_insert(1);

        self.propagate_capture(scope_id, symbol_id, &mut IndexSet::new());

        match symbol {
            Symbol::Unknown | Symbol::Module(..) | Symbol::InlineFunction(..) => {
                unreachable!()
            }
            Symbol::Let(value) | Symbol::Const(value) | Symbol::InlineConst(value) => {
                self.ref_hir(scope_id, value.hir_id);
            }
            Symbol::Function(function) => {
                self.ref_hir(function.scope_id, function.hir_id);
            }
            Symbol::Parameter(..) => {}
        }

        self.symbol_stack.shift_remove(&symbol_id);
    }

    fn ref_function_call(&mut self, scope_id: ScopeId, callee: HirId, args: Vec<HirId>) {
        if let Hir::Reference(symbol_id, ..) = self.db.hir(callee) {
            if let Symbol::InlineFunction(function) = self.db.symbol(*symbol_id).clone() {
                self.ref_inline_function_call(scope_id, *symbol_id, &function, &args);
                return;
            }
        }

        self.ref_hir(scope_id, callee);
        for arg in args {
            self.ref_hir(scope_id, arg);
        }
    }

    fn ref_inline_function_call(
        &mut self,
        scope_id: ScopeId,
        symbol_id: SymbolId,
        function: &Function,
        args: &[HirId],
    ) {
        self.graph
            .symbol_references
            .entry(symbol_id)
            .and_modify(|usages| *usages += 1)
            .or_insert(1);

        let params: Vec<SymbolId> = self
            .db
            .scope(function.scope_id)
            .local_symbols()
            .iter()
            .copied()
            .filter(|symbol_id| matches!(self.db.symbol(*symbol_id), Symbol::Parameter(..)))
            .collect();

        let mut param_map = IndexMap::new();

        for (i, &symbol_id) in params.iter().enumerate() {
            if i + 1 != params.len() || function.nil_terminated {
                param_map.insert(symbol_id, vec![args[i]]);
                continue;
            }
            param_map.insert(symbol_id, args[i..].to_vec());
        }

        self.inline_parameter_stack.push(param_map);
        self.ref_hir(scope_id, function.hir_id);
        self.inline_parameter_stack.pop().unwrap();
    }
}
