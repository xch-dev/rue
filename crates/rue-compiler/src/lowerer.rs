use std::collections::HashMap;

use indexmap::IndexSet;

use crate::{
    dependency_graph::DependencyGraph,
    environment::Environment,
    hir::Hir,
    mir::Mir,
    symbol::{Function, Symbol},
    value::Rest,
    Database, EnvironmentId, HirId, MirId, ScopeId, SymbolId,
};

#[derive(Debug)]
pub struct Lowerer<'a> {
    db: &'a mut Database,
    graph: &'a DependencyGraph,
    inline_parameter_stack: Vec<HashMap<SymbolId, MirId>>,
}

impl<'a> Lowerer<'a> {
    pub fn new(db: &'a mut Database, graph: &'a DependencyGraph) -> Self {
        Self {
            db,
            graph,
            inline_parameter_stack: Vec::new(),
        }
    }

    pub fn lower_main(&mut self, main: SymbolId) -> (EnvironmentId, MirId) {
        let Symbol::Function(function) = self.db.symbol(main).clone() else {
            unreachable!();
        };

        let env_id = self.graph.environment_id(function.scope_id);
        let env = self.db.env_mut(env_id);
        for capture in env.captures() {
            env.define(capture);
            env.remove_capture(capture);
        }

        let definitions = self.db.env(env_id).definitions();
        let mir_id = self.lower_symbols(env_id, definitions, function.hir_id);
        (env_id, mir_id)
    }

    fn lower_hir(&mut self, env_id: EnvironmentId, hir_id: HirId) -> MirId {
        match self.db.hir(hir_id).clone() {
            Hir::Unknown => unreachable!(),
            Hir::Atom(atom) => self.db.alloc_mir(Mir::Atom(atom)),
            Hir::Pair(first, rest) => {
                let first = self.lower_hir(env_id, first);
                let rest = self.lower_hir(env_id, rest);
                self.db.alloc_mir(Mir::Pair(first, rest))
            }
            Hir::Op(op, hir_id) => {
                let mir_id = self.lower_hir(env_id, hir_id);
                self.db.alloc_mir(Mir::Op(op, mir_id))
            }
            Hir::BinaryOp(op, lhs, rhs) => {
                let lhs = self.lower_hir(env_id, lhs);
                let rhs = self.lower_hir(env_id, rhs);
                self.db.alloc_mir(Mir::BinaryOp(op, lhs, rhs))
            }
            Hir::Raise(value) => {
                let value = value.map(|hir_id| self.lower_hir(env_id, hir_id));
                self.db.alloc_mir(Mir::Raise(value))
            }
            Hir::Reference(symbol_id, ..) => self.lower_reference(env_id, symbol_id),
            Hir::FunctionCall(callee, args, varargs) => {
                self.lower_function_call(env_id, callee, &args, varargs)
            }
            Hir::If(condition, then_branch, else_branch) => {
                let condition = self.lower_hir(env_id, condition);
                let then_branch = self.lower_hir(env_id, then_branch);
                let else_branch = self.lower_hir(env_id, else_branch);
                self.db
                    .alloc_mir(Mir::If(condition, then_branch, else_branch))
            }
            Hir::Definition(scope_id, hir_id) => self.lower_definition(env_id, scope_id, hir_id),
        }
    }

    fn lower_reference(&mut self, env_id: EnvironmentId, symbol_id: SymbolId) -> MirId {
        for inline_parameter_map in self.inline_parameter_stack.iter().rev() {
            if let Some(mir_id) = inline_parameter_map.get(&symbol_id) {
                return *mir_id;
            }
        }

        match self.db.symbol(symbol_id).clone() {
            Symbol::Function(function) => self.lower_closure(symbol_id, &function),
            Symbol::InlineConst(constant) => self.lower_hir(env_id, constant.hir_id),
            Symbol::Let(binding) if self.graph.symbol_references(symbol_id) == 1 => {
                self.lower_hir(env_id, binding.hir_id)
            }
            Symbol::Let(..) | Symbol::Const(..) | Symbol::Parameter(..) => {
                self.lower_predefined_reference(symbol_id)
            }
            Symbol::Unknown | Symbol::Module(..) | Symbol::InlineFunction(..) => unreachable!(),
        }
    }

    fn lower_predefined_reference(&mut self, symbol_id: SymbolId) -> MirId {
        for inline_parameter_map in self.inline_parameter_stack.iter().rev() {
            if let Some(mir_id) = inline_parameter_map.get(&symbol_id) {
                return *mir_id;
            }
        }
        self.db.alloc_mir(Mir::Reference(symbol_id))
    }

    fn lower_closure(&mut self, symbol_id: SymbolId, function: &Function) -> MirId {
        let function_env_id = self.graph.environment_id(function.scope_id);
        let body = self.lower_predefined_reference(symbol_id);

        let mut captures = Vec::new();

        for symbol_id in self.db.env(function_env_id).definitions() {
            captures.push(self.lower_predefined_reference(symbol_id));
        }

        for symbol_id in self.db.env(function_env_id).captures() {
            captures.push(self.lower_predefined_reference(symbol_id));
        }

        self.db.alloc_mir(Mir::Closure(body, captures))
    }

    fn lower_definition(
        &mut self,
        parent_env_id: EnvironmentId,
        child_scope_id: ScopeId,
        hir_id: HirId,
    ) -> MirId {
        let child_env_id = self.graph.environment_id(child_scope_id);

        for symbol_id in self.db.env_mut(child_env_id).definitions() {
            let Symbol::Let(..) = self.db.symbol(symbol_id) else {
                continue;
            };

            if self.graph.symbol_references(symbol_id) == 1 {
                self.db.env_mut(child_env_id).remove_definition(symbol_id);
            }
        }

        let value = self.lower_hir(child_env_id, hir_id);
        let body = self.db.alloc_mir(Mir::Environment(child_env_id, value));

        let mut args = Vec::new();

        for symbol_id in self.db.env(child_env_id).definitions() {
            args.push(self.lower_symbol(parent_env_id, symbol_id));
        }

        self.db.alloc_mir(Mir::Curry(body, args))
    }

    fn lower_symbols(
        &mut self,
        mut env_id: EnvironmentId,
        mut definitions: IndexSet<SymbolId>,
        body: HirId,
    ) -> MirId {
        let mut groups = Vec::new();

        while !definitions.is_empty() {
            let symbol_ids = definitions
                .iter()
                .filter(|&symbol_id| {
                    let references: IndexSet<SymbolId> = self
                        .graph
                        .references(*symbol_id)
                        .into_iter()
                        .filter(|symbol_id| self.db.symbol(*symbol_id).is_constant())
                        .collect();

                    let dependencies = references.intersection(&definitions).count();

                    dependencies == 0
                        || (!matches!(self.db.symbol(*symbol_id), Symbol::Function(..))
                            && !self.db.symbol(*symbol_id).is_constant())
                })
                .copied()
                .collect::<Vec<_>>();

            let mut group = Vec::new();

            env_id = self.db.alloc_env(Environment::binding(env_id));

            for &symbol_id in &symbol_ids {
                self.db.env_mut(env_id).define(symbol_id);
                group.push(symbol_id);
            }

            groups.push((group, env_id));
            definitions.retain(|&symbol_id| !symbol_ids.contains(&symbol_id));
        }

        let mut body = self.lower_hir(env_id, body);

        for (symbol_ids, env_id) in groups.into_iter().rev() {
            let args = symbol_ids
                .into_iter()
                .map(|symbol_id| self.lower_symbol(env_id, symbol_id))
                .collect::<Vec<_>>();
            body = self.db.alloc_mir(Mir::Environment(env_id, body));
            body = self.db.alloc_mir(Mir::Curry(body, args));
        }

        body
    }

    fn lower_symbol(&mut self, env_id: EnvironmentId, symbol_id: SymbolId) -> MirId {
        match self.db.symbol(symbol_id).clone() {
            Symbol::Function(function) => self.lower_function(&function),
            Symbol::Const(constant) => self.lower_hir(env_id, constant.hir_id),
            Symbol::Let(binding) if self.graph.symbol_references(symbol_id) > 0 => {
                self.lower_hir(env_id, binding.hir_id)
            }
            Symbol::Unknown
            | Symbol::Module(..)
            | Symbol::Parameter(..)
            | Symbol::Let(..)
            | Symbol::InlineFunction(..)
            | Symbol::InlineConst(..) => unreachable!(),
        }
    }

    fn lower_function(&mut self, function: &Function) -> MirId {
        let function_env_id = self.graph.environment_id(function.scope_id);
        let body = self.lower_symbols(
            function_env_id,
            self.db.env(function_env_id).definitions(),
            function.hir_id,
        );
        let function = self.db.alloc_mir(Mir::Environment(function_env_id, body));
        self.db.alloc_mir(Mir::Quote(function))
    }

    fn lower_function_call(
        &mut self,
        env_id: EnvironmentId,
        callee: HirId,
        args: &[HirId],
        varargs: bool,
    ) -> MirId {
        if let Hir::Reference(symbol_id, ..) = self.db.hir(callee) {
            if let Symbol::InlineFunction(function) = self.db.symbol(*symbol_id).clone() {
                return self.lower_inline_function_call(env_id, &function, args, varargs);
            }
        }

        let mut mir_id = self.db.alloc_mir(Mir::Atom(Vec::new()));

        for (i, &arg) in args.iter().rev().enumerate() {
            let arg = self.lower_hir(env_id, arg);

            if i == 0 && varargs {
                mir_id = arg;
                continue;
            }

            mir_id = self.db.alloc_mir(Mir::Pair(arg, mir_id));
        }

        let (callee, captures) = self.lower_callee_and_captures(env_id, callee);

        for capture in captures.into_iter().rev() {
            mir_id = self.db.alloc_mir(Mir::Pair(capture, mir_id));
        }

        self.db.alloc_mir(Mir::Run(callee, mir_id))
    }

    fn lower_callee_and_captures(
        &mut self,
        env_id: EnvironmentId,
        hir_id: HirId,
    ) -> (MirId, Vec<MirId>) {
        let mut captures = Vec::new();

        let Hir::Reference(symbol_id, ..) = self.db.hir(hir_id).clone() else {
            return (self.lower_hir(env_id, hir_id), captures);
        };

        let Symbol::Function(function) = self.db.symbol(symbol_id).clone() else {
            return (self.lower_hir(env_id, hir_id), captures);
        };

        let function_env_id = self.graph.environment_id(function.scope_id);

        for symbol_id in self.db.env(function_env_id).captures() {
            captures.push(self.lower_predefined_reference(symbol_id));
        }

        (self.lower_predefined_reference(symbol_id), captures)
    }

    fn lower_inline_function_call(
        &mut self,
        env_id: EnvironmentId,
        function: &Function,
        args: &[HirId],
        varargs: bool,
    ) -> MirId {
        let function_env_id = self.graph.environment_id(function.scope_id);

        let params = self.db.env(function_env_id).parameters();

        let mut param_map = HashMap::new();

        for (i, &symbol_id) in params.iter().enumerate() {
            if i + 1 != params.len() || function.ty.rest != Rest::Spread {
                let mir_id = self.lower_hir(env_id, args[i]);
                param_map.insert(symbol_id, mir_id);
                continue;
            }

            let mut rest = self.db.alloc_mir(Mir::Atom(Vec::new()));

            for (i, &arg) in args[i..].iter().rev().enumerate() {
                if i == 0 && varargs {
                    rest = self.lower_hir(env_id, arg);
                } else {
                    let mir_id = self.lower_hir(env_id, arg);
                    rest = self.db.alloc_mir(Mir::Pair(mir_id, rest));
                }
            }

            param_map.insert(symbol_id, rest);
        }

        self.inline_parameter_stack.push(param_map);
        let result = self.lower_hir(env_id, function.hir_id);
        self.inline_parameter_stack.pop().unwrap();

        result
    }
}
