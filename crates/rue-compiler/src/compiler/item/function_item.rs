use rue_parser::{AstNode, FunctionItem};

use crate::{
    compiler::Compiler,
    hir::Hir,
    scope::Scope,
    symbol::{Function, Symbol},
    ty::{FunctionType, Rest, Type},
    ErrorKind, SymbolId,
};

impl Compiler<'_> {
    /// Define a function in the current scope.
    /// This does not compile the function body, but it creates a new scope for it.
    /// Parameter symbols are defined now in the inner function scope.
    /// The function body is compiled later to allow for forward references.
    pub fn declare_function_item(&mut self, function_item: &FunctionItem) -> SymbolId {
        // Add the symbol to the stack early so you can track type references.
        let symbol_id = self.db.alloc_symbol(Symbol::Unknown);
        self.symbol_stack.push(symbol_id);

        let scope_id = self.db.alloc_scope(Scope::default());
        self.scope_stack.push(scope_id);

        let mut generic_types = Vec::new();

        for generic_type in function_item
            .generic_types()
            .map(|generics| generics.idents())
            .unwrap_or_default()
        {
            let type_id = self.db.alloc_type(Type::Generic);

            if self.scope().ty(generic_type.text()).is_some() {
                self.db.error(
                    ErrorKind::DuplicateType(generic_type.text().to_string()),
                    generic_type.text_range(),
                );
            }

            self.scope_mut()
                .define_type(generic_type.to_string(), type_id);

            self.db.insert_type_token(type_id, generic_type);

            generic_types.push(type_id);
        }

        let mut param_types = Vec::new();
        let mut rest = Rest::Nil;

        let len = function_item.params().len();

        for (i, param) in function_item.params().into_iter().enumerate() {
            // Add the symbol to the stack early so you can track type references.
            let symbol_id = self.db.alloc_symbol(Symbol::Unknown);
            self.symbol_stack.push(symbol_id);

            let type_id = param
                .ty()
                .map_or(self.builtins.unknown, |ty| self.compile_type(ty));

            param_types.push(type_id);

            *self.db.symbol_mut(symbol_id) = Symbol::Parameter(type_id);

            if let Some(name) = param.name() {
                self.scope_mut().define_symbol(name.to_string(), symbol_id);
                self.db.insert_symbol_token(symbol_id, name);
            }

            if param.spread().is_some() {
                if i + 1 == len {
                    rest = Rest::Parameter;
                } else {
                    self.db
                        .error(ErrorKind::NonFinalSpread, param.syntax().text_range());
                }
            }

            self.symbol_stack.pop().unwrap();
        }

        let return_type = function_item
            .return_type()
            .map_or(self.builtins.unknown, |ty| self.compile_type(ty));

        self.scope_stack.pop().unwrap();

        let hir_id = self.db.alloc_hir(Hir::Unknown);

        let ty = FunctionType {
            generic_types,
            param_types,
            rest,
            return_type,
        };

        if function_item.inline().is_some() {
            *self.db.symbol_mut(symbol_id) = Symbol::InlineFunction(Function {
                scope_id,
                hir_id,
                ty,
            });
        } else {
            *self.db.symbol_mut(symbol_id) = Symbol::Function(Function {
                scope_id,
                hir_id,
                ty,
            });
        }

        if let Some(name) = function_item.name() {
            self.scope_mut().define_symbol(name.to_string(), symbol_id);
            self.db.insert_scope_token(scope_id, name.clone());
            self.db.insert_symbol_token(symbol_id, name);
        }

        self.symbol_stack.pop().unwrap()
    }

    /// Compiles the body of a function within the function's scope.
    pub fn compile_function_item(&mut self, function: &FunctionItem, symbol_id: SymbolId) {
        let Some(body) = function.body() else {
            return;
        };

        let (Symbol::Function(Function { scope_id, ty, .. })
        | Symbol::InlineFunction(Function { scope_id, ty, .. })) =
            self.db.symbol(symbol_id).clone()
        else {
            unreachable!();
        };

        // We don't care about explicit returns in this context.
        self.scope_stack.push(scope_id);
        self.allow_generic_inference_stack.push(false);
        let value = self.compile_block(&body, Some(ty.return_type)).0;
        self.allow_generic_inference_stack.pop().unwrap();
        self.scope_stack.pop().unwrap();

        // Ensure that the body is assignable to the return type.
        self.type_check(
            value.type_id,
            ty.return_type,
            function.body().unwrap().syntax().text_range(),
        );

        // We ignore type guards here for now.
        // Just set the function body HIR.
        let (Symbol::Function(Function { hir_id, .. })
        | Symbol::InlineFunction(Function { hir_id, .. })) = self.db.symbol_mut(symbol_id)
        else {
            unreachable!();
        };
        *hir_id = value.hir_id;
    }
}
