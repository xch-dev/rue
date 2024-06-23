use rue_parser::{AstNode, Path};

use crate::{
    compiler::Compiler,
    hir::Hir,
    symbol::{Const, Function, Let, Symbol},
    ty::{Type, Value},
    ErrorKind,
};

impl Compiler<'_> {
    pub fn compile_path_expr(&mut self, path: &Path) -> Value {
        let mut idents = path.idents();

        if idents.len() > 1 {
            self.db
                .error(ErrorKind::PathNotAllowed, path.syntax().text_range());
            return self.unknown();
        }

        let name = idents.remove(0);

        let Some(symbol_id) = self
            .scope_stack
            .iter()
            .rev()
            .find_map(|&scope_id| self.db.scope(scope_id).symbol(name.text()))
        else {
            self.db.error(
                ErrorKind::UndefinedReference(name.to_string()),
                name.text_range(),
            );
            return self.unknown();
        };

        if matches!(self.db.symbol(symbol_id), Symbol::Module(..)) {
            self.db
                .error(ErrorKind::ModuleReference, path.syntax().text_range());
            return self.unknown();
        }

        if !self.is_callee && matches!(self.db.symbol(symbol_id), Symbol::InlineFunction(..)) {
            self.db.error(
                ErrorKind::InlineFunctionReference,
                path.syntax().text_range(),
            );
            return self.unknown();
        }

        Value::new(
            self.db.alloc_hir(Hir::Reference(symbol_id)),
            self.symbol_type(symbol_id)
                .unwrap_or_else(|| match self.db.symbol(symbol_id) {
                    Symbol::Unknown | Symbol::Module(..) => unreachable!(),
                    Symbol::Function(Function { ty, .. })
                    | Symbol::InlineFunction(Function { ty, .. }) => {
                        self.db.alloc_type(Type::Function(ty.clone()))
                    }
                    Symbol::Parameter(type_id)
                    | Symbol::Let(Let { type_id, .. })
                    | Symbol::Const(Const { type_id, .. })
                    | Symbol::InlineConst(Const { type_id, .. }) => *type_id,
                }),
        )
    }
}
