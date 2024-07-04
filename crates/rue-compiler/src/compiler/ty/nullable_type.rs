use rue_parser::{AstNode, NullableType};

use crate::{compiler::Compiler, value::Type, TypeId, WarningKind};

impl Compiler<'_> {
    pub fn compile_nullable_type(&mut self, optional: &NullableType) -> TypeId {
        let ty = optional
            .ty()
            .map_or(self.builtins.unknown, |ty| self.compile_type(ty));

        if let Type::Nullable(inner) = self.db.ty_raw(ty).clone() {
            self.db.warning(
                WarningKind::RedundantNullableType(self.type_name(ty)),
                optional.syntax().text_range(),
            );
            return inner;
        }

        self.db.alloc_type(Type::Nullable(ty))
    }
}
