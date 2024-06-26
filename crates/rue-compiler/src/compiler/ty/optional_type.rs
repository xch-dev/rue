use rue_parser::{AstNode, OptionalType};

use crate::{compiler::Compiler, value::Type, TypeId, WarningKind};

impl Compiler<'_> {
    pub fn compile_optional_type(&mut self, optional: &OptionalType) -> TypeId {
        let ty = optional
            .ty()
            .map_or(self.builtins.unknown, |ty| self.compile_type(ty));

        if let Type::Optional(inner) = self.db.ty_raw(ty).clone() {
            self.db.warning(
                WarningKind::RedundantOptionalType(self.type_name(ty)),
                optional.syntax().text_range(),
            );
            return inner;
        }

        self.db.alloc_type(Type::Optional(ty))
    }
}
