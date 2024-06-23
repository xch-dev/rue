use rowan::TextRange;
use rue_parser::Path;

use crate::{compiler::Compiler, ty::Type, ErrorKind, TypeId};

impl Compiler<'_> {
    pub fn compile_path_type(&mut self, path: &Path) -> TypeId {
        let mut idents = path.idents();

        let name = idents.remove(0);
        let mut ty = None;

        for &scope_id in self.scope_stack.iter().rev() {
            if let Some(found_type_id) = self.db.scope(scope_id).type_alias(name.text()) {
                ty = Some(found_type_id);
                break;
            }
        }

        let Some(mut ty) = ty else {
            self.db.error(
                ErrorKind::UndefinedType(name.to_string()),
                name.text_range(),
            );
            return self.builtins.unknown;
        };

        self.type_reference(ty);

        for name in idents {
            ty = self.path_into_type(ty, name.text(), name.text_range());
            self.type_reference(ty);
        }

        ty
    }

    fn path_into_type(&mut self, ty: TypeId, name: &str, range: TextRange) -> TypeId {
        let Type::Enum(enum_type) = self.db.ty(ty) else {
            self.db
                .error(ErrorKind::PathIntoNonEnum(self.type_name(ty)), range);
            return self.builtins.unknown;
        };

        if let Some(&variant_type) = enum_type.variants.get(name) {
            return variant_type;
        }

        self.db
            .error(ErrorKind::UnknownEnumVariant(name.to_string()), range);
        self.builtins.unknown
    }
}
