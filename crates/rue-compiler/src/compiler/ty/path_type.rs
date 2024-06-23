use rue_parser::Path;

use crate::{compiler::Compiler, ErrorKind, TypeId};

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
}
