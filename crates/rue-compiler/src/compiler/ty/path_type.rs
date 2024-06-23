use rowan::TextRange;
use rue_parser::SyntaxToken;

use crate::{
    compiler::{
        path::{PathItem, PathKind},
        Compiler,
    },
    ErrorKind, TypeId,
};

impl Compiler<'_> {
    pub fn compile_path_type(&mut self, idents: &[SyntaxToken], text_range: TextRange) -> TypeId {
        let Some(mut item) = self.resolve_base_path(&idents[0], PathKind::Type) else {
            return self.builtins.unknown;
        };

        for name in idents.iter().skip(1) {
            let Some(next_item) = self.resolve_next_path(item, name) else {
                return self.builtins.unknown;
            };
            item = next_item;
        }

        match item {
            PathItem::Type(type_id) => type_id,
            PathItem::Symbol(..) => {
                self.db.error(ErrorKind::ExpectedTypePath, text_range);
                self.builtins.unknown
            }
        }
    }
}
