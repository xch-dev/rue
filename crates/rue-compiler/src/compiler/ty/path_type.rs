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
        let Some(mut item) = self.resolve_base_path(&idents[0], PathKind::Type, idents.len() == 1)
        else {
            return self.builtins.unknown;
        };

        let mut last_ident = idents[0].to_string();

        for (i, name) in idents.iter().enumerate().skip(1) {
            let Some(next_item) =
                self.resolve_next_path(item, name, PathKind::Type, i == idents.len() - 1)
            else {
                return self.builtins.unknown;
            };
            last_ident = name.to_string();
            item = next_item;
        }

        match item {
            PathItem::Type(type_id) => type_id,
            PathItem::Symbol(..) => {
                self.db
                    .error(ErrorKind::ExpectedTypePath(last_ident), text_range);
                self.builtins.unknown
            }
        }
    }
}
