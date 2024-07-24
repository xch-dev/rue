use rowan::TextRange;
use rue_parser::PathItem;
use rue_typing::TypeId;

use crate::{
    compiler::{
        path::{Path, PathKind},
        Compiler,
    },
    ErrorKind,
};

impl Compiler<'_> {
    pub fn compile_path_type(&mut self, items: &[PathItem], text_range: TextRange) -> TypeId {
        let Some(mut path) = self.resolve_base_path(&items[0], PathKind::Type, items.len() == 1)
        else {
            return self.ty.std().unknown;
        };

        let mut last_name = items[0].name().unwrap().to_string();

        for (i, item) in items.iter().enumerate().skip(1) {
            let Some(next_path) =
                self.resolve_next_path(path, item, PathKind::Type, i == items.len() - 1)
            else {
                return self.ty.std().unknown;
            };
            last_name = item.name().unwrap().to_string();
            path = next_path;
        }

        match path {
            Path::Type(type_id) => type_id,
            Path::Symbol(..) => {
                self.db
                    .error(ErrorKind::ExpectedTypePath(last_name), text_range);
                self.ty.std().unknown
            }
        }
    }
}
