use std::collections::HashMap;

use rowan::TextRange;
use rue_parser::PathItem;

use crate::{
    compiler::{
        path::{Path, PathKind},
        Compiler,
    },
    value::Type,
    ErrorKind, TypeId,
};

impl Compiler<'_> {
    pub fn compile_path_type(&mut self, items: &[PathItem], text_range: TextRange) -> TypeId {
        let Some(mut path) = self.resolve_base_path(&items[0], PathKind::Type, items.len() == 1)
        else {
            return self.builtins.unknown;
        };

        let mut last_ident = items[0]
            .ident()
            .map(|name| name.to_string())
            .unwrap_or("{unknown}".to_string());

        for (i, item) in items.iter().enumerate().skip(1) {
            let Some(next_path) =
                self.resolve_next_path(path, item, PathKind::Type, i == items.len() - 1)
            else {
                return self.builtins.unknown;
            };

            if let Some(name) = item.ident() {
                last_ident = name.to_string();
            }

            path = next_path;
        }

        match path {
            Path::Type(type_id) => {
                let type_id = if let Type::Alias(alias_type) = self.db.ty(type_id).clone() {
                    if alias_type.generic_types.is_empty() {
                        alias_type.type_id
                    } else {
                        self.db
                            .error(ErrorKind::ExpectedGenericArgs(last_ident), text_range);
                        self.builtins.unknown
                    }
                } else {
                    type_id
                };

                if self.type_definition_stack.is_empty() {
                    self.db.substitute_type(type_id, &HashMap::new())
                } else {
                    type_id
                }
            }
            Path::Symbol(..) => {
                self.db
                    .error(ErrorKind::ExpectedTypePath(last_ident), text_range);
                self.builtins.unknown
            }
        }
    }
}
