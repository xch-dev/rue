use indexmap::IndexSet;
use rue_parser::{AstNode, FunctionType as Ast};
use rue_typing::{Callable, Rest, Type, TypeId};

use crate::{compiler::Compiler, ErrorKind};

impl Compiler<'_> {
    pub fn compile_function_type(&mut self, function: &Ast) -> TypeId {
        let mut parameters = Vec::new();
        let mut parameter_names = IndexSet::new();
        let mut rest = Rest::Nil;

        let params = function.params();
        let len = params.len();

        for (i, param) in params.into_iter().enumerate() {
            // We don't actually use the names yet,
            // but go ahead and check for duplicates.
            // TODO: Use the name in the actual type?
            let name = param
                .name()
                .map(|token| token.to_string())
                .unwrap_or(format!("#{i}"));

            if !parameter_names.insert(name.to_string()) {
                self.db.error(
                    ErrorKind::DuplicateSymbol(name.to_string()),
                    param.name().unwrap().text_range(),
                );
            }

            // Compile the type of the parameter, if present.
            // Otherwise, it's a parser error.
            let type_id = param
                .ty()
                .map_or(self.ty.std().unknown, |ty| self.compile_type(ty));

            // Add the parameter type to the list.
            parameters.push(type_id);

            // Check if it's a spread or optional parameter.
            let last = i + 1 == len;
            let spread = param.spread().is_some();
            let optional = param.optional().is_some();

            if spread && optional {
                self.db.error(
                    ErrorKind::OptionalParameterSpread,
                    param.syntax().text_range(),
                );
            } else if spread && !last {
                self.db.error(
                    ErrorKind::InvalidSpreadParameter,
                    param.syntax().text_range(),
                );
            } else if optional && !last {
                self.db.error(
                    ErrorKind::InvalidOptionalParameter,
                    param.syntax().text_range(),
                );
            } else if spread {
                rest = Rest::Spread;
            } else if optional {
                rest = Rest::Optional;
            }
        }

        // Compile the return type of the function, if present.
        // Otherwise, it's a parser error.
        let return_type = function
            .return_type()
            .map_or(self.ty.std().unknown, |ty| self.compile_type(ty));

        // Allocate a new type for the function.
        // TODO: Support generic types.
        self.ty.alloc(Type::Callable(Callable {
            original_type_id: None,
            parameter_names,
            parameters,
            rest,
            return_type,
            generic_types: Vec::new(),
        }))
    }
}
