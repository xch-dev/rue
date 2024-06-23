use rue_parser::{AstNode, FunctionType as Ast};

use crate::{
    compiler::Compiler,
    ty::{FunctionType, Rest, Type},
    ErrorKind, TypeId,
};

impl Compiler<'_> {
    pub fn compile_function_type(&mut self, function: &Ast) -> TypeId {
        let mut param_types = Vec::new();
        let mut rest = Rest::Nil;

        let len = function.params().len();

        for (i, param) in function.params().into_iter().enumerate() {
            let type_id = param
                .ty()
                .map_or(self.builtins.unknown, |ty| self.compile_type(ty));

            param_types.push(type_id);

            if param.spread().is_some() {
                if i + 1 == len {
                    rest = Rest::Parameter;
                } else {
                    self.db
                        .error(ErrorKind::NonFinalSpread, param.syntax().text_range());
                }
            }
        }

        let return_type = function
            .ret()
            .map_or(self.builtins.unknown, |ty| self.compile_type(ty));

        self.db.alloc_type(Type::Function(FunctionType {
            param_types,
            rest,
            return_type,
        }))
    }
}
