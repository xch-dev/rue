use rue_ast::AstLambdaType;
use rue_types::{FunctionType, Type, TypeId};

use crate::{Compiler, compile_type};

pub fn compile_lambda_type(ctx: &mut Compiler, lambda: &AstLambdaType) -> TypeId {
    let mut params = Vec::new();

    for param in lambda.parameters() {
        let ty = if let Some(ty) = param.ty() {
            compile_type(ctx, &ty)
        } else {
            ctx.builtins().unresolved.ty
        };

        params.push(ty);
    }

    let ret = if let Some(ty) = lambda.return_type() {
        compile_type(ctx, &ty)
    } else {
        ctx.builtins().unresolved.ty
    };

    ctx.alloc_type(Type::Function(FunctionType { params, ret }))
}
