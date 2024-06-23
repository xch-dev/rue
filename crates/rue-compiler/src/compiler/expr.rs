use rue_parser::Expr;

use crate::{ty::Value, TypeId};

use super::Compiler;

mod binary_expr;
mod block_expr;
mod if_expr;
mod initializer_expr;
mod pair_expr;
mod prefix_expr;

impl Compiler<'_> {
    pub fn compile_expr(&mut self, expr: &Expr, expected_type: Option<TypeId>) -> Value {
        match &expr {
            Expr::Path(..) => {}
            _ => self.is_callee = false,
        }

        let value = match expr {
            Expr::Path(path) => self.compile_path_expr(path),
            Expr::InitializerExpr(initializer) => self.compile_initializer_expr(initializer),
            Expr::LiteralExpr(literal) => self.compile_literal_expr(literal),
            Expr::ListExpr(list) => self.compile_list_expr(list, expected_type),
            Expr::PairExpr(pair) => self.compile_pair_expr(pair, expected_type),
            Expr::Block(block) => self.compile_block_expr(block, expected_type),
            Expr::LambdaExpr(lambda) => self.compile_lambda_expr(lambda, expected_type),
            Expr::PrefixExpr(prefix) => self.compile_prefix_expr(prefix),
            Expr::BinaryExpr(binary) => self.compile_binary_expr(binary),
            Expr::GroupExpr(expr) => self.compile_group_expr(expr, expected_type),
            Expr::CastExpr(cast) => self.compile_cast_expr(cast, expected_type),
            Expr::GuardExpr(guard) => self.compile_guard_expr(guard, expected_type),
            Expr::IfExpr(if_expr) => self.compile_if_expr(if_expr, expected_type),
            Expr::FunctionCall(call) => self.compile_function_call(call),
            Expr::FieldAccess(field_access) => self.compile_field_access(field_access),
            Expr::IndexAccess(index_access) => self.compile_index_access(index_access),
        };

        self.is_callee = false;

        value
    }
}
