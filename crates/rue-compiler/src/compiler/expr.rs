use rue_parser::{AstNode, Expr};
use rue_typing::TypeId;

use crate::value::Value;

use super::Compiler;

mod binary_expr;
mod block_expr;
mod cast_expr;
mod exists_expr;
mod field_access_expr;
mod function_call_expr;
mod group_expr;
mod guard_expr;
mod if_expr;
mod index_access_expr;
mod initializer_expr;
mod lambda_expr;
mod list_expr;
mod literal_expr;
mod pair_expr;
mod path_expr;
mod prefix_expr;

impl Compiler<'_> {
    pub fn compile_expr(&mut self, expr: &Expr, expected_type: Option<TypeId>) -> Value {
        match &expr {
            Expr::PathExpr(..) => {}
            _ => self.is_callee = false,
        }

        let value = match expr {
            Expr::PathExpr(path) => {
                self.compile_path_expr(&path.idents(), path.syntax().text_range())
            }
            Expr::InitializerExpr(initializer) => self.compile_initializer_expr(initializer),
            Expr::LiteralExpr(literal) => self.compile_literal_expr(literal),
            Expr::ListExpr(list) => self.compile_list_expr(list, expected_type),
            Expr::PairExpr(pair) => self.compile_pair_expr(pair, expected_type),
            Expr::Block(block) => self.compile_block_expr(block, expected_type),
            Expr::LambdaExpr(lambda) => self.compile_lambda_expr(lambda, expected_type),
            Expr::PrefixExpr(prefix) => self.compile_prefix_expr(prefix),
            Expr::BinaryExpr(binary) => self.compile_binary_expr(binary),
            Expr::GroupExpr(expr) => self.compile_group_expr(expr, expected_type),
            Expr::CastExpr(cast) => self.compile_cast_expr(cast),
            Expr::GuardExpr(guard) => self.compile_guard_expr(guard, expected_type),
            Expr::IfExpr(if_expr) => self.compile_if_expr(if_expr, expected_type),
            Expr::FunctionCallExpr(call) => self.compile_function_call_expr(call),
            Expr::FieldAccessExpr(field_access) => self.compile_field_access_expr(field_access),
            Expr::IndexAccessExpr(index_access) => self.compile_index_access_expr(index_access),
            Expr::ExistsExpr(exists) => self.compile_exists_expr(exists),
        };

        self.is_callee = false;

        value
    }
}
