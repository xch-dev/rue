use rue_parser::{LiteralExpr, SyntaxKind};

use crate::{compiler::Compiler, hir::Hir, ty::Value};

impl Compiler<'_> {
    pub fn compile_literal_expr(&mut self, literal: &LiteralExpr) -> Value {
        let Some(value) = literal.value() else {
            return self.unknown();
        };

        match value.kind() {
            SyntaxKind::Int => self.compile_int(&value),
            SyntaxKind::Hex => self.compile_hex(&value),
            SyntaxKind::String => self.compile_string(&value),
            SyntaxKind::True => {
                Value::new(self.db.alloc_hir(Hir::Atom(vec![1])), self.builtins.bool)
            }
            SyntaxKind::False => {
                Value::new(self.db.alloc_hir(Hir::Atom(Vec::new())), self.builtins.bool)
            }
            SyntaxKind::Nil => {
                Value::new(self.db.alloc_hir(Hir::Atom(Vec::new())), self.builtins.nil)
            }
            _ => unreachable!(),
        }
    }
}
