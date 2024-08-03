use rue_parser::{LiteralType, SyntaxKind, SyntaxToken};
use rue_typing::{Type, TypeId};

use crate::compiler::Compiler;

impl Compiler<'_> {
    pub fn compile_literal_type(&mut self, literal: &LiteralType) -> TypeId {
        let Some(value) = literal.value() else {
            return self.ty.std().unknown;
        };

        match value.kind() {
            SyntaxKind::Int => self.compile_int_type(&value),
            SyntaxKind::True => self.ty.std().true_bool,
            SyntaxKind::False => self.ty.std().false_bool,
            SyntaxKind::Nil => self.ty.std().nil,
            _ => unreachable!(),
        }
    }

    fn compile_int_type(&mut self, int: &SyntaxToken) -> TypeId {
        // Parse the literal into `BigInt`.
        // It should not be possible to have a syntax error at this point.
        let bigint = int
            .text()
            .replace('_', "")
            .parse()
            .expect("failed to parse integer literal");

        self.ty.alloc(Type::Value(bigint))
    }
}
