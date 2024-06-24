use clvmr::Allocator;
use num_bigint::BigInt;
use rue_parser::{LiteralExpr, SyntaxKind, SyntaxToken};

use crate::{compiler::Compiler, hir::Hir, ty::Value, ErrorKind};

impl Compiler<'_> {
    pub fn compile_literal_expr(&mut self, literal: &LiteralExpr) -> Value {
        let Some(value) = literal.value() else {
            return self.unknown();
        };

        match value.kind() {
            SyntaxKind::Int => self.compile_int_literal(&value),
            SyntaxKind::Hex => self.compile_hex_literal(&value),
            SyntaxKind::String => self.compile_string_literal(&value),
            SyntaxKind::True => self.compile_bool_literal(true),
            SyntaxKind::False => self.compile_bool_literal(false),
            SyntaxKind::Nil => self.compile_nil_literal(),
            _ => unreachable!(),
        }
    }

    fn compile_bool_literal(&mut self, value: bool) -> Value {
        let atom = if value { vec![1] } else { vec![] };
        Value::new(self.db.alloc_hir(Hir::Atom(atom)), self.builtins.bool)
    }

    fn compile_nil_literal(&mut self) -> Value {
        Value::new(self.db.alloc_hir(Hir::Atom(Vec::new())), self.builtins.nil)
    }

    fn compile_int_literal(&mut self, int: &SyntaxToken) -> Value {
        // Parse the literal into `BigInt`.
        // It should not be possible to have a syntax error at this point.
        let bigint = int
            .text()
            .replace('_', "")
            .parse()
            .expect("failed to parse integer literal");

        let atom = Self::bigint_to_bytes(bigint).unwrap_or_else(|| {
            self.db.error(ErrorKind::IntegerTooLarge, int.text_range());
            Vec::new()
        });

        // Extract the atom representation of the number.
        Value::new(self.db.alloc_hir(Hir::Atom(atom)), self.builtins.int)
    }

    fn compile_hex_literal(&mut self, hex: &SyntaxToken) -> Value {
        // Parse the hex literal into bytes.
        // It should not be possible to have a syntax error at this point.
        let bytes = hex::decode(
            hex.text()
                .replace("0x", "")
                .replace("0X", "")
                .replace('_', ""),
        )
        .expect("failed to parse hex literal");

        let bytes_len = bytes.len();

        // Return the atom with the corresponding type based on the length.
        Value::new(
            self.db.alloc_hir(Hir::Atom(bytes)),
            if bytes_len == 32 {
                // We'll assume this is a `Bytes32` since it's the correct length.
                // This makes putting hashes in the code more convenient.
                self.builtins.bytes32
            } else if bytes_len == 48 {
                // We'll assume this is a `PublicKey` since it's the correct length.
                // It's unlikely to intend the type being `Bytes`, but you can cast if needed.
                self.builtins.public_key
            } else {
                // Everything else is just `Bytes`.
                // Leading zeros are not removed, so `0x00` is different than `0`.
                self.builtins.bytes
            },
        )
    }

    fn compile_string_literal(&mut self, string: &SyntaxToken) -> Value {
        // Extract the quote character.
        let text = string.text();
        let quote = text.chars().next().unwrap();

        // Remove the quotes, if present, and create an atom.
        Value::new(
            self.db
                .alloc_hir(Hir::Atom(text.replace(quote, "").as_bytes().to_vec())),
            self.builtins.bytes,
        )
    }

    pub fn bigint_to_bytes(bigint: BigInt) -> Option<Vec<u8>> {
        // Create a CLVM allocator.
        let mut allocator = Allocator::new();

        // Try to allocate the number.
        let ptr = allocator.new_number(bigint).ok()?;

        // Extract the atom representation of the number.
        Some(allocator.atom(ptr).as_ref().to_vec())
    }
}
