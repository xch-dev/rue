use std::{fmt, str::FromStr};

use clvmr::Allocator;
use rue_parser::{LiteralExpr, SyntaxKind, SyntaxToken};

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

    pub fn compile_int_raw<T, E>(int: &SyntaxToken) -> T
    where
        T: FromStr<Err = E>,
        E: fmt::Debug,
    {
        int.text()
            .replace('_', "")
            .parse()
            .expect("failed to parse into BigInt")
    }

    pub fn compile_int(&mut self, int: &SyntaxToken) -> Value {
        let num = Self::compile_int_raw(int);

        let mut allocator = Allocator::new();
        let ptr = allocator
            .new_number(num)
            .expect("number is too large to be represented in memory in an Allocator instance");

        Value::new(
            self.db
                .alloc_hir(Hir::Atom(allocator.atom(ptr).as_ref().to_vec())),
            self.builtins.int,
        )
    }

    fn compile_hex(&mut self, hex: &SyntaxToken) -> Value {
        let Ok(bytes) = hex::decode(
            hex.text()
                .replace("0x", "")
                .replace("0X", "")
                .replace('_', ""),
        ) else {
            return Value::new(self.builtins.unknown_hir, self.builtins.bytes);
        };

        let bytes_len = bytes.len();

        Value::new(
            self.db.alloc_hir(Hir::Atom(bytes)),
            if bytes_len == 32 {
                self.builtins.bytes32
            } else if bytes_len == 48 {
                self.builtins.public_key
            } else {
                self.builtins.bytes
            },
        )
    }

    fn compile_string(&mut self, string: &SyntaxToken) -> Value {
        let text = string.text();
        let quote = text.chars().next().unwrap();
        let after_prefix = &text[1..];
        let before_suffix = after_prefix.strip_suffix(quote).unwrap_or(after_prefix);

        let bytes = before_suffix.as_bytes();

        Value::new(
            self.db.alloc_hir(Hir::Atom(bytes.to_vec())),
            if bytes.len() == 32 {
                self.builtins.bytes32
            } else {
                self.builtins.bytes
            },
        )
    }
}
