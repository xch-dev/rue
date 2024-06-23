use rue_parser::{AstNode, BinaryExpr, BinaryOp};

use crate::{
    compiler::Compiler,
    hir::{BinOp, Hir},
    ty::{Guard, Value},
    Comparison, ErrorKind,
};

impl Compiler<'_> {
    pub fn compile_binary_expr(&mut self, binary: &BinaryExpr) -> Value {
        let Some(op) = binary.op() else {
            return self.unknown();
        };

        let text_range = binary.syntax().text_range();
        let mut value = self.unknown();

        macro_rules! lhs {
            () => {
                binary
                    .lhs()
                    .map(|lhs| self.compile_expr(&lhs, None))
                    .unwrap_or_else(|| self.unknown())
            };
        }

        macro_rules! rhs {
            () => {
                binary
                    .rhs()
                    .map(|rhs| self.compile_expr(&rhs, None))
                    .unwrap_or_else(|| self.unknown())
            };
        }

        let (op, lhs, rhs, type_id) = match op {
            BinaryOp::Add => {
                let lhs = lhs!();
                let rhs = rhs!();

                if self.db.compare_type(lhs.type_id, self.builtins.public_key) == Comparison::Equal
                {
                    self.type_check(rhs.type_id, self.builtins.public_key, text_range);
                    (
                        BinOp::PointAdd,
                        lhs.hir_id,
                        rhs.hir_id,
                        self.builtins.public_key,
                    )
                } else if self.db.compare_type(lhs.type_id, self.builtins.bytes)
                    == Comparison::Equal
                {
                    self.type_check(rhs.type_id, self.builtins.bytes, text_range);
                    (BinOp::Concat, lhs.hir_id, rhs.hir_id, self.builtins.bytes)
                } else {
                    self.type_check(lhs.type_id, self.builtins.int, text_range);
                    self.type_check(rhs.type_id, self.builtins.int, text_range);
                    (BinOp::Add, lhs.hir_id, rhs.hir_id, self.builtins.int)
                }
            }
            BinaryOp::Subtract => {
                let lhs = lhs!();
                let rhs = rhs!();
                self.type_check(lhs.type_id, self.builtins.int, text_range);
                self.type_check(rhs.type_id, self.builtins.int, text_range);
                (BinOp::Subtract, lhs.hir_id, rhs.hir_id, self.builtins.int)
            }
            BinaryOp::Multiply => {
                let lhs = lhs!();
                let rhs = rhs!();
                self.type_check(lhs.type_id, self.builtins.int, text_range);
                self.type_check(rhs.type_id, self.builtins.int, text_range);
                (BinOp::Multiply, lhs.hir_id, rhs.hir_id, self.builtins.int)
            }
            BinaryOp::Divide => {
                let lhs = lhs!();
                let rhs = rhs!();
                self.type_check(lhs.type_id, self.builtins.int, text_range);
                self.type_check(rhs.type_id, self.builtins.int, text_range);
                (BinOp::Divide, lhs.hir_id, rhs.hir_id, self.builtins.int)
            }
            BinaryOp::Remainder => {
                let lhs = lhs!();
                let rhs = rhs!();
                self.type_check(lhs.type_id, self.builtins.int, text_range);
                self.type_check(rhs.type_id, self.builtins.int, text_range);
                (BinOp::Remainder, lhs.hir_id, rhs.hir_id, self.builtins.int)
            }
            BinaryOp::Equals => {
                let lhs = lhs!();
                let rhs = rhs!();

                if self.db.compare_type(lhs.type_id, self.builtins.bytes) > Comparison::Castable
                    || self.db.compare_type(rhs.type_id, self.builtins.bytes) > Comparison::Castable
                {
                    self.db.error(
                        ErrorKind::NonAtomEquality(self.type_name(lhs.type_id)),
                        text_range,
                    );
                } else if self.db.compare_type(lhs.type_id, self.builtins.nil) == Comparison::Equal
                {
                    if let Hir::Reference(symbol_id) = self.db.hir(rhs.hir_id) {
                        value.guards.insert(
                            *symbol_id,
                            Guard::new(self.builtins.nil, self.try_unwrap_optional(rhs.type_id)),
                        );
                    }
                } else if self.db.compare_type(rhs.type_id, self.builtins.nil) == Comparison::Equal
                {
                    if let Hir::Reference(symbol_id) = self.db.hir(lhs.hir_id) {
                        value.guards.insert(
                            *symbol_id,
                            Guard::new(self.builtins.nil, self.try_unwrap_optional(lhs.type_id)),
                        );
                    }
                } else {
                    self.type_check(rhs.type_id, lhs.type_id, text_range);
                }

                (BinOp::Equals, lhs.hir_id, rhs.hir_id, self.builtins.bool)
            }
            BinaryOp::NotEquals => {
                let lhs = lhs!();
                let rhs = rhs!();

                if self.db.compare_type(lhs.type_id, self.builtins.bytes) > Comparison::Castable
                    || self.db.compare_type(rhs.type_id, self.builtins.bytes) > Comparison::Castable
                {
                    self.db.error(
                        ErrorKind::NonAtomEquality(self.type_name(lhs.type_id)),
                        text_range,
                    );
                } else if self.db.compare_type(lhs.type_id, self.builtins.nil) == Comparison::Equal
                {
                    if let Hir::Reference(symbol_id) = self.db.hir(rhs.hir_id) {
                        value.guards.insert(
                            *symbol_id,
                            Guard::new(self.try_unwrap_optional(rhs.type_id), self.builtins.nil),
                        );
                    }
                } else if self.db.compare_type(rhs.type_id, self.builtins.nil) == Comparison::Equal
                {
                    if let Hir::Reference(symbol_id) = self.db.hir(lhs.hir_id) {
                        value.guards.insert(
                            *symbol_id,
                            Guard::new(self.try_unwrap_optional(lhs.type_id), self.builtins.nil),
                        );
                    }
                } else {
                    self.type_check(rhs.type_id, lhs.type_id, text_range);
                }

                (BinOp::NotEquals, lhs.hir_id, rhs.hir_id, self.builtins.bool)
            }
            BinaryOp::GreaterThan => {
                let lhs = lhs!();
                let rhs = rhs!();
                self.type_check(lhs.type_id, self.builtins.int, text_range);
                self.type_check(rhs.type_id, self.builtins.int, text_range);
                (
                    BinOp::GreaterThan,
                    lhs.hir_id,
                    rhs.hir_id,
                    self.builtins.bool,
                )
            }
            BinaryOp::LessThan => {
                let lhs = lhs!();
                let rhs = rhs!();
                self.type_check(lhs.type_id, self.builtins.int, text_range);
                self.type_check(rhs.type_id, self.builtins.int, text_range);
                (BinOp::LessThan, lhs.hir_id, rhs.hir_id, self.builtins.bool)
            }
            BinaryOp::GreaterThanEquals => {
                let lhs = lhs!();
                let rhs = rhs!();
                self.type_check(lhs.type_id, self.builtins.int, text_range);
                self.type_check(rhs.type_id, self.builtins.int, text_range);
                (
                    BinOp::GreaterThanEquals,
                    lhs.hir_id,
                    rhs.hir_id,
                    self.builtins.bool,
                )
            }
            BinaryOp::LessThanEquals => {
                let lhs = lhs!();
                let rhs = rhs!();
                self.type_check(lhs.type_id, self.builtins.int, text_range);
                self.type_check(rhs.type_id, self.builtins.int, text_range);
                (
                    BinOp::LessThanEquals,
                    lhs.hir_id,
                    rhs.hir_id,
                    self.builtins.bool,
                )
            }
            BinaryOp::And => {
                let lhs = lhs!();
                self.type_guard_stack.push(lhs.then_guards());
                let rhs = rhs!();
                self.type_guard_stack.pop().unwrap();

                self.type_check(lhs.type_id, self.builtins.bool, text_range);
                self.type_check(rhs.type_id, self.builtins.bool, text_range);

                value.type_id = self.builtins.bool;
                value.guards.extend(lhs.guards);
                value.guards.extend(rhs.guards);

                (
                    BinOp::LogicalAnd,
                    lhs.hir_id,
                    rhs.hir_id,
                    self.builtins.bool,
                )
            }
            BinaryOp::Or => {
                let lhs = lhs!();
                self.type_guard_stack.push(lhs.else_guards());
                let rhs = rhs!();
                self.type_guard_stack.pop().unwrap();

                self.type_check(lhs.type_id, self.builtins.bool, text_range);
                self.type_check(rhs.type_id, self.builtins.bool, text_range);

                value.type_id = self.builtins.bool;

                // TODO: Support type guards for `||`.

                (BinOp::LogicalOr, lhs.hir_id, rhs.hir_id, self.builtins.bool)
            }
        };

        value.type_id = type_id;
        value.hir_id = self.db.alloc_hir(Hir::BinaryOp { op, lhs, rhs });
        value
    }
}
