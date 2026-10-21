use std::collections::HashSet;

use rue_lir::ClvmOp;

use crate::{Database, Hir, HirId, Statement, Symbol, SymbolId};

/// Whether evaluating the given HIR could explicitly raise an error.
///
/// This is intentionally conservative, so that it can be used for warnings without flagging
/// ordinary code. It only looks for `raise` and `assert` statements, builtins whose purpose is to
/// fail (such as signature verification), and calls to functions that contain any of these.
/// Operations that only fail on malformed values (such as arithmetic, field access, or division
/// by zero) aren't considered, and neither are calls to lambdas or other dynamic functions.
pub fn may_raise(db: &Database, hir: HirId) -> bool {
    let mut visitor = Visitor {
        db,
        visited: HashSet::new(),
    };
    visitor.visit_hir(hir)
}

/// The same as [`may_raise`], but for a sequence of statements followed by an optional body.
pub fn statements_may_raise(db: &Database, statements: &[Statement], body: Option<HirId>) -> bool {
    let mut visitor = Visitor {
        db,
        visited: HashSet::new(),
    };
    visitor.visit_statements(statements) || visitor.visit_all(body)
}

struct Visitor<'a> {
    db: &'a Database,
    visited: HashSet<SymbolId>,
}

impl Visitor<'_> {
    fn visit_function(&mut self, symbol: SymbolId) -> bool {
        if !self.visited.insert(symbol) {
            return false;
        }

        match self.db.symbol(symbol) {
            Symbol::Function(function) => self.visit_hir(function.body),
            _ => false,
        }
    }

    fn visit_all(&mut self, hirs: impl IntoIterator<Item = HirId>) -> bool {
        hirs.into_iter().any(|hir| self.visit_hir(hir))
    }

    fn visit_statements(&mut self, statements: &[Statement]) -> bool {
        for stmt in statements {
            let raises = match *stmt {
                Statement::Raise(..) | Statement::Assert(..) => true,
                Statement::Let(symbol) => match self.db.symbol(symbol) {
                    Symbol::Binding(binding) => self.visit_hir(binding.value.hir),
                    _ => false,
                },
                Statement::Expr(stmt) => self.visit_hir(stmt.hir),
                Statement::If(stmt) => self.visit_all([stmt.condition, stmt.then]),
                Statement::Return(hir) | Statement::Debug(hir, _) => self.visit_hir(hir),
            };

            if raises {
                return true;
            }
        }

        false
    }

    fn visit_hir(&mut self, hir: HirId) -> bool {
        match self.db.hir(hir).clone() {
            Hir::Unresolved
            | Hir::Nil
            | Hir::String(_)
            | Hir::Int(_)
            | Hir::Bytes(_)
            | Hir::Bool(_)
            | Hir::InfinityG1
            | Hir::InfinityG2
            // Constant expressions are evaluated at compile time
            | Hir::Const(_)
            // Referenced values are evaluated where they are defined
            | Hir::Reference(_)
            // Lambda bodies are only evaluated when called
            | Hir::Lambda(_) => false,
            Hir::BlsPairingIdentity(_)
            | Hir::BlsVerify(..)
            | Hir::Secp256K1Verify(..)
            | Hir::Secp256R1Verify(..)
            | Hir::ClvmOp(ClvmOp::Raise, _) => true,
            Hir::Block(block) => {
                self.visit_statements(&block.statements) || self.visit_all(block.body)
            }
            Hir::If(condition, then, else_, _) => self.visit_all([condition, then, else_]),
            Hir::FunctionCall(call) => {
                (if let Hir::Reference(symbol) = *self.db.hir(call.function) {
                    self.visit_function(symbol)
                } else {
                    self.visit_hir(call.function)
                }) || self.visit_all(call.args)
            }
            Hir::Unary(_, hir) | Hir::ClvmOp(_, hir) => self.visit_hir(hir),
            Hir::Pair(a, b) | Hir::Binary(_, a, b) => self.visit_all([a, b]),
            Hir::CoinId(a, b, c) | Hir::Modpow(a, b, c) => self.visit_all([a, b, c]),
            Hir::Substr(hir, start, end) => self.visit_all([hir, start]) || self.visit_all(end),
            Hir::G1Map(data, dst) | Hir::G2Map(data, dst) => {
                self.visit_hir(data) || self.visit_all(dst)
            }
        }
    }
}
