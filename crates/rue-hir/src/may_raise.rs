use std::collections::HashSet;

use rue_lir::ClvmOp;

use crate::{
    Database, Hir, HirId, Statement, Symbol, SymbolId, reachable_body, reachable_statements,
};

/// Whether evaluating the given HIR could explicitly raise an error.
///
/// This is intentionally conservative, so that it can be used for warnings without flagging
/// ordinary code. It only looks for `raise` and `assert` statements, builtins whose purpose is to
/// fail (such as signature verification), and calls to functions that contain any of these.
/// Inline bindings and constants are checked where they are referenced, since that's where they
/// are evaluated.
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
    visitor.visit_statements(statements) || visitor.visit_all(reachable_body(statements, body))
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

    fn visit_inline_value(&mut self, symbol: SymbolId) -> bool {
        if !self.visited.insert(symbol) {
            return false;
        }

        match self.db.symbol(symbol) {
            Symbol::Binding(binding) if binding.inline => self.visit_hir(binding.value.hir),
            Symbol::Constant(constant) if constant.inline => self.visit_hir(constant.value.hir),
            _ => false,
        }
    }

    fn visit_all(&mut self, hirs: impl IntoIterator<Item = HirId>) -> bool {
        hirs.into_iter().any(|hir| self.visit_hir(hir))
    }

    fn visit_statements(&mut self, statements: &[Statement]) -> bool {
        // Nothing after a `return` or `raise` is evaluated
        for stmt in reachable_statements(statements) {
            let raises = match *stmt {
                Statement::Raise(..) | Statement::Assert(..) => true,
                // Inline bindings are evaluated where they are referenced instead
                Statement::Let(symbol, _) => match self.db.symbol(symbol) {
                    Symbol::Binding(binding) if !binding.inline => {
                        self.visit_hir(binding.value.hir)
                    }
                    _ => false,
                },
                Statement::Expr(ref stmt) => self.visit_hir(stmt.hir),
                Statement::If(ref stmt) => self.visit_all([stmt.condition, stmt.then]),
                Statement::Return(hir, _) | Statement::Debug(hir, _) => self.visit_hir(hir),
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
            // Lambda bodies are only evaluated when called
            | Hir::Lambda(_) => false,
            // Referenced values are evaluated where they are defined, unless they are inline
            Hir::Reference(symbol) => self.visit_inline_value(symbol),
            Hir::BlsPairingIdentity(_)
            | Hir::BlsVerify(..)
            | Hir::Secp256K1Verify(..)
            | Hir::Secp256R1Verify(..)
            | Hir::ClvmOp(ClvmOp::Raise, _) => true,
            Hir::Block(block) => {
                self.visit_statements(&block.statements) || self.visit_all(block.reachable_body())
            }
            Hir::If(condition, then, else_, _) => self.visit_all([condition, then, else_]),
            Hir::List(list) => self.visit_all(list.items),
            Hir::Struct(initializer) => {
                self.visit_all(initializer.fields.into_iter().map(|field| field.value))
            }
            Hir::FieldAccess(access) => self.visit_hir(access.value),
            Hir::Cast(hir) => self.visit_hir(hir),
            // The value is only evaluated if the check it's lowered to uses it
            Hir::Guard(guard) => guard.value_uses() > 0 && self.visit_hir(guard.value),
            Hir::FunctionCall(call) => {
                (match *self.db.hir(self.db.without_casts(call.function)) {
                    Hir::Reference(symbol)
                        if matches!(self.db.symbol(symbol), Symbol::Function(_)) =>
                    {
                        self.visit_function(symbol)
                    }
                    _ => self.visit_hir(call.function),
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

#[cfg(test)]
mod tests {
    use rue_types::BuiltinTypes;

    use crate::{BindingSymbol, Block, ConstantSymbol, Value};

    use rue_diagnostic::{Source, SourceKind, SrcLoc};

    use super::*;

    fn srcloc() -> SrcLoc {
        SrcLoc::new(Source::new("".into(), SourceKind::Std(String::new())), 0..0)
    }

    fn raise(db: &mut Database) -> HirId {
        let nil = db.alloc_hir(Hir::Nil);
        db.alloc_hir(Hir::ClvmOp(ClvmOp::Raise, nil))
    }

    fn binding(db: &mut Database, value: HirId, inline: bool) -> SymbolId {
        let ty = BuiltinTypes::new(db.types_mut()).int;
        db.alloc_symbol(Symbol::Binding(BindingSymbol {
            name: None,
            value: Value::new(value, ty),
            inline,
        }))
    }

    fn constant(db: &mut Database, value: HirId, inline: bool) -> SymbolId {
        let ty = BuiltinTypes::new(db.types_mut()).int;
        db.alloc_symbol(Symbol::Constant(ConstantSymbol {
            name: None,
            value: Value::new(value, ty),
            inline,
        }))
    }

    #[test]
    fn inline_references_are_evaluated_at_use() {
        for inline in [false, true] {
            let mut db = Database::new();
            let value = raise(&mut db);
            let binding = binding(&mut db, value, inline);
            let constant = constant(&mut db, value, inline);
            let binding = db.alloc_hir(Hir::Reference(binding));
            let constant = db.alloc_hir(Hir::Reference(constant));

            assert_eq!(may_raise(&db, binding), inline);
            assert_eq!(may_raise(&db, constant), inline);
        }
    }

    #[test]
    fn inline_bindings_are_not_evaluated_at_definition() {
        for inline in [false, true] {
            let mut db = Database::new();
            let value = raise(&mut db);
            let binding = binding(&mut db, value, inline);
            let block = db.alloc_hir(Hir::Block(Block {
                statements: vec![Statement::Let(binding, srcloc())],
                body: None,
            }));

            assert_eq!(may_raise(&db, block), !inline);
        }
    }

    #[test]
    fn recursive_inline_constant() {
        let mut db = Database::new();
        let value = db.alloc_hir(Hir::Unresolved);
        let symbol = constant(&mut db, value, true);
        *db.hir_mut(value) = Hir::Reference(symbol);

        assert!(!may_raise(&db, value));
    }
}
