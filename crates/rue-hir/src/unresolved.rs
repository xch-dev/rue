use std::collections::HashSet;

use crate::{Database, Hir, HirId, Statement, Symbol, SymbolId};

/// Whether lowering the given symbol would reach unresolved HIR, or a symbol which can't be
/// used as a value (such as a module or builtin).
///
/// Unresolved HIR should only ever be produced alongside an error diagnostic, in which case code
/// generation doesn't happen. This is a final safety check to ensure that if a diagnostic is
/// missing, the compiler refuses to generate code rather than panicking or emitting an incorrect
/// program. Unreachable code (such as code after a `return` or `raise`) is skipped, since it's
/// never lowered.
pub fn has_reachable_unresolved(db: &Database, symbol: SymbolId) -> bool {
    let mut visitor = Visitor {
        db,
        visited: HashSet::new(),
    };
    visitor.visit_symbol(symbol)
}

struct Visitor<'a> {
    db: &'a Database,
    visited: HashSet<SymbolId>,
}

impl Visitor<'_> {
    fn visit_symbol(&mut self, symbol: SymbolId) -> bool {
        if !self.visited.insert(symbol) {
            return false;
        }

        match self.db.symbol(symbol) {
            Symbol::Unresolved | Symbol::Module(_) | Symbol::Builtin(_) => true,
            Symbol::Parameter(_) => false,
            Symbol::Function(function) => self.visit_hir(function.body),
            Symbol::Constant(constant) => self.visit_hir(constant.value.hir),
            Symbol::Binding(binding) => self.visit_hir(binding.value.hir),
        }
    }

    fn visit_all(&mut self, hirs: impl IntoIterator<Item = HirId>) -> bool {
        hirs.into_iter().any(|hir| self.visit_hir(hir))
    }

    fn visit_hir(&mut self, hir: HirId) -> bool {
        match self.db.hir(hir).clone() {
            Hir::Unresolved => true,
            Hir::Nil
            | Hir::String(_)
            | Hir::Int(_)
            | Hir::Bytes(_)
            | Hir::Bool(_)
            | Hir::InfinityG1
            | Hir::InfinityG2 => false,
            Hir::Reference(symbol) | Hir::Lambda(symbol) => self.visit_symbol(symbol),
            Hir::Const(expr) => self.visit_hir(expr.value),
            Hir::Block(block) => {
                for stmt in block.statements {
                    match stmt {
                        // Bindings are only lowered if they're referenced
                        Statement::Let(_) => {}
                        Statement::Expr(stmt) => {
                            if self.visit_hir(stmt.hir) {
                                return true;
                            }
                        }
                        Statement::If(stmt) => {
                            if self.visit_all([stmt.condition, stmt.then]) {
                                return true;
                            }
                        }
                        Statement::Assert(hir, _) | Statement::Debug(hir, _) => {
                            if self.visit_hir(hir) {
                                return true;
                            }
                        }
                        // Nothing after these statements is lowered
                        Statement::Return(hir) => return self.visit_hir(hir),
                        Statement::Raise(hir, _) => return self.visit_all(hir),
                    }
                }

                self.visit_all(block.body)
            }
            Hir::If(condition, then, else_, _) => self.visit_all([condition, then, else_]),
            Hir::FunctionCall(call) => self.visit_hir(call.function) || self.visit_all(call.args),
            Hir::Unary(_, hir) | Hir::ClvmOp(_, hir) => self.visit_hir(hir),
            Hir::Pair(a, b) | Hir::Binary(_, a, b) => self.visit_all([a, b]),
            Hir::CoinId(a, b, c)
            | Hir::Modpow(a, b, c)
            | Hir::Secp256K1Verify(a, b, c)
            | Hir::Secp256R1Verify(a, b, c) => self.visit_all([a, b, c]),
            Hir::Substr(hir, start, end) => self.visit_all([hir, start]) || self.visit_all(end),
            Hir::G1Map(data, dst) | Hir::G2Map(data, dst) => {
                self.visit_hir(data) || self.visit_all(dst)
            }
            Hir::BlsPairingIdentity(args) => self.visit_all(args),
            Hir::BlsVerify(sig, args) => self.visit_hir(sig) || self.visit_all(args),
        }
    }
}

#[cfg(test)]
mod tests {
    use rue_diagnostic::{Source, SourceKind, SrcLoc};
    use rue_types::BuiltinTypes;

    use crate::{BindingSymbol, Block, Builtin, ExprStatement, Value};

    use super::*;

    fn binding(db: &mut Database, hir: HirId) -> SymbolId {
        let ty = BuiltinTypes::new(db.types_mut()).int;
        db.alloc_symbol(Symbol::Binding(BindingSymbol {
            name: None,
            value: Value::new(hir, ty),
            inline: false,
        }))
    }

    fn srcloc() -> SrcLoc {
        SrcLoc::new(Source::new("".into(), SourceKind::Std(String::new())), 0..0)
    }

    #[test]
    fn test_resolved() {
        let mut db = Database::new();
        let one = db.alloc_hir(Hir::Int(1.into()));
        let two = db.alloc_hir(Hir::Int(2.into()));
        let pair = db.alloc_hir(Hir::Pair(one, two));
        let symbol = binding(&mut db, pair);
        assert!(!has_reachable_unresolved(&db, symbol));
    }

    #[test]
    fn test_unresolved_hir() {
        let mut db = Database::new();
        let one = db.alloc_hir(Hir::Int(1.into()));
        let unresolved = db.alloc_hir(Hir::Unresolved);
        let pair = db.alloc_hir(Hir::Pair(one, unresolved));
        let symbol = binding(&mut db, pair);
        assert!(has_reachable_unresolved(&db, symbol));
    }

    #[test]
    fn test_unresolved_symbols() {
        let mut db = Database::new();

        for target in [
            Symbol::Unresolved,
            Symbol::Builtin(Builtin::Sha256 { inline: false }),
        ] {
            let target = db.alloc_symbol(target);
            let reference = db.alloc_hir(Hir::Reference(target));
            let symbol = binding(&mut db, reference);
            assert!(has_reachable_unresolved(&db, symbol));
        }
    }

    #[test]
    fn test_unresolved_through_reference() {
        let mut db = Database::new();
        let unresolved = db.alloc_hir(Hir::Unresolved);
        let inner = binding(&mut db, unresolved);
        let reference = db.alloc_hir(Hir::Reference(inner));
        let symbol = binding(&mut db, reference);
        assert!(has_reachable_unresolved(&db, symbol));
    }

    #[test]
    fn test_unreachable_after_raise() {
        let mut db = Database::new();
        let one = db.alloc_hir(Hir::Int(1.into()));
        let unresolved = db.alloc_hir(Hir::Unresolved);

        // This is how blocks which end in `raise` are represented
        let block = db.alloc_hir(Hir::Block(Block {
            statements: vec![
                Statement::Expr(ExprStatement {
                    hir: one,
                    always_nil: false,
                }),
                Statement::Raise(None, srcloc()),
                Statement::Expr(ExprStatement {
                    hir: unresolved,
                    always_nil: false,
                }),
            ],
            body: Some(unresolved),
        }));
        let symbol = binding(&mut db, block);
        assert!(!has_reachable_unresolved(&db, symbol));

        // But the value being raised is still lowered
        let block = db.alloc_hir(Hir::Block(Block {
            statements: vec![Statement::Raise(Some(unresolved), srcloc())],
            body: None,
        }));
        let symbol = binding(&mut db, block);
        assert!(has_reachable_unresolved(&db, symbol));
    }

    #[test]
    fn test_unreachable_after_return() {
        let mut db = Database::new();
        let one = db.alloc_hir(Hir::Int(1.into()));
        let unresolved = db.alloc_hir(Hir::Unresolved);

        let block = db.alloc_hir(Hir::Block(Block {
            statements: vec![Statement::Return(one)],
            body: Some(unresolved),
        }));
        let symbol = binding(&mut db, block);
        assert!(!has_reachable_unresolved(&db, symbol));
    }
}
