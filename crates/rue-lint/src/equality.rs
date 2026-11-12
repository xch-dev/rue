use rue_hir::{Database, FieldKind, Hir, HirId, Statement, Symbol, SymbolId};

/// Whether two HIR nodes always evaluate to the same value. Evaluating code has no side effects
/// (other than raising), so two expressions that are written the same way, and reference the
/// same symbols, always produce the same value.
///
/// Bindings are compared by their values rather than by their symbols, so that the same code in
/// two different blocks (such as both branches of an `if`) is considered equal. Casts don't change
/// the value at runtime, so they're ignored.
pub(crate) fn hir_eq(db: &Database, a: HirId, b: HirId) -> bool {
    let a = db.without_casts(a);
    let b = db.without_casts(b);

    if a == b {
        return true;
    }

    match (db.hir(a), db.hir(b)) {
        (Hir::Nil, Hir::Nil) | (Hir::InfinityG1, Hir::InfinityG1) => true,
        (Hir::InfinityG2, Hir::InfinityG2) => true,
        (Hir::String(a), Hir::String(b)) => a == b,
        (Hir::Int(a), Hir::Int(b)) => a == b,
        (Hir::Bytes(a), Hir::Bytes(b)) => a == b,
        (Hir::Bool(a), Hir::Bool(b)) => a == b,
        (Hir::Pair(a1, a2), Hir::Pair(b1, b2)) => all_eq(db, &[*a1, *a2], &[*b1, *b2]),
        (Hir::List(a), Hir::List(b)) => {
            a.nil_terminated == b.nil_terminated && all_eq(db, &a.items, &b.items)
        }
        (Hir::Struct(a), Hir::Struct(b)) => {
            a.ty == b.ty
                && a.nil_terminated == b.nil_terminated
                && a.fields.len() == b.fields.len()
                && a.fields
                    .iter()
                    .zip(&b.fields)
                    .all(|(a, b)| a.name == b.name && hir_eq(db, a.value, b.value))
        }
        (Hir::FieldAccess(a), Hir::FieldAccess(b)) => {
            a.name == b.name
                && match (&a.kind, &b.kind) {
                    (FieldKind::Path(a), FieldKind::Path(b)) => a == b,
                    (FieldKind::Length, FieldKind::Length) => true,
                    _ => false,
                }
                && hir_eq(db, a.value, b.value)
        }
        (Hir::Guard(a), Hir::Guard(b)) => a.ty == b.ty && hir_eq(db, a.value, b.value),
        (Hir::Const(a), Hir::Const(b)) => hir_eq(db, a.value, b.value),
        (Hir::Reference(a), Hir::Reference(b)) => symbol_eq(db, *a, *b),
        (Hir::Block(a), Hir::Block(b)) => {
            a.statements.len() == b.statements.len()
                && a.statements
                    .iter()
                    .zip(&b.statements)
                    .all(|(a, b)| statement_eq(db, a, b))
                && option_eq(db, a.body, b.body)
        }
        (Hir::Lambda(a), Hir::Lambda(b)) => a == b,
        (Hir::If(ac, at, ae, ai), Hir::If(bc, bt, be, bi)) => {
            ai == bi && all_eq(db, &[*ac, *at, *ae], &[*bc, *bt, *be])
        }
        (Hir::FunctionCall(a), Hir::FunctionCall(b)) => {
            a.nil_terminated == b.nil_terminated
                && hir_eq(db, a.function, b.function)
                && all_eq(db, &a.args, &b.args)
        }
        (Hir::Unary(a_op, a), Hir::Unary(b_op, b)) => a_op == b_op && hir_eq(db, *a, *b),
        (Hir::Binary(a_op, a1, a2), Hir::Binary(b_op, b1, b2)) => {
            a_op == b_op && all_eq(db, &[*a1, *a2], &[*b1, *b2])
        }
        (Hir::ClvmOp(a_op, a), Hir::ClvmOp(b_op, b)) => a_op == b_op && hir_eq(db, *a, *b),
        (Hir::CoinId(a1, a2, a3), Hir::CoinId(b1, b2, b3))
        | (Hir::Modpow(a1, a2, a3), Hir::Modpow(b1, b2, b3))
        | (Hir::Secp256K1Verify(a1, a2, a3), Hir::Secp256K1Verify(b1, b2, b3))
        | (Hir::Secp256R1Verify(a1, a2, a3), Hir::Secp256R1Verify(b1, b2, b3)) => {
            all_eq(db, &[*a1, *a2, *a3], &[*b1, *b2, *b3])
        }
        (Hir::Substr(a1, a2, a3), Hir::Substr(b1, b2, b3)) => {
            all_eq(db, &[*a1, *a2], &[*b1, *b2]) && option_eq(db, *a3, *b3)
        }
        (Hir::G1Map(a1, a2), Hir::G1Map(b1, b2)) | (Hir::G2Map(a1, a2), Hir::G2Map(b1, b2)) => {
            hir_eq(db, *a1, *b1) && option_eq(db, *a2, *b2)
        }
        (Hir::BlsPairingIdentity(a), Hir::BlsPairingIdentity(b)) => all_eq(db, a, b),
        (Hir::BlsVerify(a, a_args), Hir::BlsVerify(b, b_args)) => {
            hir_eq(db, *a, *b) && all_eq(db, a_args, b_args)
        }
        _ => false,
    }
}

fn all_eq(db: &Database, a: &[HirId], b: &[HirId]) -> bool {
    a.len() == b.len() && a.iter().zip(b).all(|(&a, &b)| hir_eq(db, a, b))
}

fn option_eq(db: &Database, a: Option<HirId>, b: Option<HirId>) -> bool {
    match (a, b) {
        (Some(a), Some(b)) => hir_eq(db, a, b),
        (None, None) => true,
        _ => false,
    }
}

/// Bindings with the same value are interchangeable, since they're evaluated the same way.
/// Anything else (such as a parameter) is only equal to itself.
fn symbol_eq(db: &Database, a: SymbolId, b: SymbolId) -> bool {
    if a == b {
        return true;
    }

    match (db.symbol(a), db.symbol(b)) {
        (Symbol::Binding(a), Symbol::Binding(b)) => {
            a.inline == b.inline && hir_eq(db, a.value.hir, b.value.hir)
        }
        _ => false,
    }
}

fn statement_eq(db: &Database, a: &Statement, b: &Statement) -> bool {
    match (a, b) {
        (Statement::Expr(a), Statement::Expr(b)) => hir_eq(db, a.hir, b.hir),
        (Statement::Let(a, _), Statement::Let(b, _)) => symbol_eq(db, *a, *b),
        (Statement::If(a), Statement::If(b)) => {
            a.inline == b.inline && all_eq(db, &[a.condition, a.then], &[b.condition, b.then])
        }
        (Statement::Return(a, _), Statement::Return(b, _))
        | (Statement::Assert(a, _), Statement::Assert(b, _))
        | (Statement::Debug(a, _), Statement::Debug(b, _)) => hir_eq(db, *a, *b),
        (Statement::Raise(a, _), Statement::Raise(b, _)) => option_eq(db, *a, *b),
        _ => false,
    }
}
