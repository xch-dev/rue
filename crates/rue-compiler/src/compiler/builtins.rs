use indexmap::indexset;
use rowan::TextRange;
use rue_typing::{Callable, Rest, Type, TypeSystem};

use crate::{
    hir::{BinOp, Hir, Op},
    scope::Scope,
    symbol::{Function, Symbol},
    Database, HirId, ScopeId, SymbolId,
};

/// These are the built-in types and most commonly used HIR nodes.
pub struct Builtins {
    pub scope_id: ScopeId,
    pub nil: HirId,
    pub unknown: HirId,
}

/// Defines intrinsics that cannot be implemented in Rue.
pub fn builtins(db: &mut Database, ty: &mut TypeSystem) -> Builtins {
    let mut scope = Scope::default();

    let std = ty.standard_types();
    let nil = db.alloc_hir(Hir::Atom(Vec::new()));
    let unknown = db.alloc_hir(Hir::Unknown);

    scope.define_type("Nil".to_string(), std.nil);
    scope.define_type("Int".to_string(), std.int);
    scope.define_type("Bool".to_string(), std.bool);
    scope.define_type("Bytes".to_string(), std.bytes);
    scope.define_type("Bytes32".to_string(), std.bytes32);
    scope.define_type("PublicKey".to_string(), std.public_key);
    scope.define_type("Any".to_string(), std.any);

    let builtins = Builtins {
        scope_id: db.alloc_scope(scope),
        nil,
        unknown,
    };

    let sha256 = sha256(db, ty);
    let pubkey_for_exp = pubkey_for_exp(db, ty);
    let divmod = divmod(db, ty);
    let substr = substr(db, ty);

    db.scope_mut(builtins.scope_id)
        .define_symbol("sha256".to_string(), sha256);

    db.scope_mut(builtins.scope_id)
        .define_symbol("pubkey_for_exp".to_string(), pubkey_for_exp);

    db.scope_mut(builtins.scope_id)
        .define_symbol("divmod".to_string(), divmod);

    db.scope_mut(builtins.scope_id)
        .define_symbol("substr".to_string(), substr);

    builtins
}

fn sha256(db: &mut Database, ty: &mut TypeSystem) -> SymbolId {
    let std = ty.standard_types();

    let mut scope = Scope::default();

    let param = db.alloc_symbol(Symbol::Parameter(std.bytes));
    scope.define_symbol("bytes".to_string(), param);
    let param_ref = db.alloc_hir(Hir::Reference(param, TextRange::default()));
    let hir_id = db.alloc_hir(Hir::Op(Op::Sha256, param_ref));
    let scope_id = db.alloc_scope(scope);

    db.alloc_symbol(Symbol::InlineFunction(Function {
        scope_id,
        hir_id,
        ty: Callable {
            original_type_id: None,
            parameter_names: indexset!["bytes".to_string()],
            parameters: ty.alloc(Type::Pair(std.bytes, std.nil)),
            rest: Rest::Nil,
            return_type: std.bytes32,
            generic_types: Vec::new(),
        },
    }))
}

fn pubkey_for_exp(db: &mut Database, ty: &mut TypeSystem) -> SymbolId {
    let std = ty.standard_types();

    let mut scope = Scope::default();
    let param = db.alloc_symbol(Symbol::Parameter(std.bytes32));
    scope.define_symbol("exponent".to_string(), param);
    let param_ref = db.alloc_hir(Hir::Reference(param, TextRange::default()));
    let hir_id = db.alloc_hir(Hir::Op(Op::PubkeyForExp, param_ref));
    let scope_id = db.alloc_scope(scope);

    db.alloc_symbol(Symbol::InlineFunction(Function {
        scope_id,
        hir_id,
        ty: Callable {
            original_type_id: None,
            parameter_names: indexset!["exponent".to_string()],
            parameters: ty.alloc(Type::Pair(std.bytes32, std.nil)),
            rest: Rest::Nil,
            return_type: std.public_key,
            generic_types: Vec::new(),
        },
    }))
}

fn divmod(db: &mut Database, ty: &mut TypeSystem) -> SymbolId {
    let std = ty.standard_types();

    let mut scope = Scope::default();

    let lhs = db.alloc_symbol(Symbol::Parameter(std.int));
    let rhs = db.alloc_symbol(Symbol::Parameter(std.int));
    scope.define_symbol("lhs".to_string(), lhs);
    scope.define_symbol("rhs".to_string(), rhs);
    let lhs_ref = db.alloc_hir(Hir::Reference(lhs, TextRange::default()));
    let rhs_ref = db.alloc_hir(Hir::Reference(rhs, TextRange::default()));
    let hir_id = db.alloc_hir(Hir::BinaryOp(BinOp::DivMod, lhs_ref, rhs_ref));
    let scope_id = db.alloc_scope(scope);

    let int_pair = ty.alloc(Type::Pair(std.int, std.int));

    db.alloc_symbol(Symbol::InlineFunction(Function {
        scope_id,
        hir_id,
        ty: Callable {
            original_type_id: None,
            parameter_names: indexset!["lhs".to_string(), "rhs".to_string()],
            parameters: ty.alloc(Type::Pair(int_pair, std.nil)),
            rest: Rest::Nil,
            return_type: int_pair,
            generic_types: Vec::new(),
        },
    }))
}

fn substr(db: &mut Database, ty: &mut TypeSystem) -> SymbolId {
    let std = ty.standard_types();

    let mut scope = Scope::default();

    let value = db.alloc_symbol(Symbol::Parameter(std.bytes));
    let start = db.alloc_symbol(Symbol::Parameter(std.int));
    let end = db.alloc_symbol(Symbol::Parameter(std.int));
    scope.define_symbol("value".to_string(), value);
    scope.define_symbol("start".to_string(), start);
    scope.define_symbol("end".to_string(), end);
    let value_ref = db.alloc_hir(Hir::Reference(value, TextRange::default()));
    let start_ref = db.alloc_hir(Hir::Reference(start, TextRange::default()));
    let end_ref = db.alloc_hir(Hir::Reference(end, TextRange::default()));
    let hir_id = db.alloc_hir(Hir::Substr(value_ref, start_ref, end_ref));
    let scope_id = db.alloc_scope(scope);

    let end = ty.alloc(Type::Pair(std.int, std.nil));
    let int = ty.alloc(Type::Pair(std.int, end));
    let parameters = ty.alloc(Type::Pair(std.bytes, int));

    db.alloc_symbol(Symbol::InlineFunction(Function {
        scope_id,
        hir_id,
        ty: Callable {
            original_type_id: None,
            parameter_names: indexset!["value".to_string(), "start".to_string(), "end".to_string()],
            parameters,
            rest: Rest::Nil,
            return_type: std.bytes,
            generic_types: Vec::new(),
        },
    }))
}
