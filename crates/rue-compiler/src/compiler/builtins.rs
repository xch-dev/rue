use rowan::TextRange;

use crate::{
    hir::{BinOp, Hir, Op},
    scope::Scope,
    symbol::{Function, Symbol},
    value::{FunctionType, PairType, Rest, Type},
    Database, HirId, ScopeId, SymbolId, TypeId,
};

/// These are the built-in types and most commonly used HIR nodes.
pub struct Builtins {
    pub scope_id: ScopeId,
    pub any: TypeId,
    pub int: TypeId,
    pub bool: TypeId,
    pub bytes: TypeId,
    pub bytes32: TypeId,
    pub public_key: TypeId,
    pub nil: TypeId,
    pub nil_hir: HirId,
    pub one_hir: HirId,
    pub unknown: TypeId,
    pub unknown_hir: HirId,
}

/// Defines intrinsics that cannot be implemented in Rue.
pub fn builtins(db: &mut Database) -> Builtins {
    let mut scope = Scope::default();

    let int = db.alloc_type(Type::Int);
    let bool = db.alloc_type(Type::Bool);
    let bytes = db.alloc_type(Type::Bytes);
    let bytes32 = db.alloc_type(Type::Bytes32);
    let public_key = db.alloc_type(Type::PublicKey);
    let any = db.alloc_type(Type::Any);
    let nil = db.alloc_type(Type::Nil);
    let nil_hir = db.alloc_hir(Hir::Atom(Vec::new()));
    let one_hir = db.alloc_hir(Hir::Atom(vec![1]));
    let unknown = db.alloc_type(Type::Unknown);
    let unknown_hir = db.alloc_hir(Hir::Unknown);

    scope.define_type("Nil".to_string(), nil);
    scope.define_type("Int".to_string(), int);
    scope.define_type("Bool".to_string(), bool);
    scope.define_type("Bytes".to_string(), bytes);
    scope.define_type("Bytes32".to_string(), bytes32);
    scope.define_type("PublicKey".to_string(), public_key);
    scope.define_type("Any".to_string(), any);

    let builtins = Builtins {
        scope_id: db.alloc_scope(scope),
        any,
        int,
        bool,
        bytes,
        bytes32,
        public_key,
        nil,
        nil_hir,
        one_hir,
        unknown,
        unknown_hir,
    };

    let sha256 = sha256(db, &builtins);
    let pubkey_for_exp = pubkey_for_exp(db, &builtins);
    let divmod = divmod(db, &builtins);

    db.scope_mut(builtins.scope_id)
        .define_symbol("sha256".to_string(), sha256);

    db.scope_mut(builtins.scope_id)
        .define_symbol("pubkey_for_exp".to_string(), pubkey_for_exp);

    db.scope_mut(builtins.scope_id)
        .define_symbol("divmod".to_string(), divmod);

    builtins
}

fn sha256(db: &mut Database, builtins: &Builtins) -> SymbolId {
    let mut scope = Scope::default();

    let param = db.alloc_symbol(Symbol::Parameter(builtins.bytes));
    scope.define_symbol("bytes".to_string(), param);
    let param_ref = db.alloc_hir(Hir::Reference(param, TextRange::default()));
    let hir_id = db.alloc_hir(Hir::Op(Op::Sha256, param_ref));
    let scope_id = db.alloc_scope(scope);

    db.alloc_symbol(Symbol::InlineFunction(Function {
        scope_id,
        hir_id,
        ty: FunctionType {
            param_types: vec![builtins.bytes],
            rest: Rest::Nil,
            return_type: builtins.bytes32,
            generic_types: Vec::new(),
        },
    }))
}

fn pubkey_for_exp(db: &mut Database, builtins: &Builtins) -> SymbolId {
    let mut scope = Scope::default();
    let param = db.alloc_symbol(Symbol::Parameter(builtins.bytes32));
    scope.define_symbol("exponent".to_string(), param);
    let param_ref = db.alloc_hir(Hir::Reference(param, TextRange::default()));
    let hir_id = db.alloc_hir(Hir::Op(Op::PubkeyForExp, param_ref));
    let scope_id = db.alloc_scope(scope);

    db.alloc_symbol(Symbol::InlineFunction(Function {
        scope_id,
        hir_id,
        ty: FunctionType {
            param_types: vec![builtins.bytes32],
            rest: Rest::Nil,
            return_type: builtins.public_key,
            generic_types: Vec::new(),
        },
    }))
}

fn divmod(db: &mut Database, builtins: &Builtins) -> SymbolId {
    let mut scope = Scope::default();
    let lhs = db.alloc_symbol(Symbol::Parameter(builtins.int));
    let rhs = db.alloc_symbol(Symbol::Parameter(builtins.int));
    scope.define_symbol("lhs".to_string(), lhs);
    scope.define_symbol("rhs".to_string(), rhs);
    let lhs_ref = db.alloc_hir(Hir::Reference(lhs, TextRange::default()));
    let rhs_ref = db.alloc_hir(Hir::Reference(rhs, TextRange::default()));
    let hir_id = db.alloc_hir(Hir::BinaryOp(BinOp::DivMod, lhs_ref, rhs_ref));
    let scope_id = db.alloc_scope(scope);

    let int_pair = db.alloc_type(Type::Pair(PairType {
        first: builtins.int,
        rest: builtins.int,
    }));

    db.alloc_symbol(Symbol::InlineFunction(Function {
        scope_id,
        hir_id,
        ty: FunctionType {
            param_types: vec![builtins.int, builtins.int],
            rest: Rest::Nil,
            return_type: int_pair,
            generic_types: Vec::new(),
        },
    }))
}
