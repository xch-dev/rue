use rue_lir::ClvmOp;
use rue_types::{BuiltinTypes, FunctionType, Generic, Pair, Type, TypeId, Union};
use strum::IntoEnumIterator;

use crate::{
    ConstantSymbol, Database, FunctionKind, FunctionSymbol, Hir, ModuleDeclarations, ModuleSymbol,
    ParameterSymbol, Scope, ScopeId, Symbol, SymbolId, UnaryOp, Value,
};

#[derive(Debug, Clone)]
pub struct Builtins {
    pub types: BuiltinTypes,
    pub scope: ScopeId,
    pub unresolved: Value,
    pub nil: Value,
    pub true_value: Value,
    pub false_value: Value,
}

impl Builtins {
    pub fn new(db: &mut Database) -> Self {
        let types = BuiltinTypes::new(db.types_mut());

        let unresolved_hir = db.alloc_hir(Hir::Unresolved);

        let nil_hir = db.alloc_hir(Hir::Nil);
        let true_hir = db.alloc_hir(Hir::Bool(true));
        let false_hir = db.alloc_hir(Hir::Bool(false));

        let function_inner = db.alloc_type(Type::Unresolved);
        let function_inner_pair =
            db.alloc_type(Type::Pair(Pair::new(function_inner, function_inner)));
        *db.ty_mut(function_inner_pair) =
            Type::Union(Union::new(vec![types.atom, function_inner_pair]));

        let mut scope = Scope::new();

        scope.insert_type("Atom".to_string(), types.atom, false);
        scope.insert_type("Bytes".to_string(), types.bytes, false);
        scope.insert_type("Bytes32".to_string(), types.bytes32, false);
        scope.insert_type("PublicKey".to_string(), types.public_key, false);
        scope.insert_type("Signature".to_string(), types.signature, false);
        scope.insert_type("Int".to_string(), types.int, false);
        scope.insert_type("Bool".to_string(), types.bool, false);
        scope.insert_type("Any".to_string(), types.permissive_any, false);
        scope.insert_type("List".to_string(), types.list, false);

        let unchecked_cast_generic = db.alloc_type(Type::Generic(Generic { name: None }));

        scope.insert_symbol(
            "unchecked_cast".to_string(),
            unchecked_cast(db, unchecked_cast_generic, types.permissive_any),
            false,
        );
        scope.insert_symbol(
            "sha256".to_string(),
            sha256(db, types.bytes, types.bytes32, false),
            false,
        );
        scope.insert_symbol(
            "sha256_inline".to_string(),
            sha256(db, types.bytes, types.bytes32, true),
            false,
        );
        scope.insert_symbol(
            "keccak256".to_string(),
            keccak256(db, types.bytes, types.bytes32, false),
            false,
        );
        scope.insert_symbol(
            "keccak256_inline".to_string(),
            keccak256(db, types.bytes, types.bytes32, true),
            false,
        );
        scope.insert_symbol(
            "coinid".to_string(),
            coinid(db, types.bytes, types.bytes32, types.int),
            false,
        );
        scope.insert_symbol(
            "substr".to_string(),
            substr(db, types.bytes, types.int, true),
            false,
        );
        scope.insert_symbol(
            "substr_start".to_string(),
            substr(db, types.bytes, types.int, false),
            false,
        );
        scope.insert_symbol(
            "g1_map".to_string(),
            g1_or_g2_map(db, types.atom, types.public_key, false, false),
            false,
        );
        scope.insert_symbol(
            "g1_map_dst".to_string(),
            g1_or_g2_map(db, types.atom, types.public_key, true, false),
            false,
        );
        scope.insert_symbol(
            "g2_map".to_string(),
            g1_or_g2_map(db, types.atom, types.signature, false, true),
            false,
        );
        scope.insert_symbol(
            "g2_map_dst".to_string(),
            g1_or_g2_map(db, types.atom, types.signature, true, true),
            false,
        );
        scope.insert_symbol(
            "pubkey_for_exp".to_string(),
            pubkey_for_exp(db, types.bytes, types.public_key),
            false,
        );
        scope.insert_symbol("modpow".to_string(), modpow(db, types.int), false);

        let mut clvm = Scope::new();

        for op in ClvmOp::iter() {
            clvm.insert_symbol(op.to_string(), db.alloc_symbol(Symbol::ClvmOp(op)), true);
        }

        let clvm_scope = db.alloc_scope(clvm);
        let clvm_module = db.alloc_symbol(Symbol::Module(ModuleSymbol {
            name: None,
            scope: clvm_scope,
            declarations: ModuleDeclarations::default(),
        }));
        scope.insert_symbol("clvm".to_string(), clvm_module, false);

        let infinity_g1 = db.alloc_hir(Hir::InfinityG1);
        let infinity_g2 = db.alloc_hir(Hir::InfinityG2);
        scope.insert_symbol(
            "INFINITY_G1".to_string(),
            db.alloc_symbol(Symbol::Constant(ConstantSymbol {
                name: None,
                ty: types.public_key,
                value: infinity_g1,
                inline: false,
            })),
            false,
        );
        scope.insert_symbol(
            "INFINITY_G2".to_string(),
            db.alloc_symbol(Symbol::Constant(ConstantSymbol {
                name: None,
                ty: types.signature,
                value: infinity_g2,
                inline: false,
            })),
            false,
        );

        let scope = db.alloc_scope(scope);

        Self {
            types,
            scope,
            unresolved: Value::new(unresolved_hir, types.unresolved),
            nil: Value::new(nil_hir, types.nil),
            true_value: Value::new(true_hir, types.bool_true),
            false_value: Value::new(false_hir, types.bool_false),
        }
    }
}

fn unchecked_cast(db: &mut Database, generic: TypeId, any: TypeId) -> SymbolId {
    let scope = db.alloc_scope(Scope::new());

    let parameter = db.alloc_symbol(Symbol::Parameter(ParameterSymbol {
        name: None,
        ty: any,
    }));

    db.scope_mut(scope)
        .insert_symbol("value".to_string(), parameter, false);

    let reference = db.alloc_hir(Hir::Reference(parameter));

    let ty = db.alloc_type(Type::Function(FunctionType {
        params: vec![any],
        nil_terminated: true,
        ret: generic,
    }));

    db.alloc_symbol(Symbol::Function(FunctionSymbol {
        name: None,
        ty,
        scope,
        vars: vec![generic],
        parameters: vec![parameter],
        nil_terminated: true,
        return_type: generic,
        body: reference,
        kind: FunctionKind::Inline,
    }))
}

fn sha256(db: &mut Database, bytes: TypeId, bytes32: TypeId, inline: bool) -> SymbolId {
    let scope = db.alloc_scope(Scope::new());

    let parameter = db.alloc_symbol(Symbol::Parameter(ParameterSymbol {
        name: None,
        ty: bytes,
    }));

    db.scope_mut(scope)
        .insert_symbol("value".to_string(), parameter, false);

    let reference = db.alloc_hir(Hir::Reference(parameter));
    let body = db.alloc_hir(Hir::Unary(
        if inline {
            UnaryOp::Sha256Inline
        } else {
            UnaryOp::Sha256
        },
        reference,
    ));

    let ty = db.alloc_type(Type::Function(FunctionType {
        params: vec![bytes],
        nil_terminated: true,
        ret: bytes32,
    }));

    db.alloc_symbol(Symbol::Function(FunctionSymbol {
        name: None,
        ty,
        scope,
        vars: vec![],
        parameters: vec![parameter],
        nil_terminated: true,
        return_type: bytes32,
        body,
        kind: FunctionKind::Inline,
    }))
}

fn keccak256(db: &mut Database, bytes: TypeId, bytes32: TypeId, inline: bool) -> SymbolId {
    let scope = db.alloc_scope(Scope::new());

    let parameter = db.alloc_symbol(Symbol::Parameter(ParameterSymbol {
        name: None,
        ty: bytes,
    }));

    db.scope_mut(scope)
        .insert_symbol("value".to_string(), parameter, false);

    let reference = db.alloc_hir(Hir::Reference(parameter));
    let body = db.alloc_hir(Hir::Unary(
        if inline {
            UnaryOp::Keccak256Inline
        } else {
            UnaryOp::Keccak256
        },
        reference,
    ));

    let ty = db.alloc_type(Type::Function(FunctionType {
        params: vec![bytes],
        nil_terminated: true,
        ret: bytes32,
    }));

    db.alloc_symbol(Symbol::Function(FunctionSymbol {
        name: None,
        ty,
        scope,
        vars: vec![],
        parameters: vec![parameter],
        nil_terminated: true,
        return_type: bytes32,
        body,
        kind: FunctionKind::Inline,
    }))
}

fn coinid(db: &mut Database, bytes: TypeId, bytes32: TypeId, int: TypeId) -> SymbolId {
    let scope = db.alloc_scope(Scope::new());

    let parent = db.alloc_symbol(Symbol::Parameter(ParameterSymbol {
        name: None,
        ty: bytes,
    }));

    let puzzle = db.alloc_symbol(Symbol::Parameter(ParameterSymbol {
        name: None,
        ty: bytes,
    }));

    let amount = db.alloc_symbol(Symbol::Parameter(ParameterSymbol {
        name: None,
        ty: int,
    }));

    db.scope_mut(scope)
        .insert_symbol("parent".to_string(), parent, false);

    db.scope_mut(scope)
        .insert_symbol("puzzle".to_string(), puzzle, false);

    db.scope_mut(scope)
        .insert_symbol("amount".to_string(), amount, false);

    let parent_ref = db.alloc_hir(Hir::Reference(parent));
    let puzzle_ref = db.alloc_hir(Hir::Reference(puzzle));
    let amount_ref = db.alloc_hir(Hir::Reference(amount));
    let body = db.alloc_hir(Hir::CoinId(parent_ref, puzzle_ref, amount_ref));

    let ty = db.alloc_type(Type::Function(FunctionType {
        params: vec![bytes, bytes, int],
        nil_terminated: true,
        ret: bytes32,
    }));

    db.alloc_symbol(Symbol::Function(FunctionSymbol {
        name: None,
        ty,
        scope,
        vars: vec![],
        parameters: vec![parent, puzzle, amount],
        nil_terminated: true,
        return_type: bytes32,
        body,
        kind: FunctionKind::Inline,
    }))
}

fn modpow(db: &mut Database, int: TypeId) -> SymbolId {
    let scope = db.alloc_scope(Scope::new());

    let base = db.alloc_symbol(Symbol::Parameter(ParameterSymbol {
        name: None,
        ty: int,
    }));

    let exponent = db.alloc_symbol(Symbol::Parameter(ParameterSymbol {
        name: None,
        ty: int,
    }));

    let modulus = db.alloc_symbol(Symbol::Parameter(ParameterSymbol {
        name: None,
        ty: int,
    }));

    db.scope_mut(scope)
        .insert_symbol("base".to_string(), base, false);

    db.scope_mut(scope)
        .insert_symbol("exponent".to_string(), exponent, false);

    db.scope_mut(scope)
        .insert_symbol("modulus".to_string(), modulus, false);

    let base_ref = db.alloc_hir(Hir::Reference(base));
    let exponent_ref = db.alloc_hir(Hir::Reference(exponent));
    let modulus_ref = db.alloc_hir(Hir::Reference(modulus));
    let body = db.alloc_hir(Hir::Modpow(base_ref, exponent_ref, modulus_ref));

    let ty = db.alloc_type(Type::Function(FunctionType {
        params: vec![int, int, int],
        nil_terminated: true,
        ret: int,
    }));

    db.alloc_symbol(Symbol::Function(FunctionSymbol {
        name: None,
        ty,
        scope,
        vars: vec![],
        parameters: vec![base, exponent, modulus],
        nil_terminated: true,
        return_type: int,
        body,
        kind: FunctionKind::Inline,
    }))
}

fn g1_or_g2_map(
    db: &mut Database,
    atom: TypeId,
    ret: TypeId,
    include_dst: bool,
    is_g2: bool,
) -> SymbolId {
    let scope = db.alloc_scope(Scope::new());

    let data = db.alloc_symbol(Symbol::Parameter(ParameterSymbol {
        name: None,
        ty: atom,
    }));

    let dst = db.alloc_symbol(Symbol::Parameter(ParameterSymbol {
        name: None,
        ty: atom,
    }));

    db.scope_mut(scope)
        .insert_symbol("data".to_string(), data, false);

    if include_dst {
        db.scope_mut(scope)
            .insert_symbol("dst".to_string(), dst, false);
    }

    let data_ref = db.alloc_hir(Hir::Reference(data));
    let dst_ref = db.alloc_hir(Hir::Reference(dst));
    let body = db.alloc_hir(if is_g2 {
        Hir::G2Map(data_ref, include_dst.then_some(dst_ref))
    } else {
        Hir::G1Map(data_ref, include_dst.then_some(dst_ref))
    });

    let ty = db.alloc_type(Type::Function(FunctionType {
        params: if include_dst {
            vec![atom, atom]
        } else {
            vec![atom]
        },
        nil_terminated: true,
        ret,
    }));

    db.alloc_symbol(Symbol::Function(FunctionSymbol {
        name: None,
        ty,
        scope,
        vars: vec![],
        parameters: if include_dst {
            vec![data, dst]
        } else {
            vec![data]
        },
        nil_terminated: true,
        return_type: ret,
        body,
        kind: FunctionKind::Inline,
    }))
}

fn pubkey_for_exp(db: &mut Database, bytes: TypeId, public_key: TypeId) -> SymbolId {
    let scope = db.alloc_scope(Scope::new());

    let parameter = db.alloc_symbol(Symbol::Parameter(ParameterSymbol {
        name: None,
        ty: bytes,
    }));

    db.scope_mut(scope)
        .insert_symbol("secret_key".to_string(), parameter, false);

    let reference = db.alloc_hir(Hir::Reference(parameter));
    let body = db.alloc_hir(Hir::Unary(UnaryOp::PubkeyForExp, reference));

    let ty = db.alloc_type(Type::Function(FunctionType {
        params: vec![bytes],
        nil_terminated: true,
        ret: public_key,
    }));

    db.alloc_symbol(Symbol::Function(FunctionSymbol {
        name: None,
        ty,
        scope,
        vars: vec![],
        parameters: vec![parameter],
        nil_terminated: true,
        return_type: public_key,
        body,
        kind: FunctionKind::Inline,
    }))
}

fn substr(db: &mut Database, bytes: TypeId, int: TypeId, include_to: bool) -> SymbolId {
    let scope = db.alloc_scope(Scope::new());

    let input = db.alloc_symbol(Symbol::Parameter(ParameterSymbol {
        name: None,
        ty: bytes,
    }));

    let from = db.alloc_symbol(Symbol::Parameter(ParameterSymbol {
        name: None,
        ty: int,
    }));

    let to = db.alloc_symbol(Symbol::Parameter(ParameterSymbol {
        name: None,
        ty: int,
    }));

    db.scope_mut(scope)
        .insert_symbol("input".to_string(), input, false);

    db.scope_mut(scope)
        .insert_symbol("from".to_string(), from, false);

    if include_to {
        db.scope_mut(scope)
            .insert_symbol("to".to_string(), to, false);
    }

    let input_ref = db.alloc_hir(Hir::Reference(input));
    let from_ref = db.alloc_hir(Hir::Reference(from));
    let to_ref = db.alloc_hir(Hir::Reference(to));
    let body = db.alloc_hir(Hir::Substr(
        input_ref,
        from_ref,
        include_to.then_some(to_ref),
    ));

    let ty = db.alloc_type(Type::Function(FunctionType {
        params: if include_to {
            vec![bytes, int, int]
        } else {
            vec![bytes, int]
        },
        nil_terminated: true,
        ret: bytes,
    }));

    db.alloc_symbol(Symbol::Function(FunctionSymbol {
        name: None,
        ty,
        scope,
        vars: vec![],
        parameters: if include_to {
            vec![input, from, to]
        } else {
            vec![input, from]
        },
        nil_terminated: true,
        return_type: bytes,
        body,
        kind: FunctionKind::Inline,
    }))
}
