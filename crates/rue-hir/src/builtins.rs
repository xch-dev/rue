use rue_types::{BuiltinTypes, FunctionType, Generic, Pair, Type, TypeId, Union};

use crate::{
    Builtin, ConstantSymbol, Database, FunctionKind, FunctionSymbol, Hir, ParameterSymbol, Scope,
    ScopeId, Symbol, SymbolId, Value,
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
        scope.insert_type("K1PublicKey".to_string(), types.k1_public_key, false);
        scope.insert_type("K1Signature".to_string(), types.k1_signature, false);
        scope.insert_type("R1PublicKey".to_string(), types.r1_public_key, false);
        scope.insert_type("R1Signature".to_string(), types.r1_signature, false);
        scope.insert_type("Int".to_string(), types.int, false);
        scope.insert_type("Bool".to_string(), types.bool, false);
        scope.insert_type("Any".to_string(), types.permissive_any, false);
        scope.insert_type("List".to_string(), types.list, false);
        scope.insert_type("AlternatingList".to_string(), types.alternating_list, false);

        let unchecked_cast_generic = db.alloc_type(Type::Generic(Generic { name: None }));

        scope.insert_symbol(
            "unchecked_cast".to_string(),
            unchecked_cast(db, unchecked_cast_generic, types.permissive_any),
            false,
        );

        scope.insert_symbol(
            "sha256".to_string(),
            db.alloc_symbol(Symbol::Builtin(Builtin::Sha256 { inline: false })),
            false,
        );

        scope.insert_symbol(
            "sha256_inline".to_string(),
            db.alloc_symbol(Symbol::Builtin(Builtin::Sha256 { inline: true })),
            false,
        );

        scope.insert_symbol(
            "keccak256".to_string(),
            db.alloc_symbol(Symbol::Builtin(Builtin::Keccak256 { inline: false })),
            false,
        );

        scope.insert_symbol(
            "keccak256_inline".to_string(),
            db.alloc_symbol(Symbol::Builtin(Builtin::Keccak256 { inline: true })),
            false,
        );

        scope.insert_symbol(
            "concat".to_string(),
            db.alloc_symbol(Symbol::Builtin(Builtin::Concat)),
            false,
        );

        scope.insert_symbol(
            "coinid".to_string(),
            db.alloc_symbol(Symbol::Builtin(Builtin::CoinId)),
            false,
        );

        scope.insert_symbol(
            "substr".to_string(),
            db.alloc_symbol(Symbol::Builtin(Builtin::Substr)),
            false,
        );

        scope.insert_symbol(
            "sum".to_string(),
            db.alloc_symbol(Symbol::Builtin(Builtin::Sum)),
            false,
        );

        scope.insert_symbol(
            "difference".to_string(),
            db.alloc_symbol(Symbol::Builtin(Builtin::Difference)),
            false,
        );

        scope.insert_symbol(
            "product".to_string(),
            db.alloc_symbol(Symbol::Builtin(Builtin::Product)),
            false,
        );

        scope.insert_symbol(
            "divmod".to_string(),
            db.alloc_symbol(Symbol::Builtin(Builtin::Divmod)),
            false,
        );

        scope.insert_symbol(
            "modpow".to_string(),
            db.alloc_symbol(Symbol::Builtin(Builtin::Modpow)),
            false,
        );

        scope.insert_symbol(
            "any".to_string(),
            db.alloc_symbol(Symbol::Builtin(Builtin::Any)),
            false,
        );

        scope.insert_symbol(
            "all".to_string(),
            db.alloc_symbol(Symbol::Builtin(Builtin::All)),
            false,
        );

        scope.insert_symbol(
            "pubkey_for_exp".to_string(),
            db.alloc_symbol(Symbol::Builtin(Builtin::PubkeyForExp)),
            false,
        );

        scope.insert_symbol(
            "g1_sum".to_string(),
            db.alloc_symbol(Symbol::Builtin(Builtin::G1Sum)),
            false,
        );

        scope.insert_symbol(
            "g1_difference".to_string(),
            db.alloc_symbol(Symbol::Builtin(Builtin::G1Difference)),
            false,
        );

        scope.insert_symbol(
            "g2_sum".to_string(),
            db.alloc_symbol(Symbol::Builtin(Builtin::G2Sum)),
            false,
        );

        scope.insert_symbol(
            "g2_difference".to_string(),
            db.alloc_symbol(Symbol::Builtin(Builtin::G2Difference)),
            false,
        );

        scope.insert_symbol(
            "bls_pairing_identity".to_string(),
            db.alloc_symbol(Symbol::Builtin(Builtin::BlsPairingIdentity)),
            false,
        );

        scope.insert_symbol(
            "bls_verify".to_string(),
            db.alloc_symbol(Symbol::Builtin(Builtin::BlsVerify)),
            false,
        );

        scope.insert_symbol(
            "secp256k1_verify".to_string(),
            db.alloc_symbol(Symbol::Builtin(Builtin::Secp256K1Verify)),
            false,
        );

        scope.insert_symbol(
            "secp256r1_verify".to_string(),
            db.alloc_symbol(Symbol::Builtin(Builtin::Secp256R1Verify)),
            false,
        );

        let infinity_g1 = db.alloc_hir(Hir::InfinityG1);
        let infinity_g2 = db.alloc_hir(Hir::InfinityG2);

        scope.insert_symbol(
            "INFINITY_G1".to_string(),
            db.alloc_symbol(Symbol::Constant(ConstantSymbol {
                name: None,
                value: Value::new(infinity_g1, types.public_key),
                inline: false,
            })),
            false,
        );

        scope.insert_symbol(
            "INFINITY_G2".to_string(),
            db.alloc_symbol(Symbol::Constant(ConstantSymbol {
                name: None,
                value: Value::new(infinity_g2, types.signature),
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
