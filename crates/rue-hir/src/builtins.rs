use rue_diagnostic::Name;
use rue_types::{BuiltinTypes, FunctionParameter, FunctionType, Generic, Type, TypeId};

use crate::{
    Builtin, ConstantSymbol, Database, FunctionCall, FunctionKind, FunctionSymbol, Hir, HirId,
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
    /// The function that a spread in a list before its last item is compiled to a call of (see
    /// [`spread_list`]). It isn't in any scope, so it can't be named, and it's only compiled into
    /// programs that spread a list there.
    pub spread_list: SymbolId,
}

impl Builtins {
    pub fn new(db: &mut Database) -> Self {
        let types = BuiltinTypes::new(db.types_mut());

        let unresolved_hir = db.alloc_hir(Hir::Unresolved);

        let nil_hir = db.alloc_hir(Hir::Nil);
        let true_hir = db.alloc_hir(Hir::Bool(true));
        let false_hir = db.alloc_hir(Hir::Bool(false));

        let mut scope = Scope::new(None);

        scope.insert_type("Atom".to_string(), types.atom, false);
        scope.insert_type("Bytes".to_string(), types.bytes, false);
        scope.insert_type("Bytes32".to_string(), types.bytes32, false);
        scope.insert_type("String".to_string(), types.string, false);
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

        let unchecked_cast_generic = db.alloc_type(Type::Generic(Generic {
            name: Some(Name::new("T", None)),
        }));

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
            "g1_map".to_string(),
            db.alloc_symbol(Symbol::Builtin(Builtin::G1Map)),
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
            "g2_map".to_string(),
            db.alloc_symbol(Symbol::Builtin(Builtin::G2Map)),
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

        scope.insert_symbol(
            "keccak256_verify".to_string(),
            db.alloc_symbol(Symbol::Builtin(Builtin::Keccak256Verify)),
            false,
        );

        let infinity_g1 = db.alloc_hir(Hir::InfinityG1);
        let infinity_g2 = db.alloc_hir(Hir::InfinityG2);

        scope.insert_symbol(
            "INFINITY_G1".to_string(),
            db.alloc_symbol(Symbol::Constant(ConstantSymbol {
                name: Some(Name::new("INFINITY_G1", None)),
                value: Value::new(infinity_g1, types.public_key),
                inline: false,
            })),
            false,
        );

        scope.insert_symbol(
            "INFINITY_G2".to_string(),
            db.alloc_symbol(Symbol::Constant(ConstantSymbol {
                name: Some(Name::new("INFINITY_G2", None)),
                value: Value::new(infinity_g2, types.signature),
                inline: false,
            })),
            false,
        );

        let scope = db.alloc_scope(scope);
        let spread_list = spread_list(db, &types);

        Self {
            types,
            scope,
            spread_list,
            unresolved: Value::new(unresolved_hir, types.unresolved),
            nil: Value::new(nil_hir, types.nil),
            true_value: Value::new(true_hir, types.bool_true),
            false_value: Value::new(false_hir, types.bool_false),
        }
    }

    /// Whether the HIR node is shared by every use of one of the builtin values, rather than
    /// belonging to a single expression.
    pub fn is_shared_hir(&self, hir: HirId) -> bool {
        [
            self.unresolved.hir,
            self.nil.hir,
            self.true_value.hir,
            self.false_value.hir,
        ]
        .contains(&hir)
    }
}

/// `fn spread(items: Any, rest: Any) -> Any`, which puts the items of a list in front of the
/// rest, so that `[...items, last]` is `spread(items, [last])`. The items are copied, since the
/// last pair of the list has to point to the rest rather than nil:
///
/// ```text
/// if items { (first(items), spread(rest(items), rest)) } else { rest }
/// ```
///
/// It's one function that every such spread calls, so a program that spreads lists before their
/// last item more than once only has the code once.
fn spread_list(db: &mut Database, types: &BuiltinTypes) -> SymbolId {
    let scope = db.alloc_scope(Scope::new(None));
    let any = types.permissive_any;

    let parameter = |db: &mut Database, name: &str| {
        let symbol = db.alloc_symbol(Symbol::Parameter(ParameterSymbol {
            name: Some(Name::new(name, None)),
            ty: any,
        }));
        db.scope_mut(scope)
            .insert_symbol(name.to_string(), symbol, false);
        symbol
    };

    let items = parameter(db, "items");
    let rest = parameter(db, "rest");

    let ty = db.alloc_type(Type::Function(FunctionType {
        generics: vec![],
        params: vec![
            FunctionParameter::new("items", any),
            FunctionParameter::new("rest", any),
        ],
        nil_terminated: true,
        ret: any,
    }));

    let placeholder = db.alloc_hir(Hir::Nil);

    let function = db.alloc_symbol(Symbol::Function(FunctionSymbol {
        name: Some(Name::new("spread", None)),
        ty,
        scope,
        vars: vec![],
        parameters: vec![items, rest],
        nil_terminated: true,
        return_type: any,
        body: placeholder,
        kind: FunctionKind::BinaryTree,
    }));

    let items_ref = |db: &mut Database| db.alloc_hir(Hir::Reference(items));
    let rest_ref = db.alloc_hir(Hir::Reference(rest));

    // The items are a list, so they're a pair unless they're nil
    let condition = items_ref(db);

    let first = items_ref(db);
    let first = db.alloc_hir(Hir::Unary(UnaryOp::First, first));
    let remaining = items_ref(db);
    let remaining = db.alloc_hir(Hir::Unary(UnaryOp::Rest, remaining));
    let callee = db.alloc_hir(Hir::Reference(function));
    let recursive_rest = db.alloc_hir(Hir::Reference(rest));
    let call = db.alloc_hir(Hir::FunctionCall(FunctionCall {
        function: callee,
        args: vec![remaining, recursive_rest],
        nil_terminated: true,
    }));
    let pair = db.alloc_hir(Hir::Pair(first, call));

    let body = db.alloc_hir(Hir::If(condition, pair, rest_ref, false));

    if let Symbol::Function(symbol) = db.symbol_mut(function) {
        symbol.body = body;
    }

    function
}

fn unchecked_cast(db: &mut Database, generic: TypeId, any: TypeId) -> SymbolId {
    // TODO: Use the parent scope
    let scope = db.alloc_scope(Scope::new(None));

    let parameter = db.alloc_symbol(Symbol::Parameter(ParameterSymbol {
        name: Some(Name::new("value", None)),
        ty: any,
    }));

    db.scope_mut(scope)
        .insert_symbol("value".to_string(), parameter, false);

    let reference = db.alloc_hir(Hir::Reference(parameter));

    let ty = db.alloc_type(Type::Function(FunctionType {
        generics: vec![generic],
        params: vec![FunctionParameter::new("value", any)],
        nil_terminated: true,
        ret: generic,
    }));

    db.alloc_symbol(Symbol::Function(FunctionSymbol {
        name: Some(Name::new("unchecked_cast", None)),
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
