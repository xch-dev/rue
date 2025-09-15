use rue_types::{BuiltinTypes, FunctionType, Pair, Type, TypeId, Union};

use crate::{
    Database, FunctionSymbol, Hir, ParameterSymbol, Scope, ScopeId, Symbol, SymbolId, UnaryOp,
    Value,
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

        scope.insert_type("Atom".to_string(), types.atom);
        scope.insert_type("Bytes".to_string(), types.bytes);
        scope.insert_type("Bytes32".to_string(), types.bytes32);
        scope.insert_type("PublicKey".to_string(), types.public_key);
        scope.insert_type("Int".to_string(), types.int);
        scope.insert_type("Bool".to_string(), types.bool);
        scope.insert_type("Any".to_string(), types.any);
        scope.insert_type("List".to_string(), types.list);

        scope.insert_symbol("sha256".to_string(), sha256(db, types.bytes, types.bytes32));
        scope.insert_symbol(
            "calculate_coin_id".to_string(),
            calculate_coin_id(db, types.bytes, types.bytes32, types.int),
        );
        scope.insert_symbol("substr".to_string(), substr(db, types.bytes, types.int));

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

fn sha256(db: &mut Database, bytes: TypeId, bytes32: TypeId) -> SymbolId {
    let scope = db.alloc_scope(Scope::new());

    let parameter = db.alloc_symbol(Symbol::Parameter(ParameterSymbol {
        name: None,
        ty: bytes,
    }));

    db.scope_mut(scope)
        .insert_symbol("value".to_string(), parameter);

    let reference = db.alloc_hir(Hir::Reference(parameter));
    let body = db.alloc_hir(Hir::Unary(UnaryOp::Sha256, reference));

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
        inline: true,
    }))
}

fn calculate_coin_id(db: &mut Database, bytes: TypeId, bytes32: TypeId, int: TypeId) -> SymbolId {
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
        .insert_symbol("parent".to_string(), parent);

    db.scope_mut(scope)
        .insert_symbol("puzzle".to_string(), puzzle);

    db.scope_mut(scope)
        .insert_symbol("amount".to_string(), amount);

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
        inline: true,
    }))
}

fn substr(db: &mut Database, bytes: TypeId, int: TypeId) -> SymbolId {
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
        .insert_symbol("input".to_string(), input);

    db.scope_mut(scope).insert_symbol("from".to_string(), from);

    db.scope_mut(scope).insert_symbol("to".to_string(), to);

    let input_ref = db.alloc_hir(Hir::Reference(input));
    let from_ref = db.alloc_hir(Hir::Reference(from));
    let to_ref = db.alloc_hir(Hir::Reference(to));
    let body = db.alloc_hir(Hir::Substr(input_ref, from_ref, Some(to_ref)));

    let ty = db.alloc_type(Type::Function(FunctionType {
        params: vec![bytes, int, int],
        nil_terminated: true,
        ret: bytes,
    }));

    db.alloc_symbol(Symbol::Function(FunctionSymbol {
        name: None,
        ty,
        scope,
        vars: vec![],
        parameters: vec![input, from, to],
        nil_terminated: true,
        return_type: bytes,
        body,
        inline: true,
    }))
}
