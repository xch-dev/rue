use crate::{Atom, Database, Hir, Scope, ScopeId, Type, TypeId, Value};

#[derive(Debug, Clone)]
pub struct Builtins {
    pub scope: ScopeId,
    pub unresolved: Value,
    pub bytes: TypeId,
    pub bytes32: TypeId,
    pub public_key: TypeId,
    pub int: TypeId,
    pub bool: TypeId,
    pub nil: Value,
    pub true_value: Value,
    pub false_value: Value,
}

impl Builtins {
    pub fn new(db: &mut Database) -> Self {
        let unresolved_type = db.alloc_type(Type::Unresolved);
        let unresolved_hir = db.alloc_hir(Hir::Unresolved);

        let bytes = db.alloc_type(Type::Atom(Atom::Bytes));
        let bytes32 = db.alloc_type(Type::Atom(Atom::Bytes32));
        let public_key = db.alloc_type(Type::Atom(Atom::PublicKey));
        let int = db.alloc_type(Type::Atom(Atom::Int));
        let bool = db.alloc_type(Type::Atom(Atom::Bool));

        let nil_hir = db.alloc_hir(Hir::Nil);
        let true_hir = db.alloc_hir(Hir::Bool(true));
        let false_hir = db.alloc_hir(Hir::Bool(false));

        let nil_type = db.alloc_type(Type::Atom(Atom::Nil));
        let true_type = db.alloc_type(Type::Atom(Atom::BoolValue(true)));
        let false_type = db.alloc_type(Type::Atom(Atom::BoolValue(false)));

        let mut scope = Scope::new();

        scope.insert_type("Bytes".to_string(), bytes);
        scope.insert_type("Bytes32".to_string(), bytes32);
        scope.insert_type("PublicKey".to_string(), public_key);
        scope.insert_type("Int".to_string(), int);
        scope.insert_type("Bool".to_string(), bool);

        let scope = db.alloc_scope(scope);

        Self {
            scope,
            unresolved: Value::new(unresolved_hir, unresolved_type),
            bytes,
            bytes32,
            public_key,
            int,
            bool,
            nil: Value::new(nil_hir, nil_type),
            true_value: Value::new(true_hir, true_type),
            false_value: Value::new(false_hir, false_type),
        }
    }
}
