use std::collections::HashMap;

use rue_types::{BuiltinTypes, TypeId};

use crate::{Database, HirId, SymbolId, TypePath, intersect_types};

pub type Mappings = HashMap<SymbolId, HashMap<Vec<TypePath>, TypeId>>;

#[derive(Debug, Clone)]
#[must_use]
pub struct Value {
    pub hir: HirId,
    pub ty: TypeId,
    pub reference: Option<SymbolPath>,
    pub then_map: Mappings,
    pub else_map: Mappings,
}

impl Value {
    pub fn new(hir: HirId, ty: TypeId) -> Self {
        Self {
            hir,
            ty,
            reference: None,
            then_map: Mappings::new(),
            else_map: Mappings::new(),
        }
    }

    pub fn with_hir(self, hir: HirId) -> Self {
        Self { hir, ..self }
    }

    pub fn with_type(self, ty: TypeId) -> Self {
        Self { ty, ..self }
    }

    pub fn with_reference(self, reference: SymbolPath) -> Self {
        Self {
            reference: Some(reference),
            ..self
        }
    }

    pub fn with_mappings(self, then_map: Mappings, else_map: Mappings) -> Self {
        Self {
            then_map,
            else_map,
            ..self
        }
    }

    pub fn flip_mappings(self) -> Self {
        Self {
            then_map: self.else_map,
            else_map: self.then_map,
            ..self
        }
    }
}

#[derive(Debug, Clone)]
pub struct SymbolPath {
    pub symbol: SymbolId,
    pub path: Vec<TypePath>,
}

impl SymbolPath {
    pub fn new(symbol: SymbolId, path: Vec<TypePath>) -> Self {
        Self { symbol, path }
    }
}

/// Combines two sets of narrowing facts which are known to hold at the same time.
///
/// If both sets narrow the same path of the same symbol, the result is narrowed to the
/// intersection of both types rather than letting one side overwrite the other.
pub fn merge_mappings(
    db: &mut Database,
    builtins: &BuiltinTypes,
    a: &Mappings,
    b: &Mappings,
) -> Mappings {
    let mut map = a.clone();

    for (&symbol, mappings) in b {
        for (path, &ty) in mappings {
            let paths = map.entry(symbol).or_default();

            let ty = if let Some(&existing) = paths.get(path) {
                intersect_types(db, builtins, existing, ty)
            } else {
                ty
            };

            paths.insert(path.clone(), ty);
        }
    }

    map
}

#[cfg(test)]
mod tests {
    use rue_types::{Pair, Type, Union};

    use crate::Symbol;

    use super::*;

    fn mappings(entries: &[(SymbolId, Vec<TypePath>, TypeId)]) -> Mappings {
        let mut map = Mappings::new();
        for (symbol, path, ty) in entries {
            map.entry(*symbol).or_default().insert(path.clone(), *ty);
        }
        map
    }

    fn setup() -> (Database, BuiltinTypes, SymbolId, SymbolId) {
        let mut db = Database::new();
        let builtins = BuiltinTypes::new(db.types_mut());
        let x = db.alloc_symbol(Symbol::Unresolved);
        let y = db.alloc_symbol(Symbol::Unresolved);
        (db, builtins, x, y)
    }

    #[test]
    fn test_merge_disjoint() {
        let (mut db, builtins, x, y) = setup();

        let left = mappings(&[(x, vec![], builtins.int)]);
        let right = mappings(&[
            (y, vec![], builtins.bytes32),
            (x, vec![TypePath::First], builtins.nil),
        ]);
        let merged = merge_mappings(&mut db, &builtins, &left, &right);

        assert_eq!(merged.len(), 2);
        assert_eq!(merged[&x].len(), 2);
        assert_eq!(merged[&x][&vec![]], builtins.int);
        assert_eq!(merged[&x][&vec![TypePath::First]], builtins.nil);
        assert_eq!(merged[&y][&vec![]], builtins.bytes32);
    }

    #[test]
    fn test_merge_empty() {
        let (mut db, builtins, x, _) = setup();

        let left = mappings(&[(x, vec![], builtins.int)]);
        let merged = merge_mappings(&mut db, &builtins, &left, &Mappings::new());
        assert_eq!(merged[&x][&vec![]], builtins.int);

        let merged = merge_mappings(&mut db, &builtins, &Mappings::new(), &left);
        assert_eq!(merged[&x][&vec![]], builtins.int);
    }

    #[test]
    fn test_merge_same_path_intersects() {
        let (mut db, builtins, x, _) = setup();

        // `x: Bytes32 | nil | (Int, Int)`, after `(x is nil) | (x is Bytes32)` is false
        let pair = db.alloc_type(Type::Pair(Pair::new(builtins.int, builtins.int)));
        let not_nil = db.alloc_type(Type::Union(Union::new(vec![builtins.bytes32, pair])));
        let not_bytes32 = db.alloc_type(Type::Union(Union::new(vec![builtins.nil, pair])));

        let left = mappings(&[(x, vec![], not_nil)]);
        let right = mappings(&[(x, vec![], not_bytes32)]);
        let merged = merge_mappings(&mut db, &builtins, &left, &right);

        let ty = merged[&x][&vec![]];
        assert_eq!(rue_types::stringify(db.types_mut(), ty), "(Int, Int)");
    }

    #[test]
    fn test_merge_same_path_subset() {
        let (mut db, builtins, x, _) = setup();

        let atom_or_nil = db.alloc_type(Type::Union(Union::new(vec![
            builtins.bytes32,
            builtins.nil,
        ])));

        // Whichever side is more specific wins, regardless of order
        let left = mappings(&[(x, vec![TypePath::Rest], builtins.bytes32)]);
        let right = mappings(&[(x, vec![TypePath::Rest], atom_or_nil)]);
        assert_eq!(
            merge_mappings(&mut db, &builtins, &left, &right)[&x][&vec![TypePath::Rest]],
            builtins.bytes32
        );
        assert_eq!(
            merge_mappings(&mut db, &builtins, &right, &left)[&x][&vec![TypePath::Rest]],
            builtins.bytes32
        );
    }

    #[test]
    fn test_merge_same_path_disjoint() {
        let (mut db, builtins, x, _) = setup();

        // Both facts can't hold at once, so the value can't exist
        let pair = db.alloc_type(Type::Pair(Pair::new(builtins.int, builtins.int)));
        let left = mappings(&[(x, vec![], builtins.bytes32)]);
        let right = mappings(&[(x, vec![], pair)]);
        let merged = merge_mappings(&mut db, &builtins, &left, &right);

        assert_eq!(merged[&x][&vec![]], builtins.never);
    }
}
