//! The walk over the members of a union that several operations share, down through references,
//! aliases, nested unions and (unless they're wanted as they are) structs, to the types that
//! aren't any of those.

use std::{convert::Infallible, ops::ControlFlow};

use id_arena::Arena;

use crate::{FxHashMap, FxHashSet, Type, TypeId, grow_stack, step};

/// What a walk over the leaves of a type goes through, and what it treats as the same leaf.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum Walk {
    /// Goes through structs, and visits each type once.
    Leaves,
    /// Stops at structs, which are leaves, and visits each type once.
    ToStructs,
    /// Goes through structs, and visits each type once for each path of aliases and structs that
    /// it's reached through, since the path is part of what's visited.
    WithPaths,
}

/// An empty union is visited as `Never`, which it's the same as, so that every walk treats the two
/// the same way.
const NEVER: &Type = &Type::Never;

/// Calls `visit` on each leaf of the type, in the order the members are written, along with the
/// aliases and structs it was reached through, outermost first. A leaf is never a reference, an
/// alias, a union or an application, and only a struct with [`Walk::ToStructs`].
///
/// A type that's reached again is skipped, along with everything under it, since it would only add
/// the same leaves again: a union that repeats a type at each level, such as `type U1 = U0 | U0`,
/// would otherwise double the work at each level (#472, #496). With [`Walk::WithPaths`], a type is
/// only skipped if it's reached again through the same aliases and structs. This also stops at a
/// non-contractive cycle, which doesn't add any leaves.
///
/// The walk stops as soon as `visit` breaks, and returns what it broke with.
pub(crate) fn walk_leaves<B>(
    arena: &Arena<Type>,
    id: TypeId,
    walk: Walk,
    visit: impl FnMut(TypeId, &Type, &[TypeId]) -> ControlFlow<B>,
) -> ControlFlow<B> {
    let mut walker = Walker {
        arena,
        walk,
        visit,
        seen: FxHashSet::default(),
        paths: FxHashMap::default(),
        path: Vec::new(),
        path_index: 0,
    };

    walker.walk(id)
}

/// Like [`walk_leaves`], for a visit that never stops the walk.
pub(crate) fn for_each_leaf(
    arena: &Arena<Type>,
    id: TypeId,
    walk: Walk,
    mut visit: impl FnMut(TypeId, &Type, &[TypeId]),
) {
    let ControlFlow::Continue(()) = walk_leaves::<Infallible>(arena, id, walk, |id, ty, path| {
        visit(id, ty, path);
        ControlFlow::Continue(())
    });
}

struct Walker<'a, F> {
    arena: &'a Arena<Type>,
    walk: Walk,
    visit: F,
    /// The types that have been reached, with the index of the path they were reached through
    /// (always 0 unless the walk is [`Walk::WithPaths`]).
    seen: FxHashSet<(TypeId, usize)>,
    /// An index for each path, by the index of the path it extends and the alias or struct it
    /// extends it with. The empty path is 0.
    paths: FxHashMap<(usize, TypeId), usize>,
    /// The aliases and structs that the current type was reached through, outermost first.
    path: Vec<TypeId>,
    path_index: usize,
}

impl<B, F> Walker<'_, F>
where
    F: FnMut(TypeId, &Type, &[TypeId]) -> ControlFlow<B>,
{
    fn walk(&mut self, id: TypeId) -> ControlFlow<B> {
        step();

        if !self.seen.insert((id, self.path_index)) {
            return ControlFlow::Continue(());
        }

        grow_stack(|| self.walk_inner(id))
    }

    fn walk_inner(&mut self, id: TypeId) -> ControlFlow<B> {
        let arena = self.arena;

        match &arena[id] {
            Type::Apply(_) => unreachable!(),
            &Type::Ref(inner) => self.walk(inner),
            Type::Alias(alias) => self.walk_named(id, alias.inner),
            Type::Struct(ty) if self.walk != Walk::ToStructs => self.walk_named(id, ty.inner),
            Type::Union(union) if union.types.is_empty() => (self.visit)(id, NEVER, &self.path),
            Type::Union(union) => {
                for &member in &union.types {
                    self.walk(member)?;
                }

                ControlFlow::Continue(())
            }
            ty => (self.visit)(id, ty, &self.path),
        }
    }

    /// Walks the inner type of an alias or struct, with it added to the path.
    fn walk_named(&mut self, id: TypeId, inner: TypeId) -> ControlFlow<B> {
        let outer = self.path_index;

        if self.walk == Walk::WithPaths {
            let next = self.paths.len() + 1;
            self.path_index = *self.paths.entry((outer, id)).or_insert(next);
        }

        self.path.push(id);
        let result = self.walk(inner);
        self.path.pop();
        self.path_index = outer;

        result
    }
}

#[cfg(test)]
mod tests {
    use crate::{extract_atoms, test_types::Db};

    use super::*;

    /// The leaves of a type, each with the path it was reached through.
    fn leaves(db: &Db, id: TypeId, walk: Walk) -> Vec<(TypeId, Vec<TypeId>)> {
        let mut leaves = Vec::new();
        for_each_leaf(&db.arena, id, walk, |id, _, path| {
            leaves.push((id, path.to_vec()));
        });
        leaves
    }

    #[test]
    fn test_walk_leaves_paths() {
        let mut db = Db::new();
        let (int, nil) = (db.builtins.int, db.builtins.nil);
        let a = db.alias("A", &[], int);
        let b = db.alias("B", &[], int);
        let s = db.strukt("S", a);
        let union = db.union(&[s, a, b, a, nil, s]);

        // The `Int` reached through `A` is the same leaf as the one reached through `S` and `A`,
        // which is reached first
        assert_eq!(
            leaves(&db, union, Walk::Leaves),
            [(int, vec![s, a]), (nil, vec![])]
        );

        // Unless the path is part of what's visited, in which case each path is visited once, with
        // the outermost alias or struct first
        assert_eq!(
            leaves(&db, union, Walk::WithPaths),
            [
                (int, vec![s, a]),
                (int, vec![a]),
                (int, vec![b]),
                (nil, vec![]),
            ]
        );

        // A struct is a leaf of its own if it's wanted as it is
        assert_eq!(
            leaves(&db, union, Walk::ToStructs),
            [(s, vec![]), (int, vec![a]), (nil, vec![])]
        );
    }

    #[test]
    fn test_walk_leaves_stops() {
        let mut db = Db::new();
        let (int, nil, bytes) = (db.builtins.int, db.builtins.nil, db.builtins.bytes);
        let union = db.union(&[int, nil, bytes]);

        let mut visited = Vec::new();
        let result = walk_leaves(&db.arena, union, Walk::Leaves, |id, _, _| {
            visited.push(id);
            if id == nil {
                ControlFlow::Break(id)
            } else {
                ControlFlow::Continue(())
            }
        });

        assert_eq!(result, ControlFlow::Break(nil));
        assert_eq!(visited, [int, nil]);
    }

    #[test]
    fn test_walk_leaves_empty_union() {
        let mut db = Db::new();
        let int = db.builtins.int;
        let empty = db.union(&[]);
        let union = db.union(&[int, empty]);

        // An empty union is visited as `Never`, which contributes nothing, and so makes a strict
        // extraction empty
        assert!(matches!(
            extract_atoms(&mut db.arena, union, false),
            Some(crate::Atoms::Unrestricted)
        ));
        assert!(extract_atoms(&mut db.arena, union, true).is_none());
    }
}
