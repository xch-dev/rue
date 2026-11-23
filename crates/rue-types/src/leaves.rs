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
