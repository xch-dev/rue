use id_arena::Arena;
use indexmap::IndexSet;
use rue_diagnostic::Name;

use crate::{BuiltinTypes, Comparison, Type, TypeId, compare};

#[derive(Debug, Clone)]
pub struct Struct {
    pub name: Option<Name>,
    pub semantic: TypeId,
    pub inner: TypeId,
    pub generics: Vec<TypeId>,
    pub fields: IndexSet<String>,
    pub nil_terminated: bool,
    /// Whether the inner type has been narrowed, in which case the generic arguments no longer
    /// describe the fields.
    pub narrowed: bool,
}

impl Struct {
    /// Replaces the inner type with a narrower one, keeping track of whether it's strictly narrower
    /// than before (rather than only being restructured).
    #[must_use]
    pub fn narrow(self, arena: &mut Arena<Type>, builtins: &BuiltinTypes, inner: TypeId) -> Self {
        let narrowed = self.narrowed
            || (inner != self.inner
                && compare(arena, builtins, self.inner, inner) > Comparison::Assign);

        Self {
            inner,
            narrowed,
            ..self
        }
    }
}
