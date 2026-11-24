use std::mem;

use id_arena::Arena;
use indexmap::IndexSet;
use rue_diagnostic::Name;

use crate::{BuiltinTypes, Comparison, Type, TypeId, compare};

/// The name of the field at the start of an enum variant that holds its discriminant. It can't be
/// written in source code, so it's never named by a field access, initializer or binding, and is
/// left out wherever a struct's fields are shown.
pub const DISCRIMINANT_FIELD: &str = "{discriminant}";

#[derive(Debug, Clone)]
pub struct Struct {
    pub name: Option<Name>,
    pub semantic: TypeId,
    pub inner: TypeId,
    pub generics: Vec<TypeId>,
    pub fields: IndexSet<String>,
    pub nil_terminated: bool,
    /// How the fields are arranged in the value.
    pub layout: StructLayout,
    /// Whether the inner type has been narrowed, in which case the generic arguments no longer
    /// describe the fields.
    pub narrowed: bool,
}

impl Struct {
    /// Where each of the fields is in the value.
    pub fn shape(&self) -> FieldShape {
        FieldShape::new(self.fields.len(), self.nil_terminated, self.layout)
    }

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

/// A step from a pair to one of its parts.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum TypePath {
    First,
    Rest,
}

/// How the fields of a struct are arranged in its value.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum StructLayout {
    /// A list of the fields, which ends in nil unless the last field is spread into it.
    List,
    /// A binary tree of the fields, as balanced as it can be, in the order they're declared
    /// (`tree struct`). The first `prefix` fields come before it as a list that ends in the tree,
    /// which is where an enum variant's discriminant is, so that it's in the same place in every
    /// variant.
    Tree { prefix: usize },
}

impl StructLayout {
    pub fn is_tree(self) -> bool {
        matches!(self, Self::Tree { .. })
    }
}

/// Where each field of a struct is in its value (see [`StructLayout`]).
#[derive(Debug, PartialEq, Eq)]
pub enum FieldShape {
    Nil,
    /// The field with the index.
    Field(usize),
    Pair(Box<FieldShape>, Box<FieldShape>),
}

impl FieldShape {
    pub fn new(len: usize, nil_terminated: bool, layout: StructLayout) -> Self {
        match layout {
            StructLayout::List => {
                let mut shape = Self::Nil;

                for index in (0..len).rev() {
                    shape = if !nil_terminated && index == len - 1 {
                        Self::Field(index)
                    } else {
                        Self::Pair(Box::new(Self::Field(index)), Box::new(shape))
                    };
                }

                shape
            }
            StructLayout::Tree { prefix } => {
                let prefix = prefix.min(len);
                let mut shape = Self::tree(prefix, len);

                for index in (0..prefix).rev() {
                    shape = Self::Pair(Box::new(Self::Field(index)), Box::new(shape));
                }

                shape
            }
        }
    }

    /// A tree of the fields from `start` up to `end`, with the first half of them (rounded down)
    /// in its first part. This is the shape a function's parameters have when they're each
    /// referenced as often as the others.
    fn tree(start: usize, end: usize) -> Self {
        match end - start {
            0 => Self::Nil,
            1 => Self::Field(start),
            len => {
                let split = start + len / 2;
                Self::Pair(
                    Box::new(Self::tree(start, split)),
                    Box::new(Self::tree(split, end)),
                )
            }
        }
    }

    /// The steps from the value to the field with the index, if it's in the shape.
    pub fn path(&self, index: usize) -> Option<Vec<TypePath>> {
        let mut path = Vec::new();
        let mut shape = self;

        loop {
            match shape {
                Self::Nil => return None,
                Self::Field(field) => return (*field == index).then_some(path),
                Self::Pair(first, rest) => {
                    // The fields are in order, so the field is in the first part unless the rest
                    // starts at or before it
                    if rest.first_field().is_none_or(|start| index < start) {
                        path.push(TypePath::First);
                        shape = first;
                    } else {
                        path.push(TypePath::Rest);
                        shape = rest;
                    }
                }
            }
        }
    }

    /// The index of the first field in the shape, if it has any.
    fn first_field(&self) -> Option<usize> {
        let mut shape = self;

        loop {
            match shape {
                Self::Nil => return None,
                Self::Field(field) => return Some(*field),
                Self::Pair(first, _) => shape = first,
            }
        }
    }

    /// Builds a value with the shape, from one for each field (given its index) or nil (given
    /// `None`), and the pairs of them. This goes through the shape iteratively, since a list is as
    /// deep as it has fields.
    pub fn fold<T>(
        &self,
        mut leaf: impl FnMut(Option<usize>) -> T,
        mut pair: impl FnMut(T, T) -> T,
    ) -> T {
        enum Task<'a> {
            Visit(&'a FieldShape),
            Pair,
        }

        let mut tasks = vec![Task::Visit(self)];
        let mut values = Vec::new();

        while let Some(task) = tasks.pop() {
            match task {
                Task::Visit(Self::Nil) => values.push(leaf(None)),
                Task::Visit(Self::Field(index)) => values.push(leaf(Some(*index))),
                Task::Visit(Self::Pair(first, rest)) => {
                    tasks.push(Task::Pair);
                    tasks.push(Task::Visit(rest));
                    tasks.push(Task::Visit(first));
                }
                Task::Pair => {
                    let rest = values.pop().expect("a pair has a rest");
                    let first = values.pop().expect("a pair has a first");
                    values.push(pair(first, rest));
                }
            }
        }

        values.pop().expect("the shape has a value")
    }
}

/// A list is as deep as it has fields, so it's taken apart iteratively.
impl Drop for FieldShape {
    fn drop(&mut self) {
        let mut stack = Vec::new();

        if let Self::Pair(first, rest) = self {
            stack.push(mem::replace(&mut **first, Self::Nil));
            stack.push(mem::replace(&mut **rest, Self::Nil));
        }

        while let Some(mut shape) = stack.pop() {
            if let Self::Pair(first, rest) = &mut shape {
                stack.push(mem::replace(&mut **first, Self::Nil));
                stack.push(mem::replace(&mut **rest, Self::Nil));
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    /// The shape written out as pairs of field indices, with `()` for nil.
    fn show(shape: &FieldShape) -> String {
        shape.fold(
            |index| index.map_or_else(|| "()".to_string(), |index| index.to_string()),
            |first, rest| format!("({first} . {rest})"),
        )
    }

    fn tree(len: usize, prefix: usize) -> String {
        show(&FieldShape::new(len, true, StructLayout::Tree { prefix }))
    }

    #[test]
    fn test_list_shape() {
        let list =
            |len, nil_terminated| show(&FieldShape::new(len, nil_terminated, StructLayout::List));

        assert_eq!(list(0, true), "()");
        assert_eq!(list(2, true), "(0 . (1 . ()))");
        assert_eq!(list(1, false), "0");
        assert_eq!(list(3, false), "(0 . (1 . 2))");
    }

    #[test]
    fn test_tree_shape() {
        assert_eq!(tree(0, 0), "()");
        assert_eq!(tree(1, 0), "0");
        assert_eq!(tree(2, 0), "(0 . 1)");
        assert_eq!(tree(3, 0), "(0 . (1 . 2))");
        assert_eq!(tree(4, 0), "((0 . 1) . (2 . 3))");
        assert_eq!(tree(5, 0), "((0 . 1) . (2 . (3 . 4)))");
        assert_eq!(tree(7, 0), "((0 . (1 . 2)) . ((3 . 4) . (5 . 6)))");

        // An enum variant's discriminant comes before the tree of its fields
        assert_eq!(tree(1, 1), "(0 . ())");
        assert_eq!(tree(2, 1), "(0 . 1)");
        assert_eq!(tree(4, 1), "(0 . (1 . (2 . 3)))");
        assert_eq!(tree(5, 1), "(0 . ((1 . 2) . (3 . 4)))");
    }

    #[test]
    fn test_paths() {
        use TypePath::{First, Rest};

        for (len, nil_terminated, layout) in [
            (6, true, StructLayout::List),
            (6, false, StructLayout::List),
            (7, true, StructLayout::Tree { prefix: 0 }),
            (7, true, StructLayout::Tree { prefix: 1 }),
        ] {
            let shape = FieldShape::new(len, nil_terminated, layout);

            // Each field's path leads to it, and no two fields share one
            let paths: Vec<_> = (0..len).map(|index| shape.path(index).unwrap()).collect();

            for (index, path) in paths.iter().enumerate() {
                let mut part = &shape;

                for step in path {
                    let FieldShape::Pair(first, rest) = part else {
                        panic!("the path goes past a field");
                    };

                    part = match step {
                        First => first,
                        Rest => rest,
                    };
                }

                assert_eq!(part, &FieldShape::Field(index));
            }

            assert_eq!(shape.path(len), None);
        }

        let shape = FieldShape::new(5, true, StructLayout::Tree { prefix: 0 });
        assert_eq!(shape.path(0), Some(vec![First, First]));
        assert_eq!(shape.path(4), Some(vec![Rest, Rest, Rest]));
    }

    #[test]
    fn test_deep_list() {
        // A list as long as this would overflow the stack if it were built, folded, searched or
        // dropped recursively
        let shape = FieldShape::new(1_000_000, true, StructLayout::List);
        assert_eq!(shape.path(999_999).unwrap().len(), 1_000_000);
        assert_eq!(
            shape.fold(|_| 1usize, |first, rest| first + rest),
            1_000_001
        );
    }
}
