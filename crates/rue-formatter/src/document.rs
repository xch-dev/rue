//! A deliberately small, private pretty-printing document model.

use std::rc::Rc;

/// Children are reference counted so that cloning a document is cheap. Layout choices such as
/// [`Doc::Fill`] hold a flat and a broken variant of the same contents, and without sharing,
/// every level of nesting would double the size of the document.
#[derive(Debug, Clone)]
pub enum Doc {
    Nil,
    Text(Rc<str>),
    Concat(Rc<[Self]>),
    Line(LineKind),
    Indent(Rc<Self>),
    Outdent(Rc<Self>),
    Group(Rc<Self>),
    /// A local fill boundary with continuation indentation applied only when
    /// that boundary breaks. Ordinary groups cannot express that conditional
    /// indentation when their surrounding binary chain is already broken.
    Fill {
        flat: Rc<Self>,
        broken: Rc<Self>,
        space_when_flat: bool,
        indent_levels_on_break: usize,
    },
    IfBreak {
        broken: Rc<Self>,
        flat: Rc<Self>,
    },
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum LineKind {
    Soft,
    Hard,
    Empty,
}

impl Doc {
    pub fn text(text: impl Into<Rc<str>>) -> Self {
        Self::Text(text.into())
    }

    pub fn concat(docs: impl IntoIterator<Item = Self>) -> Self {
        let mut flattened = Vec::new();
        for doc in docs {
            match doc {
                Self::Nil => {}
                Self::Concat(children) => flattened.extend(children.iter().cloned()),
                doc => flattened.push(doc),
            }
        }
        Self::Concat(flattened.into())
    }

    pub fn space() -> Self {
        Self::text(" ")
    }

    pub fn soft_line() -> Self {
        Self::Line(LineKind::Soft)
    }

    #[cfg(test)]
    pub fn fill(doc: Self, indent_levels_on_break: usize) -> Self {
        Self::Fill {
            flat: Rc::new(doc.clone()),
            broken: Rc::new(doc),
            space_when_flat: true,
            indent_levels_on_break,
        }
    }

    pub fn fill_choice(
        flat: Self,
        broken: Self,
        space_when_flat: bool,
        indent_levels_on_break: usize,
    ) -> Self {
        Self::Fill {
            flat: Rc::new(flat),
            broken: Rc::new(broken),
            space_when_flat,
            indent_levels_on_break,
        }
    }

    pub fn hard_line() -> Self {
        Self::Line(LineKind::Hard)
    }

    pub fn empty_line() -> Self {
        Self::Line(LineKind::Empty)
    }

    pub fn indent(self) -> Self {
        Self::Indent(Rc::new(self))
    }

    pub fn outdent(self) -> Self {
        Self::Outdent(Rc::new(self))
    }

    pub fn group(self) -> Self {
        Self::Group(Rc::new(self))
    }

    pub fn if_break(broken: Self, flat: Self) -> Self {
        Self::IfBreak {
            broken: Rc::new(broken),
            flat: Rc::new(flat),
        }
    }
}
