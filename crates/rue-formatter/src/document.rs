//! A deliberately small, private pretty-printing document model.

#[derive(Debug, Clone)]
pub enum Doc {
    Nil,
    Text(String),
    Concat(Vec<Self>),
    Line(LineKind),
    Indent(Box<Self>),
    Outdent(Box<Self>),
    Group(Box<Self>),
    /// A local fill boundary with continuation indentation applied only when
    /// that boundary breaks. Ordinary groups cannot express that conditional
    /// indentation when their surrounding binary chain is already broken.
    Fill {
        flat: Box<Self>,
        broken: Box<Self>,
        space_when_flat: bool,
        indent_levels_on_break: usize,
    },
    IfBreak {
        broken: Box<Self>,
        flat: Box<Self>,
    },
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum LineKind {
    Soft,
    Hard,
    Empty,
}

impl Doc {
    pub fn text(text: impl Into<String>) -> Self {
        Self::Text(text.into())
    }

    pub fn concat(docs: impl IntoIterator<Item = Self>) -> Self {
        let mut flattened = Vec::new();
        for doc in docs {
            match doc {
                Self::Nil => {}
                Self::Concat(children) => flattened.extend(children),
                doc => flattened.push(doc),
            }
        }
        Self::Concat(flattened)
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
            flat: Box::new(doc.clone()),
            broken: Box::new(doc),
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
            flat: Box::new(flat),
            broken: Box::new(broken),
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
        Self::Indent(Box::new(self))
    }

    pub fn outdent(self) -> Self {
        Self::Outdent(Box::new(self))
    }

    pub fn group(self) -> Self {
        Self::Group(Box::new(self))
    }

    pub fn if_break(broken: Self, flat: Self) -> Self {
        Self::IfBreak {
            broken: Box::new(broken),
            flat: Box::new(flat),
        }
    }
}
