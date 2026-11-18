//! Displays CLVM values as Rue expressions, using their types to choose how each part is written.
//!
//! The same atom can be an integer, a string, some bytes or a boolean, and the same pairs can be a
//! list, a struct or nested pairs, so a value on its own can only be displayed by guessing. With
//! its type, `0xcafef00d` is written as bytes rather than `-889262067`, `1` as `true` where it's a
//! `Bool`, and a struct as `Point { x: 1, y: 2 }` rather than `(1 2)`.
//!
//! Where the type doesn't describe the value (such as `Any`, a generic, or a value that doesn't
//! match its type because of an unchecked cast), that part is displayed by guessing, the way the
//! CLVM disassembler does, but still with Rue syntax.

use std::collections::{HashMap, HashSet};

use clvmr::{Allocator, NodePtr, SExp};
use id_arena::Arena;

use crate::{
    AtomRestriction, AtomSemantic, Struct, Type, TypeId, grow_stack,
    stringify_without_substitution, substitute,
};

/// How to lay out a displayed value.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct DisplayOptions {
    /// The width that lines are kept within where possible, by breaking lists and structs over
    /// several lines. If missing, the value is always displayed on a single line.
    pub max_width: Option<usize>,
    /// The number of spaces each level of nesting is indented by, when broken over several lines.
    pub indent: usize,
}

impl DisplayOptions {
    /// Displays the value on a single line, however long it is.
    pub const SINGLE_LINE: Self = Self {
        max_width: None,
        indent: 4,
    };

    /// Breaks the value over several lines where it's wider than `max_width`.
    pub const fn multiline(max_width: usize) -> Self {
        Self {
            max_width: Some(max_width),
            indent: 4,
        }
    }
}

impl Default for DisplayOptions {
    fn default() -> Self {
        Self::multiline(80)
    }
}

/// Displays a CLVM value of the given type as a Rue expression.
pub fn display_value(
    arena: &mut Arena<Type>,
    ty: TypeId,
    allocator: &Allocator,
    node: NodePtr,
    options: DisplayOptions,
) -> String {
    let ty = substitute(arena, ty);
    display_value_without_substitution(arena, ty, allocator, node, options)
}

/// Like `display_value`, but for a type that has already been substituted.
pub fn display_value_without_substitution(
    arena: &Arena<Type>,
    ty: TypeId,
    allocator: &Allocator,
    node: NodePtr,
    options: DisplayOptions,
) -> String {
    let doc = ValueDisplay::new(arena, allocator).doc(ty, node);
    let mut output = String::new();
    doc.render(&options, 0, &mut output);
    output
}

/// Displays a CLVM value as a Rue expression, guessing how to write each part of it.
pub fn display_untyped_value(
    allocator: &Allocator,
    node: NodePtr,
    options: DisplayOptions,
) -> String {
    let arena = Arena::new();
    let doc = ValueDisplay::new(&arena, allocator).untyped(node);
    let mut output = String::new();
    doc.render(&options, 0, &mut output);
    output
}

struct ValueDisplay<'a> {
    arena: &'a Arena<Type>,
    allocator: &'a Allocator,
    /// Whether each value matches each type it has been checked against. A check that's still in
    /// progress is `false`, so that a type which refers to itself without a pair in between (and
    /// would otherwise be checked forever) doesn't match.
    matches: HashMap<(TypeId, NodePtr), bool>,
}

/// A concrete type that a value is displayed as, once aliases, references and unions are resolved.
#[derive(Clone, Copy)]
struct Resolved {
    ty: TypeId,
    /// Whether a union had to be resolved to reach it, which is how lists are told apart from
    /// nested pairs (the rest of a list is a union of nil and a pair).
    from_union: bool,
}

impl<'a> ValueDisplay<'a> {
    fn new(arena: &'a Arena<Type>, allocator: &'a Allocator) -> Self {
        Self {
            arena,
            allocator,
            matches: HashMap::new(),
        }
    }

    fn doc(&mut self, ty: TypeId, node: NodePtr) -> Doc {
        grow_stack(|| self.doc_inner(ty, node))
    }

    fn doc_inner(&mut self, ty: TypeId, node: NodePtr) -> Doc {
        if self.is_nil(node) && self.is_list(ty) {
            return Doc::list(Vec::new());
        }

        let Some(resolved) = self.resolve(ty, node) else {
            return self.untyped(node);
        };

        match (&self.arena[resolved.ty], self.allocator.sexp(node)) {
            (Type::Atom(atom), SExp::Atom) => {
                let semantic = atom.semantic;
                self.atom(semantic, node)
            }
            (Type::Pair(_), SExp::Pair(..)) => self.pairs(resolved, node),
            (Type::Struct(ty), _) => {
                let ty = ty.clone();
                self.structure(&ty, node)
                    .unwrap_or_else(|| self.untyped(node))
            }
            _ => self.untyped(node),
        }
    }

    /// Resolves the type to the concrete type the value should be displayed as. For a union, this
    /// is the most specific variant that the value matches. Returns `None` if the value doesn't
    /// match any variant.
    fn resolve(&mut self, ty: TypeId, node: NodePtr) -> Option<Resolved> {
        let mut from_union = false;
        let mut visited = HashSet::new();
        let mut ty = ty;

        loop {
            if !visited.insert(ty) {
                return None;
            }

            match &self.arena[ty] {
                Type::Ref(inner) => ty = *inner,
                Type::Alias(alias) => ty = alias.inner,
                Type::Apply(apply) => ty = apply.inner,
                Type::Struct(Struct {
                    name: None, inner, ..
                }) => ty = *inner,
                Type::Union(_) => {
                    from_union = true;
                    ty = self.variant(ty, node)?;
                }
                _ => return Some(Resolved { ty, from_union }),
            }
        }
    }

    /// The most specific variant of a union that the value matches. A variant that only matches a
    /// single value (such as `nil` or `true`) is preferred over one that matches values of a given
    /// length, which is preferred over any other. Otherwise, the first variant is chosen.
    ///
    /// If the value doesn't match any variant, but only one variant has its shape at the top level
    /// (such as the pair of a list whose items don't all match), that variant is chosen, so that
    /// the parts that don't match are displayed without a type rather than the whole value.
    fn variant(&mut self, union: TypeId, node: NodePtr) -> Option<TypeId> {
        let variants = self.variants(union);
        let mut best: Option<(TypeId, u8)> = None;

        for &variant in &variants {
            if !self.matches(variant, node) {
                continue;
            }

            let specificity = self.specificity(variant);

            if best.is_none_or(|(_, best)| specificity > best) {
                best = Some((variant, specificity));
            }
        }

        if let Some((variant, _)) = best {
            return Some(variant);
        }

        let mut shaped = variants
            .into_iter()
            .filter(|&variant| self.has_shape(variant, node));

        match (shaped.next(), shaped.next()) {
            (Some(variant), None) => Some(variant),
            _ => None,
        }
    }

    /// Whether the value has the shape of the type at the top level: an atom that meets the type's
    /// restriction, or a pair. The contents of pairs aren't checked.
    fn has_shape(&self, ty: TypeId, node: NodePtr) -> bool {
        let mut ty = ty;
        let mut visited = HashSet::new();

        while visited.insert(ty) {
            match (&self.arena[ty], self.allocator.sexp(node)) {
                (Type::Ref(inner), _) => ty = *inner,
                (Type::Alias(alias), _) => ty = alias.inner,
                (Type::Apply(apply), _) => ty = apply.inner,
                (Type::Struct(ty_struct), _) => ty = ty_struct.inner,
                (Type::Union(union), _) => {
                    return union
                        .types
                        .iter()
                        .any(|&variant| self.has_shape(variant, node));
                }
                (Type::Atom(atom), SExp::Atom) => {
                    return match &atom.restriction {
                        None => true,
                        Some(AtomRestriction::Value(value)) => {
                            self.allocator.atom(node).as_ref() == value.as_ref()
                        }
                        Some(AtomRestriction::Length(length)) => {
                            self.allocator.atom_len(node) == *length
                        }
                    };
                }
                (Type::Pair(_), SExp::Pair(..)) => return true,
                (Type::Atom(_), SExp::Pair(..))
                | (Type::Pair(_), SExp::Atom)
                | (Type::Never, _) => {
                    return false;
                }
                (Type::Unresolved | Type::Generic(_) | Type::Any | Type::Function(_), _) => {
                    return true;
                }
            }
        }

        false
    }

    /// The variants of a union, including those of nested unions and aliases of unions.
    fn variants(&self, union: TypeId) -> Vec<TypeId> {
        let mut variants = Vec::new();
        let mut stack = vec![union];
        let mut visited = HashSet::new();

        while let Some(ty) = stack.pop() {
            if !visited.insert(ty) {
                continue;
            }

            match &self.arena[ty] {
                Type::Union(union) => stack.extend(union.types.iter().rev()),
                Type::Ref(inner) => stack.push(*inner),
                Type::Alias(alias) => stack.push(alias.inner),
                _ => variants.push(ty),
            }
        }

        variants
    }

    /// Whether the type is a list, which is a union of nil and a pair whose rest is a list too
    /// (or at least can be nil, such as a list of alternating types). This tells an empty list
    /// apart from other types that can be nil, such as an optional pair.
    fn is_list(&self, ty: TypeId) -> bool {
        let Some(pair) = self.list_pair(ty) else {
            return false;
        };

        let Type::Pair(pair) = &self.arena[pair] else {
            unreachable!();
        };

        self.list_pair(pair.rest).is_some()
    }

    /// If the type is a union of nil and a pair, the pair.
    fn list_pair(&self, ty: TypeId) -> Option<TypeId> {
        let mut ty = ty;
        let mut visited = HashSet::new();

        while visited.insert(ty) {
            match &self.arena[ty] {
                Type::Ref(inner) => ty = *inner,
                Type::Alias(alias) => ty = alias.inner,
                Type::Union(_) => break,
                _ => return None,
            }
        }

        let mut pair = None;
        let mut nil = false;

        for variant in self.variants(ty) {
            match &self.arena[variant] {
                Type::Pair(_) => pair = Some(variant),
                Type::Atom(atom) => {
                    nil |= matches!(&atom.restriction, Some(AtomRestriction::Value(value)) if value.is_empty());
                }
                _ => {}
            }
        }

        pair.filter(|_| nil)
    }

    fn specificity(&self, ty: TypeId) -> u8 {
        let mut ty = ty;
        let mut visited = HashSet::new();

        while visited.insert(ty) {
            match &self.arena[ty] {
                Type::Ref(inner) => ty = *inner,
                Type::Alias(alias) => ty = alias.inner,
                Type::Atom(atom) => {
                    return match atom.restriction {
                        Some(AtomRestriction::Value(_)) => 2,
                        Some(AtomRestriction::Length(_)) => 1,
                        None => 0,
                    };
                }
                _ => return 0,
            }
        }

        0
    }

    /// Whether the value matches the type, structurally. Types that don't constrain the value
    /// (such as `Any` and generics) match anything.
    fn matches(&mut self, ty: TypeId, node: NodePtr) -> bool {
        if let Some(&matches) = self.matches.get(&(ty, node)) {
            return matches;
        }

        self.matches.insert((ty, node), false);
        let matches = grow_stack(|| self.matches_inner(ty, node));
        self.matches.insert((ty, node), matches);
        matches
    }

    fn matches_inner(&mut self, ty: TypeId, node: NodePtr) -> bool {
        match (&self.arena[ty], self.allocator.sexp(node)) {
            (Type::Ref(inner), _) => self.matches(*inner, node),
            (Type::Alias(alias), _) => self.matches(alias.inner, node),
            (Type::Apply(apply), _) => self.matches(apply.inner, node),
            (Type::Struct(ty), _) => self.matches(ty.inner, node),
            (Type::Union(union), _) => union
                .types
                .clone()
                .into_iter()
                .any(|variant| self.matches(variant, node)),
            (Type::Unresolved | Type::Generic(_) | Type::Any | Type::Function(_), _) => true,
            (Type::Never, _) => false,
            (Type::Atom(atom), SExp::Atom) => match &atom.restriction {
                None => true,
                Some(AtomRestriction::Value(value)) => {
                    self.allocator.atom(node).as_ref() == value.as_ref()
                }
                Some(AtomRestriction::Length(length)) => self.allocator.atom_len(node) == *length,
            },
            (Type::Pair(pair), SExp::Pair(first, rest)) => {
                let pair = *pair;
                self.matches(pair.first, first) && self.matches(pair.rest, rest)
            }
            (Type::Atom(_), SExp::Pair(..)) | (Type::Pair(_), SExp::Atom) => false,
        }
    }

    /// Displays a chain of pairs, starting with one whose type is `start`, as a list if its type
    /// is a list, and otherwise as nested pairs.
    fn pairs(&mut self, start: Resolved, node: NodePtr) -> Doc {
        let mut items = Vec::new();
        // A union around the first pair (such as an optional pair) doesn't make it a list, only a
        // union in the rest of the chain does
        let mut is_list = false;
        let mut resolved = start;
        let mut node = node;

        // The type and value of the rest of the chain, once it's no longer a pair of the type
        let (tail_ty, tail) = loop {
            let (Type::Pair(pair), SExp::Pair(first, rest)) =
                (&self.arena[resolved.ty], self.allocator.sexp(node))
            else {
                unreachable!("only pairs of pair types are displayed as pairs");
            };

            let pair = *pair;
            items.push(self.doc(pair.first, first));
            node = rest;

            let Some(next) = self.resolve(pair.rest, rest) else {
                break (pair.rest, rest);
            };

            // Only a union that continues the chain or ends it with nil makes it a list, since
            // the last item of nested pairs can have a union type of its own (such as `Bool`)
            if let (Type::Pair(_), SExp::Pair(..)) =
                (&self.arena[next.ty], self.allocator.sexp(rest))
            {
                is_list |= next.from_union;
                resolved = next;
            } else {
                is_list |= next.from_union && self.is_nil(rest);
                break (pair.rest, rest);
            }
        };

        // A chain of several pairs that ends in nil is written as a list even if its type isn't a
        // list, since that's how list literals are typed
        if (is_list || items.len() > 1) && self.is_nil(tail) {
            return Doc::list(items);
        }

        let tail = self.doc(tail_ty, tail);

        if is_list {
            items.push(Doc::spread(tail));
            return Doc::list(items);
        }

        items
            .into_iter()
            .rev()
            .fold(tail, |rest, first| Doc::pair(first, rest))
    }

    /// Displays a struct with its field names, or returns `None` if the value doesn't have the
    /// structure of its fields.
    fn structure(&mut self, ty: &Struct, node: NodePtr) -> Option<Doc> {
        let Some(name) = &ty.name else {
            return Some(self.doc(ty.inner, node));
        };

        // The generic arguments are written out, since they can't always be inferred from the
        // fields. A narrowed struct's arguments don't describe its fields, so they're left out.
        let name = if ty.generics.is_empty() || ty.narrowed {
            name.text().to_string()
        } else {
            let generics = ty
                .generics
                .iter()
                .map(|&generic| stringify_without_substitution(self.arena, generic))
                .collect::<Vec<_>>()
                .join(", ");
            format!("{}::<{generics}>", name.text())
        };

        let mut fields = Vec::new();
        let mut current = ty.inner;
        let mut node = node;

        for (index, field) in ty.fields.iter().enumerate() {
            if !ty.nil_terminated && index == ty.fields.len() - 1 {
                let value = self.doc(current, node);
                fields.push(Doc::field(field, value));
                return Some(Doc::structure(&name, fields));
            }

            let resolved = self.resolve(current, node)?;

            let (Type::Pair(pair), SExp::Pair(first, rest)) =
                (&self.arena[resolved.ty], self.allocator.sexp(node))
            else {
                return None;
            };

            let pair = *pair;
            let value = self.doc(pair.first, first);
            fields.push(Doc::field(field, value));
            current = pair.rest;
            node = rest;
        }

        self.is_nil(node).then(|| Doc::structure(&name, fields))
    }

    fn atom(&self, semantic: AtomSemantic, node: NodePtr) -> Doc {
        let bytes = self.allocator.atom(node);
        let bytes = bytes.as_ref();

        let text = match semantic {
            AtomSemantic::Any => untyped_atom(bytes),
            AtomSemantic::Int => {
                if bytes.is_empty() || is_canonical_int(bytes) {
                    self.allocator.number(node).to_string()
                } else {
                    hex(bytes)
                }
            }
            AtomSemantic::Bool => match bytes {
                [] => "false".to_string(),
                [1] => "true".to_string(),
                _ => hex(bytes),
            },
            AtomSemantic::String => string(bytes).unwrap_or_else(|| hex(bytes)),
            AtomSemantic::Bytes
            | AtomSemantic::PublicKey
            | AtomSemantic::Signature
            | AtomSemantic::K1PublicKey
            | AtomSemantic::K1Signature
            | AtomSemantic::R1PublicKey
            | AtomSemantic::R1Signature => hex(bytes),
        };

        Doc::Text(text)
    }

    /// Displays a value without a type, by guessing how to write each part of it.
    fn untyped(&self, node: NodePtr) -> Doc {
        grow_stack(|| self.untyped_inner(node))
    }

    fn untyped_inner(&self, node: NodePtr) -> Doc {
        if let SExp::Atom = self.allocator.sexp(node) {
            return Doc::Text(untyped_atom(self.allocator.atom(node).as_ref()));
        }

        let mut items = Vec::new();
        let mut node = node;

        while let SExp::Pair(first, rest) = self.allocator.sexp(node) {
            items.push(self.untyped(first));
            node = rest;
        }

        if self.is_nil(node) {
            return Doc::list(items);
        }

        let tail = self.untyped(node);

        items
            .into_iter()
            .rev()
            .fold(tail, |rest, first| Doc::pair(first, rest))
    }

    fn is_nil(&self, node: NodePtr) -> bool {
        matches!(self.allocator.sexp(node), SExp::Atom) && self.allocator.atom_len(node) == 0
    }
}

/// The largest atom that's guessed to be an integer. Larger atoms are usually hashes or keys
/// rather than numbers, so they're written as bytes instead.
const MAX_GUESSED_INT_LEN: usize = 8;

/// The shortest atom that's guessed to be a string. Shorter atoms are usually small integers.
const MIN_GUESSED_STRING_LEN: usize = 3;

/// Writes an atom without a type, guessing whether it's a string, an integer or bytes.
fn untyped_atom(bytes: &[u8]) -> String {
    if bytes.is_empty() {
        return "nil".to_string();
    }

    if bytes.len() >= MIN_GUESSED_STRING_LEN
        && let Some(string) = string(bytes)
    {
        return string;
    }

    if bytes.len() <= MAX_GUESSED_INT_LEN && is_canonical_int(bytes) {
        return num_bigint::BigInt::from_signed_bytes_be(bytes).to_string();
    }

    hex(bytes)
}

/// Writes bytes as a hex literal, or `nil` if there aren't any.
fn hex(bytes: &[u8]) -> String {
    if bytes.is_empty() {
        "nil".to_string()
    } else {
        format!("0x{}", hex::encode(bytes))
    }
}

/// Writes bytes as a string literal, if they can be. Rue strings have no escapes, so this is only
/// possible for printable text without quotes.
fn string(bytes: &[u8]) -> Option<String> {
    let text = std::str::from_utf8(bytes).ok()?;

    if text.chars().any(|char| char == '"' || char.is_control()) {
        return None;
    }

    Some(format!("\"{text}\""))
}

/// Whether an atom is the minimal encoding of a nonzero signed integer.
fn is_canonical_int(bytes: &[u8]) -> bool {
    match bytes {
        [] | [0x00] => false,
        [0x00, second, ..] => second & 0x80 != 0,
        [0xff, second, ..] => second & 0x80 == 0,
        _ => true,
    }
}

/// A displayed value, before it's laid out.
#[derive(Debug, Clone)]
enum Doc {
    Text(String),
    /// Items between delimiters, separated by commas. If `padded`, there's a space inside each
    /// delimiter when the items fit on one line, as in `Point { x: 1 }`.
    Group {
        open: String,
        close: &'static str,
        padded: bool,
        items: Vec<Doc>,
    },
    /// A value with a prefix, such as a field name or a spread.
    Prefixed(String, Box<Doc>),
}

impl Doc {
    fn list(items: Vec<Doc>) -> Self {
        Self::Group {
            open: "[".to_string(),
            close: "]",
            padded: false,
            items,
        }
    }

    fn pair(first: Doc, rest: Doc) -> Self {
        Self::Group {
            open: "(".to_string(),
            close: ")",
            padded: false,
            items: vec![first, rest],
        }
    }

    fn structure(name: &str, fields: Vec<Doc>) -> Self {
        Self::Group {
            open: format!("{name} {{"),
            close: "}",
            padded: true,
            items: fields,
        }
    }

    fn field(name: &str, value: Doc) -> Self {
        Self::Prefixed(format!("{name}: "), Box::new(value))
    }

    fn spread(value: Doc) -> Self {
        Self::Prefixed("...".to_string(), Box::new(value))
    }

    /// The width of the doc on a single line.
    fn flat_width(&self) -> usize {
        match self {
            Self::Text(text) => text.len(),
            Self::Prefixed(prefix, doc) => prefix.len() + doc.flat_width(),
            Self::Group {
                open,
                close,
                padded,
                items,
            } => {
                let separators = items.len().saturating_sub(1) * 2;
                let padding = if *padded && !items.is_empty() { 2 } else { 0 };
                open.len()
                    + close.len()
                    + padding
                    + separators
                    + items.iter().map(Doc::flat_width).sum::<usize>()
            }
        }
    }

    fn render_flat(&self, output: &mut String) {
        match self {
            Self::Text(text) => output.push_str(text),
            Self::Prefixed(prefix, doc) => {
                output.push_str(prefix);
                doc.render_flat(output);
            }
            Self::Group {
                open,
                close,
                padded,
                items,
            } => {
                output.push_str(open);

                if *padded && !items.is_empty() {
                    output.push(' ');
                }

                for (index, item) in items.iter().enumerate() {
                    if index > 0 {
                        output.push_str(", ");
                    }
                    item.render_flat(output);
                }

                if *padded && !items.is_empty() {
                    output.push(' ');
                }

                output.push_str(close);
            }
        }
    }

    /// Renders the doc starting at `column`, breaking groups that don't fit over several lines.
    /// The doc is followed by `trailing` characters on the same line (such as a comma).
    fn render(&self, options: &DisplayOptions, indent: usize, output: &mut String) {
        self.render_at(options, indent, 0, output);
    }

    fn render_at(
        &self,
        options: &DisplayOptions,
        indent: usize,
        trailing: usize,
        output: &mut String,
    ) {
        let column = output.len() - output.rfind('\n').map_or(0, |index| index + 1);

        let fits = options
            .max_width
            .is_none_or(|max_width| column + self.flat_width() + trailing <= max_width);

        if fits {
            self.render_flat(output);
            return;
        }

        match self {
            Self::Text(text) => output.push_str(text),
            Self::Prefixed(prefix, doc) => {
                output.push_str(prefix);
                doc.render_at(options, indent, trailing, output);
            }
            Self::Group {
                open, close, items, ..
            } => {
                let inner = indent + options.indent;

                output.push_str(open);

                for item in items {
                    output.push('\n');
                    output.push_str(&" ".repeat(inner));
                    item.render_at(options, inner, 1, output);
                    output.push(',');
                }

                if !items.is_empty() {
                    output.push('\n');
                    output.push_str(&" ".repeat(indent));
                }

                output.push_str(close);
            }
        }
    }
}
