//! Inlay hints show what the code leaves out: the types that were inferred for bindings and lambda
//! parameters, the generic arguments that were inferred for calls, and the names of the parameters
//! that arguments are for.

use std::collections::HashMap;
use std::ops::Range;

use rowan::{TextRange, TextSize, WalkEvent};
use rue_ast::{
    AstBinding, AstExpr, AstFunctionCallExpr, AstLambdaExpr, AstLetStmt, AstMatchArm, AstNode,
    AstPathSegment,
};
use rue_compiler::{Compiler, SyntaxCall, SyntaxItemKind};
use rue_hir::{Symbol, SymbolId};
use rue_types::{ELIDED, Type, TypeId};
use tower_lsp_server::ls_types::Position;

use super::rename::is_identifier;
use super::{Cache, Scopes};
use crate::position::source_range;

/// How many parts of a type are written in a hint, before it's shortened to fit.
const HINT_PARTS: usize = 24;

/// How many characters of a type are shown in a hint, so that it stays short next to the code.
const HINT_CHARS: usize = 32;

/// How many parts of a type are written in the tooltip of a hint that was shortened.
const TOOLTIP_PARTS: usize = 64;

/// Which hints to show.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct HintOptions {
    /// The types of `let` bindings and lambda parameters that don't have one written.
    pub types: bool,
    /// The generic arguments that were inferred for a call.
    pub generic_arguments: bool,
    /// The names of the parameters that the arguments of a call are for.
    pub parameter_names: bool,
}

impl Default for HintOptions {
    fn default() -> Self {
        Self {
            types: true,
            generic_arguments: true,
            parameter_names: true,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Hint {
    pub position: Position,
    pub label: String,
    pub kind: HintKind,
    /// The whole of what the label is short for, if it was shortened.
    pub tooltip: Option<String>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum HintKind {
    /// A type, written after the name it's the type of, as `: Int`.
    Type,
    /// Generic arguments, written after the name of the function that's called, as `::<Int>`.
    GenericArguments,
    /// A parameter's name, written before the argument for it, as `value:`.
    Parameter,
}

/// A hint that hasn't been written out yet.
enum Pending {
    Type(SymbolId),
    GenericArguments(Vec<TypeId>),
    Parameter(String),
}

impl Cache<Compiler> {
    /// The hints in a range of the file, in the order they appear in it.
    pub fn inlay_hints(&mut self, range: Range<usize>, options: HintOptions) -> Vec<Hint> {
        let Some(file) = self.tree.find(&self.source.kind) else {
            return Vec::new();
        };

        let root = file.document.syntax().clone();

        let (Ok(start), Ok(end)) = (u32::try_from(range.start), u32::try_from(range.end)) else {
            return Vec::new();
        };

        let range = TextRange::new(TextSize::from(start), TextSize::from(end.max(start)));

        // If the same code was compiled more than once, the last time is the one that was kept
        let mut declarations = HashMap::new();
        let mut references = HashMap::new();
        let mut calls = HashMap::new();
        let mut scopes = Vec::new();

        for item in self
            .syntax_map
            .items()
            // Only what's in the range is looked at, and a scope that a hint in the range is in
            // has part of the range in it
            .filter(|item| {
                item.source_kind == self.source.kind && item.span.intersect(range).is_some()
            })
        {
            match &item.kind {
                SyntaxItemKind::SymbolDeclaration(symbol) => {
                    declarations.insert(item.span, *symbol);
                }
                SyntaxItemKind::SymbolReference(symbol) => {
                    references.insert(item.span, *symbol);
                }
                SyntaxItemKind::Call(call) => {
                    calls.insert(item.span, call);
                }
                SyntaxItemKind::Scope(scope) => scopes.push((item.span, *scope)),
                _ => {}
            }
        }

        let mut pending = Vec::new();
        let mut nodes = root.preorder();

        while let Some(event) = nodes.next() {
            let WalkEvent::Enter(node) = event else {
                continue;
            };

            // Only the code in the range is looked at, which is what's on screen
            if node.text_range().intersect(range).is_none() {
                nodes.skip_subtree();
                continue;
            }

            if let Some(stmt) = AstLetStmt::cast(node.clone()) {
                if options.types
                    && stmt.ty().is_none()
                    && !stmt.value().is_some_and(|value| is_obvious(&value))
                    && let Some(binding) = stmt.binding()
                {
                    binding_types(&binding, &declarations, &mut pending);
                }
            } else if let Some(arm) = AstMatchArm::cast(node.clone()) {
                // An arm's binding destructures a value of the arm's type, which is written right
                // before it, like the type of a `let` can be. Only `_` leaves the type out.
                if options.types
                    && arm.ty().is_some_and(|ty| ty.is_placeholder())
                    && let Some(binding) = arm.binding()
                {
                    binding_types(&binding, &declarations, &mut pending);
                }
            } else if let Some(lambda) = AstLambdaExpr::cast(node.clone()) {
                for parameter in lambda.parameters() {
                    if options.types
                        && parameter.ty().is_none()
                        && let Some(binding) = parameter.binding()
                    {
                        binding_types(&binding, &declarations, &mut pending);
                    }
                }
            } else if let Some(call) = AstFunctionCallExpr::cast(node.clone())
                && let Some(recorded) = calls.get(&node.text_range())
            {
                // Generic arguments can only be written for a function that's called by its
                // name, rather than for a binding that holds one
                let named = matches!(call.expr(), Some(AstExpr::PathExpr(path)) if path
                    .segments()
                    .last()
                    .and_then(|segment| segment.name())
                    .and_then(|name| references.get(&name.text_range()))
                    .is_some_and(|&symbol| matches!(self.ctx.symbol(symbol), Symbol::Function(_))));

                call_hints(&call, recorded, options, named, &mut pending);
            }
        }

        pending.retain(|(offset, _)| range.contains_inclusive(*offset));
        pending.sort_by_key(|(offset, _)| *offset);

        let mut hints = Vec::new();

        let mut scopes = ScopeSweep::new(scopes);

        for (offset, hint) in pending {
            let index = usize::from(offset);

            let (label, kind, tooltip) = match hint {
                // Types are named as they are where the hint is
                Pending::Type(symbol) => {
                    let scopes = self.scope_chain(scopes.innermost(offset));
                    let ty = self.symbol_type(&scopes, symbol);

                    let Some((name, tooltip)) = self.hint_type(&scopes, ty) else {
                        continue;
                    };

                    (format!(": {name}"), HintKind::Type, tooltip)
                }
                Pending::GenericArguments(types) => {
                    let scopes = self.scope_chain(scopes.innermost(offset));

                    let Some(names) = types
                        .into_iter()
                        .map(|ty| self.hint_type(&scopes, ty))
                        .collect::<Option<Vec<_>>>()
                    else {
                        continue;
                    };

                    // The arguments that fit are shown, so that the hint stays short however
                    // many generics there are, and the tooltip has all of them
                    let mut shown = Vec::new();
                    let mut length = 0;

                    for (name, _) in &names {
                        length += name.chars().count() + ", ".len();

                        if !shown.is_empty() && length > HINT_CHARS {
                            shown.push(ELIDED);
                            break;
                        }

                        shown.push(name.as_str());
                    }

                    let shortened = shown.last() == Some(&ELIDED)
                        || names.iter().any(|(_, more)| more.is_some());

                    let tooltip = shortened.then(|| {
                        let names = names
                            .iter()
                            .map(|(name, more)| more.as_deref().unwrap_or(name))
                            .collect::<Vec<_>>();

                        format!("::<{}>", names.join(", "))
                    });

                    (
                        format!("::<{}>", shown.join(", ")),
                        HintKind::GenericArguments,
                        tooltip,
                    )
                }
                Pending::Parameter(name) => (format!("{name}:"), HintKind::Parameter, None),
            };

            hints.push(Hint {
                position: source_range(&self.source, index, index).start,
                label,
                kind,
                tooltip,
            });
        }

        hints
    }

    /// The name of a type that's short enough for a hint, and the longer name that it's short for
    /// if it was shortened. A type that couldn't be worked out has no hint.
    fn hint_type(&mut self, scopes: &Scopes, ty: TypeId) -> Option<(String, Option<String>)> {
        let semantic = rue_types::unwrap_semantic(self.ctx.types_mut(), ty, true);

        // A generic that isn't one of the code's own is another function's, which is left where
        // a type should be when a call's generics can't be inferred
        if matches!(self.ctx.ty(semantic), Type::Generic(_))
            && !scopes.0.iter().any(|&scope| {
                let scope = self.ctx.scope(scope);
                scope.type_name(ty).is_some() || scope.type_name(semantic).is_some()
            })
        {
            return None;
        }

        // A part of the type that couldn't be worked out is an error that's already reported
        if rue_types::contains_unresolved(self.ctx.types(), ty) {
            return None;
        }

        let (name, cut) = self.bounded_type(scopes, ty, HINT_PARTS);

        let fits = |name: &str| name.chars().count() <= HINT_CHARS;

        if !cut && fits(&name) {
            return Some((name, None));
        }

        // The tooltip has more of a type that was cut short, by the printer or to fit. With more
        // parts, the type can turn out to be short enough to show whole, such as a union whose
        // members are mostly written the same
        let (more, cut_again) = if cut {
            self.bounded_type(scopes, ty, TOOLTIP_PARTS)
        } else {
            (name.clone(), false)
        };

        if !cut_again && fits(&more) {
            return Some((more, None));
        }

        // What the printer cut already says so
        let name = if fits(&name) {
            name
        } else {
            shorten(&name, HINT_CHARS)
        };

        Some((name, Some(more)))
    }
}

/// Finds the innermost scope at each of a series of offsets that only go up, like
/// [`super::innermost_scope`] does for one, without looking at every scope for each of them.
struct ScopeSweep<T> {
    /// The scopes in the order they start, each with its place in the syntax map.
    scopes: Vec<(TextRange, usize, T)>,
    /// How many of the scopes have started.
    started: usize,
    /// The scopes that have started and haven't ended.
    open: Vec<(TextRange, usize, T)>,
}

impl<T: Copy> ScopeSweep<T> {
    fn new(scopes: Vec<(TextRange, T)>) -> Self {
        let mut scopes = scopes
            .into_iter()
            .enumerate()
            .map(|(order, (span, scope))| (span, order, scope))
            .collect::<Vec<_>>();

        scopes.sort_by_key(|(span, ..)| span.start());

        Self {
            scopes,
            started: 0,
            open: Vec::new(),
        }
    }

    fn innermost(&mut self, offset: TextSize) -> Option<T> {
        while let Some(scope) = self
            .scopes
            .get(self.started)
            .filter(|(span, ..)| span.start() <= offset)
        {
            self.open.push(*scope);
            self.started += 1;
        }

        self.open.retain(|(span, ..)| offset <= span.end());

        // The narrowest scope is the innermost, and of those with the same span, the first one
        // in the syntax map is
        self.open
            .iter()
            .min_by_key(|(span, order, _)| (span.len(), *order))
            .map(|&(.., scope)| scope)
    }
}

/// Whether the type of a value is plain from how it's written, so a hint would only repeat it: a
/// literal, a struct initializer (which names its type), or a lambda (whose parameters have hints
/// of their own).
fn is_obvious(value: &AstExpr) -> bool {
    match value {
        AstExpr::GroupExpr(group) => group.expr().is_some_and(|value| is_obvious(&value)),
        AstExpr::LiteralExpr(_) | AstExpr::StructInitializerExpr(_) | AstExpr::LambdaExpr(_) => {
            true
        }
        _ => false,
    }
}

/// Adds a hint for the type of each name that a binding declares. The fields of a struct binding
/// are left out, since a type after a field's name would read as the binding for it.
fn binding_types(
    binding: &AstBinding,
    declarations: &HashMap<TextRange, SymbolId>,
    pending: &mut Vec<(TextSize, Pending)>,
) {
    match binding {
        AstBinding::NamedBinding(binding) => {
            // A discarded value has no binding
            if let Some(name) = binding.name()
                && name.text() != "_"
                && let Some(&symbol) = declarations.get(&name.text_range())
            {
                pending.push((name.text_range().end(), Pending::Type(symbol)));
            }
        }
        AstBinding::PairBinding(binding) => {
            for binding in binding.first().into_iter().chain(binding.rest()) {
                binding_types(&binding, declarations, pending);
            }
        }
        AstBinding::ListBinding(binding) => {
            for binding in binding.items().filter_map(|item| item.binding()) {
                binding_types(&binding, declarations, pending);
            }
        }
        AstBinding::StructBinding(_) => {}
    }
}

/// Adds the hints for a call: the generic arguments that were inferred for it, and the names of
/// the parameters that its arguments are for.
fn call_hints(
    call: &AstFunctionCallExpr,
    recorded: &SyntaxCall,
    options: HintOptions,
    named: bool,
    pending: &mut Vec<(TextSize, Pending)>,
) {
    let function = &recorded.function;

    // Generic arguments are written after the name of a function, so there's nowhere to show
    // them for a function that isn't called by its name, and nothing to show if they're written
    if options.generic_arguments
        && named
        && !function.generics.is_empty()
        && let Some(AstExpr::PathExpr(path)) = call.expr()
        && let Some(segment) = path.segments().last()
        && segment.generic_arguments().is_none()
        && let Some(name) = segment.name()
        && let Some(types) = function
            .generics
            .iter()
            .map(|generic| {
                recorded
                    .generics
                    .iter()
                    .find(|(inferred, _)| inferred == generic)
                    .map(|&(_, ty)| ty)
            })
            .collect::<Option<Vec<_>>>()
    {
        pending.push((name.text_range().end(), Pending::GenericArguments(types)));
    }

    if !options.parameter_names {
        return;
    }

    let last = function.params.len().saturating_sub(1);

    for (i, argument) in call.args().enumerate() {
        // The last parameter of a function that isn't nil terminated is the rest of the
        // arguments, so only an argument that's spread there is all of it. A spread anywhere
        // else is an error, and the arguments after the last parameter have none
        if argument.spread().is_some() != (!function.nil_terminated && i == last) {
            continue;
        }

        let Some(parameter) = function.params.get(i) else {
            break;
        };

        let Some(expr) = argument.expr() else {
            continue;
        };

        // A parameter that's destructured has its pattern in place of a name. An argument that's
        // named like the parameter already says what it's for
        if !is_identifier(&parameter.name)
            || parameter.name == "_"
            || argument_name(&expr).as_deref() == Some(&parameter.name)
        {
            continue;
        }

        pending.push((
            argument.syntax().text_range().start(),
            Pending::Parameter(parameter.name.clone()),
        ));
    }
}

/// The name that an argument is written with, if it's a name, a path to one or a field.
fn argument_name(expr: &AstExpr) -> Option<String> {
    match expr {
        AstExpr::GroupExpr(group) => argument_name(&group.expr()?),
        AstExpr::PathExpr(path) => path
            .segments()
            .last()
            .as_ref()
            .and_then(AstPathSegment::name)
            .map(|name| name.text().to_string()),
        AstExpr::FieldAccessExpr(access) => access.field().map(|name| name.text().to_string()),
        _ => None,
    }
}

/// Shortens the name of a type to fit in a hint. A union is cut after the last of its members that
/// fits, and anything else after as many characters as fit.
fn shorten(name: &str, max: usize) -> String {
    let members = union_members(name);

    if members.len() > 1 {
        let mut short = String::new();

        for member in members {
            if member == ELIDED {
                break;
            }

            // There has to be room left to say that there are more
            let separator = if short.is_empty() { 0 } else { " | ".len() };
            let rest = " | ".len() + ELIDED.len();

            if short.chars().count() + separator + member.chars().count() + rest > max {
                break;
            }

            if !short.is_empty() {
                short.push_str(" | ");
            }

            short.push_str(member);
        }

        if !short.is_empty() {
            return format!("{short} | {ELIDED}");
        }
    }

    let kept = name
        .chars()
        .take(max.saturating_sub(ELIDED.len()))
        .collect::<String>();

    format!("{}{ELIDED}", kept.trim_end())
}

/// The members of a union as it's written, which are separated by a `|` that isn't inside of
/// another type or a string.
fn union_members(name: &str) -> Vec<&str> {
    let mut members = Vec::new();
    let mut depth = 0usize;
    let mut string = false;
    let mut escaped = false;
    let mut start = 0;

    for (i, c) in name.char_indices() {
        if string {
            string = escaped || c != '"';
            escaped = !escaped && c == '\\';
            continue;
        }

        match c {
            '"' => string = true,
            '(' | '<' | '[' | '{' => depth += 1,
            // The `>` of a `->` isn't a bracket
            ')' | '>' | ']' | '}' if !name[..i].ends_with('-') => depth = depth.saturating_sub(1),
            '|' if depth == 0 => {
                members.push(name[start..i].trim());
                start = i + 1;
            }
            _ => {}
        }
    }

    members.push(name[start..].trim());
    members
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_scope_sweep() {
        // The sweep gives what looking at every scope gives, at every offset, for scopes that
        // are nested, have the same span, touch each other, or have nothing in them
        let scopes = [
            (0, 20),
            (0, 20),
            (2, 10),
            (2, 5),
            (5, 10),
            (5, 5),
            (10, 12),
            (12, 20),
            (30, 31),
        ]
        .into_iter()
        .enumerate()
        .map(|(id, (start, end))| (TextRange::new(start.into(), end.into()), id))
        .collect::<Vec<_>>();

        for first in 0..=32 {
            let mut sweep = ScopeSweep::new(scopes.clone());

            for offset in first..=32u32 {
                assert_eq!(
                    sweep.innermost(offset.into()),
                    super::super::innermost_scope(scopes.iter().copied(), offset as usize),
                    "from {first} at {offset}"
                );
            }
        }
    }

    #[test]
    fn test_union_members() {
        assert_eq!(union_members("Int"), ["Int"]);
        assert_eq!(union_members("Int | Bytes | nil"), ["Int", "Bytes", "nil"]);

        // A `|` inside of another type or a string isn't between the members of the union
        assert_eq!(
            union_members("List<A | B> | (C | D, E) | fn(a: A | B) -> C | D"),
            ["List<A | B>", "(C | D, E)", "fn(a: A | B) -> C", "D"]
        );
        assert_eq!(
            union_members("(fn(a: A) -> B | C, D) | E"),
            ["(fn(a: A) -> B | C, D)", "E"]
        );
        assert_eq!(
            union_members(r#""a | b" | "(" | "\" | c""#),
            [r#""a | b""#, r#""(""#, r#""\" | c""#]
        );
    }

    #[test]
    fn test_shorten() {
        // A union is cut after the last member that leaves room to say that there are more
        assert_eq!(shorten("Int | Bytes | nil", 12), "Int | ...");
        assert_eq!(shorten("Int | Bytes | nil", 17), "Int | Bytes | ...");
        assert_eq!(shorten("Int | Bytes | nil", 16), "Int | ...");
        assert_eq!(shorten("Int | Bytes | ...", 30), "Int | Bytes | ...");

        // Anything else, and a union whose first member doesn't fit, is cut where it has to be
        assert_eq!(shorten("(Int, Bytes, nil)", 10), "(Int, B...");
        assert_eq!(shorten("(Int, Bytes, nil)", 9), "(Int,...");
        assert_eq!(shorten("Bytes32 | Int", 9), "Bytes3...");
        assert_eq!(
            shorten("\u{e9}\u{e9}\u{e9}\u{e9}\u{e9}\u{e9}", 5),
            "\u{e9}\u{e9}..."
        );
    }
}
