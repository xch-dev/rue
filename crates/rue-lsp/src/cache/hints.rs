//! Inlay hints show what the code leaves out: the types that were inferred for bindings and lambda
//! parameters, the generic arguments that were inferred for calls, and the names of the parameters
//! that arguments are for.

use std::collections::HashMap;
use std::ops::Range;

use rowan::{TextRange, TextSize, WalkEvent};
use rue_ast::{
    AstBinding, AstExpr, AstFunctionCallExpr, AstLambdaExpr, AstLetStmt, AstNode, AstPathSegment,
};
use rue_compiler::{Compiler, SyntaxCall, SyntaxItemKind};
use rue_hir::{ScopeId, SymbolId};
use rue_types::{ELIDED, Type, TypeId};
use tower_lsp_server::ls_types::Position;

use super::{Cache, Scopes, contains};
use crate::position::source_range;

/// How many parts of a type are written in a hint, before it's shortened to fit.
const HINT_PARTS: usize = 24;

/// How many characters of a type are shown in a hint, so that it stays short next to the code.
const HINT_CHARS: usize = 32;

/// How many parts of a type are written in the tooltip of a hint that was shortened.
const TOOLTIP_PARTS: usize = 64;

/// How a type that couldn't be worked out is written.
const UNRESOLVED: &str = "{unresolved}";

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

        // The same code can be compiled more than once (such as a lambda's body, until the types
        // of its parameters are known), and the last time is the one that was kept
        let mut declarations = HashMap::new();
        let mut calls = HashMap::new();
        let mut scopes = Vec::new();

        for item in self
            .syntax_map
            .items()
            .filter(|item| item.source_kind == self.source.kind)
        {
            match &item.kind {
                SyntaxItemKind::SymbolDeclaration(symbol) => {
                    declarations.insert(item.span, *symbol);
                }
                SyntaxItemKind::Call(call) => {
                    calls.insert(item.span, call);
                }
                SyntaxItemKind::Scope(scope) => scopes.push((item.span, *scope)),
                SyntaxItemKind::FileModule(_)
                | SyntaxItemKind::SymbolReference(_)
                | SyntaxItemKind::TypeDeclaration(_)
                | SyntaxItemKind::TypeReference(_)
                | SyntaxItemKind::FieldDeclaration(_)
                | SyntaxItemKind::FieldReference(_)
                | SyntaxItemKind::FieldInitializer(_)
                | SyntaxItemKind::CompletionContext(_) => {}
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
                call_hints(&call, recorded, options, &mut pending);
            }
        }

        pending.retain(|(offset, _)| range.contains_inclusive(*offset));
        pending.sort_by_key(|(offset, _)| *offset);

        let mut hints = Vec::new();

        for (offset, hint) in pending {
            let index = usize::from(offset);

            // Types are named as they are where the hint is
            let innermost = innermost_scope(scopes.iter().copied(), index);
            let scopes = self.scope_chain(innermost);

            let (label, kind, tooltip) = match hint {
                Pending::Type(symbol) => {
                    let ty = match innermost {
                        Some(scope) => self.ctx.symbol_type_in(scope, symbol),
                        None => self.ctx.symbol_type(symbol),
                    };

                    let Some((name, tooltip)) = self.hint_type(&scopes, ty) else {
                        continue;
                    };

                    (format!(": {name}"), HintKind::Type, tooltip)
                }
                Pending::GenericArguments(types) => {
                    let Some(names) = types
                        .into_iter()
                        .map(|ty| self.hint_type(&scopes, ty))
                        .collect::<Option<Vec<_>>>()
                    else {
                        continue;
                    };

                    let shortened = names.iter().any(|(_, tooltip)| tooltip.is_some());

                    let join = |names: Vec<String>| format!("::<{}>", names.join(", "));

                    let tooltip = shortened.then(|| {
                        join(
                            names
                                .iter()
                                .map(|(name, tooltip)| tooltip.clone().unwrap_or(name.clone()))
                                .collect(),
                        )
                    });

                    (
                        join(names.into_iter().map(|(name, _)| name).collect()),
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

        if matches!(self.ctx.ty(semantic), Type::Unresolved) {
            return None;
        }

        let name = self.bounded_type_name(scopes, ty, HINT_PARTS);

        // A part of the type that couldn't be worked out is an error that's already reported
        if name.contains(UNRESOLVED) {
            return None;
        }

        if name.chars().count() <= HINT_CHARS {
            return Some((name, None));
        }

        let tooltip = self.bounded_type_name(scopes, ty, TOOLTIP_PARTS);

        Some((shorten(&name, HINT_CHARS), Some(tooltip)))
    }
}

/// The scope that contains the index and no other scope that does, like [`Cache::scopes`].
pub(super) fn innermost_scope(
    scopes: impl Iterator<Item = (TextRange, ScopeId)>,
    index: usize,
) -> Option<ScopeId> {
    scopes
        .filter(|&(span, _)| contains(span, index))
        .min_by_key(|&(span, _)| span.len())
        .map(|(_, scope)| scope)
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
    pending: &mut Vec<(TextSize, Pending)>,
) {
    let function = &recorded.function;

    // Generic arguments are written after the name of a function, so there's nowhere to show
    // them for a function that isn't called by its name, and nothing to show if they're written
    if options.generic_arguments
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
        // arguments, so only an argument that's spread there is all of it, and the arguments
        // that are its items have no parameter of their own. A spread anywhere else is an error
        let spread = argument.spread().is_some();
        let rest = !function.nil_terminated && i >= last;

        if spread != (rest && i == last) || (rest && !spread) {
            continue;
        }

        let Some(parameter) = function.params.get(i) else {
            break;
        };

        let Some(expr) = argument.expr() else {
            continue;
        };

        // An argument that's named like the parameter already says what it's for
        if parameter.name.is_empty() || argument_name(&expr).as_deref() == Some(&parameter.name) {
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
/// another type.
fn union_members(name: &str) -> Vec<&str> {
    let mut members = Vec::new();
    let mut depth = 0usize;
    let mut start = 0;

    for (i, c) in name.char_indices() {
        match c {
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
