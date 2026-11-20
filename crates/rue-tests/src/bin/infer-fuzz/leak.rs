//! Checks that no binding, parameter or expression of a compiled program has a type that names a
//! generic outside of where it's bound (#490).
//!
//! A generic is bound by the function or lambda that declares it, which includes the copies a
//! lambda makes of an expected generic function type's generics, or by a generic function type it's
//! inside of, such as `T` in `fn<T>(a: T) -> T`. A callee's generic that inference left in a type,
//! or an expected type's `T` named outside of its function type, is a generic that nothing in
//! scope binds. The explicit program writes every generic argument out, so it never has one, and a
//! variant that does compiled through a type that doesn't mean anything where it's used, even when
//! its output is the same.
//!
//! The types of expressions are the ones the compiler records for each one it compiles (for lints),
//! which are what it inferred of each, before they're checked against what's expected of them. So
//! a leak is seen where a call's inferred type goes straight into an annotated binding, as
//! `make_list(nil)` in `let f: (fn<T>(a: T) -> List<T>) | Any = make_list(nil);` did before #490,
//! whose binding has the annotation's type, as well as where it reaches a binding or parameter, as
//! `x: T` in `let f: (fn<T>(a: T) -> T) | Any = make_from_lambda(fn(x) => x);` did. Compiling
//! records nothing new for it, so normal compiles pay nothing.

use std::collections::{HashMap, HashSet};

use rowan::TextRange;
use std::ops::Range;

use id_arena::{ArenaBehavior, DefaultArenaBehavior};
use rue_ast::{AstDocument, AstFunctionItem, AstNode};
use rue_compiler::{Compiler, SyntaxItemKind};
use rue_diagnostic::SourceKind;
use rue_hir::{Hir, HirId, Symbol, SymbolId};
use rue_types::{Type, TypeId};

/// The first binding or parameter in the source, or else the first expression, whose type names a
/// generic outside of where it's bound, as `x: T names T`, if there's one. `first_hir` is where
/// the program's nodes start.
pub fn leak(
    ctx: &mut Compiler,
    document: &AstDocument,
    kind: &SourceKind,
    first_hir: HirId,
) -> Option<String> {
    let mut declarations: HashMap<SymbolId, TextRange> = HashMap::new();
    let mut order = Vec::new();

    for item in ctx.syntax_map().items() {
        if item.source_kind != *kind {
            continue;
        }
        if let SyntaxItemKind::SymbolDeclaration(symbol) = item.kind {
            declarations.entry(symbol).or_insert_with(|| {
                order.push(symbol);
                item.span
            });
        }
    }

    // Each function and lambda, by where it is, with the generics it binds, and where each named
    // function's generics are declared
    let mut binders: Vec<(TextRange, Vec<TypeId>)> = Vec::new();
    let mut declared: HashSet<Range<usize>> = HashSet::new();

    for &symbol in &order {
        if let Symbol::Function(function) = ctx.symbol(symbol)
            && let Some(range) = enclosing::<AstFunctionItem>(document, declarations[&symbol])
        {
            declared.extend(
                function
                    .vars
                    .iter()
                    .filter_map(|&var| declaration(ctx, kind, var)),
            );
            binders.push((range, function.vars.clone()));
        }
    }

    // Each lambda's range is where its node was compiled from, which covers one without
    // parameters too
    let lambdas: Vec<(HirId, SymbolId)> = hir_ids(first_hir, ctx.next_hir_id())
        .filter_map(|hir| match ctx.hir(hir) {
            Hir::Lambda(symbol) => Some((hir, *symbol)),
            _ => None,
        })
        .collect();

    let mut lambda_ranges = Vec::new();

    for (hir, symbol) in lambdas {
        let Symbol::Function(function) = ctx.symbol(symbol) else {
            continue;
        };
        let Some(range) = main_range(ctx, kind, hir) else {
            continue;
        };
        // Only a lambda with parameters can have one compiled as a callee's generic (see below)
        if !function.parameters.is_empty() {
            lambda_ranges.push(range);
        }
        binders.push((range, function.vars.clone()));
    }

    // The first generic in the type that isn't in scope at the span: one that a function or lambda
    // around it binds, or, with `function_generics`, any that one of the program's named functions
    // declares
    let leaked = |ctx: &mut Compiler, ty: TypeId, span: TextRange, function_generics: bool| {
        let mut free = Vec::new();
        free_generics(ctx, ty, &mut Vec::new(), &mut HashSet::new(), &mut free);
        free.into_iter().find(|&generic| {
            let bound = binders
                .iter()
                .any(|(range, vars)| range.contains_range(span) && vars.contains(&generic));
            let declared_by_function = function_generics
                && declaration(ctx, kind, generic).is_some_and(|span| declared.contains(&span));
            !bound && !declared_by_function
        })
    };

    let message = |ctx: &mut Compiler, label: &str, ty: TypeId, generic: TypeId| {
        format!(
            "`{label}: {}` names `{}` outside of where it's bound",
            ctx.type_name(ty),
            ctx.type_name(generic)
        )
    };

    // A lambda's untyped parameter can be compiled as a callee's generic, or a renamed copy of one,
    // which keeps its name and where it's declared, and the lambda's type is instantiated from it
    // once the call is solved, as an element lambda that's compiled where it is (#485). Those are
    // declared by a function, unlike the generic of an expected generic function type that #490
    // named outside of it, so a parameter can have them
    for symbol in order {
        let (name, ty, parameter) = match ctx.symbol(symbol) {
            Symbol::Binding(binding) => (binding.name.clone(), binding.value.ty, false),
            Symbol::Parameter(parameter) => (parameter.name.clone(), parameter.ty, true),
            _ => continue,
        };
        let span = declarations[&symbol];

        if let Some(generic) = leaked(ctx, ty, span, parameter) {
            let name = name.map_or_else(|| "_".to_string(), |name| name.text().to_string());
            return Some(message(ctx, &name, ty, generic));
        }
    }

    // The expressions, in the order they were compiled. One in a lambda can be made of such a
    // parameter, and a pair or list around a lambda has its type recorded before the call is
    // solved, so they can have a function's generics too. A call's type never has its callee's
    // generics, so a call around a lambda can't
    for hir in hir_ids(first_hir, ctx.next_hir_id()) {
        let Some(ty) = ctx.hir_type(hir) else {
            continue;
        };
        let Some(span) = main_range(ctx, kind, hir) else {
            continue;
        };
        let function_generics = lambda_ranges.iter().any(|range| {
            range.contains_range(span)
                || span.contains_range(*range) && !matches!(ctx.hir(hir), Hir::FunctionCall(_))
        });

        if let Some(generic) = leaked(ctx, ty, span, function_generics) {
            let text = document.syntax().text().slice(span).to_string();
            return Some(message(ctx, &text, ty, generic));
        }
    }

    None
}

/// The ids of the nodes allocated from `first` up to `next`, both of which are from
/// `Database::next_hir_id`.
pub(crate) fn hir_ids(first: HirId, next: HirId) -> impl Iterator<Item = HirId> {
    let arena = DefaultArenaBehavior::<Hir>::arena_id(first);
    (first.index()..next.index())
        .map(move |index| DefaultArenaBehavior::<Hir>::new_id(arena, index))
}

/// Where in the main file the node was compiled from, if it was compiled from there.
fn main_range(ctx: &Compiler, kind: &SourceKind, hir: HirId) -> Option<TextRange> {
    let srcloc = ctx.hir_srcloc(hir)?;
    if srcloc.source.kind != *kind {
        return None;
    }
    let start = u32::try_from(srcloc.span.start).ok()?;
    let end = u32::try_from(srcloc.span.end).ok()?;
    Some(TextRange::new(start.into(), end.into()))
}

/// Where the generic's name is declared in the source, if it's named there. Only the main file's
/// declarations count, since a generic of the standard library can have the same span as one of
/// the main file's, which no test program can arrange.
fn declaration(ctx: &Compiler, kind: &SourceKind, generic: TypeId) -> Option<Range<usize>> {
    let Type::Generic(generic) = ctx.ty(generic) else {
        return None;
    };
    let srcloc = generic.name.as_ref()?.srcloc()?;
    (srcloc.source.kind == *kind).then(|| srcloc.span.clone())
}

/// The range of the innermost node of the kind around the span.
fn enclosing<N: AstNode>(document: &AstDocument, span: TextRange) -> Option<TextRange> {
    document
        .syntax()
        .covering_element(span)
        .ancestors()
        .find_map(N::cast)
        .map(|node| node.syntax().text_range())
}

/// The generics in the type that no function type in it binds, in the order they're found. A
/// generic alias or struct binds its own generics, which its generic arguments stand for.
pub(crate) fn free_generics(
    ctx: &mut Compiler,
    ty: TypeId,
    bound: &mut Vec<TypeId>,
    visited: &mut HashSet<TypeId>,
    free: &mut Vec<TypeId>,
) {
    // A generic is a leaf, which is free wherever it isn't bound, so it's checked each time it's
    // reached, as in `(fn<T>(a: T) -> T, T)`
    if let Type::Generic(_) = ctx.ty(ty) {
        if !bound.contains(&ty) && !free.contains(&ty) {
            free.push(ty);
        }
        return;
    }

    if !visited.insert(ty) {
        return;
    }

    // An applied type is walked as it's written, its generic arguments and the type they're
    // applied to, with the generics they stand for bound in it, rather than substituted, which
    // makes new types each time a recursive alias such as `Tree<T>` is expanded. That's an alias
    // or struct, which binds its own generics too, or a generic function's type, as
    // `wrap_maybe::<Int>`'s is `fn(value: T) -> Maybe<T>` with `T` as `Int`
    match ctx.ty(ty).clone() {
        Type::Generic(_) => unreachable!("checked above"),
        Type::Ref(inner) => free_generics(ctx, inner, bound, visited, free),
        Type::Pair(pair) => {
            free_generics(ctx, pair.first, bound, visited, free);
            free_generics(ctx, pair.rest, bound, visited, free);
        }
        Type::Union(union) => {
            for member in union.types {
                free_generics(ctx, member, bound, visited, free);
            }
        }
        Type::Function(function) => {
            let depth = bound.len();
            bound.extend(&function.generics);
            for param in function.param_types() {
                free_generics(ctx, param, bound, visited, free);
            }
            free_generics(ctx, function.ret, bound, visited, free);
            bound.truncate(depth);
        }
        // An alias or struct whose generics weren't applied binds them, as its declaration does
        Type::Alias(alias) => {
            let depth = bound.len();
            bound.extend(&alias.generics);
            free_generics(ctx, alias.inner, bound, visited, free);
            bound.truncate(depth);
        }
        Type::Struct(structure) => {
            let depth = bound.len();
            bound.extend(&structure.generics);
            free_generics(ctx, structure.inner, bound, visited, free);
            bound.truncate(depth);
        }
        Type::Apply(apply) => {
            for &arg in apply.generics.values() {
                free_generics(ctx, arg, bound, visited, free);
            }
            let depth = bound.len();
            bound.extend(apply.generics.keys());
            free_generics(ctx, apply.inner, bound, visited, free);
            bound.truncate(depth);
        }
        Type::Unresolved | Type::Any | Type::Never | Type::Atom(_) => {}
    }
}
