use std::collections::HashSet;

use log::debug;
use rowan::TextRange;
use rue_ast::{AstBinding, AstMatchArm, AstMatchExpr, AstNode};
use rue_diagnostic::{DiagnosticKind, Note, SrcLoc};
use rue_hir::{
    BindingSymbol, Block, Declaration, Guard, Hir, HirId, Mappings, Statement, Symbol, SymbolPath,
    Value, control_fallthroughs,
};
use rue_lint::{IndistinguishableMatchArm, UnreachableMatchArm, UnreachableReason};
use rue_types::{BuiltinTypes, Check, Type, TypeId, Union};

use crate::{
    Branches, Compiler, compile_branching_expr, compile_expr, compile_type, create_binding,
    type_at_path,
};

/// Compiles a `match` expression into the `if` and `else if` chain of type guards that it's
/// equivalent to, so that it's checked, narrowed and generated the same way. Each arm checks the
/// value against its pattern's type, out of the values that the arms before it didn't handle, and
/// an arm that handles every value that's left is the chain's `else` branch. An `inline match` is an
/// `inline if` chain, so every arm is evaluated and none of them narrow the value.
pub fn compile_match_expr(
    ctx: &mut Compiler,
    expr: &AstMatchExpr,
    expected_type: Option<TypeId>,
    arm_kind: Option<Branches>,
) -> (Value, Option<Mappings>) {
    let inline = expr.inline().is_some();

    let scrutinee = if let Some(scrutinee) = expr.expr() {
        compile_expr(ctx, &scrutinee, None)
    } else {
        debug!("Unresolved match expr");
        ctx.unresolved()
    };

    let mut statements = Vec::new();

    // The value is checked by each arm, so a value that isn't a reference is bound first, rather
    // than being evaluated again for each check. A reference is narrowed in the arms, like it is
    // by an `is` guard.
    let reference = if let Some(reference) = scrutinee.reference.clone() {
        reference
    } else {
        let symbol = ctx.alloc_symbol(Symbol::Binding(BindingSymbol {
            name: None,
            value: scrutinee.clone(),
            inline: false,
        }));
        let srcloc = expr.expr().map_or_else(
            || ctx.trimmed_srcloc(expr.syntax()),
            |scrutinee| ctx.trimmed_srcloc(scrutinee.syntax()),
        );
        statements.push(Statement::Let(symbol, srcloc));
        SymbolPath::new(symbol, vec![])
    };

    // Once there's an error, the arms might not handle what they're meant to, so nothing is
    // reported about which values they handle
    let mut has_error = !expr.arms_are_complete() || contains_unresolved(ctx, scrutinee.ty);

    // The values that the arms so far haven't handled, which is what subtracting each pattern from
    // the value's type leaves, like the `else` branch of an `is` guard
    let mut remaining = rue_types::substitute(ctx.types_mut(), scrutinee.ty);
    let substituted = remaining;
    let mut remaining_members = members(ctx, remaining);
    let mut catch_all = None;
    let mut branches = Vec::new();
    let mut else_branch = None;
    let mut types = Vec::new();
    // The types of the value in the arms of a statement that fall through to the rest of the
    // block, which only sees those values
    let mut fallthrough_types = Vec::new();
    let mut handled = Vec::new();
    let mut indistinguishable = Vec::new();

    let checkpoint = ctx.mapping_checkpoint();

    // A cast keeps the reference of what it casts, which the arms narrow, but a binding
    // destructures the value as the type it was cast to. Comparing the types can be slow for a
    // large union, so it's only done if there's a binding.
    let recast = expr.arms().any(|arm| arm.binding().is_some()) && {
        let own = type_at_path(ctx, ctx.symbol_type(reference.symbol), &reference.path);
        own.is_none_or(|own| {
            own != scrutinee.ty
                && !(ctx.is_assignable(own, scrutinee.ty) && ctx.is_assignable(scrutinee.ty, own))
        })
    };

    let matched = Matched {
        scrutinee: &scrutinee,
        reference: &reference,
        recast,
        inline,
        expected_type,
        branches: arm_kind,
    };

    for arm in expr.arms() {
        let ty_node = arm.ty();
        let arm_srcloc = ctx.trimmed_srcloc(arm.syntax());
        let pattern_srcloc = ty_node
            .as_ref()
            .map_or_else(|| arm_srcloc.clone(), |ty| ctx.trimmed_srcloc(ty.syntax()));

        let pattern = match &ty_node {
            Some(ty) if ty.is_placeholder() => None,
            Some(ty) => {
                let ty = compile_type(ctx, ty);
                has_error |= contains_unresolved(ctx, ty);
                Some(ty)
            }
            None => {
                debug!("Unresolved match arm pattern");
                has_error = true;
                None
            }
        };

        // Once an arm has handled every value that's left, the ones after it are never taken
        if catch_all.is_some() || remaining_members.is_empty() {
            if !has_error {
                let reason = catch_all
                    .clone()
                    .map_or(UnreachableReason::Handled, UnreachableReason::CatchAll);
                report_unreachable(ctx, pattern_srcloc, pattern, reason, &mut indistinguishable);
            }
            compile_unreachable_arm(ctx, &matched, &arm, pattern, remaining);
            continue;
        }

        // Subtracting a pattern only removes the members of a union that it covers, so a pattern
        // such as a literal that's already been matched also needs to be compared with the earlier
        // patterns
        if !has_error
            && let Some(ty) = pattern
            && !handled.is_empty()
        {
            let earlier = ctx.alloc_type(Type::Union(Union::new(handled.clone())));
            if ctx.is_assignable(ty, earlier) {
                let reason = UnreachableReason::Handled;
                report_unreachable(ctx, pattern_srcloc, pattern, reason, &mut indistinguishable);
                compile_unreachable_arm(ctx, &matched, &arm, pattern, remaining);
                continue;
            }
        }

        let check = match (pattern, &ty_node) {
            (Some(ty), Some(node)) => {
                if let Some(check) = ctx.runtime_check(node.syntax(), remaining, ty) {
                    check
                } else {
                    has_error = true;
                    Check::Impossible
                }
            }
            _ => Check::None,
        };

        if check == Check::Impossible {
            // A pattern that doesn't overlap the value at all is a mistake, as it is for `is`,
            // rather than one whose values the earlier arms handle
            if !has_error && let Some(ty) = pattern {
                if overlaps(ctx, scrutinee.ty, ty) {
                    let reason = UnreachableReason::Handled;
                    report_unreachable(
                        ctx,
                        pattern_srcloc,
                        pattern,
                        reason,
                        &mut indistinguishable,
                    );
                } else {
                    let from = ctx.type_name(scrutinee.ty);
                    let to = ctx.type_name(ty);
                    ctx.diagnostic_at(pattern_srcloc, DiagnosticKind::IncompatibleGuard(from, to));
                    has_error = true;
                }
            }
            compile_unreachable_arm(ctx, &matched, &arm, pattern, remaining);
            continue;
        }

        let builtins = ctx.builtins().types;

        // Nothing is left after an arm that doesn't need to check anything, so there's nothing
        // to subtract, which can be slow for large unions
        let else_type = match pattern {
            Some(ty) if check != Check::None => ctx.subtract_type(&pattern_srcloc, remaining, ty),
            _ => builtins.never,
        };

        if !has_error && let Some(ty) = pattern {
            let members = indistinguishable_members(ctx, &remaining_members, ty);
            if !members.is_empty() {
                indistinguishable.push(Indistinguishable {
                    pattern: pattern_srcloc,
                    ty,
                    members,
                });
            }
        }

        let else_members = members(ctx, else_type);

        // A wildcard arm sees the value as the earlier arms left it, which is already in scope
        let narrowed = pattern.filter(|_| !inline);
        let body = compile_arm(ctx, &matched, &arm, pattern, narrowed, remaining);
        types.push(body.ty);

        let catches_all = check == Check::None || else_members.is_empty();

        // An arm that handles everything that's left sees the members that are left, rather
        // than the type they're left in, which can keep the name of an alias that's been narrowed
        if arm_kind.is_some_and(Branches::is_statement) && control_fallthroughs(ctx, body.hir) > 0 {
            match pattern {
                Some(ty) if !catches_all => fallthrough_types.push(ty),
                _ => fallthrough_types.extend(remaining_members.iter().copied()),
            }
        }

        // An arm that handles every value that's left is the `else` branch, which doesn't need to
        // check anything
        if catches_all {
            else_branch = Some(body.hir);
            catch_all = Some(arm_srcloc);
            continue;
        }

        let ty = pattern.expect("only a pattern with a type has a check");

        // Every check of an `inline match` is evaluated, even on the values that the arms before
        // it handle, so it can't rely on them being left out
        let check = if inline {
            let node = ty_node
                .as_ref()
                .expect("only a pattern with a type has a check");
            if let Some(check) = ctx.runtime_check(node.syntax(), substituted, ty) {
                check
            } else {
                has_error = true;
                check
            }
        } else {
            check
        };

        let value = guard_value(ctx, &scrutinee, &reference);
        let condition = ctx.alloc_hir(Hir::Guard(Guard { value, ty, check }));
        branches.push((condition, body.hir));
        handled.push(ty);

        // The arms after this one only see the values that it didn't handle, like the `else`
        // branch of an `is` guard, unless they're evaluated eagerly
        if !inline {
            ctx.push_mappings(
                reference.narrowed_to(else_type),
                arm.syntax().text_range().end(),
            );
        }
        remaining = else_type;
        remaining_members = else_members;
    }

    ctx.revert_mappings(checkpoint, expr.syntax().text_range().end());

    // The rest of the block after a statement only sees the values of the arms that fall through
    // to it, unless the arms are evaluated eagerly
    let narrowing = (arm_kind.is_some_and(Branches::is_statement)
        && !inline
        && !has_error
        && !fallthrough_types.is_empty())
    .then(|| {
        let ty = match fallthrough_types.len() {
            1 => fallthrough_types[0],
            _ => ctx.alloc_type(Type::Union(Union::new(fallthrough_types))),
        };
        reference.narrowed_to(ty)
    });

    // The arms that take members which no later arm names are reported themselves, since nothing
    // else shows that those members end up there
    if !has_error {
        for arm in indistinguishable
            .into_iter()
            .filter(|arm| !arm.members.is_empty())
        {
            let ty = ctx.type_name(arm.ty);
            let members = format_members(ctx, arm.members);
            ctx.lint_sites_mut()
                .indistinguishable_match_arms
                .push(IndistinguishableMatchArm {
                    pattern: arm.pattern,
                    ty,
                    members,
                });
        }
    }

    // The arms that are evaluated, which are all of them for an `inline match`
    let arms: Vec<HirId> = branches
        .iter()
        .map(|&(_, then)| then)
        .chain(else_branch)
        .collect();

    let else_branch = if let Some(else_branch) = else_branch {
        else_branch
    } else if has_error || !remaining_members.is_empty() {
        if !has_error {
            let ty = ctx.type_name(scrutinee.ty);
            let missing = format_members(ctx, remaining_members);
            let node = expr.expr().map_or_else(
                || expr.syntax().clone(),
                |scrutinee| scrutinee.syntax().clone(),
            );
            ctx.diagnostic(&node, DiagnosticKind::NonExhaustiveMatch(ty, missing));
        }

        ctx.unresolved().hir
    } else {
        // A match with no arms on a value that never exists (such as a block that raises), which
        // is never evaluated
        let srcloc = ctx.trimmed_srcloc(expr.syntax());
        ctx.alloc_hir(Hir::Block(Block {
            statements: vec![Statement::Raise(None, srcloc)],
            body: None,
        }))
    };

    let chain = branches
        .into_iter()
        .rev()
        .fold(else_branch, |else_branch, (condition, then)| {
            ctx.alloc_hir(Hir::If(condition, then, else_branch, inline))
        });

    let ty = match types.len() {
        0 => ctx.builtins().types.never,
        1 => types[0],
        _ => ctx.alloc_type(Type::Union(Union::new(types))),
    };

    // The chain is wrapped in a block, which is what the match's location is recorded on, so that
    // the lints about `if` expressions don't see the chain as one
    let hir = ctx.alloc_hir(Hir::Block(Block {
        statements,
        body: Some(chain),
    }));

    if inline {
        ctx.lint_sites_mut().inline_match_arms.insert(hir, arms);
    }

    (Value::new(hir, ty), narrowing)
}

/// The value that a `match` is on, and what its arms are compiled with.
struct Matched<'a> {
    scrutinee: &'a Value,
    /// What the arms narrow, which is the binding of a value that isn't a reference.
    reference: &'a SymbolPath,
    /// Whether the value's type isn't the type of what the arms narrow, such as `value as T`.
    recast: bool,
    inline: bool,
    expected_type: Option<TypeId>,
    branches: Option<Branches>,
}

/// Compiles an arm's body, with the value narrowed to a type, if the arm narrows it. If the arms
/// are blocks of a given kind, such as in a statement (see [`compile_branching_expr`]), so is a
/// body that's a block, `if` or `match`, and in a statement any other body is an error, since its
/// value isn't used.
///
/// An arm's binding destructures the value as the body sees it, so it's in scope there.
/// `remaining` is the type of the values that the arms before this one didn't handle.
fn compile_arm(
    ctx: &mut Compiler,
    matched: &Matched<'_>,
    arm: &AstMatchArm,
    pattern: Option<TypeId>,
    narrowed: Option<TypeId>,
    remaining: TypeId,
) -> Value {
    let body = arm.expr();

    // An arm without a body still declares its bindings, so that they can be looked up
    let range = body.as_ref().map_or_else(
        || TextRange::empty(arm.syntax().text_range().end()),
        |body| body.syntax().text_range(),
    );
    let mappings = narrowed
        .map(|ty| matched.reference.narrowed_to(ty))
        .unwrap_or_default();

    // The bindings are declared in the scope that the mappings are in, which is the arm's own,
    // so a name that's bound twice is reported, and one that's already in scope is shadowed
    let index = ctx.push_mappings(mappings, range.start());

    if let Some(binding) = arm.binding() {
        let count = ctx.diagnostic_count();
        compile_arm_binding(ctx, matched, &binding, narrowed, remaining);

        // The arms of an `inline match` don't narrow the value, which is the most likely reason
        // that it can't be destructured like the arm's type
        if matched.inline
            && let Some(ty) = pattern
            && ctx.diagnostic_count() > count
            && !ctx.is_assignable(matched.scrutinee.ty, ty)
        {
            note_unnarrowed(ctx, count, ty);
        }
    }

    let expected_type = matched.expected_type;

    let value = match (body, matched.branches) {
        (None, _) => {
            debug!("Unresolved match arm body");
            ctx.unresolved()
        }
        (Some(body), Some(branches)) if arm.ends_with_brace() => {
            compile_branching_expr(ctx, &body, expected_type, branches).0
        }
        (Some(body), Some(branches)) if branches.is_statement() => {
            ctx.diagnostic(body.syntax(), DiagnosticKind::StatementMatchArmValue);
            compile_expr(ctx, &body, expected_type)
        }
        (Some(body), _) => compile_expr(ctx, &body, expected_type),
    };

    ctx.revert_mappings(index, range.end());
    value
}

/// Compiles the body of an arm that's never taken, so that its errors are still reported. It's
/// left out of the HIR, so it isn't generated. The arms of an `inline match` don't narrow the
/// value, so it isn't narrowed here either.
fn compile_unreachable_arm(
    ctx: &mut Compiler,
    matched: &Matched<'_>,
    arm: &AstMatchArm,
    pattern: Option<TypeId>,
    remaining: TypeId,
) {
    let narrowed = (!matched.inline).then(|| pattern.unwrap_or(ctx.builtins().types.never));
    let _ = compile_arm(ctx, matched, arm, pattern, narrowed, remaining);
}

/// Declares the bindings of an arm, such as `x` and `y` in `Point { x, y } => x + y`, which
/// destructure the value as the arm sees it: narrowed to the arm's type, unless the arms don't
/// narrow it. This is the same as a `let` with the binding at the start of the arm's body, so it
/// has nothing to do with which values the arm matches.
///
/// The value is substituted for the binding, like the parts that a `let` destructures a value
/// into are, so that the generated code is the same as reading the fields from the value.
fn compile_arm_binding(
    ctx: &mut Compiler,
    matched: &Matched<'_>,
    binding: &AstBinding,
    narrowed: Option<TypeId>,
    remaining: TypeId,
) {
    let Matched {
        scrutinee,
        reference,
        ..
    } = *matched;

    let ty = if matched.recast {
        // What the arms narrow isn't the type that the value was cast to, so that type is
        // narrowed the same way instead
        match narrowed {
            Some(ty) => {
                let builtins = ctx.builtins().types;
                rue_types::intersect(ctx.types_mut(), &builtins, remaining, ty)
            }
            None if matched.inline => scrutinee.ty,
            None => remaining,
        }
    } else {
        // The mappings of the arm are in scope here, so this is the value's narrowed type. It's
        // the type that a field access in the arm reads from, which keeps the references that an
        // untyped lambda parameter's type is made of (see `narrowed_with_references`), unlike
        // narrowing the type again would.
        type_at_path(ctx, ctx.symbol_type(reference.symbol), &reference.path)
            .unwrap_or(scrutinee.ty)
    };

    let hir = guard_value(ctx, scrutinee, reference);

    let symbol = ctx.alloc_symbol(Symbol::Binding(BindingSymbol {
        name: None,
        value: Value::new(hir, ty).with_reference(reference.clone()),
        inline: true,
    }));

    ctx.push_declaration(Declaration::Symbol(symbol));
    create_binding(ctx, symbol, binding);
    ctx.pop_declaration();
}

/// Adds a note to the errors about what a binding in an `inline match` arm can't destructure,
/// out of the diagnostics reported since there were `count` of them, since the value isn't
/// narrowed to the arm's type there.
fn note_unnarrowed(ctx: &mut Compiler, count: usize, ty: TypeId) {
    let name = ctx.type_name(ty);
    let note = Note::new(format!(
        "Every arm of an `inline match` is evaluated, so the value isn't narrowed to `{name}` before it's destructured"
    ));

    for diagnostic in ctx.diagnostics_since_mut(count) {
        if matches!(
            diagnostic.kind,
            DiagnosticKind::UnknownField(..)
                | DiagnosticKind::MissingField(..)
                | DiagnosticKind::CannotDestructurePair(..)
        ) {
            diagnostic.notes.push(note.clone());
        }
    }
}

/// An arm whose check also lets through members it can't tell apart from its own type.
struct Indistinguishable {
    pattern: SrcLoc,
    ty: TypeId,
    /// The members that no later arm has named yet.
    members: Vec<TypeId>,
}

/// Records that an arm is never taken. If it names members that an earlier arm takes, although
/// they aren't of that arm's type, that's the reason given, since it's the one that's least
/// expected.
fn report_unreachable(
    ctx: &mut Compiler,
    pattern_srcloc: SrcLoc,
    pattern: Option<TypeId>,
    mut reason: UnreachableReason,
    indistinguishable: &mut [Indistinguishable],
) {
    if let Some(ty) = pattern {
        for arm in indistinguishable.iter_mut() {
            // A pattern that covers the earlier arm's own type is unreachable whatever else it takes
            if ctx.is_assignable(arm.ty, ty) {
                continue;
            }

            let (named, unnamed): (Vec<TypeId>, Vec<TypeId>) = arm
                .members
                .iter()
                .partition(|&&member| ctx.is_assignable(member, ty));

            if !named.is_empty() {
                arm.members = unnamed;
                reason = UnreachableReason::Indistinguishable {
                    arm: arm.pattern.clone(),
                    members: format_members(ctx, named),
                };
                break;
            }
        }
    }

    ctx.lint_sites_mut()
        .unreachable_match_arms
        .push(UnreachableMatchArm {
            pattern: pattern_srcloc,
            reason,
        });
}

/// The members of what's left that the arm's check lets through, although they aren't values of
/// its type. They're the same at runtime as the pattern, so there's no way to check which one a
/// value is, and the arm sees them as its type. An `is` guard warns about this when it doesn't
/// check anything (see `indistinguishable_guard`).
fn indistinguishable_members(ctx: &mut Compiler, remaining: &[TypeId], ty: TypeId) -> Vec<TypeId> {
    // Each member is compared on its own, rather than comparing the whole union with the pattern,
    // since the members of a large union are mostly the same few types (see #496). A member that
    // the pattern is narrower than, such as `Atom` for an `Int` pattern, is what checking is for.
    remaining
        .iter()
        .copied()
        .filter(|&member| {
            ctx.is_castable(member, ty)
                && !ctx.is_assignable(member, ty)
                && !ctx.is_assignable(ty, member)
        })
        .collect()
}

/// The value that an arm's type guard checks. Each guard has a node of its own, like the guards of
/// an `else if` chain would.
fn guard_value(ctx: &mut Compiler, scrutinee: &Value, reference: &SymbolPath) -> HirId {
    if scrutinee.reference.is_some() {
        let hir = ctx.hir(scrutinee.hir).clone();
        ctx.alloc_hir(hir)
    } else {
        ctx.alloc_hir(Hir::Reference(reference.symbol))
    }
}

/// Whether some value of type `from` could be a `to` at runtime.
fn overlaps(ctx: &mut Compiler, from: TypeId, to: TypeId) -> bool {
    let builtins = ctx.builtins().types;
    !matches!(
        rue_types::check(ctx.types_mut(), &builtins, from, to),
        Ok(Check::Impossible)
    )
}

fn contains_unresolved(ctx: &Compiler, ty: TypeId) -> bool {
    rue_types::contains_unresolved(ctx.types(), ty)
}

/// A list of types for a diagnostic, such as "`A`, `B` and `C`".
fn format_members(ctx: &mut Compiler, members: Vec<TypeId>) -> String {
    let builtins = ctx.builtins().types;
    let mut names: Vec<String> = Vec::new();

    for member in members {
        let name = format!("`{}`", member_name(ctx, &builtins, member));

        if !names.contains(&name) {
            names.push(name);
        }
    }

    match names.split_last() {
        Some((last, [])) => last.clone(),
        Some((last, rest)) => format!("{} and {last}", rest.join(", ")),
        None => "every value".to_string(),
    }
}

/// The name of a member in a diagnostic. What's left of `Any` once some of it is handled is written
/// in terms of `Any`, rather than the recursive type that it's made of. For example, `(Int, Any)`
/// leaves the pairs `((Any, Any), Any)`, since every atom is an `Int`.
fn member_name(ctx: &mut Compiler, builtins: &BuiltinTypes, ty: TypeId) -> String {
    member_name_impl(ctx, builtins, ty, &mut HashSet::new())
}

fn member_name_impl(
    ctx: &mut Compiler,
    builtins: &BuiltinTypes,
    ty: TypeId,
    visited: &mut HashSet<TypeId>,
) -> String {
    let equivalent = |ctx: &mut Compiler, other: TypeId| {
        ctx.is_assignable(ty, other) && ctx.is_assignable(other, ty)
    };

    if equivalent(ctx, builtins.recursive_any) {
        return "Any".to_string();
    }

    // The parts of a recursive type lead back to it, and are named as a whole there
    if !visited.insert(ty) {
        return ctx.type_name(ty);
    }

    let name = match ctx.ty(ty).clone() {
        Type::Pair(_) if equivalent(ctx, builtins.recursive_any_pair) => "(Any, Any)".to_string(),
        Type::Pair(pair) => {
            let first = member_name_impl(ctx, builtins, pair.first, visited);
            let rest = member_name_impl(ctx, builtins, pair.rest, visited);
            format!("({first}, {rest})")
        }
        Type::Union(union) => union
            .types
            .iter()
            .map(|&member| member_name_impl(ctx, builtins, member, visited))
            .collect::<Vec<_>>()
            .join(" | "),
        _ => ctx.type_name(ty),
    };

    visited.remove(&ty);
    name
}

/// The members of a union, with aliases looked through, since an alias that's been narrowed by
/// the arms is only part of what its name means. `Any` is split into an atom and a pair, as
/// subtracting from it does. There are none if the type has no values, which is what's left once
/// every member has been subtracted. This is quicker than comparing the type with `Never`, which
/// compares each member of a union, even when they're the same type.
fn members(ctx: &Compiler, ty: TypeId) -> Vec<TypeId> {
    let mut members = Vec::new();
    collect_members(ctx, ty, &mut HashSet::new(), &mut members);
    members
}

fn collect_members(
    ctx: &Compiler,
    ty: TypeId,
    visited: &mut HashSet<TypeId>,
    members: &mut Vec<TypeId>,
) {
    if !visited.insert(ty) {
        return;
    }

    match ctx.ty(ty) {
        Type::Ref(inner) => collect_members(ctx, *inner, visited, members),
        Type::Alias(alias) => collect_members(ctx, alias.inner, visited, members),
        Type::Union(union) => {
            for &ty in &union.types {
                collect_members(ctx, ty, visited, members);
            }
        }
        Type::Any => {
            let builtins = ctx.builtins().types;
            members.push(builtins.atom);
            members.push(builtins.recursive_any_pair);
        }
        Type::Never => {}
        // The types here have already been substituted, so an application is only kept as a
        // member, rather than being looked inside of
        Type::Unresolved
        | Type::Generic(_)
        | Type::Atom(_)
        | Type::Pair(_)
        | Type::Struct(_)
        | Type::Function(_)
        | Type::Apply(_) => members.push(ty),
    }
}
