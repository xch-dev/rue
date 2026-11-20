use std::collections::HashSet;

use log::debug;
use rue_ast::{AstMatchArm, AstMatchExpr, AstNode};
use rue_diagnostic::{DiagnosticKind, SrcLoc};
use rue_hir::{BindingSymbol, Block, Guard, Hir, HirId, Statement, Symbol, SymbolPath, Value};
use rue_lint::UnreachableMatchArm;
use rue_types::{AtomSemantic, Check, Type, TypeId, Union};

use crate::{Compiler, compile_expr, compile_type};

/// Compiles a `match` expression into the `if` and `else if` chain of type guards that it's
/// equivalent to, so that it's checked, narrowed and generated the same way. Each arm checks the
/// value against its pattern's type, out of the values that the arms before it didn't handle, and
/// an arm that handles every value that's left is the chain's `else` branch.
pub fn compile_match_expr(
    ctx: &mut Compiler,
    expr: &AstMatchExpr,
    expected_type: Option<TypeId>,
) -> Value {
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
    let mut catch_all = None;
    let mut branches = Vec::new();
    let mut else_branch = None;
    let mut types = Vec::new();
    let mut handled = Vec::new();

    let checkpoint = ctx.mapping_checkpoint();

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
        if catch_all.is_some() || is_empty(ctx, remaining) {
            if !has_error {
                report_unreachable(ctx, pattern_srcloc, catch_all.clone());
            }
            compile_unreachable_arm(ctx, &arm, &reference, pattern, expected_type);
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
                report_unreachable(ctx, pattern_srcloc, None);
                compile_unreachable_arm(ctx, &arm, &reference, pattern, expected_type);
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
                    report_unreachable(ctx, pattern_srcloc, None);
                } else {
                    let from = ctx.type_name(scrutinee.ty);
                    let to = ctx.type_name(ty);
                    ctx.diagnostic_at(pattern_srcloc, DiagnosticKind::IncompatibleGuard(from, to));
                    has_error = true;
                }
            }
            compile_unreachable_arm(ctx, &arm, &reference, pattern, expected_type);
            continue;
        }

        let builtins = ctx.builtins().types;

        // Nothing is left after an arm that doesn't need to check anything, so there's nothing
        // to subtract, which can be slow for large unions
        let else_type = match pattern {
            Some(ty) if check != Check::None => {
                rue_types::subtract(ctx.types_mut(), &builtins, remaining, ty)
            }
            _ => builtins.never,
        };

        if !has_error && let Some(ty) = pattern {
            check_distinguishable(ctx, remaining, ty, pattern_srcloc);
        }

        // A wildcard arm sees the value as the earlier arms left it, which is already in scope
        let body = compile_arm(ctx, &arm, &reference, pattern, expected_type);
        types.push(body.ty);

        // An arm that handles every value that's left is the `else` branch, which doesn't need to
        // check anything
        if check == Check::None || is_empty(ctx, else_type) {
            else_branch = Some(body.hir);
            catch_all = Some(arm_srcloc);
            continue;
        }

        let ty = pattern.expect("only a pattern with a type has a check");
        let value = guard_value(ctx, &scrutinee, &reference);
        let condition = ctx.alloc_hir(Hir::Guard(Guard { value, ty, check }));
        branches.push((condition, body.hir));
        handled.push(ty);

        // The arms after this one only see the values that it didn't handle, like the `else`
        // branch of an `is` guard
        ctx.push_mappings(
            reference.narrowed_to(else_type),
            arm.syntax().text_range().end(),
        );
        remaining = else_type;
    }

    ctx.revert_mappings(checkpoint, expr.syntax().text_range().end());

    let else_branch = if let Some(else_branch) = else_branch {
        else_branch
    } else if has_error || !is_empty(ctx, remaining) {
        if !has_error {
            let ty = ctx.type_name(scrutinee.ty);
            let missing = list_members(ctx, remaining);
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
            ctx.alloc_hir(Hir::If(condition, then, else_branch, false))
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

    Value::new(hir, ty)
}

/// Compiles an arm's body, with the value narrowed to the arm's type, if it has one.
fn compile_arm(
    ctx: &mut Compiler,
    arm: &AstMatchArm,
    reference: &SymbolPath,
    ty: Option<TypeId>,
    expected_type: Option<TypeId>,
) -> Value {
    let Some(body) = arm.expr() else {
        debug!("Unresolved match arm body");
        return ctx.unresolved();
    };

    let range = body.syntax().text_range();
    let mappings = ty.map(|ty| reference.narrowed_to(ty)).unwrap_or_default();
    let index = ctx.push_mappings(mappings, range.start());
    let value = compile_expr(ctx, &body, expected_type);
    ctx.revert_mappings(index, range.end());
    value
}

/// Compiles the body of an arm that's never taken, so that its errors are still reported. It's
/// left out of the HIR, so it isn't generated.
fn compile_unreachable_arm(
    ctx: &mut Compiler,
    arm: &AstMatchArm,
    reference: &SymbolPath,
    pattern: Option<TypeId>,
    expected_type: Option<TypeId>,
) {
    let ty = pattern.unwrap_or(ctx.builtins().types.never);
    let _ = compile_arm(ctx, arm, reference, Some(ty), expected_type);
}

fn report_unreachable(ctx: &mut Compiler, pattern: SrcLoc, catch_all: Option<SrcLoc>) {
    ctx.lint_sites_mut()
        .unreachable_match_arms
        .push(UnreachableMatchArm { pattern, catch_all });
}

/// Reports the members of what's left that the arm's check lets through, although they aren't
/// values of its type. They're the same at runtime, so there's no way to check which one a value
/// is, and the arm would see them as its type. An `is` guard warns about this when it doesn't
/// check anything (see `indistinguishable_guard`), but a match must handle each member on purpose.
fn check_distinguishable(ctx: &mut Compiler, remaining: TypeId, ty: TypeId, srcloc: SrcLoc) {
    // Each member is compared on its own, rather than comparing the whole union with the pattern,
    // since the members of a large union are mostly the same few types (see #496)
    let mut members = Vec::new();
    collect_members(ctx, remaining, &mut HashSet::new(), &mut members);

    let indistinguishable: Vec<TypeId> = members
        .into_iter()
        .filter(|&member| {
            // An atom that has no meaning of its own (such as one of `Any`'s) can be read as any
            // kind of atom, which is what checking it is for
            !matches!(ctx.ty(member), Type::Atom(atom) if atom.semantic == AtomSemantic::Any)
                && ctx.is_castable(member, ty)
                && !ctx.is_assignable(member, ty)
        })
        .collect();

    if indistinguishable.is_empty() {
        return;
    }

    let name = ctx.type_name(ty);
    let members = format_members(ctx, indistinguishable);
    ctx.diagnostic_at(
        srcloc,
        DiagnosticKind::IndistinguishableMatchArm(name, members),
    );
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

/// The members of the union that the arms didn't handle, as a list for a diagnostic.
fn list_members(ctx: &mut Compiler, ty: TypeId) -> String {
    let mut members = Vec::new();
    collect_members(ctx, ty, &mut HashSet::new(), &mut members);
    format_members(ctx, members)
}

/// A list of types for a diagnostic, such as "`A`, `B` and `C`".
fn format_members(ctx: &mut Compiler, members: Vec<TypeId>) -> String {
    let builtins = ctx.builtins().types;
    let mut names: Vec<String> = Vec::new();

    for member in members {
        // What's left of `Any` once its atoms are handled is written as the pair it checks for,
        // rather than the recursive type that it's made of
        let name = if matches!(ctx.ty(member), Type::Pair(_))
            && ctx.is_assignable(member, builtins.recursive_any_pair)
            && ctx.is_assignable(builtins.recursive_any_pair, member)
        {
            "`(Any, Any)`".to_string()
        } else {
            format!("`{}`", ctx.type_name(member))
        };

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

/// The members of a union, with aliases looked through, since an alias that's been narrowed by
/// the arms is only part of what its name means. `Any` is split into an atom and a pair, as
/// subtracting from it does.
fn collect_members(
    ctx: &mut Compiler,
    ty: TypeId,
    visited: &mut HashSet<TypeId>,
    members: &mut Vec<TypeId>,
) {
    if !visited.insert(ty) {
        return;
    }

    match ctx.ty(ty).clone() {
        Type::Ref(inner) => collect_members(ctx, inner, visited, members),
        Type::Alias(alias) => collect_members(ctx, alias.inner, visited, members),
        Type::Apply(_) => {
            let ty = rue_types::substitute(ctx.types_mut(), ty);
            collect_members(ctx, ty, visited, members);
        }
        Type::Union(union) => {
            for ty in union.types {
                collect_members(ctx, ty, visited, members);
            }
        }
        Type::Any => {
            let builtins = ctx.builtins().types;
            members.push(builtins.atom);
            members.push(builtins.recursive_any_pair);
        }
        Type::Never => {}
        Type::Unresolved
        | Type::Generic(_)
        | Type::Atom(_)
        | Type::Pair(_)
        | Type::Struct(_)
        | Type::Function(_) => members.push(ty),
    }
}

/// Whether the type has no values, judging by its structure, which is what's left when every
/// member of a union has been subtracted. This is quicker than comparing the type with `Never`,
/// which compares each member of a union, even when they're the same type.
fn is_empty(ctx: &Compiler, ty: TypeId) -> bool {
    is_empty_inner(ctx, ty, &mut HashSet::new())
}

fn is_empty_inner(ctx: &Compiler, ty: TypeId, visited: &mut HashSet<TypeId>) -> bool {
    // A union that contains itself (through an alias) is empty if its other members are
    if !visited.insert(ty) {
        return true;
    }

    match ctx.ty(ty) {
        Type::Never => true,
        Type::Ref(inner) => is_empty_inner(ctx, *inner, visited),
        Type::Alias(alias) => is_empty_inner(ctx, alias.inner, visited),
        Type::Union(union) => union
            .types
            .iter()
            .all(|&ty| is_empty_inner(ctx, ty, visited)),
        // The types here have already been substituted, so an application is only kept as a
        // member, rather than being looked inside of
        Type::Unresolved
        | Type::Generic(_)
        | Type::Any
        | Type::Atom(_)
        | Type::Pair(_)
        | Type::Struct(_)
        | Type::Function(_)
        | Type::Apply(_) => false,
    }
}
