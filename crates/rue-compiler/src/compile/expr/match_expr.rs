use std::collections::{HashMap, HashSet};

use log::debug;
use rue_ast::{AstMatchArm, AstMatchExpr, AstNode};
use rue_diagnostic::DiagnosticKind;
use rue_hir::{
    BindingSymbol, Block, Guard, Hir, HirId, Mappings, Statement, Symbol, SymbolPath, Value,
};
use rue_lint::UnreachableMatchArm;
use rue_types::{Check, Type, TypeId, Union};

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
    let mut has_error = !expr.arms_are_complete() || ctx.is_unresolved(scrutinee.ty);
    let mut remaining = scrutinee.ty;
    let mut catch_all = None;
    let mut branches = Vec::new();
    let mut else_branch = None;
    let mut types = Vec::new();
    let mut handled = Vec::new();

    let checkpoint = ctx.mapping_checkpoint();

    for arm in expr.arms() {
        let pattern = if arm.is_wildcard() {
            None
        } else if let Some(ty) = arm.ty() {
            let ty = compile_type(ctx, &ty);
            has_error |= ctx.is_unresolved(ty);
            Some(ty)
        } else {
            debug!("Unresolved match arm pattern");
            has_error = true;
            None
        };

        let srcloc = ctx.trimmed_srcloc(arm.syntax());
        let pattern_srcloc = arm
            .ty()
            .map_or_else(|| srcloc.clone(), |ty| ctx.trimmed_srcloc(ty.syntax()));

        // Once an arm has handled every value that's left, the ones after it are never taken
        if catch_all.is_some() || is_empty(ctx, remaining) {
            report_unreachable(ctx, has_error, pattern_srcloc, catch_all.clone());
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
                report_unreachable(ctx, has_error, pattern_srcloc, None);
                compile_unreachable_arm(ctx, &arm, &reference, pattern, expected_type);
                continue;
            }
        }

        let check = match pattern {
            None => Check::None,
            Some(ty) => {
                let node = arm
                    .ty()
                    .map_or_else(|| arm.syntax().clone(), |ty| ty.syntax().clone());
                if let Some(check) = ctx.runtime_check(&node, remaining, ty) {
                    check
                } else {
                    has_error = true;
                    Check::Impossible
                }
            }
        };

        if check == Check::Impossible {
            // A pattern that doesn't overlap the value at all is a mistake, as it is for `is`,
            // rather than one whose values the earlier arms handle
            if !has_error && let Some(ty) = pattern {
                if overlaps(ctx, scrutinee.ty, ty) {
                    report_unreachable(ctx, has_error, pattern_srcloc, None);
                } else {
                    let from = ctx.type_name(scrutinee.ty);
                    let to = ctx.type_name(ty);
                    ctx.diagnostic_at(pattern_srcloc, DiagnosticKind::IncompatibleGuard(from, to));
                }
            }
            compile_unreachable_arm(ctx, &arm, &reference, pattern, expected_type);
            continue;
        }

        let narrowed = pattern;
        let pattern = pattern.unwrap_or(remaining);
        let builtins = ctx.builtins().types;

        // Nothing is left after an arm that doesn't need to check anything, so there's nothing
        // to subtract, which can be slow for large unions
        let else_type = if check == Check::None {
            builtins.never
        } else {
            rue_types::subtract(ctx.types_mut(), &builtins, remaining, pattern)
        };

        // A wildcard arm sees the value as the earlier arms left it, which is already in scope
        let body = compile_arm(ctx, &arm, &reference, narrowed, expected_type);
        types.push(body.ty);

        // An arm that handles every value that's left is the `else` branch, which doesn't need to
        // check anything
        if check == Check::None || is_empty(ctx, else_type) {
            else_branch = Some(body.hir);
            catch_all = Some(srcloc);
            continue;
        }

        let value = guard_value(ctx, &scrutinee, &reference);
        let condition = ctx.alloc_hir(Hir::Guard(Guard {
            value,
            ty: pattern,
            check,
        }));
        branches.push((condition, body.hir));
        handled.push(pattern);

        // The arms after this one only see the values that it didn't handle, like the `else`
        // branch of an `is` guard
        ctx.push_mappings(
            narrowing(&reference, else_type),
            arm.syntax().text_range().end(),
        );
        remaining = else_type;
    }

    ctx.revert_mappings(checkpoint, expr.syntax().text_range().end());

    let else_branch = if let Some(else_branch) = else_branch {
        else_branch
    } else {
        // Comparing a large union can be slow, so it's only done once nothing else is left to try
        let exhaustive = is_empty(ctx, remaining) || ctx.is_never(remaining);

        if !has_error && !exhaustive {
            let ty = ctx.type_name(scrutinee.ty);
            let missing = missing_members(ctx, remaining);
            let node = expr.expr().map_or_else(
                || expr.syntax().clone(),
                |scrutinee| scrutinee.syntax().clone(),
            );
            ctx.diagnostic(&node, DiagnosticKind::NonExhaustiveMatch(ty, missing));
        }

        if has_error || !exhaustive {
            ctx.unresolved().hir
        } else {
            // A match with no arms on a value that never exists, which is never evaluated
            let srcloc = ctx.trimmed_srcloc(expr.syntax());
            ctx.alloc_hir(Hir::Block(Block {
                statements: vec![Statement::Raise(None, srcloc)],
                body: None,
            }))
        }
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

    // The chain is wrapped in a block, so that the lints about `if` expressions don't see it
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
    let mappings = ty.map(|ty| narrowing(reference, ty)).unwrap_or_default();
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

fn report_unreachable(
    ctx: &mut Compiler,
    has_error: bool,
    pattern: rue_diagnostic::SrcLoc,
    catch_all: Option<rue_diagnostic::SrcLoc>,
) {
    // An arm after an unresolved pattern could be reachable once it's fixed
    if has_error {
        return;
    }

    ctx.lint_sites_mut()
        .unreachable_match_arms
        .push(UnreachableMatchArm { pattern, catch_all });
}

/// The narrowing of the value to `ty`, as the mappings of a type guard.
fn narrowing(reference: &SymbolPath, ty: TypeId) -> Mappings {
    let mut paths = HashMap::new();
    paths.insert(reference.path.clone(), ty);
    let mut mappings = HashMap::new();
    mappings.insert(reference.symbol, paths);
    mappings
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

/// The members of the union that the arms didn't handle, as a list for a diagnostic, such as
/// "`A`, `B` and `C`". Aliases are listed by what they're made of, since an alias that's been
/// narrowed by the arms is only part of what its name means.
fn missing_members(ctx: &mut Compiler, ty: TypeId) -> String {
    let mut members = Vec::new();
    collect_members(ctx, ty, &mut HashSet::new(), &mut members);

    let builtins = ctx.builtins().types;
    let mut names: Vec<String> = Vec::new();
    for member in members {
        // What's left of `Any` once its atoms are handled is written as the pair it checks for,
        // rather than the recursive type that it's made of
        let name = if member == builtins.recursive_any_pair {
            "`(Any, Any)`".to_string()
        } else if member == builtins.recursive_any {
            "`Any`".to_string()
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
        Type::Union(union) => {
            for ty in union.types {
                collect_members(ctx, ty, visited, members);
            }
        }
        Type::Never => {}
        _ => members.push(ty),
    }
}

/// Whether the type has no values, judging by its structure, which is what's left when every
/// member of a union has been subtracted. This is quicker than comparing the type with `Never`,
/// which compares each member of a union, even when they're the same type.
fn is_empty(ctx: &Compiler, ty: TypeId) -> bool {
    is_empty_inner(ctx, ty, &mut HashSet::new())
}

fn is_empty_inner(ctx: &Compiler, ty: TypeId, visited: &mut HashSet<TypeId>) -> bool {
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
        _ => false,
    }
}
