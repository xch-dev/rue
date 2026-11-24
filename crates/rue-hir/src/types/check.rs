use rue_types::{AtomRestriction, Check, CheckPlan};

use crate::{
    BinaryOp, Database, FieldAccess, FieldKind, FunctionCall, Hir, HirId, TypePath, UnaryOp,
};

/// Generates the HIR that checks whether the value matches a type at runtime. This is done when
/// a [`Hir::Guard`] is lowered. A check that doesn't use the value is a constant, so the guard's
/// value is evaluated separately for its effect if it may fail.
///
/// A check that calls helpers (see [`CheckPlan`]) is given the tree of their programs, and the
/// path to each in it. The tree is passed on to each helper along with the part of the value it
/// checks.
pub fn generate_check_hir(
    db: &mut Database,
    check: Check,
    hir: HirId,
    helpers: Option<&CheckHelpers>,
) -> HirId {
    match check {
        Check::None => db.alloc_hir(Hir::Bool(true)),
        Check::Impossible => db.alloc_hir(Hir::Bool(false)),
        Check::Pair(first, rest) => {
            let first_hir = db.alloc_hir(Hir::Unary(UnaryOp::First, hir));
            let first = (first.as_ref() != &Check::None)
                .then_some(generate_check_hir(db, *first, first_hir, helpers));

            let rest_hir = db.alloc_hir(Hir::Unary(UnaryOp::Rest, hir));
            let rest = (rest.as_ref() != &Check::None)
                .then_some(generate_check_hir(db, *rest, rest_hir, helpers));

            match (first, rest) {
                (Some(first), Some(rest)) => db.alloc_hir(Hir::Binary(BinaryOp::And, first, rest)),
                (Some(first), None) => first,
                (None, Some(rest)) => rest,
                (None, None) => db.alloc_hir(Hir::Bool(true)),
            }
        }
        // If the only atom the value can be is nil, it's checked the same way Chialisp checks
        // whether a list has ended, by whether it's nil. This is also cheaper in conditions. A
        // value that's some other atom is treated as a pair, so reading from it fails, the same
        // way in debug and release builds.
        Check::IsAtom {
            can_be_truthy: false,
        } => db.alloc_hir(Hir::Unary(UnaryOp::Not, hir)),
        Check::IsPair {
            can_be_truthy: false,
        } => {
            let not = db.alloc_hir(Hir::Unary(UnaryOp::Not, hir));
            db.alloc_hir(Hir::Unary(UnaryOp::Not, not))
        }
        Check::IsAtom {
            can_be_truthy: true,
        } => {
            let listp = db.alloc_hir(Hir::Unary(UnaryOp::Listp, hir));
            db.alloc_hir(Hir::Unary(UnaryOp::Not, listp))
        }
        Check::IsPair {
            can_be_truthy: true,
        } => db.alloc_hir(Hir::Unary(UnaryOp::Listp, hir)),
        Check::Atom(AtomRestriction::Length(length)) => {
            let strlen = db.alloc_hir(Hir::Unary(UnaryOp::Strlen, hir));
            let length = db.alloc_hir(Hir::Int(length.into()));
            db.alloc_hir(Hir::Binary(BinaryOp::Eq, strlen, length))
        }
        Check::Atom(AtomRestriction::Value(value)) => {
            let value = db.alloc_hir(Hir::Bytes(value.to_vec()));
            db.alloc_hir(Hir::Binary(BinaryOp::Eq, hir, value))
        }
        Check::And(checks) => {
            let mut checks = checks
                .into_iter()
                .map(|check| generate_check_hir(db, check, hir, helpers))
                .collect::<Vec<_>>();

            // Fold from the right so that the checks are evaluated in order, since later checks
            // can depend on earlier ones (for example, `strlen` must only run on atoms)
            let Some(mut result) = checks.pop() else {
                return db.alloc_hir(Hir::Bool(true));
            };

            while let Some(check) = checks.pop() {
                result = db.alloc_hir(Hir::Binary(BinaryOp::And, check, result));
            }

            result
        }
        Check::Or(checks) => {
            let mut checks = checks
                .into_iter()
                .map(|check| generate_check_hir(db, check, hir, helpers))
                .collect::<Vec<_>>();

            // Fold from the right so that the checks are evaluated in order, since later checks
            // can depend on earlier ones (for example, `strlen` must only run on atoms)
            let Some(mut result) = checks.pop() else {
                return db.alloc_hir(Hir::Bool(false));
            };

            while let Some(check) = checks.pop() {
                result = db.alloc_hir(Hir::Binary(BinaryOp::Or, check, result));
            }

            result
        }
        // Runs `(a helper (c helpers value))`, so the helper can call the others
        Check::Helper(index) => {
            let helpers = helpers.expect("a check that calls helpers is given them");
            let helper = db.alloc_hir(Hir::FieldAccess(FieldAccess {
                value: helpers.tree,
                name: format!("helper {index}"),
                kind: FieldKind::Path(helpers.paths[index].clone()),
            }));
            db.alloc_hir(Hir::FunctionCall(FunctionCall {
                function: helper,
                args: vec![helpers.tree, hir],
                nil_terminated: false,
            }))
        }
        Check::Shared(_) => unreachable!("a planned check has no shared parts"),
    }
}

/// The helpers that a check calls (see [`generate_check_hir`]).
#[derive(Debug, Clone)]
pub struct CheckHelpers {
    /// The tree of the helpers' programs.
    pub tree: HirId,
    /// The path to each helper in the tree.
    pub paths: Vec<Vec<TypePath>>,
}

/// The number of times each helper is called by the check, added to the counts.
pub fn count_helper_calls(check: &Check, counts: &mut [usize]) {
    match check {
        Check::None
        | Check::Impossible
        | Check::IsAtom { .. }
        | Check::IsPair { .. }
        | Check::Atom(_) => {}
        Check::Helper(index) => counts[*index] += 1,
        Check::Pair(first, rest) => {
            count_helper_calls(first, counts);
            count_helper_calls(rest, counts);
        }
        Check::And(checks) | Check::Or(checks) => {
            for check in checks {
                count_helper_calls(check, counts);
            }
        }
        Check::Shared(_) => unreachable!("a planned check has no shared parts"),
    }
}

/// The number of times the HIR generated by [`generate_check_hir`] uses the value being checked.
/// Analyses that run before lowering (such as the dependency graph) use this to treat a
/// [`Hir::Guard`] exactly like the check it's lowered to, without generating it. A value that's
/// used no times is evaluated for its effect if it may fail (see
/// [`crate::DependencyGraph::is_guard_effect`]). A check with helpers is run as a program of its
/// own, which the value is passed to once.
pub fn check_value_uses(plan: &CheckPlan) -> usize {
    if plan.helpers.is_empty() {
        check_tree_value_uses(&plan.check)
    } else {
        1
    }
}

fn check_tree_value_uses(check: &Check) -> usize {
    match check {
        Check::None | Check::Impossible => 0,
        Check::Pair(first, rest) => check_tree_value_uses(first) + check_tree_value_uses(rest),
        Check::IsAtom { .. } | Check::IsPair { .. } | Check::Atom(_) | Check::Helper(_) => 1,
        Check::And(checks) | Check::Or(checks) => checks.iter().map(check_tree_value_uses).sum(),
        Check::Shared(_) => unreachable!("a planned check has no shared parts"),
    }
}

#[cfg(test)]
mod tests {
    use std::borrow::Cow;

    use super::*;

    /// Counts how many times the value appears in the generated HIR, as a tree.
    fn count_uses(db: &Database, hir: HirId, value: HirId) -> usize {
        if hir == value {
            return 1;
        }

        match db.hir(hir) {
            Hir::Unary(_, hir) => count_uses(db, *hir, value),
            Hir::Binary(_, left, right) => {
                count_uses(db, *left, value) + count_uses(db, *right, value)
            }
            _ => 0,
        }
    }

    #[test]
    fn test_check_value_uses() {
        let atom = || Check::IsAtom {
            can_be_truthy: true,
        };
        let pair = || Check::IsPair {
            can_be_truthy: false,
        };

        let checks = [
            Check::None,
            Check::Impossible,
            atom(),
            pair(),
            Check::Atom(AtomRestriction::Length(32)),
            Check::Atom(AtomRestriction::Value(Cow::Owned(vec![1]))),
            Check::Pair(Box::new(atom()), Box::new(Check::None)),
            Check::Pair(Box::new(Check::None), Box::new(Check::None)),
            Check::Pair(
                Box::new(Check::And(vec![
                    atom(),
                    Check::Atom(AtomRestriction::Length(32)),
                ])),
                Box::new(Check::Or(vec![pair(), Check::None])),
            ),
            Check::And(vec![]),
            Check::Or(vec![]),
        ];

        for check in checks {
            let mut db = Database::new();
            let value = db.alloc_hir(Hir::Unresolved);
            let hir = generate_check_hir(&mut db, check.clone(), value, None);
            assert_eq!(
                check_value_uses(&check.plan()),
                count_uses(&db, hir, value),
                "{check:?}"
            );
        }
    }
}
