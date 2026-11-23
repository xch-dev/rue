use rue_diagnostic::{Applicability, Lint, Note, SrcLoc, Suggestion};
use rue_types::Comparison;

use crate::{LintContext, LintInput, UnionMember};

pub(crate) const LINTS: &[Lint] = &[
    Lint::RedundantUnionMember,
    Lint::IndistinguishableUnionMember,
];

/// How a member of a union overlaps with another member.
#[derive(Debug, Clone, Copy)]
struct Overlap {
    /// The index of the other member.
    other: usize,
    kind: OverlapKind,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum OverlapKind {
    /// Every value of the member is already a value of the other one, so the union is the same
    /// without it.
    Redundant,
    /// The member is the same type as the other one, which is earlier in the union.
    Same,
    /// The member and the other one are different types, but they're the same at runtime, so a
    /// type guard or `match` can't tell them apart. A member that's only partly the same at
    /// runtime as another one, such as `nil` in `Int | nil`, is narrowed by a runtime check
    /// instead, so it isn't reported.
    Indistinguishable,
}

impl OverlapKind {
    fn is_redundant(self) -> bool {
        matches!(self, Self::Redundant | Self::Same)
    }
}

/// Reports the members of union types written in the source that overlap with another member of
/// the same union. Only the unions that are written are checked, since those are the ones that
/// can be changed.
pub(crate) fn check(cx: &mut LintContext<'_>, input: &LintInput<'_>) {
    for union in &input.sites.written_unions {
        check_union(cx, &union.members);
    }
}

fn check_union(cx: &mut LintContext<'_>, members: &[UnionMember]) {
    let Some(first) = members.first() else {
        return;
    };

    // Comparing every pair of members is skipped where neither lint would be reported
    if !LINTS
        .iter()
        .any(|&lint| cx.is_enabled_at(lint, &first.srcloc))
    {
        return;
    }

    // An error has already been reported for an unresolved member, which is compatible with
    // everything, so it's left out
    let resolved: Vec<bool> = members
        .iter()
        .map(|member| !cx.contains_unresolved(member.ty))
        .collect();

    let mut overlaps: Vec<Option<Overlap>> = (0..members.len())
        .map(|index| redundancy(cx, members, &resolved, index))
        .collect();

    for index in 0..members.len() {
        if overlaps[index].is_none() {
            overlaps[index] = indistinguishability(cx, members, &resolved, &overlaps, index);
        }
    }

    for (index, overlap) in overlaps.iter().enumerate() {
        let Some(Overlap { other, kind }) = *overlap else {
            continue;
        };

        let member = &members[index];
        let other = &members[other].srcloc;
        let (name, other_name) = (member.srcloc.text(), other.text());

        let (lint, message) = match kind {
            OverlapKind::Redundant => (
                Lint::RedundantUnionMember,
                format!(
                    "Redundant union member `{name}`, since `{other_name}` is already in the union, and it includes every `{name}`"
                ),
            ),
            OverlapKind::Same => (
                Lint::RedundantUnionMember,
                format!(
                    "Redundant union member `{name}`, since it's the same type as `{other_name}`, which is already in the union"
                ),
            ),
            OverlapKind::Indistinguishable => (
                Lint::IndistinguishableUnionMember,
                format!(
                    "Union member `{name}` can't be told apart from `{other_name}`, since they're the same at runtime, so a type guard or `match` can't narrow the union to either of them"
                ),
            ),
        };

        let suggestion = kind.is_redundant().then(|| {
            // A generic can be inferred from the member, even though its values are already in
            // the union, so the call sites might not compile without it. A copy of a member that's
            // kept references the same generics.
            let applicability =
                if kind == OverlapKind::Redundant && cx.references_generics(member.ty) {
                    Applicability::MaybeIncorrect
                } else {
                    Applicability::MachineApplicable
                };

            Suggestion::replace(
                format!("Remove `{name}` from the union"),
                removal(members, &overlaps, index),
                "",
                applicability,
            )
        });

        let Some(diagnostic) = cx.report(lint, member.srcloc.clone(), message) else {
            continue;
        };

        diagnostic.notes.push(Note::at(
            other.clone(),
            format!("`{other_name}` is in the union here"),
        ));

        diagnostic.suggestions.extend(suggestion);
    }
}

/// The member that the member at the index is redundant with, if there is one.
fn redundancy(
    cx: &mut LintContext<'_>,
    members: &[UnionMember],
    resolved: &[bool],
    index: usize,
) -> Option<Overlap> {
    if !resolved[index] {
        return None;
    }

    let ty = members[index].ty;

    for (other, member) in members.iter().enumerate() {
        if other == index || !resolved[other] || !cx.is_assignable(ty, member.ty) {
            continue;
        }

        if !cx.is_assignable(member.ty, ty) {
            return Some(Overlap {
                other,
                kind: OverlapKind::Redundant,
            });
        }

        // Of two members that are the same type, the later one is redundant
        if other < index {
            return Some(Overlap {
                other,
                kind: OverlapKind::Same,
            });
        }
    }

    None
}

/// The earlier member that the member at the index is the same as at runtime, if there is one.
/// Members that are redundant aren't compared, since they're removed from the union, so a member
/// that's only partly the same at runtime as what's left isn't reported (such as `nil` in
/// `false | Bool | nil`).
fn indistinguishability(
    cx: &mut LintContext<'_>,
    members: &[UnionMember],
    resolved: &[bool],
    overlaps: &[Option<Overlap>],
    index: usize,
) -> Option<Overlap> {
    if !resolved[index] {
        return None;
    }

    let ty = members[index].ty;

    (0..index)
        .find(|&other| {
            resolved[other]
                && !overlaps[other].is_some_and(|overlap| overlap.kind.is_redundant())
                && cx.compare(ty, members[other].ty) == Comparison::Cast
                && cx.compare(members[other].ty, ty) == Comparison::Cast
        })
        .map(|other| Overlap {
            other,
            kind: OverlapKind::Indistinguishable,
        })
}

/// The code to remove to take the member at the index out of the union, along with one of the
/// `|` separators next to it. This is the separator before it, unless every member before it is
/// removed too, so that removing each of the redundant members gives a valid union between them.
fn removal(members: &[UnionMember], overlaps: &[Option<Overlap>], index: usize) -> SrcLoc {
    let member = &members[index].srcloc;

    let all_before_removed = overlaps[..index]
        .iter()
        .all(|overlap| overlap.is_some_and(|overlap| overlap.kind.is_redundant()));

    // At least one member isn't redundant, so if every member up to this one is, there's another
    // member after it
    let span = if all_before_removed {
        member.span.start..members[index + 1].srcloc.span.start
    } else {
        members[index - 1].srcloc.span.end..member.span.end
    };

    SrcLoc::new(member.source.clone(), span)
}
