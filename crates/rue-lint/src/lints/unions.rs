use std::collections::HashSet;

use rue_diagnostic::{Applicability, Lint, Note, SrcLoc, Suggestion};
use rue_types::{Comparison, TypeId};

use crate::{LintContext, LintInput, WrittenUnion, srcloc_key};

pub(crate) const LINTS: &[Lint] = &[
    Lint::RedundantUnionMember,
    Lint::IndistinguishableUnionMember,
];

/// How a member of a union overlaps with another member.
enum Overlap {
    /// Every value of the member is already a value of the other one, so the union is the same
    /// without it.
    Redundant { other: usize, same: bool },
    /// The member and the other one are different types, but they're the same at runtime, so a
    /// type guard or `match` can't tell them apart. A member that's only partly the same at
    /// runtime as another one, such as `nil` in `Int | nil`, is narrowed by a runtime check
    /// instead, so it isn't reported.
    Indistinguishable { other: usize },
}

/// Reports the members of union types written in the source that overlap with another member of
/// the same union. Only the unions that are written are checked, since those are the ones that
/// can be changed.
pub(crate) fn check(cx: &mut LintContext<'_>, input: &LintInput<'_>) {
    let mut seen = HashSet::new();

    for union in &input.sites.written_unions {
        // The same union can be compiled more than once, such as for each call of an inline
        // function
        let Some((first, _)) = union.members.first() else {
            continue;
        };

        if !seen.insert((srcloc_key(first), first.span.end)) {
            continue;
        }

        check_union(cx, union);
    }
}

fn check_union(cx: &mut LintContext<'_>, union: &WrittenUnion) {
    let members = &union.members;

    // An error has already been reported for an unresolved member, which is compatible with
    // everything
    let resolved: Vec<bool> = members
        .iter()
        .map(|&(_, ty)| !cx.contains_unresolved(ty))
        .collect();

    let types: Vec<TypeId> = members.iter().map(|&(_, ty)| ty).collect();

    let mut overlaps = Vec::with_capacity(members.len());

    for index in 0..members.len() {
        overlaps.push(if resolved[index] {
            overlap(cx, &types, &resolved, index)
        } else {
            None
        });
    }

    for (index, overlap) in overlaps.iter().enumerate() {
        let Some(overlap) = overlap else {
            continue;
        };

        let member = &members[index].0;

        match *overlap {
            Overlap::Redundant { other, same } => {
                let other_srcloc = &members[other].0;
                let (name, other_name) = (member.text(), other_srcloc.text());

                let message = if same {
                    format!(
                        "Redundant union member `{name}`, since it's the same type as `{other_name}`, which is already in the union"
                    )
                } else {
                    format!(
                        "Redundant union member `{name}`, since `{other_name}` is already in the union, and it includes every `{name}`"
                    )
                };

                let removal = removal(members, &overlaps, index);

                let Some(diagnostic) =
                    cx.report(Lint::RedundantUnionMember, member.clone(), message)
                else {
                    continue;
                };

                diagnostic.notes.push(Note::at(
                    other_srcloc.clone(),
                    format!("`{other_name}` is in the union here"),
                ));

                diagnostic.suggestions.push(Suggestion::replace(
                    format!("Remove `{name}` from the union"),
                    removal,
                    "",
                    Applicability::MachineApplicable,
                ));
            }
            Overlap::Indistinguishable { other } => {
                let other_srcloc = &members[other].0;
                let (name, other_name) = (member.text(), other_srcloc.text());

                let Some(diagnostic) = cx.report(
                    Lint::IndistinguishableUnionMember,
                    member.clone(),
                    format!(
                        "Union member `{name}` can't be told apart from `{other_name}`, since they're the same at runtime, so a type guard or `match` can't narrow the union to either of them"
                    ),
                ) else {
                    continue;
                };

                diagnostic.notes.push(Note::at(
                    other_srcloc.clone(),
                    format!("`{other_name}` is in the union here"),
                ));
            }
        }
    }
}

/// How the member at the index overlaps with the other members of the union, if it does.
fn overlap(
    cx: &mut LintContext<'_>,
    types: &[TypeId],
    resolved: &[bool],
    index: usize,
) -> Option<Overlap> {
    let ty = types[index];
    let mut indistinguishable = None;

    for (other, &other_ty) in types.iter().enumerate() {
        if other == index || !resolved[other] {
            continue;
        }

        match cx.compare(ty, other_ty) {
            Comparison::Assign => {
                let same = cx.is_assignable(other_ty, ty);

                // Of two members that are the same type, the later one is redundant
                if same && other > index {
                    continue;
                }

                return Some(Overlap::Redundant { other, same });
            }
            // Of two members that are the same at runtime, the later one is reported. If the
            // other member is assignable to this one, it's the redundant one instead.
            Comparison::Cast
                if other < index
                    && indistinguishable.is_none()
                    && cx.compare(other_ty, ty) == Comparison::Cast =>
            {
                indistinguishable = Some(Overlap::Indistinguishable { other });
            }
            Comparison::Cast | Comparison::Invalid => {}
        }
    }

    indistinguishable
}

/// The code to remove to take the member at the index out of the union, along with one of the
/// `|` separators next to it. This is the separator before it, unless every member before it is
/// removed too, so that removing each of the redundant members gives a valid union between them.
fn removal(members: &[(SrcLoc, TypeId)], overlaps: &[Option<Overlap>], index: usize) -> SrcLoc {
    let member = &members[index].0;

    let all_before_removed = overlaps[..index]
        .iter()
        .all(|overlap| matches!(overlap, Some(Overlap::Redundant { .. })));

    // At least one member isn't redundant, so if every member up to this one is, there's another
    // member after it
    let span = if all_before_removed {
        member.span.start..members[index + 1].0.span.start
    } else {
        members[index - 1].0.span.end..member.span.end
    };

    SrcLoc::new(member.source.clone(), span)
}
