use std::cmp::max;

use id_arena::Arena;
use indexmap::{IndexMap, IndexSet, indexset};
use log::{Level, log_enabled, trace};
use thiserror::Error;

use crate::{
    AtomRestriction, Atoms, BuiltinTypes, Comparison, ComparisonContext, Memo, Pair, Type, TypeId,
    compare_impl, reaches_non_contractive_cycle, stringify_impl, substitute,
};

#[derive(Debug, Clone, PartialEq, Eq)]
#[must_use]
pub enum Check {
    None,
    Impossible,
    IsAtom { can_be_truthy: bool },
    IsPair { can_be_truthy: bool },
    Pair(Box<Check>, Box<Check>),
    Atom(AtomRestriction),
    And(Vec<Check>),
    Or(Vec<Check>),
}

impl Check {
    pub fn simplify(self) -> Check {
        match self {
            Check::None => Check::None,
            Check::Impossible => Check::Impossible,
            Check::Pair(first, rest) => {
                let first = first.simplify();
                let rest = rest.simplify();

                // Pair checks are only ever evaluated against values that are known to be pairs
                if first == Check::Impossible || rest == Check::Impossible {
                    Check::Impossible
                } else if first == Check::None && rest == Check::None {
                    Check::None
                } else {
                    Check::Pair(Box::new(first), Box::new(rest))
                }
            }
            Check::IsAtom { can_be_truthy } => Check::IsAtom { can_be_truthy },
            Check::IsPair { can_be_truthy } => Check::IsPair { can_be_truthy },
            Check::Atom(restriction) => Check::Atom(restriction),
            Check::And(checks) => {
                let mut flattened = Vec::new();

                for check in checks {
                    match check.simplify() {
                        Check::None => {}
                        Check::Impossible => {
                            return Check::Impossible;
                        }
                        Check::And(inner) => {
                            flattened.extend(inner);
                        }
                        check => {
                            flattened.push(check);
                        }
                    }
                }

                let mut listp = None;
                let mut length = None;
                let mut value = None;
                let mut result = Vec::new();

                for check in flattened {
                    match check {
                        Check::None | Check::Impossible | Check::And(_) => unreachable!(),
                        Check::IsAtom { can_be_truthy } => {
                            if matches!(listp, Some(Check::IsPair { .. })) {
                                return Check::Impossible;
                            }
                            listp = Some(Check::IsAtom { can_be_truthy });
                        }
                        Check::IsPair { can_be_truthy } => {
                            if matches!(listp, Some(Check::IsAtom { .. })) {
                                return Check::Impossible;
                            }
                            listp = Some(Check::IsPair { can_be_truthy });
                        }
                        Check::Atom(AtomRestriction::Length(check)) => {
                            if length.is_some_and(|length| length != check) {
                                return Check::Impossible;
                            }
                            length = Some(check);
                        }
                        Check::Atom(AtomRestriction::Value(check)) => {
                            if value.is_some_and(|value| value != check) {
                                return Check::Impossible;
                            }
                            value = Some(check);
                        }
                        check @ (Check::Or(_) | Check::Pair(..)) => {
                            result.push(check);
                        }
                    }
                }

                match (length, value) {
                    (Some(length), Some(value)) => {
                        if length != value.len() {
                            return Check::Impossible;
                        }
                        result.insert(0, Check::Atom(AtomRestriction::Value(value)));
                    }
                    (None, Some(value)) => {
                        result.insert(0, Check::Atom(AtomRestriction::Value(value)));
                    }
                    (Some(length), None) => {
                        result.insert(0, Check::Atom(AtomRestriction::Length(length)));
                    }
                    (None, None) => {}
                }

                if let Some(listp) = listp {
                    result.insert(0, listp);
                }

                if result.is_empty() {
                    Check::None
                } else if result.len() == 1 {
                    result[0].clone()
                } else {
                    Check::And(result)
                }
            }
            Check::Or(checks) => {
                let mut result = Vec::new();

                for check in checks {
                    match check.simplify() {
                        Check::None => {
                            return Check::None;
                        }
                        Check::Impossible => {}
                        Check::Or(inner) => {
                            for check in inner {
                                if !result.contains(&check) {
                                    result.push(check);
                                }
                            }
                        }
                        check => {
                            if !result.contains(&check) {
                                result.push(check);
                            }
                        }
                    }
                }

                if result.is_empty() {
                    Check::Impossible
                } else if result.len() == 1 {
                    result.remove(0)
                } else {
                    Check::Or(result)
                }
            }
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Error)]
pub enum CheckError {
    #[error("Maximum type check depth reached")]
    DepthExceeded,

    #[error("Cannot check if value is of function type at runtime")]
    FunctionType,

    #[error("Cannot check if value is of generic type at runtime")]
    GenericType,
}

pub fn check(
    arena: &mut Arena<Type>,
    builtins: &BuiltinTypes,
    lhs: TypeId,
    rhs: TypeId,
) -> Result<Check, CheckError> {
    let lhs = substitute(arena, lhs);
    let rhs = substitute(arena, rhs);

    // These should be rejected before reaching this point, but they would recurse forever
    if reaches_non_contractive_cycle(arena, lhs) || reaches_non_contractive_cycle(arena, rhs) {
        return Err(CheckError::DepthExceeded);
    }

    let names = log_enabled!(Level::Trace).then(|| {
        (
            stringify_impl(arena, lhs, &mut IndexMap::new()),
            stringify_impl(arena, rhs, &mut IndexMap::new()),
        )
    });

    if let Some((lhs_name, rhs_name)) = &names {
        trace!("Checking {lhs_name} to {rhs_name}");
    }

    let result = check_impl(arena, builtins, 0, lhs, rhs);

    if let Some((lhs_name, rhs_name)) = &names {
        trace!("Check from {lhs_name} to {rhs_name} yielded {result:?}");
    }

    result
}

fn check_impl(
    arena: &Arena<Type>,
    builtins: &BuiltinTypes,
    depth: usize,
    lhs: TypeId,
    rhs: TypeId,
) -> Result<Check, CheckError> {
    let variants = variants_of(arena, builtins, lhs);
    check_each(arena, builtins, depth, &variants, rhs)
}

fn check_each(
    arena: &Arena<Type>,
    builtins: &BuiltinTypes,
    depth: usize,
    lhs: &[TypeId],
    rhs: TypeId,
) -> Result<Check, CheckError> {
    let depth = depth + 1;

    if depth > 25 {
        return Err(CheckError::DepthExceeded);
    }

    let mut result = Comparison::Assign;

    for &id in lhs {
        result = max(
            result,
            compare_impl(
                arena,
                builtins,
                &mut ComparisonContext::default(),
                id,
                rhs,
                None,
                None,
            ),
        );
    }

    if result <= Comparison::Cast {
        return Ok(Check::None);
    }

    let target_atoms = atoms_of(arena, rhs, true)?;

    let mut overlap = IndexSet::new();
    let mut exceeds_overlap = false;
    let mut lhs_has_atom = false;
    let mut can_be_truthy = false;

    for &id in lhs {
        let atoms = atoms_of(arena, id, false)?;

        if let Some(atoms) = &atoms {
            lhs_has_atom = true;

            match atoms {
                Atoms::Unrestricted => {
                    can_be_truthy = true;
                }
                Atoms::Restricted(restrictions) => {
                    for restriction in restrictions {
                        match restriction {
                            AtomRestriction::Value(value) => {
                                if !value.is_empty() {
                                    can_be_truthy = true;
                                }
                            }
                            AtomRestriction::Length(length) => {
                                if *length > 0 {
                                    can_be_truthy = true;
                                }
                            }
                        }
                    }
                }
            }
        }

        match (atoms, &target_atoms) {
            (Some(_), None) => {}
            (Some(_), Some(Atoms::Unrestricted)) | (None, _) => {}
            (Some(Atoms::Unrestricted), Some(Atoms::Restricted(restrictions))) => {
                exceeds_overlap = true;

                for restriction in restrictions {
                    if let AtomRestriction::Value(value) = restriction
                        && overlap.contains(&AtomRestriction::Length(value.len()))
                    {
                        continue;
                    }
                    overlap.insert(restriction.clone());
                }
            }
            (
                Some(Atoms::Restricted(restrictions)),
                Some(Atoms::Restricted(target_restrictions)),
            ) => {
                for restriction in restrictions {
                    if target_restrictions.contains(&restriction) {
                        overlap.insert(restriction);
                        continue;
                    }

                    match restriction {
                        AtomRestriction::Value(value) => {
                            let length = AtomRestriction::Length(value.len());
                            if target_restrictions.contains(&length) {
                                overlap.insert(length);
                                continue;
                            }
                        }
                        AtomRestriction::Length(length) => {
                            // Some values of this length may still be in the target
                            for target_restriction in target_restrictions {
                                if let AtomRestriction::Value(value) = target_restriction
                                    && value.len() == length
                                {
                                    overlap.insert(target_restriction.clone());
                                }
                            }
                        }
                    }

                    exceeds_overlap = true;
                }
            }
        }
    }

    let atom_result = lhs_has_atom.then(|| {
        if target_atoms.is_none() {
            Check::Impossible
        } else if !exceeds_overlap {
            Check::None
        } else if overlap.is_empty() {
            Check::Impossible
        } else if overlap.len() == 1 {
            overlap.into_iter().next().map(Check::Atom).unwrap()
        } else {
            Check::Or(overlap.into_iter().map(Check::Atom).collect())
        }
    });

    let target_pairs = pairs_of(arena, builtins, rhs, true)?;

    let mut pairs = Vec::new();

    for &lhs in lhs {
        pairs.extend(pairs_of(arena, builtins, lhs, false)?);
    }

    let pair_result = if pairs.is_empty() {
        None
    } else {
        let firsts: Vec<TypeId> = pairs.iter().map(|pair| pair.first).collect();

        let mut checks = Vec::new();

        // For each target pair, the first check is exact for every possible first value. The rest
        // check only runs if the first check passes, so it must be exact for the rest of every pair
        // whose first could pass the first check (not just those whose first is assignable to the
        // target's first). Each check in the resulting union is therefore exact for every pair.
        for target_pair in &target_pairs {
            let first = check_each(arena, builtins, depth, &firsts, target_pair.first)?;

            if first == Check::Impossible {
                continue;
            }

            let rests: Vec<TypeId> = pairs
                .iter()
                .filter(|pair| {
                    may_overlap(
                        arena,
                        builtins,
                        pair.first,
                        target_pair.first,
                        &mut IndexSet::new(),
                        &mut Memo::default(),
                    )
                })
                .map(|pair| pair.rest)
                .collect();

            if rests.is_empty() {
                continue;
            }

            let rest = check_each(arena, builtins, depth, &rests, target_pair.rest)?;

            checks.push(Check::Pair(Box::new(first), Box::new(rest)));
        }

        // If any target pair accepts every pair, this simplifies to `Check::None`
        Some(Check::Or(checks).simplify())
    };

    let check = match (atom_result, pair_result) {
        (None, None) => Check::Impossible,
        (Some(atom), None) => atom,
        (None, Some(pair)) => pair,
        (Some(atom), Some(Check::Impossible)) => {
            Check::And(vec![Check::IsAtom { can_be_truthy }, atom])
        }
        (Some(Check::Impossible), Some(pair)) => {
            Check::And(vec![Check::IsPair { can_be_truthy }, pair])
        }
        (Some(atom), Some(Check::None)) => Check::Or(vec![Check::IsPair { can_be_truthy }, atom]),
        (Some(Check::None), Some(pair)) => Check::Or(vec![Check::IsAtom { can_be_truthy }, pair]),
        (Some(atom), Some(pair)) => Check::Or(vec![
            Check::And(vec![Check::IsAtom { can_be_truthy }, atom]),
            Check::And(vec![Check::IsPair { can_be_truthy }, pair]),
        ]),
    };

    Ok(check.simplify())
}

fn variants_of(arena: &Arena<Type>, builtins: &BuiltinTypes, id: TypeId) -> Vec<TypeId> {
    match arena[id].clone() {
        Type::Apply(_) => unreachable!(),
        Type::Ref(id) => variants_of(arena, builtins, id),
        Type::Unresolved | Type::Atom(_) | Type::Pair(_) | Type::Generic(_) => {
            vec![id]
        }
        Type::Never => vec![],
        Type::Alias(alias) => variants_of(arena, builtins, alias.inner),
        Type::Struct(ty) => variants_of(arena, builtins, ty.inner),
        Type::Function(_) | Type::Any => vec![builtins.atom, builtins.recursive_any_pair],
        Type::Union(ty) => {
            let mut variants = Vec::new();

            for variant in ty.types {
                variants.extend(variants_of(arena, builtins, variant));
            }

            variants
        }
    }
}

/// Returns the atoms that a type can contain. If `target` is true, the type is the one being checked
/// against, so generic types are an error since their values can't be known at runtime. Generic
/// targets are still allowed if the value is already known to be assignable (see `check_each`).
fn atoms_of(arena: &Arena<Type>, id: TypeId, target: bool) -> Result<Option<Atoms>, CheckError> {
    Ok(match arena[id].clone() {
        Type::Apply(_) => unreachable!(),
        Type::Ref(id) => atoms_of(arena, id, target)?,
        Type::Unresolved | Type::Never | Type::Pair(_) => None,
        Type::Generic(_) if target => return Err(CheckError::GenericType),
        Type::Generic(_) => Some(Atoms::Unrestricted),
        Type::Any => Some(Atoms::Unrestricted),
        Type::Atom(atom) => {
            let Some(restriction) = atom.restriction else {
                return Ok(Some(Atoms::Unrestricted));
            };
            Some(Atoms::Restricted(indexset![restriction]))
        }
        Type::Alias(alias) => atoms_of(arena, alias.inner, target)?,
        Type::Struct(ty) => atoms_of(arena, ty.inner, target)?,
        Type::Function(_) => return Err(CheckError::FunctionType),
        Type::Union(ty) => {
            let mut restrictions = IndexSet::new();

            for variant in ty.types {
                match atoms_of(arena, variant, target)? {
                    None => {}
                    Some(Atoms::Unrestricted) => return Ok(Some(Atoms::Unrestricted)),
                    Some(Atoms::Restricted(new)) => {
                        for restriction in new {
                            match &restriction {
                                AtomRestriction::Value(value) => {
                                    if restrictions.contains(&AtomRestriction::Length(value.len()))
                                    {
                                        continue;
                                    }
                                    restrictions.insert(restriction);
                                }
                                AtomRestriction::Length(length) => {
                                    restrictions.retain(|item| match item {
                                        AtomRestriction::Value(value) => value.len() != *length,
                                        AtomRestriction::Length(_) => true,
                                    });
                                    restrictions.insert(restriction);
                                }
                            }
                        }
                    }
                }
            }

            if restrictions.is_empty() {
                None
            } else {
                Some(Atoms::Restricted(restrictions))
            }
        }
    })
}

/// Returns the pairs that a type can contain. See [`atoms_of`] for the meaning of `target`.
fn pairs_of(
    arena: &Arena<Type>,
    builtins: &BuiltinTypes,
    id: TypeId,
    target: bool,
) -> Result<Vec<Pair>, CheckError> {
    Ok(match arena[id].clone() {
        Type::Apply(_) => unreachable!(),
        Type::Ref(id) => pairs_of(arena, builtins, id, target)?,
        Type::Unresolved | Type::Never | Type::Atom(_) => vec![],
        Type::Pair(pair) => vec![pair],
        Type::Generic(_) if target => return Err(CheckError::GenericType),
        Type::Generic(_) | Type::Any => {
            vec![Pair::new(builtins.recursive_any, builtins.recursive_any)]
        }
        Type::Alias(alias) => pairs_of(arena, builtins, alias.inner, target)?,
        Type::Struct(ty) => pairs_of(arena, builtins, ty.inner, target)?,
        Type::Function(_) => return Err(CheckError::FunctionType),
        Type::Union(ty) => {
            let mut pairs = Vec::new();

            for variant in ty.types {
                pairs.extend(pairs_of(arena, builtins, variant, target)?);
            }

            pairs
        }
    })
}

/// Conservatively determines whether two types may have any values in common. This must return
/// true if there is any overlap, but may also return true if there isn't.
fn may_overlap(
    arena: &Arena<Type>,
    builtins: &BuiltinTypes,
    lhs: TypeId,
    rhs: TypeId,
    stack: &mut IndexSet<(TypeId, TypeId)>,
    memo: &mut Memo<(TypeId, TypeId), (TypeId, TypeId, usize), bool>,
) -> bool {
    if stack.len() > 25 {
        return true;
    }

    if let Some(index) = stack.get_index_of(&(lhs, rhs)) {
        memo.reached(index);
        return true;
    }

    let item = (lhs, rhs);
    stack.insert(item);

    let rhs_variants = variants_of(arena, builtins, rhs);

    let result = variants_of(arena, builtins, lhs).into_iter().any(|lhs| {
        rhs_variants
            .iter()
            .any(|&rhs| match (&arena[lhs], &arena[rhs]) {
                (Type::Atom(lhs), Type::Atom(rhs)) => match (&lhs.restriction, &rhs.restriction) {
                    (None, _) | (_, None) => true,
                    (Some(AtomRestriction::Value(lhs)), Some(AtomRestriction::Value(rhs))) => {
                        lhs == rhs
                    }
                    (Some(AtomRestriction::Length(lhs)), Some(AtomRestriction::Length(rhs))) => {
                        lhs == rhs
                    }
                    (
                        Some(AtomRestriction::Value(value)),
                        Some(AtomRestriction::Length(length)),
                    )
                    | (
                        Some(AtomRestriction::Length(length)),
                        Some(AtomRestriction::Value(value)),
                    ) => value.len() == *length,
                },
                (Type::Pair(lhs_pair), Type::Pair(rhs_pair)) => {
                    // The pairs are cached rather than the types they came from, since each
                    // reference to a type (such as `T` in `(T, T)`) is a distinct alias of it.
                    // This depends on the depth, since overlap is assumed past a certain depth.
                    let key = (lhs, rhs, stack.len());

                    if let Some(result) = memo.get(&key, stack.iter().copied()) {
                        return result;
                    }

                    let frame = memo.enter(stack.len());
                    let result =
                        may_overlap(arena, builtins, lhs_pair.first, rhs_pair.first, stack, memo)
                            && may_overlap(
                                arena,
                                builtins,
                                lhs_pair.rest,
                                rhs_pair.rest,
                                stack,
                                memo,
                            );
                    memo.exit(frame, item, Some(key), result);

                    result
                }
                (Type::Atom(_), Type::Pair(_)) | (Type::Pair(_), Type::Atom(_)) => false,
                _ => true,
            })
    });

    stack.pop().unwrap();

    result
}
