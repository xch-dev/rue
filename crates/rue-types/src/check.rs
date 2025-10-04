use std::cmp::max;

use id_arena::Arena;
use indexmap::{IndexMap, IndexSet, indexset};
use log::trace;
use thiserror::Error;

use crate::{
    AtomRestriction, Atoms, BuiltinTypes, Comparison, ComparisonContext, Pair, Type, TypeId,
    compare_impl, stringify_impl, substitute,
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
                Check::Pair(Box::new(first), Box::new(rest))
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
            Check::Or(checks) => Check::Or(checks.into_iter().map(Check::simplify).collect()),
        }
    }
}

#[derive(Debug, Clone, Copy, Error)]
pub enum CheckError {
    #[error("Maximum type check depth reached")]
    DepthExceeded,

    #[error("Cannot check if value is of function type at runtime")]
    FunctionType,
}

#[derive(Debug)]
struct CheckContext {
    depth: usize,
}

pub fn check(
    arena: &mut Arena<Type>,
    builtins: &BuiltinTypes,
    lhs: TypeId,
    rhs: TypeId,
) -> Result<Check, CheckError> {
    let lhs = substitute(arena, lhs);
    let rhs = substitute(arena, rhs);
    let lhs_name = stringify_impl(arena, lhs, &mut IndexMap::new());
    let rhs_name = stringify_impl(arena, rhs, &mut IndexMap::new());
    trace!("Checking {lhs_name} to {rhs_name}");
    let result = check_impl(arena, builtins, &mut CheckContext { depth: 0 }, lhs, rhs);
    trace!("Check from {lhs_name} to {rhs_name} yielded {result:?}");
    result
}

fn check_impl(
    arena: &Arena<Type>,
    builtins: &BuiltinTypes,
    ctx: &mut CheckContext,
    lhs: TypeId,
    rhs: TypeId,
) -> Result<Check, CheckError> {
    let variants = variants_of(arena, builtins, lhs);
    check_each(arena, builtins, ctx, &variants, rhs)
}

fn check_each(
    arena: &Arena<Type>,
    builtins: &BuiltinTypes,
    ctx: &mut CheckContext,
    lhs: &[TypeId],
    rhs: TypeId,
) -> Result<Check, CheckError> {
    ctx.depth += 1;

    if ctx.depth > 25 {
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

    let target_atoms = atoms_of(arena, rhs)?;

    let mut overlap = IndexSet::new();
    let mut exceeds_overlap = false;
    let mut lhs_has_atom = false;
    let mut can_be_truthy = false;

    for &id in lhs {
        let atoms = atoms_of(arena, id)?;

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
                        AtomRestriction::Length(_) => {}
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

    let target_pairs = pairs_of(arena, builtins, rhs)?;

    let mut checks = Vec::new();
    let mut requires_check = false;

    let mut pairs = Vec::new();

    for &lhs in lhs {
        for pair in pairs_of(arena, builtins, lhs)? {
            pairs.push(pair);
        }
    }

    for target_pair in &target_pairs {
        let pairs = pairs.clone();

        let firsts: Vec<TypeId> = pairs.iter().map(|pair| pair.first).collect();
        let first = check_each(arena, builtins, ctx, &firsts, target_pair.first)?;

        if first == Check::Impossible {
            requires_check = true;
            continue;
        }

        let mut rests = Vec::new();

        // TODO: We can do this in inverse (rest then first) as well and see which check is simpler
        for pair in pairs {
            if compare_impl(
                arena,
                builtins,
                &mut ComparisonContext::default(),
                target_pair.first,
                pair.first,
                None,
                None,
            ) > Comparison::Cast
            {
                continue;
            }

            rests.push(pair.rest);
        }

        if rests.is_empty() {
            requires_check = true;
            continue;
        }

        let rest = check_each(arena, builtins, ctx, &rests, target_pair.rest)?;

        if rest == Check::Impossible {
            requires_check = true;
            continue;
        }

        if first == Check::None && rest == Check::None {
            // TODO: Should this set requires_check to false and break?
            continue;
        }

        requires_check = true;

        checks.push(Check::Pair(Box::new(first), Box::new(rest)));
    }

    let pair_result = (!pairs.is_empty()).then(|| {
        if target_pairs.is_empty() {
            Check::Impossible
        } else if !requires_check {
            Check::None
        } else if checks.is_empty() {
            Check::Impossible
        } else if checks.len() == 1 {
            checks[0].clone()
        } else {
            Check::Or(checks)
        }
    });

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

fn atoms_of(arena: &Arena<Type>, id: TypeId) -> Result<Option<Atoms>, CheckError> {
    Ok(match arena[id].clone() {
        Type::Apply(_) => unreachable!(),
        Type::Ref(id) => atoms_of(arena, id)?,
        Type::Unresolved | Type::Never | Type::Pair(_) => None,
        Type::Generic(_) | Type::Any => Some(Atoms::Unrestricted),
        Type::Atom(atom) => {
            let Some(restriction) = atom.restriction else {
                return Ok(Some(Atoms::Unrestricted));
            };
            Some(Atoms::Restricted(indexset![restriction]))
        }
        Type::Alias(alias) => atoms_of(arena, alias.inner)?,
        Type::Struct(ty) => atoms_of(arena, ty.inner)?,
        Type::Function(_) => return Err(CheckError::FunctionType),
        Type::Union(ty) => {
            let mut restrictions = IndexSet::new();

            for variant in ty.types {
                match atoms_of(arena, variant)? {
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

fn pairs_of(
    arena: &Arena<Type>,
    builtins: &BuiltinTypes,
    id: TypeId,
) -> Result<Vec<Pair>, CheckError> {
    Ok(match arena[id].clone() {
        Type::Apply(_) => unreachable!(),
        Type::Ref(id) => pairs_of(arena, builtins, id)?,
        Type::Unresolved | Type::Never | Type::Atom(_) => vec![],
        Type::Pair(pair) => vec![pair],
        Type::Generic(_) | Type::Any => {
            vec![Pair::new(builtins.recursive_any, builtins.recursive_any)]
        }
        Type::Alias(alias) => pairs_of(arena, builtins, alias.inner)?,
        Type::Struct(ty) => pairs_of(arena, builtins, ty.inner)?,
        Type::Function(_) => return Err(CheckError::FunctionType),
        Type::Union(ty) => {
            let mut pairs = Vec::new();

            for variant in ty.types {
                pairs.extend(pairs_of(arena, builtins, variant)?);
            }

            pairs
        }
    })
}
