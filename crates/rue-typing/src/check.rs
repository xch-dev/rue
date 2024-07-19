use std::{
    collections::{HashSet, VecDeque},
    fmt,
    hash::BuildHasher,
};

use crate::{Type, TypeId, TypeSystem};

#[derive(Debug, Clone, Copy)]
pub enum CheckError {
    Recursive(TypeId, TypeId),
    Impossible(TypeId, TypeId),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Check {
    None,
    IsPair,
    IsAtom,
    IsBool,
    IsNil,
    Length(usize),
    And(Vec<Check>),
    Or(Vec<Check>),
    If(Box<Check>, Box<Check>, Box<Check>),
    Pair(Box<Check>, Box<Check>),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum CheckPath {
    First,
    Rest,
}

impl fmt::Display for Check {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt_check(self, f, &mut Vec::new())
    }
}

/// Returns [`None`] for recursive checks.
pub(crate) fn check_type<S>(
    types: &TypeSystem,
    lhs: TypeId,
    rhs: TypeId,
    visited: &mut HashSet<(TypeId, TypeId), S>,
) -> Result<Check, CheckError>
where
    S: BuildHasher,
{
    if !visited.insert((lhs, rhs)) {
        return Err(CheckError::Recursive(lhs, rhs));
    }

    let check = match (types.get(lhs), types.get(rhs)) {
        (Type::Ref(..), _) | (_, Type::Ref(..)) => unreachable!(),

        (Type::Unknown, _) | (_, Type::Unknown) => Check::None,

        (Type::Bytes, Type::Bytes) => Check::None,
        (Type::Bytes32, Type::Bytes32) => Check::None,
        (Type::PublicKey, Type::PublicKey) => Check::None,
        (Type::Int, Type::Int) => Check::None,
        (Type::Bool, Type::Bool) => Check::None,
        (Type::Nil, Type::Nil) => Check::None,

        (Type::Bytes32, Type::Bytes) => Check::None,
        (Type::PublicKey, Type::Bytes) => Check::None,
        (Type::Int, Type::Bytes) => Check::None,
        (Type::Bool, Type::Bytes) => Check::None,
        (Type::Nil, Type::Bytes) => Check::None,

        (Type::Bytes32, Type::Int) => Check::None,
        (Type::PublicKey, Type::Int) => Check::None,
        (Type::Bytes, Type::Int) => Check::None,
        (Type::Bool, Type::Int) => Check::None,
        (Type::Nil, Type::Int) => Check::None,

        (Type::Nil, Type::Bool) => Check::None,

        (Type::Bytes, Type::Bool) => Check::IsBool,
        (Type::Bytes, Type::Nil) => Check::IsNil,
        (Type::Bytes, Type::PublicKey) => Check::Length(48),
        (Type::Bytes, Type::Bytes32) => Check::Length(32),

        (Type::Int, Type::Bool) => Check::IsBool,
        (Type::Int, Type::Nil) => Check::IsNil,
        (Type::Int, Type::PublicKey) => Check::Length(48),
        (Type::Int, Type::Bytes32) => Check::Length(32),

        (Type::Bool, Type::Nil) => Check::IsNil,

        (_, Type::Union(items)) => {
            let mut result = Vec::new();
            for item in items.clone() {
                result.push(check_type(types, lhs, item, visited)?);
            }
            Check::Or(result)
        }

        (Type::Union(items), _) => check_union_against_rhs(types, lhs, items, rhs, visited)?,

        (Type::PublicKey, Type::Bytes32) => return Err(CheckError::Impossible(lhs, rhs)),
        (Type::Bytes32, Type::PublicKey) => return Err(CheckError::Impossible(lhs, rhs)),
        (Type::Nil, Type::PublicKey) => return Err(CheckError::Impossible(lhs, rhs)),
        (Type::Nil, Type::Bytes32) => return Err(CheckError::Impossible(lhs, rhs)),
        (Type::PublicKey, Type::Nil) => return Err(CheckError::Impossible(lhs, rhs)),
        (Type::Bytes32, Type::Nil) => return Err(CheckError::Impossible(lhs, rhs)),
        (Type::Bool, Type::PublicKey) => return Err(CheckError::Impossible(lhs, rhs)),
        (Type::Bool, Type::Bytes32) => return Err(CheckError::Impossible(lhs, rhs)),
        (Type::PublicKey, Type::Bool) => return Err(CheckError::Impossible(lhs, rhs)),
        (Type::Bytes32, Type::Bool) => return Err(CheckError::Impossible(lhs, rhs)),

        (Type::Bytes, Type::Pair(..)) => return Err(CheckError::Impossible(lhs, rhs)),
        (Type::Bytes32, Type::Pair(..)) => return Err(CheckError::Impossible(lhs, rhs)),
        (Type::PublicKey, Type::Pair(..)) => return Err(CheckError::Impossible(lhs, rhs)),
        (Type::Int, Type::Pair(..)) => return Err(CheckError::Impossible(lhs, rhs)),
        (Type::Bool, Type::Pair(..)) => return Err(CheckError::Impossible(lhs, rhs)),
        (Type::Nil, Type::Pair(..)) => return Err(CheckError::Impossible(lhs, rhs)),

        (Type::Pair(..), Type::Bytes) => return Err(CheckError::Impossible(lhs, rhs)),
        (Type::Pair(..), Type::Bytes32) => return Err(CheckError::Impossible(lhs, rhs)),
        (Type::Pair(..), Type::PublicKey) => return Err(CheckError::Impossible(lhs, rhs)),
        (Type::Pair(..), Type::Int) => return Err(CheckError::Impossible(lhs, rhs)),
        (Type::Pair(..), Type::Bool) => return Err(CheckError::Impossible(lhs, rhs)),
        (Type::Pair(..), Type::Nil) => return Err(CheckError::Impossible(lhs, rhs)),

        (Type::Pair(lhs_first, lhs_rest), Type::Pair(rhs_first, rhs_rest)) => {
            let (lhs_first, lhs_rest) = (*lhs_first, *lhs_rest);
            let (rhs_first, rhs_rest) = (*rhs_first, *rhs_rest);
            let first = check_type(types, lhs_first, rhs_first, visited)?;
            let rest = check_type(types, lhs_rest, rhs_rest, visited)?;
            Check::Pair(Box::new(first), Box::new(rest))
        }
    };

    visited.remove(&(lhs, rhs));

    Ok(check)
}

fn check_union_against_rhs<S>(
    types: &TypeSystem,
    original_type_id: TypeId,
    items: &[TypeId],
    rhs: TypeId,
    visited: &mut HashSet<(TypeId, TypeId), S>,
) -> Result<Check, CheckError>
where
    S: BuildHasher,
{
    let mut atom_count = 0;
    let mut bool_count = 0;
    let mut nil_count = 0;
    let mut bytes32_count = 0;
    let mut public_key_count = 0;
    let mut pairs = Vec::new();

    let mut items: VecDeque<_> = items.iter().copied().collect::<VecDeque<_>>();
    let mut length = 0;

    while !items.is_empty() {
        let item = items.remove(0).unwrap();
        length += 1;

        if !visited.insert((item, rhs)) {
            return Err(CheckError::Recursive(item, rhs));
        }

        match types.get(item) {
            Type::Ref(..) => unreachable!(),
            Type::Union(child_items) => {
                items.extend(child_items);
            }
            Type::Unknown => {}
            Type::Bytes | Type::Int => {
                atom_count += 1;
            }
            Type::Bytes32 => {
                atom_count += 1;
                bytes32_count += 1;
            }
            Type::PublicKey => {
                atom_count += 1;
                public_key_count += 1;
            }
            Type::Bool => {
                atom_count += 1;
                bool_count += 1;
            }
            Type::Nil => {
                atom_count += 1;
                nil_count += 1;
                bool_count += 1;
            }
            Type::Pair(first, rest) => {
                pairs.push((*first, *rest));
            }
        }

        visited.remove(&(item, rhs));
    }

    let always_atom = atom_count == length;
    let always_pair = pairs.len() == length;
    let always_bool = bool_count == length;
    let always_nil = nil_count == length;
    let always_bytes32 = bytes32_count == length;
    let always_public_key = public_key_count == length;

    Ok(match types.get(rhs) {
        Type::Unknown => Check::None,
        Type::Ref(..) => unreachable!(),
        Type::Union(..) => unreachable!(),
        Type::Bytes if always_atom => Check::None,
        Type::Int if always_atom => Check::None,
        Type::Bool if always_bool => Check::None,
        Type::Nil if always_nil => Check::None,
        Type::Bytes32 if always_bytes32 => Check::None,
        Type::PublicKey if always_public_key => Check::None,
        Type::Bytes32 if always_atom => Check::Length(32),
        Type::PublicKey if always_atom => Check::Length(48),
        Type::Bool if always_atom => Check::IsBool,
        Type::Nil if always_atom => Check::IsNil,
        Type::Bytes => Check::IsAtom,
        Type::Int => Check::IsAtom,
        Type::Bytes32 => Check::And(vec![Check::IsAtom, Check::Length(32)]),
        Type::PublicKey => Check::And(vec![Check::IsAtom, Check::Length(48)]),
        Type::Bool => Check::And(vec![Check::IsAtom, Check::IsBool]),
        Type::Nil => Check::And(vec![Check::IsAtom, Check::IsNil]),
        Type::Pair(..) if always_atom => return Err(CheckError::Impossible(original_type_id, rhs)),
        Type::Pair(first, rest) => {
            let (first, rest) = (*first, *rest);

            let first_items: Vec<_> = pairs.iter().map(|(first, _)| *first).collect();
            let rest_items: Vec<_> = pairs.iter().map(|(_, rest)| *rest).collect();

            let first =
                check_union_against_rhs(types, original_type_id, &first_items, first, visited)?;
            let rest =
                check_union_against_rhs(types, original_type_id, &rest_items, rest, visited)?;

            let pair_check = Check::Pair(Box::new(first), Box::new(rest));

            if always_pair {
                pair_check
            } else {
                Check::And(vec![Check::IsPair, pair_check])
            }
        }
    })
}

fn fmt_val(f: &mut fmt::Formatter<'_>, path: &[CheckPath]) -> fmt::Result {
    for path in path.iter().rev() {
        match path {
            CheckPath::First => write!(f, "(f ")?,
            CheckPath::Rest => write!(f, "(r ")?,
        }
    }
    write!(f, "val")?;
    for _ in 0..path.len() {
        write!(f, ")")?;
    }
    Ok(())
}

fn fmt_check(check: &Check, f: &mut fmt::Formatter<'_>, path: &mut Vec<CheckPath>) -> fmt::Result {
    match check {
        Check::None => write!(f, "1"),
        Check::IsPair => {
            write!(f, "(l ")?;
            fmt_val(f, path)?;
            write!(f, ")")
        }
        Check::IsAtom => {
            write!(f, "(not (l ")?;
            fmt_val(f, path)?;
            write!(f, "))")
        }
        Check::IsBool => {
            write!(f, "(any (= ")?;
            fmt_val(f, path)?;
            write!(f, " ()) (= ")?;
            fmt_val(f, path)?;
            write!(f, " 1))")
        }
        Check::IsNil => {
            write!(f, "(= ")?;
            fmt_val(f, path)?;
            write!(f, " ())")
        }
        Check::Length(len) => {
            write!(f, "(= (strlen ")?;
            fmt_val(f, path)?;
            write!(f, ") {len})")
        }
        Check::And(checks) => {
            write!(f, "(and")?;
            for check in checks {
                write!(f, " ")?;
                fmt_check(check, f, path)?;
            }
            write!(f, ")")
        }
        Check::Or(checks) => {
            write!(f, "(or")?;
            for check in checks {
                write!(f, " ")?;
                fmt_check(check, f, path)?;
            }
            write!(f, ")")
        }
        Check::If(cond, then, else_) => {
            write!(f, "(if ")?;
            fmt_check(cond, f, path)?;
            write!(f, " ")?;
            fmt_check(then, f, path)?;
            write!(f, " ")?;
            fmt_check(else_, f, path)?;
            write!(f, ")")
        }
        Check::Pair(first, rest) => {
            let has_first = first.as_ref() != &Check::None;
            let has_rest = rest.as_ref() != &Check::None;

            if has_first && has_rest {
                write!(f, "(all ")?;
                path.push(CheckPath::First);
                fmt_check(first, f, path)?;
                path.pop().unwrap();
                write!(f, " ")?;
                path.push(CheckPath::Rest);
                fmt_check(rest, f, path)?;
                path.pop().unwrap();
                write!(f, ")")
            } else if has_first {
                path.push(CheckPath::First);
                fmt_check(first, f, path)?;
                path.pop().unwrap();
                Ok(())
            } else if has_rest {
                path.push(CheckPath::Rest);
                fmt_check(rest, f, path)?;
                path.pop().unwrap();
                Ok(())
            } else {
                write!(f, "1")
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::StandardTypes;

    use super::*;

    #[test]
    fn check_any_bytes32() {
        let mut db = TypeSystem::new();
        let types = StandardTypes::alloc(&mut db);
        assert_eq!(
            format!("{}", db.check(types.any, types.bytes32).unwrap()),
            "(and (not (l val)) (= (strlen val) 32))"
        );
    }

    #[test]
    fn check_any_int() {
        let mut db = TypeSystem::new();
        let types = StandardTypes::alloc(&mut db);
        assert_eq!(
            format!("{}", db.check(types.any, types.int).unwrap()),
            "(not (l val))"
        );
    }

    #[test]
    fn check_any_any() {
        let mut db = TypeSystem::new();
        let types = StandardTypes::alloc(&mut db);
        assert!(matches!(
            db.check(types.any, types.any).unwrap_err(),
            CheckError::Recursive(..)
        ));
    }

    #[test]
    fn check_optional_int() {
        let mut db = TypeSystem::new();
        let types = StandardTypes::alloc(&mut db);
        let pair = db.alloc(Type::Pair(types.int, types.int));
        let optional_pair = db.alloc(Type::Union(vec![pair, types.nil]));
        assert_eq!(
            format!("{}", db.check(optional_pair, types.nil).unwrap()),
            "(and (not (l val)) (= val ()))"
        );
        assert_eq!(
            format!("{}", db.check(optional_pair, pair).unwrap()),
            "(and (l val) 1)"
        );
    }

    #[test]
    fn check_any_pair_bytes32_pair_int_nil() {
        let mut db = TypeSystem::new();
        let types = StandardTypes::alloc(&mut db);

        let int_nil_pair = db.alloc(Type::Pair(types.int, types.nil));
        let ty = db.alloc(Type::Pair(types.bytes32, int_nil_pair));

        assert_eq!(
            format!("{}", db.check(types.any, ty).unwrap()),
            "(and (l val) (all (and (not (l (f val))) (= (strlen (f val)) 32)) (and (l (r val)) (all (not (l (f (r val)))) (and (not (l (r (r val)))) (= (r (r val)) ()))))))"
        );
    }

    #[test]
    fn check_complex_type_unions() {
        let mut db = TypeSystem::new();
        let types = StandardTypes::alloc(&mut db);

        let int_nil_pair = db.alloc(Type::Pair(types.int, types.nil));
        let bytes32_pair = db.alloc(Type::Pair(types.bytes32, types.bytes32));
        let complex_pair = db.alloc(Type::Pair(int_nil_pair, bytes32_pair));

        let lhs = db.alloc(Type::Union(vec![
            types.bytes32,
            types.public_key,
            types.nil,
            complex_pair,
            int_nil_pair,
            bytes32_pair,
            types.bool,
        ]));

        let rhs = db.alloc(Type::Union(vec![
            types.bytes32,
            bytes32_pair,
            types.nil,
            complex_pair,
        ]));

        assert_eq!(
            format!("{}", db.check(lhs, rhs).unwrap()),
            "(or (and (not (l val)) (= (strlen val) 32)) (and (l val) (all (and (not (l (f val))) (= (strlen (f val)) 32)) (and (not (l (r val))) (= (strlen (r val)) 32)))) (and (not (l val)) (= val ())) (and (l val) (all (and (l (f val)) 1) (and (l (r val)) 1))))"
        );
        assert_eq!(format!("{}", db.check(types.any, rhs).unwrap()),
            "(or (and (not (l val)) (= (strlen val) 32)) (and (l val) (all (and (not (l (f val))) (= (strlen (f val)) 32)) (and (not (l (r val))) (= (strlen (r val)) 32)))) (and (not (l val)) (= val ())) (and (l val) (all (and (l (f val)) (all (not (l (f (f val)))) (and (not (l (r (f val)))) (= (r (f val)) ())))) (and (l (r val)) (all (and (not (l (f (r val)))) (= (strlen (f (r val))) 32)) (and (not (l (r (r val)))) (= (strlen (r (r val))) 32)))))))"
        );
    }
}
