use std::{
    collections::{HashSet, VecDeque},
    fmt,
    hash::BuildHasher,
};

use crate::{Comparison, Type, TypeId, TypePath, TypeSystem};

mod check_error;
mod simplify_and;
mod simplify_check;
mod simplify_or;

pub use check_error::*;

pub(crate) use simplify_and::*;
pub(crate) use simplify_check::*;
pub(crate) use simplify_or::*;

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

impl fmt::Display for Check {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt_check(self, f, &mut Vec::new())
    }
}

/// Returns [`None`] for recursive checks.
pub(crate) fn check_type<S>(
    types: &mut TypeSystem,
    lhs: TypeId,
    rhs: TypeId,
    visited: &mut HashSet<(TypeId, TypeId), S>,
) -> Result<Check, CheckError>
where
    S: BuildHasher,
{
    if !visited.insert((lhs, rhs)) {
        if types.compare(lhs, rhs) <= Comparison::Castable {
            return Ok(Check::None);
        }
        return Err(CheckError::Recursive(lhs, rhs));
    }

    let check = match (types.get(lhs), types.get(rhs)) {
        (Type::Ref(..), _) | (_, Type::Ref(..)) => unreachable!(),
        (Type::Lazy(..), _) | (_, Type::Lazy(..)) => unreachable!(),
        (Type::Alias(..), _) | (_, Type::Alias(..)) => unreachable!(),

        // TODO: Implement generic type checking?
        (Type::Generic, _) => Check::None,
        (_, Type::Generic) => Check::None,

        (Type::Unknown, _) => Check::None,
        (_, Type::Unknown) => Check::None,

        (Type::Never, _) => Check::None,
        (_, Type::Never) => return Err(CheckError::Impossible(lhs, rhs)),

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

        (Type::Union(items), _) => {
            let items = items.clone();
            check_union_against_rhs(types, lhs, &items, rhs, visited)?
        }

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
    types: &mut TypeSystem,
    original_type_id: TypeId,
    items: &[TypeId],
    rhs: TypeId,
    visited: &mut HashSet<(TypeId, TypeId), S>,
) -> Result<Check, CheckError>
where
    S: BuildHasher,
{
    let union = types.alloc(Type::Union(items.to_vec()));

    if types.compare(union, rhs) <= Comparison::Castable {
        return Ok(Check::None);
    }

    if let Type::Union(union) = types.get(rhs) {
        let rhs_items = union.clone();
        let mut result = Vec::new();
        for rhs_item in rhs_items {
            if !visited.insert((original_type_id, rhs_item)) {
                return Err(CheckError::Recursive(original_type_id, rhs_item));
            }
            result.push(check_union_against_rhs(
                types,
                original_type_id,
                items,
                rhs_item,
                visited,
            )?);
        }
        return Ok(Check::Or(result));
    }

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
            Type::Lazy(..) => unreachable!(),
            Type::Alias(..) => unreachable!(),

            Type::Union(child_items) => {
                items.extend(child_items);
            }
            Type::Generic => return Err(CheckError::Impossible(item, rhs)),
            Type::Unknown => {}
            Type::Never => {
                length -= 1;
            }
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
    let atom_always_nil = nil_count == atom_count;
    let atom_always_bool = bool_count == atom_count;
    let atom_always_bytes32 = bytes32_count == atom_count;
    let atom_always_public_key = public_key_count == atom_count;

    Ok(match types.get(rhs) {
        Type::Ref(..) => unreachable!(),
        Type::Lazy(..) => unreachable!(),
        Type::Alias(..) => unreachable!(),
        Type::Union(..) => unreachable!(),
        Type::Unknown => Check::None,
        Type::Generic => return Err(CheckError::Impossible(original_type_id, rhs)),
        Type::Never => return Err(CheckError::Impossible(original_type_id, rhs)),
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
        Type::Bytes32 if atom_always_bytes32 => Check::IsAtom,
        Type::PublicKey if atom_always_public_key => Check::IsAtom,
        Type::Bool if atom_always_bool => Check::IsAtom,
        Type::Nil if atom_always_nil => Check::IsAtom,
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

fn fmt_val(f: &mut fmt::Formatter<'_>, path: &[TypePath]) -> fmt::Result {
    for path in path.iter().rev() {
        match path {
            TypePath::First => write!(f, "(f ")?,
            TypePath::Rest => write!(f, "(r ")?,
        }
    }
    write!(f, "val")?;
    for _ in 0..path.len() {
        write!(f, ")")?;
    }
    Ok(())
}

fn fmt_check(check: &Check, f: &mut fmt::Formatter<'_>, path: &mut Vec<TypePath>) -> fmt::Result {
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
                path.push(TypePath::First);
                fmt_check(first, f, path)?;
                path.pop().unwrap();
                write!(f, " ")?;
                path.push(TypePath::Rest);
                fmt_check(rest, f, path)?;
                path.pop().unwrap();
                write!(f, ")")
            } else if has_first {
                path.push(TypePath::First);
                fmt_check(first, f, path)?;
                path.pop().unwrap();
                Ok(())
            } else if has_rest {
                path.push(TypePath::Rest);
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

    fn setup() -> (TypeSystem, StandardTypes) {
        let mut db = TypeSystem::new();
        let types = StandardTypes::alloc(&mut db);
        (db, types)
    }

    fn check_str(db: &mut TypeSystem, lhs: TypeId, rhs: TypeId, expected: &str) {
        assert_eq!(format!("{}", db.check(lhs, rhs).unwrap()), expected);
    }

    fn check_recursive(db: &mut TypeSystem, lhs: TypeId, rhs: TypeId) {
        assert!(matches!(db.check(lhs, rhs), Err(CheckError::Recursive(..))));
    }

    fn check_impossible(db: &mut TypeSystem, lhs: TypeId, rhs: TypeId) {
        assert!(matches!(
            db.check(lhs, rhs),
            Err(CheckError::Impossible(..))
        ));
    }

    fn alloc_list(db: &mut TypeSystem, types: &StandardTypes, item_type_id: TypeId) -> TypeId {
        let list = db.alloc(Type::Unknown);
        let pair = db.alloc(Type::Pair(item_type_id, list));
        *db.get_mut(list) = Type::Union(vec![pair, types.nil]);
        list
    }

    #[test]
    fn check_any_bytes() {
        let (mut db, types) = setup();
        check_str(&mut db, types.any, types.bytes, "(not (l val))");
    }

    #[test]
    fn check_any_bytes32() {
        let (mut db, types) = setup();
        check_str(
            &mut db,
            types.any,
            types.bytes32,
            "(and (not (l val)) (= (strlen val) 32))",
        );
    }

    #[test]
    fn check_any_public_key() {
        let (mut db, types) = setup();
        check_str(
            &mut db,
            types.any,
            types.public_key,
            "(and (not (l val)) (= (strlen val) 48))",
        );
    }

    #[test]
    fn check_any_int() {
        let (mut db, types) = setup();
        check_str(&mut db, types.any, types.int, "(not (l val))");
    }

    #[test]
    fn check_any_bool() {
        let (mut db, types) = setup();
        check_str(
            &mut db,
            types.any,
            types.bool,
            "(and (not (l val)) (any (= val ()) (= val 1)))",
        );
    }

    #[test]
    fn check_any_nil() {
        let (mut db, types) = setup();
        check_str(
            &mut db,
            types.any,
            types.nil,
            "(and (not (l val)) (= val ()))",
        );
    }

    #[test]
    fn check_bytes_bytes() {
        let (mut db, types) = setup();
        check_str(&mut db, types.bytes, types.bytes, "1");
    }

    #[test]
    fn check_bytes32_bytes32() {
        let (mut db, types) = setup();
        check_str(&mut db, types.bytes32, types.bytes32, "1");
    }

    #[test]
    fn check_public_key_public_key() {
        let (mut db, types) = setup();
        check_str(&mut db, types.public_key, types.public_key, "1");
    }

    #[test]
    fn check_int_int() {
        let (mut db, types) = setup();
        check_str(&mut db, types.int, types.int, "1");
    }

    #[test]
    fn check_bool_bool() {
        let (mut db, types) = setup();
        check_str(&mut db, types.bool, types.bool, "1");
    }

    #[test]
    fn check_nil_nil() {
        let (mut db, types) = setup();
        check_str(&mut db, types.nil, types.nil, "1");
    }

    #[test]
    fn check_bytes_bytes32() {
        let (mut db, types) = setup();
        check_str(&mut db, types.bytes, types.bytes32, "(= (strlen val) 32)");
    }

    #[test]
    fn check_bytes32_bytes() {
        let (mut db, types) = setup();
        check_str(&mut db, types.bytes32, types.bytes, "1");
    }

    #[test]
    fn check_bytes_public_key() {
        let (mut db, types) = setup();
        check_str(
            &mut db,
            types.bytes,
            types.public_key,
            "(= (strlen val) 48)",
        );
    }

    #[test]
    fn check_public_key_bytes() {
        let (mut db, types) = setup();
        check_str(&mut db, types.public_key, types.bytes, "1");
    }

    #[test]
    fn check_bytes_nil() {
        let (mut db, types) = setup();
        check_str(&mut db, types.bytes, types.nil, "(= val ())");
    }

    #[test]
    fn check_nil_bytes() {
        let (mut db, types) = setup();
        check_str(&mut db, types.nil, types.bytes, "1");
    }

    #[test]
    fn check_bytes_bool() {
        let (mut db, types) = setup();
        check_str(
            &mut db,
            types.bytes,
            types.bool,
            "(any (= val ()) (= val 1))",
        );
    }

    #[test]
    fn check_bool_bytes() {
        let (mut db, types) = setup();
        check_str(&mut db, types.bool, types.bytes, "1");
    }

    #[test]
    fn check_bytes32_public_key() {
        let (mut db, types) = setup();
        check_impossible(&mut db, types.bytes32, types.public_key);
    }

    #[test]
    fn check_bytes_int() {
        let (mut db, types) = setup();
        check_str(&mut db, types.bytes, types.int, "1");
    }

    #[test]
    fn check_int_bytes() {
        let (mut db, types) = setup();
        check_str(&mut db, types.int, types.bytes, "1");
    }

    #[test]
    fn check_bool_nil() {
        let (mut db, types) = setup();
        check_str(&mut db, types.bool, types.nil, "(= val ())");
    }

    #[test]
    fn check_nil_bool() {
        let (mut db, types) = setup();
        check_str(&mut db, types.nil, types.bool, "1");
    }

    #[test]
    fn check_any_any() {
        let (mut db, types) = setup();
        check_str(&mut db, types.any, types.any, "1");
    }

    #[test]
    fn check_bytes_any() {
        let (mut db, types) = setup();
        check_str(&mut db, types.any, types.any, "1");
    }

    #[test]
    fn check_list_nil() {
        let (mut db, types) = setup();
        let list = alloc_list(&mut db, &types, types.bytes);
        check_str(&mut db, list, types.nil, "(not (l val))");
    }

    #[test]
    fn check_list_pair() {
        let (mut db, types) = setup();
        let list = alloc_list(&mut db, &types, types.bytes);
        let pair = db.alloc(Type::Pair(types.bytes, list));
        check_str(&mut db, list, pair, "(l val)");
    }

    #[test]
    fn check_any_list() {
        let (mut db, types) = setup();
        let list = alloc_list(&mut db, &types, types.bytes);
        check_recursive(&mut db, types.any, list);
    }
}
