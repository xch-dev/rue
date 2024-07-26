use std::collections::{HashMap, HashSet};

use crate::{Callable, Enum, Struct, Type, TypeId, TypeSystem, Variant};

pub(crate) fn stringify_type(
    types: &TypeSystem,
    type_id: TypeId,
    names: &HashMap<TypeId, String>,
    debug: bool,
    visited: &mut HashSet<TypeId>,
) -> String {
    if let Some(name) = names.get(&type_id) {
        return name.clone();
    }

    if !visited.insert(type_id) {
        return format!(
            "{{recursive{}}}",
            if debug {
                format!("{type_id:?}")
            } else {
                String::new()
            }
        );
    }

    let result = match types.get(type_id) {
        Type::Ref(..) => unreachable!(),
        Type::Unknown => "{unknown}".to_string(),
        Type::Generic => format!(
            "{{generic{}}}",
            if debug {
                format!("{type_id:?}")
            } else {
                String::new()
            }
        ),
        Type::Never => "Never".to_string(),
        Type::Any => "Any".to_string(),
        Type::Bytes => "Bytes".to_string(),
        Type::Bytes32 => "Bytes32".to_string(),
        Type::PublicKey => "PublicKey".to_string(),
        Type::Int => "Int".to_string(),
        Type::True => "True".to_string(),
        Type::False => "False".to_string(),
        Type::Nil => "Nil".to_string(),
        Type::Value(value) => format!("{value}"),
        Type::Pair(first, rest) => {
            let first = stringify_type(types, *first, names, debug, visited);
            let rest = stringify_type(types, *rest, names, debug, visited);
            format!("({first}, {rest})")
        }
        Type::Union(items) => {
            let mut result = String::new();

            for (index, item) in items.iter().enumerate() {
                if index > 0 {
                    result.push_str(" | ");
                }
                result.push_str(&stringify_type(types, *item, names, debug, visited));
            }

            result
        }
        Type::Lazy(lazy) => {
            let name = stringify_type(types, lazy.type_id, names, debug, visited);
            let mut generics = "<".to_string();

            for (index, (_, generic)) in lazy.substitutions.iter().enumerate() {
                if index > 0 {
                    generics.push_str(", ");
                }
                generics.push_str(&stringify_type(types, *generic, names, debug, visited));
            }

            generics.push('>');
            name + &generics
        }
        Type::Alias(alias) => stringify_type(types, alias.type_id, names, debug, visited),
        Type::Struct(Struct { type_id, .. }) | Type::Variant(Variant { type_id, .. }) => {
            stringify_type(types, *type_id, names, debug, visited)
        }
        Type::Enum(Enum { type_id, .. }) => stringify_type(types, *type_id, names, debug, visited),
        Type::Callable(Callable {
            parameters,
            return_type,
            ..
        }) => {
            let mut result = "fun(".to_string();
            result.push_str(&stringify_type(types, *parameters, names, debug, visited));
            result.push_str(") -> ");
            result.push_str(&stringify_type(types, *return_type, names, debug, visited));
            result
        }
    };

    visited.remove(&type_id);

    result
}

#[cfg(test)]
mod tests {
    use indexmap::indexmap;

    use crate::{alloc_callable, Rest};

    use super::*;

    #[test]
    fn stringify_atoms() {
        let db = TypeSystem::new();
        let types = db.std();

        assert_eq!(db.stringify(types.unknown, false), "{unknown}");
        assert_eq!(db.stringify(types.never, false), "Never");
        assert_eq!(db.stringify(types.bytes, false), "Bytes");
        assert_eq!(db.stringify(types.bytes32, false), "Bytes32");
        assert_eq!(db.stringify(types.public_key, false), "PublicKey");
        assert_eq!(db.stringify(types.int, false), "Int");
        assert_eq!(db.stringify(types.bool, false), "Bool");
        assert_eq!(db.stringify(types.nil, false), "Nil");
        assert_eq!(db.stringify(types.any, false), "Any");
    }

    #[test]
    fn stringify_named() {
        let db = TypeSystem::new();
        let types = db.std();

        let mut names = HashMap::new();
        names.insert(types.any, "CustomAny".to_string());

        assert_eq!(db.stringify_named(types.any, names, false), "CustomAny");
    }

    #[test]
    fn test_stringify_callable() {
        let mut db = TypeSystem::new();
        let types = db.std();

        let callable = alloc_callable(
            &mut db,
            &indexmap! {
                "a".to_string() => types.int,
                "b".to_string() => types.bytes,
            },
            types.bool,
            Rest::Nil,
        );

        assert_eq!(
            db.stringify_named(callable, HashMap::new(), false),
            "fun((Int, (Bytes, Nil))) -> Bool"
        );
    }
}
