use crate::HashSet;

use crate::{Type, TypeId, TypeSystem};

pub(crate) fn debug_type(
    ty: &TypeSystem,
    prefix: &str,
    type_id: TypeId,
    indent: usize,
    visited: &mut HashSet<TypeId>,
) -> String {
    let mut result = String::new();

    for _ in 0..indent {
        result.push_str("  ");
    }

    result.push_str(prefix);
    result.push_str(&format!("({}) ", type_id.index()));

    if !visited.insert(type_id) {
        result.push_str("...");
        return result;
    }

    match ty.get_raw(type_id) {
        Type::Unknown => result.push_str("Unknown"),
        Type::Any => result.push_str("Any"),
        Type::Never => result.push_str("Never"),
        Type::Bytes => result.push_str("Bytes"),
        Type::Bytes32 => result.push_str("Bytes32"),
        Type::PublicKey => result.push_str("PublicKey"),
        Type::Int => result.push_str("Int"),
        Type::Nil => result.push_str("Nil"),
        Type::True => result.push_str("True"),
        Type::False => result.push_str("False"),
        Type::Generic => result.push_str("Generic"),
        Type::Value(value) => result.push_str(&format!("Literal {value}")),
        Type::Pair(first, rest) => {
            let first = debug_type(ty, "First", *first, indent + 1, visited);
            let rest = debug_type(ty, "Rest", *rest, indent + 1, visited);
            result.push_str(&format!("Pair\n{first}\n{rest}"));
        }
        Type::Union(types) => {
            result.push_str("Union");
            for type_id in types {
                let type_str = debug_type(ty, "", *type_id, indent + 1, visited);
                result.push_str(&format!("\n{type_str}"));
            }
        }
        Type::Ref(inner) => {
            let inner = debug_type(ty, "", *inner, indent + 1, visited);
            result.push_str(&format!("Ref\n{inner}"));
        }
        Type::Alias(alias) => {
            result.push_str(&format!("Alias {}", alias.original_type_id.index()));
            if !alias.generic_types.is_empty() {
                generics(&mut result, &alias.generic_types);
            }
            let inner = debug_type(ty, "", alias.type_id, indent + 1, visited);
            result.push_str(&format!("\n{inner}"));
        }
        Type::Lazy(lazy) => {
            result.push_str("Lazy");
            if !lazy.substitutions.is_empty() {
                result.push_str(" <");
                for (i, (from, to)) in lazy.substitutions.iter().enumerate() {
                    if i != 0 {
                        result.push_str(", ");
                    }
                    result.push_str(&format!("{} = {}", from.index(), to.index()));
                }
                result.push('>');
            }
            let inner = debug_type(ty, "", lazy.type_id, indent + 1, visited);
            result.push_str(&format!("\n{inner}"));
        }
        Type::Struct(struct_type) => {
            result.push_str("Struct");
            if !struct_type.generic_types.is_empty() {
                generics(&mut result, &struct_type.generic_types);
            }
            let inner = debug_type(ty, "", struct_type.type_id, indent + 1, visited);
            result.push_str(&format!("\n{inner}"));
        }
        Type::Enum(enum_type) => {
            result.push_str("Enum");
            let inner = debug_type(ty, "", enum_type.type_id, indent + 1, visited);
            result.push_str(&format!("\n{inner}"));
        }
        Type::Variant(variant) => {
            result.push_str("Variant");
            let inner = debug_type(ty, "", variant.type_id, indent + 1, visited);
            result.push_str(&format!("\n{inner}"));
        }
        Type::Callable(callable) => {
            result.push_str("Callable");
            if !callable.generic_types.is_empty() {
                generics(&mut result, &callable.generic_types);
            }
            let inner = debug_type(ty, "Parameters", callable.parameters, indent + 1, visited);
            result.push_str(&format!("\n{inner}"));
            let inner = debug_type(ty, "Return", callable.return_type, indent + 1, visited);
            result.push_str(&format!("\n{inner}"));
        }
    }

    visited.remove(&type_id);
    result
}

fn generics(result: &mut String, generics: &[TypeId]) {
    if !generics.is_empty() {
        result.push_str(" <");
        for (i, type_id) in generics.iter().enumerate() {
            if i != 0 {
                result.push_str(", ");
            }
            result.push_str(&format!("{}", type_id.index()));
        }
        result.push('>');
    }
}
