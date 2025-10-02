use log::debug;
use rue_diagnostic::DiagnosticKind;
use rue_hir::{Hir, TypePath, UnaryOp, Value};
use rue_parser::SyntaxToken;
use rue_types::{Type, Union};

use crate::{Compiler, GetTextRange};

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Field<'a> {
    Named(&'a str),
    First,
    Rest,
}

#[derive(Debug, Clone)]
pub enum FieldResult {
    Value(Value),
    Unknown,
    Error,
}

pub fn compile_named_field(ctx: &mut Compiler, expr: &Value, name: &SyntaxToken) -> Value {
    match compile_field(ctx, expr.clone(), &Field::Named(name.text())) {
        FieldResult::Value(value) => value,
        FieldResult::Unknown => {
            debug!("Unresolved field access due to unknown field");
            let type_name = ctx.type_name(expr.ty);
            ctx.diagnostic(
                name,
                DiagnosticKind::UnknownField(name.text().to_string(), type_name),
            );
            ctx.builtins().unresolved.clone()
        }
        FieldResult::Error => {
            debug!("Unresolved field access due to missing field in underlying struct type");
            let type_name = ctx.type_name(expr.ty);
            ctx.diagnostic(
                name,
                DiagnosticKind::MissingField(name.text().to_string(), type_name),
            );
            ctx.builtins().unresolved.clone()
        }
    }
}

pub fn compile_pair_fields(
    ctx: &mut Compiler,
    node: &impl GetTextRange,
    expr: &Value,
) -> (Value, Value) {
    let first = compile_field(ctx, expr.clone(), &Field::First);
    let rest = compile_field(ctx, expr.clone(), &Field::Rest);

    let (FieldResult::Value(first), FieldResult::Value(rest)) = (first, rest) else {
        let name = ctx.type_name(expr.ty);
        ctx.diagnostic(node, DiagnosticKind::CannotDestructurePair(name));
        return (
            ctx.builtins().unresolved.clone(),
            ctx.builtins().unresolved.clone(),
        );
    };

    (first, rest)
}

pub fn compile_field(ctx: &mut Compiler, expr: Value, name: &Field<'_>) -> FieldResult {
    let ty = rue_types::unwrap_semantic(ctx.types_mut(), expr.ty, true);

    match ctx.ty(ty).clone() {
        Type::Apply(_) | Type::Alias(_) | Type::Ref(_) => unreachable!(),
        Type::Unresolved => FieldResult::Unknown,
        Type::Generic(_) | Type::Function(_) | Type::Never | Type::Any => FieldResult::Unknown,
        Type::Atom(_) => {
            if let &Field::Named(name) = name
                && name == "length"
            {
                let hir = ctx.alloc_hir(Hir::Unary(UnaryOp::Strlen, expr.hir));
                FieldResult::Value(Value::new(hir, ctx.builtins().types.int))
            } else {
                FieldResult::Unknown
            }
        }
        Type::Pair(_) | Type::Union(_) => {
            let pairs = rue_types::extract_pairs(ctx.types_mut(), ty, true);

            match name {
                Field::Named("first") | Field::First if !pairs.is_empty() => {
                    let hir = ctx.alloc_hir(Hir::Unary(UnaryOp::First, expr.hir));

                    let first = if pairs.len() == 1 {
                        pairs[0].first
                    } else {
                        ctx.alloc_type(Type::Union(Union::new(
                            pairs.iter().map(|pair| pair.first).collect(),
                        )))
                    };

                    let mut value = Value::new(hir, first);

                    if let Some(mut reference) = expr.reference {
                        reference.path.push(TypePath::First);
                        value = value.with_reference(reference);
                    }

                    FieldResult::Value(value)
                }
                Field::Named("rest") | Field::Rest if !pairs.is_empty() => {
                    let hir = ctx.alloc_hir(Hir::Unary(UnaryOp::Rest, expr.hir));

                    let rest = if pairs.len() == 1 {
                        pairs[0].rest
                    } else {
                        ctx.alloc_type(Type::Union(Union::new(
                            pairs.iter().map(|pair| pair.rest).collect(),
                        )))
                    };

                    let mut value = Value::new(hir, rest);

                    if let Some(mut reference) = expr.reference {
                        reference.path.push(TypePath::Rest);
                        value = value.with_reference(reference);
                    }

                    FieldResult::Value(value)
                }
                _ => FieldResult::Unknown,
            }
        }
        Type::Struct(ty) => {
            let &Field::Named(name) = name else {
                return FieldResult::Unknown;
            };

            let Some(index) = ty.fields.get_index_of(name) else {
                return FieldResult::Unknown;
            };

            let mut hir = expr.hir;
            let mut field_type = ty.inner;
            let mut reference = expr.reference;

            let needs_first = index + 1 < ty.fields.len() || ty.nil_terminated;

            for i in 0..index {
                hir = ctx.alloc_hir(Hir::Unary(UnaryOp::Rest, hir));

                let pairs = rue_types::extract_pairs(ctx.types_mut(), field_type, true);

                if pairs.is_empty() || (pairs.len() > 1 && (i + 1 < index || needs_first)) {
                    return FieldResult::Error;
                }

                field_type = if pairs.len() == 1 {
                    pairs[0].rest
                } else {
                    ctx.alloc_type(Type::Union(Union::new(
                        pairs.into_iter().map(|pair| pair.rest).collect(),
                    )))
                };

                if let Some(reference) = reference.as_mut() {
                    reference.path.push(TypePath::Rest);
                }
            }

            if needs_first {
                hir = ctx.alloc_hir(Hir::Unary(UnaryOp::First, hir));

                let pairs = rue_types::extract_pairs(ctx.types_mut(), field_type, true);

                if pairs.is_empty() {
                    return FieldResult::Error;
                }

                field_type = if pairs.len() == 1 {
                    pairs[0].first
                } else {
                    ctx.alloc_type(Type::Union(Union::new(
                        pairs.into_iter().map(|pair| pair.first).collect(),
                    )))
                };

                if let Some(reference) = reference.as_mut() {
                    reference.path.push(TypePath::First);
                }
            }

            let mut value = Value::new(hir, field_type);

            if let Some(reference) = reference {
                value = value.with_reference(reference);
            }

            FieldResult::Value(value)
        }
    }
}
