use std::collections::HashMap;

use log::debug;
use rue_ast::{AstNode, AstStructInitializerExpr, AstStructInitializerSpread};
use rue_diagnostic::{DiagnosticKind, SrcLoc};
use rue_hir::{
    Block, FieldAccess, FieldKind, Hir, Statement, StructField, StructInitializer, StructTail,
    SymbolPath, TypePath, Value,
};
use rue_lint::{UnusedSpreadReason, UnusedStructSpread};
use rue_types::{Atom, AtomRestriction, Struct, Type, TypeId, Union};

use crate::{
    BoundValue, Compiler, CompletionContext, Field, FieldResult, PathKind, PathResult,
    PendingDefault, SyntaxField, SyntaxItemKind, alloc_field_access, bind_unless_path,
    check_symbol_value, compile_expr, compile_field, compile_path,
};

pub fn compile_struct_initializer_expr(
    ctx: &mut Compiler,
    expr: &AstStructInitializerExpr,
) -> Value {
    let ty = if let Some(path) = expr.path()
        && let PathResult::Type(ty, _) =
            compile_path(ctx, path.syntax(), path.segments(), PathKind::Type, true)
    {
        ty
    } else {
        debug!("Unresolved struct initializer path");
        ctx.unresolved().ty
    };

    let semantic = rue_types::unwrap_semantic(ctx.types_mut(), ty, true);

    ctx.add_syntax(
        SyntaxItemKind::CompletionContext(CompletionContext::StructFields {
            ty: semantic,
            specified_fields: Some(
                expr.fields()
                    .filter_map(|field| field.name())
                    .map(|field| field.text().to_string())
                    .collect(),
            ),
        }),
        expr.syntax().text_range(),
    );

    let Type::Struct(struct_type) = ctx.ty(semantic).clone() else {
        debug!("Unresolved struct initializer due to non struct type");
        let name = ctx.type_name(ty);
        ctx.diagnostic(expr.syntax(), DiagnosticKind::NonStructInitializer(name));
        return ctx.unresolved();
    };

    let mut expected_field_types = HashMap::new();
    let mut current = struct_type.inner;
    let mut unresolved = false;

    for (i, field) in struct_type.fields.iter().enumerate() {
        if i == struct_type.fields.len() - 1 && !struct_type.nil_terminated {
            expected_field_types.insert(field.clone(), current);
            continue;
        }

        let pairs = rue_types::extract_pairs(ctx.types_mut(), current, false);

        // The struct's type is only unresolved if it was rejected, since every type has been
        // resolved by the time expressions are compiled. The fields can't be checked against it.
        if pairs.is_empty() {
            unresolved = true;
            break;
        }

        let first = if pairs.len() == 1 {
            pairs[0].first
        } else {
            ctx.alloc_type(Type::Union(Union::new(
                pairs.iter().map(|pair| pair.first).collect(),
            )))
        };

        let rest = if pairs.len() == 1 {
            pairs[0].rest
        } else {
            ctx.alloc_type(Type::Union(Union::new(
                pairs.iter().map(|pair| pair.rest).collect(),
            )))
        };

        expected_field_types.insert(field.clone(), first);
        current = rest;
    }

    let base = compile_spreads(ctx, expr, &struct_type);

    let mut fields = HashMap::new();

    for field in expr.fields() {
        let value = if let Some(expr) = field.expr() {
            let expected = field
                .name()
                .and_then(|name| expected_field_types.get(name.text()).copied());

            // A field's type is one of its own, which its value is checked against below as it is
            // (see `Compiler::with_expected_type_provisional`)
            ctx.with_expected_type_provisional(false, |ctx| compile_expr(ctx, &expr, expected))
        } else {
            let Some(name) = field.name() else {
                continue;
            };

            if let PathResult::Symbol(symbol, override_type, _) = compile_path(
                ctx,
                field.syntax(),
                [name].into_iter(),
                PathKind::Symbol,
                false,
            ) && check_symbol_value(ctx, field.syntax(), symbol)
            {
                let ty = ctx.symbol_type(symbol);

                let mut value = Value::new(ctx.alloc_hir(Hir::Reference(symbol)), ty)
                    .with_reference(SymbolPath {
                        symbol,
                        path: vec![],
                    });

                if let Some(override_type) = override_type {
                    value = value.with_type(override_type);
                }

                value
            } else {
                debug!("Unresolved field path in struct initializer");
                ctx.unresolved()
            }
        };

        let Some(name) = field.name() else {
            continue;
        };

        ctx.add_syntax(
            SyntaxItemKind::FieldInitializer(SyntaxField {
                name: name.text().to_string(),
                container: semantic,
                ty: expected_field_types
                    .get(name.text())
                    .copied()
                    .unwrap_or(value.ty),
            }),
            name.text_range(),
        );

        if struct_type.fields.contains(name.text()) {
            if let Some(expected_type) = expected_field_types.get(name.text()).copied() {
                let inferred = ctx.is_inferred_field(struct_type.semantic, name.text());

                match inferred.then(|| ctx.try_is_assignable(value.ty, expected_type)) {
                    Some(Ok(false)) => {
                        let from = ctx.type_name(value.ty);
                        let to = ctx.type_name(expected_type);
                        ctx.diagnostic(
                            field.syntax(),
                            DiagnosticKind::InferredFieldType(name.text().to_string(), from, to),
                        );
                    }
                    Some(Err(_)) => {
                        ctx.too_complex(field.syntax(), value.ty, expected_type);
                    }
                    Some(Ok(true)) | None => {
                        ctx.assign_type(field.syntax(), value.ty, expected_type);
                    }
                }
            }
        } else {
            let type_name = ctx.type_name(ty);
            ctx.diagnostic(
                &name,
                DiagnosticKind::UnknownField(name.text().to_string(), type_name),
            );
            continue;
        }

        if fields.insert(name.text().to_string(), value).is_some() {
            ctx.diagnostic(
                &name,
                DiagnosticKind::DuplicateField(name.text().to_string()),
            );
        }
    }

    if unresolved {
        debug!("Unresolved struct initializer due to unresolved struct type");
        return ctx.unresolved();
    }

    // The fields that aren't written are copied from the spread value, if there is one
    let mut copied = match &base {
        Some(base) => copy_fields(ctx, base, &struct_type, &fields, &expected_field_types),
        None => HashMap::new(),
    };

    // The fields after the last one that's written are the rest of the spread value as it is,
    // rather than a copy of each of them, unless they're all constants, which don't need the
    // value at all
    let prefix = struct_type
        .fields
        .iter()
        .rposition(|name| fields.contains_key(name))
        .map_or(0, |index| index + 1);

    let rest_reads = struct_type
        .fields
        .iter()
        .skip(prefix)
        .filter(|name| copied.get(*name).is_some_and(|field| !field.constant))
        .count();

    let tail = base.as_ref().filter(|_| rest_reads > 0).map(|base| {
        let value = if prefix == 0 {
            base.value.hir
        } else {
            let path = FieldKind::Path(vec![TypePath::Rest; prefix]);
            alloc_field_access(ctx, base.value.hir, "rest", path)
        };

        StructTail {
            value,
            reads: rest_reads,
        }
    });

    let field_count = if tail.is_some() {
        prefix
    } else {
        struct_type.fields.len()
    };

    // Each field's value, and whether it's the default. A field without either is missing
    let mut values = Vec::new();

    for name in struct_type.fields.into_iter().take(field_count).rev() {
        let value = if let Some(value) = fields.remove(&name) {
            FieldValue::Given(value)
        } else if base.is_some() {
            // A field that couldn't be copied has already been reported
            FieldValue::Given(copied.remove(&name).map_or_else(
                || {
                    debug!("Unresolved struct initializer field of spread value");
                    ctx.unresolved()
                },
                |field| field.value,
            ))
        } else if let Some(value) = ctx.default_field_use(struct_type.semantic, &name) {
            // Defaults are checked against the declared field type, but check them against the
            // field type of this instantiation as well, in case generics were substituted.
            if let Some(expected_type) = expected_field_types.get(&name) {
                ctx.assign_type(expr.syntax(), value.ty, *expected_type);
            }
            ctx.record_default_use(struct_type.semantic, &name);
            FieldValue::Default(value)
        } else if ctx.has_declared_default(struct_type.semantic, &name) {
            // This is in the default value of another field, and this field's default hasn't been
            // compiled yet, so it's filled in once every default has been (see
            // `resolve_pending_defaults`)
            ctx.record_default_use(struct_type.semantic, &name);
            FieldValue::Pending
        } else {
            FieldValue::Missing
        };

        values.push((name, value));
    }

    values.reverse();

    let missing_fields = values
        .iter()
        .filter(|(_, value)| matches!(value, FieldValue::Missing))
        .map(|(name, _)| name.as_str())
        .collect::<Vec<_>>();

    // Missing fields are reported before they're filled in with placeholders, so that every
    // placeholder comes after the error that explains it
    if !missing_fields.is_empty() {
        let type_name = ctx.type_name(ty);
        ctx.diagnostic(
            expr.syntax(),
            DiagnosticKind::MissingRequiredFields(type_name, missing_fields.join(", ")),
        );
    }

    let mut pending = Vec::new();

    let initializer_fields = values
        .into_iter()
        .enumerate()
        .map(|(index, (name, value))| {
            let (hir, is_default) = match value {
                FieldValue::Given(value) => (value.hir, false),
                FieldValue::Default(value) => (value.hir, true),
                FieldValue::Pending => {
                    pending.push((index, name.clone()));
                    (ctx.builtins().unresolved.hir, true)
                }
                FieldValue::Missing => {
                    debug!("Unresolved struct initializer field");
                    (ctx.unresolved().hir, false)
                }
            };

            StructField {
                name,
                value: hir,
                is_default,
            }
        })
        .collect();

    let hir = ctx.alloc_hir(Hir::Struct(StructInitializer {
        ty,
        fields: initializer_fields,
        tail,
        nil_terminated: struct_type.nil_terminated,
    }));

    for (index, name) in pending {
        let srcloc = ctx.srcloc(expr.syntax());
        let scope = ctx.last_scope_id();
        let user = ctx.compiling_default();

        ctx.push_pending_default(PendingDefault {
            hir,
            index,
            ty: struct_type.semantic,
            name,
            srcloc,
            scope,
            user,
        });
    }

    // A spread value that isn't a path is bound before the struct is built from it
    let hir = match base.and_then(|base| base.statement) {
        Some(statement) => ctx.alloc_hir(Hir::Block(Block {
            statements: vec![statement],
            body: Some(hir),
        })),
        None => hir,
    };

    Value::new(hir, ty)
}

/// The value spread in a struct initializer (`...base`), which the fields that aren't written are
/// copied from.
struct Base {
    node: AstStructInitializerSpread,
    /// The value, which is a reference to what it's bound to if it isn't a path.
    value: Value,
    /// The statement that binds the value, if it isn't a path, so that it's only evaluated once
    /// however many of its fields are copied.
    statement: Option<Statement>,
    /// Whether the value is an instance of the struct being initialized, so its fields can be
    /// copied. If it isn't, that has been reported.
    is_struct: bool,
}

/// Compiles the spreads of a struct initializer, of which the first one is the value the struct is
/// updated from. There can only be one, before the fields: the fields that are written replace
/// the ones in it, wherever they are in the struct.
fn compile_spreads(
    ctx: &mut Compiler,
    expr: &AstStructInitializerExpr,
    struct_type: &Struct,
) -> Option<Base> {
    let first_field = expr
        .fields()
        .next()
        .map(|field| field.syntax().text_range().start());

    let mut base: Option<Base> = None;

    for node in expr.spreads() {
        let value = if let Some(expr) = node.expr() {
            compile_expr(ctx, &expr, None)
        } else {
            debug!("Unresolved struct initializer spread");
            ctx.unresolved()
        };

        if base.is_some() {
            ctx.diagnostic(node.syntax(), DiagnosticKind::DuplicateStructSpread);
            continue;
        }

        if first_field.is_some_and(|start| start < node.syntax().text_range().start()) {
            ctx.diagnostic(node.syntax(), DiagnosticKind::MisplacedStructSpread);
        }

        let is_struct = is_instance(ctx, value.ty, struct_type);

        if !is_struct && !ctx.is_unresolved(value.ty) {
            let from = ctx.type_name(value.ty);
            let to = ctx.type_name(struct_type.semantic);
            ctx.diagnostic(node.syntax(), DiagnosticKind::StructSpreadType(from, to));
        }

        // The value's fields are each read from it, so a value that isn't a path is bound first,
        // rather than being evaluated again for each of them
        let srcloc = node.expr().map_or_else(
            || ctx.trimmed_srcloc(node.syntax()),
            |expr| ctx.trimmed_srcloc(expr.syntax()),
        );
        let (value, statement) = bind_base(ctx, value, srcloc);

        base = Some(Base {
            node,
            value,
            statement,
            is_struct,
        });
    }

    base
}

/// Makes the value being updated safe to read for each field that's copied from it (see
/// [`bind_unless_path`]). When it's the field of a value that isn't a path, such as `f().inner`,
/// what's bound is the value that the field is read from rather than the field, which is what a
/// struct that's rebuilt by hand binds. The fields being copied are paths into the binding then,
/// where a binding of the field would leave reading it as operations around the value.
fn bind_base(ctx: &mut Compiler, value: Value, srcloc: SrcLoc) -> (Value, Option<Statement>) {
    let mut inner = value.hir;
    let mut outer = Vec::new();

    if ctx.path_symbol(value.hir).is_none() {
        loop {
            let hir = ctx.hir(inner).clone();

            inner = match &hir {
                Hir::Cast(value) => *value,
                Hir::FieldAccess(FieldAccess {
                    value,
                    kind: FieldKind::Path(_),
                    ..
                }) => *value,
                _ => break,
            };

            outer.push(hir);
        }
    }

    // The value that the field is read from has a type if it was written as an expression
    let Some(ty) = ctx.hir_type(inner).filter(|_| inner != value.hir) else {
        let BoundValue {
            value, statement, ..
        } = bind_unless_path(ctx, value, srcloc);
        return (value, statement);
    };

    let bound = bind_unless_path(ctx, Value::new(inner, ty), srcloc);

    let hir = outer.into_iter().rev().fold(bound.value.hir, |inner, hir| {
        ctx.alloc_hir(match hir {
            Hir::FieldAccess(access) => Hir::FieldAccess(FieldAccess {
                value: inner,
                ..access
            }),
            _ => Hir::Cast(inner),
        })
    });

    (Value::new(hir, value.ty), bound.statement)
}

/// Whether every value of the type is an instance of the struct, whatever its generic arguments
/// are and however it's been narrowed. The fields that are copied from it are checked against the
/// types of the fields being initialized, so the instances don't need to be the same.
fn is_instance(ctx: &mut Compiler, ty: TypeId, struct_type: &Struct) -> bool {
    // The instances are all of the same struct, so only one of them needs to be compared
    rue_types::extract_structs(ctx.types_mut(), ty)
        .is_some_and(|structs| structs[0].semantic == struct_type.semantic)
}

/// A field that isn't written in a struct initializer, which is copied from the spread value.
struct CopiedField {
    value: Value,
    /// Whether the value is a constant rather than a read of the spread value (see
    /// [`constant_field`]).
    constant: bool,
}

/// The values of the fields that aren't written in the initializer, which are copied from the
/// spread value. Each of them is checked against the type of the field being initialized, since
/// the value can be another instance of the struct (such as with different generic arguments).
fn copy_fields(
    ctx: &mut Compiler,
    base: &Base,
    struct_type: &Struct,
    fields: &HashMap<String, Value>,
    expected_field_types: &HashMap<String, TypeId>,
) -> HashMap<String, CopiedField> {
    let mut copied = HashMap::new();

    if !base.is_struct {
        return copied;
    }

    // Copying a field into the same struct isn't a use of it, so that a field that's only ever
    // copied is still reported as never read
    let field_reads = ctx.lint_sites_mut().field_reads.len();

    let unwritten: Vec<&String> = struct_type
        .fields
        .iter()
        .filter(|name| !fields.contains_key(*name))
        .collect();

    for &name in &unwritten {
        let value = match compile_field(ctx, base.value.clone(), &Field::Named(name)) {
            FieldResult::Value(value) => value,
            FieldResult::Unknown | FieldResult::Error => {
                let type_name = ctx.type_name(base.value.ty);
                ctx.diagnostic(
                    base.node.syntax(),
                    DiagnosticKind::MissingField(name.clone(), type_name),
                );
                continue;
            }
        };

        let expected_type = expected_field_types.get(name).copied();

        if let Some(expected_type) = expected_type {
            match ctx.try_is_assignable(value.ty, expected_type) {
                Ok(true) => {}
                Ok(false) => {
                    let from = ctx.type_name(value.ty);
                    let to = ctx.type_name(expected_type);
                    ctx.diagnostic(
                        base.node.syntax(),
                        DiagnosticKind::StructSpreadFieldType(name.clone(), from, to),
                    );
                }
                Err(_) => ctx.too_complex(base.node.syntax(), value.ty, expected_type),
            }
        }

        let field = match expected_type.and_then(|ty| constant_field(ctx, ty)) {
            Some(constant) => CopiedField {
                value: Value::new(ctx.alloc_hir(Hir::Bytes(constant)), value.ty),
                constant: true,
            },
            None => CopiedField {
                value,
                constant: false,
            },
        };

        copied.insert(name.clone(), field);
    }

    ctx.lint_sites_mut().field_reads.truncate(field_reads);

    // Nothing is read from the spread value if every field that isn't written is a constant.
    // Removing it leaves the fields to their default values, if they have them, which are the
    // same constants.
    if unwritten
        .iter()
        .all(|name| copied.get(*name).is_some_and(|field| field.constant))
    {
        let removable = unwritten
            .iter()
            .all(|name| ctx.has_declared_default(struct_type.semantic, name));
        let reason = if unwritten.is_empty() {
            UnusedSpreadReason::Written
        } else if fields.is_empty() {
            UnusedSpreadReason::Constants
        } else {
            UnusedSpreadReason::OtherConstants
        };
        let srcloc = ctx.trimmed_srcloc(base.node.syntax());
        let struct_name = ctx.type_name(struct_type.semantic);
        ctx.lint_sites_mut()
            .unused_struct_spreads
            .push(UnusedStructSpread {
                srcloc,
                struct_name,
                reason,
                removable,
                evaluated: base.statement.is_some(),
            });
    }

    copied
}

/// The value of a copied field that's used rather than being read from the spread value, so that
/// the struct is never larger than one that's rebuilt by hand. That's the value of a field whose
/// type is a single value, such as the opcode of a condition or the discriminant of an enum
/// variant, which is what would be written (or left to its default) by hand. Reading it instead
/// is often smaller, but it's one more use of the spread value, which can be what makes a binding
/// necessary or evaluates the argument of an inline function again.
///
/// A field that's only a single value in the spread value, because it has been narrowed, is
/// read like any other, as it would be by hand.
fn constant_field(ctx: &mut Compiler, ty: TypeId) -> Option<Vec<u8>> {
    let ty = rue_types::unwrap_semantic(ctx.types_mut(), ty, true);

    match ctx.ty(ty) {
        Type::Atom(Atom {
            restriction: Some(AtomRestriction::Value(value)),
            ..
        }) => Some(value.to_vec()),
        _ => None,
    }
}

/// The value of a field in a struct initializer.
enum FieldValue {
    Given(Value),
    Default(Value),
    /// The field has a default value that hasn't been compiled yet.
    Pending,
    Missing,
}
