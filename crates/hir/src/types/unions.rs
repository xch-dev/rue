use crate::{
    Atom, BinaryOp, Builtins, Comparison, ComparisonContext, Constraint, Database, Hir, HirId,
    Type, TypeId, TypePath, UnaryOp, bigint_atom, compare_types, unwrap_type,
};

#[derive(Debug, Clone)]
pub enum ShapeType {
    Atom(Atom),
    Pair(TypeId, TypeId),
}

impl ShapeType {
    pub fn is_atom(&self) -> bool {
        matches!(self, ShapeType::Atom(..))
    }

    pub fn is_pair(&self) -> bool {
        matches!(self, ShapeType::Pair(..))
    }
}

pub fn constrain_union(
    db: &mut Database,
    ctx: &mut ComparisonContext,
    builtins: &Builtins,
    to_id: TypeId,
    mut wrapped: Vec<TypeId>,
) -> Comparison {
    let mut unwrapped_ids = Vec::new();

    while let Some(id) = wrapped.pop() {
        let id = unwrap_type(db, ctx, id);

        if let Type::Union(ids) = db.ty(id) {
            wrapped.extend_from_slice(ids);
        } else {
            unwrapped_ids.push(id);
        }
    }

    let mut atoms = Vec::new();
    let mut pairs = Vec::new();

    for id in unwrapped_ids {
        match db.ty(id).clone() {
            Type::Alias(_) | Type::Union(_) => unreachable!(),
            Type::Unresolved => {}
            // TODO: Handle generic types
            Type::Generic(_) | Type::Fn(_) => return Comparison::Incompatible,
            Type::Atom(atom) => atoms.push((id, atom)),
            Type::Pair(first, rest) => pairs.push((id, first, rest)),
        }
    }

    let shape = match db.ty(to_id).clone() {
        Type::Unresolved | Type::Generic(_) | Type::Alias(_) | Type::Union(_) => unreachable!(),
        Type::Fn(..) => return Comparison::Incompatible,
        Type::Atom(atom) => ShapeType::Atom(atom),
        Type::Pair(first, rest) => ShapeType::Pair(first, rest),
    };

    // If the target is a pair, we need to have at least one pair in the union for it to be constrainable
    if shape.is_pair() && pairs.is_empty() {
        return Comparison::Incompatible;
    }

    // If the target is an atom, we need to have at least one atom in the union for it to be constrainable
    if !shape.is_pair() && atoms.is_empty() {
        return Comparison::Incompatible;
    }

    // If the target is a pair but we have atoms, we need to check at runtime that the type is a pair
    if let ShapeType::Pair(first, rest) = shape
        && !atoms.is_empty()
    {
        let shape_check = db.alloc_hir(Hir::Unary(UnaryOp::Listp, ctx.hir()));

        let first_ids = pairs.iter().map(|(_, first, _)| *first).collect::<Vec<_>>();
        let first_hir = db.alloc_hir(Hir::Unary(UnaryOp::First, ctx.hir()));
        ctx.push_first(first_hir);
        let first_comparison = constrain_union(db, ctx, builtins, first, first_ids);
        ctx.pop_hir();

        let rest_ids = pairs.iter().map(|(_, _, rest)| *rest).collect::<Vec<_>>();
        let rest_hir = db.alloc_hir(Hir::Unary(UnaryOp::Rest, ctx.hir()));
        ctx.push_rest(rest_hir);
        let rest_comparison = constrain_union(db, ctx, builtins, rest, rest_ids);
        ctx.pop_hir();

        let atom_ids = atoms.iter().map(|(id, _)| *id).collect::<Vec<_>>();

        match (first_comparison, rest_comparison) {
            (Comparison::Incompatible, _) | (_, Comparison::Incompatible) => {
                return Comparison::Incompatible;
            }
            (
                Comparison::Assignable | Comparison::Castable,
                Comparison::Assignable | Comparison::Castable,
            ) => {
                return make_constraint(db, shape_check, ctx.path(), to_id, atom_ids);
            }
            (
                Comparison::Constrainable(_constraint),
                Comparison::Assignable | Comparison::Castable,
            ) => {
                todo!()
            }
            (
                Comparison::Assignable | Comparison::Castable,
                Comparison::Constrainable(_constraint),
            ) => {
                todo!()
            }
            (Comparison::Constrainable(_first), Comparison::Constrainable(_rest)) => {
                todo!()
            }
        }
    }

    // If the target is an atom but we have pairs, we need to check at runtime that the type is an atom
    if let ShapeType::Atom(atom) = shape
        && !pairs.is_empty()
    {
        let shape_check = db.alloc_hir(Hir::Unary(UnaryOp::Listp, ctx.hir()));
        let shape_check = db.alloc_hir(Hir::Unary(UnaryOp::Not, shape_check));

        let pair_ids = pairs.iter().map(|(id, _, _)| *id).collect::<Vec<_>>();

        let mut otherwise = pair_ids;

        return match atom {
            Atom::Bytes | Atom::Int => {
                make_constraint(db, shape_check, ctx.path(), to_id, otherwise)
            }
            Atom::Nil | Atom::BoolValue(false) => {
                for (_, atom) in atoms {
                    if let Some(atom) = atom.subtract_value(vec![]) {
                        otherwise.push(db.alloc_type(Type::Atom(atom)));
                    }
                }

                let hir = db.alloc_hir(Hir::Binary(BinaryOp::Eq, ctx.hir(), builtins.nil.hir));
                let check = db.alloc_hir(Hir::Binary(BinaryOp::And, shape_check, hir));

                make_constraint(db, check, ctx.path(), to_id, otherwise)
            }
            Atom::BoolValue(true) => {
                for (_, atom) in atoms {
                    if let Some(atom) = atom.subtract_value(vec![1]) {
                        otherwise.push(db.alloc_type(Type::Atom(atom)));
                    }
                }

                let hir = db.alloc_hir(Hir::Binary(
                    BinaryOp::Eq,
                    ctx.hir(),
                    builtins.true_value.hir,
                ));
                let check = db.alloc_hir(Hir::Binary(BinaryOp::And, shape_check, hir));

                make_constraint(db, check, ctx.path(), to_id, otherwise)
            }
            Atom::Bool => {
                for (_, atom) in atoms {
                    if let Some(atom) = atom.subtract_value(vec![])
                        && let Some(atom) = atom.subtract_value(vec![1])
                    {
                        otherwise.push(db.alloc_type(Type::Atom(atom)));
                    }
                }

                let eq_true = db.alloc_hir(Hir::Binary(
                    BinaryOp::Eq,
                    ctx.hir(),
                    builtins.true_value.hir,
                ));
                let eq_false = db.alloc_hir(Hir::Binary(
                    BinaryOp::Eq,
                    ctx.hir(),
                    builtins.false_value.hir,
                ));
                let any = db.alloc_hir(Hir::Binary(BinaryOp::Any, eq_true, eq_false));
                let check = db.alloc_hir(Hir::Binary(BinaryOp::And, shape_check, any));

                make_constraint(db, check, ctx.path(), to_id, otherwise)
            }
            Atom::BytesValue(bytes) => {
                for (_, atom) in atoms {
                    if let Some(atom) = atom.subtract_value(bytes.clone()) {
                        otherwise.push(db.alloc_type(Type::Atom(atom)));
                    }
                }

                let value = db.alloc_hir(Hir::Bytes(bytes));
                let hir = db.alloc_hir(Hir::Binary(BinaryOp::Eq, ctx.hir(), value));
                let check = db.alloc_hir(Hir::Binary(BinaryOp::And, shape_check, hir));

                make_constraint(db, check, ctx.path(), to_id, otherwise)
            }
            Atom::StringValue(string) => {
                for (_, atom) in atoms {
                    if let Some(atom) = atom.subtract_value(string.as_bytes().to_vec()) {
                        otherwise.push(db.alloc_type(Type::Atom(atom)));
                    }
                }

                let value = db.alloc_hir(Hir::String(string));
                let hir = db.alloc_hir(Hir::Binary(BinaryOp::Eq, ctx.hir(), value));
                let check = db.alloc_hir(Hir::Binary(BinaryOp::And, shape_check, hir));

                make_constraint(db, check, ctx.path(), to_id, otherwise)
            }
            Atom::IntValue(int) => {
                for (_, atom) in atoms {
                    if let Some(atom) = atom.subtract_value(bigint_atom(int.clone())) {
                        otherwise.push(db.alloc_type(Type::Atom(atom)));
                    }
                }

                let value = db.alloc_hir(Hir::Int(int));
                let hir = db.alloc_hir(Hir::Binary(BinaryOp::Eq, ctx.hir(), value));
                let check = db.alloc_hir(Hir::Binary(BinaryOp::And, shape_check, hir));

                make_constraint(db, check, ctx.path(), to_id, otherwise)
            }
            Atom::Bytes32 | Atom::PublicKey => {
                let length = if matches!(atom, Atom::Bytes32) {
                    32
                } else {
                    48
                };

                for (_, atom) in atoms {
                    if let Some(atom) = atom.subtract_length(length) {
                        otherwise.push(db.alloc_type(Type::Atom(atom)));
                    }
                }

                let hir_length = db.alloc_hir(Hir::Unary(UnaryOp::Strlen, ctx.hir()));
                let length_value = db.alloc_hir(Hir::Int(length.into()));
                let hir = db.alloc_hir(Hir::Binary(BinaryOp::Eq, hir_length, length_value));
                let check = db.alloc_hir(Hir::Binary(BinaryOp::And, shape_check, hir));

                make_constraint(db, check, ctx.path(), to_id, otherwise)
            }
        };
    }

    Comparison::Assignable
}

fn make_constraint(
    db: &mut Database,
    check: HirId,
    path: Vec<TypePath>,
    to_id: TypeId,
    otherwise: Vec<TypeId>,
) -> Comparison {
    Comparison::Constrainable(match otherwise.len() {
        0 => Constraint::to(check, path, to_id),
        1 => Constraint::if_else(check, path, to_id, otherwise[0]),
        _ => Constraint::if_else(check, path, to_id, db.alloc_type(Type::Union(otherwise))),
    })
}

pub fn compare_to_union(
    db: &mut Database,
    ctx: &mut ComparisonContext,
    builtins: &Builtins,
    from_id: TypeId,
    to_id: TypeId,
    ids: Vec<TypeId>,
) -> Comparison {
    let mut result = Comparison::Incompatible;

    for id in ids {
        let comparison = compare_types(db, ctx, builtins, from_id, id);

        match (&mut result, comparison) {
            (Comparison::Incompatible, comparison) => {
                result = comparison;
            }
            (_, Comparison::Assignable) => {
                result = Comparison::Assignable;
                break;
            }
            (Comparison::Assignable, _) => {
                result = Comparison::Assignable;
                break;
            }
            (Comparison::Castable, Comparison::Castable) => {}
            (Comparison::Constrainable(..), Comparison::Castable) => {
                result = Comparison::Castable;
            }
            (Comparison::Constrainable(old), Comparison::Constrainable(new)) => {
                // TODO: Handle multiple constraints
                let or = db.alloc_hir(Hir::Binary(BinaryOp::Or, old.hir, new.hir));
                result = Comparison::Constrainable(Constraint::to(or, ctx.path(), to_id));
            }
            (Comparison::Castable, comparison @ Comparison::Constrainable(..)) => {
                result = comparison;
            }
            (_, Comparison::Incompatible) => {}
        };
    }

    result
}
