use std::collections::HashMap;

use clvmr::Allocator;
use id_arena::Id;
use num_bigint::BigInt;
use rue_mir::{BinaryOp, UnaryOp};
use rue_parser::SyntaxToken;

use crate::{Database, Hir, HirId, ScopeId};

pub type TypeId = Id<Type>;

#[derive(Debug, Clone)]
pub enum Type {
    Unresolved,
    Atom(Atom),
    Pair(TypeId, TypeId),
    Generic(Generic),
    Alias(Alias),
    Union(Vec<TypeId>),
    Apply(TypeId, HashMap<TypeId, TypeId>),
}

#[derive(Debug, Clone)]
pub enum Atom {
    Bytes,
    BytesValue(Vec<u8>),
    Bytes32,
    PublicKey,
    Int,
    IntValue(BigInt),
    Nil,
    Bool,
    BoolValue(bool),
}

#[derive(Debug, Clone)]
pub struct Generic {
    pub name: Option<SyntaxToken>,
}

#[derive(Debug, Clone)]
pub struct Alias {
    pub name: Option<SyntaxToken>,
    pub scope: ScopeId,
    pub vars: Vec<TypeId>,
    pub inner: TypeId,
}

#[derive(Debug, Clone)]
pub enum Comparison {
    Assignable,
    Castable,
    Constrainable(HirId),
    Incompatible,
}

pub fn compare_types(
    db: &mut Database,
    hir: HirId,
    from_map: &HashMap<TypeId, TypeId>,
    from_id: TypeId,
    to_map: &HashMap<TypeId, TypeId>,
    to_id: TypeId,
    inferred: &mut HashMap<TypeId, TypeId>,
) -> Comparison {
    if let Some(from_id) = from_map.get(&from_id) {
        return compare_types(db, hir, from_map, *from_id, to_map, to_id, inferred);
    }

    if let Some(to_id) = to_map.get(&to_id) {
        return compare_types(db, hir, from_map, from_id, to_map, *to_id, inferred);
    }

    if let Some(from_id) = inferred.get(&from_id) {
        return compare_types(db, hir, from_map, *from_id, to_map, to_id, inferred);
    }

    if let Some(to_id) = inferred.get(&to_id) {
        return compare_types(db, hir, from_map, from_id, to_map, *to_id, inferred);
    }

    match (db.ty(from_id).clone(), db.ty(to_id).clone()) {
        (Type::Unresolved, _) | (_, Type::Unresolved) => Comparison::Assignable,
        (Type::Generic(_), _) => Comparison::Incompatible,
        (_, Type::Generic(_)) => {
            inferred.insert(to_id, from_id);
            Comparison::Assignable
        }
        (Type::Apply(from_id, from_map), _) => {
            compare_types(db, hir, &from_map, from_id, to_map, to_id, inferred)
        }
        (_, Type::Apply(to_id, to_map)) => {
            compare_types(db, hir, from_map, from_id, &to_map, to_id, inferred)
        }
        (Type::Alias(from), _) => {
            compare_types(db, hir, from_map, from.inner, to_map, to_id, inferred)
        }
        (_, Type::Alias(to)) => {
            compare_types(db, hir, from_map, from_id, to_map, to.inner, inferred)
        }
        (Type::Union(..), _) => todo!(),
        (_, Type::Union(to_ids)) => {
            let mut result = Comparison::Incompatible;

            for to_id in to_ids {
                let comparison = compare_types(db, hir, from_map, from_id, to_map, to_id, inferred);

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
                    (
                        Comparison::Constrainable(existing_constraints),
                        Comparison::Constrainable(new_constraints),
                    ) => {
                        let or = db.alloc_hir(Hir::Binary(
                            BinaryOp::Or,
                            *existing_constraints,
                            new_constraints,
                        ));
                        result = Comparison::Constrainable(or);
                    }
                    (Comparison::Castable, comparison @ Comparison::Constrainable(..)) => {
                        result = comparison;
                    }
                    (_, Comparison::Incompatible) => {}
                };
            }

            result
        }
        (Type::Atom(..), Type::Pair(..)) | (Type::Pair(..), Type::Atom(..)) => {
            Comparison::Incompatible
        }
        (Type::Pair(from_first, from_rest), Type::Pair(to_first, to_rest)) => {
            let first_hir = db.alloc_hir(Hir::Unary(UnaryOp::First, hir));
            let rest_hir = db.alloc_hir(Hir::Unary(UnaryOp::Rest, hir));
            let first = compare_types(
                db, first_hir, from_map, from_first, to_map, to_first, inferred,
            );
            let rest = compare_types(db, rest_hir, from_map, from_rest, to_map, to_rest, inferred);
            match (first, rest) {
                (Comparison::Assignable, Comparison::Assignable) => Comparison::Assignable,
                (Comparison::Castable, Comparison::Castable) => Comparison::Castable,
                (
                    Comparison::Constrainable(first_constraints),
                    Comparison::Constrainable(rest_constraints),
                ) => {
                    let and = db.alloc_hir(Hir::Binary(
                        BinaryOp::And,
                        first_constraints,
                        rest_constraints,
                    ));
                    Comparison::Constrainable(and)
                }
                (Comparison::Assignable, Comparison::Castable)
                | (Comparison::Castable, Comparison::Assignable) => Comparison::Castable,
                (
                    Comparison::Assignable | Comparison::Castable,
                    Comparison::Constrainable(constraints),
                )
                | (
                    Comparison::Constrainable(constraints),
                    Comparison::Assignable | Comparison::Castable,
                ) => Comparison::Constrainable(constraints),
                (Comparison::Incompatible, _) | (_, Comparison::Incompatible) => {
                    Comparison::Incompatible
                }
            }
        }
        (Type::Atom(from), Type::Atom(to)) => match (from, to) {
            (Atom::Bytes, Atom::Bytes) => Comparison::Assignable,
            (Atom::Bytes, Atom::Int) => Comparison::Castable,
            (Atom::BytesValue(..), Atom::Bytes) => Comparison::Assignable,
            (Atom::BytesValue(..), Atom::Int) => Comparison::Castable,
            (Atom::Int, Atom::Int) => Comparison::Assignable,
            (Atom::Int, Atom::Bytes) => Comparison::Castable,
            (Atom::IntValue(..), Atom::Int) => Comparison::Assignable,
            (Atom::IntValue(..), Atom::Bytes) => Comparison::Castable,
            (Atom::Bool, Atom::Bool) => Comparison::Assignable,
            (Atom::Bool, Atom::Bytes) => Comparison::Castable,
            (Atom::Bool, Atom::Int) => Comparison::Castable,
            (Atom::Bool, Atom::Bytes32) => Comparison::Incompatible,
            (Atom::Bool, Atom::PublicKey) => Comparison::Incompatible,
            (Atom::BoolValue(..), Atom::Bool) => Comparison::Assignable,
            (Atom::BoolValue(..), Atom::Int) => Comparison::Castable,
            (Atom::BoolValue(..), Atom::Bytes) => Comparison::Castable,
            (Atom::BoolValue(..), Atom::Bytes32) => Comparison::Incompatible,
            (Atom::BoolValue(..), Atom::PublicKey) => Comparison::Incompatible,
            (Atom::Bytes32, Atom::Bytes32) => Comparison::Assignable,
            (Atom::Bytes32, Atom::Bytes) => Comparison::Assignable,
            (Atom::Bytes32, Atom::Int) => Comparison::Castable,
            (Atom::Bytes32, Atom::Bool) => Comparison::Incompatible,
            (Atom::Bytes32, Atom::BoolValue(..)) => Comparison::Incompatible,
            (Atom::Bytes32, Atom::PublicKey) => Comparison::Incompatible,
            (Atom::Bytes32, Atom::Nil) => Comparison::Incompatible,
            (Atom::PublicKey, Atom::PublicKey) => Comparison::Assignable,
            (Atom::PublicKey, Atom::Bytes) => Comparison::Castable,
            (Atom::PublicKey, Atom::Int) => Comparison::Castable,
            (Atom::PublicKey, Atom::Bool) => Comparison::Incompatible,
            (Atom::PublicKey, Atom::BoolValue(..)) => Comparison::Incompatible,
            (Atom::PublicKey, Atom::Bytes32) => Comparison::Incompatible,
            (Atom::PublicKey, Atom::Nil) => Comparison::Incompatible,
            (Atom::Nil, Atom::Nil) => Comparison::Assignable,
            (Atom::Nil, Atom::Bytes) => Comparison::Assignable,
            (Atom::Nil, Atom::Int) => Comparison::Castable,
            (Atom::Nil, Atom::Bytes32) => Comparison::Incompatible,
            (Atom::Nil, Atom::PublicKey) => Comparison::Incompatible,
            (Atom::Nil, Atom::Bool) => Comparison::Castable,
            (Atom::Bytes | Atom::Int, Atom::Bytes32) => {
                let strlen = db.alloc_hir(Hir::Unary(UnaryOp::Strlen, hir));
                let len = db.alloc_hir(Hir::Atom(bigint_atom(BigInt::from(32))));
                let eq = db.alloc_hir(Hir::Binary(BinaryOp::Eq, strlen, len));
                Comparison::Constrainable(eq)
            }
            (Atom::Bytes | Atom::Int, Atom::PublicKey) => {
                let strlen = db.alloc_hir(Hir::Unary(UnaryOp::Strlen, hir));
                let len = db.alloc_hir(Hir::Atom(bigint_atom(BigInt::from(48))));
                let eq = db.alloc_hir(Hir::Binary(BinaryOp::Eq, strlen, len));
                Comparison::Constrainable(eq)
            }
            (Atom::Bytes | Atom::Int, Atom::Bool) => {
                let true_atom = db.alloc_hir(Hir::Atom(vec![1]));
                let false_atom = db.alloc_hir(Hir::Atom(vec![]));
                let eq_true = db.alloc_hir(Hir::Binary(BinaryOp::Eq, hir, true_atom));
                let eq_false = db.alloc_hir(Hir::Binary(BinaryOp::Eq, hir, false_atom));
                let or = db.alloc_hir(Hir::Binary(BinaryOp::Or, eq_true, eq_false));
                Comparison::Constrainable(or)
            }
            (Atom::Bytes | Atom::Int, Atom::BytesValue(value)) => {
                let atom = db.alloc_hir(Hir::Atom(value.clone()));
                let eq = db.alloc_hir(Hir::Binary(BinaryOp::Eq, hir, atom));
                Comparison::Constrainable(eq)
            }
            (Atom::Bool, Atom::BytesValue(value)) => {
                if value.is_empty() || value == [1] {
                    let atom = db.alloc_hir(Hir::Atom(value.clone()));
                    let eq = db.alloc_hir(Hir::Binary(BinaryOp::Eq, hir, atom));
                    Comparison::Constrainable(eq)
                } else {
                    Comparison::Incompatible
                }
            }
            (Atom::Bool, Atom::IntValue(value)) => {
                let atom = bigint_atom(value.clone());
                if atom.is_empty() || atom == [1] {
                    let atom = db.alloc_hir(Hir::Atom(atom));
                    let eq = db.alloc_hir(Hir::Binary(BinaryOp::Eq, hir, atom));
                    Comparison::Constrainable(eq)
                } else {
                    Comparison::Incompatible
                }
            }
            (Atom::Bytes | Atom::Int, Atom::IntValue(value)) => {
                let atom = db.alloc_hir(Hir::Atom(bigint_atom(value.clone())));
                let eq = db.alloc_hir(Hir::Binary(BinaryOp::Eq, hir, atom));
                Comparison::Constrainable(eq)
            }
            (Atom::Bytes | Atom::Int | Atom::Bool, Atom::BoolValue(value)) => {
                let atom = db.alloc_hir(Hir::Atom(if value { vec![1] } else { vec![] }));
                let eq = db.alloc_hir(Hir::Binary(BinaryOp::Eq, hir, atom));
                Comparison::Constrainable(eq)
            }
            (Atom::Bytes | Atom::Int | Atom::Bool, Atom::Nil) => {
                let atom = db.alloc_hir(Hir::Atom(Vec::new()));
                let eq = db.alloc_hir(Hir::Binary(BinaryOp::Eq, hir, atom));
                Comparison::Constrainable(eq)
            }
            (Atom::BytesValue(from), Atom::BytesValue(to)) => {
                if from == to {
                    Comparison::Assignable
                } else {
                    Comparison::Incompatible
                }
            }
            (Atom::IntValue(from), Atom::IntValue(to)) => {
                if bigint_atom(from.clone()) == bigint_atom(to.clone()) {
                    Comparison::Assignable
                } else {
                    Comparison::Incompatible
                }
            }
            (Atom::BoolValue(from), Atom::BoolValue(to)) => {
                if from == to {
                    Comparison::Assignable
                } else {
                    Comparison::Incompatible
                }
            }
            (Atom::BytesValue(from), Atom::Bytes32) => {
                if from.len() == 32 {
                    Comparison::Assignable
                } else {
                    Comparison::Incompatible
                }
            }
            (Atom::IntValue(from), Atom::Bytes32) => {
                if bigint_atom(from.clone()).len() == 32 {
                    Comparison::Castable
                } else {
                    Comparison::Incompatible
                }
            }
            (Atom::BytesValue(from), Atom::PublicKey) => {
                if from.len() == 48 {
                    Comparison::Assignable
                } else {
                    Comparison::Incompatible
                }
            }
            (Atom::IntValue(from), Atom::PublicKey) => {
                if bigint_atom(from.clone()).len() == 48 {
                    Comparison::Castable
                } else {
                    Comparison::Incompatible
                }
            }
            (Atom::BytesValue(bytes), Atom::Nil) | (Atom::Nil, Atom::BytesValue(bytes)) => {
                if bytes.is_empty() {
                    Comparison::Assignable
                } else {
                    Comparison::Incompatible
                }
            }
            (Atom::IntValue(int), Atom::Nil) | (Atom::Nil, Atom::IntValue(int)) => {
                if bigint_atom(int.clone()).is_empty() {
                    Comparison::Castable
                } else {
                    Comparison::Incompatible
                }
            }
            (Atom::BytesValue(from), Atom::Bool) => {
                if from.is_empty() || from == [1] {
                    Comparison::Castable
                } else {
                    Comparison::Incompatible
                }
            }
            (Atom::IntValue(from), Atom::Bool) => {
                let atom = bigint_atom(from.clone());
                if atom.is_empty() || atom == [1] {
                    Comparison::Castable
                } else {
                    Comparison::Incompatible
                }
            }
            (Atom::BytesValue(from), Atom::BoolValue(value))
            | (Atom::BoolValue(value), Atom::BytesValue(from)) => {
                if (from.is_empty() && !value) || (from == [1] && value) {
                    Comparison::Castable
                } else {
                    Comparison::Incompatible
                }
            }
            (Atom::IntValue(from), Atom::BoolValue(value))
            | (Atom::BoolValue(value), Atom::IntValue(from)) => {
                let atom = bigint_atom(from.clone());
                if (atom.is_empty() && !value) || (atom == [1] && value) {
                    Comparison::Castable
                } else {
                    Comparison::Incompatible
                }
            }
            (Atom::BytesValue(bytes), Atom::IntValue(int))
            | (Atom::IntValue(int), Atom::BytesValue(bytes)) => {
                if bigint_atom(int.clone()) == bytes {
                    Comparison::Castable
                } else {
                    Comparison::Incompatible
                }
            }
            (Atom::Bytes32, Atom::BytesValue(bytes)) => {
                if bytes.len() == 32 {
                    let atom = db.alloc_hir(Hir::Atom(bytes.clone()));
                    let eq = db.alloc_hir(Hir::Binary(BinaryOp::Eq, hir, atom));
                    Comparison::Constrainable(eq)
                } else {
                    Comparison::Incompatible
                }
            }
            (Atom::PublicKey, Atom::BytesValue(bytes)) => {
                if bytes.len() == 48 {
                    let atom = db.alloc_hir(Hir::Atom(bytes.clone()));
                    let eq = db.alloc_hir(Hir::Binary(BinaryOp::Eq, hir, atom));
                    Comparison::Constrainable(eq)
                } else {
                    Comparison::Incompatible
                }
            }
            (Atom::Bytes32, Atom::IntValue(int)) => {
                let atom = bigint_atom(int.clone());
                if atom.len() == 32 {
                    let atom = db.alloc_hir(Hir::Atom(atom));
                    let eq = db.alloc_hir(Hir::Binary(BinaryOp::Eq, hir, atom));
                    Comparison::Constrainable(eq)
                } else {
                    Comparison::Incompatible
                }
            }
            (Atom::PublicKey, Atom::IntValue(int)) => {
                let atom = bigint_atom(int.clone());
                if atom.len() == 48 {
                    let atom = db.alloc_hir(Hir::Atom(atom));
                    let eq = db.alloc_hir(Hir::Binary(BinaryOp::Eq, hir, atom));
                    Comparison::Constrainable(eq)
                } else {
                    Comparison::Incompatible
                }
            }
            (Atom::Nil, Atom::BoolValue(value)) | (Atom::BoolValue(value), Atom::Nil) => {
                if value {
                    Comparison::Incompatible
                } else {
                    Comparison::Castable
                }
            }
        },
    }
}

pub fn bigint_atom(value: BigInt) -> Vec<u8> {
    let mut allocator = Allocator::new();
    let atom = allocator.new_number(value).unwrap();
    allocator.atom(atom).to_vec()
}
