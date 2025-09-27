use id_arena::Arena;

use crate::{Lir, LirId};

pub fn opt_truthy(arena: &mut Arena<Lir>, value: LirId) -> Result<bool, LirId> {
    match arena[value].clone() {
        Lir::Atom(atom) => Ok(!atom.is_empty()),
        Lir::Cons(..) => Ok(true),
        Lir::Listp(inner, atom_can_be_truthy) => {
            if atom_can_be_truthy {
                Err(value)
            } else {
                opt_truthy(arena, inner)
            }
        }
        Lir::Not(inner) => {
            if let Lir::Eq(left, right) = arena[inner].clone() {
                if let Lir::Atom(left) = arena[left].clone()
                    && left.is_empty()
                {
                    return Err(right);
                }

                if let Lir::Atom(right) = arena[right].clone()
                    && right.is_empty()
                {
                    return Err(left);
                }
            }

            Err(value)
        }
        _ => Err(value),
    }
}
