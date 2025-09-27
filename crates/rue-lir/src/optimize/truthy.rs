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
        _ => Err(value),
    }
}
