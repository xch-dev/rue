mod alias;
mod apply;
mod atom;
mod pair;
mod structs;

pub use alias::*;
pub use apply::*;
pub use atom::*;
pub use pair::*;
pub use structs::*;

use id_arena::Id;

pub type TypeId = Id<Type>;

#[derive(Debug, Clone)]
pub enum Type {
    Unresolved,
    Generic,
    Atom(Atom),
    Pair(Pair),
    Alias(Alias),
    Struct(Struct),
    Apply(Apply),
}
