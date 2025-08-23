mod alias;
mod apply;
mod atom;
mod pair;
mod structs;
mod unions;

pub use alias::*;
pub use apply::*;
pub use atom::*;
pub use pair::*;
pub use structs::*;
pub use unions::*;

use id_arena::Id;

pub type TypeId = Id<Type>;

#[derive(Debug, Clone)]
pub enum Type {
    Unresolved,
    Generic,
    Ref(TypeId),
    Atom(Atom),
    Pair(Pair),
    Alias(Alias),
    Struct(Struct),
    Apply(Apply),
    Union(Union),
}
