use rue_parser::PairType;
use rue_typing::{Type, TypeId};

use crate::compiler::Compiler;

impl Compiler<'_> {
    pub fn compile_pair_type(&mut self, pair_type: &PairType) -> TypeId {
        let first = pair_type
            .first()
            .map_or(self.ty.std().unknown, |ty| self.compile_type(ty));

        let rest = pair_type
            .rest()
            .map_or(self.ty.std().unknown, |ty| self.compile_type(ty));

        self.ty.alloc(Type::Pair(first, rest))
    }
}
