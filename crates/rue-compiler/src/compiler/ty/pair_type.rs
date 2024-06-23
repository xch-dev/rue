use rue_parser::PairType as Ast;

use crate::{
    compiler::Compiler,
    ty::{PairType, Type},
    TypeId,
};

impl Compiler<'_> {
    pub fn compile_pair_type(&mut self, pair_type: &Ast) -> TypeId {
        let first = pair_type
            .first()
            .map_or(self.builtins.unknown, |ty| self.compile_type(ty));

        let rest = pair_type
            .rest()
            .map_or(self.builtins.unknown, |ty| self.compile_type(ty));

        self.db.alloc_type(Type::Pair(PairType { first, rest }))
    }
}
