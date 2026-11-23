//! Lowers the program that a function is compiled to on its own, which is the value of the
//! function where it's used as one (see [`crate::program_value`]).

use std::sync::Arc;

use clvmr::{Allocator, serde::node_to_bytes};
use id_arena::Arena;
use rue_lir::{ClvmTarget, CodegenOptions, Lir, LirId, codegen, optimize};

use crate::{DependencyGraph, Environment, SymbolId, clvm_target};

use super::{Lowerer, TooDeep};

impl Lowerer<'_, '_, '_> {
    /// Lowers the program that the function is compiled to on its own, which is a constant. It's
    /// compiled the first time it's needed for the target, with the program options (see
    /// [`Self::with_program_options`]).
    pub(super) fn lower_program_value(&mut self, function: SymbolId) -> LirId {
        let target = clvm_target(&self.program_options);

        let program = match self.db.program(function, target) {
            Some(program) => program,
            None => match self.compile_program(function, target) {
                Ok(program) => {
                    self.db.set_program(function, target, program.clone());
                    program
                }
                Err(too_deep) => {
                    self.too_deep.get_or_insert(too_deep);
                    return self.arena.alloc(Lir::Atom(vec![]));
                }
            },
        };

        self.arena.alloc(Lir::Value(program))
    }

    /// Compiles the function on its own, as the entrypoint of a program. This must be the same
    /// program as the one `rue build` compiles the function to as an export (see `codegen` in
    /// `rue-compiler`'s `file.rs`), since its tree hash is the same as that one's.
    fn compile_program(
        &mut self,
        function: SymbolId,
        target: ClvmTarget,
    ) -> Result<Arc<[u8]>, TooDeep> {
        let options = self.program_options;
        let base_path = self.base_path.clone();

        let graph = DependencyGraph::build(self.db, function, options);
        let mut arena = Arena::new();

        let lir = {
            let mut lowerer =
                Lowerer::new(self.db, &mut arena, &graph, options, function, base_path);
            let lir = lowerer.lower_main(&Environment::default());

            if let Some(too_deep) = lowerer.too_deep {
                return Err(too_deep);
            }

            lir
        };

        let lir = optimize(&mut arena, lir, target);

        // Generating and serializing code only fails if the allocator runs out of room, which the
        // program would have to be nested far too deeply to reach
        let mut allocator = Allocator::new();

        codegen(
            &arena,
            &mut allocator,
            lir,
            CodegenOptions {
                optimize_static_pairs: options.optimize_static_pairs,
                target,
            },
        )
        .ok()
        .and_then(|ptr| node_to_bytes(&allocator, ptr).ok())
        .map(Arc::from)
        .ok_or_else(|| TooDeep {
            srcloc: self
                .db
                .symbol(function)
                .name()
                .and_then(|name| name.srcloc().cloned()),
        })
    }
}
