//! Lowers a function that's used as a value to the program it's compiled to on its own (see
//! [`crate::program_value`]).

use std::{cell::RefCell, collections::HashMap, rc::Rc};

use clvmr::{Allocator, serde::node_to_bytes};
use id_arena::Arena;
use rue_lir::{ClvmTarget, CodegenOptions, Lir, LirId, codegen, optimize};

use crate::{DependencyGraph, Environment, SymbolId};

use super::{Lowerer, TooDeep};

/// The programs that functions used as values have been compiled to, as serialized CLVM, shared
/// by the lowerers that compile them (see [`Lowerer::lower_program_value`]).
pub(super) type Programs = Rc<RefCell<HashMap<SymbolId, Rc<[u8]>>>>;

impl Lowerer<'_, '_, '_> {
    /// Lowers a function that's used as a value to the program it's compiled to on its own, which
    /// is a constant. It's compiled the first time, with the program options (see
    /// [`Self::with_program_options`]), the same way as a program's entrypoint is.
    pub(super) fn lower_program_value(&mut self, symbol: SymbolId) -> LirId {
        let cached = self.programs.borrow().get(&symbol).cloned();

        let program = match cached {
            Some(program) => program,
            None => match self.compile_program(symbol) {
                Ok(program) => {
                    self.programs.borrow_mut().insert(symbol, program.clone());
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

    fn compile_program(&mut self, symbol: SymbolId) -> Result<Rc<[u8]>, TooDeep> {
        let options = self.program_options;
        let programs = self.programs.clone();
        let base_path = self.base_path.clone();

        let graph = DependencyGraph::build(self.db, symbol, options);
        let mut arena = Arena::new();

        let lir = {
            let mut lowerer = Lowerer::new(self.db, &mut arena, &graph, options, symbol, base_path);
            lowerer.programs = programs;

            let lir = lowerer.lower_main(&Environment::default());

            if let Some(too_deep) = lowerer.too_deep {
                return Err(too_deep);
            }

            lir
        };

        let target = ClvmTarget {
            operand_limits: options.operand_limits,
            hard_fork_2: options.hard_fork_2,
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
        .map(Rc::from)
        .ok_or_else(|| TooDeep {
            srcloc: self
                .db
                .symbol(symbol)
                .name()
                .and_then(|name| name.srcloc().cloned()),
        })
    }
}
