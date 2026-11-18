use std::{
    collections::{HashMap, hash_map::Entry},
    path::Path,
    sync::Arc,
};

use clvmr::{Allocator, NodePtr};
use id_arena::Arena;
use rue_hir::{Symbol, SymbolId};
use rue_types::{DisplayOptions, Type, TypeId, display_value_without_substitution, substitute};

use crate::Compiler;

/// The types of the values that compiled programs produce, which are used to display those values
/// as Rue expressions once the programs have run. This includes the return types of functions
/// (such as `main` and tests) and the types of the values that `debug` statements print.
///
/// It keeps its own copy of the types, so it can outlive the compiler and be shared with the
/// dialect that runs the program.
#[derive(Debug, Clone)]
pub struct OutputTypes {
    arena: Arc<Arena<Type>>,
    returns: HashMap<SymbolId, TypeId>,
    /// The type printed by the `debug` statement at each location, as the location is displayed in
    /// debug builds. Locations that print values of more than one type (such as a statement in a
    /// function that's compiled more than once) are missing a type, since it's unclear which
    /// applies.
    debug: HashMap<String, Option<TypeId>>,
}

impl OutputTypes {
    /// Collects the return types of the given functions, along with the types printed by every
    /// `debug` statement. The locations of `debug` statements are displayed relative to
    /// `base_path`, which should be the same path that programs are compiled with.
    pub fn new(
        ctx: &mut Compiler,
        functions: impl IntoIterator<Item = SymbolId>,
        base_path: &Path,
    ) -> Self {
        let mut returns = HashMap::new();

        for symbol in functions {
            if let Symbol::Function(function) = ctx.symbol(symbol) {
                let ty = function.return_type;
                returns.insert(symbol, substitute(ctx.types_mut(), ty));
            }
        }

        let mut debug = HashMap::new();

        for (srcloc, ty) in ctx.debug_types().to_vec() {
            let ty = substitute(ctx.types_mut(), ty);

            match debug.entry(srcloc.display(base_path)) {
                Entry::Vacant(entry) => {
                    entry.insert(Some(ty));
                }
                Entry::Occupied(mut entry) => {
                    if *entry.get() != Some(ty) {
                        entry.insert(None);
                    }
                }
            }
        }

        Self {
            arena: Arc::new(ctx.types().clone()),
            returns,
            debug,
        }
    }

    /// Displays the value returned by a function, if its return type was collected.
    pub fn display_return(
        &self,
        function: SymbolId,
        allocator: &Allocator,
        node: NodePtr,
        options: DisplayOptions,
    ) -> Option<String> {
        let ty = *self.returns.get(&function)?;
        Some(display_value_without_substitution(
            &self.arena,
            ty,
            allocator,
            node,
            options,
        ))
    }

    /// Displays the value printed by the `debug` statement at the location, if its type is known.
    pub fn display_debug(
        &self,
        srcloc: &str,
        allocator: &Allocator,
        node: NodePtr,
        options: DisplayOptions,
    ) -> Option<String> {
        let ty = (*self.debug.get(srcloc)?)?;
        Some(display_value_without_substitution(
            &self.arena,
            ty,
            allocator,
            node,
            options,
        ))
    }
}
