use id_arena::Arena;

use crate::{Lir, LirId, stack::grow_stack};

/// Returns `true` if evaluating the generated code for this LIR node may read
/// the environment it's run in. This is conservative, so it can return `true`
/// even if the environment isn't actually observable.
///
/// The cases follow what codegen emits for each node:
/// - Quoted values are not evaluated, so they never read the environment.
/// - `Run` evaluates the callee and environment in the current environment,
///   but the program itself is run in the new environment, so only the callee
///   and environment expressions matter.
/// - `Closure` evaluates the function and its captures in the current environment,
///   and builds a new program which isn't run here.
/// - Non-inline `If` is emitted as `(a (i c (q . T) (q . E)) 1)`, which passes the
///   current environment through to the chosen branch. Reading path 1 never fails,
///   so it only reads the environment if the condition or a branch does.
/// - `Op` builds a program and runs it in a nil environment, so only the arguments
///   matter.
pub fn reads_env(arena: &Arena<Lir>, lir: LirId) -> bool {
    grow_stack(|| reads_env_step(arena, lir))
}

fn reads_env_step(arena: &Arena<Lir>, lir: LirId) -> bool {
    match &arena[lir] {
        Lir::Atom(_) | Lir::Quote(_) => false,
        Lir::Path(_) => true,
        node => node.children().any(|child| reads_env(arena, child)),
    }
}
