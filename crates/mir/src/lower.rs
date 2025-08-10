use id_arena::Arena;
use rue_lir::{Lir, LirId, first_path, rest_path};

use crate::{Mir, MirId};

#[derive(Debug, Clone)]
pub struct MirContext {
    bindings: usize,
    captures: usize,
    parameters: usize,
}

pub fn lower_mir(
    input: &Arena<Mir>,
    arena: &mut Arena<Lir>,
    mir: MirId,
    context: Option<&MirContext>,
) -> LirId {
    match input[mir].clone() {
        Mir::Atom(atom) => arena.alloc(Lir::Atom(atom)),
        Mir::Function {
            body,
            captures,
            parameters,
        } => {
            let body = lower_mir(
                input,
                arena,
                body,
                Some(&MirContext {
                    bindings: 0,
                    captures,
                    parameters,
                }),
            );
            arena.alloc(Lir::Quote(body))
        }
        Mir::Bind { bindings, body } => {
            // TODO: Do we need to unwrap the context?
            let context = context.unwrap();
            let body = lower_mir(
                input,
                arena,
                body,
                Some(&MirContext {
                    bindings: context.bindings + bindings.len(),
                    captures: context.captures,
                    parameters: context.parameters,
                }),
            );
            let bindings = bindings
                .into_iter()
                .map(|binding| lower_mir(input, arena, binding, Some(context)))
                .collect();
            arena.alloc(Lir::Bind(body, bindings))
        }
        Mir::Binding(binding) => {
            let path = calculate_path(context.unwrap(), PathKind::Binding, binding);
            arena.alloc(Lir::Path(path))
        }
        Mir::Capture(capture) => {
            let path = calculate_path(context.unwrap(), PathKind::Capture, capture);
            arena.alloc(Lir::Path(path))
        }
        Mir::Parameter(parameter) => {
            let path = calculate_path(context.unwrap(), PathKind::Parameter, parameter);
            arena.alloc(Lir::Path(path))
        }
        Mir::Closure { body, captures } => {
            let body = lower_mir(input, arena, body, context);
            let captures = captures
                .into_iter()
                .map(|capture| lower_mir(input, arena, capture, context))
                .collect();
            arena.alloc(Lir::Closure(body, captures))
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
enum PathKind {
    Binding,
    Capture,
    Parameter,
}

fn calculate_path(context: &MirContext, kind: PathKind, index: usize) -> u32 {
    let mut path = 2;

    if kind > PathKind::Binding {
        for _ in 0..context.bindings {
            path = first_path(rest_path(path));
        }
    }

    if kind > PathKind::Capture {
        for _ in 0..context.captures {
            path = first_path(rest_path(path));
        }
    }

    for _ in 0..index {
        path = first_path(rest_path(path));
    }

    path
}
