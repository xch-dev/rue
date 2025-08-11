use id_arena::Arena;
use rue_lir::{Lir, LirId, first_path, rest_path};

use crate::{BinaryOp, Mir, MirId, UnaryOp};

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
        Mir::Curry { body, args } => {
            let body = lower_mir(input, arena, body, context);
            let args = args
                .into_iter()
                .map(|capture| lower_mir(input, arena, capture, context))
                .collect();
            arena.alloc(Lir::Bind(body, args))
        }
        Mir::Unary(op, mir) => {
            let lir = lower_mir(input, arena, mir, context);
            match op {
                UnaryOp::Listp => arena.alloc(Lir::Listp(lir)),
                UnaryOp::First => arena.alloc(Lir::First(lir)),
                UnaryOp::Rest => arena.alloc(Lir::Rest(lir)),
                UnaryOp::Strlen => arena.alloc(Lir::Strlen(lir)),
                UnaryOp::Not => arena.alloc(Lir::Not(lir)),
            }
        }
        Mir::Binary(op, left, right) => {
            let left = lower_mir(input, arena, left, context);
            let right = lower_mir(input, arena, right, context);
            match op {
                BinaryOp::Add => arena.alloc(Lir::Add(vec![left, right])),
                BinaryOp::Sub => arena.alloc(Lir::Sub(vec![left, right])),
                BinaryOp::Mul => arena.alloc(Lir::Mul(vec![left, right])),
                BinaryOp::Div => arena.alloc(Lir::Div(left, right)),
                BinaryOp::Eq => arena.alloc(Lir::Eq(left, right)),
                BinaryOp::And => {
                    let true_atom = arena.alloc(Lir::Atom(vec![1]));
                    let false_atom = arena.alloc(Lir::Atom(vec![]));
                    let right = arena.alloc(Lir::If(right, true_atom, false_atom));
                    arena.alloc(Lir::If(left, right, false_atom))
                }
                BinaryOp::Or => {
                    let true_atom = arena.alloc(Lir::Atom(vec![1]));
                    let false_atom = arena.alloc(Lir::Atom(vec![]));
                    let right = arena.alloc(Lir::If(right, true_atom, false_atom));
                    arena.alloc(Lir::If(left, true_atom, right))
                }
            }
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
