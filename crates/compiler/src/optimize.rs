use id_arena::Arena;
use rue_lir::{Lir, LirId};

use crate::{BinaryOp, Context, Mir, MirId, UnaryOp};

pub fn optimize(ctx: &mut Context, arena: &mut Arena<Lir>, mir: MirId) -> LirId {
    match ctx.mir(mir).clone() {
        Mir::Unresolved => unreachable!(),
        Mir::Atom(atom) => arena.alloc(Lir::Atom(atom)),
        Mir::Reference(symbol) => todo!(),
        Mir::Unary(op, mir) => {
            let lir = optimize(ctx, arena, mir);
            match op {
                UnaryOp::Listp => arena.alloc(Lir::Listp(lir)),
                UnaryOp::Not => arena.alloc(Lir::Not(lir)),
            }
        }
        Mir::Binary(op, left, right) => {
            let left = optimize(ctx, arena, left);
            let right = optimize(ctx, arena, right);
            match op {
                BinaryOp::Add => arena.alloc(Lir::Add(vec![left, right])),
                BinaryOp::Sub => arena.alloc(Lir::Sub(vec![left, right])),
                BinaryOp::Mul => arena.alloc(Lir::Mul(vec![left, right])),
                BinaryOp::Div => arena.alloc(Lir::Div(left, right)),
            }
        }
        Mir::Environment(..) => todo!(),
    }
}
