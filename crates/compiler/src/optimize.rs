use crate::{BinaryOp, Context, Lir, LirId, Mir, MirId, UnaryOp};

pub fn optimize(ctx: &mut Context, mir: MirId) -> LirId {
    match ctx.mir(mir).clone() {
        Mir::Unresolved => unreachable!(),
        Mir::Atom(atom) => ctx.alloc_lir(Lir::Atom(atom)),
        Mir::Reference(symbol) => todo!(),
        Mir::Unary(op, mir) => {
            let lir = optimize(ctx, mir);
            match op {
                UnaryOp::Listp => ctx.alloc_lir(Lir::Listp(lir)),
                UnaryOp::Not => ctx.alloc_lir(Lir::Not(lir)),
            }
        }
        Mir::Binary(op, left, right) => {
            let left = optimize(ctx, left);
            let right = optimize(ctx, right);
            match op {
                BinaryOp::Add => ctx.alloc_lir(Lir::Add(vec![left, right])),
                BinaryOp::Sub => ctx.alloc_lir(Lir::Sub(vec![left, right])),
                BinaryOp::Mul => ctx.alloc_lir(Lir::Mul(vec![left, right])),
                BinaryOp::Div => ctx.alloc_lir(Lir::Div(left, right)),
            }
        }
        Mir::Environment(..) => todo!(),
    }
}
