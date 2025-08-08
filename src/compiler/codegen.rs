use clvm_traits::{ToClvm, clvm_list};
use clvmr::{Allocator, NodePtr};

use crate::{Context, Mir, MirId, UnaryOp};

pub fn codegen(ctx: &mut Context, allocator: &mut Allocator, mir: MirId) -> NodePtr {
    match ctx.mir(mir).clone() {
        Mir::Unresolved => todo!(),
        Mir::Atom(atom) => allocator.new_atom(&atom).unwrap(),
        Mir::Unary(op, mir) => {
            let arg = codegen(ctx, allocator, mir);
            match op {
                UnaryOp::Listp => clvm_list!(7, arg).to_clvm(allocator).unwrap(),
                UnaryOp::Not => clvm_list!(32, arg).to_clvm(allocator).unwrap(),
            }
        }
    }
}
