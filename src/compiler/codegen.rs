use clvm_traits::{ToClvm, clvm_list, clvm_quote};
use clvmr::{Allocator, NodePtr};

use crate::{BinaryOp, Context, Mir, MirId, UnaryOp};

pub fn codegen(ctx: &mut Context, allocator: &mut Allocator, mir: MirId) -> NodePtr {
    match ctx.mir(mir).clone() {
        Mir::Unresolved => todo!(),
        Mir::Atom(atom) => {
            let atom = allocator.new_atom(&atom).unwrap();
            clvm_quote!(atom).to_clvm(allocator).unwrap()
        }
        Mir::Unary(op, mir) => {
            let arg = codegen(ctx, allocator, mir);
            match op {
                UnaryOp::Listp => clvm_list!(7, arg).to_clvm(allocator).unwrap(),
                UnaryOp::Not => clvm_list!(32, arg).to_clvm(allocator).unwrap(),
            }
        }
        Mir::Binary(op, left, right) => {
            let left = codegen(ctx, allocator, left);
            let right = codegen(ctx, allocator, right);
            match op {
                BinaryOp::Add => clvm_list!(16, left, right).to_clvm(allocator).unwrap(),
                BinaryOp::Sub => clvm_list!(17, left, right).to_clvm(allocator).unwrap(),
                BinaryOp::Mul => clvm_list!(18, left, right).to_clvm(allocator).unwrap(),
                BinaryOp::Div => clvm_list!(19, left, right).to_clvm(allocator).unwrap(),
            }
        }
    }
}
