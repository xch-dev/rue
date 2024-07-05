use clvm_tools_rs::classic::clvm_tools::binutils;
use clvmr::{
    serde::{node_from_bytes, node_to_bytes},
    Allocator, NodePtr,
};

use crate::Result;

pub fn stringify_clvm(allocator: &Allocator, ptr: NodePtr) -> Result<String> {
    let mut old_allocator = clvmr_old::allocator::Allocator::new();
    let output_bytes = node_to_bytes(allocator, ptr)?;
    let output_ptr = clvmr_old::serde::node_from_bytes(&mut old_allocator, &output_bytes)?;
    Ok(binutils::disassemble(&old_allocator, output_ptr, None))
}

pub fn parse_clvm(allocator: &mut Allocator, source: &str) -> Result<NodePtr> {
    let mut old_allocator = clvmr_old::Allocator::new();
    let input_ptr = binutils::assemble(&mut old_allocator, source).unwrap();
    let input_bytes = clvmr_old::serde::node_to_bytes(&old_allocator, input_ptr).unwrap();
    Ok(node_from_bytes(allocator, &input_bytes)?)
}
