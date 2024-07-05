use clvmr::{
    reduction::{EvalErr, Reduction},
    run_program, Allocator, ChiaDialect, NodePtr, ENABLE_BLS_OPS_OUTSIDE_GUARD, ENABLE_FIXED_DIV,
};

pub fn run_clvm(
    allocator: &mut Allocator,
    program: NodePtr,
    environment: NodePtr,
    max_cost: u64,
) -> Result<(NodePtr, u64), EvalErr> {
    let Reduction(cost, ptr) = run_program(
        allocator,
        &ChiaDialect::new(ENABLE_BLS_OPS_OUTSIDE_GUARD | ENABLE_FIXED_DIV),
        program,
        environment,
        max_cost,
    )?;
    Ok((ptr, cost))
}
