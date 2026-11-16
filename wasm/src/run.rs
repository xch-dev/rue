use std::path::Path;

use clvmr::{Allocator, NodePtr, SExp, error::EvalErr, run_program, serde::node_from_bytes};
use rue_clvm::{assemble, disassemble_value};
use rue_lir::{CLVM_FLAGS, COST_PER_BYTE, DebugDialect};
use wasm_bindgen::prelude::*;

use crate::check;

/// The cost limit for running a program, which is the same as Chia's block cost limit.
const MAX_COST: u64 = 11_000_000_000;

/// The result of running a program.
#[derive(Debug, Clone)]
#[wasm_bindgen(getter_with_clone)]
pub struct Execution {
    /// The value it returned.
    pub output: Option<String>,
    /// Why it failed, such as `raise "assertion failed at main.rue:2:5"`.
    pub error: Option<String>,
    /// The cost of running it, which is missing if it failed.
    #[wasm_bindgen(js_name = runtimeCost)]
    pub runtime_cost: Option<f64>,
    /// The cost of the size of the serialized program.
    #[wasm_bindgen(js_name = byteCost)]
    pub byte_cost: f64,
    /// The values of `debug` statements it evaluated, in order. Only debug builds keep them.
    pub prints: Vec<Print>,
}

/// A value printed by a `debug` statement.
#[derive(Debug, Clone)]
#[wasm_bindgen(getter_with_clone)]
pub struct Print {
    /// The location of the statement, such as `main.rue:3:5`.
    pub location: String,
    pub value: String,
}

/// Runs a serialized program (as hex) with a solution written as CLVM, such as `(5 "hello")`.
#[wasm_bindgen]
pub fn run(program: &str, solution: &str) -> Result<Execution, JsError> {
    console_error_panic_hook::set_once();

    let mut allocator = Allocator::new();

    let bytes = hex::decode(program.trim())?;
    let program = node_from_bytes(&mut allocator, &bytes)?;
    let byte_cost = bytes.len() as u64 * COST_PER_BYTE;

    let solution = if solution.trim().is_empty() {
        NodePtr::NIL
    } else {
        assemble(&mut allocator, solution)
            .map_err(|error| JsError::new(&format!("Invalid solution: {error}")))?
    };

    let dialect = DebugDialect::new(CLVM_FLAGS, false);
    let result = run_program(&mut allocator, &dialect, program, solution, MAX_COST);

    let prints = dialect
        .log()
        .into_iter()
        .map(|print| Print {
            location: print.srcloc,
            value: print.message,
        })
        .collect();

    let (output, error, runtime_cost) = match result {
        Ok(reduction) => (
            Some(disassemble_value(&allocator, reduction.1)),
            None,
            Some(reduction.0),
        ),
        Err(error) => (None, Some(describe_error(&allocator, &error)), None),
    };

    #[allow(clippy::cast_precision_loss)]
    Ok(Execution {
        output,
        error,
        runtime_cost: runtime_cost.map(|cost| cost as f64),
        byte_cost: byte_cost as f64,
        prints,
    })
}

/// The result of running a test.
#[derive(Debug, Clone)]
#[wasm_bindgen(getter_with_clone)]
pub struct TestResult {
    pub name: String,
    /// Why it failed, if it did.
    pub error: Option<String>,
    pub prints: Vec<Print>,
}

/// Compiles the source in debug mode and runs its tests, the way `rue test` does. Returns nothing
/// if it has errors, which `compile` reports.
#[wasm_bindgen(js_name = runTests)]
pub fn run_tests(source: String) -> Result<Vec<TestResult>, JsError> {
    console_error_panic_hook::set_once();

    let mut allocator = Allocator::new();
    let (mut ctx, tree, kind, _) = check(source, true);

    if ctx.has_errors() {
        return Ok(Vec::new());
    }

    let tests = tree.tests(
        &mut ctx,
        &mut allocator,
        Some(&kind),
        None,
        Path::new("."),
        false,
    )?;

    let mut results = Vec::new();

    for test in tests {
        let Some(name) = test.name else {
            continue;
        };

        let dialect = DebugDialect::new(CLVM_FLAGS, false);
        let result = run_program(&mut allocator, &dialect, test.ptr, NodePtr::NIL, MAX_COST);

        let error = match result {
            Ok(reduction) => {
                if let SExp::Atom = allocator.sexp(reduction.1)
                    && allocator.atom(reduction.1).is_empty()
                {
                    None
                } else {
                    Some(format!(
                        "returned {} instead of nil",
                        disassemble_value(&allocator, reduction.1)
                    ))
                }
            }
            Err(error) => Some(describe_error(&allocator, &error)),
        };

        results.push(TestResult {
            name,
            error,
            prints: dialect
                .log()
                .into_iter()
                .map(|print| Print {
                    location: print.srcloc,
                    value: print.message,
                })
                .collect(),
        });
    }

    Ok(results)
}

fn describe_error(allocator: &Allocator, error: &EvalErr) -> String {
    match error {
        EvalErr::Raise(value) => format!("raise {}", disassemble_value(allocator, *value)),
        EvalErr::CostExceeded => format!("{error} (the cost limit is {MAX_COST})"),
        _ => error.to_string(),
    }
}
