use std::env;
use std::fs;
use std::sync::Arc;

use anyhow::Result;
use clvm_tools_rs::classic::clvm_tools::binutils::{assemble, disassemble};
use clvm_utils::tree_hash;
use clvmr::ENABLE_KECCAK_OPS_OUTSIDE_GUARD;
use clvmr::MEMPOOL_MODE;
use clvmr::NodePtr;
use clvmr::SExp;
use clvmr::{Allocator, ChiaDialect, run_program, serde::node_to_bytes};
use rue_compiler::compile_file;
use rue_diagnostic::Source;
use rue_diagnostic::SourceKind;
use rue_options::CompilerOptions;
use serde::{Deserialize, Serialize};
use walkdir::DirEntry;
use walkdir::WalkDir;

#[derive(Debug, Default, Clone, PartialEq, Eq, Serialize, Deserialize)]
#[serde(default)]
struct TestCase {
    #[serde(skip_serializing_if = "Option::is_none")]
    program: Option<String>,

    #[serde(skip_serializing_if = "Option::is_none")]
    debug_program: Option<String>,

    #[serde(skip_serializing_if = "Option::is_none")]
    solution: Option<String>,

    #[serde(skip_serializing_if = "Option::is_none")]
    output: Option<String>,

    #[serde(skip_serializing_if = "Option::is_none")]
    runtime_cost: Option<u64>,

    #[serde(skip_serializing_if = "Option::is_none")]
    byte_cost: Option<u64>,

    #[serde(skip_serializing_if = "Option::is_none")]
    total_cost: Option<u64>,

    #[serde(skip_serializing_if = "Option::is_none")]
    clvm_error: Option<String>,

    #[serde(skip_serializing_if = "Vec::is_empty")]
    diagnostics: Vec<String>,

    tests_passed: bool,
}

fn main() -> Result<()> {
    env_logger::init();

    let args: Vec<String> = env::args().collect();
    let filter_arg = args.get(1).cloned();

    for entry in WalkDir::new("tests")
        .into_iter()
        .chain(WalkDir::new("examples").into_iter())
    {
        let entry = entry?;

        if let Some(name) = entry.file_name().to_str().unwrap().strip_suffix(".rue") {
            if !entry
                .path()
                .parent()
                .unwrap()
                .join(format!("{name}.yaml"))
                .try_exists()?
            {
                handle_test_case(name, &entry)?;
            }
            continue;
        }

        let Some(name) = entry.file_name().to_str().unwrap().strip_suffix(".yaml") else {
            continue;
        };

        // If a filter argument is provided, skip tests that don't contain it
        if let Some(ref filter) = filter_arg
            && !name.contains(filter)
        {
            continue;
        }

        handle_test_case(name, &entry)?;
    }

    Ok(())
}

fn handle_test_case(name: &str, entry: &DirEntry) -> Result<()> {
    let parent = entry.path().parent().unwrap();
    let yaml_file = parent.join(format!("{name}.yaml"));
    let rue_file = parent.join(format!("{name}.rue"));

    println!("Running {name}");

    let mut test_case: TestCase = if yaml_file.try_exists()? {
        serde_yml::from_str(&fs::read_to_string(&yaml_file)?)?
    } else {
        TestCase::default()
    };

    let original = test_case.clone();

    let source = fs::read_to_string(rue_file)?;

    let source = Source::new(Arc::from(source), SourceKind::File("test".to_string()));

    let mut allocator = Allocator::new();
    let result = compile_file(&mut allocator, source.clone(), CompilerOptions::default())?;

    let mut diagnostics = Vec::new();

    for diagnostic in result.diagnostics {
        diagnostics.push(diagnostic.message());
    }

    if let Some(ptr) = result.program {
        let env = assemble(
            &mut allocator,
            test_case.solution.as_ref().expect("missing solution"),
        )
        .expect("failed to assemble env");

        test_case.program = Some(disassemble(&allocator, ptr, None));

        let debug_ptr = compile_file(&mut allocator, source, CompilerOptions::debug())?
            .program
            .unwrap();

        test_case.debug_program = Some(disassemble(&allocator, debug_ptr, None));

        let response = run_program(
            &mut allocator,
            &ChiaDialect::new(ENABLE_KECCAK_OPS_OUTSIDE_GUARD | MEMPOOL_MODE),
            ptr,
            env,
            100_000_000,
        );
        let debug_response = run_program(
            &mut allocator,
            &ChiaDialect::new(ENABLE_KECCAK_OPS_OUTSIDE_GUARD | MEMPOOL_MODE),
            debug_ptr,
            env,
            100_000_000,
        );

        let bytes = node_to_bytes(&allocator, ptr)?.len();
        test_case.byte_cost = Some(bytes as u64 * 12_000);

        match response {
            Ok(output) => {
                let output_hash = tree_hash(&allocator, output.1);
                let debug_output_hash =
                    tree_hash(&allocator, debug_response.expect("debug program failed").1);

                if output_hash != debug_output_hash {
                    println!(
                        "Debug output mismatch: {}",
                        disassemble(&allocator, debug_ptr, None)
                    );
                }

                test_case.output = Some(disassemble(&allocator, output.1, None));
                test_case.runtime_cost = Some(output.0);
                test_case.total_cost = Some(output.0 + bytes as u64 * 12_000);
                test_case.clvm_error = None;
            }
            Err(error) => {
                debug_response.expect_err("debug program succeeded unexpectedly");

                test_case.clvm_error = Some(error.to_string());
                test_case.runtime_cost = None;
                test_case.total_cost = None;
                test_case.output = None;
            }
        }
    } else {
        test_case.program = None;
        test_case.debug_program = None;
        test_case.output = None;
        test_case.clvm_error = None;
        test_case.runtime_cost = None;
        test_case.byte_cost = None;
        test_case.total_cost = None;
    }

    test_case.tests_passed = true;

    for test in result.tests {
        let response = run_program(
            &mut allocator,
            &ChiaDialect::new(ENABLE_KECCAK_OPS_OUTSIDE_GUARD | MEMPOOL_MODE),
            test,
            NodePtr::NIL,
            100_000_000,
        );

        if let Ok(output) = response
            && let SExp::Atom = allocator.sexp(output.1)
            && allocator.atom(output.1).is_empty()
        {
            continue;
        }

        test_case.tests_passed = false;
        break;
    }

    test_case.diagnostics = diagnostics;

    if test_case != original {
        println!("Failed, updated test case");
    }

    fs::write(yaml_file, serde_yml::to_string(&test_case)?)?;

    Ok(())
}
