use std::env;
use std::fs;
use std::path::Path;
use std::process;
use std::sync::Arc;

use anyhow::Result;
use clvm_tools_rs::classic::clvm_tools::binutils::{assemble, disassemble};
use clvmr::ENABLE_KECCAK_OPS_OUTSIDE_GUARD;
use clvmr::MEMPOOL_MODE;
use clvmr::NodePtr;
use clvmr::error::EvalErr;
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
struct TestFile {
    #[serde(flatten)]
    main: TestCase,

    #[serde(skip_serializing_if = "Vec::is_empty")]
    tests: Vec<TestCase>,

    #[serde(skip_serializing_if = "Vec::is_empty")]
    diagnostics: Vec<String>,
}

#[derive(Debug, Default, Clone, PartialEq, Eq, Serialize, Deserialize)]
#[serde(default)]
struct TestCase {
    #[serde(skip_serializing_if = "Option::is_none")]
    name: Option<String>,

    #[serde(skip_serializing_if = "Option::is_none")]
    program: Option<String>,

    #[serde(skip_serializing_if = "Option::is_none")]
    debug_program: Option<String>,

    #[serde(skip_serializing_if = "Option::is_none")]
    solution: Option<String>,

    #[serde(skip_serializing_if = "Option::is_none")]
    output: Option<String>,

    #[serde(skip_serializing_if = "Option::is_none")]
    debug_output: Option<String>,

    #[serde(skip_serializing_if = "Option::is_none")]
    runtime_cost: Option<u64>,

    #[serde(skip_serializing_if = "Option::is_none")]
    byte_cost: Option<u64>,

    #[serde(skip_serializing_if = "Option::is_none")]
    total_cost: Option<u64>,

    #[serde(skip_serializing_if = "Option::is_none")]
    clvm_error: Option<String>,

    #[serde(skip_serializing_if = "Option::is_none")]
    debug_clvm_error: Option<String>,
}

fn main() -> Result<()> {
    env_logger::init();

    let args: Vec<String> = env::args().collect();
    let filter_arg = args.get(1).cloned();

    let failed = run_tests(filter_arg.as_deref(), ".", true)?;

    if failed {
        process::exit(1);
    }

    Ok(())
}

fn run_tests(filter_arg: Option<&str>, base_path: &str, update: bool) -> Result<bool> {
    let mut failed = false;

    for entry in WalkDir::new(Path::new(base_path).join("tests"))
        .into_iter()
        .chain(WalkDir::new(Path::new(base_path).join("examples")).into_iter())
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
                handle_test_file(name, &entry, &mut failed, update)?;
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

        handle_test_file(name, &entry, &mut failed, update)?;
    }

    Ok(failed)
}

fn handle_test_file(name: &str, entry: &DirEntry, failed: &mut bool, update: bool) -> Result<()> {
    let parent = entry.path().parent().unwrap();
    let yaml_file = parent.join(format!("{name}.yaml"));
    let rue_file = parent.join(format!("{name}.rue"));

    println!("Running {name}");

    let mut file: TestFile = if yaml_file.try_exists()? {
        serde_yml::from_str(&fs::read_to_string(&yaml_file)?)?
    } else {
        TestFile::default()
    };

    let original = file.clone();

    let source = Source::new(
        Arc::from(fs::read_to_string(rue_file)?),
        SourceKind::File("test".to_string()),
    );

    let mut allocator = Allocator::new();

    let result = compile_file(&mut allocator, source.clone(), CompilerOptions::default())?;
    let debug_result = compile_file(&mut allocator, source.clone(), CompilerOptions::debug())?;

    file.diagnostics = Vec::new();

    for diagnostic in result.diagnostics {
        file.diagnostics.push(diagnostic.message());
    }

    handle_test_case(
        &mut allocator,
        &mut file.main,
        result.main.map(|output| output.program),
        debug_result.main.map(|output| output.program),
    )?;

    if file.tests.len() > result.tests.len() {
        file.tests.truncate(result.tests.len());
    }

    for _ in file.tests.len()..result.tests.len() {
        file.tests.push(TestCase::default());
    }

    assert_eq!(debug_result.tests.len(), result.tests.len());

    for (i, test_case) in file.tests.iter_mut().enumerate() {
        test_case.name = Some(result.tests[i].name.clone());

        handle_test_case(
            &mut allocator,
            test_case,
            Some(result.tests[i].output.program),
            Some(debug_result.tests[i].output.program),
        )?;
    }

    if file != original {
        if update {
            eprintln!("Test failed, updated yaml file");
        } else {
            eprintln!("Test failed, moving on");
        }
        *failed = true;
    }

    if update {
        fs::write(yaml_file, serde_yml::to_string(&file)?)?;
    }

    Ok(())
}

fn handle_test_case(
    allocator: &mut Allocator,
    test_case: &mut TestCase,
    program: Option<NodePtr>,
    debug_program: Option<NodePtr>,
) -> Result<()> {
    let Some(ptr) = program else {
        test_case.program = None;
        test_case.debug_program = None;
        test_case.output = None;
        test_case.debug_output = None;
        test_case.clvm_error = None;
        test_case.debug_clvm_error = None;
        test_case.runtime_cost = None;
        test_case.byte_cost = None;
        test_case.total_cost = None;

        return Ok(());
    };

    let debug_ptr = debug_program.unwrap();

    let env = assemble(allocator, test_case.solution.as_deref().unwrap_or("()"))?;

    test_case.program = Some(disassemble(allocator, ptr, None));
    test_case.debug_program = Some(disassemble(allocator, debug_ptr, None));

    let response = run_program(
        allocator,
        &ChiaDialect::new(ENABLE_KECCAK_OPS_OUTSIDE_GUARD | MEMPOOL_MODE),
        ptr,
        env,
        100_000_000,
    );

    let debug_response = run_program(
        allocator,
        &ChiaDialect::new(ENABLE_KECCAK_OPS_OUTSIDE_GUARD | MEMPOOL_MODE),
        debug_ptr,
        env,
        100_000_000,
    );

    let bytes = node_to_bytes(allocator, ptr)?.len();
    test_case.byte_cost = Some(bytes as u64 * 12_000);

    match response {
        Ok(output) => {
            test_case.output = Some(disassemble(allocator, output.1, None));
            test_case.runtime_cost = Some(output.0);
            test_case.total_cost = Some(output.0 + bytes as u64 * 12_000);
            test_case.clvm_error = None;
        }
        Err(error) => {
            test_case.clvm_error = Some(match error {
                EvalErr::Raise(ptr) => format!("clvm raise {}", disassemble(allocator, ptr, None)),
                _ => error.to_string(),
            });
            test_case.runtime_cost = None;
            test_case.total_cost = None;
            test_case.output = None;
        }
    }

    match debug_response {
        Ok(output) => {
            test_case.debug_output = Some(disassemble(allocator, output.1, None));
            test_case.debug_clvm_error = None;
        }
        Err(error) => {
            test_case.debug_clvm_error = Some(match error {
                EvalErr::Raise(ptr) => format!("clvm raise {}", disassemble(allocator, ptr, None)),
                _ => error.to_string(),
            });
            test_case.debug_output = None;
        }
    }

    if test_case.debug_output == test_case.output {
        test_case.debug_output = None;
    }

    if test_case.debug_clvm_error == test_case.clvm_error {
        test_case.debug_clvm_error = None;
    }

    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn tests() -> Result<()> {
        let failed = run_tests(None, "../..", false)?;

        assert!(!failed);

        Ok(())
    }
}
