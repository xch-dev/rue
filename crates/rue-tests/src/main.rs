use std::fs;

use anyhow::Result;
use clvm_tools_rs::classic::clvm_tools::binutils::{assemble, disassemble};
use clvmr::{Allocator, ChiaDialect, run_program, serde::node_to_bytes};
use rue_compiler::compile_file;
use serde::{Deserialize, Serialize};
use walkdir::WalkDir;

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
struct TestCase {
    #[serde(default, skip_serializing_if = "Option::is_none")]
    program: Option<String>,

    #[serde(default, skip_serializing_if = "Option::is_none")]
    solution: Option<String>,

    #[serde(default, skip_serializing_if = "Option::is_none")]
    output: Option<String>,

    #[serde(default, skip_serializing_if = "Option::is_none")]
    cost: Option<u64>,

    #[serde(default, skip_serializing_if = "Option::is_none")]
    clvm_error: Option<String>,

    #[serde(default, skip_serializing_if = "Vec::is_empty")]
    diagnostics: Vec<String>,
}

fn main() -> Result<()> {
    for entry in WalkDir::new("tests") {
        let entry = entry?;

        if let Some(name) = entry.file_name().to_str().unwrap().strip_suffix(".rue") {
            if !entry
                .path()
                .parent()
                .unwrap()
                .join(format!("{name}.yaml"))
                .try_exists()?
            {
                println!("Skipping {name} because it doesn't have a test case");
            }
            continue;
        };

        let Some(name) = entry.file_name().to_str().unwrap().strip_suffix(".yaml") else {
            continue;
        };

        if name != "conditions" {
            continue;
        }

        println!("Running {name}");

        let mut test_case: TestCase = serde_yml::from_str(&fs::read_to_string(entry.path())?)?;
        let original = test_case.clone();

        let source =
            fs::read_to_string(entry.path().parent().unwrap().join(format!("{name}.rue")))?;

        let mut allocator = Allocator::new();
        let result = compile_file(&mut allocator, &source)?;

        let mut diagnostics = Vec::new();

        for diagnostic in result.diagnostics {
            diagnostics.push(diagnostic.message(&source));
        }

        if let Some(ptr) = result.program {
            let env = assemble(&mut allocator, test_case.solution.as_ref().unwrap()).unwrap();

            test_case.program = Some(disassemble(&allocator, ptr, None));

            let response = run_program(&mut allocator, &ChiaDialect::new(0), ptr, env, u64::MAX);

            match response {
                Ok(output) => {
                    test_case.output = Some(disassemble(&allocator, output.1, None));
                    let bytes = node_to_bytes(&allocator, ptr)?.len();
                    test_case.cost = Some(output.0 + bytes as u64);
                    test_case.clvm_error = None;
                }
                Err(error) => {
                    test_case.clvm_error = Some(error.to_string());
                    test_case.cost = None;
                    test_case.output = None;
                }
            }
        } else {
            test_case.program = None;
            test_case.output = None;
            test_case.clvm_error = None;
            test_case.cost = None;
        }

        test_case.diagnostics = diagnostics;

        if test_case != original {
            println!("Failed, updated test case");
        }

        fs::write(entry.path(), serde_yml::to_string(&test_case)?)?;
    }

    Ok(())
}
