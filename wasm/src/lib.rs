use std::sync::Arc;

use clvm_tools_rs::classic::clvm_tools::binutils::disassemble;
use clvmr::Allocator;
use rue_compiler::compile_file;
use rue_diagnostic::{Diagnostic, Source, SourceKind};
use rue_options::CompilerOptions;
use wasm_bindgen::prelude::*;

#[derive(Debug)]
#[wasm_bindgen(getter_with_clone)]
pub struct Example {
    pub name: String,
    pub content: String,
}

#[derive(Debug)]
#[wasm_bindgen(getter_with_clone)]
pub struct Compilation {
    pub program: Option<String>,
    pub diagnostics: Vec<String>,
}

#[wasm_bindgen]
pub fn compile(source: String) -> Result<Compilation, JsError> {
    console_error_panic_hook::set_once();

    let mut allocator = Allocator::new();

    let result = compile_file(
        &mut allocator,
        Source::new(Arc::from(source), SourceKind::File("main.rue".to_string())),
        CompilerOptions::default(),
    )?;

    let program = result
        .main
        .map(|program| disassemble(&allocator, program, None));

    let diagnostics = result.diagnostics.iter().map(Diagnostic::message).collect();

    Ok(Compilation {
        program,
        diagnostics,
    })
}

#[wasm_bindgen(js_name = getExamples)]
pub fn get_examples() -> Vec<Example> {
    vec![
        Example {
            name: "Hello World".to_string(),
            content: include_str!("../../examples/hello_world.rue").to_string(),
        },
        Example {
            name: "Factorial".to_string(),
            content: include_str!("../../examples/factorial.rue").to_string(),
        },
        Example {
            name: "Fizz Buzz".to_string(),
            content: include_str!("../../examples/fizz_buzz.rue").to_string(),
        },
        Example {
            name: "Royalty Split".to_string(),
            content: include_str!("../../examples/puzzles/royalty_split.rue").to_string(),
        },
        Example {
            name: "CAT".to_string(),
            content: include_str!("../../examples/puzzles/cat.rue").to_string(),
        },
        Example {
            name: "Singleton".to_string(),
            content: include_str!("../../examples/puzzles/singleton.rue").to_string(),
        },
        Example {
            name: "P2 Delegated Conditions".to_string(),
            content: include_str!("../../examples/puzzles/p2_delegated_conditions.rue").to_string(),
        },
    ]
}
