use std::sync::Arc;

use clvm_tools_rs::classic::clvm_tools::binutils::disassemble;
use clvmr::Allocator;
use rue_compiler::compile_file;
use rue_diagnostic::{Diagnostic, Source, SourceKind};
use rue_options::CompilerOptions;
use wasm_bindgen::prelude::*;

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
        .program
        .map(|program| disassemble(&allocator, program, None));

    let diagnostics = result.diagnostics.iter().map(Diagnostic::message).collect();

    Ok(Compilation {
        program,
        diagnostics,
    })
}
