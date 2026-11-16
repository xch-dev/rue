//! WASM bindings for the Rue compiler, which the playground uses to compile, check, format and run
//! Rue programs in the browser.
//!
//! Ranges are UTF-16 code unit offsets into the source, since that's how JavaScript strings and
//! editors index text.

mod diagnostics;
mod examples;
mod run;

#[cfg(test)]
mod tests;

use std::{path::PathBuf, sync::Arc};

use clvm_utils::tree_hash;
use clvmr::{Allocator, serde::node_to_bytes};
use rue_clvm::disassemble_program;
use rue_compiler::{Compiler, File, FileTree};
use rue_diagnostic::{Source, SourceKind};
use rue_formatter::{FormatError, FormatOptions, format_source};
use rue_options::CompilerOptions;
use wasm_bindgen::prelude::*;

pub use diagnostics::*;
pub use examples::*;
pub use run::*;

/// The name of the playground's only file, which diagnostics refer to.
const FILE_NAME: &str = "main.rue";

/// The result of compiling a source file.
#[derive(Debug, Clone)]
#[wasm_bindgen(getter_with_clone)]
pub struct Compilation {
    /// The disassembled program, if the source has a `main` function and no errors.
    pub program: Option<String>,
    /// The serialized program, as hex.
    pub hex: Option<String>,
    /// The tree hash of the program, as hex.
    #[wasm_bindgen(js_name = treeHash)]
    pub tree_hash: Option<String>,
    /// The size of the serialized program, in bytes.
    pub size: Option<u32>,
    pub diagnostics: Vec<Diagnostic>,
}

/// Compiles the source, with the options `rue build` uses (or `rue build --debug` if `debug` is
/// set, which keeps `debug` statements and the locations of failed assertions).
#[wasm_bindgen]
pub fn compile(source: String, debug: bool) -> Result<Compilation, JsError> {
    console_error_panic_hook::set_once();

    let mut allocator = Allocator::new();
    let (mut ctx, tree, source) = check(source, debug);

    let has_errors = ctx.has_errors();
    let mut diagnostics = ctx.take_diagnostics();

    let program = if has_errors {
        None
    } else {
        let program = tree.main(&mut ctx, &mut allocator, &source.kind, PathBuf::from("."))?;
        // Code generation can report diagnostics of its own
        diagnostics.extend(ctx.take_diagnostics());
        program
    };

    let mut compilation = Compilation {
        program: None,
        hex: None,
        tree_hash: None,
        size: None,
        diagnostics: convert_diagnostics(&source, &diagnostics),
    };

    if let Some(program) = program {
        let bytes = node_to_bytes(&allocator, program)?;
        compilation.program = Some(disassemble_program(&allocator, program));
        compilation.tree_hash = Some(tree_hash(&allocator, program).to_string());
        compilation.size = Some(u32::try_from(bytes.len())?);
        compilation.hex = Some(hex::encode(bytes));
    }

    Ok(compilation)
}

/// Parses and checks the source as the playground's only file, along with the standard library.
fn check(source: String, debug: bool) -> (Compiler, FileTree, Source) {
    let options = if debug {
        CompilerOptions::debug()
    } else {
        CompilerOptions::default()
    };

    let mut ctx = Compiler::new(options);
    let source = Source::new(Arc::from(source), SourceKind::File(FILE_NAME.to_string()));

    let tree = FileTree::File(File::new(&mut ctx, "main".to_string(), source.clone()));
    tree.compile(&mut ctx);

    (ctx, tree, source)
}

/// Formats the source the way `rue fmt` does. Fails if it has syntax errors.
#[wasm_bindgen]
pub fn format(source: &str) -> Result<String, JsError> {
    console_error_panic_hook::set_once();

    format_source(source, &FormatOptions::default()).map_err(|error| match error {
        FormatError::Parse { .. } => JsError::new("The source has syntax errors"),
        error => JsError::new(&error.to_string()),
    })
}

/// The version of the compiler.
#[wasm_bindgen]
pub fn version() -> String {
    env!("CARGO_PKG_VERSION").to_string()
}
