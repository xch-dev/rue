//! Tests that a placeholder for code that couldn't be compiled is always covered by an error, and
//! that code isn't generated if one isn't (#130).

use std::{path::PathBuf, sync::Arc};

use clvmr::{Allocator, NodePtr};
use rue_diagnostic::{Source, SourceKind};
use rue_options::CompilerOptions;

use crate::{Compiler, Error, File, FileTree, has_uncovered_placeholder};

/// Compiles the source with the standard library, and then generates `main` after running
/// `before_codegen` on the compiler.
fn compile_main(
    source: &str,
    before_codegen: impl FnOnce(&mut Compiler),
) -> (Compiler, Result<Option<NodePtr>, Error>) {
    let mut ctx = Compiler::new(CompilerOptions::default());

    let source = Source::new(Arc::from(source), SourceKind::File("main.rue".to_string()));
    let kind = source.kind.clone();
    let tree = FileTree::File(File::new(&mut ctx, "main".to_string(), source));
    tree.compile(&mut ctx);

    before_codegen(&mut ctx);

    let result = tree.main(&mut ctx, &mut Allocator::new(), &kind, PathBuf::from("."));
    (ctx, result)
}

#[test]
fn compiling_the_standard_library_makes_no_placeholders() {
    let ctx = Compiler::new(CompilerOptions::default());
    assert_eq!(ctx.first_placeholder(), None);
}

#[test]
fn a_program_without_errors_makes_no_placeholders() {
    let (ctx, result) = compile_main("fn main() -> Int { 42 }", |_| {});
    assert_eq!(ctx.first_placeholder(), None);
    assert!(result.unwrap().is_some());
}

#[test]
fn a_placeholder_is_recorded_where_it_was_made() {
    let (ctx, result) = compile_main("fn main() -> Int { missing }", |_| {});
    let location = ctx.first_placeholder().unwrap();
    assert!(location.file().ends_with("path.rs"), "{location}");
    assert!(matches!(result, Err(Error::CompilationFailed)));
}

/// A placeholder without an error would disable type checking for whatever uses it (since its
/// type is assignable to and from every type), so code isn't generated even if no placeholder
/// value is reachable.
#[test]
fn an_uncovered_placeholder_is_detected() {
    let mut ctx = Compiler::new(CompilerOptions::default());

    let source = Source::new(
        Arc::from("fn main() -> Int { 42 }"),
        SourceKind::File("main.rue".to_string()),
    );
    let tree = FileTree::File(File::new(&mut ctx, "main".to_string(), source));
    tree.compile(&mut ctx);

    let FileTree::File(file) = &tree else {
        unreachable!();
    };
    let scope = ctx.module(file.module).scope;
    let main = ctx.scope(scope).symbol("main").unwrap();

    assert!(!has_uncovered_placeholder(&ctx, main));

    ctx.note_placeholder();

    assert!(has_uncovered_placeholder(&ctx, main));
}

/// Debug builds panic, so that tests catch an uncovered placeholder, and release builds refuse to
/// generate code.
#[test]
#[cfg_attr(
    debug_assertions,
    should_panic(expected = "without an error (the first placeholder was made at")
)]
fn an_uncovered_placeholder_stops_codegen() {
    let (_, result) = compile_main("fn main() -> Int { 42 }", Compiler::note_placeholder);
    assert!(matches!(result, Err(Error::UnresolvedCodegen)));
}
