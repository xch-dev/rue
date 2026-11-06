use std::{ops::Range, path::Path};

use clvmr::Allocator;
use rue_compiler::{Compiler, FileTree, normalize_path};
use rue_lir::{DEBUG_TAGGING, DebugTagging};
use rue_options::CompilerOptions;

fn check_main_source(case: &str, options: CompilerOptions, span: Range<usize>, wrapper: bool) {
    let path = Path::new(env!("CARGO_MANIFEST_DIR"))
        .join("../../spike/cases")
        .join(case);
    let base = path.parent().unwrap().canonicalize().unwrap();
    let kind = normalize_path(&path).unwrap();
    let mut compiler = Compiler::new(options);
    let tree = FileTree::compile_file(&mut compiler, &path).unwrap();
    assert!(compiler.take_diagnostics().is_empty());

    DEBUG_TAGGING.set(Some(DebugTagging::default()));
    let mut allocator = Allocator::new();
    let program = tree
        .main(&mut compiler, &mut allocator, &kind, base)
        .unwrap()
        .unwrap();
    let tagging = DEBUG_TAGGING.take().unwrap();

    // Inspect the emitted root directly: stack rendering can hide a setup
    // wrapper by collapsing it into the following function-body frame.
    let source_tag = tagging.node_spans.get(&program).expect("main source tag");
    let source = &compiler.debug.lir_spans[source_tag];
    assert_eq!(source.source.kind, kind);
    assert_eq!(source.span, span);
    if wrapper {
        // A setup wrapper receives the caller's environment, not the body's
        // capture-extended environment, so it must not borrow its parameters.
        assert!(!tagging.node_functions.contains_key(&program));
    } else {
        assert!(tagging.node_functions.contains_key(&program));
    }
}

#[test]
fn closure_no_params_debug_entry_source() {
    check_main_source(
        "closure_no_params.rue",
        CompilerOptions::debug(),
        23..110,
        true,
    );
}

#[test]
fn closure_no_params_release_entry_source() {
    // Release compilation inlines run(), eliminating the setup wrapper. Keep
    // the existing, more precise source location of the returned expression.
    check_main_source(
        "closure_no_params.rue",
        CompilerOptions::default(),
        94..109,
        false,
    );
}

#[test]
fn closure_capture_debug_entry_source() {
    check_main_source(
        "closure_capture.rue",
        CompilerOptions::debug(),
        45..156,
        true,
    );
}

#[test]
fn closure_capture_release_entry_source() {
    check_main_source(
        "closure_capture.rue",
        CompilerOptions::default(),
        45..156,
        true,
    );
}
