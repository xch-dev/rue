use clvm_utils::tree_hash_from_bytes;

use crate::{compile, format, get_examples, run, run_tests, snapshot_value};

/// The text of a UTF-16 range of the source, which is how JavaScript slices it.
fn utf16_slice(source: &str, from: u32, to: u32) -> String {
    let units: Vec<u16> = source.encode_utf16().collect();
    String::from_utf16(&units[from as usize..to as usize]).unwrap()
}

/// Applies edits in UTF-16 offsets, the way the playground does.
fn apply_utf16_edits(source: &str, edits: &[crate::TextEdit]) -> String {
    let mut units: Vec<u16> = source.encode_utf16().collect();
    let mut edits = edits.to_vec();
    edits.sort_by_key(|edit| edit.from);

    for edit in edits.iter().rev() {
        units.splice(
            edit.from as usize..edit.to as usize,
            edit.insert.encode_utf16(),
        );
    }

    String::from_utf16(&units).unwrap()
}

/// The snapshot reader agrees with a YAML parser on every top-level field of every example.
#[test]
fn snapshot_values_match_yaml() {
    for example in get_examples() {
        let yaml: serde_json::Value = serde_saphyr::from_str(example.snapshot).unwrap();

        for (key, value) in yaml.as_object().unwrap() {
            let expected = match value {
                serde_json::Value::String(text) => text.clone(),
                serde_json::Value::Number(number) => number.to_string(),
                _ => continue,
            };

            assert_eq!(
                snapshot_value(example.snapshot, key).as_deref(),
                Some(expected.as_str()),
                "{}: {key}",
                example.name
            );
        }
    }
}

/// Each example compiles and runs to the same result as its snapshot, which `rue-tests` checks
/// against the CLI's compiler.
#[test]
fn examples_match_snapshots() {
    for example in get_examples() {
        let name = &example.name;
        let snapshot = example.snapshot;
        let compilation = compile(example.content.clone(), false).unwrap();

        assert!(
            compilation
                .diagnostics
                .iter()
                .all(|d| d.severity != "error"),
            "{name} has errors"
        );
        assert_eq!(
            compilation.program,
            snapshot_value(snapshot, "program"),
            "{name}: program"
        );

        if let Some(hex) = &compilation.hex {
            let bytes = hex::decode(hex).unwrap();
            assert_eq!(compilation.size, Some(u32::try_from(bytes.len()).unwrap()));
            assert_eq!(
                compilation.tree_hash,
                Some(tree_hash_from_bytes(&bytes).unwrap().to_string()),
                "{name}: tree hash"
            );

            let execution = run(hex, &example.solution).unwrap();
            #[allow(clippy::cast_possible_truncation, clippy::cast_sign_loss)]
            let runtime_cost = execution.runtime_cost.map(|cost| cost as u64);
            #[allow(clippy::cast_possible_truncation, clippy::cast_sign_loss)]
            let byte_cost = execution.byte_cost as u64;

            assert_eq!(
                Some(byte_cost),
                snapshot_value(snapshot, "byte_cost").map(|cost| cost.parse().unwrap()),
                "{name}: byte cost"
            );

            assert_eq!(
                execution.output,
                snapshot_value(snapshot, "output"),
                "{name}: output"
            );
            assert_eq!(
                execution.error,
                snapshot_value(snapshot, "clvm_error"),
                "{name}: error"
            );
            assert_eq!(
                runtime_cost,
                snapshot_value(snapshot, "runtime_cost").map(|cost| cost.parse().unwrap()),
                "{name}: runtime cost"
            );
        }

        let tests: Vec<_> = run_tests(example.content.clone())
            .unwrap()
            .into_iter()
            .map(|test| test.name)
            .collect();
        let snapshot_tests: Vec<_> = snapshot
            .lines()
            .filter_map(|line| line.strip_prefix("- name: "))
            .collect();

        assert_eq!(tests, snapshot_tests, "{name}: tests");
    }
}

#[test]
fn diagnostics_use_utf16_ranges() {
    let source =
        "fn main() -> String {\n    let x = \"\u{1F600}\";\n    let y = 1;\n    \"hi\"\n}\n";
    let compilation = compile(source.to_string(), false).unwrap();

    let y = compilation
        .diagnostics
        .iter()
        .find(|d| d.message.contains("`y`"))
        .expect("a diagnostic for `y`");

    assert_eq!(y.severity, "warning");
    assert_eq!(y.lint.as_deref(), Some("unused_binding"));
    assert_eq!(utf16_slice(source, y.from, y.to), "y");
    assert_eq!((y.line, y.column), (3, 9));

    // Warnings don't stop the program from compiling
    assert!(compilation.program.is_some());
}

#[test]
fn suggestions_use_utf16_ranges() {
    let source = "fn main() -> Int {\n    let _x = \"\u{e9}\u{1F600}\";\n    +1\n}\n";
    let compilation = compile(source.to_string(), false).unwrap();

    let plus = compilation
        .diagnostics
        .iter()
        .find(|d| d.lint.as_deref() == Some("unnecessary_plus"))
        .expect("an unnecessary_plus warning");
    let suggestion = &plus.suggestions[0];

    assert!(suggestion.machine_applicable);
    assert_eq!(
        apply_utf16_edits(source, &suggestion.edits),
        "fn main() -> Int {\n    let _x = \"\u{e9}\u{1F600}\";\n    1\n}\n"
    );
}

#[test]
fn errors_have_no_program() {
    let compilation = compile("fn main() -> Int { \"x\" }\n".to_string(), false).unwrap();

    assert!(
        compilation
            .diagnostics
            .iter()
            .any(|d| d.severity == "error")
    );
    assert_eq!(compilation.program, None);
    assert_eq!(compilation.hex, None);
    assert_eq!(compilation.tree_hash, None);
    assert_eq!(compilation.size, None);

    assert!(
        run_tests("fn main() -> Int { \"x\" }\n".to_string())
            .unwrap()
            .is_empty()
    );
}

#[test]
fn debug_builds_print_and_locate_assertions() {
    let source = "fn main(x: Int) -> Int {\n    debug x;\n    assert x > 1;\n    x\n}\n";

    let release = compile(source.to_string(), false).unwrap();
    let execution = run(release.hex.as_ref().unwrap(), "(5)").unwrap();
    assert_eq!(execution.output.as_deref(), Some("5"));
    assert!(execution.prints.is_empty());

    let debug = compile(source.to_string(), true).unwrap();
    let execution = run(debug.hex.as_ref().unwrap(), "(5)").unwrap();
    assert_eq!(execution.output.as_deref(), Some("5"));
    assert_eq!(execution.prints.len(), 1);
    assert_eq!(execution.prints[0].location, "main.rue:2:5");
    assert_eq!(execution.prints[0].value, "5");

    let execution = run(debug.hex.as_ref().unwrap(), "(1)").unwrap();
    assert_eq!(execution.output, None);
    assert_eq!(execution.runtime_cost, None);
    assert_eq!(
        execution.error.as_deref(),
        Some("raise \"assertion failed at main.rue:3:5\"")
    );
}

#[test]
fn tests_report_failures_and_prints() {
    let source = "fn double(x: Int) -> Int {\n    x * 2\n}\n\ntest fn passes() {\n    assert double(2) == 4;\n}\n\ntest fn fails() {\n    debug double(3);\n    assert double(3) == 7;\n}\n";
    let results = run_tests(source.to_string()).unwrap();

    assert_eq!(results.len(), 2);
    assert_eq!(results[0].name, "passes");
    assert_eq!(results[0].error, None);
    assert_eq!(results[1].name, "fails");
    assert_eq!(
        results[1].error.as_deref(),
        Some("raise \"assertion failed at main.rue:11:5\"")
    );
    assert_eq!(results[1].prints[0].value, "6");
}

#[test]
fn tests_must_return_nil() {
    let results = run_tests("test fn returns_value() -> Int {\n    1\n}\n".to_string()).unwrap();

    assert_eq!(
        results[0].error.as_deref(),
        Some("returned 1 instead of nil")
    );
}

/// Programs run with the same flags as the CLI, which enable `keccak256`.
#[test]
fn runs_with_the_cli_flags() {
    let source = "fn main() -> Bytes32 {\n    keccak256(\"hello\")\n}\n\ntest fn hashes() {\n    assert main() == 0x1c8aff950685c2ed4bc3174f3472287b56d9517b9c948127319a09a7a36deac8;\n}\n";
    let compilation = compile(source.to_string(), false).unwrap();
    let execution = run(compilation.hex.as_ref().unwrap(), "()").unwrap();

    assert_eq!(
        execution.output.as_deref(),
        Some("0x1c8aff950685c2ed4bc3174f3472287b56d9517b9c948127319a09a7a36deac8")
    );
    assert_eq!(run_tests(source.to_string()).unwrap()[0].error, None);
}

#[test]
fn formats_source() {
    assert_eq!(
        format("fn main()->Int{let x=1;x+2}").unwrap(),
        "fn main() -> Int {\n    let x = 1;\n    x + 2\n}\n"
    );
}
