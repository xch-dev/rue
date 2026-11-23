//! Tests for the debug metadata sidecars of the programs in `tests/debug_metadata`. Each program is
//! compiled with and without a sidecar, in debug and release builds, and each sidecar is decoded
//! and described in a snapshot next to the program (`UPDATE_EXPECT=1` updates them).

use std::{
    fmt::Write,
    fs,
    path::{Path, PathBuf},
};

use clvmr::{
    Allocator, NodePtr, SExp,
    serde::{node_from_bytes, node_to_bytes},
};
use expect_test::expect_file;
use num_bigint::BigInt;
use rue_clvm::disassemble_program;
use rue_compiler::{Compiler, FileTree, normalize_path};
use rue_options::CompilerOptions;
use sha2::{Digest, Sha256};

fn cases() -> Vec<PathBuf> {
    let dir = Path::new(env!("CARGO_MANIFEST_DIR")).join("tests/debug_metadata");
    let mut cases = fs::read_dir(dir)
        .unwrap()
        .map(|entry| entry.unwrap().path())
        .filter(|path| path.extension().is_some_and(|extension| extension == "rue"))
        .collect::<Vec<_>>();
    cases.sort();
    cases
}

struct Compiled {
    program: Vec<u8>,
    metadata: Option<Vec<u8>>,
}

fn compile(path: &Path, options: CompilerOptions, debug_metadata: bool) -> Compiled {
    let base_path = path.parent().unwrap().canonicalize().unwrap();
    let kind = normalize_path(path).unwrap();

    let mut ctx = Compiler::new(options);
    let tree = FileTree::compile_file(&mut ctx, path).unwrap();
    let diagnostics = ctx
        .take_diagnostics()
        .iter()
        .map(|diagnostic| diagnostic.full_message(&base_path))
        .collect::<Vec<_>>();
    assert!(diagnostics.is_empty(), "{diagnostics:#?}");

    let mut allocator = Allocator::new();

    let (ptr, metadata) = if debug_metadata {
        let (ptr, metadata) = tree
            .main_with_debug_metadata(&mut ctx, &mut allocator, &kind, base_path)
            .unwrap()
            .unwrap();
        (ptr, Some(metadata))
    } else {
        let ptr = tree
            .main(&mut ctx, &mut allocator, &kind, base_path)
            .unwrap()
            .unwrap();
        (ptr, None)
    };

    Compiled {
        program: node_to_bytes(&allocator, ptr).unwrap(),
        metadata,
    }
}

#[test]
fn sidecars() {
    for path in cases() {
        let mut snapshot = String::new();

        for (build, options) in [
            ("debug", CompilerOptions::debug()),
            ("release", CompilerOptions::default()),
        ] {
            let without = compile(&path, options, false);
            let with = compile(&path, options, true);

            // Requesting a sidecar doesn't change the program
            assert_eq!(
                with.program,
                without.program,
                "{} ({build})",
                path.display()
            );

            let metadata = with.metadata.unwrap();
            writeln!(snapshot, "===== {build} =====").unwrap();
            snapshot.push_str(&describe(&with.program, &metadata));
        }

        expect_file![path.with_extension("txt")].assert_eq(&snapshot);
    }
}

/// The setup that binds `main`'s captures is located at `main`'s body, without being marked as the
/// function, since its environment isn't the body's (#188).
#[test]
fn main_capture_wrapper_is_located_at_the_body() {
    let dir = Path::new(env!("CARGO_MANIFEST_DIR")).join("tests/debug_metadata");

    for (case, options, wrapper, root) in [
        (
            "closure_capture.rue",
            CompilerOptions::debug(),
            true,
            "3:46-9:2",
        ),
        (
            "closure_capture.rue",
            CompilerOptions::default(),
            true,
            "3:46-9:2",
        ),
        (
            "closure_no_params.rue",
            CompilerOptions::debug(),
            true,
            "3:24-9:2",
        ),
        // A release build inlines `run`, so `main` captures nothing and has no setup, and the root
        // is its body, which keeps its own location
        (
            "closure_no_params.rue",
            CompilerOptions::default(),
            false,
            "8:5-8:19",
        ),
    ] {
        let compiled = compile(&dir.join(case), options, true);
        let metadata = Metadata::decode(compiled.metadata.as_deref().unwrap());
        let (span, function) = metadata.root_annotation();

        assert_eq!(
            span.map(|span| metadata.span(span)).as_deref(),
            Some(&*format!("{case}:{root}")),
            "{case} ({options:?})"
        );
        assert_eq!(function.is_none(), wrapper, "{case}");
    }
}

/// Exports and tests have sidecars of their own, and are the same with and without them.
#[test]
fn exports() {
    let temp = tempfile::tempdir().unwrap();
    let dir = temp.path();
    let path = dir.join("exports.rue");
    fs::write(
        &path,
        "export fn double(value: Int) -> Int {\n    value * 2\n}\n\nexport fn square(value: Int) -> Int {\n    value * value\n}\n\ntest fn squares() {\n    assert square(3) == 9;\n}\n",
    )
    .unwrap();

    for options in [CompilerOptions::debug(), CompilerOptions::default()] {
        let mut ctx = Compiler::new(options);
        let tree = FileTree::compile_file(&mut ctx, &path).unwrap();
        assert!(ctx.take_diagnostics().is_empty());

        let kind = normalize_path(&path).unwrap();
        let mut allocator = Allocator::new();
        let without = tree
            .exports(&mut ctx, &mut allocator, Some(&kind), None, dir)
            .unwrap();
        let with = tree
            .exports_with_debug_metadata(&mut ctx, &mut allocator, Some(&kind), None, dir)
            .unwrap();

        assert_eq!(with.len(), 2);
        assert_eq!(without.len(), with.len());

        for (without, with) in without.iter().zip(&with) {
            assert_eq!(without.name, with.name);
            assert!(without.debug_metadata.is_none());

            let program = node_to_bytes(&allocator, with.ptr).unwrap();
            assert_eq!(node_to_bytes(&allocator, without.ptr).unwrap(), program);

            // Each export is the function itself, which takes its arguments as a list
            let metadata = Metadata::decode(with.debug_metadata.as_deref().unwrap());
            assert_eq!(
                Metadata::program_sha256(with.debug_metadata.as_deref().unwrap()),
                Sha256::digest(&program).to_vec()
            );
            let (_, function) = metadata.root_annotation();
            assert_eq!(
                metadata.function(function.unwrap()),
                format!("{} (value@2: integer . _)", with.name)
            );
        }

        // So do tests, which `rue test` compiles again with sidecars when they fail
        let tests = tree
            .tests(&mut ctx, &mut allocator, Some(&kind), None, dir, false)
            .unwrap();
        assert_eq!(tests.len(), 1);

        let (ptr, metadata) = tests[0]
            .with_debug_metadata(&mut ctx, &mut allocator, dir)
            .unwrap();
        let program = node_to_bytes(&allocator, ptr).unwrap();
        assert_eq!(node_to_bytes(&allocator, tests[0].ptr).unwrap(), program);
        assert_eq!(
            Metadata::program_sha256(&metadata),
            Sha256::digest(&program).to_vec()
        );
    }
}

/// The decoded sidecar, with each record as a list of CLVM values.
struct Metadata {
    allocator: Allocator,
    files: Vec<NodePtr>,
    strings: Vec<NodePtr>,
    spans: Vec<NodePtr>,
    functions: Vec<NodePtr>,
    tree: NodePtr,
}

impl Metadata {
    fn decode(bytes: &[u8]) -> Self {
        let mut allocator = Allocator::new();
        let root = node_from_bytes(&mut allocator, bytes).unwrap();
        let fields = items(&allocator, root);
        assert_eq!(fields.len(), 9);
        assert_eq!(allocator.atom(fields[0]).as_ref(), b"CHIALISP_DEBUG");
        assert_eq!(number(&allocator, fields[1]), 1);

        let coordinates = items(&allocator, fields[2])
            .into_iter()
            .map(|field| number(&allocator, field))
            .collect::<Vec<_>>();
        assert_eq!(coordinates, [1, 8]);

        Self {
            files: items(&allocator, fields[4]),
            strings: items(&allocator, fields[5]),
            spans: items(&allocator, fields[6]),
            functions: items(&allocator, fields[7]),
            tree: fields[8],
            allocator,
        }
    }

    fn program_sha256(bytes: &[u8]) -> Vec<u8> {
        let mut allocator = Allocator::new();
        let root = node_from_bytes(&mut allocator, bytes).unwrap();
        allocator.atom(items(&allocator, root)[3]).as_ref().to_vec()
    }

    fn text(&self, node: NodePtr) -> String {
        String::from_utf8(self.allocator.atom(node).as_ref().to_vec()).unwrap()
    }

    fn string(&self, index: NodePtr) -> String {
        self.text(self.strings[number(&self.allocator, index)])
    }

    fn file(&self, index: usize) -> String {
        self.text(items(&self.allocator, self.files[index])[0])
    }

    fn span(&self, index: usize) -> String {
        let fields = items(&self.allocator, self.spans[index])
            .into_iter()
            .map(|field| number(&self.allocator, field))
            .collect::<Vec<_>>();
        format!(
            "{}:{}:{}-{}:{}",
            self.file(fields[0]),
            fields[1],
            fields[2],
            fields[3],
            fields[4]
        )
    }

    fn function(&self, index: usize) -> String {
        let fields = items(&self.allocator, self.functions[index]);
        let name = self.string(fields[0]);
        assert_eq!(number(&self.allocator, fields[1]), 0, "left_env");
        format!("{name} {}", self.parameter(fields[2], &mut Vec::new()))
    }

    /// Describes a parameter pattern, which is at the position in the environment that the steps
    /// lead to (`false` for the first of a pair, and `true` for the rest).
    fn parameter(&self, node: NodePtr, steps: &mut Vec<bool>) -> String {
        let fields = items(&self.allocator, node);
        match number(&self.allocator, fields[0]) {
            0 => "_".to_string(),
            1 => {
                let constraint = items(&self.allocator, fields[3]);
                let constraint = match number(&self.allocator, constraint[0]) {
                    0 => "unknown".to_string(),
                    1 => "integer".to_string(),
                    2 => "atom".to_string(),
                    3 => format!("bytes{}", number(&self.allocator, constraint[1])),
                    4 => "pair".to_string(),
                    5 => "list".to_string(),
                    tag => panic!("unexpected constraint {tag}"),
                };
                let path = number(&self.allocator, fields[2]);

                // The pattern mirrors the environment, so the value is at the pattern's position,
                // whose path has a bit for each step from the lowest up, below a final 1
                let expected = steps
                    .iter()
                    .rev()
                    .fold(1, |path, &rest| path * 2 + usize::from(rest));
                assert_eq!(path, expected, "the path of {}", self.string(fields[1]));

                format!("{}@{path}: {constraint}", self.string(fields[1]))
            }
            2 => {
                steps.push(false);
                let first = self.parameter(fields[1], steps);
                steps.pop();
                steps.push(true);
                let rest = self.parameter(fields[2], steps);
                steps.pop();
                format!("({first} . {rest})")
            }
            tag => panic!("unexpected parameter {tag}"),
        }
    }

    /// The span and function of the root of the program.
    fn root_annotation(&self) -> (Option<usize>, Option<usize>) {
        let fields = items(&self.allocator, self.tree);
        (
            reference(&self.allocator, fields[1]),
            reference(&self.allocator, fields[3]),
        )
    }

    /// Rebuilds the program from the shadow tree, along with the annotation of each pair.
    fn program(
        &self,
        node: NodePtr,
        program: &mut Allocator,
        annotations: &mut Vec<(NodePtr, Annotation)>,
    ) -> NodePtr {
        let fields = items(&self.allocator, node);

        match number(&self.allocator, fields[0]) {
            0 => program
                .new_atom(self.allocator.atom(fields[4]).as_ref())
                .unwrap(),
            1 => {
                let first = self.program(fields[4], program, annotations);
                let rest = self.program(fields[5], program, annotations);
                let pair = program.new_pair(first, rest).unwrap();
                annotations.push((
                    pair,
                    Annotation {
                        span: reference(&self.allocator, fields[1]),
                        label: reference(&self.allocator, fields[2]),
                        function: reference(&self.allocator, fields[3]),
                    },
                ));
                pair
            }
            tag => panic!("unexpected shadow node {tag}"),
        }
    }
}

/// Describes the sidecar: its functions, and the pairs of the program whose annotations differ
/// from the pair they're in.
fn describe(program_bytes: &[u8], bytes: &[u8]) -> String {
    let metadata = Metadata::decode(bytes);

    // The shadow tree is the program, and the sidecar is for its serialization
    assert_eq!(
        Metadata::program_sha256(bytes),
        Sha256::digest(program_bytes).to_vec()
    );

    let mut program = Allocator::new();
    let mut annotations = Vec::new();
    let root = metadata.program(metadata.tree, &mut program, &mut annotations);
    assert_eq!(node_to_bytes(&program, root).unwrap(), program_bytes);

    let mut output = String::new();

    writeln!(output, "program: {}", disassemble_program(&program, root)).unwrap();
    writeln!(output, "sidecar: {} bytes", bytes.len()).unwrap();

    writeln!(output, "files:").unwrap();
    for index in 0..metadata.files.len() {
        writeln!(output, "  {}", metadata.file(index)).unwrap();
    }

    writeln!(output, "functions:").unwrap();
    for index in 0..metadata.functions.len() {
        writeln!(output, "  {index}: {}", metadata.function(index)).unwrap();
    }

    writeln!(output, "nodes:").unwrap();
    let annotations = annotations
        .into_iter()
        .collect::<std::collections::HashMap<_, _>>();
    describe_nodes(
        &metadata,
        &program,
        root,
        None,
        &annotations,
        0,
        &mut output,
    );

    output
}

/// The span, label and function that a node of the shadow tree refers to.
#[derive(Debug, Clone, Copy)]
struct Annotation {
    span: Option<usize>,
    label: Option<usize>,
    function: Option<usize>,
}

type Annotations = std::collections::HashMap<NodePtr, Annotation>;

fn describe_nodes(
    metadata: &Metadata,
    program: &Allocator,
    node: NodePtr,
    parent_span: Option<usize>,
    annotations: &Annotations,
    depth: usize,
    output: &mut String,
) {
    let SExp::Pair(first, rest) = program.sexp(node) else {
        return;
    };

    let Annotation {
        span,
        label,
        function,
    } = annotations[&node];
    let mut depth = depth;

    if function.is_some() || label.is_some() || span != parent_span {
        let mut text = disassemble_program(program, node);
        if text.chars().count() > 60 {
            text = text.chars().take(57).collect::<String>() + "...";
        }

        let span = span.map_or("-".to_string(), |span| metadata.span(span));
        let function = function.map_or(String::new(), |function| {
            let fields = items(&metadata.allocator, metadata.functions[function]);
            format!(" fn {}", metadata.string(fields[0]))
        });
        let label = label.map_or(String::new(), |label| {
            format!(" label {:?}", metadata.text(metadata.strings[label]))
        });

        writeln!(
            output,
            "  {}{span}{function}{label}  {text}",
            "  ".repeat(depth)
        )
        .unwrap();
        depth += 1;
    }

    describe_nodes(metadata, program, first, span, annotations, depth, output);
    describe_nodes(metadata, program, rest, span, annotations, depth, output);
}

fn items(allocator: &Allocator, mut node: NodePtr) -> Vec<NodePtr> {
    let mut items = Vec::new();

    while let SExp::Pair(first, rest) = allocator.sexp(node) {
        items.push(first);
        node = rest;
    }

    assert_eq!(allocator.atom(node).as_ref(), b"", "proper list");
    items
}

fn number(allocator: &Allocator, node: NodePtr) -> usize {
    usize::try_from(allocator.number(node)).unwrap()
}

/// An optional reference, which is its index plus one, or zero if there isn't one.
fn reference(allocator: &Allocator, node: NodePtr) -> Option<usize> {
    let number = allocator.number(node);
    (number != BigInt::ZERO).then(|| usize::try_from(number - 1).unwrap())
}
