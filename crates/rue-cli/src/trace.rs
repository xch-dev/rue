//! Running programs with a stack trace when they fail, symbolized with their debug metadata
//! sidecars (see `rue_compiler::debug_metadata`) by Chialisp's renderer.

use chialisp::compiler::debug_metadata::{
    DebugMetadataCollection, SerializedFrame, StackFrameStyle,
};
use clvmr::{
    Allocator, NodePtr, diagnostics::run_program_with_diagnostics, error::EvalErr,
    reduction::Reduction, serde::node_to_bytes,
};
use rue_lir::DebugDialect;

use crate::compact::compact_trace;

/// How many of the innermost frames are kept when a program fails.
const MAX_FRAMES: usize = 64;

/// The debug metadata sidecars that failures are symbolized with.
#[derive(Debug, Default, Clone)]
pub struct Sidecars {
    collection: DebugMetadataCollection,
}

impl Sidecars {
    /// Adds a sidecar, unless it isn't valid debug metadata.
    pub fn insert(&mut self, sidecar: &[u8]) -> Result<(), String> {
        self.collection.insert(sidecar)
    }
}

/// A program that failed, along with where it was when it did.
#[derive(Debug)]
pub struct Failure {
    pub error: EvalErr,
    /// The stack trace, or why it couldn't be shown, which doesn't keep the failure itself from
    /// being reported.
    pub trace: Result<String, String>,
}

/// Runs the program like `run_program`, and if it fails, symbolizes the frames that were active
/// with the sidecars, and shows them concisely (see [`compact_trace`]). Frames that no sidecar
/// describes are shown by their tree hash.
pub fn run_with_trace(
    allocator: &mut Allocator,
    dialect: &DebugDialect,
    program: NodePtr,
    env: NodePtr,
    max_cost: u64,
    sidecars: &Sidecars,
) -> Result<Reduction, Failure> {
    let failure = match run_program_with_diagnostics(
        allocator, dialect, program, env, max_cost, MAX_FRAMES,
    ) {
        Ok(reduction) => return Ok(reduction),
        Err(failure) => failure,
    };

    // A frame can hold a value too large to serialize, in which case there's no trace
    let trace = failure
        .frames
        .iter()
        .map(|frame| {
            Ok(SerializedFrame {
                program: node_to_bytes(allocator, frame.program)?,
                environment: node_to_bytes(allocator, frame.environment)?,
            })
        })
        .collect::<Result<Vec<_>, EvalErr>>()
        .map_err(|error| error.to_string())
        .and_then(|frames| {
            sidecars.collection.format_captured_stack(
                &frames,
                failure.truncated,
                StackFrameStyle::Python,
            )
        })
        .map(|trace| compact_trace(&trace));

    Err(Failure {
        error: failure.error,
        trace,
    })
}

#[cfg(test)]
mod tests {
    use std::{fmt::Write, fs, path::Path};

    use chialisp::compiler::debug_metadata::DebugMetadata;
    use expect_test::expect_file;
    use rue_clvm::{assemble, disassemble_value};
    use rue_compiler::{Compiler, FileTree, normalize_path};
    use rue_lir::CLVM_FLAGS;
    use rue_options::CompilerOptions;

    use super::*;

    /// Runs the programs in `rue-compiler/tests/debug_metadata` with each of their solutions (the
    /// `// solution:` lines at the top), in debug and release builds, and snapshots the stack trace
    /// of each failure in `tests/traces` (`UPDATE_EXPECT=1` updates them).
    #[test]
    fn traces() {
        let cases =
            Path::new(env!("CARGO_MANIFEST_DIR")).join("../rue-compiler/tests/debug_metadata");
        let mut paths = fs::read_dir(&cases)
            .unwrap()
            .map(|entry| entry.unwrap().path())
            .filter(|path| path.extension().is_some_and(|extension| extension == "rue"))
            .collect::<Vec<_>>();
        paths.sort();

        for path in paths {
            let source = fs::read_to_string(&path).unwrap();
            let solutions = source
                .lines()
                .map_while(|line| line.strip_prefix("// solution: "))
                .collect::<Vec<_>>();
            assert!(!solutions.is_empty(), "{} has no solution", path.display());

            let mut snapshot = String::new();

            for (build, options) in [
                ("debug", CompilerOptions::debug()),
                ("release", CompilerOptions::default()),
            ] {
                let base_path = path.parent().unwrap().canonicalize().unwrap();
                let kind = normalize_path(&path).unwrap();
                let mut ctx = Compiler::new(options);
                let tree = FileTree::compile_file(&mut ctx, &path).unwrap();
                let mut allocator = Allocator::new();
                let (program, metadata) = tree
                    .main_with_debug_metadata(&mut ctx, &mut allocator, &kind, base_path)
                    .unwrap()
                    .unwrap();

                // Chialisp's reader accepts the sidecar, and it's for exactly this program
                DebugMetadata::decode(&metadata)
                    .unwrap()
                    .verify_program(&node_to_bytes(&allocator, program).unwrap())
                    .unwrap();

                let mut sidecars = Sidecars::default();
                sidecars.insert(&metadata).unwrap();

                for solution in &solutions {
                    let env = assemble(&mut allocator, solution).unwrap();
                    writeln!(snapshot, "===== {build}: {solution} =====").unwrap();

                    match run_with_trace(
                        &mut allocator,
                        &DebugDialect::new(CLVM_FLAGS, false),
                        program,
                        env,
                        11_000_000_000,
                        &sidecars,
                    ) {
                        Ok(output) => {
                            writeln!(
                                snapshot,
                                "output: {}",
                                disassemble_value(&allocator, output.1)
                            )
                            .unwrap();
                        }
                        Err(failure) => {
                            writeln!(snapshot, "error: {}", failure.error).unwrap();
                            writeln!(snapshot, "{}", failure.trace.unwrap()).unwrap();
                        }
                    }
                }
            }

            let name = path.file_stem().unwrap().to_string_lossy().to_string();
            expect_file![
                Path::new(env!("CARGO_MANIFEST_DIR"))
                    .join("tests/traces")
                    .join(format!("{name}.txt"))
            ]
            .assert_eq(&snapshot);
        }
    }
}
