//! The right operand of `&&` and `||` is only evaluated when the left operand decides the result
//! if that can't fail on mainnet, which enforces `LIMITS` until hard fork 2, even when the program
//! isn't compiled with `operand_limits` (see <https://github.com/xch-dev/rue/issues/617>).

use std::path::PathBuf;

use clvmr::{Allocator, ChiaDialect, run_program};
use rue_clvm::{assemble, disassemble_value};
use rue_diagnostic::SourceKind;
use rue_lir::ClvmTarget;
use rue_options::CompilerOptions;

use crate::test_utils::compile_with;

/// What `main` returns for the solution under `LIMITS`, as mainnet runs it today, or the error.
fn run_with_limits(
    source: &str,
    options: CompilerOptions,
    solution: &str,
) -> Result<String, String> {
    let mut compiled = compile_with(source, |o| *o = options);
    compiled.assert_no_errors();

    let mut allocator = Allocator::new();
    let kind = SourceKind::File("main.rue".to_string());
    let program = compiled
        .tree
        .main(&mut compiled.ctx, &mut allocator, &kind, PathBuf::from("."))
        .unwrap()
        .unwrap();

    let solution = assemble(&mut allocator, solution).unwrap();
    let target = ClvmTarget {
        operand_limits: true,
        hard_fork_2: false,
    };
    let dialect = ChiaDialect::new(target.flags());

    run_program(&mut allocator, &dialect, program, solution, u64::MAX)
        .map(|output| disassemble_value(&allocator, output.1))
        .map_err(|error| error.to_string())
}

#[test]
fn guards_on_long_operands_are_kept() {
    // 300 bytes, too long for `*` and `/` under `LIMITS`
    let long = format!("0x01{}", "00".repeat(299));

    for (source, expected) in [
        (
            "fn main(x: Int) -> Bool { (x as Bytes).length <= 256 && x * 3 > 5 }",
            "()",
        ),
        ("fn main(x: Int) -> Bool { x > 1000 || x * 3 > 5 }", "1"),
        ("fn main(x: Int) -> Bool { x < 1000 && x / 3 > 5 }", "()"),
        ("fn main(x: Int) -> Bool { x < 1000 && x % 3 > 1 }", "()"),
    ] {
        for options in [
            CompilerOptions::default(),
            CompilerOptions::debug(),
            CompilerOptions {
                operand_limits: true,
                ..CompilerOptions::default()
            },
        ] {
            assert_eq!(
                run_with_limits(source, options, &format!("({long})")).as_deref(),
                Ok(expected),
                "{source} ({options:?})"
            );
        }
    }
}
