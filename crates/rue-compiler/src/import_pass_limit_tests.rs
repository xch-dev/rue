use crate::{compile::PASS_LIMIT, test_utils::compile};

/// A chain of glob re-exports, each of the next module, which the first pass resolves from the
/// start, so each pass brings `X` one module further: 6 passes before diagnostics are reported, and
/// one more after.
const CHAIN: &str = "
    mod m0 { export super::m1::*; }
    mod m1 { export super::m2::*; }
    mod m2 { export super::m3::*; }
    mod m3 { export super::m4::*; }
    mod m4 { export super::m5::*; }
    mod m5 { export const X: Int = 1; }

    fn main() -> Int {
        m0::X
    }
";

/// The passes that resolving the chain's imports takes without diagnostics.
const CHAIN_PASSES: usize = 6;

/// The errors from compiling a source with the pass limit set to `limit`.
fn errors_with_limit(source: &str, limit: usize) -> Vec<String> {
    PASS_LIMIT.set(Some(limit));
    let compiled = compile(source);
    PASS_LIMIT.set(None);
    compiled.errors()
}

#[test]
fn imports_that_settle_at_the_pass_limit_are_resolved() {
    assert_eq!(errors_with_limit(CHAIN, CHAIN_PASSES), Vec::<String>::new());
}

#[test]
fn reaching_the_pass_limit_reports_an_internal_error() {
    // Resolving stops one pass short, which is reported at the first import. Resolving them again
    // with diagnostics finishes the chain, so nothing else is reported.
    assert_eq!(
        errors_with_limit(CHAIN, CHAIN_PASSES - 1),
        ["2:32 ImportPassLimit(5)"]
    );
}

#[test]
fn reaching_the_pass_limit_skips_the_other_import_errors() {
    // `import super::m0::X` only finds `X` once the chain has brought it in, so the imports'
    // other errors would report it as unresolved, although it's only resolving that stopped
    let source = "
        mod a { export super::m0::X; }
        mod m0 { export super::m1::*; }
        mod m1 { export super::m2::*; }
        mod m2 { export super::m3::*; }
        mod m3 { export const X: Int = 1; }

        fn main() -> Int {
            a::X
        }
    ";

    assert_eq!(
        errors_with_limit(source, 1),
        ["2:35 ImportPassLimit(1)", "9:16 UndeclaredSymbol(\"X\")"]
    );
}

#[test]
fn the_pass_limit_is_reported_once() {
    // Both calls to `resolve_imports` reach the limit, before resolving anything
    let errors = errors_with_limit(CHAIN, 0);

    assert_eq!(
        errors
            .iter()
            .filter(|error| error.contains("ImportPassLimit"))
            .collect::<Vec<_>>(),
        ["2:32 ImportPassLimit(0)"],
        "{errors:?}"
    );
}
