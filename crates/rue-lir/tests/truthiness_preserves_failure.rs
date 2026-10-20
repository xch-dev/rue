use clvmr::{Allocator, ChiaDialect, MEMPOOL_MODE, NodePtr, run_program};
use id_arena::Arena;
use rue_lir::{CodegenOptions, Lir};

#[derive(Clone, Copy, Debug)]
enum Context {
    If,
    IfListp,
    Not,
    All,
    Any,
}

#[test]
fn truthiness_does_not_discard_failing_cons_fields() {
    for context in [
        Context::If,
        Context::IfListp,
        Context::Not,
        Context::All,
        Context::Any,
    ] {
        for failing_first in [false, true] {
            for optimize in [false, true] {
                let mut arena = Arena::new();
                let nil = arena.alloc(Lir::Atom(vec![]));
                let one = arena.alloc(Lir::Atom(vec![1]));
                let failure = arena.alloc(Lir::Raise(vec![]));
                let pair = if failing_first {
                    arena.alloc(Lir::Cons(failure, nil))
                } else {
                    arena.alloc(Lir::Cons(nil, failure))
                };
                let condition = match context {
                    Context::IfListp => arena.alloc(Lir::Listp(pair, false)),
                    _ => pair,
                };
                let mut root = match context {
                    Context::If | Context::IfListp => {
                        arena.alloc(Lir::If(condition, one, nil, false))
                    }
                    Context::Not => arena.alloc(Lir::Not(condition)),
                    Context::All => arena.alloc(Lir::All(vec![condition, one])),
                    // The second always-truthy operand must still be evaluated.
                    Context::Any => arena.alloc(Lir::Any(vec![one, condition])),
                };
                if optimize {
                    root = rue_lir::optimize(&mut arena, root);
                }
                let mut allocator = Allocator::new();
                let program = rue_lir::codegen(
                    &arena,
                    &mut allocator,
                    root,
                    CodegenOptions {
                        optimize_static_pairs: false,
                    },
                )
                .unwrap();
                assert!(
                    run_program(
                        &mut allocator,
                        &ChiaDialect::new(MEMPOOL_MODE),
                        program,
                        NodePtr::NIL,
                        1_000_000
                    )
                    .is_err(),
                    "context={context:?}, failing_first={failing_first}, optimize={optimize} accepted a failing cons",
                );
            }
        }
    }
}

#[test]
fn truthiness_folds_static_cons() {
    let mut arena = Arena::new();
    let nil = arena.alloc(Lir::Atom(vec![]));
    let one = arena.alloc(Lir::Atom(vec![1]));
    let two = arena.alloc(Lir::Atom(vec![2]));
    let pair = arena.alloc(Lir::Cons(one, nil));
    let root = arena.alloc(Lir::If(pair, one, two, false));
    let root = rue_lir::optimize(&mut arena, root);
    assert!(matches!(&arena[root], Lir::Atom(atom) if atom == &[1]));
}
