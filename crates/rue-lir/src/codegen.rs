use clvm_traits::{ToClvm, clvm_list, clvm_quote};
use clvmr::{
    Allocator, NodePtr, SExp,
    serde::{node_from_bytes, node_to_bytes},
};
use id_arena::Arena;
use num_bigint::BigUint;

use crate::{
    ClvmOp, ClvmTarget, DebugTags, KECCAK256_COST_PER_BYTE, KECCAK256_VERIFY_COST, Lir, LirId,
    NodeTags, Result, path_to_atom, stack::grow_stack,
};

#[derive(Debug, Default, Clone, Copy)]
pub struct CodegenOptions {
    pub optimize_static_pairs: bool,

    /// What the program is compiled for. After hard fork 2, the secp operators are the one byte
    /// opcodes 64 and 65, and before it, the four byte opcodes that encode their cost.
    pub target: ClvmTarget,
}

pub fn codegen(
    arena: &Arena<Lir>,
    allocator: &mut Allocator,
    lir: LirId,
    options: CodegenOptions,
) -> Result<NodePtr> {
    codegen_node(arena, allocator, lir, options, &mut None)
}

/// How many bytes the program that the LIR compiles to takes up when it's serialized, optimizing
/// it first if `optimize` is set, or `None` if it can't be generated. This is for comparing ways
/// of lowering the same code by what they cost on chain, where each byte costs
/// [`COST_PER_BYTE`](crate::COST_PER_BYTE). The LIR itself is left as it is.
pub fn compiled_size(
    arena: &mut Arena<Lir>,
    lir: LirId,
    optimize: bool,
    options: CodegenOptions,
) -> Option<usize> {
    let lir = if optimize {
        crate::optimize(arena, lir, options.target)
    } else {
        lir
    };

    let mut allocator = Allocator::new();
    let ptr = codegen(arena, &mut allocator, lir, options).ok()?;
    Some(node_to_bytes(&allocator, ptr).ok()?.len())
}

/// Generates CLVM like [`codegen`], and carries the tags of the LIR over to the pairs that each
/// node generates. A node's tag only applies to the pairs that aren't already tagged by the nodes
/// inside of it, so the innermost tag wins.
pub fn codegen_with_tags(
    arena: &Arena<Lir>,
    allocator: &mut Allocator,
    lir: LirId,
    options: CodegenOptions,
    tags: &DebugTags,
) -> Result<(NodePtr, NodeTags)> {
    let mut trace = Some(Trace {
        tags,
        nodes: NodeTags::default(),
    });
    let ptr = codegen_node(arena, allocator, lir, options, &mut trace)?;
    Ok((
        ptr,
        trace.expect("the trace is kept while generating").nodes,
    ))
}

/// What codegen keeps track of to carry tags over to the pairs it generates.
struct Trace<'a> {
    tags: &'a DebugTags,
    nodes: NodeTags,
}

impl Trace<'_> {
    fn tag(&mut self, allocator: &Allocator, lir: LirId, ptr: NodePtr) {
        if let SExp::Pair(..) = allocator.sexp(ptr) {
            if let Some(&function) = self.tags.functions.get(&lir) {
                self.nodes.functions.entry(ptr).or_insert(function);
            }

            if let Some(&label) = self.tags.labels.get(&lir) {
                self.nodes.labels.entry(ptr).or_insert(label);
            }
        }

        let Some(&span) = self.tags.spans.get(&lir) else {
            return;
        };

        // The pairs that inner nodes generated are already tagged, so this stops there, and only
        // visits each pair once overall
        let mut stack = vec![ptr];

        while let Some(ptr) = stack.pop() {
            if let SExp::Pair(first, rest) = allocator.sexp(ptr)
                && !self.nodes.spans.contains_key(&ptr)
            {
                self.nodes.spans.insert(ptr, span);
                stack.push(first);
                stack.push(rest);
            }
        }
    }
}

fn codegen_node(
    arena: &Arena<Lir>,
    allocator: &mut Allocator,
    lir: LirId,
    options: CodegenOptions,
    trace: &mut Option<Trace<'_>>,
) -> Result<NodePtr> {
    let ptr = codegen_step(arena, allocator, lir, options, trace)?;

    if let Some(trace) = trace {
        trace.tag(allocator, lir, ptr);
    }

    Ok(ptr)
}

fn codegen_step(
    arena: &Arena<Lir>,
    allocator: &mut Allocator,
    lir: LirId,
    options: CodegenOptions,
    trace: &mut Option<Trace<'_>>,
) -> Result<NodePtr> {
    let mut codegen = |arena: &Arena<Lir>, allocator: &mut Allocator, lir: LirId| {
        grow_stack(|| codegen_node(arena, allocator, lir, options, trace))
    };

    match &arena[lir] {
        Lir::Atom(atom) => {
            if atom.is_empty() {
                return Ok(NodePtr::NIL);
            }
            let atom = allocator.new_atom(atom)?;
            Ok(clvm_quote!(atom).to_clvm(allocator)?)
        }
        Lir::Quote(arg) => {
            let arg = codegen(arena, allocator, *arg)?;
            quote_code(allocator, arg)
        }
        Lir::Value(value) => {
            let value = node_from_bytes(allocator, value)?;
            quote_code(allocator, value)
        }
        Lir::Path(path) => {
            let atom = path_to_atom(path);
            Ok(allocator.new_atom(&atom)?)
        }
        Lir::Run(callee, env)
            if options.optimize_static_pairs
                && matches!(&arena[*env], Lir::Path(path) if path == &BigUint::from(1u8))
                && is_static_value(arena, *callee) =>
        {
            codegen_static_value(arena, allocator, *callee)
        }
        Lir::Cons(first, rest)
            if options.optimize_static_pairs
                && is_static_value(arena, *first)
                && is_static_value(arena, *rest) =>
        {
            let first = codegen_static_value(arena, allocator, *first)?;
            let rest = codegen_static_value(arena, allocator, *rest)?;
            let pair = allocator.new_pair(first, rest)?;
            Ok(clvm_quote!(pair).to_clvm(allocator)?)
        }
        Lir::Closure(function, captures, has_parameters) => {
            let function = codegen(arena, allocator, *function)?;
            let fold = options.optimize_static_pairs;

            let mut args = if *has_parameters {
                clvm_quote!(1).to_clvm(allocator)?
            } else {
                NodePtr::NIL
            };

            // Builds `(a (q . FUNCTION) (c (q . CAPTURE) ... 1))` at runtime, so the closure is
            // a program that runs the function with the captures in front of its arguments
            for capture in captures.iter().rev() {
                let capture = codegen(arena, allocator, *capture)?;
                args = operator_code(allocator, ClvmOp::Cons, capture, args, fold)?;
            }

            operator_code(allocator, ClvmOp::Apply, function, args, fold)
        }
        Lir::If(cond, then_lir, else_lir, inline) => {
            let cond = codegen(arena, allocator, *cond)?;
            let then_ptr = codegen(arena, allocator, *then_lir)?;
            let else_ptr = codegen(arena, allocator, *else_lir)?;

            if *inline {
                Ok(clvm_list!(ClvmOp::If, cond, then_ptr, else_ptr).to_clvm(allocator)?)
            } else {
                let then_ptr = quote_code(allocator, then_ptr)?;
                let else_ptr = quote_code(allocator, else_ptr)?;
                Ok(clvm_list!(
                    ClvmOp::Apply,
                    clvm_list!(ClvmOp::If, cond, then_ptr, else_ptr),
                    1
                )
                .to_clvm(allocator)?)
            }
        }
        Lir::Keccak256Verify(value, hash) => {
            let value = codegen(arena, allocator, *value)?;
            let hash = codegen(arena, allocator, *hash)?;
            keccak256_verify(allocator, options.target, value, hash)
        }
        Lir::Op(op, args) => {
            let args = codegen(arena, allocator, *args)?;
            Ok(clvm_list!(
                ClvmOp::Apply,
                clvm_list!(
                    ClvmOp::Cons,
                    clvm_list!(ClvmOp::Cons, clvm_quote!(op), ()),
                    args
                ),
                ()
            )
            .to_clvm(allocator)?)
        }
        Lir::DebugPrint(srcloc, value) => {
            let value = codegen(arena, allocator, *value)?;
            Ok(clvm_list!(ClvmOp::DebugPrint, clvm_quote!(srcloc), value).to_clvm(allocator)?)
        }
        node => operator_call(arena, allocator, node, options.target, codegen),
    }
}

/// Generates `(OPERATOR CHILD ...)` for a node that's called as its operator (see
/// [`Lir::operator`]), with the code for each child as an operand.
fn operator_call(
    arena: &Arena<Lir>,
    allocator: &mut Allocator,
    node: &Lir,
    target: ClvmTarget,
    mut codegen: impl FnMut(&Arena<Lir>, &mut Allocator, LirId) -> Result<NodePtr>,
) -> Result<NodePtr> {
    let operator = node
        .operator()
        .expect("every other node is generated as a call to its operator");
    let args = node
        .children()
        .map(|arg| codegen(arena, allocator, arg))
        .collect::<Result<Vec<_>>>()?;
    let operator = allocator.new_atom(&operator.to_target_atom(target))?;
    Ok((operator, args).to_clvm(allocator)?)
}

/// Generates code that evaluates to the value, which is `(q . VALUE)`, or `()` for nil, since nil
/// evaluates to itself and is two bytes shorter than `(q)`. Either form is a constant to
/// [`constant_code`], so what's built from it is the same.
fn quote_code(allocator: &mut Allocator, value: NodePtr) -> Result<NodePtr> {
    if is_nil_code(allocator, value) {
        return Ok(NodePtr::NIL);
    }

    Ok(clvm_quote!(value).to_clvm(allocator)?)
}

fn is_nil_code(allocator: &Allocator, code: NodePtr) -> bool {
    matches!(allocator.sexp(code), SExp::Atom) && allocator.atom(code).as_ref().is_empty()
}

/// Generates code that builds `(OPERATOR (q . VALUE) ARGS)` at runtime from the code for `VALUE`
/// and `ARGS`, which is `(c (q . OPERATOR) (c (c (q . 1) VALUE) (c ARGS ())))` with its constant
/// parts quoted if `fold` is set. A value that's always nil is built as `()` rather than `(q)`,
/// like [`quote_code`] does.
fn operator_code(
    allocator: &mut Allocator,
    operator: ClvmOp,
    value: NodePtr,
    args: NodePtr,
    fold: bool,
) -> Result<NodePtr> {
    let quoted_value = if is_nil_code(allocator, value) {
        NodePtr::NIL
    } else {
        let quote = clvm_quote!(ClvmOp::Quote).to_clvm(allocator)?;
        cons_code(allocator, quote, value, fold)?
    };
    let rest = cons_code(allocator, args, NodePtr::NIL, fold)?;
    let rest = cons_code(allocator, quoted_value, rest, fold)?;
    let operator = clvm_quote!(operator).to_clvm(allocator)?;
    cons_code(allocator, operator, rest, fold)
}

/// Generates `(c FIRST REST)`, or the quoted pair if both are constant and `fold` is set.
fn cons_code(
    allocator: &mut Allocator,
    first: NodePtr,
    rest: NodePtr,
    fold: bool,
) -> Result<NodePtr> {
    if fold
        && let Some(first) = constant_code(allocator, first)
        && let Some(rest) = constant_code(allocator, rest)
    {
        let pair = allocator.new_pair(first, rest)?;
        return Ok(clvm_quote!(pair).to_clvm(allocator)?);
    }

    Ok(clvm_list!(ClvmOp::Cons, first, rest).to_clvm(allocator)?)
}

/// The value of generated code that's constant: nil, or a quoted value `(q . VALUE)`. Any other
/// atom is a path into the environment.
fn constant_code(allocator: &Allocator, code: NodePtr) -> Option<NodePtr> {
    match allocator.sexp(code) {
        SExp::Atom => is_nil_code(allocator, code).then_some(NodePtr::NIL),
        SExp::Pair(first, rest) => match allocator.sexp(first) {
            SExp::Atom if allocator.atom(first).as_ref() == ClvmOp::Quote.to_atom() => Some(rest),
            _ => None,
        },
    }
}

fn is_static_value(arena: &Arena<Lir>, lir: LirId) -> bool {
    match &arena[lir] {
        Lir::Atom(_) => true,
        Lir::Cons(first, rest) => {
            grow_stack(|| is_static_value(arena, *first) && is_static_value(arena, *rest))
        }
        _ => false,
    }
}

fn codegen_static_value(
    arena: &Arena<Lir>,
    allocator: &mut Allocator,
    lir: LirId,
) -> Result<NodePtr> {
    match &arena[lir] {
        Lir::Atom(atom) => {
            if atom.is_empty() {
                Ok(NodePtr::NIL)
            } else {
                Ok(allocator.new_atom(atom)?)
            }
        }
        Lir::Cons(first, rest) => grow_stack(|| {
            let first = codegen_static_value(arena, allocator, *first)?;
            let rest = codegen_static_value(arena, allocator, *rest)?;
            Ok(allocator.new_pair(first, rest)?)
        }),
        _ => unreachable!("static LIR value contained a dynamic expression"),
    }
}

/// Generates a check that fails unless the Keccak-256 hash of the value is the hash, which is
/// `(a (i (= (keccak256 VALUE) HASH) () (q 8)) 1)`.
///
/// Before hard fork 2, `keccak256` is only available inside the softfork guard (extension 1), which
/// always returns nil, so only the check can be run there. The values are put in the environment,
/// so that each is only evaluated once, and the guard declares the exact cost of the check, which
/// depends on the length of the value:
///
/// `(a (q softfork (+ (q . 1230) (* (strlen 2) (q . 2))) (q . 1) (q . CHECK) 1) (c VALUE (c HASH ())))`
///
/// where `CHECK` reads the value and hash from paths 2 and 5. If the check fails, the guard fails
/// before its cost is compared, so the declared cost only has to be right when the hash matches.
fn keccak256_verify(
    allocator: &mut Allocator,
    target: ClvmTarget,
    value: NodePtr,
    hash: NodePtr,
) -> Result<NodePtr> {
    if target.hard_fork_2 {
        return keccak256_check(allocator, value, hash);
    }

    let value_path = 2u8.to_clvm(allocator)?;
    let hash_path = 5u8.to_clvm(allocator)?;
    let check = keccak256_check(allocator, value_path, hash_path)?;

    let cost = clvm_list!(
        ClvmOp::Add,
        clvm_quote!(KECCAK256_VERIFY_COST),
        clvm_list!(
            ClvmOp::Mul,
            clvm_list!(ClvmOp::Strlen, 2u8),
            clvm_quote!(KECCAK256_COST_PER_BYTE)
        )
    );
    let guard = clvm_list!(
        ClvmOp::Softfork,
        cost,
        clvm_quote!(1u8),
        clvm_quote!(check),
        1u8
    );
    let env = clvm_list!(ClvmOp::Cons, value, clvm_list!(ClvmOp::Cons, hash, ()));

    Ok(clvm_list!(ClvmOp::Apply, clvm_quote!(guard), env).to_clvm(allocator)?)
}

/// `(a (i (= (keccak256 VALUE) HASH) () (q 8)) 1)`, which fails unless the hash matches.
fn keccak256_check(allocator: &mut Allocator, value: NodePtr, hash: NodePtr) -> Result<NodePtr> {
    let condition = clvm_list!(ClvmOp::Eq, clvm_list!(ClvmOp::Keccak256, value), hash);
    let check = clvm_list!(
        ClvmOp::If,
        condition,
        (),
        clvm_quote!(clvm_list!(ClvmOp::Raise))
    );

    Ok(clvm_list!(ClvmOp::Apply, check, 1u8).to_clvm(allocator)?)
}

#[cfg(test)]
mod tests {
    use expect_test::{Expect, expect};
    use rue_clvm::disassemble_program;

    use super::*;

    /// Runs `keccak256_verify` on values of many lengths, with their hash and with a wrong one,
    /// for each target, and with the flags of blocks as well as the mempool. The softfork guard
    /// fails unless the cost it declares is exactly the cost of the check, so a matching hash
    /// only passes if the cost is right for the length of the value.
    ///
    /// A program compiled before hard fork 2 is also run with the flags after it, since a coin
    /// locked to its puzzle hash before the fork must still be spendable after it, where the guard
    /// is run with the new cost model.
    #[test]
    fn keccak256_verify_declares_the_exact_cost() {
        use clvmr::{ChiaDialect, ClvmFlags, run_program};
        use sha3::{Digest, Keccak256};

        let mut arena = Arena::new();
        let value = arena.alloc(Lir::Path(BigUint::from(2u8)));
        let hash = arena.alloc(Lir::Path(BigUint::from(5u8)));
        let lir = arena.alloc(Lir::Keccak256Verify(value, hash));

        // Blocks don't add the mempool's restrictions, and allow unknown operators
        let block = |target: ClvmTarget| {
            target.flags().difference(
                ClvmFlags::NO_UNKNOWN_OPS | ClvmFlags::LIMIT_HEAP | ClvmFlags::LIMIT_SOFTFORK,
            )
        };

        for target in ClvmTarget::ALL {
            let mut allocator = Allocator::new();
            let program = codegen(
                &arena,
                &mut allocator,
                lir,
                CodegenOptions {
                    optimize_static_pairs: true,
                    target,
                },
            )
            .unwrap();

            let mut flag_sets = vec![target.flags(), block(target)];

            if !target.hard_fork_2 {
                flag_sets.extend([
                    ClvmTarget::HARD_FORK_2.flags(),
                    block(ClvmTarget::HARD_FORK_2),
                ]);
            }

            for flags in flag_sets {
                for len in (0..300).chain([1000, 4096]) {
                    let value = vec![0x61; len];
                    let digest = Keccak256::digest(&value).to_vec();

                    for (hash, passes) in [(digest, true), (vec![0; 32], false)] {
                        let value_ptr = allocator.new_atom(&value).unwrap();
                        let hash_ptr = allocator.new_atom(&hash).unwrap();
                        let env = clvm_list!(value_ptr, hash_ptr)
                            .to_clvm(&mut allocator)
                            .unwrap();
                        let result = run_program(
                            &mut allocator,
                            &ChiaDialect::new(flags),
                            program,
                            env,
                            crate::MAX_BLOCK_COST,
                        );
                        assert_eq!(
                            result.is_ok(),
                            passes,
                            "{target:?} {flags:?} for a value of {len} bytes: {result:?}"
                        );
                    }
                }
            }
        }
    }

    #[allow(clippy::needless_pass_by_value)]
    fn check(arena: &Arena<Lir>, lir: LirId, expect: Expect) {
        let mut allocator = Allocator::new();
        let ptr = codegen(arena, &mut allocator, lir, CodegenOptions::default()).unwrap();
        let result = disassemble_program(&allocator, ptr);
        expect.assert_eq(&result);
    }

    #[allow(clippy::needless_pass_by_value)]
    fn check_with_options(
        arena: &Arena<Lir>,
        lir: LirId,
        optimize_static_pairs: bool,
        expect: Expect,
    ) {
        let mut allocator = Allocator::new();
        let ptr = codegen(
            arena,
            &mut allocator,
            lir,
            CodegenOptions {
                optimize_static_pairs,
                target: ClvmTarget::default(),
            },
        )
        .unwrap();
        let result = disassemble_program(&allocator, ptr);
        expect.assert_eq(&result);
    }

    #[test]
    fn test_atom() {
        let mut arena = Arena::new();
        let lir = arena.alloc(Lir::Atom(b"hello".to_vec()));
        check(&arena, lir, expect![[r#"(q . "hello")"#]]);
    }

    #[test]
    fn test_nil() {
        let mut arena = Arena::new();
        let lir = arena.alloc(Lir::Atom(Vec::new()));
        check(&arena, lir, expect!["()"]);
    }

    #[test]
    fn test_path() {
        let mut arena = Arena::new();
        let lir = arena.alloc(Lir::Path(1u8.into()));
        check(&arena, lir, expect!["1"]);
    }

    #[test]
    fn test_quote() {
        let mut arena = Arena::new();
        let lir = arena.alloc(Lir::Atom(b"hello".to_vec()));
        let quote = arena.alloc(Lir::Quote(lir));
        check(&arena, quote, expect![[r#"(q 1 . "hello")"#]]);
    }

    #[test]
    fn quoted_nil_is_left_unquoted() {
        let mut arena = Arena::new();
        let nil = arena.alloc(Lir::Atom(Vec::new()));
        let quote = arena.alloc(Lir::Quote(nil));
        check(&arena, quote, expect!["()"]);

        // Code that evaluates to nil, but isn't nil itself, is still quoted
        let not = arena.alloc(Lir::Not(quote));
        let listp = arena.alloc(Lir::Listp(not));
        let quote = arena.alloc(Lir::Quote(listp));
        check(&arena, quote, expect!["(q 7 (not ()))"]);
    }

    /// Nil is the same code quoted or not, so everything that uses a quoted value as data builds
    /// the same value from it: the branches of an `if`, a function value that's run or captured,
    /// and static pairs.
    #[test]
    fn unquoted_nil_is_the_same_value() {
        use clvmr::{ChiaDialect, ClvmFlags, run_program};

        let mut arena = Arena::new();
        let nil = arena.alloc(Lir::Atom(Vec::new()));
        let one = arena.alloc(Lir::Atom(vec![1]));
        let raise = arena.alloc(Lir::Raise(vec![]));
        let condition = arena.alloc(Lir::Path(2u8.into()));
        let capture = arena.alloc(Lir::Path(3u8.into()));
        let entire_env = arena.alloc(Lir::Path(1u8.into()));
        let nil_function = arena.alloc(Lir::Quote(nil));
        let pair = arena.alloc(Lir::Cons(nil, one));
        let nil_pair = arena.alloc(Lir::Cons(nil_function, nil));
        let dynamic_pair = arena.alloc(Lir::Cons(nil_function, capture));

        let cases = [
            (
                arena.alloc(Lir::If(condition, nil, raise, false)),
                expect!["(a (i 2 () (q 8)) 1)"],
                [Some("()"), None],
            ),
            (
                arena.alloc(Lir::If(condition, one, nil, false)),
                expect!["(a (i 2 (q 1 . 1) ()) 1)"],
                [Some("1"), Some("()")],
            ),
            (
                arena.alloc(Lir::If(condition, nil, nil, false)),
                expect!["(a (i 2 () ()) 1)"],
                [Some("()"), Some("()")],
            ),
            (
                arena.alloc(Lir::If(condition, nil, one, true)),
                expect!["(i 2 () (q . 1))"],
                [Some("()"), Some("1")],
            ),
            (
                arena.alloc(Lir::Run(nil_function, entire_env)),
                expect!["(a () 1)"],
                [Some("()"), Some("()")],
            ),
            (
                arena.alloc(Lir::Closure(nil_function, vec![], true)),
                expect!["(q 2 () 1)"],
                [Some("(a () 1)"), Some("(a () 1)")],
            ),
            (
                arena.alloc(Lir::Closure(nil_function, vec![nil, capture], false)),
                expect![
                    "(c (q . 2) (c () (c (c (q . 4) (c () (c (c (q . 4) (c (c (q . 1) 3) (q ()))) ()))) ())))"
                ],
                [
                    Some("(a () (c () (c (q . 7) ())))"),
                    Some("(a () (c () (c (q . 7) ())))"),
                ],
            ),
            (
                pair,
                expect!["(q () . 1)"],
                [Some("(() . 1)"), Some("(() . 1)")],
            ),
            (nil_pair, expect!["(c () ())"], [Some("(())"), Some("(())")]),
            (
                dynamic_pair,
                expect!["(c () 3)"],
                [Some("(() . 7)"), Some("(() . 7)")],
            ),
        ];

        for (lir, expect, outputs) in cases {
            check_with_options(&arena, lir, true, expect);

            for optimize_static_pairs in [false, true] {
                for (condition, expected) in ["1", "()"].into_iter().zip(outputs) {
                    let mut allocator = Allocator::new();
                    let program = codegen(
                        &arena,
                        &mut allocator,
                        lir,
                        CodegenOptions {
                            optimize_static_pairs,
                            ..Default::default()
                        },
                    )
                    .unwrap();
                    let env =
                        rue_clvm::assemble(&mut allocator, &format!("({condition} . 7)")).unwrap();
                    let output = run_program(
                        &mut allocator,
                        &ChiaDialect::new(ClvmFlags::empty()),
                        program,
                        env,
                        u64::MAX,
                    )
                    .ok()
                    .map(|output| disassemble_program(&allocator, output.1));
                    assert_eq!(output.as_deref(), expected);
                }
            }
        }
    }

    #[test]
    fn test_run() {
        let mut arena = Arena::new();
        let a = arena.alloc(Lir::Atom(b"a".to_vec()));
        let b = arena.alloc(Lir::Atom(b"b".to_vec()));
        let c = arena.alloc(Lir::Atom(b"c".to_vec()));
        let mut env = arena.alloc(Lir::Atom(Vec::new()));
        for &arg in [b, c].iter().rev() {
            env = arena.alloc(Lir::Cons(arg, env));
        }
        let lir = arena.alloc(Lir::Run(a, env));
        check(
            &arena,
            lir,
            expect!["(a (q . 97) (c (q . 98) (c (q . 99) ())))"],
        );
    }

    #[test]
    fn test_run_static_program_with_entire_env() {
        let mut arena = Arena::new();
        let nil = arena.alloc(Lir::Atom(Vec::new()));
        let five = arena.alloc(Lir::Atom(vec![5]));
        let args = arena.alloc(Lir::Cons(five, nil));
        let two = arena.alloc(Lir::Atom(vec![2]));
        let args = arena.alloc(Lir::Cons(two, args));
        let add = arena.alloc(Lir::Atom(vec![16]));
        let program = arena.alloc(Lir::Cons(add, args));
        let env = arena.alloc(Lir::Path(1u8.into()));
        let lir = arena.alloc(Lir::Run(program, env));

        check_with_options(
            &arena,
            lir,
            false,
            expect!["(a (c (q . 16) (c (q . 2) (c (q . 5) ()))) 1)"],
        );
        check_with_options(&arena, lir, true, expect!["(+ 2 5)"]);
    }

    #[test]
    fn test_closure() {
        let mut arena = Arena::new();
        let path = arena.alloc(Lir::Path(2u8.into()));
        let add = arena.alloc(Lir::Add(vec![path, path]));
        let function = arena.alloc(Lir::Quote(add));
        let capture = arena.alloc(Lir::Atom(vec![0x10]));
        let closure = arena.alloc(Lir::Closure(function, vec![capture], true));
        check(
            &arena,
            closure,
            expect![
                "(c (q . 2) (c (c (q . 1) (q 16 2 2)) (c (c (q . 4) (c (c (q . 1) (q . 16)) (c (q . 1) ()))) ())))"
            ],
        );
    }

    /// The code that builds a closure quotes its constant parts when static pairs are optimized,
    /// and builds the same closure either way.
    #[test]
    fn closure_with_constant_parts() {
        use clvmr::{ChiaDialect, ClvmFlags, run_program};

        let mut arena = Arena::new();
        let path = arena.alloc(Lir::Path(2u8.into()));
        let add = arena.alloc(Lir::Add(vec![path, path]));
        let function = arena.alloc(Lir::Quote(add));
        let constant = arena.alloc(Lir::Atom(vec![0x10]));
        let nil = arena.alloc(Lir::Atom(vec![]));
        let env_capture = arena.alloc(Lir::Path(5u8.into()));
        let env_function = arena.alloc(Lir::Path(2u8.into()));

        let closures = [
            (
                arena.alloc(Lir::Closure(function, vec![constant], true)),
                expect!["(q 2 (q 16 2 2) (c (q . 16) 1))"],
            ),
            (
                arena.alloc(Lir::Closure(function, vec![constant, nil], false)),
                expect!["(q 2 (q 16 2 2) (c (q . 16) (c () ())))"],
            ),
            (
                arena.alloc(Lir::Closure(function, vec![env_capture, constant], true)),
                expect![
                    "(c (q . 2) (c (q 1 16 2 2) (c (c (q . 4) (c (c (q . 1) 5) (q (c (q . 16) 1)))) ())))"
                ],
            ),
            (
                arena.alloc(Lir::Closure(env_function, vec![env_capture], false)),
                expect![
                    "(c (q . 2) (c (c (q . 1) 2) (c (c (q . 4) (c (c (q . 1) 5) (q ()))) ())))"
                ],
            ),
        ];

        for (closure, expect) in closures {
            check_with_options(&arena, closure, true, expect);

            let mut built = Vec::new();

            for optimize_static_pairs in [false, true] {
                let mut allocator = Allocator::new();
                let program = codegen(
                    &arena,
                    &mut allocator,
                    closure,
                    CodegenOptions {
                        optimize_static_pairs,
                        ..Default::default()
                    },
                )
                .unwrap();
                let env = clvm_list!(clvm_quote!(1u8), 7u8)
                    .to_clvm(&mut allocator)
                    .unwrap();
                let value = run_program(
                    &mut allocator,
                    &ChiaDialect::new(ClvmFlags::empty()),
                    program,
                    env,
                    u64::MAX,
                )
                .unwrap()
                .1;
                built.push(disassemble_program(&allocator, value));
            }

            assert_eq!(built[0], built[1]);
        }
    }

    #[test]
    fn test_first() {
        let mut arena = Arena::new();
        let first = arena.alloc(Lir::Atom(b"first".to_vec()));
        let rest = arena.alloc(Lir::Atom(b"rest".to_vec()));
        let pair = arena.alloc(Lir::Cons(first, rest));
        let lir = arena.alloc(Lir::First(pair));
        check(
            &arena,
            lir,
            expect![[r#"(f (c (q . "first") (q . "rest")))"#]],
        );
    }

    #[test]
    fn test_rest() {
        let mut arena = Arena::new();
        let first = arena.alloc(Lir::Atom(b"first".to_vec()));
        let rest = arena.alloc(Lir::Atom(b"rest".to_vec()));
        let pair = arena.alloc(Lir::Cons(first, rest));
        let lir = arena.alloc(Lir::Rest(pair));
        check(
            &arena,
            lir,
            expect![[r#"(r (c (q . "first") (q . "rest")))"#]],
        );
    }

    #[test]
    fn test_cons() {
        let mut arena = Arena::new();
        let first = arena.alloc(Lir::Atom(b"first".to_vec()));
        let rest = arena.alloc(Lir::Atom(b"rest".to_vec()));
        let lir = arena.alloc(Lir::Cons(first, rest));
        check(&arena, lir, expect![[r#"(c (q . "first") (q . "rest"))"#]]);
        check_with_options(&arena, lir, true, expect![[r#"(q "first" . "rest")"#]]);

        let path = arena.alloc(Lir::Path(2u8.into()));
        let lir = arena.alloc(Lir::Cons(path, lir));
        check_with_options(
            &arena,
            lir,
            true,
            expect![[r#"(c 2 (q "first" . "rest"))"#]],
        );
    }

    #[test]
    fn test_listp() {
        let mut arena = Arena::new();
        let value = arena.alloc(Lir::Atom(b"value".to_vec()));
        let lir = arena.alloc(Lir::Listp(value));
        check(&arena, lir, expect![[r#"(l (q . "value"))"#]]);
    }

    #[test]
    fn test_add() {
        let mut arena = Arena::new();
        let a = arena.alloc(Lir::Atom(vec![0x01]));
        let b = arena.alloc(Lir::Atom(vec![0x02]));
        let lir = arena.alloc(Lir::Add(vec![a, b]));
        check(&arena, lir, expect!["(+ (q . 1) (q . 2))"]);
    }

    #[test]
    fn test_sub() {
        let mut arena = Arena::new();
        let a = arena.alloc(Lir::Atom(vec![0x01]));
        let b = arena.alloc(Lir::Atom(vec![0x02]));
        let lir = arena.alloc(Lir::Sub(vec![a, b]));
        check(&arena, lir, expect!["(- (q . 1) (q . 2))"]);
    }

    #[test]
    fn test_mul() {
        let mut arena = Arena::new();
        let a = arena.alloc(Lir::Atom(vec![0x01]));
        let b = arena.alloc(Lir::Atom(vec![0x02]));
        let lir = arena.alloc(Lir::Mul(vec![a, b]));
        check(&arena, lir, expect!["(* (q . 1) (q . 2))"]);
    }

    #[test]
    fn test_div() {
        let mut arena = Arena::new();
        let a = arena.alloc(Lir::Atom(vec![0x01]));
        let b = arena.alloc(Lir::Atom(vec![0x02]));
        let lir = arena.alloc(Lir::Div(a, b));
        check(&arena, lir, expect!["(/ (q . 1) (q . 2))"]);
    }

    #[test]
    fn test_divmod() {
        let mut arena = Arena::new();
        let a = arena.alloc(Lir::Atom(vec![0x34]));
        let b = arena.alloc(Lir::Atom(vec![0x07]));
        let lir = arena.alloc(Lir::Divmod(a, b));
        check(&arena, lir, expect!["(divmod (q . 52) (q . 7))"]);
    }

    #[test]
    fn test_mod() {
        let mut arena = Arena::new();
        let a = arena.alloc(Lir::Atom(vec![0x34]));
        let b = arena.alloc(Lir::Atom(vec![0x07]));
        let lir = arena.alloc(Lir::Mod(a, b));
        check(&arena, lir, expect!["(% (q . 52) (q . 7))"]);
    }

    #[test]
    fn test_modpow() {
        let mut arena = Arena::new();
        let a = arena.alloc(Lir::Atom(vec![0x34]));
        let b = arena.alloc(Lir::Atom(vec![0x07]));
        let c = arena.alloc(Lir::Atom(vec![0x03]));
        let lir = arena.alloc(Lir::Modpow(a, b, c));
        check(&arena, lir, expect!["(modpow (q . 52) (q . 7) (q . 3))"]);
    }

    #[test]
    fn test_eq() {
        let mut arena = Arena::new();
        let a = arena.alloc(Lir::Atom(vec![0x01]));
        let b = arena.alloc(Lir::Atom(vec![0x02]));
        let lir = arena.alloc(Lir::Eq(a, b));
        check(&arena, lir, expect!["(= (q . 1) (q . 2))"]);
    }

    #[test]
    fn test_gt() {
        let mut arena = Arena::new();
        let a = arena.alloc(Lir::Atom(vec![0x01]));
        let b = arena.alloc(Lir::Atom(vec![0x02]));
        let lir = arena.alloc(Lir::Gt(a, b));
        check(&arena, lir, expect!["(> (q . 1) (q . 2))"]);
    }

    #[test]
    fn test_not() {
        let mut arena = Arena::new();
        let a = arena.alloc(Lir::Atom(vec![0x01]));
        let lir = arena.alloc(Lir::Not(a));
        check(&arena, lir, expect!["(not (q . 1))"]);
    }

    #[test]
    fn test_all() {
        let mut arena = Arena::new();
        let a = arena.alloc(Lir::Atom(vec![0x01]));
        let b = arena.alloc(Lir::Atom(vec![0x02]));
        let lir = arena.alloc(Lir::All(vec![a, b]));
        check(&arena, lir, expect!["(all (q . 1) (q . 2))"]);
    }

    #[test]
    fn test_any() {
        let mut arena = Arena::new();
        let a = arena.alloc(Lir::Atom(vec![0x01]));
        let b = arena.alloc(Lir::Atom(vec![0x02]));
        let lir = arena.alloc(Lir::Any(vec![a, b]));
        check(&arena, lir, expect!["(any (q . 1) (q . 2))"]);
    }

    #[test]
    fn test_if() {
        let mut arena = Arena::new();
        let cond = arena.alloc(Lir::Atom(vec![0x01]));
        let then = arena.alloc(Lir::Atom(vec![0x02]));
        let else_ = arena.alloc(Lir::Atom(vec![0x03]));
        let lir = arena.alloc(Lir::If(cond, then, else_, false));
        check(
            &arena,
            lir,
            expect!["(a (i (q . 1) (q 1 . 2) (q 1 . 3)) 1)"],
        );
    }

    #[test]
    fn test_raise() {
        let mut arena = Arena::new();
        let a = arena.alloc(Lir::Atom(vec![0x01]));
        let lir = arena.alloc(Lir::Raise(vec![a]));
        check(&arena, lir, expect!["(x (q . 1))"]);
    }

    #[test]
    fn test_gt_bytes() {
        let mut arena = Arena::new();
        let a = arena.alloc(Lir::Atom(b"abc".to_vec()));
        let b = arena.alloc(Lir::Atom(b"def".to_vec()));
        let lir = arena.alloc(Lir::GtBytes(a, b));
        check(&arena, lir, expect![[r#"(>s (q . "abc") (q . "def"))"#]]);
    }

    #[test]
    fn test_concat() {
        let mut arena = Arena::new();
        let a = arena.alloc(Lir::Atom(b"hello ".to_vec()));
        let b = arena.alloc(Lir::Atom(b"world".to_vec()));
        let lir = arena.alloc(Lir::Concat(vec![a, b]));
        check(
            &arena,
            lir,
            expect![[r#"(concat (q . "hello ") (q . "world"))"#]],
        );
    }

    #[test]
    fn test_strlen() {
        let mut arena = Arena::new();
        let a = arena.alloc(Lir::Atom(b"hello".to_vec()));
        let lir = arena.alloc(Lir::Strlen(a));
        check(&arena, lir, expect![[r#"(strlen (q . "hello"))"#]]);
    }

    #[test]
    fn test_substr() {
        let mut arena = Arena::new();
        let str = arena.alloc(Lir::Atom(b"hello world".to_vec()));
        let start = arena.alloc(Lir::Atom(vec![]));
        let end = arena.alloc(Lir::Atom(vec![0x05]));
        let lir = arena.alloc(Lir::Substr(str, start, Some(end)));
        check(
            &arena,
            lir,
            expect![[r#"(substr (q . "hello world") () (q . 5))"#]],
        );

        // Test without end parameter
        let lir = arena.alloc(Lir::Substr(str, start, None));
        check(&arena, lir, expect![[r#"(substr (q . "hello world") ())"#]]);
    }

    #[test]
    fn test_bitwise_ops() {
        let mut arena = Arena::new();
        let a = arena.alloc(Lir::Atom(vec![0x0F]));
        let b = arena.alloc(Lir::Atom(vec![0xF0]));

        let logand = arena.alloc(Lir::Logand(vec![a, b]));
        check(&arena, logand, expect!["(logand (q . 15) (q . -16))"]);

        let logior = arena.alloc(Lir::Logior(vec![a, b]));
        check(&arena, logior, expect!["(logior (q . 15) (q . -16))"]);

        let logxor = arena.alloc(Lir::Logxor(vec![a, b]));
        check(&arena, logxor, expect!["(logxor (q . 15) (q . -16))"]);

        let lognot = arena.alloc(Lir::Lognot(a));
        check(&arena, lognot, expect!["(lognot (q . 15))"]);
    }

    #[test]
    fn test_shifts() {
        let mut arena = Arena::new();
        let value = arena.alloc(Lir::Atom(vec![0x0F]));
        let shift = arena.alloc(Lir::Atom(vec![0x02]));

        let ash = arena.alloc(Lir::Ash(value, shift));
        check(&arena, ash, expect!["(ash (q . 15) (q . 2))"]);

        let lsh = arena.alloc(Lir::Lsh(value, shift));
        check(&arena, lsh, expect!["(lsh (q . 15) (q . 2))"]);
    }

    #[test]
    fn test_pubkey_for_exp() {
        let mut arena = Arena::new();
        let exp = arena.alloc(Lir::Atom(vec![0x01]));
        let lir = arena.alloc(Lir::PubkeyForExp(exp));
        check(&arena, lir, expect!["(pubkey_for_exp (q . 1))"]);
    }

    #[test]
    fn test_g1_ops() {
        let mut arena = Arena::new();
        let a = arena.alloc(Lir::Atom(vec![0x01]));
        let b = arena.alloc(Lir::Atom(vec![0x02]));

        let add = arena.alloc(Lir::G1Add(vec![a, b]));
        check(&arena, add, expect!["(point_add (q . 1) (q . 2))"]);

        let sub = arena.alloc(Lir::G1Subtract(vec![a, b]));
        check(&arena, sub, expect!["(g1_subtract (q . 1) (q . 2))"]);

        let mul = arena.alloc(Lir::G1Multiply(a, b));
        check(&arena, mul, expect!["(g1_multiply (q . 1) (q . 2))"]);

        let neg = arena.alloc(Lir::G1Negate(a));
        check(&arena, neg, expect!["(g1_negate (q . 1))"]);

        let map = arena.alloc(Lir::G1Map(a, Some(b)));
        check(&arena, map, expect!["(g1_map (q . 1) (q . 2))"]);

        let map_no_dst = arena.alloc(Lir::G1Map(a, None));
        check(&arena, map_no_dst, expect!["(g1_map (q . 1))"]);
    }

    #[test]
    fn test_g2_ops() {
        let mut arena = Arena::new();
        let a = arena.alloc(Lir::Atom(vec![0x01]));
        let b = arena.alloc(Lir::Atom(vec![0x02]));

        let add = arena.alloc(Lir::G2Add(vec![a, b]));
        check(&arena, add, expect!["(g2_add (q . 1) (q . 2))"]);

        let sub = arena.alloc(Lir::G2Subtract(vec![a, b]));
        check(&arena, sub, expect!["(g2_subtract (q . 1) (q . 2))"]);

        let mul = arena.alloc(Lir::G2Multiply(a, b));
        check(&arena, mul, expect!["(g2_multiply (q . 1) (q . 2))"]);

        let neg = arena.alloc(Lir::G2Negate(a));
        check(&arena, neg, expect!["(g2_negate (q . 1))"]);

        let map = arena.alloc(Lir::G2Map(a, Some(b)));
        check(&arena, map, expect!["(g2_map (q . 1) (q . 2))"]);

        let map_no_dst = arena.alloc(Lir::G2Map(a, None));
        check(&arena, map_no_dst, expect!["(g2_map (q . 1))"]);
    }

    #[test]
    fn test_bls_ops() {
        let mut arena = Arena::new();
        let a = arena.alloc(Lir::Atom(vec![0x01]));
        let b = arena.alloc(Lir::Atom(vec![0x02]));

        let identity = arena.alloc(Lir::BlsPairingIdentity(vec![a, b]));
        check(
            &arena,
            identity,
            expect!["(bls_pairing_identity (q . 1) (q . 2))"],
        );

        let verify = arena.alloc(Lir::BlsVerify(a, vec![b]));
        check(&arena, verify, expect!["(bls_verify (q . 1) (q . 2))"]);
    }

    #[test]
    fn test_hash_functions() {
        let mut arena = Arena::new();
        let a = arena.alloc(Lir::Atom(b"hello".to_vec()));
        let b = arena.alloc(Lir::Atom(b"world".to_vec()));

        let sha256 = arena.alloc(Lir::Sha256(vec![a, b]));
        check(
            &arena,
            sha256,
            expect![[r#"(sha256 (q . "hello") (q . "world"))"#]],
        );

        let keccak256 = arena.alloc(Lir::Keccak256(vec![a, b]));
        check(
            &arena,
            keccak256,
            expect![[r#"(keccak256 (q . "hello") (q . "world"))"#]],
        );
    }

    #[test]
    fn test_coin_id() {
        let mut arena = Arena::new();
        let parent = arena.alloc(Lir::Atom(vec![0x01]));
        let puzzle = arena.alloc(Lir::Atom(vec![0x02]));
        let amount = arena.alloc(Lir::Atom(vec![0x03]));
        let lir = arena.alloc(Lir::CoinId(parent, puzzle, amount));
        check(&arena, lir, expect!["(coinid (q . 1) (q . 2) (q . 3))"]);
    }

    #[test]
    fn test_signature_verification() {
        let mut arena = Arena::new();
        let pubkey = arena.alloc(Lir::Atom(vec![0x01]));
        let message = arena.alloc(Lir::Atom(vec![0x02]));
        let signature = arena.alloc(Lir::Atom(vec![0x03]));

        let k1_verify = arena.alloc(Lir::K1Verify(pubkey, message, signature));
        check(
            &arena,
            k1_verify,
            expect!["(secp256k1_verify (q . 1) (q . 2) (q . 3))"],
        );

        let r1_verify = arena.alloc(Lir::R1Verify(pubkey, message, signature));
        check(
            &arena,
            r1_verify,
            expect!["(secp256r1_verify (q . 1) (q . 2) (q . 3))"],
        );
    }
}
