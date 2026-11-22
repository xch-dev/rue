use std::collections::HashMap;

use id_arena::Arena;

use crate::{DebugTags, Lir, LirId, is_below, optimize::same_value, stack::grow_stack};

/// How many nodes of a value are looked at to find out whether it reads into the value that a
/// check guards (see [`fails_on_atom`]), which bounds the work for each check.
const MAX_IMPLIED_GUARD_SIZE: usize = 256;

/// Drops each check whose failure is implied by the value after it (see [`remaining_check`]).
///
/// This runs once the rest of the optimizer is done, and nothing is rewritten after it. The other
/// rewrites rely on a value having its type, which a check like `assert value is (Any, Any)` is
/// what makes true: `(c (f x) (r x))` is `x` if `x` is a pair. Without the check in front of them,
/// `(c 4 6)` from two checked reads of a pair at path 2 would become `2`, which doesn't fail for
/// an atom.
///
/// The nodes that are rebuilt around a dropped check keep the tags of the nodes they replace.
pub(super) fn drop_implied_guards(
    arena: &mut Arena<Lir>,
    lir: LirId,
    tags: Option<&mut DebugTags>,
) -> LirId {
    let mut pass = Pass {
        rebuilt: HashMap::new(),
        tags,
    };
    pass.visit(arena, lir)
}

struct Pass<'a> {
    /// What each node that's been visited became, since a node can be shared.
    rebuilt: HashMap<LirId, LirId>,
    tags: Option<&'a mut DebugTags>,
}

impl Pass<'_> {
    fn visit(&mut self, arena: &mut Arena<Lir>, lir: LirId) -> LirId {
        if let Some(&rebuilt) = self.rebuilt.get(&lir) {
            return rebuilt;
        }

        let result = grow_stack(|| self.visit_step(arena, lir));
        self.rebuilt.insert(lir, result);
        self.inherit(lir, result);
        result
    }

    fn inherit(&mut self, from: LirId, to: LirId) {
        if from != to
            && let Some(tags) = self.tags.as_deref_mut()
        {
            tags.inherit(from, to);
        }
    }

    fn visit_step(&mut self, arena: &mut Arena<Lir>, lir: LirId) -> LirId {
        let mut node = arena[lir].clone();
        let mut changed = false;
        let mut visit = |arena: &mut Arena<Lir>, child: &mut LirId| {
            let rebuilt = self.visit(arena, *child);
            changed |= rebuilt != *child;
            *child = rebuilt;
        };

        match &mut node {
            Lir::Atom(_) | Lir::Path(_) => {}
            Lir::Quote(a)
            | Lir::First(a)
            | Lir::Rest(a)
            | Lir::Listp(a)
            | Lir::Not(a)
            | Lir::Strlen(a)
            | Lir::Lognot(a)
            | Lir::PubkeyForExp(a)
            | Lir::G1Negate(a)
            | Lir::G2Negate(a)
            | Lir::Op(_, a)
            | Lir::DebugPrint(_, a) => visit(arena, a),
            Lir::Run(a, b)
            | Lir::Cons(a, b)
            | Lir::Div(a, b)
            | Lir::Divmod(a, b)
            | Lir::Mod(a, b)
            | Lir::Eq(a, b)
            | Lir::Gt(a, b)
            | Lir::GtBytes(a, b)
            | Lir::Ash(a, b)
            | Lir::Lsh(a, b)
            | Lir::G1Multiply(a, b)
            | Lir::G2Multiply(a, b)
            | Lir::Keccak256Verify(a, b) => {
                visit(arena, a);
                visit(arena, b);
            }
            Lir::If(a, b, c, _)
            | Lir::Modpow(a, b, c)
            | Lir::CoinId(a, b, c)
            | Lir::K1Verify(a, b, c)
            | Lir::R1Verify(a, b, c) => {
                visit(arena, a);
                visit(arena, b);
                visit(arena, c);
            }
            Lir::Substr(a, b, c) => {
                visit(arena, a);
                visit(arena, b);
                if let Some(c) = c {
                    visit(arena, c);
                }
            }
            Lir::G1Map(a, b) | Lir::G2Map(a, b) => {
                visit(arena, a);
                if let Some(b) = b {
                    visit(arena, b);
                }
            }
            Lir::Closure(a, args, _) | Lir::BlsVerify(a, args) => {
                visit(arena, a);
                for arg in args {
                    visit(arena, arg);
                }
            }
            Lir::Add(args)
            | Lir::Sub(args)
            | Lir::Mul(args)
            | Lir::All(args)
            | Lir::Any(args)
            | Lir::Raise(args)
            | Lir::Concat(args)
            | Lir::Logand(args)
            | Lir::Logior(args)
            | Lir::Logxor(args)
            | Lir::G1Add(args)
            | Lir::G1Subtract(args)
            | Lir::G2Add(args)
            | Lir::G2Subtract(args)
            | Lir::BlsPairingIdentity(args)
            | Lir::Sha256(args)
            | Lir::Sha256Inline(args)
            | Lir::Keccak256(args)
            | Lir::Keccak256Inline(args) => {
                for arg in args {
                    visit(arena, arg);
                }
            }
        }

        // An inline `if` evaluates the raise either way, so its check can't be dropped
        if let Lir::If(condition, then, otherwise, false) = &mut node
            && matches!(&arena[*otherwise], Lir::Raise(args) if args.is_empty())
        {
            let Some(rest) = remaining_check(arena, *condition, *then) else {
                return *then;
            };

            if rest != *condition {
                self.inherit(*condition, rest);
                *condition = rest;
                changed = true;
            }
        }

        if changed { arena.alloc(node) } else { lir }
    }
}

/// What's left of the check of `(a (i x (q . B) (q 8)) 1)`, or `None` if `B` implies all of it.
/// `B` implies `x` if it fails whenever `x` is nil, and `(l x)` if it fails whenever `x` is an
/// atom, which both hold when every way through `B` reads into `x` (see [`fails_on_atom`]), as
/// the fields of a struct are read after `assert value is Struct`. A list that ends in an atom
/// other than nil fails the same way, since the check passes for it and reading into the atom
/// fails. A check that's `all` of several, as checks in a row are merged into (see
/// `merge_checks`), drops the ones that are implied, since `B` is still only reached if the rest
/// pass. Evaluating `x` can fail, but only where `B` would, since it evaluates `x` as well.
///
/// This changes which failure is raised, but not whether the program fails, and a release build
/// has nothing to tell one failure from another: CLVM has no side effects, and the cost of a spend
/// that fails doesn't matter. The raise of a debug build has a message, so it's kept there.
fn remaining_check(arena: &mut Arena<Lir>, condition: LirId, then: LirId) -> Option<LirId> {
    let implied = |arena: &Arena<Lir>, check: LirId| {
        let guarded = match arena[check] {
            Lir::Listp(value) => value,
            _ => check,
        };

        let mut budget = MAX_IMPLIED_GUARD_SIZE;
        fails_on_atom(arena, then, guarded, &mut budget)
    };

    let checks = match &arena[condition] {
        Lir::All(checks) => checks.clone(),
        _ => vec![condition],
    };

    let rest: Vec<LirId> = checks
        .iter()
        .copied()
        .filter(|&check| !implied(arena, check))
        .collect();

    match rest.as_slice() {
        [] => None,
        _ if rest.len() == checks.len() => Some(condition),
        // `all` of one value has its truthiness, which is all that an `if` looks at
        [only] => Some(*only),
        _ => Some(arena.alloc(Lir::All(rest))),
    }
}

/// Whether evaluating the value always fails if the guarded value is an atom: on every way
/// through it, it evaluates `(f x)` or `(r x)`, a path below `x` if `x` is a path, or a raise.
/// Every operand of an operator is evaluated, and so are the condition of an `if` and, if it's
/// inline, both of its branches. A quoted value isn't evaluated, and neither is the program that
/// a run evaluates, which reads from the environment it's run with. This is conservative, so it
/// can return `false` for a value that fails.
fn fails_on_atom(arena: &Arena<Lir>, value: LirId, guarded: LirId, budget: &mut usize) -> bool {
    let Some(rest) = budget.checked_sub(1) else {
        return false;
    };
    *budget = rest;

    let mut any = |values: &[LirId]| {
        values
            .iter()
            .any(|&value| fails_on_atom(arena, value, guarded, budget))
    };

    match &arena[value] {
        Lir::Path(path) => matches!(&arena[guarded], Lir::Path(guarded) if is_below(path, guarded)),
        Lir::Atom(_) | Lir::Quote(_) => false,
        Lir::Raise(_) => true,
        Lir::First(inner) | Lir::Rest(inner) => {
            same_value(arena, *inner, guarded) || any(&[*inner])
        }
        Lir::If(condition, then, otherwise, inline) => {
            if *inline {
                any(&[*condition, *then, *otherwise])
            } else {
                any(&[*condition]) || (any(&[*then]) && any(&[*otherwise]))
            }
        }
        Lir::Run(callee, env) => any(&[*callee, *env]),
        // The captures are evaluated to build the closure, but the function is code
        Lir::Closure(_, args, _) => any(args),
        Lir::Listp(inner)
        | Lir::Not(inner)
        | Lir::Strlen(inner)
        | Lir::Lognot(inner)
        | Lir::PubkeyForExp(inner)
        | Lir::G1Negate(inner)
        | Lir::G2Negate(inner)
        | Lir::Op(_, inner)
        | Lir::DebugPrint(_, inner) => any(&[*inner]),
        Lir::Cons(a, b)
        | Lir::Div(a, b)
        | Lir::Divmod(a, b)
        | Lir::Mod(a, b)
        | Lir::Eq(a, b)
        | Lir::Gt(a, b)
        | Lir::GtBytes(a, b)
        | Lir::Ash(a, b)
        | Lir::Lsh(a, b)
        | Lir::G1Multiply(a, b)
        | Lir::G2Multiply(a, b)
        | Lir::Keccak256Verify(a, b) => any(&[*a, *b]),
        Lir::Modpow(a, b, c)
        | Lir::CoinId(a, b, c)
        | Lir::K1Verify(a, b, c)
        | Lir::R1Verify(a, b, c) => any(&[*a, *b, *c]),
        Lir::Substr(a, b, c) => any(&[*a, *b]) || c.is_some_and(|c| any(&[c])),
        Lir::G1Map(a, b) | Lir::G2Map(a, b) => any(&[*a]) || b.is_some_and(|b| any(&[b])),
        Lir::Add(args)
        | Lir::Sub(args)
        | Lir::Mul(args)
        | Lir::All(args)
        | Lir::Any(args)
        | Lir::Concat(args)
        | Lir::Logand(args)
        | Lir::Logior(args)
        | Lir::Logxor(args)
        | Lir::G1Add(args)
        | Lir::G1Subtract(args)
        | Lir::G2Add(args)
        | Lir::G2Subtract(args)
        | Lir::BlsPairingIdentity(args)
        | Lir::Sha256(args)
        | Lir::Sha256Inline(args)
        | Lir::Keccak256(args)
        | Lir::Keccak256Inline(args) => any(args),
        Lir::BlsVerify(sig, args) => any(&[*sig]) || any(args),
    }
}
