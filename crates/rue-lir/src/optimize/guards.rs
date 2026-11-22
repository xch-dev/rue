use std::collections::HashMap;

use id_arena::Arena;

use crate::{DebugTags, Lir, LirId, is_below, optimize::same_value, stack::grow_stack};

/// How many nodes of a value are looked at to find out whether it reads into the value that a
/// check guards (see [`fails_on_atom`]), which bounds the work for each check.
const MAX_IMPLIED_GUARD_SIZE: usize = 256;

/// Drops each check whose failure is implied by the value after it (see [`implied_guard`]).
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

        if result != lir
            && let Some(tags) = self.tags.as_deref_mut()
        {
            for tags in [&mut tags.spans, &mut tags.functions, &mut tags.labels] {
                if let Some(&tag) = tags.get(&lir) {
                    tags.entry(result).or_insert(tag);
                }
            }
        }

        result
    }

    fn visit_step(&mut self, arena: &mut Arena<Lir>, lir: LirId) -> LirId {
        let mut changed = false;
        let mut visit = |arena: &mut Arena<Lir>, child: LirId| {
            let rebuilt = self.visit(arena, child);
            changed |= rebuilt != child;
            rebuilt
        };

        let node = match arena[lir].clone() {
            node @ (Lir::Atom(_) | Lir::Path(_)) => node,
            Lir::Quote(value) => Lir::Quote(visit(arena, value)),
            Lir::Run(callee, env) => Lir::Run(visit(arena, callee), visit(arena, env)),
            Lir::Closure(callee, args, has_parameters) => Lir::Closure(
                visit(arena, callee),
                args.into_iter().map(|arg| visit(arena, arg)).collect(),
                has_parameters,
            ),
            Lir::First(value) => Lir::First(visit(arena, value)),
            Lir::Rest(value) => Lir::Rest(visit(arena, value)),
            Lir::Cons(first, rest) => Lir::Cons(visit(arena, first), visit(arena, rest)),
            Lir::Listp(value) => Lir::Listp(visit(arena, value)),
            Lir::Add(args) => Lir::Add(args.into_iter().map(|a| visit(arena, a)).collect()),
            Lir::Sub(args) => Lir::Sub(args.into_iter().map(|a| visit(arena, a)).collect()),
            Lir::Mul(args) => Lir::Mul(args.into_iter().map(|a| visit(arena, a)).collect()),
            Lir::Div(left, right) => Lir::Div(visit(arena, left), visit(arena, right)),
            Lir::Divmod(left, right) => Lir::Divmod(visit(arena, left), visit(arena, right)),
            Lir::Mod(left, right) => Lir::Mod(visit(arena, left), visit(arena, right)),
            Lir::Modpow(base, exponent, modulus) => Lir::Modpow(
                visit(arena, base),
                visit(arena, exponent),
                visit(arena, modulus),
            ),
            Lir::Eq(left, right) => Lir::Eq(visit(arena, left), visit(arena, right)),
            Lir::Gt(left, right) => Lir::Gt(visit(arena, left), visit(arena, right)),
            Lir::GtBytes(left, right) => Lir::GtBytes(visit(arena, left), visit(arena, right)),
            Lir::Not(value) => Lir::Not(visit(arena, value)),
            Lir::All(args) => Lir::All(args.into_iter().map(|a| visit(arena, a)).collect()),
            Lir::Any(args) => Lir::Any(args.into_iter().map(|a| visit(arena, a)).collect()),
            Lir::If(condition, then, otherwise, inline) => {
                let condition = visit(arena, condition);
                let then = visit(arena, then);
                let otherwise = visit(arena, otherwise);

                if inline || !matches!(&arena[otherwise], Lir::Raise(args) if args.is_empty()) {
                    Lir::If(condition, then, otherwise, inline)
                } else {
                    match implied_guard(arena, condition, then) {
                        Guard::Implied => return then,
                        Guard::Rest(condition) => {
                            changed = true;
                            Lir::If(condition, then, otherwise, inline)
                        }
                        Guard::Kept => Lir::If(condition, then, otherwise, inline),
                    }
                }
            }
            Lir::Raise(args) => Lir::Raise(args.into_iter().map(|a| visit(arena, a)).collect()),
            Lir::Concat(args) => Lir::Concat(args.into_iter().map(|a| visit(arena, a)).collect()),
            Lir::Strlen(value) => Lir::Strlen(visit(arena, value)),
            Lir::Substr(string, start, end) => Lir::Substr(
                visit(arena, string),
                visit(arena, start),
                end.map(|end| visit(arena, end)),
            ),
            Lir::Logand(args) => Lir::Logand(args.into_iter().map(|a| visit(arena, a)).collect()),
            Lir::Logior(args) => Lir::Logior(args.into_iter().map(|a| visit(arena, a)).collect()),
            Lir::Logxor(args) => Lir::Logxor(args.into_iter().map(|a| visit(arena, a)).collect()),
            Lir::Lognot(value) => Lir::Lognot(visit(arena, value)),
            Lir::Ash(value, shift) => Lir::Ash(visit(arena, value), visit(arena, shift)),
            Lir::Lsh(value, shift) => Lir::Lsh(visit(arena, value), visit(arena, shift)),
            Lir::PubkeyForExp(exp) => Lir::PubkeyForExp(visit(arena, exp)),
            Lir::G1Add(args) => Lir::G1Add(args.into_iter().map(|a| visit(arena, a)).collect()),
            Lir::G1Subtract(args) => {
                Lir::G1Subtract(args.into_iter().map(|a| visit(arena, a)).collect())
            }
            Lir::G1Multiply(left, right) => {
                Lir::G1Multiply(visit(arena, left), visit(arena, right))
            }
            Lir::G1Negate(value) => Lir::G1Negate(visit(arena, value)),
            Lir::G1Map(value, map) => {
                Lir::G1Map(visit(arena, value), map.map(|map| visit(arena, map)))
            }
            Lir::G2Add(args) => Lir::G2Add(args.into_iter().map(|a| visit(arena, a)).collect()),
            Lir::G2Subtract(args) => {
                Lir::G2Subtract(args.into_iter().map(|a| visit(arena, a)).collect())
            }
            Lir::G2Multiply(left, right) => {
                Lir::G2Multiply(visit(arena, left), visit(arena, right))
            }
            Lir::G2Negate(value) => Lir::G2Negate(visit(arena, value)),
            Lir::G2Map(value, map) => {
                Lir::G2Map(visit(arena, value), map.map(|map| visit(arena, map)))
            }
            Lir::BlsPairingIdentity(args) => {
                Lir::BlsPairingIdentity(args.into_iter().map(|a| visit(arena, a)).collect())
            }
            Lir::BlsVerify(sig, args) => Lir::BlsVerify(
                visit(arena, sig),
                args.into_iter().map(|a| visit(arena, a)).collect(),
            ),
            Lir::Sha256(args) => Lir::Sha256(args.into_iter().map(|a| visit(arena, a)).collect()),
            Lir::Sha256Inline(args) => {
                Lir::Sha256Inline(args.into_iter().map(|a| visit(arena, a)).collect())
            }
            Lir::Keccak256(args) => {
                Lir::Keccak256(args.into_iter().map(|a| visit(arena, a)).collect())
            }
            Lir::Keccak256Inline(args) => {
                Lir::Keccak256Inline(args.into_iter().map(|a| visit(arena, a)).collect())
            }
            Lir::CoinId(parent, puzzle_hash, amount) => Lir::CoinId(
                visit(arena, parent),
                visit(arena, puzzle_hash),
                visit(arena, amount),
            ),
            Lir::K1Verify(public_key, message, signature) => Lir::K1Verify(
                visit(arena, public_key),
                visit(arena, message),
                visit(arena, signature),
            ),
            Lir::R1Verify(public_key, message, signature) => Lir::R1Verify(
                visit(arena, public_key),
                visit(arena, message),
                visit(arena, signature),
            ),
            Lir::Keccak256Verify(value, hash) => {
                Lir::Keccak256Verify(visit(arena, value), visit(arena, hash))
            }
            Lir::Op(op, arg) => Lir::Op(op, visit(arena, arg)),
            Lir::DebugPrint(srcloc, value) => Lir::DebugPrint(srcloc, visit(arena, value)),
        };

        if changed { arena.alloc(node) } else { lir }
    }
}

/// What's left of the check of an `if` whose other branch is the raise of a release build.
enum Guard {
    /// The check is implied by the value, so the `if` is the value.
    Implied,
    /// Some of the checks that the condition is `all` of are implied, and this is the rest.
    Rest(LirId),
    /// None of it is implied.
    Kept,
}

/// Whether `(a (i x (q . B) (q 8)) 1)` is `B`, because `B` fails whenever `x` is nil, or
/// `(a (i (l x) (q . B) (q 8)) 1)` is, because `B` fails whenever `x` is an atom. Both hold when
/// every way through `B` reads into `x` (see [`fails_on_atom`]), as the fields of a struct are read
/// after `assert value is Struct`. A list that ends in an atom other than nil fails the same way,
/// since the check passes for it and reading into the atom fails. A check that's `all` of several,
/// as checks in a row are merged into (see `merge_checks`), drops the ones that are implied, since
/// the value is still only reached if the rest pass. Evaluating `x` can fail, but only where `B`
/// would, since it evaluates `x` as well.
///
/// This changes which failure is raised, but not whether the program fails, and a release build
/// has nothing to tell one failure from another: CLVM has no side effects, and the cost of a spend
/// that fails doesn't matter. The raise of a debug build has a message, so it's kept there.
fn implied_guard(arena: &mut Arena<Lir>, condition: LirId, then: LirId) -> Guard {
    let implied = |arena: &Arena<Lir>, check: LirId| {
        let guarded = match arena[check] {
            Lir::Listp(value) => value,
            _ => check,
        };

        let mut budget = MAX_IMPLIED_GUARD_SIZE;
        fails_on_atom(arena, then, guarded, &mut budget)
    };

    if implied(arena, condition) {
        return Guard::Implied;
    }

    let Lir::All(checks) = &arena[condition] else {
        return Guard::Kept;
    };

    let rest: Vec<LirId> = checks
        .iter()
        .copied()
        .filter(|&check| !implied(arena, check))
        .collect();

    match rest.len() {
        0 => Guard::Implied,
        // `all` of one value has its truthiness, which is all that an `if` looks at
        1 => Guard::Rest(rest[0]),
        len if len < checks.len() => Guard::Rest(arena.alloc(Lir::All(rest))),
        _ => Guard::Kept,
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
