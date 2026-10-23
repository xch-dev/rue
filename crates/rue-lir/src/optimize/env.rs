use id_arena::Arena;

use crate::{Lir, LirId};

/// Returns `true` if evaluating the generated code for this LIR node may read
/// the environment it's run in. This is conservative, so it can return `true`
/// even if the environment isn't actually observable.
///
/// The cases follow what codegen emits for each node:
/// - Quoted values are not evaluated, so they never read the environment.
/// - `Run` evaluates the callee and environment in the current environment,
///   but the program itself is run in the new environment, so only the callee
///   and environment expressions matter.
/// - `Closure` evaluates the function and its captures in the current environment,
///   and builds a new program which isn't run here.
/// - Non-inline `If` is emitted as `(a (i c (q . T) (q . E)) 1)`, which passes the
///   current environment through to the chosen branch. Reading path 1 never fails,
///   so it only reads the environment if the condition or a branch does.
/// - `Op` builds a program and runs it in a nil environment, so only the arguments
///   matter.
pub fn reads_env(arena: &Arena<Lir>, lir: LirId) -> bool {
    match &arena[lir] {
        Lir::Atom(_) | Lir::Quote(_) => false,
        Lir::Path(_) => true,
        Lir::Run(callee, env) => reads_env(arena, *callee) || reads_env(arena, *env),
        Lir::Closure(callee, args, _) => reads_env(arena, *callee) || any_reads_env(arena, args),
        Lir::First(value)
        | Lir::Rest(value)
        | Lir::Listp(value, _)
        | Lir::Not(value)
        | Lir::Strlen(value)
        | Lir::Lognot(value)
        | Lir::PubkeyForExp(value)
        | Lir::G1Negate(value)
        | Lir::G2Negate(value)
        | Lir::Op(_, value)
        | Lir::DebugPrint(_, value) => reads_env(arena, *value),
        Lir::Cons(left, right)
        | Lir::Div(left, right)
        | Lir::Divmod(left, right)
        | Lir::Mod(left, right)
        | Lir::Eq(left, right)
        | Lir::Gt(left, right)
        | Lir::GtBytes(left, right)
        | Lir::Ash(left, right)
        | Lir::Lsh(left, right)
        | Lir::G1Multiply(left, right)
        | Lir::G2Multiply(left, right) => reads_env(arena, *left) || reads_env(arena, *right),
        Lir::G1Map(value, map) | Lir::G2Map(value, map) => {
            reads_env(arena, *value) || map.is_some_and(|map| reads_env(arena, map))
        }
        Lir::Substr(string, start, end) => {
            reads_env(arena, *string)
                || reads_env(arena, *start)
                || end.is_some_and(|end| reads_env(arena, end))
        }
        Lir::Modpow(a, b, c)
        | Lir::If(a, b, c, _)
        | Lir::CoinId(a, b, c)
        | Lir::K1Verify(a, b, c)
        | Lir::R1Verify(a, b, c) => {
            reads_env(arena, *a) || reads_env(arena, *b) || reads_env(arena, *c)
        }
        Lir::BlsVerify(sig, args) => reads_env(arena, *sig) || any_reads_env(arena, args),
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
        | Lir::Keccak256Inline(args) => any_reads_env(arena, args),
    }
}

fn any_reads_env(arena: &Arena<Lir>, args: &[LirId]) -> bool {
    args.iter().any(|arg| reads_env(arena, *arg))
}
