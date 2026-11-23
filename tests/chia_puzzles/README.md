# chia_puzzles ports

> [!WARNING]
> These ports are compiler regression tests, not examples. They haven't been audited and they're
> only checked against the solutions in their tests, so don't use them to secure real coins: use the
> original Chialisp puzzles from chia_puzzles. This is about these test files, not about Rue, which
> is used for puzzles on mainnet (see [Used in Production](../../README.md#used-in-production)).

Each `.rue` file here is a hand-written Rue port of the Chialisp puzzle with the same path in the
`puzzles` directory of [chia_puzzles](https://github.com/Chia-Network/chia_puzzles), at commit
[`c8a5d63`](https://github.com/Chia-Network/chia_puzzles/tree/c8a5d6329526fbe4186884cd5f79258b574297de/puzzles).
The `.clsp.hex` file next to it is the compiled reference puzzle, copied unchanged from that commit.
chia_puzzles is licensed under Apache-2.0, like Rue.

The ports follow the original puzzles' logic and parameter layout, so that both can be run with the
same solution, but they're written in idiomatic Rue and don't try to match the reference bytecode.
Their tree hashes are expected to differ from the reference, and to change as the compiler does.

## Known differences from the originals

Neither a port nor its original validates the solution. A Chialisp puzzle has no types to check
it against, and a port's parameter types are assumptions about the solution, which the compiler
doesn't check at runtime and relies on when it optimizes. On a solution that doesn't match them,
such as an atom where a pair is expected, the original sometimes fails, but only because some
operation it runs happens to fail on that value, and the port may fail in a different place or not
at all. So on such a solution a port can accept a spend its original rejects, or the other way
around. The tests only use solutions that match.

A puzzle that has to reject malformed values, because an attacker chooses them, should take them as
`Any` and check them with an `is` guard, which checks the whole shape of the value. That holds in
Chialisp as well, where the check has to be written by hand.

On solutions that do match, the one known difference is **coin ids**. Ports use the `coinid`
operator where the original calls `calculate_coin_id`, which only checks lengths and that the amount
isn't negative. `coinid` also rejects amounts with leading zero bytes or above the largest coin
amount, which no real coin can have.

A list that ends in an atom other than nil, such as `(a b . 5)`, isn't a difference. `is nil` on a
`List` checks whether the value is nil, the same way Chialisp's `(if list ...)` does, so a port
fails on `(f 5)` like the original, in debug and release builds alike.

## How the tests work

Every port's tests run the compiled `main` (the real puzzle, not a copy of it inlined into the
test) and the reference on the same solution, through the `(a 2 5)` program in
[`run.hex`](run.hex), which the compiler can't see into. A passing test asserts that both produce
the same output and returns it, so the snapshot shows the conditions. Spends that should fail are
tested in pairs, one test running the port and one running the reference, and both snapshots
should record an error.

The snapshot's top-level `program` is the compiled puzzle, so any change to the code the compiler
generates for it shows up there. The top-level `output` or `clvm_error` comes from running it with
an empty solution, and isn't meaningful.

## Adding or updating puzzles

1. Copy the `.clsp.hex` file from chia_puzzles to the same path here, and write the port next to it.
2. Start the port with the regression-test header the other ports use, naming the original puzzle.
3. Cover each branch of the original puzzle with a test, including the spends it rejects.
4. Run `cargo run -p rue-tests --release -- --bless`, and check that every test passed and that every
   pair of rejection tests failed on both sides.

If chia_puzzles changes a puzzle, update its hex file and port together, and update the commit
above.
