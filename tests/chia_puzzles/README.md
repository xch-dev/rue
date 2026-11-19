# chia_puzzles ports

> [!WARNING]
> These are compiler regression tests, not examples. They haven't been audited, they're only
> checked against the solutions in their tests, and they must not be used to secure real coins.
> Use the original Chialisp puzzles from chia_puzzles instead.

Each `.rue` file here is a hand-written Rue port of the Chialisp puzzle with the same path in the
`puzzles` directory of [chia_puzzles](https://github.com/Chia-Network/chia_puzzles), at commit
[`c8a5d63`](https://github.com/Chia-Network/chia_puzzles/tree/c8a5d6329526fbe4186884cd5f79258b574297de/puzzles).
The `.clsp.hex` file next to it is the compiled reference puzzle, copied unchanged from that commit.
chia_puzzles is licensed under Apache-2.0, like Rue.

The ports follow the original puzzles' logic and parameter layout, so that both can be run with the
same solution, but they're written in idiomatic Rue and don't try to match the reference bytecode.
Their tree hashes are expected to differ from the reference, and to change as the compiler does.

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
