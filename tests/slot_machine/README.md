# slot-machine puzzles

The Rue puzzles in [`puzzles`](puzzles) are copied verbatim from the `rue-puzzles` directory of
[slot-machine](https://github.com/Yakuhito/slot-machine) by Mihai Dancaescu (yakuhito), at commit
[`6edc948`](https://github.com/Yakuhito/slot-machine/tree/6edc94856306d0f77e065c6c99aacf1055e51e2f/rue-puzzles).
They are included here as a regression test with the author's permission. All credit for them goes to the original
author.

[`puzzles.yaml`](puzzles.yaml) records the tree hash of every puzzle that has a `main` function. When copied, these
matched the hashes committed to slot-machine, so a change to any of them means the compiler's output for real-world
puzzles has changed.

These files are excluded from the formatting check so that they stay identical to upstream. To update them, copy the
latest `rue-puzzles` directory from slot-machine (without the `.hex` and `.hash` files), update the commit above, and
check that the new hashes match the ones committed upstream.
