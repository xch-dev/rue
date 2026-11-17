# slot-machine puzzles

The Rue puzzles in [`puzzles`](puzzles) are copied from the `rue-puzzles` directory of
[slot-machine](https://github.com/Yakuhito/slot-machine) by Mihai Dancaescu (yakuhito), at commit
[`6edc948`](https://github.com/Yakuhito/slot-machine/tree/6edc94856306d0f77e065c6c99aacf1055e51e2f/rue-puzzles).
They have been reformatted with `rue fmt` and are included here as a regression test with the author's permission. All
credit for them goes to the original author.

[`puzzles.yaml`](puzzles.yaml) records the tree hash of every puzzle that has a `main` function, so a change to any of
them shows that the compiler's output for real-world puzzles has changed. These puzzles are here to catch bugs and
performance regressions, so the hashes are expected to change as the compiler does, and don't need to match the ones
committed to slot-machine. A changed hash is a prompt to check that the new output is correct and no more expensive.

To update the puzzles, copy the latest `rue-puzzles` directory from slot-machine (without the `.hex` and `.hash` files),
format them with `rue fmt`, and update the commit above.
