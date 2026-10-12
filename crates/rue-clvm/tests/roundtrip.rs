//! Checks that every value can be written as text and parsed back to the same value.

use clvmr::{Allocator, NodePtr, serde::node_to_bytes};
use rue_clvm::{OPERATORS, assemble, disassemble_program, disassemble_value};

/// A small deterministic PRNG, so that failures are reproducible.
struct Rng(u64);

impl Rng {
    fn next(&mut self) -> u64 {
        self.0 ^= self.0 << 13;
        self.0 ^= self.0 >> 7;
        self.0 ^= self.0 << 17;
        self.0
    }

    fn below(&mut self, n: usize) -> usize {
        (self.next() % n as u64) as usize
    }

    fn pick<'a, T>(&mut self, items: &'a [T]) -> &'a T {
        &items[self.below(items.len())]
    }
}

const INTERESTING_ATOMS: &[&[u8]] = &[
    &[],
    &[0x00],
    &[0x7f],
    &[0x80],
    &[0xff],
    &[0x00, 0x00],
    &[0x00, 0x7f],
    &[0x00, 0x80],
    &[0xff, 0x7f],
    &[0xff, 0x80],
    &[0x0f, 0x42, 0x40],
    &[0x00, 0x80, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00],
    &[0x80, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00],
    &[0x01, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00],
    b"0x12",
    b"123",
    b"-12",
    b"abc",
    b"a b",
    b"a\"b",
    b"a'b",
    b"a\\b",
    b"a;b",
    b"(a)",
    b"a\nb",
    b"\xe2\x9c\x93ok",
];

fn random_atom(rng: &mut Rng) -> Vec<u8> {
    match rng.below(4) {
        0 => rng.pick(INTERESTING_ATOMS).to_vec(),
        1 => rng.pick(OPERATORS).0.to_vec(),
        2 => {
            let len = rng.below(12);
            (0..len).map(|_| rng.next() as u8).collect()
        }
        _ => {
            let len = rng.below(8);
            (0..len)
                .map(|_| *rng.pick(b"ab0x9 \"'\\();.#-\t~"))
                .collect()
        }
    }
}

fn random_node(rng: &mut Rng, allocator: &mut Allocator, depth: usize) -> NodePtr {
    if depth == 0 || rng.below(3) == 0 {
        let atom = random_atom(rng);
        return allocator.new_atom(&atom).unwrap();
    }

    let first = random_node(rng, allocator, depth - 1);
    let rest = random_node(rng, allocator, depth - 1);
    allocator.new_pair(first, rest).unwrap()
}

#[test]
fn test_roundtrip() {
    let mut rng = Rng(0x2545_f491_4f6c_dd1d);

    for _ in 0..100_000 {
        let mut allocator = Allocator::new();
        let node = random_node(&mut rng, &mut allocator, 6);
        let expected = node_to_bytes(&allocator, node).unwrap();

        for text in [
            disassemble_program(&allocator, node),
            disassemble_value(&allocator, node),
        ] {
            let mut allocator = Allocator::new();
            let parsed = assemble(&mut allocator, &text).unwrap();
            let actual = node_to_bytes(&allocator, parsed).unwrap();
            assert_eq!(actual, expected, "round trip of {text}");
        }
    }
}
