use clvmr::{Allocator, NodePtr, SExp};
use num_bigint::BigInt;

use crate::operator_name;

/// The largest atom that is written as a decimal integer. Larger atoms are usually hashes or
/// keys rather than numbers, so they are written as hex instead.
const MAX_DECIMAL_LEN: usize = 8;

/// The shortest atom that is written as a string. Shorter atoms are usually small integers.
const MIN_STRING_LEN: usize = 3;

/// Converts a CLVM program to its textual representation.
///
/// The first item of every list is written as an operator keyword where possible. This includes
/// quoted values, since programs often quote code to run later.
pub fn disassemble_program(allocator: &Allocator, node: NodePtr) -> String {
    let mut output = String::new();
    write(allocator, node, true, &mut output);
    output
}

/// Converts a CLVM value to its textual representation, without any operator keywords.
pub fn disassemble_value(allocator: &Allocator, node: NodePtr) -> String {
    let mut output = String::new();
    write(allocator, node, false, &mut output);
    output
}

enum Step {
    /// Writes a node.
    Node(NodePtr),
    /// Writes the items of a list after the first, then the closing parenthesis.
    Rest(NodePtr),
}

/// Writes the node, with operator keywords if it's a program. Values can be nested arbitrarily
/// deeply, so this doesn't recurse.
fn write(allocator: &Allocator, node: NodePtr, program: bool, output: &mut String) {
    let mut steps = vec![Step::Node(node)];

    while let Some(step) = steps.pop() {
        match step {
            Step::Node(node) => {
                let SExp::Pair(first, rest) = allocator.sexp(node) else {
                    write_atom(allocator.atom(node).as_ref(), output);
                    continue;
                };

                output.push('(');
                steps.push(Step::Rest(rest));

                let name = match allocator.sexp(first) {
                    SExp::Atom if program => operator_name(allocator.atom(first).as_ref()),
                    _ => None,
                };
                match name {
                    Some(name) => output.push_str(name),
                    None => steps.push(Step::Node(first)),
                }
            }
            Step::Rest(rest) => match allocator.sexp(rest) {
                SExp::Pair(item, next) => {
                    output.push(' ');
                    steps.push(Step::Rest(next));
                    steps.push(Step::Node(item));
                }
                SExp::Atom => {
                    let atom = allocator.atom(rest);
                    if !atom.as_ref().is_empty() {
                        output.push_str(" . ");
                        write_atom(atom.as_ref(), output);
                    }
                    output.push(')');
                }
            },
        }
    }
}

fn write_atom(atom: &[u8], output: &mut String) {
    if atom.is_empty() {
        output.push_str("()");
    } else if atom.len() >= MIN_STRING_LEN && is_printable(atom) {
        output.push('"');
        for &byte in atom {
            if byte == b'"' || byte == b'\\' {
                output.push('\\');
            }
            output.push(byte as char);
        }
        output.push('"');
    } else if atom.len() <= MAX_DECIMAL_LEN && is_canonical_int(atom) {
        output.push_str(&BigInt::from_signed_bytes_be(atom).to_string());
    } else {
        output.push_str("0x");
        output.push_str(&hex::encode(atom));
    }
}

fn is_printable(atom: &[u8]) -> bool {
    atom.iter()
        .all(|&byte| byte == b' ' || byte.is_ascii_graphic())
}

/// Whether an atom is the minimal encoding of a signed integer. Zero is encoded as nil, so a
/// lone zero byte is not canonical.
fn is_canonical_int(atom: &[u8]) -> bool {
    match atom {
        [] | [0x00] => false,
        [0x00, second, ..] => second & 0x80 != 0,
        [0xff, second, ..] => second & 0x80 == 0,
        _ => true,
    }
}

#[cfg(test)]
mod tests {
    use crate::assemble;

    use super::*;

    #[test]
    fn test_atoms() {
        let mut a = Allocator::new();

        let cases: &[(&[u8], &str)] = &[
            (&[], "()"),
            (&[0x01], "1"),
            (&[0x7f], "127"),
            (&[0x80], "-128"),
            (&[0xff], "-1"),
            (&[0x00], "0x00"),
            (&[0x00, 0x80], "128"),
            (&[0x00, 0x7f], "0x007f"),
            (&[0xff, 0x7f], "-129"),
            (&[0xff, 0x80], "0xff80"),
            (&[0x0f, 0x42, 0x40], "1000000"),
            (
                &[0x7f, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff],
                "9223372036854775807",
            ),
            (
                &[0x00, 0x80, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00],
                "0x008000000000000000",
            ),
            (b"hi", "26729"),
            (b"abc", "\"abc\""),
            (b"a\"b", "\"a\\\"b\""),
            (b"a\\b", "\"a\\\\b\""),
            (b"a\nb", "6359650"),
            (&[0x00, 0x0a, 0x62], "0x000a62"),
            (&[0x13, 0xd6, 0x1f, 0x00], "332799744"),
        ];

        for (atom, expected) in cases {
            let node = a.new_atom(atom).unwrap();
            assert_eq!(disassemble_value(&a, node), *expected);
            assert_eq!(disassemble_program(&a, node), *expected);
        }
    }

    #[test]
    fn test_program() {
        let mut a = Allocator::new();

        let cases = [
            ("(2 (1 . 1) 1)", "(a (q . 1) 1)"),
            ("(2 (1 2 3) 1)", "(a (q 2 3) 1)"),
            ("(1 (2 3) . 4)", "(q (a 3) . 4)"),
            (
                "(3 (9 2 5) (1 . 100) (1 . 200))",
                "(i (= 2 5) (q . 100) (q . 200))",
            ),
            ("(64 2 3)", "(64 2 3)"),
            ("((4 2 3) 2)", "((c 2 3) 2)"),
            ("(\"debug_print\" 2)", "(\"debug_print\" 2)"),
        ];

        for (input, expected) in cases {
            let node = assemble(&mut a, input).unwrap();
            assert_eq!(disassemble_program(&a, node), expected);
        }
    }

    #[test]
    fn test_value() {
        let mut a = Allocator::new();
        let node = assemble(&mut a, "(2 (1 . 1) 3 . 4)").unwrap();
        assert_eq!(disassemble_value(&a, node), "(2 (1 . 1) 3 . 4)");
    }

    #[test]
    fn test_deeply_nested() {
        // Nested in the first of each pair, which is where an operator is
        let mut a = Allocator::new();
        let mut node = NodePtr::NIL;
        for _ in 0..1_000_000 {
            node = a.new_pair(node, NodePtr::NIL).unwrap();
        }
        let output = disassemble_program(&a, node);
        assert_eq!(
            output,
            format!("{}(){}", "(".repeat(1_000_000), ")".repeat(1_000_000))
        );
        assert_eq!(disassemble_value(&a, node), output);
    }

    #[test]
    fn test_long_list() {
        let mut a = Allocator::new();
        let mut list = NodePtr::NIL;
        for _ in 0..100_000 {
            let item = a.new_small_number(2).unwrap();
            list = a.new_pair(item, list).unwrap();
        }
        let output = disassemble_program(&a, list);
        assert!(output.starts_with("(a 2 2"));
        assert!(output.ends_with(" 2)"));
    }
}
