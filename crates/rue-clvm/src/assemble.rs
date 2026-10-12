use clvmr::{Allocator, NodePtr, error::EvalErr};
use num_bigint::BigInt;
use thiserror::Error;

use crate::operator_atom;

#[derive(Debug, Error)]
pub enum AssembleError {
    #[error("unterminated string starting at {0}")]
    UnterminatedString(usize),

    #[error("missing `)` for list starting at {0}")]
    UnclosedList(usize),

    #[error("unexpected `{1}` at {0}")]
    Unexpected(usize, char),

    #[error("expected a value after `.` at {0}")]
    MissingTail(usize),

    #[error("expected `)` after the tail of the list at {0}")]
    ExpectedListEnd(usize),

    #[error("invalid number `{1}` at {0}")]
    InvalidNumber(usize, String),

    #[error("invalid hex `{1}` at {0}")]
    InvalidHex(usize, String),

    #[error(transparent)]
    Eval(#[from] EvalErr),
}

/// Parses the textual representation of a CLVM program or value.
///
/// The text must contain at most one value, optionally surrounded by whitespace and `;`
/// comments. Empty text is parsed as nil.
pub fn assemble(allocator: &mut Allocator, text: &str) -> Result<NodePtr, AssembleError> {
    let mut reader = Reader {
        allocator,
        text: text.as_bytes(),
        pos: 0,
    };

    reader.skip_whitespace();

    if reader.peek().is_none() {
        return Ok(NodePtr::NIL);
    }

    let node = reader.read_value()?;

    reader.skip_whitespace();

    if let Some(byte) = reader.peek() {
        return Err(AssembleError::Unexpected(reader.pos, byte as char));
    }

    Ok(node)
}

struct Reader<'a> {
    allocator: &'a mut Allocator,
    text: &'a [u8],
    pos: usize,
}

impl Reader<'_> {
    fn peek(&self) -> Option<u8> {
        self.text.get(self.pos).copied()
    }

    fn skip_whitespace(&mut self) {
        while let Some(byte) = self.peek() {
            if byte == b';' {
                while self.peek().is_some_and(|byte| !is_eol(byte)) {
                    self.pos += 1;
                }
            } else if is_space(byte) {
                self.pos += 1;
            } else {
                break;
            }
        }
    }

    /// Reads a value, which must start at the current position.
    fn read_value(&mut self) -> Result<NodePtr, AssembleError> {
        match self.peek() {
            Some(b'(') => self.read_list(),
            Some(b'"' | b'\'') => self.read_string(),
            Some(byte @ b')') => Err(AssembleError::Unexpected(self.pos, byte as char)),
            _ => self.read_atom(),
        }
    }

    fn read_list(&mut self) -> Result<NodePtr, AssembleError> {
        let start = self.pos;
        self.pos += 1;

        let mut items = Vec::new();

        let tail = loop {
            self.skip_whitespace();

            match self.peek() {
                None => return Err(AssembleError::UnclosedList(start)),
                Some(b')') => {
                    self.pos += 1;
                    break NodePtr::NIL;
                }
                Some(b'.') if self.is_delimiter(self.pos + 1) => {
                    let dot = self.pos;

                    if items.is_empty() {
                        return Err(AssembleError::Unexpected(dot, '.'));
                    }

                    self.pos += 1;
                    self.skip_whitespace();

                    if matches!(self.peek(), None | Some(b')')) {
                        return Err(AssembleError::MissingTail(dot));
                    }

                    let tail = self.read_value()?;
                    self.skip_whitespace();

                    match self.peek() {
                        Some(b')') => self.pos += 1,
                        None => return Err(AssembleError::UnclosedList(start)),
                        Some(_) => return Err(AssembleError::ExpectedListEnd(self.pos)),
                    }

                    break tail;
                }
                Some(_) => items.push(self.read_value()?),
            }
        };

        let mut list = tail;
        for item in items.into_iter().rev() {
            list = self.allocator.new_pair(item, list)?;
        }
        Ok(list)
    }

    /// Reads a quoted string. A backslash includes the next character literally.
    fn read_string(&mut self) -> Result<NodePtr, AssembleError> {
        let start = self.pos;
        let quote = self.text[start];
        self.pos += 1;

        let mut bytes = Vec::new();

        loop {
            let Some(byte) = self.peek() else {
                return Err(AssembleError::UnterminatedString(start));
            };
            self.pos += 1;

            if byte == quote {
                break;
            }

            if byte == b'\\' {
                let Some(escaped) = self.peek() else {
                    return Err(AssembleError::UnterminatedString(start));
                };
                self.pos += 1;
                bytes.push(escaped);
            } else {
                bytes.push(byte);
            }
        }

        Ok(self.allocator.new_atom(&bytes)?)
    }

    fn read_atom(&mut self) -> Result<NodePtr, AssembleError> {
        let start = self.pos;

        while !self.is_delimiter(self.pos) {
            self.pos += 1;
        }

        // Tokens only end at ASCII bytes, so they are always valid UTF-8.
        let token = std::str::from_utf8(&self.text[start..self.pos]).expect("valid UTF-8");
        let atom = atom_from_token(start, token)?;
        Ok(self.allocator.new_atom(&atom)?)
    }

    fn is_delimiter(&self, pos: usize) -> bool {
        self.text
            .get(pos)
            .is_none_or(|&byte| matches!(byte, b'(' | b')' | b';') || is_space(byte))
    }
}

fn atom_from_token(pos: usize, token: &str) -> Result<Vec<u8>, AssembleError> {
    if let Some(digits) = token
        .strip_prefix("0x")
        .or_else(|| token.strip_prefix("0X"))
    {
        let padded = if digits.len() % 2 == 1 {
            format!("0{digits}")
        } else {
            digits.to_string()
        };
        return hex::decode(padded).map_err(|_| AssembleError::InvalidHex(pos, token.to_string()));
    }

    let unsigned = token.strip_prefix(['-', '+']).unwrap_or(token);

    if unsigned.starts_with(|char: char| char.is_ascii_digit()) {
        if !unsigned.bytes().all(|byte| byte.is_ascii_digit()) {
            return Err(AssembleError::InvalidNumber(pos, token.to_string()));
        }
        let number: BigInt = token.parse().expect("valid integer");
        return Ok(int_atom(&number));
    }

    Ok(match operator_atom(token) {
        Some(atom) => atom.to_vec(),
        None => token.as_bytes().to_vec(),
    })
}

/// Encodes an integer as its minimal signed representation, where zero is nil.
fn int_atom(number: &BigInt) -> Vec<u8> {
    if number.sign() == num_bigint::Sign::NoSign {
        Vec::new()
    } else {
        number.to_signed_bytes_be()
    }
}

fn is_eol(byte: u8) -> bool {
    byte == b'\r' || byte == b'\n'
}

fn is_space(byte: u8) -> bool {
    byte == b' ' || byte == b'\t' || is_eol(byte)
}

#[cfg(test)]
mod tests {
    use clvmr::SExp;

    use crate::disassemble_value;

    use super::*;

    fn atom(text: &str) -> Vec<u8> {
        let mut a = Allocator::new();
        let node = assemble(&mut a, text).unwrap();
        assert!(matches!(a.sexp(node), SExp::Atom), "{text} is not an atom");
        a.atom(node).as_ref().to_vec()
    }

    fn value(text: &str) -> String {
        let mut a = Allocator::new();
        let node = assemble(&mut a, text).unwrap();
        disassemble_value(&a, node)
    }

    #[test]
    fn test_atoms() {
        assert_eq!(atom(""), b"");
        assert_eq!(atom("  ; comment"), b"");
        assert_eq!(atom("()"), b"");
        assert_eq!(atom("0"), b"");
        assert_eq!(atom("-0"), b"");
        assert_eq!(atom("+5"), [0x05]);
        assert_eq!(atom("1"), [0x01]);
        assert_eq!(atom("128"), [0x00, 0x80]);
        assert_eq!(atom("-1"), [0xff]);
        assert_eq!(atom("-129"), [0xff, 0x7f]);
        assert_eq!(atom("0x"), b"");
        assert_eq!(atom("0x00"), [0x00]);
        assert_eq!(atom("0xabc"), [0x0a, 0xbc]);
        assert_eq!(atom("0XAB"), [0xab]);
        assert_eq!(atom("hello"), b"hello");
        assert_eq!(atom("-"), [0x11]);
        assert_eq!(atom("q"), [0x01]);
        assert_eq!(atom("#a"), b"#a");
        assert_eq!(atom("secp256k1_verify"), [0x13, 0xd6, 0x1f, 0x00]);
        assert_eq!(atom("\"a b\""), b"a b");
        assert_eq!(atom("'say \"hi\"'"), b"say \"hi\"");
        assert_eq!(atom(r#""a\"b\\c\n""#), b"a\"b\\cn");
        assert_eq!(atom("  ; comment\n  5 ; another\n"), [0x05]);
    }

    #[test]
    fn test_lists() {
        assert_eq!(value("(a (q . 1) ; comment\n 1)"), "(2 (1 . 1) 1)");
        assert_eq!(value("(1 2 . 3)"), "(1 2 . 3)");
        assert_eq!(value("(1 . (2 . ()))"), "(1 2)");
        assert_eq!(value("(\"abc\"\"def\")"), "(\"abc\" \"def\")");
        assert_eq!(value("(a;comment\nb)"), "(2 98)");
        assert_eq!(value("(.50 . .xy)"), "(\".50\" . \".xy\")");
    }

    #[test]
    fn test_errors() {
        let mut a = Allocator::new();

        for text in [
            "(",
            "(1 2",
            "(1 . 2 3)",
            "(1 .",
            "(1 . )",
            "(1 . 2",
            "(. 1)",
            ")",
            "1 2",
            "(1) )",
            "\"abc",
            "(\"abc",
            "'a\\",
            "1abc",
            "-1x",
            "1_000",
            "0xzz",
        ] {
            assert!(assemble(&mut a, text).is_err(), "{text} should fail");
        }
    }
}
