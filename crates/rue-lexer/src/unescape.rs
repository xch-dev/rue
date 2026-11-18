use std::{ops::Range, str::Chars};

/// An escape sequence in a string literal that can't be decoded.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum EscapeError {
    /// A backslash followed by a character that doesn't start an escape, such as `\q`.
    Unknown(char),
    /// A backslash at the end of an unterminated string.
    LoneBackslash,
    /// A `\x` escape that isn't followed by two hex digits.
    InvalidHex,
    /// A `\x` escape above `\x7f`, which isn't a character on its own in UTF-8.
    HexOutOfRange,
    /// A `\u` escape that isn't written as `\u{...}` with 1 to 6 hex digits.
    InvalidUnicode,
    /// A `\u` escape whose value is a surrogate or is above `\u{10ffff}`.
    InvalidCodePoint,
}

/// Decodes the escape sequences in the text between a string literal's quotes, the same way
/// Rust decodes a string literal:
///
/// - `\n`, `\r`, `\t`, `\\`, `\0`, `\'` and `\"`
/// - `\x41`, an ASCII character from `\x00` to `\x7f`
/// - `\u{1F600}`, a Unicode code point with 1 to 6 hex digits (underscores are allowed after the
///   first digit)
/// - A backslash at the end of a line, which skips the line break and the whitespace after it
///
/// Each escape that can't be decoded is reported to `on_error` with its byte range in `text`,
/// and is left out of the result.
pub fn unescape_string(text: &str, mut on_error: impl FnMut(Range<usize>, EscapeError)) -> String {
    let mut output = String::with_capacity(text.len());
    let mut chars = text.chars();

    while let Some(c) = chars.next() {
        if c != '\\' {
            output.push(c);
            continue;
        }

        let start = text.len() - chars.as_str().len() - 1;

        match unescape_char(&mut chars) {
            Ok(Some(c)) => output.push(c),
            Ok(None) => {}
            Err(error) => {
                let end = text.len() - chars.as_str().len();
                on_error(start..end, error);
            }
        }
    }

    output
}

/// Splits a string literal token into its quote and the text between its quotes. The closing
/// quote is missing from an unterminated string, so it's only stripped if it isn't escaped.
fn split_string(token: &str) -> Option<(char, &str, bool)> {
    let quote = token.chars().next().filter(|&c| c == '"' || c == '\'')?;
    let text = &token[1..];

    match text.strip_suffix(quote) {
        Some(stripped) if !ends_with_escape(stripped) => Some((quote, stripped, true)),
        _ => Some((quote, text, false)),
    }
}

/// Whether the text ends with an odd number of backslashes, which escape what comes after it.
fn ends_with_escape(text: &str) -> bool {
    text.bytes().rev().take_while(|&byte| byte == b'\\').count() % 2 == 1
}

/// Decodes the value of a string literal token, quoted with either `"` or `'`. Invalid escapes
/// are left out, since the parser reports them.
pub fn string_value(token: &str) -> String {
    let text = split_string(token).map_or(token, |(_, text, _)| text);
    unescape_string(text, |_, _| {})
}

/// Rewrites a string literal token to use the preferred quote (`"` or `'`), the way Prettier does:
/// the other quote is used instead if the string contains more of the preferred one, so that
/// fewer quotes need escaping. Quotes that no longer need escaping are unescaped, and every other
/// escape is kept as written, so the string's value doesn't change. An unterminated string is
/// returned as is.
pub fn requote_string(token: &str, preferred: char) -> String {
    let Some((_, text, true)) = split_string(token) else {
        return token.to_string();
    };

    let alternate = if preferred == '"' { '\'' } else { '"' };
    let count = |quote| text.chars().filter(|&c| c == quote).count();
    let quote = if count(preferred) > count(alternate) {
        alternate
    } else {
        preferred
    };

    let mut output = String::with_capacity(token.len() + 2);
    output.push(quote);

    let mut chars = text.chars();

    while let Some(c) = chars.next() {
        match c {
            '\\' => match chars.next() {
                // An escaped quote only needs its backslash if it's the enclosing quote
                Some(escaped @ ('"' | '\'')) => {
                    if escaped == quote {
                        output.push('\\');
                    }
                    output.push(escaped);
                }
                Some(escaped) => {
                    output.push('\\');
                    output.push(escaped);
                }
                None => output.push('\\'),
            },
            c if c == quote => {
                output.push('\\');
                output.push(c);
            }
            c => output.push(c),
        }
    }

    output.push(quote);
    output
}

/// Decodes one escape sequence after its backslash, or returns `None` for a line continuation.
fn unescape_char(chars: &mut Chars<'_>) -> Result<Option<char>, EscapeError> {
    let Some(c) = chars.next() else {
        return Err(EscapeError::LoneBackslash);
    };

    Ok(Some(match c {
        'n' => '\n',
        'r' => '\r',
        't' => '\t',
        '\\' => '\\',
        '0' => '\0',
        '\'' => '\'',
        '"' => '"',
        'x' => return unescape_hex(chars).map(Some),
        'u' => return unescape_unicode(chars).map(Some),
        '\n' => {
            skip_continuation_whitespace(chars);
            return Ok(None);
        }
        '\r' if chars.as_str().starts_with('\n') => {
            chars.next();
            skip_continuation_whitespace(chars);
            return Ok(None);
        }
        c => return Err(EscapeError::Unknown(c)),
    }))
}

fn unescape_hex(chars: &mut Chars<'_>) -> Result<char, EscapeError> {
    let mut value = 0;

    for _ in 0..2 {
        let digit = chars
            .clone()
            .next()
            .and_then(|c| c.to_digit(16))
            .ok_or(EscapeError::InvalidHex)?;
        chars.next();
        value = value * 16 + digit;
    }

    if value > 0x7f {
        return Err(EscapeError::HexOutOfRange);
    }

    Ok(char::from_u32(value).expect("ASCII is always a valid character"))
}

fn unescape_unicode(chars: &mut Chars<'_>) -> Result<char, EscapeError> {
    if !chars.as_str().starts_with('{') {
        return Err(EscapeError::InvalidUnicode);
    }

    chars.next();

    let mut value: u32 = 0;
    let mut digits = 0;

    loop {
        // Only consume what belongs to the escape, so that the error doesn't swallow the rest of
        // the string (or its closing quote, which the lexer has already matched)
        match chars.clone().next() {
            Some('}') => {
                chars.next();
                break;
            }
            Some('_') if digits > 0 => {
                chars.next();
            }
            Some(c) if c.is_ascii_hexdigit() => {
                chars.next();
                digits += 1;

                if digits > 6 {
                    return Err(EscapeError::InvalidUnicode);
                }

                value = value * 16 + c.to_digit(16).expect("checked to be a hex digit");
            }
            _ => return Err(EscapeError::InvalidUnicode),
        }
    }

    if digits == 0 {
        return Err(EscapeError::InvalidUnicode);
    }

    char::from_u32(value).ok_or(EscapeError::InvalidCodePoint)
}

/// Skips the whitespace at the start of the line after a line continuation, as Rust does.
fn skip_continuation_whitespace(chars: &mut Chars<'_>) {
    while chars
        .clone()
        .next()
        .is_some_and(|c| matches!(c, ' ' | '\t' | '\n' | '\r'))
    {
        chars.next();
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn unescape(text: &str) -> (String, Vec<(Range<usize>, EscapeError)>) {
        let mut errors = Vec::new();
        let output = unescape_string(text, |range, error| errors.push((range, error)));
        (output, errors)
    }

    fn ok(text: &str) -> String {
        let (output, errors) = unescape(text);
        assert_eq!(errors, vec![], "unexpected errors for {text:?}");
        output
    }

    #[test]
    fn test_plain() {
        assert_eq!(ok("hello world"), "hello world");
        assert_eq!(ok(""), "");
        assert_eq!(ok("héllo 😀"), "héllo 😀");
    }

    #[test]
    fn test_simple_escapes() {
        assert_eq!(ok(r#"\n\r\t\\\0\'\""#), "\n\r\t\\\0'\"");
        assert_eq!(ok(r"a\nb"), "a\nb");
    }

    #[test]
    fn test_hex_escapes() {
        assert_eq!(ok(r"\x41\x7f\x00\x7F"), "A\x7f\0\x7f");
        assert_eq!(
            unescape(r"\x80"),
            (String::new(), vec![(0..4, EscapeError::HexOutOfRange)])
        );
        assert_eq!(
            unescape(r"\xff"),
            (String::new(), vec![(0..4, EscapeError::HexOutOfRange)])
        );
        assert_eq!(
            unescape(r"\x4"),
            (String::new(), vec![(0..3, EscapeError::InvalidHex)])
        );
        assert_eq!(
            unescape(r"\x4g"),
            ("g".to_string(), vec![(0..3, EscapeError::InvalidHex)])
        );
        assert_eq!(
            unescape(r"\x"),
            (String::new(), vec![(0..2, EscapeError::InvalidHex)])
        );
    }

    #[test]
    fn test_unicode_escapes() {
        assert_eq!(ok(r"\u{41}\u{1F600}\u{10ffff}\u{0}"), "A😀\u{10ffff}\0");
        assert_eq!(ok(r"\u{1_F6_00}"), "😀");
        assert_eq!(ok(r"\u{000041}"), "A");

        for text in [
            r"\u41",
            r"\u{}",
            r"\u{_1}",
            r"\u{1234567}",
            r"\u{12",
            r"\u{g}",
        ] {
            assert_eq!(unescape(text).1.len(), 1, "{text:?}");
            assert_eq!(
                unescape(text).1[0].1,
                EscapeError::InvalidUnicode,
                "{text:?}"
            );
        }

        assert_eq!(
            unescape(r"\u{d800}"),
            (String::new(), vec![(0..8, EscapeError::InvalidCodePoint)])
        );
        assert_eq!(
            unescape(r"\u{110000}"),
            (String::new(), vec![(0..10, EscapeError::InvalidCodePoint)])
        );

        // The error stops at the first character that can't be part of the escape
        assert_eq!(
            unescape(r"\u{12 rest"),
            (
                " rest".to_string(),
                vec![(0..5, EscapeError::InvalidUnicode)]
            )
        );
    }

    #[test]
    fn test_unknown_escapes() {
        assert_eq!(
            unescape(r"a\qb"),
            ("ab".to_string(), vec![(1..3, EscapeError::Unknown('q'))])
        );
        assert_eq!(
            unescape(r"\😀"),
            (String::new(), vec![(0..5, EscapeError::Unknown('😀'))])
        );
        assert_eq!(
            unescape("a\\"),
            ("a".to_string(), vec![(1..2, EscapeError::LoneBackslash)])
        );
    }

    #[test]
    fn test_string_value() {
        assert_eq!(string_value(r#""a\"b""#), "a\"b");
        assert_eq!(string_value(r#"'a"b\'c'"#), "a\"b'c");
        assert_eq!(string_value(r#""\\""#), "\\");
        // Unterminated strings, where the last quote is escaped or of the other kind
        assert_eq!(string_value(r#""a\""#), "a\"");
        assert_eq!(string_value(r#"'a""#), "a\"");
    }

    #[test]
    fn test_requote_string() {
        let requote = |token: &str, preferred| {
            let output = requote_string(token, preferred);
            assert_eq!(
                string_value(&output),
                string_value(token),
                "{token:?} -> {output:?}"
            );
            output
        };

        assert_eq!(requote("'hello'", '"'), r#""hello""#);
        assert_eq!(requote(r#""hello""#, '\''), "'hello'");
        assert_eq!(requote(r#""hello""#, '"'), r#""hello""#);

        // The other quote is used when it needs fewer escapes, and ties go to the preferred one
        assert_eq!(requote(r#"'say "hi"'"#, '"'), r#"'say "hi"'"#);
        assert_eq!(requote(r#""it's""#, '\''), r#""it's""#);
        assert_eq!(requote(r#"'a"b\'c'"#, '"'), r#""a\"b'c""#);
        assert_eq!(requote(r#"'a"b\'c'"#, '\''), r#"'a"b\'c'"#);

        // Quotes are escaped or unescaped as needed
        assert_eq!(requote(r#"'it\'s'"#, '"'), r#""it's""#);
        assert_eq!(requote(r#""\'""#, '"'), r#""'""#);
        assert_eq!(requote(r#""a\"b""#, '"'), r#"'a"b'"#);

        // Other escapes are kept as written, including escaped backslashes before a quote
        assert_eq!(
            requote(r#"'\n\x41\u{1F600}\\'"#, '"'),
            r#""\n\x41\u{1F600}\\""#
        );
        assert_eq!(requote(r#"'a\\'"#, '"'), r#""a\\""#);
        assert_eq!(requote("'a\\\n   b'", '"'), "\"a\\\n   b\"");

        // Unterminated strings are left alone
        assert_eq!(requote("'abc", '"'), "'abc");
        assert_eq!(requote(r#""a\""#, '\''), r#""a\""#);
    }

    #[test]
    fn test_line_continuation() {
        assert_eq!(ok("hello \\\n    world"), "hello world");
        assert_eq!(ok("hello \\\r\n\t  \n  world"), "hello world");
        assert_eq!(
            unescape("a\\\rb"),
            ("ab".to_string(), vec![(1..3, EscapeError::Unknown('\r'))])
        );
    }
}
