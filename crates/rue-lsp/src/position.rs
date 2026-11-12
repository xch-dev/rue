//! Conversions between byte offsets into source text and LSP positions, whose characters are
//! counted in UTF-16 code units.

use tower_lsp::lsp_types::{Position, Range};

pub fn offset_to_position(text: &str, offset: usize) -> Position {
    let offset = floor_char_boundary(text, offset);

    let mut line = 0;
    let mut line_start = 0;

    for start in line_starts(text).take_while(|&start| start <= offset) {
        line += 1;
        line_start = start;
    }

    let character = text[line_start..offset].encode_utf16().count();

    Position::new(line as u32, character as u32)
}

pub fn offsets_to_range(text: &str, start: usize, end: usize) -> Range {
    Range::new(
        offset_to_position(text, start),
        offset_to_position(text, end),
    )
}

/// Positions past the end of a line are clamped to the end of that line, and positions past the
/// end of the text are clamped to the end of the text.
pub fn position_to_offset(text: &str, position: Position) -> usize {
    let mut starts = line_starts(text);

    let line_start = match position.line.checked_sub(1) {
        None => 0,
        Some(line) => match starts.nth(line as usize) {
            Some(start) => start,
            None => return text.len(),
        },
    };

    // Every line break ends with a one byte character, `\n` or `\r`. The `\r` of a `\r\n` is
    // treated as part of the line, like the editor does when it counts the characters of it.
    let line_end = starts.next().map_or(text.len(), |start| start - 1);

    let mut remaining = position.character as usize;

    for (i, c) in text[line_start..line_end].char_indices() {
        if remaining == 0 {
            return line_start + i;
        }

        remaining = remaining.saturating_sub(c.len_utf16());
    }

    line_end
}

/// The offset at which each line after the first starts. The protocol counts `\n`, `\r\n` and a
/// `\r` on its own as line breaks.
fn line_starts(text: &str) -> impl Iterator<Item = usize> + '_ {
    let bytes = text.as_bytes();

    bytes
        .iter()
        .enumerate()
        .filter_map(move |(i, &byte)| match byte {
            b'\n' => Some(i + 1),
            b'\r' if bytes.get(i + 1) != Some(&b'\n') => Some(i + 1),
            _ => None,
        })
}

fn floor_char_boundary(text: &str, offset: usize) -> usize {
    let mut offset = offset.min(text.len());

    while !text.is_char_boundary(offset) {
        offset -= 1;
    }

    offset
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_offset_to_position() {
        let text = "ab\nc\u{e9}d\n\u{1F600}x";

        assert_eq!(offset_to_position(text, 0), Position::new(0, 0));
        assert_eq!(offset_to_position(text, 2), Position::new(0, 2));
        assert_eq!(offset_to_position(text, 3), Position::new(1, 0));
        // `é` is two bytes, but one UTF-16 code unit.
        assert_eq!(offset_to_position(text, 6), Position::new(1, 2));
        // The emoji is four bytes, and two UTF-16 code units.
        assert_eq!(offset_to_position(text, 12), Position::new(2, 2));
        assert_eq!(offset_to_position(text, 13), Position::new(2, 3));
        assert_eq!(offset_to_position(text, 100), Position::new(2, 3));
    }

    #[test]
    fn test_position_to_offset() {
        let text = "ab\nc\u{e9}d\n\u{1F600}x";

        assert_eq!(position_to_offset(text, Position::new(0, 0)), 0);
        assert_eq!(position_to_offset(text, Position::new(0, 2)), 2);
        assert_eq!(position_to_offset(text, Position::new(0, 10)), 2);
        assert_eq!(position_to_offset(text, Position::new(1, 2)), 6);
        assert_eq!(position_to_offset(text, Position::new(2, 2)), 12);
        assert_eq!(position_to_offset(text, Position::new(2, 3)), 13);
        assert_eq!(position_to_offset(text, Position::new(5, 0)), text.len());
    }

    #[test]
    fn test_line_breaks() {
        let text = "a\r\nb\rc\nd\r";

        assert_eq!(offset_to_position(text, 1), Position::new(0, 1));
        // The `\r` of a `\r\n` is part of the line it ends.
        assert_eq!(offset_to_position(text, 2), Position::new(0, 2));
        assert_eq!(offset_to_position(text, 3), Position::new(1, 0));
        // A `\r` on its own ends a line.
        assert_eq!(offset_to_position(text, 4), Position::new(1, 1));
        assert_eq!(offset_to_position(text, 5), Position::new(2, 0));
        assert_eq!(offset_to_position(text, 7), Position::new(3, 0));
        assert_eq!(offset_to_position(text, 9), Position::new(4, 0));

        assert_eq!(position_to_offset(text, Position::new(0, 10)), 2);
        assert_eq!(position_to_offset(text, Position::new(1, 0)), 3);
        assert_eq!(position_to_offset(text, Position::new(1, 10)), 4);
        assert_eq!(position_to_offset(text, Position::new(2, 0)), 5);
        assert_eq!(position_to_offset(text, Position::new(3, 1)), 8);
        assert_eq!(position_to_offset(text, Position::new(4, 0)), 9);
        assert_eq!(position_to_offset(text, Position::new(5, 0)), 9);
    }

    #[test]
    fn test_round_trip() {
        let text = "fn main() {\r\n    \"h\u{e9}llo \u{1F600}\"\n}\r\r";

        for (offset, _) in text.char_indices() {
            assert_eq!(
                position_to_offset(text, offset_to_position(text, offset)),
                offset
            );
        }
    }
}
