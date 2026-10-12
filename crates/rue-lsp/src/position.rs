//! Conversions between byte offsets into source text and LSP positions, whose characters are
//! counted in UTF-16 code units.

use tower_lsp::lsp_types::{Position, Range};

pub fn offset_to_position(text: &str, offset: usize) -> Position {
    let offset = floor_char_boundary(text, offset);
    let before = &text[..offset];
    let line = before.matches('\n').count();
    let line_start = before.rfind('\n').map_or(0, |i| i + 1);
    let character = before[line_start..].encode_utf16().count();

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
    let mut line_start = 0;

    for _ in 0..position.line {
        match text[line_start..].find('\n') {
            Some(i) => line_start += i + 1,
            None => return text.len(),
        }
    }

    let line_end = text[line_start..]
        .find('\n')
        .map_or(text.len(), |i| line_start + i);

    let mut remaining = position.character as usize;

    for (i, c) in text[line_start..line_end].char_indices() {
        if remaining == 0 {
            return line_start + i;
        }

        remaining = remaining.saturating_sub(c.len_utf16());
    }

    line_end
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
    fn test_round_trip() {
        let text = "fn main() {\r\n    \"h\u{e9}llo \u{1F600}\"\n}";

        for (offset, _) in text.char_indices() {
            assert_eq!(
                position_to_offset(text, offset_to_position(text, offset)),
                offset
            );
        }
    }
}
