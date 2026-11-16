use std::fmt;

use crate::LineIndex;

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct LineCol {
    pub line: usize,
    pub col: usize,
}

impl LineCol {
    /// Returns the line and column of the given byte index in the source.
    /// Line and column numbers are from 0, and columns are counted in characters.
    ///
    /// This builds a [`LineIndex`] for the source, so to look up several positions in the same
    /// source, build one with [`LineIndex::new`] and use [`LineIndex::line_col`], or use
    /// [`Source::line_col`](crate::Source::line_col), which keeps it.
    pub fn new(source: &str, index: usize) -> Self {
        LineIndex::new(source).line_col(source, index)
    }

    /// Returns the byte index of this line and column in the source, or the length of the source
    /// if it's out of bounds.
    ///
    /// This builds a [`LineIndex`] for the source, so to look up several positions in the same
    /// source, build one with [`LineIndex::new`] and use [`LineIndex::offset`].
    pub fn index(&self, source: &str) -> usize {
        LineIndex::new(source).offset(source, *self)
    }
}

impl fmt::Display for LineCol {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}:{}", self.line + 1, self.col + 1)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    /// The implementation of [`LineCol::new`] before the line index, which scans the source.
    fn scan_line_col(source: &str, index: usize) -> LineCol {
        let mut line = 0;
        let mut col = 0;

        for (i, character) in source.char_indices() {
            if i >= index {
                break;
            }

            if character == '\n' {
                line += 1;
                col = 0;
            } else {
                col += 1;
            }
        }

        LineCol { line, col }
    }

    /// The implementation of [`LineCol::index`] before the line index, which scans the source.
    fn scan_index(position: LineCol, source: &str) -> usize {
        let mut current_line = 0;
        let mut current_col = 0;

        for (i, c) in source.char_indices() {
            if current_line == position.line && current_col == position.col {
                return i;
            }

            if c == '\n' {
                current_line += 1;
                current_col = 0;
            } else {
                current_col += 1;
            }
        }

        // Handle position at end of file
        if current_line == position.line && current_col == position.col {
            return source.len();
        }

        // Return source length if position is out of bounds
        source.len()
    }

    const SOURCES: &[&str] = &[
        "",
        "\n",
        "\n\n",
        "a",
        "a\n",
        "\na",
        "a\u{e9}b\nc",
        "ab\r\ncd\r\n",
        "a\rb\rc",
        "\r",
        "\r\n",
        "\r\r\n\n\r",
        "\u{e9}\n\u{e9}\u{e9}\n",
        "\u{1F600}x\n\u{4E2D}\u{6587}\r\n\u{1F600}",
        "fn main() {\r\n    \"h\u{e9}llo \u{1F600}\"\n}\r\r",
        "\u{0}\t\n\u{FEFF}a\u{301}\n",
    ];

    #[test]
    fn test_line_col() {
        let source = "a\u{e9}b\nc";

        assert_eq!(LineCol::new(source, 0), LineCol { line: 0, col: 0 });
        assert_eq!(LineCol::new(source, 3), LineCol { line: 0, col: 2 });
        assert_eq!(LineCol::new(source, 5), LineCol { line: 1, col: 0 });
        assert_eq!(LineCol { line: 0, col: 2 }.index(source), 3);
        assert_eq!(LineCol { line: 1, col: 1 }.index(source), 6);
    }

    #[test]
    fn test_line_col_matches_scan() {
        for source in SOURCES {
            let index = LineIndex::new(source);

            // Every offset, including ones inside a character and past the end
            for offset in 0..=source.len() + 2 {
                assert_eq!(
                    index.line_col(source, offset),
                    scan_line_col(source, offset),
                    "offset {offset} in {source:?}"
                );
                assert_eq!(
                    LineCol::new(source, offset),
                    scan_line_col(source, offset),
                    "offset {offset} in {source:?}"
                );
            }
        }
    }

    #[test]
    fn test_index_matches_scan() {
        for source in SOURCES {
            let index = LineIndex::new(source);
            let lines = source.split('\n').count();
            let longest = source
                .split('\n')
                .map(|line| line.chars().count())
                .max()
                .unwrap_or(0);

            // Every position, including ones past the end of each line and past the last line
            for line in 0..=lines + 1 {
                for col in 0..=longest + 2 {
                    let position = LineCol { line, col };

                    assert_eq!(
                        index.offset(source, position),
                        scan_index(position, source),
                        "{position:?} in {source:?}"
                    );
                    assert_eq!(
                        position.index(source),
                        scan_index(position, source),
                        "{position:?} in {source:?}"
                    );
                }
            }
        }
    }
}
