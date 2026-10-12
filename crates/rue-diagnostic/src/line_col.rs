use std::fmt;

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct LineCol {
    pub line: usize,
    pub col: usize,
}

impl LineCol {
    /// Returns the line and column of the given byte index in the source.
    /// Line and column numbers are from 0, and columns are counted in characters.
    pub fn new(source: &str, index: usize) -> Self {
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

        Self { line, col }
    }

    /// Returns the byte index of this line and column in the source.
    pub fn index(&self, source: &str) -> usize {
        let mut current_line = 0;
        let mut current_col = 0;

        for (i, c) in source.char_indices() {
            if current_line == self.line && current_col == self.col {
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
        if current_line == self.line && current_col == self.col {
            return source.len();
        }

        // Return source length if position is out of bounds
        source.len()
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

    #[test]
    fn test_line_col() {
        let source = "a\u{e9}b\nc";

        assert_eq!(LineCol::new(source, 0), LineCol { line: 0, col: 0 });
        assert_eq!(LineCol::new(source, 3), LineCol { line: 0, col: 2 });
        assert_eq!(LineCol::new(source, 5), LineCol { line: 1, col: 0 });
        assert_eq!(LineCol { line: 0, col: 2 }.index(source), 3);
        assert_eq!(LineCol { line: 1, col: 1 }.index(source), 6);
    }
}
