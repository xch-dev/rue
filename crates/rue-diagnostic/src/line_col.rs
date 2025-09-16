#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct LineCol {
    pub line: usize,
    pub col: usize,
}

impl LineCol {
    /// Returns the line and column of the given index in the source.
    /// Line and column numbers are from 0.
    pub fn new(source: &str, index: usize) -> Self {
        let mut line = 0;
        let mut col = 0;

        for (i, character) in source.chars().enumerate() {
            if i == index {
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
}
