use std::{iter, ops::Range};

use crate::LineCol;

/// The byte offset at which each line of a text starts, so that an offset can be converted to a
/// line with a binary search, rather than by scanning the text from the start.
///
/// The index doesn't keep the text, so the methods that need it take it as an argument, which
/// must be the text the index was built from.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct LineIndex {
    /// The start of each line, in increasing order. The first line starts at 0.
    starts: Vec<usize>,
    /// The length of the text, which is where the last line ends.
    len: usize,
}

impl LineIndex {
    /// Lines end at each `\n`, which is how [`LineCol`] counts them. A `\r` is part of the line.
    pub fn new(text: &str) -> Self {
        let starts = text
            .bytes()
            .enumerate()
            .filter(|&(_, byte)| byte == b'\n')
            .map(|(i, _)| i + 1);

        Self::from_starts(text, starts)
    }

    /// Lines end at each `\n`, `\r\n` or `\r` on its own, which is how the language server
    /// protocol counts them.
    pub fn with_carriage_returns(text: &str) -> Self {
        let bytes = text.as_bytes();

        let starts = bytes
            .iter()
            .enumerate()
            .filter_map(|(i, &byte)| match byte {
                b'\n' => Some(i + 1),
                b'\r' if bytes.get(i + 1) != Some(&b'\n') => Some(i + 1),
                _ => None,
            });

        Self::from_starts(text, starts)
    }

    fn from_starts(text: &str, starts: impl Iterator<Item = usize>) -> Self {
        Self {
            starts: iter::once(0).chain(starts).collect(),
            len: text.len(),
        }
    }

    /// The line that contains the byte offset, counting from 0. A line break is part of the line
    /// it ends, and offsets past the end of the text are on the last line.
    pub fn line(&self, offset: usize) -> usize {
        // The first line starts at 0, so at least one start is at or before the offset
        self.starts.partition_point(|&start| start <= offset) - 1
    }

    /// The byte range of the line, without the last byte of the line break that ends it (`\n`,
    /// or `\r` on its own). The `\r` of a `\r\n` is part of the range. Returns `None` if there's
    /// no such line.
    pub fn line_range(&self, line: usize) -> Option<Range<usize>> {
        let start = *self.starts.get(line)?;

        // Every line break ends with a one byte character, `\n` or `\r`
        let end = self.starts.get(line + 1).map_or(self.len, |&next| next - 1);

        Some(start..end)
    }

    /// The line and column of the byte offset in the text, as [`LineCol::new`] counts them. The
    /// index must have been built with [`LineIndex::new`].
    pub fn line_col(&self, text: &str, offset: usize) -> LineCol {
        debug_assert_eq!(text.len(), self.len, "the index is for a different text");

        let offset = offset.min(text.len());
        let line = self.line(offset);
        let start = self.starts[line];

        // An offset inside a character is after it, since it's counted once it has started
        let col = text[start..]
            .char_indices()
            .take_while(|&(i, _)| start + i < offset)
            .count();

        LineCol { line, col }
    }

    /// The byte offset of the line and column in the text, as [`LineCol::index`] finds it. The
    /// index must have been built with [`LineIndex::new`].
    pub fn offset(&self, text: &str, position: LineCol) -> usize {
        debug_assert_eq!(text.len(), self.len, "the index is for a different text");

        let Some(range) = self.line_range(position.line) else {
            return text.len();
        };

        // The column after the last character is the line break, and anything past it is out of
        // bounds
        text[range.clone()]
            .char_indices()
            .map(|(i, _)| range.start + i)
            .chain(iter::once(range.end))
            .nth(position.col)
            .unwrap_or(text.len())
    }
}
