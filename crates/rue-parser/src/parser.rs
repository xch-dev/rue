use rue_lexer::Token;

pub struct Parser<'a> {
    tokens: &'a [Token<'a>],
}

impl<'a> Parser<'a> {
    pub fn new(tokens: &'a [Token<'a>]) -> Self {
        Self { tokens }
    }
}
