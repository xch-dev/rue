mod block;
mod document;
mod expr;
mod generics;
mod item;
mod stmt;
mod ty;

pub(crate) use document::document;

#[cfg(test)]
mod tests {
    use expect_test::Expect;
    use rue_lexer::Lexer;

    use crate::Parser;

    pub fn check(f: impl FnOnce(&mut Parser<'_>), source: &str, expect: Expect, errors: Expect) {
        let tokens = Lexer::new(source).collect::<Vec<_>>();

        let mut parser = Parser::new(source, tokens);
        f(&mut parser);

        let result = parser.parse_raw();

        let output = format!("{:#?}", result.node);

        let mut error_output = String::new();

        for (i, error) in result.diagnostics.into_iter().enumerate() {
            if i != 0 {
                error_output.push('\n');
            }
            error_output.push_str(&error.message(source));
        }

        expect.assert_eq(&output);
        errors.assert_eq(&error_output);
    }
}
