mod block;
mod document;
mod expr;
mod generics;
mod item;
mod stmt;
mod ty;

pub use block::*;
pub use document::*;
pub use expr::*;
pub use generics::*;
pub use item::*;
pub use stmt::*;
pub use ty::*;

#[cfg(test)]
mod tests {
    use expect_test::Expect;
    use rue_ast::{AstDocument, AstNode};
    use rue_hir::{Scope, ScopeId};
    use rue_lexer::Lexer;
    use rue_parser::Parser;

    use crate::{Compiler, compile_document, declare_document};

    pub fn check(source: &str, errors: Expect) -> (Compiler, ScopeId) {
        let tokens = Lexer::new(source).collect::<Vec<_>>();

        let parser = Parser::new(source, tokens);

        let result = parser.parse();

        let ast = AstDocument::cast(result.node).unwrap();

        let mut ctx = Compiler::new();

        let scope = ctx.alloc_scope(Scope::new());
        let declarations = declare_document(&mut ctx, scope, &ast);
        compile_document(&mut ctx, scope, &ast, declarations);

        let mut error_output = String::new();
        let mut first = true;

        for error in result.errors {
            if !first {
                error_output.push('\n');
            }
            error_output.push_str(&error.message(source));
            first = false;
        }

        for error in ctx.errors() {
            if !first {
                error_output.push('\n');
            }
            error_output.push_str(&error.message(source));
            first = false;
        }

        errors.assert_eq(&error_output);

        (ctx, scope)
    }
}
