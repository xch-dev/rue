mod block;
mod document;
mod expr;
mod generics;
mod item;
mod path;
mod stmt;
mod ty;

pub use block::*;
pub use document::*;
pub use expr::*;
pub use generics::*;
pub use item::*;
pub use path::*;
pub use stmt::*;
pub use ty::*;

#[cfg(test)]
mod tests {
    use std::sync::Arc;

    use expect_test::Expect;
    use rue_ast::{AstDocument, AstNode};
    use rue_diagnostic::{Source, SourceKind};
    use rue_hir::{Scope, ScopeId};
    use rue_lexer::Lexer;
    use rue_options::CompilerOptions;
    use rue_parser::Parser;

    use crate::{Compiler, compile_document, declare_document};

    #[allow(clippy::needless_pass_by_value)]
    pub fn check(source: &str, errors: Expect) -> (Compiler, ScopeId) {
        let tokens = Lexer::new(source).collect::<Vec<_>>();

        let parser = Parser::new(
            Source::new(Arc::from(source), SourceKind::File("main.rue".to_string())),
            tokens,
        );

        let result = parser.parse();

        let ast = AstDocument::cast(result.node).unwrap();

        let mut ctx = Compiler::new(CompilerOptions::default());

        let scope = ctx.alloc_scope(Scope::new());
        let declarations = declare_document(&mut ctx, scope, &ast);
        compile_document(&mut ctx, scope, &ast, declarations);

        let mut error_output = String::new();
        let mut first = true;

        for error in result.diagnostics {
            if !first {
                error_output.push('\n');
            }
            error_output.push_str(&error.message());
            first = false;
        }

        for error in ctx.take_diagnostics() {
            if !first {
                error_output.push('\n');
            }
            error_output.push_str(&error.message());
            first = false;
        }

        errors.assert_eq(&error_output);

        (ctx, scope)
    }
}
