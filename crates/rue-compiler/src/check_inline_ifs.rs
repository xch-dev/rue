use rue_diagnostic::DiagnosticKind;
use rue_hir::{Hir, Statement, may_raise, statements_may_raise};

use crate::{Compiler, InlineIf};

/// Both branches of an `inline if` are evaluated eagerly, so a branch which can raise will do so
/// regardless of the condition. This runs after every function has been compiled, so that calls
/// to functions which may raise can be detected.
pub fn check_inline_ifs(ctx: &mut Compiler) {
    for (srcloc, inline_if) in ctx.take_inline_ifs() {
        let raises = match inline_if {
            InlineIf::Expr(hir) => match ctx.hir(hir) {
                Hir::If(_, then, else_, _) => may_raise(ctx, *then) || may_raise(ctx, *else_),
                _ => false,
            },
            InlineIf::Statement(hir, index) => match ctx.hir(hir) {
                Hir::Block(block) => match &block.statements[index] {
                    Statement::If(stmt) => {
                        may_raise(ctx, stmt.then)
                            || statements_may_raise(ctx, &block.statements[index + 1..], block.body)
                    }
                    _ => false,
                },
                _ => false,
            },
        };

        if raises {
            ctx.diagnostic_at(srcloc, DiagnosticKind::InlineIfMayRaise);
        }
    }
}
