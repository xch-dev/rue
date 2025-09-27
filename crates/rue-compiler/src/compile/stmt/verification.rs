use log::debug;
use rue_ast::{AstNode, AstVerificationStmt};
use rue_diagnostic::DiagnosticKind;
use rue_hir::{Statement, Verification};
use rue_parser::T;

use crate::{Compiler, compile_expr};

pub fn compile_verification_stmt(ctx: &mut Compiler, stmt: &AstVerificationStmt) -> Statement {
    let Some(builtin) = stmt.builtin() else {
        debug!("Unresolved verification stmt builtin");
        return Statement::Expr(ctx.builtins().unresolved.hir);
    };

    let mut args = Vec::new();

    for expr in stmt.args() {
        let value = compile_expr(ctx, &expr, Some(ctx.builtins().types.atom));
        ctx.assign_type(expr.syntax(), value.ty, ctx.builtins().types.atom);
        args.push(value.hir);
    }

    Statement::Verification(match builtin.kind() {
        T![bls_pairing_identity] => Verification::BlsPairingIdentity(args),
        T![bls_verify] => {
            if args.is_empty() {
                ctx.diagnostic(
                    stmt.syntax(),
                    DiagnosticKind::ExpectedArgumentsMinimum(1, 0),
                );
                return Statement::Expr(ctx.builtins().unresolved.hir);
            }
            Verification::BlsVerify(args[0], args[1..].to_vec())
        }
        T![secp256k1_verify] => {
            if args.len() != 3 {
                ctx.diagnostic(
                    stmt.syntax(),
                    DiagnosticKind::ExpectedArgumentsExact(3, args.len()),
                );
                return Statement::Expr(ctx.builtins().unresolved.hir);
            }
            Verification::Secp256K1Verify(args[0], args[1], args[2])
        }
        T![secp256r1_verify] => {
            if args.len() != 3 {
                ctx.diagnostic(
                    stmt.syntax(),
                    DiagnosticKind::ExpectedArgumentsExact(3, args.len()),
                );
                return Statement::Expr(ctx.builtins().unresolved.hir);
            }
            Verification::Secp256R1Verify(args[0], args[1], args[2])
        }
        _ => unreachable!(),
    })
}
