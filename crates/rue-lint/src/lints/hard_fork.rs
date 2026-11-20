use rue_diagnostic::{Applicability, Lint, Suggestion};
use rue_hir::{Hir, HirId, Statement};

use crate::{
    HirLintPass, LintContext, LintInput,
    text::{Position, comment_count, text_at},
};

/// A `keccak256_verify` call checks a hash inside of the softfork guard before hard fork 2, where
/// `keccak256` isn't available. After it, the same check can be written as an `assert`. The lint is
/// only reported by default with `hard_fork_2` (see `Lint::default_level_for`), and the `assert` is
/// only suggested then, since `keccak256` is an error without it.
#[derive(Debug)]
pub(crate) struct NeedlessKeccak256Verify {
    hard_fork_2: bool,
}

impl NeedlessKeccak256Verify {
    pub(crate) fn new(input: &LintInput<'_>) -> Self {
        Self {
            hard_fork_2: input.hard_fork_2,
        }
    }
}

impl HirLintPass for NeedlessKeccak256Verify {
    fn lints(&self) -> &'static [Lint] {
        &[Lint::NeedlessKeccak256Verify]
    }

    fn check_hir(&mut self, cx: &mut LintContext<'_>, hir: HirId, node: &Hir) {
        let Hir::Keccak256Verify(value, hash) = *node else {
            return;
        };

        let db = cx.db();

        let Some(srcloc) = db.hir_srcloc(hir).cloned() else {
            return;
        };

        // The call can only be replaced with an `assert` statement where it's a statement. As the
        // final expression of a block, removing it would leave the block without a value, which
        // most blocks must have
        let replaced = self
            .hard_fork_2
            .then(|| cx.parent())
            .flatten()
            .and_then(|parent| match db.hir(parent) {
                Hir::Block(block) => {
                    block
                        .statements
                        .iter()
                        .find_map(|statement| match statement {
                            Statement::Expr(statement) if statement.hir == hir => {
                                Some(statement.srcloc.clone())
                            }
                            _ => None,
                        })
                }
                _ => None,
            });

        let replacement = replaced.and_then(|replaced| {
            let value = text_at(db, value, Position::Whole)?;
            let hash = text_at(db, hash, Position::Operand)?;
            let text = replaced.text();

            // A statement's location starts at its attributes, which the suggestion would have to
            // move, and comments outside of the arguments would be dropped
            if text.starts_with('#')
                || comment_count(text) != comment_count(&value) + comment_count(&hash)
            {
                return None;
            }

            Some((replaced, format!("assert keccak256({value}) == {hash};")))
        });

        let Some(diagnostic) = cx.report(
            Lint::NeedlessKeccak256Verify,
            srcloc,
            "This `keccak256_verify` can be written as an `assert` of `keccak256` after hard fork 2",
        ) else {
            return;
        };

        if let Some((replaced, replacement)) = replacement {
            diagnostic.suggestions.push(Suggestion::replace(
                "Replace it with an `assert`",
                replaced,
                replacement,
                Applicability::MachineApplicable,
            ));
        }
    }
}
