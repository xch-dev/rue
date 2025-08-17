use id_arena::Id;
use num_bigint::BigInt;

use crate::{BinaryOp, SymbolId, UnaryOp};

pub type HirId = Id<Hir>;

#[derive(Debug, Clone)]
pub enum Hir {
    Unresolved,
    Nil,
    String(String),
    Int(BigInt),
    Bytes(Vec<u8>),
    Bool(bool),
    Pair(HirId, HirId),
    Reference(SymbolId),
    Block(Block),
    If(HirId, HirId, HirId),
    FunctionCall(HirId, Vec<HirId>),
    Unary(UnaryOp, HirId),
    Binary(BinaryOp, HirId, HirId),
}

#[derive(Debug, Clone)]
pub struct Block {
    pub statements: Vec<Statement>,
    pub body: Option<HirId>,
}

#[derive(Debug, Clone)]
pub enum Statement {
    Expr(HirId),
    Let(SymbolId),
    If(HirId, HirId),
    Return(HirId),
    Assert(HirId),
    Raise(HirId),
}

#[cfg(test)]
pub(crate) mod tests {
    use crate::Database;

    use super::*;

    pub(crate) fn debug_hir(db: &Database, hir: HirId) -> String {
        match db.hir(hir) {
            Hir::Unresolved => "{unknown}".to_string(),
            Hir::Nil => "nil".to_string(),
            Hir::String(value) => format!("\"{value}\""),
            Hir::Int(value) => format!("{value}"),
            Hir::Bytes(value) => {
                if value.is_empty() {
                    "nil".to_string()
                } else {
                    format!("0x{}", hex::encode(value))
                }
            }
            Hir::Bool(value) => format!("{value}"),
            Hir::Pair(first, rest) => {
                format!("({}, {})", debug_hir(db, *first), debug_hir(db, *rest))
            }
            Hir::Reference(symbol) => format!("{symbol:?}"),
            Hir::Block(block) => block
                .body
                .map_or("{empty}".to_string(), |body| debug_hir(db, body)),
            Hir::If(condition, then, else_) => format!(
                "if {} {{ {} }} else {{ {} }}",
                debug_hir(db, *condition),
                debug_hir(db, *then),
                debug_hir(db, *else_)
            ),
            Hir::FunctionCall(function, args) => format!(
                "{}({})",
                debug_hir(db, *function),
                args.iter()
                    .map(|arg| debug_hir(db, *arg))
                    .collect::<Vec<_>>()
                    .join(", ")
            ),
            Hir::Unary(op, hir) => format!("({op} {})", debug_hir(db, *hir)),
            Hir::Binary(op, left, right) => {
                format!(
                    "({} {} {})",
                    debug_hir(db, *left),
                    op,
                    debug_hir(db, *right)
                )
            }
        }
    }
}
