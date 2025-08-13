use id_arena::Id;
use num_bigint::BigInt;
use rue_mir::{BinaryOp, UnaryOp};

use crate::SymbolId;

pub type HirId = Id<Hir>;

#[derive(Debug, Clone)]
pub enum Hir {
    Unresolved,
    Nil,
    String(String),
    Int(BigInt),
    Bytes(Vec<u8>),
    Bool(bool),
    Reference(SymbolId),
    Block(Block),
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
            Hir::Reference(symbol) => format!("{symbol:?}"),
            Hir::Block(block) => block
                .body
                .map_or("{empty}".to_string(), |body| debug_hir(db, body)),
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
