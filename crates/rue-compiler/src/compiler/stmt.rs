use crate::{HirId, ScopeId, Value};

mod if_stmt;
mod let_stmt;

pub enum Statement {
    Let(ScopeId),
    If(HirId, HirId),
    Return(Value),
    Assume,
}
