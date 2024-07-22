use thiserror::Error;

use crate::TypeId;

#[derive(Debug, Error, Clone, Copy, PartialEq, Eq)]
pub enum CheckError {
    #[error("recursive check")]
    Recursive(TypeId, TypeId),

    #[error("impossible check")]
    Impossible(TypeId, TypeId),
}
