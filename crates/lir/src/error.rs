use clvm_traits::ToClvmError;
use clvmr::error::EvalErr;
use thiserror::Error;

#[derive(Debug, Error)]
pub enum Error {
    #[error("CLVM error: {0}")]
    Clvm(#[from] EvalErr),

    #[error("Conversion error: {0}")]
    Conversion(#[from] ToClvmError),
}

pub type Result<T> = std::result::Result<T, Error>;
