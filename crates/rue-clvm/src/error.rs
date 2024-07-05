use std::io;

use clvmr::reduction::EvalErr;
use thiserror::Error;

#[derive(Debug, Error)]
pub enum Error {
    #[error("Eval error: {0}")]
    Eval(#[from] EvalErr),

    #[error("Io error: {0}")]
    Io(#[from] io::Error),
}

pub type Result<T> = std::result::Result<T, Error>;
