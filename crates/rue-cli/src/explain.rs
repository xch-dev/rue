//! `rue explain`, which shows a compiled program as source-annotated pseudocode (see
//! `rue_explain`).

use std::{
    fs,
    path::{Path, PathBuf},
};

use anyhow::{Context, Result};
use clap::Parser;
use clvmr::{Allocator, serde::node_to_bytes};
use rue_clvm::assemble;
use rue_explain::Explanation;

use crate::debug_metadata_path;

#[derive(Debug, Parser)]
pub struct ExplainArgs {
    /// The program, as CLVM or hex (with `--hex`), or a file that contains it (such as one from
    /// `rue build --all`, which is read as hex if its name ends in `.hex`)
    program: String,
    /// Read the program as hex, even if it isn't a `.hex` file
    #[clap(short = 'x', long)]
    hex: bool,
    /// The debug metadata sidecar of the program. For a program file, it defaults to the sidecar
    /// next to it (`foo.rue.hex` has `foo.rue.debug.clvm.bin`), if there is one. Without one,
    /// nothing is named
    #[clap(long, value_name = "PATH")]
    debug_metadata: Option<PathBuf>,
}

pub fn explain(args: &ExplainArgs) -> Result<String> {
    let (text, hex, sidecar_path) = if Path::new(&args.program).is_file() {
        let text = fs::read_to_string(&args.program)
            .with_context(|| format!("Couldn't read {}", args.program))?;
        let default_sidecar = debug_metadata_path(&args.program);
        let sidecar = args.debug_metadata.clone().or_else(|| {
            let default_sidecar = PathBuf::from(default_sidecar);
            default_sidecar.is_file().then_some(default_sidecar)
        });
        (
            text.trim().to_string(),
            args.hex || args.program.ends_with(".hex"),
            sidecar,
        )
    } else {
        (args.program.clone(), args.hex, args.debug_metadata.clone())
    };

    let program = if hex {
        hex::decode(&text).context("The program isn't valid hex")?
    } else {
        let mut allocator = Allocator::new();
        let ptr = assemble(&mut allocator, &text)?;
        node_to_bytes(&allocator, ptr)?
    };

    let sidecar = sidecar_path
        .map(|path| fs::read(&path).with_context(|| format!("Couldn't read {}", path.display())))
        .transpose()?;

    let explanation = Explanation::new(&program, sidecar.as_deref())?;
    Ok(explanation.render().trim_end().to_string())
}
