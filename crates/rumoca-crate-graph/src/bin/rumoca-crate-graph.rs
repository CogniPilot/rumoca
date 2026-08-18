use anyhow::{Context, Result};
use clap::Parser;
use rumoca_crate_graph::CrateDagArgs;
use std::path::PathBuf;

#[derive(Debug, Parser)]
#[command(about = "Render Rumoca's workspace crate dependency graph")]
struct Cli {
    #[command(flatten)]
    graph: CrateDagArgs,
}

fn main() -> Result<()> {
    let root = workspace_root()?;
    rumoca_crate_graph::run(&root, Cli::parse().graph)
}

fn workspace_root() -> Result<PathBuf> {
    let cwd = std::env::current_dir().context("failed to read current directory")?;
    cwd.ancestors()
        .find(|dir| dir.join("Cargo.toml").is_file() && dir.join("crates").is_dir())
        .map(PathBuf::from)
        .context("run this command from within the Rumoca workspace")
}
