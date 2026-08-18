use anyhow::{Context, Result};
use clap::Parser;
use rumoca_release::ReleaseArgs;
use std::path::PathBuf;

fn main() -> Result<()> {
    let root = workspace_root()?;
    rumoca_release::run(ReleaseArgs::parse(), &root)
}

fn workspace_root() -> Result<PathBuf> {
    let cwd = std::env::current_dir().context("failed to read current directory")?;
    cwd.ancestors()
        .find(|dir| {
            dir.join("Cargo.toml").is_file()
                && dir
                    .join("crates/rumoca-bind-python/pyproject.toml")
                    .is_file()
        })
        .map(PathBuf::from)
        .context("run this command from within the Rumoca workspace")
}
