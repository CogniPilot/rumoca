use anyhow::{Context, Result};
use clap::{Parser, Subcommand};
use rumoca_modelica_cache::{CmmArgs, ModelicaDepsArgs};
use std::path::PathBuf;

#[derive(Debug, Parser)]
#[command(about = "Manage Rumoca's verified Modelica library caches")]
struct Cli {
    #[command(subcommand)]
    command: Command,
}

#[derive(Debug, Subcommand)]
enum Command {
    /// Manage the CogniPilot Modelica Models cache.
    Cmm(CmmArgs),
    /// Manage all Modelica dependencies used by examples.
    ModelicaDeps(ModelicaDepsArgs),
}

fn main() -> Result<()> {
    let root = workspace_root()?;
    match Cli::parse().command {
        Command::Cmm(args) => rumoca_modelica_cache::run_cmm_command(args, &root),
        Command::ModelicaDeps(args) => {
            rumoca_modelica_cache::run_modelica_deps_command(args, &root)
        }
    }
}

fn workspace_root() -> Result<PathBuf> {
    let cwd = std::env::current_dir().context("failed to read current directory")?;
    cwd.ancestors()
        .find(|dir| {
            dir.join("Cargo.toml").is_file()
                && dir.join("examples/modelica_dependencies.toml").is_file()
        })
        .map(PathBuf::from)
        .context("run this command from within the Rumoca workspace")
}
