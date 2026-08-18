use anyhow::{Context, Result};
use clap::{Parser, Subcommand};
use rumoca_review::{packet::ReviewPacketArgs, scan::ReviewScanArgs};
use std::path::PathBuf;

#[derive(Debug, Parser)]
#[command(about = "Generate deterministic Rumoca review evidence")]
struct Cli {
    #[command(subcommand)]
    command: Command,
}

#[derive(Debug, Subcommand)]
enum Command {
    /// Scan a diff for review-policy findings.
    Scan(ReviewScanArgs),
    /// Generate a Markdown review packet for a diff.
    Packet(ReviewPacketArgs),
}

fn main() -> Result<()> {
    let root = workspace_root()?;
    match Cli::parse().command {
        Command::Scan(args) => rumoca_review::scan::run(args, &root),
        Command::Packet(args) => rumoca_review::packet::run(args, &root),
    }
}

fn workspace_root() -> Result<PathBuf> {
    let cwd = std::env::current_dir().context("failed to read current directory")?;
    cwd.ancestors()
        .find(|dir| dir.join("Cargo.toml").is_file() && dir.join("spec").is_dir())
        .map(PathBuf::from)
        .context("run this command from within the Rumoca workspace")
}
