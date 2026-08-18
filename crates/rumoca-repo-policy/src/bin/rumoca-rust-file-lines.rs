use anyhow::{Context, Result, bail, ensure};
use clap::Parser;
use std::fs;
use std::io::{BufRead, BufReader};
use std::path::{Path, PathBuf};
use std::process::Command;

#[derive(Debug, Parser)]
#[command(about = "Enforce Rumoca's Rust source file-size policy")]
struct Args {
    /// Maximum allowed lines in a Rust source file.
    #[arg(long, default_value_t = 2000)]
    max_lines: usize,
    /// Check every tracked Rust file instead of only staged files.
    #[arg(long)]
    all_files: bool,
}

fn main() -> Result<()> {
    let args = Args::parse();
    let root = workspace_root()?;
    let mut command = Command::new("git");
    if args.all_files {
        command.args(["ls-files", "-z", "--", "*.rs"]);
    } else {
        command.args([
            "diff",
            "--cached",
            "--name-only",
            "--diff-filter=ACMR",
            "-z",
            "--",
            "*.rs",
        ]);
    }
    let output = command
        .current_dir(&root)
        .output()
        .context("failed to query Rust files")?;
    ensure!(
        output.status.success(),
        "git query failed: {}",
        String::from_utf8_lossy(&output.stderr)
    );

    let mut rust_files = output
        .stdout
        .split(|byte| *byte == 0)
        .filter(|chunk| !chunk.is_empty())
        .filter_map(|chunk| std::str::from_utf8(chunk).ok())
        .collect::<Vec<_>>();
    rust_files.sort_unstable();

    let mut violations = Vec::new();
    for relative in rust_files {
        if relative.replace('\\', "/").contains("/generated/") {
            continue;
        }
        let path = root.join(relative);
        if !path.is_file() {
            continue;
        }
        let file =
            fs::File::open(&path).with_context(|| format!("failed to open {}", path.display()))?;
        let lines = BufReader::new(file).lines().count();
        if lines > args.max_lines {
            violations.push((relative.to_owned(), lines));
        }
    }

    if violations.is_empty() {
        println!(
            "Rust file line-count check passed (max {} lines).",
            args.max_lines
        );
        return Ok(());
    }
    for (file, lines) in &violations {
        eprintln!(
            "ERROR: {file} has {lines} lines (max allowed: {}).",
            args.max_lines
        );
    }
    bail!("Rust file line-count check failed. Split oversized Rust files before committing.")
}

fn workspace_root() -> Result<PathBuf> {
    let cwd = std::env::current_dir().context("failed to read current directory")?;
    cwd.ancestors()
        .find(|dir| is_workspace_root(dir))
        .map(PathBuf::from)
        .context("run this command from within the Rumoca workspace")
}

fn is_workspace_root(path: &Path) -> bool {
    path.join("Cargo.toml").is_file() && path.join("spec/SPEC_0021_CODE_COMPLEXITY.md").is_file()
}
