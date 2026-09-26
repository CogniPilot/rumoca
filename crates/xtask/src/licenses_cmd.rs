//! `cargo xtask licenses`: regenerate or check `THIRD_PARTY_LICENSES.md`, the
//! attribution notice that ships with the published binaries.
//!
//! The binaries are statically linked, so they carry the code of every crate
//! the file lists, and every one of those licenses conditions redistribution
//! on its notice being reproduced. The file is that reproduction, rendered by
//! `cargo-about` from `infra/licenses/about.toml` and `about.hbs`.

use anyhow::{Context, Result, bail};
use clap::Args;
use std::fs;
use std::path::Path;
use std::process::Command;

const CONFIG: &str = "infra/licenses/about.toml";
const TEMPLATE: &str = "infra/licenses/about.hbs";
const OUTPUT: &str = "THIRD_PARTY_LICENSES.md";

#[derive(Debug, Args, PartialEq, Eq)]
pub(crate) struct LicensesArgs {
    /// Fail when the committed file differs from a fresh rendering instead of
    /// writing it
    #[arg(long)]
    pub(crate) check: bool,
}

/// How `cargo-about` is reached: installed as a Cargo subcommand, or through
/// `nix run` when it is not.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum AboutLauncher {
    CargoSubcommand,
    NixRun,
}

impl AboutLauncher {
    pub(crate) fn command(self, root: &Path) -> Command {
        let arguments = ["generate", "--config", CONFIG, TEMPLATE];
        let mut command = match self {
            Self::CargoSubcommand => {
                let mut command = Command::new("cargo");
                command.arg("about");
                command
            }
            Self::NixRun => {
                let mut command = Command::new("nix");
                command.args(["run", "nixpkgs#cargo-about", "--"]);
                command
            }
        };
        command.args(arguments).current_dir(root);
        command
    }
}

/// The first line where two renderings differ, 1-based, if they differ.
pub(crate) fn first_difference(committed: &str, fresh: &str) -> Option<usize> {
    let mut committed_lines = committed.lines();
    let mut fresh_lines = fresh.lines();
    let mut line = 1;
    loop {
        match (committed_lines.next(), fresh_lines.next()) {
            (None, None) => return None,
            (left, right) if left != right => return Some(line),
            _ => line += 1,
        }
    }
}

fn cargo_about_installed() -> bool {
    Command::new("cargo")
        .args(["about", "--version"])
        .output()
        .is_ok_and(|output| output.status.success())
}

fn render(root: &Path) -> Result<String> {
    let launcher = if cargo_about_installed() {
        AboutLauncher::CargoSubcommand
    } else {
        AboutLauncher::NixRun
    };
    let output = launcher.command(root).output().context(
        "run cargo-about (install it with `cargo install cargo-about`, or make `nix` available)",
    )?;
    if !output.status.success() {
        bail!(
            "cargo-about failed: {}",
            String::from_utf8_lossy(&output.stderr).trim()
        );
    }
    let text = String::from_utf8(output.stdout).context("cargo-about wrote non-UTF-8 output")?;
    if text.trim().is_empty() {
        bail!("cargo-about produced no output");
    }
    Ok(text)
}

pub(crate) fn run(root: &Path, args: &LicensesArgs) -> Result<()> {
    let fresh = render(root)?;
    let path = root.join(OUTPUT);
    if args.check {
        let committed = fs::read_to_string(&path).unwrap_or_default();
        if let Some(line) = first_difference(&committed, &fresh) {
            bail!("{OUTPUT} is stale from line {line}; run `cargo xtask licenses`");
        }
        println!("{OUTPUT} is current.");
        return Ok(());
    }
    fs::write(&path, fresh).with_context(|| format!("write {}", path.display()))?;
    println!("wrote {}", path.display());
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{Cli, Commands};
    use clap::Parser;

    #[test]
    fn licenses_parses_the_check_mode() {
        let Commands::Licenses(args) = Cli::try_parse_from(["xtask", "licenses", "--check"])
            .expect("licenses parses")
            .command
        else {
            panic!("licenses");
        };
        assert_eq!(args, LicensesArgs { check: true });
    }

    #[test]
    fn a_stale_rendering_names_its_first_differing_line() {
        assert_eq!(first_difference("a\nb\n", "a\nb\n"), None);
        assert_eq!(first_difference("a\nb\n", "a\nc\n"), Some(2));
        assert_eq!(first_difference("a\n", "a\nb\n"), Some(2));
        assert_eq!(first_difference("", "a"), Some(1));
    }

    #[test]
    fn both_launchers_render_with_the_checked_in_configuration() {
        for launcher in [AboutLauncher::CargoSubcommand, AboutLauncher::NixRun] {
            let command = launcher.command(Path::new("/repo"));
            let arguments = command
                .get_args()
                .map(|argument| argument.to_string_lossy().into_owned())
                .collect::<Vec<_>>();
            assert!(arguments.ends_with(&[
                "generate".to_string(),
                "--config".to_string(),
                CONFIG.to_string(),
                TEMPLATE.to_string(),
            ]));
            assert_eq!(command.get_current_dir(), Some(Path::new("/repo")));
        }
    }
}
