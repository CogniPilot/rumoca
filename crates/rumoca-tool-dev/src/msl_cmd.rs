use anyhow::{Context, Result, bail};
use clap::Parser;
use std::env;
use std::path::{Path, PathBuf};
use std::process::Command;

#[derive(Debug, Parser, Clone)]
pub(crate) struct MslArgs {
    /// Arguments forwarded to rumoca-msl-tools
    #[arg(trailing_var_arg = true, allow_hyphen_values = true)]
    pub(crate) args: Vec<String>,
}

pub(crate) fn run(args: MslArgs, repo_root: &Path) -> Result<()> {
    if let Some(exe) = resolve_local_executable()? {
        let status = Command::new(&exe)
            .args(&args.args)
            .status()
            .with_context(|| format!("failed to run {}", exe.display()))?;
        if status.success() {
            return Ok(());
        }
        bail!("rumoca-msl-tools failed (status={status})");
    }

    let status = Command::new("cargo")
        .arg("run")
        .arg("--bin")
        .arg("rumoca-msl-tools")
        .arg("--")
        .args(&args.args)
        .current_dir(repo_root)
        .status()
        .context("failed to invoke cargo fallback for rumoca-msl-tools")?;
    if status.success() {
        return Ok(());
    }
    bail!("cargo fallback for rumoca-msl-tools failed (status={status})");
}

fn resolve_local_executable() -> Result<Option<PathBuf>> {
    if let Ok(explicit) = env::var("RUMOCA_MSL_TOOLS_EXE") {
        let path = PathBuf::from(explicit);
        if path.is_file() {
            return Ok(Some(path));
        }
    }

    let current_exe =
        env::current_exe().context("failed to determine current executable for msl dispatch")?;
    let Some(dir) = current_exe.parent() else {
        return Ok(None);
    };
    let mut candidate = dir.join("rumoca-msl-tools");
    if cfg!(windows) {
        candidate.set_extension("exe");
    }
    if candidate.is_file() {
        Ok(Some(candidate))
    } else {
        Ok(None)
    }
}
