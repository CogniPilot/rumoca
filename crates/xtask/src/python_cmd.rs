//! Installed-wheel verification shared by CI and the full local suite.

#[cfg(test)]
mod tests;

use std::path::{Path, PathBuf};
use std::process::Command;

use anyhow::{Context, Result, ensure};
use clap::Args;

use crate::{repo_root, run_status};

#[derive(Debug, Args, Clone)]
pub(crate) struct PythonTestArgs {
    /// Provisioned Python interpreter with the requested optional dependencies.
    #[arg(long, default_value = "python3")]
    python: PathBuf,
    /// Existing wheel to install and test; otherwise build the current source.
    #[arg(long)]
    wheel: Option<PathBuf>,
    /// Require Linux x86_64 guide, JAX, CasADi, plotting and CMake checks.
    #[arg(long)]
    require_guide: bool,
}

pub(crate) fn run(args: PythonTestArgs) -> Result<()> {
    let root = repo_root();
    let temporary = tempfile::tempdir().context("create isolated wheel-test directory")?;
    let wheel = resolve_wheel(&root, temporary.path(), &args)?;
    if args.require_guide {
        // The guide benchmark invokes this CLI inside its bounded runtime check.
        let mut cli = Command::new("cargo");
        cli.args(["build", "--locked", "--release", "-p", "rumoca"])
            .current_dir(&root);
        run_status(cli)?;
    }
    let installation = temporary.path().join("installed");
    let mut install = Command::new(&args.python);
    install
        .args(["-m", "pip", "install", "--no-deps", "--target"])
        .arg(&installation)
        .arg(&wheel)
        .current_dir(&root);
    run_status(install)?;

    // Install into a process-owned directory without mutating the supplied
    // interpreter. Existing dependency paths remain available after this wheel.
    let mut python_paths = vec![installation.clone()];
    if let Some(existing) = std::env::var_os("PYTHONPATH") {
        python_paths.extend(std::env::split_paths(&existing));
    }
    let mut test = Command::new(&args.python);
    test.arg(root.join("crates/rumoca-bind-python/tests/wheel_test.py"))
        .arg("--installed-root")
        .arg(&installation)
        .env("PYTHONPATH", std::env::join_paths(python_paths)?)
        .current_dir(&root);
    if args.require_guide {
        test.arg("--require-guide");
    }
    run_status(test)
}

fn resolve_wheel(root: &Path, temporary: &Path, args: &PythonTestArgs) -> Result<PathBuf> {
    if let Some(wheel) = &args.wheel {
        let path = if wheel.is_absolute() {
            wheel.clone()
        } else {
            root.join(wheel)
        };
        ensure!(
            path.is_file() && path.extension().is_some_and(|ext| ext == "whl"),
            "--wheel must name an existing wheel: {}",
            path.display()
        );
        return Ok(path);
    }
    let directory = temporary.join("wheels");
    let mut build = Command::new("maturin");
    build
        .arg("build")
        .arg("--manifest-path")
        .arg(root.join("crates/rumoca-bind-python/Cargo.toml"))
        .arg("--out")
        .arg(&directory)
        .env("PYO3_PYTHON", &args.python)
        .current_dir(root);
    if cfg!(target_os = "linux") {
        build.args(["--compatibility", "linux"]);
    }
    run_status(build)?;
    let wheels = std::fs::read_dir(&directory)?
        .map(|entry| entry.map(|entry| entry.path()))
        .collect::<std::io::Result<Vec<_>>>()?
        .into_iter()
        .filter(|path| path.is_file() && path.extension().is_some_and(|ext| ext == "whl"))
        .collect::<Vec<_>>();
    ensure!(
        wheels.len() == 1,
        "expected exactly one newly built wheel, got {}",
        wheels.len()
    );
    Ok(wheels.into_iter().next().expect("exactly one wheel"))
}
