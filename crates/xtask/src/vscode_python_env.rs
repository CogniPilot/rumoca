//! Examples Python environment setup for `xtask vscode edit`.
//!
//! Builds/installs the local rumoca wheel into `examples/.venv` so the example
//! notebooks (and the `%%modelica` magic) are ready to use against the
//! working-tree build instead of the published PyPI package.

use std::path::{Path, PathBuf};
use std::process::{Command, Stdio};

use anyhow::{Context, Result, ensure};

use crate::run_status;

/// Executable paths (`python`, `maturin`) inside a venv directory, per platform.
pub(crate) fn examples_venv_bins(venv_dir: &Path) -> (PathBuf, PathBuf) {
    let (subdir, python, maturin) = if cfg!(windows) {
        ("Scripts", "python.exe", "maturin.exe")
    } else {
        ("bin", "python", "maturin")
    };
    (
        venv_dir.join(subdir).join(python),
        venv_dir.join(subdir).join(maturin),
    )
}

/// A `Command` with `PYTHONPATH` cleared so the venv is isolated from an
/// overlaid environment (e.g. a ROS-sourced shell). Without this, a sourced
/// `PYTHONPATH` leaks system/ROS site-packages into the venv, and pip then warns
/// about *those* packages' unmet dependencies during install.
fn isolated_command(program: impl AsRef<std::ffi::OsStr>) -> Command {
    let mut command = Command::new(program);
    command.env_remove("PYTHONPATH");
    command
}

/// First working base interpreter from `python3`/`python`, used to bootstrap the
/// examples venv.
fn resolve_base_python() -> Option<&'static str> {
    ["python3", "python"].into_iter().find(|bin| {
        Command::new(bin)
            .arg("--version")
            .stdout(Stdio::null())
            .stderr(Stdio::null())
            .status()
            .map(|status| status.success())
            .unwrap_or(false)
    })
}

/// Create/reuse `examples/.venv` and make it ready to run the example notebooks
/// against the local rumoca build.
///
/// Flow: build the local wheel with `maturin develop --extras notebook` (this
/// overrides any PyPI `rumoca` with the working-tree build and pulls IPython for
/// the `%%modelica` magic), then `pip install -r examples/requirements.txt` for
/// the rest of the example deps (rumoca is already satisfied, so pip leaves the
/// local build in place). Users instead just run the `pip install -r` step,
/// which pulls `rumoca` from PyPI.
pub(crate) fn prepare_examples_python_env(root: &Path) -> Result<()> {
    let examples_dir = root.join("examples");
    let requirements = examples_dir.join("requirements.txt");
    ensure!(
        requirements.is_file(),
        "missing {} (cannot prepare examples Python env)",
        requirements.display()
    );
    let binding_dir = root.join("crates/rumoca-bind-python");

    let venv_dir = examples_dir.join(".venv");
    let (venv_python, venv_maturin) = examples_venv_bins(&venv_dir);

    if !venv_python.is_file() {
        let base_python = resolve_base_python()
            .context("no `python3`/`python` found on PATH to create examples/.venv")?;
        println!(
            "[xtask vscode edit] creating examples venv at {}",
            venv_dir.display()
        );
        let mut create = isolated_command(base_python);
        create.arg("-m").arg("venv").arg(&venv_dir);
        run_status(create)?;
    }

    println!("[xtask vscode edit] ensuring pip + maturin in examples venv...");
    let mut tooling = isolated_command(&venv_python);
    tooling
        .arg("-m")
        .arg("pip")
        .arg("install")
        .arg("--upgrade")
        .arg("pip")
        .arg("maturin");
    run_status(tooling)?;

    println!("[xtask vscode edit] building local rumoca (maturin develop --extras notebook)...");
    let mut develop = isolated_command(&venv_maturin);
    develop
        .arg("develop")
        .arg("--extras")
        .arg("notebook")
        .current_dir(&binding_dir)
        .env("VIRTUAL_ENV", &venv_dir);
    run_status(develop)?;

    println!("[xtask vscode edit] installing example requirements...");
    let mut reqs = isolated_command(&venv_python);
    reqs.arg("-m")
        .arg("pip")
        .arg("install")
        .arg("-r")
        .arg(&requirements);
    run_status(reqs)?;

    println!(
        "[xtask vscode edit] examples venv ready: {}",
        venv_python.display()
    );
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::examples_venv_bins;
    use crate::repo_root;

    #[test]
    fn examples_venv_bins_are_platform_correct() {
        let (python, maturin) = examples_venv_bins(std::path::Path::new("/tmp/ex/.venv"));
        if cfg!(windows) {
            assert!(python.ends_with("python.exe"), "{}", python.display());
            assert!(maturin.ends_with("maturin.exe"), "{}", maturin.display());
        } else {
            assert!(python.ends_with("bin/python"), "{}", python.display());
            assert!(maturin.ends_with("bin/maturin"), "{}", maturin.display());
        }
    }

    #[test]
    fn examples_requirements_pin_notebook_extra() {
        let reqs = repo_root().join("examples/requirements.txt");
        let body = std::fs::read_to_string(&reqs).expect("examples/requirements.txt exists");
        assert!(
            body.contains("rumoca[notebook]"),
            "examples requirements must install the notebook extra so the magic works"
        );
    }
}
