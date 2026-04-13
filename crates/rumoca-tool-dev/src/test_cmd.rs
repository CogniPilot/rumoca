use anyhow::Result;
use std::env;
use std::path::{Path, PathBuf};
use std::process::Command;

use crate::run_status;

pub(crate) fn run_workspace_fmt_check(root: &Path) -> Result<()> {
    run_cargo(root, &["fmt", "--all", "--", "--check"])
}

pub(crate) fn run_workspace_clippy(root: &Path) -> Result<()> {
    let mut cmd = Command::new("cargo");
    cmd.arg("clippy")
        .arg("--workspace")
        .arg("--all-targets")
        .arg("--all-features")
        .arg("--")
        .arg("-D")
        .arg("warnings")
        .current_dir(root);

    if let Some(python) = resolve_python_for_pyo3() {
        cmd.env("PYO3_PYTHON", python);
    }

    run_status(cmd)
}

pub(crate) fn run_workspace_docs(root: &Path) -> Result<()> {
    let mut doc = Command::new("cargo");
    doc.arg("doc")
        .arg("--workspace")
        .arg("--no-deps")
        .arg("--exclude")
        .arg("rumoca-bind-python")
        .arg("--exclude")
        .arg("rumoca-bind-wasm")
        .env("RUSTDOCFLAGS", "-D warnings")
        .current_dir(root);
    run_status(doc)
}

pub(crate) fn run_workspace_tests(root: &Path) -> Result<()> {
    run_cargo(
        root,
        &[
            "test",
            "--workspace",
            "--verbose",
            "--exclude",
            "rumoca-bind-python",
            "--exclude",
            "rumoca-bind-wasm",
        ],
    )
}

pub(crate) fn run_workspace_binary_build(root: &Path) -> Result<()> {
    run_cargo(
        root,
        &[
            "build",
            "--verbose",
            "--bin",
            "rumoca",
            "--bin",
            "rumoca-lsp",
        ],
    )
}

fn run_cargo(root: &Path, args: &[&str]) -> Result<()> {
    let mut cmd = Command::new("cargo");
    cmd.args(args).current_dir(root);
    run_status(cmd)
}

fn resolve_python_for_pyo3() -> Option<PathBuf> {
    if let Some(from_env) = env::var_os("PYO3_PYTHON") {
        let path = PathBuf::from(from_env);
        if path.is_file() {
            return Some(path);
        }
    }

    resolve_from_path("python3").or_else(|| resolve_from_path("python"))
}

fn resolve_from_path(bin: &str) -> Option<PathBuf> {
    let path_var = env::var_os("PATH")?;
    for dir in env::split_paths(&path_var) {
        let candidate = dir.join(bin);
        if candidate.is_file() {
            return Some(candidate);
        }
    }
    None
}
