use anyhow::Result;
use std::env;
use std::path::{Path, PathBuf};
use std::process::Command;

use crate::run_status;

pub(crate) fn run_workspace_fmt_check(root: &Path) -> Result<()> {
    run_cargo(root, &["fmt", "--all", "--", "--check"])
}

/// Fast, deterministic architecture/policy/budget gates, grouped so they can run
/// in `verify quick` without the full integration-test sweep. Covers the existing
/// hardening/policy test targets in the `rumoca` crate:
/// - `architecture_hardening_test` — crate layering, file-size, and the
///   RUMOCA_* env-var registry (a regression here means a new unregistered env
///   var: remove it or route debug output through `--trace`).
/// - `spec_budget_test` — SPEC set size / per-spec budgets.
/// - `code_size_budget_test` — SPEC_0021 source size guard.
/// - `history_policy_test` — git-history policy.
///
/// Add new architecture/policy test targets here so they're grouped in one fast
/// gate rather than only discovered by the full workspace test run.
pub(crate) fn run_architecture_gates(root: &Path) -> Result<()> {
    run_cargo(
        root,
        &[
            "test",
            "-p",
            "rumoca",
            "--test",
            "architecture_hardening_test",
            "--test",
            "spec_budget_test",
            "--test",
            "code_size_budget_test",
            "--test",
            "history_policy_test",
        ],
    )
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
    run_status(doc)?;
    run_mdbook(root, "docs/user-guide")?;
    run_mdbook(root, "docs/dev-guide")
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

fn run_mdbook(root: &Path, book_dir: &str) -> Result<()> {
    let mut cmd = Command::new("mdbook");
    cmd.arg("build").arg(book_dir).current_dir(root);
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
