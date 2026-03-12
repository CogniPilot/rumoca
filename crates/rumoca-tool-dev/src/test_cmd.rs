use anyhow::Result;
use std::path::Path;
use std::process::Command;

use crate::run_status;

pub(crate) fn run_workspace_fmt_check(root: &Path) -> Result<()> {
    run_cargo(root, &["fmt", "--all", "--", "--check"])
}

pub(crate) fn run_workspace_clippy(root: &Path) -> Result<()> {
    run_cargo(
        root,
        &[
            "clippy",
            "--workspace",
            "--all-targets",
            "--all-features",
            "--",
            "-D",
            "warnings",
        ],
    )
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
