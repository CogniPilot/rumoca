use anyhow::Result;
use std::env;
use std::path::{Path, PathBuf};
use std::process::Command;

use crate::{docs_cmd, run_status};

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
    docs_cmd::check(root)
}

/// The two binding crates are excluded from the workspace test lane: they
/// need special targets (wasm32 / a Python interpreter) and are covered by the
/// dedicated playground/python jobs. This is the single source of truth shared
/// by the plain-`cargo test`, doctest, and nextest-shard paths so the three
/// can never drift apart.
const WORKSPACE_TEST_EXCLUDES: &[&str] = &[
    "--exclude",
    "rumoca-bind-python",
    "--exclude",
    "rumoca-bind-wasm",
];

pub(crate) fn run_workspace_tests(root: &Path) -> Result<()> {
    let mut args = vec!["test", "--workspace", "--verbose"];
    args.extend_from_slice(WORKSPACE_TEST_EXCLUDES);
    run_cargo(root, &args)
}

/// Doctests only. `cargo nextest` cannot run doctests, so the sharded CI lane
/// pairs its nextest partitions with exactly one invocation of this to preserve
/// the doctest coverage that plain `cargo test --workspace` provided.
pub(crate) fn run_workspace_doctests(root: &Path) -> Result<()> {
    let mut args = vec!["test", "--doc", "--workspace", "--verbose"];
    args.extend_from_slice(WORKSPACE_TEST_EXCLUDES);
    run_cargo(root, &args)
}

/// Run the workspace unit + integration tests under `cargo nextest`, sharded by
/// `--partition count:<partition>` (e.g. "1/4"). Excludes mirror
/// [`run_workspace_tests`]. Doctests are NOT included (nextest skips them) and
/// must be run separately via [`run_workspace_doctests`].
pub(crate) fn run_workspace_nextest_partition(root: &Path, partition: &str) -> Result<()> {
    let partition_arg = format!("count:{partition}");
    let mut args = vec!["nextest", "run", "--workspace", "--verbose"];
    args.extend_from_slice(WORKSPACE_TEST_EXCLUDES);
    args.push("--partition");
    args.push(&partition_arg);
    run_cargo(root, &args)
}

/// CLI options for `verify workspace`, co-located with the workspace-test
/// runners they dispatch to. With no flags this runs the full workspace
/// `cargo test` (unit + integration + doctests), exactly as before, so the
/// verify-suite step (`cargo xtask verify workspace`) is unchanged. The flags
/// let CI split that lane across parallel shards without duplicating the
/// load-bearing crate-exclude list in YAML.
#[derive(Debug, clap::Args, Clone, PartialEq, Eq)]
pub(crate) struct WorkspaceArgs {
    /// Run only the unit + integration tests in this `count:M/N` nextest shard
    /// (e.g. `--nextest-partition 1/4`). Doctests are excluded (nextest skips
    /// them); pair with a single `--doc` run to keep doctest coverage.
    #[arg(long, value_name = "M/N", conflicts_with = "doc")]
    nextest_partition: Option<String>,
    /// Run only the workspace doctests (`cargo test --doc`).
    #[arg(long)]
    doc: bool,
}

impl WorkspaceArgs {
    /// Dispatch to the doctest-only, nextest-shard, or full-workspace path.
    pub(crate) fn run(&self, root: &Path) -> Result<()> {
        if self.doc {
            run_workspace_doctests(root)
        } else if let Some(partition) = self.nextest_partition.as_deref() {
            run_workspace_nextest_partition(root, partition)
        } else {
            run_workspace_tests(root)
        }
    }
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
