//! `cargo xtask verify fuzz` — bounded libFuzzer runs of the parser fuzz target.
//!
//! The fuzz crate lives at the repo root in `fuzz/`, outside the cargo
//! workspace, because `cargo fuzz` builds with nightly sanitizer flags that must
//! not leak into normal workspace builds. This command is deliberately *not* a
//! member of `VERIFY_SUITE_STEPS`: `verify full`/`verify quick` must stay
//! bounded, so fuzzing runs on demand and from the nightly workflow.

use anyhow::{Context, Result, bail};
use clap::Parser;
use std::path::Path;
use std::process::Command;

use crate::run_status;

/// Memory ceiling for one libFuzzer worker. Parsing a Modelica buffer should
/// never approach this; crossing it is itself a finding.
const FUZZ_RSS_LIMIT_MB: u64 = 2048;

#[derive(Debug, Parser, Clone, PartialEq, Eq)]
pub(crate) struct VerifyFuzzArgs {
    /// Wall-clock budget for the fuzz run, in seconds.
    #[arg(long, default_value_t = 60)]
    pub(crate) max_total_secs: u64,
    /// Fuzz target name (a `[[bin]]` in `fuzz/Cargo.toml`).
    #[arg(long, default_value = "parse_modelica")]
    pub(crate) target: String,
    /// Additional libFuzzer `-max_len` cap on generated inputs.
    #[arg(long, default_value_t = 8192)]
    pub(crate) max_len: u64,
}

pub(crate) fn run(args: &VerifyFuzzArgs, root: &Path) -> Result<()> {
    let fuzz_dir = root.join("fuzz");
    if !fuzz_dir.join("Cargo.toml").is_file() {
        bail!(
            "fuzz crate not found at {} — expected the standalone cargo-fuzz manifest",
            fuzz_dir.display()
        );
    }
    ensure_cargo_fuzz_available(root)?;

    let mut cmd = Command::new("cargo");
    cmd.arg("fuzz")
        .arg("run")
        .arg(&args.target)
        .arg("--")
        .arg(format!("-max_total_time={}", args.max_total_secs))
        .arg(format!("-rss_limit_mb={FUZZ_RSS_LIMIT_MB}"))
        .arg(format!("-max_len={}", args.max_len))
        .current_dir(&fuzz_dir);
    run_status(cmd).with_context(|| {
        format!(
            "fuzz target '{}' reported a failure; reproducers are written to {}",
            args.target,
            fuzz_dir.join("artifacts").join(&args.target).display()
        )
    })
}

fn ensure_cargo_fuzz_available(root: &Path) -> Result<()> {
    let mut probe = Command::new("cargo");
    probe.arg("fuzz").arg("--version").current_dir(root);
    crate::resource_budget::apply_to_child(&mut probe);
    let output = probe
        .output()
        .context("failed to run `cargo fuzz --version`")?;
    if output.status.success() {
        return Ok(());
    }
    bail!(
        "`cargo fuzz` is not available; install it with `cargo install cargo-fuzz` \
         (it also needs a nightly toolchain for the sanitizer flags)"
    )
}

#[cfg(test)]
mod tests {
    use super::*;

    #[derive(Debug, Parser)]
    struct Harness {
        #[command(flatten)]
        args: VerifyFuzzArgs,
    }

    #[test]
    fn fuzz_args_default_to_a_bounded_parser_run() {
        let parsed = Harness::parse_from(["verify-fuzz"]);
        assert_eq!(parsed.args.max_total_secs, 60);
        assert_eq!(parsed.args.target, "parse_modelica");
        assert_eq!(parsed.args.max_len, 8192);
    }

    #[test]
    fn fuzz_args_accept_an_explicit_budget_and_target() {
        let parsed = Harness::parse_from([
            "verify-fuzz",
            "--max-total-secs",
            "900",
            "--target",
            "parse_modelica",
        ]);
        assert_eq!(parsed.args.max_total_secs, 900);
        assert_eq!(parsed.args.target, "parse_modelica");
    }

    #[test]
    fn missing_fuzz_crate_is_reported_rather_than_silently_skipped() {
        let empty = tempfile::tempdir().expect("temp dir");
        let error = run(
            &VerifyFuzzArgs {
                max_total_secs: 1,
                target: "parse_modelica".to_string(),
                max_len: 64,
            },
            empty.path(),
        )
        .expect_err("missing fuzz crate must fail");
        assert!(
            error.to_string().contains("fuzz crate not found"),
            "unexpected error: {error}"
        );
    }
}
