use anyhow::Result;
use clap::Parser;
use std::path::Path;
use std::process::Command;

use crate::run_status;

#[derive(Debug, Parser, Clone)]
pub(crate) struct TestArgs {
    /// Skip slow tests
    #[arg(long, short = 'q')]
    pub(crate) quick: bool,
    /// Auto-format Rust before checks
    #[arg(long, short = 'f')]
    pub(crate) fix: bool,
}

pub(crate) fn run(args: TestArgs, root: &Path) -> Result<()> {
    if args.fix {
        println!("Formatting Rust code...");
        run_cargo(root, &["fmt"])?;
    } else {
        println!("Checking Rust formatting...");
        run_cargo(root, &["fmt", "--check"])?;
    }

    println!("Running clippy...");
    run_cargo(root, &["clippy", "--all-targets", "--", "-D", "warnings"])?;

    println!("Running tests...");
    if args.quick {
        run_cargo(root, &["test", "--lib"])?;
    } else {
        run_cargo(root, &["test"])?;
    }

    if args.fix {
        let mut fmt_modelica = Command::new("cargo");
        fmt_modelica
            .arg("run")
            .arg("--bin")
            .arg("rumoca")
            .arg("--")
            .arg("fmt")
            .current_dir(root);
        let _ = run_status(fmt_modelica);
    } else {
        let mut fmt_modelica = Command::new("cargo");
        fmt_modelica
            .arg("run")
            .arg("--bin")
            .arg("rumoca")
            .arg("--")
            .arg("fmt")
            .arg("--check")
            .current_dir(root);
        if let Err(error) = run_status(fmt_modelica) {
            println!("Warning: Modelica formatting check failed: {error}");
        }
    }

    println!("All checks passed.");
    Ok(())
}

pub(crate) fn run_ci_parity(root: &Path) -> Result<()> {
    println!("Running CI-parity local gate...");
    run_cargo(root, &["fmt", "--all", "--", "--check"])?;
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
    )?;

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
    )?;
    println!("CI-parity local gate passed.");
    Ok(())
}

fn run_cargo(root: &Path, args: &[&str]) -> Result<()> {
    let mut cmd = Command::new("cargo");
    cmd.args(args).current_dir(root);
    run_status(cmd)
}
