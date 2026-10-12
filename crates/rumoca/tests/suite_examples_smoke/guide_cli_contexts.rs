//! Guide CLI contracts, selected by `cargo xtask verify examples`.

mod teaching_projects;

use std::path::{Path, PathBuf};
use std::process::Command;

use serde::Deserialize;

#[derive(Deserialize)]
struct Entry {
    page: String,
    line: usize,
    profile: String,
    argv: Vec<Vec<String>>,
}

fn repository_root() -> PathBuf {
    Path::new(env!("CARGO_MANIFEST_DIR")).join("../..")
}

fn python() -> Command {
    let mut command = Command::new("python");
    command.env("PYTHONDONTWRITEBYTECODE", "1");
    command
}

fn entries(mode: &str) -> Vec<Entry> {
    let output = python()
        .arg(
            Path::new(env!("CARGO_MANIFEST_DIR"))
                .join("tests/suite_examples_smoke/guide_cli_contexts/inventory_probe.py"),
        )
        .arg(repository_root())
        .arg(mode)
        .output()
        .expect("Python is a required example-smoke prerequisite");
    assert!(
        output.status.success(),
        "stdout:\n{}\nstderr:\n{}",
        String::from_utf8_lossy(&output.stdout),
        String::from_utf8_lossy(&output.stderr)
    );
    serde_json::from_slice(&output.stdout).expect("validated source span inventory")
}

// Every reviewed bash span has a declared handler. New commands must be assigned
// a teaching context or their real execution owner, rather than silently skipped.
fn reviewed(entry: &Entry) -> bool {
    matches!(
        (entry.page.as_str(), entry.profile.as_str()),
        (
            "codegen/custom-targets.md" | "language/support-status.md" | "simulation/inspect.md",
            "project-template-bash"
        ) | (
            "codegen/targets.md"
                | "getting-started/first-model.md"
                | "simulation/interactive.md"
                | "simulation/reusable-booster-landing.md"
                | "simulation/running.md"
                | "simulation/scenario-tomls.md"
                | "simulation/stdio-session.md",
            "executable-command-bash"
        ) | (
            "codegen/galec-efmi.md"
                | "getting-started/quickstart.md"
                | "language/libraries.md"
                | "tools/cli.md",
            "project-template-bash" | "executable-command-bash"
        ) | (
            "language/neural-odes.md",
            "executable-command-bash" | "executable-benchmark-bash"
        ) | (
            "tools/fmt-lint.md",
            "project-template-bash" | "executable-tool-bash"
        ) | (
            "getting-started/installation.md" | "tools/python.md",
            "environment-setup-bash"
        )
    )
}

#[test]
fn bash_sources_have_valid_syntax() {
    assert_eq!(entries("syntax").len(), 66);
}

#[test]
fn cli_commands_use_declared_teaching_contexts() {
    let entries = entries("argv");
    let mut parsed = 0;
    for entry in &entries {
        let owner = execution_owner(entry);
        eprintln!("{}:{}: {owner}", entry.page, entry.line);
        assert!(matches!(
            entry.profile.as_str(),
            "project-template-bash"
                | "environment-setup-bash"
                | "executable-command-bash"
                | "executable-tool-bash"
                | "executable-benchmark-bash"
        ));
        for argv in &entry.argv {
            let result = rumoca::cli::parse_args(argv.iter().cloned());
            if argv == &["--help"] || argv == &["--version"] {
                assert!(
                    result.is_err(),
                    "Clap displays help/version without dispatch"
                );
            } else {
                assert!(
                    result.is_ok(),
                    "{}:{} {argv:?}: {result:?}",
                    entry.page,
                    entry.line
                );
            }
            parsed += 1;
        }
    }
    assert_eq!(parsed, 78, "review changes to the original CLI argv roster");
    teaching_projects::execute(&entries);
    teaching_projects::first_model();
    eprintln!("66 reviewed bash spans; {parsed} original CLI argv parsed");
}

fn execution_owner(entry: &Entry) -> &'static str {
    assert!(reviewed(entry), "unhandled {}:{}", entry.page, entry.line);
    if teaching_projects::selected(entry) {
        return "actual disposable CLI teaching project";
    }
    match (entry.page.as_str(), entry.profile.as_str()) {
        ("getting-started/first-model.md", _) => {
            "actual HangingMass direct/scenario CLI and analytic trace"
        }
        ("simulation/stdio-session.md", _) => "actual exact Python controller and JSON transcript",
        ("language/neural-odes.md", "executable-benchmark-bash") => {
            "required installed-wheel guide benchmark: actual Backprop1k/JAX"
        }
        ("getting-started/installation.md" | "tools/python.md", _) => {
            "installation syntax; passive CLI argv only, no installer execution"
        }
        ("language/libraries.md", "executable-command-bash") => {
            "required examples CI dependency staging"
        }
        ("codegen/galec-efmi.md", "executable-command-bash") => {
            "required guide actual GALEC model export"
        }
        (_, _) if entry.profile == "project-template-bash" => {
            "original CLI argv; caller supplies model/library/configuration paths"
        }
        ("simulation/interactive.md" | "simulation/reusable-booster-landing.md", _) => {
            "required guide browser/scenario runtime owner; CLI argv parsed here"
        }
        _ => "required guide scenario/model runtime owner; CLI argv parsed here",
    }
}

#[test]
fn stdio_controller_follows_the_sampled_curve() {
    let output = python()
        .arg(
            Path::new(env!("CARGO_MANIFEST_DIR"))
                .join("tests/suite_examples_smoke/guide_cli_contexts/stdio_controller.py"),
        )
        .arg(repository_root())
        .arg(env!("CARGO_BIN_EXE_rumoca"))
        .output()
        .expect("run the exact guide Python controller against the actual CLI");
    assert!(
        output.status.success(),
        "stdout={}\nstderr={}",
        String::from_utf8_lossy(&output.stdout),
        String::from_utf8_lossy(&output.stderr)
    );
    eprintln!("{}", String::from_utf8_lossy(&output.stdout));
}
