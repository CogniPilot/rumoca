use super::VerifyCommand;
use super::gate::{
    GateStep, VerifyGateArgs, blocking_template_failures, changed_crates, gate_steps,
};
use clap::Parser;

#[derive(Debug, Parser)]
struct Harness {
    #[command(subcommand)]
    command: VerifyCommand,
}

fn parse(args: &[&str]) -> Result<VerifyCommand, clap::Error> {
    Harness::try_parse_from(std::iter::once("verify").chain(args.iter().copied()))
        .map(|harness| harness.command)
}

#[test]
fn gate_parses_rev_worktree_coverage_crates_and_keep() {
    let VerifyCommand::Gate(args) = parse(&[
        "gate",
        "--rev",
        "abc123",
        "--coverage",
        "--crates",
        "rumoca-solver",
        "xtask",
        "--keep",
    ])
    .unwrap() else {
        panic!("gate");
    };
    assert_eq!(
        args,
        VerifyGateArgs {
            rev: Some("abc123".into()),
            worktree: None,
            coverage: true,
            crates: vec!["rumoca-solver".into(), "xtask".into()],
            keep: true,
        }
    );
    let VerifyCommand::Gate(args) = parse(&["gate", "--worktree", "/tmp/tree"]).unwrap() else {
        panic!("gate");
    };
    assert_eq!(
        args.worktree.as_deref(),
        Some(std::path::Path::new("/tmp/tree"))
    );
    assert!(
        parse(&["gate", "--rev", "a", "--worktree", "b"]).is_err(),
        "a revision and a worktree are exclusive"
    );
}

fn names(steps: &[GateStep]) -> Vec<&'static str> {
    steps.iter().map(|step| step.name).collect()
}

#[test]
fn gate_steps_follow_the_blocking_list_in_order() {
    let packages = vec!["rumoca-solver".to_string()];
    let steps = gate_steps(&packages, false);
    assert_eq!(
        names(&steps),
        [
            "fmt",
            "clippy",
            "lint",
            "crate-tests",
            "suite_core",
            "msl-sim-tests",
            "arch-gates",
            "doc",
            "template-runtime"
        ]
    );
    assert!(
        steps[1]
            .args
            .windows(2)
            .any(|pair| pair == ["--exclude", "rumoca-phase-instantiate"])
    );
    assert!(
        steps[3]
            .args
            .windows(2)
            .any(|pair| pair == ["-p", "rumoca-solver"])
    );
    assert!(
        steps[3]
            .args
            .ends_with(&["--skip".to_string(), "commit_messages".to_string()])
    );
    assert_eq!(steps[7].env, [("RUSTDOCFLAGS", "-D warnings")]);
    assert!(steps[8].template_runtime);
    // No changed crate: no crate tests or docs.
    assert!(!names(&gate_steps(&[], false)).contains(&"crate-tests"));
    // Coverage appends CI's run, report, and trim gate.
    let coverage = gate_steps(&packages, true);
    assert_eq!(
        names(&coverage)[9..],
        [
            "coverage-run",
            "coverage-summary",
            "coverage-report",
            "coverage-gate"
        ]
    );
    assert!(
        coverage[12]
            .args
            .contains(&"--enforce-trim-regressions".to_string())
    );
}

#[test]
fn changed_crates_are_the_first_components_under_crates() {
    let paths = "crates/rumoca-solver/src/lib.rs\nspec/SPEC_0025.md\ncrates/xtask/src/main.rs\ncrates/rumoca-solver/Cargo.toml\nCargo.lock\n";
    assert_eq!(changed_crates(paths), ["rumoca-solver", "xtask"]);
}

#[test]
fn only_required_template_targets_block_the_gate() {
    let output = "test fmi::export ... ok\ntest casadi::ode ... FAILED\ntest jax::ode ... FAILED\ntest fmi3::runtime ... FAILED\ntest result: FAILED.";
    assert_eq!(
        blocking_template_failures(output),
        ["test fmi3::runtime ... FAILED"]
    );
}
