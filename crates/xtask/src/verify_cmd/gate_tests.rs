use super::VerifyCommand;
use super::gate::{
    GateStep, VerifyGateArgs, blocking_template_failures, changed_crates, changed_packages,
    extract_snapshot, gate_steps, link_shared_caches, resolve_rev, run, run_steps, upstream_main,
};
use clap::Parser;
use std::path::Path;
use std::process::Command;

/// Run `git` in `repo` with a fixed identity, asserting success.
fn git(repo: &Path, args: &[&str]) -> String {
    let output = Command::new("git")
        .args([
            "-c",
            "user.name=Gate Fixture",
            "-c",
            "user.email=gate@fixture.invalid",
        ])
        .arg("-C")
        .arg(repo)
        .args(args)
        .output()
        .expect("git runs");
    assert!(output.status.success(), "git {args:?}: {output:?}");
    String::from_utf8_lossy(&output.stdout).trim().to_string()
}

fn write(path: &Path, text: &str) {
    std::fs::create_dir_all(path.parent().unwrap()).unwrap();
    std::fs::write(path, text).unwrap();
}

/// A repository on `main` with one commit adding `crates/alpha`, and the
/// commit hash.
fn fixture_repository(root: &Path) -> String {
    git(root, &["init", "-q", "-b", "main"]);
    write(
        &root.join("crates/alpha/Cargo.toml"),
        "[package]\nname = \"alpha\"\n",
    );
    git(root, &["add", "-A"]);
    git(root, &["commit", "-q", "-m", "alpha"]);
    git(root, &["rev-parse", "HEAD"])
}

#[test]
fn a_snapshot_holds_the_committed_tree_and_links_the_shared_caches() {
    let repo = tempfile::tempdir().unwrap();
    let head = fixture_repository(repo.path());
    write(
        &repo.path().join("crates/alpha/uncommitted.rs"),
        "// not in HEAD\n",
    );
    assert_eq!(resolve_rev(repo.path(), "HEAD").unwrap(), head);
    assert!(resolve_rev(repo.path(), "no-such-revision").is_err());

    let work = tempfile::tempdir().unwrap();
    let snapshot = work.path().join("snap");
    // A second extraction replaces the first.
    for _ in 0..2 {
        extract_snapshot(repo.path(), &head, &snapshot).unwrap();
    }
    assert!(snapshot.join("crates/alpha/Cargo.toml").is_file());
    assert!(!snapshot.join("crates/alpha/uncommitted.rs").exists());

    let root = work.path().join("root");
    std::fs::create_dir_all(root.join("target/msl")).unwrap();
    link_shared_caches(&root, &snapshot).unwrap();
    assert!(snapshot.join("target/msl").exists());
    assert!(!snapshot.join("target/fmi-conformance").exists());
}

#[test]
fn changed_packages_are_the_snapshot_crates_a_revision_touches() {
    let repo = tempfile::tempdir().unwrap();
    let base = fixture_repository(repo.path());
    let remote = repo.path().to_string_lossy().into_owned();
    assert_eq!(upstream_main(repo.path(), &remote), base);
    assert_eq!(
        upstream_main(repo.path(), "/no/such/remote"),
        "origin/main",
        "an unreadable upstream falls back to the tracking branch"
    );
    write(
        &repo.path().join("crates/beta/Cargo.toml"),
        "[package]\nname = \"beta\"\n",
    );
    write(&repo.path().join("crates/gone/lib.rs"), "\n");
    git(repo.path(), &["add", "-A"]);
    git(repo.path(), &["commit", "-q", "-m", "beta"]);
    let head = git(repo.path(), &["rev-parse", "HEAD"]);
    let snapshot = tempfile::tempdir().unwrap();
    extract_snapshot(repo.path(), &head, snapshot.path()).unwrap();
    // `gone` has no manifest in the snapshot, so it is not a package.
    assert_eq!(
        changed_packages(repo.path(), &base, &head, snapshot.path()),
        ["beta"]
    );
}

fn step(name: &'static str, args: &[&str], template_runtime: bool) -> GateStep {
    GateStep {
        name,
        program: "git",
        args: args.iter().map(|arg| (*arg).to_string()).collect(),
        env: vec![("GATE_FIXTURE", "1")],
        template_runtime,
    }
}

#[test]
fn steps_run_in_order_and_stop_at_the_first_failure() {
    let work = tempfile::tempdir().unwrap();
    let mut log = std::fs::File::create(work.path().join("gate.log")).unwrap();
    let steps = [
        step("version", &["--version"], false),
        step("template", &["--version"], true),
        step("broken", &["no-such-subcommand"], false),
        step("never", &["--version"], false),
    ];
    let failed = run_steps(&steps, work.path(), &work.path().join("target"), &mut log).unwrap();
    assert_eq!(failed, Some("broken"));
    let text = std::fs::read_to_string(work.path().join("gate.log")).unwrap();
    assert!(text.contains("### template ok") && text.contains("### broken FAILED"));
    assert!(
        !text.contains("### never"),
        "the gate stops at the first failure"
    );
    assert_eq!(
        run_steps(&steps[..2], work.path(), work.path(), &mut log).unwrap(),
        None
    );
}

#[test]
fn a_snapshot_without_a_workspace_fails_the_gate_at_its_first_step() {
    let repo = tempfile::tempdir().unwrap();
    fixture_repository(repo.path());
    let root = tempfile::tempdir().unwrap();
    let args = VerifyGateArgs {
        worktree: Some(repo.path().to_path_buf()),
        crates: vec!["alpha".into()],
        ..Default::default()
    };
    let error = run(root.path(), &args).expect_err("the fixture has no Cargo workspace");
    assert!(error.to_string().contains("`fmt`"), "{error}");
}

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
