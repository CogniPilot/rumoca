//! `cargo xtask verify gate`: the pre-landing gate over a committed snapshot.
//!
//! The gate extracts a `git archive` of one revision (or of a worktree's
//! `HEAD`) into a fresh directory with its own Cargo target directory, links
//! the shared MSL and FMI conformance caches, and runs the blocking steps of
//! CI in order, stopping at the first failure. It prints one
//! `GATE_OK <rev> (<log>)` or `GATE_FAILED <rev> (<log>)` line plus the
//! failing step, and removes the target directory after a pass unless
//! `--keep` is given. Uncommitted changes are never part of the snapshot.

use anyhow::{Context, Result, bail};
use clap::Args;
use std::fs;
use std::io::Write;
use std::path::{Path, PathBuf};
use std::process::{Command, Stdio};

/// The repository whose `main` branch decides which crates changed.
const UPSTREAM: &str = "https://github.com/CogniPilot/rumoca";

/// Template targets whose Python packages a local shell need not provide;
/// their failures are reported but do not fail the gate.
const OPTIONAL_TEMPLATE_TARGETS: [&str; 2] = ["casadi", "jax"];

#[derive(Debug, Args, Clone, PartialEq, Eq, Default)]
pub(crate) struct VerifyGateArgs {
    /// Revision to snapshot (default: `HEAD` of this repository)
    #[arg(long, conflicts_with = "worktree")]
    pub(crate) rev: Option<String>,
    /// Worktree whose committed `HEAD` is snapshotted
    #[arg(long)]
    pub(crate) worktree: Option<PathBuf>,
    /// Also run the coverage trim gate with CI's flags (about an hour)
    #[arg(long)]
    pub(crate) coverage: bool,
    /// Crates whose unit tests and docs run (default: crates changed relative
    /// to upstream `main`)
    #[arg(long, num_args = 1..)]
    pub(crate) crates: Vec<String>,
    /// Keep the gate's Cargo target directory after a pass
    #[arg(long)]
    pub(crate) keep: bool,
}

/// One blocking step: a command run in the snapshot.
#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) struct GateStep {
    pub(crate) name: &'static str,
    pub(crate) args: Vec<String>,
    pub(crate) env: Vec<(&'static str, &'static str)>,
    /// Template runtime tests: judged by their failing test names.
    pub(crate) template_runtime: bool,
}

impl GateStep {
    fn cargo(name: &'static str, args: &[&str]) -> Self {
        Self {
            name,
            args: args.iter().map(|arg| (*arg).to_string()).collect(),
            env: Vec::new(),
            template_runtime: false,
        }
    }

    fn with_packages(mut self, packages: &[String], trailing: &[&str]) -> Self {
        for package in packages {
            self.args.push("-p".into());
            self.args.push(package.clone());
        }
        self.args
            .extend(trailing.iter().map(|arg| (*arg).to_string()));
        self
    }
}

/// The ordered blocking steps for `packages` (the crates whose unit tests and
/// docs run), with the coverage steps when requested.
pub(crate) fn gate_steps(packages: &[String], coverage: bool) -> Vec<GateStep> {
    let mut steps = vec![
        GateStep::cargo("fmt", &["fmt", "--all", "--", "--check"]),
        GateStep::cargo(
            "clippy",
            &[
                "clippy",
                "-j",
                "8",
                "--workspace",
                "--all-targets",
                "--exclude",
                "rumoca-phase-instantiate",
                "--",
                "-D",
                "warnings",
            ],
        ),
        GateStep::cargo("lint", &["xtask", "verify", "lint"]),
    ];
    // `commit_messages` reads git history, which an archived snapshot lacks.
    if !packages.is_empty() {
        steps.push(
            GateStep::cargo("crate-tests", &["test", "-j", "8"])
                .with_packages(packages, &["--", "--skip", "commit_messages"]),
        );
    }
    steps.push(GateStep::cargo(
        "suite_core",
        &["test", "-j", "8", "-p", "rumoca", "--test", "suite_core"],
    ));
    steps.push(GateStep::cargo(
        "msl-sim-tests",
        &[
            "test",
            "-j",
            "8",
            "-p",
            "rumoca",
            "-p",
            "rumoca-sim",
            "--features",
            "rumoca/msl-sim-tests",
            "--lib",
            "--tests",
            "--",
            "--skip",
            "commit_messages",
        ],
    ));
    steps.push(GateStep::cargo(
        "arch-gates",
        &[
            "test",
            "-j",
            "8",
            "-p",
            "rumoca",
            "--test",
            "architecture_hardening_test",
            "--test",
            "suite_gates",
            "--",
            "--skip",
            "commit_messages",
        ],
    ));
    if !packages.is_empty() {
        let mut doc =
            GateStep::cargo("doc", &["doc", "-j", "8"]).with_packages(packages, &["--no-deps"]);
        doc.env.push(("RUSTDOCFLAGS", "-D warnings"));
        steps.push(doc);
    }
    let mut template = GateStep::cargo(
        "template-runtime",
        &[
            "test",
            "-j",
            "8",
            "-p",
            "rumoca",
            "--features",
            "template-runtime-tests",
            "--test",
            "suite_template_runtime",
        ],
    );
    template.template_runtime = true;
    steps.push(template);
    if coverage {
        steps.extend(coverage_steps());
    }
    steps
}

fn coverage_steps() -> Vec<GateStep> {
    vec![
        GateStep::cargo(
            "coverage-run",
            &[
                "llvm-cov",
                "--workspace",
                "--tests",
                "--json",
                "--output-path",
                "target/llvm-cov/workspace-full.json",
                "--ignore-run-fail",
            ],
        ),
        GateStep::cargo(
            "coverage-summary",
            &[
                "llvm-cov",
                "report",
                "--json",
                "--summary-only",
                "--output-path",
                "target/llvm-cov/workspace-summary.json",
            ],
        ),
        GateStep::cargo(
            "coverage-report",
            &[
                "run", "-q", "-p", "xtask", "--bin", "xtask", "--", "coverage", "report",
            ],
        ),
        GateStep::cargo(
            "coverage-gate",
            &[
                "run",
                "-q",
                "-p",
                "xtask",
                "--bin",
                "xtask",
                "--",
                "coverage",
                "gate",
                "--enforce-trim-regressions",
                "--allowed-zero-count-growth",
                "2",
                "--allowed-dead-likely-growth",
                "2",
                "--allowed-total-candidate-growth",
                "2",
                "--allowed-needs-targeted-test-growth",
                "2",
            ],
        ),
    ]
}

/// The crates a change touches: the first path component under `crates/`.
pub(crate) fn changed_crates(changed_paths: &str) -> Vec<String> {
    let mut crates = changed_paths
        .lines()
        .filter_map(|path| path.strip_prefix("crates/"))
        .filter_map(|rest| rest.split_once('/').map(|(name, _)| name.to_string()))
        .collect::<Vec<_>>();
    crates.sort();
    crates.dedup();
    crates
}

/// Failing template tests other than the optional Python targets.
pub(crate) fn blocking_template_failures(output: &str) -> Vec<&str> {
    output
        .lines()
        .filter(|line| line.starts_with("test ") && line.ends_with("... FAILED"))
        .filter(|line| {
            !OPTIONAL_TEMPLATE_TARGETS
                .iter()
                .any(|target| line.contains(target))
        })
        .collect()
}

pub(crate) fn run(root: &Path, args: &VerifyGateArgs) -> Result<()> {
    let repo = args.worktree.clone().unwrap_or_else(|| root.to_path_buf());
    let rev = resolve_rev(&repo, args.rev.as_deref().unwrap_or("HEAD"))?;
    let short = &rev[..rev.len().min(12)];
    let base = std::env::temp_dir().join("rumoca-gate").join(short);
    let snapshot = base.join("snap");
    let target = base.join("target");
    let log_path = base.join("gate.log");
    extract_snapshot(&repo, &rev, &snapshot)?;
    link_shared_caches(root, &snapshot)?;
    let packages = if args.crates.is_empty() {
        changed_packages(&repo, &rev, &snapshot)
    } else {
        args.crates.clone()
    };
    let mut log =
        fs::File::create(&log_path).with_context(|| format!("create {}", log_path.display()))?;
    writeln!(
        log,
        "gate: {rev} crates={} coverage={}",
        packages.join(" "),
        args.coverage
    )?;
    let failed = run_steps(
        &gate_steps(&packages, args.coverage),
        &snapshot,
        &target,
        &mut log,
    )?;
    match failed {
        None => {
            println!("GATE_OK {rev} ({})", log_path.display());
            if !args.keep {
                fs::remove_dir_all(&target).ok();
            }
            Ok(())
        }
        Some(step) => {
            println!("GATE_FAILED {rev} ({})", log_path.display());
            println!("failed step: {step}");
            bail!("gate failed at `{step}`")
        }
    }
}

fn git(repo: &Path) -> Command {
    let mut command = Command::new("git");
    command.arg("-C").arg(repo);
    command
}

fn resolve_rev(repo: &Path, rev: &str) -> Result<String> {
    let output = git(repo)
        .args(["rev-parse", "--verify", &format!("{rev}^{{commit}}")])
        .output()
        .context("run git rev-parse")?;
    if !output.status.success() {
        bail!("`{rev}` is not a commit in {}", repo.display());
    }
    Ok(String::from_utf8_lossy(&output.stdout).trim().to_string())
}

/// Extract `git archive <rev>` into a fresh `snapshot` directory.
fn extract_snapshot(repo: &Path, rev: &str, snapshot: &Path) -> Result<()> {
    if snapshot.exists() {
        fs::remove_dir_all(snapshot).with_context(|| format!("clear {}", snapshot.display()))?;
    }
    fs::create_dir_all(snapshot)?;
    let archive = snapshot.with_extension("zip");
    let status = git(repo)
        .args(["archive", "--format=zip", "-o"])
        .arg(&archive)
        .arg(rev)
        .status()
        .context("run git archive")?;
    if !status.success() {
        bail!("git archive {rev} failed");
    }
    let file = fs::File::open(&archive)?;
    zip::ZipArchive::new(file)
        .and_then(|mut zip| zip.extract(snapshot))
        .with_context(|| format!("extract {}", archive.display()))?;
    fs::remove_file(&archive).ok();
    Ok(())
}

/// Link the repository's MSL and FMI conformance caches into the snapshot.
fn link_shared_caches(root: &Path, snapshot: &Path) -> Result<()> {
    let target = snapshot.join("target");
    fs::create_dir_all(&target)?;
    for cache in ["msl", "fmi-conformance"] {
        let source = root.join("target").join(cache);
        if source.exists() {
            link_dir(&source, &target.join(cache))
                .with_context(|| format!("link {}", source.display()))?;
        }
    }
    Ok(())
}

#[cfg(unix)]
fn link_dir(source: &Path, link: &Path) -> std::io::Result<()> {
    std::os::unix::fs::symlink(source, link)
}

#[cfg(windows)]
fn link_dir(source: &Path, link: &Path) -> std::io::Result<()> {
    std::os::windows::fs::symlink_dir(source, link)
}

/// Packages under `crates/` changed between upstream `main` and `rev`.
fn changed_packages(repo: &Path, rev: &str, snapshot: &Path) -> Vec<String> {
    let main = git(repo)
        .args(["ls-remote", "-q", UPSTREAM, "refs/heads/main"])
        .output()
        .ok()
        .and_then(|output| {
            let text = String::from_utf8_lossy(&output.stdout).into_owned();
            text.split_whitespace().next().map(str::to_string)
        })
        .unwrap_or_else(|| "origin/main".to_string());
    let diff = git(repo)
        .args(["diff", "--name-only", &main, rev])
        .output()
        .map(|output| String::from_utf8_lossy(&output.stdout).into_owned())
        .unwrap_or_default();
    changed_crates(&diff)
        .into_iter()
        .filter(|name| {
            snapshot
                .join("crates")
                .join(name)
                .join("Cargo.toml")
                .is_file()
        })
        .collect()
}

/// Run each step in order; the name of the first failing step, if any.
fn run_steps(
    steps: &[GateStep],
    snapshot: &Path,
    target: &Path,
    log: &mut fs::File,
) -> Result<Option<&'static str>> {
    for step in steps {
        println!("### {}", step.name);
        writeln!(log, "### {}", step.name)?;
        let mut command = Command::new("cargo");
        command
            .args(&step.args)
            .current_dir(snapshot)
            .env("CARGO_TARGET_DIR", target)
            .stdin(Stdio::null());
        for (key, value) in &step.env {
            command.env(key, value);
        }
        let output = command
            .output()
            .with_context(|| format!("run gate step `{}`", step.name))?;
        log.write_all(&output.stdout)?;
        log.write_all(&output.stderr)?;
        let passed = if step.template_runtime {
            let text = String::from_utf8_lossy(&output.stdout);
            let blocking = blocking_template_failures(&text);
            for failure in &blocking {
                writeln!(log, "blocking: {failure}")?;
            }
            // A failed build reports no test result at all.
            blocking.is_empty() && (output.status.success() || text.contains("test result:"))
        } else {
            output.status.success()
        };
        writeln!(
            log,
            "### {} {}",
            step.name,
            if passed { "ok" } else { "FAILED" }
        )?;
        if !passed {
            return Ok(Some(step.name));
        }
    }
    Ok(None)
}
