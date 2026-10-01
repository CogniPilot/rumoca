//! `cargo xtask verify gate`: the pre-landing gate over a committed snapshot.
//!
//! The gate extracts a `git archive` of one revision (or of a worktree's
//! `HEAD`) into a fresh directory with its own Cargo target directory, links
//! the shared MSL and FMI conformance caches, and runs the blocking steps of
//! CI in order, stopping at the first failure. It prints one
//! `GATE_OK <rev> (<log>)` or `GATE_FAILED <rev> (<log>)` line plus the
//! failing step, and removes the target directory after a pass unless
//! `--keep` is given. The coverage steps run under one host-wide lock, so two
//! gates never build coverage at once. Uncommitted changes are never part of
//! the snapshot.

use anyhow::{Context, Result, bail};
use clap::Args;
use std::fs;
use std::io::Write;
use std::path::{Path, PathBuf};
use std::process::{Command, Stdio};

/// The repository whose `main` branch decides which crates changed.
const UPSTREAM: &str = "https://github.com/CogniPilot/rumoca";

/// The host-wide lock the coverage steps hold, shared with every gate on the
/// host (the scratch `gate.sh` takes the same file): each coverage build needs
/// about 50 GB of target directory, so two at once can fill the disk. A fixed
/// path, since `nix develop` points `TMPDIR` at a per-shell directory.
const COVERAGE_LOCK: &str = "/tmp/rumoca-coverage.lock";

/// The snapshot-relative directory the coverage steps write their reports to.
const COVERAGE_OUTPUT_DIR: &str = "target/llvm-cov";

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
    /// Crates whose rustdoc runs (default: crates changed relative to upstream
    /// `main`); every crate's tests always run
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
    /// The program the step runs in the snapshot (`cargo` for every CI step).
    pub(crate) program: &'static str,
    pub(crate) args: Vec<String>,
    pub(crate) env: Vec<(&'static str, &'static str)>,
    /// Template runtime tests: judged by their failing test names.
    pub(crate) template_runtime: bool,
    /// Runs under the host-wide coverage lock, held from the first such step to
    /// the end of the gate.
    pub(crate) host_lock: bool,
}

impl GateStep {
    fn cargo(name: &'static str, args: &[&str]) -> Self {
        Self {
            name,
            program: "cargo",
            args: args.iter().map(|arg| (*arg).to_string()).collect(),
            env: Vec::new(),
            template_runtime: false,
            host_lock: false,
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

/// The ordered blocking steps: every crate's tests, rustdoc for `packages`,
/// and the coverage steps when requested.
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
    // CI's `verify workspace` runs every crate's tests, not only the crates a
    // change touches: a change can break another crate's tests, such as the
    // trusted-reference differential in `rumoca-reference`. The same excludes
    // and features apply; only `commit_messages` is skipped, since it reads
    // git history that an archived snapshot lacks.
    let mut workspace = vec!["test", "-j", "8", "--workspace"];
    workspace.extend_from_slice(crate::test_cmd::WORKSPACE_TEST_EXCLUDES);
    workspace.extend_from_slice(crate::test_cmd::WORKSPACE_TEST_FEATURES);
    workspace.extend_from_slice(&SKIP_SNAPSHOT_ONLY_TESTS);
    steps.push(GateStep::cargo("workspace-tests", &workspace));
    steps.push(GateStep::cargo(
        "msl-harness-tests",
        crate::test_cmd::MSL_HARNESS_UNIT_TEST_ARGS,
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

/// The test-binary arguments that skip the one test an archived snapshot
/// cannot run: `commit_messages` reads git history.
const SKIP_SNAPSHOT_ONLY_TESTS: [&str; 3] = ["--", "--skip", "commit_messages"];

fn coverage_steps() -> Vec<GateStep> {
    // CI's `coverage run` fails on a failing test; a measurement that ignored
    // run failures would pass a broken tree.
    let mut run = vec![
        "llvm-cov",
        "--workspace",
        "--tests",
        "--json",
        "--output-path",
        "target/llvm-cov/workspace-full.json",
    ];
    run.extend_from_slice(&SKIP_SNAPSHOT_ONLY_TESTS);
    let steps = vec![
        GateStep::cargo("coverage-run", &run),
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
    ];
    steps
        .into_iter()
        .map(|step| GateStep {
            host_lock: true,
            ..step
        })
        .collect()
}

/// Take the exclusive host-wide lock at `path`, waiting for any other holder;
/// the lock is released when the returned file is dropped.
pub(crate) fn lock_host(path: &Path) -> Result<fs::File> {
    let file = fs::OpenOptions::new()
        .create(true)
        .truncate(false)
        .write(true)
        .open(path)
        .context(format!("open {}", path.display()))?;
    file.lock().context(format!("lock {}", path.display()))?;
    Ok(file)
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
    let repo = match &args.worktree {
        Some(worktree) => worktree.clone(),
        None => root.to_path_buf(),
    };
    let rev = resolve_rev(&repo, args.rev.as_deref().unwrap_or("HEAD"))?;
    let short = &rev[..rev.len().min(12)];
    let base = std::env::temp_dir().join("rumoca-gate").join(short);
    let snapshot = base.join("snap");
    let target = base.join("target");
    let log_path = base.join("gate.log");
    extract_snapshot(&repo, &rev, &snapshot)?;
    link_shared_caches(root, &snapshot)?;
    if args.coverage {
        create_coverage_output(&snapshot)?;
    }
    let packages = if args.crates.is_empty() {
        changed_packages(&repo, &upstream_main(&repo, UPSTREAM), &rev, &snapshot)
    } else {
        args.crates.clone()
    };
    let mut log = fs::File::create(&log_path).context(format!("create {}", log_path.display()))?;
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
    if let Some(step) = failed {
        println!("GATE_FAILED {rev} ({})", log_path.display());
        println!("failed step: {step}");
        bail!("gate failed at `{step}`");
    }
    println!("GATE_OK {rev} ({})", log_path.display());
    if !args.keep {
        fs::remove_dir_all(&target).ok();
    }
    Ok(())
}

fn git(repo: &Path) -> Command {
    let mut command = Command::new("git");
    command.arg("-C").arg(repo);
    command
}

pub(crate) fn resolve_rev(repo: &Path, rev: &str) -> Result<String> {
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
pub(crate) fn extract_snapshot(repo: &Path, rev: &str, snapshot: &Path) -> Result<()> {
    if snapshot.exists() {
        fs::remove_dir_all(snapshot).context(format!("clear {}", snapshot.display()))?;
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
    let mut zip = zip::ZipArchive::new(fs::File::open(&archive)?)
        .context(format!("open {}", archive.display()))?;
    zip.extract(snapshot)
        .context(format!("extract {}", archive.display()))?;
    fs::remove_file(&archive).ok();
    Ok(())
}

/// Link the repository's MSL and FMI conformance caches into the snapshot,
/// and create the `target/llvm-cov` directory the coverage steps write their
/// reports into.
pub(crate) fn link_shared_caches(root: &Path, snapshot: &Path) -> Result<()> {
    let target = snapshot.join("target");
    fs::create_dir_all(target.join("llvm-cov"))?;
    for cache in ["msl", "fmi-conformance"] {
        let source = root.join("target").join(cache);
        if source.exists() {
            link_dir(&source, &target.join(cache)).context(format!("link {}", source.display()))?;
        }
    }
    Ok(())
}

/// Create the snapshot's `target/llvm-cov`, where the coverage steps write
/// their reports: `cargo llvm-cov --output-path` does not create the parent
/// directory, and the gate's Cargo target directory lies outside the snapshot.
/// `xtask coverage run` creates the same directory before it measures.
pub(crate) fn create_coverage_output(snapshot: &Path) -> Result<()> {
    let output = snapshot.join(COVERAGE_OUTPUT_DIR);
    fs::create_dir_all(&output).context(format!("create {}", output.display()))
}

#[cfg(unix)]
fn link_dir(source: &Path, link: &Path) -> std::io::Result<()> {
    std::os::unix::fs::symlink(source, link)
}

#[cfg(windows)]
fn link_dir(source: &Path, link: &Path) -> std::io::Result<()> {
    std::os::windows::fs::symlink_dir(source, link)
}

/// The commit `main` of `remote` names, or `origin/main` when it cannot be
/// read.
pub(crate) fn upstream_main(repo: &Path, remote: &str) -> String {
    let fallback = "origin/main".to_string();
    let Ok(output) = git(repo)
        .args(["ls-remote", "-q", remote, "refs/heads/main"])
        .output()
    else {
        return fallback;
    };
    let text = String::from_utf8_lossy(&output.stdout);
    match text.split_whitespace().next() {
        Some(sha) if output.status.success() => sha.to_string(),
        _ => fallback,
    }
}

/// Packages under `crates/` changed between `base` and `rev` that the
/// snapshot contains.
pub(crate) fn changed_packages(repo: &Path, base: &str, rev: &str, snapshot: &Path) -> Vec<String> {
    let diff = match git(repo).args(["diff", "--name-only", base, rev]).output() {
        Ok(output) => String::from_utf8_lossy(&output.stdout).into_owned(),
        Err(_) => String::new(),
    };
    let mut packages = changed_crates(&diff);
    packages.retain(|name| {
        snapshot
            .join("crates")
            .join(name)
            .join("Cargo.toml")
            .is_file()
    });
    packages
}

/// Run each step in order; the name of the first failing step, if any.
pub(crate) fn run_steps(
    steps: &[GateStep],
    snapshot: &Path,
    target: &Path,
    log: &mut fs::File,
) -> Result<Option<&'static str>> {
    let mut host_lock = None;
    for step in steps {
        if step.host_lock && host_lock.is_none() {
            writeln!(log, "### waiting for {COVERAGE_LOCK}")?;
            host_lock = Some(lock_host(Path::new(COVERAGE_LOCK))?);
        }
        println!("### {}", step.name);
        writeln!(log, "### {}", step.name)?;
        let mut command = Command::new(step.program);
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
            .context(format!("run gate step `{}`", step.name))?;
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
        let verdict = if passed { "ok" } else { "FAILED" };
        writeln!(log, "### {} {verdict}", step.name)?;
        if !passed {
            return Ok(Some(step.name));
        }
    }
    Ok(None)
}
