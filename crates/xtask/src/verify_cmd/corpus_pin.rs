//! `cargo xtask verify corpus-pin`: the real-model half of the oracle net.
//!
//! # What this gate is for
//!
//! The random-program fuzz proves the compiler against programs nobody flies.
//! This gate proves it against the ones we do: the RDD2 flight stack out of the
//! `modelica_models` checkout, and the twenty-model MSL canary roster the MSL
//! campaign already tracks. Each row states what the compiler must do with one
//! real model, and the gate goes red on any deviation.
//!
//! # The contract
//!
//! * A row pinned to compile that stops compiling is red.
//! * A row pinned to be refused that starts compiling is *also* red. Refusal is
//!   free under the top-level theorem, so getting better is welcome, but the
//!   manifest is the reviewed record of what the compiler accepts and widening
//!   that set without review is the drift this gate exists to make visible.
//!   Turning the improvement green is a one-line manifest diff.
//! * A row pinned to be refused that is refused for a *different* reason is red:
//!   a changed refusal is a behavior change.
//! * A simulated row must reproduce every pinned reading within its tolerance,
//!   and its pins must be able to see the run move at all: a row whose every
//!   pinned variable is constant over the run is red as unobserved, because a
//!   truncated run would reproduce it exactly. [`corpus_pin::observability`]
//!   owns that judgement.
//! * A compiled row must emit every artifact it declares. Exit status alone
//!   would keep a target green while it wrote nothing. [`corpus_pin::artifacts`]
//!   owns that judgement.
//! * A row that outruns its deadline is killed and red. The gate runs inside
//!   `verify quick`, so a model that stops terminating must cost one row rather
//!   than the developer loop.
//! * A corpus that is not on the machine is red with
//!   [`roots::CORPUS_UNMEASURED_HEADLINE`], never a skip. This mirrors the
//!   `parity unmeasured` precedent exactly: a green tick that means "nothing was
//!   compared" is the one outcome an oracle net cannot survive.
//!
//! # Where the corpus root comes from
//!
//! Never from this repository. See [`roots`] for the run-time resolution order.
//!
//! # Runtime budget
//!
//! The corpus is sized to finish in about three minutes on a developer machine,
//! and the shape of that budget is the reason the pins look the way they do:
//!
//! * Roughly nine seconds of every MSL row is parsing the standard library, so
//!   the twenty canary rows cost about 210 seconds of CPU no matter how short
//!   the simulation is. They are run across several workers instead.
//! * `t_end` is 0.02 with `dt` 0.01 wherever that is enough: initialization
//!   plus a couple of reported steps, which catches a wrong initial value or a
//!   wrong first derivative, where lowering bugs show up, and keeps a flight
//!   mission that runs for minutes down to seconds. Rows whose model does
//!   nothing that early state a longer `t_end` and say why: a window in which
//!   nothing moves buys speed by measuring nothing.
//! * The flight-stack component models (navigation estimator, controller, outer
//!   loop, UKF estimator) are driven by inputs a standalone simulation cannot
//!   supply, so their established target is code generation. Those rows compile
//!   and are judged on the artifacts they emit, which costs one to three
//!   seconds each.
//!
//! The measured wall time is printed against the manifest's budget but never
//! asserted: the sibling RDD2 performance guard documents at length why a
//! wall-clock assertion on a shared machine measures load rather than the
//! compiler. The budget does derive the per-row deadline
//! ([`manifest::CorpusManifest::row_deadline`]), which is a different claim: not
//! "this row was fast enough" but "this row still terminates".

mod artifacts;
mod execution;
mod manifest;
mod observability;
mod record;
mod roots;
#[cfg(test)]
mod tests;
mod trace;
mod verdict;

use anyhow::{Context, Result, bail, ensure};
use clap::Args;
use serde::Serialize;
use std::fs;
use std::path::{Path, PathBuf};
use std::sync::Mutex;
use std::sync::atomic::{AtomicUsize, Ordering};
use std::time::{Duration, Instant};

use crate::run_status;
use execution::{ModelRun, RunContext};
use manifest::{CorpusEntry, CorpusManifest};
use roots::CorpusRoots;
use verdict::Verdict;

/// Build profile for the compiler the gate measures. Release-quality runtime
/// without the release link, and already the profile the MSL campaign builds.
const RUMOCA_BUILD_PROFILE: &str = "msl-fast";

const ARTIFACT_DIR: &str = "target/verification/corpus-pin";
const SUMMARY_PATH: &str = "target/verification/corpus-pin-summary.json";
const PROPOSAL_PATH: &str = "target/verification/corpus-pin-proposed.json";
const LOCK_PATH: &str = "target/verification/corpus-pin.lock";

/// Ceiling on concurrent compiler processes. Each one holds a whole standard
/// library in memory; more workers than this trades wall time for swap.
const MAX_JOBS: usize = 6;

#[derive(Debug, Args, Clone, PartialEq, Eq, Default)]
pub(crate) struct VerifyCorpusPinArgs {
    /// Checkout of the out-of-tree flight-model library. Highest-priority
    /// source for the corpus root; see the module docs for the full order.
    #[arg(long, value_name = "PATH")]
    models_root: Option<PathBuf>,
    /// Modelica Standard Library release root, when it is not the cached one.
    #[arg(long, value_name = "PATH")]
    msl_root: Option<PathBuf>,
    /// Manifest to gate against (default: the checked-in corpus pin).
    #[arg(long, value_name = "PATH")]
    manifest: Option<PathBuf>,
    /// Compiler binary to measure. Default: build it under the `msl-fast`
    /// profile and use that.
    #[arg(long, value_name = "PATH")]
    rumoca_binary: Option<PathBuf>,
    /// Concurrent compiler processes (default: host-derived, capped).
    #[arg(long, value_name = "N")]
    jobs: Option<usize>,
    /// Also write a proposed manifest carrying what this run measured. The
    /// checked-in manifest is never touched, and the run still fails on every
    /// deviation: a proposal is an input to review, not a way to go green.
    #[arg(long)]
    record: bool,
    /// Gate only the rows whose id is listed (repeatable). The full corpus runs
    /// when this is absent. Root resolution follows the selection, so a focused
    /// MSL run does not need the flight checkout.
    #[arg(long = "only", value_name = "ID")]
    only: Vec<String>,
}

pub(super) fn run(root: &Path, args: &VerifyCorpusPinArgs) -> Result<()> {
    let manifest_path = args
        .manifest
        .clone()
        .unwrap_or_else(|| manifest::manifest_path(root));
    let manifest = manifest::load(&manifest_path)?;
    let _lock = RunLock::acquire(root)?;
    // Resolve roots against exactly the rows that will run. A focused
    // `--only msl/...` run then needs only the MSL release, while the suite's
    // unfiltered run still demands both corpora.
    let selected = selected_manifest(&manifest, &args.only)?;
    let roots = resolve_roots(root, &selected, args)?;
    roots::ensure_usable(&selected, &roots)?;
    let rumoca = resolve_compiler(root, args)?;
    let entries: Vec<&CorpusEntry> = selected.entries.iter().collect();
    // Read from the whole manifest, never the selection: a `--only` run must
    // not hand its one row the budget of all twenty-eight.
    let deadline = manifest.row_deadline();

    println!(
        "corpus pin: {} row(s) from {}",
        entries.len(),
        manifest_path.display()
    );
    // Only the corpora that actually feed a selected row, so the header never
    // names a root nothing was read from.
    for corpus in [manifest::Corpus::FlightModels, manifest::Corpus::Msl] {
        if entries.iter().any(|entry| entry.corpus == corpus) {
            println!("  {}: {}", corpus.label(), roots.root_for(corpus).display());
        }
    }
    println!("  compiler: {}", rumoca.display());
    println!("  row deadline: {:.0}s", deadline.as_secs_f64());

    let artifact_dir = prepare_artifact_dir(root)?;
    let started = Instant::now();
    let runs = execute(
        &entries,
        &roots,
        &rumoca,
        &artifact_dir,
        Schedule {
            jobs: jobs(args),
            deadline,
        },
    )?;
    let wall_seconds = started.elapsed().as_secs_f64();

    let verdicts: Vec<Verdict> = entries
        .iter()
        .zip(&runs)
        .map(|(entry, run)| verdict::judge(entry, run))
        .collect();
    if args.record {
        write_proposal(root, &manifest, &entries, &runs)?;
    }
    report(root, &manifest, &entries, &verdicts, wall_seconds)
}

fn resolve_roots(
    root: &Path,
    manifest: &CorpusManifest,
    args: &VerifyCorpusPinArgs,
) -> Result<CorpusRoots> {
    let config = roots::read_config(root)?;
    // argv beats the config file beats the cache. Only fall through to the
    // cache lookup when neither named a root, so a run that was handed one
    // never triggers a download it would then discard.
    let named = args
        .msl_root
        .clone()
        .or_else(|| config.as_ref().and_then(|config| config.msl_root.clone()));
    let msl_root = match named {
        Some(path) => path,
        None => super::cached_msl_source_root(root)?,
    };
    roots::resolve(
        root,
        manifest,
        args.models_root.as_deref(),
        msl_root,
        config.as_ref(),
    )
}

/// The manifest narrowed to the requested rows, keeping manifest order.
fn selected_manifest(manifest: &CorpusManifest, only: &[String]) -> Result<CorpusManifest> {
    if only.is_empty() {
        return Ok(manifest.clone());
    }
    let entries: Vec<CorpusEntry> = manifest
        .entries
        .iter()
        .filter(|entry| only.contains(&entry.id))
        .cloned()
        .collect();
    for id in only {
        ensure!(
            entries.iter().any(|entry| &entry.id == id),
            "no corpus row has id `{id}`"
        );
    }
    Ok(CorpusManifest {
        schema_version: manifest.schema_version,
        runtime_budget_seconds: manifest.runtime_budget_seconds,
        entries,
    })
}

fn jobs(args: &VerifyCorpusPinArgs) -> usize {
    args.jobs
        .unwrap_or_else(|| num_cpus::get().saturating_sub(1))
        .clamp(1, MAX_JOBS)
}

/// Clear and recreate the per-run artifact directory.
///
/// The compiler's on-disk cache lives inside it, so every run starts cold. That
/// costs about nine seconds per MSL row and is worth paying: a gate whose
/// evidence could come from a cache entry written by a different compiler build
/// is not evidence about this one.
fn prepare_artifact_dir(root: &Path) -> Result<PathBuf> {
    let dir = root.join(ARTIFACT_DIR).join("artifacts");
    if dir.exists() {
        fs::remove_dir_all(&dir).with_context(|| format!("failed to clear {}", dir.display()))?;
    }
    fs::create_dir_all(&dir).with_context(|| format!("failed to create {}", dir.display()))?;
    Ok(dir)
}

/// How the run queue is worked: how many rows at a time, and how long each of
/// them may take.
#[derive(Clone, Copy)]
struct Schedule {
    jobs: usize,
    deadline: Duration,
}

/// One worker's shared view of the run queue.
struct Worker<'a> {
    entries: &'a [&'a CorpusEntry],
    roots: &'a CorpusRoots,
    rumoca: &'a Path,
    artifact_dir: &'a Path,
    cache_dir: &'a Path,
    deadline: Duration,
    next: &'a AtomicUsize,
    slots: &'a [Mutex<Option<Result<ModelRun>>>],
}

impl Worker<'_> {
    /// Take rows until the queue is empty. Each row's result lands in its own
    /// slot, so the report keeps manifest order however the workers interleave.
    fn drain(&self) {
        while let Some(index) = self.claim() {
            let entry = self.entries[index];
            let context = RunContext {
                rumoca: self.rumoca,
                corpus_root: self.roots.root_for(entry.corpus),
                artifact_dir: self.artifact_dir,
                cache_dir: self.cache_dir,
                deadline: self.deadline,
            };
            let outcome = execution::run_entry(entry, &context);
            *self.slots[index].lock().expect("corpus slot mutex") = Some(outcome);
        }
    }

    fn claim(&self) -> Option<usize> {
        let index = self.next.fetch_add(1, Ordering::Relaxed);
        (index < self.entries.len()).then_some(index)
    }
}

/// Run every selected row, at most `schedule.jobs` compiler processes at a
/// time, each under `schedule.deadline`.
fn execute(
    entries: &[&CorpusEntry],
    roots: &CorpusRoots,
    rumoca: &Path,
    artifact_dir: &Path,
    schedule: Schedule,
) -> Result<Vec<ModelRun>> {
    let cache_dir = artifact_dir.join("cache");
    fs::create_dir_all(&cache_dir)
        .with_context(|| format!("failed to create {}", cache_dir.display()))?;
    let slots: Vec<Mutex<Option<Result<ModelRun>>>> =
        entries.iter().map(|_| Mutex::new(None)).collect();
    let next = AtomicUsize::new(0);
    let worker = Worker {
        entries,
        roots,
        rumoca,
        artifact_dir,
        cache_dir: &cache_dir,
        deadline: schedule.deadline,
        next: &next,
        slots: &slots,
    };
    std::thread::scope(|scope| {
        for _ in 0..schedule.jobs.min(entries.len().max(1)) {
            scope.spawn(|| worker.drain());
        }
    });
    slots
        .into_iter()
        .enumerate()
        .map(|(index, slot)| {
            slot.into_inner()
                .expect("corpus slot mutex")
                .unwrap_or_else(|| bail!("corpus row `{}` was never run", entries[index].id))
        })
        .collect()
}

fn write_proposal(
    root: &Path,
    manifest: &CorpusManifest,
    entries: &[&CorpusEntry],
    runs: &[ModelRun],
) -> Result<()> {
    let mut proposed = manifest.clone();
    let mut notes: Vec<(String, String)> = Vec::new();
    for (entry, run) in entries.iter().zip(runs) {
        let proposal = record::propose_entry(entry, run);
        if let Some(note) = proposal.note {
            notes.push((entry.id.clone(), note));
        }
        if let Some(slot) = proposed
            .entries
            .iter_mut()
            .find(|candidate| candidate.id == entry.id)
        {
            *slot = proposal.entry;
        }
    }
    let path = root.join(PROPOSAL_PATH);
    record::write_proposal(&path, &proposed)?;
    println!(
        "corpus pin: proposal written to {}\n  Diff it against {} and copy across what you \
         have adjudicated. The gate never edits its own expectations.",
        path.display(),
        manifest::MANIFEST_PATH
    );
    if notes.is_empty() {
        return Ok(());
    }
    println!("  The proposal deliberately left these rows unfilled:");
    for (id, note) in &notes {
        println!("    {id}\n      {note}");
    }
    Ok(())
}

#[derive(Serialize)]
struct SummaryRow {
    id: String,
    passed: bool,
    elapsed_seconds: f64,
    command: String,
    findings: Vec<String>,
}

#[derive(Serialize)]
struct Summary {
    rows: usize,
    failed: usize,
    wall_seconds: f64,
    budget_seconds: f64,
    row_deadline_seconds: f64,
    entries: Vec<SummaryRow>,
}

fn report(
    root: &Path,
    manifest: &CorpusManifest,
    entries: &[&CorpusEntry],
    verdicts: &[Verdict],
    wall_seconds: f64,
) -> Result<()> {
    for (entry, verdict) in entries.iter().zip(verdicts) {
        println!(
            "  {} {:<44} {:>6.1}s  {}",
            if verdict.passed() { "pass" } else { "FAIL" },
            verdict.id,
            verdict.elapsed_seconds,
            entry.check.label()
        );
    }
    let failed: Vec<&Verdict> = verdicts.iter().filter(|v| !v.passed()).collect();
    write_summary(root, manifest, verdicts, wall_seconds)?;
    println!(
        "corpus pin: {} row(s), {} failed, {wall_seconds:.1}s wall (budget {:.0}s)",
        verdicts.len(),
        failed.len(),
        manifest.runtime_budget_seconds
    );
    if failed.is_empty() {
        return Ok(());
    }
    let mut report = String::from("the pinned corpus does not behave as recorded:\n");
    for verdict in &failed {
        report.push_str(&format!("\n  {}\n", verdict.id));
        for finding in &verdict.findings {
            report.push_str(&format!("    {finding}\n"));
        }
        report.push_str(&format!("    reproduce: {}\n", verdict.command_line));
    }
    bail!("{report}");
}

fn write_summary(
    root: &Path,
    manifest: &CorpusManifest,
    verdicts: &[Verdict],
    wall_seconds: f64,
) -> Result<()> {
    let summary = Summary {
        rows: verdicts.len(),
        failed: verdicts.iter().filter(|v| !v.passed()).count(),
        wall_seconds,
        budget_seconds: manifest.runtime_budget_seconds,
        row_deadline_seconds: manifest.row_deadline().as_secs_f64(),
        entries: verdicts
            .iter()
            .map(|verdict| SummaryRow {
                id: verdict.id.clone(),
                passed: verdict.passed(),
                elapsed_seconds: verdict.elapsed_seconds,
                command: verdict.command_line.clone(),
                findings: verdict.findings.clone(),
            })
            .collect(),
    };
    let path = root.join(SUMMARY_PATH);
    if let Some(parent) = path.parent() {
        fs::create_dir_all(parent)?;
    }
    fs::write(
        &path,
        format!("{}\n", serde_json::to_string_pretty(&summary)?),
    )
    .with_context(|| format!("failed to write {}", path.display()))?;
    Ok(())
}

/// Build (or accept) the compiler binary the gate measures.
fn resolve_compiler(root: &Path, args: &VerifyCorpusPinArgs) -> Result<PathBuf> {
    if let Some(path) = &args.rumoca_binary {
        ensure!(
            path.is_file(),
            "--rumoca-binary {} is not a file",
            path.display()
        );
        return Ok(path.clone());
    }
    let mut command = std::process::Command::new("cargo");
    command
        .arg("build")
        .arg("--profile")
        .arg(RUMOCA_BUILD_PROFILE)
        .arg("--package")
        .arg("rumoca")
        .arg("--bin")
        .arg("rumoca")
        .current_dir(root);
    run_status(command)?;
    let path = root
        .join("target")
        .join(RUMOCA_BUILD_PROFILE)
        .join("rumoca");
    ensure!(
        path.is_file(),
        "the `{RUMOCA_BUILD_PROFILE}` build did not produce {}",
        path.display()
    );
    Ok(path)
}

/// Exclusive ownership of the gate's artifact and cache directories.
///
/// Two concurrent runs would share one compiler cache and one artifact
/// directory, and the second one's `prepare_artifact_dir` would delete traces
/// the first is still reading back. Waiting is the correct behavior.
struct RunLock {
    _file: fs::File,
}

impl RunLock {
    fn acquire(root: &Path) -> Result<Self> {
        let path = root.join(LOCK_PATH);
        if let Some(parent) = path.parent() {
            fs::create_dir_all(parent)
                .with_context(|| format!("failed to create {}", parent.display()))?;
        }
        let file = fs::OpenOptions::new()
            .read(true)
            .write(true)
            .create(true)
            .truncate(false)
            .open(&path)
            .with_context(|| format!("failed to open {}", path.display()))?;
        file.lock()
            .with_context(|| format!("failed to lock {}", path.display()))?;
        Ok(Self { _file: file })
    }
}
