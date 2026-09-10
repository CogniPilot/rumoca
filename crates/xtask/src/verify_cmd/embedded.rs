//! `cargo xtask verify embedded`: the gate that keeps the flight artifacts
//! flyable.
//!
//! # What this gate is for
//!
//! The sibling corpus gate proves the flight models still compile. This one
//! proves the C that comes out still fits on the microcontroller it flies on,
//! and still runs in the time the block declares. Those are different
//! failures: a projection change can keep every row of the corpus pin green
//! while doubling the scratch struct, while letting one `double` back into an
//! inner loop, or while turning a contracted product back into a quartic nest,
//! and none of them shows up as a compile error. Size, precision and
//! arithmetic count are acceptance criteria for this target, so they are gated
//! like any other correctness property.
//!
//! # The contract, per row
//!
//! 1. The model compiles to C with the workspace compiler this gate builds.
//! 2. Every emitted `.c` cross-compiles at `-Os` for Cortex-M7 hard float with
//!    the measured baseline's exact flags, and **without a single warning**:
//!    warning-free compilation is one of the artifact's advertised properties.
//! 3. Summed `.text` is at or under the manifest's ceiling.
//! 4. `sizeof(<Model>State)` for the target ABI is at or under its ceiling,
//!    measured by a probe translation unit rather than computed.
//! 5. No object leaves a forbidden symbol undefined: no allocator, no
//!    `__aeabi_d*` soft-float helper, no double-precision libm entry point.
//!    Any of those means the artifact stopped being the statically allocated
//!    single-precision thing it claims to be.
//! 6. The single-precision arithmetic instructions in the objects, counted from
//!    the disassembly, are at or under their ceiling. Bytes say whether the
//!    artifact fits; this number is what says whether the step makes its rate,
//!    and it is the one the step-rate work moves. The counted instruction set
//!    is defined once, in [`fp_ops`].
//!
//! # Fail-closed
//!
//! A row that cannot be measured is red, naming the row and the command that
//! failed. Compile refusal, a missing toolchain, an unreadable probe: all of
//! them are failures, none of them is a skip. A green tick that meant "nothing
//! was weighed" is the one outcome a budget gate cannot survive, and it is
//! exactly the outcome an operator would read as "still fits".
//!
//! # Where the inputs come from
//!
//! Both roots are named on argv and neither has a fallback. The models root is
//! an out-of-tree checkout, so a path committed here would be wrong for
//! everyone but its author. The toolchain root has a sharper reason: a size
//! ceiling is a statement about one compiler, and a gate that silently used a
//! different `arm-none-eabi-gcc` would compare this run's bytes against a
//! ceiling measured elsewhere and call the difference a regression.

mod cross;
mod emit;
mod fp_ops;
mod manifest;
mod symbols;
#[cfg(test)]
mod tests;
mod toolchain;
mod verdict;

use anyhow::{Context, Result, bail, ensure};
use clap::Args;
use serde::Serialize;
use std::fs;
use std::path::{Path, PathBuf};
use std::time::Instant;

use crate::run_status;
use cross::Measurement;
use emit::{EmitContext, artifact_stem};
use manifest::{BudgetEntry, BudgetManifest};
use toolchain::ArmToolchain;
use verdict::Verdict;

const ARTIFACT_DIR: &str = "target/verification/embedded-budget";
const SUMMARY_PATH: &str = "target/verification/embedded-budget-summary.json";
const LOCK_PATH: &str = "target/verification/embedded-budget.lock";

#[derive(Debug, Args, Clone, PartialEq, Eq)]
pub(crate) struct VerifyEmbeddedArgs {
    /// Checkout of the out-of-tree flight-model library. Required and
    /// authoritative: the gate never substitutes a fallback for a named root.
    #[arg(long, value_name = "PATH")]
    models_root: PathBuf,
    /// Root of the ARM cross toolchain, the directory whose `bin/` holds
    /// `arm-none-eabi-gcc`, `-size`, `-nm`, and `-objdump`. Required and
    /// authoritative: a ceiling is a statement about one compiler, so the gate
    /// refuses rather than measuring with whichever one happens to be on
    /// `PATH`.
    #[arg(long, value_name = "PATH")]
    arm_toolchain: PathBuf,
    /// Manifest to gate against (default: the checked-in embedded budget).
    #[arg(long, value_name = "PATH")]
    manifest: Option<PathBuf>,
    /// Compiler binary to measure. Default: build the workspace `rumoca` and
    /// use that.
    #[arg(long, value_name = "PATH")]
    rumoca_binary: Option<PathBuf>,
    /// Gate only the rows whose id is listed (repeatable). The full budget runs
    /// when this is absent.
    #[arg(long = "only", value_name = "ID")]
    only: Vec<String>,
}

pub(super) fn run(root: &Path, args: &VerifyEmbeddedArgs) -> Result<()> {
    let manifest_path = args
        .manifest
        .clone()
        .unwrap_or_else(|| manifest::manifest_path(root));
    let manifest = manifest::load(&manifest_path)?;
    let entries = selected_entries(&manifest, &args.only)?;
    let _lock = RunLock::acquire(root)?;
    let toolchain = ArmToolchain::resolve(&args.arm_toolchain)?;
    ensure_models_root(&args.models_root, &entries)?;
    let rumoca = resolve_compiler(root, args)?;

    println!(
        "embedded budget: {} row(s) from {}",
        entries.len(),
        manifest_path.display()
    );
    println!("  models:    {}", args.models_root.display());
    println!("  toolchain: {}", toolchain.root().display());
    println!("  compiler:  {}", rumoca.display());

    let artifact_dir = prepare_artifact_dir(root)?;
    let started = Instant::now();
    let verdicts: Vec<Verdict> = entries
        .iter()
        .map(|entry| {
            let context = RowContext {
                rumoca: &rumoca,
                models_root: &args.models_root,
                toolchain: &toolchain,
                artifact_dir: &artifact_dir,
            };
            measure_row(entry, &context)
        })
        .collect();
    let wall_seconds = started.elapsed().as_secs_f64();
    report(root, &entries, &verdicts, wall_seconds)
}

/// Everything one row's measurement needs.
struct RowContext<'a> {
    rumoca: &'a Path,
    models_root: &'a Path,
    toolchain: &'a ArmToolchain,
    artifact_dir: &'a Path,
}

/// Measure one row, turning any failure along the way into a red verdict rather
/// than aborting the run: an operator fixing three rows wants all three
/// findings, not the first one.
fn measure_row(entry: &BudgetEntry, context: &RowContext<'_>) -> Verdict {
    let started = Instant::now();
    match measure(entry, context) {
        Ok((measurement, reproduce)) => verdict::judge(
            entry,
            measurement.metrics,
            &measurement.undefined,
            started.elapsed().as_secs_f64(),
            reproduce,
        ),
        Err(error) => verdict::unmeasured(entry, &error, started.elapsed().as_secs_f64()),
    }
}

/// Emit, cross-compile, and weigh one row. The second half of the pair is both
/// commands the row ran, in order: an operator reproducing a failure needs the
/// compile that produced the C as well as the cross-compile that weighed it.
fn measure(entry: &BudgetEntry, context: &RowContext<'_>) -> Result<(Measurement, String)> {
    let stem = artifact_stem(&entry.id);
    let out_dir = context.artifact_dir.join(&stem).join("emitted");
    let work_dir = context.artifact_dir.join(&stem).join("objects");
    let cache_dir = context.artifact_dir.join("cache");
    fs::create_dir_all(&out_dir)
        .with_context(|| format!("failed to create {}", out_dir.display()))?;
    let emit_context = EmitContext {
        rumoca: context.rumoca,
        models_root: context.models_root,
        out_dir: &out_dir,
        cache_dir: &cache_dir,
    };
    let emission = emit::emit(
        &entry.model,
        &entry.entry_point,
        &entry.target,
        &emit_context,
    )?;
    let measurement = cross::measure(&emission, context.toolchain, &work_dir)?;
    let reproduce = format!(
        "{}\n    then: {}",
        emission.command_line, measurement.cross_command_line
    );
    Ok((measurement, reproduce))
}

/// The manifest rows the run was asked for, keeping manifest order.
fn selected_entries<'a>(
    manifest: &'a BudgetManifest,
    only: &[String],
) -> Result<Vec<&'a BudgetEntry>> {
    if only.is_empty() {
        return Ok(manifest.entries.iter().collect());
    }
    let entries: Vec<&BudgetEntry> = manifest
        .entries
        .iter()
        .filter(|entry| only.contains(&entry.id))
        .collect();
    for id in only {
        ensure!(
            entries.iter().any(|entry| &entry.id == id),
            "no embedded budget row has id `{id}`"
        );
    }
    Ok(entries)
}

/// Refuse a models root that does not carry every entry point the run needs.
///
/// "Carries every entry point" rather than "exists": a directory that happens
/// to be there but holds none of the models would otherwise be accepted and
/// then fail every row with a compiler error, reporting a code-size regression
/// where the real fact is that nothing was measured.
fn ensure_models_root(root: &Path, entries: &[&BudgetEntry]) -> Result<()> {
    // Deduplicated: two rows for the same model name the same entry point, and
    // a refusal that listed it twice would read as two separate problems.
    let missing: std::collections::BTreeSet<&str> = entries
        .iter()
        .map(|entry| entry.entry_point.as_str())
        .filter(|entry_point| !root.join(entry_point).is_file())
        .collect();
    ensure!(
        missing.is_empty(),
        "embedded budget unmeasured: the flight models are not at this root\n  --models-root \
         {}\n  missing entry points: {}\n  Point the gate at your `modelica_models` checkout. \
         This is a hard failure rather than a skip, because a green run that compiled nothing \
         would report the flight artifacts as within budget.",
        root.display(),
        missing.into_iter().collect::<Vec<_>>().join(", ")
    );
    Ok(())
}

/// Clear and recreate the per-run artifact directory, so a stale object file
/// from a previous compiler build can never be the thing that gets weighed.
fn prepare_artifact_dir(root: &Path) -> Result<PathBuf> {
    let dir = root.join(ARTIFACT_DIR);
    if dir.exists() {
        fs::remove_dir_all(&dir).with_context(|| format!("failed to clear {}", dir.display()))?;
    }
    fs::create_dir_all(&dir).with_context(|| format!("failed to create {}", dir.display()))?;
    Ok(dir)
}

/// Build (or accept) the compiler whose output is measured.
///
/// The debug profile is deliberate: this gate weighs the *emitted C*, and the
/// bytes the compiler emits do not depend on how the compiler itself was
/// optimized. Paying for a release build here would buy nothing the measurement
/// can see.
fn resolve_compiler(root: &Path, args: &VerifyEmbeddedArgs) -> Result<PathBuf> {
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
        .arg("--manifest-path")
        .arg(root.join("Cargo.toml"))
        .arg("--package")
        .arg("rumoca")
        .arg("--bin")
        .arg("rumoca");
    run_status(command)?;
    let path = root.join("target").join("debug").join("rumoca");
    ensure!(
        path.is_file(),
        "the workspace build did not produce {}",
        path.display()
    );
    Ok(path)
}

#[derive(Serialize)]
struct SummaryRow {
    id: String,
    passed: bool,
    text_bytes: Option<u64>,
    state_bytes: Option<u64>,
    /// Single-precision arithmetic instructions summed over the row's objects.
    fp_ops: Option<u64>,
    /// `.text` per emitted translation unit, so a review of a size change can
    /// see which unit moved without re-running the gate.
    unit_text_bytes: Vec<(String, u64)>,
    /// The same breakdown for the arithmetic count, so a step-rate change can
    /// be attributed to a translation unit without re-running the gate.
    unit_fp_ops: Vec<(String, u64)>,
    elapsed_seconds: f64,
    command: String,
    findings: Vec<String>,
}

#[derive(Serialize)]
struct Summary {
    rows: usize,
    failed: usize,
    wall_seconds: f64,
    entries: Vec<SummaryRow>,
}

fn report(
    root: &Path,
    entries: &[&BudgetEntry],
    verdicts: &[Verdict],
    wall_seconds: f64,
) -> Result<()> {
    for (entry, judged) in entries.iter().zip(verdicts) {
        let detail = judged.metrics.as_ref().map_or_else(
            || "not measured".to_string(),
            |metrics| verdict::headroom(&entry.budget, metrics),
        );
        println!(
            "  {} {:<44} {:>6.1}s  {detail}",
            if judged.passed() { "pass" } else { "FAIL" },
            judged.id,
            judged.elapsed_seconds,
        );
    }
    let failed: Vec<&Verdict> = verdicts.iter().filter(|judged| !judged.passed()).collect();
    write_summary(root, verdicts, wall_seconds)?;
    println!(
        "embedded budget: {} row(s), {} failed, {wall_seconds:.1}s wall",
        verdicts.len(),
        failed.len()
    );
    if failed.is_empty() {
        return Ok(());
    }
    let mut text = String::from("the flight artifacts are outside their embedded budget:\n");
    for judged in &failed {
        text.push_str(&format!("\n  {}\n", judged.id));
        for finding in &judged.findings {
            text.push_str(&format!("    {finding}\n"));
        }
        text.push_str(&format!("    reproduce: {}\n", judged.command_line));
    }
    bail!("{text}");
}

fn write_summary(root: &Path, verdicts: &[Verdict], wall_seconds: f64) -> Result<()> {
    let summary = Summary {
        rows: verdicts.len(),
        failed: verdicts.iter().filter(|judged| !judged.passed()).count(),
        wall_seconds,
        entries: verdicts
            .iter()
            .map(|judged| SummaryRow {
                id: judged.id.clone(),
                passed: judged.passed(),
                text_bytes: judged.metrics.as_ref().map(verdict::Metrics::text_bytes),
                state_bytes: judged.metrics.as_ref().map(|metrics| metrics.state_bytes),
                fp_ops: judged.metrics.as_ref().map(verdict::Metrics::fp_ops),
                unit_text_bytes: judged
                    .metrics
                    .as_ref()
                    .map(|metrics| metrics.text_units.clone())
                    .unwrap_or_default(),
                unit_fp_ops: judged
                    .metrics
                    .as_ref()
                    .map(|metrics| metrics.fp_units.clone())
                    .unwrap_or_default(),
                elapsed_seconds: judged.elapsed_seconds,
                command: judged.command_line.clone(),
                findings: judged.findings.clone(),
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

/// Exclusive ownership of the gate's artifact directory.
///
/// Two concurrent runs would share one output tree, and the second one's
/// [`prepare_artifact_dir`] would delete objects the first is still sizing.
/// Waiting is the correct behavior.
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
