//! Host-level MSL parity sweep controls: a lock that keeps two sweeps from
//! overlapping on one host, isolated reruns of the models a loaded sweep timed
//! out on, and the band-table diff against a reference run.

use anyhow::{Context, Result, bail};
use clap::{Args, Subcommand};
use serde_json::Value;
use std::collections::BTreeMap;
use std::fs;
use std::path::{Path, PathBuf};

/// The band table a parity run writes into its results directory.
pub(crate) const BAND_TABLE: &str = "msl_band_table.json";
/// Subdirectory of the results directory holding isolated reruns.
const RERUN_DIR: &str = "rerun";
/// The band table with isolated reruns replacing their sweep rows.
const MERGED_TABLE: &str = "merged_band_table.json";

#[derive(Debug, Subcommand, Clone, PartialEq, Eq)]
pub(crate) enum MslParityAction {
    /// Compare a band table with a reference: strict-high counts and the rows
    /// lost, gained, or otherwise changed
    Diff(MslParityDiffArgs),
}

#[derive(Debug, Args, Clone, PartialEq, Eq)]
pub(crate) struct MslParityDiffArgs {
    /// Band table under test (`msl_band_table.json` or a merged table)
    pub(crate) table: PathBuf,
    /// Reference band table
    pub(crate) reference: PathBuf,
}

/// An exclusive host-wide lock held for the life of one parity sweep.
pub(crate) struct SweepLock {
    _file: fs::File,
}

impl SweepLock {
    pub(crate) fn acquire() -> Result<Self> {
        let path = std::env::temp_dir().join("rumoca-msl-parity.lock");
        let file = fs::OpenOptions::new()
            .create(true)
            .truncate(false)
            .write(true)
            .open(&path)
            .with_context(|| format!("open {}", path.display()))?;
        println!("waiting for the parity sweep lock {}", path.display());
        file.lock()
            .with_context(|| format!("lock {}", path.display()))?;
        Ok(Self { _file: file })
    }
}

/// A row that reached `high` with no channel deviation.
pub(crate) fn strict_high(row: &Value) -> bool {
    row["band"] == "high" && row["channel_deviation_count"].as_u64().unwrap_or(0) == 0
}

/// Models of a band table that missed `high` because a wall budget ran out,
/// which a loaded host can cause and an isolated rerun can settle.
pub(crate) fn timed_out_models(table: &Value) -> Vec<String> {
    rows(table)
        .filter(|row| row["band"] != "high")
        .filter(|row| {
            let text = row.to_string().to_ascii_lowercase();
            text.contains("timeout") || (text.contains("exceeded") && text.contains("budget"))
        })
        .filter_map(|row| row["model_name"].as_str().map(str::to_string))
        .collect()
}

fn rows(table: &Value) -> impl Iterator<Item = &Value> {
    table["rows"].as_array().into_iter().flatten()
}

fn model_name(row: &Value) -> &str {
    row["model_name"].as_str().unwrap_or_default()
}

/// `table` with each rerun row replacing the sweep row of its model.
pub(crate) fn merge_reruns(table: &Value, reruns: &[Value]) -> Value {
    let mut by_model = rows(table)
        .map(|row| (model_name(row).to_string(), row.clone()))
        .collect::<BTreeMap<_, _>>();
    for row in reruns.iter().flat_map(rows) {
        let mut row = row.clone();
        row["rerun_alone"] = Value::Bool(true);
        by_model.insert(model_name(&row).to_string(), row);
    }
    let mut merged = table.clone();
    merged["rows"] = Value::Array(by_model.into_values().collect());
    merged
}

/// The strict-high summary of `table` and, against `reference`, the rows lost,
/// gained, and otherwise changed.
pub(crate) fn diff_report(table: &Value, reference: &Value) -> String {
    let count = |table: &Value| rows(table).filter(|row| strict_high(row)).count();
    let total = |table: &Value| rows(table).count();
    let mut report = format!(
        "strict-high {} / {} rows\nreference strict-high {} / {}\n",
        count(table),
        total(table),
        count(reference),
        total(reference)
    );
    let current = rows(table)
        .map(|row| (model_name(row), row))
        .collect::<BTreeMap<_, _>>();
    let previous = rows(reference)
        .map(|row| (model_name(row), row))
        .collect::<BTreeMap<_, _>>();
    let (mut lost, mut gained, mut changed) = (Vec::new(), Vec::new(), Vec::new());
    for (&name, &before) in &previous {
        let Some(&after) = current.get(name) else {
            changed.push(format!("{name}: {} -> MISSING", text(before, "band")));
            continue;
        };
        if strict_high(before) && !strict_high(after) {
            let detail = text(after, "exit_detail");
            lost.push(format!(
                "{name}: {}/{} {}",
                text(after, "band"),
                text(after, "exit_reason"),
                detail.chars().take(120).collect::<String>()
            ));
        } else if !strict_high(before) && strict_high(after) {
            gained.push(name.to_string());
        } else if before["band"] != after["band"] || before["exit_reason"] != after["exit_reason"] {
            changed.push(format!(
                "{name}: {}/{} -> {}/{}",
                text(before, "band"),
                text(before, "exit_reason"),
                text(after, "band"),
                text(after, "exit_reason")
            ));
        }
    }
    for (&name, &after) in &current {
        if !previous.contains_key(name) {
            changed.push(format!("{name}: MISSING -> {}", text(after, "band")));
        }
    }
    report.push_str(&format!("lost {}\n", lost.len()));
    for line in &lost {
        report.push_str(&format!("  {line}\n"));
    }
    report.push_str(&format!("gained {}: {}\n", gained.len(), gained.join(", ")));
    report.push_str(&format!("other changes {}\n", changed.len()));
    for line in &changed {
        report.push_str(&format!("  {line}\n"));
    }
    report
}

fn text<'a>(row: &'a Value, field: &str) -> &'a str {
    row[field].as_str().unwrap_or_default()
}

/// Run `verify msl-parity`: the diff action, or the gate under the optional
/// host lock followed by the optional isolated reruns of timeouts.
pub(crate) fn run(root: &Path, args: &super::VerifyMslParityArgs) -> Result<()> {
    if let Some(MslParityAction::Diff(diff)) = &args.action {
        return run_diff(diff);
    }
    let _lock = args.serialize.then(SweepLock::acquire).transpose()?;
    let result = super::run_msl_quality_gate(root, args);
    if let Some(secs) = args.rerun_timeouts_alone {
        rerun_timeouts_alone(root, args, secs)?;
    }
    result
}

/// Rerun every timed-out model of the finished sweep alone, then merge.
fn rerun_timeouts_alone(root: &Path, args: &super::VerifyMslParityArgs, secs: u64) -> Result<()> {
    let results = args
        .results_dir
        .clone()
        .unwrap_or_else(|| root.join("target/msl/results"));
    let Some(table) = find_band_table(&results) else {
        bail!("no {BAND_TABLE} under {}", results.display());
    };
    let models = timed_out_models(&load(&table)?);
    for model in &models {
        println!("rerun alone: {model}");
        let rerun = isolated_rerun_args(args, &results, model, secs);
        if let Err(error) = super::run_msl_quality_gate(root, &rerun) {
            eprintln!("rerun of {model}: {error:#}");
        }
    }
    write_merged_table(&results, &models)
}

/// The arguments of one model's isolated rerun with a `secs` wall budget.
pub(crate) fn isolated_rerun_args(
    args: &super::VerifyMslParityArgs,
    results: &Path,
    model: &str,
    secs: u64,
) -> super::VerifyMslParityArgs {
    let mut rerun = args.clone();
    rerun.results_dir = Some(rerun_results_dir(results, model));
    rerun.clean_results = true;
    rerun.sim_match = vec![model.to_string()];
    rerun.sim_match_exact = true;
    rerun.stage_parallelism = Some(1);
    rerun.sim_parallelism = Some(1);
    rerun.budgets.set_wall_budgets(secs);
    rerun.serialize = false;
    rerun.rerun_timeouts_alone = None;
    rerun
}

pub(crate) fn run_diff(args: &MslParityDiffArgs) -> Result<()> {
    let table = load(&args.table)?;
    let reference = load(&args.reference)?;
    print!("{}", diff_report(&table, &reference));
    Ok(())
}

pub(crate) fn load(path: &Path) -> Result<Value> {
    let text = fs::read_to_string(path).with_context(|| format!("read {}", path.display()))?;
    serde_json::from_str(&text).with_context(|| format!("parse {}", path.display()))
}

/// The sweep's own band table under `results`, outside the rerun directory.
pub(crate) fn find_band_table(results: &Path) -> Option<PathBuf> {
    walkdir::WalkDir::new(results)
        .into_iter()
        .filter_entry(|entry| entry.depth() != 1 || entry.file_name() != RERUN_DIR)
        .filter_map(Result::ok)
        .find(|entry| entry.file_name() == BAND_TABLE)
        .map(walkdir::DirEntry::into_path)
}

/// The results directory of the isolated rerun of `model`.
pub(crate) fn rerun_results_dir(results: &Path, model: &str) -> PathBuf {
    results.join(RERUN_DIR).join(model)
}

/// Merge the reruns into the sweep table, write the merged table, and print
/// its strict-high count.
pub(crate) fn write_merged_table(results: &Path, models: &[String]) -> Result<()> {
    let Some(table_path) = find_band_table(results) else {
        bail!("no {BAND_TABLE} under {}", results.display());
    };
    let table = load(&table_path)?;
    let reruns = models
        .iter()
        .filter_map(|model| find_band_table(&rerun_results_dir(results, model)))
        .map(|path| load(&path))
        .collect::<Result<Vec<_>>>()?;
    let merged = merge_reruns(&table, &reruns);
    let path = results.join(MERGED_TABLE);
    fs::write(&path, serde_json::to_string_pretty(&merged)?)?;
    let strict = rows(&merged).filter(|row| strict_high(row)).count();
    println!(
        "strict-high {strict} / {} rows ({} rerun alone): {}",
        rows(&merged).count(),
        reruns.len(),
        path.display()
    );
    Ok(())
}
