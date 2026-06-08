use super::common::{MslPaths, get_git_commit, unix_timestamp_seconds, write_pretty_json};
use anyhow::{Context, Result, bail};
use clap::Args as ClapArgs;
use indexmap::{IndexMap, IndexSet};
use rumoca_sim::sim_trace_compare::{
    AgreementBand, MODEL_HIGH_MAX_DEVIATION_CHANNEL_SHARE, MODEL_HIGH_MIN_HIGH_CHANNEL_SHARE,
    MODEL_MINOR_MAX_DEVIATION_CHANNEL_SHARE, MODEL_MINOR_MIN_HIGH_PLUS_MINOR_CHANNEL_SHARE,
    ModelDeviationMetric, classify_trace_metric_channel_distribution,
};
use serde::{Deserialize, Serialize};
use serde_json::{Value, json};
use std::fs;
use std::path::{Path, PathBuf};

const MSL_RESULTS_FILE: &str = "msl_results.json";
const PACKAGE_PASS_RATES_FILE: &str = "msl_package_pass_rates.json";
const TRACE_COMPARISON_FILE: &str = "sim_trace_comparison.json";
const DEFAULT_OUTPUT_JSON: &str = "msl_transition_diff.json";
const DEFAULT_OUTPUT_MD: &str = "msl_transition_diff.md";

#[derive(Debug, Clone, ClapArgs)]
pub struct Args {
    /// Baseline results directory containing msl_results.json.
    #[arg(long)]
    before: PathBuf,
    /// Candidate results directory containing msl_results.json.
    #[arg(long)]
    after: PathBuf,
    /// Output transition diff JSON.
    #[arg(long)]
    output_json: Option<PathBuf>,
    /// Output transition diff Markdown.
    #[arg(long)]
    output_md: Option<PathBuf>,
    /// Maximum model rows per transition section in Markdown.
    #[arg(long, default_value_t = 25)]
    top: usize,
}

#[derive(Debug, Clone)]
struct InputPaths {
    repo_root: PathBuf,
    before_dir: PathBuf,
    after_dir: PathBuf,
    before_results_file: PathBuf,
    after_results_file: PathBuf,
    before_trace_file: PathBuf,
    after_trace_file: PathBuf,
    before_package_pass_rates_file: PathBuf,
    after_package_pass_rates_file: PathBuf,
    output_json: PathBuf,
    output_md: PathBuf,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
struct ModelPhaseState {
    phase_reached: String,
    sim_status: String,
    trace_band: Option<String>,
}

impl ModelPhaseState {
    fn sim_ok(&self) -> bool {
        self.sim_status == "sim_ok"
    }

    fn trace_high(&self) -> bool {
        self.trace_band.as_deref() == Some("high")
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
struct TransitionRecord {
    model_name: String,
    before: ModelPhaseState,
    after: ModelPhaseState,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
struct TransitionCounts {
    before_models: usize,
    after_models: usize,
    common_models: usize,
    new_models: usize,
    removed_models: usize,
    phase_fail_to_phase_pass: usize,
    phase_pass_to_phase_fail: usize,
    sim_solver_fail_to_sim_ok: usize,
    sim_ok_to_sim_solver_fail: usize,
    non_sim_ok_to_sim_ok: usize,
    sim_ok_to_non_sim_ok: usize,
    near_to_high_agreement: usize,
    high_to_non_high_agreement: usize,
}

#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize)]
struct PackagePhaseCounts {
    n: usize,
    parse_passed: usize,
    flatten_passed: usize,
    dae_passed: usize,
    solve_passed: usize,
    ic_passed: usize,
    sim_passed: usize,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
struct PackagePhaseDelta {
    package: String,
    before: PackagePhaseCounts,
    after: PackagePhaseCounts,
    delta: PackagePhaseCountsDelta,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
struct PackagePhaseCountsDelta {
    n: isize,
    parse_passed: isize,
    flatten_passed: isize,
    dae_passed: isize,
    solve_passed: isize,
    ic_passed: isize,
    sim_passed: isize,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
struct TransitionDiffReport {
    generated_at_unix_seconds: i64,
    git_commit: String,
    inputs: IndexMap<String, String>,
    counts: TransitionCounts,
    new_models: Vec<String>,
    removed_models: Vec<String>,
    phase_fail_to_phase_pass: Vec<TransitionRecord>,
    phase_pass_to_phase_fail: Vec<TransitionRecord>,
    sim_solver_fail_to_sim_ok: Vec<TransitionRecord>,
    sim_ok_to_sim_solver_fail: Vec<TransitionRecord>,
    non_sim_ok_to_sim_ok: Vec<TransitionRecord>,
    sim_ok_to_non_sim_ok: Vec<TransitionRecord>,
    near_to_high_agreement: Vec<TransitionRecord>,
    high_to_non_high_agreement: Vec<TransitionRecord>,
    package_phase_deltas: Vec<PackagePhaseDelta>,
}

pub fn run(args: Args) -> Result<()> {
    let top = args.top;
    let paths = resolve_input_paths(args)?;
    let report = build_report_from_paths(&paths)?;
    write_outputs(&paths, &report, top)?;
    print_summary(&paths, &report);
    Ok(())
}

fn resolve_input_paths(args: Args) -> Result<InputPaths> {
    let mut msl_paths = MslPaths::current();
    msl_paths.repo_root = msl_paths
        .repo_root
        .canonicalize()
        .with_context(|| format!("failed to canonicalize {}", msl_paths.repo_root.display()))?;
    let before_dir = resolve_path(&msl_paths.repo_root, args.before);
    let after_dir = resolve_path(&msl_paths.repo_root, args.after);
    let output_json = args
        .output_json
        .map(|path| resolve_path(&msl_paths.repo_root, path))
        .unwrap_or_else(|| after_dir.join(DEFAULT_OUTPUT_JSON));
    let output_md = args
        .output_md
        .map(|path| resolve_path(&msl_paths.repo_root, path))
        .unwrap_or_else(|| after_dir.join(DEFAULT_OUTPUT_MD));
    Ok(InputPaths {
        repo_root: msl_paths.repo_root,
        before_results_file: before_dir.join(MSL_RESULTS_FILE),
        after_results_file: after_dir.join(MSL_RESULTS_FILE),
        before_trace_file: before_dir.join(TRACE_COMPARISON_FILE),
        after_trace_file: after_dir.join(TRACE_COMPARISON_FILE),
        before_package_pass_rates_file: before_dir.join(PACKAGE_PASS_RATES_FILE),
        after_package_pass_rates_file: after_dir.join(PACKAGE_PASS_RATES_FILE),
        before_dir,
        after_dir,
        output_json,
        output_md,
    })
}

fn resolve_path(repo_root: &Path, path: PathBuf) -> PathBuf {
    if path.is_absolute() {
        path
    } else {
        repo_root.join(path)
    }
}

fn build_report_from_paths(paths: &InputPaths) -> Result<TransitionDiffReport> {
    let before_results = read_required_json(&paths.before_results_file)?;
    let after_results = read_required_json(&paths.after_results_file)?;
    let before_trace = read_optional_json(&paths.before_trace_file)?;
    let after_trace = read_optional_json(&paths.after_trace_file)?;
    let before_package_rates = read_optional_json(&paths.before_package_pass_rates_file)?;
    let after_package_rates = read_optional_json(&paths.after_package_pass_rates_file)?;
    let before = collect_model_states(&before_results, before_trace.as_ref())?;
    let after = collect_model_states(&after_results, after_trace.as_ref())?;
    Ok(build_report(
        paths,
        before,
        after,
        collect_package_phase_deltas(before_package_rates.as_ref(), after_package_rates.as_ref())?,
    ))
}

fn build_report(
    paths: &InputPaths,
    before: IndexMap<String, ModelPhaseState>,
    after: IndexMap<String, ModelPhaseState>,
    package_phase_deltas: Vec<PackagePhaseDelta>,
) -> TransitionDiffReport {
    let before_names = before.keys().cloned().collect::<IndexSet<_>>();
    let after_names = after.keys().cloned().collect::<IndexSet<_>>();
    let new_models = after_names
        .difference(&before_names)
        .cloned()
        .collect::<Vec<_>>();
    let removed_models = before_names
        .difference(&after_names)
        .cloned()
        .collect::<Vec<_>>();

    let mut phase_fail_to_phase_pass = Vec::new();
    let mut phase_pass_to_phase_fail = Vec::new();
    let mut sim_solver_fail_to_sim_ok = Vec::new();
    let mut sim_ok_to_sim_solver_fail = Vec::new();
    let mut non_sim_ok_to_sim_ok = Vec::new();
    let mut sim_ok_to_non_sim_ok = Vec::new();
    let mut near_to_high_agreement = Vec::new();
    let mut high_to_non_high_agreement = Vec::new();

    for (model_name, before_state) in &before {
        let Some(after_state) = after.get(model_name) else {
            continue;
        };
        let record = || TransitionRecord {
            model_name: model_name.clone(),
            before: before_state.clone(),
            after: after_state.clone(),
        };
        let before_success = before_state.phase_reached == "Success";
        let after_success = after_state.phase_reached == "Success";
        if !before_success && after_success {
            phase_fail_to_phase_pass.push(record());
        }
        if before_success && !after_success {
            phase_pass_to_phase_fail.push(record());
        }
        if before_state.sim_status == "sim_solver_fail" && after_state.sim_ok() {
            sim_solver_fail_to_sim_ok.push(record());
        }
        if before_state.sim_ok() && after_state.sim_status == "sim_solver_fail" {
            sim_ok_to_sim_solver_fail.push(record());
        }
        if !before_state.sim_ok() && after_state.sim_ok() {
            non_sim_ok_to_sim_ok.push(record());
        }
        if before_state.sim_ok() && !after_state.sim_ok() {
            sim_ok_to_non_sim_ok.push(record());
        }
        if before_state.trace_band.as_deref() == Some("near") && after_state.trace_high() {
            near_to_high_agreement.push(record());
        }
        if before_state.trace_high() && !after_state.trace_high() {
            high_to_non_high_agreement.push(record());
        }
    }

    let counts = TransitionCounts {
        before_models: before.len(),
        after_models: after.len(),
        common_models: before_names.intersection(&after_names).count(),
        new_models: new_models.len(),
        removed_models: removed_models.len(),
        phase_fail_to_phase_pass: phase_fail_to_phase_pass.len(),
        phase_pass_to_phase_fail: phase_pass_to_phase_fail.len(),
        sim_solver_fail_to_sim_ok: sim_solver_fail_to_sim_ok.len(),
        sim_ok_to_sim_solver_fail: sim_ok_to_sim_solver_fail.len(),
        non_sim_ok_to_sim_ok: non_sim_ok_to_sim_ok.len(),
        sim_ok_to_non_sim_ok: sim_ok_to_non_sim_ok.len(),
        near_to_high_agreement: near_to_high_agreement.len(),
        high_to_non_high_agreement: high_to_non_high_agreement.len(),
    };

    TransitionDiffReport {
        generated_at_unix_seconds: unix_timestamp_seconds(),
        git_commit: get_git_commit(&paths.repo_root),
        inputs: report_inputs(paths),
        counts,
        new_models,
        removed_models,
        phase_fail_to_phase_pass,
        phase_pass_to_phase_fail,
        sim_solver_fail_to_sim_ok,
        sim_ok_to_sim_solver_fail,
        non_sim_ok_to_sim_ok,
        sim_ok_to_non_sim_ok,
        near_to_high_agreement,
        high_to_non_high_agreement,
        package_phase_deltas,
    }
}

fn report_inputs(paths: &InputPaths) -> IndexMap<String, String> {
    IndexMap::from([
        (
            "before_dir".to_string(),
            paths.before_dir.display().to_string(),
        ),
        (
            "after_dir".to_string(),
            paths.after_dir.display().to_string(),
        ),
        (
            "before_results_file".to_string(),
            paths.before_results_file.display().to_string(),
        ),
        (
            "after_results_file".to_string(),
            paths.after_results_file.display().to_string(),
        ),
        (
            "before_trace_file".to_string(),
            paths.before_trace_file.display().to_string(),
        ),
        (
            "after_trace_file".to_string(),
            paths.after_trace_file.display().to_string(),
        ),
        (
            "before_package_pass_rates_file".to_string(),
            paths.before_package_pass_rates_file.display().to_string(),
        ),
        (
            "after_package_pass_rates_file".to_string(),
            paths.after_package_pass_rates_file.display().to_string(),
        ),
    ])
}

fn collect_package_phase_deltas(
    before: Option<&Value>,
    after: Option<&Value>,
) -> Result<Vec<PackagePhaseDelta>> {
    let (before, after) = match (before, after) {
        (Some(before), Some(after)) => (before, after),
        (None, None) => return Ok(Vec::new()),
        (Some(_), None) => {
            bail!("before package pass rates exist but after package pass rates are missing");
        }
        (None, Some(_)) => {
            bail!("after package pass rates exist but before package pass rates are missing");
        }
    };
    let before_rows =
        collect_package_phase_counts(before).context("invalid before package pass rates")?;
    let after_rows =
        collect_package_phase_counts(after).context("invalid after package pass rates")?;
    let mut package_order = before_rows.keys().cloned().collect::<IndexSet<_>>();
    package_order.extend(after_rows.keys().cloned());
    Ok(package_order
        .into_iter()
        .filter_map(|package| {
            let before = before_rows.get(&package).cloned().unwrap_or_default();
            let after = after_rows.get(&package).cloned().unwrap_or_default();
            let delta = package_phase_delta(&before, &after);
            has_package_delta(&delta).then_some(PackagePhaseDelta {
                package,
                before,
                after,
                delta,
            })
        })
        .collect())
}

fn collect_package_phase_counts(payload: &Value) -> Result<IndexMap<String, PackagePhaseCounts>> {
    let mut rows = IndexMap::new();
    let Some(package_rows) = payload.get("rows").and_then(Value::as_array) else {
        bail!("missing rows array");
    };
    for row in package_rows {
        let (package, counts) = parse_package_phase_row(row)?;
        rows.insert(package, counts);
    }
    if let Some((package, counts)) = payload
        .get("overall")
        .map(parse_package_phase_row)
        .transpose()?
        .filter(|(package, _)| !rows.contains_key(package))
    {
        rows.insert(package, counts);
    }
    Ok(rows)
}

fn parse_package_phase_row(row: &Value) -> Result<(String, PackagePhaseCounts)> {
    let package = required_str(row, "package")?.to_string();
    Ok((
        package,
        PackagePhaseCounts {
            n: required_usize(row, "n")?,
            parse_passed: required_usize(row, "parse_passed")?,
            flatten_passed: required_usize(row, "flatten_passed")?,
            dae_passed: required_usize(row, "dae_passed")?,
            solve_passed: required_usize(row, "solve_passed")?,
            ic_passed: required_usize(row, "ic_passed")?,
            sim_passed: required_usize(row, "sim_passed")?,
        },
    ))
}

fn required_str<'a>(row: &'a Value, key: &str) -> Result<&'a str> {
    row.get(key)
        .and_then(Value::as_str)
        .with_context(|| format!("missing or non-string field `{key}`"))
}

fn required_usize(row: &Value, key: &str) -> Result<usize> {
    let raw = row
        .get(key)
        .and_then(Value::as_u64)
        .with_context(|| format!("missing or non-unsigned-integer field `{key}`"))?;
    raw.try_into()
        .with_context(|| format!("field `{key}` does not fit usize"))
}

fn package_phase_delta(
    before: &PackagePhaseCounts,
    after: &PackagePhaseCounts,
) -> PackagePhaseCountsDelta {
    PackagePhaseCountsDelta {
        n: count_delta(before.n, after.n),
        parse_passed: count_delta(before.parse_passed, after.parse_passed),
        flatten_passed: count_delta(before.flatten_passed, after.flatten_passed),
        dae_passed: count_delta(before.dae_passed, after.dae_passed),
        solve_passed: count_delta(before.solve_passed, after.solve_passed),
        ic_passed: count_delta(before.ic_passed, after.ic_passed),
        sim_passed: count_delta(before.sim_passed, after.sim_passed),
    }
}

fn count_delta(before: usize, after: usize) -> isize {
    after as isize - before as isize
}

fn has_package_delta(delta: &PackagePhaseCountsDelta) -> bool {
    delta.n != 0
        || delta.parse_passed != 0
        || delta.flatten_passed != 0
        || delta.dae_passed != 0
        || delta.solve_passed != 0
        || delta.ic_passed != 0
        || delta.sim_passed != 0
}

fn collect_model_states(
    results: &Value,
    trace: Option<&Value>,
) -> Result<IndexMap<String, ModelPhaseState>> {
    let trace_bands = trace
        .map(collect_trace_bands)
        .transpose()?
        .unwrap_or_default();
    let Some(model_results) = results.get("model_results").and_then(Value::as_array) else {
        bail!("MSL results JSON is missing model_results array");
    };

    let mut states = IndexMap::new();
    for (index, model) in model_results.iter().enumerate() {
        let model_name = required_str(model, "model_name")
            .with_context(|| format!("invalid model_results[{index}]"))?;
        let phase_reached = required_str(model, "phase_reached")
            .with_context(|| format!("invalid model_results entry for {model_name}"))?
            .to_string();
        let sim_status = required_str(model, "sim_status")
            .with_context(|| format!("invalid model_results entry for {model_name}"))?
            .to_string();
        states.insert(
            model_name.to_string(),
            ModelPhaseState {
                phase_reached,
                sim_status,
                trace_band: trace_bands.get(model_name).cloned(),
            },
        );
    }
    Ok(states)
}

fn collect_trace_bands(trace: &Value) -> Result<IndexMap<String, String>> {
    let Some(models) = trace.get("models").and_then(Value::as_object) else {
        bail!("trace comparison JSON is missing models object");
    };
    let mut bands = IndexMap::new();
    for (model_name, metric_value) in models {
        let metric: ModelDeviationMetric = serde_json::from_value(metric_value.clone())
            .with_context(|| format!("invalid trace metric for {model_name}"))?;
        bands.insert(
            model_name.clone(),
            trace_band_name(classify_metric(&metric)),
        );
    }
    Ok(bands)
}

fn classify_metric(metric: &ModelDeviationMetric) -> AgreementBand {
    classify_trace_metric_channel_distribution(
        metric,
        MODEL_HIGH_MIN_HIGH_CHANNEL_SHARE,
        MODEL_HIGH_MAX_DEVIATION_CHANNEL_SHARE,
        MODEL_MINOR_MIN_HIGH_PLUS_MINOR_CHANNEL_SHARE,
        MODEL_MINOR_MAX_DEVIATION_CHANNEL_SHARE,
    )
}

fn trace_band_name(band: AgreementBand) -> String {
    match band {
        AgreementBand::HighAgreement => "high",
        AgreementBand::MinorAgreement => "near",
        AgreementBand::Deviation => "deviation",
    }
    .to_string()
}

fn write_outputs(paths: &InputPaths, report: &TransitionDiffReport, top: usize) -> Result<()> {
    write_pretty_json(&paths.output_json, &json!(report))?;
    if let Some(parent) = paths.output_md.parent() {
        fs::create_dir_all(parent)
            .with_context(|| format!("failed to create {}", parent.display()))?;
    }
    fs::write(&paths.output_md, render_markdown(report, top))
        .with_context(|| format!("failed to write {}", paths.output_md.display()))?;
    Ok(())
}

fn render_markdown(report: &TransitionDiffReport, top: usize) -> String {
    let mut lines = vec![
        "# MSL Transition Diff".to_string(),
        String::new(),
        format!("- before models: {}", report.counts.before_models),
        format!("- after models: {}", report.counts.after_models),
        format!("- common models: {}", report.counts.common_models),
        String::new(),
        "| Transition | Count |".to_string(),
        "|---|---:|".to_string(),
        format!(
            "| phase fail to phase pass | {} |",
            report.counts.phase_fail_to_phase_pass
        ),
        format!(
            "| phase pass to phase fail | {} |",
            report.counts.phase_pass_to_phase_fail
        ),
        format!(
            "| sim solver fail to sim ok | {} |",
            report.counts.sim_solver_fail_to_sim_ok
        ),
        format!(
            "| sim ok to sim solver fail | {} |",
            report.counts.sim_ok_to_sim_solver_fail
        ),
        format!(
            "| non-sim-ok to sim ok | {} |",
            report.counts.non_sim_ok_to_sim_ok
        ),
        format!(
            "| sim ok to non-sim-ok | {} |",
            report.counts.sim_ok_to_non_sim_ok
        ),
        format!(
            "| near to high agreement | {} |",
            report.counts.near_to_high_agreement
        ),
        format!(
            "| high to non-high agreement | {} |",
            report.counts.high_to_non_high_agreement
        ),
        String::new(),
    ];
    append_section(
        &mut lines,
        "Phase Fail To Phase Pass",
        &report.phase_fail_to_phase_pass,
        top,
    );
    append_section(
        &mut lines,
        "Phase Pass To Phase Fail",
        &report.phase_pass_to_phase_fail,
        top,
    );
    append_section(
        &mut lines,
        "Sim Solver Fail To Sim OK",
        &report.sim_solver_fail_to_sim_ok,
        top,
    );
    append_section(
        &mut lines,
        "Sim OK To Sim Solver Fail",
        &report.sim_ok_to_sim_solver_fail,
        top,
    );
    append_section(
        &mut lines,
        "Near To High Agreement",
        &report.near_to_high_agreement,
        top,
    );
    append_section(
        &mut lines,
        "High To Non-High Agreement",
        &report.high_to_non_high_agreement,
        top,
    );
    append_package_deltas(&mut lines, &report.package_phase_deltas, top);
    lines.join("\n")
}

fn append_section(lines: &mut Vec<String>, title: &str, records: &[TransitionRecord], top: usize) {
    if records.is_empty() {
        return;
    }
    lines.push(format!("## {title}"));
    lines.push(String::new());
    lines.push("| Model | Before | After |".to_string());
    lines.push("|---|---|---|".to_string());
    lines.extend(records.iter().take(top).map(|record| {
        format!(
            "| {} | {} | {} |",
            record.model_name,
            format_state(&record.before),
            format_state(&record.after)
        )
    }));
    if records.len() > top {
        lines.push(format!("| ... | {} more | |", records.len() - top));
    }
    lines.push(String::new());
}

fn append_package_deltas(lines: &mut Vec<String>, records: &[PackagePhaseDelta], top: usize) {
    if records.is_empty() {
        return;
    }
    lines.push("## Package Phase Deltas".to_string());
    lines.push(String::new());
    lines.push("| Package | n | Ast | Flat | Dae | Solve | IC | Sim |".to_string());
    lines.push("|---|---:|---:|---:|---:|---:|---:|---:|".to_string());
    lines.extend(records.iter().take(top).map(|record| {
        format!(
            "| {} | {} | {} | {} | {} | {} | {} | {} |",
            record.package,
            format_delta(record.delta.n),
            format_delta(record.delta.parse_passed),
            format_delta(record.delta.flatten_passed),
            format_delta(record.delta.dae_passed),
            format_delta(record.delta.solve_passed),
            format_delta(record.delta.ic_passed),
            format_delta(record.delta.sim_passed)
        )
    }));
    if records.len() > top {
        lines.push(format!(
            "| ... | {} more | | | | | | |",
            records.len() - top
        ));
    }
    lines.push(String::new());
}

fn format_delta(delta: isize) -> String {
    if delta > 0 {
        return format!("+{delta}");
    }
    delta.to_string()
}

fn format_state(state: &ModelPhaseState) -> String {
    match &state.trace_band {
        Some(trace_band) => format!(
            "phase={}, sim={}, trace={}",
            state.phase_reached, state.sim_status, trace_band
        ),
        None => format!("phase={}, sim={}", state.phase_reached, state.sim_status),
    }
}

fn print_summary(paths: &InputPaths, report: &TransitionDiffReport) {
    println!(
        "MSL transition diff written to {}",
        paths.output_json.display()
    );
    println!("  Markdown: {}", paths.output_md.display());
    println!(
        "  common={} | sim gains={} | sim regressions={} | high gains={} | high regressions={}",
        report.counts.common_models,
        report.counts.non_sim_ok_to_sim_ok,
        report.counts.sim_ok_to_non_sim_ok,
        report.counts.near_to_high_agreement,
        report.counts.high_to_non_high_agreement
    );
}

fn read_required_json(path: &Path) -> Result<Value> {
    if !path.is_file() {
        bail!("missing input file '{}'", path.display());
    }
    let raw =
        fs::read_to_string(path).with_context(|| format!("failed to read {}", path.display()))?;
    serde_json::from_str(&raw).with_context(|| format!("invalid JSON {}", path.display()))
}

fn read_optional_json(path: &Path) -> Result<Option<Value>> {
    if !path.is_file() {
        return Ok(None);
    }
    read_required_json(path).map(Some)
}

#[cfg(test)]
mod tests {
    use super::*;
    use serde_json::json;

    #[test]
    fn transition_report_tracks_sim_and_trace_movements() {
        let paths = InputPaths {
            repo_root: PathBuf::from("."),
            before_dir: PathBuf::from("before"),
            after_dir: PathBuf::from("after"),
            before_results_file: PathBuf::from("before/msl_results.json"),
            after_results_file: PathBuf::from("after/msl_results.json"),
            before_trace_file: PathBuf::from("before/sim_trace_comparison.json"),
            after_trace_file: PathBuf::from("after/sim_trace_comparison.json"),
            before_package_pass_rates_file: PathBuf::from("before/msl_package_pass_rates.json"),
            after_package_pass_rates_file: PathBuf::from("after/msl_package_pass_rates.json"),
            output_json: PathBuf::from("out.json"),
            output_md: PathBuf::from("out.md"),
        };
        let before = states([
            ("A", "Flatten", "not-simulated", None),
            ("B", "Success", "sim_solver_fail", None),
            ("C", "Success", "sim_ok", Some("near")),
            ("D", "Success", "sim_ok", Some("high")),
        ]);
        let after = states([
            ("A", "Success", "sim_ok", Some("high")),
            ("B", "Success", "sim_ok", Some("high")),
            ("C", "Success", "sim_ok", Some("high")),
            ("D", "Success", "sim_ok", Some("deviation")),
            ("E", "Success", "sim_ok", Some("high")),
        ]);

        let report = build_report(&paths, before, after, Vec::new());

        assert_eq!(report.counts.phase_fail_to_phase_pass, 1);
        assert_eq!(report.counts.sim_solver_fail_to_sim_ok, 1);
        assert_eq!(report.counts.non_sim_ok_to_sim_ok, 2);
        assert_eq!(report.counts.near_to_high_agreement, 1);
        assert_eq!(report.counts.high_to_non_high_agreement, 1);
        assert_eq!(report.new_models, vec!["E"]);
        assert_eq!(report.phase_fail_to_phase_pass[0].model_name, "A");
    }

    #[test]
    fn package_phase_deltas_preserve_package_order_and_counts() {
        let before = json!({
            "rows": [
                package_row("Blocks", [2, 2, 1, 1, 1, 1, 1]),
                package_row("Fluid", [1, 1, 0, 0, 0, 0, 0])
            ],
            "overall": package_row("Overall", [3, 3, 1, 1, 1, 1, 1])
        });
        let after = json!({
            "rows": [
                package_row("Blocks", [2, 2, 2, 2, 2, 2, 2]),
                package_row("Electrical", [1, 1, 1, 1, 0, 0, 0])
            ],
            "overall": package_row("Overall", [3, 3, 3, 3, 2, 2, 2])
        });

        let deltas =
            collect_package_phase_deltas(Some(&before), Some(&after)).expect("collect deltas");

        assert_eq!(
            deltas
                .iter()
                .map(|delta| delta.package.as_str())
                .collect::<Vec<_>>(),
            vec!["Blocks", "Fluid", "Overall", "Electrical"]
        );
        assert_eq!(deltas[0].delta.flatten_passed, 1);
        assert_eq!(deltas[1].delta.n, -1);
        assert_eq!(deltas[3].delta.n, 1);
    }

    #[test]
    fn collect_model_states_uses_trace_gate_classifier() {
        let results = json!({
            "model_results": [
                {"model_name": "High", "phase_reached": "Success", "sim_status": "sim_ok"},
                {"model_name": "Near", "phase_reached": "Success", "sim_status": "sim_ok"}
            ]
        });
        let trace = json!({
            "models": {
                "High": trace_metric("High", 8, 0, 0),
                "Near": trace_metric("Near", 7, 3, 0)
            }
        });

        let states = collect_model_states(&results, Some(&trace)).expect("collect states");

        assert_eq!(states["High"].trace_band.as_deref(), Some("high"));
        assert_eq!(states["Near"].trace_band.as_deref(), Some("near"));
    }

    fn states<const N: usize>(
        entries: [(&str, &str, &str, Option<&str>); N],
    ) -> IndexMap<String, ModelPhaseState> {
        entries
            .into_iter()
            .map(|(name, phase, sim, trace)| {
                (
                    name.to_string(),
                    ModelPhaseState {
                        phase_reached: phase.to_string(),
                        sim_status: sim.to_string(),
                        trace_band: trace.map(ToOwned::to_owned),
                    },
                )
            })
            .collect()
    }

    fn trace_metric(model_name: &str, high: usize, minor: usize, deviation: usize) -> Value {
        json!({
            "model_name": model_name,
            "compared_variables": high + minor + deviation,
            "samples_compared": 10,
            "bounded_normalized_l1_score": 0.0,
            "mean_channel_bounded_normalized_l1": 0.0,
            "max_channel_bounded_normalized_l1": 0.0,
            "channel_high_count": high,
            "channel_minor_count": minor,
            "channel_deviation_count": deviation,
            "channel_severe_count": 0,
            "worst_variables": []
        })
    }

    fn package_row(package: &str, counts: [usize; 7]) -> Value {
        let [n, parse, flat, dae, solve, ic, sim] = counts;
        json!({
            "package": package,
            "n": n,
            "parse_passed": parse,
            "flatten_passed": flat,
            "dae_passed": dae,
            "solve_passed": solve,
            "ic_passed": ic,
            "sim_passed": sim
        })
    }
}
