use super::common::{MslPaths, get_git_commit, unix_timestamp_seconds, write_pretty_json};
use anyhow::{Context, Result, bail};
use clap::Args as ClapArgs;
use serde::Serialize;
use serde_json::Value;
use std::collections::BTreeMap;
use std::fs;
use std::path::{Path, PathBuf};

#[derive(Debug, Clone, ClapArgs)]
pub struct Args {
    /// Results directory containing MSL JSON artifacts.
    #[arg(long)]
    results_dir: Option<PathBuf>,
    /// Rumoca MSL results JSON.
    #[arg(long)]
    rumoca_results_file: Option<PathBuf>,
    /// MSL quality snapshot JSON.
    #[arg(long)]
    quality_file: Option<PathBuf>,
    /// OMC/Rumoca trace comparison JSON.
    #[arg(long)]
    trace_file: Option<PathBuf>,
    /// Output triage JSON.
    #[arg(long)]
    output_json: Option<PathBuf>,
    /// Output triage Markdown.
    #[arg(long)]
    output_md: Option<PathBuf>,
    /// Maximum records to print in ranked sections.
    #[arg(long, default_value_t = 20)]
    top: usize,
}

#[derive(Debug, Clone, Serialize, PartialEq)]
struct TriageRecord {
    model_name: String,
    category: String,
    reason: String,
    phase: Option<String>,
    error_code: Option<String>,
    detail: Option<String>,
    reproduction: String,
    rank_score: f64,
}

#[derive(Debug, Clone, Serialize, PartialEq)]
struct TraceTriageRecord {
    model_name: String,
    mean_channel_bounded_normalized_l1: Option<f64>,
    max_channel_bounded_normalized_l1: Option<f64>,
    bounded_normalized_l1_score: Option<f64>,
    compared_variables: Option<u64>,
    dominant_shapes: Vec<String>,
    worst_channels: Vec<String>,
    reproduction: String,
    rank_score: f64,
}

#[derive(Debug, Clone, Serialize, PartialEq, Eq)]
struct MissingTraceRecord {
    model_name: String,
    reason: String,
    detail: Option<String>,
    reproduction: String,
}

#[derive(Debug, Clone, Serialize, PartialEq)]
struct TaxonomyCoverage {
    classified_records: usize,
    unknown_records: usize,
    classified_percent: f64,
}

#[derive(Debug, Clone, Serialize, PartialEq)]
struct TriageSummary {
    compiled_models: Option<u64>,
    balanced_models: Option<u64>,
    sim_ok: Option<u64>,
    sim_attempted: Option<u64>,
    sim_success_rate: Option<f64>,
    trace_high_plus_near_percent: Option<f64>,
    trace_bad_channels_percent: Option<f64>,
    trace_severe_channels_percent: Option<f64>,
    trace_mean_l1: Option<f64>,
}

#[derive(Debug, Clone, Serialize, PartialEq)]
struct TriageReport {
    generated_at_unix_seconds: i64,
    git_commit: String,
    #[serde(skip)]
    display_top: usize,
    inputs: BTreeMap<String, String>,
    summary: TriageSummary,
    taxonomy_coverage: TaxonomyCoverage,
    reason_counts: BTreeMap<String, usize>,
    compile_failures: Vec<TriageRecord>,
    balance_failures: Vec<TriageRecord>,
    simulation_failures: Vec<TriageRecord>,
    worst_trace_models: Vec<TraceTriageRecord>,
    missing_trace_models: Vec<MissingTraceRecord>,
}

#[derive(Debug, Clone)]
struct InputPaths {
    results_dir: PathBuf,
    rumoca_results_file: PathBuf,
    quality_file: PathBuf,
    trace_file: PathBuf,
    output_json: PathBuf,
    output_md: PathBuf,
}

pub fn run(args: Args) -> Result<()> {
    let paths = resolve_input_paths(&args);
    let report = build_report_from_paths(&paths, args.top)?;
    write_outputs(&paths, &report)?;
    print_summary(&paths, &report);
    Ok(())
}

fn resolve_input_paths(args: &Args) -> InputPaths {
    let paths = MslPaths::current();
    let results_dir = args
        .results_dir
        .clone()
        .unwrap_or_else(|| paths.results_dir.clone());
    let rumoca_results_file = args
        .rumoca_results_file
        .clone()
        .unwrap_or_else(|| results_dir.join("msl_results.json"));
    let quality_file = args
        .quality_file
        .clone()
        .unwrap_or_else(|| results_dir.join("msl_quality_current.json"));
    let trace_file = args
        .trace_file
        .clone()
        .unwrap_or_else(|| results_dir.join("sim_trace_comparison.json"));
    let output_json = args
        .output_json
        .clone()
        .unwrap_or_else(|| results_dir.join("msl_triage.json"));
    let output_md = args
        .output_md
        .clone()
        .unwrap_or_else(|| results_dir.join("msl_triage.md"));
    InputPaths {
        results_dir,
        rumoca_results_file,
        quality_file,
        trace_file,
        output_json,
        output_md,
    }
}

fn build_report_from_paths(paths: &InputPaths, top: usize) -> Result<TriageReport> {
    let rumoca = read_required_json(&paths.rumoca_results_file)?;
    let quality = read_optional_json(&paths.quality_file)?;
    let trace = read_optional_json(&paths.trace_file)?;
    Ok(build_report(
        paths,
        &rumoca,
        quality.as_ref(),
        trace.as_ref(),
        top,
    ))
}

fn build_report(
    paths: &InputPaths,
    rumoca: &Value,
    quality: Option<&Value>,
    trace: Option<&Value>,
    top: usize,
) -> TriageReport {
    let model_results = model_results(rumoca);
    let compile_failures = collect_compile_failures(model_results);
    let balance_failures = collect_balance_failures(model_results);
    let simulation_failures = collect_simulation_failures(model_results);
    let worst_trace_models =
        trace.map_or_else(Vec::new, |payload| collect_worst_traces(payload, top));
    let missing_trace_models = trace.map_or_else(Vec::new, collect_missing_traces);
    let mut reason_counts = BTreeMap::new();
    add_reason_counts(&mut reason_counts, &compile_failures);
    add_reason_counts(&mut reason_counts, &balance_failures);
    add_reason_counts(&mut reason_counts, &simulation_failures);
    add_missing_trace_reason_counts(&mut reason_counts, &missing_trace_models);
    let taxonomy_coverage = build_taxonomy_coverage(
        &compile_failures,
        &balance_failures,
        &simulation_failures,
        &missing_trace_models,
    );
    TriageReport {
        generated_at_unix_seconds: unix_timestamp_seconds(),
        git_commit: get_git_commit(&MslPaths::current().repo_root),
        display_top: top,
        inputs: input_summary(paths),
        summary: build_summary(rumoca, quality, trace),
        taxonomy_coverage,
        reason_counts,
        compile_failures,
        balance_failures,
        simulation_failures,
        worst_trace_models,
        missing_trace_models,
    }
}

fn input_summary(paths: &InputPaths) -> BTreeMap<String, String> {
    BTreeMap::from([
        (
            "results_dir".to_string(),
            paths.results_dir.display().to_string(),
        ),
        (
            "rumoca_results_file".to_string(),
            paths.rumoca_results_file.display().to_string(),
        ),
        (
            "quality_file".to_string(),
            paths.quality_file.display().to_string(),
        ),
        (
            "trace_file".to_string(),
            paths.trace_file.display().to_string(),
        ),
    ])
}

fn build_summary(rumoca: &Value, quality: Option<&Value>, trace: Option<&Value>) -> TriageSummary {
    let source = quality.unwrap_or(rumoca);
    TriageSummary {
        compiled_models: get_u64(source, "compiled_models"),
        balanced_models: get_u64(source, "balanced_models"),
        sim_ok: get_u64(source, "sim_ok"),
        sim_attempted: get_u64(source, "sim_attempted"),
        sim_success_rate: get_f64(source, "sim_success_rate"),
        trace_high_plus_near_percent: trace_high_plus_near_percent(source, trace),
        trace_bad_channels_percent: trace_metric(source, trace, "bad_channels_percent"),
        trace_severe_channels_percent: trace_metric(source, trace, "severe_channels_percent"),
        trace_mean_l1: trace_metric(
            source,
            trace,
            "mean_model_mean_channel_bounded_normalized_l1",
        ),
    }
}

fn trace_high_plus_near_percent(source: &Value, trace: Option<&Value>) -> Option<f64> {
    trace
        .and_then(|payload| get_nested_f64(payload, &["agreement_bands_percent", "high_agreement"]))
        .zip(trace.and_then(|payload| {
            get_nested_f64(payload, &["agreement_bands_percent", "minor_agreement"])
        }))
        .map(|(high, near)| high + near)
        .or_else(|| {
            let stats = source.get("trace_accuracy_stats")?;
            Some(
                get_f64(stats, "agreement_high_percent")?
                    + get_f64(stats, "agreement_minor_percent")?,
            )
        })
}

fn trace_metric(source: &Value, trace: Option<&Value>, key: &str) -> Option<f64> {
    trace
        .and_then(|payload| get_nested_f64(payload, &["summary", key]))
        .or_else(|| {
            source
                .get("trace_accuracy_stats")
                .and_then(|stats| get_f64(stats, key))
        })
}

fn read_required_json(path: &Path) -> Result<Value> {
    if !path.is_file() {
        bail!("missing required MSL result file: {}", path.display());
    }
    let raw =
        fs::read_to_string(path).with_context(|| format!("failed to read {}", path.display()))?;
    serde_json::from_str(&raw).with_context(|| format!("invalid JSON in {}", path.display()))
}

fn read_optional_json(path: &Path) -> Result<Option<Value>> {
    if !path.is_file() {
        return Ok(None);
    }
    read_required_json(path).map(Some)
}

fn model_results(rumoca: &Value) -> &[Value] {
    rumoca
        .get("model_results")
        .and_then(Value::as_array)
        .map(Vec::as_slice)
        .unwrap_or_default()
}

fn collect_compile_failures(results: &[Value]) -> Vec<TriageRecord> {
    let mut records = results
        .iter()
        .filter(|entry| phase(entry).is_some_and(|phase| !compile_phase_is_success_like(phase)))
        .map(compile_record)
        .collect::<Vec<_>>();
    records.sort_by(compare_triage_record);
    records
}

fn compile_phase_is_success_like(phase: &str) -> bool {
    matches!(phase, "Success" | "NonSim")
}

fn compile_record(entry: &Value) -> TriageRecord {
    let raw_detail = get_str(entry, "error");
    let phase = classify_compile_phase(phase(entry), raw_detail);
    let error_code = get_str(entry, "error_code").map(str::to_string);
    let detail = raw_detail.map(truncate_detail);
    let reason = classify_compile_reason(phase.as_deref(), error_code.as_deref());
    base_record(entry, "compile", &reason, phase, error_code, detail, 0.0)
}

fn classify_compile_phase(phase: Option<&str>, detail: Option<&str>) -> Option<String> {
    detail
        .and_then(compile_phase_from_detail)
        .or_else(|| phase.map(str::to_string))
}

fn compile_phase_from_detail(detail: &str) -> Option<String> {
    for phase in [
        "Parse",
        "Resolve",
        "Instantiate",
        "Typecheck",
        "Flatten",
        "ToDae",
        "Solve",
    ] {
        if detail.contains(&format!(" failed in {phase}:")) {
            return Some(phase.to_string());
        }
    }
    None
}

fn classify_compile_reason(phase: Option<&str>, error_code: Option<&str>) -> String {
    if error_code.is_some_and(|code| code.starts_with("EP")) {
        return "compile.parse".to_string();
    }
    match phase.unwrap_or("unknown") {
        "Parse" => "compile.parse",
        "Resolve" => "compile.resolve",
        "NeedsInner" => "compile.needs_inner",
        "Instantiate" => "compile.instantiate",
        "Typecheck" => "compile.typecheck",
        "Flatten" => "compile.flatten",
        "ToDae" => "compile.dae",
        "Solve" => "compile.solve",
        _ => "compile.unknown",
    }
    .to_string()
}

fn collect_balance_failures(results: &[Value]) -> Vec<TriageRecord> {
    let mut records = results
        .iter()
        .filter(|entry| phase(entry) == Some("Success"))
        .flat_map(balance_records)
        .collect::<Vec<_>>();
    records.sort_by(compare_triage_record);
    records
}

fn balance_records(entry: &Value) -> Vec<TriageRecord> {
    let mut records = Vec::new();
    if get_bool(entry, "is_balanced") == Some(false) {
        let balance = get_i64(entry, "balance");
        let reason = classify_balance_reason(balance);
        records.push(base_record(
            entry,
            "balance",
            &reason,
            Some("Balance".to_string()),
            None,
            balance.map(|value| format!("balance={value}")),
            balance.map_or(0.0, |value| value.unsigned_abs() as f64),
        ));
    }
    if get_bool(entry, "initial_balance_ok") == Some(false) {
        let before = get_i64(entry, "initial_balance_deficit_before").unwrap_or_default();
        let after = get_i64(entry, "initial_balance_deficit_after").unwrap_or_default();
        records.push(base_record(
            entry,
            "balance",
            "balance.initial",
            Some("InitialBalance".to_string()),
            None,
            Some(format!(
                "initial_balance_deficit_before={before}, initial_balance_deficit_after={after}"
            )),
            after.unsigned_abs().max(before.unsigned_abs()) as f64,
        ));
    }
    records
}

fn classify_balance_reason(balance: Option<i64>) -> String {
    match balance.unwrap_or_default().cmp(&0) {
        std::cmp::Ordering::Less => "balance.underdetermined",
        std::cmp::Ordering::Equal => "balance.structural_singular",
        std::cmp::Ordering::Greater => "balance.overdetermined",
    }
    .to_string()
}

fn collect_simulation_failures(results: &[Value]) -> Vec<TriageRecord> {
    let mut records = results
        .iter()
        .filter_map(simulation_record)
        .collect::<Vec<_>>();
    records.sort_by(compare_triage_record);
    records
}

fn simulation_record(entry: &Value) -> Option<TriageRecord> {
    let status = get_str(entry, "sim_status")?;
    if status == "sim_ok" {
        return None;
    }
    let detail = get_str(entry, "sim_error")
        .or_else(|| get_str(entry, "sim_trace_error"))
        .map(truncate_detail);
    let reason = classify_sim_reason(status, detail.as_deref());
    Some(base_record(
        entry,
        "simulation",
        &reason,
        Some("Simulation".to_string()),
        None,
        detail,
        simulation_rank(status),
    ))
}

fn classify_sim_reason(status: &str, detail: Option<&str>) -> String {
    match status {
        "sim_timeout" => return "sim.timeout".to_string(),
        "sim_nan" => return "sim.nonfinite".to_string(),
        "sim_balance_fail" => return "sim.balance".to_string(),
        "sim_trace_fail" => return "sim.trace_output".to_string(),
        _ => {}
    }
    let detail = detail.unwrap_or_default().to_ascii_lowercase();
    // Structural-lowering singularities are the dominant solver-failure family;
    // bucket them by the unmatched-unknown pattern so the cluster map is visible
    // without re-running `--inspect structure` per model.
    if detail.contains("structurally singular") {
        return classify_structural_singularity(&detail);
    }
    if detail.contains("initial") || detail.contains("start") || detail.contains("fixed") {
        "sim.init"
    } else if detail.contains("event") || detail.contains("root") || detail.contains("pre(") {
        "sim.event"
    } else if detail.contains("table") {
        "sim.table"
    } else if detail.contains("external") {
        "sim.external"
    } else if detail.contains("non-finite") || detail.contains("nan") || detail.contains("inf") {
        "sim.nonfinite"
    } else {
        "sim.solver"
    }
    .to_string()
}

/// Sub-classify a structural-singularity failure by the unmatched-unknown
/// pattern parsed from the (lowercased) failure detail. Mirrors the categories
/// of `rumoca sim --inspect structure`'s diagnosis.
fn classify_structural_singularity(detail: &str) -> String {
    let unknowns = detail
        .rsplit("unmatched unknowns:")
        .next()
        .unwrap_or_default();
    let sub = if unknowns.contains("der(") {
        "derivative"
    } else if unknowns.contains(".y2") {
        "inverse_block"
    } else if unknowns.contains(".tau") || unknowns.contains("flange") || unknowns.contains(".f") {
        "connector_flow"
    } else if unknowns.contains(".v") {
        "node_potential"
    } else if unknowns.contains("qdd") || unknowns.contains("qd") {
        "clocked_kinematic"
    } else {
        "algebraic"
    };
    format!("sim.structural.{sub}")
}

fn simulation_rank(status: &str) -> f64 {
    match status {
        "sim_timeout" => 4.0,
        "sim_solver_fail" => 3.0,
        "sim_nan" => 2.0,
        _ => 1.0,
    }
}

fn base_record(
    entry: &Value,
    category: &str,
    reason: &str,
    phase: Option<String>,
    error_code: Option<String>,
    detail: Option<String>,
    rank_score: f64,
) -> TriageRecord {
    let model_name = model_name(entry);
    TriageRecord {
        reproduction: reproduction_command(&model_name),
        model_name,
        category: category.to_string(),
        reason: reason.to_string(),
        phase,
        error_code,
        detail,
        rank_score,
    }
}

fn collect_worst_traces(trace: &Value, top: usize) -> Vec<TraceTriageRecord> {
    let mut records = trace
        .get("worst_models")
        .and_then(Value::as_array)
        .into_iter()
        .flatten()
        .map(trace_record)
        .collect::<Vec<_>>();
    if records.is_empty() {
        records = trace
            .get("models")
            .and_then(Value::as_object)
            .into_iter()
            .flatten()
            .map(|(_, entry)| trace_record(entry))
            .collect();
    }
    records.sort_by(|lhs, rhs| compare_f64_desc(lhs.rank_score, rhs.rank_score));
    records.truncate(top);
    records
}

fn trace_record(entry: &Value) -> TraceTriageRecord {
    let model_name = get_str(entry, "model_name")
        .unwrap_or("unknown")
        .to_string();
    let mean_l1 = get_f64(entry, "mean_channel_bounded_normalized_l1");
    let max_l1 = get_f64(entry, "max_channel_bounded_normalized_l1");
    let score = get_f64(entry, "bounded_normalized_l1_score");
    TraceTriageRecord {
        reproduction: reproduction_command(&model_name),
        model_name,
        mean_channel_bounded_normalized_l1: mean_l1,
        max_channel_bounded_normalized_l1: max_l1,
        bounded_normalized_l1_score: score,
        compared_variables: get_u64(entry, "compared_variables"),
        dominant_shapes: dominant_trace_shapes(entry),
        worst_channels: worst_channel_names(entry),
        rank_score: mean_l1.or(max_l1).or(score).unwrap_or_default(),
    }
}

fn dominant_trace_shapes(entry: &Value) -> Vec<String> {
    let mut counts = BTreeMap::<String, usize>::new();
    for shape in entry
        .get("worst_variables")
        .and_then(Value::as_array)
        .into_iter()
        .flatten()
        .filter_map(|channel| channel.get("shape").and_then(Value::as_str))
    {
        *counts.entry(shape.to_string()).or_default() += 1;
    }
    let mut ranked = counts.into_iter().collect::<Vec<_>>();
    ranked.sort_by(|lhs, rhs| rhs.1.cmp(&lhs.1).then_with(|| lhs.0.cmp(&rhs.0)));
    ranked.into_iter().take(3).map(|(shape, _)| shape).collect()
}

fn worst_channel_names(entry: &Value) -> Vec<String> {
    entry
        .get("worst_variables")
        .and_then(Value::as_array)
        .into_iter()
        .flatten()
        .filter_map(format_worst_channel)
        .take(5)
        .collect()
}

fn format_worst_channel(channel: &Value) -> Option<String> {
    let name = channel.get("name").and_then(Value::as_str)?;
    let Some(shape) = channel.get("shape").and_then(Value::as_str) else {
        return Some(name.to_string());
    };
    Some(format!("{name} [{shape}]"))
}

fn collect_missing_traces(trace: &Value) -> Vec<MissingTraceRecord> {
    let mut records = trace
        .get("missing_trace")
        .and_then(Value::as_object)
        .into_iter()
        .flatten()
        .map(|(model_name, reason)| MissingTraceRecord {
            model_name: model_name.clone(),
            reason: "trace.missing_channel".to_string(),
            detail: reason.as_str().map(truncate_detail),
            reproduction: reproduction_command(model_name),
        })
        .collect::<Vec<_>>();
    records.sort_by(|lhs, rhs| lhs.model_name.cmp(&rhs.model_name));
    records
}

fn add_reason_counts(counts: &mut BTreeMap<String, usize>, records: &[TriageRecord]) {
    for record in records {
        *counts.entry(record.reason.clone()).or_default() += 1;
    }
}

fn add_missing_trace_reason_counts(
    counts: &mut BTreeMap<String, usize>,
    records: &[MissingTraceRecord],
) {
    for record in records {
        *counts.entry(record.reason.clone()).or_default() += 1;
    }
}

fn build_taxonomy_coverage(
    compile_failures: &[TriageRecord],
    balance_failures: &[TriageRecord],
    simulation_failures: &[TriageRecord],
    missing_trace_models: &[MissingTraceRecord],
) -> TaxonomyCoverage {
    let reasons = compile_failures
        .iter()
        .chain(balance_failures)
        .chain(simulation_failures)
        .map(|record| record.reason.as_str())
        .chain(
            missing_trace_models
                .iter()
                .map(|record| record.reason.as_str()),
        );
    let (classified_records, unknown_records) =
        reasons.fold((0, 0), |(classified, unknown), reason| {
            if reason_is_unknown(reason) {
                (classified, unknown + 1)
            } else {
                (classified + 1, unknown)
            }
        });
    let total = classified_records + unknown_records;
    TaxonomyCoverage {
        classified_records,
        unknown_records,
        classified_percent: percent(classified_records, total),
    }
}

fn reason_is_unknown(reason: &str) -> bool {
    reason.ends_with(".unknown")
}

fn percent(numerator: usize, denominator: usize) -> f64 {
    if denominator == 0 {
        return 100.0;
    }
    numerator as f64 * 100.0 / denominator as f64
}

fn write_outputs(paths: &InputPaths, report: &TriageReport) -> Result<()> {
    if let Some(parent) = paths.output_json.parent() {
        fs::create_dir_all(parent)
            .with_context(|| format!("failed to create {}", parent.display()))?;
    }
    write_pretty_json(&paths.output_json, report)?;
    fs::write(&paths.output_md, render_markdown(report))
        .with_context(|| format!("failed to write {}", paths.output_md.display()))
}

fn render_markdown(report: &TriageReport) -> String {
    let mut out = String::new();
    out.push_str("# MSL Triage Report\n\n");
    push_summary_table(&mut out, &report.summary, &report.taxonomy_coverage);
    push_reason_counts(&mut out, &report.reason_counts);
    push_records(
        &mut out,
        "Compile Failures",
        &report.compile_failures,
        report.display_top,
    );
    push_records(
        &mut out,
        "Balance Failures",
        &report.balance_failures,
        report.display_top,
    );
    push_records(
        &mut out,
        "Simulation Failures",
        &report.simulation_failures,
        report.display_top,
    );
    push_trace_records(&mut out, &report.worst_trace_models);
    push_missing_traces(&mut out, &report.missing_trace_models, report.display_top);
    out
}

fn push_summary_table(out: &mut String, summary: &TriageSummary, coverage: &TaxonomyCoverage) {
    out.push_str("## Summary\n\n");
    out.push_str("| Metric | Value |\n|---|---:|\n");
    push_u64_row(out, "Compiled models", summary.compiled_models);
    push_u64_row(out, "Balanced models", summary.balanced_models);
    push_u64_row(out, "Sim OK", summary.sim_ok);
    push_u64_row(out, "Sim attempted", summary.sim_attempted);
    push_f64_row(out, "Sim success rate", summary.sim_success_rate);
    push_f64_row(
        out,
        "Trace high + near percent",
        summary.trace_high_plus_near_percent,
    );
    push_f64_row(
        out,
        "Trace bad channels percent",
        summary.trace_bad_channels_percent,
    );
    push_f64_row(
        out,
        "Trace severe channels percent",
        summary.trace_severe_channels_percent,
    );
    push_f64_row(out, "Trace mean L1", summary.trace_mean_l1);
    push_usize_row(
        out,
        "Taxonomy classified records",
        coverage.classified_records,
    );
    push_usize_row(out, "Taxonomy unknown records", coverage.unknown_records);
    push_f64_row(
        out,
        "Taxonomy classified percent",
        Some(coverage.classified_percent),
    );
    out.push('\n');
}

fn push_reason_counts(out: &mut String, counts: &BTreeMap<String, usize>) {
    out.push_str("## Reason Counts\n\n");
    out.push_str("| Reason | Count |\n|---|---:|\n");
    for (reason, count) in counts {
        out.push_str(&format!("| `{}` | {} |\n", escape_md(reason), count));
    }
    out.push('\n');
}

fn push_records(out: &mut String, title: &str, records: &[TriageRecord], limit: usize) {
    out.push_str(&format!("## {title}\n\n"));
    push_display_limit_note(out, records.len(), limit);
    out.push_str("| Model | Reason | Detail |\n|---|---|---|\n");
    for record in records.iter().take(limit) {
        out.push_str(&format!(
            "| `{}` | `{}` | {} |\n",
            escape_md(&record.model_name),
            escape_md(&record.reason),
            escape_md(record.detail.as_deref().unwrap_or(""))
        ));
    }
    out.push('\n');
}

fn push_trace_records(out: &mut String, records: &[TraceTriageRecord]) {
    out.push_str("## Worst Trace Models\n\n");
    out.push_str("| Model | Mean L1 | Max L1 | Dominant Shapes | Worst Channels |\n|---|---:|---:|---|---|\n");
    for record in records {
        out.push_str(&format!(
            "| `{}` | {} | {} | {} | {} |\n",
            escape_md(&record.model_name),
            format_optional_f64(record.mean_channel_bounded_normalized_l1),
            format_optional_f64(record.max_channel_bounded_normalized_l1),
            escape_md(&record.dominant_shapes.join(", ")),
            escape_md(&record.worst_channels.join(", "))
        ));
    }
    out.push('\n');
}

fn push_missing_traces(out: &mut String, records: &[MissingTraceRecord], limit: usize) {
    out.push_str("## Missing Trace Models\n\n");
    push_display_limit_note(out, records.len(), limit);
    out.push_str("| Model | Reason | Detail |\n|---|---|---|\n");
    for record in records.iter().take(limit) {
        out.push_str(&format!(
            "| `{}` | `{}` | {} |\n",
            escape_md(&record.model_name),
            escape_md(&record.reason),
            escape_md(record.detail.as_deref().unwrap_or(""))
        ));
    }
    out.push('\n');
}

fn push_display_limit_note(out: &mut String, total: usize, limit: usize) {
    if total > limit {
        out.push_str(&format!(
            "_Showing top {limit} of {total}. JSON output contains all records._\n\n"
        ));
    }
}

fn push_u64_row(out: &mut String, label: &str, value: Option<u64>) {
    out.push_str(&format!(
        "| {label} | {} |\n",
        value.map_or_else(|| "-".to_string(), |v| v.to_string())
    ));
}

fn push_usize_row(out: &mut String, label: &str, value: usize) {
    out.push_str(&format!("| {label} | {value} |\n"));
}

fn push_f64_row(out: &mut String, label: &str, value: Option<f64>) {
    out.push_str(&format!("| {label} | {} |\n", format_optional_f64(value)));
}

fn print_summary(paths: &InputPaths, report: &TriageReport) {
    println!("MSL triage report written:");
    println!("  json: {}", paths.output_json.display());
    println!("  markdown: {}", paths.output_md.display());
    println!(
        "  compile_failures={} balance_failures={} simulation_failures={} worst_trace_models={} missing_traces={}",
        report.compile_failures.len(),
        report.balance_failures.len(),
        report.simulation_failures.len(),
        report.worst_trace_models.len(),
        report.missing_trace_models.len()
    );
}

fn compare_triage_record(lhs: &TriageRecord, rhs: &TriageRecord) -> std::cmp::Ordering {
    compare_f64_desc(lhs.rank_score, rhs.rank_score)
        .then_with(|| lhs.reason.cmp(&rhs.reason))
        .then_with(|| lhs.model_name.cmp(&rhs.model_name))
}

fn compare_f64_desc(lhs: f64, rhs: f64) -> std::cmp::Ordering {
    rhs.partial_cmp(&lhs).unwrap_or(std::cmp::Ordering::Equal)
}

fn model_name(entry: &Value) -> String {
    get_str(entry, "model_name")
        .unwrap_or("unknown")
        .to_string()
}

fn phase(entry: &Value) -> Option<&str> {
    get_str(entry, "phase_reached")
}

fn get_str<'a>(value: &'a Value, key: &str) -> Option<&'a str> {
    value.get(key).and_then(Value::as_str)
}

fn get_bool(value: &Value, key: &str) -> Option<bool> {
    value.get(key).and_then(Value::as_bool)
}

fn get_i64(value: &Value, key: &str) -> Option<i64> {
    value.get(key).and_then(Value::as_i64)
}

fn get_u64(value: &Value, key: &str) -> Option<u64> {
    value.get(key).and_then(Value::as_u64)
}

fn get_f64(value: &Value, key: &str) -> Option<f64> {
    value.get(key).and_then(Value::as_f64)
}

fn get_nested_f64(value: &Value, path: &[&str]) -> Option<f64> {
    path.iter()
        .try_fold(value, |current, key| current.get(*key))
        .and_then(Value::as_f64)
}

fn truncate_detail(value: &str) -> String {
    const LIMIT: usize = 220;
    let value = value.replace('\n', " ");
    if value.len() <= LIMIT {
        return value;
    }
    format!("{}...", &value[..LIMIT])
}

fn reproduction_command(model_name: &str) -> String {
    format!("cargo run --bin xtask -- repo msl rerun --model '{model_name}'")
}

fn escape_md(value: &str) -> String {
    value.replace('|', "\\|").replace(['\n', '\r'], " ")
}

fn format_optional_f64(value: Option<f64>) -> String {
    value.map_or_else(|| "-".to_string(), |value| format!("{value:.6}"))
}

#[cfg(test)]
mod tests {
    use super::*;
    use serde_json::json;

    fn fixture_paths(dir: &Path) -> InputPaths {
        InputPaths {
            results_dir: dir.to_path_buf(),
            rumoca_results_file: dir.join("msl_results.json"),
            quality_file: dir.join("msl_quality_current.json"),
            trace_file: dir.join("sim_trace_comparison.json"),
            output_json: dir.join("msl_triage.json"),
            output_md: dir.join("msl_triage.md"),
        }
    }

    fn sample_rumoca_results() -> Value {
        json!({
            "compiled_models": 2,
            "balanced_models": 1,
            "sim_attempted": 2,
            "sim_ok": 1,
            "sim_success_rate": 0.5,
            "model_results": [
                {
                    "model_name": "Modelica.BadCompile",
                    "phase_reached": "Instantiate",
                    "error_code": "EI031",
                    "error": "recursive class instantiation"
                },
                {
                    "model_name": "Modelica.BadBalance",
                    "phase_reached": "Success",
                    "is_balanced": false,
                    "balance": -2,
                    "initial_balance_ok": true
                },
                {
                    "model_name": "Modelica.BadInit",
                    "phase_reached": "Success",
                    "is_balanced": true,
                    "initial_balance_ok": false,
                    "initial_balance_deficit_before": 3,
                    "initial_balance_deficit_after": 1
                },
                {
                    "model_name": "Modelica.BadSim",
                    "phase_reached": "Success",
                    "is_balanced": true,
                    "initial_balance_ok": true,
                    "sim_status": "sim_solver_fail",
                    "sim_error": "Step size is too small at time = 0.1"
                },
                {
                    "model_name": "Modelica.Good",
                    "phase_reached": "Success",
                    "is_balanced": true,
                    "initial_balance_ok": true,
                    "sim_status": "sim_ok"
                }
            ]
        })
    }

    fn sample_trace_results() -> Value {
        json!({
            "agreement_bands_percent": {
                "high_agreement": 10.0,
                "minor_agreement": 20.0
            },
            "summary": {
                "bad_channels_percent": 40.0,
                "severe_channels_percent": 1.0,
                "mean_model_mean_channel_bounded_normalized_l1": 0.12
            },
            "worst_models": [
                {
                    "model_name": "Modelica.TraceA",
                    "mean_channel_bounded_normalized_l1": 0.5,
                    "max_channel_bounded_normalized_l1": 0.9,
                    "bounded_normalized_l1_score": 0.4,
                    "compared_variables": 3,
                    "worst_variables": [
                        { "name": "x", "shape": "event_time_mismatch" },
                        { "name": "y", "shape": "constant_offset" }
                    ]
                }
            ],
            "missing_trace": {
                "Modelica.MissingTrace": "missing rumoca trace"
            }
        })
    }

    #[test]
    fn build_report_groups_compile_balance_simulation_and_trace_work() {
        let temp = tempfile::tempdir().expect("tempdir");
        let paths = fixture_paths(temp.path());
        let report = build_report(
            &paths,
            &sample_rumoca_results(),
            None,
            Some(&sample_trace_results()),
            20,
        );

        assert_eq!(report.compile_failures.len(), 1);
        assert_eq!(report.compile_failures[0].reason, "compile.instantiate");
        assert_eq!(
            report.compile_failures[0].error_code.as_deref(),
            Some("EI031")
        );
        assert_eq!(report.balance_failures.len(), 2);
        assert!(
            report
                .balance_failures
                .iter()
                .any(|record| record.reason == "balance.underdetermined")
        );
        assert_eq!(report.simulation_failures.len(), 1);
        assert_eq!(report.simulation_failures[0].reason, "sim.solver");
        assert_eq!(report.worst_trace_models[0].model_name, "Modelica.TraceA");
        assert_eq!(
            report.worst_trace_models[0].dominant_shapes,
            vec!["constant_offset", "event_time_mismatch"]
        );
        assert_eq!(
            report.worst_trace_models[0].worst_channels,
            vec!["x [event_time_mismatch]", "y [constant_offset]"]
        );
        assert_eq!(
            report.missing_trace_models[0].model_name,
            "Modelica.MissingTrace"
        );
        assert_eq!(
            report.missing_trace_models[0].reason,
            "trace.missing_channel"
        );
        assert_eq!(
            report.missing_trace_models[0].detail.as_deref(),
            Some("missing rumoca trace")
        );
        assert_eq!(report.summary.trace_high_plus_near_percent, Some(30.0));
        assert_eq!(report.taxonomy_coverage.unknown_records, 0);
        assert_eq!(report.taxonomy_coverage.classified_percent, 100.0);
        assert_eq!(report.reason_counts.get("trace.missing_channel"), Some(&1));
    }
    #[test]
    fn compile_taxonomy_uses_phase_bucket_and_preserves_error_code() {
        assert_eq!(
            classify_compile_reason(Some("Instantiate"), Some("EI031")),
            "compile.instantiate"
        );
        assert_eq!(
            classify_compile_reason(Some("ToDae"), Some("ED007")),
            "compile.dae"
        );
        assert_eq!(
            classify_compile_reason(Some("Parse"), Some("EP001")),
            "compile.parse"
        );
        assert_eq!(
            classify_compile_reason(Some("Unexpected"), None),
            "compile.unknown"
        );
    }

    #[test]
    fn compile_phase_uses_error_detail_when_phase_reached_is_stale() {
        let phase = classify_compile_phase(
            Some("Resolve"),
            Some("Modelica.A failed in ToDae: invalid Appendix B discrete solved form"),
        );

        assert_eq!(phase.as_deref(), Some("ToDae"));
        assert_eq!(
            classify_compile_reason(phase.as_deref(), None),
            "compile.dae"
        );
    }

    #[test]
    fn write_outputs_writes_json_and_markdown_reports() {
        let temp = tempfile::tempdir().expect("tempdir");
        let paths = fixture_paths(temp.path());
        let report = build_report(
            &paths,
            &sample_rumoca_results(),
            None,
            Some(&sample_trace_results()),
            20,
        );

        write_outputs(&paths, &report).expect("write reports");

        let json_text = fs::read_to_string(&paths.output_json).expect("read json");
        assert!(json_text.contains("Modelica.BadCompile"));
        let md_text = fs::read_to_string(&paths.output_md).expect("read markdown");
        assert!(md_text.contains("## Compile Failures"));
        assert!(md_text.contains("Modelica.TraceA"));
    }

    #[test]
    fn json_keeps_all_records_when_markdown_is_limited() {
        let temp = tempfile::tempdir().expect("tempdir");
        let paths = fixture_paths(temp.path());
        let rumoca = json!({
            "model_results": [
                {
                    "model_name": "Modelica.BadSimA",
                    "phase_reached": "Success",
                    "sim_status": "sim_solver_fail",
                    "sim_error": "solver failed"
                },
                {
                    "model_name": "Modelica.BadSimB",
                    "phase_reached": "Success",
                    "sim_status": "sim_timeout",
                    "sim_error": "timeout"
                }
            ]
        });
        let report = build_report(&paths, &rumoca, None, None, 1);

        assert_eq!(report.simulation_failures.len(), 2);
        let markdown = render_markdown(&report);
        assert!(markdown.contains("Showing top 1 of 2"));
        assert!(markdown.contains("Modelica.BadSimB"));
        assert!(!markdown.contains("Modelica.BadSimA"));
    }
}
