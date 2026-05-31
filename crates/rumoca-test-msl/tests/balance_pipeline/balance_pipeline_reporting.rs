use super::*;
use rumoca_sim::sim_trace_compare::{
    AgreementBand, MODEL_HIGH_MAX_DEVIATION_CHANNEL_SHARE, MODEL_HIGH_MIN_HIGH_CHANNEL_SHARE,
    MODEL_MINOR_MAX_DEVIATION_CHANNEL_SHARE, MODEL_MINOR_MIN_HIGH_PLUS_MINOR_CHANNEL_SHARE,
    ModelDeviationMetric, classify_trace_metric_channel_distribution,
};
use std::collections::{BTreeMap, BTreeSet};

// =============================================================================
// Result JSON write + balance summary printing
// =============================================================================

#[derive(Serialize)]
struct MslTargetModelList<'a> {
    git_commit: &'a str,
    msl_version: &'a str,
    selection_kind: &'a str,
    model_count: usize,
    model_names: &'a [String],
    #[serde(skip_serializing_if = "Vec::is_empty")]
    records: Vec<MslTargetModelRecord<'a>>,
}

#[derive(Serialize)]
struct MslTargetModelRecord<'a> {
    model_name: &'a str,
    scheduled_index: usize,
    schedule_lane: usize,
    #[serde(skip_serializing_if = "Option::is_none")]
    num_states: Option<usize>,
    #[serde(skip_serializing_if = "Option::is_none")]
    num_algebraics: Option<usize>,
    #[serde(skip_serializing_if = "Option::is_none")]
    num_f_x: Option<usize>,
    #[serde(skip_serializing_if = "Option::is_none")]
    scalar_equations: Option<usize>,
    #[serde(skip_serializing_if = "Option::is_none")]
    scalar_unknowns: Option<usize>,
}

#[derive(Debug, Clone, Serialize)]
struct MslPackagePassRateRow {
    package: String,
    n: usize,
    parse_passed: usize,
    flatten_passed: usize,
    dae_passed: usize,
    solve_passed: usize,
    ic_passed: usize,
    sim_passed: usize,
    parse_percent: f64,
    flatten_percent: f64,
    dae_percent: f64,
    solve_percent: f64,
    ic_percent: f64,
    sim_percent: f64,
    parse_avg_seconds: Option<f64>,
    flatten_avg_seconds: Option<f64>,
    dae_avg_seconds: Option<f64>,
    solve_avg_seconds: Option<f64>,
    ic_avg_seconds: Option<f64>,
    sim_avg_seconds: Option<f64>,
}

#[derive(Serialize)]
struct MslPackagePassRateReport {
    git_commit: String,
    msl_version: String,
    selection_kind: String,
    selection_pattern: String,
    model_count: usize,
    rows: Vec<MslPackagePassRateRow>,
    overall: MslPackagePassRateRow,
}

#[derive(Default)]
struct MslPackagePassRateCounts {
    n: usize,
    parse_passed: usize,
    flatten_passed: usize,
    dae_passed: usize,
    solve_passed: usize,
    ic_passed: usize,
    sim_passed: usize,
    flatten_seconds: f64,
    flatten_timed: usize,
    dae_seconds: f64,
    dae_timed: usize,
    solve_seconds: f64,
    solve_timed: usize,
    ic_seconds: f64,
    ic_timed: usize,
    sim_seconds: f64,
    sim_timed: usize,
}

#[derive(Default)]
struct MslPackagePassRateParity {
    models: BTreeMap<String, MslPackagePassRateParityModel>,
    accepted_sim_ok_without_trace: BTreeSet<String>,
}

#[derive(Clone, Copy)]
struct MslPackagePassRateParityModel {
    ic_matches: Option<bool>,
    sim_matches: Option<bool>,
}

#[derive(Debug, Clone, Serialize)]
struct MslPackageTraceAccuracyRow {
    package: String,
    n: usize,
    compared: usize,
    high_agreement: usize,
    near_agreement: usize,
    acceptable_agreement: usize,
    deviation: usize,
    no_severe_models: usize,
    bad_channels: usize,
    severe_channels: usize,
    compared_percent: f64,
    high_percent: f64,
    near_percent: f64,
    trace_percent: f64,
    deviation_percent: f64,
    channel_ok_percent: f64,
    no_severe_percent: f64,
}

#[derive(Serialize)]
struct MslPackageTraceAccuracyReport {
    selection_pattern: String,
    model_count: usize,
    rows: Vec<MslPackageTraceAccuracyRow>,
    overall: MslPackageTraceAccuracyRow,
}

#[derive(Default)]
struct MslPackageTraceAccuracyCounts {
    n: usize,
    compared: usize,
    high_agreement: usize,
    near_agreement: usize,
    deviation: usize,
    no_severe_models: usize,
    compared_channels: usize,
    bad_channels: usize,
    severe_channels: usize,
}

#[derive(Debug, Default, Clone, Serialize, PartialEq, Eq)]
pub(super) struct MlsContractCategoryCoverage {
    models: usize,
    compiled: usize,
    solve_ir: usize,
    balanced: usize,
    sim_ok: usize,
    phase_counts: BTreeMap<String, usize>,
    error_code_counts: BTreeMap<String, usize>,
}

#[derive(Debug, Clone, Serialize)]
struct MlsContractCoverageRow {
    category: String,
    models: usize,
    compiled: usize,
    solve_ir: usize,
    balanced: usize,
    sim_ok: usize,
    phase_counts: BTreeMap<String, usize>,
    error_code_counts: BTreeMap<String, usize>,
}

#[derive(Debug, Clone, Serialize)]
struct MlsContractCoverageReport {
    git_commit: String,
    msl_version: String,
    selection_kind: String,
    model_count: usize,
    rows: Vec<MlsContractCoverageRow>,
}

fn compile_target_lookup(summary: &MslSummary) -> HashMap<&str, &MslModelResult> {
    summary
        .model_results
        .iter()
        .map(|result| (result.model_name.as_str(), result))
        .collect()
}

fn collect_compile_target_models(summary: &MslSummary) -> Vec<String> {
    let mut compile_target_models = Vec::new();
    let mut seen_compile_targets = HashSet::new();
    for result in summary
        .model_results
        .iter()
        .filter(|result| result.phase_reached != "NonSim")
    {
        if seen_compile_targets.insert(result.model_name.clone()) {
            compile_target_models.push(result.model_name.clone());
        }
    }
    compile_target_models
}

fn build_target_records<'a>(
    model_names: &'a [String],
    target_lane_count: usize,
    target_lookup: &HashMap<&'a str, &'a MslModelResult>,
) -> Vec<MslTargetModelRecord<'a>> {
    model_names
        .iter()
        .enumerate()
        .map(|(scheduled_index, model_name)| {
            let metrics = target_lookup.get(model_name.as_str()).copied();
            MslTargetModelRecord {
                model_name,
                scheduled_index,
                schedule_lane: scheduled_index % target_lane_count,
                num_states: metrics.and_then(|result| result.num_states),
                num_algebraics: metrics.and_then(|result| result.num_algebraics),
                num_f_x: metrics.and_then(|result| result.num_f_x),
                scalar_equations: metrics.and_then(|result| result.scalar_equations),
                scalar_unknowns: metrics.and_then(|result| result.scalar_unknowns),
            }
        })
        .collect()
}

fn write_target_model_list(
    results_dir: &Path,
    file_name: &str,
    list: &MslTargetModelList<'_>,
) -> io::Result<()> {
    let json = serde_json::to_string_pretty(list)?;
    let mut file = File::create(results_dir.join(file_name))?;
    file.write_all(json.as_bytes())
}

fn root_msl_package_name(model_name: &str) -> Option<String> {
    if !is_root_msl_example_model_name(model_name) {
        return None;
    }
    let rest = model_name.strip_prefix("Modelica.")?;
    let (package_name, _) = rest.split_once(".Examples.")?;
    (!package_name.is_empty()).then(|| package_name.to_string())
}

fn result_reached_flatten_output(result: &MslModelResult) -> bool {
    matches!(result.phase_reached.as_str(), "ToDae" | "Success")
}

fn result_reached_dae_output(result: &MslModelResult) -> bool {
    result.phase_reached == "Success"
}

fn result_reached_solve_output(result: &MslModelResult) -> bool {
    result.ir_solve_file.is_some()
        || result.ir_solve_seconds.is_some()
        || matches!(
            result.timeout_phase,
            Some(
                rumoca_worker::WorkerProgressPhase::SimBuild
                    | rumoca_worker::WorkerProgressPhase::IC
                    | rumoca_worker::WorkerProgressPhase::Sim,
            )
        )
}

fn result_simulated_successfully(result: &MslModelResult) -> bool {
    result_solved_initial_conditions(result) && result.sim_status.as_deref() == Some("sim_ok")
}

fn result_solved_initial_conditions(result: &MslModelResult) -> bool {
    result_reached_solve_output(result)
        && (result.ic_status.as_deref() == Some("ic_ok")
            || matches!(
                result.timeout_phase,
                Some(rumoca_worker::WorkerProgressPhase::Sim)
            ))
}

fn rounded_percent(passed: usize, total: usize) -> f64 {
    if total == 0 {
        0.0
    } else {
        ((passed as f64 / total as f64) * 100.0).round()
    }
}

fn percent_cell(passed: usize, total: usize) -> String {
    format!("{:.0}%", rounded_percent(passed, total))
}

fn count_map_cell(counts: &BTreeMap<String, usize>) -> String {
    if counts.is_empty() {
        return "-".to_string();
    }
    counts
        .iter()
        .map(|(key, count)| format!("{key}:{count}"))
        .collect::<Vec<_>>()
        .join(", ")
}

fn mls_contract_category(result: &MslModelResult) -> &'static str {
    let name = result.model_name.to_ascii_lowercase();
    let error_code = result
        .error_code
        .as_deref()
        .unwrap_or_default()
        .to_ascii_lowercase();
    let text = result
        .error
        .as_deref()
        .unwrap_or_default()
        .to_ascii_lowercase();
    if name.contains("array")
        || name.contains("matrix")
        || name.contains("vector")
        || error_code.contains("array")
        || text.contains("array")
    {
        return "ARR";
    }
    if name.contains("connect")
        || name.contains("bus")
        || name.contains("fluid")
        || name.contains("electrical")
        || name.contains("mechanics")
        || error_code.contains("connector")
        || text.contains("connect")
    {
        return "CONN_STRM";
    }
    if name.contains("function")
        || name.contains("external")
        || name.contains("table")
        || text.contains("function")
    {
        return "FUNC";
    }
    if name.contains("initial")
        || name.contains("event")
        || name.contains("when")
        || name.contains("reinit")
        || text.contains("initial")
        || text.contains("event")
    {
        return "EQN_ALG_SIM";
    }
    if name.contains("clock") || name.contains("stategraph") || name.contains("statemachine") {
        return "CLK_SM";
    }
    if name.contains("type")
        || name.contains("record")
        || name.contains("enum")
        || text.contains("record")
    {
        return "DECL_TYPE";
    }
    if name.contains("package") || name.contains("import") || text.contains("import") {
        return "PKG";
    }
    "OTHER"
}

pub(super) fn build_mls_contract_coverage(
    summary: &MslSummary,
) -> BTreeMap<String, MlsContractCategoryCoverage> {
    let mut categories = BTreeMap::new();
    for result in &summary.model_results {
        let entry = categories
            .entry(mls_contract_category(result).to_string())
            .or_insert_with(MlsContractCategoryCoverage::default);
        entry.models += 1;
        *entry
            .phase_counts
            .entry(result.phase_reached.clone())
            .or_default() += 1;
        if matches!(result.phase_reached.as_str(), "ToDae" | "Success") {
            entry.compiled += 1;
        }
        if result.ir_solve_file.is_some() || result.ir_solve_seconds.is_some() {
            entry.solve_ir += 1;
        }
        if result.is_balanced == Some(true) {
            entry.balanced += 1;
        }
        if result.sim_status.as_deref() == Some("sim_ok") {
            entry.sim_ok += 1;
        }
        if let Some(error_code) = result.error_code.as_deref() {
            *entry
                .error_code_counts
                .entry(error_code.to_string())
                .or_default() += 1;
        }
    }
    categories
}

fn build_mls_contract_coverage_report(summary: &MslSummary) -> MlsContractCoverageReport {
    let coverage = build_mls_contract_coverage(summary);
    let rows = coverage
        .into_iter()
        .map(|(category, coverage)| MlsContractCoverageRow {
            category,
            models: coverage.models,
            compiled: coverage.compiled,
            solve_ir: coverage.solve_ir,
            balanced: coverage.balanced,
            sim_ok: coverage.sim_ok,
            phase_counts: coverage.phase_counts,
            error_code_counts: coverage.error_code_counts,
        })
        .collect::<Vec<_>>();
    MlsContractCoverageReport {
        git_commit: summary.git_commit.clone(),
        msl_version: summary.msl_version.clone(),
        selection_kind: msl_target_scope().as_str().to_string(),
        model_count: summary.model_results.len(),
        rows,
    }
}

fn format_mls_contract_coverage_markdown(report: &MlsContractCoverageReport) -> String {
    let mut markdown = String::new();
    markdown
        .push_str("| MLS Category | n | Compile | Solve IR | Balance | Sim | Phases | Errors |\n");
    markdown.push_str("|---|---:|---:|---:|---:|---:|---|---|\n");
    for row in &report.rows {
        markdown.push_str(&format!(
            "| {} | {} | {} | {} | {} | {} | {} | {} |\n",
            row.category,
            row.models,
            percent_cell(row.compiled, row.models),
            percent_cell(row.solve_ir, row.models),
            percent_cell(row.balanced, row.models),
            percent_cell(row.sim_ok, row.models),
            count_map_cell(&row.phase_counts),
            count_map_cell(&row.error_code_counts)
        ));
    }
    markdown
}

fn format_mls_contract_coverage_terminal_table(report: &MlsContractCoverageReport) -> String {
    let mut table = String::new();
    table.push_str(&format!(
        "{:<14} {:>4} {:>7} {:>8} {:>7} {:>6}  {:<24} {}\n",
        "MLS Category", "n", "Compile", "Solve", "Balance", "Sim", "Phases", "Errors"
    ));
    table.push_str(&format!(
        "{:-<14} {:-<4} {:-<7} {:-<8} {:-<7} {:-<6}  {:-<24} {:-<1}\n",
        "", "", "", "", "", "", "", ""
    ));
    for row in &report.rows {
        table.push_str(&format!(
            "{:<14} {:>4} {:>7} {:>8} {:>7} {:>6}  {:<24} {}\n",
            row.category,
            row.models,
            percent_cell(row.compiled, row.models),
            percent_cell(row.solve_ir, row.models),
            percent_cell(row.balanced, row.models),
            percent_cell(row.sim_ok, row.models),
            count_map_cell(&row.phase_counts),
            count_map_cell(&row.error_code_counts)
        ));
    }
    table
}

fn write_mls_contract_coverage_report(
    results_dir: &Path,
    report: &MlsContractCoverageReport,
) -> io::Result<()> {
    let json = serde_json::to_string_pretty(report)?;
    let mut json_file = File::create(results_dir.join("mls_contract_coverage.json"))?;
    json_file.write_all(json.as_bytes())?;

    let markdown = format_mls_contract_coverage_markdown(report);
    let mut markdown_file = File::create(results_dir.join("mls_contract_coverage.md"))?;
    markdown_file.write_all(markdown.as_bytes())?;

    let terminal_table = format_mls_contract_coverage_terminal_table(report);
    let mut text_file = File::create(results_dir.join("mls_contract_coverage.txt"))?;
    text_file.write_all(terminal_table.as_bytes())?;

    println!("\n=== MLS Contract Coverage ===");
    print!("{terminal_table}");
    Ok(())
}

fn finite_nonnegative_seconds(seconds: Option<f64>) -> Option<f64> {
    seconds.filter(|value| value.is_finite() && *value >= 0.0)
}

fn avg_seconds(total_seconds: f64, count: usize) -> Option<f64> {
    (count > 0).then(|| total_seconds / count as f64)
}

fn pass_rate_row(
    package: String,
    counts: MslPackagePassRateCounts,
    parse_avg_seconds: Option<f64>,
) -> MslPackagePassRateRow {
    MslPackagePassRateRow {
        package,
        n: counts.n,
        parse_passed: counts.parse_passed,
        flatten_passed: counts.flatten_passed,
        dae_passed: counts.dae_passed,
        solve_passed: counts.solve_passed,
        ic_passed: counts.ic_passed,
        sim_passed: counts.sim_passed,
        parse_percent: rounded_percent(counts.parse_passed, counts.n),
        flatten_percent: rounded_percent(counts.flatten_passed, counts.n),
        dae_percent: rounded_percent(counts.dae_passed, counts.n),
        solve_percent: rounded_percent(counts.solve_passed, counts.n),
        ic_percent: rounded_percent(counts.ic_passed, counts.n),
        sim_percent: rounded_percent(counts.sim_passed, counts.n),
        parse_avg_seconds,
        flatten_avg_seconds: avg_seconds(counts.flatten_seconds, counts.flatten_timed),
        dae_avg_seconds: avg_seconds(counts.dae_seconds, counts.dae_timed),
        solve_avg_seconds: avg_seconds(counts.solve_seconds, counts.solve_timed),
        ic_avg_seconds: avg_seconds(counts.ic_seconds, counts.ic_timed),
        sim_avg_seconds: avg_seconds(counts.sim_seconds, counts.sim_timed),
    }
}

fn build_msl_package_pass_rate_report(summary: &MslSummary) -> MslPackagePassRateReport {
    build_msl_package_pass_rate_report_with_parity(summary, None)
}

fn build_msl_package_pass_rate_report_with_trace_payload(
    summary: &MslSummary,
    payload: &serde_json::Value,
) -> io::Result<MslPackagePassRateReport> {
    let parity = pass_rate_parity_from_trace_payload(payload)?;
    Ok(build_msl_package_pass_rate_report_with_parity(
        summary,
        Some(&parity),
    ))
}

fn build_msl_package_pass_rate_report_with_parity(
    summary: &MslSummary,
    parity: Option<&MslPackagePassRateParity>,
) -> MslPackagePassRateReport {
    let mut by_package = BTreeMap::<String, MslPackagePassRateCounts>::new();
    let mut overall_counts = MslPackagePassRateCounts::default();
    for result in summary.model_results.iter() {
        let Some(package_name) = root_msl_package_name(&result.model_name) else {
            continue;
        };
        let counts = by_package.entry(package_name).or_default();
        add_result_to_pass_rate_counts(counts, result, parity);
        add_result_to_pass_rate_counts(&mut overall_counts, result, parity);
    }

    let parse_avg_seconds = avg_seconds(summary.timings.parse_seconds, overall_counts.n);

    let rows = by_package
        .into_iter()
        .map(|(package, counts)| pass_rate_row(package, counts, parse_avg_seconds))
        .collect::<Vec<_>>();
    let overall = pass_rate_row("Overall".to_string(), overall_counts, parse_avg_seconds);

    MslPackagePassRateReport {
        git_commit: summary.git_commit.clone(),
        msl_version: summary.msl_version.clone(),
        selection_kind: msl_target_scope().as_str().to_string(),
        selection_pattern: ROOT_MSL_EXAMPLE_SELECTION_PATTERN.to_string(),
        model_count: overall.n,
        rows,
        overall,
    }
}

fn pass_rate_parity_from_trace_payload(
    payload: &serde_json::Value,
) -> io::Result<MslPackagePassRateParity> {
    let models = payload
        .get("models")
        .and_then(serde_json::Value::as_object)
        .ok_or_else(|| {
            io::Error::other("trace comparison JSON is missing object field 'models'")
        })?;
    let mut parity = MslPackagePassRateParity::default();
    for (model_name, model_payload) in models {
        let mut metric = serde_json::from_value::<ModelDeviationMetric>(model_payload.clone())
            .map_err(|error| {
                io::Error::other(format!(
                    "failed to parse trace metric for {model_name}: {error}"
                ))
            })?;
        if metric.model_name.is_empty() {
            metric.model_name.clone_from(model_name);
        }
        parity.models.insert(
            metric.model_name.clone(),
            MslPackagePassRateParityModel {
                ic_matches: (metric.initial_condition.channels_compared > 0)
                    .then_some(metric.initial_condition.deviation_count == 0),
                sim_matches: trace_metric_has_data(&metric)
                    .then(|| trace_metric_matches_omc(&metric)),
            },
        );
    }
    parity.accepted_sim_ok_without_trace = accepted_sim_ok_without_trace(payload);
    Ok(parity)
}

fn accepted_sim_ok_without_trace(payload: &serde_json::Value) -> BTreeSet<String> {
    payload
        .get("skipped")
        .and_then(serde_json::Value::as_object)
        .into_iter()
        .flat_map(|skipped| skipped.iter())
        .filter(|(_, reason)| reason.as_str().is_some_and(trace_skip_accepts_sim_ok))
        .map(|(model_name, _)| model_name.clone())
        .collect()
}

fn trace_skip_accepts_sim_ok(reason: &str) -> bool {
    reason.contains("trace has no comparable variable samples")
}

fn trace_metric_matches_omc(metric: &ModelDeviationMetric) -> bool {
    matches!(
        classify_trace_metric_channel_distribution(
            metric,
            MODEL_HIGH_MIN_HIGH_CHANNEL_SHARE,
            MODEL_HIGH_MAX_DEVIATION_CHANNEL_SHARE,
            MODEL_MINOR_MIN_HIGH_PLUS_MINOR_CHANNEL_SHARE,
            MODEL_MINOR_MAX_DEVIATION_CHANNEL_SHARE,
        ),
        AgreementBand::HighAgreement | AgreementBand::MinorAgreement
    )
}

fn trace_metric_has_data(metric: &ModelDeviationMetric) -> bool {
    metric.compared_variables > 0 && metric.samples_compared > 0
}

fn add_result_to_pass_rate_counts(
    counts: &mut MslPackagePassRateCounts,
    result: &MslModelResult,
    parity: Option<&MslPackagePassRateParity>,
) {
    counts.n += 1;
    counts.parse_passed += 1;
    let parity_model = parity.and_then(|parity| parity.models.get(&result.model_name));
    let ic_passed = parity_model
        .and_then(|model| model.ic_matches)
        .unwrap_or_else(|| result_solved_initial_conditions(result));
    let sim_passed = sim_passed_with_trace_parity(result, parity_model, parity);
    if result_reached_flatten_output(result) {
        counts.flatten_passed += 1;
    }
    if result_reached_dae_output(result) {
        counts.dae_passed += 1;
    }
    if result_reached_solve_output(result) {
        counts.solve_passed += 1;
    }
    if ic_passed {
        counts.ic_passed += 1;
    }
    if sim_passed {
        counts.sim_passed += 1;
    }
    if let Some(seconds) = stage_time_or_timeout(
        result_reached_flatten_output(result),
        result.flatten_seconds,
        result,
        rumoca_worker::WorkerProgressPhase::Flatten,
    ) {
        counts.flatten_seconds += seconds;
        counts.flatten_timed += 1;
    }
    if let Some(seconds) = stage_time_or_timeout(
        result_reached_dae_output(result),
        result.dae_seconds,
        result,
        rumoca_worker::WorkerProgressPhase::ToDae,
    ) {
        counts.dae_seconds += seconds;
        counts.dae_timed += 1;
    }
    if let Some(seconds) = stage_time_or_timeout(
        result_reached_solve_output(result),
        result.ir_solve_seconds,
        result,
        rumoca_worker::WorkerProgressPhase::Solve,
    ) {
        counts.solve_seconds += seconds;
        counts.solve_timed += 1;
    }
    if let Some(seconds) = stage_time_or_timeout(
        ic_passed,
        result.ic_seconds,
        result,
        rumoca_worker::WorkerProgressPhase::IC,
    ) {
        counts.ic_seconds += seconds;
        counts.ic_timed += 1;
    }
    if let Some(seconds) = stage_time_or_timeout(
        sim_passed,
        result.sim_wall_seconds,
        result,
        rumoca_worker::WorkerProgressPhase::Sim,
    ) {
        counts.sim_seconds += seconds;
        counts.sim_timed += 1;
    }
}

fn sim_passed_with_trace_parity(
    result: &MslModelResult,
    parity_model: Option<&MslPackagePassRateParityModel>,
    parity: Option<&MslPackagePassRateParity>,
) -> bool {
    if let Some(sim_matches) = parity_model.and_then(|model| model.sim_matches) {
        return sim_matches;
    }
    let Some(parity) = parity else {
        return result_simulated_successfully(result);
    };
    parity
        .accepted_sim_ok_without_trace
        .contains(&result.model_name)
        && result_simulated_successfully(result)
}

fn stage_time_or_timeout(
    completed: bool,
    seconds: Option<f64>,
    result: &MslModelResult,
    phase: rumoca_worker::WorkerProgressPhase,
) -> Option<f64> {
    if completed && let Some(seconds) = finite_nonnegative_seconds(seconds) {
        return Some(seconds);
    }
    if result.timeout_phase == Some(phase) {
        return finite_nonnegative_seconds(result.timeout_seconds);
    }
    None
}

fn write_msl_package_pass_rate_report(
    results_dir: &Path,
    report: &MslPackagePassRateReport,
    print_table: bool,
) -> io::Result<()> {
    let json = serde_json::to_string_pretty(report)?;
    let mut json_file = File::create(results_dir.join("msl_package_pass_rates.json"))?;
    json_file.write_all(json.as_bytes())?;

    let markdown = format_msl_package_pass_rate_markdown(report);
    let mut markdown_file = File::create(results_dir.join("msl_package_pass_rates.md"))?;
    markdown_file.write_all(markdown.as_bytes())?;

    let terminal_table = format_msl_package_pass_rate_terminal_table(report);
    let mut text_file = File::create(results_dir.join("msl_package_pass_rates.txt"))?;
    text_file.write_all(terminal_table.as_bytes())?;

    let compact_table = format_msl_package_pass_rate_compact_table(report);
    let mut compact_file = File::create(results_dir.join("msl_package_pass_rates_compact.txt"))?;
    compact_file.write_all(compact_table.as_bytes())?;

    if print_table {
        println!("\n=== MSL Package Pass Rates ===");
        print!("{terminal_table}");
    }
    Ok(())
}

fn format_msl_package_pass_rate_markdown(report: &MslPackagePassRateReport) -> String {
    let mut markdown = String::new();
    markdown.push_str("| MSL Package | n | Ast | Time | Flat | Time | Dae | Time | Solve | Time | IC | Time | Sim | Time |\n");
    markdown.push_str("|---|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|\n");
    for row in &report.rows {
        append_pass_rate_markdown_row(&mut markdown, row);
    }
    append_pass_rate_markdown_row(&mut markdown, &report.overall);
    markdown
}

fn append_pass_rate_markdown_row(markdown: &mut String, row: &MslPackagePassRateRow) {
    markdown.push_str(&format!(
        "| {} | {} | {} | {} | {} | {} | {} | {} | {} | {} | {} | {} | {} | {} |\n",
        row.package,
        row.n,
        percent_cell(row.parse_passed, row.n),
        avg_time_cell(row.parse_avg_seconds),
        percent_cell(row.flatten_passed, row.n),
        avg_time_cell(row.flatten_avg_seconds),
        percent_cell(row.dae_passed, row.n),
        avg_time_cell(row.dae_avg_seconds),
        percent_cell(row.solve_passed, row.n),
        avg_time_cell(row.solve_avg_seconds),
        percent_cell(row.ic_passed, row.n),
        avg_time_cell(row.ic_avg_seconds),
        percent_cell(row.sim_passed, row.n),
        avg_time_cell(row.sim_avg_seconds)
    ));
}

fn avg_time_cell(avg_seconds: Option<f64>) -> String {
    match avg_seconds {
        Some(value) if value.is_finite() => format!("{value:.2}s"),
        _ => "-".to_string(),
    }
}

fn format_msl_package_pass_rate_terminal_table(report: &MslPackagePassRateReport) -> String {
    let widths = PassRateTerminalWidths::from_report(report);
    let mut table = String::new();
    append_pass_rate_terminal_header(&mut table, &widths);
    for row in &report.rows {
        append_pass_rate_terminal_row(&mut table, row, &widths);
    }
    append_pass_rate_terminal_separator(&mut table, &widths);
    append_pass_rate_terminal_row(&mut table, &report.overall, &widths);
    table
}

#[derive(Clone, Copy)]
struct PassRateTerminalWidths {
    package: usize,
    count: usize,
    parse: StageColumnWidths,
    flatten: StageColumnWidths,
    dae: StageColumnWidths,
    solve: StageColumnWidths,
    ic: StageColumnWidths,
    sim: StageColumnWidths,
}

#[derive(Clone, Copy)]
struct StageColumnWidths {
    percent: usize,
    time: usize,
}

impl PassRateTerminalWidths {
    fn from_report(report: &MslPackagePassRateReport) -> Self {
        let rows = pass_rate_rows_with_overall(report);
        Self {
            package: rows
                .iter()
                .map(|row| row.package.len())
                .chain(std::iter::once("MSL Package".len()))
                .max()
                .unwrap_or("MSL Package".len())
                + 1,
            count: rows
                .iter()
                .map(|row| row.n.to_string().len())
                .chain(std::iter::once("n".len()))
                .max()
                .unwrap_or("n".len()),
            parse: stage_widths(
                "Ast",
                rows.iter().map(|row| {
                    (
                        percent_cell(row.parse_passed, row.n),
                        avg_time_cell(row.parse_avg_seconds),
                    )
                }),
            ),
            flatten: stage_widths(
                "Flat",
                rows.iter().map(|row| {
                    (
                        percent_cell(row.flatten_passed, row.n),
                        avg_time_cell(row.flatten_avg_seconds),
                    )
                }),
            ),
            dae: stage_widths(
                "Dae",
                rows.iter().map(|row| {
                    (
                        percent_cell(row.dae_passed, row.n),
                        avg_time_cell(row.dae_avg_seconds),
                    )
                }),
            ),
            solve: stage_widths(
                "Solve",
                rows.iter().map(|row| {
                    (
                        percent_cell(row.solve_passed, row.n),
                        avg_time_cell(row.solve_avg_seconds),
                    )
                }),
            ),
            ic: stage_widths(
                "IC",
                rows.iter().map(|row| {
                    (
                        percent_cell(row.ic_passed, row.n),
                        avg_time_cell(row.ic_avg_seconds),
                    )
                }),
            ),
            sim: stage_widths(
                "Sim",
                rows.iter().map(|row| {
                    (
                        percent_cell(row.sim_passed, row.n),
                        avg_time_cell(row.sim_avg_seconds),
                    )
                }),
            ),
        }
    }
}

fn pass_rate_rows_with_overall(report: &MslPackagePassRateReport) -> Vec<&MslPackagePassRateRow> {
    report
        .rows
        .iter()
        .chain(std::iter::once(&report.overall))
        .collect()
}

fn stage_widths(
    label: &str,
    cells: impl IntoIterator<Item = (String, String)>,
) -> StageColumnWidths {
    let mut percent = label.len();
    let mut time = PASS_RATE_TERMINAL_MIN_TIME_WIDTH;
    for (percent_cell, time_cell) in cells {
        percent = percent.max(percent_cell.len());
        time = time.max(time_cell.len());
    }
    StageColumnWidths { percent, time }
}

const PASS_RATE_TERMINAL_MIN_TIME_WIDTH: usize = "10.00s".len();

fn append_pass_rate_terminal_header(table: &mut String, widths: &PassRateTerminalWidths) {
    table.push_str(&format!(
        "{:<package_width$}{:>count_width$} | {} | {} | {} | {} | {} | {}\n",
        "MSL Package",
        "n",
        stage_header("Ast", widths.parse),
        stage_header("Flat", widths.flatten),
        stage_header("Dae", widths.dae),
        stage_header("Solve", widths.solve),
        stage_header("IC", widths.ic),
        stage_header("Sim", widths.sim),
        package_width = widths.package,
        count_width = widths.count,
    ));
    append_pass_rate_terminal_separator(table, widths);
}

fn append_pass_rate_terminal_separator(table: &mut String, widths: &PassRateTerminalWidths) {
    table.push_str(&format!(
        "{:-<package_width$}{:-<count_width$}-+-{}-+-{}-+-{}-+-{}-+-{}-+-{}\n",
        "",
        "",
        stage_separator(widths.parse),
        stage_separator(widths.flatten),
        stage_separator(widths.dae),
        stage_separator(widths.solve),
        stage_separator(widths.ic),
        stage_separator(widths.sim),
        package_width = widths.package,
        count_width = widths.count,
    ));
}

fn append_pass_rate_terminal_row(
    table: &mut String,
    row: &MslPackagePassRateRow,
    widths: &PassRateTerminalWidths,
) {
    table.push_str(&format!(
        "{:<package_width$}{:>count_width$} | {} | {} | {} | {} | {} | {}\n",
        row.package,
        row.n,
        stage_cell(
            percent_cell(row.parse_passed, row.n),
            avg_time_cell(row.parse_avg_seconds),
            widths.parse
        ),
        stage_cell(
            percent_cell(row.flatten_passed, row.n),
            avg_time_cell(row.flatten_avg_seconds),
            widths.flatten
        ),
        stage_cell(
            percent_cell(row.dae_passed, row.n),
            avg_time_cell(row.dae_avg_seconds),
            widths.dae
        ),
        stage_cell(
            percent_cell(row.solve_passed, row.n),
            avg_time_cell(row.solve_avg_seconds),
            widths.solve
        ),
        stage_cell(
            percent_cell(row.ic_passed, row.n),
            avg_time_cell(row.ic_avg_seconds),
            widths.ic
        ),
        stage_cell(
            percent_cell(row.sim_passed, row.n),
            avg_time_cell(row.sim_avg_seconds),
            widths.sim
        ),
        package_width = widths.package,
        count_width = widths.count,
    ));
}

fn stage_header(label: &str, widths: StageColumnWidths) -> String {
    format!(
        "{:>percent_width$}  {:>time_width$}",
        label,
        "Time",
        percent_width = widths.percent,
        time_width = widths.time
    )
}

fn stage_cell(percent: String, time: String, widths: StageColumnWidths) -> String {
    format!(
        "{:>percent_width$}  {:>time_width$}",
        percent,
        time,
        percent_width = widths.percent,
        time_width = widths.time
    )
}

fn stage_separator(widths: StageColumnWidths) -> String {
    "-".repeat(widths.percent + 2 + widths.time)
}

const PASS_RATE_PACKAGE_WIDTH: usize = 42;
const PASS_RATE_COUNT_WIDTH: usize = 4;
const PASS_RATE_PERCENT_WIDTH: usize = 6;
const PASS_RATE_TIME_WIDTH: usize = 10;

fn format_msl_package_pass_rate_compact_table(report: &MslPackagePassRateReport) -> String {
    let mut table = String::new();
    append_pass_rate_compact_header(&mut table);
    for row in &report.rows {
        append_pass_rate_compact_row(&mut table, row);
    }
    append_pass_rate_compact_separator(&mut table);
    append_pass_rate_compact_row(&mut table, &report.overall);
    table
}

fn append_pass_rate_compact_header(table: &mut String) {
    table.push_str(&format!(
        "{:<package_width$} {:>count_width$} {:>percent_width$} {:>time_width$} {:>percent_width$} {:>time_width$} {:>percent_width$} {:>time_width$} {:>solve_width$} {:>time_width$} {:>percent_width$} {:>time_width$} {:>percent_width$} {:>time_width$}\n",
        "MSL Package",
        "n",
        "Ast",
        "Time",
        "Flatten",
        "Time",
        "DAE",
        "Time",
        "Solve",
        "Time",
        "IC",
        "Time",
        "Sim",
        "Time",
        package_width = PASS_RATE_PACKAGE_WIDTH,
        count_width = PASS_RATE_COUNT_WIDTH,
        percent_width = PASS_RATE_PERCENT_WIDTH,
        time_width = PASS_RATE_TIME_WIDTH,
        solve_width = PASS_RATE_SOLVE_WIDTH,
    ));
    append_pass_rate_compact_separator(table);
}

const PASS_RATE_SOLVE_WIDTH: usize = 8;

fn append_pass_rate_compact_separator(table: &mut String) {
    table.push_str(&format!(
        "{:-<package_width$} {:-<count_width$} {:-<percent_width$} {:-<time_width$} {:-<percent_width$} {:-<time_width$} {:-<percent_width$} {:-<time_width$} {:-<solve_width$} {:-<time_width$} {:-<percent_width$} {:-<time_width$} {:-<percent_width$} {:-<time_width$}\n",
        "",
        "",
        "",
        "",
        "",
        "",
        "",
        "",
        "",
        "",
        "",
        "",
        "",
        "",
        package_width = PASS_RATE_PACKAGE_WIDTH,
        count_width = PASS_RATE_COUNT_WIDTH,
        percent_width = PASS_RATE_PERCENT_WIDTH,
        time_width = PASS_RATE_TIME_WIDTH,
        solve_width = PASS_RATE_SOLVE_WIDTH,
    ));
}

fn append_pass_rate_compact_row(table: &mut String, row: &MslPackagePassRateRow) {
    table.push_str(&format!(
        "{:<package_width$} {:>count_width$} {:>percent_width$} {:>time_width$} {:>percent_width$} {:>time_width$} {:>percent_width$} {:>time_width$} {:>solve_width$} {:>time_width$} {:>percent_width$} {:>time_width$} {:>percent_width$} {:>time_width$}\n",
        row.package,
        row.n,
        percent_cell(row.parse_passed, row.n),
        avg_time_cell(row.parse_avg_seconds),
        percent_cell(row.flatten_passed, row.n),
        avg_time_cell(row.flatten_avg_seconds),
        percent_cell(row.dae_passed, row.n),
        avg_time_cell(row.dae_avg_seconds),
        percent_cell(row.solve_passed, row.n),
        avg_time_cell(row.solve_avg_seconds),
        percent_cell(row.ic_passed, row.n),
        avg_time_cell(row.ic_avg_seconds),
        percent_cell(row.sim_passed, row.n),
        avg_time_cell(row.sim_avg_seconds),
        package_width = PASS_RATE_PACKAGE_WIDTH,
        count_width = PASS_RATE_COUNT_WIDTH,
        percent_width = PASS_RATE_PERCENT_WIDTH,
        time_width = PASS_RATE_TIME_WIDTH,
        solve_width = PASS_RATE_SOLVE_WIDTH,
    ));
}

fn build_msl_package_trace_accuracy_report(
    summary: &MslSummary,
    payload: &serde_json::Value,
) -> io::Result<MslPackageTraceAccuracyReport> {
    let mut by_package = BTreeMap::<String, MslPackageTraceAccuracyCounts>::new();
    let mut overall_counts = MslPackageTraceAccuracyCounts::default();
    seed_trace_accuracy_denominators(summary, &mut by_package, &mut overall_counts);
    let models = payload
        .get("models")
        .and_then(serde_json::Value::as_object)
        .ok_or_else(|| {
            io::Error::other("trace comparison JSON is missing object field 'models'")
        })?;

    for (model_name, model_payload) in models {
        let mut metric = serde_json::from_value::<ModelDeviationMetric>(model_payload.clone())
            .map_err(|error| {
                io::Error::other(format!(
                    "failed to parse trace metric for {model_name}: {error}"
                ))
            })?;
        if metric.model_name.is_empty() {
            metric.model_name.clone_from(model_name);
        }
        let Some(package_name) = root_msl_package_name(&metric.model_name) else {
            continue;
        };
        let counts = by_package.entry(package_name).or_default();
        add_trace_metric_to_counts(counts, &metric);
        add_trace_metric_to_counts(&mut overall_counts, &metric);
    }

    let rows = by_package
        .into_iter()
        .map(|(package, counts)| trace_accuracy_row(package, counts))
        .collect::<Vec<_>>();
    let overall = trace_accuracy_row("Overall".to_string(), overall_counts);

    Ok(MslPackageTraceAccuracyReport {
        selection_pattern: ROOT_MSL_EXAMPLE_SELECTION_PATTERN.to_string(),
        model_count: overall.n,
        rows,
        overall,
    })
}

fn seed_trace_accuracy_denominators(
    summary: &MslSummary,
    by_package: &mut BTreeMap<String, MslPackageTraceAccuracyCounts>,
    overall_counts: &mut MslPackageTraceAccuracyCounts,
) {
    for result in &summary.model_results {
        let Some(package_name) = root_msl_package_name(&result.model_name) else {
            continue;
        };
        by_package.entry(package_name).or_default().n += 1;
        overall_counts.n += 1;
    }
}

fn add_trace_metric_to_counts(
    counts: &mut MslPackageTraceAccuracyCounts,
    metric: &ModelDeviationMetric,
) {
    counts.compared += 1;
    counts.compared_channels += metric.compared_variables;
    counts.bad_channels += metric.channel_deviation_count;
    counts.severe_channels += metric.channel_severe_count;
    if metric.channel_severe_count == 0 {
        counts.no_severe_models += 1;
    }
    match classify_trace_metric_channel_distribution(
        metric,
        MODEL_HIGH_MIN_HIGH_CHANNEL_SHARE,
        MODEL_HIGH_MAX_DEVIATION_CHANNEL_SHARE,
        MODEL_MINOR_MIN_HIGH_PLUS_MINOR_CHANNEL_SHARE,
        MODEL_MINOR_MAX_DEVIATION_CHANNEL_SHARE,
    ) {
        AgreementBand::HighAgreement => counts.high_agreement += 1,
        AgreementBand::MinorAgreement => counts.near_agreement += 1,
        AgreementBand::Deviation => counts.deviation += 1,
    }
}

fn trace_accuracy_row(
    package: String,
    counts: MslPackageTraceAccuracyCounts,
) -> MslPackageTraceAccuracyRow {
    let acceptable_agreement = counts.high_agreement + counts.near_agreement;
    let channel_ok = counts.compared_channels.saturating_sub(counts.bad_channels);
    MslPackageTraceAccuracyRow {
        package,
        n: counts.n,
        compared: counts.compared,
        high_agreement: counts.high_agreement,
        near_agreement: counts.near_agreement,
        acceptable_agreement,
        deviation: counts.deviation,
        no_severe_models: counts.no_severe_models,
        bad_channels: counts.bad_channels,
        severe_channels: counts.severe_channels,
        compared_percent: rounded_percent(counts.compared, counts.n),
        high_percent: rounded_percent(counts.high_agreement, counts.n),
        near_percent: rounded_percent(counts.near_agreement, counts.n),
        trace_percent: rounded_percent(acceptable_agreement, counts.n),
        deviation_percent: rounded_percent(counts.deviation, counts.n),
        channel_ok_percent: rounded_percent(channel_ok, counts.compared_channels),
        no_severe_percent: rounded_percent(counts.no_severe_models, counts.n),
    }
}

fn format_msl_package_trace_accuracy_markdown(report: &MslPackageTraceAccuracyReport) -> String {
    let mut markdown = String::new();
    markdown.push_str(
        "| MSL Package | n | Compared | Trace | High | Near | Channel OK | No Severe |\n",
    );
    markdown.push_str("|---|---:|---:|---:|---:|---:|---:|---:|\n");
    for row in &report.rows {
        append_trace_accuracy_markdown_row(&mut markdown, row);
    }
    append_trace_accuracy_markdown_row(&mut markdown, &report.overall);
    markdown
}

fn append_trace_accuracy_markdown_row(markdown: &mut String, row: &MslPackageTraceAccuracyRow) {
    markdown.push_str(&format!(
        "| {} | {} | {} | {} | {} | {} | {} | {} |\n",
        row.package,
        row.n,
        percent_cell(row.compared, row.n),
        percent_cell(row.acceptable_agreement, row.n),
        percent_cell(row.high_agreement, row.n),
        percent_cell(row.near_agreement, row.n),
        format_args!("{:.0}%", row.channel_ok_percent),
        percent_cell(row.no_severe_models, row.n)
    ));
}

pub(super) fn write_msl_package_trace_accuracy_report(summary: &MslSummary) -> io::Result<bool> {
    let results_dir = get_msl_cache_dir().join("results");
    let trace_file = results_dir.join("sim_trace_comparison.json");
    if !trace_file.is_file() {
        println!(
            "MSL package trace accuracy: skipped; {} does not exist.",
            trace_file.display()
        );
        return Ok(false);
    }
    let payload = serde_json::from_slice::<serde_json::Value>(&fs::read(&trace_file)?)
        .map_err(|error| io::Error::other(format!("failed to parse trace comparison: {error}")))?;
    let report = build_msl_package_trace_accuracy_report(summary, &payload)?;
    let json = serde_json::to_string_pretty(&report)?;
    let mut json_file = File::create(results_dir.join("msl_package_trace_accuracy.json"))?;
    json_file.write_all(json.as_bytes())?;
    let markdown = format_msl_package_trace_accuracy_markdown(&report);
    let mut markdown_file = File::create(results_dir.join("msl_package_trace_accuracy.md"))?;
    markdown_file.write_all(markdown.as_bytes())?;

    println!("\n=== MSL Package Trace Accuracy vs OMC ===");
    print!("{markdown}");

    let pass_rate_report =
        build_msl_package_pass_rate_report_with_trace_payload(summary, &payload)?;
    write_msl_package_pass_rate_report(&results_dir, &pass_rate_report, true)?;
    Ok(true)
}

/// Write MSL test results to a JSON file.
pub(super) fn write_msl_results(summary: &MslSummary) -> io::Result<()> {
    let results_dir = get_msl_cache_dir().join("results");
    fs::create_dir_all(&results_dir)?;

    let json = serde_json::to_string_pretty(summary)?;
    let mut file = File::create(results_dir.join("msl_results.json"))?;
    file.write_all(json.as_bytes())?;
    let compile_target_models = collect_compile_target_models(summary);
    let compile_target_lookup = compile_target_lookup(summary);
    let compile_target_lane_count = msl_stage_parallelism().max(1);
    let compile_target_records = build_target_records(
        &compile_target_models,
        compile_target_lane_count,
        &compile_target_lookup,
    );
    let compile_targets = MslTargetModelList {
        git_commit: &summary.git_commit,
        msl_version: &summary.msl_version,
        selection_kind: "all_models_except_nonsim",
        model_count: compile_target_models.len(),
        model_names: &compile_target_models,
        records: compile_target_records,
    };
    let sim_target_lane_count = simulation_parallelism().max(1);
    let sim_target_records = build_target_records(
        &summary.sim_target_models,
        sim_target_lane_count,
        &compile_target_lookup,
    );
    let sim_targets = MslTargetModelList {
        git_commit: &summary.git_commit,
        msl_version: &summary.msl_version,
        selection_kind: "standalone_root_examples_without_unbound_inputs_or_unbound_fixed_parameters",
        model_count: summary.sim_target_models.len(),
        model_names: &summary.sim_target_models,
        records: sim_target_records,
    };
    write_target_model_list(&results_dir, "msl_compile_targets.json", &compile_targets)?;
    write_target_model_list(&results_dir, "msl_simulation_targets.json", &sim_targets)?;
    let package_report = build_msl_package_pass_rate_report(summary);
    write_msl_package_pass_rate_report(&results_dir, &package_report, summary.sim_attempted == 0)?;
    let contract_report = build_mls_contract_coverage_report(summary);
    write_mls_contract_coverage_report(&results_dir, &contract_report)?;

    println!("\nResults written to {:?}", results_dir);
    Ok(())
}

pub(super) fn print_msl_balance_summary(summary: &MslSummary) {
    println!("\n=== MSL Test Summary ===");
    println!("MSL Version: {}", summary.msl_version);
    println!("Total .mo files: {}", summary.total_mo_files);
    println!();

    // Print class type breakdown
    if !summary.class_type_counts.is_empty() {
        let total_classes: usize = summary.class_type_counts.values().sum();
        println!("Class Types in MSL ({} total):", total_classes);
        let mut sorted: Vec<_> = summary.class_type_counts.iter().collect();
        sorted.sort_by_key(|(_, count)| std::cmp::Reverse(**count));
        for (class_type, count) in &sorted {
            println!("  - {}: {}", class_type, count);
        }
    }
    println!("Simulatable (model+block+class): {}", summary.total_models);
    println!();

    println!("Compilation:");
    println!("  - Resolve failed: {}", summary.resolve_failed);
    println!("  - Needs inner: {} (not failures)", summary.needs_inner);
    println!("  - Instantiate failed: {}", summary.instantiate_failed);
    println!("  - Typecheck failed: {}", summary.typecheck_failed);
    println!("  - Flatten failed: {}", summary.flatten_failed);
    println!("  - ToDae failed: {}", summary.todae_failed);
    println!("  - Non-simulatable: {}", summary.non_sim_models);
    println!("  - Successfully compiled: {}", summary.compiled_models);
    println!();
    println!("Balance Results:");
    println!("  - Balanced: {}", summary.balanced_models);
    println!("  - Unbalanced: {}", summary.unbalanced_models);
    println!(
        "  - Partial (intentionally incomplete): {}",
        summary.partial_models
    );
    println!("Initialization Balance:");
    println!(
        "  - Initial balance OK: {}",
        summary.initial_balanced_models
    );
    println!(
        "  - Initial balance deficit: {}",
        summary.initial_unbalanced_models
    );

    // Calculate balance rate excluding partial models
    let simulatable_models = summary.compiled_models - summary.partial_models;
    if simulatable_models > 0 {
        let balance_rate = (summary.balanced_models as f64 / simulatable_models as f64) * 100.0;
        println!(
            "  - Balance rate: {:.1}% (of {} simulatable models)",
            balance_rate, simulatable_models
        );
        let initial_balance_rate =
            (summary.initial_balanced_models as f64 / simulatable_models as f64) * 100.0;
        println!(
            "  - Initial balance rate: {:.1}% (of {} simulatable models)",
            initial_balance_rate, simulatable_models
        );
    }
    println!();
}

#[cfg(test)]
mod tests {
    use super::*;

    fn model_result(name: &str, phase_reached: &str, sim_status: Option<&str>) -> MslModelResult {
        let mut result = phase_error_result(name.to_string(), phase_reached, None, None);
        result.sim_status = sim_status.map(str::to_string);
        if phase_reached == "Success" && result.sim_status.is_some() {
            result.ir_solve_file = Some("ir_solve/test.json".to_string());
            result.ic_status = Some("ic_ok".to_string());
        }
        result
    }

    #[test]
    fn package_name_groups_nested_msl_libraries() {
        assert_eq!(
            root_msl_package_name("Modelica.Electrical.Analog.Examples.HeatingRectifier"),
            Some("Electrical.Analog".to_string())
        );
        assert_eq!(
            root_msl_package_name(
                "Modelica.Mechanics.MultiBody.Examples.Elementary.DoublePendulum"
            ),
            Some("Mechanics.MultiBody".to_string())
        );
        assert_eq!(
            root_msl_package_name("Modelica.Blocks.Examples.Utilities.InternalHelper"),
            None
        );
    }

    #[test]
    fn package_pass_rate_report_counts_pipeline_stage_reachability() {
        let mut summary = empty_summary(0, 0);
        summary.model_results = vec![
            model_result(
                "Modelica.Blocks.Examples.BooleanNetwork1",
                "Success",
                Some("sim_ok"),
            ),
            model_result(
                "Modelica.Electrical.Analog.Examples.HeatingRectifier",
                "ToDae",
                None,
            ),
            model_result(
                "Modelica.Electrical.Analog.Examples.Rectifier",
                "Flatten",
                None,
            ),
            model_result(
                "Modelica.Mechanics.MultiBody.Examples.Elementary.DoublePendulum",
                "Success",
                Some("sim_solver_fail"),
            ),
            model_result(
                "Modelica.Blocks.Examples.Utilities.SupportModel",
                "Success",
                Some("sim_ok"),
            ),
            model_result(
                "ModelicaTest.Blocks.Routing.ShowRealPassThrough",
                "Success",
                None,
            ),
            MslModelResult {
                ic_status: Some("ic_ok".to_string()),
                sim_status: Some("sim_ok".to_string()),
                ..phase_error_result(
                    "Modelica.Blocks.Examples.InvalidDownstreamStatus".to_string(),
                    "Success",
                    None,
                    None,
                )
            },
        ];

        let report = build_msl_package_pass_rate_report(&summary);

        assert_eq!(report.model_count, 5);
        let electrical = report
            .rows
            .iter()
            .find(|row| row.package == "Electrical.Analog")
            .expect("Electrical.Analog row");
        assert_eq!(electrical.n, 2);
        assert_eq!(electrical.parse_passed, 2);
        assert_eq!(electrical.flatten_passed, 1);
        assert_eq!(electrical.dae_passed, 0);
        assert_eq!(electrical.solve_passed, 0);
        assert_eq!(electrical.sim_passed, 0);
        assert_eq!(electrical.flatten_percent, 50.0);

        assert_eq!(report.overall.n, 5);
        assert_eq!(report.overall.parse_percent, 100.0);
        assert_eq!(report.overall.flatten_percent, 80.0);
        assert_eq!(report.overall.dae_percent, 60.0);
        assert_eq!(report.overall.solve_percent, 40.0);
        assert_eq!(report.overall.ic_percent, 40.0);
        assert_eq!(report.overall.sim_percent, 20.0);

        let markdown = format_msl_package_pass_rate_markdown(&report);
        assert!(
            markdown.contains(
                "| MSL Package | n | Ast | Time | Flat | Time | Dae | Time | Solve | Time | IC | Time | Sim | Time |"
            )
        );
        assert!(markdown.contains(
            "| Overall | 5 | 100% | 0.00s | 80% | - | 60% | - | 40% | - | 40% | - | 20% | - |"
        ));

        let terminal_table = format_msl_package_pass_rate_terminal_table(&report);
        assert!(terminal_table.contains("MSL Package"));
        assert!(terminal_table.contains("Time"));
        assert!(terminal_table.contains("Electrical.Analog"));
        assert!(terminal_table.contains("Overall"));
        assert!(terminal_table.contains(" |  Ast"));
        assert!(terminal_table.contains(" | Flat"));
        assert!(terminal_table.contains("Solve    Time"));
        assert!(!terminal_table.contains("| MSL Package |"));

        let compact_table = format_msl_package_pass_rate_compact_table(&report);
        assert!(compact_table.contains("MSL Package"));
        assert!(compact_table.contains("Ast"));
        assert!(compact_table.contains("Solve"));
        assert!(compact_table.contains("Electrical.Analog"));
        assert!(compact_table.contains("Overall"));
        assert!(!compact_table.contains("| MSL Package |"));
    }

    #[test]
    fn package_pass_rate_report_charges_timeout_to_active_stage() {
        let mut summary = empty_summary(0, 0);
        let mut result = phase_error_result(
            "Modelica.Math.Nonlinear.Examples.SolveTimeout".to_string(),
            "Success",
            None,
            None,
        );
        result.timeout_phase = Some(rumoca_worker::WorkerProgressPhase::Solve);
        result.timeout_seconds = Some(10.0);
        result.sim_status = Some("sim_timeout".to_string());
        summary.model_results = vec![result];

        let report = build_msl_package_pass_rate_report(&summary);

        assert_eq!(report.overall.dae_percent, 100.0);
        assert_eq!(report.overall.solve_percent, 0.0);
        assert_eq!(report.overall.solve_avg_seconds, Some(10.0));
        assert!(format_msl_package_pass_rate_terminal_table(&report).contains("0%  10.00s"));
    }

    #[test]
    fn mls_contract_coverage_report_renders_markdown_and_terminal_tables() {
        let mut summary = empty_summary(0, 0);
        let mut array_result = phase_error_result(
            "Modelica.Blocks.Examples.MatrixGain".to_string(),
            "Success",
            None,
            None,
        );
        array_result.is_balanced = Some(true);
        array_result.ir_solve_file = Some("MatrixGain.solve.json".to_string());
        array_result.sim_status = Some("sim_ok".to_string());
        let connector_result = phase_error_result(
            "Modelica.Electrical.Analog.Examples.ConnectorFailure".to_string(),
            "Flatten",
            Some("connect equation failed".to_string()),
            Some("ECONN001".to_string()),
        );
        summary.model_results = vec![array_result, connector_result];

        let report = build_mls_contract_coverage_report(&summary);

        assert_eq!(report.model_count, 2);
        let arr = report
            .rows
            .iter()
            .find(|row| row.category == "ARR")
            .expect("ARR row");
        assert_eq!(arr.models, 1);
        assert_eq!(arr.compiled, 1);
        assert_eq!(arr.solve_ir, 1);
        assert_eq!(arr.balanced, 1);
        assert_eq!(arr.sim_ok, 1);
        let conn = report
            .rows
            .iter()
            .find(|row| row.category == "CONN_STRM")
            .expect("CONN_STRM row");
        assert_eq!(conn.error_code_counts.get("ECONN001"), Some(&1));

        let markdown = format_mls_contract_coverage_markdown(&report);
        assert!(markdown.contains("| MLS Category | n | Compile | Solve IR | Balance | Sim |"));
        assert!(markdown.contains("| ARR | 1 | 100% | 100% | 100% | 100% | Success:1 | - |"));
        assert!(
            markdown.contains("| CONN_STRM | 1 | 0% | 0% | 0% | 0% | Flatten:1 | ECONN001:1 |")
        );

        let terminal_table = format_mls_contract_coverage_terminal_table(&report);
        assert!(terminal_table.contains("MLS Category"));
        assert!(terminal_table.contains("CONN_STRM"));
        assert!(!terminal_table.contains("| MLS Category |"));
    }

    fn trace_metric_json(
        model_name: &str,
        high: usize,
        near: usize,
        deviation: usize,
        severe: usize,
    ) -> serde_json::Value {
        trace_metric_json_with_ic(model_name, high, near, deviation, severe, 0, 0)
    }

    fn trace_metric_json_with_ic(
        model_name: &str,
        high: usize,
        near: usize,
        deviation: usize,
        severe: usize,
        ic_channels: usize,
        ic_deviation: usize,
    ) -> serde_json::Value {
        serde_json::json!({
            "model_name": model_name,
            "compared_variables": high + near + deviation,
            "samples_compared": 10,
            "bounded_normalized_l1_score": 0.0,
            "mean_channel_bounded_normalized_l1": 0.0,
            "max_channel_bounded_normalized_l1": 0.0,
            "channel_high_count": high,
            "channel_minor_count": near,
            "channel_deviation_count": deviation,
            "channel_severe_count": severe,
            "initial_condition": {
                "channels_compared": ic_channels,
                "high_count": ic_channels.saturating_sub(ic_deviation),
                "minor_count": 0,
                "deviation_count": ic_deviation,
                "severe_count": 0,
                "high_percent": 0.0,
                "minor_percent": 0.0,
                "deviation_percent": 0.0,
                "severe_percent": 0.0,
                "violation_mass_total": 0.0,
                "violation_mass_mean_per_channel": 0.0,
                "mean_channel_bounded_normalized_error": 0.0,
                "max_channel_bounded_normalized_error": 0.0
            },
            "worst_variables": []
        })
    }

    #[test]
    fn package_pass_rate_report_uses_omc_parity_for_ic_and_sim() {
        let mut summary = empty_summary(0, 0);
        summary.model_results = vec![
            model_result(
                "Modelica.Blocks.Examples.TraceMatch",
                "Success",
                Some("sim_ok"),
            ),
            model_result(
                "Modelica.Blocks.Examples.TraceDeviation",
                "Success",
                Some("sim_ok"),
            ),
            model_result(
                "Modelica.Blocks.Examples.InitialDeviation",
                "Success",
                Some("sim_ok"),
            ),
            model_result(
                "Modelica.Blocks.Examples.NoInitialParity",
                "Success",
                Some("sim_ok"),
            ),
            model_result(
                "Modelica.Blocks.Examples.NoTraceParity",
                "Success",
                Some("sim_ok"),
            ),
        ];
        let payload = serde_json::json!({
            "models": {
                "Modelica.Blocks.Examples.TraceMatch":
                    trace_metric_json_with_ic("Modelica.Blocks.Examples.TraceMatch", 10, 0, 0, 0, 2, 0),
                "Modelica.Blocks.Examples.TraceDeviation":
                    trace_metric_json_with_ic("Modelica.Blocks.Examples.TraceDeviation", 0, 0, 10, 0, 2, 0),
                "Modelica.Blocks.Examples.InitialDeviation":
                    trace_metric_json_with_ic("Modelica.Blocks.Examples.InitialDeviation", 10, 0, 0, 0, 2, 1),
                "Modelica.Blocks.Examples.NoInitialParity":
                    trace_metric_json("Modelica.Blocks.Examples.NoInitialParity", 10, 0, 0, 0)
            }
        });

        let report = build_msl_package_pass_rate_report_with_trace_payload(&summary, &payload)
            .expect("pass report with trace parity");

        assert_eq!(report.overall.n, 5);
        assert_eq!(report.overall.ic_passed, 4);
        assert_eq!(report.overall.sim_passed, 3);
        assert_eq!(report.overall.ic_percent, 80.0);
        assert_eq!(report.overall.sim_percent, 60.0);
    }

    #[test]
    fn package_pass_rate_report_accepts_sim_ok_for_non_comparable_omc_trace() {
        let mut summary = empty_summary(0, 0);
        summary.model_results = vec![model_result(
            "Modelica.Media.Examples.SolveOneNonlinearEquation.Inverse_sine",
            "Success",
            Some("sim_ok"),
        )];
        let payload = serde_json::json!({
            "models": {},
            "skipped": {
                "Modelica.Media.Examples.SolveOneNonlinearEquation.Inverse_sine":
                    "trace compare failed: trace has no comparable variable samples"
            }
        });

        let report = build_msl_package_pass_rate_report_with_trace_payload(&summary, &payload)
            .expect("pass report with accepted trace skip");

        assert_eq!(report.overall.n, 1);
        assert_eq!(report.overall.sim_passed, 1);
        assert_eq!(report.overall.sim_percent, 100.0);
    }

    #[test]
    fn package_trace_accuracy_report_groups_by_same_msl_subpackages() {
        let mut summary = empty_summary(0, 0);
        summary.model_results = vec![
            model_result(
                "Modelica.Electrical.Analog.Examples.AD_DA_conversion",
                "Success",
                Some("sim_ok"),
            ),
            model_result(
                "Modelica.Electrical.Analog.Examples.ChuaCircuit",
                "Success",
                Some("sim_ok"),
            ),
            model_result(
                "Modelica.Electrical.Analog.Examples.HeatingRectifier",
                "Success",
                Some("sim_solver_fail"),
            ),
            model_result(
                "Modelica.Electrical.Digital.Examples.Counter",
                "Success",
                Some("sim_ok"),
            ),
            model_result(
                "Modelica.Blocks.Examples.Utilities.SupportModel",
                "Success",
                Some("sim_ok"),
            ),
        ];
        let payload = serde_json::json!({
            "models": {
                "Modelica.Electrical.Analog.Examples.AD_DA_conversion":
                    trace_metric_json("Modelica.Electrical.Analog.Examples.AD_DA_conversion", 10, 0, 0, 0),
                "Modelica.Electrical.Analog.Examples.ChuaCircuit":
                    trace_metric_json("Modelica.Electrical.Analog.Examples.ChuaCircuit", 7, 3, 0, 0),
                "Modelica.Electrical.Digital.Examples.Counter":
                    trace_metric_json("Modelica.Electrical.Digital.Examples.Counter", 8, 0, 2, 1),
                "Modelica.Blocks.Examples.Utilities.SupportModel":
                    trace_metric_json("Modelica.Blocks.Examples.Utilities.SupportModel", 10, 0, 0, 0)
            }
        });

        let report = build_msl_package_trace_accuracy_report(&summary, &payload)
            .expect("trace accuracy report");

        assert_eq!(report.model_count, 4);
        let analog = report
            .rows
            .iter()
            .find(|row| row.package == "Electrical.Analog")
            .expect("Electrical.Analog row");
        assert_eq!(analog.n, 3);
        assert_eq!(analog.compared, 2);
        assert_eq!(analog.high_agreement, 1);
        assert_eq!(analog.near_agreement, 1);
        assert_eq!(analog.trace_percent, 67.0);

        let digital = report
            .rows
            .iter()
            .find(|row| row.package == "Electrical.Digital")
            .expect("Electrical.Digital row");
        assert_eq!(digital.n, 1);
        assert_eq!(digital.deviation, 1);
        assert_eq!(digital.trace_percent, 0.0);
        assert_eq!(digital.channel_ok_percent, 80.0);
        assert_eq!(digital.no_severe_models, 0);
        assert_eq!(digital.no_severe_percent, 0.0);

        let markdown = format_msl_package_trace_accuracy_markdown(&report);
        assert!(
            markdown.contains("| Electrical.Analog | 3 | 67% | 67% | 33% | 33% | 100% | 67% |")
        );
        assert!(markdown.contains("| Overall | 4 | 75% | 50% | 25% | 25% | 93% | 50% |"));
    }
}
