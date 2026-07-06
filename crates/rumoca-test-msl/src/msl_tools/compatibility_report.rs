use super::common::{MslPaths, unix_timestamp_seconds, write_pretty_json};
use anyhow::{Context, Result};
use clap::Args as ClapArgs;
use rumoca_compile::compile::core as rumoca_core;
use serde::Serialize;
use serde_json::{Value, json};
use std::collections::BTreeMap;
use std::path::{Path, PathBuf};

#[derive(Debug, Clone, ClapArgs)]
pub struct Args {
    /// Rumoca MSL results JSON (default: CACHE_DIR/results/msl_results.json)
    #[arg(long)]
    msl_results_file: Option<PathBuf>,
    /// OMC simulation reference JSON (default: CACHE_DIR/results/omc_simulation_reference.json)
    #[arg(long)]
    omc_simulation_reference_file: Option<PathBuf>,
    /// Markdown output path (default: CACHE_DIR/results/msl_compatibility_report.md)
    #[arg(long)]
    out: Option<PathBuf>,
    /// Optional JSON output path for machine-readable release artifacts
    #[arg(long)]
    json_out: Option<PathBuf>,
    /// OMC flattened Modelica output directory
    #[arg(long)]
    omc_flat_dir: Option<PathBuf>,
    /// Rumoca flattened Modelica output directory
    #[arg(long)]
    rumoca_flat_dir: Option<PathBuf>,
}

#[derive(Debug, Default, Clone, Serialize)]
struct PackageStats {
    models: usize,
    compiled: usize,
    balanced: usize,
    initial_balanced: usize,
    rumoca_sim_ok: usize,
    omc_sim_ok: usize,
    trace_compared: usize,
    trace_deviation: usize,
    unsupported_features: BTreeMap<String, usize>,
    top_failures: Vec<String>,
}

#[derive(Debug, Clone, Serialize)]
struct TraceDeviationRecord {
    model_name: String,
    package: String,
    max_channel_bounded_normalized_l1: f64,
    channel_deviation_count: usize,
    channel_severe_count: usize,
    flat_comparison: FlatComparisonRecord,
}

#[derive(Debug, Clone, Serialize)]
struct FlatComparisonRecord {
    omc_flat_available: bool,
    rumoca_flat_available: bool,
    omc_normalized_lines: usize,
    rumoca_normalized_lines: usize,
    normalized_line_delta: isize,
    first_different_line: Option<usize>,
}

pub fn run(args: Args) -> Result<()> {
    let paths = MslPaths::current();
    let omc_flat_dir = args.omc_flat_dir.unwrap_or_else(|| paths.flat_dir.clone());
    let rumoca_flat_dir = args
        .rumoca_flat_dir
        .unwrap_or_else(|| paths.results_dir.join("rumoca_flat"));
    let msl_file = args
        .msl_results_file
        .unwrap_or_else(|| paths.results_dir.join("msl_results.json"));
    let omc_file = args
        .omc_simulation_reference_file
        .unwrap_or_else(|| paths.results_dir.join("omc_simulation_reference.json"));
    let out = args
        .out
        .unwrap_or_else(|| paths.results_dir.join("msl_compatibility_report.md"));
    let json_out = args
        .json_out
        .unwrap_or_else(|| paths.results_dir.join("msl_compatibility_report.json"));

    let msl = read_json(&msl_file)?;
    let omc = read_json_if_present(&omc_file)?;
    let trace_models = load_trace_models(omc.as_ref());
    let packages = build_package_stats(&msl, omc.as_ref(), &trace_models);
    let top_trace_deviations =
        build_top_trace_deviations(&trace_models, &omc_flat_dir, &rumoca_flat_dir);
    let payload = json!({
        "generated_at_unix_seconds": unix_timestamp_seconds(),
        "msl_results_file": msl_file.display().to_string(),
        "omc_simulation_reference_file": omc_file.display().to_string(),
        "omc_flat_dir": omc_flat_dir.display().to_string(),
        "rumoca_flat_dir": rumoca_flat_dir.display().to_string(),
        "package_count": packages.len(),
        "top_trace_deviations": top_trace_deviations,
        "packages": packages,
    });

    write_pretty_json(&json_out, &payload)?;
    write_markdown_report(&out, &payload)?;
    println!(
        "MSL compatibility report wrote {} and {}",
        out.display(),
        json_out.display()
    );
    Ok(())
}

fn read_json(path: &Path) -> Result<Value> {
    let raw = std::fs::read_to_string(path)
        .with_context(|| format!("failed to read '{}'", path.display()))?;
    serde_json::from_str(&raw).with_context(|| format!("invalid JSON '{}'", path.display()))
}

fn read_json_if_present(path: &Path) -> Result<Option<Value>> {
    if path.is_file() {
        read_json(path).map(Some)
    } else {
        Ok(None)
    }
}

fn build_package_stats(
    msl: &Value,
    omc: Option<&Value>,
    trace_models: &BTreeMap<String, Value>,
) -> BTreeMap<String, PackageStats> {
    let omc_models = omc.and_then(|payload| payload.get("models"));
    let mut packages = BTreeMap::new();
    for model in msl
        .get("model_results")
        .and_then(Value::as_array)
        .into_iter()
        .flatten()
    {
        let Some(model_name) = model.get("model_name").and_then(Value::as_str) else {
            continue;
        };
        let package = package_key(model_name);
        let stats = packages
            .entry(package)
            .or_insert_with(PackageStats::default);
        stats.models += 1;
        if phase_reached(model).is_some_and(phase_is_success_or_later) {
            stats.compiled += 1;
        }
        if model.get("is_balanced").and_then(Value::as_bool) == Some(true) {
            stats.balanced += 1;
        }
        if model.get("initial_balance_ok").and_then(Value::as_bool) == Some(true) {
            stats.initial_balanced += 1;
        }
        if model.get("sim_status").and_then(Value::as_str) == Some("sim_ok") {
            stats.rumoca_sim_ok += 1;
        }
        collect_unsupported_features(model, &mut stats.unsupported_features);
        collect_failure(model_name, model, stats);
        if omc_model_status(omc_models, model_name) == Some("success") {
            stats.omc_sim_ok += 1;
        }
        if let Some(trace) = trace_models.get(model_name) {
            stats.trace_compared += 1;
            let deviation_channels = trace
                .get("channel_deviation_count")
                .and_then(Value::as_u64)
                .unwrap_or(0);
            let severe_channels = trace
                .get("channel_severe_count")
                .and_then(Value::as_u64)
                .unwrap_or(0);
            if deviation_channels > 0 || severe_channels > 0 {
                stats.trace_deviation += 1;
            }
        }
    }
    packages
}

fn build_top_trace_deviations(
    trace_models: &BTreeMap<String, Value>,
    omc_flat_dir: &Path,
    rumoca_flat_dir: &Path,
) -> Vec<TraceDeviationRecord> {
    let mut records = trace_models
        .iter()
        .filter_map(|(model_name, trace)| {
            let max_l1 = trace
                .get("max_channel_bounded_normalized_l1")
                .and_then(Value::as_f64)
                .unwrap_or(0.0);
            let deviation_count = trace
                .get("channel_deviation_count")
                .and_then(Value::as_u64)
                .unwrap_or(0) as usize;
            let severe_count = trace
                .get("channel_severe_count")
                .and_then(Value::as_u64)
                .unwrap_or(0) as usize;
            (deviation_count > 0 || severe_count > 0 || max_l1 > 0.0).then(|| {
                TraceDeviationRecord {
                    model_name: model_name.clone(),
                    package: package_key(model_name),
                    max_channel_bounded_normalized_l1: max_l1,
                    channel_deviation_count: deviation_count,
                    channel_severe_count: severe_count,
                    flat_comparison: compare_flat_files(model_name, omc_flat_dir, rumoca_flat_dir),
                }
            })
        })
        .collect::<Vec<_>>();
    records.sort_by(|a, b| {
        b.max_channel_bounded_normalized_l1
            .total_cmp(&a.max_channel_bounded_normalized_l1)
    });
    records.truncate(10);
    records
}

fn compare_flat_files(
    model_name: &str,
    omc_flat_dir: &Path,
    rumoca_flat_dir: &Path,
) -> FlatComparisonRecord {
    let omc_flat = normalized_flat_lines(&omc_flat_dir.join(format!("{model_name}.mo")));
    let rumoca_flat = normalized_flat_lines(&rumoca_flat_dir.join(format!("{model_name}.mo")));
    let first_different_line = match (&omc_flat, &rumoca_flat) {
        (Some(omc), Some(rumoca)) => first_different_normalized_line(omc, rumoca),
        _ => None,
    };
    let omc_normalized_lines = omc_flat.as_ref().map_or(0, Vec::len);
    let rumoca_normalized_lines = rumoca_flat.as_ref().map_or(0, Vec::len);
    FlatComparisonRecord {
        omc_flat_available: omc_flat.is_some(),
        rumoca_flat_available: rumoca_flat.is_some(),
        omc_normalized_lines,
        rumoca_normalized_lines,
        normalized_line_delta: rumoca_normalized_lines as isize - omc_normalized_lines as isize,
        first_different_line,
    }
}

fn normalized_flat_lines(path: &Path) -> Option<Vec<String>> {
    let raw = std::fs::read_to_string(path).ok()?;
    Some(
        raw.lines()
            .map(str::trim)
            .filter(|line| !line.is_empty())
            .map(ToOwned::to_owned)
            .collect(),
    )
}

fn first_different_normalized_line(lhs: &[String], rhs: &[String]) -> Option<usize> {
    let common = lhs.len().min(rhs.len());
    for index in 0..common {
        if lhs[index] != rhs[index] {
            return Some(index + 1);
        }
    }
    (lhs.len() != rhs.len()).then_some(common + 1)
}

fn load_trace_models(omc: Option<&Value>) -> BTreeMap<String, Value> {
    let Some(report_file) = omc
        .and_then(|payload| payload.pointer("/trace_comparison/report_file"))
        .and_then(Value::as_str)
    else {
        return BTreeMap::new();
    };
    let Ok(trace_payload) = read_json(Path::new(report_file)) else {
        return BTreeMap::new();
    };
    trace_payload
        .get("models")
        .and_then(Value::as_object)
        .into_iter()
        .flat_map(|models| models.iter())
        .map(|(name, value)| (name.clone(), value.clone()))
        .collect()
}

fn package_key(model_name: &str) -> String {
    let mut parts = rumoca_core::split_path_with_indices(model_name).into_iter();
    let Some(root) = parts.next() else {
        return model_name.to_string();
    };
    let Some(package) = parts.next() else {
        return root.to_string();
    };
    format!("{root}.{package}")
}

fn phase_reached(model: &Value) -> Option<&str> {
    model
        .get("phase_reached")
        .or_else(|| model.get("phase"))
        .and_then(Value::as_str)
}

fn phase_is_success_or_later(phase: &str) -> bool {
    matches!(phase, "Success" | "Simulate" | "Simulation" | "Render")
}

fn collect_unsupported_features(model: &Value, counts: &mut BTreeMap<String, usize>) {
    for text in [
        model.get("error_code").and_then(Value::as_str),
        model.get("failure_summary").and_then(Value::as_str),
        model.get("error").and_then(Value::as_str),
    ]
    .into_iter()
    .flatten()
    {
        for feature in unsupported_features_in_text(text) {
            *counts.entry(feature).or_insert(0) += 1;
        }
    }
}

fn unsupported_features_in_text(text: &str) -> Vec<String> {
    text.split(|ch: char| ch.is_whitespace() || ch == ',' || ch == ';')
        .filter_map(|token| token.strip_prefix("unsupported-feature:"))
        .map(|feature| {
            feature
                .trim_matches(|ch: char| !ch.is_ascii_alphanumeric() && ch != '_' && ch != '-')
                .to_string()
        })
        .filter(|feature| !feature.is_empty())
        .collect()
}

fn collect_failure(model_name: &str, model: &Value, stats: &mut PackageStats) {
    if stats.top_failures.len() >= 10 {
        return;
    }
    if phase_reached(model).is_some_and(phase_is_success_or_later)
        && model.get("sim_status").and_then(Value::as_str) != Some("sim_solver_fail")
    {
        return;
    }
    let sim_status = model.get("sim_status").and_then(Value::as_str);
    let phase = if sim_status.is_some_and(|status| status != "sim_ok") {
        "Simulation"
    } else {
        phase_reached(model).unwrap_or("Unknown")
    };
    let detail = if phase == "Simulation" {
        model.get("sim_error").and_then(Value::as_str)
    } else {
        model
            .get("error_code")
            .or_else(|| model.get("failure_summary"))
            .and_then(Value::as_str)
    }
    .unwrap_or("no detail");
    stats
        .top_failures
        .push(format!("{model_name}: {phase}: {detail}"));
}

fn omc_model_status<'a>(omc_models: Option<&'a Value>, model_name: &str) -> Option<&'a str> {
    omc_models?
        .get(model_name)?
        .get("status")
        .and_then(Value::as_str)
}

fn write_markdown_report(path: &Path, payload: &Value) -> Result<()> {
    let mut out = String::new();
    render_report_header(&mut out, payload);
    render_package_table(&mut out, payload);
    render_top_trace_deviations(&mut out, payload);
    render_top_phase_failures(&mut out, payload);
    std::fs::write(path, out).with_context(|| format!("failed to write '{}'", path.display()))
}

fn render_report_header(out: &mut String, payload: &Value) {
    out.push_str("# MSL Compatibility Report\n\n");
    out.push_str(&format!(
        "- Generated: `{}`\n",
        payload["generated_at_unix_seconds"]
    ));
    out.push_str(&format!(
        "- MSL results: `{}`\n",
        payload["msl_results_file"].as_str().unwrap_or_default()
    ));
    out.push_str(&format!(
        "- OMC simulation reference: `{}`\n\n",
        payload["omc_simulation_reference_file"]
            .as_str()
            .unwrap_or_default()
    ));
}

fn render_package_table(out: &mut String, payload: &Value) {
    out.push_str("| Package | Models | Compiled | Balanced | Initial Balanced | Rumoca Sim OK | OMC Sim OK | Trace Compared | Trace Deviations |\n");
    out.push_str("|---|---:|---:|---:|---:|---:|---:|---:|---:|\n");
    if let Some(packages) = payload.get("packages").and_then(Value::as_object) {
        for (package, stats) in packages {
            out.push_str(&format!(
                "| `{}` | {} | {} | {} | {} | {} | {} | {} | {} |\n",
                package,
                json_usize(stats, "models"),
                json_usize(stats, "compiled"),
                json_usize(stats, "balanced"),
                json_usize(stats, "initial_balanced"),
                json_usize(stats, "rumoca_sim_ok"),
                json_usize(stats, "omc_sim_ok"),
                json_usize(stats, "trace_compared"),
                json_usize(stats, "trace_deviation"),
            ));
        }
    }
}

fn render_top_trace_deviations(out: &mut String, payload: &Value) {
    out.push_str("\n## Top Trace Deviations\n\n");
    let deviations = payload
        .get("top_trace_deviations")
        .and_then(Value::as_array)
        .into_iter()
        .flatten()
        .collect::<Vec<_>>();
    if deviations.is_empty() {
        out.push_str("No trace deviations were recorded in the available report.\n\n");
    } else {
        for deviation in deviations {
            out.push_str(&format!(
                "- `{}` (`{}`): max_channel_l1={:.6e}, deviation_channels={}, severe_channels={}, flat_lines(omc={}, rumoca={}, delta={}), first_flat_diff={}\n",
                deviation
                    .get("model_name")
                    .and_then(Value::as_str)
                    .unwrap_or("unknown"),
                deviation
                    .get("package")
                    .and_then(Value::as_str)
                    .unwrap_or("unknown"),
                deviation
                    .get("max_channel_bounded_normalized_l1")
                    .and_then(Value::as_f64)
                    .unwrap_or(0.0),
                json_usize(deviation, "channel_deviation_count"),
                json_usize(deviation, "channel_severe_count"),
                deviation
                    .pointer("/flat_comparison/omc_normalized_lines")
                    .and_then(Value::as_u64)
                    .unwrap_or(0),
                deviation
                    .pointer("/flat_comparison/rumoca_normalized_lines")
                    .and_then(Value::as_u64)
                    .unwrap_or(0),
                deviation
                    .pointer("/flat_comparison/normalized_line_delta")
                    .and_then(Value::as_i64)
                    .unwrap_or(0),
                deviation
                    .pointer("/flat_comparison/first_different_line")
                    .and_then(Value::as_u64)
                    .map(|line| line.to_string())
                    .unwrap_or_else(|| "-".to_string()),
            ));
        }
        out.push('\n');
    }
}

fn render_top_phase_failures(out: &mut String, payload: &Value) {
    out.push_str("## Top Phase Failures\n\n");
    if let Some(packages) = payload.get("packages").and_then(Value::as_object) {
        for (package, stats) in packages {
            let failures = stats
                .get("top_failures")
                .and_then(Value::as_array)
                .into_iter()
                .flatten()
                .filter_map(Value::as_str)
                .collect::<Vec<_>>();
            if failures.is_empty() {
                continue;
            }
            out.push_str(&format!("### `{package}`\n\n"));
            for failure in failures {
                out.push_str(&format!("- {failure}\n"));
            }
            out.push('\n');
        }
    }
}

fn json_usize(value: &Value, key: &str) -> usize {
    value.get(key).and_then(Value::as_u64).unwrap_or(0) as usize
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn package_key_uses_root_and_first_package() {
        assert_eq!(
            package_key("Modelica.Blocks.Examples.Foo"),
            "Modelica.Blocks"
        );
        assert_eq!(
            package_key("ModelicaTest.Tables.Basic"),
            "ModelicaTest.Tables"
        );
        assert_eq!(package_key("Modelica"), "Modelica");
    }

    #[test]
    fn compatibility_stats_aggregate_by_package() {
        let msl = json!({
            "model_results": [
                {
                    "model_name": "Modelica.Blocks.Examples.A",
                    "phase_reached": "Success",
                    "is_balanced": true,
                    "initial_balance_ok": true,
                    "sim_status": "sim_ok"
                },
                {
                    "model_name": "Modelica.Blocks.Examples.B",
                    "phase_reached": "ToDae",
                    "error_code": "unsupported-feature:events"
                },
                {
                    "model_name": "Modelica.Blocks.Examples.C",
                    "phase_reached": "Success",
                    "sim_status": "sim_solver_fail",
                    "sim_error": "solver failed"
                }
            ]
        });
        let omc = json!({
            "models": {
                "Modelica.Blocks.Examples.A": { "status": "success" },
                "Modelica.Blocks.Examples.B": { "status": "error" }
            }
        });

        let trace_models = BTreeMap::new();
        let stats = build_package_stats(&msl, Some(&omc), &trace_models);
        let blocks = stats.get("Modelica.Blocks").expect("Modelica.Blocks");

        assert_eq!(blocks.models, 3);
        assert_eq!(blocks.compiled, 2);
        assert_eq!(blocks.balanced, 1);
        assert_eq!(blocks.rumoca_sim_ok, 1);
        assert_eq!(blocks.omc_sim_ok, 1);
        assert_eq!(blocks.unsupported_features.get("events"), Some(&1));
        assert_eq!(blocks.top_failures.len(), 2);
        assert!(
            blocks
                .top_failures
                .iter()
                .any(|failure| failure.contains("Simulation: solver failed"))
        );
    }

    #[test]
    fn top_trace_deviations_are_sorted_by_l1() {
        let trace_models = BTreeMap::from([
            (
                "Modelica.Blocks.Examples.A".to_string(),
                json!({
                    "max_channel_bounded_normalized_l1": 0.2,
                    "channel_deviation_count": 1,
                    "channel_severe_count": 0
                }),
            ),
            (
                "Modelica.Blocks.Examples.B".to_string(),
                json!({
                    "max_channel_bounded_normalized_l1": 0.8,
                    "channel_deviation_count": 2,
                    "channel_severe_count": 1
                }),
            ),
        ]);

        let records =
            build_top_trace_deviations(&trace_models, Path::new("missing"), Path::new("missing"));

        assert_eq!(records[0].model_name, "Modelica.Blocks.Examples.B");
        assert_eq!(records[0].channel_severe_count, 1);
    }

    #[test]
    fn flat_comparison_records_availability_and_first_difference() {
        let dir = tempfile::tempdir().expect("tempdir");
        let omc_dir = dir.path().join("omc_flat");
        let rumoca_dir = dir.path().join("rumoca_flat");
        std::fs::create_dir_all(&omc_dir).expect("omc dir");
        std::fs::create_dir_all(&rumoca_dir).expect("rumoca dir");
        std::fs::write(
            omc_dir.join("Modelica.Blocks.Examples.A.mo"),
            "model A\n  Real x;\nend A;\n",
        )
        .expect("omc flat");
        std::fs::write(
            rumoca_dir.join("Modelica.Blocks.Examples.A.mo"),
            "model A\n  Real y;\nend A;\n",
        )
        .expect("rumoca flat");

        let comparison = compare_flat_files("Modelica.Blocks.Examples.A", &omc_dir, &rumoca_dir);

        assert!(comparison.omc_flat_available);
        assert!(comparison.rumoca_flat_available);
        assert_eq!(comparison.omc_normalized_lines, 3);
        assert_eq!(comparison.rumoca_normalized_lines, 3);
        assert_eq!(comparison.normalized_line_delta, 0);
        assert_eq!(comparison.first_different_line, Some(2));
    }
}
