#![cfg(feature = "msl-external-tests")]

//! CasADi cross-validation against rumoca's built-in simulator on MSL models.
//!
//! For each model in the committed MSL simulation target list, we:
//! 1. Compile the model from the MSL source root
//! 2. Run rumoca's built-in diffsol simulator to get a reference trace
//! 3. Render the checked CasADi Solve export, run via `python3`
//! 4. Compare the two traces using `compare_model_traces`
//!
//! Run with:
//! ```text
//! cargo test --release --package rumoca-test-msl --features msl-external-tests --test casadi_msl_test test_casadi_vs_rumoca_msl -- --nocapture
//! ```
//!

use flate2::read::GzDecoder;
use rumoca_compile::codegen::{SolveTemplateRenderer, templates};
use rumoca_compile::compile::{CompilationResult, CompiledSourceRoot, PhaseResult};
use rumoca_compile::parsing::parse_files_parallel_lenient;
use rumoca_sim::sim_trace_compare::{ModelDeviationMetric, SimTrace, compare_model_traces};
use rumoca_sim::simulate_dae;
use rumoca_sim::{SimOptions, SimResult};
use std::collections::HashMap;
use std::fs;
use std::io::Read;
use std::path::{Path, PathBuf};
use std::process::Command;
use tar::Archive;
use tempfile::tempdir;
use walkdir::WalkDir;

// =============================================================================
// MSL download infrastructure (shared with msl_tests.rs)
// =============================================================================

const MSL_VERSION: &str = "v4.1.0";
const MSL_URL: &str =
    "https://github.com/modelica/ModelicaStandardLibrary/archive/refs/tags/v4.1.0.tar.gz";

fn get_msl_cache_dir() -> PathBuf {
    let cache_dir =
        rumoca_compile::compile::core::msl_cache_dir_from_manifest(env!("CARGO_MANIFEST_DIR"));
    fs::create_dir_all(&cache_dir).expect("Failed to create MSL cache directory");
    cache_dir
}

fn get_msl_dir() -> PathBuf {
    get_msl_cache_dir().join(format!("ModelicaStandardLibrary-{}", &MSL_VERSION[1..]))
}

fn msl_exists() -> bool {
    let msl_dir = get_msl_dir();
    msl_dir.exists() && msl_dir.join("Modelica").exists()
}

fn ensure_msl_downloaded() -> std::io::Result<PathBuf> {
    let msl_dir = get_msl_dir();
    if msl_exists() {
        println!("MSL {} already cached at {:?}", MSL_VERSION, msl_dir);
        return Ok(msl_dir);
    }
    println!("Downloading MSL {} from GitHub...", MSL_VERSION);
    let response = ureq::get(MSL_URL)
        .call()
        .map_err(|e| std::io::Error::other(format!("Download failed: {e}")))?;
    let mut data = Vec::new();
    response
        .into_reader()
        .read_to_end(&mut data)
        .map_err(|e| std::io::Error::other(format!("Read failed: {e}")))?;
    println!("Downloaded {} bytes, extracting...", data.len());
    let tar = GzDecoder::new(&data[..]);
    let mut archive = Archive::new(tar);
    archive.unpack(get_msl_cache_dir())?;
    println!("Extracted MSL to {:?}", msl_dir);
    Ok(msl_dir)
}

fn find_mo_files(msl_dir: &Path) -> Vec<PathBuf> {
    let has_modelica_versioned = msl_dir.join("Modelica 4.1.0").is_dir();
    let has_modelica_services_versioned = msl_dir.join("ModelicaServices 4.1.0").is_dir();
    let has_modelica_reference_versioned = msl_dir.join("ModelicaReference 4.1.0").is_dir();

    WalkDir::new(msl_dir)
        .into_iter()
        .filter_map(|e| e.ok())
        .filter(|e| {
            let path = e.path();
            let path_str = path.to_string_lossy();
            let is_unversioned_alias = path
                .strip_prefix(msl_dir)
                .ok()
                .and_then(|relative| relative.components().next())
                .and_then(|component| component.as_os_str().to_str())
                .is_some_and(|top| {
                    (top == "Modelica" && has_modelica_versioned)
                        || (top == "ModelicaServices" && has_modelica_services_versioned)
                        || (top == "ModelicaReference" && has_modelica_reference_versioned)
                });
            path.is_file()
                && path.extension().is_some_and(|ext| ext == "mo")
                && !path_str.contains("Obsolete")
                && !path_str.contains("ModelicaTestConversion")
                && !path_str.contains("ModelicaTest/")
                && !is_unversioned_alias
        })
        .map(|e| e.path().to_path_buf())
        .collect()
}

// =============================================================================
// Model target selection
// =============================================================================

fn load_target_models() -> Vec<String> {
    let path =
        Path::new(env!("CARGO_MANIFEST_DIR")).join("tests/msl_tests/msl_simulation_targets.json");
    let raw = fs::read_to_string(&path)
        .unwrap_or_else(|e| panic!("failed to read target list {}: {e}", path.display()));
    let value: serde_json::Value =
        serde_json::from_str(&raw).expect("failed to parse target list JSON");
    value
        .as_array()
        .expect("target list must be a JSON array")
        .iter()
        .map(|v| v.as_str().expect("each entry must be a string").to_string())
        .collect()
}

// =============================================================================
// Python driver for the checked CasADi Solve export
// =============================================================================

fn python_available() -> bool {
    Command::new("python3")
        .args(["-c", "import casadi; import numpy"])
        .output()
        .map(|o| o.status.success())
        .unwrap_or(false)
}

const DRIVER: &str = r#"
import importlib.util, json, sys, os
import numpy as np

spec = importlib.util.spec_from_file_location("model", os.path.join(os.path.dirname(__file__), "model.py"))
mod = importlib.util.module_from_spec(spec)
spec.loader.exec_module(mod)

dt = float(sys.argv[1])
t0 = float(sys.argv[2])
tf = float(sys.argv[3])
tgrid = np.arange(t0, tf + dt * 0.5, dt)
if any(value is None for value in mod.DEFAULT_X):
    raise RuntimeError("checked Solve export has a state without a numeric initial value")
if any(value is None for value in mod.DEFAULT_P):
    raise RuntimeError("checked Solve export has a static variable without a numeric value")
x = np.asarray(mod.DEFAULT_X, dtype=float)
p = np.asarray(mod.DEFAULT_P, dtype=float)
u = np.zeros(mod.N_U)
samples = [x.copy()]
for left, right in zip(tgrid[:-1], tgrid[1:]):
    h = right - left
    def f(t, value):
        return np.asarray(mod.rhs(t, value, u, p), dtype=float).reshape(-1)
    k1 = f(left, x)
    k2 = f(left + 0.5 * h, x + 0.5 * h * k1)
    k3 = f(left + 0.5 * h, x + 0.5 * h * k2)
    k4 = f(right, x + h * k3)
    x = x + h * (k1 + 2.0 * k2 + 2.0 * k3 + k4) / 6.0
    if not np.all(np.isfinite(x)):
        raise RuntimeError(f"non-finite checked Solve state at t={right}")
    samples.append(x.copy())
trace = {'times': tgrid.tolist(), 'names': mod.STATE_NAMES, 'data': {}}
for i, name in enumerate(mod.STATE_NAMES):
    trace['data'][name] = [float(sample[i]) for sample in samples]
print(json.dumps(trace))
"#;

// =============================================================================
// CasADi simulation pipeline
// =============================================================================

fn casadi_simulate(
    dae: &rumoca_ir_dae::Dae,
    model_name: &str,
    t_start: f64,
    t_end: f64,
    dt: f64,
) -> Result<SimTrace, String> {
    let problem =
        rumoca_sim::lower_solve_problem(dae).map_err(|error| format!("lower solve: {error}"))?;
    let artifacts = rumoca_sim::lower_solve_artifacts(&problem)
        .map_err(|error| format!("lower solve artifacts: {error}"))?;
    let renderer = SolveTemplateRenderer::new_with_dae(&problem, &artifacts, dae.clone())
        .map_err(|error| format!("render context: {error}"))?;
    let template = templates::builtin_template_source("casadi-solve", "casadi_solve.py.jinja")
        .ok_or_else(|| "checked casadi-solve template is missing".to_string())?;
    let code = renderer
        .render_with_name(template, model_name)
        .map_err(|error| format!("render: {error}"))?;

    let dir = tempdir().map_err(|e| format!("tempdir: {e}"))?;
    let model_path = dir.path().join("model.py");
    let driver_path = dir.path().join("driver.py");
    fs::write(&model_path, &code).map_err(|e| format!("write model: {e}"))?;
    fs::write(&driver_path, DRIVER).map_err(|e| format!("write driver: {e}"))?;

    let output = Command::new("python3")
        .arg(driver_path.to_str().unwrap())
        .arg(format!("{dt}"))
        .arg(format!("{t_start}"))
        .arg(format!("{t_end}"))
        .output()
        .map_err(|e| format!("python3 invoke: {e}"))?;

    if !output.status.success() {
        let stderr = String::from_utf8_lossy(&output.stderr);
        // Save failing code for debugging
        let debug_dir = Path::new("/tmp/casadi_debug");
        let _ = fs::create_dir_all(debug_dir);
        let safe_name = model_name.replace('.', "_");
        let _ = fs::write(debug_dir.join(format!("{safe_name}.py")), &code);
        let _ = fs::write(
            debug_dir.join(format!("{safe_name}.stderr")),
            stderr.as_bytes(),
        );
        // Truncate for readability
        let lines: Vec<&str> = stderr.lines().collect();
        let truncated = if lines.len() > 6 {
            lines[lines.len() - 6..].join("\n")
        } else {
            stderr.to_string()
        };
        return Err(format!("python failed:\n{truncated}"));
    }

    let stdout = String::from_utf8(output.stdout).map_err(|e| format!("utf8: {e}"))?;
    let json: serde_json::Value =
        serde_json::from_str(&stdout).map_err(|e| format!("JSON parse: {e}"))?;

    let times: Vec<f64> = json["times"]
        .as_array()
        .ok_or("missing times")?
        .iter()
        .map(|v| v.as_f64().unwrap_or(f64::NAN))
        .collect();

    let names: Vec<String> = json["names"]
        .as_array()
        .ok_or("missing names")?
        .iter()
        .map(|v| v.as_str().unwrap_or("").to_string())
        .collect();

    let data: Vec<Vec<Option<f64>>> = names
        .iter()
        .map(|name| {
            json["data"][name]
                .as_array()
                .map(|arr| arr.iter().map(|v| v.as_f64()).collect())
                .unwrap_or_default()
        })
        .collect();

    Ok(SimTrace {
        model_name: Some(model_name.to_string()),
        times,
        names,
        data,
        variable_meta: None,
    })
}

fn sim_result_to_trace(sim: &SimResult, model_name: &str) -> SimTrace {
    SimTrace {
        model_name: Some(model_name.to_string()),
        times: sim.times.clone(),
        names: sim.names.clone(),
        data: sim
            .data
            .iter()
            .map(|ch| ch.iter().map(|&v| Some(v)).collect())
            .collect(),
        variable_meta: None,
    }
}

// =============================================================================
// Per-model result tracking
// =============================================================================

#[derive(Debug)]
enum ModelOutcome {
    CompileFail(String),
    RumocaSimFail(String),
    CasadiRenderFail(String),
    CasadiPythonFail(String),
    TraceCompareFail(String),
    Pass { metric: Box<ModelDeviationMetric> },
    NoStates,
}

#[derive(Default)]
struct OutcomeCounts {
    pass_high: usize,
    pass_other: usize,
    no_states: usize,
    compile_fail: usize,
    sim_fail: usize,
    render_fail: usize,
    python_fail: usize,
    compare_fail: usize,
}

impl OutcomeCounts {
    fn total_pass(&self) -> usize {
        self.pass_high + self.pass_other
    }
}

#[cfg(debug_assertions)]
fn assert_release_mode() {
    panic!(
        "\n\nERROR: CasADi MSL tests must be run in RELEASE mode!\n\
         cargo test --release --package rumoca-test-msl --features msl-external-tests --test casadi_msl_test test_casadi_vs_rumoca_msl -- --nocapture\n"
    );
}

#[cfg(not(debug_assertions))]
fn assert_release_mode() {}

fn truncate(s: &str, max_len: usize) -> String {
    if s.len() > max_len {
        format!("{}...", &s[..max_len])
    } else {
        s.to_string()
    }
}

impl std::fmt::Display for ModelOutcome {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ModelOutcome::CompileFail(e) => write!(f, "compile_fail: {}", truncate(e, 120)),
            ModelOutcome::RumocaSimFail(e) => write!(f, "rumoca_sim_fail: {}", truncate(e, 120)),
            ModelOutcome::CasadiRenderFail(e) => {
                write!(f, "casadi_render_fail: {}", truncate(e, 120))
            }
            ModelOutcome::CasadiPythonFail(e) => {
                write!(f, "casadi_python_fail: {}", truncate(e, 200))
            }
            ModelOutcome::TraceCompareFail(e) => {
                write!(f, "trace_compare_fail: {}", truncate(e, 120))
            }
            ModelOutcome::Pass { metric } => {
                write!(
                    f,
                    "pass (median={:.2e}, max={:.2e}, vars={})",
                    metric.bounded_normalized_l1_score,
                    metric.max_channel_bounded_normalized_l1,
                    metric.compared_variables
                )
            }
            ModelOutcome::NoStates => write!(f, "no_states (skipped)"),
        }
    }
}

fn classify_tag_and_count(outcome: &ModelOutcome, counts: &mut OutcomeCounts) -> &'static str {
    match outcome {
        ModelOutcome::Pass { metric } => {
            if metric.max_channel_bounded_normalized_l1 < 0.05 {
                counts.pass_high += 1;
                "PASS_HIGH"
            } else {
                counts.pass_other += 1;
                "PASS"
            }
        }
        ModelOutcome::NoStates => {
            counts.no_states += 1;
            "SKIP"
        }
        ModelOutcome::CompileFail(_) => {
            counts.compile_fail += 1;
            "COMPILE_FAIL"
        }
        ModelOutcome::RumocaSimFail(_) => {
            counts.sim_fail += 1;
            "SIM_FAIL"
        }
        ModelOutcome::CasadiRenderFail(_) => {
            counts.render_fail += 1;
            "RENDER_FAIL"
        }
        ModelOutcome::CasadiPythonFail(_) => {
            counts.python_fail += 1;
            "PYTHON_FAIL"
        }
        ModelOutcome::TraceCompareFail(_) => {
            counts.compare_fail += 1;
            "COMPARE_FAIL"
        }
    }
}

fn track_outcome_details(
    outcome: &ModelOutcome,
    model_name: &str,
    render_fail_errors: &mut HashMap<String, usize>,
    python_fail_errors: &mut HashMap<String, usize>,
    pass_metrics: &mut Vec<(String, ModelDeviationMetric)>,
) {
    match outcome {
        ModelOutcome::CasadiRenderFail(e) => {
            let key = classify_error(e);
            *render_fail_errors.entry(key).or_default() += 1;
        }
        ModelOutcome::CasadiPythonFail(e) => {
            let key = classify_error(e);
            *python_fail_errors.entry(key).or_default() += 1;
        }
        ModelOutcome::Pass { metric } => {
            pass_metrics.push((model_name.to_string(), (**metric).clone()));
        }
        _ => {}
    }
}

fn print_summary(
    total: usize,
    counts: &OutcomeCounts,
    render_fail_errors: &HashMap<String, usize>,
    python_fail_errors: &HashMap<String, usize>,
    pass_metrics: &[(String, ModelDeviationMetric)],
) {
    let total_pass = counts.total_pass();
    println!("\n{:=<80}", "");
    println!("CasADi Solve vs rumoca MSL summary");
    println!("{:=<80}", "");
    println!("  total models:     {total}");
    println!("  pass (high):      {}", counts.pass_high);
    println!("  pass (other):     {}", counts.pass_other);
    println!("  no_states:        {}", counts.no_states);
    println!("  compile_fail:     {}", counts.compile_fail);
    println!("  rumoca_sim_fail:  {}", counts.sim_fail);
    println!("  casadi_render:    {}", counts.render_fail);
    println!("  casadi_python:    {}", counts.python_fail);
    println!("  trace_compare:    {}", counts.compare_fail);

    if !render_fail_errors.is_empty() {
        println!("\nCasADi render failure categories:");
        let mut sorted: Vec<_> = render_fail_errors.iter().collect();
        sorted.sort_by(|a, b| b.1.cmp(a.1));
        for (pattern, count) in sorted {
            println!("  {count:>4}x  {pattern}");
        }
    }

    if !python_fail_errors.is_empty() {
        println!("\nCasADi Python failure categories:");
        let mut sorted: Vec<_> = python_fail_errors.iter().collect();
        sorted.sort_by(|a, b| b.1.cmp(a.1));
        for (pattern, count) in sorted {
            println!("  {count:>4}x  {pattern}");
        }
    }

    if !pass_metrics.is_empty() {
        // Show worst agreements
        let mut sorted = pass_metrics.to_vec();
        sorted.sort_by(|a, b| {
            b.1.max_channel_bounded_normalized_l1
                .partial_cmp(&a.1.max_channel_bounded_normalized_l1)
                .unwrap()
        });
        println!("\nWorst-agreeing models (top 10):");
        for (name, metric) in sorted.iter().take(10) {
            println!(
                "  max={:.4e} median={:.4e} vars={:>3}  {name}",
                metric.max_channel_bounded_normalized_l1,
                metric.bounded_normalized_l1_score,
                metric.compared_variables,
            );
        }
    }

    println!(
        "\n{total_pass} models tested end-to-end ({} high agreement, {} other)",
        counts.pass_high, counts.pass_other
    );
    println!("{:=<80}\n", "");
}

// =============================================================================
// Core test logic
// =============================================================================

fn run_single_model(source_root: &CompiledSourceRoot, model_name: &str) -> ModelOutcome {
    // 1. Compile
    let report = match source_root.compile_model_strict_reachable_with_recovery(model_name) {
        Ok(report) => report,
        Err(error) => {
            return ModelOutcome::CompileFail(format!("strict reachable compile: {error}"));
        }
    };
    let result: CompilationResult = match report.requested_result {
        Some(PhaseResult::Success(boxed)) => *boxed,
        Some(PhaseResult::Failed { error, .. }) => {
            return ModelOutcome::CompileFail(error);
        }
        _ => {
            return ModelOutcome::CompileFail(report.failure_summary(0));
        }
    };

    let dae = &result.dae;

    // Skip models with no states — an ODE trace has no state channels.
    let has_states = dae.inspect(|view| {
        view.variables()
            .any(|(_, variable)| variable.role() == rumoca_ir_dae::VariableRole::State)
    });
    if !has_states {
        return ModelOutcome::NoStates;
    }

    // 2. Run rumoca simulator
    let t_start = result
        .experiment_start_time
        .filter(|t| t.is_finite())
        .unwrap_or(0.0);
    let t_end = result
        .experiment_stop_time
        .filter(|t| t.is_finite() && *t > t_start)
        .unwrap_or(t_start + 1.0);

    let opts = SimOptions {
        t_end,
        max_wall_seconds: Some(10.0),
        ..SimOptions::default()
    };
    let sim = match simulate_dae(dae, &opts) {
        Ok(sim) => sim,
        Err(e) => return ModelOutcome::RumocaSimFail(format!("{e}")),
    };

    let rumoca_trace = sim_result_to_trace(&sim, model_name);

    // 3. Run the checked CasADi Solve pipeline
    // Use 100 output samples over the horizon
    let dt = (t_end - t_start) / 100.0;
    let casadi_trace = match casadi_simulate(dae, model_name, t_start, t_end, dt) {
        Ok(trace) => trace,
        Err(e) => {
            if e.contains("render:") {
                return ModelOutcome::CasadiRenderFail(e);
            }
            return ModelOutcome::CasadiPythonFail(e);
        }
    };

    // 4. Compare traces
    match compare_model_traces(model_name, &rumoca_trace, &casadi_trace) {
        Ok(metric) => ModelOutcome::Pass {
            metric: Box::new(metric),
        },
        Err(e) => ModelOutcome::TraceCompareFail(format!("{e}")),
    }
}

// =============================================================================
// Test entry point
// =============================================================================

#[test]
fn test_casadi_vs_rumoca_msl() {
    assert_release_mode();

    if !python_available() {
        panic!("python3 with casadi/numpy not available");
    }

    println!("CasADi Solve vs rumoca MSL cross-validation");

    // 1. Download/cache MSL
    let msl_dir = ensure_msl_downloaded().expect("Failed to download MSL");

    // 2. Parse and build the MSL source root
    let mo_files = find_mo_files(&msl_dir);
    println!("Parsing {} MSL files...", mo_files.len());
    let (successes, failures) = parse_files_parallel_lenient(&mo_files);
    println!("Parsed {} OK, {} failures", successes.len(), failures.len());
    let source_map = rumoca_compile::parsing::source_map_for_parsed_files(&successes)
        .expect("failed to preserve parsed MSL sources");
    let source_root = CompiledSourceRoot::from_parsed_batch_tolerant(successes, source_map)
        .expect("failed to build source-root index");

    // 3. Select target models
    let targets = load_target_models();
    println!("Testing {} models\n", targets.len());

    // 4. Run each model
    let mut counts = OutcomeCounts::default();

    let mut render_fail_errors: HashMap<String, usize> = HashMap::new();
    let mut python_fail_errors: HashMap<String, usize> = HashMap::new();
    let mut pass_metrics: Vec<(String, ModelDeviationMetric)> = Vec::new();

    for (i, model_name) in targets.iter().enumerate() {
        let outcome = run_single_model(&source_root, model_name);
        let tag = classify_tag_and_count(&outcome, &mut counts);
        track_outcome_details(
            &outcome,
            model_name,
            &mut render_fail_errors,
            &mut python_fail_errors,
            &mut pass_metrics,
        );

        println!(
            "[{:>3}/{}] {:>13} {model_name}: {outcome}",
            i + 1,
            targets.len(),
            tag
        );
    }

    // 5. Summary
    let total = targets.len();
    print_summary(
        total,
        &counts,
        &render_fail_errors,
        &python_fail_errors,
        &pass_metrics,
    );

    assert!(
        counts.total_pass() > 0,
        "no models were tested end-to-end — CasADi pipeline may be broken"
    );
}

/// Classify an error message into a short category for aggregation.
fn classify_error(error: &str) -> String {
    if error.contains("NameError") {
        if let Some(line) = error.lines().find(|l| l.contains("NameError")) {
            return format!("NameError: {}", truncate(line.trim(), 80));
        }
        return "NameError".to_string();
    }
    if error.contains("TypeError") {
        if let Some(line) = error.lines().find(|l| l.contains("TypeError")) {
            return format!("TypeError: {}", truncate(line.trim(), 80));
        }
        return "TypeError".to_string();
    }
    if error.contains("RuntimeError") {
        if let Some(line) = error.lines().find(|l| l.contains("RuntimeError")) {
            return format!("RuntimeError: {}", truncate(line.trim(), 80));
        }
        return "RuntimeError".to_string();
    }
    if error.contains("ValueError") {
        if let Some(line) = error.lines().find(|l| l.contains("ValueError")) {
            return format!("ValueError: {}", truncate(line.trim(), 80));
        }
        return "ValueError".to_string();
    }
    if error.contains("IndexError") {
        return "IndexError".to_string();
    }
    if error.contains("render:") {
        return truncate(error, 80);
    }
    truncate(error.lines().last().unwrap_or(error), 80)
}
