//! CasADi cross-validation against rumoca's built-in simulator on MSL models.
//!
//! For each model in the 180-model MSL simulation target list, we:
//! 1. Compile the model from the MSL source root
//! 2. Run rumoca's built-in diffsol simulator to get a reference trace
//! 3. Render CasADi MX Python code, run via `python3`
//! 4. Compare the two traces using `compare_model_traces`
//!
//! Run with:
//! ```text
//! cargo test --release --package rumoca-test-msl --test casadi_msl_test -- --ignored --nocapture
//! ```
//!
//! Environment variables:
//! - `RUMOCA_CASADI_MSL_MATCH=pattern` — filter models by substring match
//! - `RUMOCA_CASADI_MSL_LIMIT=N` — cap number of models tested

use flate2::read::GzDecoder;
use rumoca_session::analysis::{ModelDeviationMetric, SimTrace, compare_model_traces};
use rumoca_session::compile::{CompilationResult, CompiledSourceRoot, PhaseResult};
use rumoca_session::parsing::parse_files_parallel_lenient;
use rumoca_session::runtime::{SimOptions, SimResult, simulate_dae};
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
        rumoca_session::compile::core::msl_cache_dir_from_manifest(env!("CARGO_MANIFEST_DIR"));
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
    let path = Path::new(env!("CARGO_MANIFEST_DIR"))
        .join("tests/msl_tests/msl_simulation_targets_180.json");
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

fn apply_env_filters(names: &mut Vec<String>) {
    if let Ok(pattern) = std::env::var("RUMOCA_CASADI_MSL_MATCH") {
        let pattern = pattern.trim().to_string();
        if !pattern.is_empty() {
            names.retain(|n| n.contains(&pattern));
            println!("RUMOCA_CASADI_MSL_MATCH={pattern} → {} models", names.len());
        }
    }
    if let Ok(raw) = std::env::var("RUMOCA_CASADI_MSL_LIMIT")
        && let Ok(limit) = raw.trim().parse::<usize>()
        && names.len() > limit
    {
        names.truncate(limit);
        println!("RUMOCA_CASADI_MSL_LIMIT={limit} → {} models", names.len());
    }
}

// =============================================================================
// Python driver for CasADi MX
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

model = mod.create_model()
dt = float(sys.argv[1])
tf = float(sys.argv[2])
tgrid = np.arange(0, tf + dt * 0.5, dt)
integrator = model['build_integrator'](tgrid)
n_x = model['n_x']
n_z_c = model.get('n_z_continuous', 0)
z0 = model.get('z0', np.array([]))
# Augmented z0: [xdot0 (zeros), z_continuous0]
z0_aug = np.concatenate([np.zeros(n_x), z0[:n_z_c]]) if n_z_c > 0 or n_x > 0 else np.array([])
# Discrete variables appended as extra parameters
z0_d = z0[n_z_c:] if len(z0) > n_z_c else np.array([])
p_full = np.concatenate([model['p0'], np.array([]), z0_d])
kwargs = dict(x0=model['x0'], p=p_full)
if len(z0_aug) > 0:
    kwargs['z0'] = z0_aug
result = integrator(**kwargs)
xf = np.array(result['xf'])
trace = {'times': tgrid.tolist(), 'names': model['state_names'], 'data': {}}
for i, name in enumerate(model['state_names']):
    trace['data'][name] = [float(xf[i, j]) for j in range(xf.shape[1])]
print(json.dumps(trace))
"#;

// =============================================================================
// CasADi simulation pipeline
// =============================================================================

fn casadi_simulate(
    dae: &rumoca_ir_dae::Dae,
    model_name: &str,
    t_end: f64,
    dt: f64,
) -> Result<SimTrace, String> {
    let code = rumoca_phase_codegen::render_template_with_name(
        dae,
        rumoca_phase_codegen::templates::CASADI_MX,
        model_name,
    )
    .map_err(|e| format!("render: {e}"))?;

    let dir = tempdir().map_err(|e| format!("tempdir: {e}"))?;
    let model_path = dir.path().join("model.py");
    let driver_path = dir.path().join("driver.py");
    fs::write(&model_path, &code).map_err(|e| format!("write model: {e}"))?;
    fs::write(&driver_path, DRIVER).map_err(|e| format!("write driver: {e}"))?;

    let output = Command::new("python3")
        .arg(driver_path.to_str().unwrap())
        .arg(format!("{dt}"))
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
    Pass { metric: ModelDeviationMetric },
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
         cargo test --release --package rumoca-test-msl --test casadi_msl_test -- --ignored --nocapture\n"
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
            pass_metrics.push((model_name.to_string(), metric.clone()));
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
    println!("CasADi MX vs rumoca MSL summary");
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
    let report = source_root.compile_model_strict_reachable_with_recovery(model_name);
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

    // Skip models with no states — CasADi integrator needs states
    if dae.states.is_empty() {
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

    // 3. Run CasADi MX pipeline
    // Use 100 output samples over the horizon
    let dt = (t_end - t_start) / 100.0;
    let casadi_trace = match casadi_simulate(dae, model_name, t_end, dt) {
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
        Ok(metric) => ModelOutcome::Pass { metric },
        Err(e) => ModelOutcome::TraceCompareFail(format!("{e}")),
    }
}

// =============================================================================
// Test entry point
// =============================================================================

#[test]
#[ignore]
fn test_casadi_vs_rumoca_msl() {
    assert_release_mode();

    if !python_available() {
        panic!("python3 with casadi/numpy not available");
    }

    println!("CasADi MX vs rumoca MSL cross-validation");

    // 1. Download/cache MSL
    let msl_dir = ensure_msl_downloaded().expect("Failed to download MSL");

    // 2. Parse and build the MSL source root
    let mo_files = find_mo_files(&msl_dir);
    println!("Parsing {} MSL files...", mo_files.len());
    let (successes, failures) = parse_files_parallel_lenient(&mo_files);
    println!("Parsed {} OK, {} failures", successes.len(), failures.len());
    let source_root = CompiledSourceRoot::from_parsed_batch_tolerant(successes)
        .expect("failed to build source-root index");

    // 3. Select target models
    let mut targets = load_target_models();
    apply_env_filters(&mut targets);
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
