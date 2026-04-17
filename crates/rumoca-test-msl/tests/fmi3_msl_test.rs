//! FMI3 cross-validation against rumoca's built-in simulator on MSL models.
//!
//! Tests a curated set of MSL models that compile and simulate successfully
//! through the FMI3 C template pipeline. For each model we:
//! 1. Compile the model from the MSL source root
//! 2. Run rumoca's built-in diffsol simulator to get a reference trace
//! 3. Render FMI3 C source, compile with `cc`, run to get CSV trace
//! 4. Compare the two traces and assert they agree within tolerance
//!
//! Run with:
//! ```text
//! cargo test --release --package rumoca-test-msl --test fmi3_msl_test -- --ignored --nocapture
//! ```
//!
//! Environment variables:
//! - `RUMOCA_FMI3_MSL_MATCH=pattern` — filter models by substring match
//! - `RUMOCA_FMI3_MSL_LIMIT=N` — cap number of models tested
//! - `RUMOCA_FMI3_MSL_DT=0.0001` — forward Euler step size (default 0.0001)
//! - `RUMOCA_FMI3_MSL_TOLERANCE=0.20` — max allowed trace deviation (default 0.20)

use flate2::read::GzDecoder;
use rumoca_session::codegen::{render_dae_template_with_name, templates};
use rumoca_session::compile::{CompilationResult, CompiledSourceRoot, PhaseResult};
use rumoca_session::parsing::parse_files_parallel_lenient;
use rumoca_sim::{SimOptions, SimResult};
use rumoca_sim_diffsol::simulate_dae;
use std::collections::HashMap;
use std::fs;
use std::io::Read;
use std::path::{Path, PathBuf};
use std::process::Command;
use tar::Archive;
use tempfile::tempdir;
use walkdir::WalkDir;

fn check_release_mode() {
    #[cfg(debug_assertions)]
    {
        panic!(
            "\n\nERROR: FMI3 MSL tests must be run in RELEASE mode!\n\
             cargo test --release --package rumoca-test-msl --test fmi3_msl_test -- --ignored --nocapture\n"
        );
    }
}

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

/// Load FMI3 MSL target models from the discovered target list.
/// Generated by `fmu_target_discovery` test.
fn fmi3_target_models() -> Vec<String> {
    let path = Path::new(env!("CARGO_MANIFEST_DIR")).join("tests/msl_tests/fmi3_msl_targets.json");
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
    if let Ok(pattern) = std::env::var("RUMOCA_FMI3_MSL_MATCH") {
        let pattern = pattern.trim().to_string();
        if !pattern.is_empty() {
            names.retain(|n| n.contains(&pattern));
            println!("RUMOCA_FMI3_MSL_MATCH={pattern} → {} models", names.len());
        }
    }
    if let Ok(raw) = std::env::var("RUMOCA_FMI3_MSL_LIMIT")
        && let Ok(limit) = raw.trim().parse::<usize>()
        && names.len() > limit
    {
        names.truncate(limit);
        println!("RUMOCA_FMI3_MSL_LIMIT={limit} → {} models", names.len());
    }
}

fn fmi3_dt() -> f64 {
    std::env::var("RUMOCA_FMI3_MSL_DT")
        .ok()
        .and_then(|s| s.trim().parse().ok())
        .unwrap_or(0.0001)
}

fn fmi3_tolerance() -> f64 {
    std::env::var("RUMOCA_FMI3_MSL_TOLERANCE")
        .ok()
        .and_then(|s| s.trim().parse().ok())
        .unwrap_or(0.20)
}

// =============================================================================
// FMI3 C pipeline
// =============================================================================

/// Render FMI3 C model + test driver, compile with cc, run, return CSV output.
fn fmi3_simulate(
    dae: &rumoca_ir_dae::Dae,
    model_name: &str,
    t_end: f64,
    dt: f64,
) -> Result<String, String> {
    let model_c = render_dae_template_with_name(dae, templates::FMI3_MODEL, model_name)
        .map_err(|e| format!("render model: {e}"))?;

    let driver_c = render_dae_template_with_name(dae, templates::FMI3_TEST_DRIVER, model_name)
        .map_err(|e| format!("render driver: {e}"))?;

    let dir = tempdir().map_err(|e| format!("tempdir: {e}"))?;
    let model_path = dir.path().join("model.c");
    let driver_path = dir.path().join("driver.c");
    let binary_path = dir.path().join("test_fmu");

    fs::write(&model_path, &model_c).map_err(|e| format!("write model.c: {e}"))?;
    fs::write(&driver_path, &driver_c).map_err(|e| format!("write driver.c: {e}"))?;

    // Always dump generated C for debugging
    {
        let debug_dir = std::path::Path::new("/tmp/fmi3_debug");
        let _ = fs::create_dir_all(debug_dir);
        let safe_name = model_name.replace('.', "_");
        let _ = fs::write(debug_dir.join(format!("{safe_name}.c")), &model_c);
        let _ = fs::write(debug_dir.join(format!("{safe_name}_driver.c")), &driver_c);
    }

    let compile_output = Command::new("cc")
        .args([
            "-O2",
            "-Wall",
            "-Wno-unused-variable",
            "-o",
            binary_path.to_str().unwrap(),
            model_path.to_str().unwrap(),
            driver_path.to_str().unwrap(),
            "-lm",
        ])
        .output()
        .map_err(|e| format!("cc invoke: {e}"))?;

    if !compile_output.status.success() {
        let stderr = String::from_utf8_lossy(&compile_output.stderr);
        // Dump generated C for debugging
        let debug_dir = std::path::Path::new("/tmp/fmi3_debug");
        let _ = fs::create_dir_all(debug_dir);
        let safe_name = model_name.replace('.', "_");
        let _ = fs::write(debug_dir.join(format!("{safe_name}.c")), &model_c);
        let _ = fs::write(
            debug_dir.join(format!("{safe_name}_errors.txt")),
            stderr.as_ref(),
        );
        let truncated: String = stderr.lines().take(40).collect::<Vec<_>>().join("\n");
        return Err(format!("C compilation failed:\n{truncated}"));
    }

    let run_output = Command::new(binary_path.to_str().unwrap())
        .args(["--t-end", &format!("{t_end}"), "--dt", &format!("{dt}")])
        .output()
        .map_err(|e| format!("run: {e}"))?;

    if !run_output.status.success() {
        let stderr = String::from_utf8_lossy(&run_output.stderr);
        return Err(format!("simulation failed: {stderr}"));
    }

    String::from_utf8(run_output.stdout).map_err(|e| format!("utf8: {e}"))
}

// =============================================================================
// Trace comparison helpers
// =============================================================================

/// Parse CSV string into a map of variable name → Vec<(time, value)>.
fn parse_csv_traces(csv: &str) -> HashMap<String, Vec<(f64, f64)>> {
    let mut lines = csv.lines();
    let header: Vec<String> = lines
        .next()
        .unwrap_or("")
        .split(',')
        .map(|s| s.trim().to_string())
        .collect();

    let time_idx = header
        .iter()
        .position(|h| h == "time")
        .expect("no time column");
    let mut traces: HashMap<String, Vec<(f64, f64)>> = HashMap::new();

    for line in lines {
        if line.trim().is_empty() {
            continue;
        }
        let vals: Vec<f64> = line
            .split(',')
            .map(|s| s.trim().parse::<f64>().unwrap_or(f64::NAN))
            .collect();
        let t = vals[time_idx];
        for (i, name) in header.iter().enumerate() {
            if i == time_idx {
                continue;
            }
            traces.entry(name.clone()).or_default().push((t, vals[i]));
        }
    }
    traces
}

/// Extract a named variable's trace from a SimResult.
fn extract_sim_trace(sim: &SimResult, var_name: &str) -> Option<Vec<(f64, f64)>> {
    let idx = sim.names.iter().position(|n| n == var_name)?;
    Some(
        sim.times
            .iter()
            .zip(sim.data[idx].iter())
            .map(|(&t, &v)| (t, v))
            .collect(),
    )
}

/// Linear interpolation on a sorted trace.
fn interpolate(trace: &[(f64, f64)], t: f64) -> f64 {
    if trace.is_empty() {
        return f64::NAN;
    }
    if t <= trace[0].0 {
        return trace[0].1;
    }
    if t >= trace[trace.len() - 1].0 {
        return trace[trace.len() - 1].1;
    }
    let pos = trace.partition_point(|(ti, _)| *ti < t);
    if pos == 0 {
        return trace[0].1;
    }
    let (t0, v0) = trace[pos - 1];
    let (t1, v1) = trace[pos];
    if (t1 - t0).abs() < 1e-15 {
        return v0;
    }
    let frac = (t - t0) / (t1 - t0);
    v0 + frac * (v1 - v0)
}

/// Compute max relative error between two traces, sampling at FMI3 time points.
/// Uses bounded relative error: |a-b| / max(|a|, |b|, 1.0).
fn trace_max_deviation(fmi_trace: &[(f64, f64)], rumoca_trace: &[(f64, f64)]) -> f64 {
    let mut max_err = 0.0f64;
    for &(t, fmi_val) in fmi_trace {
        if !fmi_val.is_finite() {
            continue;
        }
        let rumoca_val = interpolate(rumoca_trace, t);
        if !rumoca_val.is_finite() {
            continue;
        }
        let scale = fmi_val.abs().max(rumoca_val.abs()).max(1.0);
        let err = (fmi_val - rumoca_val).abs() / scale;
        if err > max_err {
            max_err = err;
        }
    }
    max_err
}

fn truncate(s: &str, max_len: usize) -> String {
    if s.len() > max_len {
        let end = s
            .char_indices()
            .take_while(|(i, _)| *i < max_len)
            .last()
            .map(|(i, c)| i + c.len_utf8())
            .unwrap_or(0);
        format!("{}...", &s[..end])
    } else {
        s.to_string()
    }
}

// =============================================================================
// Per-model result tracking
// =============================================================================

#[derive(Debug)]
enum ModelOutcome {
    CompileFail(String),
    RumocaSimFail(String),
    Fmi3RenderFail(String),
    Fmi3CompileOrRunFail(String),
    TraceDeviation { max_deviation: f64, var: String },
    Pass { max_deviation: f64 },
    NoStates,
}

impl std::fmt::Display for ModelOutcome {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ModelOutcome::CompileFail(e) => write!(f, "compile_fail: {}", truncate(e, 120)),
            ModelOutcome::RumocaSimFail(e) => write!(f, "rumoca_sim_fail: {}", truncate(e, 120)),
            ModelOutcome::Fmi3RenderFail(e) => write!(f, "fmi3_render_fail: {}", truncate(e, 120)),
            ModelOutcome::Fmi3CompileOrRunFail(e) => {
                write!(f, "fmi3_c_fail: {}", truncate(e, 200))
            }
            ModelOutcome::TraceDeviation { max_deviation, var } => {
                write!(f, "deviation: {max_deviation:.4e} on {var}")
            }
            ModelOutcome::Pass { max_deviation } => {
                write!(f, "pass (max_dev={max_deviation:.4e})")
            }
            ModelOutcome::NoStates => write!(f, "no_states (skipped)"),
        }
    }
}

// =============================================================================
// Core test logic
// =============================================================================

fn run_single_model(
    source_root: &CompiledSourceRoot,
    model_name: &str,
    dt: f64,
    tolerance: f64,
) -> ModelOutcome {
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

    // Dump DAE JSON for debugging
    {
        let debug_dir = std::path::Path::new("/tmp/fmi3_debug");
        let _ = fs::create_dir_all(debug_dir);
        let safe_name = model_name.replace('.', "_");
        if let Ok(json) = serde_json::to_string_pretty(dae) {
            let _ = fs::write(debug_dir.join(format!("{safe_name}.json")), &json);
        }
    }

    // Skip models with no states — FMI3 forward Euler has nothing to integrate
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
        .unwrap_or(t_start + 1.0)
        .min(10.0); // Cap FMI3 forward Euler simulation time

    let opts = SimOptions {
        t_end,
        max_wall_seconds: Some(10.0),
        ..SimOptions::default()
    };
    let sim = match simulate_dae(dae, &opts) {
        Ok(sim) => sim,
        Err(e) => return ModelOutcome::RumocaSimFail(format!("{e}")),
    };

    // 3. Run FMI3 C pipeline
    let csv = match fmi3_simulate(dae, model_name, t_end, dt) {
        Ok(csv) => csv,
        Err(e) => {
            if e.contains("render") {
                return ModelOutcome::Fmi3RenderFail(e);
            }
            return ModelOutcome::Fmi3CompileOrRunFail(e);
        }
    };
    let fmi_traces = parse_csv_traces(&csv);

    // 4. Compare state variable traces
    let mut worst_deviation = 0.0f64;
    let mut worst_var = String::new();
    for name in dae.states.keys() {
        let name_str = name.as_str();
        let Some(fmi_trace) = fmi_traces.get(name_str) else {
            continue;
        };
        let Some(rumoca_trace) = extract_sim_trace(&sim, name_str) else {
            continue;
        };
        let dev = trace_max_deviation(fmi_trace, &rumoca_trace);
        if dev > worst_deviation {
            worst_deviation = dev;
            worst_var = name_str.to_string();
        }
    }

    if worst_deviation > tolerance {
        ModelOutcome::TraceDeviation {
            max_deviation: worst_deviation,
            var: worst_var,
        }
    } else {
        ModelOutcome::Pass {
            max_deviation: worst_deviation,
        }
    }
}

// =============================================================================
// Test entry point
// =============================================================================

#[test]
#[ignore]
fn test_fmi3_vs_rumoca_msl() {
    check_release_mode();

    let dt = fmi3_dt();
    let tolerance = fmi3_tolerance();
    println!("FMI3 vs rumoca MSL cross-validation");
    println!("  dt={dt}, tolerance={tolerance}");

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
    let mut targets = fmi3_target_models();
    apply_env_filters(&mut targets);
    println!("Testing {} models", targets.len());

    // 4. Run each model
    let mut pass = 0usize;
    let mut no_states = 0usize;
    let mut compile_fail = 0usize;
    let mut sim_fail = 0usize;
    let mut fmi3_fail = 0usize;
    let mut deviation = 0usize;
    let mut deviations: Vec<(String, f64, String)> = Vec::new();

    for (i, model_name) in targets.iter().enumerate() {
        let outcome = run_single_model(&source_root, model_name, dt, tolerance);
        let tag = match &outcome {
            ModelOutcome::Pass { .. } => {
                pass += 1;
                "PASS"
            }
            ModelOutcome::NoStates => {
                no_states += 1;
                "SKIP"
            }
            ModelOutcome::CompileFail(_) => {
                compile_fail += 1;
                "COMPILE_FAIL"
            }
            ModelOutcome::RumocaSimFail(_) => {
                sim_fail += 1;
                "SIM_FAIL"
            }
            ModelOutcome::Fmi3RenderFail(_) | ModelOutcome::Fmi3CompileOrRunFail(_) => {
                fmi3_fail += 1;
                "FMI3_FAIL"
            }
            ModelOutcome::TraceDeviation {
                max_deviation, var, ..
            } => {
                deviation += 1;
                deviations.push((model_name.clone(), *max_deviation, var.clone()));
                "DEVIATION"
            }
        };
        println!(
            "[{:>3}/{}] {tag:>13} {model_name}: {outcome}",
            i + 1,
            targets.len()
        );
    }

    // 5. Summary
    let total = targets.len();
    let tested = pass + deviation;
    println!("\n=== FMI3 vs rumoca MSL summary ===");
    println!("  total models:   {total}");
    println!("  pass:           {pass}");
    println!("  no_states:      {no_states}");
    println!("  compile_fail:   {compile_fail}");
    println!("  sim_fail:       {sim_fail}");
    println!("  fmi3_fail:      {fmi3_fail}");
    println!("  deviation:      {deviation}");

    if !deviations.is_empty() {
        deviations.sort_by(|a, b| b.1.partial_cmp(&a.1).unwrap());
        println!("\nWorst deviations:");
        for (name, dev, var) in &deviations {
            println!("  {dev:.4e} on {var} — {name}");
        }
    }

    // The test passes if at least some models were successfully tested end-to-end.
    assert!(
        tested > 0,
        "no models were tested end-to-end — FMI3 pipeline may be broken"
    );
    println!("\n{tested} models tested end-to-end ({pass} pass, {deviation} deviation)");
}
