//! Embedded C cross-validation against rumoca's built-in simulator on MSL models.
//!
//! Tests the same curated set of MSL models as the FMI2 test, but using the
//! embedded C template with its built-in RK4 integrator. For each model we:
//! 1. Compile the model from the MSL source root
//! 2. Run rumoca's built-in diffsol simulator to get a reference trace
//! 3. Render embedded C source with a test harness, compile with `cc`, run to get CSV
//! 4. Compare the two traces and assert they agree within tolerance
//!
//! Run with:
//! ```text
//! cargo test --release --package rumoca-test-msl --test embedded_c_msl_test -- --ignored --nocapture
//! ```
//!
//! Environment variables:
//! - `RUMOCA_EMBEDDED_MSL_MATCH=pattern` — filter models by substring match
//! - `RUMOCA_EMBEDDED_MSL_DT=0.0001` — RK4 step size (default 0.0001)
//! - `RUMOCA_EMBEDDED_MSL_TOLERANCE=0.20` — max allowed trace deviation (default 0.20)

use flate2::read::GzDecoder;
use rumoca_session::codegen::{render_dae_template_with_name, templates};
use rumoca_session::compile::{CompilationResult, CompiledSourceRoot, PhaseResult};
use rumoca_session::parsing::parse_files_parallel_lenient;
use rumoca_sim::{SimOptions, SimResult};
use rumoca_solver_diffsol::simulate_dae;
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
            "\n\nERROR: Embedded C MSL tests must be run in RELEASE mode!\n\
             cargo test --release --package rumoca-test-msl --test embedded_c_msl_test -- --ignored --nocapture\n"
        );
    }
}

// =============================================================================
// MSL download infrastructure (shared with fmi2_msl_test.rs)
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
// Model target selection (same as FMI2 test)
// =============================================================================

fn target_models() -> Vec<String> {
    let path = std::path::Path::new(env!("CARGO_MANIFEST_DIR"))
        .join("tests/msl_tests/msl_simulation_targets_180.json");
    let raw = std::fs::read_to_string(&path)
        .unwrap_or_else(|e| panic!("failed to read target list {}: {e}", path.display()));
    let value: serde_json::Value = serde_json::from_str(&raw).expect("invalid JSON in target list");
    value
        .as_array()
        .expect("target list must be a JSON array")
        .iter()
        .filter_map(|v| v.as_str().map(|s| s.to_string()))
        .collect()
}

fn apply_env_filters(names: &mut Vec<String>) {
    if let Ok(pattern) = std::env::var("RUMOCA_EMBEDDED_MSL_MATCH") {
        let pattern = pattern.trim().to_string();
        if !pattern.is_empty() {
            names.retain(|n| n.contains(&pattern));
            println!(
                "RUMOCA_EMBEDDED_MSL_MATCH={pattern} → {} models",
                names.len()
            );
        }
    }
    if let Ok(raw) = std::env::var("RUMOCA_EMBEDDED_MSL_LIMIT")
        && let Ok(limit) = raw.trim().parse::<usize>()
        && names.len() > limit
    {
        names.truncate(limit);
        println!("RUMOCA_EMBEDDED_MSL_LIMIT={limit} → {} models", names.len());
    }
}

fn embedded_dt() -> f64 {
    std::env::var("RUMOCA_EMBEDDED_MSL_DT")
        .ok()
        .and_then(|s| s.trim().parse().ok())
        .unwrap_or(0.0001)
}

fn embedded_tolerance() -> f64 {
    std::env::var("RUMOCA_EMBEDDED_MSL_TOLERANCE")
        .ok()
        .and_then(|s| s.trim().parse().ok())
        .unwrap_or(0.20)
}

// =============================================================================
// Embedded C pipeline
// =============================================================================

/// Sanitize a variable name for C (dots → underscores).
fn sanitize_c_name(name: &str) -> String {
    name.chars()
        .map(|c| {
            if c.is_alphanumeric() || c == '_' {
                c
            } else {
                '_'
            }
        })
        .collect()
}

/// Generate a test harness that includes the embedded C model and outputs CSV.
fn make_test_harness(
    model_h: &str,
    model_c: &str,
    dae: &rumoca_ir_dae::Dae,
    model_name: &str,
    t_end: f64,
    dt: f64,
) -> (String, String) {
    // Sanitize model name the same way the template does (dots → underscores)
    let safe_name = model_name.replace('.', "_");

    // Build CSV header from state names
    let state_names: Vec<&str> = dae.states.keys().map(|s| s.as_str()).collect();
    let header_str = std::iter::once("time".to_string())
        .chain(state_names.iter().map(|s| s.to_string()))
        .collect::<Vec<_>>()
        .join(",");

    // Build printf format and args for states using named struct fields
    let printf_args: String = state_names
        .iter()
        .map(|n| format!("m.{}", sanitize_c_name(n)))
        .collect::<Vec<_>>()
        .join(", ");

    let printf_fmt: String = std::iter::once("%.10g".to_string())
        .chain(state_names.iter().map(|_| ",%.10g".to_string()))
        .collect::<Vec<_>>()
        .join("");

    let harness_main = format!(
        r#"/* Embedded C test harness — auto-generated */
#include <stdio.h>
#include "{safe_name}.h"

/* Inline the generated model implementation */
{model_c}

int main(void) {{
    {safe_name}_t m;
    {safe_name}_init(&m);

    double t = 0.0;
    double dt = {dt};
    double t_end = {t_end};
    int n_steps = (int)(t_end / dt);

    printf("{header_str}\n");

    for (int step = 0; step <= n_steps; step++) {{
        printf("{printf_fmt}\n", t, {printf_args});
        {safe_name}_step(&m, t, dt);
        t += dt;
    }}

    return 0;
}}
"#
    );

    (model_h.to_string(), harness_main)
}

/// Render embedded C model, wrap in test harness, compile, run, return CSV output.
fn embedded_simulate(
    dae: &rumoca_ir_dae::Dae,
    model_name: &str,
    t_end: f64,
    dt: f64,
) -> Result<String, String> {
    let safe_name = model_name.replace('.', "_");

    // Render embedded C directly from the compiler-owned DAE
    let header = render_dae_template_with_name(dae, templates::EMBEDDED_C_H, model_name)
        .map_err(|e| format!("render embedded C header: {e}"))?;
    let impl_c = render_dae_template_with_name(dae, templates::EMBEDDED_C_IMPL, model_name)
        .map_err(|e| format!("render embedded C impl: {e}"))?;

    let (model_h, harness_main) = make_test_harness(&header, &impl_c, dae, model_name, t_end, dt);

    let dir = tempdir().map_err(|e| format!("tempdir: {e}"))?;
    let header_path = dir.path().join(format!("{safe_name}.h"));
    let src_path = dir.path().join("test.c");
    let binary_path = dir.path().join("test_embedded");

    fs::write(&header_path, &model_h).map_err(|e| format!("write header: {e}"))?;
    fs::write(&src_path, &harness_main).map_err(|e| format!("write test.c: {e}"))?;

    // Dump for debugging
    {
        let debug_dir = std::path::Path::new("/tmp/embedded_debug");
        let _ = fs::create_dir_all(debug_dir);
        let _ = fs::write(debug_dir.join(format!("{safe_name}.h")), &model_h);
        let _ = fs::write(debug_dir.join(format!("{safe_name}.c")), &harness_main);
    }

    let compile_output = Command::new("cc")
        .args([
            "-O2",
            "-Wall",
            "-Wno-unused-function",
            "-o",
            binary_path.to_str().unwrap(),
            src_path.to_str().unwrap(),
            "-lm",
        ])
        .output()
        .map_err(|e| format!("cc invoke: {e}"))?;

    if !compile_output.status.success() {
        let stderr = String::from_utf8_lossy(&compile_output.stderr);
        let debug_dir = std::path::Path::new("/tmp/embedded_debug");
        let _ = fs::create_dir_all(debug_dir);
        let safe_name = model_name.replace('.', "_");
        let _ = fs::write(
            debug_dir.join(format!("{safe_name}_errors.txt")),
            stderr.as_ref(),
        );
        let truncated: String = stderr.lines().take(40).collect::<Vec<_>>().join("\n");
        return Err(format!("C compilation failed:\n{truncated}"));
    }

    let run_output = Command::new(binary_path.to_str().unwrap())
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
        if vals.len() <= time_idx {
            continue;
        }
        let t = vals[time_idx];
        for (i, name) in header.iter().enumerate() {
            if i == time_idx || i >= vals.len() {
                continue;
            }
            traces.entry(name.clone()).or_default().push((t, vals[i]));
        }
    }
    traces
}

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

fn trace_max_deviation(emb_trace: &[(f64, f64)], rumoca_trace: &[(f64, f64)]) -> f64 {
    let mut max_err = 0.0f64;
    for &(t, emb_val) in emb_trace {
        if !emb_val.is_finite() {
            continue;
        }
        let rumoca_val = interpolate(rumoca_trace, t);
        if !rumoca_val.is_finite() {
            continue;
        }
        let scale = emb_val.abs().max(rumoca_val.abs()).max(1.0);
        let err = (emb_val - rumoca_val).abs() / scale;
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
    EmbeddedRenderFail(String),
    EmbeddedCompileOrRunFail(String),
    TraceDeviation { max_deviation: f64, var: String },
    Pass { max_deviation: f64 },
    NoStates,
}

impl std::fmt::Display for ModelOutcome {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ModelOutcome::CompileFail(e) => write!(f, "compile_fail: {}", truncate(e, 120)),
            ModelOutcome::RumocaSimFail(e) => write!(f, "rumoca_sim_fail: {}", truncate(e, 120)),
            ModelOutcome::EmbeddedRenderFail(e) => {
                write!(f, "embedded_render_fail: {}", truncate(e, 120))
            }
            ModelOutcome::EmbeddedCompileOrRunFail(e) => {
                write!(f, "embedded_c_fail: {}", truncate(e, 200))
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
    // 1. Compile (with structural preparation for correct equation ordering)
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

    if dae.states.is_empty() {
        return ModelOutcome::NoStates;
    }

    // 2. Run rumoca simulator for reference trace
    let t_start = result
        .experiment_start_time
        .filter(|t| t.is_finite())
        .unwrap_or(0.0);
    let t_end = result
        .experiment_stop_time
        .filter(|t| t.is_finite() && *t > t_start)
        .unwrap_or(t_start + 1.0)
        .min(10.0);

    let opts = SimOptions {
        t_end,
        max_wall_seconds: Some(10.0),
        ..SimOptions::default()
    };
    let sim = match simulate_dae(dae, &opts) {
        Ok(sim) => sim,
        Err(e) => return ModelOutcome::RumocaSimFail(format!("{e}")),
    };

    // 3. Run embedded C pipeline against the compiler-owned DAE.
    let csv = match embedded_simulate(dae, model_name, t_end, dt) {
        Ok(csv) => csv,
        Err(e) => {
            if e.contains("render") {
                return ModelOutcome::EmbeddedRenderFail(e);
            }
            return ModelOutcome::EmbeddedCompileOrRunFail(e);
        }
    };
    let emb_traces = parse_csv_traces(&csv);

    // 4. Compare state variable traces
    let mut worst_deviation = 0.0f64;
    let mut worst_var = String::new();
    for name in dae.states.keys() {
        let name_str = name.as_str();
        let Some(emb_trace) = emb_traces.get(name_str) else {
            continue;
        };
        let Some(rumoca_trace) = extract_sim_trace(&sim, name_str) else {
            continue;
        };
        let dev = trace_max_deviation(emb_trace, &rumoca_trace);
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
fn test_embedded_c_vs_rumoca_msl() {
    check_release_mode();

    let dt = embedded_dt();
    let tolerance = embedded_tolerance();
    println!("Embedded C (RK4) vs rumoca MSL cross-validation");
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
    let mut targets = target_models();
    apply_env_filters(&mut targets);
    println!("Testing {} models", targets.len());

    // 4. Run each model
    let mut pass = 0usize;
    let mut no_states = 0usize;
    let mut compile_fail = 0usize;
    let mut sim_fail = 0usize;
    let mut embedded_fail = 0usize;
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
            ModelOutcome::EmbeddedRenderFail(_) | ModelOutcome::EmbeddedCompileOrRunFail(_) => {
                embedded_fail += 1;
                "EMBEDDED_FAIL"
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
            "[{:>3}/{}] {tag:>15} {model_name}: {outcome}",
            i + 1,
            targets.len()
        );
    }

    // 5. Summary
    let total = targets.len();
    let tested = pass + deviation;
    println!("\n=== Embedded C vs rumoca MSL summary ===");
    println!("  total models:     {total}");
    println!("  pass:             {pass}");
    println!("  no_states:        {no_states}");
    println!("  compile_fail:     {compile_fail}");
    println!("  sim_fail:         {sim_fail}");
    println!("  embedded_fail:    {embedded_fail}");
    println!("  deviation:        {deviation}");

    if !deviations.is_empty() {
        deviations.sort_by(|a, b| b.1.partial_cmp(&a.1).unwrap());
        println!("\nWorst deviations:");
        for (name, dev, var) in &deviations {
            println!("  {dev:.4e} on {var} — {name}");
        }
    }

    assert!(
        tested > 0,
        "no models were tested end-to-end — embedded C pipeline may be broken"
    );
    println!("\n{tested} models tested end-to-end ({pass} pass, {deviation} deviation)");
}
