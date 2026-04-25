//! Backend stress test — 30 models across all backends.
//!
//! Tests ~24 MSL models (from backend_stress_targets.json) plus 6 inline
//! synthetic models covering targeted math ops (sin, cos, exp, sqrt, division,
//! pow). Each backend gets its own `#[test] #[ignore]` function.
//!
//! Run with:
//! ```text
//! # One backend:
//! cargo test --release --package rumoca-test-msl --test backend_stress_test stress_test_onnx -- --ignored --nocapture
//!
//! # All backends:
//! cargo test --release --package rumoca-test-msl --test backend_stress_test -- --ignored --nocapture
//! ```
//!
//! Environment variables:
//! - `RUMOCA_STRESS_MATCH=pattern` — filter models by substring match
//! - `RUMOCA_STRESS_LIMIT=N` — cap number of models tested

use flate2::read::GzDecoder;
use rumoca_compile::codegen::{render_dae_template_with_name, templates};
use rumoca_compile::compile::{CompilationResult, CompiledSourceRoot, PhaseResult};
use rumoca_compile::parsing::parse_files_parallel_lenient;
use rumoca_sim::{SimOptions, SimResult};
use rumoca_solver_diffsol::simulate_dae;
use std::collections::HashMap;
use std::fs;
use std::io::Read;
use std::path::{Path, PathBuf};
use std::process::Command;
use std::sync::OnceLock;
use tar::Archive;
use tempfile::tempdir;
use walkdir::WalkDir;

// =============================================================================
// MSL download infrastructure (copy-pasted per convention)
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
// Shared source root (built once across all backends)
// =============================================================================

static MSL_SOURCE_ROOT: OnceLock<CompiledSourceRoot> = OnceLock::new();

fn get_msl_source_root() -> &'static CompiledSourceRoot {
    MSL_SOURCE_ROOT.get_or_init(|| {
        let msl_dir = ensure_msl_downloaded().expect("Failed to download MSL");
        let mo_files = find_mo_files(&msl_dir);
        println!("Parsing {} MSL files...", mo_files.len());
        let (successes, failures) = parse_files_parallel_lenient(&mo_files);
        println!("Parsed {} OK, {} failures", successes.len(), failures.len());
        CompiledSourceRoot::from_parsed_batch_tolerant(successes)
            .expect("failed to build source-root index")
    })
}

// =============================================================================
// Model definitions
// =============================================================================

struct ModelDef {
    name: String,
    source: ModelSource,
}

enum ModelSource {
    Msl,
    Inline(String),
}

fn load_msl_targets() -> Vec<String> {
    let path =
        Path::new(env!("CARGO_MANIFEST_DIR")).join("tests/msl_tests/backend_stress_targets.json");
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

fn synthetic_models() -> Vec<(String, String)> {
    vec![
        (
            "SinDecay".to_string(),
            r#"model SinDecay
  Real x(start=1);
equation
  der(x) = -sin(x);
end SinDecay;"#
                .to_string(),
        ),
        (
            "DampedPendulum".to_string(),
            r#"model DampedPendulum
  parameter Real g = 9.81;
  parameter Real L = 1.0;
  parameter Real c = 0.5;
  Real theta(start=1);
  Real omega(start=0);
equation
  der(theta) = omega;
  der(omega) = -(g/L)*sin(theta) - c*omega;
end DampedPendulum;"#
                .to_string(),
        ),
        (
            "ExpDecay".to_string(),
            r#"model ExpDecay
  Real x(start=2);
equation
  der(x) = -exp(-x);
end ExpDecay;"#
                .to_string(),
        ),
        (
            "SqrtDrive".to_string(),
            r#"model SqrtDrive
  Real x(start=4);
equation
  der(x) = -sqrt(x);
end SqrtDrive;"#
                .to_string(),
        ),
        (
            "LogisticGrowth".to_string(),
            r#"model LogisticGrowth
  parameter Real r = 1;
  parameter Real K = 10;
  Real x(start=0.5);
equation
  der(x) = r*x*(1 - x/K);
end LogisticGrowth;"#
                .to_string(),
        ),
        (
            "TrigDrive".to_string(),
            r#"model TrigDrive
  Real x(start=0);
equation
  der(x) = cos(time) - x;
end TrigDrive;"#
                .to_string(),
        ),
    ]
}

fn build_model_list() -> Vec<ModelDef> {
    let mut models: Vec<ModelDef> = Vec::new();

    // MSL models
    let mut msl_names = load_msl_targets();

    // Synthetic models
    let synthetics = synthetic_models();

    // Apply env filters to combined name list for matching
    let mut all_names: Vec<String> = msl_names
        .iter()
        .chain(synthetics.iter().map(|(n, _)| n))
        .cloned()
        .collect();
    apply_env_filters(&mut all_names);

    // Rebuild filtered lists
    msl_names.retain(|n| all_names.contains(n));
    let synthetics: Vec<(String, String)> = synthetics
        .into_iter()
        .filter(|(n, _)| all_names.contains(n))
        .collect();

    for name in msl_names {
        models.push(ModelDef {
            name,
            source: ModelSource::Msl,
        });
    }
    for (name, source) in synthetics {
        models.push(ModelDef {
            name,
            source: ModelSource::Inline(source),
        });
    }

    models
}

fn apply_env_filters(names: &mut Vec<String>) {
    if let Ok(pattern) = std::env::var("RUMOCA_STRESS_MATCH") {
        let pattern = pattern.trim().to_string();
        if !pattern.is_empty() {
            names.retain(|n| n.contains(&pattern));
            println!("RUMOCA_STRESS_MATCH={pattern} → {} models", names.len());
        }
    }
    if let Ok(raw) = std::env::var("RUMOCA_STRESS_LIMIT")
        && let Ok(limit) = raw.trim().parse::<usize>()
        && names.len() > limit
    {
        names.truncate(limit);
        println!("RUMOCA_STRESS_LIMIT={limit} → {} models", names.len());
    }
}

// =============================================================================
// Release mode check
// =============================================================================

fn check_release_mode() {
    #[cfg(debug_assertions)]
    {
        panic!(
            "\n\nERROR: Backend stress tests must be run in RELEASE mode!\n\
             cargo test --release --package rumoca-test-msl --test backend_stress_test -- --ignored --nocapture\n"
        );
    }
}

// =============================================================================
// Runtime detection
// =============================================================================

fn python_has_casadi() -> bool {
    Command::new("python3")
        .args(["-c", "import casadi; import numpy"])
        .output()
        .map(|o| o.status.success())
        .unwrap_or(false)
}

fn python_has_sympy() -> bool {
    Command::new("python3")
        .args(["-c", "import sympy"])
        .output()
        .map(|o| o.status.success())
        .unwrap_or(false)
}

fn python_has_onnx() -> bool {
    Command::new("python3")
        .args(["-c", "import onnx; import onnxruntime; import numpy"])
        .output()
        .map(|o| o.status.success())
        .unwrap_or(false)
}

fn cc_available() -> bool {
    Command::new("cc")
        .arg("--version")
        .output()
        .map(|o| o.status.success())
        .unwrap_or(false)
}

fn julia_has_mtk() -> bool {
    Command::new("julia")
        .args(["-e", "using ModelingToolkit; using DifferentialEquations"])
        .output()
        .map(|o| o.status.success())
        .unwrap_or(false)
}

fn python_has_jax() -> bool {
    Command::new("python3")
        .args(["-c", "import jax; import diffrax; import numpy"])
        .output()
        .map(|o| o.status.success())
        .unwrap_or(false)
}

// =============================================================================
// DAE compilation for inline models
// =============================================================================

fn compile_inline_model(source: &str, model_name: &str) -> Result<CompilationResult, String> {
    let parsed = rumoca_compile::parsing::parse_source_to_ast(source, &format!("{model_name}.mo"))
        .map_err(|e| format!("parse: {e}"))?;
    let source_root =
        CompiledSourceRoot::from_parsed_batch_tolerant(vec![(format!("{model_name}.mo"), parsed)])
            .map_err(|e| format!("source_root: {e}"))?;
    let report = source_root.compile_model_strict_reachable_with_recovery(model_name);
    match report.requested_result {
        Some(PhaseResult::Success(boxed)) => Ok(*boxed),
        Some(PhaseResult::Failed { error, .. }) => Err(error),
        _ => Err(report.failure_summary(0)),
    }
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

fn trace_max_deviation(backend_trace: &[(f64, f64)], ref_trace: &[(f64, f64)]) -> f64 {
    let mut max_err = 0.0f64;
    for &(t, val) in backend_trace {
        if !val.is_finite() {
            continue;
        }
        let ref_val = interpolate(ref_trace, t);
        if !ref_val.is_finite() {
            continue;
        }
        let scale = val.abs().max(ref_val.abs()).max(1.0);
        let err = (val - ref_val).abs() / scale;
        if err > max_err {
            max_err = err;
        }
    }
    max_err
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
// Per-model outcome tracking
// =============================================================================

#[derive(Debug)]
enum ModelOutcome {
    CompileFail(String),
    RumocaSimFail(String),
    RenderFail(String),
    BackendFail(String),
    TraceDeviation { max_deviation: f64, var: String },
    Pass { max_deviation: f64 },
    NoStates,
}

impl std::fmt::Display for ModelOutcome {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ModelOutcome::CompileFail(e) => write!(f, "compile_fail: {}", truncate(e, 120)),
            ModelOutcome::RumocaSimFail(e) => write!(f, "rumoca_sim_fail: {}", truncate(e, 120)),
            ModelOutcome::RenderFail(e) => write!(f, "render_fail: {}", truncate(e, 120)),
            ModelOutcome::BackendFail(e) => write!(f, "backend_fail: {e}"),
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

fn outcome_tag(outcome: &ModelOutcome) -> &'static str {
    match outcome {
        ModelOutcome::Pass { .. } => "PASS",
        ModelOutcome::NoStates => "SKIP",
        ModelOutcome::CompileFail(_) => "COMPILE_FAIL",
        ModelOutcome::RumocaSimFail(_) => "SIM_FAIL",
        ModelOutcome::RenderFail(_) => "RENDER_FAIL",
        ModelOutcome::BackendFail(_) => "BACKEND_FAIL",
        ModelOutcome::TraceDeviation { .. } => "DEVIATION",
    }
}

// =============================================================================
// Compile model to DAE (MSL or inline)
// =============================================================================

fn compile_to_dae(
    model: &ModelDef,
    source_root: &CompiledSourceRoot,
) -> Result<(rumoca_ir_dae::Dae, f64), String> {
    // Scalarize up front so every backend below (CasADi/FMI2/FMI3/embedded-C/
    // SymPy/ONNX/JAX/Julia) sees one equation per scalar state. The simulator
    // scalarizes again internally — idempotent.
    let (mut dae, t_end) = match &model.source {
        ModelSource::Msl => {
            let report = source_root.compile_model_strict_reachable_with_recovery(&model.name);
            let result: CompilationResult = match report.requested_result {
                Some(PhaseResult::Success(boxed)) => *boxed,
                Some(PhaseResult::Failed { error, .. }) => return Err(error),
                _ => return Err(report.failure_summary(0)),
            };
            let t_start = result
                .experiment_start_time
                .filter(|t| t.is_finite())
                .unwrap_or(0.0);
            let t_end = result
                .experiment_stop_time
                .filter(|t| t.is_finite() && *t > t_start)
                .unwrap_or(t_start + 1.0)
                .min(10.0);
            (result.dae, t_end)
        }
        ModelSource::Inline(source) => {
            let compiled = compile_inline_model(source, &model.name)?;
            (compiled.dae, 1.0)
        }
    };
    rumoca_phase_structural::scalarize::scalarize_equations(&mut dae);
    Ok((dae, t_end))
}

// =============================================================================
// Python execution helpers
// =============================================================================

fn run_python_script(code: &str, driver: &str, args: &[&str]) -> Result<String, String> {
    let dir = tempdir().map_err(|e| format!("tempdir: {e}"))?;
    let model_path = dir.path().join("model.py");
    let driver_path = dir.path().join("driver.py");
    fs::write(&model_path, code).map_err(|e| format!("write model: {e}"))?;
    fs::write(&driver_path, driver).map_err(|e| format!("write driver: {e}"))?;

    let output = Command::new("python3")
        .arg(driver_path.to_str().unwrap())
        .args(args)
        .output()
        .map_err(|e| format!("python3 invoke: {e}"))?;

    if !output.status.success() {
        let stderr = String::from_utf8_lossy(&output.stderr);
        let lines: Vec<&str> = stderr.lines().collect();
        let truncated = if lines.len() > 30 {
            lines[lines.len() - 30..].join("\n")
        } else {
            stderr.to_string()
        };
        return Err(format!("python failed:\n{truncated}"));
    }

    String::from_utf8(output.stdout).map_err(|e| format!("utf8: {e}"))
}

// =============================================================================
// Julia execution helper
// =============================================================================

fn run_julia_script(model_code: &str, driver: &str, args: &[&str]) -> Result<String, String> {
    let dir = tempdir().map_err(|e| format!("tempdir: {e}"))?;
    let model_path = dir.path().join("model.jl");
    let driver_path = dir.path().join("driver.jl");
    fs::write(&model_path, model_code).map_err(|e| format!("write model: {e}"))?;
    fs::write(&driver_path, driver).map_err(|e| format!("write driver: {e}"))?;

    let output = Command::new("julia")
        .arg("--project=@.")
        .arg(driver_path.to_str().unwrap())
        .args(args)
        .output()
        .map_err(|e| format!("julia invoke: {e}"))?;

    if !output.status.success() {
        let stderr = String::from_utf8_lossy(&output.stderr);
        let lines: Vec<&str> = stderr.lines().collect();
        let truncated = if lines.len() > 60 {
            // Show first 10 lines (error message) + last 20 lines (stacktrace tail)
            let head: Vec<&str> = lines[..10].to_vec();
            let tail: Vec<&str> = lines[lines.len() - 20..].to_vec();
            format!(
                "{}\n  ... ({} lines omitted) ...\n{}",
                head.join("\n"),
                lines.len() - 30,
                tail.join("\n")
            )
        } else {
            stderr.to_string()
        };
        return Err(format!("julia failed:\n{truncated}"));
    }

    String::from_utf8(output.stdout).map_err(|e| format!("utf8: {e}"))
}

// =============================================================================
// C compile + run helper
// =============================================================================

fn compile_and_run_c(sources: &[(&str, &str)], args: &[&str]) -> Result<String, String> {
    let dir = tempdir().map_err(|e| format!("tempdir: {e}"))?;
    let binary_path = dir.path().join("test_model");

    let mut src_paths = Vec::new();
    for (filename, content) in sources {
        let path = dir.path().join(filename);
        fs::write(&path, content).map_err(|e| format!("write {filename}: {e}"))?;
        if filename.ends_with(".c") {
            src_paths.push(path);
        }
    }

    let mut cmd = Command::new("cc");
    cmd.args(["-O2", "-Wall", "-Wno-unused-variable", "-o"])
        .arg(binary_path.to_str().unwrap());
    cmd.arg(format!("-I{}", dir.path().to_str().unwrap()));
    for path in &src_paths {
        cmd.arg(path.to_str().unwrap());
    }
    cmd.arg("-lm");

    let compile = cmd.output().map_err(|e| format!("cc invoke: {e}"))?;
    if !compile.status.success() {
        let stderr = String::from_utf8_lossy(&compile.stderr);
        let truncated: String = stderr.lines().take(40).collect::<Vec<_>>().join("\n");
        return Err(format!("C compilation failed:\n{truncated}"));
    }

    let run = Command::new(binary_path.to_str().unwrap())
        .args(args)
        .output()
        .map_err(|e| format!("run: {e}"))?;

    if !run.status.success() {
        let stderr = String::from_utf8_lossy(&run.stderr);
        return Err(format!("simulation failed: {stderr}"));
    }

    String::from_utf8(run.stdout).map_err(|e| format!("utf8: {e}"))
}

// =============================================================================
// Backend-specific simulation pipelines
// =============================================================================

// --- CasADi MX/SX ---

const CASADI_CSV_DRIVER: &str = r#"
import importlib.util, sys, os
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
z0_aug = np.concatenate([np.zeros(n_x), z0[:n_z_c]]) if n_z_c > 0 or n_x > 0 else np.array([])
z0_d = z0[n_z_c:] if len(z0) > n_z_c else np.array([])
p_full = np.concatenate([model['p0'], np.array([]), z0_d])
kwargs = dict(x0=model['x0'], p=p_full)
if len(z0_aug) > 0:
    kwargs['z0'] = z0_aug
result = integrator(**kwargs)
xf = np.array(result['xf'])

print("time," + ",".join(model['state_names']))
for i, t in enumerate(tgrid):
    row = [f"{t:.10g}"] + [f"{xf[j, i]:.10g}" for j in range(xf.shape[0])]
    print(",".join(row))
"#;

fn casadi_simulate(
    dae: &rumoca_ir_dae::Dae,
    model_name: &str,
    template: &str,
    t_end: f64,
) -> Result<String, String> {
    let code = render_dae_template_with_name(dae, template, model_name)
        .map_err(|e| format!("render: {e}"))?;
    let dt = t_end / 100.0;
    run_python_script(
        &code,
        CASADI_CSV_DRIVER,
        &[&format!("{dt}"), &format!("{t_end}")],
    )
}

// --- Embedded C ---

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

fn embedded_c_simulate(
    dae: &rumoca_ir_dae::Dae,
    model_name: &str,
    t_end: f64,
    dt: f64,
) -> Result<String, String> {
    let header = render_dae_template_with_name(dae, templates::EMBEDDED_C_H, model_name)
        .map_err(|e| format!("render header: {e}"))?;
    let impl_c = render_dae_template_with_name(dae, templates::EMBEDDED_C_IMPL, model_name)
        .map_err(|e| format!("render impl: {e}"))?;

    let mut state_names: Vec<&str> = dae.states.keys().map(|k| k.as_str()).collect();
    state_names.sort();
    let steps = (t_end / dt).round() as usize;

    let header_cols: Vec<String> = state_names.iter().map(|n| format!(",{n}")).collect();
    let print_cols: Vec<String> = state_names
        .iter()
        .map(|n| format!("        printf(\",%.10g\", m.{});", sanitize_c_name(n)))
        .collect();

    let header_name = format!("{}.h", model_name);
    let impl_name = format!("{}.c", model_name);
    let main_c = format!(
        r#"#include <stdio.h>
#include <math.h>
#include "{header_name}"

int main(void) {{
    {model_name}_t m;
    {model_name}_init(&m);

    double t = 0.0;
    double dt = {dt};
    int steps = {steps};

    printf("time{header_cols}\n");

    for (int i = 0; i <= steps; i++) {{
        printf("%.10g", t);
{print_cols}
        printf("\n");

        if (i < steps) {{
            {model_name}_step(&m, t, dt);
            t += dt;
        }}
    }}

    return 0;
}}
"#,
        model_name = model_name,
        header_name = header_name,
        dt = dt,
        steps = steps,
        header_cols = header_cols.join(""),
        print_cols = print_cols.join("\n"),
    );

    compile_and_run_c(
        &[
            (&header_name, &header),
            (&impl_name, &impl_c),
            ("main.c", &main_c),
        ],
        &[],
    )
}

// --- FMI2 ---

fn fmi2_simulate(
    dae: &rumoca_ir_dae::Dae,
    model_name: &str,
    t_end: f64,
    dt: f64,
) -> Result<String, String> {
    let model_c = render_dae_template_with_name(dae, templates::FMI2_MODEL, model_name)
        .map_err(|e| format!("render model: {e}"))?;

    let driver_c = render_dae_template_with_name(dae, templates::FMI2_TEST_DRIVER, model_name)
        .map_err(|e| format!("render driver: {e}"))?;

    compile_and_run_c(
        &[("model.c", &model_c), ("driver.c", &driver_c)],
        &["--t-end", &format!("{t_end}"), "--dt", &format!("{dt}")],
    )
}

// --- FMI3 ---

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

    compile_and_run_c(
        &[("model.c", &model_c), ("driver.c", &driver_c)],
        &["--t-end", &format!("{t_end}"), "--dt", &format!("{dt}")],
    )
}

// --- SymPy ---

const SYMPY_EVAL_DRIVER: &str = r#"
import importlib.util, sys, os, json

spec = importlib.util.spec_from_file_location("model", os.path.join(os.path.dirname(__file__), "model.py"))
mod = importlib.util.module_from_spec(spec)
spec.loader.exec_module(mod)

import sympy as sp

model = mod.Model()
summary = model.summary()

if summary['continuous_residual_count'] == 0:
    print(json.dumps({"state_names": [], "derivs_at_t0": {}}))
    sys.exit(0)

solution = model.solve_explicit()
if model.explicit_solution is None:
    print(json.dumps({"state_names": [], "derivs_at_t0": {}}))
    sys.exit(0)

subs = {}
for name, start in model.x_start.items():
    sym = model.x_index.get(name)
    if sym is not None:
        subs[model.x[sym]] = float(start) if start is not None else 0.0
for name, start in model.p_start.items():
    sym = model.p_index.get(name)
    if sym is not None:
        subs[model.p[sym]] = float(start) if start is not None else 0.0
subs[model.time] = 0.0

state_names = list(model.x_start.keys())
deriv_vals = {}
for target, expr in solution.items():
    val = float(expr.subs(subs))
    target_str = str(target)
    for sn in state_names:
        if sn in target_str:
            deriv_vals[sn] = val
            break

print(json.dumps({"state_names": state_names, "derivs_at_t0": deriv_vals}))
"#;

fn sympy_simulate(dae: &rumoca_ir_dae::Dae, model_name: &str) -> Result<String, String> {
    let code = render_dae_template_with_name(dae, templates::SYMPY, model_name)
        .map_err(|e| format!("render: {e}"))?;
    run_python_script(&code, SYMPY_EVAL_DRIVER, &[])
}

// --- ONNX ---

const ONNX_CSV_DRIVER: &str = r#"
import importlib.util, sys, os

spec = importlib.util.spec_from_file_location("model", os.path.join(os.path.dirname(__file__), "model.py"))
mod = importlib.util.module_from_spec(spec)
spec.loader.exec_module(mod)

print(mod.simulate())
"#;

fn onnx_simulate(dae: &rumoca_ir_dae::Dae, model_name: &str) -> Result<String, String> {
    let code = render_dae_template_with_name(dae, templates::ONNX, model_name)
        .map_err(|e| format!("render: {e}"))?;
    run_python_script(&code, ONNX_CSV_DRIVER, &[])
}

// --- JAX ---

const JAX_CSV_DRIVER: &str = r#"
import importlib.util, sys, os

spec = importlib.util.spec_from_file_location("model", os.path.join(os.path.dirname(__file__), "model.py"))
mod = importlib.util.module_from_spec(spec)
spec.loader.exec_module(mod)

print(mod.simulate_csv())
"#;

fn jax_simulate(dae: &rumoca_ir_dae::Dae, model_name: &str) -> Result<String, String> {
    let code = render_dae_template_with_name(dae, templates::JAX, model_name)
        .map_err(|e| format!("render: {e}"))?;
    run_python_script(&code, JAX_CSV_DRIVER, &[])
}

// --- Julia MTK ---

const JULIA_MTK_CSV_DRIVER: &str = r#"
include(joinpath(@__DIR__, "model.jl"))

t_end = parse(Float64, ARGS[1])
dt = parse(Float64, ARGS[2])

sol = simulate(; tspan=(0.0, t_end))

# Collect state names from the model
sys = create_model()
state_syms = ModelingToolkit.unknowns(sys)
state_names = [string(s) for s in state_syms]
# Strip "(t)" suffix from state names for CSV header
clean_names = [replace(n, "(t)" => "") for n in state_names]

# Output CSV
tgrid = 0.0:dt:t_end
print("time")
for n in clean_names
    print(",", n)
end
println()

for t in tgrid
    print(t)
    for (i, s) in enumerate(state_syms)
        val = sol(t; idxs=s)
        print(",", val)
    end
    println()
end
"#;

fn julia_mtk_simulate(
    dae: &rumoca_ir_dae::Dae,
    model_name: &str,
    t_end: f64,
) -> Result<String, String> {
    let code = render_dae_template_with_name(dae, templates::JULIA_MTK, model_name)
        .map_err(|e| format!("render: {e}"))?;
    let dt = t_end / 100.0;
    run_julia_script(
        &code,
        JULIA_MTK_CSV_DRIVER,
        &[&format!("{t_end}"), &format!("{dt}")],
    )
}

// =============================================================================
// Comparison logic per backend type
// =============================================================================

fn compare_csv_traces(
    csv: &str,
    dae: &rumoca_ir_dae::Dae,
    sim: &SimResult,
    tolerance: f64,
) -> ModelOutcome {
    let backend_traces = parse_csv_traces(csv);
    let mut worst_deviation = 0.0f64;
    let mut worst_var = String::new();

    for name in dae.states.keys() {
        let name_str = name.as_str();
        let Some(backend_trace) = backend_traces.get(name_str) else {
            continue;
        };
        let Some(ref_trace) = extract_sim_trace(sim, name_str) else {
            continue;
        };
        let dev = trace_max_deviation(backend_trace, &ref_trace);
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

fn compare_sympy_derivs(
    json_str: &str,
    dae: &rumoca_ir_dae::Dae,
    sim: &SimResult,
    tolerance: f64,
) -> ModelOutcome {
    let result: serde_json::Value = match serde_json::from_str(json_str.trim()) {
        Ok(v) => v,
        Err(e) => return ModelOutcome::BackendFail(format!("JSON parse: {e}")),
    };

    let derivs = match result["derivs_at_t0"].as_object() {
        Some(d) => d,
        None => return ModelOutcome::BackendFail("missing derivs_at_t0".to_string()),
    };

    if derivs.is_empty() {
        return ModelOutcome::NoStates;
    }

    let mut worst_deviation = 0.0f64;
    let mut worst_var = String::new();

    for (state_name, sympy_deriv_val) in derivs {
        let sympy_d = match sympy_deriv_val.as_f64() {
            Some(v) => v,
            None => continue,
        };
        if let Some(trace) = extract_sim_trace(sim, state_name)
            && trace.len() >= 2
        {
            let (t0, x0) = trace[0];
            let (t1, x1) = trace[1];
            let rumoca_d = (x1 - x0) / (t1 - t0);
            let scale = sympy_d.abs().max(rumoca_d.abs()).max(1.0);
            let err = (sympy_d - rumoca_d).abs() / scale;
            if err > worst_deviation {
                worst_deviation = err;
                worst_var = state_name.clone();
            }
        }
    }

    // Ignore dae for sympy — we already checked derivs
    let _ = dae;

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
// Generic test runner
// =============================================================================

enum BackendKind {
    CasadiMx,
    CasadiSx,
    EmbeddedC,
    Fmi2,
    Fmi3,
    Sympy,
    Onnx,
    Jax,
    JuliaMtk,
}

impl BackendKind {
    fn name(&self) -> &'static str {
        match self {
            BackendKind::CasadiMx => "CasADi MX",
            BackendKind::CasadiSx => "CasADi SX",
            BackendKind::EmbeddedC => "Embedded C",
            BackendKind::Fmi2 => "FMI2",
            BackendKind::Fmi3 => "FMI3",
            BackendKind::Sympy => "SymPy",
            BackendKind::Onnx => "ONNX",
            BackendKind::Jax => "JAX",
            BackendKind::JuliaMtk => "Julia MTK",
        }
    }

    fn tolerance(&self) -> f64 {
        match self {
            BackendKind::CasadiMx | BackendKind::CasadiSx => 0.01,
            BackendKind::Sympy => 0.10,
            BackendKind::Jax => 0.02,
            BackendKind::JuliaMtk => 0.05,
            _ => 0.05,
        }
    }
}

fn run_single_model(
    model: &ModelDef,
    source_root: &CompiledSourceRoot,
    backend: &BackendKind,
) -> ModelOutcome {
    // 1. Compile to DAE
    let (dae, t_end) = match compile_to_dae(model, source_root) {
        Ok(v) => v,
        Err(e) => return ModelOutcome::CompileFail(e),
    };

    if dae.states.is_empty() {
        return ModelOutcome::NoStates;
    }

    // 2. Reference simulation
    let sim_t_end = match backend {
        BackendKind::Sympy => 0.001,
        _ => t_end,
    };
    let opts = SimOptions {
        t_end: sim_t_end,
        max_wall_seconds: Some(30.0),
        ..SimOptions::default()
    };
    let sim = match simulate_dae(&dae, &opts) {
        Ok(sim) => sim,
        Err(e) => return ModelOutcome::RumocaSimFail(format!("{e}")),
    };

    // 3. Backend simulation + comparison
    let tolerance = backend.tolerance();
    let dt = 0.0001;

    match backend {
        BackendKind::CasadiMx => {
            match casadi_simulate(&dae, &model.name, templates::CASADI_MX, t_end) {
                Ok(csv) => compare_csv_traces(&csv, &dae, &sim, tolerance),
                Err(e) if e.contains("render:") => ModelOutcome::RenderFail(e),
                Err(e) => ModelOutcome::BackendFail(e),
            }
        }
        BackendKind::CasadiSx => {
            match casadi_simulate(&dae, &model.name, templates::CASADI_SX, t_end) {
                Ok(csv) => compare_csv_traces(&csv, &dae, &sim, tolerance),
                Err(e) if e.contains("render:") => ModelOutcome::RenderFail(e),
                Err(e) => ModelOutcome::BackendFail(e),
            }
        }
        BackendKind::EmbeddedC => match embedded_c_simulate(&dae, &model.name, t_end, dt) {
            Ok(csv) => compare_csv_traces(&csv, &dae, &sim, tolerance),
            Err(e) if e.contains("render:") => ModelOutcome::RenderFail(e),
            Err(e) => ModelOutcome::BackendFail(e),
        },
        BackendKind::Fmi2 => match fmi2_simulate(&dae, &model.name, t_end, dt) {
            Ok(csv) => compare_csv_traces(&csv, &dae, &sim, tolerance),
            Err(e) if e.contains("render") => ModelOutcome::RenderFail(e),
            Err(e) => ModelOutcome::BackendFail(e),
        },
        BackendKind::Fmi3 => match fmi3_simulate(&dae, &model.name, t_end, dt) {
            Ok(csv) => compare_csv_traces(&csv, &dae, &sim, tolerance),
            Err(e) if e.contains("render") => ModelOutcome::RenderFail(e),
            Err(e) => ModelOutcome::BackendFail(e),
        },
        BackendKind::Sympy => match sympy_simulate(&dae, &model.name) {
            Ok(json) => compare_sympy_derivs(&json, &dae, &sim, tolerance),
            Err(e) if e.contains("render:") => ModelOutcome::RenderFail(e),
            Err(e) => ModelOutcome::BackendFail(e),
        },
        BackendKind::Onnx => match onnx_simulate(&dae, &model.name) {
            Ok(csv) => compare_csv_traces(&csv, &dae, &sim, tolerance),
            Err(e) if e.contains("render:") => ModelOutcome::RenderFail(e),
            Err(e) => ModelOutcome::BackendFail(e),
        },
        BackendKind::Jax => match jax_simulate(&dae, &model.name) {
            Ok(csv) => compare_csv_traces(&csv, &dae, &sim, tolerance),
            Err(e) if e.contains("render:") => ModelOutcome::RenderFail(e),
            Err(e) => ModelOutcome::BackendFail(e),
        },
        BackendKind::JuliaMtk => match julia_mtk_simulate(&dae, &model.name, t_end) {
            Ok(csv) => compare_csv_traces(&csv, &dae, &sim, tolerance),
            Err(e) if e.contains("render:") => ModelOutcome::RenderFail(e),
            Err(e) => ModelOutcome::BackendFail(e),
        },
    }
}

fn run_stress_test(backend: BackendKind) {
    check_release_mode();

    let models = build_model_list();
    let source_root = get_msl_source_root();

    println!(
        "\n{:=<80}",
        format!(" {} stress test ({} models) ", backend.name(), models.len())
    );

    let mut pass = 0usize;
    let mut no_states = 0usize;
    let mut compile_fail = 0usize;
    let mut sim_fail = 0usize;
    let mut render_fail = 0usize;
    let mut backend_fail = 0usize;
    let mut deviation = 0usize;
    let mut deviations: Vec<(String, f64, String)> = Vec::new();

    for (i, model) in models.iter().enumerate() {
        let outcome = run_single_model(model, source_root, &backend);
        let tag = outcome_tag(&outcome);

        match &outcome {
            ModelOutcome::Pass { .. } => pass += 1,
            ModelOutcome::NoStates => no_states += 1,
            ModelOutcome::CompileFail(_) => compile_fail += 1,
            ModelOutcome::RumocaSimFail(_) => sim_fail += 1,
            ModelOutcome::RenderFail(_) => render_fail += 1,
            ModelOutcome::BackendFail(_) => backend_fail += 1,
            ModelOutcome::TraceDeviation {
                max_deviation, var, ..
            } => {
                deviation += 1;
                deviations.push((model.name.clone(), *max_deviation, var.clone()));
            }
        }

        println!(
            "[{:>3}/{}] {:>13} {}: {outcome}",
            i + 1,
            models.len(),
            tag,
            model.name,
        );
    }

    // Summary
    let total = models.len();
    let tested = pass + deviation;
    println!("\n=== {} stress test summary ===", backend.name());
    println!("  total models:   {total}");
    println!("  pass:           {pass}");
    println!("  no_states:      {no_states}");
    println!("  compile_fail:   {compile_fail}");
    println!("  sim_fail:       {sim_fail}");
    println!("  render_fail:    {render_fail}");
    println!("  backend_fail:   {backend_fail}");
    println!("  deviation:      {deviation}");

    if !deviations.is_empty() {
        deviations.sort_by(|a, b| b.1.partial_cmp(&a.1).unwrap());
        println!("\nWorst deviations:");
        for (name, dev, var) in &deviations {
            println!("  {dev:.4e} on {var} — {name}");
        }
    }

    println!("\n{tested} models tested end-to-end ({pass} pass, {deviation} deviation)");
    println!("{:=<80}\n", "");

    assert!(
        tested > 0,
        "no models were tested end-to-end — {} pipeline may be broken",
        backend.name()
    );
}

// =============================================================================
// Per-backend test entry points
// =============================================================================

#[test]
#[ignore]
fn stress_test_casadi_mx() {
    if !python_has_casadi() {
        panic!("python3 with casadi/numpy not available");
    }
    run_stress_test(BackendKind::CasadiMx);
}

#[test]
#[ignore]
fn stress_test_casadi_sx() {
    if !python_has_casadi() {
        panic!("python3 with casadi/numpy not available");
    }
    run_stress_test(BackendKind::CasadiSx);
}

#[test]
#[ignore]
fn stress_test_embedded_c() {
    if !cc_available() {
        panic!("C compiler not available");
    }
    run_stress_test(BackendKind::EmbeddedC);
}

#[test]
#[ignore]
fn stress_test_fmi2() {
    if !cc_available() {
        panic!("C compiler not available");
    }
    run_stress_test(BackendKind::Fmi2);
}

#[test]
#[ignore]
fn stress_test_fmi3() {
    if !cc_available() {
        panic!("C compiler not available");
    }
    run_stress_test(BackendKind::Fmi3);
}

#[test]
#[ignore]
fn stress_test_sympy() {
    if !python_has_sympy() {
        panic!("python3 with sympy not available");
    }
    run_stress_test(BackendKind::Sympy);
}

#[test]
#[ignore]
fn stress_test_onnx() {
    if !python_has_onnx() {
        panic!("python3 with onnx/onnxruntime/numpy not available");
    }
    run_stress_test(BackendKind::Onnx);
}

#[test]
#[ignore]
fn stress_test_jax() {
    if !python_has_jax() {
        panic!("python3 with jax/diffrax/numpy not available");
    }
    run_stress_test(BackendKind::Jax);
}

#[test]
#[ignore]
fn stress_test_julia_mtk() {
    if !julia_has_mtk() {
        panic!("julia with ModelingToolkit/DifferentialEquations not available");
    }
    run_stress_test(BackendKind::JuliaMtk);
}
