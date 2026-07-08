//! Runtime regression tests for backend templates.
//!
//! For runtime-capable backends (CasADi MX, CasADi SX, FMI2/FMI3, Julia MTK,
//! ONNX, SymPy, JAX) and each test model (Ball, ParamDecay, Oscillator), we:
//!   1. Compile the Modelica source and render the backend template
//!   2. Execute the generated code (Python or C) to produce a CSV trace
//!   3. Run rumoca's built-in simulator to produce a reference trace
//!   4. Compare the two traces and assert bounded relative error
//!
//! Embedded C renders from Solve IR and is covered here at template level.

#[cfg(feature = "template-runtime-tests")]
use std::collections::HashMap;
use std::{fs, process::Command};

use rumoca::{CompilationResult, Compiler, TemplateIr};
use rumoca_phase_codegen::templates;
use rumoca_sim::{SimOptions, SimResult, SimSolverMode, simulate_dae_with_diagnostics};
use tempfile::Builder;

// ============================================================================
// Tolerance — max bounded relative error: |a-b| / max(|a|, |b|, 1.0)
// ============================================================================

/// CasADi uses CVODES (adaptive high-order), so traces should be tight.
#[cfg(feature = "template-runtime-tests")]
const CASADI_TOLERANCE: f64 = 0.01;

/// Embedded C uses RK4 with dt=0.001, FMI2 uses forward Euler with dt=0.001.
#[cfg(feature = "template-runtime-tests")]
const C_TOLERANCE: f64 = 0.05;

// ============================================================================
// Runtime detection
// ============================================================================

#[cfg(feature = "template-runtime-tests")]
fn python_command() -> &'static str {
    for candidate in ["python3", "python"] {
        if Command::new(candidate).arg("--version").output().is_ok() {
            return candidate;
        }
    }
    panic!("expected python3 or python to be available");
}

#[cfg(feature = "template-runtime-tests")]
fn strict_runtime_dependencies() -> bool {
    std::env::var_os("RUMOCA_TEMPLATE_RUNTIME_STRICT").is_some()
}

#[cfg(feature = "template-runtime-tests")]
fn runtime_dependency_available(available: bool, dependency: &str) -> bool {
    if available {
        return true;
    }
    if strict_runtime_dependencies() {
        panic!("{dependency} not available; strict template runtime checks require it");
    }
    eprintln!("SKIP: {dependency} not available");
    false
}

#[cfg(feature = "template-runtime-tests")]
fn julia_command() -> Option<&'static str> {
    ["julia"]
        .into_iter()
        .find(|candidate| Command::new(candidate).arg("--version").output().is_ok())
}

#[cfg(feature = "template-runtime-tests")]
fn julia_project_dir() -> std::path::PathBuf {
    std::path::PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("../../infra/julia")
}

#[cfg(feature = "template-runtime-tests")]
fn julia_has_template_deps() -> bool {
    let Some(julia) = julia_command() else {
        return false;
    };
    Command::new(julia)
        .arg(format!("--project={}", julia_project_dir().display()))
        .args([
            "-e",
            "using ModelingToolkit, OrdinaryDiffEqTsit5, IfElse, SciMLBase, Sundials",
        ])
        .output()
        .map(|o| o.status.success())
        .unwrap_or(false)
}

fn cc_command() -> &'static str {
    for candidate in ["cc", "gcc", "clang"] {
        if Command::new(candidate).arg("--version").output().is_ok() {
            return candidate;
        }
    }
    panic!("expected a C compiler (cc, gcc, or clang) to be available");
}

// ============================================================================
// Compilation helpers
// ============================================================================

fn compile_model(source: &str, model_name: &str) -> rumoca::CompilationResult {
    Compiler::new()
        .model(model_name)
        .compile_str(source, &format!("{model_name}.mo"))
        .expect("compile test model")
}

#[cfg(feature = "template-runtime-tests")]
fn model_dae(source: &str, model_name: &str) -> rumoca_ir_dae::Dae {
    compile_model(source, model_name).dae
}

#[cfg(feature = "template-runtime-tests")]
fn render_template(source: &str, model_name: &str, template: &str) -> String {
    compile_model(source, model_name)
        .render_template_str_with_name(template, model_name)
        .expect("render template")
}

// ============================================================================
// Reference trace from rumoca's built-in simulator
// ============================================================================

fn reference_trace(source: &str, model_name: &str, t_end: f64) -> (CompilationResult, SimResult) {
    let compiled = compile_model(source, model_name);
    let sim = reference_simulation(&compiled.dae, t_end);
    (compiled, sim)
}

#[cfg(feature = "template-runtime-tests")]
fn simulate_compiled(compiled: &CompilationResult, t_end: f64) -> SimResult {
    let opts = reference_sim_options(t_end, Some(30.0));
    simulate_dae_with_diagnostics(&compiled.dae, &opts).expect("rumoca simulation")
}

fn reference_simulation(dae: &rumoca_ir_dae::Dae, t_end: f64) -> SimResult {
    let opts = reference_sim_options(t_end, None);
    simulate_dae_with_diagnostics(dae, &opts).expect("rumoca simulation")
}

fn reference_sim_options(t_end: f64, max_wall_seconds: Option<f64>) -> SimOptions {
    SimOptions {
        t_end,
        max_wall_seconds,
        solver_mode: SimSolverMode::RkLike,
        ..SimOptions::default()
    }
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

// ============================================================================
// CSV trace parsing and comparison
// ============================================================================

#[cfg(feature = "template-runtime-tests")]
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
        .expect("no 'time' column in CSV");
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

#[cfg(feature = "template-runtime-tests")]
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

#[cfg(feature = "template-runtime-tests")]
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

#[cfg(feature = "template-runtime-tests")]
fn state_trace_names(dae: &rumoca_ir_dae::Dae) -> Vec<String> {
    let mut names = Vec::new();
    for (name, var) in &dae.variables.states {
        let name = name.as_str();
        let scalar_count = var
            .dims
            .iter()
            .map(|dim| usize::try_from(*dim).expect("state dimension should be nonnegative"))
            .product::<usize>()
            .max(1);
        if scalar_count == 1 {
            names.push(name.to_string());
            continue;
        }
        for idx in 1..=scalar_count {
            names.push(format!("{name}[{idx}]"));
        }
    }
    names
}

#[cfg(feature = "template-runtime-tests")]
fn assert_traces_match(
    backend_traces: &HashMap<String, Vec<(f64, f64)>>,
    dae: &rumoca_ir_dae::Dae,
    sim: &SimResult,
    tolerance: f64,
    backend_name: &str,
) {
    for name in state_trace_names(dae) {
        let Some(backend_trace) = backend_traces.get(&name) else {
            continue;
        };
        let Some(ref_trace) = extract_sim_trace(sim, &name) else {
            continue;
        };
        let dev = trace_max_deviation(backend_trace, &ref_trace);
        assert!(
            dev <= tolerance,
            "{backend_name}: state '{name}' deviation {dev:.4e} exceeds tolerance {tolerance:.4e}"
        );
    }
}

// ============================================================================
// Python execution helper (returns stdout)
// ============================================================================

#[cfg(feature = "template-runtime-tests")]
fn run_python(rendered: &str, driver: &str) -> String {
    let dir = Builder::new()
        .prefix("rumoca_runtime_test_")
        .tempdir()
        .expect("create temp dir");
    let model_path = dir.path().join("model.py");
    let driver_path = dir.path().join("driver.py");
    fs::write(&model_path, rendered).expect("write model.py");
    fs::write(&driver_path, driver).expect("write driver.py");

    let output = Command::new(python_command())
        .arg(driver_path.to_str().unwrap())
        .output()
        .expect("run Python driver");

    assert!(
        output.status.success(),
        "Python execution failed\nstdout:\n{}\nstderr:\n{}",
        String::from_utf8_lossy(&output.stdout),
        String::from_utf8_lossy(&output.stderr)
    );

    String::from_utf8(output.stdout).expect("stdout is utf8")
}

#[cfg(feature = "template-runtime-tests")]
fn run_julia(rendered: &str, driver: &str) -> String {
    let dir = Builder::new()
        .prefix("rumoca_julia_runtime_test_")
        .tempdir()
        .expect("create temp dir");
    let model_path = dir.path().join("model.jl");
    let driver_path = dir.path().join("driver.jl");
    fs::write(&model_path, rendered).expect("write model.jl");
    fs::write(&driver_path, driver).expect("write driver.jl");

    let output = Command::new(julia_command().expect("julia available"))
        .arg(format!("--project={}", julia_project_dir().display()))
        .arg(driver_path.to_str().unwrap())
        .output()
        .expect("run Julia driver");

    assert!(
        output.status.success(),
        "Julia execution failed\nstdout:\n{}\nstderr:\n{}",
        String::from_utf8_lossy(&output.stdout),
        String::from_utf8_lossy(&output.stderr)
    );

    String::from_utf8(output.stdout).expect("stdout is utf8")
}

// ============================================================================
// C compilation + execution helper (returns stdout)
// ============================================================================

fn compile_and_run_c(sources: &[(&str, &str)], args: &[&str]) -> String {
    let dir = Builder::new()
        .prefix("rumoca_c_runtime_test_")
        .tempdir()
        .expect("create temp dir");
    let binary_path = dir.path().join("test_model");

    let mut src_paths = Vec::new();
    for (filename, content) in sources {
        let path = dir.path().join(filename);
        fs::write(&path, content).unwrap_or_else(|_| panic!("write {filename}"));
        // Only compile .c files, not headers
        if filename.ends_with(".c") {
            src_paths.push(path);
        }
    }

    let mut cmd = Command::new(cc_command());
    cmd.args(["-O0", "-Wall", "-o"])
        .arg(binary_path.to_str().unwrap());
    cmd.arg(format!("-I{}", dir.path().to_str().unwrap()));
    for path in &src_paths {
        cmd.arg(path.to_str().unwrap());
    }
    cmd.arg("-lm");

    let compile = cmd.output().expect("invoke C compiler");
    assert!(
        compile.status.success(),
        "C compilation failed\nstderr:\n{}",
        String::from_utf8_lossy(&compile.stderr)
    );

    let run = Command::new(binary_path.to_str().unwrap())
        .args(args)
        .output()
        .expect("run compiled binary");

    assert!(
        run.status.success(),
        "C execution failed\nstdout:\n{}\nstderr:\n{}",
        String::from_utf8_lossy(&run.stdout),
        String::from_utf8_lossy(&run.stderr)
    );

    String::from_utf8(run.stdout).expect("stdout is utf8")
}

// ============================================================================
// Test models
// ============================================================================

const BALL_SOURCE: &str = r#"
model Ball
  Real x(start=0);
equation
  der(x) = -x;
end Ball;
"#;

const COSIM_DECAY_SOURCE: &str = r#"
model CosimDecay
  Real x(start=1);
equation
  der(x) = -x;
end CosimDecay;
"#;

const EVENT_REINIT_SOURCE: &str = r#"
model EventReinit
  Real x(start=0);
equation
  der(x) = 1;
  when time > 0.5 then
    reinit(x, -1);
  end when;
end EventReinit;
"#;

const PARAM_DECAY_SOURCE: &str = r#"
model ParamDecay
  parameter Real k = 3;
  Real x(start=2);
equation
  der(x) = -k * x;
end ParamDecay;
"#;

const OSCILLATOR_SOURCE: &str = r#"
model Oscillator
  Real x(start=1);
  Real v(start=0);
equation
  der(x) = v;
  der(v) = -x;
end Oscillator;
"#;

const MATRIX_DER_PRODUCT_SOURCE: &str = r#"
model MatrixDerProduct
  Real R[3,3](start={{1,0,0},{0,1,0},{0,0,1}}, fixed=true);
  Real skew[3,3] = {{0,-1,0},{1,0,0},{0,0,0}};
equation
  der(R) = R * skew;
end MatrixDerProduct;
"#;

#[test]
fn native_simulates_matrix_derivative_product() {
    let (_compiled, sim) = reference_trace(MATRIX_DER_PRODUCT_SOURCE, "MatrixDerProduct", 0.5);
    let r12 = extract_sim_trace(&sim, "R[1,2]").expect("R[1,2] trace");
    let r21 = extract_sim_trace(&sim, "R[2,1]").expect("R[2,1] trace");
    let (_, r12_last) = *r12.last().expect("R[1,2] has samples");
    let (_, r21_last) = *r21.last().expect("R[2,1] has samples");

    assert!(
        (r12_last + 0.5_f64.sin()).abs() < 0.02,
        "expected R[1,2] to follow -sin(t), got {r12_last}"
    );
    assert!(
        (r21_last - 0.5_f64.sin()).abs() < 0.02,
        "expected R[2,1] to follow sin(t), got {r21_last}"
    );
}

// ============================================================================
// CasADi driver — outputs CSV: time,state1,state2,...
// ============================================================================

#[cfg(feature = "template-runtime-tests")]
const CASADI_CSV_DRIVER: &str = r#"
import importlib.util, sys, os
import numpy as np

spec = importlib.util.spec_from_file_location("model", os.path.join(os.path.dirname(__file__), "model.py"))
mod = importlib.util.module_from_spec(spec)
spec.loader.exec_module(mod)

model = mod.create_model()

assert 'x0' in model, "missing x0"
assert 'build_integrator' in model, "missing build_integrator"
assert 'state_names' in model, "missing state_names"

tgrid = np.arange(0, 1.001, 0.001)
integrator = model['build_integrator'](tgrid)
p_full = np.concatenate([model['p0'], np.array([])])
result = integrator(x0=model['x0'], p=p_full)
xf = np.array(result['xf'])

assert not np.any(np.isnan(xf)), "NaN detected in simulation output"

# Print CSV header
print("time," + ",".join(model['state_names']))

# Print data rows
for i, t in enumerate(tgrid):
    row = [f"{t:.10g}"] + [f"{xf[j, i]:.10g}" for j in range(xf.shape[0])]
    print(",".join(row))
"#;

// ============================================================================
// CasADi MX runtime tests
// ============================================================================

#[cfg(feature = "template-runtime-tests")]
fn casadi_trace_test(source: &str, model_name: &str, template: &str) {
    let rendered = render_template(source, model_name, template);
    let csv = run_python(&rendered, CASADI_CSV_DRIVER);
    let backend_traces = parse_csv_traces(&csv);
    let (dae, sim) = reference_trace(source, model_name, 1.0);
    assert_traces_match(&backend_traces, &dae.dae, &sim, CASADI_TOLERANCE, "CasADi");
}

#[test]
#[cfg(feature = "template-runtime-tests")]
fn casadi_mx_ball() {
    casadi_trace_test(
        BALL_SOURCE,
        "Ball",
        templates::builtin_template_source("casadi-mx", "casadi_mx.py.jinja").unwrap(),
    );
}

#[test]
#[cfg(feature = "template-runtime-tests")]
fn casadi_mx_param_decay() {
    casadi_trace_test(
        PARAM_DECAY_SOURCE,
        "ParamDecay",
        templates::builtin_template_source("casadi-mx", "casadi_mx.py.jinja").unwrap(),
    );
}

#[test]
#[cfg(feature = "template-runtime-tests")]
fn casadi_mx_oscillator() {
    casadi_trace_test(
        OSCILLATOR_SOURCE,
        "Oscillator",
        templates::builtin_template_source("casadi-mx", "casadi_mx.py.jinja").unwrap(),
    );
}

// ============================================================================
// CasADi SX runtime tests
// ============================================================================

#[test]
#[cfg(feature = "template-runtime-tests")]
fn casadi_sx_ball() {
    casadi_trace_test(
        BALL_SOURCE,
        "Ball",
        templates::builtin_template_source("casadi-sx", "casadi_sx.py.jinja").unwrap(),
    );
}

#[test]
#[cfg(feature = "template-runtime-tests")]
fn casadi_sx_param_decay() {
    casadi_trace_test(
        PARAM_DECAY_SOURCE,
        "ParamDecay",
        templates::builtin_template_source("casadi-sx", "casadi_sx.py.jinja").unwrap(),
    );
}

#[test]
#[cfg(feature = "template-runtime-tests")]
fn casadi_sx_oscillator() {
    casadi_trace_test(
        OSCILLATOR_SOURCE,
        "Oscillator",
        templates::builtin_template_source("casadi-sx", "casadi_sx.py.jinja").unwrap(),
    );
}

// ============================================================================
// Embedded C template compatibility tests
// ============================================================================

fn embedded_c_renders_solve_ir(source: &str, model_name: &str) {
    let compiled = compile_model(source, model_name);

    for template in [
        templates::builtin_template_source("embedded-c", "model.h.jinja").unwrap(),
        templates::builtin_template_source("embedded-c", "model.c.jinja").unwrap(),
    ] {
        let rendered = compiled
            .render_template_str_with_name_and_ir(template, model_name, TemplateIr::Solve)
            .expect("Embedded C should render from Solve IR");
        assert!(
            rendered.contains("_derivative_rhs") || rendered.contains("_DERIVATIVE_LEN"),
            "expected embedded-C Solve-IR runtime surface in rendered template, got:\n{rendered}"
        );
    }
}

#[test]
fn embedded_c_ball() {
    embedded_c_renders_solve_ir(BALL_SOURCE, "Ball");
}

#[test]
fn embedded_c_param_decay() {
    embedded_c_renders_solve_ir(PARAM_DECAY_SOURCE, "ParamDecay");
}

#[test]
fn embedded_c_oscillator() {
    embedded_c_renders_solve_ir(OSCILLATOR_SOURCE, "Oscillator");
}

#[test]
fn embedded_c_event_reinit_renders_solve_ir() {
    embedded_c_renders_solve_ir(EVENT_REINIT_SOURCE, "EventReinit");
}

// ============================================================================
// Julia ModelingToolkit runtime smoke
// ============================================================================

#[cfg(feature = "template-runtime-tests")]
const JULIA_MTK_DRIVER: &str = r#"
include(joinpath(@__DIR__, "model.jl"))

sys = create_model()
defaults = get_default_values()

@assert haskey(defaults.x0, "x")
@assert isapprox(defaults.x0["x"], 2.0)
@assert haskey(defaults.p0, "k")
@assert isapprox(defaults.p0["k"], 3.0)

sol = simulate(tspan=(0.0, 0.1), p=Dict("k" => 3.0), saveat=[0.0, 0.1])
@assert length(sol.t) >= 2

println("JULIA_MTK_OK")
"#;

#[test]
#[cfg(feature = "template-runtime-tests")]
fn julia_mtk_param_decay_executes() {
    if !runtime_dependency_available(julia_has_template_deps(), "julia template runtime deps") {
        return;
    }

    let rendered = render_template(
        PARAM_DECAY_SOURCE,
        "ParamDecay",
        templates::builtin_template_source("julia-mtk", "julia_mtk.jl.jinja").unwrap(),
    );
    let stdout = run_julia(&rendered, JULIA_MTK_DRIVER);
    assert!(
        stdout.contains("JULIA_MTK_OK"),
        "expected Julia MTK smoke output, got:\n{stdout}"
    );
}

fn render_fmi_solve_template(
    compiled: &CompilationResult,
    target: &str,
    template: &str,
    model_name: &str,
) -> String {
    compiled
        .render_template_str_with_name_and_ir(
            templates::builtin_template_source(target, template).unwrap(),
            model_name,
            TemplateIr::Solve,
        )
        .unwrap_or_else(|err| panic!("render {target}:{template} from Solve IR: {err}"))
}

// ============================================================================
// FMI 2.0 runtime tests
// ============================================================================

#[cfg(feature = "template-runtime-tests")]
fn fmi2_trace_test(source: &str, model_name: &str) {
    let compiled = compile_model(source, model_name);
    let model_c = render_fmi_solve_template(&compiled, "fmi2", "model.c.jinja", model_name);
    let driver_c = render_fmi_solve_template(&compiled, "fmi2", "test_driver.c.jinja", model_name);

    let csv = compile_and_run_c(
        &[("model.c", &model_c), ("driver.c", &driver_c)],
        &["--t-end", "1.0", "--dt", "0.001"],
    );
    let backend_traces = parse_csv_traces(&csv);

    let sim = reference_simulation(&compiled.dae, 1.0);
    assert_traces_match(&backend_traces, &compiled.dae, &sim, C_TOLERANCE, "FMI2");
}

#[cfg(feature = "template-runtime-tests")]
fn fmi2_trace_csv(source: &str, model_name: &str) -> String {
    let compiled = compile_model(source, model_name);
    let model_c = render_fmi_solve_template(&compiled, "fmi2", "model.c.jinja", model_name);
    let driver_c = render_fmi_solve_template(&compiled, "fmi2", "test_driver.c.jinja", model_name);
    compile_and_run_c(
        &[("model.c", &model_c), ("driver.c", &driver_c)],
        &["--t-end", "1.0", "--dt", "0.001"],
    )
}

#[cfg(feature = "template-runtime-tests")]
fn assert_event_reinit_trace(csv: &str, backend_name: &str) {
    let traces = parse_csv_traces(csv);
    let x_trace = traces
        .get("x")
        .unwrap_or_else(|| panic!("{backend_name}: expected x trace:\n{csv}"));
    let final_x = x_trace.last().map(|(_, value)| *value).unwrap_or(f64::NAN);
    let min_x = x_trace
        .iter()
        .map(|(_, value)| *value)
        .fold(f64::INFINITY, f64::min);
    assert!(
        (final_x + 0.5).abs() <= C_TOLERANCE,
        "{backend_name}: expected final x near -0.5 after reinit, got {final_x}\n{csv}"
    );
    assert!(
        min_x < -0.95,
        "{backend_name}: expected reinit jump below -0.95, got min x={min_x}\n{csv}"
    );
}

#[test]
#[cfg(feature = "template-runtime-tests")]
fn fmi2_ball() {
    fmi2_trace_test(BALL_SOURCE, "Ball");
}

#[test]
#[cfg(feature = "template-runtime-tests")]
fn fmi2_param_decay() {
    fmi2_trace_test(PARAM_DECAY_SOURCE, "ParamDecay");
}

#[test]
#[cfg(feature = "template-runtime-tests")]
fn fmi2_oscillator() {
    fmi2_trace_test(OSCILLATOR_SOURCE, "Oscillator");
}

#[test]
#[cfg(feature = "template-runtime-tests")]
fn fmi2_event_reinit_runtime() {
    let csv = fmi2_trace_csv(EVENT_REINIT_SOURCE, "EventReinit");
    assert_event_reinit_trace(&csv, "FMI2");
}

// ============================================================================
// FMI 3.0 runtime tests
// ============================================================================

#[cfg(feature = "template-runtime-tests")]
fn fmi3_trace_from_compiled(compiled: &CompilationResult, model_name: &str, t_end: f64) -> String {
    let model_c = render_fmi_solve_template(compiled, "fmi3", "model.c.jinja", model_name);
    let driver_c = render_fmi_solve_template(compiled, "fmi3", "test_driver.c.jinja", model_name);

    std::fs::write("/tmp/arraydecay_model.c", &model_c).ok();
    std::fs::write("/tmp/arraydecay_driver.c", &driver_c).ok();
    std::fs::write(
        "/tmp/arraydecay_dae.json",
        serde_json::to_string_pretty(&compiled.dae).unwrap_or_default(),
    )
    .ok();

    compile_and_run_c(
        &[("model.c", &model_c), ("driver.c", &driver_c)],
        &["--t-end", &t_end.to_string(), "--dt", "0.001"],
    )
}

#[cfg(feature = "template-runtime-tests")]
fn fmi3_trace_test(source: &str, model_name: &str) {
    let compiled = compile_model(source, model_name);
    let csv = fmi3_trace_from_compiled(&compiled, model_name, 1.0);
    let backend_traces = parse_csv_traces(&csv);

    let sim = simulate_compiled(&compiled, 1.0);
    assert_traces_match(&backend_traces, &compiled.dae, &sim, C_TOLERANCE, "FMI3");
}

#[test]
#[cfg(feature = "template-runtime-tests")]
fn fmi3_ball() {
    fmi3_trace_test(BALL_SOURCE, "Ball");
}

#[test]
#[cfg(feature = "template-runtime-tests")]
fn fmi3_param_decay() {
    fmi3_trace_test(PARAM_DECAY_SOURCE, "ParamDecay");
}

#[test]
#[cfg(feature = "template-runtime-tests")]
fn fmi3_oscillator() {
    fmi3_trace_test(OSCILLATOR_SOURCE, "Oscillator");
}

#[test]
#[cfg(feature = "template-runtime-tests")]
fn fmi3_event_reinit_runtime() {
    let compiled = compile_model(EVENT_REINIT_SOURCE, "EventReinit");
    let csv = fmi3_trace_from_compiled(&compiled, "EventReinit", 1.0);
    assert_event_reinit_trace(&csv, "FMI3");
}

#[test]
#[cfg(feature = "template-runtime-tests")]
fn fmi3_matrix_derivative_product_runtime() {
    fmi3_trace_test(MATRIX_DER_PRODUCT_SOURCE, "MatrixDerProduct");
}

// ============================================================================
// FMI 3.0 — Tunable parameters (Phase 1)
//
// Verify that tunable Real parameters get variability="tunable" in the XML
// and structural Integer parameters get variability="fixed".
// ============================================================================

const TUNABLE_PARAM_SOURCE: &str = r#"
model TunableParam
  parameter Integer n = 2;
  parameter Real k = 3;
  Real x(start=1);
equation
  der(x) = -k * x;
end TunableParam;
"#;

#[test]
fn fmi3_tunable_param_xml() {
    let compiled = compile_model(TUNABLE_PARAM_SOURCE, "TunableParam");
    let xml = render_fmi_solve_template(
        &compiled,
        "fmi3",
        "modelDescription.xml.jinja",
        "TunableParam",
    );

    // k should be tunable (Real, non-structural)
    assert!(
        xml.contains(r#"variability="tunable""#),
        "expected tunable variability for Real parameter k:\n{xml}"
    );
    // n should be fixed (Integer, structural)
    assert!(
        xml.contains(r#"variability="fixed""#),
        "expected fixed variability for Integer parameter n:\n{xml}"
    );
}

#[test]
#[cfg(feature = "template-runtime-tests")]
fn fmi3_tunable_param_runtime() {
    fmi3_trace_test(TUNABLE_PARAM_SOURCE, "TunableParam");
}

// ============================================================================
// FMI 3.0 — Directional derivatives (Phase 3)
//
// Verify that fmi3GetDirectionalDerivative returns correct Jacobian entries
// from solve-IR AD rows for a simple model: der(x) = -k*x -> d(xdot)/dx = -k.
// ============================================================================

#[test]
#[cfg(feature = "template-runtime-tests")]
fn fmi3_directional_derivative() {
    let compiled = compile_model(PARAM_DECAY_SOURCE, "ParamDecay");

    let model_c = render_fmi_solve_template(&compiled, "fmi3", "model.c.jinja", "ParamDecay");

    // Verify XML advertises directional derivatives
    let xml = render_fmi_solve_template(
        &compiled,
        "fmi3",
        "modelDescription.xml.jinja",
        "ParamDecay",
    );
    assert!(
        xml.contains(r#"providesDirectionalDerivatives="true""#),
        "expected providesDirectionalDerivatives in XML"
    );

    // Build a driver that calls fmi3GetDirectionalDerivative
    let driver_c = r#"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>

typedef unsigned int fmi3ValueReference;
typedef double       fmi3Float64;
typedef int          fmi3Boolean;
typedef const char*  fmi3String;
typedef void*        fmi3Instance;
typedef void*        fmi3InstanceEnvironment;
typedef enum { fmi3OK, fmi3Warning, fmi3Discard, fmi3Error, fmi3Fatal } fmi3Status;
typedef void (*fmi3LogMessageCallback)(fmi3InstanceEnvironment, fmi3Status, fmi3String, fmi3String);

extern fmi3Instance fmi3InstantiateModelExchange(fmi3String, fmi3String, fmi3String, fmi3Boolean, fmi3Boolean, fmi3InstanceEnvironment, fmi3LogMessageCallback);
extern void fmi3FreeInstance(fmi3Instance);
extern fmi3Status fmi3EnterInitializationMode(fmi3Instance, fmi3Boolean, fmi3Float64, fmi3Float64, fmi3Boolean, fmi3Float64);
extern fmi3Status fmi3ExitInitializationMode(fmi3Instance);
extern fmi3Status fmi3EnterContinuousTimeMode(fmi3Instance);
extern fmi3Status fmi3UpdateDiscreteStates(fmi3Instance, fmi3Boolean*, fmi3Boolean*, fmi3Boolean*, fmi3Boolean*, fmi3Boolean*, fmi3Float64*);
extern fmi3Status fmi3GetDirectionalDerivative(fmi3Instance, const fmi3ValueReference[], size_t, const fmi3ValueReference[], size_t, const fmi3Float64[], size_t, fmi3Float64[], size_t);

static void dummy_logger(fmi3InstanceEnvironment e, fmi3Status s, fmi3String c, fmi3String m) {
    (void)e; (void)s; (void)c; (void)m;
}

int main(void) {
    fmi3Instance inst = fmi3InstantiateModelExchange(
        "test", "ParamDecay-rumoca", "", 0, 0, NULL, dummy_logger);
    if (!inst) return 1;

    fmi3EnterInitializationMode(inst, 0, 0.0, 0.0, 1, 1.0);
    fmi3ExitInitializationMode(inst);
    {
        fmi3Boolean a, b, c, d, e; fmi3Float64 f;
        fmi3UpdateDiscreteStates(inst, &a, &b, &c, &d, &e, &f);
    }
    fmi3EnterContinuousTimeMode(inst);

    /* ParamDecay: der(x) = -k*x, k=3, x(0)=2
     * VR layout: x=0, xdot=1, k=2
     * Jacobian: d(xdot)/d(x) = -k = -3, d(xdot)/d(k) = -x = -2
     */
    fmi3ValueReference unknown = 1;  /* xdot */
    fmi3ValueReference known_x = 0;  /* x */
    fmi3ValueReference known_k = 2;  /* k */
    fmi3Float64 seed = 1.0;
    fmi3Float64 sensitivity_x = 0.0;
    fmi3Float64 sensitivity_k = 0.0;

    fmi3Status s_x = fmi3GetDirectionalDerivative(
        inst, &unknown, 1, &known_x, 1, &seed, 1, &sensitivity_x, 1);
    fmi3Status s_k = fmi3GetDirectionalDerivative(
        inst, &unknown, 1, &known_k, 1, &seed, 1, &sensitivity_k, 1);

    printf("status_x=%d\n", s_x);
    printf("status_k=%d\n", s_k);
    printf("sensitivity_x=%.10g\n", sensitivity_x);
    printf("sensitivity_k=%.10g\n", sensitivity_k);
    printf("error_x=%.10g\n", fabs(sensitivity_x - (-3.0)));
    printf("error_k=%.10g\n", fabs(sensitivity_k - (-2.0)));

    fmi3FreeInstance(inst);
    return (s_x == fmi3OK && s_k == fmi3OK
        && fabs(sensitivity_x - (-3.0)) < 0.01
        && fabs(sensitivity_k - (-2.0)) < 0.01) ? 0 : 1;
}
"#;

    let csv = compile_and_run_c(&[("model.c", &model_c), ("driver.c", driver_c)], &[]);
    // If we get here without panic, the test passed (driver returns 0 on success)
    assert!(
        csv.contains("sensitivity_x=") && csv.contains("sensitivity_k="),
        "expected sensitivity output:\n{csv}"
    );
}

// ============================================================================
// FMI 3.0 — Native array variables (Phase 4)
//
// Verify that array variables produce <Dimension> elements in the XML and
// that the simulation still works correctly with per-variable VR layout.
// ============================================================================

const ARRAY_DECAY_SOURCE: &str = r#"
model ArrayDecay
  Real x[3](start={1,2,3});
equation
  der(x) = -x;
end ArrayDecay;
"#;

#[test]
fn fmi3_native_array_xml() {
    let compiled = compile_model(ARRAY_DECAY_SOURCE, "ArrayDecay");
    let xml = render_fmi_solve_template(
        &compiled,
        "fmi3",
        "modelDescription.xml.jinja",
        "ArrayDecay",
    );

    // Verify array variable has <Dimension> element
    assert!(
        xml.contains("<Dimension start=\"3\"/>"),
        "expected <Dimension start=\"3\"/> for array variable x[3]:\n{xml}"
    );
    // Verify native array: single Float64 for x, not x[1], x[2], x[3]
    assert!(
        xml.contains("name=\"x\""),
        "expected native array name=\"x\" (not x[1]):\n{xml}"
    );
    assert!(
        !xml.contains("name=\"x[1]\""),
        "should not have scalar-expanded x[1] with native arrays:\n{xml}"
    );
}

#[test]
#[cfg(feature = "template-runtime-tests")]
fn fmi3_native_array_runtime() {
    // Test FMI3 C compile + run with array variables (per-variable VR layout).
    // Uses a standalone driver since the reference simulator doesn't handle
    // array state variables directly.
    //
    // Routes through `Compiler::render_template_str_with_name_and_ir` so the
    // runtime C template exercises the same Solve-IR target path as CI.
    let compiled = compile_model(ARRAY_DECAY_SOURCE, "ArrayDecay");
    let model_c = render_fmi_solve_template(&compiled, "fmi3", "model.c.jinja", "ArrayDecay");
    let driver_c =
        render_fmi_solve_template(&compiled, "fmi3", "test_driver.c.jinja", "ArrayDecay");

    let csv = compile_and_run_c(
        &[("model.c", &model_c), ("driver.c", &driver_c)],
        &["--t-end", "1.0", "--dt", "0.001"],
    );
    let traces = parse_csv_traces(&csv);

    // der(x) = -x with x(0) = {1,2,3} → x(t) = x0*exp(-t)
    // At t=1: x[i] ≈ x0[i] * exp(-1) ≈ x0[i] * 0.3679
    for (col, x0) in [("x[1]", 1.0), ("x[2]", 2.0), ("x[3]", 3.0)] {
        let trace = traces
            .get(col)
            .unwrap_or_else(|| panic!("missing column {col}"));
        let (t_last, v_last) = *trace.last().expect("trace should not be empty");
        assert!(t_last >= 0.99, "expected t_end >= 0.99, got {t_last}");
        let expected = x0 * (-1.0f64).exp();
        let scale = expected.abs().max(1.0);
        let err = (v_last - expected).abs() / scale;
        assert!(
            err <= C_TOLERANCE,
            "Array state {col}: final={v_last:.6}, expected={expected:.6}, err={err:.4e}"
        );
    }
}

// ============================================================================
// FMI 3.0 — Adjoint derivatives
//
// Verify that fmi3GetAdjointDerivative returns correct transposed Jacobian
// entries. For der(x) = -k*x, the adjoint with seed on xdot should give
// sensitivity on x = -k (same magnitude, transposed).
// ============================================================================

#[test]
#[cfg(feature = "template-runtime-tests")]
fn fmi3_adjoint_derivative() {
    let compiled = compile_model(PARAM_DECAY_SOURCE, "ParamDecay");

    let model_c = render_fmi_solve_template(&compiled, "fmi3", "model.c.jinja", "ParamDecay");

    // Verify XML advertises adjoint derivatives
    let xml = render_fmi_solve_template(
        &compiled,
        "fmi3",
        "modelDescription.xml.jinja",
        "ParamDecay",
    );
    assert!(
        xml.contains(r#"providesAdjointDerivatives="true""#),
        "expected providesAdjointDerivatives in XML"
    );

    let driver_c = r#"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>

typedef unsigned int fmi3ValueReference;
typedef double       fmi3Float64;
typedef int          fmi3Boolean;
typedef const char*  fmi3String;
typedef void*        fmi3Instance;
typedef void*        fmi3InstanceEnvironment;
typedef enum { fmi3OK, fmi3Warning, fmi3Discard, fmi3Error, fmi3Fatal } fmi3Status;
typedef void (*fmi3LogMessageCallback)(fmi3InstanceEnvironment, fmi3Status, fmi3String, fmi3String);

extern fmi3Instance fmi3InstantiateModelExchange(fmi3String, fmi3String, fmi3String, fmi3Boolean, fmi3Boolean, fmi3InstanceEnvironment, fmi3LogMessageCallback);
extern void fmi3FreeInstance(fmi3Instance);
extern fmi3Status fmi3EnterInitializationMode(fmi3Instance, fmi3Boolean, fmi3Float64, fmi3Float64, fmi3Boolean, fmi3Float64);
extern fmi3Status fmi3ExitInitializationMode(fmi3Instance);
extern fmi3Status fmi3EnterContinuousTimeMode(fmi3Instance);
extern fmi3Status fmi3UpdateDiscreteStates(fmi3Instance, fmi3Boolean*, fmi3Boolean*, fmi3Boolean*, fmi3Boolean*, fmi3Boolean*, fmi3Float64*);
extern fmi3Status fmi3GetAdjointDerivative(fmi3Instance, const fmi3ValueReference[], size_t, const fmi3ValueReference[], size_t, const fmi3Float64[], size_t, fmi3Float64[], size_t);

static void dummy_logger(fmi3InstanceEnvironment e, fmi3Status s, fmi3String c, fmi3String m) {
    (void)e; (void)s; (void)c; (void)m;
}

int main(void) {
    fmi3Instance inst = fmi3InstantiateModelExchange(
        "test", "ParamDecay-rumoca", "", 0, 0, NULL, dummy_logger);
    if (!inst) return 1;

    fmi3EnterInitializationMode(inst, 0, 0.0, 0.0, 1, 1.0);
    fmi3ExitInitializationMode(inst);
    {
        fmi3Boolean a, b, c, d, e; fmi3Float64 f;
        fmi3UpdateDiscreteStates(inst, &a, &b, &c, &d, &e, &f);
    }
    fmi3EnterContinuousTimeMode(inst);

    /* ParamDecay: der(x) = -k*x, k=3, x(0)=2
     * VR layout: x=0, xdot=1
     * Adjoint: seed on xdot, sensitivity on x = d(xdot)/d(x) = -k = -3 */
    fmi3ValueReference unknown = 1;  /* xdot */
    fmi3ValueReference known = 0;    /* x */
    fmi3Float64 seed = 1.0;
    fmi3Float64 sensitivity = 0.0;

    fmi3Status s = fmi3GetAdjointDerivative(
        inst, &unknown, 1, &known, 1, &seed, 1, &sensitivity, 1);

    printf("status=%d\n", s);
    printf("sensitivity=%.10g\n", sensitivity);
    printf("expected=-3\n");
    printf("error=%.10g\n", fabs(sensitivity - (-3.0)));

    fmi3FreeInstance(inst);
    return (s == fmi3OK && fabs(sensitivity - (-3.0)) < 0.01) ? 0 : 1;
}
"#;

    let csv = compile_and_run_c(&[("model.c", &model_c), ("driver.c", driver_c)], &[]);
    assert!(
        csv.contains("sensitivity="),
        "expected sensitivity output:\n{csv}"
    );
}

// ============================================================================
// FMI 3.0 — FMU state serialization
//
// Verify get/set/serialize/deserialize FMU state round-trips correctly.
// Save state at t=0.5, continue to t=1.0, restore to t=0.5, continue
// again to t=1.0 — both runs should produce the same final value.
// ============================================================================

/// C driver source for FMI 3.0 state serialization round-trip test.
#[cfg(feature = "template-runtime-tests")]
const FMI3_STATE_SERIALIZATION_DRIVER: &str = r#"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>

typedef unsigned int fmi3ValueReference;
typedef double       fmi3Float64;
typedef float        fmi3Float32;
typedef int          fmi3Int32;
typedef int          fmi3Boolean;
typedef const char*  fmi3String;
typedef void*        fmi3Instance;
typedef void*        fmi3InstanceEnvironment;
typedef void*        fmi3FMUState;
typedef unsigned char fmi3Byte;
typedef enum { fmi3OK, fmi3Warning, fmi3Discard, fmi3Error, fmi3Fatal } fmi3Status;
typedef void (*fmi3LogMessageCallback)(fmi3InstanceEnvironment, fmi3Status, fmi3String, fmi3String);

extern fmi3Instance fmi3InstantiateModelExchange(fmi3String, fmi3String, fmi3String, fmi3Boolean, fmi3Boolean, fmi3InstanceEnvironment, fmi3LogMessageCallback);
extern void fmi3FreeInstance(fmi3Instance);
extern fmi3Status fmi3EnterInitializationMode(fmi3Instance, fmi3Boolean, fmi3Float64, fmi3Float64, fmi3Boolean, fmi3Float64);
extern fmi3Status fmi3ExitInitializationMode(fmi3Instance);
extern fmi3Status fmi3EnterContinuousTimeMode(fmi3Instance);
extern fmi3Status fmi3EnterEventMode(fmi3Instance);
extern fmi3Status fmi3UpdateDiscreteStates(fmi3Instance, fmi3Boolean*, fmi3Boolean*, fmi3Boolean*, fmi3Boolean*, fmi3Boolean*, fmi3Float64*);
extern fmi3Status fmi3SetTime(fmi3Instance, fmi3Float64);
extern fmi3Status fmi3SetContinuousStates(fmi3Instance, const fmi3Float64[], size_t);
extern fmi3Status fmi3GetContinuousStates(fmi3Instance, fmi3Float64[], size_t);
extern fmi3Status fmi3GetContinuousStateDerivatives(fmi3Instance, fmi3Float64[], size_t);
extern fmi3Status fmi3CompletedIntegratorStep(fmi3Instance, fmi3Boolean, fmi3Boolean*, fmi3Boolean*);
extern fmi3Status fmi3GetFMUState(fmi3Instance, fmi3FMUState*);
extern fmi3Status fmi3SetFMUState(fmi3Instance, fmi3FMUState);
extern fmi3Status fmi3FreeFMUState(fmi3Instance, fmi3FMUState*);
extern fmi3Status fmi3SerializedFMUStateSize(fmi3Instance, fmi3FMUState, size_t*);
extern fmi3Status fmi3SerializeFMUState(fmi3Instance, fmi3FMUState, fmi3Byte[], size_t);
extern fmi3Status fmi3DeserializeFMUState(fmi3Instance, const fmi3Byte[], size_t, fmi3FMUState*);
extern fmi3Status fmi3GetFloat32(fmi3Instance, const fmi3ValueReference[], size_t, fmi3Float32[], size_t);
extern fmi3Status fmi3SetFloat32(fmi3Instance, const fmi3ValueReference[], size_t, const fmi3Float32[], size_t);
extern fmi3Status fmi3GetInt32(fmi3Instance, const fmi3ValueReference[], size_t, fmi3Int32[], size_t);
extern fmi3Status fmi3GetBoolean(fmi3Instance, const fmi3ValueReference[], size_t, fmi3Boolean[], size_t);

static void dummy_logger(fmi3InstanceEnvironment e, fmi3Status s, fmi3String c, fmi3String m) {
    (void)e; (void)s; (void)c; (void)m;
}

/* Euler integrate from current state for n steps of size dt */
static void euler_steps(fmi3Instance inst, double* t, double* x, int n, double dt) {
    double xdot;
    for (int i = 0; i < n; i++) {
        fmi3SetTime(inst, *t);
        fmi3SetContinuousStates(inst, x, 1);
        fmi3GetContinuousStateDerivatives(inst, &xdot, 1);
        *x += dt * xdot;
        *t += dt;
    }
}

int main(void) {
    fmi3Instance inst = fmi3InstantiateModelExchange(
        "test", "ParamDecay-rumoca", "", 0, 0, NULL, dummy_logger);
    if (!inst) return 1;

    fmi3EnterInitializationMode(inst, 0, 0.0, 0.0, 1, 1.0);
    fmi3ExitInitializationMode(inst);
    {
        fmi3Boolean a, b, c, d, e; fmi3Float64 f;
        fmi3UpdateDiscreteStates(inst, &a, &b, &c, &d, &e, &f);
    }
    fmi3EnterContinuousTimeMode(inst);

    /* Integrate to t=0.5 */
    double t = 0.0, x = 2.0, dt = 0.001;
    euler_steps(inst, &t, &x, 500, dt);

    /* Sync FMU internal state to match local variables before saving */
    fmi3SetTime(inst, t);
    fmi3SetContinuousStates(inst, &x, 1);

    /* Save state via get/serialize/deserialize round-trip */
    fmi3FMUState state = NULL;
    fmi3GetFMUState(inst, &state);

    size_t sz;
    fmi3SerializedFMUStateSize(inst, state, &sz);
    fmi3Byte* buf = (fmi3Byte*)malloc(sz);
    fmi3SerializeFMUState(inst, state, buf, sz);
    fmi3FreeFMUState(inst, &state);

    /* Continue to t=1.0 (first run) */
    euler_steps(inst, &t, &x, 500, dt);
    double x_first = x;

    /* Restore from serialized state (back to t=0.5) */
    fmi3FMUState restored = NULL;
    fmi3DeserializeFMUState(inst, buf, sz, &restored);
    fmi3SetFMUState(inst, restored);
    fmi3FreeFMUState(inst, &restored);
    free(buf);

    /* Get state back and re-integrate to t=1.0 */
    fmi3GetContinuousStates(inst, &x, 1);
    t = 0.5;
    euler_steps(inst, &t, &x, 500, dt);
    double x_second = x;

    /* Also test typed access (Float32, Int32, Boolean) */
    fmi3ValueReference vr_x = 0;
    fmi3Float32 f32_val;
    fmi3GetFloat32(inst, &vr_x, 1, &f32_val, 1);
    fmi3Int32 i32_val;
    fmi3GetInt32(inst, &vr_x, 1, &i32_val, 1);
    fmi3Boolean bool_val;
    fmi3GetBoolean(inst, &vr_x, 1, &bool_val, 1);

    /* Float32 set round-trip */
    fmi3Float32 f32_set = 1.5f;
    fmi3SetFloat32(inst, &vr_x, 1, &f32_set, 1);
    fmi3Float64 f64_check;
    fmi3GetContinuousStates(inst, &f64_check, 1);

    double err = fabs(x_first - x_second);
    double scale = fabs(x_first) > 1.0 ? fabs(x_first) : 1.0;

    printf("x_first=%.10g\n", x_first);
    printf("x_second=%.10g\n", x_second);
    printf("error=%.10g\n", err / scale);
    printf("f32_val=%.6g\n", (double)f32_val);
    printf("i32_val=%d\n", i32_val);
    printf("bool_val=%d\n", bool_val);
    printf("f32_roundtrip_err=%.6g\n", fabs(f64_check - 1.5));

    fmi3FreeInstance(inst);
    int ok = (err / scale < 1e-10) && (fabs(f64_check - 1.5) < 0.01);
    return ok ? 0 : 1;
}
"#;

#[test]
#[cfg(feature = "template-runtime-tests")]
fn fmi3_fmu_state_serialization() {
    let compiled = compile_model(PARAM_DECAY_SOURCE, "ParamDecay");
    let model_c = render_fmi_solve_template(&compiled, "fmi3", "model.c.jinja", "ParamDecay");

    // Verify XML advertises state capabilities
    let xml = render_fmi_solve_template(
        &compiled,
        "fmi3",
        "modelDescription.xml.jinja",
        "ParamDecay",
    );
    assert!(
        xml.contains(r#"canGetAndSetFMUState="true""#),
        "expected canGetAndSetFMUState in XML"
    );
    assert!(
        xml.contains(r#"canSerializeFMUState="true""#),
        "expected canSerializeFMUState in XML"
    );

    let csv = compile_and_run_c(
        &[
            ("model.c", &model_c),
            ("driver.c", FMI3_STATE_SERIALIZATION_DRIVER),
        ],
        &[],
    );
    assert!(
        csv.contains("x_first=") && csv.contains("x_second="),
        "expected state output:\n{csv}"
    );
}

// ============================================================================
// FMI 3.0 — Co-Simulation with DoStep
//
// Verify that fmi3InstantiateCoSimulation + fmi3DoStep produces correct
// results via the built-in forward Euler integrator.
// ============================================================================

#[test]
#[cfg(feature = "template-runtime-tests")]
fn fmi3_cosimulation_dostep() {
    let compiled = compile_model(COSIM_DECAY_SOURCE, "CosimDecay");
    let model_c = render_fmi_solve_template(&compiled, "fmi3", "model.c.jinja", "CosimDecay");

    let driver_c = r#"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>

typedef unsigned int fmi3ValueReference;
typedef double       fmi3Float64;
typedef int          fmi3Boolean;
typedef const char*  fmi3String;
typedef void*        fmi3Instance;
typedef void*        fmi3InstanceEnvironment;
typedef enum { fmi3OK, fmi3Warning, fmi3Discard, fmi3Error, fmi3Fatal } fmi3Status;
typedef void (*fmi3LogMessageCallback)(fmi3InstanceEnvironment, fmi3Status, fmi3String, fmi3String);

extern fmi3Instance fmi3InstantiateCoSimulation(
    fmi3String, fmi3String, fmi3String, fmi3Boolean, fmi3Boolean,
    fmi3Boolean, fmi3Boolean, const fmi3ValueReference[], size_t,
    fmi3InstanceEnvironment, fmi3LogMessageCallback, void*);
extern void fmi3FreeInstance(fmi3Instance);
extern fmi3Status fmi3EnterInitializationMode(fmi3Instance, fmi3Boolean, fmi3Float64, fmi3Float64, fmi3Boolean, fmi3Float64);
extern fmi3Status fmi3ExitInitializationMode(fmi3Instance);
extern fmi3Status fmi3DoStep(fmi3Instance, fmi3Float64, fmi3Float64, fmi3Boolean, fmi3Boolean*, fmi3Boolean*, fmi3Boolean*, fmi3Float64*);
extern fmi3Status fmi3GetFloat64(fmi3Instance, const fmi3ValueReference[], size_t, fmi3Float64[], size_t);
extern fmi3Status fmi3Terminate(fmi3Instance);

static void dummy_logger(fmi3InstanceEnvironment e, fmi3Status s, fmi3String c, fmi3String m) {
    (void)e; (void)s; (void)c; (void)m;
}

int main(void) {
    fmi3Instance inst = fmi3InstantiateCoSimulation(
        "test", "CosimDecay-rumoca", "", 0, 0, 0, 0, NULL, 0, NULL, dummy_logger, NULL);
    if (!inst) { fprintf(stderr, "instantiate failed\n"); return 1; }

    fmi3EnterInitializationMode(inst, 0, 0.0, 0.0, 1, 1.0);
    fmi3ExitInitializationMode(inst);

    /* Step to t=1.0 in 0.01 increments */
    double t = 0.0, dt = 0.01;
    for (int i = 0; i < 100; i++) {
        fmi3Boolean eventNeeded, terminate, earlyReturn;
        fmi3Float64 lastTime;
        fmi3Status s = fmi3DoStep(inst, t, dt, 1, &eventNeeded, &terminate, &earlyReturn, &lastTime);
        if (s != fmi3OK) { fprintf(stderr, "DoStep failed at t=%g\n", t); return 1; }
        t += dt;
    }

    /* Read state x (VR=0) */
    fmi3ValueReference vr = 0;
    fmi3Float64 x;
    fmi3GetFloat64(inst, &vr, 1, &x, 1);

    /* Built-in co-simulation uses explicit Euler: x_n = (1 - dt)^n. */
    double expected = pow(1.0 - dt, 100.0);
    printf("x_final=%.10g\n", x);
    printf("expected=%.10g\n", expected);

    fmi3Terminate(inst);
    fmi3FreeInstance(inst);

    return (fabs(x - expected) < 0.01 && fabs(x - 1.0) > 0.1) ? 0 : 1;
}
"#;

    let csv = compile_and_run_c(&[("model.c", &model_c), ("driver.c", driver_c)], &[]);
    assert!(csv.contains("x_final="), "expected x_final output:\n{csv}");
}

// ============================================================================
// FMI 3.0 — Structural parameters in XML
//
// Verify that non-tunable parameters get causality="structuralParameter"
// and tunable parameters get causality="parameter" variability="tunable".
// ============================================================================

#[test]
fn fmi3_structural_parameter_xml() {
    let compiled = compile_model(TUNABLE_PARAM_SOURCE, "TunableParam");
    let xml = render_fmi_solve_template(
        &compiled,
        "fmi3",
        "modelDescription.xml.jinja",
        "TunableParam",
    );

    // k (Real) should be tunable parameter
    assert!(
        xml.contains(r#"causality="parameter""#) && xml.contains(r#"variability="tunable""#),
        "expected tunable parameter for k:\n{xml}"
    );
    // n (Integer, non-tunable) should be structural parameter
    assert!(
        xml.contains(r#"causality="structuralParameter""#),
        "expected structuralParameter for n:\n{xml}"
    );
}

// ============================================================================
// FMI 3.0 — modelDescription/buildDescription schema split
//
// Verify that modelDescription follows the FMI 3 schema and build
// configuration is emitted in sources/buildDescription.xml.
// ============================================================================

#[test]
fn fmi3_xml_build_description_schema_split() {
    let compiled = compile_model(BALL_SOURCE, "Ball");
    let xml = render_fmi_solve_template(&compiled, "fmi3", "modelDescription.xml.jinja", "Ball");

    assert!(
        !xml.contains("<BuildConfiguration"),
        "BuildConfiguration belongs in sources/buildDescription.xml:\n{xml}"
    );
    assert!(
        !xml.contains("<Terminals"),
        "Terminals belong in terminalsAndIcons/terminalsAndIcons.xml:\n{xml}"
    );

    let build_xml =
        render_fmi_solve_template(&compiled, "fmi3", "buildDescription.xml.jinja", "Ball");

    assert!(
        build_xml.contains(r#"<fmiBuildDescription fmiVersion="3.0">"#),
        "expected FMI 3 build description root:\n{build_xml}"
    );
    assert!(
        build_xml.contains(r#"<SourceFile name="Ball.c"/>"#),
        "expected source file in buildDescription:\n{build_xml}"
    );
}

// ============================================================================
// SymPy runtime tests
//
// SymPy generates a symbolic model, not a time-domain simulation. We verify
// that the explicit symbolic solve produces correct derivative expressions
// by evaluating them at the initial condition and comparing against the
// rumoca reference derivatives.
// ============================================================================

#[cfg(feature = "template-runtime-tests")]
const SYMPY_EVAL_DRIVER: &str = r#"
import importlib.util, sys, os
import json

spec = importlib.util.spec_from_file_location("model", os.path.join(os.path.dirname(__file__), "model.py"))
mod = importlib.util.module_from_spec(spec)
spec.loader.exec_module(mod)

import sympy as sp

model = mod.Model()
summary = model.summary()

assert summary['continuous_residual_count'] > 0, "expected at least one residual"

solution = model.solve_explicit()
assert model.explicit_solution is not None, "solve_explicit() failed"

for target in model.explicit_targets:
    assert target in solution, f"missing solution for {target}"

# Evaluate derivatives at initial conditions
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

# Build CSV: time=0 row with derivative values
state_names = list(model.x_start.keys())
deriv_vals = {}
for target, expr in solution.items():
    val = float(expr.subs(subs))
    target_str = str(target)
    # Map derivative target back to state name
    for sn in state_names:
        if sn in target_str:
            deriv_vals[sn] = val
            break

# Output as JSON for easy parsing
print(json.dumps({"state_names": state_names, "derivs_at_t0": deriv_vals}))
"#;

#[cfg(feature = "template-runtime-tests")]
fn sympy_trace_test(source: &str, model_name: &str) {
    let dae = model_dae(source, model_name);
    let rendered = rumoca_phase_codegen::render_template_with_name(
        &dae,
        templates::builtin_template_source("sympy", "sympy.py.jinja").unwrap(),
        model_name,
    )
    .expect("render template");

    let stdout = run_python(&rendered, SYMPY_EVAL_DRIVER);
    let result: serde_json::Value = serde_json::from_str(stdout.trim()).expect("parse JSON output");

    // Get reference derivatives at t=0 from rumoca simulator
    let sim = reference_simulation(&dae, 0.001);

    // Compare: for each state, check that SymPy's derivative at t=0
    // matches the finite-difference derivative from rumoca's first step
    let derivs = result["derivs_at_t0"].as_object().expect("derivs_at_t0");
    for (state_name, sympy_deriv_val) in derivs {
        let sympy_d = sympy_deriv_val.as_f64().expect("float deriv");
        // Get rumoca's derivative via finite difference on first two time points
        if let Some(trace) = extract_sim_trace(&sim, state_name)
            && trace.len() >= 2
        {
            let (t0, x0) = trace[0];
            let (t1, x1) = trace[1];
            let rumoca_d = (x1 - x0) / (t1 - t0);
            let scale = sympy_d.abs().max(rumoca_d.abs()).max(1.0);
            let err = (sympy_d - rumoca_d).abs() / scale;
            assert!(
                err <= 0.1,
                "SymPy: state '{state_name}' derivative at t=0: sympy={sympy_d:.6}, rumoca≈{rumoca_d:.6}, err={err:.4e}"
            );
        }
    }
}

#[test]
#[cfg(feature = "template-runtime-tests")]
fn sympy_ball() {
    sympy_trace_test(BALL_SOURCE, "Ball");
}

#[test]
#[cfg(feature = "template-runtime-tests")]
fn sympy_param_decay() {
    sympy_trace_test(PARAM_DECAY_SOURCE, "ParamDecay");
}

#[test]
#[cfg(feature = "template-runtime-tests")]
fn sympy_oscillator() {
    sympy_trace_test(OSCILLATOR_SOURCE, "Oscillator");
}

// ============================================================================
// ONNX runtime tests
//
// ONNX uses operator-overloading (OnnxVar) to build computational graphs
// from render_expr output, then runs forward Euler via ONNX Runtime.
// ============================================================================

#[cfg(feature = "template-runtime-tests")]
const ONNX_CSV_DRIVER: &str = r#"
import importlib.util, sys, os

spec = importlib.util.spec_from_file_location("model", os.path.join(os.path.dirname(__file__), "model.py"))
mod = importlib.util.module_from_spec(spec)
spec.loader.exec_module(mod)

print(mod.simulate())
"#;

#[cfg(feature = "template-runtime-tests")]
fn python_has_onnx() -> bool {
    Command::new(python_command())
        .args(["-c", "import onnx; import onnxruntime; import numpy"])
        .output()
        .map(|o| o.status.success())
        .unwrap_or(false)
}

#[cfg(feature = "template-runtime-tests")]
fn onnx_trace_test(source: &str, model_name: &str) {
    if !runtime_dependency_available(python_has_onnx(), "onnx/onnxruntime") {
        return;
    }
    let rendered = render_template(
        source,
        model_name,
        templates::builtin_template_source("onnx", "onnx.py.jinja").unwrap(),
    );
    let csv = run_python(&rendered, ONNX_CSV_DRIVER);
    let backend_traces = parse_csv_traces(&csv);
    let (dae, sim) = reference_trace(source, model_name, 1.0);
    assert_traces_match(&backend_traces, &dae.dae, &sim, C_TOLERANCE, "ONNX");
}

#[test]
#[cfg(feature = "template-runtime-tests")]
fn onnx_ball() {
    onnx_trace_test(BALL_SOURCE, "Ball");
}

#[test]
#[cfg(feature = "template-runtime-tests")]
fn onnx_param_decay() {
    onnx_trace_test(PARAM_DECAY_SOURCE, "ParamDecay");
}

#[test]
#[cfg(feature = "template-runtime-tests")]
fn onnx_oscillator() {
    onnx_trace_test(OSCILLATOR_SOURCE, "Oscillator");
}

// ============================================================================
// JAX/Diffrax runtime tests
//
// JAX uses Diffrax's Tsit5 adaptive solver via jax.jit-compiled ODE function.
// ============================================================================

/// JAX uses Tsit5 (adaptive 5th-order), should be tight.
#[cfg(feature = "template-runtime-tests")]
const JAX_TOLERANCE: f64 = 0.02;

#[cfg(feature = "template-runtime-tests")]
const JAX_CSV_DRIVER: &str = r#"
import importlib.util, sys, os

spec = importlib.util.spec_from_file_location("model", os.path.join(os.path.dirname(__file__), "model.py"))
mod = importlib.util.module_from_spec(spec)
spec.loader.exec_module(mod)

print(mod.simulate_csv())
"#;

#[cfg(feature = "template-runtime-tests")]
fn python_has_jax() -> bool {
    Command::new(python_command())
        .args(["-c", "import jax; import diffrax; import numpy"])
        .output()
        .map(|o| o.status.success())
        .unwrap_or(false)
}

#[cfg(feature = "template-runtime-tests")]
fn jax_trace_test(source: &str, model_name: &str) {
    if !runtime_dependency_available(python_has_jax(), "jax/diffrax") {
        return;
    }
    let rendered = render_template(
        source,
        model_name,
        templates::builtin_template_source("jax", "jax.py.jinja").unwrap(),
    );
    let csv = run_python(&rendered, JAX_CSV_DRIVER);
    let backend_traces = parse_csv_traces(&csv);
    let (dae, sim) = reference_trace(source, model_name, 1.0);
    assert_traces_match(&backend_traces, &dae.dae, &sim, JAX_TOLERANCE, "JAX");
}

#[test]
#[cfg(feature = "template-runtime-tests")]
fn jax_ball() {
    jax_trace_test(BALL_SOURCE, "Ball");
}

#[test]
#[cfg(feature = "template-runtime-tests")]
fn jax_param_decay() {
    jax_trace_test(PARAM_DECAY_SOURCE, "ParamDecay");
}

#[test]
#[cfg(feature = "template-runtime-tests")]
fn jax_oscillator() {
    jax_trace_test(OSCILLATOR_SOURCE, "Oscillator");
}

// ============================================================================
// Regression test for issue #115: component output variables in FMU C code
// ============================================================================

/// Regression test for issue #115: FMI2/FMI3 generated C code must
/// compile when the DAE has non-empty output variables (dae.w).
///
/// Uses two coupled blocks with an algebraic loop so that the output
/// variables cannot be trivially eliminated and remain in dae.w after
/// the prepare phase.
#[test]
fn fmi2_fmi3_component_output_compiles() {
    // Two coupled output blocks: g1.y and g2.y form an algebraic loop
    // that prevents the eliminator from inlining both outputs.
    const SOURCE: &str = r#"
block Gain
  parameter Real k = 1;
  input Real u;
  output Real y;
equation
  y = k * u;
end Gain;

model CoupledGains
  Real x(start = 1);
  Gain g1(k = 2);
  Gain g2(k = 0.5);
equation
  g1.u = x + g2.y;
  g2.u = g1.y;
  der(x) = -g1.y;
end CoupledGains;
"#;

    let compiled = compile_model(SOURCE, "CoupledGains");
    let dae = &compiled.dae;

    // At least one algebraic or output variable must survive elimination
    // (the coupled gain blocks create a 2-unknown algebraic loop).
    let n_alg = dae.variables.outputs.len() + dae.variables.algebraics.len();
    assert!(
        n_alg > 0,
        "test model should have algebraic/output variables that survive elimination, \
         got outputs={:?} algebraics={:?}",
        dae.variables.outputs.keys().collect::<Vec<_>>(),
        dae.variables.algebraics.keys().collect::<Vec<_>>(),
    );

    let model = "CoupledGains";

    // FMI2: render and compile
    let fmi2_c = render_fmi_solve_template(&compiled, "fmi2", "model.c.jinja", model);
    let fmi2_driver = render_fmi_solve_template(&compiled, "fmi2", "test_driver.c.jinja", model);
    compile_and_run_c(
        &[("model.c", &fmi2_c), ("driver.c", &fmi2_driver)],
        &["--t-end", "1.0", "--dt", "0.001"],
    );

    // FMI3: render and compile
    let fmi3_c = render_fmi_solve_template(&compiled, "fmi3", "model.c.jinja", model);
    let fmi3_driver = render_fmi_solve_template(&compiled, "fmi3", "test_driver.c.jinja", model);
    compile_and_run_c(
        &[("model.c", &fmi3_c), ("driver.c", &fmi3_driver)],
        &["--t-end", "1.0", "--dt", "0.001"],
    );
}
