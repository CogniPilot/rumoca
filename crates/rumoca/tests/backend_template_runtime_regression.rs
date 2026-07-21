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
    std::path::PathBuf::from(env!("CARGO_MANIFEST_DIR"))
        .join("../../target/template-runtimes/strict")
        .is_file()
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

    if sources
        .iter()
        .any(|(_, content)| content.contains("\"fmi3Functions.h\""))
    {
        for (template, filename) in [
            ("fmi3PlatformTypes.h.jinja", "fmi3PlatformTypes.h"),
            ("fmi3FunctionTypes.h.jinja", "fmi3FunctionTypes.h"),
            ("fmi3Functions.h.jinja", "fmi3Functions.h"),
        ] {
            let content = templates::builtin_template_source("fmi3", template)
                .unwrap_or_else(|| panic!("load built-in FMI3 header {template}"));
            fs::write(dir.path().join(filename), content)
                .unwrap_or_else(|_| panic!("write {filename}"));
        }
    }

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
        "C execution failed with status {}\nstdout:\n{}\nstderr:\n{}",
        run.status,
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

const CONTACT_LANDING_SOURCE: &str = r#"
model ContactLanding
  Real height(start=0.12, fixed=true);
  Real verticalSpeed(start=0.0, fixed=true);
equation
  der(height) = verticalSpeed;
  der(verticalSpeed) = if height < 0.1 then
      1000.0 * (0.1 - height) - 50.0 * verticalSpeed - 9.80665
    else
      -9.80665;
end ContactLanding;
"#;

const CONTACT_AT_INITIAL_ROOT_SOURCE: &str = r#"
model ContactAtInitialRoot
  Real height(start=0.1, fixed=true);
  Real verticalSpeed(start=-0.25, fixed=true);
equation
  der(height) = verticalSpeed;
  der(verticalSpeed) = if height < 0.1 then
      1000.0 * (0.1 - height) - 50.0 * verticalSpeed - 9.80665
    else
      -9.80665;
end ContactAtInitialRoot;
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

const ROOT_EVENT_REINIT_SOURCE: &str = r#"
model RootEventReinit
  Real x(start=0);
equation
  der(x) = 1;
  when x > 0.5 then
    reinit(x, -1);
  end when;
end RootEventReinit;
"#;

const COINCIDENT_EDGE_SOURCE: &str = r#"
model CoincidentEdge
  Real x(start=0, fixed=true);
  Real marker;
  discrete output Real launched(start=0, fixed=true);
equation
  der(x) = 1;
  marker = if x > 0.095 then x else -x;
  when time >= 0.1 then
    launched = 1;
  end when;
end CoincidentEdge;
"#;

const CASCADED_EVENT_SOURCE: &str = r#"
model CascadedEvent
  Real x(start=0, fixed=true);
  Real projected;
  discrete output Real first(start=0, fixed=true);
  discrete output Real second(start=0, fixed=true);
equation
  der(x) = 0;
  projected = first;
  when time >= 0.1 then
    first = 1;
  end when;
  when projected >= 1 then
    second = pre(second) + 1;
  end when;
end CascadedEvent;
"#;

const PERIODIC_SAMPLE_SOURCE: &str = r#"
model PeriodicSample
  Real x(start=0, fixed=true);
  discrete output Real ticks(start=0, fixed=true);
equation
  der(x) = 0;
  when sample(0, 0.05) then
    ticks = pre(ticks) + 1;
  end when;
end PeriodicSample;
"#;

const TINY_PERIODIC_SAMPLE_SOURCE: &str = r#"
model TinyPeriodicSample
  Real x(start=0, fixed=true);
  discrete output Real ticks(start=0, fixed=true);
equation
  der(x) = 0;
  when sample(0, 1e-12) then
    ticks = pre(ticks) + 1;
  end when;
end TinyPeriodicSample;
"#;

const ALGEBRAIC_ROOT_SOURCE: &str = r#"
model AlgebraicRoot
  Real x(start=0, fixed=true);
  Real doubled;
  discrete output Real crossed(start=0, fixed=true);
equation
  der(x) = 0;
  doubled = 2 * x;
  when doubled >= 1 then
    crossed = 1;
  end when;
end AlgebraicRoot;
"#;

const DYNAMIC_TIME_EVENT_SOURCE: &str = r#"
model DynamicTimeEvent
  input Real trigger(start=0.1);
  Real x(start=0, fixed=true);
  discrete output Real fired(start=0, fixed=true);
equation
  der(x) = 0;
  when time >= trigger then
    fired = 1;
  end when;
end DynamicTimeEvent;
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
    if template == "modelDescription.xml.jinja" {
        return compiled
            .render_fmi_model_description_template_str_with_name(
                templates::builtin_template_source(target, template).unwrap(),
                model_name,
            )
            .unwrap_or_else(|err| {
                panic!("render {target}:{template} from FMI metadata DAE: {err}")
            });
    }
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
    let compiled = compile_model(ROOT_EVENT_REINIT_SOURCE, "RootEventReinit");
    let csv = fmi3_trace_from_compiled(&compiled, "RootEventReinit", 1.0);
    assert_event_reinit_trace(&csv, "FMI3");
}

#[test]
#[cfg(feature = "template-runtime-tests")]
fn fmi3_generated_importer_localizes_state_events() {
    let compiled = compile_model(COINCIDENT_EDGE_SOURCE, "CoincidentEdge");
    let model_c = render_fmi_solve_template(&compiled, "fmi3", "model.c.jinja", "CoincidentEdge");
    let driver_c =
        render_fmi_solve_template(&compiled, "fmi3", "test_driver.c.jinja", "CoincidentEdge");
    let csv = compile_and_run_c(
        &[("model.c", &model_c), ("driver.c", &driver_c)],
        &["--t-end", "0.11", "--dt", "0.01"],
    );
    let localized = csv
        .lines()
        .skip(1)
        .filter_map(|line| line.split(',').next()?.parse::<f64>().ok());
    assert!(
        localized
            .into_iter()
            .any(|time| (time - 0.095).abs() < 1.0e-12),
        "FMI3 importer must localize the state event inside the integration step:\n{csv}"
    );
}

#[test]
#[cfg(feature = "template-runtime-tests")]
fn fmi3_reinit_reports_continuous_state_change() {
    let compiled = compile_model(ROOT_EVENT_REINIT_SOURCE, "RootEventReinit");
    let model_c = render_fmi_solve_template(&compiled, "fmi3", "model.c.jinja", "RootEventReinit");
    let driver = r#"
int main(void) {
    fmi3Instance instance = fmi3InstantiateModelExchange(
        "root-reinit", MODEL_INSTANTIATION_TOKEN, NULL, 0, 0, NULL, NULL);
    if (!instance) return 10;
    if (fmi3EnterInitializationMode(instance, 1, 1.0e-7, 0.0, 1, 1.0) != fmi3OK) return 11;
    if (fmi3ExitInitializationMode(instance) != fmi3OK) return 12;

    fmi3Boolean update = 0, terminate = 0, nominalsChanged = 0, valuesChanged = 0;
    fmi3Boolean nextTimeDefined = 0;
    fmi3Float64 nextTime = 0.0;
    if (fmi3UpdateDiscreteStates(instance, &update, &terminate, &nominalsChanged,
            &valuesChanged, &nextTimeDefined, &nextTime) != fmi3OK) return 13;
    if (fmi3EnterContinuousTimeMode(instance) != fmi3OK) return 14;

    const fmi3Float64 eventTime = 0.5001;
    const fmi3Float64 eventState = 0.5001;
    if (fmi3SetTime(instance, eventTime) != fmi3OK) return 15;
    if (fmi3SetContinuousStates(instance, &eventState, 1) != fmi3OK) return 16;
    if (fmi3EnterEventMode(instance) != fmi3OK) return 17;
    if (fmi3UpdateDiscreteStates(instance, &update, &terminate, &nominalsChanged,
            &valuesChanged, &nextTimeDefined, &nextTime) != fmi3OK) return 18;
    if (!valuesChanged || update || terminate || nominalsChanged) return 19;

    fmi3Float64 state = 0.0;
    if (fmi3GetContinuousStates(instance, &state, 1) != fmi3OK) return 20;
    fmi3FreeInstance(instance);
    return state == -1.0 ? 0 : 21;
}
"#;
    let source = format!("{model_c}\n{driver}");
    compile_and_run_c(&[("root_event_reinit_flag.c", &source)], &[]);
}

#[test]
#[cfg(feature = "template-runtime-tests")]
fn fmi3_event_refreshes_all_relations_before_discrete_updates() {
    let compiled = compile_model(COINCIDENT_EDGE_SOURCE, "CoincidentEdge");
    let model_c = render_fmi_solve_template(&compiled, "fmi3", "model.c.jinja", "CoincidentEdge");
    let driver = r#"
int main(void) {
    fmi3Instance instance = fmi3InstantiateModelExchange(
        "coincident-edge", MODEL_INSTANTIATION_TOKEN, NULL, 0, 0, NULL, NULL);
    if (!instance) return 10;
    if (fmi3EnterInitializationMode(instance, 1, 1.0e-7, 0.0, 1, 0.2) != fmi3OK) return 11;
    if (fmi3ExitInitializationMode(instance) != fmi3OK) return 12;

    fmi3Boolean update = 0, terminate = 0, nominalsChanged = 0, valuesChanged = 0;
    fmi3Boolean nextTimeDefined = 0;
    fmi3Float64 nextTime = 0.0;
    if (fmi3UpdateDiscreteStates(instance, &update, &terminate, &nominalsChanged,
            &valuesChanged, &nextTimeDefined, &nextTime) != fmi3OK) return 13;
    if (fmi3EnterContinuousTimeMode(instance) != fmi3OK) return 14;

    fmi3Float64 rootsBefore[N_EVENT_INDICATORS > 0 ? N_EVENT_INDICATORS : 1];
    fmi3Float64 rootsAfter[N_EVENT_INDICATORS > 0 ? N_EVENT_INDICATORS : 1];
    if (fmi3GetEventIndicators(instance, rootsBefore, N_EVENT_INDICATORS) != fmi3OK) return 15;

    const fmi3Float64 time = 0.1;
    const fmi3Float64 x = 0.1;
    if (fmi3SetTime(instance, time) != fmi3OK) return 16;
    if (fmi3SetContinuousStates(instance, &x, 1) != fmi3OK) return 17;
    if (fmi3GetEventIndicators(instance, rootsAfter, N_EVENT_INDICATORS) != fmi3OK) return 18;
    int rootEvent = 0;
    for (int i = 0; i < N_EVENT_INDICATORS; i++) {
        if ((rootsBefore[i] > 0.0) != (rootsAfter[i] > 0.0)) rootEvent = 1;
    }
    if (!rootEvent) return 19;

    fmi3Boolean enterEvent = 0;
    if (fmi3CompletedIntegratorStep(instance, 1, &enterEvent, &terminate) != fmi3OK) return 20;
    if (enterEvent || terminate) return 21;
    if (fmi3EnterEventMode(instance) != fmi3OK) return 22;
    if (fmi3UpdateDiscreteStates(instance, &update, &terminate, &nominalsChanged,
            &valuesChanged, &nextTimeDefined, &nextTime) != fmi3OK) return 23;

    const fmi3ValueReference launchedVr = VR_Z;
    fmi3Float64 launched = 0.0;
    if (fmi3GetFloat64(instance, &launchedVr, 1, &launched, 1) != fmi3OK) return 24;
    fmi3FreeInstance(instance);
    return launched == 1.0 ? 0 : 25;
}
"#;
    let source = format!("{model_c}\n{driver}");
    compile_and_run_c(&[("coincident_edge.c", &source)], &[]);
}

#[test]
#[cfg(feature = "template-runtime-tests")]
fn fmi3_event_iteration_reprojects_cascaded_relations() {
    let compiled = compile_model(CASCADED_EVENT_SOURCE, "CascadedEvent");
    let model_c = render_fmi_solve_template(&compiled, "fmi3", "model.c.jinja", "CascadedEvent");
    let driver = r#"
int main(void) {
    fmi3Instance instance = fmi3InstantiateModelExchange(
        "cascaded-event", MODEL_INSTANTIATION_TOKEN, NULL, 0, 0, NULL, NULL);
    if (!instance) return 10;
    if (fmi3EnterInitializationMode(instance, 1, 1.0e-7, 0.0, 1, 0.2) != fmi3OK) return 11;
    if (fmi3ExitInitializationMode(instance) != fmi3OK) return 12;

    fmi3Boolean update = 0, terminate = 0, nominalsChanged = 0, valuesChanged = 0;
    fmi3Boolean nextTimeDefined = 0;
    fmi3Float64 nextTime = 0.0;
    if (fmi3UpdateDiscreteStates(instance, &update, &terminate, &nominalsChanged,
            &valuesChanged, &nextTimeDefined, &nextTime) != fmi3OK) return 13;
    if (!nextTimeDefined || fabs(nextTime - 0.1) > 1.0e-12) return 14;
    if (fmi3EnterContinuousTimeMode(instance) != fmi3OK) return 15;

    if (fmi3SetTime(instance, nextTime) != fmi3OK) return 16;
    if (fmi3EnterEventMode(instance) != fmi3OK) return 17;
    if (fmi3UpdateDiscreteStates(instance, &update, &terminate, &nominalsChanged,
            &valuesChanged, &nextTimeDefined, &nextTime) != fmi3OK) return 18;

    const fmi3ValueReference firstVr = VR_Z;
    const fmi3ValueReference secondVr = VR_Z + 1;
    fmi3Float64 first = 0.0;
    fmi3Float64 second = 0.0;
    if (fmi3GetFloat64(instance, &firstVr, 1, &first, 1) != fmi3OK) return 19;
    if (fmi3GetFloat64(instance, &secondVr, 1, &second, 1) != fmi3OK) return 20;
    fmi3FreeInstance(instance);
    return first == 1.0 && second == 1.0 ? 0 : 21;
}
"#;
    let source = format!("{model_c}\n{driver}");
    compile_and_run_c(&[("cascaded_event.c", &source)], &[]);
}

#[test]
#[cfg(feature = "template-runtime-tests")]
fn fmi3_periodic_sample_updates_at_event_instant() {
    let compiled = compile_model(PERIODIC_SAMPLE_SOURCE, "PeriodicSample");
    let model_c = render_fmi_solve_template(&compiled, "fmi3", "model.c.jinja", "PeriodicSample");
    let driver = r#"
static int update_event(fmi3Instance instance, double expectedNextTime) {
    fmi3Boolean update = 0, terminate = 0, nominalsChanged = 0, valuesChanged = 0;
    fmi3Boolean nextTimeDefined = 0;
    fmi3Float64 nextTime = 0.0;
    fmi3Status status = fmi3UpdateDiscreteStates(instance, &update, &terminate, &nominalsChanged,
        &valuesChanged, &nextTimeDefined, &nextTime);
    int valid = status == fmi3OK && !update && !terminate && nextTimeDefined
        && fabs(nextTime - expectedNextTime) < 1.0e-12;
    if (!valid) printf("update status=%d update=%d terminate=%d defined=%d next=%.17g expected=%.17g\n",
        status, update, terminate, nextTimeDefined, nextTime, expectedNextTime);
    return valid;
}

static int ticks_equal(fmi3Instance instance, double expected) {
    const fmi3ValueReference ticksVr = VR_Z;
    fmi3Float64 ticks = 0.0;
    fmi3Status status = fmi3GetFloat64(instance, &ticksVr, 1, &ticks, 1);
    int valid = status == fmi3OK && ticks == expected;
    if (!valid) printf("ticks status=%d value=%.17g expected=%.17g\n", status, ticks, expected);
    return valid;
}

int main(void) {
    fmi3Instance instance = fmi3InstantiateModelExchange(
        "periodic-sample", MODEL_INSTANTIATION_TOKEN, NULL, 0, 0, NULL, NULL);
    if (!instance) return 10;
    if (fmi3EnterInitializationMode(instance, 1, 1.0e-7, 0.0, 1, 0.2) != fmi3OK) return 11;
    if (fmi3ExitInitializationMode(instance) != fmi3OK) return 12;
    if (!update_event(instance, 0.05) || !ticks_equal(instance, 1.0)) return 13;
    if (fmi3EnterContinuousTimeMode(instance) != fmi3OK) return 14;

    fmi3Float64 previousRoot = 0.0;
    if (fmi3GetEventIndicators(instance, &previousRoot, 1) != fmi3OK || previousRoot <= 0.0) {
        return 15;
    }

    for (int event = 1; event <= 2; event++) {
        const fmi3Float64 time = 0.05 * event;
        if (fmi3SetTime(instance, time) != fmi3OK) return 16;
        fmi3Float64 currentRoot = 0.0;
        if (fmi3GetEventIndicators(instance, &currentRoot, 1) != fmi3OK || currentRoot <= 0.0) {
            return 17;
        }
        fmi3Boolean enterEvent = 0, terminate = 0;
        if (fmi3CompletedIntegratorStep(instance, 1, &enterEvent, &terminate) != fmi3OK) return 18;
        if (enterEvent || terminate) return 19;
        if (fmi3EnterEventMode(instance) != fmi3OK) return 20;
        if (!update_event(instance, time + 0.05) || !ticks_equal(instance, event + 1.0)) return 21;
        if (fmi3EnterContinuousTimeMode(instance) != fmi3OK) return 22;
    }

    fmi3FreeInstance(instance);
    return 0;
}
"#;
    let source = format!("{model_c}\n{driver}");
    compile_and_run_c(&[("periodic_sample.c", &source)], &[]);
}

#[test]
#[cfg(feature = "template-runtime-tests")]
fn fmi3_periodic_sample_preserves_subnanosecond_events() {
    let compiled = compile_model(TINY_PERIODIC_SAMPLE_SOURCE, "TinyPeriodicSample");
    let model_c =
        render_fmi_solve_template(&compiled, "fmi3", "model.c.jinja", "TinyPeriodicSample");
    let driver = r#"
int main(void) {
    fmi3Instance instance = fmi3InstantiateModelExchange(
        "tiny-periodic", MODEL_INSTANTIATION_TOKEN, NULL, 0, 0, NULL, NULL);
    if (!instance) return 10;
    if (fmi3EnterInitializationMode(instance, 1, 1.0e-7, 0.0, 1, 1.0e-9) != fmi3OK) return 11;
    if (fmi3ExitInitializationMode(instance) != fmi3OK) return 12;

    fmi3Boolean update = 0, terminate = 0, nominalsChanged = 0, valuesChanged = 0;
    fmi3Boolean nextTimeDefined = 0;
    fmi3Float64 nextTime = 0.0;
    if (fmi3UpdateDiscreteStates(instance, &update, &terminate, &nominalsChanged,
            &valuesChanged, &nextTimeDefined, &nextTime) != fmi3OK) return 13;
    if (!nextTimeDefined || fabs(nextTime - 1.0e-12) > 1.0e-24) return 14;
    if (fmi3EnterContinuousTimeMode(instance) != fmi3OK) return 15;

    if (fmi3SetTime(instance, nextTime) != fmi3OK) return 16;
    if (fmi3EnterEventMode(instance) != fmi3OK) return 17;
    if (fmi3UpdateDiscreteStates(instance, &update, &terminate, &nominalsChanged,
            &valuesChanged, &nextTimeDefined, &nextTime) != fmi3OK) return 18;
    if (!nextTimeDefined || fabs(nextTime - 2.0e-12) > 1.0e-24) return 19;

    const fmi3ValueReference ticksVr = VR_Z;
    fmi3Float64 ticks = 0.0;
    if (fmi3GetFloat64(instance, &ticksVr, 1, &ticks, 1) != fmi3OK) return 20;
    fmi3FreeInstance(instance);
    return ticks == 2.0 ? 0 : 21;
}
"#;
    let source = format!("{model_c}\n{driver}");
    compile_and_run_c(&[("tiny_periodic_sample.c", &source)], &[]);
}

#[test]
#[cfg(feature = "template-runtime-tests")]
fn fmi3_periodic_schedule_recovery_advances_an_integer_cycle() {
    let compiled = compile_model(PARAM_DECAY_SOURCE, "ParamDecay");
    let model_c = render_fmi_solve_template(&compiled, "fmi3", "model.c.jinja", "ParamDecay");
    let driver = r#"
int main(void) {
    double nextTime = 0.0;
    if (!periodic_next_time(1.0, -1.0e16, 1.0, &nextTime)) return 10;
    return nextTime == 2.0 ? 0 : 11;
}
"#;
    let source = format!("{model_c}\n{driver}");
    compile_and_run_c(&[("periodic_schedule_recovery.c", &source)], &[]);
}

#[test]
#[cfg(feature = "template-runtime-tests")]
fn fmi3_event_indicators_project_algebraics_before_root_detection() {
    let compiled = compile_model(ALGEBRAIC_ROOT_SOURCE, "AlgebraicRoot");
    let model_c = render_fmi_solve_template(&compiled, "fmi3", "model.c.jinja", "AlgebraicRoot");
    assert!(
        model_c.contains("if (root_index == 0) return 1;"),
        "a non-strict relation must classify equality in the nonpositive root domain"
    );
    let driver = r#"
int main(void) {
    fmi3Instance instance = fmi3InstantiateModelExchange(
        "algebraic-root", MODEL_INSTANTIATION_TOKEN, NULL, 0, 0, NULL, NULL);
    if (!instance) return 10;
    if (fmi3EnterInitializationMode(instance, 1, 1.0e-7, 0.0, 1, 1.0) != fmi3OK) return 11;
    if (fmi3ExitInitializationMode(instance) != fmi3OK) return 12;
    fmi3Boolean update = 0, terminate = 0, nominalsChanged = 0, valuesChanged = 0;
    fmi3Boolean nextTimeDefined = 0;
    fmi3Float64 nextTime = 0.0;
    if (fmi3UpdateDiscreteStates(instance, &update, &terminate, &nominalsChanged,
            &valuesChanged, &nextTimeDefined, &nextTime) != fmi3OK) return 13;
    if (fmi3EnterContinuousTimeMode(instance) != fmi3OK) return 14;

    fmi3Float64 previousRoot = 0.0;
    if (fmi3GetEventIndicators(instance, &previousRoot, 1) != fmi3OK) return 15;

    const fmi3Float64 time = 0.5;
    const fmi3Float64 boundary = 0.5;
    if (fmi3SetTime(instance, time) != fmi3OK) return 16;
    if (fmi3SetContinuousStates(instance, &boundary, 1) != fmi3OK) return 17;
    fmi3Float64 boundaryRoot = 0.0;
    if (fmi3GetEventIndicators(instance, &boundaryRoot, 1) != fmi3OK) return 18;
    if (boundaryRoot >= 0.0 || (previousRoot > 0.0) == (boundaryRoot > 0.0)) return 19;
    fmi3Boolean enterEvent = 0;
    if (fmi3CompletedIntegratorStep(instance, 1, &enterEvent, &terminate) != fmi3OK) return 20;
    if (enterEvent || terminate) return 21;
    if (fmi3EnterEventMode(instance) != fmi3OK) return 22;
    if (fmi3UpdateDiscreteStates(instance, &update, &terminate, &nominalsChanged,
            &valuesChanged, &nextTimeDefined, &nextTime) != fmi3OK) return 23;

    const fmi3ValueReference crossedVr = VR_Z;
    fmi3Float64 crossed = 0.0;
    if (fmi3GetFloat64(instance, &crossedVr, 1, &crossed, 1) != fmi3OK) return 24;
    fmi3FreeInstance(instance);
    return crossed == 1.0 ? 0 : 25;
}
"#;
    let source = format!("{model_c}\n{driver}");
    compile_and_run_c(&[("algebraic_root.c", &source)], &[]);
}

#[test]
#[cfg(feature = "template-runtime-tests")]
fn fmi3_dynamic_time_event_is_advertised_and_applied() {
    let compiled = compile_model(DYNAMIC_TIME_EVENT_SOURCE, "DynamicTimeEvent");
    let model_c = render_fmi_solve_template(&compiled, "fmi3", "model.c.jinja", "DynamicTimeEvent");
    let driver = r#"
int main(void) {
    fmi3Instance instance = fmi3InstantiateModelExchange(
        "dynamic-time", MODEL_INSTANTIATION_TOKEN, NULL, 0, 0, NULL, NULL);
    if (!instance) return 10;
    if (fmi3EnterInitializationMode(instance, 1, 1.0e-7, 0.0, 1, 0.2) != fmi3OK) return 11;
    if (fmi3ExitInitializationMode(instance) != fmi3OK) return 12;
    fmi3Boolean update = 0, terminate = 0, nominalsChanged = 0, valuesChanged = 0;
    fmi3Boolean nextTimeDefined = 0;
    fmi3Float64 nextTime = 0.0;
    if (fmi3UpdateDiscreteStates(instance, &update, &terminate, &nominalsChanged,
            &valuesChanged, &nextTimeDefined, &nextTime) != fmi3OK) return 13;
    if (!nextTimeDefined || fabs(nextTime - 0.1) > 1.0e-12) return 14;
    if (fmi3EnterContinuousTimeMode(instance) != fmi3OK) return 15;
    if (fmi3SetTime(instance, nextTime) != fmi3OK) return 16;
    if (fmi3EnterEventMode(instance) != fmi3OK) return 17;
    if (fmi3UpdateDiscreteStates(instance, &update, &terminate, &nominalsChanged,
            &valuesChanged, &nextTimeDefined, &nextTime) != fmi3OK) return 18;

    const fmi3ValueReference firedVr = VR_Z;
    fmi3Float64 fired = 0.0;
    if (fmi3GetFloat64(instance, &firedVr, 1, &fired, 1) != fmi3OK) return 19;
    fmi3FreeInstance(instance);
    return fired == 1.0 ? 0 : 20;
}
"#;
    let source = format!("{model_c}\n{driver}");
    compile_and_run_c(&[("dynamic_time_event.c", &source)], &[]);
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

    // The generic model-description context cannot prove that Solve-IR AD rows
    // are available, so it must not over-advertise this optional capability.
    let xml = render_fmi_solve_template(
        &compiled,
        "fmi3",
        "modelDescription.xml.jinja",
        "ParamDecay",
    );
    assert!(
        !xml.contains("providesDirectionalDerivatives"),
        "unexpected providesDirectionalDerivatives in XML"
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

    // The generic model-description context cannot prove that Solve-IR AD rows
    // are available, so it must not over-advertise this optional capability.
    let xml = render_fmi_solve_template(
        &compiled,
        "fmi3",
        "modelDescription.xml.jinja",
        "ParamDecay",
    );
    assert!(
        !xml.contains("providesAdjointDerivatives"),
        "unexpected providesAdjointDerivatives in XML"
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

    /* The generated model declares only Float64 variables. Other typed
       accessors must fail closed rather than reinterpret Float64 storage. */
    fmi3ValueReference vr_x = 0;
    fmi3Float32 f32_val = 0.0f;
    fmi3Status f32_get = fmi3GetFloat32(inst, &vr_x, 1, &f32_val, 1);
    fmi3Int32 i32_val = 0;
    fmi3Status i32_get = fmi3GetInt32(inst, &vr_x, 1, &i32_val, 1);
    fmi3Boolean bool_val = 0;
    fmi3Status bool_get = fmi3GetBoolean(inst, &vr_x, 1, &bool_val, 1);

    fmi3Float64 f64_before;
    fmi3GetContinuousStates(inst, &f64_before, 1);
    fmi3Float32 f32_set = 1.5f;
    fmi3Status f32_set_status = fmi3SetFloat32(inst, &vr_x, 1, &f32_set, 1);
    fmi3Float64 f64_after;
    fmi3GetContinuousStates(inst, &f64_after, 1);

    double err = fabs(x_first - x_second);
    double scale = fabs(x_first) > 1.0 ? fabs(x_first) : 1.0;

    printf("x_first=%.10g\n", x_first);
    printf("x_second=%.10g\n", x_second);
    printf("error=%.10g\n", err / scale);
    printf("f32_get=%d\n", f32_get);
    printf("i32_get=%d\n", i32_get);
    printf("bool_get=%d\n", bool_get);
    printf("f32_set=%d\n", f32_set_status);
    printf("f32_set_state_error=%.6g\n", fabs(f64_after - f64_before));

    fmi3FreeInstance(inst);
    int ok = (err / scale < 1e-10)
        && f32_get == fmi3Error && i32_get == fmi3Error
        && bool_get == fmi3Error && f32_set_status == fmi3Error
        && fabs(f64_after - f64_before) < 1e-12;
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

#[path = "backend_template_runtime_regression/extended_targets.rs"]
mod extended_targets;
