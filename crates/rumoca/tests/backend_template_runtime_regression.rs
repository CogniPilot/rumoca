//! Runtime regression tests for backend templates.
//!
//! For each backend (CasADi MX, CasADi SX, Embedded C, FMI2, SymPy) and each
//! test model (Ball, ParamDecay, Oscillator), we:
//!   1. Compile the Modelica source and render the backend template
//!   2. Execute the generated code (Python or C) to produce a CSV trace
//!   3. Run rumoca's built-in diffsol simulator to produce a reference trace
//!   4. Compare the two traces and assert bounded relative error

use std::collections::HashMap;
use std::{fs, process::Command};

use rumoca::Compiler;
use rumoca_phase_codegen::templates;
use rumoca_session::runtime::{
    SimOptions, SimResult, prepare_dae_for_template_codegen, simulate_dae,
};
use tempfile::Builder;

// ============================================================================
// Tolerance — max bounded relative error: |a-b| / max(|a|, |b|, 1.0)
// ============================================================================

/// CasADi uses CVODES (adaptive high-order), so traces should be tight.
const CASADI_TOLERANCE: f64 = 0.01;

/// Embedded C uses RK4 with dt=0.001, FMI2 uses forward Euler with dt=0.001.
const C_TOLERANCE: f64 = 0.05;

// ============================================================================
// Runtime detection
// ============================================================================

fn python_command() -> &'static str {
    for candidate in ["python3", "python"] {
        if Command::new(candidate).arg("--version").output().is_ok() {
            return candidate;
        }
    }
    panic!("expected python3 or python to be available");
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

fn prepare_dae(source: &str, model_name: &str) -> rumoca_ir_dae::Dae {
    let compiled = compile_model(source, model_name);
    prepare_dae_for_template_codegen(&compiled.dae, true).expect("prepare DAE for codegen")
}

fn render_template(source: &str, model_name: &str, template: &str) -> String {
    let dae = prepare_dae(source, model_name);
    rumoca_phase_codegen::render_template_with_name(&dae, template, model_name)
        .expect("render template")
}

// ============================================================================
// Reference trace from rumoca's built-in simulator
// ============================================================================

fn reference_trace(source: &str, model_name: &str, t_end: f64) -> (rumoca_ir_dae::Dae, SimResult) {
    let dae = prepare_dae(source, model_name);
    let opts = SimOptions {
        t_end,
        ..SimOptions::default()
    };
    let sim = simulate_dae(&dae, &opts).expect("rumoca simulation");
    (dae, sim)
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

fn assert_traces_match(
    backend_traces: &HashMap<String, Vec<(f64, f64)>>,
    dae: &rumoca_ir_dae::Dae,
    sim: &SimResult,
    tolerance: f64,
    backend_name: &str,
) {
    for name in dae.states.keys() {
        let name_str = name.as_str();
        let Some(backend_trace) = backend_traces.get(name_str) else {
            continue;
        };
        let Some(ref_trace) = extract_sim_trace(sim, name_str) else {
            continue;
        };
        let dev = trace_max_deviation(backend_trace, &ref_trace);
        assert!(
            dev <= tolerance,
            "{backend_name}: state '{name_str}' deviation {dev:.4e} exceeds tolerance {tolerance:.4e}"
        );
    }
}

// ============================================================================
// Python execution helper (returns stdout)
// ============================================================================

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

// ============================================================================
// CasADi driver — outputs CSV: time,state1,state2,...
// ============================================================================

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
// Embedded C CSV driver template
// ============================================================================

fn embedded_c_csv_main(model_name: &str, n_states: usize, state_names: &[&str]) -> String {
    let header_cols: Vec<String> = state_names.iter().map(|n| format!(",{n}")).collect();
    let print_cols: Vec<String> = (0..n_states)
        .map(|i| format!("        printf(\",%.10g\", m.x[{i}]);"))
        .collect();

    format!(
        r#"#include <stdio.h>
#include <math.h>
#include "model.h"

int main(void) {{
    {model_name}_t m;
    {model_name}_init(&m);

    double t = 0.0;
    double dt = 0.001;
    int steps = 1000;

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
        header_cols = header_cols.join(""),
        print_cols = print_cols.join("\n"),
    )
}

// ============================================================================
// CasADi MX runtime tests
// ============================================================================

fn casadi_trace_test(source: &str, model_name: &str, template: &str) {
    let rendered = render_template(source, model_name, template);
    let csv = run_python(&rendered, CASADI_CSV_DRIVER);
    let backend_traces = parse_csv_traces(&csv);
    let (dae, sim) = reference_trace(source, model_name, 1.0);
    assert_traces_match(&backend_traces, &dae, &sim, CASADI_TOLERANCE, "CasADi");
}

#[test]
#[ignore = "requires runtimes; run via `rum verify template-runtimes`"]
fn casadi_mx_ball() {
    casadi_trace_test(BALL_SOURCE, "Ball", templates::CASADI_MX);
}

#[test]
#[ignore = "requires runtimes; run via `rum verify template-runtimes`"]
fn casadi_mx_param_decay() {
    casadi_trace_test(PARAM_DECAY_SOURCE, "ParamDecay", templates::CASADI_MX);
}

#[test]
#[ignore = "requires runtimes; run via `rum verify template-runtimes`"]
fn casadi_mx_oscillator() {
    casadi_trace_test(OSCILLATOR_SOURCE, "Oscillator", templates::CASADI_MX);
}

// ============================================================================
// CasADi SX runtime tests
// ============================================================================

#[test]
#[ignore = "requires runtimes; run via `rum verify template-runtimes`"]
fn casadi_sx_ball() {
    casadi_trace_test(BALL_SOURCE, "Ball", templates::CASADI_SX);
}

#[test]
#[ignore = "requires runtimes; run via `rum verify template-runtimes`"]
fn casadi_sx_param_decay() {
    casadi_trace_test(PARAM_DECAY_SOURCE, "ParamDecay", templates::CASADI_SX);
}

#[test]
#[ignore = "requires runtimes; run via `rum verify template-runtimes`"]
fn casadi_sx_oscillator() {
    casadi_trace_test(OSCILLATOR_SOURCE, "Oscillator", templates::CASADI_SX);
}

// ============================================================================
// Embedded C runtime tests
// ============================================================================

fn embedded_c_trace_test(source: &str, model_name: &str) {
    let dae = prepare_dae(source, model_name);
    let rendered =
        rumoca_phase_codegen::render_template_with_name(&dae, templates::EMBEDDED_C, model_name)
            .expect("render template");

    // State names must be sorted alphabetically to match JSON serialization order
    // (serde_json::Map uses BTreeMap, so templates see states in sorted order)
    let mut state_names: Vec<&str> = dae.states.keys().map(|k| k.as_str()).collect();
    state_names.sort();
    let main_c = embedded_c_csv_main(model_name, state_names.len(), &state_names);

    let csv = compile_and_run_c(&[("model.h", &rendered), ("main.c", &main_c)], &[]);
    let backend_traces = parse_csv_traces(&csv);

    let opts = SimOptions {
        t_end: 1.0,
        ..SimOptions::default()
    };
    let sim = simulate_dae(&dae, &opts).expect("rumoca simulation");
    assert_traces_match(&backend_traces, &dae, &sim, C_TOLERANCE, "Embedded C");
}

#[test]
#[ignore = "requires runtimes; run via `rum verify template-runtimes`"]
fn embedded_c_ball() {
    embedded_c_trace_test(BALL_SOURCE, "Ball");
}

#[test]
#[ignore = "requires runtimes; run via `rum verify template-runtimes`"]
fn embedded_c_param_decay() {
    embedded_c_trace_test(PARAM_DECAY_SOURCE, "ParamDecay");
}

#[test]
#[ignore = "requires runtimes; run via `rum verify template-runtimes`"]
fn embedded_c_oscillator() {
    embedded_c_trace_test(OSCILLATOR_SOURCE, "Oscillator");
}

// ============================================================================
// FMI 2.0 runtime tests
// ============================================================================

fn fmi2_trace_test(source: &str, model_name: &str) {
    let dae = prepare_dae(source, model_name);

    let model_c =
        rumoca_phase_codegen::render_template_with_name(&dae, templates::FMI2_MODEL, model_name)
            .expect("render FMI2 model");

    let driver_c = rumoca_phase_codegen::render_template_with_name(
        &dae,
        templates::FMI2_TEST_DRIVER,
        model_name,
    )
    .expect("render FMI2 test driver");

    let csv = compile_and_run_c(
        &[("model.c", &model_c), ("driver.c", &driver_c)],
        &["--t-end", "1.0", "--dt", "0.001"],
    );
    let backend_traces = parse_csv_traces(&csv);

    let opts = SimOptions {
        t_end: 1.0,
        ..SimOptions::default()
    };
    let sim = simulate_dae(&dae, &opts).expect("rumoca simulation");
    assert_traces_match(&backend_traces, &dae, &sim, C_TOLERANCE, "FMI2");
}

#[test]
#[ignore = "requires runtimes; run via `rum verify template-runtimes`"]
fn fmi2_ball() {
    fmi2_trace_test(BALL_SOURCE, "Ball");
}

#[test]
#[ignore = "requires runtimes; run via `rum verify template-runtimes`"]
fn fmi2_param_decay() {
    fmi2_trace_test(PARAM_DECAY_SOURCE, "ParamDecay");
}

#[test]
#[ignore = "requires runtimes; run via `rum verify template-runtimes`"]
fn fmi2_oscillator() {
    fmi2_trace_test(OSCILLATOR_SOURCE, "Oscillator");
}

// ============================================================================
// SymPy runtime tests
//
// SymPy generates a symbolic model, not a time-domain simulation. We verify
// that the explicit symbolic solve produces correct derivative expressions
// by evaluating them at the initial condition and comparing against the
// rumoca reference derivatives.
// ============================================================================

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

fn sympy_trace_test(source: &str, model_name: &str) {
    let dae = prepare_dae(source, model_name);
    let rendered =
        rumoca_phase_codegen::render_template_with_name(&dae, templates::SYMPY, model_name)
            .expect("render template");

    let stdout = run_python(&rendered, SYMPY_EVAL_DRIVER);
    let result: serde_json::Value = serde_json::from_str(stdout.trim()).expect("parse JSON output");

    // Get reference derivatives at t=0 from rumoca simulator
    let opts = SimOptions {
        t_end: 0.001,
        ..SimOptions::default()
    };
    let sim = simulate_dae(&dae, &opts).expect("rumoca simulation");

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
#[ignore = "requires runtimes; run via `rum verify template-runtimes`"]
fn sympy_ball() {
    sympy_trace_test(BALL_SOURCE, "Ball");
}

#[test]
#[ignore = "requires runtimes; run via `rum verify template-runtimes`"]
fn sympy_param_decay() {
    sympy_trace_test(PARAM_DECAY_SOURCE, "ParamDecay");
}

#[test]
#[ignore = "requires runtimes; run via `rum verify template-runtimes`"]
fn sympy_oscillator() {
    sympy_trace_test(OSCILLATOR_SOURCE, "Oscillator");
}
