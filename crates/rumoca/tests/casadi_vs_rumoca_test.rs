//! Cross-validation tests: compare CasADi backend simulation traces against
//! rumoca's built-in diffsol simulator.
//!
//! For each model we:
//! 1. Compile the Modelica source with `Compiler`
//! 2. Run the built-in simulator (`simulate_dae`) to get the reference trace
//! 3. Render CasADi Python code (MX, SX), run via `python3`
//! 4. Compare the traces using `compare_model_traces` and assert agreement
//!
//! This follows the same pattern as the OMC comparison pipeline but runs
//! against CasADi backends instead.
//!
//! Requires: python3 with casadi and numpy installed.

use rumoca::Compiler;
use rumoca_phase_codegen::templates;
use rumoca_session::analysis::{ModelDeviationMetric, SimTrace, compare_model_traces};
use std::f64::consts::TAU;

/// High-agreement threshold for max channel bounded-normalized L1 error.
/// Matches `HIGH_AGREEMENT_THRESHOLD` from `sim_trace_compare`.
const HIGH_AGREEMENT_THRESHOLD: f64 = 0.05;
use rumoca_session::runtime::{SimOptions, SimResult, simulate_dae};
use std::collections::HashMap;
use std::fs;
use std::process::Command;
use tempfile::tempdir;

// ============================================================================
// Python driver
// ============================================================================

/// Driver for MX/SX templates: imports the generated module, uses `build_integrator`.
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
p_full = np.concatenate([model['p0'], np.array([])])
result = integrator(x0=model['x0'], p=p_full)
xf = np.array(result['xf'])
trace = {'times': tgrid.tolist(), 'names': model['state_names'], 'data': {}}
for i, name in enumerate(model['state_names']):
    trace['data'][name] = [float(xf[i, j]) for j in range(xf.shape[1])]
print(json.dumps(trace))
"#;

// ============================================================================
// Helpers
// ============================================================================

fn python_available() -> bool {
    Command::new("python3")
        .args(["-c", "import casadi; import numpy"])
        .output()
        .map(|o| o.status.success())
        .unwrap_or(false)
}

#[derive(Debug, Clone, Copy)]
enum CasadiBackend {
    Mx,
    Sx,
}

impl CasadiBackend {
    fn template(self) -> &'static str {
        match self {
            CasadiBackend::Mx => templates::CASADI_MX,
            CasadiBackend::Sx => templates::CASADI_SX,
        }
    }

    fn name(self) -> &'static str {
        match self {
            CasadiBackend::Mx => "MX",
            CasadiBackend::Sx => "SX",
        }
    }
}

/// Parsed CasADi simulation output (raw JSON).
struct CasadiRawTrace {
    times: Vec<f64>,
    names: Vec<String>,
    data: HashMap<String, Vec<f64>>,
}

/// Render a CasADi template, run via python3, parse JSON output.
fn casadi_simulate(
    dae: &rumoca_ir_dae::Dae,
    model_name: &str,
    backend: CasadiBackend,
    t_end: f64,
    dt: f64,
) -> Result<CasadiRawTrace, String> {
    let code = rumoca_phase_codegen::render_template_with_name(dae, backend.template(), model_name)
        .map_err(|e| format!("{} render error: {e}", backend.name()))?;

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
        return Err(format!(
            "{} python execution failed:\n{stderr}",
            backend.name()
        ));
    }

    let stdout = String::from_utf8(output.stdout).map_err(|e| format!("utf8 error: {e}"))?;

    let json: serde_json::Value =
        serde_json::from_str(&stdout).map_err(|e| format!("JSON parse error: {e}"))?;

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

    let mut data = HashMap::new();
    for name in &names {
        let vals: Vec<f64> = json["data"][name]
            .as_array()
            .ok_or_else(|| format!("missing data for {name}"))?
            .iter()
            .map(|v| v.as_f64().unwrap_or(f64::NAN))
            .collect();
        data.insert(name.clone(), vals);
    }

    Ok(CasadiRawTrace { times, names, data })
}

/// Convert a rumoca `SimResult` into the `SimTrace` format used by the
/// trace comparison infrastructure.
fn sim_result_to_trace(sim: &SimResult, model_name: &str) -> SimTrace {
    SimTrace {
        model_name: Some(model_name.to_string()),
        times: sim.times.clone(),
        names: sim.names.clone(),
        data: sim
            .data
            .iter()
            .map(|channel| channel.iter().map(|&v| Some(v)).collect())
            .collect(),
        variable_meta: None,
    }
}

/// Convert a CasADi raw trace into the `SimTrace` format.
fn casadi_trace_to_sim_trace(raw: &CasadiRawTrace, model_name: &str) -> SimTrace {
    let data: Vec<Vec<Option<f64>>> = raw
        .names
        .iter()
        .map(|name| {
            raw.data
                .get(name)
                .map(|vals| vals.iter().map(|&v| Some(v)).collect())
                .unwrap_or_default()
        })
        .collect();

    SimTrace {
        model_name: Some(model_name.to_string()),
        times: raw.times.clone(),
        names: raw.names.clone(),
        data,
        variable_meta: None,
    }
}

// ============================================================================
// Test model definitions
// ============================================================================

struct TestModel {
    name: &'static str,
    source: &'static str,
    t_end: f64,
    dt: f64,
}

/// Comprehensive set of test models covering different equation structures.
const TEST_MODELS: &[TestModel] = &[
    // --- Tier 2: Basic ODEs ---
    TestModel {
        name: "ExpDecay",
        source: r#"
model ExpDecay
    Real x(start=1);
equation
    der(x) = -x;
end ExpDecay;
"#,

        t_end: 2.0,
        dt: 0.01,
    },
    TestModel {
        name: "Integrator",
        source: r#"
model Integrator
    Real x(start=0);
equation
    der(x) = 1.0;
end Integrator;
"#,

        t_end: 1.0,
        dt: 0.01,
    },
    TestModel {
        name: "Oscillator",
        source: r#"
model Oscillator
    Real x(start=1);
    Real v(start=0);
equation
    der(x) = v;
        der(v) = -x;
end Oscillator;
"#,

        t_end: TAU,
        dt: 0.01,
    },
    // --- Tier 3: Parameters ---
    TestModel {
        name: "ParamDecay",
        source: r#"
model ParamDecay
    parameter Real k = 3;
    Real x(start=2);
equation
    der(x) = -k * x;
end ParamDecay;
"#,

        t_end: 1.0,
        dt: 0.01,
    },
    TestModel {
        name: "DampedSpring",
        source: r#"
model DampedSpring
    parameter Real k = 10;
    parameter Real c = 0.5;
    parameter Real m = 1;
    Real x(start=1);
    Real v(start=0);
equation
    der(x) = v;
    der(v) = -(k / m) * x - (c / m) * v;
end DampedSpring;
"#,

        t_end: 5.0,
        dt: 0.01,
    },
    TestModel {
        name: "MultiParam",
        source: r#"
model MultiParam
    parameter Real a = 2;
    parameter Real b = 0.5;
    parameter Real c = 1;
    Real x(start=1);
    Real y(start=0);
equation
    der(x) = -a * x + b * y;
    der(y) = c * x - b * y;
end MultiParam;
"#,

        t_end: 3.0,
        dt: 0.01,
    },
    // --- Coupled nonlinear systems ---
    TestModel {
        name: "LotkaVolterra",
        source: r#"
model LotkaVolterra
    parameter Real alpha = 1.5;
    parameter Real beta = 1.0;
    parameter Real delta = 1.0;
    parameter Real gamma = 3.0;
    Real x(start=1);
    Real y(start=1);
equation
    der(x) = alpha * x - beta * x * y;
    der(y) = delta * x * y - gamma * y;
end LotkaVolterra;
"#,

        t_end: 2.0,
        dt: 0.01,
    },
    // --- Implicit mass-matrix form ---
    TestModel {
        name: "ImplicitMass",
        source: r#"
model ImplicitMass
    parameter Real k = 10;
    parameter Real c = 0.5;
    parameter Real m = 2;
    Real x(start=1);
    Real v(start=0);
equation
    der(x) = v;
    m * der(v) = -k * x - c * v;
end ImplicitMass;
"#,

        t_end: 5.0,
        dt: 0.01,
    },
    // --- Transcendental / time-dependent ---
    TestModel {
        name: "SinForcing",
        source: r#"
model SinForcing
    Real x(start=0);
equation
    der(x) = sin(time);
end SinForcing;
"#,
        t_end: TAU,
        dt: 0.01,
    },
    // --- Nonlinear expressions ---
    TestModel {
        name: "ExpGrowthDecay",
        source: r#"
model ExpGrowthDecay
    Real x(start=0.1);
equation
    der(x) = x * (1 - x);
end ExpGrowthDecay;
"#,

        t_end: 5.0,
        dt: 0.01,
    },
    // --- Stiff system ---
    TestModel {
        name: "StiffDecay",
        source: r#"
model StiffDecay
    Real x(start=1);
    Real y(start=0);
equation
    der(x) = -100 * x + y;
    der(y) = x - y;
end StiffDecay;
"#,

        t_end: 1.0,
        dt: 0.001,
    },
    // --- Three-state system ---
    TestModel {
        name: "Lorenz",
        source: r#"
model Lorenz
    parameter Real sigma = 10;
    parameter Real rho = 28;
    parameter Real beta = 2.6667;
    Real x(start=1);
    Real y(start=1);
    Real z(start=1);
equation
    der(x) = sigma * (y - x);
    der(y) = x * (rho - z) - y;
    der(z) = x * y - beta * z;
end Lorenz;
"#,

        t_end: 1.0,
        dt: 0.001,
    },
    // --- Van der Pol oscillator ---
    TestModel {
        name: "VanDerPol",
        source: r#"
model VanDerPol
    parameter Real mu = 1;
    Real x(start=2);
    Real y(start=0);
equation
    der(x) = y;
    der(y) = mu * (1 - x * x) * y - x;
end VanDerPol;
"#,

        t_end: 10.0,
        dt: 0.01,
    },
    // --- If/else expression (piecewise) ---
    TestModel {
        name: "PiecewiseForcing",
        source: r#"
model PiecewiseForcing
    Real x(start=0);
equation
    der(x) = if time < 1 then 1 else if time < 2 then -1 else 0;
end PiecewiseForcing;
"#,
        t_end: 3.0,
        dt: 0.01,
    },
    // --- Algebraic variable (DAE) ---
    TestModel {
        name: "SimpleDAE",
        source: r#"
model SimpleDAE
    Real x(start=1);
    Real y;
    parameter Real a = -0.5;
    parameter Real b = 2.0;
equation
    der(x) = a * x;
    y = b * x;
end SimpleDAE;
"#,
        t_end: 3.0,
        dt: 0.01,
    },
    // --- Boolean parameter ---
    TestModel {
        name: "BoolParam",
        source: r#"
model BoolParam
    parameter Boolean useGain = true;
    parameter Real k = 2.0;
    Real x(start=1);
equation
    der(x) = if useGain then -k * x else -x;
end BoolParam;
"#,
        t_end: 2.0,
        dt: 0.01,
    },
    // --- Constants ---
    TestModel {
        name: "WithConstants",
        source: r#"
model WithConstants
    constant Real pi = 3.14159265358979;
    parameter Real omega = 2.0;
    Real x(start=0);
equation
    der(x) = sin(omega * pi * time);
end WithConstants;
"#,
        t_end: 1.0,
        dt: 0.01,
    },
    // --- abs/min/max builtins ---
    TestModel {
        name: "BuiltinFunctions",
        source: r#"
model BuiltinFunctions
    Real x(start=1);
    Real y(start=-1);
equation
    der(x) = -abs(y);
    der(y) = -sign(x) * sqrt(abs(x));
end BuiltinFunctions;
"#,
        t_end: 2.0,
        dt: 0.01,
    },
    // --- Multiple algebraic variables ---
    TestModel {
        name: "MultiAlgebraic",
        source: r#"
model MultiAlgebraic
    Real x(start=1);
    Real y;
    Real z;
    parameter Real k = 1.0;
equation
    der(x) = -k * x;
    y = 2 * x;
    z = y + x;
end MultiAlgebraic;
"#,
        t_end: 3.0,
        dt: 0.01,
    },
    // --- exp/log builtins ---
    TestModel {
        name: "TranscendentalODE",
        source: r#"
model TranscendentalODE
    Real x(start=1);
equation
    der(x) = -exp(-time) * cos(x);
end TranscendentalODE;
"#,
        t_end: 5.0,
        dt: 0.01,
    },
];

// ============================================================================
// Core test runner
// ============================================================================

/// Run a single model through a CasADi backend and compare against rumoca.
/// Returns the deviation metric for inspection.
fn cross_validate_single(
    model: &TestModel,
    backend: CasadiBackend,
    dae: &rumoca_ir_dae::Dae,
    rumoca_trace: &SimTrace,
) -> ModelDeviationMetric {
    let raw = casadi_simulate(dae, model.name, backend, model.t_end, model.dt)
        .unwrap_or_else(|e| panic!("{} {} simulation failed: {e}", model.name, backend.name()));

    let casadi_trace = casadi_trace_to_sim_trace(&raw, model.name);

    compare_model_traces(model.name, rumoca_trace, &casadi_trace).unwrap_or_else(|e| {
        panic!(
            "{} {} trace comparison failed: {e}",
            model.name,
            backend.name()
        )
    })
}

/// Run all CasADi backends for a single model and assert high agreement.
fn cross_validate_model(model: &TestModel) {
    if !python_available() {
        eprintln!("SKIP: python3 with casadi/numpy not available");
        return;
    }

    // 1. Compile
    let result = Compiler::new()
        .model(model.name)
        .compile_str(model.source, &format!("{}.mo", model.name))
        .unwrap_or_else(|e| panic!("{} compilation failed: {e}", model.name));

    // 2. Run rumoca built-in simulator (reference)
    let opts = SimOptions {
        t_end: model.t_end,
        ..SimOptions::default()
    };
    let sim = simulate_dae(&result.dae, &opts)
        .unwrap_or_else(|e| panic!("{} rumoca simulation failed: {e}", model.name));

    let rumoca_trace = sim_result_to_trace(&sim, model.name);

    // 3. Test each CasADi backend
    let backends = [CasadiBackend::Mx, CasadiBackend::Sx];

    for backend in backends {
        let metric = cross_validate_single(model, backend, &result.dae, &rumoca_trace);

        // Assert high agreement: max channel bounded-normalized L1 < 0.05
        assert!(
            metric.max_channel_bounded_normalized_l1 < HIGH_AGREEMENT_THRESHOLD,
            "{} {}: max channel bounded L1 = {:.6e} exceeds high-agreement threshold {:.6e}\n\
             median={:.6e}, mean={:.6e}, worst={:?}",
            model.name,
            backend.name(),
            metric.max_channel_bounded_normalized_l1,
            HIGH_AGREEMENT_THRESHOLD,
            metric.bounded_normalized_l1_score,
            metric.mean_channel_bounded_normalized_l1,
            metric
                .worst_variables
                .iter()
                .map(|w| format!("{}={:.6e}", w.name, w.bounded_normalized_l1_error))
                .collect::<Vec<_>>(),
        );
    }
}

// ============================================================================
// Individual test cases (one per model for granular CI reporting)
// ============================================================================

fn find_model(name: &str) -> &'static TestModel {
    TEST_MODELS
        .iter()
        .find(|m| m.name == name)
        .unwrap_or_else(|| panic!("test model '{name}' not found"))
}

#[test]
fn casadi_vs_rumoca_exp_decay() {
    cross_validate_model(find_model("ExpDecay"));
}

#[test]
fn casadi_vs_rumoca_integrator() {
    cross_validate_model(find_model("Integrator"));
}

#[test]
fn casadi_vs_rumoca_oscillator() {
    cross_validate_model(find_model("Oscillator"));
}

#[test]
fn casadi_vs_rumoca_param_decay() {
    cross_validate_model(find_model("ParamDecay"));
}

#[test]
fn casadi_vs_rumoca_damped_spring() {
    cross_validate_model(find_model("DampedSpring"));
}

#[test]
fn casadi_vs_rumoca_multi_param() {
    cross_validate_model(find_model("MultiParam"));
}

#[test]
fn casadi_vs_rumoca_lotka_volterra() {
    cross_validate_model(find_model("LotkaVolterra"));
}

#[test]
fn casadi_vs_rumoca_implicit_mass() {
    cross_validate_model(find_model("ImplicitMass"));
}

#[test]
fn casadi_vs_rumoca_sin_forcing() {
    cross_validate_model(find_model("SinForcing"));
}

#[test]
fn casadi_vs_rumoca_exp_growth_decay() {
    cross_validate_model(find_model("ExpGrowthDecay"));
}

#[test]
fn casadi_vs_rumoca_stiff_decay() {
    cross_validate_model(find_model("StiffDecay"));
}

#[test]
fn casadi_vs_rumoca_lorenz() {
    cross_validate_model(find_model("Lorenz"));
}

#[test]
fn casadi_vs_rumoca_van_der_pol() {
    cross_validate_model(find_model("VanDerPol"));
}

#[test]
fn casadi_vs_rumoca_piecewise_forcing() {
    cross_validate_model(find_model("PiecewiseForcing"));
}

#[test]
fn casadi_vs_rumoca_simple_dae() {
    cross_validate_model(find_model("SimpleDAE"));
}

#[test]
fn casadi_vs_rumoca_bool_param() {
    cross_validate_model(find_model("BoolParam"));
}

#[test]
fn casadi_vs_rumoca_with_constants() {
    cross_validate_model(find_model("WithConstants"));
}

#[test]
fn casadi_vs_rumoca_builtin_functions() {
    cross_validate_model(find_model("BuiltinFunctions"));
}

#[test]
fn casadi_vs_rumoca_multi_algebraic() {
    cross_validate_model(find_model("MultiAlgebraic"));
}

#[test]
fn casadi_vs_rumoca_transcendental_ode() {
    cross_validate_model(find_model("TranscendentalODE"));
}

// ============================================================================
// Aggregate summary test
// ============================================================================

/// Run all models through all backends and print a summary table.
/// This provides a single-shot view analogous to the OMC quality baseline.
#[test]
fn casadi_vs_rumoca_all_models_summary() {
    if !python_available() {
        eprintln!("SKIP: python3 with casadi/numpy not available");
        return;
    }

    let backends = [CasadiBackend::Mx, CasadiBackend::Sx];
    let (results, failures) = collect_summary_results(backends);
    let (high_count, total_count) = print_summary_table(&results, &failures);

    // Assert all succeeded with high agreement
    assert!(
        failures.is_empty(),
        "{} model/backend combinations failed:\n{}",
        failures.len(),
        failures.join("\n")
    );
    assert_eq!(
        high_count, total_count,
        "Expected all {total_count} model/backend combinations to have high agreement, \
         got {high_count}"
    );
}

fn collect_summary_results(
    backends: [CasadiBackend; 2],
) -> (Vec<(String, String, ModelDeviationMetric)>, Vec<String>) {
    let mut results: Vec<(String, String, ModelDeviationMetric)> = Vec::new();
    let mut failures: Vec<String> = Vec::new();

    for model in TEST_MODELS {
        let Some((dae, rumoca_trace)) = prepare_summary_model_data(model, &mut failures) else {
            continue;
        };

        for backend in backends {
            push_backend_summary_result(
                model,
                backend,
                &dae,
                &rumoca_trace,
                &mut results,
                &mut failures,
            );
        }
    }

    (results, failures)
}

fn prepare_summary_model_data(
    model: &TestModel,
    failures: &mut Vec<String>,
) -> Option<(rumoca_ir_dae::Dae, SimTrace)> {
    let compile_result = match Compiler::new()
        .model(model.name)
        .compile_str(model.source, &format!("{}.mo", model.name))
    {
        Ok(r) => r,
        Err(e) => {
            failures.push(format!("{}: compile error: {e}", model.name));
            return None;
        }
    };

    let opts = SimOptions {
        t_end: model.t_end,
        ..SimOptions::default()
    };
    let dae = compile_result.dae;
    let sim = match simulate_dae(&dae, &opts) {
        Ok(s) => s,
        Err(e) => {
            failures.push(format!("{}: rumoca sim error: {e}", model.name));
            return None;
        }
    };

    Some((dae, sim_result_to_trace(&sim, model.name)))
}

fn push_backend_summary_result(
    model: &TestModel,
    backend: CasadiBackend,
    dae: &rumoca_ir_dae::Dae,
    rumoca_trace: &SimTrace,
    results: &mut Vec<(String, String, ModelDeviationMetric)>,
    failures: &mut Vec<String>,
) {
    let raw = match casadi_simulate(dae, model.name, backend, model.t_end, model.dt) {
        Ok(raw) => raw,
        Err(e) => {
            failures.push(format!(
                "{} {}: casadi sim error: {e}",
                model.name,
                backend.name()
            ));
            return;
        }
    };

    let casadi_trace = casadi_trace_to_sim_trace(&raw, model.name);
    match compare_model_traces(model.name, rumoca_trace, &casadi_trace) {
        Ok(metric) => results.push((model.name.to_string(), backend.name().to_string(), metric)),
        Err(e) => failures.push(format!(
            "{} {}: trace compare error: {e}",
            model.name,
            backend.name()
        )),
    }
}

fn print_summary_table(
    results: &[(String, String, ModelDeviationMetric)],
    failures: &[String],
) -> (usize, usize) {
    eprintln!("\n{:=<80}", "");
    eprintln!("CasADi vs Rumoca Trace Comparison Summary");
    eprintln!("{:=<80}", "");
    eprintln!(
        "{:<20} {:<6} {:>8} {:>10} {:>10} {:>10}",
        "Model", "Back.", "Vars", "Median", "Mean", "Max"
    );
    eprintln!("{:-<80}", "");

    let mut high_count = 0usize;
    let mut total_count = 0usize;

    for (name, backend, metric) in results {
        let agreement = if metric.max_channel_bounded_normalized_l1 < HIGH_AGREEMENT_THRESHOLD {
            high_count += 1;
            "HIGH"
        } else {
            "    "
        };
        total_count += 1;
        eprintln!(
            "{:<20} {:<6} {:>8} {:>10.2e} {:>10.2e} {:>10.2e} {}",
            name,
            backend,
            metric.compared_variables,
            metric.bounded_normalized_l1_score,
            metric.mean_channel_bounded_normalized_l1,
            metric.max_channel_bounded_normalized_l1,
            agreement,
        );
    }

    eprintln!("{:-<80}", "");
    eprintln!(
        "High agreement: {}/{} ({:.1}%)",
        high_count,
        total_count,
        if total_count > 0 {
            100.0 * high_count as f64 / total_count as f64
        } else {
            0.0
        }
    );

    if !failures.is_empty() {
        eprintln!("\nFailures ({}):", failures.len());
        for failure in failures {
            eprintln!("  {failure}");
        }
    }
    eprintln!("{:=<80}\n", "");
    (high_count, total_count)
}
