//! Roadmap Track 0.1: CasADi oracle for the forward parameter Jacobian.
//!
//! Renders the CasADi (MX) target for a small pure-ODE model, computes
//! `∂(der)/∂p = ca.jacobian(ode, p)` at the initial point in a Python driver,
//! and compares it to rumoca's `parameter_jacobian_for_dae` by (state, param)
//! name. Independent exact-gradient check of the parameter-seed AD.
//!
//! Requires: python3 with casadi and numpy installed (skipped otherwise).

use rumoca::Compiler;
use rumoca_phase_codegen::templates;
use rumoca_sim::SimOptions;
use std::collections::HashMap;
use std::fs;
use std::process::Command;
use tempfile::tempdir;

// Pure ODE (no algebraics): `∂(der)/∂p` is unambiguous, so CasADi's
// `ca.jacobian(ode, p)` is the exact oracle for rumoca's parameter-seed AD.
const SOURCE: &str = r#"
model PureOde
  parameter Real a = 2.0;
  parameter Real b = 3.0;
  parameter Real k = 1.25;
  Real x(start = 1.5);
  Real w(start = 0.7);
equation
  der(x) = -a * x + b * w + k;
  der(w) = a * w - b * x;
end PureOde;
"#;

// Computes `∂(der)/∂p` of the rendered CasADi model at its initial point and
// prints `{state_names, param_names, matrix}` (matrix[row=state][col=param]).
const DRIVER: &str = r#"
import importlib.util, json, os
import numpy as np
import casadi as ca

spec = importlib.util.spec_from_file_location(
    "model", os.path.join(os.path.dirname(__file__), "model.py"))
mod = importlib.util.module_from_spec(spec)
spec.loader.exec_module(mod)

d = mod.create_model()
ode, x, p, t, z, u = d['ode'], d['x'], d['p'], d['t'], d['z'], d['u']
dder_dp = ca.jacobian(ode, p)
dder_dp_fn = ca.Function('dder_dp', [x, z, u, p, t], [dder_dp])
val = np.array(dder_dp_fn(d['x0'], d['z0'], d['u0'], d['p_full0'], 0.0))
out = {
    'state_names': d['state_names'],
    'param_names': d['param_names'],
    'matrix': [[float(val[i, j]) for j in range(val.shape[1])]
               for i in range(val.shape[0])],
}
print(json.dumps(out))
"#;

fn python_casadi_available() -> bool {
    Command::new("python3")
        .args(["-c", "import casadi; import numpy"])
        .output()
        .map(|o| o.status.success())
        .unwrap_or(false)
}

struct CasadiParamJacobian {
    state_names: Vec<String>,
    param_names: Vec<String>,
    matrix: Vec<Vec<f64>>,
}

fn casadi_parameter_jacobian(dae: &rumoca_ir_dae::Dae, model: &str) -> CasadiParamJacobian {
    let template = templates::builtin_template_source("casadi-mx", "casadi_mx.py.jinja")
        .expect("casadi-mx template should exist");
    let code = rumoca_phase_codegen::render_template_with_name(dae, template, model)
        .expect("casadi-mx should render");
    let dir = tempdir().expect("tempdir");
    fs::write(dir.path().join("model.py"), &code).expect("write model.py");
    fs::write(dir.path().join("driver.py"), DRIVER).expect("write driver.py");

    let output = Command::new("python3")
        .arg(dir.path().join("driver.py"))
        .output()
        .expect("run python3 driver");
    assert!(
        output.status.success(),
        "casadi driver failed:\n{}",
        String::from_utf8_lossy(&output.stderr)
    );
    let json: serde_json::Value =
        serde_json::from_slice(&output.stdout).expect("driver should print JSON");
    let strings = |key: &str| {
        json[key]
            .as_array()
            .unwrap()
            .iter()
            .map(|v| v.as_str().unwrap().to_string())
            .collect::<Vec<_>>()
    };
    let matrix = json["matrix"]
        .as_array()
        .unwrap()
        .iter()
        .map(|row| {
            row.as_array()
                .unwrap()
                .iter()
                .map(|v| v.as_f64().unwrap())
                .collect::<Vec<_>>()
        })
        .collect();
    CasadiParamJacobian {
        state_names: strings("state_names"),
        param_names: strings("param_names"),
        matrix,
    }
}

#[test]
fn parameter_jacobian_matches_casadi_oracle() {
    if !python_casadi_available() {
        eprintln!("skipping CasADi parameter-Jacobian oracle: python3 with casadi/numpy not found");
        return;
    }

    let result = Compiler::new()
        .model("PureOde")
        .compile_str(SOURCE, "PureOde.mo")
        .expect("PureOde should compile");

    let probe =
        rumoca_sim::parameter_jacobian_for_dae(&result.dae, &SimOptions::default(), &[], 0.0)
            .expect("rumoca parameter Jacobian should evaluate");
    let report = &probe.report;
    assert!(
        report.error.is_none(),
        "rumoca parameter Jacobian: {:?}",
        report.error
    );

    // Index rumoca's report by name so it does not matter that its column set
    // may also carry internal parameter slots (CasADi only exposes a, b, k).
    let row_index: HashMap<&str, usize> = report
        .row_labels
        .iter()
        .enumerate()
        .map(|(i, n)| (n.as_str(), i))
        .collect();
    let col_index: HashMap<&str, usize> = report
        .param_labels
        .iter()
        .enumerate()
        .map(|(j, n)| (n.as_str(), j))
        .collect();

    let oracle = casadi_parameter_jacobian(&result.dae, "PureOde");
    let mut compared = 0;
    for (ci, state) in oracle.state_names.iter().enumerate() {
        for (cj, param) in oracle.param_names.iter().enumerate() {
            let ri = *row_index
                .get(state.as_str())
                .unwrap_or_else(|| panic!("rumoca report missing state row `{state}`"));
            let rj = *col_index
                .get(param.as_str())
                .unwrap_or_else(|| panic!("rumoca report missing parameter column `{param}`"));
            let casadi = oracle.matrix[ci][cj];
            let rumoca = report.matrix[ri][rj];
            assert!(
                (casadi - rumoca).abs() <= 1.0e-9 + 1.0e-9 * casadi.abs(),
                "∂der({state})/∂{param}: rumoca={rumoca}, casadi={casadi}"
            );
            compared += 1;
        }
    }
    assert_eq!(
        compared,
        oracle.state_names.len() * oracle.param_names.len()
    );
    assert!(
        compared >= 6,
        "expected 2 states x 3 params compared, got {compared}"
    );
}
