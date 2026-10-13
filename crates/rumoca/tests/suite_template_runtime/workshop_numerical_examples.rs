//! Execute the checked-in workshop models, including parameter/state initialization
//! and algebraic refresh, against the canonical native runtime at multiple points.
use rumoca::{CompilationResult, Compiler};
use rumoca_ir_solve::ScalarSlot;
use rumoca_sim::{SimOptions, lower_for_simulation_with_overrides};
use std::{fs, path::Path, process::Command};

const EXAMPLES: &[(&str, &str)] = &[
    (
        "SympyDecay",
        include_str!(concat!(
            env!("CARGO_MANIFEST_DIR"),
            "/../../examples/models/SympyDecay.mo"
        )),
    ),
    (
        "NeuralODETensor",
        include_str!(concat!(
            env!("CARGO_MANIFEST_DIR"),
            "/../../examples/models/NeuralODETensor.mo"
        )),
    ),
    (
        "NeuralPredatorPrey",
        include_str!(concat!(
            env!("CARGO_MANIFEST_DIR"),
            "/../../examples/models/NeuralPredatorPrey.mo"
        )),
    ),
    (
        "NeuralLatentOscillator",
        include_str!(concat!(
            env!("CARGO_MANIFEST_DIR"),
            "/../../examples/models/NeuralLatentOscillator.mo"
        )),
    ),
    (
        "NeuralODEBackprop",
        include_str!(concat!(
            env!("CARGO_MANIFEST_DIR"),
            "/../../examples/models/NeuralODEBackprop.mo"
        )),
    ),
];

struct NativeSnapshot {
    parameters: Vec<f64>,
    initial_y: Vec<f64>,
    state_count: usize,
    computed_starts: Vec<usize>,
    probes: Vec<(f64, Vec<f64>, Vec<f64>)>,
    first_state_direction: Vec<f64>,
}

fn native_snapshot(compiled: &CompilationResult, model: &str) -> NativeSnapshot {
    let native = lower_for_simulation_with_overrides(&compiled.dae, &SimOptions::default())
        .unwrap_or_else(|error| panic!("{model} prepares natively: {error:?}"));
    let runtime = rumoca_solver::SolveRuntime::new(&native)
        .unwrap_or_else(|error| panic!("{model} constructs its native runtime: {error:?}"));
    let state_count = native.state_scalar_count();
    assert!(state_count > 0 && !native.parameters.is_empty(), "{model}");
    assert!(
        native
            .parameters
            .iter()
            .chain(&native.initial_y[..state_count])
            .all(|value| value.is_finite()),
        "{model} has finite initialized defaults"
    );
    let initial = native.initial_y[..state_count].to_vec();
    let rhs = |time, state: &[f64]| {
        let report = runtime.eval_at(time, state, &native.parameters, 1.0e-10, 32);
        assert!(
            report.error.is_none(),
            "{model} native RHS: {:?}",
            report.error
        );
        let values = report
            .derivatives
            .into_iter()
            .map(|slot| slot.value)
            .collect::<Vec<_>>();
        assert_eq!(values.len(), state_count, "{model}");
        assert!(values.iter().all(|value| value.is_finite()), "{model}");
        values
    };
    let mut changed = initial.clone();
    changed[0] += 0.125;
    let probes = vec![
        (0.125, initial.clone(), rhs(0.125, &initial)),
        (0.375, changed.clone(), rhs(0.375, &changed)),
    ];
    let mut above = initial.clone();
    let mut below = initial;
    above[0] += 1.0e-5;
    below[0] -= 1.0e-5;
    let first_state_direction = rhs(0.125, &above)
        .into_iter()
        .zip(rhs(0.125, &below))
        .map(|(above, below)| (above - below) / 2.0e-5)
        .collect();
    let computed_starts = native
        .problem
        .initialization
        .update_targets()
        .iter()
        .filter_map(|target| match target {
            ScalarSlot::Y { index, .. } if *index < state_count => Some(*index),
            _ => None,
        })
        .collect();
    NativeSnapshot {
        parameters: native.parameters.to_vec(),
        initial_y: native.initial_y.to_vec(),
        state_count,
        computed_starts,
        probes,
        first_state_direction,
    }
}

fn run(command: &mut Command, context: &str) {
    let output = command
        .output()
        .unwrap_or_else(|error| panic!("{context}: {error}"));
    assert!(
        output.status.success(),
        "{context}: {:?}\n{}\n{}",
        output.status.code(),
        String::from_utf8_lossy(&output.stdout),
        String::from_utf8_lossy(&output.stderr)
    );
}

fn c_values(values: &[f64]) -> String {
    values
        .iter()
        .map(|value| format!("{value:.17e}"))
        .collect::<Vec<_>>()
        .join(", ")
}

#[test]
fn c_ode_actual_workshop_examples_initialize_and_match_native_rhs() {
    for &(model, source) in EXAMPLES {
        let compiled = Compiler::new()
            .model(model)
            .compile_str(source, &format!("{model}.mo"))
            .unwrap_or_else(|error| panic!("{model} compiles: {error:?}"));
        let expected = native_snapshot(&compiled, model);
        let files = rumoca::render_target_files(&compiled, model, "c-ode", None)
            .unwrap_or_else(|error| panic!("{model} renders C: {error:#}"));
        let content = |path: &str| {
            files
                .iter()
                .find(|file| file.path == path)
                .expect("target file")
                .content
                .as_str()
        };
        let prefix = format!("RUMOCA_{}", model.to_uppercase());
        let mut driver = format!(
            "{}\n{}\nint main(void) {{\n",
            content(&format!("{model}_ode.h")),
            content(&format!("{model}_ode.c")).replace(&format!("#include \"{model}_ode.h\""), "")
        );
        driver.push_str(&format!("double p[{prefix}_P_LEN];\ndouble y[{prefix}_Y_LEN] = {{{}}};\ndouble out[{prefix}_DERIVATIVE_LEN];\nconst double expected_p[] = {{{}}};\nconst double expected_x[] = {{{}}};\n", c_values(&expected.initial_y), c_values(&expected.parameters), c_values(&expected.initial_y[..expected.state_count])));
        driver.push_str(&format!("if ({prefix}_P_LEN != {} || {prefix}_Y_LEN != {}) return 1;\nfor (size_t i=0; i<{prefix}_P_LEN; ++i) p[i]={model}_default_p[i];\nif ({model}_init_params(p) != 0) return 2;\nfor (size_t i=0; i<{prefix}_P_LEN; ++i) if (!isfinite(p[i]) || fabs(p[i]-expected_p[i]) > 1e-8*(1+fabs(expected_p[i]))) return 3;\n", expected.parameters.len(), expected.initial_y.len()));
        for index in &expected.computed_starts {
            driver.push_str(&format!("y[{index}]=NAN;\n"));
        }
        driver.push_str(&format!("if ({model}_init_state(p,y) != 0) return 4;\nfor (size_t i=0; i<{}; ++i) if (!isfinite(y[i]) || fabs(y[i]-expected_x[i]) > 1e-8*(1+fabs(expected_x[i]))) return 5;\n", expected.state_count));
        for (probe, (time, state, rhs)) in expected.probes.iter().enumerate() {
            driver.push_str(&format!("{{\nconst double x[] = {{{}}};\nconst double expected_rhs[] = {{{}}};\nfor (size_t i=0; i<{}; ++i) y[i]=x[i];\nif ({model}_derivative_rhs({time:.17e},y,p,out) != 0) return {};\nfor (size_t i=0; i<{}; ++i) if (!isfinite(out[i]) || fabs(out[i]-expected_rhs[i]) > 1e-8*(1+fabs(expected_rhs[i]))) return {};\n}}\n", c_values(state), c_values(rhs), expected.state_count, 6 + probe * 2, expected.state_count, 7 + probe * 2));
        }
        driver.push_str("return 0;\n}\n");
        let work = tempfile::tempdir().unwrap();
        let input = work.path().join("workshop.c");
        let executable = work.path().join("workshop");
        fs::write(&input, driver).unwrap();
        run(
            Command::new("cc")
                .args(["-std=c99", "-Wall", "-Wextra", "-Wpedantic", "-Werror"])
                .arg(&input)
                .arg("-lm")
                .arg("-o")
                .arg(&executable),
            &format!("{model} C compilation"),
        );
        run(
            &mut Command::new(executable),
            &format!("{model} C initialization and RHS"),
        );
    }
}

fn python_examples(target: &str, script: &str) {
    for &(model, source) in EXAMPLES {
        let compiled = Compiler::new()
            .model(model)
            .compile_str(source, &format!("{model}.mo"))
            .unwrap_or_else(|error| panic!("{model} compiles: {error:?}"));
        let expected = native_snapshot(&compiled, model);
        let files = rumoca::render_target_files(&compiled, model, target, None)
            .unwrap_or_else(|error| panic!("{model} renders {target}: {error:#}"));
        let module = files
            .iter()
            .find(|file| file.path.ends_with(".py"))
            .expect("Python module");
        let work = tempfile::tempdir().unwrap();
        let module_path = work.path().join(&module.path);
        fs::write(&module_path, &module.content).unwrap();
        let snapshot = serde_json::json!({ "parameters": expected.parameters, "initial": expected.initial_y[..expected.state_count], "probes": expected.probes, "direction": expected.first_state_direction });
        let snapshot_path = work.path().join("native.json");
        fs::write(&snapshot_path, serde_json::to_vec(&snapshot).unwrap()).unwrap();
        python_probe(
            &module_path,
            &snapshot_path,
            script,
            &format!("{model} {target}"),
        );
    }
}

fn python_probe(module: &Path, snapshot: &Path, script: &str, context: &str) {
    run(
        Command::new("python")
            .args(["-c", script])
            .arg(module)
            .arg(snapshot),
        context,
    );
}

const PYTHON_SETUP: &str = r#"
import importlib.util, json, sys, numpy as np
spec = importlib.util.spec_from_file_location('generated', sys.argv[1])
generated = importlib.util.module_from_spec(spec)
spec.loader.exec_module(generated)
expected = json.load(open(sys.argv[2]))
p = generated.init_params(generated.DEFAULT_P)
np.testing.assert_allclose(p, expected['parameters'][:generated.N_P], rtol=1e-8, atol=1e-8)
np.testing.assert_allclose(generated.init_state(p), expected['initial'], rtol=1e-8, atol=1e-8)
"#;

#[test]
fn jax_actual_workshop_examples_jit_initialize_and_differentiate() {
    python_examples(
        "jax-ode",
        &format!(
            r#"{PYTHON_SETUP}
import jax, jax.numpy as jnp
p = jnp.asarray(p)
for t, x, rhs in expected['probes']:
    actual = jax.jit(generated.rhs)(t, jnp.asarray(x), jnp.zeros(generated.N_U), p)
    np.testing.assert_allclose(actual, rhs, rtol=1e-8, atol=1e-8)
x = jnp.asarray(expected['initial'])
f = lambda value: generated.rhs(0.125, value, jnp.zeros(generated.N_U), p)
direction = jnp.zeros_like(x).at[0].set(1.0)
_, tangent = jax.jvp(f, (x,), (direction,))
np.testing.assert_allclose(tangent, expected['direction'], rtol=5e-5, atol=5e-5)
reverse = jax.grad(lambda value: jnp.sum(f(value)))(x)
np.testing.assert_allclose(reverse[0], sum(expected['direction']), rtol=5e-5, atol=5e-5)
"#
        ),
    );
}

#[test]
fn casadi_actual_workshop_examples_initialize_evaluate_and_differentiate() {
    python_examples(
        "casadi-ode",
        &format!(
            r#"{PYTHON_SETUP}
import casadi as ca
p = ca.DM(p)
for t, x, rhs in expected['probes']:
    actual = np.array(generated.rhs(t, ca.DM(x), ca.DM.zeros(generated.N_U), p)).reshape(-1)
    np.testing.assert_allclose(actual, rhs, rtol=1e-8, atol=1e-8)
x = ca.SX.sym('x', generated.N_Y)
f = generated.rhs(0.125, x, ca.SX.zeros(generated.N_U), p)
direction = ca.Function('direction', [x], [ca.jacobian(f, x)[:,0]])
actual = np.array(direction(ca.DM(expected['initial']))).reshape(-1)
np.testing.assert_allclose(actual, expected['direction'], rtol=5e-5, atol=5e-5)
"#
        ),
    );
}
