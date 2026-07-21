use super::*;

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

#[test]
#[cfg(feature = "template-runtime-tests")]
fn fmi3_cosimulation_preserves_landing_contact_events() {
    let compiled = compile_model(CONTACT_LANDING_SOURCE, "ContactLanding");
    let model_c = render_fmi_solve_template(&compiled, "fmi3", "model.c.jinja", "ContactLanding");
    let driver = r#"
int main(void) {
    fmi3Instance instance = fmi3InstantiateCoSimulation(
        "landing", MODEL_INSTANTIATION_TOKEN, NULL, 0, 0, 0, 0,
        NULL, 0, NULL, NULL, NULL);
    if (!instance) return 10;
    if (fmi3EnterInitializationMode(instance, 1, 1.0e-7, 0.0, 1, 2.0) != fmi3OK) return 11;
    if (fmi3ExitInitializationMode(instance) != fmi3OK) return 12;

    const fmi3ValueReference heightVr = 0;
    fmi3Float64 height = 0.0;
    fmi3Float64 minimumHeight = 1.0;
    fmi3Float64 time = 0.0;
    for (int step = 0; step < 400; step++) {
        fmi3Boolean eventNeeded = 0;
        fmi3Boolean terminate = 0;
        fmi3Boolean earlyReturn = 0;
        fmi3Float64 lastSuccessfulTime = time;
        if (fmi3DoStep(instance, time, 0.005, 1, &eventNeeded, &terminate,
                       &earlyReturn, &lastSuccessfulTime) != fmi3OK) return 13;
        if (eventNeeded || terminate || earlyReturn) return 14;
        time = lastSuccessfulTime;
        if (fmi3GetFloat64(instance, &heightVr, 1, &height, 1) != fmi3OK) return 15;
        if (height < minimumHeight) minimumHeight = height;
    }

    fmi3FreeInstance(instance);
    if (!(height > 0.085 && height < 0.1)) return 16;
    if (!(minimumHeight > 0.075)) return 17;
    return 0;
}
"#;
    let source = format!("{model_c}\n{driver}");
    compile_and_run_c(&[("contact_landing.c", &source)], &[]);
}

#[test]
#[cfg(feature = "template-runtime-tests")]
fn fmi3_cosimulation_detects_departure_from_an_initial_root() {
    let compiled = compile_model(CONTACT_AT_INITIAL_ROOT_SOURCE, "ContactAtInitialRoot");
    let model_c =
        render_fmi_solve_template(&compiled, "fmi3", "model.c.jinja", "ContactAtInitialRoot");
    let driver = r#"
int main(void) {
    fmi3Instance instance = fmi3InstantiateCoSimulation(
        "initial-root", MODEL_INSTANTIATION_TOKEN, NULL, 0, 0, 0, 0,
        NULL, 0, NULL, NULL, NULL);
    if (!instance) return 10;
    if (fmi3EnterInitializationMode(instance, 1, 1.0e-7, 0.0, 1, 1.0) != fmi3OK) return 11;
    if (fmi3ExitInitializationMode(instance) != fmi3OK) return 12;

    const fmi3ValueReference heightVr = 0;
    fmi3Float64 height = 0.0;
    fmi3Float64 minimumHeight = 1.0;
    fmi3Float64 time = 0.0;
    for (int step = 0; step < 200; step++) {
        fmi3Boolean eventNeeded = 0;
        fmi3Boolean terminate = 0;
        fmi3Boolean earlyReturn = 0;
        fmi3Float64 lastSuccessfulTime = time;
        if (fmi3DoStep(instance, time, 0.005, 1, &eventNeeded, &terminate,
                       &earlyReturn, &lastSuccessfulTime) != fmi3OK) return 13;
        if (eventNeeded || terminate || earlyReturn) return 14;
        time = lastSuccessfulTime;
        if (fmi3GetFloat64(instance, &heightVr, 1, &height, 1) != fmi3OK) return 15;
        if (height < minimumHeight) minimumHeight = height;
    }

    fmi3FreeInstance(instance);
    if (!(height > 0.085 && height < 0.1)) return 16;
    if (!(minimumHeight > 0.075)) return 17;
    return 0;
}
"#;
    let source = format!("{model_c}\n{driver}");
    compile_and_run_c(&[("contact_at_initial_root.c", &source)], &[]);
}

// ============================================================================
// FMI 3.0 — Fixed parameters in XML
//
// Verify that ordinary non-tunable parameters remain fixed parameters and are
// not mislabeled as structural parameters, while tunable parameters stay tunable.
// ============================================================================

#[test]
fn fmi3_fixed_parameter_xml() {
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
    // n (Integer, non-tunable) is an ordinary fixed parameter.
    assert!(
        xml.contains(r#"name="n" valueReference="2" causality="parameter" variability="fixed""#),
        "expected fixed parameter for n:\n{xml}"
    );
    assert!(!xml.contains("structuralParameter"), "{xml}");
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
