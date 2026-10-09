//! Sensitivities over a trajectory: the forward variational equations, the
//! adjoint gradient of a trajectory objective, and the state-space
//! linearization, each checked against closed forms derived by hand.

use rumoca::Compiler;
use rumoca_sim::{
    DataSeries, ObjectiveGradient, RunningKind, RunningTerm, SimOptions, SimResult, TerminalTerm,
    TrajectoryObjective,
};

fn compile(source: &str, model: &str) -> rumoca::CompilationResult {
    Compiler::new()
        .model(model)
        .compile_str(source, &format!("{model}.mo"))
        .unwrap_or_else(|error| panic!("{model} should compile: {error:?}"))
}

fn options(t_end: f64, dt: f64) -> SimOptions {
    SimOptions {
        t_end,
        dt: Some(dt),
        rtol: 1.0e-9,
        atol: 1.0e-11,
        ..SimOptions::default()
    }
}

fn column<'a>(result: &'a SimResult, name: &str) -> &'a [f64] {
    let index = result
        .names
        .iter()
        .position(|candidate| candidate == name)
        .unwrap_or_else(|| panic!("no column `{name}` in {:?}", result.names));
    &result.data[index]
}

fn assert_close(got: f64, want: f64, tolerance: f64, what: &str) {
    assert!(
        (got - want).abs() <= tolerance * (1.0 + want.abs()),
        "{what}: got {got}, want {want}"
    );
}

// ---------------------------------------------------------------------------
// x' = -a x + b, x(0) = 1.
//   x(t) = b/a + (1 - b/a) e^{-a t}
//   dx/db = (1 - e^{-a t}) / a
//   dx/da = -(1 - b/a) t e^{-a t} - (b / a^2) (1 - e^{-a t})
// ---------------------------------------------------------------------------

const FIRST_ORDER: &str = r#"
model FirstOrder
  parameter Real a = 2;
  parameter Real b = 1;
  Real x(start = 1, fixed = true);
equation
  der(x) = -a * x + b;
  annotation(experiment(StartTime = 0, StopTime = 1));
end FirstOrder;
"#;

const A: f64 = 2.0;
const B: f64 = 1.0;

fn first_order_dx_da(t: f64) -> f64 {
    let decay = (-A * t).exp();
    -(1.0 - B / A) * t * decay - (B / (A * A)) * (1.0 - decay)
}

fn first_order_dx_db(t: f64) -> f64 {
    (1.0 - (-A * t).exp()) / A
}

#[test]
fn first_order_forward_sensitivity_matches_the_closed_form() {
    let result = compile(FIRST_ORDER, "FirstOrder");
    let trace = rumoca_sim::trajectory_sensitivity_for_dae(&result.dae, &options(1.0, 0.125), &[])
        .expect("forward sensitivity");
    let dx_da = column(&trace, "d(x)/d(a)");
    let dx_db = column(&trace, "d(x)/d(b)");
    assert_eq!(trace.times.len(), 9);
    assert_eq!(dx_da[0], 0.0, "x(0) does not depend on a");
    for (index, t) in trace.times.iter().enumerate() {
        assert_close(dx_da[index], first_order_dx_da(*t), 1.0e-7, "dx/da");
        assert_close(dx_db[index], first_order_dx_db(*t), 1.0e-7, "dx/db");
    }
}

#[test]
fn a_named_parameter_set_selects_the_columns() {
    let result = compile(FIRST_ORDER, "FirstOrder");
    let wrt = ["b".to_string()];
    let trace = rumoca_sim::trajectory_sensitivity_for_dae(&result.dae, &options(1.0, 0.5), &wrt)
        .expect("forward sensitivity");
    assert!(trace.names.contains(&"d(x)/d(b)".to_string()));
    assert!(!trace.names.contains(&"d(x)/d(a)".to_string()));
}

#[test]
fn an_unknown_parameter_is_refused() {
    let result = compile(FIRST_ORDER, "FirstOrder");
    let wrt = ["nope".to_string()];
    let error = rumoca_sim::trajectory_sensitivity_for_dae(&result.dae, &options(1.0, 0.5), &wrt)
        .expect_err("`nope` is not a parameter");
    assert_eq!(error.diagnostic_code(), "EX003");
    assert!(error.to_string().contains("nope"), "{error}");
}

// J = integral_0^T x^2 dt with x = c + d e^{-a t}, c = b/a, d = 1 - b/a:
//   J = c^2 T + 2 c d (1 - E1)/a + d^2 (1 - E2)/(2a),  E1 = e^{-aT}, E2 = e^{-2aT}
//   dJ/dc = 2 c T + 2 d (1 - E1)/a        dJ/dd = 2 c (1 - E1)/a + d (1 - E2)/a
//   dJ/db = (dJ/dc - dJ/dd) / a
//   dJ/da = (b/a^2)(dJ/dd - dJ/dc)
//           + 2 c d [T E1/a - (1 - E1)/a^2] + d^2 [T E2/a - (1 - E2)/(2 a^2)]
fn first_order_objective(t_end: f64) -> (f64, f64, f64) {
    let (c, d) = (B / A, 1.0 - B / A);
    let (e1, e2) = ((-A * t_end).exp(), (-2.0 * A * t_end).exp());
    let value = c * c * t_end + 2.0 * c * d * (1.0 - e1) / A + d * d * (1.0 - e2) / (2.0 * A);
    let dj_dc = 2.0 * c * t_end + 2.0 * d * (1.0 - e1) / A;
    let dj_dd = 2.0 * c * (1.0 - e1) / A + d * (1.0 - e2) / A;
    let dj_db = (dj_dc - dj_dd) / A;
    let dj_da = (B / (A * A)) * (dj_dd - dj_dc)
        + 2.0 * c * d * (t_end * e1 / A - (1.0 - e1) / (A * A))
        + d * d * (t_end * e2 / A - (1.0 - e2) / (2.0 * A * A));
    (value, dj_da, dj_db)
}

/// `J = integral x^2 dt` as a least-squares fit to zero data.
fn squared_x_objective(t_end: f64) -> TrajectoryObjective {
    let zero = DataSeries::new(vec![0.0, t_end], vec![0.0, 0.0]).expect("two samples");
    TrajectoryObjective {
        running: vec![RunningTerm {
            variable: "x".to_string(),
            weight: 1.0,
            kind: RunningKind::SquaredError(zero),
        }],
        terminal: Vec::new(),
    }
}

fn gradient_of(gradient: &ObjectiveGradient, name: &str) -> f64 {
    let index = gradient
        .parameters
        .iter()
        .position(|candidate| candidate == name)
        .unwrap_or_else(|| panic!("no parameter `{name}` in {:?}", gradient.parameters));
    gradient.gradient[index]
}

#[test]
fn forward_and_adjoint_gradients_match_the_closed_form_of_the_integral_of_x_squared() {
    let result = compile(FIRST_ORDER, "FirstOrder");
    let opts = options(1.0, 0.25);
    let (value, dj_da, dj_db) = first_order_objective(1.0);
    for adjoint in [false, true] {
        let gradient = rumoca_sim::trajectory_objective_gradient_for_dae(
            &result.dae,
            &opts,
            &[],
            &squared_x_objective(1.0),
            adjoint,
        )
        .unwrap_or_else(|error| panic!("adjoint={adjoint}: {error}"));
        let mode = if adjoint { "adjoint" } else { "forward" };
        assert_close(gradient.value, value, 1.0e-6, &format!("{mode} J"));
        assert_close(
            gradient_of(&gradient, "a"),
            dj_da,
            1.0e-5,
            &format!("{mode} dJ/da"),
        );
        assert_close(
            gradient_of(&gradient, "b"),
            dj_db,
            1.0e-5,
            &format!("{mode} dJ/db"),
        );
    }
}

const QUADRATURE: &str = r#"
model Quadrature
  parameter Real a = 2;
  parameter Real b = 1;
  Real x(start = 1, fixed = true);
  Real J(start = 0, fixed = true);
equation
  der(x) = -a * x + b;
  der(J) = x * x;
end Quadrature;
"#;

#[test]
fn a_terminal_quadrature_state_gives_the_same_gradient_as_the_running_integral() {
    let result = compile(QUADRATURE, "Quadrature");
    let objective = TrajectoryObjective {
        running: Vec::new(),
        terminal: vec![TerminalTerm {
            variable: "J".to_string(),
            weight: 1.0,
        }],
    };
    let (value, dj_da, dj_db) = first_order_objective(1.0);
    for adjoint in [false, true] {
        let gradient = rumoca_sim::trajectory_objective_gradient_for_dae(
            &result.dae,
            &options(1.0, 0.25),
            &["a".to_string(), "b".to_string()],
            &objective,
            adjoint,
        )
        .expect("terminal gradient");
        assert_close(gradient.value, value, 1.0e-6, "J(T)");
        assert_close(gradient_of(&gradient, "a"), dj_da, 1.0e-5, "dJ/da");
        assert_close(gradient_of(&gradient, "b"), dj_db, 1.0e-5, "dJ/db");
    }
}

// ---------------------------------------------------------------------------
// x' = v, v' = -k x - c v, x(0) = 1, v(0) = 0, underdamped (k = 4, c = 0.6).
//   zeta = c/2, w = sqrt(k - zeta^2)
//   x = e^{-zeta t} (cos w t + (zeta/w) sin w t)
//   dx/dw|zeta = e^{-zeta t} [ -t sin w t - (zeta/w^2) sin w t + (zeta/w) t cos w t ]
//   dx/dzeta|w = -t x + e^{-zeta t} sin(w t) / w
//   dx/dk = dx/dw / (2 w)       dx/dc = (dx/dzeta - (zeta/w) dx/dw) / 2
// ---------------------------------------------------------------------------

const OSCILLATOR: &str = r#"
model Oscillator
  parameter Real k = 4;
  parameter Real c = 0.6;
  Real x(start = 1, fixed = true);
  Real v(start = 0, fixed = true);
equation
  der(x) = v;
  der(v) = -k * x - c * v;
  annotation(experiment(StartTime = 0, StopTime = 2));
end Oscillator;
"#;

fn oscillator_state(t: f64) -> (f64, f64, f64) {
    let (k, c) = (4.0_f64, 0.6_f64);
    let zeta = c / 2.0;
    let w = (k - zeta * zeta).sqrt();
    let decay = (-zeta * t).exp();
    let (sin, cos) = (w * t).sin_cos();
    let x = decay * (cos + zeta / w * sin);
    let dx_dw = decay * (-t * sin - zeta / (w * w) * sin + zeta / w * t * cos);
    let dx_dzeta = -t * x + decay * sin / w;
    (x, dx_dw / (2.0 * w), (dx_dzeta - zeta / w * dx_dw) / 2.0)
}

#[test]
fn two_parameter_oscillator_sensitivities_match_the_closed_form() {
    let result = compile(OSCILLATOR, "Oscillator");
    let trace = rumoca_sim::trajectory_sensitivity_for_dae(&result.dae, &options(2.0, 0.25), &[])
        .expect("oscillator sensitivity");
    let (x, dx_dk, dx_dc) = (
        column(&trace, "x"),
        column(&trace, "d(x)/d(k)"),
        column(&trace, "d(x)/d(c)"),
    );
    for (index, t) in trace.times.iter().enumerate() {
        let (want_x, want_k, want_c) = oscillator_state(*t);
        assert_close(x[index], want_x, 1.0e-7, "x");
        assert_close(dx_dk[index], want_k, 1.0e-6, "dx/dk");
        assert_close(dx_dc[index], want_c, 1.0e-6, "dx/dc");
    }
}

#[test]
fn oscillator_adjoint_agrees_with_forward_for_a_terminal_position_objective() {
    let result = compile(OSCILLATOR, "Oscillator");
    let objective = TrajectoryObjective {
        running: Vec::new(),
        terminal: vec![TerminalTerm {
            variable: "x".to_string(),
            weight: 1.0,
        }],
    };
    let (_, want_k, want_c) = oscillator_state(2.0);
    for adjoint in [false, true] {
        let gradient = rumoca_sim::trajectory_objective_gradient_for_dae(
            &result.dae,
            &options(2.0, 0.25),
            &[],
            &objective,
            adjoint,
        )
        .expect("terminal gradient");
        assert_close(gradient_of(&gradient, "k"), want_k, 1.0e-5, "dx(T)/dk");
        assert_close(gradient_of(&gradient, "c"), want_c, 1.0e-5, "dx(T)/dc");
    }
}

// ---------------------------------------------------------------------------
// x' = -a x + z1 with the algebraic loop z1 - z2 = x, z1 + z2 = b x, so
// z1 = (1 + b) x / 2 and x' = r x with r = (1 + b)/2 - a, x = e^{r t}.
//   dx/da = -t e^{r t}      dx/db = (t/2) e^{r t}
//   dz1/db = x/2 + (1 + b)/2 dx/db
// ---------------------------------------------------------------------------

const ALGEBRAIC_LOOP: &str = r#"
model AlgebraicLoop
  parameter Real a = 2;
  parameter Real b = 1;
  Real x(start = 1, fixed = true);
  Real z1;
  Real z2;
equation
  der(x) = -a * x + z1;
  z1 - z2 = x;
  z1 + z2 = b * x;
  annotation(experiment(StartTime = 0, StopTime = 1));
end AlgebraicLoop;
"#;

#[test]
fn an_algebraic_loop_carries_sensitivities_through_the_projection() {
    let result = compile(ALGEBRAIC_LOOP, "AlgebraicLoop");
    let trace = rumoca_sim::trajectory_sensitivity_for_dae(&result.dae, &options(1.0, 0.25), &[])
        .expect("algebraic sensitivity");
    let rate = (1.0 + B) / 2.0 - A;
    let (dx_da, dx_db, dz1_db) = (
        column(&trace, "d(x)/d(a)"),
        column(&trace, "d(x)/d(b)"),
        column(&trace, "d(z1)/d(b)"),
    );
    for (index, t) in trace.times.iter().enumerate() {
        let x = (rate * t).exp();
        assert_close(dx_da[index], -t * x, 1.0e-7, "dx/da");
        assert_close(dx_db[index], 0.5 * t * x, 1.0e-7, "dx/db");
        assert_close(
            dz1_db[index],
            0.5 * x + (1.0 + B) / 2.0 * 0.5 * t * x,
            1.0e-7,
            "dz1/db",
        );
    }
}

#[test]
fn an_algebraic_loop_gives_equal_forward_and_adjoint_gradients() {
    let result = compile(ALGEBRAIC_LOOP, "AlgebraicLoop");
    // J = integral z1 dt + z1(T): an algebraic variable in both terms.
    let objective = TrajectoryObjective {
        running: vec![RunningTerm {
            variable: "z1".to_string(),
            weight: 1.0,
            kind: RunningKind::Value,
        }],
        terminal: vec![TerminalTerm {
            variable: "z1".to_string(),
            weight: 1.0,
        }],
    };
    let rate = (1.0 + B) / 2.0 - A;
    // z1 = (1 + b)/2 e^{r t}.  With r = (1 + b)/2 - a:
    //   int_0^T z1 dt = (1 + b)/(2 r) (e^{rT} - 1)
    //   dr/da = -1, dr/db = 1/2
    let t_end = 1.0_f64;
    let growth = (rate * t_end).exp();
    let integral = |b: f64, r: f64| (1.0 + b) / (2.0 * r) * ((r * t_end).exp() - 1.0);
    let terminal = |b: f64, r: f64| (1.0 + b) / 2.0 * (r * t_end).exp();
    let total = |b: f64, r: f64| integral(b, r) + terminal(b, r);
    let d_integral_dr = -(1.0 + B) / (2.0 * rate * rate) * (growth - 1.0)
        + (1.0 + B) / (2.0 * rate) * t_end * growth;
    let d_terminal_dr = (1.0 + B) / 2.0 * t_end * growth;
    let d_integral_db = 1.0 / (2.0 * rate) * (growth - 1.0);
    let d_terminal_db = 0.5 * growth;
    let want_da = -(d_integral_dr + d_terminal_dr);
    let want_db = 0.5 * (d_integral_dr + d_terminal_dr) + d_integral_db + d_terminal_db;
    for adjoint in [false, true] {
        let gradient = rumoca_sim::trajectory_objective_gradient_for_dae(
            &result.dae,
            &options(t_end, 0.25),
            &[],
            &objective,
            adjoint,
        )
        .unwrap_or_else(|error| panic!("adjoint={adjoint}: {error}"));
        assert_close(gradient.value, total(B, rate), 1.0e-6, "J");
        assert_close(gradient_of(&gradient, "a"), want_da, 1.0e-5, "dJ/da");
        assert_close(gradient_of(&gradient, "b"), want_db, 1.0e-5, "dJ/db");
    }
}

// ---------------------------------------------------------------------------
// Hybrid models are outside the variational equations and refused.
// ---------------------------------------------------------------------------

const HYBRID: &str = r#"
model Hybrid
  parameter Real k = 1;
  Real x(start = 0, fixed = true);
equation
  der(x) = if x < 0.5 then k else -k;
  annotation(experiment(StartTime = 0, StopTime = 1));
end Hybrid;
"#;

#[test]
fn a_model_with_event_roots_is_refused_instead_of_differentiated_wrongly() {
    let result = compile(HYBRID, "Hybrid");
    let error = rumoca_sim::trajectory_sensitivity_for_dae(&result.dae, &options(1.0, 0.25), &[])
        .expect_err("hybrid model");
    assert_eq!(error.diagnostic_code(), "EX002");
    assert!(error.to_string().contains("event"), "{error}");
}

// ---------------------------------------------------------------------------
// x' = -a x with x(0) = x0 fixed by the initialization: x = x0 e^{-a t}
//   dx/dx0 = e^{-a t}        dx/da = -x0 t e^{-a t}
// The initial sensitivity comes from the initialization's update row.
// ---------------------------------------------------------------------------

const PARAMETER_START: &str = r#"
model ParameterStart
  parameter Real x0 = 3;
  parameter Real a = 1;
  Real x(start = x0, fixed = true);
equation
  der(x) = -a * x;
end ParameterStart;
"#;

#[test]
fn a_parameter_that_fixes_the_initial_state_has_a_nonzero_initial_sensitivity() {
    let result = compile(PARAMETER_START, "ParameterStart");
    let trace = rumoca_sim::trajectory_sensitivity_for_dae(&result.dae, &options(1.0, 0.5), &[])
        .expect("initial sensitivity");
    let (dx_dx0, dx_da) = (column(&trace, "d(x)/d(x0)"), column(&trace, "d(x)/d(a)"));
    assert_close(dx_dx0[0], 1.0, 1.0e-9, "dx(0)/dx0");
    assert_eq!(dx_da[0], 0.0, "x(0) does not depend on a");
    for (index, t) in trace.times.iter().enumerate() {
        assert_close(dx_dx0[index], (-t).exp(), 1.0e-7, "dx/dx0");
        assert_close(dx_da[index], -3.0 * t * (-t).exp(), 1.0e-7, "dx/da");
    }
}

// x' = -x0 x with x(0) = x0 fixed: x = x0 e^{-x0 t}, dx/dx0 = e^{-x0 t} (1 - x0 t).
// Both the derivative rows and the initialization read x0.
const PARAMETER_IN_BOTH: &str = r#"
model ParameterInBoth
  parameter Real x0 = 3;
  Real x(start = x0, fixed = true);
equation
  der(x) = -x0 * x;
end ParameterInBoth;
"#;

#[test]
fn a_parameter_read_by_the_rows_and_the_initialization_gets_both_contributions() {
    let result = compile(PARAMETER_IN_BOTH, "ParameterInBoth");
    let trace = rumoca_sim::trajectory_sensitivity_for_dae(&result.dae, &options(1.0, 0.5), &[])
        .expect("combined sensitivity");
    let dx = column(&trace, "d(x)/d(x0)");
    for (index, t) in trace.times.iter().enumerate() {
        let want = (-3.0 * t).exp() * (1.0 - 3.0 * t);
        assert_close(dx[index], want, 1.0e-7, "dx/dx0");
    }
}

// Without `fixed = true` the start is a constant of the lowered model, which
// would not follow the parameter: refused rather than answered with a zero.
const BAKED_START: &str = r#"
model BakedStart
  parameter Real x0 = 3;
  Real x(start = x0);
equation
  der(x) = -x0 * x;
end BakedStart;
"#;

#[test]
fn a_parameter_baked_into_a_start_constant_is_refused_never_given_a_zero() {
    let result = compile(BAKED_START, "BakedStart");
    let error = rumoca_sim::trajectory_sensitivity_for_dae(&result.dae, &options(1.0, 0.5), &[])
        .expect_err("baked start");
    assert_eq!(error.diagnostic_code(), "EX003");
    assert!(error.to_string().contains("fixed = true"), "{error}");
}

// ---------------------------------------------------------------------------
// Linearization: x' = -a x + b u, y = c x + d u.
// ---------------------------------------------------------------------------

const STATE_SPACE: &str = r#"
model StateSpace
  parameter Real a = 2;
  parameter Real b = 3;
  parameter Real c = 5;
  parameter Real d = 7;
  input Real u = 0.5;
  output Real y;
  Real x(start = 1, fixed = true);
equation
  der(x) = -a * x + b * u;
  y = c * x + d * u;
end StateSpace;
"#;

#[test]
fn the_linearization_reports_a_b_c_and_d() {
    let result = compile(STATE_SPACE, "StateSpace");
    let linearization =
        rumoca_sim::linearization_for_dae(&result.dae, &SimOptions::default(), &[], 0.0)
            .expect("linearization");
    assert_eq!(linearization.states, ["x"]);
    assert_eq!(linearization.inputs, ["u"]);
    assert_eq!(linearization.outputs, ["y"]);
    assert_close(linearization.a[0][0], -2.0, 1.0e-12, "A");
    assert_close(linearization.b[0][0], 3.0, 1.0e-12, "B");
    assert_close(linearization.c[0][0], 5.0, 1.0e-12, "C");
    assert_close(linearization.d[0][0], 7.0, 1.0e-12, "D");
}

// ---------------------------------------------------------------------------
// Requests the construction refuses with a typed error.
// ---------------------------------------------------------------------------

#[test]
fn a_horizon_of_no_length_is_refused() {
    let result = compile(FIRST_ORDER, "FirstOrder");
    let error = rumoca_sim::trajectory_sensitivity_for_dae(&result.dae, &options(0.0, 0.5), &[])
        .expect_err("zero-length horizon");
    assert_eq!(error.diagnostic_code(), "EX003");
    assert!(error.to_string().contains("positive length"), "{error}");
}

#[test]
fn an_objective_over_something_that_is_not_a_solver_variable_is_refused() {
    let result = compile(FIRST_ORDER, "FirstOrder");
    for (objective, needle) in [
        (
            TrajectoryObjective {
                running: Vec::new(),
                terminal: vec![TerminalTerm {
                    variable: "a".to_string(),
                    weight: 1.0,
                }],
            },
            "not a solver variable",
        ),
        (TrajectoryObjective::default(), "no term"),
        (
            TrajectoryObjective {
                running: vec![RunningTerm {
                    variable: "x".to_string(),
                    weight: f64::NAN,
                    kind: RunningKind::Value,
                }],
                terminal: Vec::new(),
            },
            "weight",
        ),
    ] {
        let error = rumoca_sim::trajectory_objective_gradient_for_dae(
            &result.dae,
            &options(1.0, 0.5),
            &[],
            &objective,
            true,
        )
        .expect_err(needle);
        assert_eq!(error.diagnostic_code(), "EX003", "{error}");
        assert!(error.to_string().contains(needle), "{needle}: {error}");
    }
}

#[test]
fn measured_data_must_cover_the_horizon() {
    let result = compile(FIRST_ORDER, "FirstOrder");
    let short = DataSeries::new(vec![0.0, 0.5], vec![0.0, 0.0]).expect("two samples");
    let objective = TrajectoryObjective {
        running: vec![RunningTerm {
            variable: "x".to_string(),
            weight: 1.0,
            kind: RunningKind::SquaredError(short),
        }],
        terminal: Vec::new(),
    };
    let error = rumoca_sim::trajectory_objective_gradient_for_dae(
        &result.dae,
        &options(1.0, 0.5),
        &[],
        &objective,
        false,
    )
    .expect_err("data end before the horizon");
    assert!(error.to_string().contains("span"), "{error}");
}

#[test]
fn a_data_series_is_checked_at_construction() {
    assert!(DataSeries::new(vec![0.0], vec![1.0]).is_err());
    assert!(DataSeries::new(vec![0.0, 1.0], vec![1.0]).is_err());
    assert!(DataSeries::new(vec![0.0, 0.0], vec![1.0, 2.0]).is_err());
    assert!(DataSeries::new(vec![0.0, 1.0], vec![1.0, f64::NAN]).is_err());
    let series = DataSeries::new(vec![0.0, 1.0, 3.0], vec![0.0, 2.0, 6.0]).expect("valid");
    assert_eq!((series.first_time(), series.last_time()), (0.0, 3.0));
    assert_close(series.value_at(0.5), 1.0, 1.0e-12, "first segment");
    assert_close(series.value_at(2.0), 4.0, 1.0e-12, "second segment");
    assert_close(series.value_at(3.0), 6.0, 1.0e-12, "last sample");
}

// A trajectory that is too long to store for the adjoint is refused by size.
#[test]
fn a_model_with_events_that_reset_a_state_is_refused() {
    let source = r#"
model Reset
  parameter Real k = 1;
  Real x(start = 1, fixed = true);
equation
  der(x) = -k * x;
  when x < 0.5 then
    reinit(x, 1);
  end when;
end Reset;
"#;
    let result = compile(source, "Reset");
    let error = rumoca_sim::trajectory_sensitivity_for_dae(&result.dae, &options(2.0, 0.5), &[])
        .expect_err("a state reset is an event the variational equations do not cover");
    assert_eq!(error.diagnostic_code(), "EX002");
    assert!(error.to_string().contains("event"), "{error}");
}

// Event machinery that cannot change the trajectory is admitted: the
// resistor and capacitor of the Modelica Standard Library carry assertions
// whose conditions read the solver vector.
#[test]
fn an_assertion_is_inert_and_does_not_block_the_sensitivities() {
    let source = r#"
model Asserted
  parameter Real k = 1;
  Real x(start = 1, fixed = true);
equation
  der(x) = -k * x;
  assert(x > -1, "x left its range");
end Asserted;
"#;
    let result = compile(source, "Asserted");
    let trace = rumoca_sim::trajectory_sensitivity_for_dae(&result.dae, &options(1.0, 0.5), &[])
        .expect("an assertion owns no value the continuous rows read");
    // x = e^{-k t}, dx/dk = -t e^{-k t}.
    let dx = column(&trace, "d(x)/d(k)");
    assert_close(dx[2], -(-1.0_f64).exp(), 1.0e-7, "dx/dk(1)");
}

// ---------------------------------------------------------------------------
// Data knots are step ends: a feature narrower than a step is integrated.
//   x' = -2 x + 1, x(0) = 1/2 sits at equilibrium (huge steps), and the data
//   is 1/2 with a triangle of half-width h = 0.002 and height 2 at t = 1.
//   J = int (x - d)^2 dt = 2 int_0^h (2 s / h)^2 ds = 8 h / 3
//   S = dx/da = -(x / a)(1 - e^{-a t}), dJ/da = 2 int (x - d) S dt
//             = 2 * (area 2 h) * (-S(1)) = 4 h * 0.25 (1 - e^{-2})
// ---------------------------------------------------------------------------

const EQUILIBRIUM: &str = r#"
model Equilibrium
  parameter Real a = 2;
  parameter Real b = 1;
  Real x(start = 0.5, fixed = true);
equation
  der(x) = -a * x + b;
end Equilibrium;
"#;

fn spike(half_width: f64) -> TrajectoryObjective {
    let times = vec![0.0, 1.0 - half_width, 1.0, 1.0 + half_width, 2.0];
    let values = vec![0.5, 0.5, 2.5, 0.5, 0.5];
    TrajectoryObjective {
        running: vec![RunningTerm {
            variable: "x".to_string(),
            weight: 1.0,
            kind: RunningKind::SquaredError(DataSeries::new(times, values).expect("data")),
        }],
        terminal: Vec::new(),
    }
}

#[test]
fn a_data_feature_narrower_than_a_step_is_integrated_by_forward_and_adjoint() {
    let result = compile(EQUILIBRIUM, "Equilibrium");
    for half_width in [0.002, 0.05] {
        let value = 8.0 * half_width / 3.0;
        let dj_da = 4.0 * half_width * 0.25 * (1.0 - (-2.0_f64).exp());
        for adjoint in [false, true] {
            let gradient = rumoca_sim::trajectory_objective_gradient_for_dae(
                &result.dae,
                &options(2.0, 0.5),
                &["a".to_string()],
                &spike(half_width),
                adjoint,
            )
            .unwrap_or_else(|error| panic!("h={half_width} adjoint={adjoint}: {error}"));
            assert_close(gradient.value, value, 1.0e-4, "J");
            // The spike's S varies over its width, so the gradient is checked
            // against the constant-S value to second order in the width.
            assert_close(gradient_of(&gradient, "a"), dj_da, 2.0e-2, "dJ/da");
        }
    }
}

// ---------------------------------------------------------------------------
// noEvent over a discontinuous right-hand side: no one-sided sensitivity.
// ---------------------------------------------------------------------------

const NO_EVENT_STATE: &str = r#"
model NoEventState
  parameter Real a = 1;
  parameter Real th = 0.6;
  Real x(start = 1, fixed = true);
equation
  der(x) = noEvent(if x > th then -a * x else -0.2 * a * x);
end NoEventState;
"#;

#[test]
fn a_no_event_relation_of_a_state_is_refused_and_names_the_relation() {
    let result = compile(NO_EVENT_STATE, "NoEventState");
    let error = rumoca_sim::trajectory_sensitivity_for_dae(&result.dae, &options(2.0, 0.5), &[])
        .expect_err("a switching surface in the right-hand side");
    assert_eq!(error.diagnostic_code(), "EX002");
    assert!(error.source_span().is_some(), "the refusal names a span");
    assert!(
        error.to_string().contains("one-sided sensitivities"),
        "{error}"
    );
}

const NO_EVENT_PARAMETER: &str = r#"
model NoEventParameter
  parameter Real a = 1;
  parameter Real th = 1;
  Real x(start = 1, fixed = true);
equation
  der(x) = noEvent(if th > 0.5 then -a * x else -2 * a * x);
end NoEventParameter;
"#;

#[test]
fn a_no_event_relation_of_an_undifferentiated_parameter_is_admitted() {
    let result = compile(NO_EVENT_PARAMETER, "NoEventParameter");
    let wrt = ["a".to_string()];
    let trace = rumoca_sim::trajectory_sensitivity_for_dae(&result.dae, &options(1.0, 0.5), &wrt)
        .expect("the relation reads only `th`, which is not differentiated");
    // th = 1 selects x' = -a x: dx/da = -t e^{-a t}.
    let dx = column(&trace, "d(x)/d(a)");
    assert_close(dx[2], -(-1.0_f64).exp(), 1.0e-7, "dx/da(1)");
    // The relation is constant along the trajectory even when `th` is
    // requested: its sensitivity is that of the active branch, which `th`
    // does not enter.
    let both = ["a".to_string(), "th".to_string()];
    let trace = rumoca_sim::trajectory_sensitivity_for_dae(&result.dae, &options(1.0, 0.5), &both)
        .expect("a relation of parameters alone is admitted");
    assert!(
        column(&trace, "d(x)/d(th)")
            .iter()
            .all(|value| *value == 0.0)
    );
}

const NO_EVENT_TIME: &str = r#"
model NoEventTime
  parameter Real a = 1;
  parameter Real ts = 0.5;
  Real x(start = 1, fixed = true);
equation
  der(x) = noEvent(if time > ts then -2 * a * x else -a * x);
end NoEventTime;
"#;

#[test]
fn a_no_event_relation_of_time_against_a_requested_parameter_is_refused() {
    let result = compile(NO_EVENT_TIME, "NoEventTime");
    // The switching instant moves with `ts`: a jump term the construction does
    // not state.
    let error = rumoca_sim::trajectory_sensitivity_for_dae(&result.dae, &options(1.0, 0.5), &[])
        .expect_err("the switching instant depends on `ts`");
    assert_eq!(error.diagnostic_code(), "EX002");
    assert!(error.source_span().is_some());
    // Without `ts` in the request the instant is fixed and the model is admitted.
    let only_a = ["a".to_string()];
    rumoca_sim::trajectory_sensitivity_for_dae(&result.dae, &options(1.0, 0.5), &only_a)
        .expect("a fixed switching time");
}

// ---------------------------------------------------------------------------
// The checkpoint budget and the parameter classification.
// ---------------------------------------------------------------------------

#[test]
fn the_adjoint_refuses_a_forward_path_above_the_checkpoint_budget() {
    let result = compile(FIRST_ORDER, "FirstOrder");
    let tight = SimOptions {
        checkpoint_budget_bytes: 64,
        ..options(1.0, 0.25)
    };
    let error = rumoca_sim::trajectory_objective_gradient_for_dae(
        &result.dae,
        &tight,
        &[],
        &squared_x_objective(1.0),
        true,
    )
    .expect_err("the path does not fit 64 bytes");
    assert_eq!(error.diagnostic_code(), "EX002");
    assert!(error.to_string().contains("budget"), "{error}");
    // The forward gradient stores no path and is not subject to the budget.
    rumoca_sim::trajectory_objective_gradient_for_dae(
        &result.dae,
        &tight,
        &[],
        &squared_x_objective(1.0),
        false,
    )
    .expect("forward gradient");
}

const CLASSIFIED: &str = r#"
model Classified
  parameter Real a = 2;
  parameter Real c = 4;
  parameter Real d = 2 * c;
  parameter Real unused = 5;
  final parameter Real f = 3;
  parameter Integer n = 2;
  Real x(start = 1, fixed = true);
equation
  der(x) = -a * x - d * x * 0.01;
end Classified;
"#;

#[test]
fn a_default_request_lists_the_parameters_it_leaves_out_and_why() {
    use rumoca_sim::{ExclusionReason, TrajectorySession};
    let result = compile(CLASSIFIED, "Classified");
    let session = TrajectorySession::new(&result.dae, &options(1.0, 0.5), &[]).expect("session");
    assert_eq!(session.parameter_names(), ["a"]);
    let reason = |name: &str| {
        session
            .excluded()
            .iter()
            .find(|entry| entry.name == name)
            .unwrap_or_else(|| panic!("`{name}` is not listed: {:?}", session.excluded()))
            .reason
    };
    assert_eq!(reason("c"), ExclusionReason::ReadByParameters);
    assert_eq!(reason("d"), ExclusionReason::DependsOnParameters);
    assert_eq!(reason("f"), ExclusionReason::NotTunable);
    assert_eq!(reason("n"), ExclusionReason::NotReal);
    assert_eq!(reason("unused"), ExclusionReason::FoldedAtTranslation);
    // A named request that was excluded is refused with its reason.
    let error = TrajectorySession::new(&result.dae, &options(1.0, 0.5), &["d".to_string()])
        .err()
        .expect("`d` is dependent");
    assert!(
        error.to_string().contains("binding reads other parameters"),
        "{error}"
    );
}

#[test]
fn a_session_moves_to_a_new_parameter_point_without_lowering_again() {
    use rumoca_sim::TrajectorySession;
    let result = compile(FIRST_ORDER, "FirstOrder");
    let session = TrajectorySession::new(&result.dae, &options(1.0, 0.5), &[]).expect("session");
    let moved = session.with_parameter_values(&[3.0, 1.0]).expect("moved");
    let trace = moved.sensitivity().expect("trace");
    let x = column(&trace, "x");
    // a = 3, b = 1: x(1) = 1/3 + (2/3) e^{-3}.
    assert_close(
        x[2],
        1.0 / 3.0 + 2.0 / 3.0 * (-3.0_f64).exp(),
        1.0e-7,
        "x(1)",
    );
}

// ---------------------------------------------------------------------------
// An initialization that defines a parameter from a requested one.
// ---------------------------------------------------------------------------

const DEFINED_PARAMETER: &str = r#"
model DefinedParameter
  parameter Real a = 2;
  parameter Real b(fixed = false);
  Real x(start = 1, fixed = true);
initial equation
  b = 3 * a;
equation
  der(x) = -b * x;
end DefinedParameter;
"#;

#[test]
fn a_parameter_the_initialization_defines_from_a_requested_one_is_refused() {
    let result = compile(DEFINED_PARAMETER, "DefinedParameter");
    let error = rumoca_sim::trajectory_sensitivity_for_dae(
        &result.dae,
        &options(1.0, 0.5),
        &["a".to_string()],
    )
    .expect_err("b moves with a and the rows read b");
    assert_eq!(error.diagnostic_code(), "EX002");
    assert!(error.to_string().contains("`b`"), "{error}");
}

#[test]
fn a_requested_parameter_on_a_relation_switching_value_is_reported() {
    use rumoca_sim::TrajectorySession;
    let result = compile(NO_EVENT_PARAMETER, "NoEventParameter");
    let both = ["a".to_string(), "th".to_string()];
    let session = TrajectorySession::new(&result.dae, &options(1.0, 0.5), &both).expect("session");
    // th = 1 is away from the switching value 0.5.
    assert!(session.switching_value_notes().is_empty());
    let on_surface = session
        .with_parameter_values(&[1.0, 0.5])
        .expect("moved onto the switching value");
    let notes = on_surface.switching_value_notes();
    assert_eq!(notes.len(), 1, "{notes:?}");
    assert_eq!(notes[0].parameter, "th");
    assert_eq!(notes[0].value, 0.5);
    assert!(notes[0].span.is_some());
    // An unrequested parameter on its switching value is not one-sided.
    let only_a = ["a".to_string()];
    let session =
        TrajectorySession::new(&result.dae, &options(1.0, 0.5), &only_a).expect("session");
    let on_surface = session.with_parameter_values(&[1.0]).expect("moved");
    assert!(on_surface.switching_value_notes().is_empty());
}

#[test]
fn the_adjoint_refuses_a_bdf_plugin_that_declares_no_extension_order() {
    use rumoca_sim::{TrajectoryPlugin, TrajectorySession};
    let result = compile(FIRST_ORDER, "FirstOrder");
    let session = TrajectorySession::new(&result.dae, &options(1.0, 0.25), &[]).expect("session");
    let objective = squared_x_objective(1.0);
    // The Dormand-Prince plugin declares order four and the adjoint runs.
    session
        .gradient_with(&objective, true, TrajectoryPlugin::Rk45)
        .expect("rk45 declares its extension order");
    // BDF declares none: the checkpoint contract cannot be proved, so the
    // adjoint is refused before any step with the typed refusal.
    let error = session
        .gradient_with(&objective, true, TrajectoryPlugin::Bdf)
        .expect_err("BDF declares no continuous-extension order");
    let text = error.to_string();
    assert!(
        text.contains("plugin declares no continuous-extension order"),
        "{text}"
    );
}
