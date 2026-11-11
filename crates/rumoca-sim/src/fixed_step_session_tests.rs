//! The `fixed-step` solver selected by name with explicit options, through
//! the batch entry and the incremental session.
//!
//! The model is the facade's `Observed` decay `der(x) = -x`, `x(0) = 1`, whose
//! solution is `exp(-t)`. With `h = 1/32` on `[0, 1]` the three methods leave
//! `h/2`, `h^2/6` and `h^4/120` times `exp(-1)` at the end (5.8e-3, 6.0e-5 and
//! 2.9e-9); the stated tolerances are those plus the continuous extension's
//! own error at the outputs that fall inside a step.

use crate::numerical_observation_tests::model;
use crate::{
    BackendOptions, BackendOptionsError, NumericalBackend, SimExecutionPolicy, SimOptions,
    SimSolverMode, SimulationDiagnosticError, SimulationSession, fixed_step_backend_options,
    reset_step_counts, simulate_with_diagnostics, step_counts,
};

const STEP: f64 = 0.03125;
/// Order and the stated absolute tolerance against `exp(-t)` on `[0, 1]`.
const ORDERS: [(u32, f64); 3] = [(1, 7.0e-3), (2, 1.0e-4), (4, 2.0e-8)];

fn options(solver: &str, backend_options: Option<BackendOptions>) -> SimOptions {
    let name = rumoca_core::canonical_solver_name(solver).expect("a solver this tree runs");
    SimOptions {
        solver_mode: SimSolverMode::from_external_name(name),
        backend_options,
        execution_policy: SimExecutionPolicy::Interpreter,
        t_end: 1.0,
        dt: Some(0.1),
        ..SimOptions::default()
    }
}

/// The largest `|x - exp(-t)|` over every output row of one batch run, and
/// the accepted steps it took.
fn batch_error(opts: &SimOptions) -> (f64, u64) {
    reset_step_counts();
    let result = simulate_with_diagnostics(&model(), opts).expect("the decay simulates");
    let counts = step_counts();
    assert_eq!(counts.integrator, Some("fixed-step"));
    let x = result
        .names
        .iter()
        .position(|name| name == "x")
        .expect("the state is recorded");
    assert_eq!(result.times.len(), 11, "{:?}", result.times);
    let worst = result
        .times
        .iter()
        .zip(&result.data[x])
        .map(|(time, value)| (value - (-time).exp()).abs())
        .fold(0.0_f64, f64::max);
    (worst, counts.solver_steps)
}

#[test]
fn the_named_solver_tracks_the_analytic_decay_at_each_order() {
    let mut errors = Vec::new();
    for (order, tolerance) in ORDERS {
        let opts = options("Fixed_Step", Some(fixed_step_backend_options(STEP, order)));
        assert_eq!(opts.solver_mode, SimSolverMode::FixedStep);
        let (worst, steps) = batch_error(&opts);
        // Outputs every 0.1 s are sampled, never landed on: 32 steps of 1/32.
        assert_eq!(steps, 32, "order {order}");
        assert!(worst <= tolerance, "order {order}: error {worst:e}");
        errors.push(worst);
    }
    // Each order ran its own method, not a shared default.
    assert!(errors[0] > 10.0 * errors[1] && errors[1] > 10.0 * errors[2]);
}

#[test]
fn the_named_solver_without_options_runs_its_default_configuration() {
    let opts = SimOptions {
        t_end: 0.25,
        ..options("fixed-step", None)
    };
    reset_step_counts();
    let result = simulate_with_diagnostics(&model(), &opts).expect("the decay simulates");
    // The default is a 1 ms classical step.
    assert_eq!(step_counts().solver_steps, 250);
    let x = result.names.iter().position(|name| name == "x").unwrap();
    let last = result.times.len() - 1;
    assert!((result.times[last] - 0.25).abs() < 1.0e-12);
    assert!((result.data[x][last] - (-0.25_f64).exp()).abs() < 1.0e-12);
}

fn session_value(opts: SimOptions, time: f64) -> f64 {
    let mut session = SimulationSession::new(&model(), opts).expect("the session prepares");
    session.advance_to(time).expect("the session advances");
    assert_eq!(session.time().to_bits(), time.to_bits());
    session.get("x").unwrap().expect("the state is visible")
}

#[test]
fn the_incremental_session_agrees_with_rk_like_and_the_analytic_value() {
    let time = 0.75;
    let reference = session_value(options("rk-like", None), time);
    for (order, tolerance) in ORDERS {
        let opts = options("fixed-step", Some(fixed_step_backend_options(STEP, order)));
        let value = session_value(opts, time);
        assert!(
            (value - (-time).exp()).abs() <= tolerance,
            "order {order}: {value}"
        );
        // `rk-like` runs at its default 1e-6 tolerances.
        assert!(
            (value - reference).abs() <= tolerance + 1.0e-5,
            "order {order}: {value} vs rk-like {reference}"
        );
    }
}

fn refusal(opts: SimOptions) -> BackendOptionsError {
    let batch = simulate_with_diagnostics(&model(), &opts);
    let Err(SimulationDiagnosticError::BackendOptions(batch)) = batch else {
        panic!("the batch entry must refuse the request with the typed option error");
    };
    let Err(SimulationDiagnosticError::BackendOptions(session)) =
        SimulationSession::new(&model(), opts)
    else {
        panic!("the session must refuse the request with the typed option error");
    };
    assert_eq!(batch, session);
    session
}

#[test]
fn invalid_or_misdirected_options_are_typed_refusals_before_any_model_work() {
    for (step, order, field) in [
        (STEP, 3, "order"),
        (STEP, 0, "order"),
        (0.0, 4, "step"),
        (-STEP, 4, "step"),
        (f64::NAN, 4, "step"),
    ] {
        let opts = options("fixed-step", Some(fixed_step_backend_options(step, order)));
        assert_eq!(
            refusal(opts),
            BackendOptionsError::InvalidValue {
                backend: NumericalBackend::FixedStep,
                field,
            }
        );
    }
    let misdirected = options("rk-like", Some(fixed_step_backend_options(STEP, 4)));
    assert_eq!(
        refusal(misdirected),
        BackendOptionsError::BackendMismatch {
            requested: NumericalBackend::Rk45,
            supplied: NumericalBackend::FixedStep,
        }
    );
    let foreign = options(
        "fixed-step",
        Some(BackendOptions::new(NumericalBackend::FixedStep, 4_u32)),
    );
    assert_eq!(
        refusal(foreign),
        BackendOptionsError::ConcreteType {
            backend: NumericalBackend::FixedStep,
        }
    );
}
