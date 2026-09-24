use std::{cell::RefCell, rc::Rc};

use diffsol::{
    BacktrackingLineSearch, Bdf, BdfState, DiffsolError, FaerLU, FaerMat, FaerSparseLU,
    FaerSparseMat, FaerVec, NewtonNonlinearSolver, OdeBuilder, OdeSolverMethod, OdeSolverState,
    SupplementalErrorNorm, VectorHost,
};

struct VoltageObserver {
    resistance: f64,
    amplitude: f64,
    frequency: f64,
    nominal: f64,
    relative_tolerance: f64,
    absolute_tolerance: f64,
}

impl SupplementalErrorNorm<FaerVec<f64>> for VoltageObserver {
    fn supplemental_error(
        &mut self,
        time: f64,
        actual_trial_state: &FaerVec<f64>,
        estimated_delta: &FaerVec<f64>,
    ) -> Result<f64, DiffsolError> {
        let voltage =
            |state: f64| self.amplitude * (self.frequency * time).sin() - self.resistance * state;
        let actual = voltage(actual_trial_state.as_slice()[0]);
        let corrected = voltage(actual_trial_state.as_slice()[0] + estimated_delta.as_slice()[0]);
        let absolute = (self.absolute_tolerance * self.nominal).clamp(f64::MIN_POSITIVE, f64::MAX);
        let scale = actual.abs().max(corrected.abs());
        Ok((corrected - actual).abs() / (absolute + self.relative_tolerance * scale))
    }
}

#[test]
fn native_bdf_minimum_step_still_rejects_unresolved_dynamics() {
    let mut problem = OdeBuilder::<FaerMat<f64>>::new()
        .rtol(1e-6)
        .atol([1e-6])
        .rhs_implicit(
            |x, _p, _t, out| out[0] = -1e8 * x[0],
            |_x, _p, _t, v, out| out[0] = -1e8 * v[0],
        )
        .init(|_p, _t, out| out[0] = 1.0, 1)
        .build()
        .unwrap();
    problem.ode_options.min_timestep = 1e-3;
    let mut method = problem.bdf::<FaerLU<f64>>().unwrap();
    let error = method.step().unwrap_err();
    assert!(matches!(
        error,
        DiffsolError::OdeSolverError(diffsol::error::OdeSolverError::StepSizeTooSmall { .. })
    ));
}

#[test]
fn native_bdf_initial_step_near_zero_state_can_grow() {
    let initial_time = 0.01333333333333379;
    let initial_value = 3.31e-11;
    let slope = 72599.85;
    for direction in [1.0, -1.0] {
        let problem = OdeBuilder::<FaerMat<f64>>::new()
            .t0(initial_time)
            .h0(direction * 1e-3)
            .rtol(1e-6)
            .atol([1e-6])
            .rhs_implicit(
                move |_x, _p, _t, out| out[0] = slope,
                |_x, _p, _t, _v, out| out[0] = 0.0,
            )
            .init(move |_p, _t, out| out[0] = initial_value, 1)
            .build()
            .unwrap();
        let mut method = problem.bdf::<FaerLU<f64>>().unwrap();
        let stop_time = initial_time + direction * 1e-3;
        method.solve(stop_time).unwrap();
        let value = method.interpolate(stop_time).unwrap();
        let exact = initial_value + slope * (stop_time - initial_time);
        assert!((value.as_slice()[0] - exact).abs() < 1e-6);
    }
}

#[test]
fn native_bdf_can_continue_after_a_short_stop_interval() {
    for direction in [1.0, -1.0] {
        let problem = OdeBuilder::<FaerMat<f64>>::new()
            .h0(direction * 1e-3)
            .rtol(1e-6)
            .atol([1e-6])
            .rhs_implicit(
                |_x, _p, _t, out| out[0] = 1.0,
                |_x, _p, _t, _v, out| out[0] = 0.0,
            )
            .init(|_p, _t, out| out[0] = 0.0, 1)
            .build()
            .unwrap();
        let mut method = problem.bdf::<FaerLU<f64>>().unwrap();
        let short_stop = direction * 5e-15;
        method.set_stop_time(short_stop).unwrap();
        method.step().unwrap();
        assert_eq!(
            method.state().t,
            short_stop,
            "a hard stop cannot be crossed"
        );
        let stop_time = direction * 1e-3;
        method.solve(stop_time).unwrap();
        let value = method.interpolate(stop_time).unwrap();
        assert!((value.as_slice()[0] - stop_time).abs() < 1e-12);
    }
}

#[test]
fn native_bdf_endpoint_matches_its_continuous_extension() {
    // This calls Diffsol directly: no Modelica, FMI, events, or Rumoca sampler
    // can account for a discrepancy between two views of one accepted point.
    let problem = OdeBuilder::<FaerMat<f64>>::new()
        .t0(0.0)
        .h0(1e-3)
        .rtol(1e-6)
        .atol([1e-6])
        .rhs_implicit(
            |x, _p, _t, out| out[0] = x[0],
            |_x, _p, _t, v, out| out[0] = v[0],
        )
        .init(|_p, _t, out| out[0] = 1.0, 1)
        .build()
        .unwrap();
    let mut method = problem.bdf::<FaerLU<f64>>().unwrap();
    for _ in 0..20 {
        method.step().unwrap();
        let state = method.state();
        let at_endpoint = method.interpolate(state.t).unwrap();
        let immediately_before = method.interpolate(state.t.next_down()).unwrap();
        for sampled in [at_endpoint, immediately_before] {
            assert!(
                (sampled.as_slice()[0] - state.y.as_slice()[0]).abs() < 1e-12,
                "at {}: state={}, interpolated={}",
                state.t,
                state.y.as_slice()[0],
                sampled.as_slice()[0]
            );
        }
    }
}

#[test]
fn shortened_bdf_step_preserves_the_stiff_voltage_solution() {
    // SPEC_0038 / ME-INT-004: a hard stop changes the numerical step's
    // coefficient just as ordinary step-size adaptation does.
    const RESISTANCE: f64 = 3e6;
    const FREQUENCY: f64 = 100.0 * std::f64::consts::PI;
    const INDUCTANCE: f64 = 0.1 / FREQUENCY;
    let problem = OdeBuilder::<FaerSparseMat<f64>>::new()
        .h0(1e-3)
        .rtol(1e-6)
        .atol([1e-6])
        .rhs_implicit(
            |x, _p, t, out| {
                out[0] = (100.0 * (FREQUENCY * t).sin() - RESISTANCE * x[0]) / INDUCTANCE;
            },
            |_x, _p, _t, seed, out| {
                out[0] = -RESISTANCE / INDUCTANCE * seed[0];
            },
        )
        .init(|_p, _t, out| out[0] = 0.0, 1)
        .build()
        .unwrap();
    let mut state = BdfState::new_without_initialise(&problem).unwrap();
    state.set_step_size(problem.h0, &problem.atol, problem.rtol, &problem.eqn, 1);
    let mut method = Bdf::<_, _, FaerMat<f64>>::new(
        &problem,
        state,
        NewtonNonlinearSolver::new(
            FaerSparseLU::<f64>::default(),
            BacktrackingLineSearch::default(),
        ),
    )
    .unwrap();
    method.set_supplemental_error_norm(Rc::new(RefCell::new(VoltageObserver {
        resistance: RESISTANCE,
        amplitude: 100.0,
        frequency: FREQUENCY,
        nominal: 1.0,
        relative_tolerance: 1e-6,
        absolute_tolerance: 1e-6,
    })));
    let stop_time = 0.02;
    method.set_stop_time(stop_time).unwrap();
    while method.state().t < stop_time {
        method.step().unwrap();
    }
    let state = method.state();
    assert_eq!(state.t, stop_time);
    let voltage = 100.0 * (FREQUENCY * state.t).sin() - RESISTANCE * state.y.as_slice()[0];
    let exact = 100.0
        * FREQUENCY
        * INDUCTANCE
        * (RESISTANCE * (FREQUENCY * stop_time).cos()
            + FREQUENCY * INDUCTANCE * (FREQUENCY * stop_time).sin()
            - RESISTANCE * (-RESISTANCE * stop_time / INDUCTANCE).exp())
        / (RESISTANCE.powi(2) + (FREQUENCY * INDUCTANCE).powi(2));
    assert!(
        (voltage - exact).abs() < 1e-5,
        "shortened step voltage {voltage} differs from the analytic value {exact}"
    );
}
