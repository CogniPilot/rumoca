use diffsol::{DiffsolError, FaerLU, FaerMat, OdeBuilder, OdeSolverMethod, VectorHost};

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
