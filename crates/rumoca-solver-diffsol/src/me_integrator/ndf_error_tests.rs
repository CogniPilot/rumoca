use diffsol::{
    BacktrackingLineSearch, Bdf, BdfState, FaerLU, FaerMat, FaerVec, NewtonNonlinearSolver,
    OdeBuilder, OdeSolverMethod, OdeSolverState, VectorHost,
};

#[test]
fn native_bdf_first_order_accepts_correction_inside_ndf_budget() {
    let h = 0.0016;
    let rtol = 1e-6;
    let atol = 1e-10;
    let problem = OdeBuilder::<FaerMat<f64>>::new()
        .t0(0.0)
        .h0(h)
        .rtol(rtol)
        .atol([atol])
        .rhs_implicit(
            |x, _p, _t, out| out[0] = -x[0],
            |_x, _p, _t, seed, out| out[0] = -seed[0],
        )
        .init(|_p, _t, out| out[0] = 1.0, 1)
        .build()
        .unwrap();
    // Fix the first trial's state and step, independently of the initial-step
    // heuristic. The exact ODE is y'= -y, y(0)=1.
    let state = BdfState::<FaerVec<f64>>::new_without_initialise(&problem).unwrap();
    let mut method: Bdf<'_, _, _, FaerMat<f64>> = Bdf::new(
        &problem,
        state,
        NewtonNonlinearSolver::new(FaerLU::<f64>::default(), BacktrackingLineSearch::default()),
    )
    .unwrap();
    method.step().unwrap();
    // At order one, alpha=1/1.185 and C1=.315. The implicit NDF
    // correction is h²/(1.185+h); its normalized error is below one,
    // and its continuous defect also fits. Using the order-zero sentinel
    // would still reject this trial.
    let correction = h * h / (1.185 + h);
    let expected_y = 1.0 - h + correction;
    let error_scale = atol + rtol * expected_y.abs();
    assert!(0.315 * correction / error_scale < 1.0);
    assert!(correction / error_scale > 1.0);
    assert_eq!(method.get_statistics().number_of_error_test_failures, 0);
    assert_eq!(method.state().t, h);
    assert!((method.state().y.as_slice()[0] - expected_y).abs() < 1e-14);
}

#[test]
fn native_bdf_relative_error_scale_uses_the_new_zero_crossing_state() {
    let h: f64 = 0.001;
    let rtol: f64 = 1e-6;
    let atol: f64 = 1e-10;
    let problem = OdeBuilder::<FaerMat<f64>>::new()
        .t0(0.0)
        .h0(h)
        .rtol(rtol)
        .atol([atol])
        .rhs_implicit(
            |_x, _p, t, out| out[0] = -1000.0 + 0.001 * t,
            |_x, _p, _t, _seed, out| out[0] = 0.0,
        )
        .init(|_p, _t, out| out[0] = 1.0, 1)
        .build()
        .unwrap();
    let state = BdfState::<FaerVec<f64>>::new_without_initialise(&problem).unwrap();
    let mut method: Bdf<'_, _, _, FaerMat<f64>> = Bdf::new(
        &problem,
        state,
        NewtonNonlinearSolver::new(FaerLU::<f64>::default(), BacktrackingLineSearch::default()),
    )
    .unwrap();
    method.step().unwrap();
    let t = method.state().t;
    let actual = method.state().y.as_slice()[0];
    let exact = 1.0 - 1000.0 * t + 0.0005 * t * t;
    let budget = atol + rtol * exact.abs();
    assert!(
        (actual - exact).abs() <= budget,
        "accepted y={actual} at{t}, exact={exact}, error={} exceeds new-state budget={budget}",
        (actual - exact).abs()
    );
    assert!(
        method.get_statistics().number_of_error_test_failures >= 1,
        "the original trial must be rejected at its new zero-crossing coordinate"
    );
    assert!(t < h);
}

#[test]
fn replaced_bdf_state_starts_a_new_equal_step_history() {
    let problem = OdeBuilder::<FaerMat<f64>>::new()
        .t0(0.0)
        .h0(0.001)
        .rtol(1e-6)
        .atol([1e-10])
        .rhs_implicit(
            |_x, _p, _t, out| out[0] = 0.0,
            |_x, _p, _t, _seed, out| out[0] = 0.0,
        )
        .init(|_p, _t, out| out[0] = 1.0, 1)
        .build()
        .unwrap();
    let state = BdfState::<FaerVec<f64>>::new_without_initialise(&problem).unwrap();
    let mut method: Bdf<'_, _, _, FaerMat<f64>> = Bdf::new(
        &problem,
        state,
        NewtonNonlinearSolver::new(FaerLU::<f64>::default(), BacktrackingLineSearch::default()),
    )
    .unwrap();
    method.step().unwrap();
    assert_eq!(method.order(), 1);
    let time = method.state().t;
    let mut fresh = BdfState::<FaerVec<f64>>::new_without_initialise(&problem).unwrap();
    *fresh.as_mut().t = time;
    fresh.set_problem(&problem).unwrap();
    method.set_state(fresh);
    method.step().unwrap();
    assert_eq!(
        method.order(),
        1,
        "first step of replaced history must not reuse equal steps from the old history"
    );
}

#[test]
fn stiff_tracking_dense_output_preserves_derivative_accuracy() {
    for (quadratic, h) in [(true, 0.001), (false, 0.01)] {
        let problem = OdeBuilder::<FaerMat<f64>>::new()
            .t0(0.0)
            .h0(h)
            .rtol(1e-6)
            .atol([1e-10])
            .rhs_implicit(
                move |x, _p, t, out| {
                    let g = if quadratic { 1.0 + t * t } else { 1.0 + t };
                    let dg = if quadratic { 2.0 * t } else { 1.0 };
                    out[0] = -1e4 * (x[0] - g) + dg;
                },
                |_x, _p, _t, seed, out| out[0] = -1e4 * seed[0],
            )
            .init(|_p, _t, out| out[0] = 1.0, 1)
            .build()
            .unwrap();
        let state = BdfState::<FaerVec<f64>>::new_without_initialise(&problem).unwrap();
        let mut method: Bdf<'_, _, _, FaerMat<f64>> = Bdf::new(
            &problem,
            state,
            NewtonNonlinearSolver::new(FaerLU::<f64>::default(), BacktrackingLineSearch::default()),
        )
        .unwrap();
        method.step().unwrap();
        let step = method.state().t;
        let mid = 0.5 * step;
        let actual = method.interpolate(mid).unwrap().as_slice()[0];
        let exact = if quadratic {
            1.0 + mid * mid
        } else {
            1.0 + mid
        };
        let derivative_error = 1e4 * (actual - exact).abs();
        let budget = 1e-10 + 1e-6 * exact.abs();
        assert!(
            step * derivative_error <= budget,
            "dense derivative error over step={} exceeds budget={budget}; h={step}, sample={actual}, exact={exact}",
            step * derivative_error
        );
        if !quadratic {
            assert_eq!(
                step, h,
                "exact stiff ramp must not be forced to small steps"
            );
            assert_eq!(method.get_statistics().number_of_error_test_failures, 0);
        }
    }
}

#[test]
fn native_bdf_retries_a_forced_trial_outside_the_rhs_domain() {
    let refusals = std::cell::Cell::new(0);
    let h = 2.0;
    let problem = OdeBuilder::<FaerMat<f64>>::new()
        .t0(0.0)
        .h0(h)
        .rtol(1e-6)
        .atol([1e-10])
        .rhs_implicit(
            |x, _p, _t, out| {
                if x[0] < 0.0 || !x[0].is_finite() {
                    refusals.set(refusals.get() + 1);
                    out[0] = f64::NAN;
                } else {
                    out[0] = -x[0];
                }
            },
            |_x, _p, _t, seed, out| out[0] = -seed[0],
        )
        .init(|_p, _t, out| out[0] = 1.0, 1)
        .build()
        .unwrap();
    // Skip the initial-step heuristic: the first predictor is 1 + 2*(-1)=-1,
    // outside the declared nonnegative RHS domain, while exp(-t) stays inside.
    let state = BdfState::<FaerVec<f64>>::new_without_initialise(&problem).unwrap();
    let mut method: Bdf<'_, _, _, FaerMat<f64>> = Bdf::new(
        &problem,
        state,
        NewtonNonlinearSolver::new(FaerLU::<f64>::default(), BacktrackingLineSearch::default()),
    )
    .unwrap();
    method
        .step()
        .expect("BDF rejects the refused trial and recovers");
    assert!(
        refusals.get() > 0,
        "the forced predictor must leave the RHS domain"
    );
    assert!(method.get_statistics().number_of_nonlinear_solver_fails > 0);
    let time = method.state().t;
    assert!(time > 0.0 && time < h);
    let actual = method.state().y.as_slice()[0];
    assert!(actual.is_finite() && actual >= 0.0);
    assert!((actual - (-time).exp()).abs() <= 1e-10 + 1e-6 * actual.abs());
}

#[test]
fn native_bdf_rejects_nonfinite_midpoint_without_poisoning_history() {
    for invalid_value in [f64::NAN, f64::INFINITY] {
        let h = 0.001;
        let midpoint_refusals = std::cell::Cell::new(0);
        let finite_endpoint = std::cell::Cell::new(false);
        let nonfinite_inputs = std::cell::Cell::new(0);
        let problem = OdeBuilder::<FaerMat<f64>>::new()
            .t0(0.0)
            .h0(h)
            .rtol(1e-6)
            .atol([1e-10])
            .rhs_implicit(
                |x, _p, t, out| {
                    let excess = x[0] - (1.0 + t * t);
                    if !x[0].is_finite() || !t.is_finite() {
                        nonfinite_inputs.set(nonfinite_inputs.get() + 1);
                        out[0] = invalid_value;
                    } else if excess > 1e-7 {
                        midpoint_refusals.set(midpoint_refusals.get() + u32::from(t == 0.5 * h));
                        out[0] = invalid_value;
                    } else {
                        finite_endpoint.set(finite_endpoint.get() || (t == h && excess > 0.0));
                        out[0] = -1e4 * excess + 2.0 * t;
                    }
                },
                |_x, _p, _t, seed, out| out[0] = -1e4 * seed[0],
            )
            .init(|_p, _t, out| out[0] = 1.0, 1)
            .build()
            .unwrap();
        // The exact solution 1+t² remains inside the RHS domain. The coarse
        // trial has valid endpoints but its native midpoint leaves the domain.
        let state = BdfState::<FaerVec<f64>>::new_without_initialise(&problem).unwrap();
        let mut method: Bdf<'_, _, _, FaerMat<f64>> = Bdf::new(
            &problem,
            state,
            NewtonNonlinearSolver::new(FaerLU::<f64>::default(), BacktrackingLineSearch::default()),
        )
        .unwrap();
        let mut previous_time = 0.0;
        for step in 0..12 {
            method
                .step()
                .expect("a refused midpoint must recover at a finite smaller step");
            let state = method.state();
            assert!(state.t.is_finite() && state.t > previous_time);
            assert!(state.h.is_finite() && state.h > 0.0);
            assert!(state.y.as_slice()[0].is_finite());
            assert!(state.dy.as_slice()[0].is_finite());
            if step == 0 {
                assert!(state.t < h, "the coarse midpoint must force rejection");
            }
            // Every dense sample uses the retained difference history. Continue
            // stepping so a poisoned table cannot hide behind one finite output.
            for sample in 0..=6 {
                let t = previous_time + (state.t - previous_time) * f64::from(sample) / 6.0;
                let y = method.interpolate(t).unwrap().as_slice()[0];
                let exact = 1.0 + t * t;
                assert!(y.is_finite());
                assert!((y - exact).abs() <= 1e-10 + 1e-6 * exact.abs());
            }
            previous_time = state.t;
        }
        assert!(
            finite_endpoint.get(),
            "the coarse corrected endpoint must be valid"
        );
        assert!(
            midpoint_refusals.get() > 0,
            "the first midpoint must actually be refused"
        );
        assert!(method.get_statistics().number_of_error_test_failures > 0);
        assert_eq!(
            nonfinite_inputs.get(),
            0,
            "rejection must not poison history or time"
        );
    }
}

#[test]
fn ode_defect_guard_does_not_reinterpret_a_mass_matrix_as_identity() {
    let h = 0.01;
    let problem = OdeBuilder::<FaerMat<f64>>::new()
        .t0(0.0)
        .h0(h)
        .rtol(1e-6)
        .atol([1e-10])
        .rhs_implicit(
            |_x, _p, _t, out| out[0] = 2.0,
            |_x, _p, _t, _seed, out| out[0] = 0.0,
        )
        .mass(|x, _p, _t, beta, out| out[0] = 2.0 * x[0] + beta * out[0])
        .init(|_p, _t, out| out[0] = 1.0, 1)
        .build()
        .unwrap();
    // M*y'=f with M=2 and f=2 has exact y=1+t. Testing P'-f would
    // invent a residual of -1 and reject an exact continuous extension.
    let mut state = BdfState::<FaerVec<f64>>::new_without_initialise(&problem).unwrap();
    state.as_mut().dy.as_mut_slice()[0] = 1.0;
    let mut method: Bdf<'_, _, _, FaerMat<f64>> = Bdf::new(
        &problem,
        state,
        NewtonNonlinearSolver::new(FaerLU::<f64>::default(), BacktrackingLineSearch::default()),
    )
    .unwrap();
    method.step().unwrap();
    assert_eq!(method.state().t, h);
    assert_eq!(method.get_statistics().number_of_error_test_failures, 0);
    assert!((method.state().y.as_slice()[0] - (1.0 + h)).abs() < 1e-14);
}
