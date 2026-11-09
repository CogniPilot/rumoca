use diffsol::{
    BacktrackingLineSearch, Bdf, BdfState, FaerLU, FaerMat, FaerVec, NewtonNonlinearSolver,
    OdeBuilder, OdeSolverMethod, OdeSolverState, VectorHost,
};

#[test]
fn native_bdf_first_order_accepts_correction_inside_ndf_budget() {
    let h = 0.0018;
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
    // whereas using the order-zero sentinel would reject it.
    let correction = h * h / (1.185 + h);
    let expected_y = 1.0 - h + correction;
    let error_scale = atol + rtol * expected_y.abs();
    assert!(0.315 * correction / error_scale < 1.0);
    assert!(correction / error_scale > 1.0);
    assert_eq!(method.get_statistics().number_of_error_test_failures, 0);
    assert_eq!(method.state().t, h);
    assert!((method.state().y.as_slice()[0] - expected_y).abs() < 1e-14);
}
