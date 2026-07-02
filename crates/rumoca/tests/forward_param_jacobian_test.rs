//! Roadmap Track 0.3: the parameter Jacobian `∂(der(state))/∂p`, assembled by
//! the exact forward-mode AD JVP (parameter-seed), must agree with a
//! finite-difference of the primal derivative — including the path through
//! parameter-dependent algebraics.

use rumoca::Compiler;
use rumoca_eval_solve::{AlgebraicSettle, SolveRuntime};
use rumoca_sim::SimOptions;

// `y` is a parameter-dependent algebraic, and der(x) depends on it, so
// ∂der(x)/∂a and ∂der(x)/∂b exercise the algebraic-projection path; `c` and the
// der(w) equation add direct parameter dependence.
const SOURCE: &str = r#"
model ParamGrad
  parameter Real a = 2.0;
  parameter Real b = 3.0;
  parameter Real c = 0.5;
  Real x(start = 1.5);
  Real w(start = 0.7);
  Real y "parameter-dependent algebraic";
equation
  y = a * x + b * w;
  der(x) = -y + c * x;
  der(w) = a * w - b;
end ParamGrad;
"#;

#[test]
fn parameter_jacobian_matches_finite_difference() {
    let result = Compiler::new()
        .model("ParamGrad")
        .compile_str(SOURCE, "ParamGrad.mo")
        .expect("ParamGrad should compile");

    let opts = SimOptions::default();
    let solve_model = rumoca_sim::lower_dae_for_simulation(&result.dae, &opts)
        .expect("ParamGrad should lower to a solve model");
    let runtime = SolveRuntime::new(&solve_model).expect("solve runtime should build");

    let state_count = solve_model.state_scalar_count();
    let state: Vec<f64> = solve_model.initial_y[..state_count].to_vec();
    let params = solve_model.parameters.clone();
    let settle = AlgebraicSettle {
        tol: 1.0e-12,
        max_iters: 256,
    };

    // Exact forward-AD parameter Jacobian.
    let report = runtime.eval_parameter_jacobian(0.0, &state, &params, settle);
    assert!(
        report.error.is_none(),
        "parameter Jacobian failed: {:?}",
        report.error
    );
    assert_eq!(report.rows(), state_count);
    assert!(
        report.cols() >= 3,
        "expected at least params a,b,c, got {}",
        report.cols()
    );

    // Finite-difference of the primal derivative w.r.t. each parameter slot.
    let der0 = runtime
        .eval_state_derivatives(0.0, &state, &params, settle.tol, settle.max_iters)
        .expect("primal derivative should evaluate");
    let eps = 1.0e-6;
    let mut compared = 0;
    for col in 0..report.cols() {
        // Columns map back to the parameter vector via `param_slots`.
        let mut perturbed = params.clone();
        perturbed[report.param_slots[col]] += eps;
        let der_p = runtime
            .eval_state_derivatives(0.0, &state, &perturbed, settle.tol, settle.max_iters)
            .expect("perturbed derivative should evaluate");
        for row in 0..state_count {
            let fd = (der_p[row] - der0[row]) / eps;
            let ad = report.matrix[row][col];
            assert!(
                (fd - ad).abs() <= 1.0e-4 + 1.0e-4 * fd.abs(),
                "∂der({})/∂{} mismatch: AD={ad}, FD={fd}",
                report.row_labels[row],
                report.param_labels[col],
            );
            compared += 1;
        }
    }
    assert!(
        compared >= state_count * 3,
        "should compare every (state, param) entry"
    );

    // Spot-check the known analytic values at (x=1.5, w=0.7):
    //   ∂der(x)/∂a = -x,  ∂der(x)/∂b = -w,  ∂der(x)/∂c = x.
    let col = |name: &str| report.param_labels.iter().position(|p| p == name);
    let row = |name: &str| report.row_labels.iter().position(|r| r == name);
    if let (Some(rx), Some(ca), Some(cb), Some(cc)) = (row("x"), col("a"), col("b"), col("c")) {
        assert!(
            (report.matrix[rx][ca] - (-1.5)).abs() < 1.0e-6,
            "∂der(x)/∂a should be -x"
        );
        assert!(
            (report.matrix[rx][cb] - (-0.7)).abs() < 1.0e-6,
            "∂der(x)/∂b should be -w"
        );
        assert!(
            (report.matrix[rx][cc] - 1.5).abs() < 1.0e-6,
            "∂der(x)/∂c should be x"
        );
    }
}

#[test]
fn forward_sensitivity_rhs_matches_directional_finite_difference() {
    // Track 0.2 foundation: the forward-sensitivity RHS
    // ds/dt = ∂f/∂state·s + ∂f/∂p must equal the directional finite difference
    // of the primal derivative along (state += eps*s, p += eps*e_p).
    let result = Compiler::new()
        .model("ParamGrad")
        .compile_str(SOURCE, "ParamGrad.mo")
        .expect("ParamGrad should compile");
    let solve_model = rumoca_sim::lower_dae_for_simulation(&result.dae, &SimOptions::default())
        .expect("ParamGrad should lower");
    let runtime = SolveRuntime::new(&solve_model).expect("solve runtime should build");

    let n_state = solve_model.state_scalar_count();
    let state: Vec<f64> = solve_model.initial_y[..n_state].to_vec();
    let params = solve_model.parameters.clone();
    let settle = AlgebraicSettle {
        tol: 1.0e-12,
        max_iters: 256,
    };

    // Arbitrary nonzero sensitivity column and the first model parameter slot.
    let sens: Vec<f64> = (0..n_state).map(|i| 0.4 - 0.25 * i as f64).collect();
    let param_slot = 0; // `a`
    let mut rhs = vec![0.0; n_state];
    runtime
        .eval_forward_sensitivity_column_into(
            rumoca_eval_solve::AlgebraicLinearization {
                t: 0.0,
                params: &params,
                settle,
            },
            &state,
            &sens,
            param_slot,
            &mut rhs,
        )
        .expect("forward sensitivity RHS should evaluate");

    let eps = 1.0e-6;
    let f0 = runtime
        .eval_state_derivatives(0.0, &state, &params, settle.tol, settle.max_iters)
        .expect("primal derivative");
    let state_p: Vec<f64> = state.iter().zip(&sens).map(|(y, s)| y + eps * s).collect();
    let mut params_p = params.clone();
    params_p[param_slot] += eps;
    let f1 = runtime
        .eval_state_derivatives(0.0, &state_p, &params_p, settle.tol, settle.max_iters)
        .expect("perturbed primal derivative");

    for i in 0..n_state {
        let fd = (f1[i] - f0[i]) / eps;
        assert!(
            (fd - rhs[i]).abs() <= 1.0e-4 + 1.0e-4 * fd.abs(),
            "forward-sensitivity RHS[{i}]: ad={}, fd={fd}",
            rhs[i]
        );
    }
}
