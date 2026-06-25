//! Roadmap Track A (scalar reverse core): the reverse-mode VJP `Jᵀλ` of the
//! state-derivative function must be the exact transpose of the forward JVP `Jv`.
//! The correctness gate is the dot-product identity `λᵀ(J v) = (Jᵀλ)ᵀ v`
//! (roadmap §9.1), checked here against the existing forward parameter-seed AD.

use rumoca::Compiler;
use rumoca_eval_solve::{AlgebraicLinearization, AlgebraicSettle, SolveRuntime};
use rumoca_sim::SimOptions;

// Smooth, pure-ODE model (no algebraics → solver_y == states, no projection):
//   der(x) = sin(a*x) + b*y^2
//   der(y) = exp(-x) - a*y + b
// States x, y; parameters a, b. The seed/cotangent space is [x, y | a, b].
const SOURCE: &str = r#"
model RevModel
  parameter Real a = 0.7;
  parameter Real b = 1.3;
  Real x(start = 0.5);
  Real y(start = -0.2);
equation
  der(x) = sin(a * x) + b * y * y;
  der(y) = exp(-x) - a * y + b;
end RevModel;
"#;

#[test]
fn reverse_vjp_matches_forward_jvp_dot_product() {
    let result = Compiler::new()
        .model("RevModel")
        .compile_str(SOURCE, "RevModel.mo")
        .expect("RevModel should compile");
    let solve_model = rumoca_sim::lower_dae_for_simulation(&result.dae, &SimOptions::default())
        .expect("lowering should succeed");
    let runtime = SolveRuntime::new(&solve_model).expect("runtime should build");

    // Pure-ODE model: solver_y is exactly the two states.
    assert_eq!(runtime.state_count, 2, "expected 2 states");
    assert_eq!(runtime.solver_count, 2, "pure ODE: solver_y == states");
    // The seed/cotangent space is [solver_y | parameter scalars]; the parameter
    // count includes any compiler-internal scalars beyond `a`/`b`.
    let p_scalars = solve_model.problem.layout.p_scalars();
    let seed_len = runtime.solver_count + p_scalars;

    let state = vec![0.5_f64, -0.2];
    let params = solve_model.parameters.clone();
    let settle = AlgebraicSettle {
        tol: 1.0e-12,
        max_iters: 64,
    };
    let lin = AlgebraicLinearization {
        t: 0.0,
        params: &params,
        settle,
    };

    // A fixed, non-trivial tangent v (over [solver_y | params]) and output
    // cotangent λ. v is generated deterministically to cover the full seed space.
    let v: Vec<f64> = (0..seed_len)
        .map(|i| 0.35 + 0.2 * (i as f64) * if i % 2 == 0 { 1.0 } else { -1.0 })
        .collect();
    let lambda = [0.60_f64, -0.90];
    assert_eq!(lambda.len(), runtime.state_count);

    // Forward: Jv via the parameter-seed JVP (seed laid out as [solver_y | p]).
    let mut jv = vec![0.0_f64; runtime.state_count];
    runtime
        .eval_full_jacobian_v_ad_into(lin, &state, &v, &mut jv)
        .expect("forward JVP should evaluate");

    // Reverse: Jᵀλ via the new reverse sweep, same [solver_y | p] layout.
    let mut jt_lambda = vec![0.0_f64; seed_len];
    runtime
        .reverse_state_derivative_vjp(lin, &state, &lambda, &mut jt_lambda)
        .expect("reverse VJP should evaluate");

    let lhs: f64 = lambda.iter().zip(&jv).map(|(l, j)| l * j).sum();
    let rhs: f64 = jt_lambda.iter().zip(&v).map(|(g, vi)| g * vi).sum();
    assert!(
        (lhs - rhs).abs() < 1.0e-9,
        "dot-product identity failed: λᵀ(Jv)={lhs}, (Jᵀλ)ᵀv={rhs}, Jv={jv:?}, Jᵀλ={jt_lambda:?}"
    );

    // Sanity: the gradient is non-trivial (not a vacuous 0 == 0 pass).
    assert!(
        lhs.abs() > 1.0e-6,
        "test point should produce a nonzero pairing"
    );

    // Stronger gate: the full reverse Jacobian is the exact transpose of the
    // full forward Jacobian, entry by entry. Column j of J via forward unit seed
    // e_j; row i of J via reverse unit cotangent e_i.
    let mut forward = vec![vec![0.0_f64; seed_len]; runtime.state_count]; // J[i][j]
    for j in 0..seed_len {
        let mut seed = vec![0.0_f64; seed_len];
        seed[j] = 1.0;
        let mut col = vec![0.0_f64; runtime.state_count];
        runtime
            .eval_full_jacobian_v_ad_into(lin, &state, &seed, &mut col)
            .expect("forward column");
        for (i, value) in col.iter().enumerate() {
            forward[i][j] = *value;
        }
    }
    for i in 0..runtime.state_count {
        let mut cot = vec![0.0_f64; runtime.state_count];
        cot[i] = 1.0;
        let mut row = vec![0.0_f64; seed_len];
        runtime
            .reverse_state_derivative_vjp(lin, &state, &cot, &mut row)
            .expect("reverse row");
        for (j, value) in row.iter().enumerate() {
            assert!(
                (value - forward[i][j]).abs() < 1.0e-9,
                "transpose mismatch at ({i},{j}): reverse={value}, forward={}",
                forward[i][j]
            );
        }
    }
}

// A model with a genuine nonlinear solver algebraic (z* = sqrt(x+1)).
const ALGEBRAIC_SOURCE: &str = r#"
model AlgModel
  parameter Real a = 2.0;
  Real x(start = 1.0);
  Real z(start = 1.0);
equation
  der(x) = -a * x + z;
  z * z = x + 1.0;
end AlgModel;
"#;

/// The bare reverse VJP does not chain through the algebraic projection, so for a
/// model with solver algebraics it must fail loudly rather than return a partial
/// (wrong) gradient. Locks in the Track-A scope boundary (projection adjoint = B).
#[test]
fn reverse_vjp_rejects_models_with_algebraics() {
    let result = Compiler::new()
        .model("AlgModel")
        .compile_str(ALGEBRAIC_SOURCE, "AlgModel.mo")
        .expect("AlgModel should compile");
    let solve_model = rumoca_sim::lower_dae_for_simulation(&result.dae, &SimOptions::default())
        .expect("lowering should succeed");
    let runtime = SolveRuntime::new(&solve_model).expect("runtime should build");

    // Premise: z is a solver algebraic, so solver_y is wider than the state set.
    assert!(
        runtime.solver_count > runtime.state_count,
        "model should carry a solver algebraic (solver_count {} > state_count {})",
        runtime.solver_count,
        runtime.state_count
    );

    let params = solve_model.parameters.clone();
    let lin = AlgebraicLinearization {
        t: 0.0,
        params: &params,
        settle: AlgebraicSettle {
            tol: 1.0e-12,
            max_iters: 64,
        },
    };
    let mut out = vec![0.0_f64; runtime.solver_count + solve_model.problem.layout.p_scalars()];
    let err = runtime
        .reverse_state_derivative_vjp(lin, &[1.0], &vec![1.0; runtime.state_count], &mut out)
        .expect_err("reverse VJP must reject a model with solver algebraics");
    let message = err.to_string();
    assert!(
        message.contains("algebraic"),
        "error should explain the algebraic limitation, got: {message}"
    );
}

// Pure-ODE model exercising the Max subgradient: der(x) = max(a², x²) - x.
const MAX_SOURCE: &str = r#"
model MaxModel
  parameter Real a = 0.5;
  Real x(start = 2.0);
equation
  der(x) = max(a * a, x * x) - x;
end MaxModel;
"#;

/// The reverse `Max` partial must agree with the forward JVP's branch selection.
/// Evaluated at x=2 (x²=4 strictly exceeds a²=0.25), the active branch is
/// unambiguous, so the dot-product identity must hold exactly.
#[test]
fn reverse_vjp_max_subgradient_matches_forward() {
    let result = Compiler::new()
        .model("MaxModel")
        .compile_str(MAX_SOURCE, "MaxModel.mo")
        .expect("MaxModel should compile");
    let solve_model = rumoca_sim::lower_dae_for_simulation(&result.dae, &SimOptions::default())
        .expect("lowering should succeed");
    let runtime = SolveRuntime::new(&solve_model).expect("runtime should build");
    assert_eq!(runtime.solver_count, runtime.state_count, "pure ODE");

    let p_scalars = solve_model.problem.layout.p_scalars();
    let seed_len = runtime.solver_count + p_scalars;
    let state = vec![2.0_f64];
    let params = solve_model.parameters.clone();
    let lin = AlgebraicLinearization {
        t: 0.0,
        params: &params,
        settle: AlgebraicSettle {
            tol: 1.0e-12,
            max_iters: 32,
        },
    };

    let v: Vec<f64> = (0..seed_len).map(|i| 0.5 - 0.3 * i as f64).collect();
    let lambda = [1.4_f64];
    let mut jv = vec![0.0_f64; runtime.state_count];
    runtime
        .eval_full_jacobian_v_ad_into(lin, &state, &v, &mut jv)
        .expect("forward JVP");
    let mut jt = vec![0.0_f64; seed_len];
    runtime
        .reverse_state_derivative_vjp(lin, &state, &lambda, &mut jt)
        .expect("reverse VJP");

    let lhs: f64 = lambda.iter().zip(&jv).map(|(l, j)| l * j).sum();
    let rhs: f64 = jt.iter().zip(&v).map(|(g, vi)| g * vi).sum();
    assert!(
        (lhs - rhs).abs() < 1.0e-9,
        "Max subgradient mismatch: λᵀ(Jv)={lhs}, (Jᵀλ)ᵀv={rhs}"
    );
}
