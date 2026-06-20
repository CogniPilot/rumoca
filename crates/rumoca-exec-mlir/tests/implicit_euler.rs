use rumoca_core::{SourceId, Span};
/// Phase 6.7: Implicit Euler integration driven by MLIR eval_implicit_rhs + eval_jacobian_v.
///
/// Each implicit Euler step solves  G(y_new) = 0  via scalar Newton iteration:
///   G(y_new)   = y_new - y_old - dt * eval_implicit_rhs(y_new)
///   G'(v)      = v     - dt    * eval_jacobian_v(y_new, v)
///   update     = G / G'  (solved with a single Newton step in the scalar case,
///                          iterated to convergence in the vector / nonlinear case)
///
/// Two models:
///   1. Linear decay  xdot = -x                — exact sol exp(-t)
///   2. Logistic      xdot = x*(1-x)           — exact sol 1/(1+exp(-t))  with x(0)=0.5
use rumoca_exec_mlir::{
    CompiledMlirResidual, MlirError, compile_derivative_rhs as exec_compile_derivative_rhs,
};
use rumoca_ir_solve::{
    BinaryOp, ComputeBlock, ContinuousSolveSystem, LinearOp, ScalarProgramBlock, SolveProblem,
    UnaryOp,
};

// ── helpers ──────────────────────────────────────────────────────────────────

fn compile_or_skip(solve: &SolveProblem, name: &str) -> Option<CompiledMlirResidual> {
    let artifacts =
        rumoca_phase_solve::lower_solve_artifacts(solve).expect("test solve artifacts lower");
    match exec_compile_derivative_rhs(solve, &artifacts, name) {
        Ok(c) => Some(c),
        Err(MlirError::ToolNotFound { tool, .. }) => {
            eprintln!("SKIP: {tool} not found");
            None
        }
        Err(e) => panic!("compile failed: {e}"),
    }
}

fn spb(rows: Vec<Vec<LinearOp>>, label: &str) -> ScalarProgramBlock {
    ScalarProgramBlock::with_source_span(
        rows,
        Span::from_offsets(SourceId::from_source_name(label), 0, label.len()),
    )
}

/// Implicit Euler integrator using MLIR-compiled residual + JVP.
///
/// Newton convergence tolerance: 1e-12 absolute.
/// Panics if Newton fails to converge within `max_newton` iterations.
fn implicit_euler(
    compiled: &CompiledMlirResidual,
    y0: &[f64],
    p: &[f64],
    dt: f64,
    n_steps: usize,
    max_newton: usize,
) -> Vec<f64> {
    let n = y0.len();
    let mut y = y0.to_vec();
    let mut y_new = vec![0.0f64; n];
    let mut g_vec = vec![0.0f64; n];
    let mut rhs_vec = vec![0.0f64; n];
    let mut gp_vec = vec![0.0f64; n]; // G'(v) result
    let mut t = 0.0f64;

    for _step in 0..n_steps {
        // Initial guess: explicit Euler (good starting point for small dt)
        compiled
            .call(&y, p, t, &mut rhs_vec)
            .expect("eval_derivative failed");
        for i in 0..n {
            y_new[i] = y[i] + dt * rhs_vec[i];
        }

        // Newton iteration
        for iter in 0..max_newton {
            // G(y_new) = y_new - y_old - dt * f(y_new)
            compiled
                .call_implicit_rhs(&y_new, p, t + dt, &mut rhs_vec)
                .expect("eval_implicit_rhs absent")
                .expect("eval_implicit_rhs call failed");
            let mut g_norm = 0.0f64;
            for i in 0..n {
                g_vec[i] = y_new[i] - y[i] - dt * rhs_vec[i];
                g_norm += g_vec[i] * g_vec[i];
            }
            g_norm = g_norm.sqrt();
            if g_norm < 1e-12 {
                break;
            }
            assert!(
                iter + 1 < max_newton,
                "Newton failed to converge at step, g_norm={g_norm:.3e}"
            );

            // G'(v) * v = v - dt * J * v   where v = -G (Newton direction)
            // For scalar Newton: delta = -G / G'(1)
            // For vector Newton (diagonal approx): solve G' * delta = -G component-wise.
            // We use a per-component diagonal approximation: G'_i(e_i) = e_i - dt * J_{ii}.
            for i in 0..n {
                let mut ei = vec![0.0f64; n];
                ei[i] = 1.0;
                compiled
                    .call_jacobian_v(&y_new, p, &ei, t + dt, &mut gp_vec)
                    .expect("eval_jacobian_v absent")
                    .expect("eval_jacobian_v call failed");
                // G'_i(e_i) = e_i[i] - dt * gp_vec[i] = 1 - dt * J_{ii}
                let gp_ii = 1.0 - dt * gp_vec[i];
                y_new[i] -= g_vec[i] / gp_ii;
            }
        }
        t += dt;
        y.copy_from_slice(&y_new);
    }
    y
}

// ── SolveProblem builders ─────────────────────────────────────────────────────

/// Model: xdot = -x
///   implicit_rhs:    g(y) = -y[0]
///   implicit_jac_v:  J*v  = -v[0]
fn decay_solve() -> SolveProblem {
    let rhs_row = vec![
        LinearOp::LoadY { dst: 0, index: 0 },
        LinearOp::Unary {
            dst: 1,
            op: UnaryOp::Neg,
            arg: 0,
        },
        LinearOp::StoreOutput { src: 1 },
    ];
    SolveProblem {
        continuous: ContinuousSolveSystem {
            derivative_rhs: ComputeBlock::from_scalar_program_block(spb(
                vec![rhs_row.clone()],
                "implicit_euler_decay_derivative.mo",
            )),
            implicit_rhs: ComputeBlock::from_scalar_program_block(spb(
                vec![rhs_row],
                "implicit_euler_decay_implicit.mo",
            )),
            ..Default::default()
        },
        ..Default::default()
    }
}

/// Model: xdot = x*(1-x)  (logistic growth)
///   f(y)      = y[0] * (1 - y[0])
///   J*v       = (1 - 2*y[0]) * v[0]
fn logistic_solve() -> SolveProblem {
    // f(y) = y[0] * (1 - y[0])
    //      = y[0] * 1 - y[0]*y[0]
    // Registers: 0=y[0], 1=1.0, 2=(1-y[0]), 3=y[0]*(1-y[0])
    let rhs_row = vec![
        LinearOp::LoadY { dst: 0, index: 0 },
        LinearOp::Const { dst: 1, value: 1.0 },
        LinearOp::Binary {
            dst: 2,
            op: BinaryOp::Sub,
            lhs: 1,
            rhs: 0,
        }, // 1 - y
        LinearOp::Binary {
            dst: 3,
            op: BinaryOp::Mul,
            lhs: 0,
            rhs: 2,
        }, // y*(1-y)
        LinearOp::StoreOutput { src: 3 },
    ];
    SolveProblem {
        continuous: ContinuousSolveSystem {
            derivative_rhs: ComputeBlock::from_scalar_program_block(spb(
                vec![rhs_row.clone()],
                "implicit_euler_logistic_derivative.mo",
            )),
            implicit_rhs: ComputeBlock::from_scalar_program_block(spb(
                vec![rhs_row],
                "implicit_euler_logistic_implicit.mo",
            )),
            ..Default::default()
        },
        ..Default::default()
    }
}

// ── tests ─────────────────────────────────────────────────────────────────────

#[test]
fn implicit_euler_decay_matches_analytical() {
    let compiled = match compile_or_skip(&decay_solve(), "ie_decay") {
        Some(c) => c,
        None => return,
    };

    let dt = 1e-3f64;
    let n_steps = 1000usize; // t = 0 → 1
    let y_final = implicit_euler(&compiled, &[1.0], &[], dt, n_steps, 20);

    let analytical = (-1.0f64).exp();
    let error = (y_final[0] - analytical).abs();
    // Implicit Euler O(dt) global error; A-stable so no blow-up.
    assert!(
        error < 5e-4,
        "decay: x(1)={:.8}, analytical={:.8}, error={:.2e}",
        y_final[0],
        analytical,
        error
    );
}

#[test]
fn implicit_euler_logistic_matches_analytical() {
    let compiled = match compile_or_skip(&logistic_solve(), "ie_logistic") {
        Some(c) => c,
        None => return,
    };

    // x(0)=0.5, xdot=x*(1-x). Analytical: x(t) = 1 / (1 + exp(-t)) with x(0)=0.5.
    let dt = 1e-3f64;
    let n_steps = 1000usize; // t = 0 → 1
    let y_final = implicit_euler(&compiled, &[0.5], &[], dt, n_steps, 20);

    // Analytical: x(1) = 1/(1+exp(-1)) ≈ 0.7310585786
    let analytical = 1.0 / (1.0 + (-1.0f64).exp());
    let error = (y_final[0] - analytical).abs();
    assert!(
        error < 5e-4,
        "logistic: x(1)={:.8}, analytical={:.8}, error={:.2e}",
        y_final[0],
        analytical,
        error
    );
}

#[test]
fn implicit_euler_newton_converges_in_few_iters() {
    // Verify Newton converges quickly for the nonlinear logistic model at a single step.
    let compiled = match compile_or_skip(&logistic_solve(), "ie_newton_iters") {
        Some(c) => c,
        None => return,
    };

    // One large step to force Newton to do real work (Euler initialisation will be off).
    let y_final = implicit_euler(&compiled, &[0.5], &[], 0.1, 1, 20);

    // Compare against reference: fine-grained explicit Euler (1000 sub-steps)
    let mut y_ref = [0.5f64];
    for _ in 0..1000 {
        let f = y_ref[0] * (1.0 - y_ref[0]);
        y_ref[0] += 1e-4 * f;
    }
    let error = (y_final[0] - y_ref[0]).abs();
    // Implicit Euler has O(dt) error vs explicit reference; dt=0.1 → expect ~0.01
    assert!(
        error < 0.02,
        "single large step: implicit={:.6}, explicit_ref={:.6}, diff={:.2e}",
        y_final[0],
        y_ref[0],
        error
    );
}
