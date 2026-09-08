/// End-to-end test: compile an ODE via MLIR, integrate with a simple
/// fixed-step Euler loop, and verify numerics against the analytical solution.
///
/// Model: der(x) = -x   with   x(0) = 1
/// Analytical: x(t) = exp(-t)
use rumoca_exec_mlir::{MlirError, build_ode_model};

use super::support::{derivative, fixture, missing_cpu_tool};

/// Build a `SolveModel` for `xdot = -y[0]` (exponential decay) directly
/// from solve-IR rows, mirroring the rk45 test helper pattern.
fn decay_model() -> std::sync::Arc<rumoca_ir_solve::SolveModel> {
    use rumoca_core::{SourceId, Span};
    use rumoca_ir_solve::{ComputeBlock, LinearOp, ScalarProgramBlock, UnaryOp};

    fn spb(rows: Vec<Vec<LinearOp>>, label: &str) -> ScalarProgramBlock {
        ScalarProgramBlock::with_source_span(
            rows,
            Span::from_offsets(SourceId::from_source_name(label), 0, label.len())
                .require_provenance("MLIR integration fixture")
                .expect("fixture span is source-backed"),
        )
        .expect("fixture program is computable")
    }

    // xdot = -y[0]
    let rhs_rows = vec![vec![
        LinearOp::LoadY { dst: 0, index: 0 },
        LinearOp::Unary {
            dst: 1,
            op: UnaryOp::Neg,
            arg: 0,
        },
        LinearOp::StoreOutput { src: 1 },
    ]];

    let derivative_rhs_cb =
        ComputeBlock::from_scalar_program_block(spb(rhs_rows, "integrate_derivative.mo"));

    let problem = derivative::derivative_problem(
        fixture::ContinuousInventory::new(vec![fixture::FixtureScalar::state("x", 1.0, 1.0)]),
        derivative_rhs_cb,
    );
    fixture::complete_model(problem)
}

/// Issue the checked continuous refresh owners of a finished fixture problem.
///
/// Solve lowering issues them from the whole problem once it is complete
/// (`rumoca-phase-solve/src/lower.rs`), and every runtime fixture in the
/// workspace re-derives them the same way rather than hand-writing plans, so a
/// fixture can never carry a refresh inventory the real pipeline would not
/// produce for the same problem.
#[test]
fn mlir_euler_decay_matches_analytical() {
    let model = decay_model();

    let compiled = match build_ode_model(model, "decay") {
        Ok(c) => c,
        Err(MlirError::ToolNotFound { tool, .. }) => {
            missing_cpu_tool::missing_cpu_tool(tool);
            return;
        }
        Err(e) => panic!("compile failed: {e}"),
    };

    // Fixed-step forward Euler: x += dt * xdot
    let dt = 1e-3f64;
    let t_end = 1.0f64;
    let steps = (t_end / dt).round() as usize;

    let mut y = compiled.initial_y.clone();
    let mut t = 0.0f64;

    for _ in 0..steps {
        let xdot = compiled.eval_state_derivatives(t, &y).expect("eval failed");
        for (yi, di) in y.iter_mut().zip(&xdot) {
            *yi += dt * di;
        }
        t += dt;
    }

    let analytical = (-t_end).exp(); // x(1) = exp(-1) ≈ 0.3679
    let error = (y[0] - analytical).abs();

    // Forward Euler with dt=1e-3 gives ~O(dt) error ≈ 5e-4
    assert!(
        error < 1e-3,
        "MLIR Euler decay: got x(1)={:.6}, expected {:.6}, error={:.2e}",
        y[0],
        analytical,
        error
    );
}

#[test]
fn mlir_derivatives_match_analytical_at_multiple_points() {
    let model = decay_model();

    let compiled = match build_ode_model(model, "decay_pts") {
        Ok(c) => c,
        Err(MlirError::ToolNotFound { tool, .. }) => {
            missing_cpu_tool::missing_cpu_tool(tool);
            return;
        }
        Err(e) => panic!("compile failed: {e}"),
    };

    // For der(x) = -x: xdot should equal -x at each point
    for &x_val in &[0.0, 0.5, 1.0, -1.5, 2.75] {
        let y = [x_val];
        let xdot = compiled
            .eval_state_derivatives(0.0, &y)
            .expect("eval failed");
        let expected = -x_val;
        assert!(
            (xdot[0] - expected).abs() < 1e-12,
            "at x={x_val}: xdot={} expected {expected}",
            xdot[0]
        );
    }
}
