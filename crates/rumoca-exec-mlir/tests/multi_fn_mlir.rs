use rumoca_core::{SourceId, Span};
/// Phase 6.6: Multi-function MLIR module tests.
///
/// The MLIR shared library now exports three functions from a single compilation:
///   - `eval_derivative`   — explicit ODE RHS: xdot = f(y, p, t)
///   - `eval_implicit_rhs` — implicit residual: g(y, p, t) = 0
///   - `eval_jacobian_v`   — forward-mode JVP:  dg/dy * seed
///
/// Model: xdot = -y[0]
///   implicit_rhs:    g(y)     = -y[0]
///   implicit_jac_v:  J*seed   = -seed[0]   (because dg/dy[0] = -1)
use rumoca_exec_mlir::{MlirError, compile_derivative_rhs as exec_compile_derivative_rhs};
use rumoca_ir_solve::{
    ComputeBlock, ContinuousSolveSystem, LinearOp, ScalarProgramBlock, SolveProblem, UnaryOp,
};

fn spb(rows: Vec<Vec<LinearOp>>, label: &str) -> ScalarProgramBlock {
    ScalarProgramBlock::with_source_span(
        rows,
        Span::from_offsets(SourceId::from_source_name(label), 0, label.len()),
    )
}

fn decay_solve_problem() -> SolveProblem {
    // derivative_rhs: xdot = -y[0]
    let drv_row = vec![
        LinearOp::LoadY { dst: 0, index: 0 },
        LinearOp::Unary {
            dst: 1,
            op: UnaryOp::Neg,
            arg: 0,
        },
        LinearOp::StoreOutput { src: 1 },
    ];
    // implicit_rhs: g = -y[0]  (same computation, residual form)
    let impl_row = drv_row.clone();
    SolveProblem {
        continuous: ContinuousSolveSystem {
            derivative_rhs: ComputeBlock::from_scalar_program_block(spb(
                vec![drv_row],
                "multi_fn_derivative.mo",
            )),
            implicit_rhs: ComputeBlock::from_scalar_program_block(spb(
                vec![impl_row],
                "multi_fn_implicit.mo",
            )),
            ..Default::default()
        },
        ..Default::default()
    }
}

fn compile_or_skip(
    solve: &SolveProblem,
    name: &str,
) -> Option<rumoca_exec_mlir::CompiledMlirResidual> {
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

#[test]
fn multi_fn_all_three_symbols_present() {
    let solve = decay_solve_problem();
    let compiled = match compile_or_skip(&solve, "multi_presence") {
        Some(c) => c,
        None => return,
    };
    assert!(
        compiled.has_implicit_rhs(),
        "eval_implicit_rhs symbol not found in .so"
    );
    assert!(
        compiled.has_jacobian_v(),
        "eval_jacobian_v symbol not found in .so"
    );
}

#[test]
fn multi_fn_implicit_rhs_numerics() {
    let solve = decay_solve_problem();
    let compiled = match compile_or_skip(&solve, "multi_implicit") {
        Some(c) => c,
        None => return,
    };

    assert_eq!(compiled.implicit_rows(), 1);

    for &y0 in &[0.5, 1.0, -2.0, 3.7] {
        let y = [y0];
        let mut out = [0.0f64];
        compiled
            .call_implicit_rhs(&y, &[], 0.0, &mut out)
            .expect("eval_implicit_rhs not present")
            .expect("eval_implicit_rhs call failed");
        let expected = -y0;
        assert!(
            (out[0] - expected).abs() < 1e-12,
            "implicit_rhs at y={y0}: got {}, expected {expected}",
            out[0]
        );
    }
}

#[test]
fn multi_fn_jacobian_v_numerics() {
    let solve = decay_solve_problem();
    let compiled = match compile_or_skip(&solve, "multi_jvp") {
        Some(c) => c,
        None => return,
    };

    // J*seed = -seed[0]  for any y (linear system, Jacobian is constant)
    for &(y0, s0) in &[(1.0, 1.0), (2.0, 0.5), (-1.0, 3.0), (0.0, -2.5)] {
        let y = [y0];
        let seed = [s0];
        let mut out = [0.0f64];
        compiled
            .call_jacobian_v(&y, &[], &seed, 0.0, &mut out)
            .expect("eval_jacobian_v not present")
            .expect("eval_jacobian_v call failed");
        let expected = -s0;
        assert!(
            (out[0] - expected).abs() < 1e-12,
            "jacobian_v at y={y0}, seed={s0}: got {}, expected {expected}",
            out[0]
        );
    }
}

#[test]
fn multi_fn_derivative_still_correct() {
    // Regression: eval_derivative must still work after adding the extra functions.
    let solve = decay_solve_problem();
    let compiled = match compile_or_skip(&solve, "multi_deriv_regress") {
        Some(c) => c,
        None => return,
    };

    for &y0 in &[0.0, 1.0, -3.0, 2.5] {
        let y = [y0];
        let mut out = [0.0f64];
        compiled
            .call(&y, &[], 0.0, &mut out)
            .expect("eval_derivative failed");
        let expected = -y0;
        assert!(
            (out[0] - expected).abs() < 1e-12,
            "derivative at y={y0}: got {}, expected {expected}",
            out[0]
        );
    }
}

#[test]
fn multi_fn_empty_implicit_no_symbol() {
    // A SolveProblem with no implicit_rhs rows should NOT export eval_implicit_rhs.
    let solve = SolveProblem::with_derivative_rhs(ComputeBlock::from_scalar_program_block(spb(
        vec![vec![
            LinearOp::LoadY { dst: 0, index: 0 },
            LinearOp::StoreOutput { src: 0 },
        ]],
        "multi_fn_empty_implicit.mo",
    )));
    // implicit_rhs left as default (empty)

    let compiled = match compile_or_skip(&solve, "multi_empty_impl") {
        Some(c) => c,
        None => return,
    };
    assert!(
        !compiled.has_implicit_rhs(),
        "eval_implicit_rhs should be absent when implicit_rhs is empty"
    );
    assert!(
        !compiled.has_jacobian_v(),
        "eval_jacobian_v should be absent when jacobian is empty"
    );
}
