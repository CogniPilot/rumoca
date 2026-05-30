/// Phase 6.5: ComputeNode::LinSolve and LinearSolveComponent end-to-end tests.
///
/// Model: xdot = A⁻¹ · b(y)   where A = [[2,1],[1,3]]
///
/// A is constant; b is loaded from the state vector y.
/// At y = [5, 10]:  A⁻¹·[5,10] = [1, 3]
///   (2·1 + 1·3 = 5 ✓,  1·1 + 3·3 = 10 ✓)
///
/// Three paths are tested:
///   1. ComputeNode::LinSolve   — node-aware MLIR path via render_linsolve_mlir
///   2. Scalarized ScalarProgramBlock     — scalar-row path with LinearSolveComponent ops
///   3. Analytical reference    — manual computation for cross-check
use rumoca_exec_mlir::{
    CompiledMlirResidual, MlirError, compile_derivative_rhs as exec_compile_derivative_rhs,
};
use rumoca_ir_solve::{ComputeBlock, ComputeNode, LinearOp, SolveProblem};

// ── helpers ──────────────────────────────────────────────────────────────────

fn solve_problem_for(derivative_rhs: ComputeBlock) -> SolveProblem {
    SolveProblem::with_derivative_rhs(derivative_rhs)
}

fn compile_derivative_rhs(
    solve: &SolveProblem,
    name: &str,
) -> Result<CompiledMlirResidual, MlirError> {
    let artifacts =
        rumoca_phase_solve::lower_solve_artifacts(solve).expect("test solve artifacts lower");
    exec_compile_derivative_rhs(solve, &artifacts, name)
}

fn eval_at(compiled: &rumoca_exec_mlir::CompiledMlirResidual, y: &[f64]) -> Vec<f64> {
    let mut out = vec![0.0; compiled.rows()];
    compiled.call(y, &[], 0.0, &mut out).expect("eval failed");
    out
}

/// Build `ComputeNode::LinSolve` for xdot = A⁻¹ · b(y).
///
/// A = [[2, 1], [1, 3]]  (registers 0-3, row-major)
/// b = [y[0], y[1]]       (registers 4-5)
fn linsolve_block() -> ComputeBlock {
    // setup_ops loads A and b into registers 0..5
    let setup_ops = vec![
        // A row-major: [2, 1, 1, 3]
        LinearOp::Const { dst: 0, value: 2.0 },
        LinearOp::Const { dst: 1, value: 1.0 },
        LinearOp::Const { dst: 2, value: 1.0 },
        LinearOp::Const { dst: 3, value: 3.0 },
        // b = y[0], y[1]
        LinearOp::LoadY { dst: 4, index: 0 },
        LinearOp::LoadY { dst: 5, index: 1 },
    ];
    ComputeBlock {
        nodes: vec![ComputeNode::LinSolve {
            setup_ops,
            matrix_start: 0,
            rhs_start: 4,
            n: 2,
            next_reg: 6,
            metadata: rumoca_ir_solve::TensorNodeMetadata::default(),
            span: rumoca_core::Span::DUMMY,
        }],
    }
}

/// Scalarized version — exercises LinearSolveComponent.
fn linsolve_scalar_block() -> ComputeBlock {
    let node_block = linsolve_block();
    ComputeBlock::from_scalar_program_block(rumoca_eval_solve::to_scalar_program_block(&node_block))
}

// ── tests ─────────────────────────────────────────────────────────────────────

#[test]
fn linsolve_node_numerics_match_analytical() {
    let problem = solve_problem_for(linsolve_block());

    let compiled = match compile_derivative_rhs(&problem, "linsolve_node") {
        Ok(c) => c,
        Err(MlirError::ToolNotFound { tool, .. }) => {
            eprintln!("SKIP: {tool} not found");
            return;
        }
        Err(e) => panic!("compile failed: {e}"),
    };

    // At y=[5,10]: A⁻¹·[5,10] = [1,3]
    let y = [5.0_f64, 10.0];
    let xdot = eval_at(&compiled, &y);
    assert_eq!(xdot.len(), 2, "expected 2 outputs");
    assert!(
        (xdot[0] - 1.0).abs() < 1e-10,
        "xdot[0]: got {}, expected 1.0",
        xdot[0]
    );
    assert!(
        (xdot[1] - 3.0).abs() < 1e-10,
        "xdot[1]: got {}, expected 3.0",
        xdot[1]
    );
}

#[test]
fn linsolve_scalar_path_numerics_match_analytical() {
    let problem = solve_problem_for(linsolve_scalar_block());

    let compiled = match compile_derivative_rhs(&problem, "linsolve_scalar") {
        Ok(c) => c,
        Err(MlirError::ToolNotFound { tool, .. }) => {
            eprintln!("SKIP: {tool} not found");
            return;
        }
        Err(e) => panic!("compile failed: {e}"),
    };

    // At y=[5,10]: A⁻¹·[5,10] = [1,3]
    let y = [5.0_f64, 10.0];
    let xdot = eval_at(&compiled, &y);
    assert_eq!(xdot.len(), 2, "expected 2 outputs");
    assert!(
        (xdot[0] - 1.0).abs() < 1e-10,
        "xdot[0]: got {}, expected 1.0",
        xdot[0]
    );
    assert!(
        (xdot[1] - 3.0).abs() < 1e-10,
        "xdot[1]: got {}, expected 3.0",
        xdot[1]
    );
}

#[test]
fn linsolve_node_and_scalar_agree_at_multiple_points() {
    let node_p = solve_problem_for(linsolve_block());
    let scalar_p = solve_problem_for(linsolve_scalar_block());

    let (node_c, scalar_c) = match (
        compile_derivative_rhs(&node_p, "ls_agree_node"),
        compile_derivative_rhs(&scalar_p, "ls_agree_scalar"),
    ) {
        (Ok(a), Ok(b)) => (a, b),
        (Err(MlirError::ToolNotFound { tool, .. }), _)
        | (_, Err(MlirError::ToolNotFound { tool, .. })) => {
            eprintln!("SKIP: {tool} not found");
            return;
        }
        (Err(e), _) | (_, Err(e)) => panic!("compile failed: {e}"),
    };

    // Various RHS vectors
    for &(b0, b1) in &[(5.0, 10.0), (1.0, 0.0), (0.0, 1.0), (3.0, 7.0), (-2.0, 4.0)] {
        let y = [b0, b1];
        let n = eval_at(&node_c, &y);
        let s = eval_at(&scalar_c, &y);
        for i in 0..2 {
            assert!(
                (n[i] - s[i]).abs() < 1e-10,
                "node vs scalar at y={y:?}: n[{i}]={}, s[{i}]={}",
                n[i],
                s[i]
            );
        }
    }
}

#[test]
fn linsolve_partial_pivoting_correctness() {
    // Use a system where partial pivoting matters:
    // A = [[0.001, 1], [1, 0]] — small pivot in top-left
    // Without pivoting, catastrophic cancellation; with pivoting, exact.
    // A^{-1} · [1, 0] ≈ [0, 1]  (swap rows first, then eliminate)
    // More precisely: A·[0,1]^T = [1,0] (since 0.001·0 + 1·1 = 1; 1·0 + 0·1 = 0)
    let setup_ops = vec![
        LinearOp::Const {
            dst: 0,
            value: 0.001,
        },
        LinearOp::Const { dst: 1, value: 1.0 },
        LinearOp::Const { dst: 2, value: 1.0 },
        LinearOp::Const { dst: 3, value: 0.0 },
        LinearOp::Const { dst: 4, value: 1.0 },
        LinearOp::Const { dst: 5, value: 0.0 },
    ];
    let block = ComputeBlock {
        nodes: vec![ComputeNode::LinSolve {
            setup_ops,
            matrix_start: 0,
            rhs_start: 4,
            n: 2,
            next_reg: 6,
            metadata: rumoca_ir_solve::TensorNodeMetadata::default(),
            span: rumoca_core::Span::DUMMY,
        }],
    };
    let problem = solve_problem_for(block);

    let compiled = match compile_derivative_rhs(&problem, "ls_pivot") {
        Ok(c) => c,
        Err(MlirError::ToolNotFound { tool, .. }) => {
            eprintln!("SKIP: {tool} not found");
            return;
        }
        Err(e) => panic!("compile failed: {e}"),
    };

    let y = [0.0_f64; 2]; // b is loaded from constants in setup_ops (b=[1,0])
    let xdot = eval_at(&compiled, &y);
    // Expected: A^{-1}·[1,0] = [0, 1] (column 0 of A^{-1})
    assert!(
        (xdot[0] - 0.0).abs() < 1e-8,
        "xdot[0]: got {}, expected 0.0",
        xdot[0]
    );
    assert!(
        (xdot[1] - 1.0).abs() < 1e-8,
        "xdot[1]: got {}, expected 1.0",
        xdot[1]
    );
}
