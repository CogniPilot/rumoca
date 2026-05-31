/// Phase 6.3/6.4: linalg.matmul, Explicit sparsity, and scalarized path comparison.
///
/// Phase 6.3 model: dx/dt = A * x   where A = diag(-1, -2), x(0) = [1, 1]
///   Analytical: x1(t) = exp(-t),  x2(t) = exp(-2t)
///
/// Phase 6.4 model: dx/dt = B * x   where B is 3×3 sparse (5 nonzeros), x = [1,1,1]
///   B = [[1,0,2],[0,3,0],[4,0,5]]   → xdot = [3, 3, 9] at x=[1,1,1]
///
/// Four compute paths are compared:
///   1. Dense MatMul node     → linalg.matmul in MLIR
///   2. Diagonal MatMul node  → element-wise multiply (fast path)
///   3. Explicit MatMul node  → scalar FMA over each nonzero (Phase 6.4)
///   4. Scalarized ScalarProgramBlock   → scalar-row path
use rumoca_exec_mlir::{
    CompiledMlirResidual, MlirError, compile_derivative_rhs as exec_compile_derivative_rhs,
};
use rumoca_ir_solve::{
    ComputeBlock, ComputeNode, LinearOp, ScalarProgramBlock, SolveProblem, SparsityPattern,
};
use std::time::Instant;

// ── helpers ──────────────────────────────────────────────────────────────────

/// Build a `ComputeBlock` with a single `MatMul` node for  xdot = A * x.
///
/// A = [[-1, 0], [0, -2]]  (2×2 dense or diagonal)
/// x  = [x1, x2]           (loaded from y[0], y[1])
fn matmul_block(sparsity: SparsityPattern) -> ComputeBlock {
    // lhs_ops: fill registers 0-3 with A row-major: -1, 0, 0, -2
    let lhs_ops = vec![
        LinearOp::Const {
            dst: 0,
            value: -1.0,
        },
        LinearOp::Const { dst: 1, value: 0.0 },
        LinearOp::Const { dst: 2, value: 0.0 },
        LinearOp::Const {
            dst: 3,
            value: -2.0,
        },
    ];
    // rhs_ops: fill registers 4-5 with x from state vector
    let rhs_ops = vec![
        LinearOp::LoadY { dst: 4, index: 0 },
        LinearOp::LoadY { dst: 5, index: 1 },
    ];
    ComputeBlock {
        nodes: vec![ComputeNode::MatMul {
            lhs_ops,
            lhs_start: 0,
            rhs_ops,
            rhs_start: 4,
            m: 2,
            k: 2,
            n: 1,
            lhs_sparsity: sparsity,
            rhs_sparsity: SparsityPattern::Dense,
            metadata: rumoca_ir_solve::TensorNodeMetadata::default(),
            span: rumoca_core::Span::DUMMY,
        }],
    }
}

/// Build a scalarized `ComputeBlock` for the same  xdot = A * x  system.
///
/// Emits individual scalar rows that compute exactly the same values as the
/// MatMul node but using the scalar-row path.
fn scalar_block() -> ComputeBlock {
    // xdot[0] = -1 * x1
    let row0 = vec![
        LinearOp::Const {
            dst: 0,
            value: -1.0,
        },
        LinearOp::LoadY { dst: 1, index: 0 },
        LinearOp::Binary {
            dst: 2,
            op: rumoca_ir_solve::BinaryOp::Mul,
            lhs: 0,
            rhs: 1,
        },
        LinearOp::StoreOutput { src: 2 },
    ];
    // xdot[1] = -2 * x2
    let row1 = vec![
        LinearOp::Const {
            dst: 0,
            value: -2.0,
        },
        LinearOp::LoadY { dst: 1, index: 1 },
        LinearOp::Binary {
            dst: 2,
            op: rumoca_ir_solve::BinaryOp::Mul,
            lhs: 0,
            rhs: 1,
        },
        LinearOp::StoreOutput { src: 2 },
    ];
    ComputeBlock::from_scalar_program_block(ScalarProgramBlock::new(vec![row0, row1]))
}

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

// ── tests ─────────────────────────────────────────────────────────────────────

fn eval_at(compiled: &rumoca_exec_mlir::CompiledMlirResidual, y: &[f64]) -> Vec<f64> {
    let mut out = vec![0.0; compiled.rows()];
    compiled.call(y, &[], 0.0, &mut out).expect("eval failed");
    out
}

#[test]
fn matmul_dense_numerics_match_analytical() {
    let problem = solve_problem_for(matmul_block(SparsityPattern::Dense));

    let t0 = Instant::now();
    let compiled = match compile_derivative_rhs(&problem, "matmul_dense") {
        Ok(c) => c,
        Err(MlirError::ToolNotFound { tool, .. }) => {
            eprintln!("SKIP: {tool} not found");
            return;
        }
        Err(e) => panic!("compile failed: {e}"),
    };
    let compile_ms = t0.elapsed().as_millis();
    eprintln!("dense MatMul compile: {compile_ms} ms");

    let y = [1.0_f64, 1.0];
    let t_eval = Instant::now();
    let xdot = eval_at(&compiled, &y);
    let eval_us = t_eval.elapsed().as_micros();
    eprintln!("dense MatMul eval:    {eval_us} µs");

    // xdot = A * [1,1] = [-1, -2]
    assert!(
        (xdot[0] - (-1.0)).abs() < 1e-12,
        "xdot[0]: got {}, expected -1.0",
        xdot[0]
    );
    assert!(
        (xdot[1] - (-2.0)).abs() < 1e-12,
        "xdot[1]: got {}, expected -2.0",
        xdot[1]
    );
}

#[test]
fn matmul_diagonal_numerics_match_analytical() {
    let problem = solve_problem_for(matmul_block(SparsityPattern::Diagonal));

    let t0 = Instant::now();
    let compiled = match compile_derivative_rhs(&problem, "matmul_diag") {
        Ok(c) => c,
        Err(MlirError::ToolNotFound { tool, .. }) => {
            eprintln!("SKIP: {tool} not found");
            return;
        }
        Err(e) => panic!("compile failed: {e}"),
    };
    let compile_ms = t0.elapsed().as_millis();
    eprintln!("diagonal MatMul compile: {compile_ms} ms");

    let y = [1.0_f64, 1.0];
    let t_eval = Instant::now();
    let xdot = eval_at(&compiled, &y);
    let eval_us = t_eval.elapsed().as_micros();
    eprintln!("diagonal MatMul eval:    {eval_us} µs");

    assert!(
        (xdot[0] - (-1.0)).abs() < 1e-12,
        "xdot[0]: got {}, expected -1.0",
        xdot[0]
    );
    assert!(
        (xdot[1] - (-2.0)).abs() < 1e-12,
        "xdot[1]: got {}, expected -2.0",
        xdot[1]
    );
}

#[test]
fn matmul_scalar_path_numerics_match_analytical() {
    let problem = solve_problem_for(scalar_block());

    let t0 = Instant::now();
    let compiled = match compile_derivative_rhs(&problem, "matmul_scalar") {
        Ok(c) => c,
        Err(MlirError::ToolNotFound { tool, .. }) => {
            eprintln!("SKIP: {tool} not found");
            return;
        }
        Err(e) => panic!("compile failed: {e}"),
    };
    let compile_ms = t0.elapsed().as_millis();
    eprintln!("scalar rows compile: {compile_ms} ms");

    let y = [1.0_f64, 1.0];
    let t_eval = Instant::now();
    let xdot = eval_at(&compiled, &y);
    let eval_us = t_eval.elapsed().as_micros();
    eprintln!("scalar rows eval:    {eval_us} µs");

    assert!(
        (xdot[0] - (-1.0)).abs() < 1e-12,
        "xdot[0]: got {}, expected -1.0",
        xdot[0]
    );
    assert!(
        (xdot[1] - (-2.0)).abs() < 1e-12,
        "xdot[1]: got {}, expected -2.0",
        xdot[1]
    );
}

#[test]
fn all_three_paths_agree_at_multiple_points() {
    let dense_p = solve_problem_for(matmul_block(SparsityPattern::Dense));
    let diag_p = solve_problem_for(matmul_block(SparsityPattern::Diagonal));
    let scalar_p = solve_problem_for(scalar_block());

    let (dense, diag, scalar) = match (
        compile_derivative_rhs(&dense_p, "agree_dense"),
        compile_derivative_rhs(&diag_p, "agree_diag"),
        compile_derivative_rhs(&scalar_p, "agree_scalar"),
    ) {
        (Ok(a), Ok(b), Ok(c)) => (a, b, c),
        (Err(MlirError::ToolNotFound { tool, .. }), _, _)
        | (_, Err(MlirError::ToolNotFound { tool, .. }), _)
        | (_, _, Err(MlirError::ToolNotFound { tool, .. })) => {
            eprintln!("SKIP: {tool} not found");
            return;
        }
        (Err(e), _, _) | (_, Err(e), _) | (_, _, Err(e)) => panic!("compile failed: {e}"),
    };

    for &(x1, x2) in &[(1.0, 1.0), (0.5, -0.3), (-1.0, 2.0), (0.0, 0.0)] {
        let y = [x1, x2];
        let d = eval_at(&dense, &y);
        let di = eval_at(&diag, &y);
        let s = eval_at(&scalar, &y);

        for i in 0..2 {
            assert!(
                (d[i] - s[i]).abs() < 1e-12,
                "dense vs scalar mismatch at y={y:?}: d[{i}]={}, s[{i}]={}",
                d[i],
                s[i]
            );
            assert!(
                (di[i] - s[i]).abs() < 1e-12,
                "diagonal vs scalar mismatch at y={y:?}: di[{i}]={}, s[{i}]={}",
                di[i],
                s[i]
            );
        }
    }
}

#[test]
fn matmul_euler_integration_matches_analytical() {
    let problem = solve_problem_for(matmul_block(SparsityPattern::Dense));

    let compiled = match compile_derivative_rhs(&problem, "matmul_euler") {
        Ok(c) => c,
        Err(MlirError::ToolNotFound { tool, .. }) => {
            eprintln!("SKIP: {tool} not found");
            return;
        }
        Err(e) => panic!("compile failed: {e}"),
    };

    let dt = 1e-3f64;
    let t_end = 1.0f64;
    let steps = (t_end / dt).round() as usize;
    let mut y = [1.0_f64, 1.0];
    let mut t = 0.0f64;

    for _ in 0..steps {
        let mut xdot = [0.0_f64; 2];
        compiled.call(&y, &[], t, &mut xdot).expect("eval");
        y[0] += dt * xdot[0];
        y[1] += dt * xdot[1];
        t += dt;
    }

    let a1 = (-t_end).exp();
    let a2 = (-2.0 * t_end).exp();

    assert!(
        (y[0] - a1).abs() < 1e-3,
        "x1(1): got {:.6}, expected {:.6}",
        y[0],
        a1
    );
    assert!(
        (y[1] - a2).abs() < 1e-3,
        "x2(1): got {:.6}, expected {:.6}",
        y[1],
        a2
    );
}

// ── Phase 6.4: Explicit sparsity tests ───────────────────────────────────────

/// Build a 3×3 sparse `ComputeBlock` using `SparsityPattern::Explicit`.
///
/// B = [[1, 0, 2],       nnz = {(0,0), (0,2), (1,1), (2,0), (2,2)}
///      [0, 3, 0],
///      [4, 0, 5]]
///
/// x = [x1, x2, x3]  loaded from y[0..2].
/// At x=[1,1,1]: xdot = [1+2, 3, 4+5] = [3, 3, 9].
fn sparse_explicit_block() -> ComputeBlock {
    // lhs_ops: fill registers 0-8 with B row-major: [1,0,2, 0,3,0, 4,0,5]
    let lhs_ops = vec![
        LinearOp::Const { dst: 0, value: 1.0 },
        LinearOp::Const { dst: 1, value: 0.0 },
        LinearOp::Const { dst: 2, value: 2.0 },
        LinearOp::Const { dst: 3, value: 0.0 },
        LinearOp::Const { dst: 4, value: 3.0 },
        LinearOp::Const { dst: 5, value: 0.0 },
        LinearOp::Const { dst: 6, value: 4.0 },
        LinearOp::Const { dst: 7, value: 0.0 },
        LinearOp::Const { dst: 8, value: 5.0 },
    ];
    // rhs_ops: registers 9-11 = x from state vector
    let rhs_ops = vec![
        LinearOp::LoadY { dst: 9, index: 0 },
        LinearOp::LoadY { dst: 10, index: 1 },
        LinearOp::LoadY { dst: 11, index: 2 },
    ];
    ComputeBlock {
        nodes: vec![ComputeNode::MatMul {
            lhs_ops,
            lhs_start: 0,
            rhs_ops,
            rhs_start: 9,
            m: 3,
            k: 3,
            n: 1,
            lhs_sparsity: SparsityPattern::Explicit {
                nnz: vec![(0, 0), (0, 2), (1, 1), (2, 0), (2, 2)],
            },
            rhs_sparsity: SparsityPattern::Dense,
            metadata: rumoca_ir_solve::TensorNodeMetadata::default(),
            span: rumoca_core::Span::DUMMY,
        }],
    }
}

/// Same 3×3 system as above but using the Dense path (linalg.matmul).
fn sparse_dense_block() -> ComputeBlock {
    let lhs_ops = vec![
        LinearOp::Const { dst: 0, value: 1.0 },
        LinearOp::Const { dst: 1, value: 0.0 },
        LinearOp::Const { dst: 2, value: 2.0 },
        LinearOp::Const { dst: 3, value: 0.0 },
        LinearOp::Const { dst: 4, value: 3.0 },
        LinearOp::Const { dst: 5, value: 0.0 },
        LinearOp::Const { dst: 6, value: 4.0 },
        LinearOp::Const { dst: 7, value: 0.0 },
        LinearOp::Const { dst: 8, value: 5.0 },
    ];
    let rhs_ops = vec![
        LinearOp::LoadY { dst: 9, index: 0 },
        LinearOp::LoadY { dst: 10, index: 1 },
        LinearOp::LoadY { dst: 11, index: 2 },
    ];
    ComputeBlock {
        nodes: vec![ComputeNode::MatMul {
            lhs_ops,
            lhs_start: 0,
            rhs_ops,
            rhs_start: 9,
            m: 3,
            k: 3,
            n: 1,
            lhs_sparsity: SparsityPattern::Dense,
            rhs_sparsity: SparsityPattern::Dense,
            metadata: rumoca_ir_solve::TensorNodeMetadata::default(),
            span: rumoca_core::Span::DUMMY,
        }],
    }
}

#[test]
fn explicit_sparse_numerics_match_expected() {
    let problem = solve_problem_for(sparse_explicit_block());

    let t0 = Instant::now();
    let compiled = match compile_derivative_rhs(&problem, "sparse_explicit") {
        Ok(c) => c,
        Err(MlirError::ToolNotFound { tool, .. }) => {
            eprintln!("SKIP: {tool} not found");
            return;
        }
        Err(e) => panic!("compile failed: {e}"),
    };
    let compile_ms = t0.elapsed().as_millis();
    eprintln!("Explicit sparse compile: {compile_ms} ms");

    // At x=[1,1,1]: B*x = [1+2, 3, 4+5] = [3, 3, 9]
    let y = [1.0_f64, 1.0, 1.0];
    let t_eval = Instant::now();
    let xdot = eval_at(&compiled, &y);
    let eval_us = t_eval.elapsed().as_micros();
    eprintln!("Explicit sparse eval:    {eval_us} µs  →  xdot={xdot:?}");

    assert!(
        (xdot[0] - 3.0).abs() < 1e-12,
        "xdot[0]: got {}, expected 3.0",
        xdot[0]
    );
    assert!(
        (xdot[1] - 3.0).abs() < 1e-12,
        "xdot[1]: got {}, expected 3.0",
        xdot[1]
    );
    assert!(
        (xdot[2] - 9.0).abs() < 1e-12,
        "xdot[2]: got {}, expected 9.0",
        xdot[2]
    );
}

#[test]
fn explicit_sparse_agrees_with_dense_at_multiple_points() {
    let explicit_p = solve_problem_for(sparse_explicit_block());
    let dense_p = solve_problem_for(sparse_dense_block());

    let (explicit, dense) = match (
        compile_derivative_rhs(&explicit_p, "sp_agree_explicit"),
        compile_derivative_rhs(&dense_p, "sp_agree_dense"),
    ) {
        (Ok(a), Ok(b)) => (a, b),
        (Err(MlirError::ToolNotFound { tool, .. }), _)
        | (_, Err(MlirError::ToolNotFound { tool, .. })) => {
            eprintln!("SKIP: {tool} not found");
            return;
        }
        (Err(e), _) | (_, Err(e)) => panic!("compile failed: {e}"),
    };

    for &(x1, x2, x3) in &[
        (1.0, 1.0, 1.0),
        (2.0, -1.0, 0.5),
        (0.0, 0.0, 0.0),
        (-1.0, 3.0, -2.0),
    ] {
        let y = [x1, x2, x3];
        let e = eval_at(&explicit, &y);
        let d = eval_at(&dense, &y);
        for i in 0..3 {
            assert!(
                (e[i] - d[i]).abs() < 1e-12,
                "explicit vs dense mismatch at y={y:?}: e[{i}]={}, d[{i}]={}",
                e[i],
                d[i]
            );
        }
    }
}
