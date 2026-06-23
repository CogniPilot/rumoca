use rumoca_core::{SourceId, Span};
use rumoca_exec_mlir::{
    CompiledMlirResidual, MlirError, compile_derivative_rhs as exec_compile_derivative_rhs,
};
use rumoca_ir_solve::{
    BinaryOp, ComputeBlock, LinearOp, ScalarProgramBlock, SolveProblem, UnaryOp,
};

fn compile_derivative_rhs(
    solve: &SolveProblem,
    name: &str,
) -> Result<CompiledMlirResidual, MlirError> {
    let artifacts =
        rumoca_phase_solve::lower_solve_artifacts(solve).expect("test solve artifacts lower");
    exec_compile_derivative_rhs(solve, &artifacts, name)
}

fn scalar_program_block(rows: Vec<Vec<LinearOp>>, label: &str) -> ScalarProgramBlock {
    ScalarProgramBlock::with_source_span(
        rows,
        Span::from_offsets(SourceId::from_source_name(label), 0, label.len()),
    )
}

/// Build a SolveProblem whose derivative_rhs computes:
///   xdot = -y[0] + p[0]   (single state, single parameter)
fn simple_decay_solve() -> SolveProblem {
    let row: Vec<LinearOp> = vec![
        LinearOp::LoadY { dst: 0, index: 0 },
        LinearOp::Unary {
            dst: 1,
            op: UnaryOp::Neg,
            arg: 0,
        },
        LinearOp::LoadP { dst: 2, index: 0 },
        LinearOp::Binary {
            dst: 3,
            op: BinaryOp::Add,
            lhs: 1,
            rhs: 2,
        },
        LinearOp::StoreOutput { src: 3 },
    ];
    SolveProblem::with_derivative_rhs(ComputeBlock::from_scalar_program_block(
        scalar_program_block(vec![row], "compile_basic_decay.mo"),
    ))
}

#[test]
fn mlir_derivative_rhs_matches_expected() {
    let solve = simple_decay_solve();
    let result = compile_derivative_rhs(&solve, "decay");

    let compiled = match result {
        Ok(c) => c,
        Err(MlirError::ToolNotFound { tool, .. }) => {
            eprintln!("SKIP: {tool} not found — install mlir-18-tools to run this test");
            return;
        }
        Err(e) => panic!("compile failed: {e}"),
    };

    assert_eq!(compiled.rows(), 1);

    // xdot = -y[0] + p[0] = -1.0 + 2.0 = 1.0
    let y = [1.0f64];
    let p = [2.0f64];
    let mut out = [0.0f64];
    compiled.call(&y, &p, 0.0, &mut out).expect("call failed");
    assert!((out[0] - 1.0).abs() < 1e-12, "expected 1.0 got {}", out[0]);
}

#[test]
fn mlir_derivative_rhs_time_dependency() {
    // xdot = t  (LoadTime)
    let row: Vec<LinearOp> = vec![
        LinearOp::LoadTime { dst: 0 },
        LinearOp::StoreOutput { src: 0 },
    ];
    let solve = SolveProblem::with_derivative_rhs(ComputeBlock::from_scalar_program_block(
        scalar_program_block(vec![row], "compile_basic_time.mo"),
    ));

    let result = compile_derivative_rhs(&solve, "time_dep");
    let compiled = match result {
        Ok(c) => c,
        Err(MlirError::ToolNotFound { tool, .. }) => {
            eprintln!("SKIP: {tool} not found — install mlir-18-tools to run this test");
            return;
        }
        Err(e) => panic!("compile failed: {e}"),
    };

    let y: [f64; 0] = [];
    let p: [f64; 0] = [];
    let mut out = [0.0f64];
    let t = 3.125;
    compiled.call(&y, &p, t, &mut out).expect("call failed");
    assert!((out[0] - t).abs() < 1e-12, "expected {t} got {}", out[0]);
}

#[test]
fn mlir_derivative_rhs_trig() {
    // xdot = sin(y[0])  (Unary::Sin)
    let row: Vec<LinearOp> = vec![
        LinearOp::LoadY { dst: 0, index: 0 },
        LinearOp::Unary {
            dst: 1,
            op: UnaryOp::Sin,
            arg: 0,
        },
        LinearOp::StoreOutput { src: 1 },
    ];
    let solve = SolveProblem::with_derivative_rhs(ComputeBlock::from_scalar_program_block(
        scalar_program_block(vec![row], "compile_basic_trig.mo"),
    ));

    let result = compile_derivative_rhs(&solve, "trig");
    let compiled = match result {
        Ok(c) => c,
        Err(MlirError::ToolNotFound { tool, .. }) => {
            eprintln!("SKIP: {tool} not found — install mlir-18-tools to run this test");
            return;
        }
        Err(e) => panic!("compile failed: {e}"),
    };

    let y = [std::f64::consts::FRAC_PI_2]; // sin(pi/2) = 1.0
    let p: [f64; 0] = [];
    let mut out = [0.0f64];
    compiled.call(&y, &p, 0.0, &mut out).expect("call failed");
    assert!((out[0] - 1.0).abs() < 1e-10, "expected 1.0 got {}", out[0]);
}
