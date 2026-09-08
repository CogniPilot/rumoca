/// Tests for MlirBackendOptions: CpuVectorized produces identical results to
/// CpuNative for the decay model, and GPU stubs return ToolNotFound.
use rumoca_core::{SourceId, Span};
use rumoca_exec_mlir::{
    MlirBackendOptions, MlirError, MlirTarget, OptLevel, build_ode_model_with_opts,
};
use rumoca_ir_solve::{ComputeBlock, LinearOp, ScalarProgramBlock, UnaryOp};

use super::support::{derivative, fixture, missing_cpu_tool};

fn spb(rows: Vec<Vec<LinearOp>>, label: &str) -> ScalarProgramBlock {
    ScalarProgramBlock::with_source_span(
        rows,
        Span::from_offsets(SourceId::from_source_name(label), 0, label.len())
            .require_provenance("MLIR options fixture")
            .expect("fixture span is source-backed"),
    )
    .expect("fixture program is computable")
}

fn decay_model() -> std::sync::Arc<rumoca_ir_solve::SolveModel> {
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
        ComputeBlock::from_scalar_program_block(spb(rhs_rows, "options_derivative.mo"));

    let problem = derivative::derivative_problem(
        fixture::ContinuousInventory::new(vec![fixture::FixtureScalar::state("x", 1.0, 1.0)]),
        derivative_rhs_cb,
    );
    fixture::complete_model(problem)
}
#[test]
fn cpu_vectorized_matches_cpu_native() {
    let model = decay_model();

    let native_opts = MlirBackendOptions {
        target: MlirTarget::CpuNative,
        opt_level: OptLevel::O2,
        ..Default::default()
    };
    let vec_opts = MlirBackendOptions {
        target: MlirTarget::CpuVectorized,
        opt_level: OptLevel::O3,
        ..Default::default()
    };

    let native = match build_ode_model_with_opts(
        std::sync::Arc::clone(&model),
        "decay_native",
        &native_opts,
    ) {
        Ok(m) => m,
        Err(MlirError::ToolNotFound { tool, .. }) => {
            missing_cpu_tool::missing_cpu_tool(tool);
            return;
        }
        Err(e) => panic!("native compile failed: {e}"),
    };

    let vectorized = match build_ode_model_with_opts(model, "decay_vec", &vec_opts) {
        Ok(m) => m,
        Err(MlirError::ToolNotFound { tool, .. }) => {
            missing_cpu_tool::missing_cpu_tool(tool);
            return;
        }
        Err(e) => panic!("vectorized compile failed: {e}"),
    };

    // Both should produce identical derivatives: xdot = -x
    for &x_val in &[0.0, 0.5, 1.0, -1.5, 2.75] {
        let y = [x_val];
        let native_xdot = native
            .eval_state_derivatives(0.0, &y)
            .expect("native eval failed");
        let vec_xdot = vectorized
            .eval_state_derivatives(0.0, &y)
            .expect("vec eval failed");
        assert!(
            (native_xdot[0] - vec_xdot[0]).abs() < 1e-14,
            "at x={x_val}: native={} vec={}",
            native_xdot[0],
            vec_xdot[0]
        );
    }
}

#[test]
fn gpu_cuda_returns_tool_not_found() {
    let model = decay_model();
    let opts = MlirBackendOptions {
        target: MlirTarget::GpuCuda,
        opt_level: OptLevel::O2,
        ..Default::default()
    };
    match build_ode_model_with_opts(model, "decay_cuda", &opts) {
        Err(MlirError::ToolNotFound { .. }) => {}
        Err(e) => panic!("expected ToolNotFound, got: {e}"),
        Ok(_) => panic!("expected error for GpuCuda target"),
    }
}

#[test]
fn gpu_rocm_returns_tool_not_found() {
    let model = decay_model();
    let opts = MlirBackendOptions {
        target: MlirTarget::GpuRocm,
        opt_level: OptLevel::O2,
        ..Default::default()
    };
    match build_ode_model_with_opts(model, "decay_rocm", &opts) {
        Err(MlirError::ToolNotFound { .. }) => {}
        Err(e) => panic!("expected ToolNotFound, got: {e}"),
        Ok(_) => panic!("expected error for GpuRocm target"),
    }
}
