use indexmap::IndexMap;
/// Tests for MlirBackendOptions: CpuVectorized produces identical results to
/// CpuNative for the decay model, and GPU stubs return ToolNotFound.
use rumoca_core::{SourceId, Span};
use rumoca_exec_mlir::{
    MlirBackendOptions, MlirError, MlirTarget, OptLevel, build_ode_model_with_opts,
};
use rumoca_ir_solve::{
    ComputeBlock, ContinuousSolveSystem, DiscreteSolveSystem, InitializationSolveSystem, LinearOp,
    ScalarProgramBlock, SolveClockPartition, SolveEventPartition, SolveLayout, SolveProblem,
    SolverNameIndexMaps, UnaryOp,
};

fn spb(rows: Vec<Vec<LinearOp>>, label: &str) -> ScalarProgramBlock {
    ScalarProgramBlock::with_source_span(
        rows,
        Span::from_offsets(SourceId::from_source_name(label), 0, label.len()),
    )
}

fn decay_model() -> rumoca_ir_solve::SolveModel {
    let rhs_rows = vec![vec![
        LinearOp::LoadY { dst: 0, index: 0 },
        LinearOp::Unary {
            dst: 1,
            op: UnaryOp::Neg,
            arg: 0,
        },
        LinearOp::StoreOutput { src: 1 },
    ]];
    let zero_rb = spb(
        vec![vec![
            LinearOp::Const { dst: 0, value: 0.0 },
            LinearOp::StoreOutput { src: 0 },
        ]],
        "options_zero_row.mo",
    );
    let zero_block = ComputeBlock::from_scalar_program_block(zero_rb.clone());

    let implicit_rhs_cb =
        ComputeBlock::from_scalar_program_block(spb(rhs_rows.clone(), "options_implicit.mo"));
    let derivative_rhs_cb =
        ComputeBlock::from_scalar_program_block(spb(rhs_rows.clone(), "options_derivative.mo"));

    rumoca_ir_solve::SolveModel {
        problem: SolveProblem {
            schema_version: rumoca_ir_solve::SOLVE_SCHEMA_VERSION,
            layout: rumoca_ir_solve::VarLayout::from_parts(Default::default(), 1, 1),
            continuous: ContinuousSolveSystem {
                implicit_rhs: implicit_rhs_cb,
                implicit_row_targets: vec![Some(rumoca_ir_solve::scalar_slot_y(0))],
                residual: ComputeBlock::from_scalar_program_block(spb(
                    rhs_rows,
                    "options_residual.mo",
                )),
                derivative_rhs: derivative_rhs_cb,
                algebraic_projection_plan: rumoca_ir_solve::AlgebraicProjectionPlan::default(),
            },
            initialization: InitializationSolveSystem {
                residual: ComputeBlock::from_scalar_program_block(zero_rb.clone()),
                row_targets: Vec::new(),
                projection_indices: Vec::new(),
                projection_plan: rumoca_ir_solve::AlgebraicProjectionPlan::default(),
                update_rhs: ScalarProgramBlock::default(),
                update_targets: Vec::new(),
            },
            discrete: DiscreteSolveSystem {
                rhs: zero_rb.clone(),
                ..Default::default()
            },
            events: SolveEventPartition::default(),
            clocks: SolveClockPartition::default(),
            solve_layout: SolveLayout {
                solver_maps: SolverNameIndexMaps {
                    names: vec!["x".to_string()],
                    name_to_idx: IndexMap::from([("x".to_string(), 0)]),
                    base_to_indices: IndexMap::from([("x".to_string(), vec![0])]),
                },
                state_scalar_count: 1,
                algebraic_scalar_count: 0,
                output_scalar_count: 0,
                parameter_count: 0,
                compiled_parameter_len: 0,
                input_scalar_names: Vec::new(),
                discrete_real_scalar_names: Vec::new(),
                discrete_valued_scalar_names: Vec::new(),
                relation_memory_parameter_indices: Vec::new(),
                initial_event_parameter_index: None,
                pre_param_bindings: Vec::new(),
            },
        },
        artifacts: rumoca_ir_solve::SolveArtifacts {
            continuous: rumoca_ir_solve::ContinuousSolveArtifacts {
                mass_matrix: vec![vec![1.0]],
                implicit_jacobian_v: zero_block,
                implicit_jacobian_v_scalar: zero_rb.clone(),
                full_jacobian_v: zero_rb.clone(),
            },
            ..Default::default()
        },
        initial_y: vec![1.0],
        parameters: Vec::new(),
        external_tables: rumoca_ir_solve::ExternalTables::default(),
        visible_names: vec!["x".to_string()],
        visible_value_rows: ScalarProgramBlock::default(),
        variable_meta: Vec::new(),
    }
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

    let native = match build_ode_model_with_opts(&model, "decay_native", &native_opts) {
        Ok(m) => m,
        Err(MlirError::ToolNotFound { tool, .. }) => {
            eprintln!("SKIP: {tool} not found");
            return;
        }
        Err(e) => panic!("native compile failed: {e}"),
    };

    let vectorized = match build_ode_model_with_opts(&model, "decay_vec", &vec_opts) {
        Ok(m) => m,
        Err(MlirError::ToolNotFound { tool, .. }) => {
            eprintln!("SKIP: {tool} not found");
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
    match build_ode_model_with_opts(&model, "decay_cuda", &opts) {
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
    match build_ode_model_with_opts(&model, "decay_rocm", &opts) {
        Err(MlirError::ToolNotFound { .. }) => {}
        Err(e) => panic!("expected ToolNotFound, got: {e}"),
        Ok(_) => panic!("expected error for GpuRocm target"),
    }
}
