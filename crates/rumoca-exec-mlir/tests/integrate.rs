/// End-to-end Phase 4 test: compile an ODE via MLIR, integrate with a simple
/// fixed-step Euler loop, and verify numerics against the analytical solution.
///
/// Model: der(x) = -x   with   x(0) = 1
/// Analytical: x(t) = exp(-t)
use rumoca_exec_mlir::{MlirError, build_ode_model};

/// Build a `SolveModel` for `xdot = -y[0]` (exponential decay) directly
/// from solve-IR rows, mirroring the rk45 test helper pattern.
fn decay_model() -> rumoca_ir_solve::SolveModel {
    use indexmap::IndexMap;
    use rumoca_ir_solve::{
        ComputeBlock, ContinuousSolveSystem, DiscreteSolveSystem, InitializationSolveSystem,
        LinearOp, ScalarProgramBlock, SolveClockPartition, SolveEventPartition, SolveLayout,
        SolveProblem, SolverNameIndexMaps, UnaryOp,
    };

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

    let zero_block = ComputeBlock::from_scalar_program_block(ScalarProgramBlock::new(vec![vec![
        LinearOp::Const { dst: 0, value: 0.0 },
        LinearOp::StoreOutput { src: 0 },
    ]]));
    let zero_rb = ScalarProgramBlock::new(vec![vec![
        LinearOp::Const { dst: 0, value: 0.0 },
        LinearOp::StoreOutput { src: 0 },
    ]]);

    let implicit_rhs_cb =
        ComputeBlock::from_scalar_program_block(ScalarProgramBlock::new(rhs_rows.clone()));
    let derivative_rhs_cb =
        ComputeBlock::from_scalar_program_block(ScalarProgramBlock::new(rhs_rows.clone()));

    rumoca_ir_solve::SolveModel {
        problem: SolveProblem {
            schema_version: rumoca_ir_solve::SOLVE_SCHEMA_VERSION,
            layout: rumoca_ir_solve::VarLayout::from_parts(Default::default(), 1, 1),
            continuous: ContinuousSolveSystem {
                implicit_rhs: implicit_rhs_cb,
                implicit_row_targets: vec![Some(rumoca_ir_solve::scalar_slot_y(0))],
                residual: ScalarProgramBlock::new(rhs_rows),
                derivative_rhs: derivative_rhs_cb,
                algebraic_projection_plan: rumoca_ir_solve::AlgebraicProjectionPlan::default(),
            },
            initialization: InitializationSolveSystem {
                residual: zero_rb.clone(),
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
fn mlir_euler_decay_matches_analytical() {
    let model = decay_model();

    let compiled = match build_ode_model(&model, "decay") {
        Ok(c) => c,
        Err(MlirError::ToolNotFound { tool, .. }) => {
            eprintln!("SKIP: {tool} not found");
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

    let compiled = match build_ode_model(&model, "decay_pts") {
        Ok(c) => c,
        Err(MlirError::ToolNotFound { tool, .. }) => {
            eprintln!("SKIP: {tool} not found");
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
