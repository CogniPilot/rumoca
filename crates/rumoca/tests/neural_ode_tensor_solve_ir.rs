use rumoca::Compiler;
use rumoca_ir_solve::{ComputeBlock, ComputeNode, LinearOp, ScalarProgramBlock, SolveModel};
use rumoca_sim::SimOptions;

#[test]
fn neural_ode_tensor_indexed_parameter_loads_stay_in_parameter_vector() {
    let result = Compiler::new()
        .model("NeuralODETensor")
        .compile_file(concat!(
            env!("CARGO_MANIFEST_DIR"),
            "/../../examples/models/NeuralODETensor.mo"
        ))
        .expect("NeuralODETensor should compile");
    let opts = SimOptions {
        t_end: 0.2,
        dt: Some(0.02),
        ..SimOptions::default()
    };
    let solve = rumoca_sim::lower_for_simulation_with_overrides(&result.dae, &opts)
        .expect("NeuralODETensor should lower for simulation");

    let p_len = solve.parameters.len();
    let mut failures = Vec::new();
    check_problem_blocks(&solve, p_len, &mut failures);
    check_event_blocks(&solve, p_len, &mut failures);
    check_artifact_blocks(&solve, p_len, &mut failures);
    check_program(
        "visible_value_rows",
        &solve.visible_value_rows,
        p_len,
        &mut failures,
    );

    assert!(
        failures.is_empty(),
        "indexed parameter loads exceed p vector len {p_len}: {}",
        failures.into_iter().take(20).collect::<Vec<_>>().join(", ")
    );
}

fn check_problem_blocks(solve: &SolveModel, p_len: usize, failures: &mut Vec<String>) {
    check_block(
        "continuous.implicit_rhs",
        &solve.problem.continuous.implicit_rhs,
        p_len,
        failures,
    );
    check_block(
        "continuous.residual",
        &solve.problem.continuous.residual,
        p_len,
        failures,
    );
    check_block(
        "continuous.derivative_rhs",
        &solve.problem.continuous.derivative_rhs,
        p_len,
        failures,
    );
    check_block(
        "initialization.residual",
        &solve.problem.initialization.residual,
        p_len,
        failures,
    );
    check_block(
        "initialization.update_rhs",
        &ComputeBlock::from_scalar_program_block(solve.problem.initialization.update_rhs.clone()),
        p_len,
        failures,
    );
}

fn check_event_blocks(solve: &SolveModel, p_len: usize, failures: &mut Vec<String>) {
    check_program(
        "discrete.runtime_assignment_rhs",
        &solve.problem.discrete.runtime_assignment_rhs,
        p_len,
        failures,
    );
    check_program("discrete.rhs", &solve.problem.discrete.rhs, p_len, failures);
    check_program(
        "events.root_conditions",
        &solve.problem.events.root_conditions,
        p_len,
        failures,
    );
    check_program(
        "events.dynamic_time_event_rhs",
        &solve.problem.events.dynamic_time_event_rhs,
        p_len,
        failures,
    );
    check_program(
        "events.action_conditions",
        &solve.problem.events.action_conditions,
        p_len,
        failures,
    );
    for (action_idx, action) in solve.problem.events.actions.iter().enumerate() {
        for (part_idx, part) in action.message.parts.iter().enumerate() {
            if let rumoca_ir_solve::SolveEventMessagePart::Number(ops) = part {
                check_ops(
                    &format!("events.actions[{action_idx}].message[{part_idx}]"),
                    ops,
                    p_len,
                    failures,
                );
            }
        }
    }
}

fn check_artifact_blocks(solve: &SolveModel, p_len: usize, failures: &mut Vec<String>) {
    check_block(
        "artifacts.continuous.implicit_jacobian_v",
        &solve.artifacts.continuous.implicit_jacobian_v,
        p_len,
        failures,
    );
    check_program(
        "artifacts.continuous.implicit_jacobian_v_scalar",
        &solve.artifacts.continuous.implicit_jacobian_v_scalar,
        p_len,
        failures,
    );
    check_program(
        "artifacts.continuous.full_jacobian_v",
        &solve.artifacts.continuous.full_jacobian_v,
        p_len,
        failures,
    );
}

fn check_program(
    label: &str,
    program: &ScalarProgramBlock,
    p_len: usize,
    failures: &mut Vec<String>,
) {
    for (row_idx, row) in program.programs.iter().enumerate() {
        check_ops(&format!("{label}.row[{row_idx}]"), row, p_len, failures);
    }
}

fn check_block(label: &str, block: &ComputeBlock, p_len: usize, failures: &mut Vec<String>) {
    for (node_idx, node) in block.nodes.iter().enumerate() {
        let node_label = format!("{label}.node[{node_idx}]");
        match node {
            ComputeNode::ScalarPrograms(programs) => {
                for (row_idx, row) in programs.programs.iter().enumerate() {
                    check_ops(
                        &format!("{node_label}.row[{row_idx}]"),
                        row,
                        p_len,
                        failures,
                    );
                }
            }
            ComputeNode::MatMul {
                lhs_ops, rhs_ops, ..
            } => {
                check_ops(&format!("{node_label}.lhs"), lhs_ops, p_len, failures);
                check_ops(&format!("{node_label}.rhs"), rhs_ops, p_len, failures);
            }
            ComputeNode::LinSolve { setup_ops, .. } => {
                check_ops(&format!("{node_label}.setup"), setup_ops, p_len, failures);
            }
            ComputeNode::Map { base_ops, .. } | ComputeNode::AffineStencil { base_ops, .. } => {
                check_ops(&format!("{node_label}.base"), base_ops, p_len, failures);
                let scalarized = ComputeBlock {
                    nodes: vec![node.clone()],
                };
                match rumoca_eval_solve::to_scalar_program_block(&scalarized) {
                    Ok(program) => check_program(
                        &format!("{node_label}.scalarized"),
                        &program,
                        p_len,
                        failures,
                    ),
                    Err(err) => failures.push(format!("{node_label}: scalarize failed: {err}")),
                }
            }
        }
    }
}

fn check_ops(label: &str, ops: &[LinearOp], p_len: usize, failures: &mut Vec<String>) {
    for op in ops {
        match *op {
            LinearOp::LoadP { index, .. } if index >= p_len => {
                failures.push(format!("{label}: {op:?}; {}", ops_summary(ops)));
            }
            LinearOp::LoadIndexedP { base, count, .. } => {
                let end = base.saturating_add(count);
                if base >= p_len || end > p_len {
                    failures.push(format!("{label}: {op:?}; {}", ops_summary(ops)));
                }
            }
            _ => {}
        }
    }
}

fn ops_summary(ops: &[LinearOp]) -> String {
    let mut load_p_min = None;
    let mut load_p_max = None;
    let mut load_seed_min = None;
    let mut load_seed_max = None;
    let mut load_p_count = 0usize;
    let mut load_seed_count = 0usize;
    for op in ops {
        match *op {
            LinearOp::LoadP { index, .. } => {
                load_p_min = Some(load_p_min.map_or(index, |value: usize| value.min(index)));
                load_p_max = Some(load_p_max.map_or(index, |value: usize| value.max(index)));
                load_p_count += 1;
            }
            LinearOp::LoadSeed { index, .. } => {
                load_seed_min = Some(load_seed_min.map_or(index, |value: usize| value.min(index)));
                load_seed_max = Some(load_seed_max.map_or(index, |value: usize| value.max(index)));
                load_seed_count += 1;
            }
            _ => {}
        }
    }
    format!(
        "ops={}, LoadP={} {:?}..{:?}, LoadSeed={} {:?}..{:?}",
        ops.len(),
        load_p_count,
        load_p_min,
        load_p_max,
        load_seed_count,
        load_seed_min,
        load_seed_max
    )
}
