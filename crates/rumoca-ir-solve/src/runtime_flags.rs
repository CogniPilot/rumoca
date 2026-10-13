//! Root-owned storage proof for the initialization execution mode.

use super::*;

pub(crate) fn validate(problem: &SolveProblem) -> Result<(), SolveProblemShapeContractError> {
    let layout = &problem.solve_layout;
    let Some(flag) = layout.initial_event_parameter_index else {
        return Ok(());
    };
    validate_indices(
        "solve_layout.initial_event_parameter_index",
        &[flag],
        problem.layout.p_scalars(),
    )?;
    if flag >= layout.compiled_parameter_len
        || flag < layout.parameter_count
        || ProducerStorage::new(layout)
            .variable(scalar_slot_p(flag))
            .map_err(|detail| ownership(flag, detail))?
            .is_some()
    {
        return Err(ownership(
            flag,
            "initialization mode overlaps source storage",
        ));
    }
    let memory = layout
        .relation_memory_parameter_indices
        .iter()
        .copied()
        .chain(
            problem
                .events
                .condition_memory_parameter_indices
                .iter()
                .copied(),
        )
        .chain(problem.clocks.activation_parameter_indices.iter().copied())
        .chain(
            problem
                .events
                .delays
                .value_parameter_indices
                .iter()
                .copied(),
        )
        .chain(
            layout
                .pre_param_bindings
                .iter()
                .map(|binding| binding.dest_p_index),
        )
        .chain(layout.terminal_event_parameter_index)
        .chain(layout.initial_homotopy_parameter_index);
    if memory.into_iter().any(|index| index == flag) {
        return Err(ownership(
            flag,
            "initialization mode overlaps runtime memory",
        ));
    }
    validate_writers(problem, flag)
}

fn validate_writers(
    problem: &SolveProblem,
    flag: usize,
) -> Result<(), SolveProblemShapeContractError> {
    let discrete = &problem.discrete;
    let initialization = &problem.initialization;
    let targets = discrete
        .runtime_assignment_targets
        .iter()
        .chain(&discrete.post_commit_assignment_targets)
        .chain(&discrete.update_targets)
        .chain(&discrete.clock_partition_intermediate_targets)
        .chain(initialization.update_targets())
        .chain(initialization.projection_unknowns())
        .chain(initialization.row_targets().iter().flatten())
        .chain(
            initialization
                .projection_plan()
                .blocks
                .iter()
                .flat_map(|block| &block.unknowns),
        );
    for &target in targets {
        exclude_range(flag, target, 1)?;
    }
    for program in &discrete.guarded_assignments {
        for range in program.target_ranges() {
            exclude_range(flag, range.base(), range.count())?;
        }
    }
    for transaction in &discrete.event_transactions {
        for target in transaction.targets() {
            exclude_range(
                flag,
                target.base(),
                target.value_type().scalar_count() as usize,
            )?;
        }
    }
    for update in &discrete.structured_updates {
        let Some(ComputeNode::Map { domain, .. }) =
            discrete.structured_rhs.nodes.get(update.node_index)
        else {
            return Err(ownership(
                flag,
                "structured writer has no checked map domain",
            ));
        };
        let count = update
            .target
            .map
            .output_count(domain)
            .map_err(|_| ownership(flag, "structured writer has invalid affine bounds"))?;
        exclude_range(flag, update.target.base, count)?;
    }
    Ok(())
}

fn exclude_range(
    flag: usize,
    base: ScalarSlot,
    count: usize,
) -> Result<(), SolveProblemShapeContractError> {
    if let ScalarSlot::P { index, .. } = base {
        let end = index
            .checked_add(count)
            .ok_or_else(|| ownership(flag, "writer storage range overflowed"))?;
        if (index..end).contains(&flag) {
            return Err(ownership(
                flag,
                "initialization mode has an equation writer",
            ));
        }
    }
    Ok(())
}

fn ownership(flag: usize, detail: &'static str) -> SolveProblemShapeContractError {
    SolveProblemShapeContractError::DiscreteCertificate {
        context: "solve_layout.initial_event_parameter_index",
        row: flag,
        detail,
        span: None,
    }
}
