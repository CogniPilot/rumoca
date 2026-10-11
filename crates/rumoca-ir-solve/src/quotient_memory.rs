//! Check numeric event-buffer ownership independently of source lowering.

use super::*;

pub(crate) fn validate(problem: &SolveProblem) -> Result<(), SolveProblemShapeContractError> {
    let discrete = &problem.discrete;
    let mut targets = BTreeSet::new();
    for (row, role) in discrete.row_roles.iter().enumerate() {
        let DiscreteRowRole::QuotientMemory { step } = role else {
            continue;
        };
        let target = discrete.update_targets[row];
        let ScalarSlot::P { index, .. } = target else {
            return Err(event_iteration_contract(
                row,
                "quotient memory is not P-backed",
            ));
        };
        if !matches!(step, UnaryOp::Floor | UnaryOp::Trunc)
            || storage_variable_for_slot(&problem.solve_layout, target)
                .map_err(|detail| event_iteration_contract(row, detail))?
                .is_some()
            || problem
                .events
                .condition_memory_parameter_indices
                .contains(&index)
            || problem
                .solve_layout
                .relation_memory_parameter_indices
                .contains(&index)
            || problem.clocks.activation_parameter_indices.contains(&index)
            || !targets.insert(index)
            || discrete
                .update_targets
                .iter()
                .filter(|&&other| other == target)
                .count()
                != 1
            || discrete.clock_owners[row].is_some()
            || discrete.pre_modes[row] != DiscreteEventPreMode::FollowCurrent
            || discrete.observation_refresh[row]
        {
            return Err(event_iteration_contract(
                row,
                "quotient memory has no exclusive numeric event owner",
            ));
        }
        let program = one_output_program(&discrete.rhs, row).ok_or_else(|| {
            event_iteration_contract(row, "quotient memory has no single-output program")
        })?;
        if !matches!(program, [.., LinearOp::Unary { dst, op, .. }, LinearOp::StoreOutput { src }] if dst == src && op == step)
        {
            return Err(event_iteration_contract(
                row,
                "quotient memory does not publish its declared rounding step",
            ));
        }
        let initialization = &problem.initialization;
        let mut owners = initialization
            .update_targets()
            .iter()
            .enumerate()
            .filter(|(_, other)| **other == target);
        let Some((initial_row, _)) = owners.next() else {
            return Err(event_iteration_contract(
                row,
                "quotient memory has no initialization owner",
            ));
        };
        if owners.next().is_some()
            || one_output_program(initialization.update_rhs(), initial_row) != Some(program)
        {
            return Err(event_iteration_contract(
                row,
                "quotient memory initialization does not match its event producer",
            ));
        }
    }
    Ok(())
}

fn one_output_program(block: &ScalarProgramBlock, row: usize) -> Option<&[LinearOp]> {
    let binding = block
        .output_bindings()
        .find(|binding| binding.logical_index == row)?;
    (binding.output_count == 1).then(|| block.programs()[binding.program].as_slice())
}
