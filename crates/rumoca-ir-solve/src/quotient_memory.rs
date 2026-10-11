//! Check numeric event-buffer ownership independently of source lowering.

use super::*;
use std::collections::BTreeMap;

mod activation;
mod structured;

type Outputs<'a> = BTreeMap<usize, (&'a [LinearOp], usize, usize)>;

pub(crate) fn validate(problem: &SolveProblem) -> Result<(), SolveProblemShapeContractError> {
    let discrete = &problem.discrete;
    if !discrete
        .row_roles
        .iter()
        .any(|role| matches!(role, DiscreteRowRole::QuotientMemory { .. }))
    {
        return structured::validate(problem);
    }
    let programs = outputs(&discrete.rhs);
    let roots = outputs(&problem.events.root_conditions);
    let initialization = outputs(problem.initialization.update_rhs());
    let target_counts = parameter_target_counts(&discrete.update_targets);
    let initial_targets = initial_parameter_owners(problem.initialization.update_targets());
    let reserved = reserved_parameters(problem);
    let mut owned_roots = BTreeSet::new();
    for (row, role) in discrete.row_roles.iter().enumerate() {
        let DiscreteRowRole::QuotientMemory { step, root_index } = *role else {
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
            || reserved.contains(&index)
            || target_counts.get(&index) != Some(&1)
            || !owned_roots.insert(root_index)
            || discrete.clock_owners[row].is_some()
            || discrete.pre_modes[row] != DiscreteEventPreMode::FollowCurrent
            || discrete.observation_refresh[row]
        {
            return Err(event_iteration_contract(
                row,
                "quotient memory has no exclusive numeric event owner",
            ));
        }
        let program = single_output(&programs, row).ok_or_else(|| {
            event_iteration_contract(row, "quotient memory has no single-output program")
        })?;
        validate_root(row, step, root_index, index, program, &roots)?;
        let Some(Some(initial_row)) = initial_targets.get(&index) else {
            return Err(event_iteration_contract(
                row,
                "quotient memory has no unique initialization owner",
            ));
        };
        if single_output(&initialization, *initial_row) != Some(program) {
            return Err(event_iteration_contract(
                row,
                "quotient memory initialization does not match its event producer",
            ));
        }
    }
    structured::validate(problem)
}

fn validate_root(
    row: usize,
    step: UnaryOp,
    root: usize,
    target: usize,
    program: &[LinearOp],
    roots: &Outputs<'_>,
) -> Result<(), SolveProblemShapeContractError> {
    let expected = activation::indicator(row, step, program, target, None)?;
    let Some(&(found, output, _)) = roots.get(&root) else {
        return Err(event_iteration_contract(
            row,
            "quotient memory has no live root owner",
        ));
    };
    crate::linear_op::check_same_output(&expected, 0, found, output).map_err(|_| {
        event_iteration_contract(
            row,
            "quotient root does not monitor its producer's exact live ratio",
        )
    })
}

/// The checked quotient owner uses `sin(pi * ratio) >= 0`. Its signed Solve
/// indicator is `0 - sin(pi * ratio)`, with the same authored operation order.
fn ratio_indicator(prefix: &[LinearOp], ratio: Reg) -> Option<Vec<LinearOp>> {
    let first = Reg::try_from(
        ScalarProgramRegisterFlow::derive(prefix)
            .ok()?
            .register_count(),
    )
    .ok()?;
    ratio_indicator_at(prefix, ratio, first)
}

fn ratio_indicator_at(prefix: &[LinearOp], ratio: Reg, first: Reg) -> Option<Vec<LinearOp>> {
    let pi = first;
    let phase = first.checked_add(1)?;
    let sine = first.checked_add(2)?;
    let zero = first.checked_add(3)?;
    let output = first.checked_add(4)?;
    let mut expected = prefix.to_vec();
    expected.extend([
        LinearOp::Const {
            dst: pi,
            value: std::f64::consts::PI,
        },
        LinearOp::Binary {
            dst: phase,
            op: BinaryOp::Mul,
            lhs: pi,
            rhs: ratio,
        },
        LinearOp::Unary {
            dst: sine,
            op: UnaryOp::Sin,
            arg: phase,
        },
        LinearOp::Const {
            dst: zero,
            value: 0.0,
        },
        LinearOp::Binary {
            dst: output,
            op: BinaryOp::Sub,
            lhs: zero,
            rhs: sine,
        },
        LinearOp::StoreOutput { src: output },
    ]);
    Some(expected)
}

fn parameter_target_counts(targets: &[ScalarSlot]) -> BTreeMap<usize, usize> {
    let mut counts = BTreeMap::new();
    for target in targets {
        if let ScalarSlot::P { index, .. } = target {
            *counts.entry(*index).or_default() += 1;
        }
    }
    counts
}

fn initial_parameter_owners(targets: &[ScalarSlot]) -> BTreeMap<usize, Option<usize>> {
    let mut owners = BTreeMap::new();
    for (row, target) in targets.iter().enumerate() {
        if let ScalarSlot::P { index, .. } = target {
            owners
                .entry(*index)
                .and_modify(|owner| *owner = None)
                .or_insert(Some(row));
        }
    }
    owners
}

fn reserved_parameters(problem: &SolveProblem) -> BTreeSet<usize> {
    let layout = &problem.solve_layout;
    let discrete = &problem.discrete;
    layout
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
        .chain(layout.initial_event_parameter_index)
        .chain(layout.terminal_event_parameter_index)
        .chain(layout.initial_homotopy_parameter_index)
        .chain(
            discrete
                .runtime_assignment_targets
                .iter()
                .chain(&discrete.post_commit_assignment_targets)
                .chain(&discrete.clock_partition_intermediate_targets)
                .filter_map(|target| match target {
                    ScalarSlot::P { index, .. } => Some(*index),
                    _ => None,
                }),
        )
        .collect()
}

fn outputs(block: &ScalarProgramBlock) -> Outputs<'_> {
    block
        .output_bindings()
        .map(|binding| {
            (
                binding.logical_index,
                (
                    block.programs()[binding.program].as_slice(),
                    binding.offset,
                    binding.output_count,
                ),
            )
        })
        .collect()
}

fn single_output<'a>(outputs: &Outputs<'a>, row: usize) -> Option<&'a [LinearOp]> {
    let &(program, offset, count) = outputs.get(&row)?;
    (offset == 0 && count == 1).then_some(program)
}

#[cfg(test)]
mod tests;
