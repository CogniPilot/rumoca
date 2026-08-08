//! Recomputable certificates for post-commit discrete-event settling.

use std::collections::BTreeSet;

use crate::{
    LinearOp, RootRelationRefreshRole, RuntimeAssignmentRole, ScalarProgramBlock, ScalarSlot,
    SolveProblem, SolveProblemShapeContractError,
};

fn validate_count(
    context: &'static str,
    expected: usize,
    actual: usize,
) -> Result<(), SolveProblemShapeContractError> {
    if expected == actual {
        Ok(())
    } else {
        Err(SolveProblemShapeContractError::ScalarProgramCountMismatch {
            context,
            expected,
            actual,
            span: None,
        })
    }
}

fn validate_target(
    context: &'static str,
    row: usize,
    target: ScalarSlot,
    problem: &SolveProblem,
) -> Result<(), SolveProblemShapeContractError> {
    let (storage, index, extent) = match target {
        ScalarSlot::Y { index, .. } => ("Y", index, problem.layout.y_scalars()),
        ScalarSlot::P { index, .. } => ("P", index, problem.layout.p_scalars()),
        ScalarSlot::Time | ScalarSlot::Constant(_) => {
            return Err(SolveProblemShapeContractError::DiscreteCertificate {
                context,
                row,
                detail: "certificate target is not a writable Y/P slot",
                span: None,
            });
        }
    };
    if index < extent {
        Ok(())
    } else {
        Err(SolveProblemShapeContractError::VariableIndexOutOfBounds {
            context,
            storage,
            index,
            extent,
            span: None,
        })
    }
}

pub(crate) fn validate_discrete_certificate_shape(
    problem: &SolveProblem,
) -> Result<(), SolveProblemShapeContractError> {
    let system = &problem.discrete;
    validate_count(
        "discrete.runtime_assignment_targets",
        system.runtime_assignment_rhs.len(),
        system.runtime_assignment_targets.len(),
    )?;
    validate_count(
        "discrete.post_commit_assignment_targets",
        system.post_commit_assignment_rhs.len(),
        system.post_commit_assignment_targets.len(),
    )?;
    validate_count(
        "events.root_relation_memory_targets",
        problem.events.root_conditions.len(),
        problem.events.root_relation_memory_targets.len(),
    )?;
    for (context, block) in [
        (
            "discrete.runtime_assignment_rhs",
            &system.runtime_assignment_rhs,
        ),
        (
            "discrete.post_commit_assignment_rhs",
            &system.post_commit_assignment_rhs,
        ),
    ] {
        crate::variable_bounds::validate_scalar_program_block_variable_bounds(
            block,
            context,
            &problem.layout,
        )?;
    }
    for (row, target) in system
        .runtime_assignment_targets
        .iter()
        .copied()
        .enumerate()
    {
        validate_target("discrete.runtime_assignment_targets", row, target, problem)?;
    }
    let mut relation_memory_seen = BTreeSet::new();
    for (row, &index) in problem
        .solve_layout
        .relation_memory_parameter_indices
        .iter()
        .enumerate()
    {
        validate_target(
            "solve_layout.relation_memory_parameter_indices",
            row,
            crate::scalar_slot_p(index),
            problem,
        )?;
        if !relation_memory_seen.insert(index) {
            return Err(SolveProblemShapeContractError::DiscreteCertificate {
                context: "solve_layout.relation_memory_parameter_indices",
                row,
                detail: "relation-memory P index is duplicated",
                span: None,
            });
        }
    }
    validate_count(
        "discrete.runtime_assignment_roles",
        system.runtime_assignment_rhs.len(),
        system.runtime_assignment_roles.len(),
    )?;
    validate_count(
        "discrete.runtime_assignment_rhs.single_output_rows",
        system.runtime_assignment_rhs.len(),
        system.runtime_assignment_rhs.row_count(),
    )?;
    validate_count(
        "discrete.post_commit_assignment_runtime_rows",
        system.post_commit_assignment_rhs.len(),
        system.post_commit_assignment_runtime_rows.len(),
    )?;
    validate_count(
        "discrete.post_commit_assignment_rhs.single_output_rows",
        system.post_commit_assignment_rhs.len(),
        system.post_commit_assignment_rhs.row_count(),
    )?;
    validate_runtime_assignment_certificates(problem)
}

pub(crate) fn validate_root_certificate_shape(
    problem: &SolveProblem,
) -> Result<(), SolveProblemShapeContractError> {
    let events = &problem.events;
    crate::variable_bounds::validate_scalar_program_block_variable_bounds(
        &events.root_conditions,
        "events.root_conditions",
        &problem.layout,
    )?;
    for (row, target) in events
        .root_relation_memory_targets
        .iter()
        .copied()
        .enumerate()
    {
        let Some(target @ ScalarSlot::P { .. }) = target else {
            if target.is_none() {
                continue;
            }
            return Err(SolveProblemShapeContractError::DiscreteCertificate {
                context: "events.root_relation_memory_targets",
                row,
                detail: "root relation-memory target is not a P slot",
                span: events.root_conditions.program_span(row),
            });
        };
        validate_target("events.root_relation_memory_targets", row, target, problem)?;
    }
    validate_count(
        "events.root_relation_refresh_roles",
        events.root_conditions.len(),
        events.root_relation_refresh_roles.len(),
    )?;
    validate_count(
        "events.root_conditions.single_output_rows",
        events.root_conditions.len(),
        events.root_conditions.row_count(),
    )?;
    validate_root_relation_refresh_certificates(problem)
}

#[derive(Clone, Copy, Debug, Eq, Ord, PartialEq, PartialOrd)]
enum DependencySlot {
    Y(usize),
    P(usize),
}

fn dependency_slot(slot: ScalarSlot) -> Option<DependencySlot> {
    match slot {
        ScalarSlot::Y { index, .. } => Some(DependencySlot::Y(index)),
        ScalarSlot::P { index, .. } => Some(DependencySlot::P(index)),
        ScalarSlot::Time | ScalarSlot::Constant(_) => None,
    }
}

fn program_dependencies(program: &[LinearOp]) -> Option<BTreeSet<DependencySlot>> {
    let mut dependencies = BTreeSet::new();
    for op in program {
        match *op {
            LinearOp::LoadY { index, .. } => {
                dependencies.insert(DependencySlot::Y(index));
            }
            LinearOp::LoadP { index, .. } => {
                dependencies.insert(DependencySlot::P(index));
            }
            LinearOp::LoadIndexedP { base, count, .. } => {
                let end = base.checked_add(count)?;
                dependencies.extend((base..end).map(DependencySlot::P));
            }
            LinearOp::LoadSeed { .. }
            | LinearOp::LoadIndexedSeed { .. }
            | LinearOp::ImpureRandomInit { .. }
            | LinearOp::ImpureRandom { .. }
            | LinearOp::ImpureRandomInteger { .. } => return None,
            LinearOp::Const { .. }
            | LinearOp::LoadTime { .. }
            | LinearOp::Move { .. }
            | LinearOp::LinearSolveComponent { .. }
            | LinearOp::TableBounds { .. }
            | LinearOp::TableLookup { .. }
            | LinearOp::TableLookupSlope { .. }
            | LinearOp::TableNextEvent { .. }
            | LinearOp::RandomInitialState { .. }
            | LinearOp::RandomResult { .. }
            | LinearOp::RandomState { .. }
            | LinearOp::Unary { .. }
            | LinearOp::Binary { .. }
            | LinearOp::Compare { .. }
            | LinearOp::Select { .. }
            | LinearOp::StoreOutput { .. } => {}
        }
    }
    Some(dependencies)
}

fn checked_dependencies(
    rhs: &ScalarProgramBlock,
    context: &'static str,
) -> Result<Vec<BTreeSet<DependencySlot>>, SolveProblemShapeContractError> {
    rhs.programs()
        .iter()
        .enumerate()
        .map(|(row, program)| {
            program_dependencies(program).ok_or(
                SolveProblemShapeContractError::DiscreteCertificate {
                    context,
                    row,
                    detail: "dependency program is not certifiable",
                    span: rhs.program_span(row),
                },
            )
        })
        .collect()
}

/// Derive the row-aligned relation role of runtime B.1c assignment owners.
pub fn derive_runtime_assignment_roles(
    rhs: &ScalarProgramBlock,
    targets: &[ScalarSlot],
    relation_memory_parameter_indices: &[usize],
) -> Result<Vec<RuntimeAssignmentRole>, SolveProblemShapeContractError> {
    let relation_memory = relation_memory_parameter_indices
        .iter()
        .copied()
        .collect::<BTreeSet<_>>();
    let dependencies = checked_dependencies(rhs, "discrete.runtime_assignment_roles")?;
    let mut roles = rhs
        .programs()
        .iter()
        .zip(targets.iter().copied())
        .map(|(program, target)| {
            let writes_relation_memory =
                matches!(target, ScalarSlot::P { index, .. } if relation_memory.contains(&index));
            if writes_relation_memory
                || program
                    .iter()
                    .any(|op| matches!(op, LinearOp::Compare { .. }))
            {
                RuntimeAssignmentRole::RelationEvaluating
            } else {
                RuntimeAssignmentRole::RelationFree
            }
        })
        .collect::<Vec<_>>();

    loop {
        let relation_evaluating_targets = roles
            .iter()
            .zip(targets.iter().copied())
            .filter_map(|(role, target)| {
                (*role == RuntimeAssignmentRole::RelationEvaluating)
                    .then(|| dependency_slot(target))
                    .flatten()
            })
            .collect::<BTreeSet<_>>();
        let mut changed = false;
        for (role, dependencies) in roles.iter_mut().zip(&dependencies) {
            if *role == RuntimeAssignmentRole::RelationFree
                && !dependencies.is_disjoint(&relation_evaluating_targets)
            {
                *role = RuntimeAssignmentRole::RelationEvaluating;
                changed = true;
            }
        }
        if !changed {
            return Ok(roles);
        }
    }
}

/// Prove the root-memory-reachable closure represented by a runtime plan.
pub fn derive_root_reachable_runtime_rows(
    rhs: &ScalarProgramBlock,
    targets: &[ScalarSlot],
    root_relation_memory_targets: &[Option<ScalarSlot>],
    roles: &[RuntimeAssignmentRole],
) -> Result<Vec<bool>, SolveProblemShapeContractError> {
    derive_reachable_runtime_rows(rhs, targets, root_relation_memory_targets, roles, false)
}

fn derive_reachable_runtime_rows(
    rhs: &ScalarProgramBlock,
    targets: &[ScalarSlot],
    root_relation_memory_targets: &[Option<ScalarSlot>],
    roles: &[RuntimeAssignmentRole],
    traverse_relation_evaluating: bool,
) -> Result<Vec<bool>, SolveProblemShapeContractError> {
    let dependencies = checked_dependencies(rhs, "discrete.runtime_assignment_rhs")?;
    let mut reachable = root_relation_memory_targets
        .iter()
        .flatten()
        .copied()
        .filter_map(dependency_slot)
        .collect::<BTreeSet<_>>();
    let mut selected = vec![false; dependencies.len()];
    loop {
        let mut changed = false;
        for (row, dependencies) in dependencies.iter().enumerate() {
            if selected[row]
                || (!traverse_relation_evaluating
                    && roles.get(row) == Some(&RuntimeAssignmentRole::RelationEvaluating))
                || dependencies.is_disjoint(&reachable)
            {
                continue;
            }
            let Some(target) = targets.get(row).copied().and_then(dependency_slot) else {
                return Err(SolveProblemShapeContractError::DiscreteCertificate {
                    context: "discrete.runtime_assignment_targets",
                    row,
                    detail: "root-reachable assignment has no Y/P target",
                    span: rhs.program_span(row),
                });
            };
            selected[row] = true;
            changed |= reachable.insert(target);
        }
        if !changed {
            return Ok(selected);
        }
    }
}

/// Derive root rows whose typed dependency closure reaches algebraic/output Y.
pub fn derive_root_relation_refresh_roles(
    roots: &ScalarProgramBlock,
    runtime_rhs: &ScalarProgramBlock,
    runtime_targets: &[ScalarSlot],
    state_scalar_count: usize,
    solver_scalar_count: usize,
) -> Result<Vec<RootRelationRefreshRole>, SolveProblemShapeContractError> {
    let mut algebraic = (state_scalar_count..solver_scalar_count)
        .map(DependencySlot::Y)
        .collect::<BTreeSet<_>>();
    let dependencies = checked_dependencies(runtime_rhs, "discrete.runtime_assignment_rhs")?;
    let assignments = dependencies
        .into_iter()
        .zip(runtime_targets.iter().copied())
        .enumerate()
        .map(|(row, (dependencies, target))| {
            let target = dependency_slot(target).ok_or(
                SolveProblemShapeContractError::DiscreteCertificate {
                    context: "discrete.runtime_assignment_targets",
                    row,
                    detail: "runtime assignment has no Y/P target",
                    span: runtime_rhs.program_span(row),
                },
            )?;
            Ok((target, dependencies))
        })
        .collect::<Result<Vec<_>, SolveProblemShapeContractError>>()?;
    loop {
        let mut changed = false;
        for (target, dependencies) in &assignments {
            if !dependencies.is_disjoint(&algebraic) {
                changed |= algebraic.insert(*target);
            }
        }
        if !changed {
            break;
        }
    }
    checked_dependencies(roots, "events.root_relation_refresh_roles")?
        .into_iter()
        .map(|dependencies| {
            Ok(if dependencies.is_disjoint(&algebraic) {
                RootRelationRefreshRole::Frozen
            } else {
                RootRelationRefreshRole::AlgebraicDependent
            })
        })
        .collect()
}

pub(crate) fn validate_runtime_assignment_certificates(
    problem: &SolveProblem,
) -> Result<(), SolveProblemShapeContractError> {
    let system = &problem.discrete;
    let derived_roles = derive_runtime_assignment_roles(
        &system.runtime_assignment_rhs,
        &system.runtime_assignment_targets,
        &problem.solve_layout.relation_memory_parameter_indices,
    )?;
    for (row, (actual, expected)) in system
        .runtime_assignment_roles
        .iter()
        .zip(&derived_roles)
        .enumerate()
    {
        if actual != expected {
            return Err(SolveProblemShapeContractError::DiscreteCertificate {
                context: "discrete.runtime_assignment_roles",
                row,
                detail: "role disagrees with independently derived relation dependency",
                span: system.runtime_assignment_rhs.program_span(row),
            });
        }
    }

    let reachable = derive_root_reachable_runtime_rows(
        &system.runtime_assignment_rhs,
        &system.runtime_assignment_targets,
        &problem.events.root_relation_memory_targets,
        &derived_roles,
    )?;
    let broad_reachable = derive_reachable_runtime_rows(
        &system.runtime_assignment_rhs,
        &system.runtime_assignment_targets,
        &problem.events.root_relation_memory_targets,
        &derived_roles,
        true,
    )?;
    if let Some(row) = broad_reachable.iter().position(|reachable| !reachable) {
        return Err(SolveProblemShapeContractError::DiscreteCertificate {
            context: "discrete.runtime_assignment_rhs",
            row,
            detail: "runtime assignment is outside the root-memory-reachable closure",
            span: system.runtime_assignment_rhs.program_span(row),
        });
    }
    let expected_owners = derived_roles
        .iter()
        .zip(reachable)
        .enumerate()
        .filter_map(|(row, (role, reachable))| {
            (*role == RuntimeAssignmentRole::RelationFree && reachable).then_some(row)
        })
        .collect::<Vec<_>>();
    if system.post_commit_assignment_runtime_rows != expected_owners {
        let row = system
            .post_commit_assignment_runtime_rows
            .iter()
            .zip(&expected_owners)
            .position(|(actual, expected)| actual != expected)
            .unwrap_or_else(|| {
                system
                    .post_commit_assignment_runtime_rows
                    .len()
                    .min(expected_owners.len())
            });
        return Err(SolveProblemShapeContractError::DiscreteCertificate {
            context: "discrete.post_commit_assignment_runtime_rows",
            row,
            detail: "owners are not the exact ordered RelationFree runtime projection",
            span: system.post_commit_assignment_rhs.program_span(row),
        });
    }
    for (post_row, &runtime_row) in system
        .post_commit_assignment_runtime_rows
        .iter()
        .enumerate()
    {
        let exact_copy = system.post_commit_assignment_rhs.program(post_row)
            == system.runtime_assignment_rhs.program(runtime_row)
            && system.post_commit_assignment_rhs.program_span(post_row)
                == system.runtime_assignment_rhs.program_span(runtime_row)
            && system.post_commit_assignment_targets.get(post_row)
                == system.runtime_assignment_targets.get(runtime_row);
        if !exact_copy {
            return Err(SolveProblemShapeContractError::DiscreteCertificate {
                context: "discrete.post_commit_assignment_rhs",
                row: post_row,
                detail: "row is not an exact copy of its certified runtime owner",
                span: system.post_commit_assignment_rhs.program_span(post_row),
            });
        }
    }
    Ok(())
}

pub(crate) fn validate_root_relation_refresh_certificates(
    problem: &SolveProblem,
) -> Result<(), SolveProblemShapeContractError> {
    let events = &problem.events;
    let derived_roles = derive_root_relation_refresh_roles(
        &events.root_conditions,
        &problem.discrete.runtime_assignment_rhs,
        &problem.discrete.runtime_assignment_targets,
        problem.solve_layout.state_scalar_count(),
        problem.solve_layout.solver_scalar_count(),
    )?;
    for (row, (actual, expected)) in events
        .root_relation_refresh_roles
        .iter()
        .zip(derived_roles)
        .enumerate()
    {
        if *actual != expected {
            return Err(SolveProblemShapeContractError::DiscreteCertificate {
                context: "events.root_relation_refresh_roles",
                row,
                detail: "role disagrees with independently derived algebraic dependency",
                span: events.root_conditions.program_span(row),
            });
        }
    }
    Ok(())
}
