use super::*;

pub(super) fn derive_integrator_history_effects(
    discrete: &mut solve::DiscreteSolveSystem,
    continuous: &solve::ContinuousSolveSystem,
    state_scalar_count: usize,
) {
    let Some(sensitive) = integrator_history_sensitive_slots(
        continuous,
        &discrete.runtime_assignment_rhs,
        &discrete.runtime_assignment_targets,
        state_scalar_count,
    ) else {
        // Rows are initialized fail-closed. An incomplete compact dependency
        // proof cannot manufacture `Preserve`.
        return;
    };

    for (effect, target) in discrete
        .integrator_history_effects
        .iter_mut()
        .zip(discrete.update_targets.iter().copied())
    {
        *effect = integrator_history_effect_for_target(target, &sensitive, state_scalar_count);
    }

    for update_index in 0..discrete.structured_updates.len() {
        let effect = match discrete.structured_assignments(update_index) {
            Ok(assignments) => assignments
                .into_iter()
                .map(|(target, _)| {
                    integrator_history_effect_for_target(target, &sensitive, state_scalar_count)
                })
                .fold(
                    solve::IntegratorHistoryEffect::Preserve,
                    join_integrator_history_effect,
                ),
            Err(_) => solve::IntegratorHistoryEffect::Restart,
        };
        discrete.structured_updates[update_index].integrator_history_effect = effect;
    }
}

#[derive(Clone, Copy, Debug, Eq, Ord, PartialEq, PartialOrd)]
pub(super) enum HistoryDependencySlot {
    Y(usize),
    P(usize),
}

fn integrator_history_effect_for_target(
    target: solve::ScalarSlot,
    sensitive: &BTreeSet<HistoryDependencySlot>,
    state_scalar_count: usize,
) -> solve::IntegratorHistoryEffect {
    let dependency = match target {
        solve::ScalarSlot::Y { index, .. } => HistoryDependencySlot::Y(index),
        solve::ScalarSlot::P { index, .. } => HistoryDependencySlot::P(index),
        solve::ScalarSlot::Time | solve::ScalarSlot::Constant(_) => {
            return solve::IntegratorHistoryEffect::Restart;
        }
    };
    if matches!(dependency, HistoryDependencySlot::Y(index) if index < state_scalar_count)
        || sensitive.contains(&dependency)
    {
        solve::IntegratorHistoryEffect::Restart
    } else {
        solve::IntegratorHistoryEffect::Preserve
    }
}

fn join_integrator_history_effect(
    left: solve::IntegratorHistoryEffect,
    right: solve::IntegratorHistoryEffect,
) -> solve::IntegratorHistoryEffect {
    if left == solve::IntegratorHistoryEffect::Restart
        || right == solve::IntegratorHistoryEffect::Restart
    {
        solve::IntegratorHistoryEffect::Restart
    } else {
        solve::IntegratorHistoryEffect::Preserve
    }
}

fn integrator_history_sensitive_slots(
    continuous: &solve::ContinuousSolveSystem,
    runtime_rhs: &solve::ScalarProgramBlock,
    runtime_targets: &[solve::ScalarSlot],
    state_scalar_count: usize,
) -> Option<BTreeSet<HistoryDependencySlot>> {
    let mut sensitive = (0..state_scalar_count)
        .map(HistoryDependencySlot::Y)
        .collect::<BTreeSet<_>>();
    for block in [
        &continuous.implicit_rhs,
        &continuous.residual,
        &continuous.manifold_residual,
        &continuous.derivative_rhs,
    ] {
        collect_compute_block_dependencies(block, &mut sensitive)?;
    }
    if runtime_rhs.len() != runtime_targets.len() {
        return None;
    }

    let mut assignments = Vec::with_capacity(runtime_targets.len());
    for (program, target) in runtime_rhs
        .programs()
        .iter()
        .zip(runtime_targets.iter().copied())
    {
        let target = history_dependency_slot(target)?;
        let mut dependencies = BTreeSet::new();
        collect_linear_op_dependencies(program, &mut dependencies)?;
        assignments.push((target, dependencies));
    }

    // A disconnected runtime-assignment cycle is still not positive evidence
    // for history preservation. Mark every target in a cycle sensitive before
    // propagating ordinary dependencies toward continuous consumers.
    let graph = assignments
        .iter()
        .map(|(target, dependencies)| {
            (
                *target,
                dependencies
                    .iter()
                    .copied()
                    .filter(|dependency| {
                        runtime_targets.iter().copied().any(|candidate| {
                            history_dependency_slot(candidate) == Some(*dependency)
                        })
                    })
                    .collect::<Vec<_>>(),
            )
        })
        .collect::<BTreeMap<_, _>>();
    for target in graph.keys().copied() {
        if dependency_reaches(target, target, &graph, &mut BTreeSet::new()) {
            sensitive.insert(target);
        }
    }

    loop {
        let before = sensitive.len();
        for (target, dependencies) in &assignments {
            if sensitive.contains(target) {
                sensitive.extend(dependencies.iter().copied());
            }
        }
        if sensitive.len() == before {
            break;
        }
    }
    Some(sensitive)
}

fn dependency_reaches(
    start: HistoryDependencySlot,
    current: HistoryDependencySlot,
    graph: &BTreeMap<HistoryDependencySlot, Vec<HistoryDependencySlot>>,
    visited: &mut BTreeSet<HistoryDependencySlot>,
) -> bool {
    let Some(next) = graph.get(&current) else {
        return false;
    };
    for dependency in next.iter().copied() {
        if dependency == start {
            return true;
        }
        if visited.insert(dependency) && dependency_reaches(start, dependency, graph, visited) {
            return true;
        }
    }
    false
}

pub(super) fn history_dependency_slot(target: solve::ScalarSlot) -> Option<HistoryDependencySlot> {
    match target {
        solve::ScalarSlot::Y { index, .. } => Some(HistoryDependencySlot::Y(index)),
        solve::ScalarSlot::P { index, .. } => Some(HistoryDependencySlot::P(index)),
        solve::ScalarSlot::Time | solve::ScalarSlot::Constant(_) => None,
    }
}

fn collect_compute_block_dependencies(
    block: &solve::ComputeBlock,
    dependencies: &mut BTreeSet<HistoryDependencySlot>,
) -> Option<()> {
    for node in &block.nodes {
        match node {
            solve::ComputeNode::ScalarPrograms(rows) => {
                for program in rows.programs() {
                    collect_linear_op_dependencies(program, dependencies)?;
                }
            }
            solve::ComputeNode::MatMul {
                lhs_ops, rhs_ops, ..
            } => {
                collect_linear_op_dependencies(lhs_ops, dependencies)?;
                collect_linear_op_dependencies(rhs_ops, dependencies)?;
            }
            solve::ComputeNode::LinSolve { setup_ops, .. } => {
                collect_linear_op_dependencies(setup_ops, dependencies)?;
            }
            solve::ComputeNode::Map {
                base_ops,
                load_strides,
                ..
            }
            | solve::ComputeNode::AffineStencil {
                base_ops,
                load_strides,
                ..
            } => {
                // Keep the compact node authoritative. Until dependency
                // ranges are proven directly from its affine domain, a
                // strided load is deliberately not scalarized or guessed.
                if !load_strides.is_empty() {
                    return None;
                }
                collect_linear_op_dependencies(base_ops, dependencies)?;
            }
        }
    }
    Some(())
}

pub(super) fn collect_linear_op_dependencies(
    ops: &[solve::LinearOp],
    dependencies: &mut BTreeSet<HistoryDependencySlot>,
) -> Option<()> {
    for op in ops {
        match *op {
            solve::LinearOp::LoadY { index, .. } => {
                dependencies.insert(HistoryDependencySlot::Y(index));
            }
            solve::LinearOp::LoadP { index, .. } => {
                dependencies.insert(HistoryDependencySlot::P(index));
            }
            solve::LinearOp::LoadIndexedP { base, count, .. } => {
                let end = base.checked_add(count)?;
                dependencies.extend((base..end).map(HistoryDependencySlot::P));
            }
            solve::LinearOp::LoadSeed { .. } | solve::LinearOp::LoadIndexedSeed { .. } => {
                // Seed programs are derived artifacts and cannot prove a base
                // continuous/discrete dependency contract.
                return None;
            }
            _ => {}
        }
    }
    Some(())
}
