use super::*;

pub(super) fn apply_integrator_history_effects(
    discrete: &mut solve::DiscreteSolveSystem,
    sensitive: &BTreeSet<HistoryDependencySlot>,
    state_scalar_count: usize,
) {
    for (effect, target) in discrete
        .integrator_history_effects
        .iter_mut()
        .zip(discrete.update_targets.iter().copied())
    {
        *effect = integrator_history_effect_for_target(target, sensitive, state_scalar_count);
    }

    for update_index in 0..discrete.structured_updates.len() {
        let effect = match discrete.structured_assignments(update_index) {
            Ok(assignments) => assignments
                .into_iter()
                .map(|(target, _)| {
                    integrator_history_effect_for_target(target, sensitive, state_scalar_count)
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

pub(super) fn integrator_history_effect_for_range(
    base: solve::ScalarSlot,
    count: usize,
    sensitive: &BTreeSet<HistoryDependencySlot>,
    state_scalar_count: usize,
) -> solve::IntegratorHistoryEffect {
    let (start, end) = match base {
        solve::ScalarSlot::Y { index, .. } => {
            let Some(end) = index.checked_add(count) else {
                return solve::IntegratorHistoryEffect::Restart;
            };
            if index < state_scalar_count {
                return solve::IntegratorHistoryEffect::Restart;
            }
            (
                HistoryDependencySlot::Y(index),
                HistoryDependencySlot::Y(end),
            )
        }
        solve::ScalarSlot::P { index, .. } => {
            let Some(end) = index.checked_add(count) else {
                return solve::IntegratorHistoryEffect::Restart;
            };
            (
                HistoryDependencySlot::P(index),
                HistoryDependencySlot::P(end),
            )
        }
        solve::ScalarSlot::Time | solve::ScalarSlot::Constant(_) => {
            return solve::IntegratorHistoryEffect::Restart;
        }
    };
    if sensitive.range(start..end).next().is_some() {
        solve::IntegratorHistoryEffect::Restart
    } else {
        solve::IntegratorHistoryEffect::Preserve
    }
}

pub(super) fn join_integrator_history_effect(
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

pub(super) fn integrator_history_sensitive_slots(
    continuous_blocks: [&solve::ComputeBlock; 4],
    runtime_rhs: &solve::ScalarProgramBlock,
    runtime_targets: &[solve::ScalarSlot],
    state_scalar_count: usize,
) -> Option<BTreeSet<HistoryDependencySlot>> {
    let mut sensitive = (0..state_scalar_count)
        .map(HistoryDependencySlot::Y)
        .collect::<BTreeSet<_>>();
    for block in continuous_blocks {
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

/// Collect the Y/P slots `ops` reads, or `None` when the program contains an
/// opcode whose dependencies this analysis cannot prove.
///
/// `None` is the conservative answer: the caller falls back to `Restart`, which
/// is always sound. Returning `Some` with an incomplete set is not, because the
/// result is positive evidence that a discrete update cannot reach continuous
/// dynamics. Every opcode that reads storage is therefore either classified
/// here or fails closed; only register-to-register work may be ignored.
pub(super) fn collect_linear_op_dependencies(
    ops: &[solve::LinearOp],
    dependencies: &mut BTreeSet<HistoryDependencySlot>,
) -> Option<()> {
    for op in ops {
        match op {
            solve::LinearOp::LoadY { index, .. } => {
                dependencies.insert(HistoryDependencySlot::Y(*index));
            }
            solve::LinearOp::LoadP { index, .. } => {
                dependencies.insert(HistoryDependencySlot::P(*index));
            }
            solve::LinearOp::TensorLoad {
                input,
                input_start,
                count,
                ..
            } => {
                let end = input_start.checked_add(*count)?;
                match input {
                    solve::TensorInputKind::Y => {
                        dependencies.extend((*input_start..end).map(HistoryDependencySlot::Y));
                    }
                    solve::TensorInputKind::P => {
                        dependencies.extend((*input_start..end).map(HistoryDependencySlot::P));
                    }
                }
            }
            solve::LinearOp::FunctionFold { program, .. }
            | solve::LinearOp::GuardedFunctionFold { program, .. }
            | solve::LinearOp::StoreOutputFunctionFold { program, .. } => {
                collect_linear_op_dependencies(program.update(), dependencies)?;
            }
            solve::LinearOp::FunctionConditional { program, .. } => {
                for arm in program.arms() {
                    collect_linear_op_dependencies(arm.condition(), dependencies)?;
                    collect_linear_op_dependencies(arm.result(), dependencies)?;
                }
                collect_linear_op_dependencies(program.fallback(), dependencies)?;
            }
            // Register-to-register work reads nothing outside the program, so it
            // introduces no new dependency. `LoadTime` reads the independent
            // variable, which is neither a Y nor a P slot.
            solve::LinearOp::Const { .. }
            | solve::LinearOp::LoadTime { .. }
            | solve::LinearOp::LoadIndexedRegister { .. }
            | solve::LinearOp::Move { .. }
            | solve::LinearOp::Unary { .. }
            | solve::LinearOp::Binary { .. }
            | solve::LinearOp::Compare { .. }
            | solve::LinearOp::Select { .. }
            | solve::LinearOp::DotProduct { .. }
            | solve::LinearOp::MatrixMultiply { .. }
            | solve::LinearOp::LinearSolveComponent { .. }
            | solve::LinearOp::TensorBinary { .. }
            | solve::LinearOp::TensorCross { .. }
            | solve::LinearOp::TensorTranspose { .. }
            | solve::LinearOp::TensorConcatenate { .. }
            | solve::LinearOp::TensorUpdate { .. }
            | solve::LinearOp::TensorFill { .. }
            | solve::LinearOp::TensorIdentity { .. }
            | solve::LinearOp::StoreOutput { .. }
            | solve::LinearOp::StoreOutputRange { .. }
            | solve::LinearOp::StoreOutputFoldTensorUpdate { .. } => {}
            // Everything else reads storage this analysis does not model:
            // seeds (derived artifacts), fold/capture state, tables, random
            // state, and pure calls. Fail closed rather than under-report.
            _ => return None,
        }
    }
    Some(())
}
