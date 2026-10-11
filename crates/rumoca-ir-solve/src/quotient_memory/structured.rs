//! Checked initialization/event authority of one compact numeric memory Map.

use super::*;
use std::borrow::Cow;
use std::ops::Range;

pub(super) fn validate(problem: &SolveProblem) -> Result<(), SolveProblemShapeContractError> {
    if !problem
        .discrete
        .structured_updates
        .iter()
        .any(|update| matches!(update.role, DiscreteRowRole::QuotientMemory { .. }))
    {
        return Ok(());
    }
    let roots = outputs(&problem.events.root_conditions);
    let reserved = reserved_parameters(problem);
    let mut owned = problem
        .discrete
        .row_roles
        .iter()
        .filter_map(|role| match role {
            DiscreteRowRole::QuotientMemory { root_index, .. } => {
                root_index.checked_add(1).map(|end| (*root_index, end))
            }
            _ => None,
        })
        .collect::<BTreeMap<_, _>>();
    for (row, update) in problem.discrete.structured_updates.iter().enumerate() {
        let DiscreteRowRole::QuotientMemory { step, root_index } = update.role else {
            continue;
        };
        let node = problem
            .discrete
            .structured_rhs
            .nodes
            .get(update.node_index)
            .ok_or_else(|| event_iteration_contract(row, "numeric memory Map is missing"))?;
        node.validate_shape_contract("structured quotient memory", update.node_index)?;
        let ComputeNode::Map {
            domain,
            base_ops,
            const_strides,
            load_strides,
            ..
        } = node
        else {
            return Err(event_iteration_contract(
                row,
                "numeric memory requires a Map",
            ));
        };
        let count = domain
            .scalar_count()
            .map_err(|_| event_iteration_contract(row, "numeric memory domain overflows"))?;
        let target = validate_storage(problem, row, update, domain, count, &reserved)?;
        let end = root_index
            .checked_add(count)
            .ok_or_else(|| event_iteration_contract(row, "numeric memory root range overflows"))?;
        if end > problem.events.root_conditions.output_count() {
            return Err(event_iteration_contract(
                row,
                "numeric memory root family is incomplete",
            ));
        }
        let range = root_index..end;
        if owned
            .range(..end)
            .next_back()
            .is_some_and(|(_, &previous_end)| previous_end > root_index)
        {
            return Err(event_iteration_contract(
                row,
                "numeric memory root family is not exclusive",
            ));
        }
        owned.insert(range.start, range.end);
        let held_load = validate_held_load(row, target, domain, base_ops, load_strides)?;
        let expected = activation::indicator(row, step, base_ops, target, held_load)?;
        validate_root_family(
            row,
            root_index,
            domain,
            count,
            &expected,
            const_strides,
            &roots,
        )?;
        validate_root_policy(problem, row, range)?;
    }
    Ok(())
}

fn validate_root_family(
    row: usize,
    root_index: usize,
    domain: &rumoca_core::StructuredIndexDomain,
    count: usize,
    expected: &[LinearOp],
    const_strides: &[AffineStencilConstStride],
    roots: &Outputs<'_>,
) -> Result<(), SolveProblemShapeContractError> {
    let extents = domain
        .extents()
        .map_err(|_| event_iteration_contract(row, "numeric memory domain is invalid"))?
        .into_iter()
        .map(u32::try_from)
        .collect::<Result<Vec<_>, _>>()
        .map_err(|_| event_iteration_contract(row, "numeric memory extent overflows"))?;
    // Existing root rows are generated views. Replay their proof by streaming
    // each declared Map coordinate into the shared term checker; no per-point
    // LinearOp program is allocated, retained, or executed in the IR crate.
    for ordinal in 0..count {
        let coordinates = rumoca_core::row_major_coordinates(&extents, ordinal)
            .ok_or_else(|| event_iteration_contract(row, "numeric memory coordinate is invalid"))?;
        let &(found, output, _) = roots.get(&(root_index + ordinal)).ok_or_else(|| {
            event_iteration_contract(row, "numeric memory root family is incomplete")
        })?;
        crate::linear_op::check_same_output_transformed(
            expected,
            0,
            found,
            output,
            |position, op| project_constant(position, op, const_strides, &coordinates),
        )
        .map_err(|_| {
            event_iteration_contract(
                row,
                "numeric memory root family does not monitor its exact live ratios",
            )
        })?;
    }
    Ok(())
}

fn validate_root_policy(
    problem: &SolveProblem,
    row: usize,
    range: Range<usize>,
) -> Result<(), SolveProblemShapeContractError> {
    if range.into_iter().any(|root| {
        problem.events.root_relation_memory_targets.get(root) != Some(&None)
            || problem.events.root_zero_domains.get(root) != Some(&RootZeroDomain::NonPositive)
    }) {
        return Err(event_iteration_contract(
            row,
            "numeric memory root family has incompatible event policy",
        ));
    }
    Ok(())
}

fn overlaps(a: &Range<usize>, b: &Range<usize>) -> bool {
    a.start < b.end && b.start < a.end
}

fn validate_storage(
    problem: &SolveProblem,
    row: usize,
    update: &StructuredDiscreteUpdate,
    domain: &rumoca_core::StructuredIndexDomain,
    count: usize,
    reserved: &BTreeSet<usize>,
) -> Result<usize, SolveProblemShapeContractError> {
    let ScalarSlot::P { index, .. } = update.target.base else {
        return Err(event_iteration_contract(
            row,
            "numeric memory Map is not P-backed",
        ));
    };
    let end = index
        .checked_add(count)
        .ok_or_else(|| event_iteration_contract(row, "numeric memory target range overflows"))?;
    let target = index..end;
    let canonical = TensorOutputMap::dense_contiguous(0, domain)
        .map_err(|_| event_iteration_contract(row, "numeric memory target map overflows"))?;
    if count == 0
        || count as u128 > 1_u128 << 53
        || update.target.map != canonical
        || update.clock_owner.is_some()
        || update.observation_refresh
        || update.pre_mode != DiscreteEventPreMode::FollowCurrent
        || update.integrator_history_effect != IntegratorHistoryEffect::Restart
        || reserved.range(target.clone()).next().is_some()
        || problem
            .initialization
            .update_targets()
            .iter()
            .chain(&problem.discrete.update_targets)
            .any(|slot| matches!(slot, ScalarSlot::P { index, .. } if target.contains(index)))
        || problem
            .solve_layout
            .variable_storage_runs
            .iter()
            .any(|run| match run.base {
                ScalarSlot::P { index, .. } => index
                    .checked_add(run.scalar_count)
                    .is_none_or(|end| overlaps(&target, &(index..end))),
                _ => false,
            })
    {
        return Err(event_iteration_contract(
            row,
            "numeric memory Map has no exclusive private initialization/event span",
        ));
    }
    Ok(index)
}

fn validate_held_load(
    row: usize,
    target: usize,
    domain: &rumoca_core::StructuredIndexDomain,
    program: &[LinearOp],
    loads: &[AffineStencilLoadStride],
) -> Result<Option<usize>, SolveProblemShapeContractError> {
    let activated = matches!(
        program,
        [
            ..,
            LinearOp::FunctionConditional { .. },
            LinearOp::StoreOutput { .. }
        ]
    );
    if !activated {
        return if loads.is_empty() {
            Ok(None)
        } else {
            Err(event_iteration_contract(
                row,
                "unconditional numeric memory has foreign affine loads",
            ))
        };
    }
    let [load] = loads else {
        return Err(event_iteration_contract(
            row,
            "numeric memory has foreign affine loads",
        ));
    };
    if !matches!(program.get(load.op_position), Some(LinearOp::LoadP { index, .. }) if *index == target)
    {
        return Err(event_iteration_contract(
            row,
            "numeric memory affine hold is not its private slot",
        ));
    }
    let expected = domain
        .ordinal_strides()
        .map_err(|_| event_iteration_contract(row, "numeric memory hold strides overflow"))?
        .into_iter()
        .enumerate()
        .map(|(dimension, stride)| {
            Ok(AffineStencilIndexStrideTerm {
                dimension,
                stride: isize::try_from(stride).map_err(|_| {
                    event_iteration_contract(row, "numeric memory hold stride overflows")
                })?,
            })
        })
        .collect::<Result<Vec<_>, SolveProblemShapeContractError>>()?;
    if load.terms != expected {
        return Err(event_iteration_contract(
            row,
            "numeric memory affine hold does not follow its domain ordinals",
        ));
    }
    Ok(Some(load.op_position))
}

fn project_constant<'a>(
    position: usize,
    op: &'a LinearOp,
    strides: &[AffineStencilConstStride],
    coordinates: &[u32],
) -> Option<Cow<'a, LinearOp>> {
    if !strides.iter().any(|stride| stride.op_position == position) {
        return Some(Cow::Borrowed(op));
    }
    let LinearOp::Const { dst, value } = *op else {
        return None;
    };
    let mut projected = value;
    for (dimension, &coordinate) in coordinates.iter().enumerate() {
        let coefficient = strides
            .iter()
            .filter(|stride| stride.op_position == position)
            .flat_map(|stride| &stride.terms)
            .filter(|term| term.dimension == dimension)
            .fold(0.0, |coefficient, term| coefficient + term.stride);
        projected += f64::from(coordinate) * coefficient;
    }
    Some(Cow::Owned(LinearOp::Const {
        dst,
        value: projected,
    }))
}

#[cfg(test)]
mod tests;
