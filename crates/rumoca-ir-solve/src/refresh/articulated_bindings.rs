//! Original call channels, joined through existing exact-assignment proofs.
mod inputs;

use std::collections::{BTreeMap, BTreeSet};

use super::{BoundExactRefreshAssignment, assignment_shape::producers::UniqueProgram};
use crate::{LinearOp, SolveProblem, SolvePureCallSite, SolvePureCallTable, TargetAssignmentShape};
pub(crate) use inputs::SourceCell;

const INPUT_WIDTHS: [usize; 7] = [36, 6, 1, 1, 1, 6, 6];
pub(crate) const OUTPUT_WIDTHS: [usize; 5] = [6, 1, 1, 36, 6];

pub(crate) struct ArticulatedApplication {
    pub(crate) inputs: Vec<Vec<SourceCell>>,
    pub(crate) outputs: Vec<usize>,
}

pub(crate) fn derive(
    problem: &SolveProblem,
    calls: &SolvePureCallTable,
) -> Vec<ArticulatedApplication> {
    if !calls
        .owners()
        .iter()
        .any(|o| o.articulated_joint_reduction().is_some())
    {
        return Vec::new();
    }
    let Ok(bound) = problem
        .continuous
        .refresh_owners
        .exact_assignment_programs_for_source(&problem.continuous.implicit_rhs)
    else {
        return Vec::new();
    };
    let mut sources = BTreeMap::<_, Vec<_>>::new();
    for source in bound.iter().copied() {
        sources
            .entry(source.assignment().source())
            .or_default()
            .push(source);
    }
    sources
        .values()
        .flat_map(|source| applications(source, calls))
        .collect()
}

fn applications(
    sources: &[BoundExactRefreshAssignment<'_>],
    calls: &SolvePureCallTable,
) -> Vec<ArticulatedApplication> {
    let Some(source) = sources.first() else {
        return Vec::new();
    };
    let operations = source.source_operations();
    if !operations
        .iter()
        .any(|op| matches!(op, LinearOp::PureCall { site, .. } if candidate(site, calls)))
    {
        return Vec::new();
    }
    let Some(producers) = UniqueProgram::new(operations) else {
        return Vec::new();
    };
    operations
        .iter()
        .enumerate()
        .filter_map(|(position, operation)| {
            let LinearOp::PureCall {
                dst_start,
                input_starts,
                site,
            } = operation
            else {
                return None;
            };
            if !candidate(site, calls) {
                return None;
            }
            let prefix = producers.view().before(position)?;
            let inputs = INPUT_WIDTHS
                .iter()
                .zip(input_starts.iter())
                .map(|(&count, &start)| {
                    (0..count)
                        .map(|offset| inputs::cell(prefix, start.checked_add(offset as u32)?, 0))
                        .collect::<Option<Vec<_>>>()
                })
                .collect::<Option<Vec<_>>>()
                .filter(|v| v.len() == INPUT_WIDTHS.len())?;
            if !matches!(inputs[3].as_slice(), [SourceCell::Literal(value)] if value.to_bits() == 0)
            {
                return None;
            }
            let outputs = output_targets(sources, *dst_start, position)?;
            Some(ArticulatedApplication { inputs, outputs })
        })
        .collect()
}

fn candidate(site: &SolvePureCallSite, calls: &SolvePureCallTable) -> bool {
    calls.matches_site(site)
        && calls
            .owner(site.owner())
            .is_some_and(|o| o.articulated_joint_reduction().is_some())
}

fn output_targets(
    sources: &[BoundExactRefreshAssignment<'_>],
    start: u32,
    call: usize,
) -> Option<Vec<usize>> {
    let mut targets = vec![None; OUTPUT_WIDTHS.iter().sum()];
    for (shape, &target) in sources.iter().flat_map(|bound| {
        bound
            .assignment()
            .assignment_shapes()
            .iter()
            .zip(bound.assignment().target_indices())
    }) {
        let TargetAssignmentShape::Direct {
            expr_reg,
            target_scale,
            expr_eval_len,
            ..
        } = shape
        else {
            continue;
        };
        if target_scale.to_bits() != 1.0_f64.to_bits() || *expr_eval_len <= call {
            continue;
        }
        let Some(offset) = expr_reg.checked_sub(start).map(|i| i as usize) else {
            continue;
        };
        let Some(slot) = targets.get_mut(offset) else {
            continue;
        };
        if slot.is_some_and(|previous| previous != target) {
            return None;
        }
        *slot = Some(target);
    }
    let targets = targets.into_iter().collect::<Option<Vec<_>>>()?;
    (targets.iter().collect::<BTreeSet<_>>().len() == targets.len()).then_some(targets)
}
