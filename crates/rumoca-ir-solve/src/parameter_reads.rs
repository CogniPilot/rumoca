//! Which parameter slots a Solve problem's programs read.

use std::collections::BTreeMap;
use std::ops::Range;

use crate::{LinearOp, LinearOpSliceKind, SolveProblem, SolveVisitor, TensorInputKind};

/// Parameter storage indices, held as sorted disjoint runs so a tensor load
/// over a large parameter array costs one run rather than one entry per slot.
#[derive(Debug, Clone, Default, PartialEq, Eq)]
pub struct ParameterSlots {
    runs: Vec<Range<usize>>,
}

impl ParameterSlots {
    fn from_ranges(mut ranges: Vec<Range<usize>>) -> Self {
        ranges.retain(|range| range.start < range.end);
        ranges.sort_unstable_by_key(|range| range.start);
        let mut runs: Vec<Range<usize>> = Vec::with_capacity(ranges.len());
        for range in ranges {
            match runs.last_mut() {
                Some(last) if range.start <= last.end => last.end = last.end.max(range.end),
                _ => runs.push(range),
            }
        }
        Self { runs }
    }

    /// Whether some program reads storage index `index`.
    #[must_use]
    pub fn contains(&self, index: &usize) -> bool {
        let after = self.runs.partition_point(|run| run.start <= *index);
        after > 0 && *index < self.runs[after - 1].end
    }

    /// Whether no parameter slot is read.
    #[must_use]
    pub fn is_empty(&self) -> bool {
        self.runs.is_empty()
    }
}

/// Every parameter storage index some program of `problem` reads.
///
/// A parameter slot outside this set is folded into the programs that use it
/// (SPEC_0040 STRUCT-T10(a)), so setting it could not take effect: the FMI C
/// profile publishes such a parameter as a constant, and every override
/// surface rejects it.
#[must_use]
pub fn read_parameter_slots(problem: &SolveProblem) -> ParameterSlots {
    let mut reads = ParameterReads(Vec::new());
    let Ok(()) = reads.visit_solve_problem(problem);
    ParameterSlots::from_ranges(reads.0)
}

/// Every parameter storage index some program of the continuous system of
/// `problem` reads: the rows that define the trajectory between events.
#[must_use]
pub fn read_continuous_parameter_slots(problem: &SolveProblem) -> ParameterSlots {
    let mut reads = ParameterReads(Vec::new());
    let Ok(()) = reads.visit_continuous_system(&problem.continuous);
    ParameterSlots::from_ranges(reads.0)
}

struct ParameterReads(Vec<Range<usize>>);

pub(crate) fn scalar_parameter_output(ops: &[LinearOp]) -> Option<usize> {
    match ops {
        [
            LinearOp::LoadP { dst, index },
            LinearOp::StoreOutput { src },
        ] if dst == src => Some(*index),
        [
            LinearOp::LoadP { dst, index },
            LinearOp::LoadSeed { dst: tangent, .. },
            LinearOp::StoreOutput { src },
        ] if dst == src && tangent != dst => Some(*index),
        _ => None,
    }
}

pub(crate) fn parameter_read_range(op: &LinearOp) -> Option<Range<usize>> {
    match op {
        LinearOp::LoadP { index, .. } => Some(*index..*index + 1),
        LinearOp::LoadIndexedP { base, count, .. } => Some(*base..*base + *count),
        LinearOp::TensorLoad {
            input: TensorInputKind::P,
            input_start,
            count,
            ..
        } => Some(*input_start..*input_start + *count),
        _ => None,
    }
}

pub(crate) fn affine_parameter_read_limit(
    node: &crate::ComputeNode,
) -> Result<Option<usize>, crate::TensorOutputMapError> {
    let (domain, base_ops, load_strides) = match node {
        crate::ComputeNode::Map {
            domain,
            base_ops,
            load_strides,
            ..
        }
        | crate::ComputeNode::AffineStencil {
            domain,
            base_ops,
            load_strides,
            ..
        } => (domain, base_ops, load_strides),
        crate::ComputeNode::ScalarPrograms(_)
        | crate::ComputeNode::MatMul { .. }
        | crate::ComputeNode::LinSolve { .. } => return Ok(None),
    };
    let mut by_op = BTreeMap::<usize, Vec<crate::AffineStencilIndexStrideTerm>>::new();
    for stride in load_strides {
        by_op
            .entry(stride.op_position)
            .or_default()
            .extend(stride.terms.iter().cloned());
    }
    let mut limit = None;
    for (position, strides) in by_op {
        let Some(op) = base_ops.get(position) else {
            return Err(crate::TensorOutputMapError::OutputIndexOverflow);
        };
        let Some(range) = parameter_read_range(op) else {
            continue;
        };
        if range.is_empty() {
            continue;
        }
        let map = crate::TensorOutputMap {
            start: range.end - 1,
            strides,
        };
        let end = map.output_count(domain)?;
        limit = Some(limit.unwrap_or(0).max(end));
    }
    Ok(limit)
}

impl SolveVisitor for ParameterReads {
    type Error = std::convert::Infallible;
    fn visit_linear_op(
        &mut self,
        _kind: LinearOpSliceKind,
        _index: usize,
        op: &LinearOp,
    ) -> Result<(), Self::Error> {
        if let Some(range) = parameter_read_range(op) {
            self.0.push(range);
        }
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn slots_coalesce_overlapping_and_adjacent_ranges_and_answer_membership() {
        let slots = ParameterSlots::from_ranges(vec![10..14, 0..1, 3..3, 12..20, 20..22, 40..41]);
        let expected = |index: usize| index == 0 || (10..22).contains(&index) || index == 40;
        for index in 0..60 {
            assert_eq!(slots.contains(&index), expected(index), "slot {index}");
        }
        assert_eq!(slots.runs, vec![0..1, 10..22, 40..41]);
        assert!(!ParameterSlots::default().contains(&0));
        assert!(ParameterSlots::default().is_empty());
    }

    #[test]
    fn a_hundred_million_slot_tensor_load_is_one_run() {
        let slots = ParameterSlots::from_ranges(std::iter::once(0..100_000_000).collect());
        assert_eq!(slots.runs.len(), 1);
        assert!(slots.contains(&99_999_999));
        assert!(!slots.contains(&100_000_000));
    }
}
