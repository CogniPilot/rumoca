//! Compact, source-bound ownership of disjoint destination ranges.

use std::collections::BTreeMap;

use crate::{LinearOp, Reg};

pub(super) struct UniqueProgram<'a> {
    operations: &'a [LinearOp],
    ranges: BTreeMap<Reg, (u64, usize)>,
}

impl<'a> UniqueProgram<'a> {
    pub(super) fn new(operations: &'a [LinearOp]) -> Option<Self> {
        let mut ranges = BTreeMap::<Reg, (u64, usize)>::new();
        for (position, operation) in operations.iter().enumerate() {
            let Some(start) = operation.dst_register() else {
                continue;
            };
            let count = u64::try_from(operation.dst_register_count()).ok()?;
            if count == 0 {
                continue;
            }
            let end = u64::from(start).checked_add(count)?;
            if end > u64::from(Reg::MAX) + 1
                || ranges
                    .range(..=start)
                    .next_back()
                    .is_some_and(|(_, (previous_end, _))| *previous_end > u64::from(start))
                || ranges
                    .range(start..)
                    .next()
                    .is_some_and(|(next_start, _)| u64::from(*next_start) < end)
            {
                return None;
            }
            ranges.insert(start, (end, position));
        }
        Some(Self { operations, ranges })
    }

    pub(super) fn view(&self) -> ProgramPrefix<'_> {
        ProgramPrefix {
            operations: self.operations,
            ranges: &self.ranges,
        }
    }
}

#[derive(Clone, Copy)]
pub(super) struct ProgramPrefix<'a> {
    operations: &'a [LinearOp],
    ranges: &'a BTreeMap<Reg, (u64, usize)>,
}

impl<'a> ProgramPrefix<'a> {
    pub(super) fn before(self, position: usize) -> Option<Self> {
        Some(Self {
            operations: self.operations.get(..position)?,
            ranges: self.ranges,
        })
    }

    pub(super) fn len(self) -> usize {
        self.operations.len()
    }

    pub(super) fn operation(self, position: usize) -> Option<&'a LinearOp> {
        self.operations.get(position)
    }

    pub(super) fn producer_position(self, register: Reg) -> Option<usize> {
        let (_, &(end, position)) = self.ranges.range(..=register).next_back()?;
        (u64::from(register) < end && position < self.operations.len()).then_some(position)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn tensor(start: Reg, count: usize) -> LinearOp {
        LinearOp::TensorLoad {
            dst_start: start,
            input: crate::TensorInputKind::Y,
            input_start: 0,
            count,
            seed_start: None,
            lanes: 1,
        }
    }

    fn reference_producer(operations: &[LinearOp], register: Reg) -> Option<usize> {
        operations.iter().position(|operation| {
            let start = operation.dst_register().unwrap();
            (start..start + operation.dst_register_count() as u32).contains(&register)
        })
    }

    #[test]
    fn tensor_ownership_is_compact_through_the_final_register() {
        let operations = [tensor(1, Reg::MAX as usize)];
        let source = UniqueProgram::new(&operations).unwrap();
        let view = source.view();
        assert_eq!(view.producer_position(0), None);
        assert_eq!(view.producer_position(1), Some(0));
        assert_eq!(view.producer_position(Reg::MAX), Some(0));
        assert_eq!(view.before(0).unwrap().producer_position(1), None);
    }

    #[test]
    fn prefix_ownership_preserves_gaps_and_nonmonotone_destinations() {
        let operations = [tensor(10, 3), tensor(2, 4), tensor(6, 2)];
        let source = UniqueProgram::new(&operations).unwrap();
        for limit in 0..=operations.len() {
            let view = source.view().before(limit).unwrap();
            for register in 0..16 {
                let expected = reference_producer(&operations[..limit], register);
                assert_eq!(view.producer_position(register), expected);
            }
        }
    }

    #[test]
    fn overlaps_and_overflow_cannot_issue_unique_ownership() {
        for operations in [
            [tensor(2, 4), tensor(4, 1)],
            [tensor(4, 1), tensor(2, 4)],
            [tensor(2, 4), tensor(2, 4)],
            [tensor(0, 1), tensor(Reg::MAX, 2)],
        ] {
            assert!(UniqueProgram::new(&operations).is_none());
        }
    }

    #[test]
    fn later_register_overwrite_does_not_invalidate_an_earlier_output() {
        let operations = [
            LinearOp::LoadY { dst: 5, index: 0 },
            LinearOp::LoadP { dst: 1, index: 0 },
            LinearOp::Binary {
                dst: 9,
                op: crate::BinaryOp::Sub,
                lhs: 5,
                rhs: 1,
            },
            LinearOp::StoreOutput { src: 9 },
            LinearOp::Const { dst: 5, value: 1.0 },
            LinearOp::StoreOutput { src: 9 },
        ];
        let shapes = super::super::derive_target_assignment_shapes(&operations);
        assert_eq!(shapes.len(), 1);
        assert_eq!(shapes[0].0, 0);
        assert_eq!(shapes[0].1.target_y_index(), 0);
        assert_eq!(
            super::super::canonical_assignment_shape_for_output(&operations, 0, 0),
            Some(shapes[0].1.clone())
        );
        assert!(super::super::canonical_assignment_shape_for_output(&operations, 1, 0).is_none());
    }
}
