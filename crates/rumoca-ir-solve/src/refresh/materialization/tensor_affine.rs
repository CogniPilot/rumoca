use std::collections::BTreeMap;

use super::ExactAssignmentProgramBuilder;
use crate::refresh::assignment_shape::tensor_affine::{
    AffineTensorProjection, ProjectionRule,
    operation::{OperandRange, ProjectionOperation},
};
use crate::{LinearOp, Reg};

#[derive(Clone, Copy)]
struct ProjectedRange {
    count: usize,
    offset: Reg,
    coefficient: Reg,
}

impl ExactAssignmentProgramBuilder<'_> {
    pub(super) fn tensor_affine(
        &mut self,
        projection: &AffineTensorProjection,
    ) -> Option<(Reg, Reg)> {
        let zero = self.literal(0.0)?;
        let sources = self
            .operations
            .iter()
            .enumerate()
            .filter_map(|(position, operation)| {
                operation
                    .dst_register()
                    .map(|register| (register, position))
            })
            .collect::<BTreeMap<_, _>>();
        let mut ranges = BTreeMap::new();
        for &(source_register, rule) in projection.steps.iter() {
            let source = self
                .operations
                .get(*sources.get(&source_register)?)?
                .clone();
            let start = source.dst_register()?;
            let count = source.dst_register_count();
            let (offset, coefficient) = match rule {
                ProjectionRule::Independent => (start, self.fill(zero, count)?),
                ProjectionRule::Target { offset } => {
                    self.target_range(start, count, offset, zero)?
                }
                _ => self.project_operation(&source, rule, &ranges)?,
            };
            ranges.insert(
                start,
                ProjectedRange {
                    count,
                    offset,
                    coefficient,
                },
            );
        }
        let (start, value) = ranges.range(..=projection.output).next_back()?;
        let lane = projection.output.checked_sub(*start)?;
        (usize::try_from(lane).ok()? < value.count).then_some((
            value.offset.checked_add(lane)?,
            value.coefficient.checked_add(lane)?,
        ))
    }

    fn project_operation(
        &mut self,
        source: &LinearOp,
        rule: ProjectionRule,
        ranges: &BTreeMap<Reg, ProjectedRange>,
    ) -> Option<(Reg, Reg)> {
        let template = ProjectionOperation::new(source)?;
        let inputs = template
            .operands()
            .into_iter()
            .flatten()
            .map(|range| self.projected_operand(range, ranges))
            .collect::<Option<Vec<_>>>()?;
        let offset = self.allocate_range(source.dst_register_count())?;
        let coefficient = self.allocate_range(source.dst_register_count())?;
        let offsets = inputs.iter().map(|input| input.0).collect::<Vec<_>>();
        let coefficients = inputs
            .iter()
            .enumerate()
            .map(|(index, input)| match (rule, index) {
                (ProjectionRule::LeftProduct, 1) | (ProjectionRule::RightProduct, 0) => input.0,
                _ => input.1,
            })
            .collect::<Vec<_>>();
        self.operations
            .push(template.instantiate(offset, &offsets)?);
        self.operations
            .push(template.instantiate(coefficient, &coefficients)?);
        Some((offset, coefficient))
    }

    fn target_range(
        &mut self,
        start: Reg,
        count: usize,
        target: usize,
        zero: Reg,
    ) -> Option<(Reg, Reg)> {
        let offset = self.allocate_range(count)?;
        let coefficient = self.allocate_range(count)?;
        let target = u32::try_from(target).ok()?;
        let remaining = count.checked_sub(target as usize + 1)?;
        self.copy_range(offset, start, target as usize);
        self.operations.push(LinearOp::Const {
            dst: offset.checked_add(target)?,
            value: 0.0,
        });
        self.copy_range(
            offset.checked_add(target + 1)?,
            start.checked_add(target + 1)?,
            remaining,
        );
        self.fill_at(coefficient, zero, target as usize);
        self.operations.push(LinearOp::Const {
            dst: coefficient.checked_add(target)?,
            value: 1.0,
        });
        self.fill_at(coefficient.checked_add(target + 1)?, zero, remaining);
        Some((offset, coefficient))
    }

    fn projected_operand(
        &mut self,
        range: OperandRange,
        ranges: &BTreeMap<Reg, ProjectedRange>,
    ) -> Option<(Reg, Reg)> {
        let (&start, value) = ranges.range(..=range.start).next_back()?;
        let lane = range.start.checked_sub(start)?;
        if (lane as usize).checked_add(range.count)? <= value.count {
            return Some((
                value.offset.checked_add(lane)?,
                value.coefficient.checked_add(lane)?,
            ));
        }
        let offset = self.allocate_range(range.count)?;
        let coefficient = self.allocate_range(range.count)?;
        let mut cursor = range.start;
        let end = range.end()?;
        while cursor < end {
            let (&start, value) = ranges.range(..=cursor).next_back()?;
            let lane = cursor.checked_sub(start)?;
            let count = value
                .count
                .checked_sub(lane as usize)?
                .min((end - cursor) as usize);
            if count == 0 {
                return None;
            }
            let destination = cursor - range.start;
            self.copy_range(
                offset.checked_add(destination)?,
                value.offset.checked_add(lane)?,
                count,
            );
            self.copy_range(
                coefficient.checked_add(destination)?,
                value.coefficient.checked_add(lane)?,
                count,
            );
            cursor = cursor.checked_add(u32::try_from(count).ok()?)?;
        }
        Some((offset, coefficient))
    }

    fn literal(&mut self, value: f64) -> Option<Reg> {
        let dst = self.allocate()?;
        self.operations.push(LinearOp::Const { dst, value });
        Some(dst)
    }

    fn fill(&mut self, value: Reg, count: usize) -> Option<Reg> {
        let dst = self.allocate_range(count)?;
        self.fill_at(dst, value, count);
        Some(dst)
    }

    fn fill_at(&mut self, dst: Reg, value: Reg, count: usize) {
        if count != 0 {
            self.operations.push(LinearOp::TensorFill {
                dst_start: dst,
                value_start: value,
                count,
                lanes: 1,
            });
        }
    }

    fn copy_range(&mut self, dst: Reg, source: Reg, count: usize) {
        if count != 0 {
            self.operations.push(LinearOp::TensorTranspose {
                dst_start: dst,
                src_start: source,
                rows: 1,
                columns: 1,
                element_width: count,
                lanes: 1,
            });
        }
    }
}
