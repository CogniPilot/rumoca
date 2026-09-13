use super::{ProjectionRule, ScalarProgramYDependency};
use crate::{BinaryOp, LinearOp, Reg, UnaryOp};

#[derive(Clone, Copy)]
pub(in crate::refresh) struct OperandRange {
    pub(in crate::refresh) start: Reg,
    pub(in crate::refresh) count: usize,
}

impl OperandRange {
    pub(in crate::refresh) const fn new(start: Reg, count: usize) -> Self {
        Self { start, count }
    }

    pub(in crate::refresh) fn end(self) -> Option<Reg> {
        self.start.checked_add(u32::try_from(self.count).ok()?)
    }

    pub(in crate::refresh) fn independent_of(
        self,
        target: usize,
        dependencies: &ScalarProgramYDependency<'_>,
    ) -> bool {
        self.end().is_some_and(|end| {
            (self.start..end).all(|register| !dependencies.depends_on(register, target))
        })
    }
}

pub(in crate::refresh) struct ProjectionOperation<'a> {
    pub(in crate::refresh) source: &'a LinearOp,
    operands: [Option<OperandRange>; 2],
    kind: Kind,
}

enum Kind {
    Linear,
    Product,
    Division,
}

impl<'a> ProjectionOperation<'a> {
    pub(in crate::refresh) fn new(source: &'a LinearOp) -> Option<Self> {
        let range = |start, count| Some(OperandRange::new(start, count));
        let (operands, kind) = match *source {
            LinearOp::Move { src, .. }
            | LinearOp::Unary {
                op: UnaryOp::Neg,
                arg: src,
                ..
            } => ([range(src, 1), None], Kind::Linear),
            LinearOp::Binary { op, lhs, rhs, .. } => {
                ([range(lhs, 1), range(rhs, 1)], binary_kind(op)?)
            }
            LinearOp::TensorBinary {
                op,
                lhs_start,
                rhs_start,
                count,
                lhs_stride,
                rhs_stride,
                lanes: 1,
                ..
            } => (
                [
                    range(lhs_start, strided_count(count, lhs_stride)?),
                    range(rhs_start, strided_count(count, rhs_stride)?),
                ],
                binary_kind(op)?,
            ),
            LinearOp::TensorCross {
                lhs_start,
                rhs_start,
                lanes: 1,
                ..
            } => ([range(lhs_start, 3), range(rhs_start, 3)], Kind::Product),
            LinearOp::MatrixMultiply {
                lhs_start,
                rhs_start,
                rows,
                inner,
                columns,
                lanes: 1,
                ..
            } => (
                [
                    range(lhs_start, rows.checked_mul(inner)?),
                    range(rhs_start, inner.checked_mul(columns)?),
                ],
                Kind::Product,
            ),
            LinearOp::DotProduct {
                lhs_start,
                rhs_start,
                count,
                lhs_stride,
                rhs_stride,
                ..
            } => (
                [
                    range(lhs_start, strided_count(count, lhs_stride)?),
                    range(rhs_start, strided_count(count, rhs_stride)?),
                ],
                Kind::Product,
            ),
            LinearOp::TensorFill {
                value_start,
                lanes: 1,
                ..
            } => ([range(value_start, 1), None], Kind::Linear),
            LinearOp::TensorTranspose {
                src_start,
                rows,
                columns,
                element_width,
                lanes: 1,
                ..
            } => (
                [
                    range(
                        src_start,
                        rows.checked_mul(columns)?.checked_mul(element_width)?,
                    ),
                    None,
                ],
                Kind::Linear,
            ),
            _ => return None,
        };
        Some(Self {
            source,
            operands,
            kind,
        })
    }

    pub(in crate::refresh) const fn operands(&self) -> [Option<OperandRange>; 2] {
        self.operands
    }

    pub(in crate::refresh) fn instantiate(&self, dst: Reg, inputs: &[Reg]) -> Option<LinearOp> {
        let mut operation = self.source.clone();
        let first = *inputs.first()?;
        match &mut operation {
            LinearOp::Move { dst: out, src }
            | LinearOp::Unary {
                dst: out, arg: src, ..
            } => {
                *out = dst;
                *src = first;
            }
            LinearOp::Binary {
                dst: out, lhs, rhs, ..
            } => {
                *out = dst;
                *lhs = first;
                *rhs = *inputs.get(1)?;
            }
            LinearOp::TensorBinary {
                dst_start,
                lhs_start,
                rhs_start,
                ..
            }
            | LinearOp::TensorCross {
                dst_start,
                lhs_start,
                rhs_start,
                ..
            }
            | LinearOp::MatrixMultiply {
                dst_start,
                lhs_start,
                rhs_start,
                ..
            } => {
                *dst_start = dst;
                *lhs_start = first;
                *rhs_start = *inputs.get(1)?;
            }
            LinearOp::DotProduct {
                dst: out,
                lhs_start,
                rhs_start,
                ..
            } => {
                *out = dst;
                *lhs_start = first;
                *rhs_start = *inputs.get(1)?;
            }
            LinearOp::TensorFill {
                dst_start,
                value_start,
                ..
            } => {
                *dst_start = dst;
                *value_start = first;
            }
            LinearOp::TensorTranspose {
                dst_start,
                src_start,
                ..
            } => {
                *dst_start = dst;
                *src_start = first;
            }
            _ => return None,
        }
        Some(operation)
    }

    pub(in crate::refresh) fn rule(
        &self,
        target: usize,
        dependencies: &ScalarProgramYDependency<'_>,
    ) -> Option<ProjectionRule> {
        let independent = |index: usize| {
            self.operands[index].is_some_and(|range| range.independent_of(target, dependencies))
        };
        match self.kind {
            Kind::Linear => Some(ProjectionRule::Linear),
            Kind::Product if independent(0) => Some(ProjectionRule::RightProduct),
            Kind::Product | Kind::Division if independent(1) => Some(ProjectionRule::LeftProduct),
            _ => None,
        }
    }
}

fn binary_kind(op: BinaryOp) -> Option<Kind> {
    match op {
        BinaryOp::Add | BinaryOp::Sub => Some(Kind::Linear),
        BinaryOp::Mul => Some(Kind::Product),
        BinaryOp::Div => Some(Kind::Division),
        _ => None,
    }
}

fn strided_count(count: usize, stride: usize) -> Option<usize> {
    if count == 0 {
        Some(0)
    } else {
        (count - 1).checked_mul(stride)?.checked_add(1)
    }
}
