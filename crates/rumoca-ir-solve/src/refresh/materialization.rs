use crate::{BinaryOp, LinearOp, TargetAssignmentShape, UnaryOp};

mod tensor_affine;

/// Append the selected assignment to an already materialized source prefix at
/// a final scalar backend boundary. Returns its value and, for an affine shape,
/// the evaluated coefficient register used by runtime singularity guards.
pub fn materialize_target_assignment(
    shape: &TargetAssignmentShape,
    operations: &mut Vec<LinearOp>,
) -> Option<(u32, Option<u32>)> {
    let mut builder = ExactAssignmentProgramBuilder::new(operations)?;
    let (result, coefficient) = builder.materialize(shape)?;
    let guarded = match coefficient {
        Some(coefficient) => builder.poison_non_finite(result, coefficient)?,
        None => result,
    };
    Some((guarded, coefficient))
}

struct ExactAssignmentProgramBuilder<'a> {
    operations: &'a mut Vec<LinearOp>,
    next_register: u32,
}

impl<'a> ExactAssignmentProgramBuilder<'a> {
    // Finite coefficients contribute exactly +0, preserving the value's bits;
    // an infinite/NaN coefficient makes every materialized consumer decline.
    fn poison_non_finite(&mut self, value: u32, coefficient: u32) -> Option<u32> {
        let gap = self.allocate()?;
        let result = self.allocate()?;
        self.operations.push(LinearOp::Binary {
            dst: gap,
            op: BinaryOp::Sub,
            lhs: coefficient,
            rhs: coefficient,
        });
        self.operations.push(LinearOp::Binary {
            dst: result,
            op: BinaryOp::Sub,
            lhs: value,
            rhs: gap,
        });
        Some(result)
    }

    fn new(operations: &'a mut Vec<LinearOp>) -> Option<Self> {
        let next_register = operations.iter().try_fold(0, |next, operation| {
            let Some(start) = operation.dst_register() else {
                return Some(next);
            };
            let count = u32::try_from(operation.dst_register_count()).ok()?;
            Some(next.max(start.checked_add(count)?))
        })?;
        Some(Self {
            operations,
            next_register,
        })
    }

    fn materialize(&mut self, shape: &TargetAssignmentShape) -> Option<(u32, Option<u32>)> {
        match shape {
            TargetAssignmentShape::Zero { .. } => {
                let dst = self.allocate()?;
                self.operations.push(LinearOp::Const { dst, value: 0.0 });
                Some((dst, None))
            }
            TargetAssignmentShape::Direct { expr_reg, .. } => Some((*expr_reg, None)),
            TargetAssignmentShape::Affine {
                offset_reg,
                coefficient_reg,
                offset_scale,
                coefficient_scale,
                ..
            } => self.affine(
                *offset_reg,
                *coefficient_reg,
                *offset_scale,
                *coefficient_scale,
            ),
            TargetAssignmentShape::Additive {
                offset_terms,
                coefficient,
                ..
            } => self
                .additive(offset_terms, *coefficient)
                .map(|result| (result, None)),
            TargetAssignmentShape::TensorAffine { projection, .. } => {
                let (offset, coefficient) = self.tensor_affine(projection)?;
                self.affine(offset, Some(coefficient), 1.0, 1.0)
            }
        }
    }

    fn affine(
        &mut self,
        offset: u32,
        coefficient: Option<u32>,
        offset_scale: f64,
        coefficient_scale: f64,
    ) -> Option<(u32, Option<u32>)> {
        let offset_scale_reg = self.allocate()?;
        let scaled_offset = self.allocate()?;
        let coefficient_scale_reg = self.allocate()?;
        let scaled_coefficient = self.allocate()?;
        let negated_offset = self.allocate()?;
        let result = self.allocate()?;
        self.operations.push(LinearOp::Const {
            dst: offset_scale_reg,
            value: offset_scale,
        });
        self.operations.push(LinearOp::Binary {
            dst: scaled_offset,
            op: BinaryOp::Mul,
            lhs: offset_scale_reg,
            rhs: offset,
        });
        self.operations.push(LinearOp::Const {
            dst: coefficient_scale_reg,
            value: coefficient_scale,
        });
        self.operations.push(match coefficient {
            Some(coefficient) => LinearOp::Binary {
                dst: scaled_coefficient,
                op: BinaryOp::Mul,
                lhs: coefficient_scale_reg,
                rhs: coefficient,
            },
            None => LinearOp::Move {
                dst: scaled_coefficient,
                src: coefficient_scale_reg,
            },
        });
        self.operations.push(LinearOp::Unary {
            dst: negated_offset,
            op: UnaryOp::Neg,
            arg: scaled_offset,
        });
        self.operations.push(LinearOp::Binary {
            dst: result,
            op: BinaryOp::Div,
            lhs: negated_offset,
            rhs: scaled_coefficient,
        });
        Some((result, Some(scaled_coefficient)))
    }

    fn additive(&mut self, terms: &[(u32, f64)], coefficient: f64) -> Option<u32> {
        let mut offset = self.allocate()?;
        self.operations.push(LinearOp::Const {
            dst: offset,
            value: 0.0,
        });
        for &(register, scale) in terms {
            let scale_reg = self.allocate()?;
            let weighted = self.allocate()?;
            let sum = self.allocate()?;
            self.operations.push(LinearOp::Const {
                dst: scale_reg,
                value: scale,
            });
            self.operations.push(LinearOp::Binary {
                dst: weighted,
                op: BinaryOp::Mul,
                lhs: scale_reg,
                rhs: register,
            });
            self.operations.push(LinearOp::Binary {
                dst: sum,
                op: BinaryOp::Add,
                lhs: offset,
                rhs: weighted,
            });
            offset = sum;
        }
        let negated = self.allocate()?;
        let divisor = self.allocate()?;
        let result = self.allocate()?;
        self.operations.push(LinearOp::Unary {
            dst: negated,
            op: UnaryOp::Neg,
            arg: offset,
        });
        self.operations.push(LinearOp::Const {
            dst: divisor,
            value: coefficient,
        });
        self.operations.push(LinearOp::Binary {
            dst: result,
            op: BinaryOp::Div,
            lhs: negated,
            rhs: divisor,
        });
        Some(result)
    }

    fn allocate(&mut self) -> Option<u32> {
        self.allocate_range(1)
    }

    fn allocate_range(&mut self, count: usize) -> Option<u32> {
        let register = self.next_register;
        self.next_register = self.next_register.checked_add(u32::try_from(count).ok()?)?;
        Some(register)
    }
}
