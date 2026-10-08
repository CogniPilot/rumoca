use crate::{BinaryOp, LinearOp, TargetAssignmentShape, UnaryOp};

mod isolated_value;
mod tensor_affine;

pub use isolated_value::{
    IsolatedDivisor, IsolatedTerm, IsolatedTerms, IsolatedValue, isolated_parts,
};

/// Append the selected assignment to an already materialized source prefix at
/// a final scalar backend boundary. Returns its value and, for an affine shape,
/// the evaluated coefficient register used by runtime singularity guards.
pub fn materialize_target_assignment(
    shape: &TargetAssignmentShape,
    operations: &mut Vec<LinearOp>,
) -> Option<(u32, Option<u32>)> {
    ExactAssignmentProgramBuilder::new(operations)?.materialize_guarded(shape)
}

/// One program that assignments are appended to.
///
/// The first unused register is computed once, when the builder takes the
/// program, and every later operation goes through the builder, so appending
/// n assignments costs n appends and not n scans of the program.
pub struct ExactAssignmentProgramBuilder<'a> {
    operations: &'a mut Vec<LinearOp>,
    next_register: u32,
}

impl<'a> ExactAssignmentProgramBuilder<'a> {
    /// Append the selected assignment as [`materialize_target_assignment`]
    /// does, after the operations already in the program.
    pub fn materialize_guarded(
        &mut self,
        shape: &TargetAssignmentShape,
    ) -> Option<(u32, Option<u32>)> {
        let (result, coefficient) = self.materialize(shape)?;
        let guarded = match coefficient {
            Some(coefficient) => self.poison_non_finite(result, coefficient)?,
            None => result,
        };
        Some((guarded, coefficient))
    }

    /// Append one operation of the source program, reserving the registers it
    /// defines.
    pub fn push(&mut self, operation: LinearOp) -> Option<()> {
        if let Some(start) = operation.dst_register() {
            let count = u32::try_from(operation.dst_register_count()).ok()?;
            self.next_register = self.next_register.max(start.checked_add(count)?);
        }
        self.operations.push(operation);
        Some(())
    }

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

    pub fn new(operations: &'a mut Vec<LinearOp>) -> Option<Self> {
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
                coefficient_reg,
                coefficient_scale,
                ..
            } => {
                let guarded = coefficient_reg.is_some() || !regular(*coefficient_scale);
                self.isolated(&IsolatedValue::of(shape)?, guarded)
            }
            TargetAssignmentShape::Additive { .. } => {
                self.isolated(&IsolatedValue::of(shape)?, false)
            }
            TargetAssignmentShape::Reciprocal { .. } => {
                self.isolated(&IsolatedValue::of(shape)?, true)
            }
            TargetAssignmentShape::TensorAffine { projection, .. } => {
                let (offset, coefficient) = self.tensor_affine(projection)?;
                let value = IsolatedValue {
                    terms: vec![IsolatedTerm::Register(offset)],
                    divisor: IsolatedDivisor::DivideRegister {
                        register: coefficient,
                        scale: 1.0,
                    },
                };
                self.isolated(&value, true)
            }
        }
    }

    /// Emit `value` in the op order of its terms, then its divisor. A
    /// `guarded` value returns its coefficient register for the non-finite
    /// poison guard (a constant one included when it is singular).
    fn isolated(&mut self, value: &IsolatedValue, guarded: bool) -> Option<(u32, Option<u32>)> {
        let mut sum = None;
        for term in &value.terms {
            let term = self.term(*term)?;
            sum = Some(match sum {
                None => term,
                Some(sum) => self.binary(BinaryOp::Add, sum, term)?,
            });
        }
        let sum = match sum {
            Some(sum) => sum,
            None => self.constant(0.0)?,
        };
        let (result, coefficient) = match value.divisor {
            IsolatedDivisor::Negate => (self.negate(sum)?, None),
            IsolatedDivisor::Keep => (sum, None),
            IsolatedDivisor::Multiply(factor) => {
                let factor = self.constant(factor)?;
                (self.binary(BinaryOp::Mul, sum, factor)?, None)
            }
            IsolatedDivisor::Divide(coefficient) => {
                let negated = self.negate(sum)?;
                let coefficient = self.constant(coefficient)?;
                (
                    self.binary(BinaryOp::Div, negated, coefficient)?,
                    Some(coefficient),
                )
            }
            IsolatedDivisor::DivideRegister { register, scale } => {
                let coefficient = if scale == 1.0 {
                    register
                } else {
                    let scale = self.constant(scale)?;
                    self.binary(BinaryOp::Mul, scale, register)?
                };
                let negated = self.negate(sum)?;
                (
                    self.binary(BinaryOp::Div, negated, coefficient)?,
                    Some(coefficient),
                )
            }
        };
        Some((result, coefficient.filter(|_| guarded)))
    }

    fn term(&mut self, term: IsolatedTerm) -> Option<u32> {
        match term {
            IsolatedTerm::Register(register) => Some(register),
            IsolatedTerm::Negated(register) => self.negate(register),
            IsolatedTerm::Scaled(register, scale) => {
                let scale = self.constant(scale)?;
                self.binary(BinaryOp::Mul, scale, register)
            }
        }
    }

    fn constant(&mut self, value: f64) -> Option<u32> {
        let dst = self.allocate()?;
        self.operations.push(LinearOp::Const { dst, value });
        Some(dst)
    }

    fn negate(&mut self, arg: u32) -> Option<u32> {
        let dst = self.allocate()?;
        self.operations.push(LinearOp::Unary {
            dst,
            op: UnaryOp::Neg,
            arg,
        });
        Some(dst)
    }

    fn binary(&mut self, op: BinaryOp, lhs: u32, rhs: u32) -> Option<u32> {
        let dst = self.allocate()?;
        self.operations.push(LinearOp::Binary { dst, op, lhs, rhs });
        Some(dst)
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

/// A constant coefficient no evaluation rejects as singular.
fn regular(coefficient: f64) -> bool {
    coefficient != 0.0 && coefficient.is_finite()
}
