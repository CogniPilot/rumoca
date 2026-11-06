//! Issue-time Real folds. Facts belong to this straight-line builder alone.
//!
//! No operation is removed for an unknown IEEE value merely because a finite
//! algebraic identity holds: doing so could suppress signaling-NaN quieting.
//! A dominating finite check establishes the identity's exact domain. The
//! original check stays in place, with its original fault identity and order.
//! Region builders have independent facts; replay preserves recorded nodes.

use super::*;
use crate::SolveValueKind;
use std::collections::{HashMap, HashSet};

#[derive(Default)]
pub(super) struct ArithmeticFacts {
    literals: HashMap<SolveRegisterId, SolveValue>,
    negations: HashMap<SolveRegisterId, SolveRegisterId>,
    finite: HashSet<SolveRegisterId>,
}

impl ArithmeticFacts {
    pub(super) fn is_finite(&self, operand: SolveRegisterId) -> bool {
        self.finite.contains(&operand)
    }

    pub(super) fn record(&mut self, operation: &SolveOperation) {
        match operation {
            SolveOperation::Constant { destination, value } => {
                if real(value).is_some_and(f64::is_finite) {
                    self.finite.insert(*destination);
                }
                self.literals.insert(*destination, value.clone());
            }
            SolveOperation::CheckFinite { operand, .. } => {
                self.finite.insert(*operand);
            }
            SolveOperation::Unary {
                destination,
                operator: SolveUnaryOperator::Negate,
                operand,
            } => {
                self.negations.insert(*destination, *operand);
                if self.finite.contains(operand) {
                    self.finite.insert(*destination);
                }
            }
            _ => {}
        }
    }
}

fn real(value: &SolveValue) -> Option<f64> {
    match value.kind() {
        SolveValueKind::Real32(bits) => Some(f64::from(f32::from_bits(bits))),
        SolveValueKind::Real64(bits) => Some(f64::from_bits(bits)),
        _ => None,
    }
}

impl<'program> TypedProgramBuilder<'program> {
    pub fn constant(
        &mut self,
        value: SolveValue,
        provenance: Span,
    ) -> Result<ProgramRegister<'program>, SolveProgramConstructionError> {
        require_provenance(provenance)?;
        if !value.value_type().belongs_to(self.arithmetic) {
            return Err(SolveProgramConstructionError::ProfileMismatch { provenance });
        }
        let key = value
            .value_type()
            .dimensions()
            .is_empty()
            .then(|| ValueKey::Constant(value.clone()));
        if let Some(existing) = self.numbered(key.as_ref()) {
            return Ok(existing);
        }
        let destination = self.issue_register(value.value_type().clone(), provenance)?;
        self.push(
            SolveOperation::Constant {
                destination: destination.id,
                value,
            },
            provenance,
        );
        self.number(key, destination);
        Ok(destination)
    }

    pub fn unary(
        &mut self,
        operator: SolveUnaryOperator,
        operand: ProgramRegister<'program>,
        provenance: Span,
    ) -> Result<ProgramRegister<'program>, SolveProgramConstructionError> {
        require_provenance(provenance)?;
        let value_type = self.register_type(operand, provenance)?.clone();
        let valid = match operator {
            SolveUnaryOperator::Negate | SolveUnaryOperator::Abs | SolveUnaryOperator::Sign => {
                value_type.element_type().is_numeric()
            }
            SolveUnaryOperator::Not => value_type.element_type() == SolveScalarType::Boolean,
            SolveUnaryOperator::Sqrt
            | SolveUnaryOperator::Floor
            | SolveUnaryOperator::Ceiling
            | SolveUnaryOperator::Truncate
            | SolveUnaryOperator::Sin
            | SolveUnaryOperator::Cos
            | SolveUnaryOperator::Tan
            | SolveUnaryOperator::Asin
            | SolveUnaryOperator::Acos
            | SolveUnaryOperator::Atan
            | SolveUnaryOperator::Sinh
            | SolveUnaryOperator::Cosh
            | SolveUnaryOperator::Tanh
            | SolveUnaryOperator::Exp
            | SolveUnaryOperator::Log
            | SolveUnaryOperator::Log10 => {
                matches!(value_type.element_type(), SolveScalarType::Real { .. })
            }
        };
        if !valid {
            return Err(SolveProgramConstructionError::TypeMismatch { provenance });
        }
        if let Some(folded) = self.fold_unary(operator, operand, provenance)? {
            return Ok(folded);
        }
        let key = value_type
            .dimensions()
            .is_empty()
            .then_some(ValueKey::Unary(operator, operand.id));
        if let Some(existing) = self.numbered(key.as_ref()) {
            return Ok(existing);
        }
        let destination = self.issue_register(value_type, provenance)?;
        self.push(
            SolveOperation::Unary {
                destination: destination.id,
                operator,
                operand: operand.id,
            },
            provenance,
        );
        self.number(key, destination);
        Ok(destination)
    }

    pub fn binary(
        &mut self,
        operator: SolveBinaryOperator,
        lhs: ProgramRegister<'program>,
        rhs: ProgramRegister<'program>,
        provenance: Span,
    ) -> Result<ProgramRegister<'program>, SolveProgramConstructionError> {
        require_provenance(provenance)?;
        let lhs_type = self.register_type(lhs, provenance)?.clone();
        let rhs_type = self.register_type(rhs, provenance)?;
        let valid_elements = binary_operator_accepts(operator, lhs_type.element_type());
        if lhs_type != *rhs_type || !valid_elements {
            return Err(SolveProgramConstructionError::TypeMismatch { provenance });
        }
        if let Some(folded) = self.fold_binary(operator, lhs, rhs, provenance)? {
            return Ok(folded);
        }
        let key = lhs_type
            .dimensions()
            .is_empty()
            .then_some(ValueKey::Binary(operator, lhs.id, rhs.id));
        if let Some(existing) = self.numbered(key.as_ref()) {
            return Ok(existing);
        }
        let destination = self.issue_register(lhs_type, provenance)?;
        self.push(
            SolveOperation::Binary {
                destination: destination.id,
                operator,
                lhs: lhs.id,
                rhs: rhs.id,
            },
            provenance,
        );
        self.number(key, destination);
        Ok(destination)
    }

    fn fold_unary(
        &mut self,
        operator: SolveUnaryOperator,
        operand: ProgramRegister<'program>,
        at: Span,
    ) -> Result<Option<ProgramRegister<'program>>, SolveProgramConstructionError> {
        if !self.numbered_values
            || operator != SolveUnaryOperator::Negate
            || !matches!(
                self.register_type(operand, at)?.element_type(),
                SolveScalarType::Real { .. }
            )
        {
            return Ok(None);
        }
        if let Some(value) = self
            .arithmetic_facts
            .literals
            .get(&operand.id)
            .and_then(SolveValue::negated_real)
        {
            return self.constant(value, at).map(Some);
        }
        Ok(self
            .arithmetic_facts
            .negations
            .get(&operand.id)
            .map(|&id| ProgramRegister {
                id,
                marker: PhantomData,
            }))
    }

    fn fold_binary(
        &mut self,
        operator: SolveBinaryOperator,
        lhs: ProgramRegister<'program>,
        rhs: ProgramRegister<'program>,
        at: Span,
    ) -> Result<Option<ProgramRegister<'program>>, SolveProgramConstructionError> {
        if !self.numbered_values {
            return Ok(None);
        }
        if let (Some(left), Some(right)) = (
            self.arithmetic_facts.literals.get(&lhs.id),
            self.arithmetic_facts.literals.get(&rhs.id),
        ) && let Some(value) = literal_binary(self.arithmetic, operator, left, right)
        {
            return self.constant(value, at).map(Some);
        }
        Ok(self.identity_operand(operator, lhs, rhs))
    }

    /// Whole-value finite evidence is required for both scalar and tensor
    /// identity folds. In particular literal zero never proves 0*x removable.
    pub(super) fn identity_operand(
        &self,
        operator: SolveBinaryOperator,
        lhs: ProgramRegister<'program>,
        rhs: ProgramRegister<'program>,
    ) -> Option<ProgramRegister<'program>> {
        if !self.numbered_values {
            return None;
        }
        let left = self.arithmetic_facts.literals.get(&lhs.id).and_then(real);
        let right = self.arithmetic_facts.literals.get(&rhs.id).and_then(real);
        let candidate = match operator {
            SolveBinaryOperator::Multiply if left == Some(1.0) => rhs,
            SolveBinaryOperator::Multiply | SolveBinaryOperator::Divide if right == Some(1.0) => {
                lhs
            }
            SolveBinaryOperator::Subtract if right.is_some_and(|value| value.to_bits() == 0) => lhs,
            _ => return None,
        };
        self.arithmetic_facts
            .finite
            .contains(&candidate.id)
            .then_some(candidate)
    }
}

/// Compute in the declared format, including its per-operation rounding.
/// Integer arithmetic, non-finite results, and domain operations stay issued.
fn literal_binary(
    arithmetic: SolveArithmeticProfile,
    operator: SolveBinaryOperator,
    lhs: &SolveValue,
    rhs: &SolveValue,
) -> Option<SolveValue> {
    macro_rules! evaluate {
        ($left:expr, $right:expr) => {{
            let (left, right) = ($left, $right);
            if !left.is_finite() || !right.is_finite() {
                return None;
            }
            let result = match operator {
                SolveBinaryOperator::Add => left + right,
                SolveBinaryOperator::Subtract => left - right,
                SolveBinaryOperator::Multiply => left * right,
                SolveBinaryOperator::Divide => left / right,
                _ => return None,
            };
            result
                .is_finite()
                .then(|| SolveValue::real(arithmetic, f64::from(result)))
        }};
    }
    match (lhs.kind(), rhs.kind()) {
        (SolveValueKind::Real32(left), SolveValueKind::Real32(right)) => {
            evaluate!(f32::from_bits(left), f32::from_bits(right))
        }
        (SolveValueKind::Real64(left), SolveValueKind::Real64(right)) => {
            evaluate!(f64::from_bits(left), f64::from_bits(right))
        }
        _ => None,
    }
}
