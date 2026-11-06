//! Structural product omission before either operand is evaluated.
//!
//! This uses the incidence owner's proof, not the arithmetic builder's literal
//! facts. Domain/effectful factors and tunable defaults cannot establish it.

mod product;
mod proof;
mod selected;
mod shared;
pub(super) use shared::{SelectedCall, SelectedCalls, register_selections};

use super::*;

// Values are scoped to one builder and exact source-coordinate registers.
pub(super) type SelectedValues<'program, 'dae> = HashMap<
    (dae::ExprId<'dae>, Vec<solve::ProgramRegister<'program>>),
    solve::ProgramRegister<'program>,
>;

/// Checked handover from a DAE handle to the immutable source view it names.
#[derive(Clone, Copy)]
struct SourceExpression<'dae> {
    id: dae::ExprId<'dae>,
    node: dae::ExpressionView<'dae>,
}

impl<'dae> SourceExpression<'dae> {
    fn construct(
        view: dae::DaeView<'dae>,
        id: dae::ExprId<'dae>,
    ) -> Result<Self, solve::SolveProgramConstructionError> {
        let node = view
            .expression(id)
            .ok_or(solve::SolveProgramConstructionError::WireMismatch)?;
        Ok(Self { id, node })
    }
}

impl<'program, 'dae> ExpressionLowerer<'_, 'program, 'dae> {
    pub(super) fn proven_zero_product(
        &mut self,
        value_type: dae::ValueTypeId<'dae>,
        operator: dae::BinaryOperator,
        lhs: dae::ExprId<'dae>,
        rhs: dae::ExprId<'dae>,
        at: rumoca_core::Span,
    ) -> Result<Option<LoweredValue<'program, 'dae>>, solve::SolveProgramConstructionError> {
        if !matches!(
            operator,
            dae::BinaryOperator::Multiply | dae::BinaryOperator::ElementwiseMultiply
        ) || !self
            .view
            .value_type(value_type)
            .is_some_and(|ty| ty.scalar_type() == dae::ScalarType::Real)
        {
            return Ok(None);
        }
        let patterns = [
            self.zero_coefficients.omission_pattern(self.view, rhs, lhs),
            self.zero_coefficients.omission_pattern(self.view, lhs, rhs),
        ];
        if !patterns
            .iter()
            .flatten()
            .any(|pattern| pattern.may_prove_zero())
        {
            return Ok(None);
        }
        if !patterns
            .iter()
            .flatten()
            .any(|pattern| pattern.proves_all_zero())
        {
            return self
                .partial_zero_product(value_type, operator, lhs, rhs, patterns, at)
                .map(Some);
        }
        if self.is_zero_size(value_type)? {
            return Ok(Some(LoweredValue::empty(value_type)));
        }
        let dimensions = self
            .view
            .value_type(value_type)
            .expect("the result type resolves")
            .dimensions()
            .to_vec();
        let zero = self
            .builder
            .constant(solve::SolveValue::real(arithmetic_profile(), 0.0), at)?;
        let result = if dimensions.is_empty() {
            zero
        } else {
            self.builder.fill(zero, dimensions, at)?
        };
        Ok(Some(LoweredValue::scalar(value_type, result)))
    }
}
