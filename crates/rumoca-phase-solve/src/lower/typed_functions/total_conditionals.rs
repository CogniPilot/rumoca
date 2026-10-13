//! Conditional expressions whose branches cannot fail.
//!
//! MLS 3.7 §3.6.5 evaluates only the selected branch of an if-expression.
//! A branch whose evaluation can fail (a call, Integer arithmetic that can
//! overflow, a computed subscript) is therefore lowered in its own region,
//! entered only when selected. A branch built only from total operations
//! (IEEE Real arithmetic, comparisons, logical operators, reads, constant
//! in-range subscripts, and nested total conditionals) has no observable
//! effect besides its value, so evaluating it unconditionally and selecting
//! the result is the same program. Such a conditional lowers to `Select`
//! operations in the enclosing region, where its operands share the values
//! the region already computed instead of being rebuilt per branch
//! (SPEC_0040 SOLVE-C61).

use rumoca_ir_dae as dae;
use rumoca_ir_solve as solve;

use super::{ExpressionLowerer, LoweredValue};
use crate::lower::dae_faults::{DaeFaults, FaultMemo};

impl<'program, 'dae> ExpressionLowerer<'_, 'program, 'dae> {
    /// Lower `operands` (`[condition, value, (condition, value)*, else]`) as
    /// selections when every operand after the first condition is total.
    pub(super) fn total_conditional(
        &mut self,
        value_type: dae::ValueTypeId<'dae>,
        operands: &[dae::ExprId<'dae>],
        provenance: rumoca_core::Span,
    ) -> Result<Option<LoweredValue<'program, 'dae>>, solve::SolveProgramConstructionError> {
        if !operands[1..]
            .iter()
            .all(|operand| is_total(self.view, &mut self.totality, *operand))
        {
            return Ok(None);
        }
        let (otherwise, branches) = operands
            .split_last()
            .ok_or(solve::SolveProgramConstructionError::InvalidRegion { provenance })?;
        let otherwise = self.expression(*otherwise)?;
        let mut selected = self.coerce_value(otherwise, value_type, provenance)?;
        for branch in branches.chunks_exact(2).rev() {
            let condition = self.expression(branch[0])?.only_register(provenance)?;
            let value = self.expression(branch[1])?;
            let value = self.coerce_value(value, value_type, provenance)?;
            let leaves = value
                .leaves
                .iter()
                .zip(&selected.leaves)
                .map(|(then, otherwise)| {
                    self.builder
                        .select(condition, *then, *otherwise, provenance)
                })
                .collect::<Result<Vec<_>, _>>()?;
            selected = LoweredValue { value_type, leaves };
        }
        Ok(Some(selected))
    }
}

/// Whether evaluating `root` can neither fail nor have an effect: the shared
/// DAE fault classification, with loop binders at their declared ranges.
pub(super) fn is_total<'dae>(
    view: dae::DaeView<'dae>,
    totality: &mut FaultMemo<'dae>,
    root: dae::ExprId<'dae>,
) -> bool {
    DaeFaults::new(view, &[])
        .differentiable()
        .is_total(totality, root)
}
