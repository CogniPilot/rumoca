//! Formal coordinate differentiation shares the ordinary tensor rules.

use super::*;

impl<'source, 'borrow, 'storage, 'target> ExpressionRebuilder<'source, 'borrow, 'storage, 'target> {
    pub(super) fn formal_coordinate(
        &mut self,
        coordinate: dae::CoordinateView<'source>,
        order: u8,
        provenance: dae::DaeProvenance,
    ) -> Result<Option<Derivative<'target>>, dae::DaeConstructionError> {
        let (variable, level) = match coordinate {
            dae::CoordinateView::State(id) => (id.index(), usize::from(order)),
            dae::CoordinateView::Algebraic(id) => (id.index(), usize::from(order)),
            dae::CoordinateView::Derivative(id) => (id.index(), usize::from(order) + 1),
            dae::CoordinateView::Binder(_) => return Ok(Some(Derivative::Zero)),
            dae::CoordinateView::Parameter(_)
            | dae::CoordinateView::Time
            | dae::CoordinateView::FunctionParameter(_) => return Ok(None),
            _ => return Err(unsupported("formal derivative coordinate", 0, provenance)),
        };
        let derivative = self.variables[variable as usize]
            .formal_derivatives
            .get(level - 1)
            .copied()
            .ok_or_else(|| unsupported("formal derivative order", variable, provenance))?;
        self.target
            .at(provenance)
            .coordinate(dae::CoordinateInput::Algebraic(derivative))
            .map(|value| Some(Derivative::Expression(value)))
    }

    pub(super) fn check_formal_operation(
        &self,
        id: dae::ExprId<'source>,
        order: u8,
        provenance: dae::DaeProvenance,
    ) -> Result<(), dae::DaeConstructionError> {
        let admitted = (1..=2).contains(&order)
            && match self.source.expression(id).unwrap().operation() {
                dae::ExpressionOperation::Literal(_)
                | dae::ExpressionOperation::Coordinate(_)
                | dae::ExpressionOperation::Array(_) => true,
                dae::ExpressionOperation::Unary { operator, .. } => matches!(
                    operator,
                    dae::UnaryOperator::Plus | dae::UnaryOperator::Negate
                ),
                dae::ExpressionOperation::Binary { operator, lhs, rhs } => {
                    super::super::builtin_profiles::is_differentiable_binary(operator)
                        || super::super::builtin_profiles::is_differentiable_power(
                            self.source,
                            operator,
                            lhs,
                            rhs,
                        )
                }
                dae::ExpressionOperation::Builtin {
                    builtin: dae::PureBuiltin::Smooth,
                    arguments,
                } => super::super::smooth_order::smooth_operands(self.source, arguments).is_some(),
                dae::ExpressionOperation::Builtin {
                    builtin: dae::PureBuiltin::Min | dae::PureBuiltin::Max,
                    arguments,
                } => super::super::builtin_profiles::is_differentiable_selection(
                    self.source,
                    arguments,
                    order,
                    super::super::builtin_profiles::EventOwners::Issued,
                ),
                dae::ExpressionOperation::Builtin { builtin, .. } => {
                    super::super::builtin_profiles::is_differentiable_builtin(builtin, order)
                }
                // Past its `smooth` certificate, a conditional inside a
                // function, whose guard cannot own an event (a medium's
                // `h_T = smooth(0, if T < Tlimit ...)`), still differentiates
                // branch-wise, without one: MLS §3.7.5 lets a tool avoid the
                // events of the relations inside `smooth`. In the model's own
                // equations the guard must own one, as above.
                dae::ExpressionOperation::Conditional(operands) => {
                    super::super::smooth_order::admits_live_guards(self.smooth_certificate, order)
                        || super::super::parameter_conditionals::guards_taking_events(
                            self.source,
                            &self.function_context,
                            operands,
                        )
                        .is_some()
                        || (self.smooth_certificate.is_some() && !self.function_context.is_empty())
                }
                dae::ExpressionOperation::Index { subscripts, .. } => {
                    subscripts.iter().all(|subscript| match subscript {
                        dae::SubscriptView::Whole { .. } => true,
                        dae::SubscriptView::Index { expression, .. }
                        | dae::SubscriptView::Slice { expression, .. } => {
                            invariant_index(self.source, expression)
                        }
                    })
                }
                dae::ExpressionOperation::Field { base, field } => self
                    .function_context
                    .projected_field(self.source, base, field)
                    .is_some(),
                _ => false,
            };
        if admitted {
            Ok(())
        } else {
            // The refusal carries the operation's own site, not the site of
            // the equation that reads it.
            let operation = self.source.expression(id).unwrap().provenance();
            Err(unsupported(
                FORMAL_OPERATION_REFUSAL,
                id.index(),
                if operation.span().is_dummy() {
                    provenance
                } else {
                    operation
                },
            ))
        }
    }
}

// A compact family's binder is fixed while differentiating at a domain point.
fn invariant_index<'dae>(view: dae::DaeView<'dae>, id: dae::ExprId<'dae>) -> bool {
    if is_time_invariant(view, id) {
        return true;
    }
    match view.expression(id).unwrap().operation() {
        dae::ExpressionOperation::Coordinate(dae::CoordinateView::Binder(_)) => true,
        dae::ExpressionOperation::Unary {
            operator: dae::UnaryOperator::Plus | dae::UnaryOperator::Negate,
            operand,
        } => invariant_index(view, operand),
        dae::ExpressionOperation::Binary { operator, lhs, rhs }
            if super::super::builtin_profiles::is_differentiable_binary(operator) =>
        {
            invariant_index(view, lhs) && invariant_index(view, rhs)
        }
        _ => false,
    }
}

/// The refusal kind of an operation the formal derivative has no rule for;
/// its index is the operation's source expression ordinal.
pub(in crate::dae_transform) const FORMAL_OPERATION_REFUSAL: &str = "formal derivative operation";

fn unsupported(
    kind: &'static str,
    index: u32,
    provenance: dae::DaeProvenance,
) -> dae::DaeConstructionError {
    dae::DaeConstructionError::IncompleteDefinition {
        kind,
        index,
        span: provenance.span(),
    }
}
