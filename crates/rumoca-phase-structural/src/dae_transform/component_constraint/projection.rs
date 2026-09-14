//! Source-bound component projection, with optional inverse-definition scope.

use std::collections::BTreeSet;

use super::{ComponentExpression, DifferentiationFacts, component_indices, dae};

pub(super) struct ComponentProjection<'facts, 'dae> {
    view: dae::DaeView<'dae>,
    facts: &'facts DifferentiationFacts,
    state_arrays: Option<&'facts [Option<u32>]>,
    active: BTreeSet<(u32, usize)>,
}

impl<'facts, 'dae> ComponentProjection<'facts, 'dae> {
    pub(super) fn new(
        view: dae::DaeView<'dae>,
        facts: &'facts DifferentiationFacts,
        state_arrays: Option<&'facts [Option<u32>]>,
    ) -> Self {
        Self {
            view,
            facts,
            state_arrays,
            active: BTreeSet::new(),
        }
    }

    pub(super) fn derive(
        &mut self,
        expression: dae::ExprId<'dae>,
        scalar: usize,
    ) -> Option<ComponentExpression> {
        let key = (expression.index(), scalar);
        if !self.active.insert(key) {
            return None;
        }
        let result = self.derive_operation(expression, scalar);
        self.active.remove(&key);
        result
    }

    fn derive_operation(
        &mut self,
        expression: dae::ExprId<'dae>,
        scalar: usize,
    ) -> Option<ComponentExpression> {
        let node = self.view.expression(expression)?;
        let indices = component_indices(node.value_type(), scalar)?;
        if indices.is_empty() {
            return Some(ComponentExpression::Source {
                expression: expression.index(),
                indices,
            });
        }
        match node.operation() {
            dae::ExpressionOperation::Array(elements) => {
                let stride = self
                    .view
                    .expression(elements.get(0)?)?
                    .value_type()
                    .scalar_count()?;
                let element = elements.get(scalar.checked_div(stride)?)?;
                return self.derive(element, scalar.checked_rem(stride)?);
            }
            dae::ExpressionOperation::Binary {
                operator: operator @ (dae::BinaryOperator::Add | dae::BinaryOperator::Subtract),
                lhs,
                rhs,
            } => {
                return Some(ComponentExpression::Sum {
                    operator,
                    lhs: Box::new(self.derive(lhs, scalar)?),
                    rhs: Box::new(self.derive(rhs, scalar)?),
                });
            }
            dae::ExpressionOperation::Coordinate(dae::CoordinateView::Algebraic(algebraic)) => {
                if let Some(definition) = self.facts.algebraic_definition(self.view, algebraic) {
                    return self.derive(definition, scalar);
                }
            }
            dae::ExpressionOperation::Coordinate(dae::CoordinateView::State(state)) => {
                if let Some(definition) = self
                    .state_arrays
                    .and_then(|definitions| definitions[state.index() as usize])
                    .and_then(|definition| self.view.expression_id(definition as usize))
                {
                    return self.derive(definition, scalar);
                }
            }
            dae::ExpressionOperation::ArrayUpdate {
                base,
                value,
                subscripts,
            } => {
                let selection =
                    super::super::component_projection::literal_indices(self.view, subscripts)?;
                if selection.len() != indices.len() {
                    return None;
                }
                let selected = selection
                    .iter()
                    .zip(&indices)
                    .all(|(&selected, &index)| selected == index as usize - 1);
                return if selected {
                    self.derive(value, 0)
                } else {
                    self.derive(base, scalar)
                };
            }
            _ => {}
        }
        Some(ComponentExpression::Source {
            expression: expression.index(),
            indices,
        })
    }
}
