//! Source-bound component projection, with optional inverse-definition scope.

use std::collections::BTreeSet;

use super::super::equalities::EqualitySign;
use super::definitions::StateArrayDefinition;
use super::{ComponentExpression, DifferentiationFacts, component_indices, dae};

pub(super) struct ComponentProjection<'facts, 'dae> {
    view: dae::DaeView<'dae>,
    facts: &'facts DifferentiationFacts,
    state_arrays: Option<&'facts [Option<StateArrayDefinition>]>,
    active: BTreeSet<(u32, usize)>,
}

impl<'facts, 'dae> ComponentProjection<'facts, 'dae> {
    pub(super) fn new(
        view: dae::DaeView<'dae>,
        facts: &'facts DifferentiationFacts,
        state_arrays: Option<&'facts [Option<StateArrayDefinition>]>,
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
            dae::ExpressionOperation::Unary {
                operator: dae::UnaryOperator::Plus,
                operand,
            } => return self.derive(operand, scalar),
            dae::ExpressionOperation::Unary {
                operator: dae::UnaryOperator::Negate,
                operand,
            } => {
                return self
                    .derive(operand, scalar)
                    .map(|value| signed(value, EqualitySign::Opposite));
            }
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
            dae::ExpressionOperation::Coordinate(coordinate) => {
                if let Some((definition, sign)) = self.state_array_definition(expression) {
                    let expression = self.view.expression_id(definition.expression as usize)?;
                    return self
                        .derive(expression, scalar)
                        .map(|value| signed(signed(value, definition.sign), sign));
                }
                if let dae::CoordinateView::Algebraic(algebraic) = coordinate
                    && let Some(definition) = self.facts.algebraic_definition(self.view, algebraic)
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
    fn state_array_definition(
        &self,
        expression: dae::ExprId<'dae>,
    ) -> Option<(StateArrayDefinition, EqualitySign)> {
        let definitions = self.state_arrays?;
        let (state, sign) = super::super::constraints::exact_state_anchor(
            self.view,
            &self.facts.equalities,
            expression,
        )?;
        Some((definitions[state.index() as usize]?, sign))
    }
}

fn signed(value: ComponentExpression, sign: EqualitySign) -> ComponentExpression {
    match sign {
        EqualitySign::Same => value,
        EqualitySign::Opposite => ComponentExpression::Sum {
            operator: dae::BinaryOperator::Subtract,
            lhs: Box::new(ComponentExpression::Zero),
            rhs: Box::new(value),
        },
    }
}
