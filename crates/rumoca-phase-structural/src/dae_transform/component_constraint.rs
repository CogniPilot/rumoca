//! A component view used by index reduction without replacing tensor owners.

mod definitions;
mod projection;

use rumoca_ir_dae as dae;

pub(super) use definitions::derive_definitions;
use projection::ComponentProjection;

use super::constraints::DifferentiationFacts;
use super::differentiation::Derivative;
use super::expressions::ExpressionRebuilder;

/// Phase-local selection tied to a residual in the source DAE. The same plan
/// supplies proof leaves and reconstruction; it is never a scalar equation owner.
#[derive(Clone)]
pub(super) struct ComponentConstraint {
    pub(super) scalar: usize,
    pub(super) indices: Box<[u32]>,
    expression: ComponentExpression,
}

#[derive(Clone, PartialEq, Eq)]
enum ComponentExpression {
    Zero,
    Source {
        expression: u32,
        indices: Box<[u32]>,
    },
    Sum {
        operator: dae::BinaryOperator,
        lhs: Box<Self>,
        rhs: Box<Self>,
    },
}

impl ComponentConstraint {
    pub(super) fn derive<'dae>(
        view: dae::DaeView<'dae>,
        facts: &DifferentiationFacts,
        residual: dae::ExprId<'dae>,
        scalar: usize,
    ) -> Option<Self> {
        let node = view.expression(residual)?;
        if node.binder_domain().is_some() {
            return None;
        }
        let indices = component_indices(node.value_type(), scalar)?;
        if indices.is_empty() {
            return None;
        }
        let expression = ComponentProjection::new(view, facts, None).derive(residual, scalar)?;
        if expression.is_identically_zero() {
            return None;
        }
        Some(Self {
            scalar,
            indices,
            expression,
        })
    }

    pub(super) fn leaves(&self) -> Vec<u32> {
        let mut leaves = Vec::new();
        self.expression.collect_leaves(&mut leaves);
        leaves
    }
}

impl ComponentExpression {
    fn is_identically_zero(&self) -> bool {
        match self {
            Self::Zero => true,
            Self::Source { .. } => false,
            Self::Sum { operator, lhs, rhs } => {
                (*operator == dae::BinaryOperator::Subtract && lhs == rhs)
                    || (lhs.is_identically_zero() && rhs.is_identically_zero())
            }
        }
    }

    fn collect_leaves(&self, leaves: &mut Vec<u32>) {
        match self {
            Self::Zero => {}
            Self::Source { expression, .. } => leaves.push(*expression),
            Self::Sum { lhs, rhs, .. } => {
                lhs.collect_leaves(leaves);
                rhs.collect_leaves(leaves);
            }
        }
    }
}

fn component_indices(value_type: &dae::ValueType, mut scalar: usize) -> Option<Box<[u32]>> {
    if scalar >= value_type.scalar_count()? {
        return None;
    }
    let mut indices = vec![0; value_type.dimensions().len()];
    for (index, &extent) in indices.iter_mut().zip(value_type.dimensions()).rev() {
        *index = u32::try_from(scalar.checked_rem(extent as usize)?)
            .ok()?
            .checked_add(1)?;
        scalar = scalar.checked_div(extent as usize)?;
    }
    Some(indices.into_boxed_slice())
}

impl<'source, 'borrow, 'storage, 'target> ExpressionRebuilder<'source, 'borrow, 'storage, 'target> {
    pub(super) fn materialize_component_value(
        &mut self,
        component: &ComponentConstraint,
        provenance: dae::DaeProvenance,
    ) -> Result<dae::ExprId<'target>, dae::DaeConstructionError> {
        self.materialize_component_expression(&component.expression, provenance)
    }

    fn materialize_component_expression(
        &mut self,
        expression: &ComponentExpression,
        provenance: dae::DaeProvenance,
    ) -> Result<dae::ExprId<'target>, dae::DaeConstructionError> {
        match expression {
            ComponentExpression::Zero => self
                .target
                .at(provenance)
                .literal(dae::DaeLiteral::Real(0.0)),
            ComponentExpression::Source {
                expression,
                indices,
            } => {
                let source = self.source.expression_id(*expression as usize).unwrap();
                let value = self.materialize_exact_value(source, provenance)?;
                self.project_component_value(value, indices, provenance)
            }
            ComponentExpression::Sum { operator, lhs, rhs } => {
                let lhs = self.materialize_component_expression(lhs, provenance)?;
                let rhs = self.materialize_component_expression(rhs, provenance)?;
                self.target.at(provenance).binary(*operator, lhs, rhs)
            }
        }
    }

    pub(super) fn differentiate_component(
        &mut self,
        component: &ComponentConstraint,
        order: u8,
        provenance: dae::DaeProvenance,
    ) -> Result<Derivative<'target>, dae::DaeConstructionError> {
        let derivative =
            self.differentiate_component_expression(&component.expression, order, provenance)?;
        // The parent residual remains a tensor. A zero replacement here is
        // scalar, so it cannot use the parent's shaped-zero constructor.
        match derivative {
            Derivative::Zero => self
                .target
                .at(provenance)
                .literal(dae::DaeLiteral::Real(0.0))
                .map(Derivative::Expression),
            derivative => Ok(derivative),
        }
    }

    fn differentiate_component_expression(
        &mut self,
        expression: &ComponentExpression,
        order: u8,
        provenance: dae::DaeProvenance,
    ) -> Result<Derivative<'target>, dae::DaeConstructionError> {
        match expression {
            ComponentExpression::Zero => Ok(Derivative::Zero),
            ComponentExpression::Source {
                expression,
                indices,
            } => {
                let source = self.source.expression_id(*expression as usize).unwrap();
                match self.differentiate_order(source, order, provenance)? {
                    Derivative::Zero => Ok(Derivative::Zero),
                    Derivative::Expression(value) => self
                        .project_component_value(value, indices, provenance)
                        .map(Derivative::Expression),
                }
            }
            ComponentExpression::Sum { operator, lhs, rhs } => {
                let lhs = self.differentiate_component_expression(lhs, order, provenance)?;
                let rhs = self.differentiate_component_expression(rhs, order, provenance)?;
                self.combine_sum(*operator, lhs, rhs, provenance)
            }
        }
    }

    fn project_component_value(
        &mut self,
        value: dae::ExprId<'target>,
        indices: &[u32],
        provenance: dae::DaeProvenance,
    ) -> Result<dae::ExprId<'target>, dae::DaeConstructionError> {
        if indices.is_empty() {
            return Ok(value);
        }
        let subscripts = component_subscripts(self.target, indices, provenance)?;
        self.target.at(provenance).index(value, subscripts)
    }
}

pub(super) fn component_subscripts<'target>(
    target: &mut dae::Expressions<'_, 'target>,
    indices: &[u32],
    provenance: dae::DaeProvenance,
) -> Result<Vec<dae::Subscript<'target>>, dae::DaeConstructionError> {
    indices
        .iter()
        .map(|&index| {
            let expression = target
                .at(provenance)
                .literal(dae::DaeLiteral::Integer(i64::from(index)))?;
            Ok(dae::Subscript::Index {
                expression,
                provenance,
            })
        })
        .collect()
}
