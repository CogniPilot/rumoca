//! Replay the independent additive value proof for both lift outputs.

use super::*;
use crate::dae_transform::constraints::lifted_values::{LiftedValueProof, ValueNode};

impl<'source, 'borrow, 'storage, 'target> ExpressionRebuilder<'source, 'borrow, 'storage, 'target> {
    pub(super) fn reconstruct_lifted_value(
        &mut self,
        proof: &LiftedValueProof,
        derivative: bool,
        provenance: dae::DaeProvenance,
    ) -> Result<Derivative<'target>, dae::DaeConstructionError> {
        let mut values = Vec::with_capacity(proof.nodes().len());
        for node in proof.nodes() {
            let value = self.reconstruct_lifted_node(node, &values, derivative, provenance)?;
            values.push(value);
        }
        Ok(values[proof.root()])
    }

    fn reconstruct_lifted_node(
        &mut self,
        node: &ValueNode,
        values: &[Derivative<'target>],
        derivative: bool,
        provenance: dae::DaeProvenance,
    ) -> Result<Derivative<'target>, dae::DaeConstructionError> {
        match node {
            ValueNode::Source(source) => {
                let source = self.source.expression_id(*source as usize).unwrap();
                if derivative {
                    self.differentiate_order(source, 1, provenance)
                } else {
                    self.materialize_exact_value(source, provenance)
                        .map(Derivative::Expression)
                }
            }
            ValueNode::Sum { source, terms } => {
                let mut value = self.reconstruct_lifted_sum(terms, values, provenance)?;
                if !derivative {
                    let source = self.source.expression_id(*source as usize).unwrap();
                    value = Derivative::Expression(
                        self.materialize_derivative(value, source, provenance)?,
                    );
                }
                Ok(value)
            }
        }
    }

    fn reconstruct_lifted_sum(
        &mut self,
        terms: &[(usize, bool)],
        values: &[Derivative<'target>],
        provenance: dae::DaeProvenance,
    ) -> Result<Derivative<'target>, dae::DaeConstructionError> {
        let mut value = Derivative::Zero;
        for &(child, negated) in terms {
            let operator = if negated {
                dae::BinaryOperator::Subtract
            } else {
                dae::BinaryOperator::Add
            };
            value = self.combine_sum(operator, value, values[child], provenance)?;
        }
        Ok(value)
    }
}
