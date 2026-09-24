//! Sufficient unit-coefficient proofs for complete scalar residuals.
//!
//! Incidence remains conservative. A coordinate is a causal candidate only
//! when it occurs once as a signed additive term and nowhere in its siblings.
//! All other operations retain their dependencies without claiming an inverse.

#[cfg(test)]
mod coordinate_tests;
#[cfg(test)]
mod tests;

use super::*;

impl<'dae> IncidenceBuilder<'_, 'dae> {
    pub(super) fn unit_candidates(
        &mut self,
        expression: dae::ExprId<'dae>,
        scalar: usize,
        point: Option<(dae::DomainId<'dae>, &[i64])>,
    ) -> Result<Vec<usize>, StructuralError> {
        let node = self
            .view
            .expression(expression)
            .expect("checked expression");
        match node.operation() {
            dae::ExpressionOperation::Coordinate(_) => {
                self.unknown_dependencies(expression, scalar, point)
            }
            dae::ExpressionOperation::Index { base, subscripts }
                if matches!(
                    self.view.expression(base).map(|n| n.operation()),
                    Some(dae::ExpressionOperation::Coordinate(_))
                ) && subscripts.iter().all(|subscript| match subscript {
                    dae::SubscriptView::Index { expression, .. } => matches!(
                        self.view.expression(expression).map(|n| n.operation()),
                        Some(dae::ExpressionOperation::Literal(dae::DaeLiteral::Integer(
                            _
                        )))
                    ),
                    _ => false,
                }) =>
            {
                self.unknown_dependencies(expression, scalar, point)
            }
            dae::ExpressionOperation::Unary {
                operator: dae::UnaryOperator::Plus | dae::UnaryOperator::Negate,
                operand,
            } => self.unit_candidates(operand, scalar, point),
            dae::ExpressionOperation::Binary {
                operator: dae::BinaryOperator::Add | dae::BinaryOperator::Subtract,
                lhs,
                rhs,
            } => {
                let left_scalar = self.operand_scalar(lhs, scalar);
                let right_scalar = self.operand_scalar(rhs, scalar);
                let mut left = self.unit_candidates(lhs, left_scalar, point)?;
                let mut right = self.unit_candidates(rhs, right_scalar, point)?;
                let left_reads = self.unknown_dependencies(lhs, left_scalar, point)?;
                let right_reads = self.unknown_dependencies(rhs, right_scalar, point)?;
                left.retain(|target| !right_reads.contains(target));
                right.retain(|target| !left_reads.contains(target));
                left.extend(right);
                Ok(left)
            }
            _ => Ok(Vec::new()),
        }
    }

    fn operand_scalar(&self, expression: dae::ExprId<'dae>, scalar: usize) -> usize {
        if self
            .view
            .expression(expression)
            .expect("checked operand")
            .value_type()
            .is_scalar()
        {
            0
        } else {
            scalar
        }
    }

    pub(super) fn unknown_dependencies(
        &mut self,
        expression: dae::ExprId<'dae>,
        scalar: usize,
        point: Option<(dae::DomainId<'dae>, &[i64])>,
    ) -> Result<Vec<usize>, StructuralError> {
        let mut reads = Vec::new();
        let unknown_map = self.unknown_map;
        for_each_scalar_coordinate_cached(
            self.view,
            expression,
            scalar,
            point,
            &mut self.projection_cache,
            |coordinate, scalar| {
                if let Some(unknown) = resolve_coordinate(unknown_map, coordinate, scalar) {
                    reads.push(unknown);
                }
            },
        )
        .map_err(projection::projection_error)?;
        Ok(reads)
    }
}
