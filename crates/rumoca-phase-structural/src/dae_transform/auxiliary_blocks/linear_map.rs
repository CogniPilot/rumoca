//! Prove source tensor maps linear in one aggregate algebraic coordinate.

use super::super::constraints::DifferentiationFacts;
use super::tensor_expression::{Product, SourceValue, TensorExpression};
use super::{AuxiliaryBlock, AuxiliarySystem, vector_unknown};
use rumoca_eval_dae::FunctionCallContext;
use rumoca_ir_dae as dae;
use std::collections::BTreeSet;
use std::sync::Arc;

pub(super) fn derive_maps(
    view: dae::DaeView<'_>,
    facts: &DifferentiationFacts,
    blocks: &mut [Option<Arc<AuxiliaryBlock>>],
) {
    let mut traversal = dae::ExpressionTraversal::new();
    for residual in view.continuous_owners().flat_map(source_residuals) {
        derive_equation_map(view, facts, residual, blocks, &mut traversal);
    }
}

fn derive_equation_map<'dae>(
    view: dae::DaeView<'dae>,
    facts: &DifferentiationFacts,
    residual: dae::ExprId<'dae>,
    blocks: &mut [Option<Arc<AuxiliaryBlock>>],
    traversal: &mut dae::ExpressionTraversal<'dae>,
) {
    let node = view.expression(residual).unwrap();
    let [extent] = node.value_type().dimensions() else {
        return;
    };
    if *extent == 0 || node.binder_domain().is_some() {
        return;
    }
    let Some((lhs, rhs)) = crate::residual_normalization::equation_sides(view, residual) else {
        return;
    };
    for (map, value) in [(lhs, rhs), (rhs, lhs)] {
        let Some(anchors) = facts.materialized_state_anchors(view, value.index()) else {
            continue;
        };
        let mut candidates = Vec::new();
        traversal.visit_pruned(view, [map], |expression, _| {
            if let Some((variable, size)) = vector_unknown(view, expression)
                && size == *extent
                && blocks[variable as usize].is_none()
                && !facts.can_materialize_value(view, expression.index())
            {
                candidates.push(variable);
            }
            true
        });
        candidates.sort_unstable();
        candidates.dedup();
        for variable in candidates {
            let mut walk = LinearMap {
                view,
                facts,
                variable,
                extent: *extent,
                active: BTreeSet::new(),
            };
            let Some(matrix) = walk.coefficient(map, &FunctionCallContext::default()) else {
                continue;
            };
            let mut leaves = Vec::new();
            matrix.operands(&mut leaves);
            let mut states = anchors.clone();
            for leaf in leaves {
                states.extend(
                    facts
                        .materialized_state_anchors_in_context(
                            view,
                            leaf.expression,
                            &leaf.context(view),
                        )
                        .expect("independent coefficient leaf"),
                );
            }
            states.sort_unstable();
            states.dedup();
            blocks[variable as usize] = Some(Arc::new(AuxiliaryBlock {
                variable,
                extent: *extent,
                system: AuxiliarySystem::Map {
                    residual: residual.index(),
                    matrix,
                    rhs: SourceValue::model(value.index()),
                },
                state_anchors: states.into_boxed_slice(),
            }));
        }
    }
}

fn source_residuals(
    owner: dae::ContinuousOwnerView<'_>,
) -> Box<dyn Iterator<Item = dae::ExprId<'_>> + '_> {
    match owner {
        dae::ContinuousOwnerView::Residual { equation, .. } => {
            Box::new(std::iter::once(equation.residual()))
        }
        dae::ContinuousOwnerView::Structured { family, .. }
            if family.scalar_view() == rumoca_core::ComprehensionScalarView::RowMajorProjection =>
        {
            Box::new(family.bodies().iter())
        }
        _ => Box::new(std::iter::empty()),
    }
}

struct LinearMap<'dae, 'facts> {
    view: dae::DaeView<'dae>,
    facts: &'facts DifferentiationFacts,
    variable: u32,
    extent: u32,
    active: BTreeSet<(u32, Vec<u32>)>,
}

impl<'dae> LinearMap<'dae, '_> {
    fn coefficient(
        &mut self,
        expression: dae::ExprId<'dae>,
        context: &FunctionCallContext<'dae>,
    ) -> Option<TensorExpression> {
        let context = context.scoped_to_expression(self.view, expression);
        let key = (
            expression.index(),
            context.call_path().map(dae::ExprId::index).collect(),
        );
        if !self.active.insert(key.clone()) {
            return None;
        }
        let result = self.operation(expression, &context);
        self.active.remove(&key);
        result
    }

    fn operation(
        &mut self,
        expression: dae::ExprId<'dae>,
        context: &FunctionCallContext<'dae>,
    ) -> Option<TensorExpression> {
        if let Some(selected) = context.selected_branch(self.view, expression) {
            return self.coefficient(selected, context);
        }
        let node = self.view.expression(expression)?;
        match node.operation() {
            dae::ExpressionOperation::Coordinate(dae::CoordinateView::Algebraic(variable)) => {
                if variable.index() == self.variable {
                    return Some(TensorExpression::Identity(self.extent));
                }
                let definition = self.facts.algebraic_definition(self.view, variable)?;
                self.coefficient(definition, context)
            }
            dae::ExpressionOperation::Coordinate(dae::CoordinateView::FunctionParameter(
                parameter,
            )) => self.coefficient(context.parameter_argument(parameter)?, context),
            dae::ExpressionOperation::Call { .. } => {
                let (value, nested) = context.call_result(self.view, expression)?;
                self.coefficient(value, &nested)
            }
            dae::ExpressionOperation::Field { base, field } => {
                let (value, nested) = context.projected_field(self.view, base, field)?;
                self.coefficient(value, &nested)
            }
            dae::ExpressionOperation::Unary { operator, operand } => {
                let coefficient = self.coefficient(operand, context)?;
                match operator {
                    dae::UnaryOperator::Plus => Some(coefficient),
                    dae::UnaryOperator::Negate => {
                        Some(TensorExpression::Negate(Box::new(coefficient)))
                    }
                    _ => None,
                }
            }
            dae::ExpressionOperation::Binary { operator, lhs, rhs } => {
                self.binary(operator, lhs, rhs, context)
            }
            dae::ExpressionOperation::Index { base, subscripts } => {
                if subscripts.len() != 1 || !node.value_type().is_scalar() {
                    return None;
                }
                let dae::SubscriptView::Index {
                    expression: index, ..
                } = subscripts.get(0)?
                else {
                    return None;
                };
                if !self.invariant_index(index, context) {
                    return None;
                }
                Some(TensorExpression::Index(
                    Box::new(self.coefficient(base, context)?),
                    SourceValue::new(index, context),
                ))
            }
            dae::ExpressionOperation::Array(elements)
                if node.value_type().dimensions().len() == 1 =>
            {
                let rows = elements
                    .iter()
                    .map(|element| self.coefficient_or_zero(element, context))
                    .collect::<Option<Box<[_]>>>()?;
                Some(TensorExpression::Array(rows))
            }
            _ => None,
        }
    }

    fn coefficient_or_zero(
        &mut self,
        expression: dae::ExprId<'dae>,
        context: &FunctionCallContext<'dae>,
    ) -> Option<TensorExpression> {
        self.coefficient(expression, context).or_else(|| {
            self.facts
                .expression_is_zero(self.view, expression, context)
                .then(|| TensorExpression::Zero(vec![self.extent].into_boxed_slice()))
        })
    }

    fn sum(
        &mut self,
        operator: dae::BinaryOperator,
        lhs: dae::ExprId<'dae>,
        rhs: dae::ExprId<'dae>,
        context: &FunctionCallContext<'dae>,
    ) -> Option<TensorExpression> {
        use dae::BinaryOperator as B;
        let left = self.coefficient(lhs, context);
        let right = self.coefficient(rhs, context);
        match (left, right) {
            (Some(a), Some(b)) => Some(TensorExpression::Sum(operator, Box::new(a), Box::new(b))),
            (Some(a), None) if self.facts.expression_is_zero(self.view, rhs, context) => Some(a),
            (None, Some(b)) if self.facts.expression_is_zero(self.view, lhs, context) => {
                Some(if operator == B::Subtract {
                    TensorExpression::Negate(Box::new(b))
                } else {
                    b
                })
            }
            _ => None,
        }
    }

    fn invariant_index(
        &self,
        expression: dae::ExprId<'dae>,
        context: &FunctionCallContext<'dae>,
    ) -> bool {
        let context = context.scoped_to_expression(self.view, expression);
        match self.view.expression(expression).unwrap().operation() {
            dae::ExpressionOperation::Literal(dae::DaeLiteral::Integer(_)) => true,
            dae::ExpressionOperation::Coordinate(dae::CoordinateView::Parameter(_)) => true,
            dae::ExpressionOperation::Coordinate(dae::CoordinateView::FunctionParameter(
                parameter,
            )) => context
                .parameter_argument(parameter)
                .is_some_and(|argument| self.invariant_index(argument, &context)),
            _ => false,
        }
    }

    fn independent(
        &self,
        expression: dae::ExprId<'dae>,
        context: &FunctionCallContext<'dae>,
    ) -> Option<TensorExpression> {
        self.facts
            .materialized_state_anchors_in_context(self.view, expression.index(), context)
            .map(|_| TensorExpression::Source(SourceValue::new(expression, context)))
    }

    fn binary(
        &mut self,
        operator: dae::BinaryOperator,
        lhs: dae::ExprId<'dae>,
        rhs: dae::ExprId<'dae>,
        context: &FunctionCallContext<'dae>,
    ) -> Option<TensorExpression> {
        use dae::BinaryOperator as B;
        if matches!(operator, B::Add | B::Subtract) {
            return self.sum(operator, lhs, rhs, context);
        }
        if operator != B::Multiply {
            return None;
        }
        for (dependent, independent, dependent_left) in [(lhs, rhs, true), (rhs, lhs, false)] {
            let Some(value) = self.independent(independent, context) else {
                continue;
            };
            let Some(coefficient) = self.coefficient(dependent, context) else {
                continue;
            };
            let rank = self
                .view
                .expression(dependent)?
                .value_type()
                .dimensions()
                .len();
            let independent_rank = self
                .view
                .expression(independent)?
                .value_type()
                .dimensions()
                .len();
            return match (rank, independent_rank, dependent_left) {
                (0 | 1, 0, _) => Some(TensorExpression::product(
                    Product::Multiply,
                    value,
                    coefficient,
                )),
                (0, 1, _) => Some(TensorExpression::product(
                    Product::Outer,
                    value,
                    coefficient,
                )),
                (1, 2, false) => Some(TensorExpression::product(
                    Product::Multiply,
                    value,
                    coefficient,
                )),
                _ => None,
            };
        }
        None
    }
}
