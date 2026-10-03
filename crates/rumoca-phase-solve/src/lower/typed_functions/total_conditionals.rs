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

use std::collections::HashMap;

use rumoca_ir_dae as dae;
use rumoca_ir_solve as solve;

use super::{ExpressionLowerer, LoweredValue};

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

/// Whether evaluating `root` can neither fail nor have an effect, memoized in
/// `totality` over the expression graph (iteratively, since the graph may be
/// deep).
pub(super) fn is_total<'dae>(
    view: dae::DaeView<'dae>,
    totality: &mut HashMap<dae::ExprId<'dae>, bool>,
    root: dae::ExprId<'dae>,
) -> bool {
    let mut stack = vec![(root, false)];
    while let Some((expression, expanded)) = stack.pop() {
        if totality.contains_key(&expression) {
            continue;
        }
        let Some(node) = view.expression(expression) else {
            totality.insert(expression, false);
            continue;
        };
        let Some(operands) = total_operation_operands(view, node) else {
            totality.insert(expression, false);
            continue;
        };
        if expanded {
            let total = operands
                .iter()
                .all(|operand| totality.get(operand).copied().unwrap_or(false));
            totality.insert(expression, total);
            continue;
        }
        stack.push((expression, true));
        stack.extend(
            operands
                .into_iter()
                .filter(|operand| !totality.contains_key(operand))
                .map(|operand| (operand, false)),
        );
    }
    totality.get(&root).copied().unwrap_or(false)
}

/// The operands of a node whose own operation is total, or `None` when the
/// operation itself can fail.
fn total_operation_operands<'dae>(
    view: dae::DaeView<'dae>,
    node: dae::ExpressionView<'dae>,
) -> Option<Vec<dae::ExprId<'dae>>> {
    let scalar = node.value_type().scalar_type();
    match node.operation() {
        dae::ExpressionOperation::Literal(literal) => {
            (!matches!(literal, dae::DaeLiteral::String(_))).then(Vec::new)
        }
        dae::ExpressionOperation::Coordinate(_)
        | dae::ExpressionOperation::FunctionValue { .. } => Some(Vec::new()),
        dae::ExpressionOperation::Unary { operator, operand } => {
            // Integer negation overflows at the bottom of its domain.
            (operator != dae::UnaryOperator::Negate || scalar != dae::ScalarType::Integer)
                .then(|| vec![operand])
        }
        dae::ExpressionOperation::Binary { lhs, rhs, .. } => {
            // Real arithmetic follows IEEE 754; comparisons and logical
            // operators always produce a Boolean. Integer arithmetic is
            // checked and can fail.
            matches!(scalar, dae::ScalarType::Real | dae::ScalarType::Boolean)
                .then(|| vec![lhs, rhs])
        }
        dae::ExpressionOperation::Conditional(operands)
        | dae::ExpressionOperation::Array(operands) => Some(operands.iter().collect()),
        dae::ExpressionOperation::Builtin { builtin, arguments } => {
            let total = match builtin {
                dae::PureBuiltin::Max | dae::PureBuiltin::Min => true,
                dae::PureBuiltin::Abs | dae::PureBuiltin::Sqrt => scalar == dae::ScalarType::Real,
                _ => false,
            };
            total.then(|| arguments.iter().collect())
        }
        dae::ExpressionOperation::Index { base, subscripts } => {
            let dimensions = view.expression(base)?.value_type().dimensions().to_vec();
            let in_range = subscripts
                .iter()
                .zip(&dimensions)
                .all(|(subscript, extent)| match subscript {
                    dae::SubscriptView::Whole { .. } => true,
                    dae::SubscriptView::Index { expression, .. } => literal_index(view, expression)
                        .is_some_and(|index| index >= 1 && index <= i64::from(*extent)),
                    dae::SubscriptView::Slice { .. } => false,
                });
            (in_range && subscripts.len() <= dimensions.len()).then(|| vec![base])
        }
        _ => None,
    }
}

fn literal_index<'dae>(view: dae::DaeView<'dae>, expression: dae::ExprId<'dae>) -> Option<i64> {
    match view.expression(expression)?.operation() {
        dae::ExpressionOperation::Literal(dae::DaeLiteral::Integer(index)) => Some(*index),
        _ => None,
    }
}
