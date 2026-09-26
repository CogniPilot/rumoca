//! Constant pure-call folding and literal propagation (SPEC_0043 §4).
//!
//! A model-level expression of constant variability that contains a call of a
//! pure Modelica function, so every argument is a compile-time constant
//! (including a call with no arguments), is evaluated once at construction by
//! the checked DAE evaluator that settles STRUCT-T10(a) values and replaced by
//! its literal value. A call the evaluator cannot settle (a foreign body, an
//! unsupported operation, a value that is not a number) stays a call; a call
//! whose evaluation fails (an assertion, a non-finite or out-of-range value) is
//! a compile error naming the call, because the same evaluation would fail at
//! every instant it runs.
//!
//! A continuous algebraic declaration whose own residual owner states `v -
//! literal = 0` for the whole declaration is then read as that literal by every
//! other expression. The owner is kept, so the declaration stays computed and
//! observable, and solutions correspond one to one with the source.

use std::sync::Arc;

use rumoca_eval_dae::{NumericEvaluationErrorKind, NumericEvaluator};
use rumoca_ir_dae as dae;

use super::evaluable_parameters::FoldedValue;
use crate::StructuralError;

/// A literal replacement per source expression ordinal.
pub(super) type LiteralPlan = Vec<Option<Arc<FoldedValue>>>;

/// Fold every constant pure call of `model` and propagate every literal
/// binding, one checked reconstruction per round.
///
/// Returns `None` when nothing is folded, leaving the source untouched.
pub fn fold_constant_values(model: &dae::Dae) -> Result<Option<dae::Dae>, StructuralError> {
    let mut current: Option<dae::Dae> = None;
    let calls = current
        .as_ref()
        .unwrap_or(model)
        .inspect(constant_call_plan)?;
    if calls.iter().any(Option::is_some) {
        current = Some(super::reconstruction::rebuild_literal_expressions(
            current.as_ref().unwrap_or(model),
            &calls,
        )?);
    }
    // Each round replaces every remaining read of a bound declaration, so a
    // round only finds work when an earlier round bound a new declaration.
    for _ in 0..model.inspect(|view| view.variable_count()) {
        let bindings = current
            .as_ref()
            .unwrap_or(model)
            .inspect(literal_binding_plan);
        if bindings.iter().all(Option::is_none) {
            break;
        }
        current = Some(super::reconstruction::rebuild_literal_expressions(
            current.as_ref().unwrap_or(model),
            &bindings,
        )?);
    }
    Ok(current)
}

/// One literal per maximal constant model-level expression that contains a
/// pure call, found from the continuous owners. Conditional branches are not
/// entered: only unconditionally evaluated expressions fold, so a failure is
/// a failure of the model rather than of a branch it may never take.
pub(super) fn constant_call_plan(view: dae::DaeView<'_>) -> Result<LiteralPlan, StructuralError> {
    let mut plan = vec![None; view.expression_count()];
    let mut evaluator = NumericEvaluator::new(view);
    let mut pending = continuous_roots(view);
    while let Some(expression) = pending.pop() {
        let node = view.expression(expression).expect("checked expression");
        if foldable_call_expression(view, node) {
            if let Some(value) = settle(view, &mut evaluator, expression)? {
                plan[expression.index() as usize] = Some(value);
                continue;
            }
        }
        push_unconditional_operands(node, &mut pending);
    }
    Ok(plan)
}

/// Every continuous residual root and structured family body.
fn continuous_roots<'dae>(view: dae::DaeView<'dae>) -> Vec<dae::ExprId<'dae>> {
    let mut roots = Vec::new();
    for owner in view.continuous_owners() {
        match owner {
            dae::ContinuousOwnerView::Residual { equation, .. } => {
                roots.push(equation.residual());
            }
            dae::ContinuousOwnerView::Structured { family, .. } => {
                roots.extend(family.bodies().iter());
            }
        }
    }
    roots
}

/// A model-level numeric expression of constant variability that reads no
/// binder and contains a call.
fn foldable_call_expression<'dae>(
    view: dae::DaeView<'dae>,
    node: dae::ExpressionView<'dae>,
) -> bool {
    node.variability() == dae::ExpressionVariability::Constant
        && node.function_scope().is_none()
        && node.binder_domain().is_none()
        && numeric_type(node.value_type())
        && first_call(view, node).is_some()
}

fn numeric_type(value_type: &dae::ValueType) -> bool {
    matches!(
        value_type.scalar_type(),
        dae::ScalarType::Real | dae::ScalarType::Integer | dae::ScalarType::Boolean
    ) && value_type.scalar_count().is_some_and(|count| count > 0)
}

/// The first call `node` evaluates unconditionally, depth first.
fn first_call<'dae>(
    view: dae::DaeView<'dae>,
    node: dae::ExpressionView<'dae>,
) -> Option<dae::FunctionId<'dae>> {
    let mut pending = vec![node];
    while let Some(node) = pending.pop() {
        if let dae::ExpressionOperation::Call { function, .. } = node.operation() {
            return Some(function);
        }
        let mut operands = Vec::new();
        push_unconditional_operands(node, &mut operands);
        pending.extend(
            operands
                .into_iter()
                .filter_map(|operand| view.expression(operand)),
        );
    }
    None
}

/// The operands of `node` that are evaluated whenever it is; conditional
/// expressions and every non-arithmetic operation are opaque.
fn push_unconditional_operands<'dae>(
    node: dae::ExpressionView<'dae>,
    pending: &mut Vec<dae::ExprId<'dae>>,
) {
    match node.operation() {
        dae::ExpressionOperation::Unary { operand, .. } => pending.push(operand),
        dae::ExpressionOperation::Binary { lhs, rhs, .. } => pending.extend([lhs, rhs]),
        dae::ExpressionOperation::Array(operands)
        | dae::ExpressionOperation::Record(operands)
        | dae::ExpressionOperation::Builtin {
            arguments: operands,
            ..
        }
        | dae::ExpressionOperation::Call {
            arguments: operands,
            ..
        } => pending.extend(operands.iter()),
        dae::ExpressionOperation::Field { base, .. }
        | dae::ExpressionOperation::Index { base, .. } => pending.push(base),
        _ => {}
    }
}

/// Evaluate one foldable expression: its literal value, `None` when it is not
/// a compile-time value, or the typed failure of a call that cannot succeed.
fn settle<'dae>(
    view: dae::DaeView<'dae>,
    evaluator: &mut NumericEvaluator<'dae>,
    expression: dae::ExprId<'dae>,
) -> Result<Option<Arc<FoldedValue>>, StructuralError> {
    let node = view.expression(expression).expect("checked expression");
    let values = match evaluator.expression(expression) {
        Ok(values) => values,
        Err(error) if failing_evaluation(error.kind()) => {
            return Err(StructuralError::ConstantCallEvaluation {
                call: first_call_name(view, node),
                reason: error.to_string(),
                span: node.provenance().span(),
            });
        }
        Err(_) => return Ok(None),
    };
    let value_type = node.value_type();
    if Some(values.len()) != value_type.scalar_count() {
        return Ok(None);
    }
    Ok(Some(Arc::new(FoldedValue {
        scalar: value_type.scalar_type(),
        dimensions: value_type.dimensions().into(),
        values: values.into(),
    })))
}

/// A failure the call itself produces, as opposed to a value the evaluator
/// cannot settle at construction.
fn failing_evaluation(kind: NumericEvaluationErrorKind) -> bool {
    matches!(
        kind,
        NumericEvaluationErrorKind::AssertionFailed
            | NumericEvaluationErrorKind::InvalidValue
            | NumericEvaluationErrorKind::OutOfBounds
            | NumericEvaluationErrorKind::Overflow
    )
}

fn first_call_name<'dae>(view: dae::DaeView<'dae>, node: dae::ExpressionView<'dae>) -> String {
    first_call(view, node)
        .and_then(|function| view.function(function))
        .map_or_else(String::new, |function| function.name().to_string())
}

/// One literal per model-level read of a declaration its own continuous
/// owner binds to a literal, except the binding's own read.
pub(super) fn literal_binding_plan(view: dae::DaeView<'_>) -> LiteralPlan {
    let bindings = rumoca_eval_dae::literal_bindings(view);
    let mut plan = vec![None; view.expression_count()];
    let mut values: Vec<Option<Arc<FoldedValue>>> = vec![None; view.variable_count()];
    let mut evaluator = NumericEvaluator::new(view);
    for index in 0..view.expression_count() {
        let Some(id) = view.expression_id(index) else {
            continue;
        };
        let Some(node) = view.expression(id) else {
            continue;
        };
        let dae::ExpressionOperation::Coordinate(dae::CoordinateView::Algebraic(variable)) =
            node.operation()
        else {
            continue;
        };
        let ordinal = dae::VariableId::from(variable).index() as usize;
        let Some(binding) = bindings.get(ordinal).copied().flatten() else {
            continue;
        };
        if binding.access == id || node.function_scope().is_some() {
            continue;
        }
        if values[ordinal].is_none() {
            values[ordinal] = binding_value(view, &mut evaluator, binding);
        }
        plan[index].clone_from(&values[ordinal]);
    }
    plan
}

fn binding_value<'dae>(
    view: dae::DaeView<'dae>,
    evaluator: &mut NumericEvaluator<'dae>,
    binding: rumoca_eval_dae::LiteralBinding<'dae>,
) -> Option<Arc<FoldedValue>> {
    let access = view.expression(binding.access)?;
    let value_type = access.value_type();
    let values = evaluator.expression(binding.value).ok()?;
    (Some(values.len()) == value_type.scalar_count()).then(|| {
        Arc::new(FoldedValue {
            scalar: value_type.scalar_type(),
            dimensions: value_type.dimensions().into(),
            values: values.into(),
        })
    })
}
