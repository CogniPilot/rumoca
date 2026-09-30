//! Function-assertion discovery over checked DAE statement owners.

use std::collections::HashSet;

use rumoca_ir_dae as dae;
use rumoca_ir_solve as solve;

#[derive(Clone)]
pub(super) struct FunctionAssertion<'dae> {
    pub(super) condition: dae::ExprId<'dae>,
    pub(super) message: dae::ExprId<'dae>,
    pub(super) provenance: dae::DaeProvenance,
    /// Whether the assertion lies inside a `for` statement, where its
    /// message values differ per iteration.
    pub(super) in_loop: bool,
}

pub(super) fn assertion_conditions<'dae>(
    view: dae::DaeView<'dae>,
    function: dae::FunctionView<'dae>,
) -> Result<Vec<FunctionAssertion<'dae>>, solve::SolveProgramConstructionError> {
    let mut assertions = Vec::new();
    collect_assertion_conditions(view, function.statements(), false, &mut assertions)?;
    Ok(assertions)
}

fn collect_assertion_conditions<'dae>(
    view: dae::DaeView<'dae>,
    statements: dae::FunctionStatements<'dae>,
    in_loop: bool,
    assertions: &mut Vec<FunctionAssertion<'dae>>,
) -> Result<(), solve::SolveProgramConstructionError> {
    for statement in statements {
        match statement {
            dae::FunctionStatementView::Assignment { .. }
            | dae::FunctionStatementView::AssignmentGroup { .. } => {}
            dae::FunctionStatementView::Assertion {
                condition,
                message,
                provenance,
            } => assertions.push(FunctionAssertion {
                condition,
                message,
                provenance,
                in_loop,
            }),
            dae::FunctionStatementView::For {
                fold, statements, ..
            } => {
                view.function_fold(fold)
                    .ok_or(solve::SolveProgramConstructionError::WireMismatch)?;
                collect_assertion_conditions(view, statements, true, assertions)?;
            }
        }
    }
    Ok(())
}

/// Every value an assertion message converts to text that the declaring
/// function's frame evaluates as one owner output, in message order.
///
/// A loop assertion's values differ per iteration, and a value that calls a
/// function would invoke that callee only to render text; neither is an owner
/// output, so rendering refuses such a message at its own span.
pub(super) fn message_values<'dae>(
    view: dae::DaeView<'dae>,
    assertion: &FunctionAssertion<'dae>,
) -> Vec<dae::ExprId<'dae>> {
    let mut values = Vec::new();
    if !assertion.in_loop {
        collect_message_values(view, assertion.message, &mut values);
    }
    values
}

fn collect_message_values<'dae>(
    view: dae::DaeView<'dae>,
    message: dae::ExprId<'dae>,
    values: &mut Vec<dae::ExprId<'dae>>,
) {
    let Some(node) = view.expression(message) else {
        return;
    };
    match node.operation() {
        dae::ExpressionOperation::Binary {
            operator: dae::BinaryOperator::Add,
            lhs,
            rhs,
        } => {
            collect_message_values(view, lhs, values);
            collect_message_values(view, rhs, values);
        }
        dae::ExpressionOperation::StringConversion { value, format, .. } => {
            let options = match format {
                dae::StringConversionFormatView::Options {
                    minimum_length,
                    left_justified,
                    significant_digits,
                } => vec![minimum_length, left_justified, significant_digits],
                dae::StringConversionFormatView::Format { .. } => Vec::new(),
            };
            for value in std::iter::once(value).chain(options.into_iter().flatten()) {
                if frame_evaluable(view, value) {
                    values.push(value);
                }
            }
        }
        _ => {}
    }
}

/// A non-literal message value the declaring frame evaluates without a call.
fn frame_evaluable<'dae>(view: dae::DaeView<'dae>, value: dae::ExprId<'dae>) -> bool {
    let literal = view
        .expression(value)
        .is_some_and(|node| matches!(node.operation(), dae::ExpressionOperation::Literal(_)));
    let mut evaluable = !literal;
    dae::for_each_expression(view, value, |_, node| {
        if matches!(
            node.operation(),
            dae::ExpressionOperation::Call { .. }
                | dae::ExpressionOperation::FunctionFoldParameter { .. }
        ) {
            evaluable = false;
        }
    });
    evaluable
}

/// Every call owner the typed body lowering reads.
///
/// The body lowers each statement in order, so a definition no result or
/// assertion reads is still lowered; its calls need registered owners exactly
/// like the calls a result reaches.
pub(super) fn nested_calls<'dae>(
    view: dae::DaeView<'dae>,
    function: dae::FunctionView<'dae>,
    assertions: &[FunctionAssertion<'dae>],
) -> Vec<dae::ExprId<'dae>> {
    let mut roots = function.result_values().rhs_iter().collect::<Vec<_>>();
    roots.extend(assertions.iter().map(|assertion| assertion.condition));
    collect_statement_roots(function.statements(), &mut roots);
    let mut calls = Vec::new();
    let mut seen = HashSet::new();
    for root in roots {
        dae::for_each_expression(view, root, |_, node| {
            if let dae::ExpressionOperation::Call { owner, .. } = node.operation()
                && seen.insert(owner)
            {
                calls.push(owner);
            }
        });
    }
    calls
}

fn collect_statement_roots<'dae>(
    statements: dae::FunctionStatements<'dae>,
    roots: &mut Vec<dae::ExprId<'dae>>,
) {
    for statement in statements {
        match statement {
            dae::FunctionStatementView::Assignment { definition } => roots.push(definition.rhs()),
            dae::FunctionStatementView::AssignmentGroup {
                definitions,
                conditional,
            } => {
                roots.extend(definitions.rhs_iter());
                if let Some(conditional) = conditional {
                    collect_conditional_roots(conditional, roots);
                }
            }
            dae::FunctionStatementView::Assertion { condition, .. } => roots.push(condition),
            dae::FunctionStatementView::For { statements, .. } => {
                collect_statement_roots(statements, roots);
            }
        }
    }
}

pub(super) fn assertion_is_map_independent<'dae>(
    view: dae::DaeView<'dae>,
    condition: dae::ExprId<'dae>,
) -> bool {
    let mut independent = true;
    dae::for_each_expression(view, condition, |_, node| {
        if matches!(
            node.operation(),
            dae::ExpressionOperation::FunctionValue { .. }
                | dae::ExpressionOperation::FunctionFoldParameter { .. }
        ) {
            independent = false;
        }
    });
    independent
}

fn collect_conditional_roots<'dae>(
    conditional: dae::FunctionConditionalView<'dae>,
    roots: &mut Vec<dae::ExprId<'dae>>,
) {
    roots.extend(conditional.conditions());
    for ordinal in 0..conditional.branch_count() {
        roots.extend(conditional.branch(ordinal).into_iter().flatten());
    }
    roots.extend(conditional.fallback());
}
