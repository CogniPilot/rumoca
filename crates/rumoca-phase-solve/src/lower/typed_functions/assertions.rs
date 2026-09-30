//! Function-assertion discovery over checked DAE statement owners.

use std::collections::HashSet;

use rumoca_ir_dae as dae;
use rumoca_ir_solve as solve;

#[derive(Clone)]
pub(super) struct FunctionAssertion<'dae> {
    pub(super) condition: dae::ExprId<'dae>,
    pub(super) message: dae::ExprId<'dae>,
    pub(super) provenance: dae::DaeProvenance,
}

pub(super) fn assertion_conditions<'dae>(
    view: dae::DaeView<'dae>,
    function: dae::FunctionView<'dae>,
) -> Result<Vec<FunctionAssertion<'dae>>, solve::SolveProgramConstructionError> {
    let mut assertions = Vec::new();
    collect_assertion_conditions(view, function.statements(), &mut assertions)?;
    Ok(assertions)
}

fn collect_assertion_conditions<'dae>(
    view: dae::DaeView<'dae>,
    statements: dae::FunctionStatements<'dae>,
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
            }),
            dae::FunctionStatementView::For {
                fold, statements, ..
            } => {
                view.function_fold(fold)
                    .ok_or(solve::SolveProgramConstructionError::WireMismatch)?;
                collect_assertion_conditions(view, statements, assertions)?;
            }
        }
    }
    Ok(())
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
