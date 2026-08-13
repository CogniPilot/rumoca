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

/// Collect one entry per assertion *statement*.
///
/// SPEC_0032: a statement inside a loop stays one entry however large the
/// domain is - the loop contributes extents to the compact predicate, never
/// entries to this inventory.
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

pub(super) fn nested_calls<'dae>(
    view: dae::DaeView<'dae>,
    function: dae::FunctionView<'dae>,
    assertions: &[FunctionAssertion<'dae>],
) -> Vec<dae::ExprId<'dae>> {
    let mut calls = Vec::new();
    let mut seen = HashSet::new();
    for root in function
        .result_values()
        .rhs_iter()
        .chain(assertions.iter().map(|assertion| assertion.condition))
    {
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

pub(super) fn assertion_admitted_for_compact_reduction<'dae>(
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
