//! The executed-path state of a returned record's field (MLS 3.7 §12.4.4,
//! read at the use) is not lowered to Solve IR yet.
//!
//! The DAE carries the state of such a field to its read: a function body
//! holds `CarryAssigned`, a callee declares the parameters that may arrive
//! absent and reads them through `AssignedParameter`. Solve IR has no call
//! interface for an assigned state, so a model that needs one is refused
//! here, once and by name, before any row is lowered. Nothing below this gate
//! meets these forms; lowering one unchecked would read a value that may not
//! exist.

use rumoca_ir_dae as dae;

use crate::LowerError;

const NOT_LOWERED: &str = "a record result field the executed path may leave unwritten is checked \
     where it is read (MLS 3.7 §12.4.4), and Solve IR does not carry that state across a call yet";

pub(super) fn refuse_carried_field_states(view: dae::DaeView<'_>) -> Result<(), LowerError> {
    let functions = (0..view.function_count())
        .filter_map(|index| view.function_id(index).and_then(|id| view.function(id)));
    for function in functions {
        if !function.absent_parameters().is_empty() {
            return Err(LowerError::unsupported(
                NOT_LOWERED,
                function.declaration().span(),
            ));
        }
        if let Some(span) = carried_statement(function.statements()) {
            return Err(LowerError::unsupported(NOT_LOWERED, span));
        }
    }
    Ok(())
}

fn carried_statement(statements: dae::FunctionStatements<'_>) -> Option<rumoca_core::Span> {
    statements
        .into_iter()
        .find_map(|statement| match statement {
            dae::FunctionStatementView::CarryAssigned { provenance, .. } => Some(provenance.span()),
            dae::FunctionStatementView::Conditional { conditional } => conditional
                .branches()
                .chain(std::iter::once(conditional.fallback()))
                .find_map(|branch| carried_statement(branch.statements())),
            dae::FunctionStatementView::For { statements, .. } => carried_statement(statements),
            dae::FunctionStatementView::RequireAssigned { .. }
            | dae::FunctionStatementView::Assignment { .. }
            | dae::FunctionStatementView::AssignmentGroup { .. }
            | dae::FunctionStatementView::Assertion { .. } => None,
        })
}
