//! Assertion-presence inspection over the existing checked function body.
use super::*;

pub(super) fn first_function_assertion(statements: dae::FunctionStatements<'_>) -> Option<Span> {
    for statement in statements {
        match statement {
            // A warning-level assertion has no influence on behavior and no
            // GALEC representation (MLS §8.3.7).
            dae::FunctionStatementView::Assertion {
                level: dae::AssertionLevel::Warning,
                ..
            } => {}
            dae::FunctionStatementView::Assertion { provenance, .. } => {
                return Some(provenance.span());
            }
            dae::FunctionStatementView::For { statements, .. } => {
                if let Some(span) = first_function_assertion(statements) {
                    return Some(span);
                }
            }
            dae::FunctionStatementView::Conditional { conditional } => {
                let found = conditional
                    .branches()
                    .chain(std::iter::once(conditional.fallback()))
                    .find_map(|branch| first_function_assertion(branch.statements()));
                if let Some(span) = found {
                    return Some(span);
                }
            }
            dae::FunctionStatementView::RequireAssigned { provenance, .. } => {
                return Some(provenance.span());
            }
            dae::FunctionStatementView::Assignment { .. }
            | dae::FunctionStatementView::AssignmentGroup { .. } => {}
        }
    }
    None
}
