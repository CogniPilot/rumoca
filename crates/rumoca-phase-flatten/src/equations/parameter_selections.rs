//! Records of the if-equation branch selections flatten makes by evaluating
//! a parameter guard at translation (SPEC_0040 DAE-C22).
//!
//! Such a selection fixes each parameter its evaluated conditions read at its
//! translation-time value, so DAE construction marks those parameters
//! evaluable and warns at the if-equation. Flatten records the component
//! references the conditions read, with the flat names each can denote; DAE
//! analysis keeps the innermost one the model declares.

use rumoca_ir_ast as ast;
use rumoca_ir_flat as flat;

use super::build_qualified_name;

/// The record of a selection whose evaluated conditions are `conditions`.
pub(crate) fn parameter_branch_selection<'a>(
    conditions: impl IntoIterator<Item = &'a ast::Expression>,
    prefix: &ast::QualifiedName,
    span: rumoca_core::Span,
) -> flat::ParameterBranchSelection {
    let mut references = Vec::new();
    for condition in conditions {
        collect_references(condition, prefix, &mut references);
    }
    flat::ParameterBranchSelection { span, references }
}

fn collect_references(
    expression: &ast::Expression,
    prefix: &ast::QualifiedName,
    references: &mut Vec<Vec<String>>,
) {
    match expression {
        ast::Expression::ComponentReference(reference) => {
            references.push(scoped_candidates(prefix, reference));
        }
        ast::Expression::Binary { lhs, rhs, .. } => {
            collect_references(lhs, prefix, references);
            collect_references(rhs, prefix, references);
        }
        ast::Expression::Unary { rhs, .. } => collect_references(rhs, prefix, references),
        ast::Expression::Parenthesized { inner, .. } => {
            collect_references(inner, prefix, references);
        }
        ast::Expression::FunctionCall { args, .. } => {
            for argument in args {
                collect_references(argument, prefix, references);
            }
        }
        ast::Expression::If {
            branches,
            else_branch,
            ..
        } => {
            for (condition, value) in branches {
                collect_references(condition, prefix, references);
                collect_references(value, prefix, references);
            }
            collect_references(else_branch, prefix, references);
        }
        _ => {}
    }
}

/// The flat names `reference` can denote from `prefix`: qualified by the
/// whole prefix first, then by each enclosing scope.
fn scoped_candidates(
    prefix: &ast::QualifiedName,
    reference: &ast::ComponentReference,
) -> Vec<String> {
    (0..=prefix.parts.len())
        .rev()
        .map(|depth| {
            let scope = ast::QualifiedName {
                parts: prefix.parts[..depth].to_vec(),
            };
            build_qualified_name(&scope, reference)
        })
        .collect()
}
