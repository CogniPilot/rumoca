//! Utility helper functions for flatten operations.
//!
//! This module provides small utility functions used during the flattening process
//! for checking expression types, evaluating modifications, and creating equations.

use crate::ir;
use crate::ir::ast::{Equation, Expression, TerminalType};
use indexmap::IndexMap;

use super::connections::make_comp_ref;

// =============================================================================
// Expression Helpers
// =============================================================================

/// Checks if an expression is a simple literal that can be used as a start value
pub(super) fn is_simple_literal(expr: &Expression) -> bool {
    match expr {
        Expression::Empty => true,
        Expression::Terminal { terminal_type, .. } => {
            matches!(
                terminal_type,
                TerminalType::UnsignedInteger
                    | TerminalType::UnsignedReal
                    | TerminalType::Bool
                    | TerminalType::String
            )
        }
        Expression::Unary { op, rhs } => {
            // Allow negated literals like -1.0 or -10
            matches!(op, ir::ast::OpUnary::Minus(_) | ir::ast::OpUnary::Plus(_))
                && is_simple_literal(rhs)
        }
        _ => false,
    }
}

/// Try to evaluate a modification expression using known component values.
/// Returns Some(literal_expr) if the expression can be evaluated to a simple value.
pub(super) fn try_evaluate_modification(
    expr: &Expression,
    components: &IndexMap<String, ir::ast::Component>,
) -> Option<Expression> {
    match expr {
        // Simple literals are already evaluated
        Expression::Terminal { .. } => Some(expr.clone()),

        // Unary operations on literals
        Expression::Unary { op, rhs } => {
            if matches!(op, ir::ast::OpUnary::Minus(_) | ir::ast::OpUnary::Plus(_))
                && let Some(evaluated) = try_evaluate_modification(rhs, components)
            {
                return Some(Expression::Unary {
                    op: op.clone(),
                    rhs: Box::new(evaluated),
                });
            }
            None
        }

        // ComponentReference - look up the value from components
        Expression::ComponentReference(comp_ref) => {
            // Only handle simple references without subscripts
            if comp_ref.parts.iter().all(|p| p.subs.is_none()) {
                let name = comp_ref
                    .parts
                    .iter()
                    .map(|p| p.ident.text.as_str())
                    .collect::<Vec<_>>()
                    .join(".");

                if let Some(comp) = components.get(&name) {
                    // Only use the value if it's a simple literal
                    if is_simple_literal(&comp.start) {
                        return Some(comp.start.clone());
                    }
                }
            }
            None
        }

        _ => None,
    }
}

// =============================================================================
// Equation Helpers
// =============================================================================

/// Creates a binding equation: lhs = expr
pub(super) fn make_binding_eq(lhs: &str, rhs: Expression) -> Equation {
    Equation::Simple {
        lhs: Expression::ComponentReference(make_comp_ref(lhs)),
        rhs,
    }
}

// =============================================================================
// Type Helpers
// =============================================================================

/// Check if a class is an operator record type (like Complex).
///
/// Operator records need special handling for array subscripts during flattening.
/// For example, `u[1].re` for a Complex array should become `u.re[1]` because
/// after flattening, `u.re` is an array.
///
/// A type is considered an operator record if:
/// 1. The type name is "Complex", or
/// 2. The class has exactly two Real components named "re" and "im"
pub(super) fn is_operator_record_type(class: &ir::ast::ClassDefinition, type_name: &str) -> bool {
    // Check for built-in Complex type
    if type_name == "Complex" {
        return true;
    }

    // Check if it has the pattern of a Complex-like type (re and im components)
    if class.components.len() == 2
        && class.components.contains_key("re")
        && class.components.contains_key("im")
    {
        // Verify both are Real types
        if let (Some(re), Some(im)) = (class.components.get("re"), class.components.get("im")) {
            let re_type = re.type_name.to_string();
            let im_type = im.type_name.to_string();
            if re_type == "Real" && im_type == "Real" {
                return true;
            }
        }
    }

    false
}
