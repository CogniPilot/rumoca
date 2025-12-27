//! Expression type validation for Modelica.
//!
//! This module validates that expressions use operators with compatible types:
//! - Arithmetic operators (+, -, *, /, ^) require numeric operands
//! - Logical operators (and, or, not) require Boolean operands
//! - Relational operators (<, >, <=, >=) cannot be used on Boolean
//! - Equality operators (==, <>) require compatible types

use std::collections::HashMap;

use crate::ir::ast::{Expression, OpBinary, OpUnary};

use crate::ir::analysis::symbols::DefinedSymbol;
use crate::ir::analysis::type_inference::{SymbolType, infer_expression_type};

use super::{TypeCheckResult, TypeError, TypeErrorSeverity};

/// Validate an expression recursively, checking all binary and unary operations.
///
/// Returns any type errors found in the expression tree.
pub fn validate_expression(
    expr: &Expression,
    defined: &HashMap<String, DefinedSymbol>,
) -> TypeCheckResult {
    let mut result = TypeCheckResult::new();
    validate_expression_impl(expr, defined, &mut result);
    result
}

/// Internal implementation for expression validation
fn validate_expression_impl(
    expr: &Expression,
    defined: &HashMap<String, DefinedSymbol>,
    result: &mut TypeCheckResult,
) {
    match expr {
        Expression::Binary { lhs, op, rhs } => {
            // First validate the operands recursively
            validate_expression_impl(lhs, defined, result);
            validate_expression_impl(rhs, defined, result);

            // Then validate this binary operation
            let lhs_type = infer_expression_type(lhs, defined);
            let rhs_type = infer_expression_type(rhs, defined);

            if let Some(error) = validate_binary_op(op, &lhs_type, &rhs_type, expr) {
                result.add_error(error);
            }
        }
        Expression::Unary { op, rhs } => {
            // Validate the operand recursively
            validate_expression_impl(rhs, defined, result);

            // Validate the unary operation
            let rhs_type = infer_expression_type(rhs, defined);

            if let Some(error) = validate_unary_op(op, &rhs_type, expr) {
                result.add_error(error);
            }
        }
        Expression::If {
            branches,
            else_branch,
        } => {
            // Check each branch condition and value
            let mut branch_types: Vec<SymbolType> = Vec::new();

            for (cond, value) in branches {
                // Validate condition recursively
                validate_expression_impl(cond, defined, result);

                // Check condition is Boolean
                let cond_type = infer_expression_type(cond, defined);
                if !is_boolean_compatible(&cond_type)
                    && let Some(loc) = cond.get_location()
                {
                    result.add_error(TypeError::new(
                        loc.clone(),
                        SymbolType::Boolean,
                        cond_type,
                        "If-expression condition must be Boolean".to_string(),
                        TypeErrorSeverity::Error,
                    ));
                }

                // Validate value recursively
                validate_expression_impl(value, defined, result);

                // Collect branch type
                branch_types.push(infer_expression_type(value, defined));
            }

            // Validate else branch
            validate_expression_impl(else_branch, defined, result);
            let else_type = infer_expression_type(else_branch, defined);
            branch_types.push(else_type.clone());

            // Check all branches have compatible types
            if let Some(first_type) = branch_types.first() {
                for (i, branch_type) in branch_types.iter().enumerate().skip(1) {
                    if !are_if_branch_types_compatible(first_type, branch_type) {
                        // Get location from the appropriate branch
                        let loc = if i < branches.len() {
                            branches[i].1.get_location()
                        } else {
                            else_branch.get_location()
                        };
                        if let Some(loc) = loc {
                            result.add_error(TypeError::new(
                                loc.clone(),
                                first_type.clone(),
                                branch_type.clone(),
                                format!(
                                    "If-expression branches have incompatible types: {} vs {}",
                                    first_type, branch_type
                                ),
                                TypeErrorSeverity::Error,
                            ));
                        }
                    }
                }
            }
        }
        Expression::Array { elements, .. } => {
            for elem in elements {
                validate_expression_impl(elem, defined, result);
            }
        }
        Expression::FunctionCall { args, .. } => {
            for arg in args {
                validate_expression_impl(arg, defined, result);
            }
        }
        Expression::Parenthesized { inner } => {
            validate_expression_impl(inner, defined, result);
        }
        Expression::Range { start, step, end } => {
            validate_expression_impl(start, defined, result);
            if let Some(s) = step {
                validate_expression_impl(s, defined, result);
            }
            validate_expression_impl(end, defined, result);
        }
        Expression::ArrayComprehension { expr, indices, .. } => {
            // Add loop indices to defined symbols for this scope
            let mut local_defined = defined.clone();
            for index in indices {
                local_defined.insert(
                    index.ident.text.clone(),
                    DefinedSymbol::loop_index(
                        &index.ident.text,
                        index.ident.location.start_line,
                        index.ident.location.start_column,
                    ),
                );
            }
            validate_expression_impl(expr, &local_defined, result);
        }
        Expression::Tuple { elements } => {
            for elem in elements {
                validate_expression_impl(elem, defined, result);
            }
        }
        // Terminal expressions and component references don't need validation
        Expression::Empty | Expression::Terminal { .. } | Expression::ComponentReference(_) => {}
    }
}

/// Validate a binary operation
fn validate_binary_op(
    op: &OpBinary,
    lhs_type: &SymbolType,
    rhs_type: &SymbolType,
    expr: &Expression,
) -> Option<TypeError> {
    // Skip validation if either type is Unknown or Class (we can't validate without full type resolution)
    if matches!(lhs_type, SymbolType::Unknown | SymbolType::Class(_))
        || matches!(rhs_type, SymbolType::Unknown | SymbolType::Class(_))
    {
        return None;
    }

    let location = expr.get_location()?.clone();

    match op {
        // Arithmetic operators: require numeric types (Real or Integer)
        OpBinary::Add(_) | OpBinary::Sub(_) => {
            if !is_numeric_or_string_for_add(lhs_type, op)
                || !is_numeric_or_string_for_add(rhs_type, op)
            {
                // String + String is allowed (concatenation)
                if matches!(op, OpBinary::Add(_))
                    && matches!(lhs_type.base_type(), SymbolType::String)
                    && matches!(rhs_type.base_type(), SymbolType::String)
                {
                    return None;
                }
                return Some(TypeError::new(
                    location,
                    SymbolType::Real,
                    if !is_numeric(lhs_type) {
                        lhs_type.clone()
                    } else {
                        rhs_type.clone()
                    },
                    format!(
                        "Arithmetic operator '{}' requires numeric operands, got {} and {}",
                        op_symbol(op),
                        lhs_type,
                        rhs_type
                    ),
                    TypeErrorSeverity::Error,
                ));
            }
        }
        OpBinary::Mul(_) | OpBinary::Div(_) | OpBinary::Exp(_) => {
            if !is_numeric(lhs_type) || !is_numeric(rhs_type) {
                return Some(TypeError::new(
                    location,
                    SymbolType::Real,
                    if !is_numeric(lhs_type) {
                        lhs_type.clone()
                    } else {
                        rhs_type.clone()
                    },
                    format!(
                        "Arithmetic operator '{}' requires numeric operands, got {} and {}",
                        op_symbol(op),
                        lhs_type,
                        rhs_type
                    ),
                    TypeErrorSeverity::Error,
                ));
            }
        }

        // Logical operators: require Boolean types
        OpBinary::And(_) | OpBinary::Or(_) => {
            if !is_boolean(lhs_type) || !is_boolean(rhs_type) {
                return Some(TypeError::new(
                    location,
                    SymbolType::Boolean,
                    if !is_boolean(lhs_type) {
                        lhs_type.clone()
                    } else {
                        rhs_type.clone()
                    },
                    format!(
                        "Logical operator '{}' requires Boolean operands, got {} and {}",
                        op_symbol(op),
                        lhs_type,
                        rhs_type
                    ),
                    TypeErrorSeverity::Error,
                ));
            }
        }

        // Ordering operators: cannot be used on Boolean
        OpBinary::Lt(_) | OpBinary::Le(_) | OpBinary::Gt(_) | OpBinary::Ge(_) => {
            if is_boolean(lhs_type) || is_boolean(rhs_type) {
                return Some(TypeError::new(
                    location,
                    SymbolType::Real,
                    if is_boolean(lhs_type) {
                        lhs_type.clone()
                    } else {
                        rhs_type.clone()
                    },
                    format!(
                        "Ordering operator '{}' cannot be used on Boolean values",
                        op_symbol(op)
                    ),
                    TypeErrorSeverity::Error,
                ));
            }
            // Also check types are comparable (both numeric or both String)
            if !are_types_comparable(lhs_type, rhs_type) {
                return Some(TypeError::new(
                    location,
                    lhs_type.clone(),
                    rhs_type.clone(),
                    format!(
                        "Cannot compare {} with {} using '{}'",
                        lhs_type,
                        rhs_type,
                        op_symbol(op)
                    ),
                    TypeErrorSeverity::Error,
                ));
            }
        }

        // Equality operators: require compatible types
        OpBinary::Eq(_) | OpBinary::Neq(_) => {
            if !are_types_equality_comparable(lhs_type, rhs_type) {
                return Some(TypeError::new(
                    location,
                    lhs_type.clone(),
                    rhs_type.clone(),
                    format!("Cannot compare {} with {} for equality", lhs_type, rhs_type),
                    TypeErrorSeverity::Error,
                ));
            }
        }

        // Element-wise operators - same rules as non-element-wise versions
        OpBinary::AddElem(_) | OpBinary::SubElem(_) => {
            if !is_numeric(lhs_type) || !is_numeric(rhs_type) {
                return Some(TypeError::new(
                    location,
                    SymbolType::Real,
                    if !is_numeric(lhs_type) {
                        lhs_type.clone()
                    } else {
                        rhs_type.clone()
                    },
                    format!(
                        "Element-wise arithmetic operator '{}' requires numeric operands",
                        op_symbol(op)
                    ),
                    TypeErrorSeverity::Error,
                ));
            }
        }
        OpBinary::MulElem(_) | OpBinary::DivElem(_) => {
            if !is_numeric(lhs_type) || !is_numeric(rhs_type) {
                return Some(TypeError::new(
                    location,
                    SymbolType::Real,
                    if !is_numeric(lhs_type) {
                        lhs_type.clone()
                    } else {
                        rhs_type.clone()
                    },
                    format!(
                        "Element-wise arithmetic operator '{}' requires numeric operands",
                        op_symbol(op)
                    ),
                    TypeErrorSeverity::Error,
                ));
            }
        }

        // Assignment operator - not validated here (used in modifications)
        OpBinary::Assign(_) | OpBinary::Empty => {}
    }

    None
}

/// Validate a unary operation
fn validate_unary_op(op: &OpUnary, rhs_type: &SymbolType, expr: &Expression) -> Option<TypeError> {
    // Skip validation if type is Unknown or Class
    if matches!(rhs_type, SymbolType::Unknown | SymbolType::Class(_)) {
        return None;
    }

    let location = expr.get_location()?.clone();

    match op {
        // Unary minus requires numeric type
        OpUnary::Minus(_) | OpUnary::DotMinus(_) => {
            if !is_numeric(rhs_type) {
                return Some(TypeError::new(
                    location,
                    SymbolType::Real,
                    rhs_type.clone(),
                    format!("Unary minus requires numeric operand, got {}", rhs_type),
                    TypeErrorSeverity::Error,
                ));
            }
        }
        // Logical not requires Boolean type
        OpUnary::Not(_) => {
            if !is_boolean(rhs_type) {
                return Some(TypeError::new(
                    location,
                    SymbolType::Boolean,
                    rhs_type.clone(),
                    format!("Logical 'not' requires Boolean operand, got {}", rhs_type),
                    TypeErrorSeverity::Error,
                ));
            }
        }
        // Unary plus requires numeric type
        OpUnary::Plus(_) | OpUnary::DotPlus(_) => {
            if !is_numeric(rhs_type) {
                return Some(TypeError::new(
                    location,
                    SymbolType::Real,
                    rhs_type.clone(),
                    format!("Unary plus requires numeric operand, got {}", rhs_type),
                    TypeErrorSeverity::Error,
                ));
            }
        }
        // Empty - no validation
        OpUnary::Empty => {}
    }

    None
}

/// Check if type is numeric (Real or Integer)
fn is_numeric(t: &SymbolType) -> bool {
    matches!(t.base_type(), SymbolType::Real | SymbolType::Integer)
}

/// Check if type is numeric or String (for + operator which allows string concatenation)
fn is_numeric_or_string_for_add(t: &SymbolType, op: &OpBinary) -> bool {
    if is_numeric(t) {
        return true;
    }
    // String concatenation is only valid for +
    if matches!(op, OpBinary::Add(_)) && matches!(t.base_type(), SymbolType::String) {
        return true;
    }
    false
}

/// Check if type is Boolean
fn is_boolean(t: &SymbolType) -> bool {
    matches!(t.base_type(), SymbolType::Boolean)
}

/// Check if type is compatible with Boolean (including Unknown and Class)
fn is_boolean_compatible(t: &SymbolType) -> bool {
    matches!(
        t.base_type(),
        SymbolType::Boolean | SymbolType::Unknown | SymbolType::Class(_)
    )
}

/// Check if two types can be compared with ordering operators (<, >, <=, >=)
fn are_types_comparable(lhs: &SymbolType, rhs: &SymbolType) -> bool {
    let lhs_base = lhs.base_type();
    let rhs_base = rhs.base_type();

    // Both numeric types are comparable
    if is_numeric(lhs) && is_numeric(rhs) {
        return true;
    }

    // Both String types are comparable
    if matches!(lhs_base, SymbolType::String) && matches!(rhs_base, SymbolType::String) {
        return true;
    }

    // Same enumeration types are comparable
    if let (SymbolType::Enumeration(n1), SymbolType::Enumeration(n2)) = (lhs_base, rhs_base) {
        return n1 == n2;
    }

    false
}

/// Check if two types can be compared for equality (==, <>)
fn are_types_equality_comparable(lhs: &SymbolType, rhs: &SymbolType) -> bool {
    let lhs_base = lhs.base_type();
    let rhs_base = rhs.base_type();

    // Same primitive types
    match (lhs_base, rhs_base) {
        (SymbolType::Real, SymbolType::Real) => true,
        (SymbolType::Integer, SymbolType::Integer) => true,
        (SymbolType::Boolean, SymbolType::Boolean) => true,
        (SymbolType::String, SymbolType::String) => true,
        // Real and Integer can be compared (Integer promoted to Real)
        (SymbolType::Real, SymbolType::Integer) | (SymbolType::Integer, SymbolType::Real) => true,
        // Same enumeration types
        (SymbolType::Enumeration(n1), SymbolType::Enumeration(n2)) => n1 == n2,
        // Otherwise not comparable
        _ => false,
    }
}

/// Check if two types are compatible for if-expression branches
/// MLS is strict: branches must have the same type (no implicit promotion)
fn are_if_branch_types_compatible(t1: &SymbolType, t2: &SymbolType) -> bool {
    // Unknown types are compatible with anything
    if matches!(t1, SymbolType::Unknown) || matches!(t2, SymbolType::Unknown) {
        return true;
    }

    // Class types are compatible (we can't verify without full resolution)
    if matches!(t1, SymbolType::Class(_)) || matches!(t2, SymbolType::Class(_)) {
        return true;
    }

    match (t1, t2) {
        // Same scalar types
        (SymbolType::Real, SymbolType::Real) => true,
        (SymbolType::Integer, SymbolType::Integer) => true,
        (SymbolType::Boolean, SymbolType::Boolean) => true,
        (SymbolType::String, SymbolType::String) => true,
        // Real and Integer are compatible (Integer can be promoted to Real per MLS §6)
        (SymbolType::Real, SymbolType::Integer) | (SymbolType::Integer, SymbolType::Real) => true,
        // Arrays must have compatible element types AND same dimensions
        (SymbolType::Array(e1, dims1), SymbolType::Array(e2, dims2)) => {
            // Check dimensions match
            if dims1 != dims2 {
                return false;
            }
            // Check element types are compatible
            are_if_branch_types_compatible(e1, e2)
        }
        // Same enumeration types
        (SymbolType::Enumeration(n1), SymbolType::Enumeration(n2)) => n1 == n2,
        _ => false,
    }
}

/// Get the symbol for a binary operator (for error messages)
fn op_symbol(op: &OpBinary) -> &'static str {
    match op {
        OpBinary::Add(_) | OpBinary::AddElem(_) => "+",
        OpBinary::Sub(_) | OpBinary::SubElem(_) => "-",
        OpBinary::Mul(_) | OpBinary::MulElem(_) => "*",
        OpBinary::Div(_) | OpBinary::DivElem(_) => "/",
        OpBinary::Exp(_) => "^",
        OpBinary::And(_) => "and",
        OpBinary::Or(_) => "or",
        OpBinary::Lt(_) => "<",
        OpBinary::Le(_) => "<=",
        OpBinary::Gt(_) => ">",
        OpBinary::Ge(_) => ">=",
        OpBinary::Eq(_) => "==",
        OpBinary::Neq(_) => "<>",
        OpBinary::Assign(_) => "=",
        OpBinary::Empty => "",
    }
}
