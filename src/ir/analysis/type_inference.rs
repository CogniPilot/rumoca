//! Type inference for Modelica expressions.
//!
//! This module provides type inference capabilities for semantic analysis,
//! used by diagnostics to detect type mismatches in equations and expressions.

use std::collections::HashMap;

use crate::ir::ast::{Expression, OpBinary, TerminalType};

use super::symbols::DefinedSymbol;

/// Inferred type for an expression.
///
/// Used for semantic analysis and type checking of Modelica code.
#[derive(Clone, Debug, PartialEq)]
pub enum InferredType {
    Real,
    Integer,
    Boolean,
    String,
    /// Array type with element type and optional size
    Array(Box<InferredType>, Option<usize>),
    Unknown,
}

impl InferredType {
    /// Get the base (scalar) type, stripping array dimensions
    pub fn base_type(&self) -> &InferredType {
        match self {
            InferredType::Array(inner, _) => inner.base_type(),
            other => other,
        }
    }

    /// Check if this is a numeric type (Real or Integer)
    pub fn is_numeric(&self) -> bool {
        matches!(self.base_type(), InferredType::Real | InferredType::Integer)
    }

    /// Check if two types are compatible for assignment/equations
    pub fn is_compatible_with(&self, other: &InferredType) -> bool {
        match (self, other) {
            (InferredType::Unknown, _) | (_, InferredType::Unknown) => true,
            (InferredType::Real, InferredType::Real) => true,
            (InferredType::Integer, InferredType::Integer) => true,
            (InferredType::Boolean, InferredType::Boolean) => true,
            (InferredType::String, InferredType::String) => true,
            // Real and Integer are compatible (Integer can be promoted to Real)
            (InferredType::Real, InferredType::Integer)
            | (InferredType::Integer, InferredType::Real) => true,
            // Arrays are compatible if element types are compatible
            (InferredType::Array(t1, _), InferredType::Array(t2, _)) => t1.is_compatible_with(t2),
            // Scalar and array are not compatible
            _ => false,
        }
    }
}

impl std::fmt::Display for InferredType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            InferredType::Real => write!(f, "Real"),
            InferredType::Integer => write!(f, "Integer"),
            InferredType::Boolean => write!(f, "Boolean"),
            InferredType::String => write!(f, "String"),
            InferredType::Array(inner, size) => {
                if let Some(s) = size {
                    write!(f, "{}[{}]", inner, s)
                } else {
                    write!(f, "{}[:]", inner)
                }
            }
            InferredType::Unknown => write!(f, "Unknown"),
        }
    }
}

/// Convert a type name string to an InferredType
pub fn type_from_name(name: &str) -> InferredType {
    match name {
        "Real" => InferredType::Real,
        "Integer" => InferredType::Integer,
        "Boolean" => InferredType::Boolean,
        "String" => InferredType::String,
        _ => InferredType::Unknown, // User-defined types
    }
}

/// Infer the type of an expression given the defined symbols
pub fn infer_expression_type(
    expr: &Expression,
    defined: &HashMap<String, DefinedSymbol>,
) -> InferredType {
    match expr {
        Expression::Empty => InferredType::Unknown,
        Expression::ComponentReference(comp_ref) => {
            if let Some(first) = comp_ref.parts.first() {
                if let Some(sym) = defined.get(&first.ident.text) {
                    let base = type_from_name(&sym.type_name);
                    if sym.shape.is_empty() {
                        base
                    } else {
                        // Build array type from innermost to outermost
                        let mut result = base;
                        for &dim in sym.shape.iter().rev() {
                            result = InferredType::Array(Box::new(result), Some(dim));
                        }
                        // Account for subscripts - each index reduces one dimension
                        // e.g., q[3] where q is Real[4] becomes Real (scalar)
                        // e.g., R[1,2] where R is Real[3,3] becomes Real (scalar)
                        if let Some(subs) = &first.subs {
                            for _sub in subs {
                                // Each subscript strips one array dimension
                                if let InferredType::Array(inner, _) = result {
                                    result = *inner;
                                }
                            }
                        }
                        result
                    }
                } else {
                    // Check if it's 'time' (global Real)
                    if first.ident.text == "time" {
                        InferredType::Real
                    } else {
                        InferredType::Unknown
                    }
                }
            } else {
                InferredType::Unknown
            }
        }
        Expression::Terminal {
            terminal_type,
            token: _,
        } => match terminal_type {
            TerminalType::UnsignedInteger => InferredType::Integer,
            TerminalType::UnsignedReal => InferredType::Real,
            TerminalType::String => InferredType::String,
            TerminalType::Bool => InferredType::Boolean,
            _ => InferredType::Unknown,
        },
        Expression::FunctionCall { comp, args } => infer_function_call_type(comp, args, defined),
        Expression::Binary { lhs, op, rhs } => {
            let lhs_type = infer_expression_type(lhs, defined);
            let rhs_type = infer_expression_type(rhs, defined);
            infer_binary_op_type(op, &lhs_type, &rhs_type)
        }
        Expression::Unary { op: _, rhs } => infer_expression_type(rhs, defined),
        Expression::Array { elements } => {
            if let Some(first) = elements.first() {
                let elem_type = infer_expression_type(first, defined);
                InferredType::Array(Box::new(elem_type), Some(elements.len()))
            } else {
                InferredType::Unknown
            }
        }
        Expression::Tuple { elements: _ } => InferredType::Unknown,
        Expression::If {
            branches,
            else_branch,
        } => {
            // Type is the type of the branches (should all be the same)
            if let Some((_, then_expr)) = branches.first() {
                infer_expression_type(then_expr, defined)
            } else {
                infer_expression_type(else_branch, defined)
            }
        }
        Expression::Range { .. } => {
            // Range produces an array of integers or reals
            InferredType::Array(Box::new(InferredType::Integer), None)
        }
        Expression::Parenthesized { inner } => infer_expression_type(inner, defined),
        Expression::ArrayComprehension { expr, .. } => {
            // Array comprehension produces an array of the expression type
            let elem_type = infer_expression_type(expr, defined);
            InferredType::Array(Box::new(elem_type), None)
        }
    }
}

/// Infer the return type of a function call
fn infer_function_call_type(
    comp: &crate::ir::ast::ComponentReference,
    args: &[Expression],
    defined: &HashMap<String, DefinedSymbol>,
) -> InferredType {
    if let Some(first) = comp.parts.first() {
        match first.ident.text.as_str() {
            // Trigonometric and math functions return Real
            "sin" | "cos" | "tan" | "asin" | "acos" | "atan" | "atan2" | "sinh" | "cosh"
            | "tanh" | "exp" | "log" | "log10" | "sqrt" | "abs" | "sign" | "floor" | "ceil"
            | "mod" | "rem" | "max" | "min" | "sum" | "product" => InferredType::Real,

            // der returns the same type as its argument (preserves array dimensions)
            "der" | "pre" | "delay" => {
                if let Some(arg) = args.first() {
                    infer_expression_type(arg, defined)
                } else {
                    InferredType::Real
                }
            }

            // cross(a, b) returns a 3-vector
            "cross" => InferredType::Array(Box::new(InferredType::Real), Some(3)),

            // transpose, symmetric, skew return matrices (preserve first arg type)
            "transpose" | "symmetric" | "skew" => {
                if let Some(arg) = args.first() {
                    infer_expression_type(arg, defined)
                } else {
                    InferredType::Unknown
                }
            }

            // identity, zeros, ones, fill, diagonal return arrays
            "identity" | "diagonal" => {
                // identity(n) returns Real[n,n], diagonal(v) returns Real[n,n]
                InferredType::Array(
                    Box::new(InferredType::Array(Box::new(InferredType::Real), None)),
                    None,
                )
            }
            "zeros" | "ones" | "fill" => {
                // These return arrays, but we don't know the dimensions statically
                InferredType::Array(Box::new(InferredType::Real), None)
            }

            // Boolean functions
            "initial" | "terminal" | "edge" | "change" | "sample" => InferredType::Boolean,

            // Size returns Integer
            "size" | "ndims" => InferredType::Integer,

            // User-defined functions - look up in defined symbols
            name => {
                if let Some(sym) = defined.get(name) {
                    if let Some((ret_type, ret_shape)) = &sym.function_return {
                        let base = type_from_name(ret_type);
                        if ret_shape.is_empty() {
                            base
                        } else {
                            let mut result = base;
                            for &dim in ret_shape.iter().rev() {
                                result = InferredType::Array(Box::new(result), Some(dim));
                            }
                            result
                        }
                    } else {
                        InferredType::Unknown
                    }
                } else {
                    InferredType::Unknown
                }
            }
        }
    } else {
        InferredType::Unknown
    }
}

/// Infer the result type of a binary operation
fn infer_binary_op_type(
    op: &OpBinary,
    lhs_type: &InferredType,
    rhs_type: &InferredType,
) -> InferredType {
    match op {
        // Comparison operators return Boolean
        OpBinary::Lt(_)
        | OpBinary::Le(_)
        | OpBinary::Gt(_)
        | OpBinary::Ge(_)
        | OpBinary::Eq(_)
        | OpBinary::Neq(_) => InferredType::Boolean,

        // Logical operators return Boolean
        OpBinary::And(_) | OpBinary::Or(_) => InferredType::Boolean,

        // Arithmetic operators
        OpBinary::Add(_) | OpBinary::Sub(_) => infer_arithmetic_result(lhs_type, rhs_type, false),
        OpBinary::Mul(_) => infer_multiplication_result(lhs_type, rhs_type),
        OpBinary::Div(_) => infer_division_result(lhs_type, rhs_type),
        OpBinary::Exp(_) => infer_exponentiation_result(lhs_type, rhs_type),
        _ => InferredType::Unknown,
    }
}

/// Infer result type for addition/subtraction
fn infer_arithmetic_result(
    lhs_type: &InferredType,
    rhs_type: &InferredType,
    _is_mul: bool,
) -> InferredType {
    match (lhs_type, rhs_type) {
        (InferredType::Array(_, _), _) => lhs_type.clone(),
        (_, InferredType::Array(_, _)) => rhs_type.clone(),
        _ => {
            if matches!(lhs_type.base_type(), InferredType::Real)
                || matches!(rhs_type.base_type(), InferredType::Real)
            {
                InferredType::Real
            } else if matches!(lhs_type.base_type(), InferredType::Integer)
                && matches!(rhs_type.base_type(), InferredType::Integer)
            {
                InferredType::Integer
            } else {
                InferredType::Unknown
            }
        }
    }
}

/// Infer result type for multiplication
fn infer_multiplication_result(lhs_type: &InferredType, rhs_type: &InferredType) -> InferredType {
    match (lhs_type, rhs_type) {
        // Scalar * Array -> Array
        (InferredType::Real | InferredType::Integer, InferredType::Array(_, _)) => rhs_type.clone(),
        // Array * Scalar -> Array
        (InferredType::Array(_, _), InferredType::Real | InferredType::Integer) => lhs_type.clone(),
        // Matrix[m,n] * Vector[n] -> Vector[m]
        (InferredType::Array(inner_lhs, Some(m)), InferredType::Array(inner_rhs, _)) => {
            if let InferredType::Array(_, _) = inner_lhs.as_ref() {
                // Matrix * Vector -> Vector
                InferredType::Array(Box::new(inner_rhs.base_type().clone()), Some(*m))
            } else {
                InferredType::Unknown
            }
        }
        // Both scalars
        _ => {
            if matches!(lhs_type.base_type(), InferredType::Real)
                || matches!(rhs_type.base_type(), InferredType::Real)
            {
                InferredType::Real
            } else if matches!(lhs_type.base_type(), InferredType::Integer)
                && matches!(rhs_type.base_type(), InferredType::Integer)
            {
                InferredType::Integer
            } else {
                InferredType::Unknown
            }
        }
    }
}

/// Infer result type for division
fn infer_division_result(lhs_type: &InferredType, rhs_type: &InferredType) -> InferredType {
    match (lhs_type, rhs_type) {
        // Array / Scalar -> Array
        (InferredType::Array(_, _), InferredType::Real | InferredType::Integer) => lhs_type.clone(),
        // Scalar / Array -> Array (element-wise)
        (InferredType::Real | InferredType::Integer, InferredType::Array(_, _)) => rhs_type.clone(),
        // Both scalars
        _ => {
            if matches!(lhs_type.base_type(), InferredType::Real)
                || matches!(rhs_type.base_type(), InferredType::Real)
            {
                InferredType::Real
            } else {
                InferredType::Unknown
            }
        }
    }
}

/// Infer result type for exponentiation
fn infer_exponentiation_result(lhs_type: &InferredType, rhs_type: &InferredType) -> InferredType {
    match (lhs_type, rhs_type) {
        (InferredType::Array(_, _), InferredType::Real | InferredType::Integer) => lhs_type.clone(),
        _ => {
            if matches!(lhs_type.base_type(), InferredType::Real)
                || matches!(rhs_type.base_type(), InferredType::Real)
            {
                InferredType::Real
            } else if matches!(lhs_type.base_type(), InferredType::Integer)
                && matches!(rhs_type.base_type(), InferredType::Integer)
            {
                InferredType::Integer
            } else {
                InferredType::Unknown
            }
        }
    }
}
