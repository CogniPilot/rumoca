//! Type inference for diagnostics.
//!
//! Provides type inference for expressions to detect type mismatches.

use std::collections::HashMap;

use crate::ir::ast::{Expression, OpBinary, TerminalType};

/// Information about a defined symbol
#[derive(Clone)]
pub struct DefinedSymbol {
    pub line: u32,
    pub col: u32,
    pub is_parameter: bool,
    pub is_class: bool,
    pub has_default: bool,
    /// The base type (Real, Integer, Boolean, String)
    pub type_name: String,
    /// Array dimensions (empty for scalars)
    pub shape: Vec<usize>,
}

/// Inferred type for an expression
#[derive(Clone, Debug, PartialEq)]
pub enum InferredType {
    Real,
    Integer,
    Boolean,
    String,
    Array(Box<InferredType>, Option<usize>), // element type, optional size
    Unknown,
}

impl InferredType {
    pub fn base_type(&self) -> &InferredType {
        match self {
            InferredType::Array(inner, _) => inner.base_type(),
            other => other,
        }
    }

    pub fn is_numeric(&self) -> bool {
        matches!(self.base_type(), InferredType::Real | InferredType::Integer)
    }

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

pub fn type_from_name(name: &str) -> InferredType {
    match name {
        "Real" => InferredType::Real,
        "Integer" => InferredType::Integer,
        "Boolean" => InferredType::Boolean,
        "String" => InferredType::String,
        _ => InferredType::Unknown, // User-defined types
    }
}

/// Check if a type name represents a class instance (not a primitive type)
pub fn is_class_instance_type(type_name: &str) -> bool {
    !matches!(
        type_name,
        "Real" | "Integer" | "Boolean" | "String" | "StateSelect" | "ExternalObject"
    )
}

/// Infer the type of an expression
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
        Expression::FunctionCall { comp, args } => {
            // Infer return type based on function name
            if let Some(first) = comp.parts.first() {
                match first.ident.text.as_str() {
                    // Trigonometric and math functions return Real
                    "sin" | "cos" | "tan" | "asin" | "acos" | "atan" | "atan2" | "sinh"
                    | "cosh" | "tanh" | "exp" | "log" | "log10" | "sqrt" | "abs" | "sign"
                    | "floor" | "ceil" | "mod" | "rem" | "max" | "min" | "sum" | "product" => {
                        InferredType::Real
                    }
                    // der returns the same type as its argument (preserves array dimensions)
                    "der" => {
                        if let Some(arg) = args.first() {
                            infer_expression_type(arg, defined)
                        } else {
                            InferredType::Real
                        }
                    }
                    // Boolean functions
                    "initial" | "terminal" | "edge" | "change" | "sample" => InferredType::Boolean,
                    // Size returns Integer
                    "size" | "ndims" => InferredType::Integer,
                    // pre maintains type (simplified to Unknown here)
                    _ => InferredType::Unknown,
                }
            } else {
                InferredType::Unknown
            }
        }
        Expression::Binary { lhs, op, rhs } => {
            let lhs_type = infer_expression_type(lhs, defined);
            let rhs_type = infer_expression_type(rhs, defined);

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
                // Arithmetic operators: promote to Real if either side is Real
                OpBinary::Add(_)
                | OpBinary::Sub(_)
                | OpBinary::Mul(_)
                | OpBinary::Div(_)
                | OpBinary::Exp(_) => {
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
                _ => InferredType::Unknown,
            }
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
    }
}
