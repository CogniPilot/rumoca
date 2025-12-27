//! Type inference for Modelica expressions.
//!
//! This module provides type inference capabilities for semantic analysis,
//! used by diagnostics to detect type mismatches in equations and expressions.

use std::collections::HashMap;

use crate::ir::ast::{ClassDefinition, Expression, OpBinary, TerminalType};

use super::symbols::DefinedSymbol;

/// Trait for looking up class definitions by type name.
///
/// Implement this to provide class hierarchy lookup for type inference.
/// This enables resolving member types like `pid1.u` where `pid1` is of type `PID`.
pub trait ClassLookup {
    /// Look up a class definition by its type name.
    fn lookup_class(&self, type_name: &str) -> Option<&ClassDefinition>;
}

/// Simple implementation using a HashMap of class definitions.
impl ClassLookup for HashMap<String, ClassDefinition> {
    fn lookup_class(&self, type_name: &str) -> Option<&ClassDefinition> {
        self.get(type_name)
    }
}

/// Implementation for IndexMap (used by StoredDefinition.class_list).
impl ClassLookup for indexmap::IndexMap<String, ClassDefinition> {
    fn lookup_class(&self, type_name: &str) -> Option<&ClassDefinition> {
        self.get(type_name)
    }
}

/// Inferred type for an expression.
///
/// Used for semantic analysis and type checking of Modelica code.
#[derive(Clone, Debug, PartialEq)]
pub enum SymbolType {
    Real,
    Integer,
    Boolean,
    String,
    /// Array type with element type and optional size
    Array(Box<SymbolType>, Option<usize>),
    /// User-defined class, record, model, or block type
    Class(std::string::String),
    /// Enumeration type
    Enumeration(std::string::String),
    Unknown,
}

impl SymbolType {
    /// Get the base (scalar) type, stripping array dimensions
    pub fn base_type(&self) -> &SymbolType {
        match self {
            SymbolType::Array(inner, _) => inner.base_type(),
            other => other,
        }
    }

    /// Check if this is a numeric type (Real or Integer)
    pub fn is_numeric(&self) -> bool {
        matches!(self.base_type(), SymbolType::Real | SymbolType::Integer)
    }

    /// Check if two types are compatible for assignment/equations
    pub fn is_compatible_with(&self, other: &SymbolType) -> bool {
        match (self, other) {
            (SymbolType::Unknown, _) | (_, SymbolType::Unknown) => true,
            (SymbolType::Real, SymbolType::Real) => true,
            (SymbolType::Integer, SymbolType::Integer) => true,
            (SymbolType::Boolean, SymbolType::Boolean) => true,
            (SymbolType::String, SymbolType::String) => true,
            // Real and Integer are compatible (Integer can be promoted to Real)
            (SymbolType::Real, SymbolType::Integer) | (SymbolType::Integer, SymbolType::Real) => {
                true
            }
            // Arrays are compatible if element types and sizes are compatible
            (SymbolType::Array(t1, s1), SymbolType::Array(t2, s2)) => {
                // Element types must be compatible
                if !t1.is_compatible_with(t2) {
                    return false;
                }
                // Sizes must match if both are known
                match (s1, s2) {
                    (Some(a), Some(b)) => a == b,
                    // Unknown size (None) is compatible with any size
                    _ => true,
                }
            }
            // Class types are compatible with primitive types because connector type aliases
            // like `connector RealInput = input Real` ARE Real types.
            // Without full type resolution we can't know if a Class is an alias, so we allow it.
            // The compiler will catch actual type errors during flattening.
            (SymbolType::Class(_), SymbolType::Real)
            | (SymbolType::Real, SymbolType::Class(_))
            | (SymbolType::Class(_), SymbolType::Integer)
            | (SymbolType::Integer, SymbolType::Class(_))
            | (SymbolType::Class(_), SymbolType::Boolean)
            | (SymbolType::Boolean, SymbolType::Class(_))
            | (SymbolType::Class(_), SymbolType::String)
            | (SymbolType::String, SymbolType::Class(_)) => true,
            // Two different Class types are also considered compatible (may be related via extends)
            (SymbolType::Class(_), SymbolType::Class(_)) => true,
            // Enumeration types are compatible if they have the same name
            (SymbolType::Enumeration(n1), SymbolType::Enumeration(n2)) => n1 == n2,
            // Scalar and array are not compatible
            _ => false,
        }
    }
}

impl std::fmt::Display for SymbolType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            SymbolType::Real => write!(f, "Real"),
            SymbolType::Integer => write!(f, "Integer"),
            SymbolType::Boolean => write!(f, "Boolean"),
            SymbolType::String => write!(f, "String"),
            SymbolType::Array(inner, size) => {
                if let Some(s) = size {
                    write!(f, "{}[{}]", inner, s)
                } else {
                    write!(f, "{}[:]", inner)
                }
            }
            SymbolType::Class(name) => write!(f, "{}", name),
            SymbolType::Enumeration(name) => write!(f, "{}", name),
            SymbolType::Unknown => write!(f, "Unknown"),
        }
    }
}

/// Convert a type name string to an SymbolType
pub fn type_from_name(name: &str) -> SymbolType {
    match name {
        "Real" => SymbolType::Real,
        "Integer" => SymbolType::Integer,
        "Boolean" | "Bool" => SymbolType::Boolean,
        "String" => SymbolType::String,
        // Built-in enumerations
        "AssertionLevel" => SymbolType::Enumeration("AssertionLevel".to_string()),
        "StateSelect" => SymbolType::Enumeration("StateSelect".to_string()),
        // User-defined types are treated as Class types
        // Enumerations would need additional context to distinguish
        "" => SymbolType::Unknown,
        _ => SymbolType::Class(name.to_string()),
    }
}

/// Check if a name is a built-in Modelica enumeration
pub fn is_builtin_enum(name: &str) -> bool {
    matches!(name, "AssertionLevel" | "StateSelect")
}

impl SymbolType {
    /// Check if this type represents a user-defined class (not a primitive)
    pub fn is_class_type(&self) -> bool {
        matches!(self, SymbolType::Class(_) | SymbolType::Enumeration(_))
    }

    /// Get the type name as a string
    pub fn type_name(&self) -> String {
        self.to_string()
    }
}

/// Resolve the type of a member access path within a class.
///
/// Given a class name and remaining path parts (e.g., ["u"] for `pid1.u`),
/// looks up the member in the class definition and returns its type.
fn resolve_member_type<C: ClassLookup>(
    classes: &C,
    class_name: &str,
    path: &[crate::ir::ast::ComponentRefPart],
) -> SymbolType {
    if path.is_empty() {
        return SymbolType::Unknown;
    }

    // Look up the class definition
    let class_def = match classes.lookup_class(class_name) {
        Some(c) => c,
        None => return SymbolType::Unknown,
    };

    // Find the first member in the path
    let member_name = &path[0].ident.text;
    let component = match class_def.components.get(member_name) {
        Some(c) => c,
        None => return SymbolType::Unknown,
    };

    // Get the member's type
    let member_type = type_from_name(&component.type_name.to_string());

    // If there are more parts in the path, recurse into the member's type
    if path.len() > 1 {
        if let SymbolType::Class(nested_class) = &member_type {
            return resolve_member_type(classes, nested_class, &path[1..]);
        }
        return SymbolType::Unknown;
    }

    // Build array type if the component has dimensions
    if component.shape.is_empty() {
        member_type
    } else {
        let mut result = member_type;
        for &dim in component.shape.iter().rev() {
            result = SymbolType::Array(Box::new(result), Some(dim));
        }
        // Account for subscripts in the path
        if let Some(subs) = &path[0].subs {
            for _sub in subs {
                if let SymbolType::Array(inner, _) = result {
                    result = *inner;
                }
            }
        }
        result
    }
}

/// Infer the type of an expression given the defined symbols.
///
/// This is the simple version without class hierarchy lookup.
/// For member access like `pid1.u` where `pid1` is a class type, this will return Unknown.
pub fn infer_expression_type(
    expr: &Expression,
    defined: &HashMap<String, DefinedSymbol>,
) -> SymbolType {
    infer_expression_type_with_classes::<HashMap<String, ClassDefinition>>(expr, defined, None)
}

/// Infer the type of an expression with optional class hierarchy lookup.
///
/// When `classes` is provided, this can resolve member types like `pid1.u` by
/// looking up `u` within the class definition of `pid1`'s type.
pub fn infer_expression_type_with_classes<C: ClassLookup>(
    expr: &Expression,
    defined: &HashMap<String, DefinedSymbol>,
    classes: Option<&C>,
) -> SymbolType {
    match expr {
        Expression::Empty => SymbolType::Unknown,
        Expression::ComponentReference(comp_ref) => {
            if let Some(first) = comp_ref.parts.first() {
                if let Some(sym) = defined.get(&first.ident.text) {
                    let base = sym.declared_type.clone();

                    // Handle multi-part references (e.g., pid1.u)
                    if comp_ref.parts.len() > 1 {
                        if let SymbolType::Class(class_name) = &base {
                            // Try to resolve member type through class lookup
                            if let Some(class_lookup) = classes {
                                return resolve_member_type(
                                    class_lookup,
                                    class_name,
                                    &comp_ref.parts[1..],
                                );
                            }
                        }
                        // Can't resolve - return Unknown to avoid false positives
                        return SymbolType::Unknown;
                    }

                    if sym.shape.is_empty() {
                        base
                    } else {
                        // Build array type from innermost to outermost
                        let mut result = base;
                        for &dim in sym.shape.iter().rev() {
                            result = SymbolType::Array(Box::new(result), Some(dim));
                        }
                        // Account for subscripts - each index reduces one dimension
                        // e.g., q[3] where q is Real[4] becomes Real (scalar)
                        // e.g., R[1,2] where R is Real[3,3] becomes Real (scalar)
                        if let Some(subs) = &first.subs {
                            for _sub in subs {
                                // Each subscript strips one array dimension
                                if let SymbolType::Array(inner, _) = result {
                                    result = *inner;
                                }
                            }
                        }
                        result
                    }
                } else {
                    // Check if it's 'time' (global Real)
                    if first.ident.text == "time" {
                        SymbolType::Real
                    } else if is_builtin_enum(&first.ident.text) {
                        // Built-in enumeration literal (e.g., AssertionLevel.warning)
                        SymbolType::Enumeration(first.ident.text.clone())
                    } else {
                        SymbolType::Unknown
                    }
                }
            } else {
                SymbolType::Unknown
            }
        }
        Expression::Terminal {
            terminal_type,
            token: _,
        } => match terminal_type {
            TerminalType::UnsignedInteger => SymbolType::Integer,
            TerminalType::UnsignedReal => SymbolType::Real,
            TerminalType::String => SymbolType::String,
            TerminalType::Bool => SymbolType::Boolean,
            _ => SymbolType::Unknown,
        },
        Expression::FunctionCall { comp, args } => infer_function_call_type(comp, args, defined),
        Expression::Binary { lhs, op, rhs } => {
            let lhs_type = infer_expression_type(lhs, defined);
            let rhs_type = infer_expression_type(rhs, defined);
            infer_binary_op_type(op, &lhs_type, &rhs_type)
        }
        Expression::Unary { op: _, rhs } => infer_expression_type(rhs, defined),
        Expression::Array { elements, .. } => {
            if let Some(first) = elements.first() {
                let elem_type = infer_expression_type(first, defined);
                SymbolType::Array(Box::new(elem_type), Some(elements.len()))
            } else {
                SymbolType::Unknown
            }
        }
        Expression::Tuple { elements: _ } => SymbolType::Unknown,
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
            SymbolType::Array(Box::new(SymbolType::Integer), None)
        }
        Expression::Parenthesized { inner } => infer_expression_type(inner, defined),
        Expression::ArrayComprehension { expr, .. } => {
            // Array comprehension produces an array of the expression type
            let elem_type = infer_expression_type(expr, defined);
            SymbolType::Array(Box::new(elem_type), None)
        }
    }
}

/// Infer the return type of a function call
fn infer_function_call_type(
    comp: &crate::ir::ast::ComponentReference,
    args: &[Expression],
    defined: &HashMap<String, DefinedSymbol>,
) -> SymbolType {
    if let Some(first) = comp.parts.first() {
        match first.ident.text.as_str() {
            // Trigonometric and math functions return Real
            "sin" | "cos" | "tan" | "asin" | "acos" | "atan" | "atan2" | "sinh" | "cosh"
            | "tanh" | "exp" | "log" | "log10" | "sqrt" | "abs" | "sign" | "floor" | "ceil"
            | "mod" | "rem" | "max" | "min" | "sum" | "product" => SymbolType::Real,

            // der returns the same type as its argument (preserves array dimensions)
            "der" | "pre" | "delay" => {
                if let Some(arg) = args.first() {
                    infer_expression_type(arg, defined)
                } else {
                    SymbolType::Real
                }
            }

            // cross(a, b) returns a 3-vector
            "cross" => SymbolType::Array(Box::new(SymbolType::Real), Some(3)),

            // transpose, symmetric, skew return matrices (preserve first arg type)
            "transpose" | "symmetric" | "skew" => {
                if let Some(arg) = args.first() {
                    infer_expression_type(arg, defined)
                } else {
                    SymbolType::Unknown
                }
            }

            // identity, zeros, ones, fill, diagonal return arrays
            "identity" | "diagonal" => {
                // identity(n) returns Real[n,n], diagonal(v) returns Real[n,n]
                SymbolType::Array(
                    Box::new(SymbolType::Array(Box::new(SymbolType::Real), None)),
                    None,
                )
            }
            "zeros" | "ones" | "fill" => {
                // These return arrays, but we don't know the dimensions statically
                SymbolType::Array(Box::new(SymbolType::Real), None)
            }

            // Boolean functions
            "initial" | "terminal" | "edge" | "change" | "sample" => SymbolType::Boolean,

            // Size returns Integer
            "size" | "ndims" => SymbolType::Integer,

            // User-defined functions - look up in defined symbols
            name => {
                if let Some(sym) = defined.get(name) {
                    if let Some((ret_type, ret_shape)) = &sym.function_return {
                        if ret_shape.is_empty() {
                            ret_type.clone()
                        } else {
                            let mut result = ret_type.clone();
                            for &dim in ret_shape.iter().rev() {
                                result = SymbolType::Array(Box::new(result), Some(dim));
                            }
                            result
                        }
                    } else {
                        SymbolType::Unknown
                    }
                } else {
                    SymbolType::Unknown
                }
            }
        }
    } else {
        SymbolType::Unknown
    }
}

/// Infer the result type of a binary operation
fn infer_binary_op_type(op: &OpBinary, lhs_type: &SymbolType, rhs_type: &SymbolType) -> SymbolType {
    match op {
        // Comparison operators return Boolean
        OpBinary::Lt(_)
        | OpBinary::Le(_)
        | OpBinary::Gt(_)
        | OpBinary::Ge(_)
        | OpBinary::Eq(_)
        | OpBinary::Neq(_) => SymbolType::Boolean,

        // Logical operators return Boolean
        OpBinary::And(_) | OpBinary::Or(_) => SymbolType::Boolean,

        // Arithmetic operators
        OpBinary::Add(_) | OpBinary::Sub(_) => infer_arithmetic_result(lhs_type, rhs_type, false),
        OpBinary::Mul(_) => infer_multiplication_result(lhs_type, rhs_type),
        OpBinary::Div(_) => infer_division_result(lhs_type, rhs_type),
        OpBinary::Exp(_) => infer_exponentiation_result(lhs_type, rhs_type),
        _ => SymbolType::Unknown,
    }
}

/// Infer result type for addition/subtraction
fn infer_arithmetic_result(
    lhs_type: &SymbolType,
    rhs_type: &SymbolType,
    _is_mul: bool,
) -> SymbolType {
    match (lhs_type, rhs_type) {
        (SymbolType::Array(_, _), _) => lhs_type.clone(),
        (_, SymbolType::Array(_, _)) => rhs_type.clone(),
        _ => {
            if matches!(lhs_type.base_type(), SymbolType::Real)
                || matches!(rhs_type.base_type(), SymbolType::Real)
            {
                SymbolType::Real
            } else if matches!(lhs_type.base_type(), SymbolType::Integer)
                && matches!(rhs_type.base_type(), SymbolType::Integer)
            {
                SymbolType::Integer
            } else {
                SymbolType::Unknown
            }
        }
    }
}

/// Infer result type for multiplication
fn infer_multiplication_result(lhs_type: &SymbolType, rhs_type: &SymbolType) -> SymbolType {
    match (lhs_type, rhs_type) {
        // Scalar * Array -> Array
        (SymbolType::Real | SymbolType::Integer, SymbolType::Array(_, _)) => rhs_type.clone(),
        // Array * Scalar -> Array
        (SymbolType::Array(_, _), SymbolType::Real | SymbolType::Integer) => lhs_type.clone(),
        // Matrix[m,n] * Vector[n] -> Vector[m]
        (SymbolType::Array(inner_lhs, Some(m)), SymbolType::Array(inner_rhs, _)) => {
            if let SymbolType::Array(_, _) = inner_lhs.as_ref() {
                // Matrix * Vector -> Vector
                SymbolType::Array(Box::new(inner_rhs.base_type().clone()), Some(*m))
            } else {
                SymbolType::Unknown
            }
        }
        // Both scalars
        _ => {
            if matches!(lhs_type.base_type(), SymbolType::Real)
                || matches!(rhs_type.base_type(), SymbolType::Real)
            {
                SymbolType::Real
            } else if matches!(lhs_type.base_type(), SymbolType::Integer)
                && matches!(rhs_type.base_type(), SymbolType::Integer)
            {
                SymbolType::Integer
            } else {
                SymbolType::Unknown
            }
        }
    }
}

/// Infer result type for division
fn infer_division_result(lhs_type: &SymbolType, rhs_type: &SymbolType) -> SymbolType {
    match (lhs_type, rhs_type) {
        // Array / Scalar -> Array
        (SymbolType::Array(_, _), SymbolType::Real | SymbolType::Integer) => lhs_type.clone(),
        // Scalar / Array -> Array (element-wise)
        (SymbolType::Real | SymbolType::Integer, SymbolType::Array(_, _)) => rhs_type.clone(),
        // Both scalars
        _ => {
            if matches!(lhs_type.base_type(), SymbolType::Real)
                || matches!(rhs_type.base_type(), SymbolType::Real)
            {
                SymbolType::Real
            } else {
                SymbolType::Unknown
            }
        }
    }
}

/// Infer result type for exponentiation
fn infer_exponentiation_result(lhs_type: &SymbolType, rhs_type: &SymbolType) -> SymbolType {
    match (lhs_type, rhs_type) {
        (SymbolType::Array(_, _), SymbolType::Real | SymbolType::Integer) => lhs_type.clone(),
        _ => {
            if matches!(lhs_type.base_type(), SymbolType::Real)
                || matches!(rhs_type.base_type(), SymbolType::Real)
            {
                SymbolType::Real
            } else if matches!(lhs_type.base_type(), SymbolType::Integer)
                && matches!(rhs_type.base_type(), SymbolType::Integer)
            {
                SymbolType::Integer
            } else {
                SymbolType::Unknown
            }
        }
    }
}
