//! Expression evaluation utilities for compile-time constant folding.
//!
//! This module provides functions to evaluate expressions to concrete values
//! when the operands are known at compile time (parameters, constants, etc.).
//!
//! # Usage
//!
//! ```ignore
//! use rumoca::ir::transform::eval::{eval_integer, eval_real, eval_boolean};
//!
//! // Evaluate an expression using known component values
//! if let Some(value) = eval_integer(&expr, &components) {
//!     println!("Expression evaluates to: {}", value);
//! }
//! ```

use crate::ir::ast::{Component, Expression, OpBinary, OpUnary, TerminalType, Variability};
use indexmap::IndexMap;

/// Configuration for expression evaluation behavior.
#[derive(Debug, Clone, Copy, Default)]
pub struct EvalConfig {
    /// If true, only evaluate Parameter components.
    /// If false, evaluate any component with a start value.
    pub parameters_only: bool,
}

impl EvalConfig {
    /// Create config that only evaluates parameters
    pub fn parameters_only() -> Self {
        Self {
            parameters_only: true,
        }
    }

    /// Create config that evaluates all components
    pub fn all_components() -> Self {
        Self {
            parameters_only: false,
        }
    }
}

/// Evaluate an expression to an integer value.
///
/// Handles:
/// - Integer and real literals (if whole number)
/// - Component references (with optional parameter-only restriction)
/// - Unary +/- operators
/// - Binary +, -, *, / operators
/// - size(array, dim) function
/// - integer(), mod(), div() functions
/// - If-then-else expressions
/// - Parenthesized expressions
pub fn eval_integer(expr: &Expression, components: &IndexMap<String, Component>) -> Option<i64> {
    eval_integer_with_config(expr, components, EvalConfig::default())
}

/// Evaluate an expression to an integer with configuration.
pub fn eval_integer_with_config(
    expr: &Expression,
    components: &IndexMap<String, Component>,
    config: EvalConfig,
) -> Option<i64> {
    match expr {
        Expression::Terminal {
            terminal_type: TerminalType::UnsignedInteger,
            token,
        } => token.text.parse().ok(),

        Expression::Terminal {
            terminal_type: TerminalType::UnsignedReal,
            token,
        } => {
            let f: f64 = token.text.parse().ok()?;
            if f.fract() == 0.0 {
                Some(f as i64)
            } else {
                None
            }
        }

        Expression::ComponentReference(comp_ref) => {
            // Handle both simple names and dotted names
            if comp_ref.parts.iter().all(|p| p.subs.is_none()) {
                let name = build_component_name(comp_ref);

                if let Some(comp) = components.get(&name) {
                    // Check if we should evaluate this component
                    if !config.parameters_only
                        || matches!(comp.variability, Variability::Parameter(_))
                    {
                        return eval_integer_with_config(&comp.start, components, config);
                    }
                }
            }
            None
        }

        Expression::Unary { op, rhs } => {
            let val = eval_integer_with_config(rhs, components, config)?;
            match op {
                OpUnary::Minus(_) => Some(-val),
                OpUnary::Plus(_) => Some(val),
                _ => None,
            }
        }

        Expression::Binary { op, lhs, rhs } => {
            let l = eval_integer_with_config(lhs, components, config)?;
            let r = eval_integer_with_config(rhs, components, config)?;
            match op {
                OpBinary::Add(_) => Some(l.saturating_add(r)),
                OpBinary::Sub(_) => Some(l.saturating_sub(r)),
                OpBinary::Mul(_) => Some(l.saturating_mul(r)),
                OpBinary::Div(_) => {
                    if r != 0 {
                        Some(l / r)
                    } else {
                        None
                    }
                }
                _ => None,
            }
        }

        Expression::FunctionCall { comp, args } => {
            eval_integer_function(comp, args, components, config)
        }

        Expression::If {
            branches,
            else_branch,
        } => {
            // Evaluate if-then-else expression for conditional values
            for (condition, result) in branches {
                if let Some(cond_val) = eval_boolean_with_config(condition, components, config) {
                    if cond_val {
                        return eval_integer_with_config(result, components, config);
                    }
                } else {
                    // Condition can't be evaluated at compile time
                    return None;
                }
            }
            // All conditions were false, evaluate else branch
            eval_integer_with_config(else_branch, components, config)
        }

        Expression::Parenthesized { inner } => eval_integer_with_config(inner, components, config),

        _ => None,
    }
}

/// Evaluate integer-returning function calls
fn eval_integer_function(
    comp: &crate::ir::ast::ComponentReference,
    args: &[Expression],
    components: &IndexMap<String, Component>,
    config: EvalConfig,
) -> Option<i64> {
    let first_part = comp.parts.first()?;
    let func_name = first_part.ident.text.as_str();

    match func_name {
        // Handle size(array, dim) function
        "size" if !args.is_empty() => {
            if let Expression::ComponentReference(array_ref) = &args[0]
                && array_ref.parts.iter().all(|p| p.subs.is_none())
            {
                let array_name = build_component_name(array_ref);

                if let Some(array_comp) = components.get(&array_name) {
                    let dim_index = if args.len() >= 2 {
                        eval_integer_with_config(&args[1], components, config).unwrap_or(1) as usize
                    } else {
                        1
                    };

                    // First check evaluated shape
                    if !array_comp.shape.is_empty() && dim_index <= array_comp.shape.len() {
                        return Some(array_comp.shape[dim_index - 1] as i64);
                    }

                    // Check if it's an array literal in start
                    if let Expression::Array { elements, .. } = &array_comp.start
                        && dim_index == 1
                    {
                        return Some(elements.len() as i64);
                    }
                }
            }
            None
        }

        // Handle integer(x) - truncation toward zero
        "integer" if args.len() == 1 => {
            eval_real_with_config(&args[0], components, config).map(|val| val.trunc() as i64)
        }

        // Handle mod(x, y) - modulo operation
        "mod" if args.len() == 2 => {
            let x = eval_integer_with_config(&args[0], components, config)?;
            let y = eval_integer_with_config(&args[1], components, config)?;
            if y != 0 {
                // Modelica mod is floored division remainder
                Some(((x % y) + y) % y)
            } else {
                None
            }
        }

        // Handle div(x, y) - integer division (truncated toward zero)
        "div" if args.len() == 2 => {
            let x = eval_integer_with_config(&args[0], components, config)?;
            let y = eval_integer_with_config(&args[1], components, config)?;
            if y != 0 { Some(x / y) } else { None }
        }

        _ => None,
    }
}

/// Evaluate an expression to a real (f64) value.
pub fn eval_real(expr: &Expression, components: &IndexMap<String, Component>) -> Option<f64> {
    eval_real_with_config(expr, components, EvalConfig::default())
}

/// Evaluate an expression to a real with configuration.
pub fn eval_real_with_config(
    expr: &Expression,
    components: &IndexMap<String, Component>,
    config: EvalConfig,
) -> Option<f64> {
    match expr {
        Expression::Terminal {
            terminal_type: TerminalType::UnsignedReal,
            token,
        } => token.text.parse().ok(),

        Expression::Terminal {
            terminal_type: TerminalType::UnsignedInteger,
            token,
        } => token.text.parse::<i64>().ok().map(|v| v as f64),

        Expression::ComponentReference(comp_ref) => {
            if comp_ref.parts.iter().all(|p| p.subs.is_none()) {
                let name = build_component_name(comp_ref);

                if let Some(comp) = components.get(&name)
                    && (!config.parameters_only
                        || matches!(comp.variability, Variability::Parameter(_)))
                {
                    return eval_real_with_config(&comp.start, components, config);
                }
            }
            None
        }

        Expression::Unary { op, rhs } => {
            let val = eval_real_with_config(rhs, components, config)?;
            match op {
                OpUnary::Minus(_) => Some(-val),
                OpUnary::Plus(_) => Some(val),
                _ => None,
            }
        }

        Expression::Binary { op, lhs, rhs } => {
            let l = eval_real_with_config(lhs, components, config)?;
            let r = eval_real_with_config(rhs, components, config)?;
            match op {
                OpBinary::Add(_) => Some(l + r),
                OpBinary::Sub(_) => Some(l - r),
                OpBinary::Mul(_) => Some(l * r),
                OpBinary::Div(_) => {
                    if r != 0.0 {
                        Some(l / r)
                    } else {
                        None
                    }
                }
                _ => None,
            }
        }

        Expression::Parenthesized { inner } => eval_real_with_config(inner, components, config),

        _ => None,
    }
}

/// Evaluate an expression to a boolean value.
pub fn eval_boolean(expr: &Expression, components: &IndexMap<String, Component>) -> Option<bool> {
    eval_boolean_with_config(expr, components, EvalConfig::default())
}

/// Evaluate an expression to a boolean with configuration.
pub fn eval_boolean_with_config(
    expr: &Expression,
    components: &IndexMap<String, Component>,
    config: EvalConfig,
) -> Option<bool> {
    match expr {
        Expression::Terminal {
            terminal_type: TerminalType::Bool,
            token,
        } => Some(token.text == "true"),

        Expression::ComponentReference(comp_ref) => {
            if comp_ref.parts.len() == 1 && comp_ref.parts[0].subs.is_none() {
                let name = &comp_ref.parts[0].ident.text;
                if let Some(comp) = components.get(name) {
                    // Evaluate if it's a parameter or has a boolean start value
                    let should_eval = !config.parameters_only
                        || matches!(comp.variability, Variability::Parameter(_))
                        || matches!(
                            &comp.start,
                            Expression::Terminal {
                                terminal_type: TerminalType::Bool,
                                ..
                            }
                        );

                    if should_eval {
                        return eval_boolean_with_config(&comp.start, components, config);
                    }
                }
            }
            None
        }

        Expression::Unary { op, rhs } => {
            let val = eval_boolean_with_config(rhs, components, config)?;
            match op {
                OpUnary::Not(_) => Some(!val),
                _ => None,
            }
        }

        Expression::Binary { op, lhs, rhs } => {
            match op {
                OpBinary::And(_) => {
                    let l = eval_boolean_with_config(lhs, components, config)?;
                    let r = eval_boolean_with_config(rhs, components, config)?;
                    Some(l && r)
                }
                OpBinary::Or(_) => {
                    let l = eval_boolean_with_config(lhs, components, config)?;
                    let r = eval_boolean_with_config(rhs, components, config)?;
                    Some(l || r)
                }
                // Comparison operators
                OpBinary::Eq(_) => {
                    // Try integer comparison
                    if let (Some(l), Some(r)) = (
                        eval_integer_with_config(lhs, components, config),
                        eval_integer_with_config(rhs, components, config),
                    ) {
                        return Some(l == r);
                    }
                    // Try boolean comparison
                    if let (Some(l), Some(r)) = (
                        eval_boolean_with_config(lhs, components, config),
                        eval_boolean_with_config(rhs, components, config),
                    ) {
                        return Some(l == r);
                    }
                    None
                }
                OpBinary::Neq(_) => {
                    if let (Some(l), Some(r)) = (
                        eval_integer_with_config(lhs, components, config),
                        eval_integer_with_config(rhs, components, config),
                    ) {
                        return Some(l != r);
                    }
                    if let (Some(l), Some(r)) = (
                        eval_boolean_with_config(lhs, components, config),
                        eval_boolean_with_config(rhs, components, config),
                    ) {
                        return Some(l != r);
                    }
                    None
                }
                OpBinary::Lt(_) => {
                    let l = eval_integer_with_config(lhs, components, config)?;
                    let r = eval_integer_with_config(rhs, components, config)?;
                    Some(l < r)
                }
                OpBinary::Le(_) => {
                    let l = eval_integer_with_config(lhs, components, config)?;
                    let r = eval_integer_with_config(rhs, components, config)?;
                    Some(l <= r)
                }
                OpBinary::Gt(_) => {
                    let l = eval_integer_with_config(lhs, components, config)?;
                    let r = eval_integer_with_config(rhs, components, config)?;
                    Some(l > r)
                }
                OpBinary::Ge(_) => {
                    let l = eval_integer_with_config(lhs, components, config)?;
                    let r = eval_integer_with_config(rhs, components, config)?;
                    Some(l >= r)
                }
                _ => None,
            }
        }

        _ => None,
    }
}

/// Build a full component name from a component reference.
fn build_component_name(comp_ref: &crate::ir::ast::ComponentReference) -> String {
    if comp_ref.parts.len() == 1 {
        comp_ref.parts[0].ident.text.clone()
    } else {
        comp_ref
            .parts
            .iter()
            .map(|p| p.ident.text.as_str())
            .collect::<Vec<_>>()
            .join(".")
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ir::ast::{ComponentRefPart, ComponentReference, Token};

    fn make_int_terminal(value: i64) -> Expression {
        Expression::Terminal {
            terminal_type: TerminalType::UnsignedInteger,
            token: Token {
                text: value.to_string(),
                ..Default::default()
            },
        }
    }

    fn make_real_terminal(value: f64) -> Expression {
        Expression::Terminal {
            terminal_type: TerminalType::UnsignedReal,
            token: Token {
                text: value.to_string(),
                ..Default::default()
            },
        }
    }

    #[test]
    fn test_eval_integer_literal() {
        let components = IndexMap::new();
        let expr = make_int_terminal(42);
        assert_eq!(eval_integer(&expr, &components), Some(42));
    }

    #[test]
    fn test_eval_real_as_integer() {
        let components = IndexMap::new();
        let expr = make_real_terminal(42.0);
        assert_eq!(eval_integer(&expr, &components), Some(42));

        // Non-integer real should return None
        let expr = make_real_terminal(42.5);
        assert_eq!(eval_integer(&expr, &components), None);
    }

    #[test]
    fn test_eval_binary_ops() {
        let components = IndexMap::new();

        let add = Expression::Binary {
            op: OpBinary::Add(Token::default()),
            lhs: Box::new(make_int_terminal(10)),
            rhs: Box::new(make_int_terminal(5)),
        };
        assert_eq!(eval_integer(&add, &components), Some(15));

        let sub = Expression::Binary {
            op: OpBinary::Sub(Token::default()),
            lhs: Box::new(make_int_terminal(10)),
            rhs: Box::new(make_int_terminal(5)),
        };
        assert_eq!(eval_integer(&sub, &components), Some(5));

        let mul = Expression::Binary {
            op: OpBinary::Mul(Token::default()),
            lhs: Box::new(make_int_terminal(10)),
            rhs: Box::new(make_int_terminal(5)),
        };
        assert_eq!(eval_integer(&mul, &components), Some(50));

        let div = Expression::Binary {
            op: OpBinary::Div(Token::default()),
            lhs: Box::new(make_int_terminal(10)),
            rhs: Box::new(make_int_terminal(5)),
        };
        assert_eq!(eval_integer(&div, &components), Some(2));
    }

    #[test]
    fn test_eval_division_by_zero() {
        let components = IndexMap::new();
        let div = Expression::Binary {
            op: OpBinary::Div(Token::default()),
            lhs: Box::new(make_int_terminal(10)),
            rhs: Box::new(make_int_terminal(0)),
        };
        assert_eq!(eval_integer(&div, &components), None);
    }

    #[test]
    fn test_eval_unary() {
        let components = IndexMap::new();

        let neg = Expression::Unary {
            op: OpUnary::Minus(Token::default()),
            rhs: Box::new(make_int_terminal(42)),
        };
        assert_eq!(eval_integer(&neg, &components), Some(-42));
    }

    #[test]
    fn test_eval_boolean() {
        let components = IndexMap::new();

        let true_expr = Expression::Terminal {
            terminal_type: TerminalType::Bool,
            token: Token {
                text: "true".to_string(),
                ..Default::default()
            },
        };
        assert_eq!(eval_boolean(&true_expr, &components), Some(true));

        let false_expr = Expression::Terminal {
            terminal_type: TerminalType::Bool,
            token: Token {
                text: "false".to_string(),
                ..Default::default()
            },
        };
        assert_eq!(eval_boolean(&false_expr, &components), Some(false));
    }

    #[test]
    fn test_eval_component_reference() {
        let mut components = IndexMap::new();
        components.insert(
            "n".to_string(),
            Component {
                name: "n".to_string(),
                variability: Variability::Parameter(Token::default()),
                start: make_int_terminal(10),
                ..Default::default()
            },
        );

        let comp_ref = Expression::ComponentReference(ComponentReference {
            local: false,
            parts: vec![ComponentRefPart {
                ident: Token {
                    text: "n".to_string(),
                    ..Default::default()
                },
                subs: None,
            }],
        });

        // With default config (parameters_only = false), should evaluate
        assert_eq!(eval_integer(&comp_ref, &components), Some(10));

        // With parameters_only = true, should also evaluate (it is a parameter)
        assert_eq!(
            eval_integer_with_config(&comp_ref, &components, EvalConfig::parameters_only()),
            Some(10)
        );
    }
}
