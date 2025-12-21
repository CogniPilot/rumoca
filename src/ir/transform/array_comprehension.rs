//! Array comprehension expander
//!
//! This module expands array comprehensions like `{expr for i in 1:n}` into
//! explicit arrays `{expr[i=1], expr[i=2], ..., expr[i=n]}`.
//!
//! For example:
//! - `{i*2 for i in 1:3}` becomes `{2, 4, 6}`
//! - `{A[i,j]*x[j] for j in 1:n}` becomes `{A[i,1]*x[1], A[i,2]*x[2], ..., A[i,n]*x[n]}`

use crate::ir::ast::{
    ClassDefinition, Component, ComponentRefPart, ComponentReference, Expression, ForIndex,
    OpBinary, OpUnary, Subscript, TerminalType, Token,
};
use crate::ir::visitor::{MutVisitable, MutVisitor};
use indexmap::IndexMap;

/// Maximum recursion depth for evaluation to prevent stack overflow
const MAX_EVAL_DEPTH: usize = 100;

// =============================================================================
// Array Comprehension Expander Visitor
// =============================================================================

/// Visitor that expands array comprehensions into explicit arrays.
///
/// This replaces manual recursion through equations and expressions with
/// the standard MutVisitor pattern for cleaner, more maintainable code.
struct ComprehensionExpander<'a> {
    params: &'a IndexMap<String, Component>,
}

impl<'a> ComprehensionExpander<'a> {
    fn new(params: &'a IndexMap<String, Component>) -> Self {
        Self { params }
    }
}

impl MutVisitor for ComprehensionExpander<'_> {
    fn exit_expression(&mut self, expr: &mut Expression) {
        // After children have been processed, check if this is an ArrayComprehension
        // that can be expanded
        let expanded = match expr {
            Expression::ArrayComprehension {
                expr: inner_expr,
                indices,
            } => try_expand_comprehension(inner_expr, indices, self.params),
            _ => None,
        };
        if let Some(expanded) = expanded {
            *expr = expanded;
        }
    }
}

/// Expand all array comprehensions in a class definition.
///
/// This function uses the visitor pattern to walk through all expressions
/// in the class and replace ArrayComprehension expressions with expanded
/// Array expressions.
pub fn expand_array_comprehensions(class: &mut ClassDefinition) {
    // Build parameter map for evaluation
    let params: IndexMap<String, Component> = class
        .components
        .iter()
        .filter(|(_, c)| matches!(c.variability, crate::ir::ast::Variability::Parameter(..)))
        .map(|(k, v)| (k.clone(), v.clone()))
        .collect();

    // Use visitor pattern to expand comprehensions
    let mut expander = ComprehensionExpander::new(&params);
    class.accept_mut(&mut expander);
}

/// Try to expand an array comprehension into an explicit array
fn try_expand_comprehension(
    expr: &Expression,
    indices: &[ForIndex],
    params: &IndexMap<String, Component>,
) -> Option<Expression> {
    if indices.is_empty() {
        return None;
    }

    // For now, handle single-index comprehensions
    // Multi-index (cartesian product) support can be added later
    if indices.len() == 1 {
        let idx = &indices[0];
        let var_name = &idx.ident.text;
        let range = &idx.range;

        // Try to evaluate the range to get start and end values
        let (start, end) = try_evaluate_range(range, params)?;

        // Generate elements by substituting the index variable
        let mut elements = Vec::new();
        for i in start..=end {
            let substituted = substitute_variable(expr, var_name, i);
            elements.push(substituted);
        }

        Some(Expression::Array {
            elements,
            is_matrix: false,
        })
    } else {
        // Multi-index comprehension: expand nested
        // {expr for i in 1:n, j in 1:m} becomes
        // {{expr[i=1,j=1], expr[i=1,j=2], ...}, {expr[i=2,j=1], ...}, ...}
        let first_idx = &indices[0];
        let remaining_indices: Vec<ForIndex> = indices[1..].to_vec();

        let var_name = &first_idx.ident.text;
        let (start, end) = try_evaluate_range(&first_idx.range, params)?;

        let mut elements = Vec::new();
        for i in start..=end {
            // Substitute first variable
            let substituted_expr = substitute_variable(expr, var_name, i);
            // Recursively expand remaining indices
            if let Some(inner) =
                try_expand_comprehension(&substituted_expr, &remaining_indices, params)
            {
                elements.push(inner);
            } else {
                // Can't expand remaining - return nested comprehension
                elements.push(Expression::ArrayComprehension {
                    expr: Box::new(substituted_expr),
                    indices: remaining_indices.clone(),
                });
            }
        }

        Some(Expression::Array {
            elements,
            is_matrix: false,
        })
    }
}

/// Try to evaluate a range expression to get (start, end) integer values
fn try_evaluate_range(
    expr: &Expression,
    params: &IndexMap<String, Component>,
) -> Option<(i64, i64)> {
    match expr {
        Expression::Range { start, step, end } => {
            // For now, only handle simple ranges without step
            if step.is_some() {
                return None; // TODO: handle stepped ranges
            }
            let start_val = try_evaluate_integer(start, params, 0)?;
            let end_val = try_evaluate_integer(end, params, 0)?;
            Some((start_val, end_val))
        }
        _ => {
            // Single value - treat as 1:value range
            let val = try_evaluate_integer(expr, params, 0)?;
            Some((1, val))
        }
    }
}

/// Try to evaluate an expression as an integer
fn try_evaluate_integer(
    expr: &Expression,
    params: &IndexMap<String, Component>,
    depth: usize,
) -> Option<i64> {
    if depth > MAX_EVAL_DEPTH {
        return None;
    }

    match expr {
        Expression::Terminal { token, .. } => token.text.parse::<i64>().ok(),
        Expression::ComponentReference(cref) => {
            let name = cref.to_string();
            if let Some(param) = params.get(&name) {
                try_evaluate_integer(&param.start, params, depth + 1)
            } else {
                None
            }
        }
        Expression::Unary { op, rhs } => match op {
            OpUnary::Minus(_) => try_evaluate_integer(rhs, params, depth + 1).map(|v| -v),
            OpUnary::Plus(_) => try_evaluate_integer(rhs, params, depth + 1),
            _ => None,
        },
        Expression::Binary { lhs, op, rhs } => {
            let l = try_evaluate_integer(lhs, params, depth + 1)?;
            let r = try_evaluate_integer(rhs, params, depth + 1)?;
            match op {
                OpBinary::Add(_) => Some(l + r),
                OpBinary::Sub(_) => Some(l - r),
                OpBinary::Mul(_) => Some(l * r),
                OpBinary::Div(_) if r != 0 => Some(l / r),
                _ => None,
            }
        }
        Expression::Parenthesized { inner } => try_evaluate_integer(inner, params, depth + 1),
        Expression::FunctionCall { comp, args } => {
            let func_name = comp.to_string();
            if func_name == "size" && !args.is_empty() {
                // Handle size(array, dim) function
                if let Expression::ComponentReference(array_ref) = &args[0] {
                    let array_name = array_ref.to_string();
                    let dim_index = if args.len() >= 2 {
                        try_evaluate_integer(&args[1], params, depth + 1)? as usize
                    } else {
                        1
                    };

                    if let Some(array_comp) = params.get(&array_name) {
                        // Try evaluated shape first
                        if !array_comp.shape.is_empty()
                            && dim_index >= 1
                            && dim_index <= array_comp.shape.len()
                        {
                            return Some(array_comp.shape[dim_index - 1] as i64);
                        }
                        // Try shape_expr
                        if !array_comp.shape_expr.is_empty()
                            && dim_index >= 1
                            && dim_index <= array_comp.shape_expr.len()
                            && let crate::ir::ast::Subscript::Expression(expr) =
                                &array_comp.shape_expr[dim_index - 1]
                        {
                            return try_evaluate_integer(expr, params, depth + 1);
                        }
                        // Subscript::Range (`:`) can't be evaluated directly
                        // Try to infer from start expression (array literal)
                        if let Expression::Array { elements, .. } = &array_comp.start
                            && dim_index == 1
                        {
                            return Some(elements.len() as i64);
                        }
                    }
                }
            }
            None
        }
        _ => None,
    }
}

/// Substitute all occurrences of a variable with an integer value in an expression
fn substitute_variable(expr: &Expression, var_name: &str, value: i64) -> Expression {
    match expr {
        Expression::ComponentReference(cref) => {
            // Check if this is just the variable name
            if cref.parts.len() == 1 && cref.parts[0].ident.text == var_name {
                // Replace with integer literal
                Expression::Terminal {
                    terminal_type: TerminalType::UnsignedInteger,
                    token: Token {
                        text: value.to_string(),
                        ..Default::default()
                    },
                }
            } else {
                // Check subscripts for the variable
                let new_parts: Vec<ComponentRefPart> = cref
                    .parts
                    .iter()
                    .map(|part| {
                        let new_subs = part.subs.as_ref().map(|subs| {
                            subs.iter()
                                .map(|sub| substitute_in_subscript(sub, var_name, value))
                                .collect()
                        });
                        ComponentRefPart {
                            ident: part.ident.clone(),
                            subs: new_subs,
                        }
                    })
                    .collect();
                Expression::ComponentReference(ComponentReference {
                    local: cref.local,
                    parts: new_parts,
                })
            }
        }
        Expression::Binary { lhs, op, rhs } => Expression::Binary {
            op: op.clone(),
            lhs: Box::new(substitute_variable(lhs, var_name, value)),
            rhs: Box::new(substitute_variable(rhs, var_name, value)),
        },
        Expression::Unary { op, rhs } => Expression::Unary {
            op: op.clone(),
            rhs: Box::new(substitute_variable(rhs, var_name, value)),
        },
        Expression::FunctionCall { comp, args } => Expression::FunctionCall {
            comp: comp.clone(),
            args: args
                .iter()
                .map(|a| substitute_variable(a, var_name, value))
                .collect(),
        },
        Expression::Array {
            elements,
            is_matrix,
        } => Expression::Array {
            elements: elements
                .iter()
                .map(|e| substitute_variable(e, var_name, value))
                .collect(),
            is_matrix: *is_matrix,
        },
        Expression::Tuple { elements } => Expression::Tuple {
            elements: elements
                .iter()
                .map(|e| substitute_variable(e, var_name, value))
                .collect(),
        },
        Expression::If {
            branches,
            else_branch,
        } => Expression::If {
            branches: branches
                .iter()
                .map(|(c, t)| {
                    (
                        substitute_variable(c, var_name, value),
                        substitute_variable(t, var_name, value),
                    )
                })
                .collect(),
            else_branch: Box::new(substitute_variable(else_branch, var_name, value)),
        },
        Expression::Range { start, step, end } => Expression::Range {
            start: Box::new(substitute_variable(start, var_name, value)),
            step: step
                .as_ref()
                .map(|s| Box::new(substitute_variable(s, var_name, value))),
            end: Box::new(substitute_variable(end, var_name, value)),
        },
        Expression::Parenthesized { inner } => Expression::Parenthesized {
            inner: Box::new(substitute_variable(inner, var_name, value)),
        },
        Expression::ArrayComprehension {
            expr: inner,
            indices,
        } => {
            // Check if the variable is shadowed by the comprehension's own indices
            let is_shadowed = indices.iter().any(|idx| idx.ident.text == var_name);
            if is_shadowed {
                // Don't substitute - the variable is local to this comprehension
                expr.clone()
            } else {
                Expression::ArrayComprehension {
                    expr: Box::new(substitute_variable(inner, var_name, value)),
                    indices: indices
                        .iter()
                        .map(|idx| ForIndex {
                            ident: idx.ident.clone(),
                            range: substitute_variable(&idx.range, var_name, value),
                        })
                        .collect(),
                }
            }
        }
        Expression::Terminal { .. } | Expression::Empty => expr.clone(),
    }
}

/// Substitute a variable in a subscript
fn substitute_in_subscript(sub: &Subscript, var_name: &str, value: i64) -> Subscript {
    match sub {
        Subscript::Expression(expr) => {
            Subscript::Expression(substitute_variable(expr, var_name, value))
        }
        Subscript::Range { token } => Subscript::Range {
            token: token.clone(),
        },
        Subscript::Empty => Subscript::Empty,
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_substitute_simple() {
        // Test substituting i with 5 in expression "i"
        let expr = Expression::ComponentReference(ComponentReference {
            local: false,
            parts: vec![ComponentRefPart {
                ident: Token {
                    text: "i".to_string(),
                    ..Default::default()
                },
                subs: None,
            }],
        });

        let result = substitute_variable(&expr, "i", 5);

        match result {
            Expression::Terminal { token, .. } => {
                assert_eq!(token.text, "5");
            }
            _ => panic!("Expected Terminal expression"),
        }
    }

    #[test]
    fn test_evaluate_simple_range() {
        let params = IndexMap::new();
        let range = Expression::Range {
            start: Box::new(Expression::Terminal {
                terminal_type: TerminalType::UnsignedInteger,
                token: Token {
                    text: "1".to_string(),
                    ..Default::default()
                },
            }),
            step: None,
            end: Box::new(Expression::Terminal {
                terminal_type: TerminalType::UnsignedInteger,
                token: Token {
                    text: "3".to_string(),
                    ..Default::default()
                },
            }),
        };

        let result = try_evaluate_range(&range, &params);
        assert_eq!(result, Some((1, 3)));
    }
}
