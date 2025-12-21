//! Operator record expansion
//!
//! This module expands operator record arithmetic (like Complex +, -, *, /)
//! into field-wise operations. For Complex numbers:
//!   c = a + b  becomes  c.re = a.re + b.re; c.im = a.im + b.im
//!   c = a * b  becomes  c.re = a.re*b.re - a.im*b.im; c.im = a.re*b.im + a.im*b.re

use crate::ir::ast::{
    ClassDefinition, ClassType, ComponentRefPart, ComponentReference, Equation, Expression,
    OpBinary, Subscript, Token,
};
use indexmap::IndexMap;

/// Information about an operator record type
#[derive(Debug, Clone)]
pub struct OperatorRecordInfo {
    /// The record's field names (in order)
    pub fields: Vec<String>,
    /// Whether this is specifically the Complex type
    pub is_complex: bool,
}

/// Build a map of operator record types from the class dictionary
///
/// This always includes the built-in Complex type with `re` and `im` fields,
/// even if it's not explicitly defined in the class dictionary.
pub fn build_operator_record_map(
    class_dict: &IndexMap<String, ClassDefinition>,
) -> IndexMap<String, OperatorRecordInfo> {
    let mut result = IndexMap::new();

    // Always include the built-in Complex type
    result.insert(
        "Complex".to_string(),
        OperatorRecordInfo {
            fields: vec!["re".to_string(), "im".to_string()],
            is_complex: true,
        },
    );

    for (name, class) in class_dict {
        // Check if this is an operator record
        if matches!(class.class_type, ClassType::Record) {
            // Collect field names
            let fields: Vec<String> = class.components.keys().cloned().collect();

            // Check if this looks like Complex (has re and im fields)
            let is_complex =
                fields.contains(&"re".to_string()) && fields.contains(&"im".to_string());

            if !fields.is_empty() {
                result.insert(name.clone(), OperatorRecordInfo { fields, is_complex });
            }
        }
    }

    result
}

/// Build a map from variable name to its type name
///
/// This function detects Complex variables by checking if both `.re` and `.im`
/// components exist in the flattened class. After flattening, a Complex variable
/// `y` becomes `y.re` and `y.im`, so we infer that `y` is Complex.
pub fn build_type_map(
    flat_class: &ClassDefinition,
    _class_dict: &IndexMap<String, ClassDefinition>,
) -> IndexMap<String, String> {
    let mut result = IndexMap::new();
    let mut complex_candidates: std::collections::HashSet<String> =
        std::collections::HashSet::new();

    // First pass: add all components and collect potential Complex variables
    for (comp_name, comp) in &flat_class.components {
        let type_name = comp.type_name.to_string();
        result.insert(comp_name.clone(), type_name);

        // If this is a `.re` or `.im` component, record the parent as a candidate
        if let Some(parent) = comp_name.strip_suffix(".re") {
            complex_candidates.insert(parent.to_string());
        }
        if let Some(parent) = comp_name.strip_suffix(".im") {
            complex_candidates.insert(parent.to_string());
        }
    }

    // Second pass: check candidates - if both .re and .im exist, mark as Complex
    for candidate in complex_candidates {
        let re_name = format!("{}.re", candidate);
        let im_name = format!("{}.im", candidate);
        if result.contains_key(&re_name) && result.contains_key(&im_name) {
            result.insert(candidate, "Complex".to_string());
        }
    }

    result
}

/// Expand Complex arithmetic in equations
///
/// Given an equation like `c = a + b` where a, b, c are Complex,
/// this expands it into:
///   c.re = a.re + b.re
///   c.im = a.im + b.im
pub fn expand_complex_equations(
    equations: &[Equation],
    type_map: &IndexMap<String, String>,
    operator_records: &IndexMap<String, OperatorRecordInfo>,
) -> Vec<Equation> {
    expand_complex_equations_impl(equations, type_map, operator_records, false)
}

/// Verbose version for debugging
pub fn expand_complex_equations_verbose(
    equations: &[Equation],
    type_map: &IndexMap<String, String>,
    operator_records: &IndexMap<String, OperatorRecordInfo>,
) -> Vec<Equation> {
    expand_complex_equations_impl(equations, type_map, operator_records, true)
}

fn expand_complex_equations_impl(
    equations: &[Equation],
    type_map: &IndexMap<String, String>,
    operator_records: &IndexMap<String, OperatorRecordInfo>,
    verbose: bool,
) -> Vec<Equation> {
    let mut result = Vec::new();

    for eq in equations {
        if verbose {
            eprintln!("Processing equation: {:?}", eq);
        }
        match expand_single_equation(eq, type_map, operator_records) {
            Some(expanded) => {
                if verbose {
                    eprintln!("  -> Expanded to {} equations", expanded.len());
                }
                result.extend(expanded);
            }
            None => {
                if verbose {
                    eprintln!("  -> Not expanded");
                }
                result.push(eq.clone());
            }
        }
    }

    result
}

/// Extract the base name from a component reference (without subscripts)
fn get_base_name(comp_ref: &ComponentReference) -> String {
    comp_ref
        .parts
        .iter()
        .map(|p| p.ident.text.clone())
        .collect::<Vec<_>>()
        .join(".")
}

/// Get subscripts from the last part of a component reference
fn get_subscripts(comp_ref: &ComponentReference) -> Option<Vec<Subscript>> {
    comp_ref.parts.last().and_then(|p| p.subs.clone())
}

/// Try to expand a single equation if it involves Complex arithmetic
fn expand_single_equation(
    eq: &Equation,
    type_map: &IndexMap<String, String>,
    operator_records: &IndexMap<String, OperatorRecordInfo>,
) -> Option<Vec<Equation>> {
    // Check if this is a simple equation with a Complex variable on the LHS
    let Equation::Simple {
        lhs: Expression::ComponentReference(lhs_ref),
        rhs,
    } = eq
    else {
        return None;
    };

    // Get the base name without subscripts
    let base_name = get_base_name(lhs_ref);
    let subscripts = get_subscripts(lhs_ref);

    // Try looking up with the full name first (e.g., "y[1]"), then the base name
    let lhs_name = lhs_ref.to_string();
    let lhs_type = type_map
        .get(&lhs_name)
        .or_else(|| type_map.get(&base_name))?;
    let record_info = operator_records.get(lhs_type)?;

    if record_info.is_complex {
        // This is a Complex assignment, try to expand the RHS
        expand_complex_assignment(
            &base_name,
            subscripts.as_ref(),
            rhs,
            type_map,
            operator_records,
        )
    } else {
        None
    }
}

/// Expand a Complex assignment into field-wise equations
fn expand_complex_assignment(
    lhs_name: &str,
    subscripts: Option<&Vec<Subscript>>,
    rhs: &Expression,
    type_map: &IndexMap<String, String>,
    operator_records: &IndexMap<String, OperatorRecordInfo>,
) -> Option<Vec<Equation>> {
    // Check if the RHS involves Complex values
    if !expr_involves_complex(rhs, type_map, operator_records) {
        return None;
    }

    // Recursively transform the RHS expression to extract .re and .im parts
    let re_expr = transform_to_real_part(rhs, subscripts, type_map, operator_records)?;
    let im_expr = transform_to_imag_part(rhs, subscripts, type_map, operator_records)?;

    Some(vec![
        Equation::Simple {
            lhs: make_field_ref(lhs_name, "re", subscripts),
            rhs: re_expr,
        },
        Equation::Simple {
            lhs: make_field_ref(lhs_name, "im", subscripts),
            rhs: im_expr,
        },
    ])
}

/// Check if a function name returns Complex values
fn is_complex_function(name: &str) -> bool {
    // Complex constructor
    if name == "Complex" {
        return true;
    }
    // Modelica.ComplexMath functions that return Complex
    if name.contains("ComplexMath") {
        // Most ComplexMath functions return Complex (conj, exp, log, sin, cos, etc.)
        // Functions like abs, arg, real, imag return Real, not Complex
        let func_name = name.rsplit('.').next().unwrap_or(name);
        !matches!(
            func_name,
            "abs" | "arg" | "real" | "imag" | "'abs'" | "'arg'"
        )
    } else {
        false
    }
}

/// Check if an expression involves Complex values (recursively)
fn expr_involves_complex(
    expr: &Expression,
    type_map: &IndexMap<String, String>,
    operator_records: &IndexMap<String, OperatorRecordInfo>,
) -> bool {
    match expr {
        Expression::ComponentReference(comp) => {
            let name = comp.to_string();
            let base_name = get_base_name(comp);
            if let Some(type_name) = type_map.get(&name).or_else(|| type_map.get(&base_name)) {
                return operator_records
                    .get(type_name)
                    .is_some_and(|r| r.is_complex);
            }
            false
        }
        Expression::FunctionCall { comp, args } => {
            let func_name = comp.to_string();
            // Check if function returns Complex
            if is_complex_function(&func_name) {
                return true;
            }
            // Otherwise check if any argument involves Complex
            args.iter()
                .any(|arg| expr_involves_complex(arg, type_map, operator_records))
        }
        Expression::Binary { lhs, rhs, .. } => {
            expr_involves_complex(lhs, type_map, operator_records)
                || expr_involves_complex(rhs, type_map, operator_records)
        }
        Expression::Unary { rhs, .. } => expr_involves_complex(rhs, type_map, operator_records),
        Expression::If {
            branches,
            else_branch,
        } => {
            // Check if any branch expression involves Complex
            branches
                .iter()
                .any(|(_, expr)| expr_involves_complex(expr, type_map, operator_records))
                || expr_involves_complex(else_branch, type_map, operator_records)
        }
        Expression::Parenthesized { inner } => {
            expr_involves_complex(inner, type_map, operator_records)
        }
        _ => false,
    }
}

/// Transform an expression to get its real part
fn transform_to_real_part(
    expr: &Expression,
    subscripts: Option<&Vec<Subscript>>,
    type_map: &IndexMap<String, String>,
    operator_records: &IndexMap<String, OperatorRecordInfo>,
) -> Option<Expression> {
    match expr {
        Expression::ComponentReference(comp) => {
            // Get the base name and check if it's Complex
            let base_name = get_base_name(comp);
            let full_name = comp.to_string();
            let comp_subs = get_subscripts(comp);

            if let Some(type_name) = type_map
                .get(&full_name)
                .or_else(|| type_map.get(&base_name))
                && operator_records
                    .get(type_name)
                    .is_some_and(|r| r.is_complex)
            {
                // Use the component's own subscripts if it has them
                Some(make_field_ref(&base_name, "re", comp_subs.as_ref()))
            } else {
                Some(expr.clone())
            }
        }
        Expression::FunctionCall { comp, args } => {
            let func_name = comp.to_string();
            if func_name == "Complex" {
                // Complex(re, im) -> re
                Some(args.first().cloned().unwrap_or(make_zero()))
            } else if is_complex_function(&func_name) {
                // Handle ComplexMath functions
                let short_name = func_name.rsplit('.').next().unwrap_or(&func_name);
                match short_name {
                    "conj" => {
                        // conj(z).re = z.re
                        if let Some(arg) = args.first() {
                            transform_to_real_part(arg, subscripts, type_map, operator_records)
                        } else {
                            Some(make_zero())
                        }
                    }
                    _ => {
                        // For other Complex-returning functions, we can't easily expand
                        // Just return the expression as-is (will fail to fully expand)
                        Some(expr.clone())
                    }
                }
            } else {
                Some(expr.clone())
            }
        }
        Expression::Binary { op, lhs, rhs } => {
            match op {
                OpBinary::Add(_) | OpBinary::Sub(_) => {
                    // (a + b).re = a.re + b.re
                    let lhs_re =
                        transform_to_real_part(lhs, subscripts, type_map, operator_records)?;
                    let rhs_re =
                        transform_to_real_part(rhs, subscripts, type_map, operator_records)?;
                    Some(Expression::Binary {
                        op: op.clone(),
                        lhs: Box::new(lhs_re),
                        rhs: Box::new(rhs_re),
                    })
                }
                OpBinary::Mul(_) => {
                    // Check if one operand is Real (scalar multiplication)
                    let lhs_is_complex = expr_involves_complex(lhs, type_map, operator_records);
                    let rhs_is_complex = expr_involves_complex(rhs, type_map, operator_records);

                    if lhs_is_complex && rhs_is_complex {
                        // (a * b).re = a.re*b.re - a.im*b.im
                        let a_re =
                            transform_to_real_part(lhs, subscripts, type_map, operator_records)?;
                        let a_im =
                            transform_to_imag_part(lhs, subscripts, type_map, operator_records)?;
                        let b_re =
                            transform_to_real_part(rhs, subscripts, type_map, operator_records)?;
                        let b_im =
                            transform_to_imag_part(rhs, subscripts, type_map, operator_records)?;
                        Some(Expression::Binary {
                            op: OpBinary::Sub(Token::default()),
                            lhs: Box::new(Expression::Binary {
                                op: OpBinary::Mul(Token::default()),
                                lhs: Box::new(a_re),
                                rhs: Box::new(b_re),
                            }),
                            rhs: Box::new(Expression::Binary {
                                op: OpBinary::Mul(Token::default()),
                                lhs: Box::new(a_im),
                                rhs: Box::new(b_im),
                            }),
                        })
                    } else if lhs_is_complex {
                        // scalar * complex: (k * c).re = k * c.re
                        let c_re =
                            transform_to_real_part(lhs, subscripts, type_map, operator_records)?;
                        Some(Expression::Binary {
                            op: OpBinary::Mul(Token::default()),
                            lhs: rhs.clone(),
                            rhs: Box::new(c_re),
                        })
                    } else {
                        // complex * scalar: (c * k).re = c.re * k
                        let c_re =
                            transform_to_real_part(rhs, subscripts, type_map, operator_records)?;
                        Some(Expression::Binary {
                            op: OpBinary::Mul(Token::default()),
                            lhs: lhs.clone(),
                            rhs: Box::new(c_re),
                        })
                    }
                }
                OpBinary::Div(_) => {
                    // (a / b).re = (a.re*b.re + a.im*b.im) / (b.re^2 + b.im^2)
                    let a_re = transform_to_real_part(lhs, subscripts, type_map, operator_records)?;
                    let a_im = transform_to_imag_part(lhs, subscripts, type_map, operator_records)?;
                    let b_re = transform_to_real_part(rhs, subscripts, type_map, operator_records)?;
                    let b_im = transform_to_imag_part(rhs, subscripts, type_map, operator_records)?;
                    let num = Expression::Binary {
                        op: OpBinary::Add(Token::default()),
                        lhs: Box::new(Expression::Binary {
                            op: OpBinary::Mul(Token::default()),
                            lhs: Box::new(a_re),
                            rhs: Box::new(b_re.clone()),
                        }),
                        rhs: Box::new(Expression::Binary {
                            op: OpBinary::Mul(Token::default()),
                            lhs: Box::new(a_im),
                            rhs: Box::new(b_im.clone()),
                        }),
                    };
                    let denom = Expression::Binary {
                        op: OpBinary::Add(Token::default()),
                        lhs: Box::new(Expression::Binary {
                            op: OpBinary::Mul(Token::default()),
                            lhs: Box::new(b_re.clone()),
                            rhs: Box::new(b_re),
                        }),
                        rhs: Box::new(Expression::Binary {
                            op: OpBinary::Mul(Token::default()),
                            lhs: Box::new(b_im.clone()),
                            rhs: Box::new(b_im),
                        }),
                    };
                    Some(Expression::Binary {
                        op: OpBinary::Div(Token::default()),
                        lhs: Box::new(num),
                        rhs: Box::new(denom),
                    })
                }
                _ => Some(expr.clone()),
            }
        }
        Expression::Unary { op, rhs } => {
            let inner_re = transform_to_real_part(rhs, subscripts, type_map, operator_records)?;
            Some(Expression::Unary {
                op: op.clone(),
                rhs: Box::new(inner_re),
            })
        }
        Expression::If {
            branches,
            else_branch,
        } => {
            // Transform each branch: if cond then expr.re elseif ... else expr.re
            let transformed_branches: Option<Vec<_>> = branches
                .iter()
                .map(|(cond, branch_expr)| {
                    transform_to_real_part(branch_expr, subscripts, type_map, operator_records)
                        .map(|transformed| (cond.clone(), transformed))
                })
                .collect();
            let transformed_else =
                transform_to_real_part(else_branch, subscripts, type_map, operator_records)?;
            Some(Expression::If {
                branches: transformed_branches?,
                else_branch: Box::new(transformed_else),
            })
        }
        Expression::Parenthesized { inner } => {
            let inner_re = transform_to_real_part(inner, subscripts, type_map, operator_records)?;
            Some(Expression::Parenthesized {
                inner: Box::new(inner_re),
            })
        }
        _ => Some(expr.clone()),
    }
}

/// Transform an expression to get its imaginary part
fn transform_to_imag_part(
    expr: &Expression,
    subscripts: Option<&Vec<Subscript>>,
    type_map: &IndexMap<String, String>,
    operator_records: &IndexMap<String, OperatorRecordInfo>,
) -> Option<Expression> {
    match expr {
        Expression::ComponentReference(comp) => {
            // Get the base name and check if it's Complex
            let base_name = get_base_name(comp);
            let full_name = comp.to_string();
            let comp_subs = get_subscripts(comp);

            if let Some(type_name) = type_map
                .get(&full_name)
                .or_else(|| type_map.get(&base_name))
                && operator_records
                    .get(type_name)
                    .is_some_and(|r| r.is_complex)
            {
                // Use the component's own subscripts if it has them
                Some(make_field_ref(&base_name, "im", comp_subs.as_ref()))
            } else {
                Some(make_zero()) // Real values have im = 0
            }
        }
        Expression::FunctionCall { comp, args } => {
            let func_name = comp.to_string();
            if func_name == "Complex" {
                // Complex(re, im) -> im
                Some(args.get(1).cloned().unwrap_or(make_zero()))
            } else if is_complex_function(&func_name) {
                // Handle ComplexMath functions
                let short_name = func_name.rsplit('.').next().unwrap_or(&func_name);
                match short_name {
                    "conj" => {
                        // conj(z).im = -z.im
                        if let Some(arg) = args.first() {
                            let inner_im = transform_to_imag_part(
                                arg,
                                subscripts,
                                type_map,
                                operator_records,
                            )?;
                            Some(Expression::Unary {
                                op: crate::ir::ast::OpUnary::Minus(Token::default()),
                                rhs: Box::new(inner_im),
                            })
                        } else {
                            Some(make_zero())
                        }
                    }
                    _ => {
                        // For other Complex-returning functions, we can't easily expand
                        Some(make_zero())
                    }
                }
            } else {
                Some(make_zero())
            }
        }
        Expression::Binary { op, lhs, rhs } => {
            match op {
                OpBinary::Add(_) | OpBinary::Sub(_) => {
                    // (a + b).im = a.im + b.im
                    let lhs_im =
                        transform_to_imag_part(lhs, subscripts, type_map, operator_records)?;
                    let rhs_im =
                        transform_to_imag_part(rhs, subscripts, type_map, operator_records)?;
                    Some(Expression::Binary {
                        op: op.clone(),
                        lhs: Box::new(lhs_im),
                        rhs: Box::new(rhs_im),
                    })
                }
                OpBinary::Mul(_) => {
                    // Check if one operand is Real (scalar multiplication)
                    let lhs_is_complex = expr_involves_complex(lhs, type_map, operator_records);
                    let rhs_is_complex = expr_involves_complex(rhs, type_map, operator_records);

                    if lhs_is_complex && rhs_is_complex {
                        // (a * b).im = a.re*b.im + a.im*b.re
                        let a_re =
                            transform_to_real_part(lhs, subscripts, type_map, operator_records)?;
                        let a_im =
                            transform_to_imag_part(lhs, subscripts, type_map, operator_records)?;
                        let b_re =
                            transform_to_real_part(rhs, subscripts, type_map, operator_records)?;
                        let b_im =
                            transform_to_imag_part(rhs, subscripts, type_map, operator_records)?;
                        Some(Expression::Binary {
                            op: OpBinary::Add(Token::default()),
                            lhs: Box::new(Expression::Binary {
                                op: OpBinary::Mul(Token::default()),
                                lhs: Box::new(a_re),
                                rhs: Box::new(b_im),
                            }),
                            rhs: Box::new(Expression::Binary {
                                op: OpBinary::Mul(Token::default()),
                                lhs: Box::new(a_im),
                                rhs: Box::new(b_re),
                            }),
                        })
                    } else if lhs_is_complex {
                        // scalar * complex: (k * c).im = k * c.im
                        let c_im =
                            transform_to_imag_part(lhs, subscripts, type_map, operator_records)?;
                        Some(Expression::Binary {
                            op: OpBinary::Mul(Token::default()),
                            lhs: rhs.clone(),
                            rhs: Box::new(c_im),
                        })
                    } else {
                        // complex * scalar: (c * k).im = c.im * k
                        let c_im =
                            transform_to_imag_part(rhs, subscripts, type_map, operator_records)?;
                        Some(Expression::Binary {
                            op: OpBinary::Mul(Token::default()),
                            lhs: lhs.clone(),
                            rhs: Box::new(c_im),
                        })
                    }
                }
                OpBinary::Div(_) => {
                    // (a / b).im = (a.im*b.re - a.re*b.im) / (b.re^2 + b.im^2)
                    let a_re = transform_to_real_part(lhs, subscripts, type_map, operator_records)?;
                    let a_im = transform_to_imag_part(lhs, subscripts, type_map, operator_records)?;
                    let b_re = transform_to_real_part(rhs, subscripts, type_map, operator_records)?;
                    let b_im = transform_to_imag_part(rhs, subscripts, type_map, operator_records)?;
                    let num = Expression::Binary {
                        op: OpBinary::Sub(Token::default()),
                        lhs: Box::new(Expression::Binary {
                            op: OpBinary::Mul(Token::default()),
                            lhs: Box::new(a_im),
                            rhs: Box::new(b_re.clone()),
                        }),
                        rhs: Box::new(Expression::Binary {
                            op: OpBinary::Mul(Token::default()),
                            lhs: Box::new(a_re),
                            rhs: Box::new(b_im.clone()),
                        }),
                    };
                    let denom = Expression::Binary {
                        op: OpBinary::Add(Token::default()),
                        lhs: Box::new(Expression::Binary {
                            op: OpBinary::Mul(Token::default()),
                            lhs: Box::new(b_re.clone()),
                            rhs: Box::new(b_re),
                        }),
                        rhs: Box::new(Expression::Binary {
                            op: OpBinary::Mul(Token::default()),
                            lhs: Box::new(b_im.clone()),
                            rhs: Box::new(b_im),
                        }),
                    };
                    Some(Expression::Binary {
                        op: OpBinary::Div(Token::default()),
                        lhs: Box::new(num),
                        rhs: Box::new(denom),
                    })
                }
                _ => Some(make_zero()),
            }
        }
        Expression::Unary { op, rhs } => {
            let inner_im = transform_to_imag_part(rhs, subscripts, type_map, operator_records)?;
            Some(Expression::Unary {
                op: op.clone(),
                rhs: Box::new(inner_im),
            })
        }
        Expression::If {
            branches,
            else_branch,
        } => {
            // Transform each branch: if cond then expr.im elseif ... else expr.im
            let transformed_branches: Option<Vec<_>> = branches
                .iter()
                .map(|(cond, branch_expr)| {
                    transform_to_imag_part(branch_expr, subscripts, type_map, operator_records)
                        .map(|transformed| (cond.clone(), transformed))
                })
                .collect();
            let transformed_else =
                transform_to_imag_part(else_branch, subscripts, type_map, operator_records)?;
            Some(Expression::If {
                branches: transformed_branches?,
                else_branch: Box::new(transformed_else),
            })
        }
        Expression::Parenthesized { inner } => {
            let inner_im = transform_to_imag_part(inner, subscripts, type_map, operator_records)?;
            Some(Expression::Parenthesized {
                inner: Box::new(inner_im),
            })
        }
        _ => Some(make_zero()),
    }
}

/// Create a field reference expression: name.field or name.field[subs]
fn make_field_ref(name: &str, field: &str, subscripts: Option<&Vec<Subscript>>) -> Expression {
    Expression::ComponentReference(ComponentReference {
        local: false,
        parts: vec![ComponentRefPart {
            ident: Token {
                text: format!("{}.{}", name, field),
                ..Default::default()
            },
            subs: subscripts.cloned(),
        }],
    })
}

/// Create a zero expression
fn make_zero() -> Expression {
    Expression::Terminal {
        terminal_type: crate::ir::ast::TerminalType::UnsignedReal,
        token: Token {
            text: "0".to_string(),
            ..Default::default()
        },
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_complex_detection() {
        let mut class_dict = IndexMap::new();

        // Create a Complex-like record
        let mut complex_class = ClassDefinition {
            class_type: ClassType::Record,
            ..Default::default()
        };
        complex_class.components.insert(
            "re".to_string(),
            crate::ir::ast::Component {
                name: "re".to_string(),
                ..Default::default()
            },
        );
        complex_class.components.insert(
            "im".to_string(),
            crate::ir::ast::Component {
                name: "im".to_string(),
                ..Default::default()
            },
        );

        class_dict.insert("Complex".to_string(), complex_class);

        let op_records = build_operator_record_map(&class_dict);

        assert!(op_records.contains_key("Complex"));
        assert!(op_records["Complex"].is_complex);
        assert_eq!(op_records["Complex"].fields.len(), 2);
    }
}
