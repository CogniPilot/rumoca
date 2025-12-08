//! Operator record expansion
//!
//! This module expands operator record arithmetic (like Complex +, -, *, /)
//! into field-wise operations. For Complex numbers:
//!   c = a + b  becomes  c.re = a.re + b.re; c.im = a.im + b.im
//!   c = a * b  becomes  c.re = a.re*b.re - a.im*b.im; c.im = a.re*b.im + a.im*b.re

use crate::ir::ast::{
    ClassDefinition, ClassType, ComponentRefPart, ComponentReference, Equation, Expression,
    OpBinary, Token,
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
pub fn build_operator_record_map(
    class_dict: &IndexMap<String, ClassDefinition>,
) -> IndexMap<String, OperatorRecordInfo> {
    let mut result = IndexMap::new();

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
pub fn build_type_map(
    flat_class: &ClassDefinition,
    class_dict: &IndexMap<String, ClassDefinition>,
) -> IndexMap<String, String> {
    let mut result = IndexMap::new();

    for (comp_name, comp) in &flat_class.components {
        let type_name = comp.type_name.to_string();
        result.insert(comp_name.clone(), type_name.clone());

        // For nested components (like a.re from Complex a), also check
        // if the parent type is in the class dictionary
        if let Some(class_def) = class_dict.get(&type_name) {
            for field_name in class_def.components.keys() {
                let full_name = format!("{}.{}", comp_name, field_name);
                result.insert(full_name, "Real".to_string());
            }
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
    let mut result = Vec::new();

    for eq in equations {
        match expand_single_equation(eq, type_map, operator_records) {
            Some(expanded) => result.extend(expanded),
            None => result.push(eq.clone()),
        }
    }

    result
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

    let lhs_name = lhs_ref.to_string();
    let lhs_type = type_map.get(&lhs_name)?;
    let record_info = operator_records.get(lhs_type)?;

    if record_info.is_complex {
        // This is a Complex assignment, try to expand the RHS
        expand_complex_assignment(&lhs_name, rhs, type_map, operator_records)
    } else {
        None
    }
}

/// Expand a Complex assignment into field-wise equations
fn expand_complex_assignment(
    lhs_name: &str,
    rhs: &Expression,
    type_map: &IndexMap<String, String>,
    operator_records: &IndexMap<String, OperatorRecordInfo>,
) -> Option<Vec<Equation>> {
    // Generate .re and .im equations based on the RHS expression type
    match rhs {
        Expression::Binary {
            op,
            lhs: bin_lhs,
            rhs: bin_rhs,
        } => {
            // Check if operands are Complex
            let lhs_is_complex = is_complex_expr(bin_lhs, type_map, operator_records);
            let rhs_is_complex = is_complex_expr(bin_rhs, type_map, operator_records);

            if lhs_is_complex && rhs_is_complex {
                return expand_complex_binary_op(lhs_name, op, bin_lhs, bin_rhs);
            }
        }
        Expression::ComponentReference(ref_comp) => {
            // Simple assignment: c = a (where both are Complex)
            let ref_name = ref_comp.to_string();
            if let Some(ref_type) = type_map.get(&ref_name)
                && operator_records.get(ref_type).is_some_and(|r| r.is_complex)
            {
                // c = a => c.re = a.re; c.im = a.im
                return Some(vec![
                    make_field_eq(lhs_name, "re", &ref_name, "re"),
                    make_field_eq(lhs_name, "im", &ref_name, "im"),
                ]);
            }
        }
        Expression::FunctionCall { comp, args } => {
            // Handle Complex constructor: c = Complex(1, 2)
            let func_name = comp.to_string();
            if func_name == "Complex" && !args.is_empty() {
                let re_expr = args.first().cloned().unwrap_or(Expression::Empty);
                let im_expr = args.get(1).cloned().unwrap_or(make_zero());
                return Some(vec![
                    Equation::Simple {
                        lhs: make_field_ref(lhs_name, "re"),
                        rhs: re_expr,
                    },
                    Equation::Simple {
                        lhs: make_field_ref(lhs_name, "im"),
                        rhs: im_expr,
                    },
                ]);
            }
        }
        _ => {}
    }
    None
}

/// Check if an expression has Complex type
fn is_complex_expr(
    expr: &Expression,
    type_map: &IndexMap<String, String>,
    operator_records: &IndexMap<String, OperatorRecordInfo>,
) -> bool {
    match expr {
        Expression::ComponentReference(comp) => {
            let name = comp.to_string();
            if let Some(type_name) = type_map.get(&name) {
                return operator_records
                    .get(type_name)
                    .is_some_and(|r| r.is_complex);
            }
            false
        }
        Expression::FunctionCall { comp, .. } => comp.to_string() == "Complex",
        _ => false,
    }
}

/// Expand a binary operation on Complex numbers
fn expand_complex_binary_op(
    result_name: &str,
    op: &OpBinary,
    lhs: &Expression,
    rhs: &Expression,
) -> Option<Vec<Equation>> {
    let lhs_name = expr_to_name(lhs)?;
    let rhs_name = expr_to_name(rhs)?;

    match op {
        OpBinary::Add(_) => {
            // c = a + b => c.re = a.re + b.re; c.im = a.im + b.im
            Some(vec![
                make_binary_field_eq(result_name, "re", &lhs_name, "re", op, &rhs_name, "re"),
                make_binary_field_eq(result_name, "im", &lhs_name, "im", op, &rhs_name, "im"),
            ])
        }
        OpBinary::Sub(_) => {
            // c = a - b => c.re = a.re - b.re; c.im = a.im - b.im
            Some(vec![
                make_binary_field_eq(result_name, "re", &lhs_name, "re", op, &rhs_name, "re"),
                make_binary_field_eq(result_name, "im", &lhs_name, "im", op, &rhs_name, "im"),
            ])
        }
        OpBinary::Mul(_) => {
            // c = a * b => c.re = a.re*b.re - a.im*b.im; c.im = a.re*b.im + a.im*b.re
            Some(vec![
                make_complex_mul_re_eq(result_name, &lhs_name, &rhs_name),
                make_complex_mul_im_eq(result_name, &lhs_name, &rhs_name),
            ])
        }
        OpBinary::Div(_) => {
            // c = a / b =>
            // denom = b.re^2 + b.im^2
            // c.re = (a.re*b.re + a.im*b.im) / denom
            // c.im = (a.im*b.re - a.re*b.im) / denom
            Some(vec![
                make_complex_div_re_eq(result_name, &lhs_name, &rhs_name),
                make_complex_div_im_eq(result_name, &lhs_name, &rhs_name),
            ])
        }
        _ => None,
    }
}

/// Extract variable name from a simple component reference expression
fn expr_to_name(expr: &Expression) -> Option<String> {
    match expr {
        Expression::ComponentReference(comp) => Some(comp.to_string()),
        _ => None,
    }
}

/// Create a field reference expression: name.field
fn make_field_ref(name: &str, field: &str) -> Expression {
    Expression::ComponentReference(ComponentReference {
        local: false,
        parts: vec![ComponentRefPart {
            ident: Token {
                text: format!("{}.{}", name, field),
                ..Default::default()
            },
            subs: None,
        }],
    })
}

/// Create a simple field equation: lhs.field = rhs.field
fn make_field_eq(lhs_name: &str, lhs_field: &str, rhs_name: &str, rhs_field: &str) -> Equation {
    Equation::Simple {
        lhs: make_field_ref(lhs_name, lhs_field),
        rhs: make_field_ref(rhs_name, rhs_field),
    }
}

/// Create a binary operation field equation: result.field = lhs.field op rhs.field
fn make_binary_field_eq(
    result_name: &str,
    result_field: &str,
    lhs_name: &str,
    lhs_field: &str,
    op: &OpBinary,
    rhs_name: &str,
    rhs_field: &str,
) -> Equation {
    Equation::Simple {
        lhs: make_field_ref(result_name, result_field),
        rhs: Expression::Binary {
            op: op.clone(),
            lhs: Box::new(make_field_ref(lhs_name, lhs_field)),
            rhs: Box::new(make_field_ref(rhs_name, rhs_field)),
        },
    }
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

/// Complex multiplication: c.re = a.re*b.re - a.im*b.im
fn make_complex_mul_re_eq(result: &str, a: &str, b: &str) -> Equation {
    // a.re*b.re
    let term1 = Expression::Binary {
        op: OpBinary::Mul(Token::default()),
        lhs: Box::new(make_field_ref(a, "re")),
        rhs: Box::new(make_field_ref(b, "re")),
    };
    // a.im*b.im
    let term2 = Expression::Binary {
        op: OpBinary::Mul(Token::default()),
        lhs: Box::new(make_field_ref(a, "im")),
        rhs: Box::new(make_field_ref(b, "im")),
    };
    // term1 - term2
    Equation::Simple {
        lhs: make_field_ref(result, "re"),
        rhs: Expression::Binary {
            op: OpBinary::Sub(Token::default()),
            lhs: Box::new(term1),
            rhs: Box::new(term2),
        },
    }
}

/// Complex multiplication: c.im = a.re*b.im + a.im*b.re
fn make_complex_mul_im_eq(result: &str, a: &str, b: &str) -> Equation {
    // a.re*b.im
    let term1 = Expression::Binary {
        op: OpBinary::Mul(Token::default()),
        lhs: Box::new(make_field_ref(a, "re")),
        rhs: Box::new(make_field_ref(b, "im")),
    };
    // a.im*b.re
    let term2 = Expression::Binary {
        op: OpBinary::Mul(Token::default()),
        lhs: Box::new(make_field_ref(a, "im")),
        rhs: Box::new(make_field_ref(b, "re")),
    };
    // term1 + term2
    Equation::Simple {
        lhs: make_field_ref(result, "im"),
        rhs: Expression::Binary {
            op: OpBinary::Add(Token::default()),
            lhs: Box::new(term1),
            rhs: Box::new(term2),
        },
    }
}

/// Complex division: c.re = (a.re*b.re + a.im*b.im) / (b.re^2 + b.im^2)
fn make_complex_div_re_eq(result: &str, a: &str, b: &str) -> Equation {
    // Numerator: a.re*b.re + a.im*b.im
    let num = Expression::Binary {
        op: OpBinary::Add(Token::default()),
        lhs: Box::new(Expression::Binary {
            op: OpBinary::Mul(Token::default()),
            lhs: Box::new(make_field_ref(a, "re")),
            rhs: Box::new(make_field_ref(b, "re")),
        }),
        rhs: Box::new(Expression::Binary {
            op: OpBinary::Mul(Token::default()),
            lhs: Box::new(make_field_ref(a, "im")),
            rhs: Box::new(make_field_ref(b, "im")),
        }),
    };
    // Denominator: b.re^2 + b.im^2
    let denom = Expression::Binary {
        op: OpBinary::Add(Token::default()),
        lhs: Box::new(Expression::Binary {
            op: OpBinary::Mul(Token::default()),
            lhs: Box::new(make_field_ref(b, "re")),
            rhs: Box::new(make_field_ref(b, "re")),
        }),
        rhs: Box::new(Expression::Binary {
            op: OpBinary::Mul(Token::default()),
            lhs: Box::new(make_field_ref(b, "im")),
            rhs: Box::new(make_field_ref(b, "im")),
        }),
    };
    Equation::Simple {
        lhs: make_field_ref(result, "re"),
        rhs: Expression::Binary {
            op: OpBinary::Div(Token::default()),
            lhs: Box::new(num),
            rhs: Box::new(denom),
        },
    }
}

/// Complex division: c.im = (a.im*b.re - a.re*b.im) / (b.re^2 + b.im^2)
fn make_complex_div_im_eq(result: &str, a: &str, b: &str) -> Equation {
    // Numerator: a.im*b.re - a.re*b.im
    let num = Expression::Binary {
        op: OpBinary::Sub(Token::default()),
        lhs: Box::new(Expression::Binary {
            op: OpBinary::Mul(Token::default()),
            lhs: Box::new(make_field_ref(a, "im")),
            rhs: Box::new(make_field_ref(b, "re")),
        }),
        rhs: Box::new(Expression::Binary {
            op: OpBinary::Mul(Token::default()),
            lhs: Box::new(make_field_ref(a, "re")),
            rhs: Box::new(make_field_ref(b, "im")),
        }),
    };
    // Denominator: b.re^2 + b.im^2
    let denom = Expression::Binary {
        op: OpBinary::Add(Token::default()),
        lhs: Box::new(Expression::Binary {
            op: OpBinary::Mul(Token::default()),
            lhs: Box::new(make_field_ref(b, "re")),
            rhs: Box::new(make_field_ref(b, "re")),
        }),
        rhs: Box::new(Expression::Binary {
            op: OpBinary::Mul(Token::default()),
            lhs: Box::new(make_field_ref(b, "im")),
            rhs: Box::new(make_field_ref(b, "im")),
        }),
    };
    Equation::Simple {
        lhs: make_field_ref(result, "im"),
        rhs: Expression::Binary {
            op: OpBinary::Div(Token::default()),
            lhs: Box::new(num),
            rhs: Box::new(denom),
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
