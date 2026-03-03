use super::*;

/// Convert an AST Subscript to a string for inclusion in variable names.
///
/// MLS §10.1: Array subscripts are part of the variable identity.
pub(super) fn subscript_to_string(sub: &ast::Subscript) -> String {
    match sub {
        ast::Subscript::Expression(expr) => {
            // For simple integer literals, just use the number
            if let Some(val) = try_constant_integer(expr) {
                return val.to_string();
            }
            // For other expressions, convert to string representation
            expression_to_string(expr)
        }
        // Range subscript is just the colon operator `:`
        ast::Subscript::Range { .. } | ast::Subscript::Empty => ":".to_string(),
    }
}

/// Convert an AST Expression to a simple string representation.
///
/// Used for subscript expressions in variable names.
fn expression_to_string(expr: &ast::Expression) -> String {
    match expr {
        ast::Expression::Terminal {
            terminal_type: ast::TerminalType::UnsignedInteger,
            token,
        } => token.text.to_string(),
        ast::Expression::Terminal {
            terminal_type: ast::TerminalType::UnsignedReal,
            token,
        } => token.text.to_string(),
        ast::Expression::ComponentReference(cr) => {
            // Avoid intermediate Vec allocation by building string directly
            let mut result = String::new();
            for (i, p) in cr.parts.iter().enumerate() {
                if i > 0 {
                    result.push('.');
                }
                result.push_str(&p.ident.text);
                if let Some(subs) = &p.subs
                    && !subs.is_empty()
                {
                    let sub_strs: Vec<String> = subs.iter().map(subscript_to_string).collect();
                    result.push('[');
                    result.push_str(&sub_strs.join(","));
                    result.push(']');
                }
            }
            result
        }
        ast::Expression::ArrayIndex { base, subscripts } => {
            let base_str = expression_to_string(base);
            let sub_strs: Vec<String> = subscripts.iter().map(subscript_to_string).collect();
            format!("{base_str}[{}]", sub_strs.join(","))
        }
        ast::Expression::FieldAccess { base, field } => {
            format!("{}.{}", expression_to_string(base), field)
        }
        ast::Expression::Binary { op, lhs, rhs } => {
            format!(
                "({} {} {})",
                expression_to_string(lhs),
                op,
                expression_to_string(rhs)
            )
        }
        ast::Expression::Unary { op, rhs } => {
            format!("({}{})", op, expression_to_string(rhs))
        }
        ast::Expression::FunctionCall { comp, args } => {
            let func_name: String = comp
                .parts
                .iter()
                .map(|p| p.ident.text.as_ref())
                .collect::<Vec<_>>()
                .join(".");
            let arg_strs: Vec<String> = args.iter().map(expression_to_string).collect();
            format!("{}({})", func_name, arg_strs.join(", "))
        }
        ast::Expression::Range { start, step, end } => match step {
            Some(s) => format!(
                "{}:{}:{}",
                expression_to_string(start),
                expression_to_string(s),
                expression_to_string(end)
            ),
            None => format!(
                "{}:{}",
                expression_to_string(start),
                expression_to_string(end)
            ),
        },
        ast::Expression::Terminal {
            terminal_type: ast::TerminalType::End,
            ..
        } => "end".to_string(),
        ast::Expression::Terminal {
            terminal_type: ast::TerminalType::Bool,
            token,
        } => token.text.to_string(),
        ast::Expression::Terminal {
            terminal_type: ast::TerminalType::String,
            token,
        } => format!("\"{}\"", token.text),
        _ => "?".to_string(),
    }
}

/// Try to extract a constant integer from an expression.
fn try_constant_integer(expr: &ast::Expression) -> Option<i64> {
    match expr {
        ast::Expression::Terminal {
            terminal_type: ast::TerminalType::UnsignedInteger,
            token,
        } => token.text.parse().ok(),
        _ => None,
    }
}

impl Subscript {
    /// Convert from AST Subscript.
    pub fn from_ast(sub: &ast::Subscript) -> Self {
        match sub {
            ast::Subscript::Expression(expr) => {
                // Try to evaluate as constant integer first
                if let Some(val) = try_constant_integer(expr) {
                    return Subscript::Index(val);
                }
                // Fall back to expression subscript
                Subscript::Expr(Box::new(Expression::from_ast(expr)))
            }
            ast::Subscript::Range { .. } | ast::Subscript::Empty => Subscript::Colon,
        }
    }
}
