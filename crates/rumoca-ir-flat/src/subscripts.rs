use super::*;

/// Try to extract a constant integer from an expression.
fn try_constant_integer(expr: &ast::Expression) -> Option<i64> {
    match expr {
        ast::Expression::Terminal {
            terminal_type: ast::TerminalType::UnsignedInteger,
            token,
            ..
        } => token.text.parse().ok(),
        ast::Expression::Unary { op, rhs, .. } => {
            let value = try_constant_integer(rhs)?;
            match op {
                rumoca_core::OpUnary::Empty
                | rumoca_core::OpUnary::Plus
                | rumoca_core::OpUnary::DotPlus => Some(value),
                rumoca_core::OpUnary::Minus | rumoca_core::OpUnary::DotMinus => value.checked_neg(),
                rumoca_core::OpUnary::Not => None,
            }
        }
        ast::Expression::Binary { op, lhs, rhs, .. } => {
            let lhs = try_constant_integer(lhs)?;
            let rhs = try_constant_integer(rhs)?;
            match op {
                rumoca_core::OpBinary::Add | rumoca_core::OpBinary::AddElem => lhs.checked_add(rhs),
                rumoca_core::OpBinary::Sub | rumoca_core::OpBinary::SubElem => lhs.checked_sub(rhs),
                rumoca_core::OpBinary::Mul | rumoca_core::OpBinary::MulElem => lhs.checked_mul(rhs),
                rumoca_core::OpBinary::Div | rumoca_core::OpBinary::DivElem => {
                    (rhs != 0 && lhs % rhs == 0).then_some(lhs / rhs)
                }
                rumoca_core::OpBinary::Exp | rumoca_core::OpBinary::ExpElem => {
                    u32::try_from(rhs).ok().and_then(|exp| lhs.checked_pow(exp))
                }
                _ => None,
            }
        }
        _ => None,
    }
}

/// Convert from AST Subscript.
pub(super) fn subscript_from_ast(sub: &ast::Subscript) -> Subscript {
    match sub {
        ast::Subscript::Expression(expr) => {
            let span = expr.span();
            if let Some(val) = try_constant_integer(expr) {
                return Subscript::index(val, span);
            }
            Subscript::expr(Box::new(expression_from_ast(expr)), span)
        }
        ast::Subscript::Range { .. } | ast::Subscript::Empty => {
            Subscript::generated_colon(rumoca_core::Span::DUMMY)
        }
    }
}
