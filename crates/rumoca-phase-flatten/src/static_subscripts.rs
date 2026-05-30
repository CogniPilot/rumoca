use rumoca_ir_ast as ast;

pub(crate) fn try_constant_integer(expr: &ast::Expression) -> Option<i64> {
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

#[cfg(test)]
mod tests {
    use std::sync::Arc;

    use rumoca_core::{OpBinary, Span, Token};

    use super::*;

    fn int(value: i64) -> ast::Expression {
        ast::Expression::Terminal {
            terminal_type: ast::TerminalType::UnsignedInteger,
            token: Token {
                text: Arc::from(value.to_string()),
                ..Default::default()
            },
            span: Span::DUMMY,
        }
    }

    #[test]
    fn folds_simple_static_subscript_arithmetic() {
        let expr = ast::Expression::Binary {
            op: OpBinary::Sub,
            lhs: Arc::new(int(2)),
            rhs: Arc::new(int(1)),
            span: Span::DUMMY,
        };

        assert_eq!(try_constant_integer(&expr), Some(1));
    }
}
