use super::*;

pub(crate) fn try_eval_const_terminal_expr(
    expr: &ast::Expression,
) -> Option<rumoca_core::Expression> {
    match expr {
        ast::Expression::Terminal {
            terminal_type: rumoca_ir_ast::TerminalType::UnsignedReal,
            token,
            ..
        } => token
            .text
            .as_ref()
            .parse::<f64>()
            .ok()
            .map(Literal::Real)
            .map(|value| rumoca_core::Expression::Literal {
                value,
                span: expr.span(),
            }),
        ast::Expression::Terminal {
            terminal_type: rumoca_ir_ast::TerminalType::UnsignedInteger,
            token,
            ..
        } => token
            .text
            .as_ref()
            .parse::<i64>()
            .ok()
            .map(Literal::Integer)
            .map(|value| rumoca_core::Expression::Literal {
                value,
                span: expr.span(),
            }),
        ast::Expression::Terminal {
            terminal_type: rumoca_ir_ast::TerminalType::Bool,
            token,
            ..
        } => match token.text.as_ref() {
            "true" => Some(rumoca_core::Expression::Literal {
                value: Literal::Boolean(true),
                span: expr.span(),
            }),
            "false" => Some(rumoca_core::Expression::Literal {
                value: Literal::Boolean(false),
                span: expr.span(),
            }),
            _ => None,
        },
        ast::Expression::Terminal {
            terminal_type: rumoca_ir_ast::TerminalType::String,
            token,
            ..
        } => Some(rumoca_core::Expression::Literal {
            value: Literal::String(token.text.as_ref().to_string()),
            span: expr.span(),
        }),
        _ => None,
    }
}
