use rumoca_ir_ast as ast;

struct LiteralIntegerContext;

impl rumoca_eval_ast::ast_scalar::AstScalarContext for LiteralIntegerContext {
    fn lookup_integer(&self, _expr: &ast::Expression, _scope: &str, _depth: usize) -> Option<i64> {
        None
    }

    fn lookup_boolean(&self, _expr: &ast::Expression, _scope: &str, _depth: usize) -> Option<bool> {
        None
    }

    fn call_integer(
        &self,
        _function: &ast::ComponentReference,
        _args: &[ast::Expression],
        _scope: &str,
        _depth: usize,
        _span: rumoca_core::Span,
    ) -> Option<i64> {
        None
    }

    fn integer_binary(
        &self,
        op: &rumoca_core::OpBinary,
        lhs: i64,
        rhs: i64,
        _span: rumoca_core::Span,
    ) -> Option<i64> {
        rumoca_core::eval_ast_integer_binary(op, lhs, rhs)
    }
}

pub(crate) fn try_constant_integer(expr: &ast::Expression) -> Option<i64> {
    rumoca_eval_ast::ast_scalar::eval_integer(expr, &LiteralIntegerContext, "", 0)
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
