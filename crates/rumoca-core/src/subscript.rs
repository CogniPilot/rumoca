use serde::{Deserialize, Serialize};

use crate::{Expression, Span};

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum Subscript {
    Index { value: i64, span: Span },
    Colon { span: Span },
    Expr { expr: Box<Expression>, span: Span },
}

impl Subscript {
    pub fn index(value: i64, span: Span) -> Self {
        Self::Index { value, span }
    }

    pub fn generated_index(value: i64, span: Span) -> Self {
        Self::Index { value, span }
    }

    pub fn colon(span: Span) -> Self {
        Self::Colon { span }
    }

    pub fn generated_colon(span: Span) -> Self {
        Self::Colon { span }
    }

    pub fn expr(expr: Box<Expression>, span: Span) -> Self {
        Self::Expr { expr, span }
    }

    pub fn generated_expr(expr: Box<Expression>) -> Self {
        let span = expr.span().unwrap_or(Span::DUMMY);
        Self::Expr { expr, span }
    }

    pub fn span(&self) -> Span {
        match self {
            Self::Index { span, .. } | Self::Colon { span } | Self::Expr { span, .. } => *span,
        }
    }
}
