use serde::{Deserialize, Serialize};

use crate::{Expression, MissingProvenanceSpan, ProvenanceSpan, Span};

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

    pub fn generated_index_with_provenance(value: i64, span: ProvenanceSpan) -> Self {
        Self::Index {
            value,
            span: span.span(),
        }
    }

    pub fn try_generated_index(
        value: i64,
        span: Span,
        context: &'static str,
    ) -> Result<Self, MissingProvenanceSpan> {
        Ok(Self::generated_index_with_provenance(
            value,
            span.require_provenance(context)?,
        ))
    }

    pub fn colon(span: Span) -> Self {
        Self::Colon { span }
    }

    pub fn generated_colon(span: Span) -> Self {
        Self::Colon { span }
    }

    pub fn generated_colon_with_provenance(span: ProvenanceSpan) -> Self {
        Self::Colon { span: span.span() }
    }

    pub fn try_generated_colon(
        span: Span,
        context: &'static str,
    ) -> Result<Self, MissingProvenanceSpan> {
        Ok(Self::generated_colon_with_provenance(
            span.require_provenance(context)?,
        ))
    }

    pub fn expr(expr: Box<Expression>, span: Span) -> Self {
        Self::Expr { expr, span }
    }

    pub fn generated_expr(expr: Box<Expression>, span: Span) -> Self {
        Self::Expr { expr, span }
    }

    pub fn generated_expr_with_provenance(expr: Box<Expression>, span: ProvenanceSpan) -> Self {
        Self::Expr {
            expr,
            span: span.span(),
        }
    }

    pub fn try_generated_expr(
        expr: Box<Expression>,
        span: Span,
        context: &'static str,
    ) -> Result<Self, MissingProvenanceSpan> {
        Ok(Self::generated_expr_with_provenance(
            expr,
            span.require_provenance(context)?,
        ))
    }

    pub fn span(&self) -> Span {
        match self {
            Self::Index { span, .. } | Self::Colon { span } | Self::Expr { span, .. } => *span,
        }
    }

    pub fn require_span(
        &self,
        context: &'static str,
    ) -> Result<ProvenanceSpan, MissingProvenanceSpan> {
        self.span().require_provenance(context)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{BytePos, Literal, SourceId};

    fn test_span() -> Span {
        Span::new(
            SourceId::from_source_name("core_subscript_source_7.mo"),
            BytePos(11),
            BytePos(13),
        )
    }

    #[test]
    fn generated_index_requires_provenance_span() {
        let subscript = Subscript::try_generated_index(2, test_span(), "generated test index")
            .expect("real owner span should be accepted");

        assert_eq!(subscript.span(), test_span());
    }

    #[test]
    fn subscript_require_span_accepts_real_span() {
        let subscript = Subscript::index(2, test_span());

        assert_eq!(
            subscript
                .require_span("source subscript")
                .expect("real subscript span should be accepted")
                .span(),
            test_span()
        );
    }

    #[test]
    fn subscript_require_span_rejects_dummy_span() {
        let subscript = Subscript::index(2, Span::DUMMY);
        let err = subscript
            .require_span("source subscript")
            .expect_err("dummy subscript span should be rejected");

        assert_eq!(err.context(), "source subscript");
    }

    #[test]
    fn generated_index_rejects_dummy_span() {
        let err =
            Subscript::try_generated_index(2, Span::DUMMY, "generated test index").unwrap_err();

        assert_eq!(err.context(), "generated test index");
        assert_eq!(
            err.to_string(),
            "missing source provenance for generated test index"
        );
    }

    #[test]
    fn generated_expr_rejects_dummy_span() {
        let expr = Expression::Literal {
            value: Literal::Integer(1),
            span: test_span(),
        };
        let err = Subscript::try_generated_expr(Box::new(expr), Span::DUMMY, "generated test expr")
            .unwrap_err();

        assert_eq!(err.context(), "generated test expr");
    }
}
