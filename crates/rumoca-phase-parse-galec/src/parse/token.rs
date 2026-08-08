//! Parser-local terminal token type used by the generated GALEC actions.
//!
//! Keeps the matched lexeme text plus its source [`Span`] (SPEC_0034 D11): the
//! span is the single origin from which every AST node's span bubbles up.
//! Grammar productions retain each owner token needed to cover a node's exact
//! source occurrence; mid-level spans are unions of those source-backed tokens.
//! Numeric/boolean literals are parsed from the text in the `constant` action
//! via [`ParserToken::as_f64`] / [`ParserToken::as_i64`].

use rumoca_core::{SourceId, Span};

/// A GALEC terminal: the matched lexeme text and its source span.
#[derive(Debug, Clone, Default, PartialEq, Eq)]
pub(crate) struct ParserToken {
    /// The verbatim matched source text of the token.
    pub(crate) text: String,
    /// Source span of the matched lexeme.
    pub(crate) span: Option<Span>,
}

impl ParserToken {
    /// The verbatim token text.
    #[must_use]
    pub(crate) fn text(&self) -> &str {
        &self.text
    }

    /// The token's source span.
    pub(crate) fn span(&self) -> anyhow::Result<Span> {
        self.span
            .ok_or_else(|| anyhow::anyhow!("parser token has no source provenance"))
    }

    /// Parse the token text as an `f64` (GALEC `real` literal).
    pub(crate) fn as_f64(&self) -> Result<f64, std::num::ParseFloatError> {
        self.text.parse::<f64>()
    }

    /// Parse the token text as an `i64` (GALEC `integer` literal).
    pub(crate) fn as_i64(&self) -> Result<i64, std::num::ParseIntError> {
        self.text.parse::<i64>()
    }
}

impl From<&ParserToken> for String {
    fn from(value: &ParserToken) -> Self {
        value.text.clone()
    }
}

impl TryFrom<&parol_runtime::Token<'_>> for ParserToken {
    type Error = anyhow::Error;

    fn try_from(value: &parol_runtime::Token<'_>) -> Result<Self, Self::Error> {
        let location = &value.location;
        // parol tracks the source name on the token's location; derive a stable
        // `SourceId` from it so all tokens of one parse share a source identity
        // (byte offsets are the same basis the LSP maps against). `end` is
        // exclusive, matching `Span`'s convention.
        let source = SourceId::from_source_name(&location.file_name.to_string_lossy());
        Ok(Self {
            text: value.text().to_string(),
            span: Some(Span::from_offsets(source, location.start(), location.end())),
        })
    }
}
