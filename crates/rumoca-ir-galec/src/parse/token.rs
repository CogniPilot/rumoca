//! Parser-local terminal token type used by the generated GALEC actions.
//!
//! Keeps the matched lexeme text plus its source [`Span`] (SPEC_0034 D11): the
//! span is the single origin from which every AST node's span bubbles up (only
//! `Ident`/literal terminals retain a token through parol's `%nt_type`
//! conversion; every mid-level node's span is a union of its children's).
//! Numeric/boolean literals are parsed from the text in the `constant` action
//! via [`ParserToken::as_f64`] / [`ParserToken::as_i64`] / [`ParserToken::as_bool`].

use rumoca_core::{SourceId, Span};

/// A GALEC terminal: the matched lexeme text and its source span.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ParserToken {
    /// The verbatim matched source text of the token.
    pub text: String,
    /// Source span of the matched lexeme.
    pub span: Span,
}

impl Default for ParserToken {
    fn default() -> Self {
        Self {
            text: String::new(),
            span: Span::DUMMY,
        }
    }
}

impl ParserToken {
    /// The verbatim token text.
    #[must_use]
    pub fn text(&self) -> &str {
        &self.text
    }

    /// The token's source span.
    #[must_use]
    pub fn span(&self) -> Span {
        self.span
    }

    /// Parse the token text as an `f64` (GALEC `real` literal).
    pub fn as_f64(&self) -> Result<f64, std::num::ParseFloatError> {
        self.text.parse::<f64>()
    }

    /// Parse the token text as an `i64` (GALEC `integer` literal).
    pub fn as_i64(&self) -> Result<i64, std::num::ParseIntError> {
        self.text.parse::<i64>()
    }

    /// Parse the token text as a `bool` (`true` / `false`).
    #[must_use]
    pub fn as_bool(&self) -> Option<bool> {
        match self.text.as_str() {
            "true" => Some(true),
            "false" => Some(false),
            _ => None,
        }
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
            span: Span::from_offsets(source, location.start(), location.end()),
        })
    }
}
