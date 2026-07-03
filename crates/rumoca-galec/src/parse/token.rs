//! Parser-local terminal token type used by the generated GALEC actions.
//!
//! The GALEC AST stores no source locations (spans/tokens/offsets are absent),
//! so this newtype keeps only the matched lexeme text. Numeric/boolean literals
//! are parsed from the text in the `constant` action via [`ParserToken::as_f64`]
//! / [`ParserToken::as_i64`] / [`ParserToken::as_bool`].

/// A GALEC terminal: the matched lexeme text, nothing else.
#[derive(Debug, Clone, Default, PartialEq, Eq)]
pub struct ParserToken {
    /// The verbatim matched source text of the token.
    pub text: String,
}

impl ParserToken {
    /// The verbatim token text.
    #[must_use]
    pub fn text(&self) -> &str {
        &self.text
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
        Ok(Self {
            text: value.text().to_string(),
        })
    }
}
