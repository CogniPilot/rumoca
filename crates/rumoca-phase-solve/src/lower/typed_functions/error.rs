//! Keep source-language capability failures distinct from checked IR failures.

use rumoca_core::Span;
use rumoca_ir_solve as solve;

/// A failure while issuing the finite, source-owned pure-call graph.
#[derive(Debug, Clone)]
pub enum CallLoweringError {
    Construction(solve::SolveProgramConstructionError),
    UnsupportedExternal {
        language: String,
        function: String,
        symbol: String,
        provenance: Span,
    },
}

impl CallLoweringError {
    #[must_use]
    pub fn source_span(&self) -> Option<Span> {
        match self {
            Self::Construction(error) => error.source_span(),
            Self::UnsupportedExternal { provenance, .. } => Some(*provenance),
        }
    }

    pub(crate) fn into_lower_error(self, fallback: Span) -> crate::LowerError {
        let message = self.to_string();
        match self {
            Self::Construction(_) => crate::LowerError::contract(message, fallback),
            Self::UnsupportedExternal { provenance, .. } => {
                crate::LowerError::unsupported(message, provenance)
            }
        }
    }
}

impl From<solve::SolveProgramConstructionError> for CallLoweringError {
    fn from(error: solve::SolveProgramConstructionError) -> Self {
        Self::Construction(error)
    }
}

impl std::fmt::Display for CallLoweringError {
    fn fmt(&self, formatter: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Construction(error) => std::fmt::Display::fmt(error, formatter),
            Self::UnsupportedExternal {
                language,
                function,
                symbol,
                ..
            } => write!(
                formatter,
                "external {language} function `{function}` calls `{symbol}`, which the Solve runtime cannot execute"
            ),
        }
    }
}

impl std::error::Error for CallLoweringError {}
