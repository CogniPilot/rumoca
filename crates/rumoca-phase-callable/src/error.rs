use rumoca_core::{Diagnostic, PhaseError, PrimaryLabel, Span};
use rumoca_plan_callable::PlanConstructionError;

/// The named callable behavior a recursive call would require.
pub(crate) const RECURSIVE_CALL: &str = "recursive-callable-call";

/// All-or-nothing callable-plan construction failure.
#[derive(Debug)]
pub enum CallablePhaseError {
    Unsupported { feature: &'static str, span: Span },
    ConstructionInvariant { reason: String, span: Span },
}

impl CallablePhaseError {
    pub(crate) const fn unsupported(feature: &'static str, span: Span) -> Self {
        Self::Unsupported { feature, span }
    }

    pub const fn code(&self) -> &'static str {
        match self {
            Self::Unsupported { .. } => "EL001",
            Self::ConstructionInvariant { .. } => "EL005",
        }
    }

    pub const fn source_span(&self) -> Span {
        match self {
            Self::Unsupported { span, .. } | Self::ConstructionInvariant { span, .. } => *span,
        }
    }
}

impl std::fmt::Display for CallablePhaseError {
    fn fmt(&self, formatter: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Unsupported { feature, .. } => {
                write!(formatter, "unsupported callable feature: {feature}")
            }
            Self::ConstructionInvariant { reason, .. } => {
                write!(formatter, "invalid callable plan construction: {reason}")
            }
        }
    }
}

impl std::error::Error for CallablePhaseError {}

impl PhaseError for CallablePhaseError {
    fn to_diagnostic(&self) -> Diagnostic {
        Diagnostic::error(
            self.code(),
            self.to_string(),
            PrimaryLabel::new(self.source_span()).with_message("callable planning failed here"),
        )
    }
}

impl From<PlanConstructionError> for CallablePhaseError {
    fn from(error: PlanConstructionError) -> Self {
        match error {
            // Recursion is a callable behavior this compiler does not support
            // yet, not a violated plan invariant. Plan construction already
            // located the exact recursive call, so the phase reports that
            // occurrence as a named unsupported feature.
            PlanConstructionError::UnsupportedRecursion { span } => {
                Self::unsupported(RECURSIVE_CALL, span)
            }
            PlanConstructionError::IdentityOverflow { span }
            | PlanConstructionError::InvalidSourceOccurrence { span }
            | PlanConstructionError::InvalidOwner { span }
            | PlanConstructionError::InvalidOperation { span }
            | PlanConstructionError::DuplicateOccurrence { span }
            | PlanConstructionError::IncompleteCoverage { span } => Self::ConstructionInvariant {
                span,
                reason: error.to_string(),
            },
        }
    }
}

#[cfg(test)]
mod tests {
    use rumoca_core::SourceMap;

    use super::*;

    #[test]
    fn recursion_is_reported_at_the_recursive_call_as_a_named_unsupported_behavior() {
        let mut source_map = SourceMap::new();
        let source = source_map.add("recursive.mo", "function f algorithm f(); end f;");
        let call_span = Span::from_offsets(source, 21, 24);

        let error = CallablePhaseError::from(PlanConstructionError::UnsupportedRecursion {
            span: call_span,
        });

        assert_eq!(error.code(), "EL001");
        assert_eq!(error.source_span(), call_span);
        assert_eq!(
            error.to_string(),
            "unsupported callable feature: recursive-callable-call"
        );
        assert!(matches!(error, CallablePhaseError::Unsupported { .. }));
    }

    #[test]
    fn a_violated_plan_invariant_stays_an_invariant_report() {
        let mut source_map = SourceMap::new();
        let source = source_map.add("invalid.mo", "function f end f;");
        let span = Span::from_offsets(source, 0, 10);

        let error = CallablePhaseError::from(PlanConstructionError::IncompleteCoverage { span });

        assert_eq!(error.code(), "EL005");
        assert_eq!(error.source_span(), span);
        assert!(matches!(
            error,
            CallablePhaseError::ConstructionInvariant { .. }
        ));
    }
}
