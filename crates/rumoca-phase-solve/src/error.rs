use rumoca_core::{Diagnostic, PhaseError, PrimaryLabel, Span};

/// A failure to lower checked DAE semantics into a computable Solve program.
#[derive(Debug, Clone)]
pub enum LowerError {
    Unsupported {
        reason: String,
        span: Span,
    },
    NonComputable {
        reason: String,
        span: Span,
    },
    Structural {
        reason: String,
        span: Option<Span>,
    },
    ContractViolation {
        reason: String,
        span: Span,
    },
    /// Span-free: the violated whole-program contract has no single honest source owner.
    UnspannedContractViolation {
        reason: String,
    },
}

impl LowerError {
    pub(crate) fn unsupported(reason: impl Into<String>, span: Span) -> Self {
        Self::Unsupported {
            reason: reason.into(),
            span,
        }
    }

    pub(crate) fn non_computable(reason: impl Into<String>, span: Span) -> Self {
        Self::NonComputable {
            reason: reason.into(),
            span,
        }
    }

    pub(crate) fn unspanned_non_computable(reason: impl Into<String>) -> Self {
        Self::UnspannedContractViolation {
            reason: reason.into(),
        }
    }

    pub(crate) fn contract(reason: impl Into<String>, span: Span) -> Self {
        if span.is_dummy() {
            return Self::UnspannedContractViolation {
                reason: reason.into(),
            };
        }
        Self::ContractViolation {
            reason: reason.into(),
            span,
        }
    }

    #[must_use]
    pub const fn code(&self) -> &'static str {
        use crate::diagnostic_codes as codes;
        match self {
            Self::Unsupported { .. } => codes::EL001_UNSUPPORTED_EXPRESSION,
            Self::NonComputable { .. } => codes::EL005_INVALID_SOLVE_CONTRACT,
            Self::Structural { .. }
            | Self::ContractViolation { .. }
            | Self::UnspannedContractViolation { .. } => codes::EL005_INVALID_SOLVE_CONTRACT,
        }
    }

    #[must_use]
    pub fn source_span(&self) -> Option<Span> {
        match self {
            Self::Unsupported { span, .. }
            | Self::NonComputable { span, .. }
            | Self::ContractViolation { span, .. }
                if !span.is_dummy() =>
            {
                Some(*span)
            }
            Self::Structural { span, .. } => *span,
            Self::Unsupported { .. }
            | Self::NonComputable { .. }
            | Self::ContractViolation { .. }
            | Self::UnspannedContractViolation { .. } => None,
        }
    }
}

impl std::fmt::Display for LowerError {
    fn fmt(&self, formatter: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Unsupported { reason, .. } => {
                write!(formatter, "unsupported checked DAE semantics: {reason}")
            }
            Self::NonComputable { reason, .. } => {
                write!(formatter, "DAE system is not computable: {reason}")
            }
            Self::Structural { reason, .. } => {
                write!(formatter, "DAE structural proof failed: {reason}")
            }
            Self::ContractViolation { reason, .. }
            | Self::UnspannedContractViolation { reason } => {
                write!(formatter, "invalid Solve IR contract: {reason}")
            }
        }
    }
}

impl std::error::Error for LowerError {}

impl PhaseError for LowerError {
    fn to_diagnostic(&self) -> Diagnostic {
        match self.source_span() {
            Some(span) => Diagnostic::error(
                self.code(),
                self.to_string(),
                PrimaryLabel::new(span).with_message("Solve lowering failed here"),
            ),
            None => Diagnostic::global_error(self.code(), self.to_string()),
        }
    }
}

impl From<rumoca_ir_solve::SolveProblemShapeContractError> for LowerError {
    fn from(error: rumoca_ir_solve::SolveProblemShapeContractError) -> Self {
        let reason = error.to_string();
        match error.source_span() {
            Some(span) => Self::contract(reason, span),
            None => Self::UnspannedContractViolation { reason },
        }
    }
}

impl From<rumoca_eval_solve::ScalarizeError> for LowerError {
    fn from(error: rumoca_eval_solve::ScalarizeError) -> Self {
        let reason = error.to_string();
        match error.source_span() {
            Some(span) => Self::contract(reason, span),
            None => Self::UnspannedContractViolation { reason },
        }
    }
}

/// The one conversion from a checked typed-program rejection.
///
/// The typed vocabulary states the owner span the rejection was raised at, so
/// that span is what a reader needs; a caller's enclosing span is a coarser
/// owner that collapses two rejections raised at different source lines into
/// one location. `SolveProgramConstructionError::provenance` reports `None`
/// exactly for the two rejections that fire *because* no owner span was
/// recoverable, and those become an explicitly unspanned contract violation
/// instead of borrowing whatever span the caller happened to hold.
impl From<rumoca_ir_solve::SolveProgramConstructionError> for LowerError {
    fn from(error: rumoca_ir_solve::SolveProgramConstructionError) -> Self {
        let reason = error.to_string();
        match error.provenance() {
            Some(span) => Self::contract(reason, span),
            None => Self::UnspannedContractViolation { reason },
        }
    }
}

#[cfg(test)]
mod tests {
    use super::LowerError;
    use rumoca_core::{BytePos, SourceId, Span};
    use rumoca_ir_solve::SolveProgramConstructionError;

    fn span(start: usize, end: usize) -> Span {
        Span::new(
            SourceId::from_source_name("provenance_closure.mo"),
            BytePos(start),
            BytePos(end),
        )
    }

    #[test]
    fn two_rejections_from_distinct_source_lines_keep_distinct_owners() {
        let first = LowerError::from(SolveProgramConstructionError::InvalidCallInterface {
            provenance: span(10, 20),
        });
        let second = LowerError::from(SolveProgramConstructionError::TypeMismatch {
            provenance: span(40, 55),
        });
        let owner = |error: &LowerError| match error {
            LowerError::ContractViolation { span, .. } => Some(*span),
            _ => None,
        };
        assert_eq!(owner(&first), Some(span(10, 20)));
        assert_eq!(owner(&second), Some(span(40, 55)));
        assert_ne!(owner(&first), owner(&second));
    }

    #[test]
    fn a_provenance_free_rejection_is_explicitly_unspanned() {
        for error in [
            SolveProgramConstructionError::MissingProvenance,
            SolveProgramConstructionError::WireMismatch,
        ] {
            let reason = error.to_string();
            assert!(matches!(
                LowerError::from(error),
                LowerError::UnspannedContractViolation { reason: carried } if carried == reason
            ));
        }
    }
}
