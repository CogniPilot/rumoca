use rumoca_core::Span;

/// A checked Solve program cannot produce its mechanical differentiation artifacts.
#[derive(Debug, Clone)]
pub enum SolveDerivationError {
    ContractViolation { reason: String, span: Span },
    UnspannedContractViolation { reason: String },
}

impl SolveDerivationError {
    pub(crate) fn contract(reason: impl Into<String>, span: Span) -> Self {
        if span.is_dummy() {
            return Self::unspanned_contract(reason);
        }
        Self::ContractViolation {
            reason: reason.into(),
            span,
        }
    }

    pub(crate) fn unspanned_contract(reason: impl Into<String>) -> Self {
        Self::UnspannedContractViolation {
            reason: reason.into(),
        }
    }

    #[must_use]
    pub fn source_span(&self) -> Option<Span> {
        match self {
            Self::ContractViolation { span, .. } if !span.is_dummy() => Some(*span),
            Self::ContractViolation { .. } | Self::UnspannedContractViolation { .. } => None,
        }
    }
}

impl std::fmt::Display for SolveDerivationError {
    fn fmt(&self, formatter: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::ContractViolation { reason, .. }
            | Self::UnspannedContractViolation { reason } => {
                write!(formatter, "invalid Solve IR contract: {reason}")
            }
        }
    }
}

impl std::error::Error for SolveDerivationError {}

impl From<rumoca_ir_solve::SolveProblemShapeContractError> for SolveDerivationError {
    fn from(error: rumoca_ir_solve::SolveProblemShapeContractError) -> Self {
        let reason = error.to_string();
        match error.source_span() {
            Some(span) => Self::contract(reason, span),
            None => Self::unspanned_contract(reason),
        }
    }
}

impl From<crate::ScalarizeError> for SolveDerivationError {
    fn from(error: crate::ScalarizeError) -> Self {
        let reason = error.to_string();
        match error.source_span() {
            Some(span) => Self::contract(reason, span),
            None => Self::unspanned_contract(reason),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ScalarizeError;
    use rumoca_ir_solve::SolveProblemShapeContractError;

    fn source_span() -> Span {
        Span::from_offsets(rumoca_core::SourceId::from_source_name("error.mo"), 3, 9)
    }

    #[test]
    fn a_dummy_span_yields_an_unspanned_violation_and_a_source_span_is_kept() {
        let spanned = SolveDerivationError::contract("bad row", source_span());
        assert_eq!(spanned.source_span(), Some(source_span()));
        assert_eq!(spanned.to_string(), "invalid Solve IR contract: bad row");

        let dummy = SolveDerivationError::contract("no origin", Span::DUMMY);
        assert!(matches!(
            dummy,
            SolveDerivationError::UnspannedContractViolation { .. }
        ));
        assert_eq!(dummy.source_span(), None);
        assert_eq!(dummy.to_string(), "invalid Solve IR contract: no origin");

        let unspanned = SolveDerivationError::unspanned_contract(String::from("loose"));
        assert_eq!(unspanned.source_span(), None);
        let hidden = SolveDerivationError::ContractViolation {
            reason: "stale".into(),
            span: Span::DUMMY,
        };
        assert_eq!(hidden.source_span(), None);
    }

    #[test]
    fn shape_contract_errors_keep_their_reason_and_optional_span() {
        let schema = SolveProblemShapeContractError::SchemaVersion {
            actual: 1,
            expected: 2,
        };
        let reason = schema.to_string();
        let error = SolveDerivationError::from(schema);
        assert_eq!(error.source_span(), None);
        assert_eq!(
            error.to_string(),
            format!("invalid Solve IR contract: {reason}")
        );

        let spanned = SolveProblemShapeContractError::ScalarProgramSpanMismatch {
            context: "rows".into(),
            node_index: 0,
            programs: 2,
            spans: 1,
            span: Some(source_span()),
        };
        assert_eq!(
            SolveDerivationError::from(spanned).source_span(),
            Some(source_span())
        );
    }

    #[test]
    fn scalarization_errors_keep_their_reason_and_optional_span() {
        let spanned = ScalarizeError::NegativeLoadIndex {
            kind: "Map",
            value: -1,
            span: source_span(),
        };
        let reason = spanned.to_string();
        let error = SolveDerivationError::from(spanned);
        assert_eq!(error.source_span(), Some(source_span()));
        assert_eq!(
            error.to_string(),
            format!("invalid Solve IR contract: {reason}")
        );

        let dummy = ScalarizeError::NegativeLoadIndex {
            kind: "Map",
            value: -1,
            span: Span::DUMMY,
        };
        assert_eq!(SolveDerivationError::from(dummy).source_span(), None);
    }
}
