//! Typed, provenance-bearing failures for Flat-to-DAE construction.

use miette::Diagnostic;
use rumoca_core::{
    BoxedResult, Diagnostic as CommonDiagnostic, PhaseError, Span, miette_phase_error_to_diagnostic,
};
use rumoca_ir_dae as dae;
use thiserror::Error;

use crate::balance::{BalanceBreakdown, BalanceDetail};

pub type ToDaeResult<T> = BoxedResult<T, ToDaeError>;

#[derive(Debug, Clone, Error, Diagnostic)]
pub enum ToDaeError {
    #[error("unbalanced model: {equations} equations, {unknowns} unknowns (balance = {balance})")]
    #[diagnostic(
        code(rumoca::todae::ED001),
        help("MLS §4.9 requires a balanced non-partial model; breakdown: {detail}")
    )]
    Unbalanced {
        equations: usize,
        unknowns: usize,
        balance: i64,
        detail: BalanceBreakdown,
    },

    #[error("internal ToDAE error: {detail}")]
    #[diagnostic(code(rumoca::todae::ED003))]
    Internal { detail: String },

    #[error("Flat semantic owner is missing source provenance: {owner}")]
    #[diagnostic(
        code(rumoca::todae::ED007),
        help(
            "every declaration, expression, subscript, and semantic owner must retain a source-backed span"
        )
    )]
    MissingProvenance { owner: String },

    #[error("invalid Appendix B discrete solved form: {detail}")]
    #[diagnostic(
        code(rumoca::todae::ED010),
        help(
            "discrete-valued equations must be explicit assignments with an acyclic current-value dependency order"
        )
    )]
    DiscreteSolvedFormViolation {
        detail: String,
        #[label("invalid discrete solved-form equation")]
        span: Span,
    },

    #[error("unresolved Flat reference `{name}`")]
    #[diagnostic(
        code(rumoca::todae::ED008),
        help("Flat IR must carry a declared, fully resolved coordinate identity")
    )]
    UnresolvedReference {
        name: String,
        #[label("undefined reference used here")]
        span: Span,
    },

    #[error("unsupported {section} algorithm in canonical DAE: {origin}")]
    #[diagnostic(
        code(rumoca::todae::ED013),
        help(
            "the statement must be lowered to a supported declarative owner before DAE construction"
        )
    )]
    UnsupportedAlgorithm {
        section: String,
        origin: String,
        #[label("unsupported algorithm owner")]
        span: Span,
    },

    #[error("unsupported runtime operator `{operator}`: {detail}")]
    #[diagnostic(
        code(rumoca::todae::ED018),
        help("Rumoca rejects runtime semantics that are not represented by a checked DAE owner")
    )]
    UnsupportedRuntimeOperator {
        operator: String,
        detail: String,
        #[label("unsupported runtime operator")]
        span: Span,
    },

    #[error("unsupported Flat semantic owner `{feature}`: {detail}")]
    #[diagnostic(
        code(rumoca::todae::ED019),
        help(
            "this construct is rejected before simulation until its checked DAE representation is implemented"
        )
    )]
    UnsupportedFlatSemantics {
        feature: String,
        detail: String,
        #[label("unsupported semantic owner")]
        span: Span,
    },

    #[error("canonical DAE construction rejected an invalid operation: {source}")]
    #[diagnostic(
        code(rumoca::todae::ED020),
        help("the checked DAE constructor rejected this operation at its semantic owner")
    )]
    Construction {
        source: dae::DaeConstructionError,
        #[label("invalid DAE operation")]
        span: Span,
    },
}

impl ToDaeError {
    pub fn unbalanced_from_detail(detail: BalanceDetail) -> Self {
        let (equations, unknowns) = detail.equations_unknowns();
        let balance = detail.balance();
        Self::Unbalanced {
            equations,
            unknowns,
            balance,
            detail: detail.into(),
        }
    }

    pub fn balance_detail(&self) -> Option<&BalanceDetail> {
        match self {
            Self::Unbalanced { detail, .. } => Some(detail),
            _ => None,
        }
    }

    pub fn internal(detail: impl Into<String>) -> Self {
        Self::Internal {
            detail: detail.into(),
        }
    }

    pub fn unresolved_reference(name: impl Into<String>, span: Span) -> Self {
        Self::UnresolvedReference {
            name: name.into(),
            span,
        }
    }

    pub fn discrete_solved_form_violation(detail: impl Into<String>, span: Span) -> Self {
        Self::DiscreteSolvedFormViolation {
            detail: detail.into(),
            span,
        }
    }

    pub fn unsupported_algorithm(
        section: impl Into<String>,
        origin: impl Into<String>,
        span: Span,
    ) -> Self {
        Self::UnsupportedAlgorithm {
            section: section.into(),
            origin: origin.into(),
            span,
        }
    }

    pub fn unsupported_runtime_operator(
        operator: impl Into<String>,
        detail: impl Into<String>,
        span: Span,
    ) -> Self {
        Self::UnsupportedRuntimeOperator {
            operator: operator.into(),
            detail: detail.into(),
            span,
        }
    }

    pub fn unsupported_flat(
        feature: impl Into<String>,
        detail: impl Into<String>,
        span: Span,
    ) -> Self {
        Self::UnsupportedFlatSemantics {
            feature: feature.into(),
            detail: detail.into(),
            span,
        }
    }

    pub fn source_span(&self) -> Option<Span> {
        self.diagnostic_source_spans().first().copied()
    }

    fn diagnostic_source_spans(&self) -> &[Span] {
        match self {
            Self::UnresolvedReference { span, .. }
            | Self::DiscreteSolvedFormViolation { span, .. }
            | Self::UnsupportedAlgorithm { span, .. }
            | Self::UnsupportedRuntimeOperator { span, .. }
            | Self::UnsupportedFlatSemantics { span, .. }
            | Self::Construction { span, .. } => std::slice::from_ref(span),
            Self::Unbalanced { .. } | Self::Internal { .. } | Self::MissingProvenance { .. } => &[],
        }
    }
}

impl From<dae::DaeConstructionError> for ToDaeError {
    fn from(source: dae::DaeConstructionError) -> Self {
        if let dae::DaeConstructionError::InvalidDiscreteDependencyCycle { target, span } = source {
            return Self::discrete_solved_form_violation(
                format!("target identity {target} participates in a current-value cycle"),
                span,
            );
        }
        match source.source_span() {
            Some(span) if !span.is_dummy() => Self::Construction { source, span },
            _ => Self::internal(source.to_string()),
        }
    }
}

impl PhaseError for ToDaeError {
    fn to_diagnostic(&self) -> CommonDiagnostic {
        miette_phase_error_to_diagnostic(self, self.diagnostic_source_spans())
    }
}
