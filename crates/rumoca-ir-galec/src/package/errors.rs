//! Checked Algorithm Code package construction failures.

use super::SemanticProvenance;

pub(super) fn validation_error(error: crate::validate::ValidationFailure) -> PackageError {
    match error {
        crate::validate::ValidationFailure::Diagnostics(diagnostics) => {
            PackageError::Block(BlockDiagnostics::construct(diagnostics))
        }
        crate::validate::ValidationFailure::Index(
            crate::validate::RetainedValidationError::IntegerLiteralOutOfDomain {
                value,
                minimum,
                maximum,
                provenance,
            },
        ) => PackageError::IntegerLiteralOutOfDomain {
            value,
            minimum,
            maximum,
            provenance: super::semantic_provenance(provenance),
        },
        crate::validate::ValidationFailure::Index(
            crate::validate::RetainedValidationError::UnexpectedRealMatrixMultiplyOccurrence,
        ) => PackageError::UnexpectedRealMatrixMultiplyOccurrence,
        crate::validate::ValidationFailure::Index(
            crate::validate::RetainedValidationError::InvalidRealMatrixMultiplyOccurrence {
                detail,
                provenance,
            },
        ) => PackageError::InvalidRealMatrixMultiplyOccurrence {
            detail,
            provenance: super::semantic_provenance(provenance),
        },
        crate::validate::ValidationFailure::Index(error) => PackageError::SemanticIndex {
            detail: error.to_string(),
        },
    }
}

/// All language diagnostics collected by a failed checked-block transaction.
#[derive(Debug, Clone, PartialEq)]
pub struct BlockDiagnostics(Vec<crate::GalecError>);

impl BlockDiagnostics {
    pub(super) const fn construct(errors: Vec<crate::GalecError>) -> Self {
        Self(errors)
    }

    #[must_use]
    pub fn errors(&self) -> &[crate::GalecError] {
        &self.0
    }
}

impl std::fmt::Display for BlockDiagnostics {
    fn fmt(&self, formatter: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let message = self
            .0
            .iter()
            .map(ToString::to_string)
            .collect::<Vec<_>>()
            .join("; ");
        formatter.write_str(&message)
    }
}

/// Failure to close a block or its projection metadata.
#[derive(Debug, Clone, PartialEq, thiserror::Error)]
pub enum PackageError {
    #[error("checked GALEC block is invalid: {0}")]
    Block(BlockDiagnostics),
    #[error("checked GALEC semantic index failed to close: {detail}")]
    SemanticIndex { detail: String },
    #[error("compiler projection cannot prove a fixed shape for {subject} {provenance}")]
    UnprovenValueShape {
        subject: &'static str,
        provenance: SemanticProvenance,
    },
    #[error("block and metadata declare different variable counts ({block} != {metadata})")]
    VariableCount { block: usize, metadata: usize },
    #[error("clock variable `{0}` is not a scalar Real constant")]
    InvalidClockReference(String),
    #[error("variable nominal {ordinal} is invalid: {detail}")]
    InvalidVariableNominal {
        ordinal: usize,
        detail: &'static str,
    },
    #[error("constant-fold provenance for `{variable}` is invalid: {detail}")]
    InvalidConstantFold {
        variable: String,
        detail: &'static str,
    },
    #[error("authored checked blocks cannot carry compiler matrix-multiply occurrences")]
    UnexpectedRealMatrixMultiplyOccurrence,
    #[error("generated Real matrix-multiply occurrence failed to close at {provenance}: {detail}")]
    InvalidRealMatrixMultiplyOccurrence {
        detail: String,
        provenance: SemanticProvenance,
    },
    #[error(
        "Integer literal {value} is outside the selected source representation domain [{minimum}, {maximum}] at {provenance}"
    )]
    IntegerLiteralOutOfDomain {
        value: i64,
        minimum: i64,
        maximum: i64,
        provenance: SemanticProvenance,
    },
}
