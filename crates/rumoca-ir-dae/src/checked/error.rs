use rumoca_core::Span;

use super::{DaeProvenanceOrigin, ScalarType};

/// Failure to construct or decode the checked DAE.
#[derive(Debug, Clone, PartialEq)]
pub enum DaeConstructionError {
    MissingProvenance {
        origin: DaeProvenanceOrigin,
        attempted_span: Span,
    },
    UnknownSource {
        span: Span,
    },
    InvalidSourceRange {
        span: Span,
        source_len: usize,
    },
    CapacityExceeded {
        arena: &'static str,
        attempted_index: usize,
        span: Span,
    },
    UnknownId {
        kind: &'static str,
        index: u32,
        span: Span,
    },
    TypeMismatch {
        expected: ScalarType,
        found: ScalarType,
        span: Span,
    },
    ShapeMismatch {
        span: Span,
    },
    ExpectedScalar {
        span: Span,
    },
    ExpectedNumeric {
        found: ScalarType,
        span: Span,
    },
    InvalidArity {
        expected: usize,
        found: usize,
        span: Span,
    },
    EmptyArray {
        span: Span,
    },
    ZeroRangeStep {
        span: Span,
    },
    InvalidSubscript {
        span: Span,
    },
    DuplicateDefinition {
        kind: &'static str,
        index: u32,
        span: Span,
    },
    IncompleteDefinition {
        kind: &'static str,
        index: u32,
        span: Span,
    },
    InvalidSchemaVersion {
        expected: u16,
        found: u16,
    },
}

impl std::fmt::Display for DaeConstructionError {
    fn fmt(&self, formatter: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::MissingProvenance { origin, .. } => {
                write!(formatter, "missing source provenance for {origin}")
            }
            Self::UnknownSource { span } => {
                write!(
                    formatter,
                    "DAE provenance references an unknown source: {span:?}"
                )
            }
            Self::InvalidSourceRange { span, source_len } => write!(
                formatter,
                "DAE provenance range {span:?} is invalid for source length {source_len}"
            ),
            Self::CapacityExceeded {
                arena,
                attempted_index,
                ..
            } => write!(
                formatter,
                "{arena} exceeded its u32 identity capacity at {attempted_index}"
            ),
            Self::UnknownId { kind, index, .. } => {
                write!(formatter, "unknown {kind} identity {index}")
            }
            Self::TypeMismatch {
                expected, found, ..
            } => write!(
                formatter,
                "expression type mismatch: expected {expected:?}, found {found:?}"
            ),
            Self::ShapeMismatch { .. } => formatter.write_str("expression shape mismatch"),
            Self::ExpectedScalar { .. } => formatter.write_str("expected a scalar expression"),
            Self::ExpectedNumeric { found, .. } => {
                write!(formatter, "expected a numeric expression, found {found:?}")
            }
            Self::InvalidArity {
                expected, found, ..
            } => write!(
                formatter,
                "invalid expression arity: expected {expected}, found {found}"
            ),
            Self::EmptyArray { .. } => formatter.write_str("empty array needs an explicit type"),
            Self::ZeroRangeStep { .. } => formatter.write_str("range step cannot be zero"),
            Self::InvalidSubscript { .. } => formatter.write_str("invalid array subscript"),
            Self::DuplicateDefinition { kind, index, .. } => {
                write!(
                    formatter,
                    "duplicate {kind} definition for identity {index}"
                )
            }
            Self::IncompleteDefinition { kind, index, .. } => {
                write!(formatter, "missing {kind} definition for identity {index}")
            }
            Self::InvalidSchemaVersion { expected, found } => write!(
                formatter,
                "unsupported checked DAE schema version {found}; expected {expected}"
            ),
        }
    }
}

impl std::error::Error for DaeConstructionError {}
