use rumoca_core::{ClockLatticeErrorKind, Span, StructuredIndexDomainError, TypeId, VarName};

use crate::{DaeProvenanceOrigin, ScalarType};

/// Failure to construct or decode the DAE.
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
    InvalidEffectiveTypeId {
        type_id: TypeId,
        span: Span,
    },
    InvalidDomain {
        source: StructuredIndexDomainError,
        span: Span,
    },
    InvalidClockLattice {
        source: ClockLatticeErrorKind,
        span: Span,
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
    ExpectedPrimitiveRelation {
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
    RangeExtentOverflow {
        span: Span,
    },
    InvalidArrayExtent {
        span: Span,
    },
    InvalidPositiveParameter {
        span: Span,
    },
    InvalidSubscript {
        span: Span,
    },
    InvalidEnumerationOrdinal {
        ordinal: i64,
        span: Span,
    },
    InvalidBinderScope {
        expected_domain: Option<u32>,
        found_domain: u32,
        span: Span,
    },
    InvalidFunctionScope {
        expected_function: Option<u32>,
        found_function: u32,
        span: Span,
    },
    InvalidFunctionValueRead {
        value: u32,
        expected_definition: Option<u32>,
        found_definition: u32,
        span: Span,
    },
    InvalidVariableRole {
        name: VarName,
        span: Span,
    },
    DuplicateDefinition {
        kind: &'static str,
        index: u32,
        span: Span,
    },
    DuplicateKey {
        kind: &'static str,
        key: String,
        span: Span,
    },
    MissingClockOwnership {
        variable: u32,
        clock: u32,
        span: Span,
    },
    InvalidDiscreteDependencyCycle {
        target: u32,
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
    MalformedWire {
        column: &'static str,
    },
}

impl std::fmt::Display for DaeConstructionError {
    fn fmt(&self, formatter: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::MissingProvenance { .. }
            | Self::UnknownSource { .. }
            | Self::InvalidSourceRange { .. } => format_provenance_error(self, formatter),
            Self::InvalidEffectiveTypeId { type_id, .. } => {
                write!(formatter, "invalid effective Flat type identity {type_id}")
            }
            Self::InvalidDomain { source, .. } => {
                write!(formatter, "invalid structured DAE domain: {source}")
            }
            Self::InvalidClockLattice { source, .. } => {
                write!(formatter, "invalid exact DAE clock value: {source}")
            }
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
            Self::ExpectedPrimitiveRelation { .. } => {
                formatter.write_str("expected a primitive relational expression")
            }
            Self::InvalidArity {
                expected, found, ..
            } => format_arity(formatter, *expected, *found),
            Self::EmptyArray { .. } => formatter.write_str("empty array needs an explicit type"),
            Self::ZeroRangeStep { .. } => formatter.write_str("range step cannot be zero"),
            Self::RangeExtentOverflow { .. } => {
                formatter.write_str("range extent exceeds the DAE u32 domain")
            }
            Self::InvalidArrayExtent { .. } => {
                formatter.write_str("array extent must be a nonnegative literal Integer")
            }
            Self::InvalidPositiveParameter { .. } => {
                formatter.write_str("expected a finite, strictly-positive parameter expression")
            }
            Self::InvalidSubscript { .. } => formatter.write_str("invalid array subscript"),
            Self::InvalidEnumerationOrdinal { ordinal, .. } => {
                format_enumeration_ordinal(formatter, *ordinal)
            }
            Self::InvalidBinderScope {
                expected_domain,
                found_domain,
                ..
            } => format_binder_scope(formatter, *expected_domain, *found_domain),
            Self::InvalidFunctionScope {
                expected_function,
                found_function,
                ..
            } => format_function_scope(formatter, *expected_function, *found_function),
            Self::InvalidFunctionValueRead { .. } => format_function_value_read(self, formatter),
            Self::InvalidVariableRole { name, .. } => {
                write!(
                    formatter,
                    "variable `{name}` has the wrong DAE coordinate role"
                )
            }
            Self::DuplicateDefinition { kind, index, .. } => {
                format_duplicate(formatter, kind, *index)
            }
            Self::DuplicateKey { kind, key, .. } => {
                write!(formatter, "duplicate {kind} key `{key}`")
            }
            Self::MissingClockOwnership {
                variable, clock, ..
            } => format_clock_ownership(formatter, *variable, *clock),
            Self::InvalidDiscreteDependencyCycle { target, .. } => write!(
                formatter,
                "discrete-value target identity {target} has a cyclic current-value dependency"
            ),
            Self::IncompleteDefinition { kind, index, .. } => {
                write!(formatter, "missing {kind} definition for identity {index}")
            }
            Self::InvalidSchemaVersion { expected, found } => write!(
                formatter,
                "unsupported DAE schema version {found}; expected {expected}"
            ),
            Self::MalformedWire { column } => {
                write!(formatter, "malformed DAE wire column `{column}`")
            }
        }
    }
}

impl std::error::Error for DaeConstructionError {}

fn format_provenance_error(
    error: &DaeConstructionError,
    formatter: &mut std::fmt::Formatter<'_>,
) -> std::fmt::Result {
    match error {
        DaeConstructionError::MissingProvenance { origin, .. } => {
            write!(formatter, "missing source provenance for {origin}")
        }
        DaeConstructionError::UnknownSource { span } => {
            write!(
                formatter,
                "DAE provenance references an unknown source: {span:?}"
            )
        }
        DaeConstructionError::InvalidSourceRange { span, source_len } => write!(
            formatter,
            "DAE provenance range {span:?} is invalid for source length {source_len}"
        ),
        _ => unreachable!("provenance formatting receives only provenance errors"),
    }
}

fn format_function_value_read(
    error: &DaeConstructionError,
    formatter: &mut std::fmt::Formatter<'_>,
) -> std::fmt::Result {
    let DaeConstructionError::InvalidFunctionValueRead {
        value,
        expected_definition,
        found_definition,
        ..
    } = error
    else {
        unreachable!("function-value formatting receives its matching error")
    };
    write!(
        formatter,
        "function value {value} reads definition {found_definition}, expected {expected_definition:?}"
    )
}

fn format_binder_scope(
    formatter: &mut std::fmt::Formatter<'_>,
    expected_domain: Option<u32>,
    found_domain: u32,
) -> std::fmt::Result {
    match expected_domain {
        Some(expected) => write!(
            formatter,
            "domain binder from {found_domain} cannot be used in domain {expected}"
        ),
        None => write!(
            formatter,
            "domain binder from {found_domain} escaped its structured owner"
        ),
    }
}

fn format_function_scope(
    formatter: &mut std::fmt::Formatter<'_>,
    expected_function: Option<u32>,
    found_function: u32,
) -> std::fmt::Result {
    match expected_function {
        Some(expected) => write!(
            formatter,
            "parameter from function {found_function} cannot be used in function {expected}"
        ),
        None => write!(
            formatter,
            "parameter from function {found_function} escaped its function owner"
        ),
    }
}

fn format_arity(
    formatter: &mut std::fmt::Formatter<'_>,
    expected: usize,
    found: usize,
) -> std::fmt::Result {
    write!(
        formatter,
        "invalid expression arity: expected {expected}, found {found}"
    )
}

fn format_enumeration_ordinal(
    formatter: &mut std::fmt::Formatter<'_>,
    ordinal: i64,
) -> std::fmt::Result {
    write!(formatter, "invalid one-based enumeration ordinal {ordinal}")
}

fn format_duplicate(
    formatter: &mut std::fmt::Formatter<'_>,
    kind: &'static str,
    index: u32,
) -> std::fmt::Result {
    write!(
        formatter,
        "duplicate {kind} definition for identity {index}"
    )
}

fn format_clock_ownership(
    formatter: &mut std::fmt::Formatter<'_>,
    variable: u32,
    clock: u32,
) -> std::fmt::Result {
    write!(
        formatter,
        "variable identity {variable} is not owned by clock identity {clock}"
    )
}

impl DaeConstructionError {
    /// Source owner for construction failures that arise from a semantic add.
    ///
    /// Schema-version and malformed-column failures are wire-container errors
    /// and therefore have no semantic owner span.
    pub const fn source_span(&self) -> Option<Span> {
        match self {
            Self::MissingProvenance { attempted_span, .. } => Some(*attempted_span),
            Self::UnknownSource { span }
            | Self::InvalidSourceRange { span, .. }
            | Self::InvalidEffectiveTypeId { span, .. }
            | Self::InvalidDomain { span, .. }
            | Self::InvalidClockLattice { span, .. }
            | Self::CapacityExceeded { span, .. }
            | Self::UnknownId { span, .. }
            | Self::TypeMismatch { span, .. }
            | Self::ShapeMismatch { span }
            | Self::ExpectedScalar { span }
            | Self::ExpectedNumeric { span, .. }
            | Self::ExpectedPrimitiveRelation { span }
            | Self::InvalidArity { span, .. }
            | Self::EmptyArray { span }
            | Self::ZeroRangeStep { span }
            | Self::RangeExtentOverflow { span }
            | Self::InvalidArrayExtent { span }
            | Self::InvalidPositiveParameter { span }
            | Self::InvalidSubscript { span }
            | Self::InvalidEnumerationOrdinal { span, .. }
            | Self::InvalidBinderScope { span, .. }
            | Self::InvalidFunctionScope { span, .. }
            | Self::InvalidFunctionValueRead { span, .. }
            | Self::InvalidVariableRole { span, .. }
            | Self::DuplicateDefinition { span, .. }
            | Self::DuplicateKey { span, .. }
            | Self::MissingClockOwnership { span, .. }
            | Self::InvalidDiscreteDependencyCycle { span, .. }
            | Self::IncompleteDefinition { span, .. } => Some(*span),
            Self::InvalidSchemaVersion { .. } | Self::MalformedWire { .. } => None,
        }
    }
}
