#[cfg(test)]
mod tests;

use std::fmt;

use rumoca_core::{ClockLatticeErrorKind, Span, StructuredIndexDomainError, TypeId, VarName};

use crate::{DaeProvenanceOrigin, ScalarType};

/// Failure to construct or decode the DAE.
#[derive(Debug, Clone, PartialEq, thiserror::Error)]
pub enum DaeConstructionError {
    #[error("missing source provenance for {origin}")]
    MissingProvenance {
        origin: DaeProvenanceOrigin,
        attempted_span: Span,
    },
    #[error("DAE provenance references an unknown source: {span:?}")]
    UnknownSource { span: Span },
    #[error("DAE provenance range {span:?} is invalid for source length {source_len}")]
    InvalidSourceRange { span: Span, source_len: usize },
    #[error("invalid effective Flat type identity {type_id}")]
    InvalidEffectiveTypeId { type_id: TypeId, span: Span },
    #[error("invalid structured DAE domain: {source}")]
    InvalidDomain {
        #[source]
        source: StructuredIndexDomainError,
        span: Span,
    },
    #[error("invalid exact DAE clock value: {source}")]
    InvalidClockLattice {
        #[source]
        source: ClockLatticeErrorKind,
        span: Span,
    },
    #[error("{arena} exceeded its u32 identity capacity at {attempted_index}")]
    CapacityExceeded {
        arena: &'static str,
        attempted_index: usize,
        span: Span,
    },
    #[error("unknown {kind} identity {index}")]
    UnknownId {
        kind: &'static str,
        index: u32,
        span: Span,
    },
    #[error("expression type mismatch: expected {expected:?}, found {found:?}")]
    TypeMismatch {
        expected: ScalarType,
        found: ScalarType,
        span: Span,
    },
    #[error("expression shape mismatch")]
    ShapeMismatch { span: Span },
    #[error("expected a scalar expression")]
    ExpectedScalar { span: Span },
    #[error("expected a numeric expression, found {found:?}")]
    ExpectedNumeric { found: ScalarType, span: Span },
    #[error("expected a primitive relational expression")]
    ExpectedPrimitiveRelation { span: Span },
    #[error("invalid expression arity: expected {expected}, found {found}")]
    InvalidArity {
        expected: usize,
        found: usize,
        span: Span,
    },
    #[error("empty array needs an explicit type")]
    EmptyArray { span: Span },
    #[error("range step cannot be zero")]
    ZeroRangeStep { span: Span },
    #[error("range extent exceeds the DAE u32 domain")]
    RangeExtentOverflow { span: Span },
    #[error("array extent must be a nonnegative literal Integer")]
    InvalidArrayExtent { span: Span },
    #[error("expected a finite, strictly-positive parameter expression")]
    InvalidPositiveParameter { span: Span },
    #[error(
        "discontinuous builtin `{operator}` requires statically computable operands until it has a checked event owner"
    )]
    NonStaticDiscontinuity { operator: &'static str, span: Span },
    #[error("builtin `{operator}` operands are outside the defined numeric domain")]
    UndefinedBuiltinDomain { operator: &'static str, span: Span },
    #[error("invalid array subscript")]
    InvalidSubscript { span: Span },
    #[error("invalid one-based enumeration ordinal {ordinal}")]
    InvalidEnumerationOrdinal { ordinal: i64, span: Span },
    #[error("{}", binder_scope(.expected_domain, .found_domain))]
    InvalidBinderScope {
        expected_domain: Option<u32>,
        found_domain: u32,
        span: Span,
    },
    #[error("{}", function_scope(.expected_function, .found_function))]
    InvalidFunctionScope {
        expected_function: Option<u32>,
        found_function: u32,
        span: Span,
    },
    #[error(
        "function value {value} reads definition {found_definition}, expected {expected_definition:?}"
    )]
    InvalidFunctionValueRead {
        value: u32,
        expected_definition: Option<u32>,
        found_definition: u32,
        span: Span,
    },
    #[error("model coordinate `{coordinate}` cannot be captured by a pure function")]
    InvalidFunctionCoordinate {
        coordinate: &'static str,
        span: Span,
    },
    #[error("variable `{name}` has the wrong DAE coordinate role")]
    InvalidVariableRole { name: VarName, span: Span },
    #[error("duplicate {kind} definition for identity {index}")]
    DuplicateDefinition {
        kind: &'static str,
        index: u32,
        span: Span,
    },
    #[error("duplicate {kind} construction")]
    DuplicateTopology {
        kind: &'static str,
        span: Option<Span>,
    },
    #[error("duplicate {kind} key `{key}`")]
    DuplicateKey {
        kind: &'static str,
        key: String,
        span: Span,
    },
    #[error("variable identity {variable} is not owned by clock identity {clock}")]
    MissingClockOwnership {
        variable: u32,
        clock: u32,
        span: Span,
    },
    #[error("invalid B.1c topology plan at discrete-value target identity {target}")]
    InvalidDiscreteTopologyPlan { target: u32, span: Span },
    #[error("B.1c owner target order mismatch: expected {expected:?}, found {found:?}")]
    InvalidDiscreteTargetOrder {
        expected: Option<u32>,
        found: Option<u32>,
        span: Span,
    },
    #[error("B.1c owner must contain at least one target and one branch")]
    EmptyDiscreteValueOwner { span: Span },
    #[error("an unconditional B.1c owner must contain exactly one `always` branch")]
    InvalidDiscreteBranchSet { span: Span },
    #[error(
        "B.1c target identity {target} reads not-yet-issued current discrete value {dependency}"
    )]
    UnissuedDiscreteDependency {
        target: u32,
        dependency: u32,
        span: Span,
    },
    #[error("missing {kind} definition for identity {index}")]
    IncompleteDefinition {
        kind: &'static str,
        index: u32,
        span: Span,
    },
    #[error("unsupported DAE schema version {found}; expected {expected}")]
    InvalidSchemaVersion { expected: u16, found: u16 },
    #[error("malformed DAE wire column `{column}`")]
    MalformedWire { column: &'static str },
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum ScopeViolation {
    Binder(Option<u32>, u32),
    Function(Option<u32>, u32),
}

fn binder_scope(expected: &Option<u32>, found: &u32) -> ScopeViolation {
    ScopeViolation::Binder(*expected, *found)
}

fn function_scope(expected: &Option<u32>, found: &u32) -> ScopeViolation {
    ScopeViolation::Function(*expected, *found)
}

impl fmt::Display for ScopeViolation {
    fn fmt(&self, formatter: &mut fmt::Formatter<'_>) -> fmt::Result {
        match *self {
            Self::Binder(Some(expected), found) => write!(
                formatter,
                "domain binder from {found} cannot be used in domain {expected}"
            ),
            Self::Binder(None, found) => write!(
                formatter,
                "domain binder from {found} escaped its structured owner"
            ),
            Self::Function(Some(expected), found) => write!(
                formatter,
                "parameter from function {found} cannot be used in function {expected}"
            ),
            Self::Function(None, found) => write!(
                formatter,
                "parameter from function {found} escaped its function owner"
            ),
        }
    }
}

impl DaeConstructionError {
    /// Source owner for construction failures that arise from a semantic add.
    ///
    /// Schema-version and malformed-column failures are wire-container errors.
    /// Reusing an already-consumed empty topology capability is also source-free:
    /// there is no semantic owner from which an honest span could be obtained.
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
            | Self::NonStaticDiscontinuity { span, .. }
            | Self::UndefinedBuiltinDomain { span, .. }
            | Self::InvalidSubscript { span }
            | Self::InvalidEnumerationOrdinal { span, .. }
            | Self::InvalidBinderScope { span, .. }
            | Self::InvalidFunctionScope { span, .. }
            | Self::InvalidFunctionValueRead { span, .. }
            | Self::InvalidFunctionCoordinate { span, .. }
            | Self::InvalidVariableRole { span, .. }
            | Self::DuplicateDefinition { span, .. }
            | Self::DuplicateKey { span, .. }
            | Self::MissingClockOwnership { span, .. }
            | Self::InvalidDiscreteTopologyPlan { span, .. }
            | Self::InvalidDiscreteTargetOrder { span, .. }
            | Self::EmptyDiscreteValueOwner { span }
            | Self::InvalidDiscreteBranchSet { span }
            | Self::UnissuedDiscreteDependency { span, .. }
            | Self::IncompleteDefinition { span, .. } => Some(*span),
            Self::DuplicateTopology { span, .. } => *span,
            Self::InvalidSchemaVersion { .. } | Self::MalformedWire { .. } => None,
        }
    }
}
