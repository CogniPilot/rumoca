use crate::{ScalarProgramRegisterError, VarLayoutShapeContractError};
use rumoca_core::{Span, StructuredIndexDomainError};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum AffineTensorNodeKind {
    Map,
    AffineStencil,
}

impl std::fmt::Display for AffineTensorNodeKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Map => f.write_str("Map"),
            Self::AffineStencil => f.write_str("AffineStencil"),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum SolveProblemShapeContractError {
    SchemaVersion {
        actual: u16,
        expected: u16,
    },
    Layout(VarLayoutShapeContractError),
    ScalarProgramSpanMismatch {
        context: String,
        node_index: usize,
        programs: usize,
        spans: usize,
        span: Option<Span>,
    },
    ScalarProgramMissingProvenance {
        context: String,
        node_index: usize,
        program_index: usize,
    },
    ScalarProgramOutputIndexMismatch {
        context: String,
        node_index: usize,
        programs: usize,
        output_indices: usize,
        span: Option<Span>,
    },
    ScalarProgramMissingOutput {
        context: String,
        node_index: usize,
        program_index: usize,
        span: Option<Span>,
    },
    ScalarProgramRegisterFlow {
        context: String,
        node_index: usize,
        program_index: usize,
        error: ScalarProgramRegisterError,
        span: Option<Span>,
    },
    ScalarProgramCountMismatch {
        context: &'static str,
        expected: usize,
        actual: usize,
        span: Option<Span>,
    },
    ZeroTensorDimension {
        context: String,
        node_index: usize,
        dimension: &'static str,
        span: Span,
    },
    StructuredIndexDomain {
        context: String,
        node_index: usize,
        dimension: &'static str,
        error: StructuredIndexDomainError,
        span: Span,
    },
    TensorOutputMapDimension {
        context: String,
        node_index: usize,
        dimension: &'static str,
        output_dimension: usize,
        domain_rank: usize,
        span: Span,
    },
    TensorOutputMapNegativeIndex {
        context: String,
        node_index: usize,
        dimension: &'static str,
        value: isize,
        span: Span,
    },
    AffineStrideDimension {
        context: String,
        node_index: usize,
        dimension: &'static str,
        stride_kind: &'static str,
        op_position: usize,
        stride_dimension: usize,
        domain_rank: usize,
        span: Span,
    },
    AffineStrideOperation {
        context: Box<str>,
        node_index: usize,
        node_kind: AffineTensorNodeKind,
        stride_kind: &'static str,
        op_position: usize,
        op_count: usize,
        expected: &'static str,
        actual: Option<&'static str>,
        span: Span,
    },
    NonFiniteAffineConstantStride {
        context: String,
        node_index: usize,
        dimension: &'static str,
        op_position: usize,
        stride_dimension: usize,
        span: Span,
    },
    AffineLoadIndexRange {
        context: String,
        node_index: usize,
        dimension: &'static str,
        op_position: usize,
        minimum: i128,
        maximum: i128,
        span: Span,
    },
    AffineLoadIndexOverflow {
        context: String,
        node_index: usize,
        dimension: &'static str,
        op_position: usize,
        span: Span,
    },
    OutputIndexOverflow {
        context: String,
        node_index: usize,
        span: Option<Span>,
    },
    SolverIndexOutOfBounds {
        context: &'static str,
        index: usize,
        upper_bound: usize,
        span: Option<Span>,
    },
    DuplicateIndex {
        context: &'static str,
        index: usize,
        span: Option<Span>,
    },
    ProjectionBlockShapeMismatch {
        context: &'static str,
        row_count: usize,
        unknown_count: usize,
        span: Option<Span>,
    },
    DuplicateProjectionUnknown {
        context: &'static str,
        unknown: String,
        span: Option<Span>,
    },
    InvalidProjectionUnknown {
        context: &'static str,
        unknown: String,
        y_upper_bound: usize,
        p_upper_bound: usize,
        span: Option<Span>,
    },
    InvalidScheduledRootTiming {
        context: &'static str,
        root_index: usize,
        span: Option<Span>,
    },
}

impl SolveProblemShapeContractError {
    pub fn source_span(&self) -> Option<Span> {
        match self {
            Self::SchemaVersion { .. } => None,
            Self::Layout(err) => err.source_span(),
            Self::ScalarProgramSpanMismatch { span, .. }
            | Self::ScalarProgramOutputIndexMismatch { span, .. }
            | Self::ScalarProgramMissingOutput { span, .. }
            | Self::ScalarProgramRegisterFlow { span, .. }
            | Self::ScalarProgramCountMismatch { span, .. }
            | Self::OutputIndexOverflow { span, .. }
            | Self::SolverIndexOutOfBounds { span, .. }
            | Self::DuplicateIndex { span, .. }
            | Self::ProjectionBlockShapeMismatch { span, .. }
            | Self::DuplicateProjectionUnknown { span, .. }
            | Self::InvalidProjectionUnknown { span, .. }
            | Self::InvalidScheduledRootTiming { span, .. } => *span,
            Self::ScalarProgramMissingProvenance { .. } => None,
            Self::ZeroTensorDimension { span, .. }
            | Self::StructuredIndexDomain { span, .. }
            | Self::TensorOutputMapDimension { span, .. }
            | Self::TensorOutputMapNegativeIndex { span, .. }
            | Self::AffineStrideDimension { span, .. }
            | Self::AffineStrideOperation { span, .. }
            | Self::NonFiniteAffineConstantStride { span, .. }
            | Self::AffineLoadIndexRange { span, .. }
            | Self::AffineLoadIndexOverflow { span, .. } => Some(*span),
        }
    }
}

impl std::fmt::Display for SolveProblemShapeContractError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::SchemaVersion { actual, expected } => write!(
                f,
                "Solve schema version {actual} does not match expected {expected}"
            ),
            Self::Layout(err) => write!(f, "Solve layout shape contract failed: {err}"),
            Self::ScalarProgramSpanMismatch {
                context,
                node_index,
                programs,
                spans,
                ..
            } => write!(
                f,
                "{context} node {node_index} has {programs} scalar programs but {spans} spans"
            ),
            Self::ScalarProgramMissingProvenance {
                context,
                node_index,
                program_index,
            } => write!(
                f,
                "{context} node {node_index} scalar program {program_index} has no source provenance"
            ),
            Self::ScalarProgramOutputIndexMismatch {
                context,
                node_index,
                programs,
                output_indices,
                ..
            } => write!(
                f,
                "{context} node {node_index} has {programs} scalar programs but \
                 {output_indices} output indices"
            ),
            Self::ScalarProgramMissingOutput {
                context,
                node_index,
                program_index,
                ..
            } => write!(
                f,
                "{context} node {node_index} scalar program {program_index} stores no output"
            ),
            Self::ScalarProgramRegisterFlow {
                context,
                node_index,
                program_index,
                error,
                ..
            } => write!(
                f,
                "{context} node {node_index} scalar program {program_index} has invalid register \
                 flow: {error}"
            ),
            Self::ScalarProgramCountMismatch {
                context,
                expected,
                actual,
                ..
            } => write!(f, "{context} expected {expected} rows, got {actual}"),
            error @ (Self::ZeroTensorDimension { .. }
            | Self::StructuredIndexDomain { .. }
            | Self::TensorOutputMapDimension { .. }
            | Self::TensorOutputMapNegativeIndex { .. }
            | Self::AffineStrideDimension { .. }
            | Self::AffineStrideOperation { .. }
            | Self::NonFiniteAffineConstantStride { .. }
            | Self::AffineLoadIndexRange { .. }
            | Self::AffineLoadIndexOverflow { .. }
            | Self::OutputIndexOverflow { .. }) => fmt_tensor_shape_contract_error(error, f),
            error => fmt_index_shape_contract_error(error, f),
        }
    }
}

fn fmt_tensor_shape_contract_error(
    error: &SolveProblemShapeContractError,
    f: &mut std::fmt::Formatter<'_>,
) -> std::fmt::Result {
    use SolveProblemShapeContractError as Error;
    match error {
        Error::ZeroTensorDimension {
            context,
            node_index,
            dimension,
            ..
        } => write!(
            f,
            "{context} node {node_index} has zero {dimension} tensor dimension"
        ),
        Error::StructuredIndexDomain {
            context,
            node_index,
            dimension,
            error,
            ..
        } => write!(
            f,
            "{context} node {node_index} {dimension} domain is invalid: {error}"
        ),
        Error::TensorOutputMapDimension {
            context,
            node_index,
            dimension,
            output_dimension,
            domain_rank,
            ..
        } => write!(
            f,
            "{context} node {node_index} {dimension} output map references dimension \
             {output_dimension}, but domain rank is {domain_rank}"
        ),
        Error::TensorOutputMapNegativeIndex {
            context,
            node_index,
            dimension,
            value,
            ..
        } => write!(
            f,
            "{context} node {node_index} {dimension} output map produced negative output index \
             {value}"
        ),
        Error::OutputIndexOverflow {
            context,
            node_index,
            ..
        } => write!(
            f,
            "{context} node {node_index} output index arithmetic overflowed"
        ),
        affine => fmt_affine_shape_contract_error(affine, f),
    }
}

fn fmt_affine_shape_contract_error(
    error: &SolveProblemShapeContractError,
    f: &mut std::fmt::Formatter<'_>,
) -> std::fmt::Result {
    use SolveProblemShapeContractError as Error;
    match error {
        Error::AffineStrideDimension {
            context,
            node_index,
            dimension,
            stride_kind,
            op_position,
            stride_dimension,
            domain_rank,
            ..
        } => write!(
            f,
            "{context} node {node_index} {dimension} {stride_kind} stride at op {op_position} \
             references dimension {stride_dimension}, but domain rank is {domain_rank}"
        ),
        Error::AffineStrideOperation {
            context,
            node_index,
            node_kind,
            stride_kind,
            op_position,
            op_count,
            expected,
            actual,
            ..
        } => write!(
            f,
            "{context} node {node_index} {node_kind} {stride_kind} stride at op {op_position} of \
             {op_count} targets {}, expected {expected}",
            actual.unwrap_or("missing op")
        ),
        Error::NonFiniteAffineConstantStride {
            context,
            node_index,
            dimension,
            op_position,
            stride_dimension,
            ..
        } => write!(
            f,
            "{context} node {node_index} {dimension} constant stride at op {op_position} \
             dimension {stride_dimension} is non-finite"
        ),
        Error::AffineLoadIndexRange {
            context,
            node_index,
            dimension,
            op_position,
            minimum,
            maximum,
            ..
        } => write!(
            f,
            "{context} node {node_index} {dimension} load stride at op {op_position} produces \
             host-incompatible index range {minimum}..={maximum}"
        ),
        Error::AffineLoadIndexOverflow {
            context,
            node_index,
            dimension,
            op_position,
            ..
        } => write!(
            f,
            "{context} node {node_index} {dimension} load stride at op {op_position} overflows \
             affine index arithmetic"
        ),
        _ => unreachable!("non-affine shape error was handled by the outer formatter"),
    }
}

fn fmt_index_shape_contract_error(
    error: &SolveProblemShapeContractError,
    f: &mut std::fmt::Formatter<'_>,
) -> std::fmt::Result {
    use SolveProblemShapeContractError as Error;
    match error {
        Error::SolverIndexOutOfBounds {
            context,
            index,
            upper_bound,
            ..
        } => write!(
            f,
            "{context} references solver index {index}, but upper bound is {upper_bound}"
        ),
        Error::DuplicateIndex { context, index, .. } => {
            write!(f, "{context} contains duplicate index {index}")
        }
        Error::ProjectionBlockShapeMismatch {
            context,
            row_count,
            unknown_count,
            ..
        } => write!(
            f,
            "{context} block has {row_count} residual rows but {unknown_count} unknowns"
        ),
        Error::DuplicateProjectionUnknown {
            context, unknown, ..
        } => write!(f, "{context} contains duplicate unknown {unknown}"),
        Error::InvalidProjectionUnknown {
            context,
            unknown,
            y_upper_bound,
            p_upper_bound,
            ..
        } => write!(
            f,
            "{context} contains invalid unknown {unknown}; Y upper bound is {y_upper_bound} and P \
             upper bound is {p_upper_bound}"
        ),
        Error::InvalidScheduledRootTiming {
            context,
            root_index,
            ..
        } => write!(f, "{context} root {root_index} has invalid periodic timing"),
        _ => unreachable!("non-index shape error was handled by the outer formatter"),
    }
}

impl std::error::Error for SolveProblemShapeContractError {}
