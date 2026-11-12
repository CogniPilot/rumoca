//! The failures of exact scalar projection and how each one is reported.

use rumoca_core::Span;
use rumoca_ir_dae as dae;

/// Failure to derive one exact scalar view from a checked DAE expression.
#[derive(Debug, Clone, PartialEq, thiserror::Error)]
pub enum ProjectionError {
    #[error("scalar projection {index} is outside an expression containing {count} scalars")]
    ScalarOutOfBounds {
        index: usize,
        count: usize,
        span: Span,
    },
    #[error("array subscript is not compile-time computable")]
    DynamicSubscript { span: Span },
    #[error("Modelica index {index} is outside an axis of extent {extent}")]
    IndexOutOfBounds { index: i64, extent: u32, span: Span },
    #[error("integer evaluation overflowed during scalar projection")]
    IntegerOverflow { span: Span },
    #[error("function scalar projection exceeded the checked recursion limit")]
    FunctionRecursion { span: Span },
    #[error(
        "record field `{target}` cannot be read one field at a time: its value comes from {operation}, which this compiler does not project per record field"
    )]
    UnsupportedRecordOperation {
        target: String,
        operation: &'static str,
        span: Span,
    },
    #[error(
        "external {language} function `{name}` calls `{symbol}`, which this runtime cannot execute"
    )]
    ExternalFunction {
        name: String,
        language: &'static str,
        symbol: String,
        span: Span,
    },
}

impl ProjectionError {
    /// The source span retained by the exact scalar projection owner.
    #[must_use]
    pub const fn span(&self) -> Span {
        match self {
            Self::ScalarOutOfBounds { span, .. }
            | Self::DynamicSubscript { span }
            | Self::IndexOutOfBounds { span, .. }
            | Self::IntegerOverflow { span }
            | Self::FunctionRecursion { span }
            | Self::UnsupportedRecordOperation { span, .. }
            | Self::ExternalFunction { span, .. } => *span,
        }
    }
}

/// Report an MLS §12.9 external body that projection cannot look through.
///
/// Projection resolves a call by continuing into the callee's result
/// definition. An external body has none, so the exact interface is reported
/// instead of a silently empty incidence set.
pub(super) fn external_projection_error(
    definition: dae::FunctionView<'_>,
    external: dae::ExternalFunctionView<'_>,
    span: Span,
) -> ProjectionError {
    ProjectionError::ExternalFunction {
        name: definition.name().to_string(),
        language: external.language().as_str(),
        symbol: external.symbol().to_string(),
        span,
    }
}

/// Report a record value whose definition projection cannot split per field.
///
/// The rejected value is named the way the source names it, `Record.field`
/// where the DAE retained both names, so a reader can find the declaration
/// without reading IR. The definition form is named too, because that is what
/// decides whether the source has to change or the compiler does.
pub(super) fn unsupported_record_operation(
    node: dae::ExpressionView<'_>,
    field: usize,
) -> ProjectionError {
    let value_type = node.value_type();
    let target = match (
        value_type.record_name(),
        value_type.record_field_name(field),
    ) {
        (Some(record), Some(name)) => format!("{record}.{name}"),
        (Some(record), None) => format!("{record} field {field}"),
        (None, Some(name)) => name.to_string(),
        (None, None) => format!("field {field}"),
    };
    ProjectionError::UnsupportedRecordOperation {
        target,
        operation: record_definition_form(node.operation()),
        span: node.provenance().span(),
    }
}

/// Name the definition form a record field projection could not look through.
const fn record_definition_form(operation: dae::ExpressionOperation<'_>) -> &'static str {
    match operation {
        dae::ExpressionOperation::Field { .. } => "a field of an enclosing record",
        dae::ExpressionOperation::Coordinate(_) => "a coordinate of the model",
        dae::ExpressionOperation::Builtin { .. } => "a built-in operator result",
        dae::ExpressionOperation::Binary { .. } | dae::ExpressionOperation::Unary { .. } => {
            "an arithmetic result"
        }
        dae::ExpressionOperation::ClockTransfer { .. } => "a clocked transfer",
        _ => "a definition form record projection does not cover",
    }
}
