#[derive(Debug, Clone, PartialEq, Eq)]
pub enum LowerError {
    Unsupported {
        reason: String,
    },
    UnsupportedAt {
        reason: String,
        contexts: Vec<String>,
        span: rumoca_core::Span,
    },
    MissingBinding {
        name: String,
    },
    MissingFunction {
        name: String,
    },
    InvalidFunction {
        name: String,
        reason: String,
    },
    ContractViolation {
        reason: String,
        span: rumoca_core::Span,
    },
    UnspannedContractViolation {
        reason: String,
    },
    Scalarization {
        message: String,
        span: Option<rumoca_core::Span>,
    },
    /// A function projection produced an expression larger than the inline
    /// node budget. Recoverable only at the outermost projection boundary,
    /// where the original runtime call is the semantically equivalent
    /// lowering; everywhere else this propagates like any other error.
    ProjectionBudgetExceeded {
        function: String,
        span: rumoca_core::Span,
    },
    /// A subscript whose value is only known at runtime, in a position that
    /// requires a compile-time index. Observation lowering declines on this.
    DynamicSubscript,
    /// `size()` in a for-loop range over a dimension with no structural
    /// binding. Observation lowering declines on this.
    ForRangeUnknownDimension {
        name: String,
    },
    /// A function/constructor input with neither an actual argument nor a
    /// default binding. Observation lowering declines on this.
    MissingActualArgument {
        function: String,
        /// What kind of input: "required input", "constructor input",
        /// "record constructor field".
        what: &'static str,
        input: String,
        span: rumoca_core::Span,
    },
    /// A dynamic-binding path rooted in an expression form that has no
    /// binding key. Observation lowering declines on this.
    DynamicBindingBase {
        tag: String,
    },
    Spanned {
        source: Box<LowerError>,
        span: rumoca_core::Span,
    },
    /// Context frames accumulated around an error that must keep its typed
    /// identity (e.g. `MissingBinding` inside a binding-row lowering).
    WithContext {
        source: Box<LowerError>,
        contexts: Vec<String>,
    },
}

impl std::fmt::Display for LowerError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Unsupported { .. } | Self::UnsupportedAt { .. } => {
                write!(f, "unsupported expression: {}", self.reason())
            }
            Self::MissingBinding { name } => write!(f, "missing variable binding: {name}"),
            Self::MissingFunction { name } => write!(f, "missing function definition: {name}"),
            Self::InvalidFunction { name, reason } => {
                write!(f, "invalid function `{name}`: {reason}")
            }
            Self::ContractViolation { reason, .. }
            | Self::UnspannedContractViolation { reason } => {
                write!(f, "invalid IR contract: {reason}")
            }
            Self::Scalarization { message, .. } => {
                write!(f, "Solve-IR scalarization failed: {message}")
            }
            Self::ProjectionBudgetExceeded { function, .. } => write!(
                f,
                "function `{function}` projection exceeded the inline node budget"
            ),
            Self::DynamicSubscript
            | Self::ForRangeUnknownDimension { .. }
            | Self::DynamicBindingBase { .. } => {
                write!(f, "unsupported expression: {}", self.reason())
            }
            Self::MissingActualArgument { .. } => write!(f, "{}", self.reason()),
            Self::Spanned { source, .. } => source.fmt(f),
            Self::WithContext { .. } => write!(f, "{}", self.reason()),
        }
    }
}

impl std::error::Error for LowerError {}

impl From<rumoca_eval_solve::ScalarizeError> for LowerError {
    fn from(value: rumoca_eval_solve::ScalarizeError) -> Self {
        Self::Scalarization {
            message: value.to_string(),
            span: value.source_span(),
        }
    }
}

impl From<rumoca_ir_solve::SolveProblemShapeContractError> for LowerError {
    fn from(value: rumoca_ir_solve::SolveProblemShapeContractError) -> Self {
        let reason = value.to_string();
        match value.source_span() {
            Some(span) if !span.is_dummy() => Self::ContractViolation { reason, span },
            Some(_) | None => Self::UnspannedContractViolation { reason },
        }
    }
}

impl From<rumoca_core::MissingProvenanceSpan> for LowerError {
    fn from(value: rumoca_core::MissingProvenanceSpan) -> Self {
        Self::UnspannedContractViolation {
            reason: value.to_string(),
        }
    }
}

impl LowerError {
    pub(crate) fn contract_violation(reason: impl Into<String>, span: rumoca_core::Span) -> Self {
        let reason = reason.into();
        if span.is_dummy() {
            Self::UnspannedContractViolation { reason }
        } else {
            Self::ContractViolation { reason, span }
        }
    }

    pub fn reason(&self) -> String {
        match self {
            Self::Unsupported { reason } => reason.clone(),
            Self::UnsupportedAt {
                reason, contexts, ..
            } => contextual_reason(contexts, reason),
            Self::MissingBinding { name } => format!("missing variable binding `{name}`"),
            Self::MissingFunction { name } => format!("missing function definition `{name}`"),
            Self::InvalidFunction { name, reason } => {
                format!("invalid function `{name}`: {reason}")
            }
            Self::ContractViolation { reason, .. }
            | Self::UnspannedContractViolation { reason } => {
                format!("invalid IR contract: {reason}")
            }
            Self::Scalarization { message, .. } => {
                format!("Solve-IR scalarization failed: {message}")
            }
            Self::ProjectionBudgetExceeded { function, .. } => {
                format!("function `{function}` projection exceeded the inline node budget")
            }
            Self::DynamicSubscript => "dynamic subscript expressions are unsupported".to_string(),
            Self::ForRangeUnknownDimension { name } => {
                format!("size() in for-loop range requires known dimension `{name}`")
            }
            Self::MissingActualArgument {
                function,
                what,
                input,
                ..
            } => format!(
                "invalid function `{function}`: {what} `{input}` has no actual argument or default binding"
            ),
            Self::DynamicBindingBase { tag } => {
                format!("unsupported base expression for dynamic binding path: {tag}")
            }
            Self::Spanned { source, .. } => source.reason(),
            Self::WithContext { source, contexts } => contextual_reason(contexts, &source.reason()),
        }
    }

    pub fn label_reason(&self) -> String {
        match self {
            Self::Unsupported { reason } | Self::UnsupportedAt { reason, .. } => reason.clone(),
            Self::MissingBinding { name } => format!("missing variable binding `{name}`"),
            Self::MissingFunction { name } => format!("missing function definition `{name}`"),
            Self::InvalidFunction { name, reason } => {
                format!("invalid function `{name}`: {reason}")
            }
            Self::ContractViolation { reason, .. }
            | Self::UnspannedContractViolation { reason } => {
                format!("invalid IR contract: {reason}")
            }
            Self::Scalarization { message, .. } => {
                format!("Solve-IR scalarization failed: {message}")
            }
            Self::ProjectionBudgetExceeded { function, .. } => {
                format!("function `{function}` projection exceeded the inline node budget")
            }
            Self::DynamicSubscript
            | Self::ForRangeUnknownDimension { .. }
            | Self::MissingActualArgument { .. }
            | Self::DynamicBindingBase { .. } => self.reason(),
            Self::Spanned { source, .. } => source.label_reason(),
            Self::WithContext { source, .. } => source.label_reason(),
        }
    }

    pub fn source_span(&self) -> Option<rumoca_core::Span> {
        match self {
            Self::UnsupportedAt { span, .. } if !span.is_dummy() => Some(*span),
            Self::ContractViolation { span, .. } if !span.is_dummy() => Some(*span),
            Self::Scalarization { span, .. } => *span,
            Self::ProjectionBudgetExceeded { span, .. } if !span.is_dummy() => Some(*span),
            Self::MissingActualArgument { span, .. } if !span.is_dummy() => Some(*span),
            Self::Spanned { source, span } => source
                .source_span()
                .or_else(|| (!span.is_dummy()).then_some(*span)),
            Self::WithContext { source, .. } => source.source_span(),
            _ => None,
        }
    }

    /// The function name and span of a projection budget decline, looking
    /// through span wrappers.
    pub fn projection_budget_exceeded_parts(&self) -> Option<(&str, rumoca_core::Span)> {
        match self {
            Self::ProjectionBudgetExceeded { function, span } => Some((function, *span)),
            Self::Spanned { source, .. } | Self::WithContext { source, .. } => {
                source.projection_budget_exceeded_parts()
            }
            _ => None,
        }
    }

    /// True for a projection budget decline, looking through span wrappers.
    pub fn is_projection_budget_exceeded(&self) -> bool {
        self.projection_budget_exceeded_parts().is_some()
    }

    pub fn with_fallback_span(self, span: rumoca_core::Span) -> Self {
        if span.is_dummy() || self.source_span().is_some() {
            return self;
        }
        match self {
            Self::Unsupported { reason } => LowerError::UnsupportedAt {
                reason,
                contexts: Vec::new(),
                span,
            },
            other => LowerError::Spanned {
                source: Box::new(other),
                span,
            },
        }
    }

    pub fn with_context(self, context: impl Into<String>) -> Self {
        let context = context.into();
        match self {
            Self::Unsupported { reason } => Self::Unsupported {
                reason: format!("{context}: {reason}"),
            },
            Self::UnsupportedAt {
                reason,
                mut contexts,
                span,
            } => {
                contexts.push(context);
                Self::UnsupportedAt {
                    reason,
                    contexts,
                    span,
                }
            }
            Self::Spanned { source, span } => Self::Spanned {
                source: Box::new(source.with_context(context)),
                span,
            },
            Self::ContractViolation { reason, span } => Self::ContractViolation {
                reason: format!("{context}: {reason}"),
                span,
            },
            Self::UnspannedContractViolation { reason } => Self::UnspannedContractViolation {
                reason: format!("{context}: {reason}"),
            },
            Self::WithContext {
                source,
                mut contexts,
            } => {
                contexts.push(context);
                Self::WithContext { source, contexts }
            }
            // Everything else keeps its typed identity (so downstream
            // classification such as observation skips and the projection
            // boundary stays variant-based, never message-string-based) and
            // accumulates the context as a wrapper frame.
            other => Self::WithContext {
                source: Box::new(other),
                contexts: vec![context],
            },
        }
    }
}

pub(super) fn unsupported_at(reason: impl Into<String>, span: rumoca_core::Span) -> LowerError {
    let reason = reason.into();
    if span.is_dummy() {
        LowerError::Unsupported { reason }
    } else {
        LowerError::UnsupportedAt {
            reason,
            contexts: Vec::new(),
            span,
        }
    }
}

fn contextual_reason(contexts: &[String], reason: &str) -> String {
    contexts
        .iter()
        .rev()
        .map(String::as_str)
        .chain(std::iter::once(reason))
        .collect::<Vec<_>>()
        .join(": ")
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn contract_violation_preserves_real_span() {
        let span = rumoca_core::Span::from_offsets(rumoca_core::SourceId(3), 4, 9);
        let err = LowerError::contract_violation("metadata mismatch", span);

        assert_eq!(err.source_span(), Some(span));
        assert_eq!(err.reason(), "invalid IR contract: metadata mismatch");
    }

    #[test]
    fn contract_violation_does_not_fabricate_dummy_span() {
        let err = LowerError::contract_violation("metadata mismatch", rumoca_core::Span::DUMMY);

        assert_eq!(err.source_span(), None);
        assert_eq!(err.reason(), "invalid IR contract: metadata mismatch");
    }

    #[test]
    fn fallback_span_preserves_invalid_function_context() {
        let span = rumoca_core::Span::from_offsets(rumoca_core::SourceId(7), 11, 19);
        let err = LowerError::InvalidFunction {
            name: "Pkg.f".to_string(),
            reason: "required input `x` has no actual argument".to_string(),
        }
        .with_fallback_span(span);

        assert_eq!(err.source_span(), Some(span));
        assert_eq!(
            err.reason(),
            "invalid function `Pkg.f`: required input `x` has no actual argument"
        );
    }

    #[test]
    fn scalarization_error_preserves_source_span() {
        let span = rumoca_core::Span::from_offsets(rumoca_core::SourceId(9), 21, 34);
        let err: LowerError = rumoca_eval_solve::ScalarizeError::InvalidStrideDimension {
            kind: "map",
            dimension: 3,
            dimension_count: 2,
            span,
        }
        .into();

        assert_eq!(err.source_span(), Some(span));
        assert_eq!(
            err.reason(),
            "Solve-IR scalarization failed: native map family stride dimension 3 out of bounds for 2 dimensions"
        );
    }
}
