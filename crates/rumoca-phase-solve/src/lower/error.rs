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
    Spanned {
        source: Box<LowerError>,
        span: rumoca_core::Span,
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
            Self::ContractViolation { reason, .. } => {
                write!(f, "invalid IR contract: {reason}")
            }
            Self::Spanned { source, .. } => source.fmt(f),
        }
    }
}

impl std::error::Error for LowerError {}

impl LowerError {
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
            Self::ContractViolation { reason, .. } => format!("invalid IR contract: {reason}"),
            Self::Spanned { source, .. } => source.reason(),
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
            Self::ContractViolation { reason, .. } => format!("invalid IR contract: {reason}"),
            Self::Spanned { source, .. } => source.label_reason(),
        }
    }

    pub fn source_span(&self) -> Option<rumoca_core::Span> {
        match self {
            Self::UnsupportedAt { span, .. } if !span.is_dummy() => Some(*span),
            Self::ContractViolation { span, .. } if !span.is_dummy() => Some(*span),
            Self::Spanned { source, span } => source
                .source_span()
                .or_else(|| (!span.is_dummy()).then_some(*span)),
            _ => None,
        }
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
            Self::MissingBinding { name } => Self::Unsupported {
                reason: format!("{context}: missing variable binding `{name}`"),
            },
            Self::MissingFunction { name } => Self::Unsupported {
                reason: format!("{context}: missing function definition `{name}`"),
            },
            Self::InvalidFunction { name, reason } => Self::Unsupported {
                reason: format!("{context}: invalid function `{name}`: {reason}"),
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
}
