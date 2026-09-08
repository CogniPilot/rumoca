//! Construction-time gate for execution reuse.
//!
//! Execution reuse (as distinct from pure-term storage) is not implemented in
//! the checked Solve vocabulary yet.  Keeping the request and the checked
//! result separate makes that fact explicit at an API boundary: a future
//! caller cannot turn an unchecked `Selected` request into an executable
//! product by carrying a boolean or an unvalidated enum through the pipeline.
//!
//! This gate is deliberately about execution reuse only.  It does not forbid
//! term interning or shared term storage, which are separate decisions.

/// Whether a caller asks the current Solve implementation to eliminate an
/// execution.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ExecutionReuseRequest {
    /// Keep every execution owner; no execution-CSE is requested.
    NotSelected,
    /// Request execution reuse.  This is currently unsupported.
    Selected,
}

/// The only execution-reuse states a checked Solve product may carry today.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ExecutionReuseSelection {
    /// Execution reuse was not selected for this product.
    NotSelected,
    /// Execution reuse was selected but has no implementation yet.
    NotImplemented,
}

/// Failure raised when an unsupported execution-reuse request crosses the
/// checked construction boundary.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ExecutionReuseError {
    /// The caller requested execution reuse before its proof obligations and
    /// scheduling implementation existed.
    SelectedNotImplemented,
}

impl std::fmt::Display for ExecutionReuseError {
    fn fmt(&self, formatter: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::SelectedNotImplemented => {
                formatter.write_str("execution reuse is not implemented")
            }
        }
    }
}

impl std::error::Error for ExecutionReuseError {}

/// Check the execution-reuse request at the checked construction boundary.
pub fn check_execution_reuse(
    request: ExecutionReuseRequest,
) -> Result<ExecutionReuseSelection, ExecutionReuseError> {
    match request {
        ExecutionReuseRequest::NotSelected => Ok(ExecutionReuseSelection::NotSelected),
        ExecutionReuseRequest::Selected => Err(ExecutionReuseError::SelectedNotImplemented),
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn unselected_execution_reuse_is_the_checked_state() {
        assert_eq!(
            check_execution_reuse(ExecutionReuseRequest::NotSelected),
            Ok(ExecutionReuseSelection::NotSelected)
        );
    }

    #[test]
    fn selected_execution_reuse_fails_closed() {
        assert_eq!(
            check_execution_reuse(ExecutionReuseRequest::Selected),
            Err(ExecutionReuseError::SelectedNotImplemented)
        );
    }

    #[test]
    fn error_is_actionable() {
        assert_eq!(
            ExecutionReuseError::SelectedNotImplemented.to_string(),
            "execution reuse is not implemented"
        );
    }
}
