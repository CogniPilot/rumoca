use rumoca_ir_solve::{Reg, SolveProblemShapeContractError};

use crate::{ScalarizeError, tensor_index};

#[derive(Debug, Clone, PartialEq)]
pub enum EvalSolveError {
    CompiledExecution {
        message: String,
        span: Option<rumoca_core::Span>,
    },
    /// Native status carries an index fault without axis/index/extent details.
    NativeSourceIndexFault {
        message: String,
        span: Option<rumoca_core::Span>,
    },
    ExternalTable {
        operation: &'static str,
        table_id: f64,
        column: Option<f64>,
        reason: String,
    },
    MissingInput {
        vector: &'static str,
        index: usize,
        len: usize,
        span: Option<rumoca_core::Span>,
    },
    /// A one-based tensor axis did not contain an exact finite signed Integer.
    InvalidTensorIndex {
        axis: usize,
        value: f64,
        span: Option<rumoca_core::Span>,
    },
    /// A valid Integer lies outside this source tensor axis's checked extent.
    TensorIndexOutOfBounds {
        axis: usize,
        index: i64,
        extent: u32,
        span: Option<rumoca_core::Span>,
    },
    RegisterOutOfBounds {
        access: &'static str,
        register: Reg,
        len: usize,
        span: Option<rumoca_core::Span>,
    },
    UninitializedRegister {
        register: Reg,
        span: Option<rumoca_core::Span>,
    },
    OutputTooSmall {
        required: usize,
        len: usize,
        span: Option<rumoca_core::Span>,
    },
    UpdateRowTargetMismatch {
        rows: usize,
        targets: usize,
    },
    UpdateDidNotConverge {
        t: f64,
        max_iters: usize,
    },
    SingularTargetAssignment {
        row: usize,
        target_y_index: usize,
        coefficient: f64,
        span: Option<rumoca_core::Span>,
    },
    EventActionConditionMismatch {
        rows: usize,
        actions: usize,
    },
    MissingRuntimeState {
        operation: &'static str,
    },
    RandomStateProjectionOutOfBounds {
        index: usize,
        len: usize,
    },
    InvalidLinearOp {
        helper: &'static str,
        op: &'static str,
    },
    LinearSolve {
        size: usize,
        component: Option<usize>,
        reason: &'static str,
        span: Option<rumoca_core::Span>,
    },
    InvalidRow {
        message: String,
        span: Option<rumoca_core::Span>,
    },
    Scalarization {
        message: String,
        span: Option<rumoca_core::Span>,
    },
    ShapeContract {
        message: String,
        span: Option<rumoca_core::Span>,
    },
}

impl EvalSolveError {
    pub fn source_span(&self) -> Option<rumoca_core::Span> {
        match self {
            Self::CompiledExecution { span, .. } | Self::NativeSourceIndexFault { span, .. } => {
                *span
            }
            Self::MissingInput { span, .. } => *span,
            Self::RegisterOutOfBounds { span, .. } => *span,
            Self::InvalidTensorIndex { span, .. } | Self::TensorIndexOutOfBounds { span, .. } => {
                *span
            }
            Self::UninitializedRegister { span, .. } => *span,
            Self::OutputTooSmall { span, .. } => *span,
            Self::SingularTargetAssignment { span, .. } => *span,
            Self::LinearSolve { span, .. } => *span,
            Self::InvalidRow { span, .. } => *span,
            Self::Scalarization { span, .. } => *span,
            Self::ShapeContract { span, .. } => *span,
            _ => None,
        }
    }

    pub(crate) fn with_source_span(self, span: Option<rumoca_core::Span>) -> Self {
        match self {
            Self::CompiledExecution {
                message,
                span: None,
            } => Self::CompiledExecution { message, span },
            Self::NativeSourceIndexFault {
                message,
                span: None,
            } => Self::NativeSourceIndexFault { message, span },
            Self::MissingInput {
                vector,
                index,
                len,
                span: None,
            } => Self::MissingInput {
                vector,
                index,
                len,
                span,
            },
            Self::InvalidTensorIndex {
                axis,
                value,
                span: None,
            } => Self::InvalidTensorIndex { axis, value, span },
            Self::TensorIndexOutOfBounds {
                axis,
                index,
                extent,
                span: None,
            } => Self::TensorIndexOutOfBounds {
                axis,
                index,
                extent,
                span,
            },
            Self::RegisterOutOfBounds {
                access,
                register,
                len,
                span: None,
            } => Self::RegisterOutOfBounds {
                access,
                register,
                len,
                span,
            },
            Self::UninitializedRegister {
                register,
                span: None,
            } => Self::UninitializedRegister { register, span },
            Self::OutputTooSmall {
                required,
                len,
                span: None,
            } => Self::OutputTooSmall {
                required,
                len,
                span,
            },
            Self::SingularTargetAssignment {
                row,
                target_y_index,
                coefficient,
                span: None,
            } => Self::SingularTargetAssignment {
                row,
                target_y_index,
                coefficient,
                span,
            },
            Self::LinearSolve {
                size,
                component,
                reason,
                span: None,
            } => Self::LinearSolve {
                size,
                component,
                reason,
                span,
            },
            Self::InvalidRow {
                message,
                span: None,
            } => Self::InvalidRow { message, span },
            error => error,
        }
    }
}

impl std::fmt::Display for EvalSolveError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::CompiledExecution { message, .. } => {
                write!(f, "compiled execution failed: {message}")
            }
            Self::NativeSourceIndexFault { message, .. } => f.write_str(message),
            Self::ExternalTable {
                operation,
                table_id,
                column,
                reason,
            } => {
                if let Some(column) = column {
                    write!(
                        f,
                        "external table {operation} failed for table id {table_id} column {column}: {reason}"
                    )
                } else {
                    write!(
                        f,
                        "external table {operation} failed for table id {table_id}: {reason}"
                    )
                }
            }
            Self::MissingInput {
                vector, index, len, ..
            } => write!(
                f,
                "missing {vector}[{index}] while evaluating Solve-IR row; vector length is {len}"
            ),
            Self::InvalidTensorIndex { .. } | Self::TensorIndexOutOfBounds { .. } => {
                tensor_index::fmt_index_fault(self, f)
            }
            Self::RegisterOutOfBounds {
                access,
                register,
                len,
                ..
            } => write!(
                f,
                "cannot {access} Solve-IR register r{register}; register file length is {len}"
            ),
            Self::UninitializedRegister { register, .. } => {
                write!(f, "cannot read uninitialized Solve-IR register r{register}")
            }
            Self::OutputTooSmall { required, len, .. } => write!(
                f,
                "output buffer too small while evaluating Solve-IR row block: {len} < {required}"
            ),
            Self::UpdateRowTargetMismatch { rows, targets } => write!(
                f,
                "update RHS row count {rows} does not match target count {targets}"
            ),
            Self::UpdateDidNotConverge { t, max_iters } => write!(
                f,
                "update equations did not converge at t={t} after {max_iters} iterations"
            ),
            Self::SingularTargetAssignment {
                row,
                target_y_index,
                coefficient,
                ..
            } => write!(
                f,
                "cannot isolate target y[{target_y_index}] from Solve-IR row {row}: singular coefficient {coefficient}"
            ),
            Self::EventActionConditionMismatch { rows, actions } => write!(
                f,
                "event action condition row count {rows} does not match event action count {actions}"
            ),
            Self::MissingRuntimeState { operation } => write!(
                f,
                "missing simulation runtime state while evaluating Solve-IR {operation}"
            ),
            Self::RandomStateProjectionOutOfBounds { index, len } => write!(
                f,
                "random state projection index {index} is out of bounds for state length {len}"
            ),
            Self::InvalidLinearOp { helper, op } => {
                write!(f, "Solve-IR {helper} helper cannot evaluate {op} op")
            }
            Self::LinearSolve {
                size,
                component,
                reason,
                ..
            } => match component {
                Some(component) => write!(
                    f,
                    "Solve-IR linear solve of size {size} cannot evaluate component {component}: {reason}"
                ),
                None => write!(f, "Solve-IR linear solve of size {size} failed: {reason}"),
            },
            Self::InvalidRow { message, .. } => write!(f, "invalid Solve-IR row: {message}"),
            Self::Scalarization { message, .. } => {
                write!(f, "Solve-IR scalarization failed: {message}")
            }
            Self::ShapeContract { message, .. } => {
                write!(f, "Solve-IR shape contract failed: {message}")
            }
        }
    }
}

impl std::error::Error for EvalSolveError {}

impl From<ScalarizeError> for EvalSolveError {
    fn from(value: ScalarizeError) -> Self {
        Self::Scalarization {
            message: value.to_string(),
            span: value.source_span(),
        }
    }
}

impl From<SolveProblemShapeContractError> for EvalSolveError {
    fn from(value: SolveProblemShapeContractError) -> Self {
        Self::ShapeContract {
            message: value.to_string(),
            span: value.source_span(),
        }
    }
}
