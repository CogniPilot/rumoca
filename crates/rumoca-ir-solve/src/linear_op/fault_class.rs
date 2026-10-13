//! Fault classification of one linear operation (SPEC_0040 SOLVE-C102).
//!
//! The same `FaultClass` the typed program stores per operation: every pass
//! that drops an unread operation asks this one definition.

use super::LinearOp;
use crate::FaultClass;

impl LinearOp {
    /// Whether this operation is a pure register computation that cannot fail
    /// (`CannotFault`), or one that always runs: a call, a gather, a table or
    /// random operation, a conditional, a fold, a store (`HasEffect`).
    #[must_use]
    pub fn fault_class(&self) -> FaultClass {
        match self {
            LinearOp::Const { .. }
            | LinearOp::LoadTime { .. }
            | LinearOp::LoadY { .. }
            | LinearOp::LoadP { .. }
            | LinearOp::LoadIndexedP { .. }
            | LinearOp::Move { .. }
            | LinearOp::Unary { .. }
            | LinearOp::Binary { .. }
            | LinearOp::Compare { .. }
            | LinearOp::Select { .. }
            | LinearOp::DotProduct { .. }
            | LinearOp::MatrixMultiply { .. }
            | LinearOp::TensorLoad { .. }
            | LinearOp::TensorBinary { .. }
            | LinearOp::TensorCross { .. }
            | LinearOp::TensorTranspose { .. }
            | LinearOp::TensorConcatenate { .. }
            | LinearOp::TensorFill { .. }
            | LinearOp::TensorIdentity { .. } => FaultClass::CannotFault,
            _ => FaultClass::HasEffect,
        }
    }
}
