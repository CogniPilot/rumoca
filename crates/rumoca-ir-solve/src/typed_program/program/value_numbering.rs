//! One register per distinct scalar computation of a program.
//!
//! A program is straight-line SSA, so an operation over the same operands
//! computes the same value wherever it appears after its first occurrence, and
//! when the first one fails the program has already stopped. Issuing the
//! second occurrence would only repeat the work, so construction returns the
//! first occurrence's register. Only operations with a single-cell result are
//! numbered: array-valued registers are updated in place by their last reader
//! and must stay distinct. Scalar loads of an immutable slot use that exact
//! slot identity as their key. Writable slots are never numbered; wire replay
//! disables numbering and preserves all existing recorded register identities.

use super::{
    SolveBinaryOperator, SolveCompareOperator, SolveConversionOperator, SolveRegisterId,
    SolveSlotId, SolveUnaryOperator,
};
use crate::typed_program::types::SolveValue;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub(super) enum ValueKey {
    Constant(SolveValue),
    ReadOnlyLoad(SolveSlotId),
    Unary(SolveUnaryOperator, SolveRegisterId),
    Binary(SolveBinaryOperator, SolveRegisterId, SolveRegisterId),
    Compare(SolveCompareOperator, SolveRegisterId, SolveRegisterId),
    Convert(SolveConversionOperator, SolveRegisterId),
    Project(SolveRegisterId, Box<[u32]>),
    ProjectDynamic(SolveRegisterId, Box<[SolveRegisterId]>),
}
