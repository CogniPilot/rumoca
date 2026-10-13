//! One register per distinct scalar computation of a program.
//!
//! A program is straight-line SSA, so an operation over the same operands
//! computes the same value wherever it appears after its first occurrence, and
//! when the first one fails the program has already stopped. Issuing the
//! second occurrence would only repeat the work, so construction returns the
//! first occurrence's register. Only operations with a single-cell result are
//! numbered: array-valued registers are updated in place by their last reader
//! and must stay distinct.

use super::{
    SolveBinaryOperator, SolveCompareOperator, SolveConversionOperator, SolveRegisterId,
    SolveUnaryOperator,
};
use crate::typed_program::types::SolveValue;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub(super) enum ValueKey {
    Constant(SolveValue),
    Unary(SolveUnaryOperator, SolveRegisterId),
    Binary(SolveBinaryOperator, SolveRegisterId, SolveRegisterId),
    Compare(SolveCompareOperator, SolveRegisterId, SolveRegisterId),
    Convert(SolveConversionOperator, SolveRegisterId),
    Project(SolveRegisterId, Box<[u32]>),
    ProjectDynamic(SolveRegisterId, Box<[SolveRegisterId]>),
}
