//! Evaluation and scalar projection over valid-by-construction DAE expressions.
//!
//! This crate consumes only branded [`rumoca_ir_dae::DaeView`] values. It does
//! not accept the source-language expression tree: temporal operators, name
//! lookup, and malformed expression shapes have already been eliminated by
//! checked DAE construction.

mod numeric;
mod projection;

pub use numeric::{NumericEvaluationError, NumericEvaluationErrorKind, NumericEvaluator};
pub use projection::{
    ProjectionError, ScalarCoordinateProjectionCache, for_each_scalar_coordinate,
    for_each_scalar_coordinate_cached,
};
