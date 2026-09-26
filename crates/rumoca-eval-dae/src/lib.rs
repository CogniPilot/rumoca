//! Evaluation and scalar projection over valid-by-construction DAE expressions.
//!
//! This crate consumes only branded [`rumoca_ir_dae::DaeView`] values. It does
//! not accept the source-language expression tree: temporal operators, name
//! lookup, and malformed expression shapes have already been eliminated by
//! checked DAE construction.

mod function_context;
mod numeric;
mod projection;

pub use function_context::FunctionCallContext;
pub use numeric::{NumericEvaluationError, NumericEvaluationErrorKind, NumericEvaluator};
pub use projection::{
    LiteralBinding, ProjectionError, ScalarCoordinateProjectionCache, ZeroCoefficients,
    for_each_scalar_coordinate, for_each_scalar_coordinate_cached, literal_bindings,
    multiplication_scalar_pairs,
};
