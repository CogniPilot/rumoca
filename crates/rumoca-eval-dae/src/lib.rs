//! Evaluation and scalar projection over valid-by-construction DAE expressions.
//!
//! This crate consumes only branded [`rumoca_ir_dae::DaeView`] values. It does
//! not accept the source-language expression tree: temporal operators, name
//! lookup, and malformed expression shapes have already been eliminated by
//! checked DAE construction.

mod function_context;
mod numeric;
mod projection;
mod selected_values;

pub use function_context::FunctionCallContext;
pub use numeric::{NumericEvaluationError, NumericEvaluationErrorKind, NumericEvaluator};
pub use projection::{
    LiteralBinding, ProjectionError, PrunedScalarCoordinateProjection,
    ScalarCoordinateProjectionCache, ZeroCoefficients, for_each_scalar_coordinate,
    for_each_scalar_coordinate_cached, for_each_scalar_coordinate_filtered_cached,
    literal_bindings, multiplication_scalar_pairs,
};
pub use selected_values::for_each_selected_value_expression;
