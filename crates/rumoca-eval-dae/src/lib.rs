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

pub use function_context::{
    ExactDeclarationValues, ExactInitialError, ExactInitialRun, ExactInitialValues,
    FunctionCallContext, exact_initial_values, exact_source_values,
};
pub use numeric::{
    InputFallbackBounds, InputInitialization, InputOrigin, InputOriginRun, NumericEvaluationError,
    NumericEvaluationErrorKind, NumericEvaluator, NumericInitialRun, NumericInitialValues,
    Scalar as NumericSourceScalar, SourceValues, declaration_initial_expression,
};
pub use projection::{
    CoordinateQuery, LiteralBinding, ProjectionError, PrunedScalarCoordinateProjection,
    ScalarCoordinateProjectionCache, ZeroCoefficients, for_each_scalar_coordinate,
    for_each_scalar_coordinate_cached, for_each_scalar_coordinate_filtered_cached,
    for_each_scalar_coordinate_queried_cached, literal_bindings, multiplication_scalar_pairs,
};
pub use selected_values::for_each_selected_value_expression;
