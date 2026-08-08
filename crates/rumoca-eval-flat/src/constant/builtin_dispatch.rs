//! Mapping from the `BuiltinFunction` node kind onto the named builtin
//! implementations in [`super::builtins`].

use rumoca_core::Span;

use super::errors::EvalError;
use super::value::Value;
use super::{BuiltinFunction, eval_builtin};

/// Evaluate a builtin function call.
pub(super) fn eval_builtin_function(
    func: &BuiltinFunction,
    args: &[Value],
    span: Span,
) -> Result<Value, EvalError> {
    match func {
        // Math functions
        BuiltinFunction::Abs => eval_builtin("abs", args, span),
        BuiltinFunction::Sign => eval_builtin("sign", args, span),
        BuiltinFunction::Sqrt => eval_builtin("sqrt", args, span),
        BuiltinFunction::Div => eval_builtin("div", args, span),
        BuiltinFunction::Mod => eval_builtin("mod", args, span),
        BuiltinFunction::Rem => eval_builtin("rem", args, span),
        BuiltinFunction::Floor => eval_builtin("floor", args, span),
        BuiltinFunction::Ceil => eval_builtin("ceil", args, span),
        BuiltinFunction::Min => eval_builtin("min", args, span),
        BuiltinFunction::Max => eval_builtin("max", args, span),

        // Trig functions
        BuiltinFunction::Sin => eval_builtin("sin", args, span),
        BuiltinFunction::Cos => eval_builtin("cos", args, span),
        BuiltinFunction::Tan => eval_builtin("tan", args, span),
        BuiltinFunction::Asin => eval_builtin("asin", args, span),
        BuiltinFunction::Acos => eval_builtin("acos", args, span),
        BuiltinFunction::Atan => eval_builtin("atan", args, span),
        BuiltinFunction::Atan2 => eval_builtin("atan2", args, span),
        BuiltinFunction::Sinh => eval_builtin("sinh", args, span),
        BuiltinFunction::Cosh => eval_builtin("cosh", args, span),
        BuiltinFunction::Tanh => eval_builtin("tanh", args, span),

        // Exp/log
        BuiltinFunction::Exp => eval_builtin("exp", args, span),
        BuiltinFunction::Log => eval_builtin("log", args, span),
        BuiltinFunction::Log10 => eval_builtin("log10", args, span),

        // Array functions
        BuiltinFunction::Size => eval_builtin("size", args, span),
        BuiltinFunction::Ndims => eval_builtin("ndims", args, span),
        BuiltinFunction::Sum => eval_builtin("sum", args, span),
        BuiltinFunction::Product => eval_builtin("product", args, span),
        BuiltinFunction::Zeros => eval_builtin("zeros", args, span),
        BuiltinFunction::Ones => eval_builtin("ones", args, span),
        BuiltinFunction::Fill => eval_builtin("fill", args, span),
        BuiltinFunction::Linspace => eval_builtin("linspace", args, span),
        BuiltinFunction::Cat => eval_builtin("cat", args, span),

        // Pass-through builtins
        BuiltinFunction::NoEvent => args.first().cloned().ok_or_else(|| {
            EvalError::not_constant("noEvent requires 1 argument".to_string(), span)
        }),
        BuiltinFunction::Smooth => args.get(1).cloned().ok_or_else(|| {
            EvalError::not_constant("smooth requires 2 arguments".to_string(), span)
        }),
        BuiltinFunction::Homotopy => args.first().cloned().ok_or_else(|| {
            EvalError::not_constant("homotopy requires 1 argument".to_string(), span)
        }),
        BuiltinFunction::Delay => args
            .first()
            .cloned()
            .ok_or_else(|| EvalError::not_constant("delay requires 1 argument".to_string(), span)),
        BuiltinFunction::Integer => eval_builtin("integer", args, span),
        BuiltinFunction::SemiLinear => eval_builtin("semiLinear", args, span),

        // These are runtime-only functions
        BuiltinFunction::Der
        | BuiltinFunction::Pre
        | BuiltinFunction::Edge
        | BuiltinFunction::Change
        | BuiltinFunction::Reinit
        | BuiltinFunction::Sample
        | BuiltinFunction::Clock
        | BuiltinFunction::Hold
        | BuiltinFunction::Previous
        | BuiltinFunction::Interval
        | BuiltinFunction::SubSample
        | BuiltinFunction::SuperSample
        | BuiltinFunction::ShiftSample
        | BuiltinFunction::BackSample
        | BuiltinFunction::NoClock
        | BuiltinFunction::Initial
        | BuiltinFunction::Terminal => Err(EvalError::not_constant(
            format!("runtime function: {:?}", func),
            span,
        )),

        // Other array/matrix functions that need more work
        BuiltinFunction::Scalar
        | BuiltinFunction::Vector
        | BuiltinFunction::Matrix
        | BuiltinFunction::Identity
        | BuiltinFunction::Diagonal
        | BuiltinFunction::Transpose
        | BuiltinFunction::OuterProduct
        | BuiltinFunction::Symmetric
        | BuiltinFunction::Cross
        | BuiltinFunction::Skew => Err(EvalError::UnsupportedExpression {
            kind: format!("matrix function: {:?}", func),
            span,
        }),
    }
}
