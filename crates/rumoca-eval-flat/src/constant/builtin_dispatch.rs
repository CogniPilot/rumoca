//! Mapping from the `BuiltinFunction` node kind onto the named builtin
//! implementations in [`super::builtins`].

use rumoca_core::Span;

use super::errors::EvalError;
use super::value::Value;
use super::{BuiltinFunction, eval_builtin};

/// Check the shared Modelica signature before either validation or dispatch.
pub(super) fn validate_builtin_arity(
    func: BuiltinFunction,
    actual: usize,
    span: Span,
) -> Result<(), EvalError> {
    if func.accepts_argument_count(actual) {
        return Ok(());
    }
    let (expected, _) = func.argument_count_range();
    Err(EvalError::WrongArgCount {
        expected,
        actual,
        span,
    })
}

/// Evaluate a builtin function call.
pub(super) fn eval_builtin_function(
    func: &BuiltinFunction,
    args: &[Value],
    span: Span,
) -> Result<Value, EvalError> {
    validate_builtin_arity(*func, args.len(), span)?;
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
        BuiltinFunction::NoEvent | BuiltinFunction::Homotopy | BuiltinFunction::Delay => {
            checked_argument(func, args, 0, span)
        }
        BuiltinFunction::Smooth => checked_argument(func, args, 1, span),
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

fn checked_argument(
    func: &BuiltinFunction,
    args: &[Value],
    index: usize,
    span: Span,
) -> Result<Value, EvalError> {
    args.get(index).cloned().ok_or_else(|| EvalError::Internal {
        message: format!(
            "validated builtin signature for {} has no argument at index {index} ({span:?})",
            func.name()
        ),
    })
}
