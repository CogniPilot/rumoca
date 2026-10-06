//! New host math admission is confined to declared Real cells.
use super::*;

#[test]
fn imported_math_does_not_widen_integer_or_boolean_numeric_admission() {
    use solve::SolveUnaryOperator as U;
    let integer = solve::SolveScalarType::Integer(solve::SolveIntegerDomain::FULL);
    for operator in [
        U::Sin,
        U::Cos,
        U::Tan,
        U::Asin,
        U::Acos,
        U::Atan,
        U::Sinh,
        U::Cosh,
        U::Tanh,
        U::Exp,
        U::Log,
        U::Log10,
    ] {
        assert!(!unary_supported(operator, integer));
        assert!(!unary_supported(operator, solve::SolveScalarType::Boolean));
    }
    for operator in [
        solve::SolveBinaryOperator::Power,
        solve::SolveBinaryOperator::Atan2,
    ] {
        assert!(!binary_supported(operator, integer));
        assert!(!binary_supported(operator, solve::SolveScalarType::Boolean));
    }
}
