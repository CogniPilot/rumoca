//! MLS §6.6: Function Type Compatibility
//!
//! Tests for function type compatibility including:
//! - Input/output matching
//! - Variability compatibility
//! - Array dimension compatibility
//! - Functional input arguments
//!
//! Reference: https://specification.modelica.org/master/interface-or-type.html

use crate::spec::{expect_failure, expect_parse_success, expect_success};

// ============================================================================
// §6.6.1 FUNCTION INPUT/OUTPUT MATCHING
// ============================================================================

/// MLS §6.6: Function input/output type matching
mod io_matching {
    use super::*;

    /// Simple function call
    #[test]
    fn mls_6_6_simple_function_call() {
        expect_success(
            r#"
            function Square
                input Real x;
                output Real y;
            algorithm
                y := x * x;
            end Square;

            model Test
                Real x = Square(5);
            equation
            end Test;
            "#,
            "Test",
        );
    }

    /// Multiple inputs
    #[test]
    fn mls_6_6_multiple_inputs() {
        expect_success(
            r#"
            function Add
                input Real a;
                input Real b;
                output Real c;
            algorithm
                c := a + b;
            end Add;

            model Test
                Real x = Add(2, 3);
            equation
            end Test;
            "#,
            "Test",
        );
    }

    /// Multiple outputs
    #[test]
    fn mls_6_6_multiple_outputs() {
        expect_parse_success(
            r#"
            function SinCos
                input Real x;
                output Real s;
                output Real c;
            algorithm
                s := sin(x);
                c := cos(x);
            end SinCos;

            model Test
                Real s, c;
            equation
                (s, c) = SinCos(0.5);
            end Test;
            "#,
        );
    }

    /// Named arguments
    #[test]
    fn mls_6_6_named_arguments() {
        expect_parse_success(
            r#"
            function Power
                input Real base;
                input Real exponent;
                output Real result;
            algorithm
                result := base ^ exponent;
            end Power;

            model Test
                Real x = Power(exponent = 2, base = 3);
            end Test;
            "#,
        );
    }

    /// Default argument values
    #[test]
    fn mls_6_6_default_arguments() {
        expect_parse_success(
            r#"
            function Scale
                input Real x;
                input Real factor = 1.0;
                output Real y;
            algorithm
                y := x * factor;
            end Scale;

            model Test
                Real x1 = Scale(5);
                Real x2 = Scale(5, 2);
            end Test;
            "#,
        );
    }
}

// ============================================================================
// §6.6.2 VARIABILITY COMPATIBILITY
// ============================================================================

/// MLS §6.6: Variability in function calls
mod variability_compatibility {
    use super::*;

    /// Constant argument to function
    #[test]
    fn mls_6_6_constant_argument() {
        expect_success(
            r#"
            function Double
                input Real x;
                output Real y;
            algorithm
                y := 2 * x;
            end Double;

            model Test
                constant Real c = Double(5);
            equation
            end Test;
            "#,
            "Test",
        );
    }

    /// Parameter argument to function
    #[test]
    fn mls_6_6_parameter_argument() {
        expect_success(
            r#"
            function Double
                input Real x;
                output Real y;
            algorithm
                y := 2 * x;
            end Double;

            model Test
                parameter Real p = Double(5);
            equation
            end Test;
            "#,
            "Test",
        );
    }

    /// Variable argument to function
    #[test]
    fn mls_6_6_variable_argument() {
        expect_success(
            r#"
            function Double
                input Real x;
                output Real y;
            algorithm
                y := 2 * x;
            end Double;

            model Test
                Real x(start = 0);
                Real y;
            equation
                der(x) = 1;
                y = Double(x);
            end Test;
            "#,
            "Test",
        );
    }
}

// ============================================================================
// §6.6.3 ARRAY DIMENSION COMPATIBILITY
// ============================================================================

/// MLS §6.6: Array dimensions in function calls
mod array_dimension_compatibility {
    use super::*;

    /// Array input
    #[test]
    fn mls_6_6_array_input() {
        expect_parse_success(
            r#"
            function VectorSum
                input Real v[:];
                output Real s;
            algorithm
                s := sum(v);
            end VectorSum;

            model Test
                Real x = VectorSum({1, 2, 3, 4, 5});
            end Test;
            "#,
        );
    }

    /// Array output
    #[test]
    fn mls_6_6_array_output() {
        expect_parse_success(
            r#"
            function CreateVector
                input Integer n;
                output Real v[n];
            algorithm
                for i in 1:n loop
                    v[i] := i;
                end for;
            end CreateVector;

            model Test
                Real v[5] = CreateVector(5);
            end Test;
            "#,
        );
    }

    /// Matrix input
    #[test]
    fn mls_6_6_matrix_input() {
        expect_parse_success(
            r#"
            function MatrixTrace
                input Real A[:,:];
                output Real t;
            protected
                Integer n;
            algorithm
                n := min(size(A, 1), size(A, 2));
                t := 0;
                for i in 1:n loop
                    t := t + A[i, i];
                end for;
            end MatrixTrace;

            model Test
                Real t = MatrixTrace({{1, 2}, {3, 4}});
            end Test;
            "#,
        );
    }

    /// Flexible array size (colon)
    #[test]
    fn mls_6_6_flexible_array_size() {
        expect_parse_success(
            r#"
            function DotProduct
                input Real a[:];
                input Real b[size(a, 1)];
                output Real c;
            algorithm
                c := 0;
                for i in 1:size(a, 1) loop
                    c := c + a[i] * b[i];
                end for;
            end DotProduct;

            model Test
                Real x = DotProduct({1, 2, 3}, {4, 5, 6});
            end Test;
            "#,
        );
    }
}

// ============================================================================
// §6.6.4 FUNCTIONAL INPUT ARGUMENTS
// ============================================================================

/// MLS §6.6: Functions as input arguments
mod functional_inputs {
    use super::*;

    /// Function passed as argument (partial function)
    #[test]
    fn mls_6_6_functional_input() {
        expect_parse_success(
            r#"
            partial function UnaryOp
                input Real x;
                output Real y;
            end UnaryOp;

            function Apply
                input UnaryOp op;
                input Real x;
                output Real y;
            algorithm
                y := op(x);
            end Apply;

            function Square
                extends UnaryOp;
            algorithm
                y := x * x;
            end Square;

            model Test
                Real x = Apply(Square, 5);
            end Test;
            "#,
        );
    }

    /// Function partial application
    #[test]
    fn mls_6_6_partial_application() {
        expect_parse_success(
            r#"
            function Add
                input Real a;
                input Real b;
                output Real c;
            algorithm
                c := a + b;
            end Add;

            partial function UnaryOp
                input Real x;
                output Real y;
            end UnaryOp;

            function Apply
                input UnaryOp op;
                input Real x;
                output Real y;
            algorithm
                y := op(x);
            end Apply;

            model Test
                Real x = Apply(function Add(a = 5), 3);
            end Test;
            "#,
        );
    }
}

// ============================================================================
// NEGATIVE TESTS - Function type errors
// ============================================================================

/// Function type error cases
mod function_type_errors {
    use super::*;

    /// Wrong number of arguments
    #[test]
    #[ignore = "Function argument count validation not yet implemented"]
    fn error_wrong_argument_count() {
        expect_failure(
            r#"
            function Add
                input Real a;
                input Real b;
                output Real c;
            algorithm
                c := a + b;
            end Add;

            model Test
                Real x = Add(1);
            equation
            end Test;
            "#,
            "Test",
        );
    }

    /// Too many arguments
    #[test]
    #[ignore = "Function argument count validation not yet implemented"]
    fn error_too_many_arguments() {
        expect_failure(
            r#"
            function Square
                input Real x;
                output Real y;
            algorithm
                y := x * x;
            end Square;

            model Test
                Real x = Square(1, 2);
            equation
            end Test;
            "#,
            "Test",
        );
    }

    /// Wrong argument type
    #[test]
    #[ignore = "Argument type checking not yet implemented"]
    fn error_wrong_argument_type() {
        expect_failure(
            r#"
            function F
                input Real x;
                output Real y;
            algorithm
                y := x;
            end F;

            model Test
                Real x = F("hello");
            equation
            end Test;
            "#,
            "Test",
        );
    }

    /// Array size mismatch in function call
    #[test]
    #[ignore = "Array size validation in function calls not yet implemented"]
    fn error_array_size_mismatch() {
        expect_failure(
            r#"
            function F
                input Real x[3];
                output Real y;
            algorithm
                y := sum(x);
            end F;

            model Test
                Real x = F({1, 2});
            equation
            end Test;
            "#,
            "Test",
        );
    }

    /// Unknown function
    #[test]
    fn error_unknown_function() {
        expect_failure(
            r#"
            model Test
                Real x = UnknownFunction(5);
            equation
            end Test;
            "#,
            "Test",
        );
    }
}

// ============================================================================
// COMPLEX FUNCTION TYPE SCENARIOS
// ============================================================================

/// Complex function type scenarios
mod complex_scenarios {
    use super::*;

    /// Nested function calls
    #[test]
    fn complex_nested_calls() {
        expect_success(
            r#"
            function F
                input Real x;
                output Real y;
            algorithm
                y := x + 1;
            end F;

            model Test
                Real x = F(F(F(1)));
            equation
            end Test;
            "#,
            "Test",
        );
    }

    /// Function in expression
    #[test]
    fn complex_function_in_expression() {
        expect_success(
            r#"
            function Square
                input Real x;
                output Real y;
            algorithm
                y := x * x;
            end Square;

            model Test
                Real a = 2;
                Real b = 3;
                Real c = Square(a) + Square(b);
            equation
            end Test;
            "#,
            "Test",
        );
    }

    /// Function with array arithmetic
    #[test]
    fn complex_array_function() {
        expect_parse_success(
            r#"
            function Normalize
                input Real v[:];
                output Real n[size(v, 1)];
            protected
                Real len;
            algorithm
                len := sqrt(sum(v[i]^2 for i in 1:size(v, 1)));
                for i in 1:size(v, 1) loop
                    n[i] := v[i] / len;
                end for;
            end Normalize;

            model Test
                Real v[3] = Normalize({3, 4, 0});
            end Test;
            "#,
        );
    }

    /// Package-qualified function call
    #[test]
    fn complex_package_function() {
        expect_parse_success(
            r#"
            package Math
                function Square
                    input Real x;
                    output Real y;
                algorithm
                    y := x * x;
                end Square;
            end Math;

            model Test
                Real x = Math.Square(5);
            end Test;
            "#,
        );
    }
}
