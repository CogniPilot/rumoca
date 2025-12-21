//! MLS §12.1-12.4: Function Declarations and Calls
//!
//! Tests for:
//! - §12.1 Function declarations
//! - §12.2 Function as a specialized class
//! - §12.3 Pure Modelica functions
//! - §12.4 Function calls (positional, named, mixed arguments)
//!
//! Reference: https://specification.modelica.org/master/functions.html

use crate::spec::{expect_parse_success, expect_success};

// ============================================================================
// §12.1 FUNCTION DECLARATION
// ============================================================================

/// MLS §12.1: Function declarations
mod section_12_1_declaration {
    use super::*;

    #[test]
    fn mls_12_1_simple_function() {
        expect_parse_success(
            r#"
            function Square
                input Real x;
                output Real y;
            algorithm
                y := x * x;
            end Square;
            "#,
        );
    }

    #[test]
    fn mls_12_1_multiple_inputs() {
        expect_parse_success(
            r#"
            function Add
                input Real a;
                input Real b;
                output Real c;
            algorithm
                c := a + b;
            end Add;
            "#,
        );
    }

    #[test]
    fn mls_12_1_multiple_outputs() {
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
            "#,
        );
    }

    #[test]
    fn mls_12_1_with_protected() {
        expect_parse_success(
            r#"
            function Normalize
                input Real x;
                input Real y;
                output Real nx;
                output Real ny;
            protected
                Real len;
            algorithm
                len := sqrt(x*x + y*y);
                nx := x / len;
                ny := y / len;
            end Normalize;
            "#,
        );
    }

    #[test]
    fn mls_12_1_with_default_input() {
        expect_parse_success(
            r#"
            function Scale
                input Real x;
                input Real factor = 1.0;
                output Real y;
            algorithm
                y := x * factor;
            end Scale;
            "#,
        );
    }

    #[test]
    fn mls_12_1_array_input_output() {
        expect_parse_success(
            r#"
            function VectorSum
                input Real x[:];
                output Real s;
            algorithm
                s := sum(x);
            end VectorSum;
            "#,
        );
    }

    #[test]
    fn mls_12_1_matrix_function() {
        expect_parse_success(
            r#"
            function Trace
                input Real A[:,:];
                output Real t;
            algorithm
                t := 0;
                for i in 1:min(size(A,1), size(A,2)) loop
                    t := t + A[i,i];
                end for;
            end Trace;
            "#,
        );
    }

    #[test]
    fn mls_12_1_function_with_description() {
        expect_parse_success(
            r#"
            function Hypotenuse
                "Compute the hypotenuse of a right triangle"
                input Real a "First leg";
                input Real b "Second leg";
                output Real c "Hypotenuse";
            algorithm
                c := sqrt(a*a + b*b);
            end Hypotenuse;
            "#,
        );
    }

    #[test]
    fn mls_12_1_many_inputs() {
        expect_parse_success(
            r#"
            function F5
                input Real a;
                input Real b;
                input Real c;
                input Real d;
                input Real e;
                output Real y;
            algorithm
                y := a + b + c + d + e;
            end F5;
            "#,
        );
    }

    #[test]
    fn mls_12_1_many_outputs() {
        expect_parse_success(
            r#"
            function Stats
                input Real x[:];
                output Real minVal;
                output Real maxVal;
                output Real meanVal;
                output Real sumVal;
            algorithm
                minVal := min(x);
                maxVal := max(x);
                sumVal := sum(x);
                meanVal := sumVal / size(x, 1);
            end Stats;
            "#,
        );
    }
}

// ============================================================================
// §12.2 FUNCTION AS A SPECIALIZED CLASS
// ============================================================================

/// MLS §12.2: Function as specialized class
mod section_12_2_specialized_class {
    use super::*;

    #[test]
    fn mls_12_2_function_in_package() {
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
            "#,
        );
    }

    #[test]
    fn mls_12_2_multiple_functions_in_package() {
        expect_parse_success(
            r#"
            package MathUtils
                function Add
                    input Real a;
                    input Real b;
                    output Real c;
                algorithm
                    c := a + b;
                end Add;

                function Multiply
                    input Real a;
                    input Real b;
                    output Real c;
                algorithm
                    c := a * b;
                end Multiply;
            end MathUtils;
            "#,
        );
    }
}

// ============================================================================
// §12.3 PURE MODELICA FUNCTIONS
// ============================================================================

/// MLS §12.3: Pure functions
mod section_12_3_pure_functions {
    use super::*;

    #[test]
    fn mls_12_3_pure_function() {
        expect_parse_success(
            r#"
            pure function Square
                input Real x;
                output Real y;
            algorithm
                y := x * x;
            end Square;
            "#,
        );
    }

    #[test]
    fn mls_12_3_impure_function() {
        expect_parse_success(
            r#"
            impure function PrintValue
                input Real x;
                output Real y;
            algorithm
                y := x;
            end PrintValue;
            "#,
        );
    }
}

// ============================================================================
// §12.4 FUNCTION CALL
// ============================================================================

/// MLS §12.4: Function calls
mod section_12_4_function_call {
    use super::*;

    #[test]
    fn mls_12_4_simple_call() {
        expect_success(
            r#"
            function Double
                input Real x;
                output Real y;
            algorithm
                y := 2 * x;
            end Double;

            model Test
                Real a = 5;
                Real b = Double(a);
            equation
            end Test;
            "#,
            "Test",
        );
    }

    #[test]
    fn mls_12_4_named_arguments() {
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

    #[test]
    fn mls_12_4_mixed_arguments() {
        expect_parse_success(
            r#"
            function F
                input Real a;
                input Real b;
                input Real c;
                output Real y;
            algorithm
                y := a + b + c;
            end F;

            model Test
                Real x = F(1, c = 3, b = 2);
            end Test;
            "#,
        );
    }

    #[test]
    fn mls_12_4_nested_call() {
        expect_success(
            r#"
            function F
                input Real x;
                output Real y;
            algorithm
                y := x + 1;
            end F;

            model Test
                Real a = F(F(F(1)));
            equation
            end Test;
            "#,
            "Test",
        );
    }

    #[test]
    fn mls_12_4_call_in_expression() {
        expect_success(
            r#"
            function Square
                input Real x;
                output Real y;
            algorithm
                y := x * x;
            end Square;

            model Test
                Real x = 2 * Square(3) + 1;
            equation
            end Test;
            "#,
            "Test",
        );
    }

    #[test]
    fn mls_12_4_call_in_equation() {
        expect_success(
            r#"
            function F
                input Real x;
                output Real y;
            algorithm
                y := x * 2;
            end F;

            model Test
                Real x(start = 1);
                Real y;
            equation
                der(x) = F(x);
                y = F(x) + 1;
            end Test;
            "#,
            "Test",
        );
    }

    #[test]
    fn mls_12_4_call_with_array_arg() {
        expect_parse_success(
            r#"
            function VecSum
                input Real v[:];
                output Real s;
            algorithm
                s := sum(v);
            end VecSum;

            model Test
                Real s = VecSum({1, 2, 3, 4, 5});
            end Test;
            "#,
        );
    }

    #[test]
    fn mls_12_4_multiple_output_call() {
        expect_parse_success(
            r#"
            function MinMax
                input Real x[:];
                output Real minVal;
                output Real maxVal;
            algorithm
                minVal := min(x);
                maxVal := max(x);
            end MinMax;

            model Test
                Real a;
                Real b;
            equation
                (a, b) = MinMax({1, 2, 3, 4, 5});
            end Test;
            "#,
        );
    }
}
