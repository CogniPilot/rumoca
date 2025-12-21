//! MLS §12.4: Function Calls - Detailed Tests
//!
//! This file provides comprehensive tests for all aspects of function calls:
//! - §12.4.1 Positional or Named Input Arguments
//! - §12.4.2 Functional Input Arguments
//! - §12.4.3 Output Formal Parameters
//! - §12.4.4 Initialization and Binding Equations
//! - §12.4.5 Flexible Array Sizes and Resizing
//! - §12.4.6 Automatic Vectorization
//! - §12.4.7 Empty Function Calls
//!
//! Reference: https://specification.modelica.org/master/functions.html#function-call

use crate::spec::{expect_parse_success, expect_success};

// ============================================================================
// §12.4.1 POSITIONAL OR NAMED INPUT ARGUMENTS
// ============================================================================

/// MLS §12.4.1: Positional and named arguments
mod section_12_4_1_arguments {
    use super::*;

    /// MLS §12.4.1: All positional arguments
    #[test]
    fn mls_12_4_1_all_positional() {
        expect_success(
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
                Real x = F(1, 2, 3);
            equation
            end Test;
            "#,
            "Test",
        );
    }

    /// MLS §12.4.1: All named arguments
    #[test]
    fn mls_12_4_1_all_named() {
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
                Real x = F(a = 1, b = 2, c = 3);
            end Test;
            "#,
        );
    }

    /// MLS §12.4.1: Named arguments in different order
    #[test]
    fn mls_12_4_1_named_reordered() {
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
                Real x = F(c = 3, a = 1, b = 2);
            end Test;
            "#,
        );
    }

    /// MLS §12.4.1: Mixed positional then named
    #[test]
    fn mls_12_4_1_mixed_positional_named() {
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

    /// MLS §12.4.1: Partial positional with named completion
    #[test]
    fn mls_12_4_1_partial_positional() {
        expect_parse_success(
            r#"
            function F
                input Real a;
                input Real b;
                input Real c;
                input Real d;
                output Real y;
            algorithm
                y := a + b + c + d;
            end F;

            model Test
                Real x = F(1, 2, d = 4, c = 3);
            end Test;
            "#,
        );
    }

    /// MLS §12.4.1: Default values for unfilled slots
    #[test]
    fn mls_12_4_1_default_values() {
        expect_parse_success(
            r#"
            function F
                input Real a;
                input Real b = 10;
                input Real c = 20;
                output Real y;
            algorithm
                y := a + b + c;
            end F;

            model Test
                Real x1 = F(1);
                Real x2 = F(1, 2);
                Real x3 = F(1, c = 5);
            end Test;
            "#,
        );
    }

    /// MLS §12.4.1: Type coercion Integer to Real
    #[test]
    fn mls_12_4_1_type_coercion() {
        expect_success(
            r#"
            function F
                input Real x;
                output Real y;
            algorithm
                y := x * 2;
            end F;

            model Test
                Real a = F(5);
            equation
            end Test;
            "#,
            "Test",
        );
    }

    /// MLS §12.4.1: Expression as argument
    #[test]
    fn mls_12_4_1_expression_argument() {
        expect_success(
            r#"
            function F
                input Real x;
                output Real y;
            algorithm
                y := x * 2;
            end F;

            model Test
                Real a = 3;
                Real b = F(a * 2 + 1);
            equation
            end Test;
            "#,
            "Test",
        );
    }

    /// MLS §12.4.1: Nested function calls as arguments
    #[test]
    fn mls_12_4_1_nested_calls() {
        expect_success(
            r#"
            function F
                input Real x;
                output Real y;
            algorithm
                y := x + 1;
            end F;

            function G
                input Real a;
                input Real b;
                output Real y;
            algorithm
                y := a * b;
            end G;

            model Test
                Real x = G(F(1), F(2));
            equation
            end Test;
            "#,
            "Test",
        );
    }
}

// ============================================================================
// §12.4.2 FUNCTIONAL INPUT ARGUMENTS
// ============================================================================

/// MLS §12.4.2: Functional input arguments (functions as parameters)
mod section_12_4_2_functional_arguments {
    use super::*;

    /// MLS §12.4.2: Function type as input parameter
    #[test]
    #[ignore = "Functional input arguments not yet fully supported"]
    fn mls_12_4_2_function_type_input() {
        expect_parse_success(
            r#"
            function Apply
                input Real x;
                input function F(input Real u; output Real y);
                output Real result;
            algorithm
                result := F(x);
            end Apply;
            "#,
        );
    }

    /// MLS §12.4.2: Passing function as argument
    #[test]
    #[ignore = "Functional input arguments not yet fully supported"]
    fn mls_12_4_2_function_argument() {
        expect_parse_success(
            r#"
            function Square
                input Real x;
                output Real y;
            algorithm
                y := x * x;
            end Square;

            function Apply
                input Real x;
                input function F(input Real u; output Real y);
                output Real result;
            algorithm
                result := F(x);
            end Apply;

            model Test
                Real a = Apply(3, Square);
            end Test;
            "#,
        );
    }

    /// MLS §12.4.2.1: Function partial application
    #[test]
    #[ignore = "Function partial application not yet supported"]
    fn mls_12_4_2_1_partial_application() {
        expect_parse_success(
            r#"
            function Add
                input Real a;
                input Real b;
                output Real y;
            algorithm
                y := a + b;
            end Add;

            function ApplyUnary
                input Real x;
                input function F(input Real u; output Real y);
                output Real result;
            algorithm
                result := F(x);
            end ApplyUnary;

            model Test
                Real a = ApplyUnary(3, function Add(a = 10));
            end Test;
            "#,
        );
    }

    /// MLS §12.4.2.1: Partial application binding multiple arguments
    #[test]
    #[ignore = "Function partial application not yet supported"]
    fn mls_12_4_2_1_partial_multiple_bindings() {
        expect_parse_success(
            r#"
            function F3
                input Real a;
                input Real b;
                input Real c;
                output Real y;
            algorithm
                y := a + b + c;
            end F3;

            model Test
                function F1 = function F3(a = 1, b = 2);
                Real x = F1(c = 3);
            end Test;
            "#,
        );
    }
}

// ============================================================================
// §12.4.3 OUTPUT FORMAL PARAMETERS
// ============================================================================

/// MLS §12.4.3: Output formal parameters
mod section_12_4_3_output_parameters {
    use super::*;

    /// MLS §12.4.3: Single output
    #[test]
    fn mls_12_4_3_single_output() {
        expect_success(
            r#"
            function F
                input Real x;
                output Real y;
            algorithm
                y := x * 2;
            end F;

            model Test
                Real a = F(5);
            equation
            end Test;
            "#,
            "Test",
        );
    }

    /// MLS §12.4.3: Multiple outputs with tuple syntax
    #[test]
    fn mls_12_4_3_multiple_outputs_tuple() {
        expect_parse_success(
            r#"
            function MinMax
                input Real a;
                input Real b;
                output Real minVal;
                output Real maxVal;
            algorithm
                if a < b then
                    minVal := a;
                    maxVal := b;
                else
                    minVal := b;
                    maxVal := a;
                end if;
            end MinMax;

            model Test
                Real lo;
                Real hi;
            equation
                (lo, hi) = MinMax(3, 7);
            end Test;
            "#,
        );
    }

    /// MLS §12.4.3: Multiple outputs - using only first
    #[test]
    fn mls_12_4_3_first_output_only() {
        expect_success(
            r#"
            function F
                input Real x;
                output Real a;
                output Real b;
            algorithm
                a := x;
                b := x * 2;
            end F;

            model Test
                Real y = F(5);
            equation
            end Test;
            "#,
            "Test",
        );
    }

    /// MLS §12.4.3: Empty slots for unused outputs
    #[test]
    fn mls_12_4_3_empty_output_slots() {
        expect_parse_success(
            r#"
            function F
                input Real x;
                output Real a;
                output Real b;
                output Real c;
            algorithm
                a := x;
                b := x * 2;
                c := x * 3;
            end F;

            model Test
                Real first;
                Real third;
            equation
                (first, , third) = F(5);
            end Test;
            "#,
        );
    }

    /// MLS §12.4.3: Three outputs
    #[test]
    fn mls_12_4_3_three_outputs() {
        expect_parse_success(
            r#"
            function Stats
                input Real x;
                output Real square;
                output Real cube;
                output Real sqrt_val;
            algorithm
                square := x * x;
                cube := x * x * x;
                sqrt_val := sqrt(abs(x));
            end Stats;

            model Test
                Real a;
                Real b;
                Real c;
            equation
                (a, b, c) = Stats(4);
            end Test;
            "#,
        );
    }

    /// MLS §12.4.3: Outputs in nested function call
    #[test]
    fn mls_12_4_3_nested_output() {
        expect_success(
            r#"
            function Inner
                input Real x;
                output Real y;
            algorithm
                y := x * 2;
            end Inner;

            function Outer
                input Real x;
                output Real a;
                output Real b;
            algorithm
                a := Inner(x);
                b := Inner(x + 1);
            end Outer;

            model Test
                Real p;
                Real q;
            equation
                (p, q) = Outer(5);
            end Test;
            "#,
            "Test",
        );
    }
}

// ============================================================================
// §12.4.4 INITIALIZATION AND BINDING EQUATIONS
// ============================================================================

/// MLS §12.4.4: Initialization and binding equations in functions
mod section_12_4_4_initialization {
    use super::*;

    /// MLS §12.4.4: Protected variable with binding equation
    #[test]
    fn mls_12_4_4_protected_binding() {
        expect_parse_success(
            r#"
            function F
                input Real x;
                output Real y;
            protected
                Real doubled = x * 2;
                Real squared = x * x;
            algorithm
                y := doubled + squared;
            end F;
            "#,
        );
    }

    /// MLS §12.4.4: Output with binding equation
    #[test]
    fn mls_12_4_4_output_binding() {
        expect_parse_success(
            r#"
            function F
                input Real x;
                output Real y = x * 2;
            algorithm
            end F;
            "#,
        );
    }

    /// MLS §12.4.4: Chained binding equations
    #[test]
    fn mls_12_4_4_chained_binding() {
        expect_parse_success(
            r#"
            function F
                input Real x;
                output Real y;
            protected
                Real a = x;
                Real b = a * 2;
                Real c = b + 1;
            algorithm
                y := c;
            end F;
            "#,
        );
    }

    /// MLS §12.4.4: Binding with function call
    #[test]
    fn mls_12_4_4_binding_with_call() {
        expect_parse_success(
            r#"
            function Square
                input Real x;
                output Real y;
            algorithm
                y := x * x;
            end Square;

            function F
                input Real x;
                output Real y;
            protected
                Real sq = Square(x);
            algorithm
                y := sq + 1;
            end F;
            "#,
        );
    }

    /// MLS §12.4.4: Multiple variables in protected section
    #[test]
    fn mls_12_4_4_multiple_protected() {
        expect_parse_success(
            r#"
            function ComplexCalc
                input Real x;
                input Real y;
                output Real result;
            protected
                Real sum = x + y;
                Real diff = x - y;
                Real prod = x * y;
                Real combined = sum * diff + prod;
            algorithm
                result := combined / 2;
            end ComplexCalc;
            "#,
        );
    }
}

// ============================================================================
// §12.4.5 FLEXIBLE ARRAY SIZES AND RESIZING
// ============================================================================

/// MLS §12.4.5: Flexible array sizes
mod section_12_4_5_array_sizes {
    use super::*;

    /// MLS §12.4.5: Array dimension from input size
    #[test]
    fn mls_12_4_5_dimension_from_input() {
        expect_parse_success(
            r#"
            function F
                input Real x[:];
                output Real y[size(x, 1)];
            algorithm
                for i in 1:size(x, 1) loop
                    y[i] := x[i] * 2;
                end for;
            end F;
            "#,
        );
    }

    /// MLS §12.4.5: Colon-declared output dimension
    #[test]
    fn mls_12_4_5_colon_output() {
        expect_parse_success(
            r#"
            function CreateArray
                input Integer n;
                output Real y[:];
            algorithm
                y := zeros(n);
                for i in 1:n loop
                    y[i] := i;
                end for;
            end CreateArray;
            "#,
        );
    }

    /// MLS §12.4.5: Protected array from input dimension
    #[test]
    fn mls_12_4_5_protected_from_input() {
        expect_parse_success(
            r#"
            function F
                input Real x[:];
                output Real y;
            protected
                Real temp[size(x, 1)];
            algorithm
                for i in 1:size(x, 1) loop
                    temp[i] := x[i] * 2;
                end for;
                y := sum(temp);
            end F;
            "#,
        );
    }

    /// MLS §12.4.5: 2D array with dimensions from inputs
    #[test]
    fn mls_12_4_5_2d_from_inputs() {
        expect_parse_success(
            r#"
            function F
                input Real A[:,:];
                output Real B[size(A, 1), size(A, 2)];
            algorithm
                for i in 1:size(A, 1) loop
                    for j in 1:size(A, 2) loop
                        B[i, j] := A[i, j] * 2;
                    end for;
                end for;
            end F;
            "#,
        );
    }

    /// MLS §12.4.5: Array resizing - entire array assignment
    #[test]
    fn mls_12_4_5_array_resize() {
        expect_parse_success(
            r#"
            function F
                input Integer n;
                output Real y[:];
            protected
                Real temp[:];
            algorithm
                temp := ones(n);
                y := temp;
            end F;
            "#,
        );
    }
}

// ============================================================================
// §12.4.6 AUTOMATIC VECTORIZATION
// ============================================================================

/// MLS §12.4.6: Automatic vectorization
mod section_12_4_6_vectorization {
    use super::*;

    /// MLS §12.4.6: Scalar function applied to 1D array
    #[test]
    fn mls_12_4_6_scalar_to_1d() {
        expect_success(
            r#"
            function F
                input Real x;
                output Real y;
            algorithm
                y := x * 2;
            end F;

            model Test
                Real a[3] = {1, 2, 3};
                Real b[3] = F(a);
            equation
            end Test;
            "#,
            "Test",
        );
    }

    /// MLS §12.4.6: Scalar function applied to 2D array
    #[test]
    fn mls_12_4_6_scalar_to_2d() {
        expect_success(
            r#"
            function F
                input Real x;
                output Real y;
            algorithm
                y := x * 2;
            end F;

            model Test
                Real A[2, 2] = {{1, 2}, {3, 4}};
                Real B[2, 2] = F(A);
            equation
            end Test;
            "#,
            "Test",
        );
    }

    /// MLS §12.4.6: Built-in function vectorization (sin)
    #[test]
    #[ignore = "Automatic vectorization of built-in functions not yet implemented"]
    fn mls_12_4_6_builtin_vectorization() {
        expect_success(
            r#"
            model Test
                Real x[3] = {0, 1, 2};
                Real y[3] = sin(x);
            equation
            end Test;
            "#,
            "Test",
        );
    }
}

// ============================================================================
// §12.4.7 EMPTY FUNCTION CALLS
// ============================================================================

/// MLS §12.4.7: Empty function calls (without result assignment)
mod section_12_4_7_empty_calls {
    use super::*;

    /// MLS §12.4.7: Function call as equation without result
    #[test]
    fn mls_12_4_7_empty_call_equation() {
        expect_parse_success(
            r#"
            function Check
                input Real x;
            algorithm
                assert(x > 0, "x must be positive");
            end Check;

            model Test
                parameter Real p = 5;
            equation
                Check(p);
            end Test;
            "#,
        );
    }

    /// MLS §12.4.7: Function call as statement without result
    #[test]
    fn mls_12_4_7_empty_call_statement() {
        expect_parse_success(
            r#"
            function Check
                input Real x;
            algorithm
                assert(x > 0, "x must be positive");
            end Check;

            function F
                input Real x;
                output Real y;
            algorithm
                Check(x);
                y := x * 2;
            end F;
            "#,
        );
    }

    /// MLS §12.4.7: Built-in function for side effect
    #[test]
    fn mls_12_4_7_builtin_empty_call() {
        expect_parse_success(
            r#"
            function F
                input Real x;
                output Real y;
            algorithm
                assert(x >= 0, "Negative input");
                y := sqrt(x);
            end F;
            "#,
        );
    }
}
