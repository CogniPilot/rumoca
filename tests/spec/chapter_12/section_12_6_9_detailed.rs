//! MLS §12.6-12.9: Detailed Conformance Tests
//!
//! Comprehensive tests for:
//! - §12.6 Record constructor functions
//! - §12.7 Derivatives and inverses of functions
//! - §12.8 Function inlining and event generation
//! - §12.9 External function interface
//!
//! Reference: https://specification.modelica.org/master/functions.html

use crate::spec::expect_parse_success;

// ============================================================================
// §12.6 RECORD CONSTRUCTOR FUNCTIONS - DETAILED
// ============================================================================

/// MLS §12.6: Detailed record constructor tests
mod section_12_6_detailed {
    use super::*;

    // -------------------------------------------------------------------------
    // §12.6-1: Record constructor global scope reference
    // -------------------------------------------------------------------------

    /// MLS §12.6-1: Record constructor can be called on global record
    #[test]
    fn mls_12_6_1_global_record_constructor() {
        expect_parse_success(
            r#"
            record GlobalPoint
                Real x;
                Real y;
            end GlobalPoint;

            model Test
                GlobalPoint p1 = GlobalPoint(1, 2);
                GlobalPoint p2 = GlobalPoint(x = 3, y = 4);
            end Test;
            "#,
        );
    }

    /// MLS §12.6-1: Record constructor with package-qualified name
    #[test]
    fn mls_12_6_1_package_qualified_constructor() {
        expect_parse_success(
            r#"
            package Geom
                record Point
                    Real x;
                    Real y;
                end Point;
            end Geom;

            model Test
                Geom.Point p = Geom.Point(1, 2);
            end Test;
            "#,
        );
    }

    // -------------------------------------------------------------------------
    // §12.6-2: Protected components (final parameter)
    // -------------------------------------------------------------------------

    /// MLS §12.6-2: Record with final parameter becomes protected
    #[test]
    fn mls_12_6_2_final_parameter_protected() {
        expect_parse_success(
            r#"
            record Config
                final parameter Real version = 1.0;
                Real value;
            end Config;

            model Test
                Config c = Config(value = 5);
            end Test;
            "#,
        );
    }

    // -------------------------------------------------------------------------
    // §12.6-3: Prefix removal in constructor
    // -------------------------------------------------------------------------

    /// MLS §12.6-3: Parameter prefix is removed in constructor args
    #[test]
    fn mls_12_6_3_parameter_prefix_removed() {
        expect_parse_success(
            r#"
            record Settings
                parameter Real tolerance = 1e-6;
                parameter Integer maxIter = 100;
            end Settings;

            model Test
                Settings s = Settings(1e-8, 200);
            end Test;
            "#,
        );
    }

    /// MLS §12.6-3: Constant prefix handling
    #[test]
    fn mls_12_6_3_constant_prefix() {
        expect_parse_success(
            r#"
            record Constants
                constant Real pi = 3.14159;
                Real radius;
            end Constants;

            model Test
                Constants c = Constants(radius = 5);
            end Test;
            "#,
        );
    }

    // -------------------------------------------------------------------------
    // §12.6-4: Input prefix added to public components
    // -------------------------------------------------------------------------

    /// MLS §12.6-4: All public components become inputs
    #[test]
    fn mls_12_6_4_public_as_inputs() {
        expect_parse_success(
            r#"
            record Vector3D
                Real x;
                Real y;
                Real z;
            end Vector3D;

            model Test
                Vector3D v = Vector3D(1, 2, 3);
            end Test;
            "#,
        );
    }

    // -------------------------------------------------------------------------
    // §12.6-5: Nested record constructor
    // -------------------------------------------------------------------------

    /// MLS §12.6-5: Nested record uses nested constructor
    #[test]
    fn mls_12_6_5_nested_constructor() {
        expect_parse_success(
            r#"
            record Point2D
                Real x;
                Real y;
            end Point2D;

            record Rectangle
                Point2D corner1;
                Point2D corner2;
            end Rectangle;

            model Test
                Rectangle r = Rectangle(
                    Point2D(0, 0),
                    Point2D(10, 10)
                );
            end Test;
            "#,
        );
    }

    /// MLS §12.6-5: Deeply nested record constructor
    #[test]
    #[ignore = "Parser doesn't handle multiple record definitions with nested types"]
    fn mls_12_6_5_deeply_nested() {
        expect_parse_success(
            r#"
            record Inner
                Real value;
            end Inner;

            record Middle
                Inner inner;
            end Middle;

            record Outer
                Middle middle;
            end Outer;

            model Test
                Outer o = Outer(Middle(Inner(42)));
            end Test;
            "#,
        );
    }

    // -------------------------------------------------------------------------
    // Record constructor with mixed arguments
    // -------------------------------------------------------------------------

    /// Mixed positional and named arguments in record constructor
    #[test]
    fn mls_12_6_mixed_arguments() {
        expect_parse_success(
            r#"
            record Config
                Real a;
                Real b;
                Real c;
                Real d;
            end Config;

            model Test
                Config c = Config(1, 2, d = 4, c = 3);
            end Test;
            "#,
        );
    }

    /// Record constructor in array initialization
    #[test]
    fn mls_12_6_array_of_records() {
        expect_parse_success(
            r#"
            record Point
                Real x;
                Real y;
            end Point;

            model Test
                Point triangle[3] = {
                    Point(0, 0),
                    Point(1, 0),
                    Point(0.5, 1)
                };
            end Test;
            "#,
        );
    }
}

// ============================================================================
// §12.7 DERIVATIVES AND INVERSES - DETAILED
// ============================================================================

/// MLS §12.7: Detailed derivative and inverse tests
mod section_12_7_detailed {
    use super::*;

    // -------------------------------------------------------------------------
    // §12.7-1: Inverse annotation
    // -------------------------------------------------------------------------

    /// MLS §12.7-1: Function with one output may have inverse
    #[test]
    fn mls_12_7_1_single_output_inverse() {
        expect_parse_success(
            r#"
            function F
                input Real x;
                output Real y;
            algorithm
                y := x^3;
            annotation(inverse(x = F_inv(y)));
            end F;

            function F_inv
                input Real y;
                output Real x;
            algorithm
                x := y^(1/3);
            end F_inv;
            "#,
        );
    }

    /// MLS §12.7-1: Multiple inverses for different inputs
    #[test]
    fn mls_12_7_1_multiple_inverses() {
        expect_parse_success(
            r#"
            function TwoInputs
                input Real a;
                input Real b;
                output Real y;
            algorithm
                y := a * b;
            annotation(
                inverse(a = InvA(b, y)),
                inverse(b = InvB(a, y))
            );
            end TwoInputs;

            function InvA
                input Real b;
                input Real y;
                output Real a;
            algorithm
                a := y / b;
            end InvA;

            function InvB
                input Real a;
                input Real y;
                output Real b;
            algorithm
                b := y / a;
            end InvB;
            "#,
        );
    }

    // -------------------------------------------------------------------------
    // §12.7-2, §12.7-3: Derivative restrictions and ordering
    // -------------------------------------------------------------------------

    /// MLS §12.7-2: Valid first-order derivative
    #[test]
    fn mls_12_7_2_first_order_derivative() {
        expect_parse_success(
            r#"
            function Polynomial
                input Real x;
                output Real y;
            algorithm
                y := x^3 + 2*x^2 + x + 1;
            annotation(derivative = Polynomial_der);
            end Polynomial;

            function Polynomial_der
                input Real x;
                input Real der_x;
                output Real der_y;
            algorithm
                der_y := (3*x^2 + 4*x + 1) * der_x;
            end Polynomial_der;
            "#,
        );
    }

    /// MLS §12.7-3: Multiple derivative orders (most restrictive first)
    #[test]
    fn mls_12_7_3_ordered_derivatives() {
        expect_parse_success(
            r#"
            function G
                input Real x;
                output Real y;
            algorithm
                y := x^4;
            annotation(
                derivative(order=1) = G_der1,
                derivative(order=2) = G_der2
            );
            end G;

            function G_der1
                input Real x;
                input Real der_x;
                output Real der_y;
            algorithm
                der_y := 4*x^3 * der_x;
            end G_der1;

            function G_der2
                input Real x;
                input Real der_x;
                input Real der2_x;
                output Real der2_y;
            algorithm
                der2_y := 12*x^2 * der_x^2 + 4*x^3 * der2_x;
            end G_der2;
            "#,
        );
    }

    // -------------------------------------------------------------------------
    // §12.7-4: Real inputs requirement
    // -------------------------------------------------------------------------

    /// MLS §12.7-4: Function must have Real input for derivative
    #[test]
    fn mls_12_7_4_real_input_derivative() {
        expect_parse_success(
            r#"
            function RealFunc
                input Real x;
                input Real y;
                output Real z;
            algorithm
                z := x * y;
            annotation(derivative = RealFunc_der);
            end RealFunc;

            function RealFunc_der
                input Real x;
                input Real y;
                input Real der_x;
                input Real der_y;
                output Real der_z;
            algorithm
                der_z := der_x * y + x * der_y;
            end RealFunc_der;
            "#,
        );
    }

    // -------------------------------------------------------------------------
    // §12.7-5: Name and order preservation
    // -------------------------------------------------------------------------

    /// MLS §12.7-5: Same name, type, order in derivative
    #[test]
    fn mls_12_7_5_preserved_parameters() {
        expect_parse_success(
            r#"
            function ThreeInputs
                input Real a;
                input Real b;
                input Real c;
                output Real y;
            algorithm
                y := a*b + b*c + c*a;
            annotation(derivative = ThreeInputs_der);
            end ThreeInputs;

            function ThreeInputs_der
                input Real a;
                input Real b;
                input Real c;
                input Real der_a;
                input Real der_b;
                input Real der_c;
                output Real der_y;
            algorithm
                der_y := der_a*b + a*der_b + der_b*c + b*der_c + der_c*a + c*der_a;
            end ThreeInputs_der;
            "#,
        );
    }

    // -------------------------------------------------------------------------
    // §12.7-6: zeroDerivative annotation
    // -------------------------------------------------------------------------

    /// MLS §12.7-6: zeroDerivative for constant input
    #[test]
    fn mls_12_7_6_zero_derivative() {
        expect_parse_success(
            r#"
            function ScaledValue
                input Real x;
                input Real scale;
                output Real y;
            algorithm
                y := x * scale;
            annotation(derivative(zeroDerivative=scale) = ScaledValue_der);
            end ScaledValue;

            function ScaledValue_der
                input Real x;
                input Real scale;
                input Real der_x;
                output Real der_y;
            algorithm
                der_y := der_x * scale;
            end ScaledValue_der;
            "#,
        );
    }
}

// ============================================================================
// §12.8 FUNCTION INLINING AND EVENTS - DETAILED
// ============================================================================

/// MLS §12.8: Detailed inlining and event tests
mod section_12_8_detailed {
    use super::*;

    /// Multiple annotations combined
    #[test]
    fn mls_12_8_combined_annotations() {
        expect_parse_success(
            r#"
            function OptimizedStep
                input Real x;
                output Real y;
            algorithm
                if x > 0 then
                    y := 1;
                else
                    y := 0;
                end if;
            annotation(
                Inline = true,
                GenerateEvents = true
            );
            end OptimizedStep;
            "#,
        );
    }

    /// InlineAfterIndexReduction annotation
    #[test]
    fn mls_12_8_inline_after_index_reduction() {
        expect_parse_success(
            r#"
            function DelayedOpt
                input Real x;
                output Real y;
            algorithm
                y := x * x;
            annotation(InlineAfterIndexReduction = true);
            end DelayedOpt;
            "#,
        );
    }

    /// smoothOrder annotation
    #[test]
    fn mls_12_8_smooth_order() {
        expect_parse_success(
            r#"
            function SmoothFunc
                input Real x;
                output Real y;
            algorithm
                y := if x > 0 then x^2 else x^2;
            annotation(smoothOrder = 1);
            end SmoothFunc;
            "#,
        );
    }
}

// ============================================================================
// §12.9 EXTERNAL FUNCTION INTERFACE - DETAILED
// ============================================================================

/// MLS §12.9: Detailed external function tests
mod section_12_9_detailed {
    use super::*;

    // -------------------------------------------------------------------------
    // Basic external declarations
    // -------------------------------------------------------------------------

    /// External function with explicit return mapping
    #[test]
    fn mls_12_9_explicit_return() {
        expect_parse_success(
            r#"
            function ExtSqrt
                input Real x;
                output Real y;
            external "C" y = sqrt(x);
            end ExtSqrt;
            "#,
        );
    }

    /// External function with no return (void)
    #[test]
    fn mls_12_9_void_external() {
        expect_parse_success(
            r#"
            function ExtPrint
                input Real x;
            external "C" printf(x);
            end ExtPrint;
            "#,
        );
    }

    /// External with different function name
    #[test]
    fn mls_12_9_renamed_external() {
        expect_parse_success(
            r#"
            function MySin
                input Real x;
                output Real y;
            external "C" y = my_custom_sin(x);
            end MySin;
            "#,
        );
    }

    // -------------------------------------------------------------------------
    // External with annotations
    // -------------------------------------------------------------------------

    /// Library annotation
    #[test]
    fn mls_12_9_library_annotation() {
        expect_parse_success(
            r#"
            function ExtFunc
                input Real x;
                output Real y;
            external "C"
            annotation(Library = "mymath");
            end ExtFunc;
            "#,
        );
    }

    /// Multiple library annotation
    #[test]
    fn mls_12_9_multiple_libraries() {
        expect_parse_success(
            r#"
            function ExtFunc
                input Real x;
                output Real y;
            external "C"
            annotation(Library = {"lib1", "lib2"});
            end ExtFunc;
            "#,
        );
    }

    /// LibraryDirectory annotation
    #[test]
    fn mls_12_9_library_directory() {
        expect_parse_success(
            r#"
            function ExtFunc
                input Real x;
                output Real y;
            external "C"
            annotation(
                Library = "custom",
                LibraryDirectory = "modelica://MyLib/Resources/Library"
            );
            end ExtFunc;
            "#,
        );
    }

    /// IncludeDirectory annotation
    #[test]
    fn mls_12_9_include_directory() {
        expect_parse_success(
            r#"
            function ExtFunc
                input Real x;
                output Real y;
            external "C"
            annotation(
                IncludeDirectory = "modelica://MyLib/Resources/Include"
            );
            end ExtFunc;
            "#,
        );
    }

    // -------------------------------------------------------------------------
    // Array handling in external functions
    // -------------------------------------------------------------------------

    /// External with 1D array input
    #[test]
    fn mls_12_9_array_input() {
        expect_parse_success(
            r#"
            function ExtArraySum
                input Real x[:];
                output Real s;
            external "C" s = sum_array(x, size(x, 1));
            end ExtArraySum;
            "#,
        );
    }

    /// External with 2D array
    #[test]
    fn mls_12_9_matrix_input() {
        expect_parse_success(
            r#"
            function ExtMatrixOp
                input Real A[:,:];
                output Real det;
            external "C" det = matrix_det(A, size(A, 1), size(A, 2));
            end ExtMatrixOp;
            "#,
        );
    }

    /// External with array output
    #[test]
    fn mls_12_9_array_output() {
        expect_parse_success(
            r#"
            function ExtLinspace
                input Real start;
                input Real stop;
                input Integer n;
                output Real result[n];
            external "C" make_linspace(start, stop, n, result);
            end ExtLinspace;
            "#,
        );
    }

    // -------------------------------------------------------------------------
    // Different external languages
    // -------------------------------------------------------------------------

    /// FORTRAN 77 external
    #[test]
    fn mls_12_9_fortran77() {
        expect_parse_success(
            r#"
            function FortranBlas
                input Real x[:];
                input Real y[:];
                output Real dot;
            external "FORTRAN 77" dot = ddot(size(x, 1), x, 1, y, 1);
            end FortranBlas;
            "#,
        );
    }

    /// Built-in external (default)
    #[test]
    fn mls_12_9_builtin_external() {
        expect_parse_success(
            r#"
            function BuiltinSin
                input Real x;
                output Real y;
            external "builtin" y = sin(x);
            end BuiltinSin;
            "#,
        );
    }

    // -------------------------------------------------------------------------
    // External with multiple outputs
    // -------------------------------------------------------------------------

    /// External with multiple output parameters
    #[test]
    fn mls_12_9_multiple_outputs() {
        expect_parse_success(
            r#"
            function ExtSinCos
                input Real x;
                output Real s;
                output Real c;
            external "C" sincos(x, s, c);
            end ExtSinCos;
            "#,
        );
    }

    /// External with both return value and output parameters
    #[test]
    fn mls_12_9_mixed_outputs() {
        expect_parse_success(
            r#"
            function ExtCompute
                input Real x;
                output Real y;
                output Integer status;
            external "C" y = compute_with_status(x, status);
            end ExtCompute;
            "#,
        );
    }
}
