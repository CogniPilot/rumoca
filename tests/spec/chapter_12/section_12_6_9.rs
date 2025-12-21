//! MLS §12.6-12.9: Record Constructors, Derivatives, and External Functions
//!
//! Tests for:
//! - §12.6 Record constructor functions
//! - §12.7 Derivatives and inverses of functions
//! - §12.8 Function inlining and event generation
//! - §12.9 External function interface
//!
//! Reference: https://specification.modelica.org/master/functions.html

use crate::spec::expect_parse_success;

// ============================================================================
// §12.6 RECORD CONSTRUCTOR FUNCTIONS
// ============================================================================

/// MLS §12.6: Record constructors
mod section_12_6_record_constructor {
    use super::*;

    #[test]
    fn mls_12_6_record_constructor() {
        expect_parse_success(
            r#"
            record Point
                Real x;
                Real y;
            end Point;

            model Test
                Point p = Point(1, 2);
            end Test;
            "#,
        );
    }

    #[test]
    fn mls_12_6_record_named_constructor() {
        expect_parse_success(
            r#"
            record Rectangle
                Real width;
                Real height;
            end Rectangle;

            model Test
                Rectangle r = Rectangle(height = 10, width = 5);
            end Test;
            "#,
        );
    }

    #[test]
    fn mls_12_6_record_with_defaults() {
        expect_parse_success(
            r#"
            record Config
                Real tolerance = 1e-6;
                Integer maxIterations = 100;
                Boolean verbose = false;
            end Config;

            model Test
                Config c1 = Config();
                Config c2 = Config(tolerance = 1e-8);
                Config c3 = Config(1e-10, 200, true);
            end Test;
            "#,
        );
    }

    #[test]
    fn mls_12_6_nested_record_constructor() {
        expect_parse_success(
            r#"
            record Point
                Real x;
                Real y;
            end Point;

            record Line
                Point start;
                Point finish;
            end Line;

            model Test
                Line l = Line(Point(0, 0), Point(1, 1));
            end Test;
            "#,
        );
    }

    #[test]
    fn mls_12_6_record_array_constructor() {
        expect_parse_success(
            r#"
            record Complex
                Real re;
                Real im;
            end Complex;

            model Test
                Complex c[3] = {Complex(1, 0), Complex(0, 1), Complex(1, 1)};
            end Test;
            "#,
        );
    }
}

// ============================================================================
// §12.7 DERIVATIVES AND INVERSES OF FUNCTIONS
// ============================================================================

/// MLS §12.7: Function derivatives (annotation)
mod section_12_7_derivatives {
    use super::*;

    #[test]
    fn mls_12_7_derivative_annotation() {
        expect_parse_success(
            r#"
            function MySin
                input Real x;
                output Real y;
            algorithm
                y := sin(x);
            annotation(derivative = MyCos);
            end MySin;

            function MyCos
                input Real x;
                output Real y;
            algorithm
                y := cos(x);
            end MyCos;
            "#,
        );
    }

    #[test]
    fn mls_12_7_derivative_order() {
        expect_parse_success(
            r#"
            function F
                input Real x;
                output Real y;
            algorithm
                y := x^3;
            annotation(derivative(order=1) = F_der);
            end F;

            function F_der
                input Real x;
                input Real der_x;
                output Real der_y;
            algorithm
                der_y := 3*x^2*der_x;
            end F_der;
            "#,
        );
    }

    #[test]
    fn mls_12_7_inverse_annotation() {
        expect_parse_success(
            r#"
            function Square
                input Real x;
                output Real y;
            algorithm
                y := x * x;
            annotation(inverse(x = InvSquare(y)));
            end Square;

            function InvSquare
                input Real y;
                output Real x;
            algorithm
                x := sqrt(y);
            end InvSquare;
            "#,
        );
    }
}

// ============================================================================
// §12.8 FUNCTION INLINING AND EVENT GENERATION
// ============================================================================

/// MLS §12.8: Function annotations for inlining and events
mod section_12_8_annotations {
    use super::*;

    #[test]
    fn mls_12_8_inline_annotation() {
        expect_parse_success(
            r#"
            function FastSquare
                input Real x;
                output Real y;
            algorithm
                y := x * x;
            annotation(Inline = true);
            end FastSquare;
            "#,
        );
    }

    #[test]
    fn mls_12_8_late_inline() {
        expect_parse_success(
            r#"
            function DelayedInline
                input Real x;
                output Real y;
            algorithm
                y := x * x;
            annotation(LateInline = true);
            end DelayedInline;
            "#,
        );
    }

    #[test]
    fn mls_12_8_generate_events() {
        expect_parse_success(
            r#"
            function StepFunction
                input Real x;
                output Real y;
            algorithm
                if x > 0 then
                    y := 1;
                else
                    y := 0;
                end if;
            annotation(GenerateEvents = true);
            end StepFunction;
            "#,
        );
    }
}

// ============================================================================
// §12.9 EXTERNAL FUNCTION INTERFACE
// ============================================================================

/// MLS §12.9: External functions
mod section_12_9_external {
    use super::*;

    #[test]
    fn mls_12_9_external_c() {
        expect_parse_success(
            r#"
            function ExternalSin
                input Real x;
                output Real y;
            external "C" y = sin(x);
            end ExternalSin;
            "#,
        );
    }

    #[test]
    fn mls_12_9_external_c_implicit() {
        expect_parse_success(
            r#"
            function MyCos
                input Real x;
                output Real y;
            external "C";
            end MyCos;
            "#,
        );
    }

    #[test]
    fn mls_12_9_external_with_library() {
        expect_parse_success(
            r#"
            function MyExternal
                input Real x;
                output Real y;
            external "C"
            annotation(Library = "mylib");
            end MyExternal;
            "#,
        );
    }

    #[test]
    fn mls_12_9_external_with_include() {
        expect_parse_success(
            r##"
            function CustomFunc
                input Real x;
                output Real y;
            external "C"
            annotation(
                Library = "custom",
                Include = "#include \"custom.h\""
            );
            end CustomFunc;
            "##,
        );
    }

    #[test]
    fn mls_12_9_external_multiple_outputs() {
        expect_parse_success(
            r#"
            function DivMod
                input Integer a;
                input Integer b;
                output Integer q;
                output Integer r;
            external "C" divmod(a, b, q, r);
            end DivMod;
            "#,
        );
    }

    #[test]
    fn mls_12_9_external_array() {
        expect_parse_success(
            r#"
            function ArraySum
                input Real x[:];
                input Integer n;
                output Real s;
            external "C" s = array_sum(x, n);
            end ArraySum;
            "#,
        );
    }

    #[test]
    fn mls_12_9_external_fortran() {
        expect_parse_success(
            r#"
            function FortranFunc
                input Real x;
                output Real y;
            external "FORTRAN 77" y = ffunc(x);
            end FortranFunc;
            "#,
        );
    }
}
