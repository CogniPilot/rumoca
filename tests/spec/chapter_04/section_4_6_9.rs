//! MLS §4.6-4.9: Class Declarations, Specialized Classes, Balance, Predefined Types
//!
//! Tests for:
//! - §4.6: Class declarations
//! - §4.7: Specialized classes (model, block, connector, record, type, function, package)
//! - §4.8: Balanced models
//! - §4.9: Predefined types and classes
//!
//! Reference: https://specification.modelica.org/master/class-predefined-types-declarations.html

use crate::spec::{expect_balanced, expect_parse_failure, expect_parse_success, expect_success};

// ============================================================================
// §4.6 CLASS DECLARATIONS
// ============================================================================

/// MLS §4.6: Class declaration forms
mod section_4_6_class_declarations {
    use super::*;

    #[test]
    fn mls_4_6_empty_class() {
        expect_parse_success("class Empty end Empty;");
    }

    #[test]
    fn mls_4_6_class_with_component() {
        expect_parse_success("class C Real x; end C;");
    }

    #[test]
    fn mls_4_6_class_with_equation() {
        expect_parse_success("class C Real x; equation x = 1; end C;");
    }

    #[test]
    fn mls_4_6_nested_class() {
        expect_parse_success(
            r#"
            class Outer
                class Inner
                    Real x;
                end Inner;
                Inner i;
            end Outer;
            "#,
        );
    }

    #[test]
    fn mls_4_6_encapsulated_class() {
        expect_parse_success("encapsulated class C Real x; end C;");
    }

    #[test]
    fn mls_4_6_partial_class() {
        expect_parse_success("partial class C Real x; end C;");
    }

    #[test]
    fn mls_4_6_final_class() {
        expect_parse_success("final class C Real x; end C;");
    }

    #[test]
    fn mls_4_6_deeply_nested_class() {
        expect_parse_success(
            r#"
            class A
                class B
                    class C
                        Real x;
                    end C;
                end B;
            end A;
            "#,
        );
    }
}

// ============================================================================
// §4.7 SPECIALIZED CLASSES
// ============================================================================

/// MLS §4.7: Specialized class types
mod section_4_7_specialized_classes {
    use super::*;

    // -------------------------------------------------------------------------
    // model
    // -------------------------------------------------------------------------

    #[test]
    fn mls_4_7_model_simple() {
        expect_success(
            r#"
            model SimpleModel
                Real x;
            equation
                der(x) = 1;
            end SimpleModel;
            "#,
            "SimpleModel",
        );
    }

    #[test]
    fn mls_4_7_model_with_parameters() {
        expect_success(
            r#"
            model ParamModel
                parameter Real k = 1;
                Real x(start = 0);
            equation
                der(x) = k;
            end ParamModel;
            "#,
            "ParamModel",
        );
    }

    // -------------------------------------------------------------------------
    // block
    // -------------------------------------------------------------------------

    #[test]
    fn mls_4_7_block_simple() {
        expect_parse_success(
            r#"
            block Gain
                input Real u;
                output Real y;
                parameter Real k = 1;
            equation
                y = k * u;
            end Gain;
            "#,
        );
    }

    #[test]
    fn mls_4_7_block_multiple_io() {
        expect_parse_success(
            r#"
            block MIMO
                input Real u[2];
                output Real y[2];
            equation
                y = u;
            end MIMO;
            "#,
        );
    }

    // -------------------------------------------------------------------------
    // connector
    // -------------------------------------------------------------------------

    #[test]
    fn mls_4_7_connector_simple() {
        expect_parse_success(
            r#"
            connector Pin
                Real v;
                flow Real i;
            end Pin;
            "#,
        );
    }

    #[test]
    fn mls_4_7_connector_expandable() {
        expect_parse_success("expandable connector Bus end Bus;");
    }

    // -------------------------------------------------------------------------
    // record
    // -------------------------------------------------------------------------

    #[test]
    fn mls_4_7_record_simple() {
        expect_parse_success(
            r#"
            record Point
                Real x;
                Real y;
            end Point;
            "#,
        );
    }

    #[test]
    fn mls_4_7_record_with_defaults() {
        expect_parse_success(
            r#"
            record Rectangle
                Real width = 1;
                Real height = 1;
            end Rectangle;
            "#,
        );
    }

    // -------------------------------------------------------------------------
    // type
    // -------------------------------------------------------------------------

    #[test]
    fn mls_4_7_type_alias() {
        expect_parse_success(
            r#"
            type Voltage = Real(unit="V");
            "#,
        );
    }

    #[test]
    fn mls_4_7_type_with_constraints() {
        expect_parse_success(
            r#"
            type PositiveReal = Real(min=0);
            "#,
        );
    }

    // -------------------------------------------------------------------------
    // function
    // -------------------------------------------------------------------------

    #[test]
    fn mls_4_7_function_simple() {
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
    fn mls_4_7_function_multiple_outputs() {
        expect_parse_success(
            r#"
            function DivMod
                input Integer x;
                input Integer y;
                output Integer q;
                output Integer r;
            algorithm
                q := div(x, y);
                r := mod(x, y);
            end DivMod;
            "#,
        );
    }

    #[test]
    fn mls_4_7_function_with_protected() {
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

    // -------------------------------------------------------------------------
    // package
    // -------------------------------------------------------------------------

    #[test]
    fn mls_4_7_package_simple() {
        expect_parse_success(
            r#"
            package MyLib
                constant Real pi = 3.14159;
            end MyLib;
            "#,
        );
    }

    #[test]
    fn mls_4_7_package_with_classes() {
        expect_parse_success(
            r#"
            package Lib
                model M Real x; equation der(x)=1; end M;
                function F input Real x; output Real y; algorithm y:=x; end F;
            end Lib;
            "#,
        );
    }

    // -------------------------------------------------------------------------
    // operator and operator record
    // -------------------------------------------------------------------------

    #[test]
    fn mls_4_7_operator_record() {
        expect_parse_success(
            r#"
            operator record Complex
                Real re;
                Real im;

                encapsulated operator function '+'
                    input Complex c1;
                    input Complex c2;
                    output Complex result;
                algorithm
                    result.re := c1.re + c2.re;
                    result.im := c1.im + c2.im;
                end '+';
            end Complex;
            "#,
        );
    }
}

// ============================================================================
// §4.8 BALANCED MODELS
// ============================================================================

/// MLS §4.8: Balanced model requirements
mod section_4_8_balanced {
    use super::*;

    #[test]
    fn mls_4_8_balanced_simple() {
        expect_balanced("model Test Real x; equation x = 1; end Test;", "Test");
    }

    #[test]
    fn mls_4_8_balanced_ode() {
        expect_balanced(
            "model Test Real x(start=1); equation der(x) = -x; end Test;",
            "Test",
        );
    }

    #[test]
    fn mls_4_8_balanced_two_vars() {
        expect_balanced(
            "model Test Real x; Real y; equation x = 1; y = x; end Test;",
            "Test",
        );
    }

    #[test]
    fn mls_4_8_balanced_algebraic_loop() {
        expect_balanced(
            "model Test Real x; Real y; equation x + y = 1; x - y = 0; end Test;",
            "Test",
        );
    }

    #[test]
    fn mls_4_8_balanced_for_loop() {
        expect_balanced(
            "model Test Real x[5]; equation for i in 1:5 loop x[i] = i; end for; end Test;",
            "Test",
        );
    }
}

// ============================================================================
// §4.9 PREDEFINED TYPES
// ============================================================================

/// MLS §4.9: Predefined types and their attributes
mod section_4_9_predefined_types {
    use super::*;

    // -------------------------------------------------------------------------
    // Real type and attributes
    // -------------------------------------------------------------------------

    #[test]
    fn mls_4_9_real_basic() {
        expect_success("model Test Real x; equation x = 1; end Test;", "Test");
    }

    #[test]
    fn mls_4_9_real_all_attributes() {
        expect_success(
            r#"
            model Test
                Real x(
                    quantity = "Length",
                    unit = "m",
                    displayUnit = "mm",
                    min = 0,
                    max = 100,
                    start = 1,
                    fixed = true,
                    nominal = 1
                );
            equation
                der(x) = 0;
            end Test;
            "#,
            "Test",
        );
    }

    // -------------------------------------------------------------------------
    // Integer type and attributes
    // -------------------------------------------------------------------------

    #[test]
    fn mls_4_9_integer_basic() {
        expect_success("model Test Integer n; equation n = 5; end Test;", "Test");
    }

    #[test]
    fn mls_4_9_integer_attributes() {
        expect_success(
            "model Test Integer n(min = 0, max = 100, start = 0); equation end Test;",
            "Test",
        );
    }

    // -------------------------------------------------------------------------
    // Boolean type and attributes
    // -------------------------------------------------------------------------

    #[test]
    fn mls_4_9_boolean_basic() {
        expect_success("model Test Boolean b; equation b = true; end Test;", "Test");
    }

    #[test]
    fn mls_4_9_boolean_attributes() {
        expect_success(
            "model Test Boolean b(start = false, fixed = true); equation end Test;",
            "Test",
        );
    }

    // -------------------------------------------------------------------------
    // String type
    // -------------------------------------------------------------------------

    #[test]
    fn mls_4_9_string_basic() {
        expect_success(
            r#"model Test String s; equation s = "hello"; end Test;"#,
            "Test",
        );
    }

    // -------------------------------------------------------------------------
    // Enumeration types
    // -------------------------------------------------------------------------

    #[test]
    fn mls_4_9_enum_definition() {
        expect_parse_success("type Color = enumeration(Red, Green, Blue);");
    }

    #[test]
    fn mls_4_9_enum_with_description() {
        expect_parse_success(
            r#"
            type Priority = enumeration(
                Low "Low priority",
                Medium "Normal priority",
                High "High priority"
            );
            "#,
        );
    }

    #[test]
    fn mls_4_9_enum_usage() {
        expect_parse_success(
            r#"
            type State = enumeration(Off, Starting, Running, Stopping);

            model Machine
                State currentState = State.Off;
            equation
            end Machine;
            "#,
        );
    }

    // -------------------------------------------------------------------------
    // Clock type
    // -------------------------------------------------------------------------

    #[test]
    fn mls_4_9_clock_type() {
        expect_parse_success("model Test Clock c; end Test;");
    }
}

// ============================================================================
// PARTIAL CLASSES AND FINAL MODIFIER
// ============================================================================

/// Partial class testing
mod partial_classes {
    use super::*;

    #[test]
    fn partial_model() {
        expect_parse_success(
            r#"
            partial model PartialOscillator
                Real x;
                Real v;
            equation
                der(x) = v;
            end PartialOscillator;
            "#,
        );
    }

    #[test]
    fn extend_partial() {
        expect_success(
            r#"
            partial model PartialDynamics
                Real x;
            end PartialDynamics;

            model ConcreteDynamics
                extends PartialDynamics;
            equation
                der(x) = 1;
            end ConcreteDynamics;
            "#,
            "ConcreteDynamics",
        );
    }
}

/// Final modifier testing
mod final_modifier {
    use super::*;

    #[test]
    fn final_parameter() {
        expect_parse_success(
            r#"
            model WithFinal
                final parameter Real k = 1;
                Real x;
            equation
                der(x) = -k * x;
            end WithFinal;
            "#,
        );
    }

    #[test]
    fn final_component() {
        expect_parse_success(
            r#"
            model Test
                final Real x = 1;
            end Test;
            "#,
        );
    }
}

// ============================================================================
// DECLARATION ERRORS
// ============================================================================

/// Tests for declaration errors that should be rejected
mod declaration_errors {
    use super::*;

    #[test]
    fn error_missing_end_name() {
        expect_parse_failure("model Test Real x; end;");
    }

    #[test]
    fn error_mismatched_end_name() {
        expect_parse_failure("model Test Real x; end Other;");
    }

    #[test]
    fn error_missing_semicolon() {
        expect_parse_failure("model Test Real x end Test;");
    }
}
