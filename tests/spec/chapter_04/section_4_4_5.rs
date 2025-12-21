//! MLS §4.4-4.5: Component Declarations and Variability
//!
//! Tests for:
//! - §4.4: Component declarations and modifications
//! - §4.4.2: Causality prefixes (input/output)
//! - §4.4.4: Inner/outer for hierarchical sharing
//! - §4.5: Component variability (constant, parameter, discrete)
//!
//! Reference: https://specification.modelica.org/master/class-predefined-types-declarations.html

use crate::spec::{expect_parse_success, expect_success};

// ============================================================================
// §4.4 COMPONENT DECLARATIONS
// ============================================================================

/// MLS §4.4: Component declarations
mod section_4_4_components {
    use super::*;

    #[test]
    fn mls_4_4_simple_declaration() {
        expect_success("model Test Real x; equation x = 1; end Test;", "Test");
    }

    #[test]
    fn mls_4_4_multiple_declarations() {
        expect_success(
            "model Test Real x; Real y; Real z; equation x=1; y=2; z=3; end Test;",
            "Test",
        );
    }

    #[test]
    fn mls_4_4_declaration_with_binding() {
        expect_success("model Test Real x = 5; equation end Test;", "Test");
    }

    #[test]
    fn mls_4_4_declaration_with_modification() {
        expect_success(
            "model Test Real x(start = 1.0); equation der(x) = -x; end Test;",
            "Test",
        );
    }

    #[test]
    fn mls_4_4_multiple_modifications() {
        expect_success(
            "model Test Real x(start = 1.0, fixed = true); equation der(x) = -x; end Test;",
            "Test",
        );
    }

    #[test]
    fn mls_4_4_min_max_modification() {
        expect_success(
            "model Test Real x(min = 0, max = 10); equation x = 5; end Test;",
            "Test",
        );
    }

    #[test]
    fn mls_4_4_unit_modification() {
        expect_success(
            r#"model Test Real v(unit = "m/s"); equation v = 10; end Test;"#,
            "Test",
        );
    }

    #[test]
    fn mls_4_4_quantity_modification() {
        expect_success(
            r#"model Test Real v(quantity = "Velocity"); equation v = 10; end Test;"#,
            "Test",
        );
    }

    #[test]
    fn mls_4_4_displayunit_modification() {
        expect_success(
            r#"model Test Real l(unit = "m", displayUnit = "mm"); equation l = 1; end Test;"#,
            "Test",
        );
    }

    #[test]
    fn mls_4_4_nominal_modification() {
        expect_success(
            "model Test Real x(nominal = 1e6); equation x = 1e6; end Test;",
            "Test",
        );
    }

    #[test]
    fn mls_4_4_stateselect_modification() {
        expect_parse_success(
            "model Test Real x(stateSelect = StateSelect.prefer); equation der(x) = 1; end Test;",
        );
    }
}

// ============================================================================
// §4.5 COMPONENT VARIABILITY
// ============================================================================

/// MLS §4.5: Variability prefixes (constant, parameter, discrete)
mod section_4_5_variability {
    use super::*;

    // -------------------------------------------------------------------------
    // Constant variability
    // -------------------------------------------------------------------------

    #[test]
    fn mls_4_5_constant_real() {
        expect_success("model Test constant Real pi = 3.14159; end Test;", "Test");
    }

    #[test]
    fn mls_4_5_constant_integer() {
        expect_success("model Test constant Integer n = 10; end Test;", "Test");
    }

    #[test]
    fn mls_4_5_constant_boolean() {
        expect_success("model Test constant Boolean b = true; end Test;", "Test");
    }

    #[test]
    fn mls_4_5_constant_string() {
        expect_success(
            r#"model Test constant String s = "hello"; end Test;"#,
            "Test",
        );
    }

    #[test]
    fn mls_4_5_constant_expression() {
        expect_success(
            "model Test constant Real x = 2 * 3.14159; end Test;",
            "Test",
        );
    }

    #[test]
    fn mls_4_5_constant_array() {
        expect_success(
            "model Test constant Real x[3] = {1, 2, 3}; end Test;",
            "Test",
        );
    }

    // -------------------------------------------------------------------------
    // Parameter variability
    // -------------------------------------------------------------------------

    #[test]
    fn mls_4_5_parameter_real() {
        expect_success(
            "model Test parameter Real k = 1.0; Real x; equation der(x) = -k*x; end Test;",
            "Test",
        );
    }

    #[test]
    fn mls_4_5_parameter_integer() {
        expect_success(
            "model Test parameter Integer n = 5; Real x[n]; equation for i in 1:n loop x[i]=i; end for; end Test;",
            "Test",
        );
    }

    #[test]
    fn mls_4_5_parameter_boolean() {
        expect_success(
            "model Test parameter Boolean enabled = true; end Test;",
            "Test",
        );
    }

    #[test]
    fn mls_4_5_parameter_default() {
        expect_success("model Test parameter Real k = 1.0; end Test;", "Test");
    }

    #[test]
    fn mls_4_5_parameter_no_default() {
        expect_parse_success("model Test parameter Real k; end Test;");
    }

    #[test]
    fn mls_4_5_parameter_from_constant() {
        expect_success(
            "model Test constant Real c = 2; parameter Real k = c * 3; end Test;",
            "Test",
        );
    }

    #[test]
    fn mls_4_5_parameter_from_parameter() {
        expect_success(
            "model Test parameter Real a = 1; parameter Real b = a + 1; end Test;",
            "Test",
        );
    }

    // -------------------------------------------------------------------------
    // Discrete variability
    // -------------------------------------------------------------------------

    #[test]
    fn mls_4_5_discrete_real() {
        expect_success(
            r#"
            model Test
                discrete Real d(start = 0);
                Real x(start = 0);
            equation
                der(x) = 1;
                when x > 1 then
                    d = pre(d) + 1;
                end when;
            end Test;
            "#,
            "Test",
        );
    }

    #[test]
    fn mls_4_5_discrete_integer() {
        expect_success(
            r#"
            model Test
                discrete Integer count(start = 0);
                Real x(start = 0);
            equation
                der(x) = 1;
                when x > 1 then
                    count = pre(count) + 1;
                end when;
            end Test;
            "#,
            "Test",
        );
    }

    #[test]
    fn mls_4_5_discrete_boolean() {
        expect_success(
            r#"
            model Test
                discrete Boolean triggered(start = false);
                Real x(start = 0);
            equation
                der(x) = 1;
                when x > 1 then
                    triggered = true;
                end when;
            end Test;
            "#,
            "Test",
        );
    }
}

// ============================================================================
// §4.4.2 CAUSALITY PREFIXES
// ============================================================================

/// MLS §4.4.2: Input and output prefixes
mod section_4_4_2_causality {
    use super::*;

    #[test]
    fn mls_4_4_2_input_in_block() {
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
    fn mls_4_4_2_output_in_block() {
        expect_parse_success(
            r#"
            block Generator
                output Real y;
            equation
                y = 1;
            end Generator;
            "#,
        );
    }

    #[test]
    fn mls_4_4_2_multiple_inputs() {
        expect_parse_success(
            r#"
            block Add
                input Real u1;
                input Real u2;
                output Real y;
            equation
                y = u1 + u2;
            end Add;
            "#,
        );
    }

    #[test]
    fn mls_4_4_2_multiple_outputs() {
        expect_parse_success(
            r#"
            block SinCos
                input Real x;
                output Real s;
                output Real c;
            equation
                s = sin(x);
                c = cos(x);
            end SinCos;
            "#,
        );
    }

    #[test]
    fn mls_4_4_2_input_array() {
        expect_parse_success(
            r#"
            block VectorGain
                input Real u[3];
                output Real y[3];
                parameter Real k = 2;
            equation
                y = k * u;
            end VectorGain;
            "#,
        );
    }
}

// ============================================================================
// §4.4.4 INNER AND OUTER
// ============================================================================

/// MLS §4.4.4: Inner/outer for hierarchical component sharing
mod section_4_4_4_inner_outer {
    use super::*;

    #[test]
    fn mls_4_4_4_inner_declaration() {
        expect_parse_success(
            r#"
            model World
                inner Real g = 9.81;
            end World;
            "#,
        );
    }

    #[test]
    fn mls_4_4_4_outer_reference() {
        expect_parse_success(
            r#"
            model Ball
                outer Real g;
                Real h(start = 10);
                Real v(start = 0);
            equation
                der(h) = v;
                der(v) = -g;
            end Ball;
            "#,
        );
    }

    #[test]
    fn mls_4_4_4_inner_outer_system() {
        expect_parse_success(
            r#"
            model Environment
                inner Real g = 9.81;
            end Environment;

            model Ball
                outer Real g;
                Real h(start = 10);
                Real v(start = 0);
            equation
                der(h) = v;
                der(v) = -g;
            end Ball;

            model System
                inner Real g = 9.81;
                Ball ball;
            end System;
            "#,
        );
    }

    #[test]
    fn mls_4_4_4_inner_parameter() {
        expect_parse_success(
            r#"
            model Container
                inner parameter Real g = 9.81;
            end Container;
            "#,
        );
    }
}
