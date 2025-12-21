//! MLS §8.5-8.6: Events, Synchronization, and Initialization
//!
//! Tests for:
//! - §8.5: Event-related operators and equations
//! - §8.6: Initialization equations
//!
//! Reference: https://specification.modelica.org/master/equations.html

use crate::spec::expect_success;

// ============================================================================
// §8.5 EVENTS AND SYNCHRONIZATION
// ============================================================================

/// MLS §8.5: Event-related operators and equations
mod section_8_5_events {
    use super::*;

    #[test]
    fn mls_8_5_edge() {
        expect_success(
            r#"
            model Test
                Boolean b(start = false);
                Real x(start = 0);
                discrete Integer count(start = 0);
            equation
                b = x > 1;
                der(x) = 1;
                when edge(b) then
                    count = pre(count) + 1;
                end when;
            end Test;
            "#,
            "Test",
        );
    }

    #[test]
    fn mls_8_5_change() {
        expect_success(
            r#"
            model Test
                discrete Integer i(start = 0);
                Real x(start = 0);
                discrete Integer changes(start = 0);
            equation
                der(x) = 1;
                when x > 1 then
                    i = pre(i) + 1;
                end when;
                when change(i) then
                    changes = pre(changes) + 1;
                end when;
            end Test;
            "#,
            "Test",
        );
    }

    #[test]
    fn mls_8_5_noevent() {
        expect_success(
            r#"
            model Test
                Real x(start = 0);
                Real y;
            equation
                der(x) = 1;
                y = noEvent(if x > 0 then sqrt(x) else 0);
            end Test;
            "#,
            "Test",
        );
    }

    #[test]
    fn mls_8_5_smooth() {
        expect_success(
            r#"
            model Test
                Real x(start = 0);
                Real y;
            equation
                der(x) = 1;
                y = smooth(0, if x > 0 then x else -x);
            end Test;
            "#,
            "Test",
        );
    }

    #[test]
    fn mls_8_5_terminal() {
        expect_success(
            r#"
            model Test
                discrete Real final_value(start = 0);
                Real x(start = 0);
            equation
                der(x) = 1;
                when terminal() then
                    final_value = x;
                end when;
            end Test;
            "#,
            "Test",
        );
    }

    #[test]
    fn mls_8_5_delay() {
        expect_success(
            r#"
            model Test
                Real x(start = 0);
                Real y;
            equation
                der(x) = 1;
                y = delay(x, 0.5);
            end Test;
            "#,
            "Test",
        );
    }

    #[test]
    fn mls_8_5_delay_max() {
        expect_success(
            r#"
            model Test
                Real x(start = 0);
                Real y;
                parameter Real tau = 0.5;
            equation
                der(x) = 1;
                y = delay(x, tau, 1.0);
            end Test;
            "#,
            "Test",
        );
    }

    #[test]
    fn mls_8_5_pre_operator() {
        expect_success(
            r#"
            model Test
                discrete Integer n(start = 0);
                Real x(start = 0);
            equation
                der(x) = 1;
                when x > 1 then
                    n = pre(n) + 1;
                end when;
            end Test;
            "#,
            "Test",
        );
    }

    #[test]
    fn mls_8_5_initial_function() {
        expect_success(
            r#"
            model Test
                Real x;
                Real y;
            equation
                x = if initial() then 0 else 1;
                der(y) = x;
            end Test;
            "#,
            "Test",
        );
    }
}

// ============================================================================
// §8.6 INITIALIZATION
// ============================================================================

/// MLS §8.6: Initialization equations
mod section_8_6_initialization {
    use super::*;

    #[test]
    fn mls_8_6_initial_equation_section() {
        expect_success(
            r#"
            model Test
                Real x;
                Real v;
            initial equation
                x = 0;
                v = 1;
            equation
                der(x) = v;
                der(v) = -x;
            end Test;
            "#,
            "Test",
        );
    }

    #[test]
    fn mls_8_6_start_attribute() {
        expect_success(
            r#"
            model Test
                Real x(start = 1);
            equation
                der(x) = -x;
            end Test;
            "#,
            "Test",
        );
    }

    #[test]
    fn mls_8_6_fixed_attribute() {
        expect_success(
            r#"
            model Test
                Real x(start = 1, fixed = true);
            equation
                der(x) = -x;
            end Test;
            "#,
            "Test",
        );
    }

    #[test]
    fn mls_8_6_mixed_initialization() {
        expect_success(
            r#"
            model Test
                Real x(start = 0);
                Real y;
            initial equation
                y = 10;
            equation
                der(x) = 1;
                y = x + 5;
            end Test;
            "#,
            "Test",
        );
    }

    #[test]
    fn mls_8_6_array_start() {
        expect_success(
            r#"
            model Test
                Real x[3](each start = 0);
            equation
                for i in 1:3 loop
                    der(x[i]) = i;
                end for;
            end Test;
            "#,
            "Test",
        );
    }

    #[test]
    fn mls_8_6_multiple_initial_equations() {
        expect_success(
            r#"
            model Test
                Real x;
                Real y;
                Real z;
            initial equation
                x = 1;
                y = 2;
                z = 3;
            equation
                der(x) = 0;
                der(y) = 0;
                der(z) = 0;
            end Test;
            "#,
            "Test",
        );
    }
}
