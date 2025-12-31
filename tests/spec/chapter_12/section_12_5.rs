//! MLS §12.5: Built-in Functions
//!
//! Tests for:
//! - §12.5 Mathematical built-in functions (sin, cos, exp, log, etc.)
//! - §12.5 Event-related functions (pre, edge, change, sample, etc.)
//!
//! Reference: https://specification.modelica.org/master/functions.html

use crate::spec::{expect_failure, expect_success};

// ============================================================================
// §12.5 BUILT-IN MATHEMATICAL FUNCTIONS
// ============================================================================

/// MLS §12.5: Trigonometric functions
mod section_12_5_trigonometric {
    use super::*;

    #[test]
    fn mls_12_5_sin() {
        expect_success("model Test Real x = sin(0.5); end Test;", "Test");
    }

    #[test]
    fn mls_12_5_cos() {
        expect_success("model Test Real x = cos(0.5); end Test;", "Test");
    }

    #[test]
    fn mls_12_5_tan() {
        expect_success("model Test Real x = tan(0.5); end Test;", "Test");
    }

    #[test]
    fn mls_12_5_asin() {
        expect_success("model Test Real x = asin(0.5); end Test;", "Test");
    }

    #[test]
    fn mls_12_5_acos() {
        expect_success("model Test Real x = acos(0.5); end Test;", "Test");
    }

    #[test]
    fn mls_12_5_atan() {
        expect_success("model Test Real x = atan(0.5); end Test;", "Test");
    }

    #[test]
    fn mls_12_5_atan2() {
        expect_success(
            "model Test Real y=1; Real x=1; Real a=atan2(y,x); end Test;",
            "Test",
        );
    }

    #[test]
    fn mls_12_5_trig_expressions() {
        expect_success(
            "model Test Real x = sin(0.5) + cos(0.5); Real y = tan(atan(1)); end Test;",
            "Test",
        );
    }
}

/// MLS §12.5: Hyperbolic functions
mod section_12_5_hyperbolic {
    use super::*;

    #[test]
    fn mls_12_5_sinh() {
        expect_success("model Test Real x = sinh(1); end Test;", "Test");
    }

    #[test]
    fn mls_12_5_cosh() {
        expect_success("model Test Real x = cosh(1); end Test;", "Test");
    }

    #[test]
    fn mls_12_5_tanh() {
        expect_success("model Test Real x = tanh(1); end Test;", "Test");
    }
}

/// MLS §12.5: Exponential and logarithmic functions
mod section_12_5_exponential {
    use super::*;

    #[test]
    fn mls_12_5_exp() {
        expect_success("model Test Real x = exp(1); end Test;", "Test");
    }

    #[test]
    fn mls_12_5_log() {
        expect_success("model Test Real x = log(2.718); end Test;", "Test");
    }

    #[test]
    fn mls_12_5_log10() {
        expect_success("model Test Real x = log10(100); end Test;", "Test");
    }

    #[test]
    fn mls_12_5_exp_log_identity() {
        expect_success(
            "model Test Real x = exp(log(5)); Real y = log(exp(3)); end Test;",
            "Test",
        );
    }
}

/// MLS §12.5: Power and root functions
mod section_12_5_power {
    use super::*;

    #[test]
    fn mls_12_5_sqrt() {
        expect_success("model Test Real x = sqrt(4); end Test;", "Test");
    }

    #[test]
    fn mls_12_5_power_operator() {
        expect_success("model Test Real x = 2^3; Real y = 4^0.5; end Test;", "Test");
    }
}

/// MLS §12.5: Utility functions
mod section_12_5_utility {
    use super::*;

    #[test]
    fn mls_12_5_abs() {
        expect_success("model Test Real x = abs(-5); end Test;", "Test");
    }

    #[test]
    fn mls_12_5_sign() {
        expect_success("model Test Real x = sign(-5); end Test;", "Test");
    }

    #[test]
    fn mls_12_5_min_scalar() {
        expect_success("model Test Real x = min(3, 5); end Test;", "Test");
    }

    #[test]
    fn mls_12_5_max_scalar() {
        expect_success("model Test Real x = max(3, 5); end Test;", "Test");
    }

    #[test]
    fn mls_12_5_floor() {
        expect_success("model Test Real x = floor(3.7); end Test;", "Test");
    }

    #[test]
    fn mls_12_5_ceil() {
        expect_success("model Test Real x = ceil(3.2); end Test;", "Test");
    }

    #[test]
    fn mls_12_5_integer() {
        expect_success("model Test Integer n = integer(3.7); end Test;", "Test");
    }

    #[test]
    fn mls_12_5_div_int() {
        expect_success("model Test Integer x = div(7, 3); end Test;", "Test");
    }

    #[test]
    fn mls_12_5_mod() {
        expect_success("model Test Integer x = mod(7, 3); end Test;", "Test");
    }

    #[test]
    fn mls_12_5_rem() {
        expect_success("model Test Real x = rem(7.5, 2.5); end Test;", "Test");
    }

    #[test]
    fn mls_12_5_abs_sign_combo() {
        expect_success(
            "model Test Real x = -5; Real y = abs(x) * sign(x); end Test;",
            "Test",
        );
    }
}

// ============================================================================
// §12.5 EVENT-RELATED FUNCTIONS
// ============================================================================

/// MLS §12.5: Event-related functions
mod section_12_5_event_functions {
    use super::*;

    #[test]
    fn mls_12_5_pre() {
        expect_success(
            r#"
            model Test
                Real x(start = 0);
                discrete Real d(start = 0);
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
    fn mls_12_5_edge() {
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
    fn mls_12_5_change() {
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
    fn mls_12_5_sample() {
        expect_success(
            r#"
            model Test
                discrete Real x(start = 0);
            equation
                when sample(0, 0.1) then
                    x = pre(x) + 1;
                end when;
            end Test;
            "#,
            "Test",
        );
    }

    #[test]
    fn mls_12_5_delay() {
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
    fn mls_12_5_delay_with_max() {
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
    fn mls_12_5_reinit() {
        expect_success(
            r#"
            model Test
                Real x(start = 0);
            equation
                der(x) = 1;
                when x > 1 then
                    reinit(x, 0);
                end when;
            end Test;
            "#,
            "Test",
        );
    }

    #[test]
    fn mls_12_5_terminal() {
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
    fn mls_12_5_noevent() {
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
    fn mls_12_5_smooth() {
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
    fn mls_12_5_initial() {
        expect_success(
            r#"
            model Test
                Real x(start = 0);
                Boolean isInitial = initial();
            equation
                der(x) = if isInitial then 0 else 1;
            end Test;
            "#,
            "Test",
        );
    }
}

/// MLS §12.5: Cardinality function
mod section_12_5_cardinality {
    use super::*;

    /// MLS §3.7: "cardinality may only be used in the condition of an if-statement or an assert"
    /// Using cardinality in a binding expression is not allowed.
    #[test]
    fn mls_12_5_cardinality_invalid_context() {
        expect_failure(
            r#"
            connector C
                Real v;
                flow Real i;
            end C;

            model Test
                C c;
                Integer n = cardinality(c);
            equation
                c.v = 0;
            end Test;
            "#,
            "Test",
        );
    }

    /// MLS §3.7: cardinality is allowed in if-equation conditions
    #[test]
    fn mls_12_5_cardinality_valid_in_if() {
        expect_success(
            r#"
            connector C
                Real v;
                flow Real i;
            end C;

            model Test
                C c;
                Real y;
            equation
                c.v = 0;
                if cardinality(c) == 0 then
                    y = 0;
                else
                    y = 1;
                end if;
            end Test;
            "#,
            "Test",
        );
    }
}
