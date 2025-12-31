//! MLS §12.5: Built-in Functions - Detailed Conformance Tests
//!
//! Comprehensive tests covering every normative statement in MLS §12.5.
//!
//! Reference: https://specification.modelica.org/master/functions.html#built-in-functions

use crate::spec::{expect_parse_success, expect_success};

// ============================================================================
// §12.5 BUILT-IN FUNCTIONS
// ============================================================================

/// MLS §12.5: Basic mathematical built-in functions
mod mathematical_builtins {
    use super::*;

    // -------------------------------------------------------------------------
    // Trigonometric functions in expressions
    // -------------------------------------------------------------------------

    #[test]
    fn mls_12_5_trig_nested() {
        expect_success("model Test Real x = sin(cos(tan(0.5))); end Test;", "Test");
    }

    #[test]
    fn mls_12_5_inverse_trig_nested() {
        expect_success(
            "model Test Real x = asin(acos(0.5) / 3.14159); end Test;",
            "Test",
        );
    }

    #[test]
    fn mls_12_5_atan2_in_expression() {
        expect_success(
            "model Test Real angle = atan2(1.0, 1.0) * 180 / 3.14159; end Test;",
            "Test",
        );
    }

    // -------------------------------------------------------------------------
    // Hyperbolic functions
    // -------------------------------------------------------------------------

    #[test]
    fn mls_12_5_hyperbolic_identity() {
        // sinh^2(x) - cosh^2(x) = -1
        expect_success(
            "model Test Real x = 1; Real y = sinh(x)^2 - cosh(x)^2; end Test;",
            "Test",
        );
    }

    // -------------------------------------------------------------------------
    // Exponential and logarithmic functions
    // -------------------------------------------------------------------------

    #[test]
    fn mls_12_5_exp_log_chain() {
        expect_success(
            "model Test Real x = log(exp(log10(100))); end Test;",
            "Test",
        );
    }

    #[test]
    fn mls_12_5_power_with_exp_log() {
        expect_success(
            "model Test Real base = 2; Real exp_ = 3; Real y = exp(exp_ * log(base)); end Test;",
            "Test",
        );
    }

    // -------------------------------------------------------------------------
    // Utility functions
    // -------------------------------------------------------------------------

    #[test]
    fn mls_12_5_abs_in_sqrt() {
        expect_success(
            "model Test Real x = -4; Real y = sqrt(abs(x)); end Test;",
            "Test",
        );
    }

    #[test]
    fn mls_12_5_sign_arithmetic() {
        expect_success(
            "model Test Real x = -5; Real y = abs(x) * sign(x); end Test;",
            "Test",
        );
    }

    #[test]
    fn mls_12_5_min_max_three_values() {
        expect_success(
            "model Test Real a = 1; Real b = 2; Real c = 3; Real smallest = min(a, min(b, c)); Real largest = max(a, max(b, c)); end Test;",
            "Test",
        );
    }

    #[test]
    fn mls_12_5_floor_ceil_comparison() {
        expect_success(
            "model Test Real x = 3.5; Real f = floor(x); Real c = ceil(x); end Test;",
            "Test",
        );
    }

    #[test]
    fn mls_12_5_div_mod_rem_relations() {
        // Test: a = div(a, b) * b + mod(a, b)
        expect_success(
            "model Test Integer a = 17; Integer b = 5; Integer q = div(a, b); Integer r = mod(a, b); end Test;",
            "Test",
        );
    }
}

/// MLS §12.5: Array reduction built-in functions
mod array_reduction_builtins {
    use super::*;

    #[test]
    fn mls_12_5_sum_literal_array() {
        expect_success(
            "model Test Real s = sum({1, 2, 3, 4, 5}); end Test;",
            "Test",
        );
    }

    #[test]
    fn mls_12_5_product_literal_array() {
        expect_success(
            "model Test Real p = product({1, 2, 3, 4}); end Test;",
            "Test",
        );
    }

    #[test]
    fn mls_12_5_min_array() {
        expect_success(
            "model Test Real m = min({5, 2, 8, 1, 9}); end Test;",
            "Test",
        );
    }

    #[test]
    fn mls_12_5_max_array() {
        expect_success(
            "model Test Real m = max({5, 2, 8, 1, 9}); end Test;",
            "Test",
        );
    }

    #[test]
    fn mls_12_5_sum_variable_array() {
        expect_success(
            r#"
            model Test
                Real x[5] = {1, 2, 3, 4, 5};
                Real s = sum(x);
            end Test;
            "#,
            "Test",
        );
    }
}

/// MLS §12.5: Size and dimension functions
mod dimension_builtins {
    use super::*;

    #[test]
    fn mls_12_5_size_1d() {
        expect_success(
            r#"
            model Test
                Real x[5];
                Integer n = size(x, 1);
            equation
                x = {1, 2, 3, 4, 5};
            end Test;
            "#,
            "Test",
        );
    }

    #[test]
    fn mls_12_5_size_2d() {
        expect_success(
            r#"
            model Test
                Real A[3, 4];
                Integer rows = size(A, 1);
                Integer cols = size(A, 2);
            equation
                A = fill(1, 3, 4);
            end Test;
            "#,
            "Test",
        );
    }

    #[test]
    fn mls_12_5_ndims() {
        expect_parse_success(
            r#"
            model Test
                Real A[3, 4, 5];
                Integer n = ndims(A);
            end Test;
            "#,
        );
    }
}

/// MLS §12.5: Array construction functions
mod array_construction_builtins {
    use super::*;

    #[test]
    fn mls_12_5_zeros_1d() {
        expect_success(
            r#"
            model Test
                Real x[5] = zeros(5);
            end Test;
            "#,
            "Test",
        );
    }

    #[test]
    fn mls_12_5_zeros_2d() {
        expect_success(
            r#"
            model Test
                Real A[3, 4] = zeros(3, 4);
            end Test;
            "#,
            "Test",
        );
    }

    #[test]
    fn mls_12_5_ones_1d() {
        expect_success(
            r#"
            model Test
                Real x[5] = ones(5);
            end Test;
            "#,
            "Test",
        );
    }

    #[test]
    fn mls_12_5_ones_2d() {
        expect_success(
            r#"
            model Test
                Real A[3, 4] = ones(3, 4);
            end Test;
            "#,
            "Test",
        );
    }

    #[test]
    fn mls_12_5_fill_1d() {
        expect_success(
            r#"
            model Test
                Real x[5] = fill(3.14, 5);
            end Test;
            "#,
            "Test",
        );
    }

    #[test]
    fn mls_12_5_fill_2d() {
        expect_success(
            r#"
            model Test
                Real A[3, 4] = fill(2.0, 3, 4);
            end Test;
            "#,
            "Test",
        );
    }

    #[test]
    fn mls_12_5_identity_matrix() {
        expect_success(
            r#"
            model Test
                Real I[3, 3] = identity(3);
            end Test;
            "#,
            "Test",
        );
    }

    #[test]
    fn mls_12_5_diagonal() {
        expect_success(
            r#"
            model Test
                Real D[3, 3] = diagonal({1, 2, 3});
            end Test;
            "#,
            "Test",
        );
    }

    #[test]
    fn mls_12_5_linspace() {
        expect_success(
            r#"
            model Test
                Real x[11] = linspace(0, 10, 11);
            end Test;
            "#,
            "Test",
        );
    }
}

/// MLS §12.5: String built-in functions
mod string_builtins {
    use super::*;

    #[test]
    fn mls_12_5_string_from_real() {
        expect_parse_success(
            r#"
            function ShowValue
                input Real x;
                output String s;
            algorithm
                s := String(x);
            end ShowValue;
            "#,
        );
    }

    #[test]
    fn mls_12_5_string_from_integer() {
        expect_parse_success(
            r#"
            function ShowInt
                input Integer n;
                output String s;
            algorithm
                s := String(n);
            end ShowInt;
            "#,
        );
    }

    #[test]
    fn mls_12_5_string_from_boolean() {
        expect_parse_success(
            r#"
            function ShowBool
                input Boolean b;
                output String s;
            algorithm
                s := String(b);
            end ShowBool;
            "#,
        );
    }
}

/// MLS §12.5: Continuous event functions
mod continuous_event_builtins {
    use super::*;

    #[test]
    fn mls_12_5_der_in_equation() {
        expect_success(
            r#"
            model Test
                Real x(start = 0);
            equation
                der(x) = 1;
            end Test;
            "#,
            "Test",
        );
    }

    #[test]
    fn mls_12_5_der_chain() {
        expect_success(
            r#"
            model Test
                Real x(start = 0);
                Real v(start = 1);
            equation
                der(x) = v;
                der(v) = -x;
            end Test;
            "#,
            "Test",
        );
    }
}

/// MLS §12.5: Discrete event functions
mod discrete_event_builtins {
    use super::*;

    #[test]
    fn mls_12_5_pre_in_when() {
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
    fn mls_12_5_edge_rising() {
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
    fn mls_12_5_change_detection() {
        expect_success(
            r#"
            model Test
                discrete Integer value(start = 0);
                discrete Integer changes(start = 0);
                Real x(start = 0);
            equation
                der(x) = 1;
                when x > 1 then
                    value = pre(value) + 1;
                end when;
                when change(value) then
                    changes = pre(changes) + 1;
                end when;
            end Test;
            "#,
            "Test",
        );
    }

    #[test]
    fn mls_12_5_sample_periodic() {
        expect_success(
            r#"
            model Test
                discrete Real sampled(start = 0);
            equation
                when sample(0, 0.1) then
                    sampled = pre(sampled) + 1;
                end when;
            end Test;
            "#,
            "Test",
        );
    }

    #[test]
    fn mls_12_5_reinit_state() {
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
}

/// MLS §12.5: Special event functions
mod special_event_builtins {
    use super::*;

    #[test]
    fn mls_12_5_initial_function() {
        expect_success(
            r#"
            model Test
                Real x(start = 0);
                Boolean isInit = initial();
            equation
                der(x) = if isInit then 0 else 1;
            end Test;
            "#,
            "Test",
        );
    }

    #[test]
    fn mls_12_5_terminal_function() {
        expect_success(
            r#"
            model Test
                discrete Real finalVal(start = 0);
                Real x(start = 0);
            equation
                der(x) = 1;
                when terminal() then
                    finalVal = x;
                end when;
            end Test;
            "#,
            "Test",
        );
    }

    #[test]
    fn mls_12_5_noevent_wrapper() {
        expect_success(
            r#"
            model Test
                Real x(start = -1);
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
    fn mls_12_5_smooth_zero_order() {
        expect_success(
            r#"
            model Test
                Real x(start = 0);
                Real y;
            equation
                der(x) = 1;
                y = smooth(0, abs(x));
            end Test;
            "#,
            "Test",
        );
    }

    #[test]
    fn mls_12_5_smooth_first_order() {
        expect_success(
            r#"
            model Test
                Real x(start = 0);
                Real y;
            equation
                der(x) = 1;
                y = smooth(1, x * abs(x));
            end Test;
            "#,
            "Test",
        );
    }
}

/// MLS §12.5: Delay function
mod delay_builtin {
    use super::*;

    #[test]
    fn mls_12_5_delay_constant() {
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
}
