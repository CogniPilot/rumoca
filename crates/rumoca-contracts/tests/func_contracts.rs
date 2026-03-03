//! FUNC (Function) contract tests - MLS §12
//!
//! Tests for the 35 function contracts defined in SPEC_0022.

use rumoca_contracts::test_support::{
    expect_parse_err_with_code, expect_parse_ok, expect_resolve_failure_with_code, expect_success,
};

// =============================================================================
// FUNC-001: Input/output only
// "Each public component must have prefix input or output"
// =============================================================================

#[test]
fn func_001_input_output_ok() {
    expect_success(
        r#"
        function F
            input Real x;
            output Real y;
        algorithm
            y := x * 2;
        end F;
        model Test
            Real z;
        equation
            z = F(2.0);
        end Test;
    "#,
        "Test",
    );
}

#[test]
fn func_001_public_no_prefix_fails() {
    expect_resolve_failure_with_code(
        r#"
        function F
            Real x;
            output Real y;
        algorithm
            y := x;
        end F;
    "#,
        "F",
        "ER013",
    );
}

// =============================================================================
// FUNC-002: Input read-only
// "Input formal parameters are read-only after being bound"
// =============================================================================

#[test]
fn func_002_input_readonly() {
    expect_resolve_failure_with_code(
        r#"
        function F
            input Real x;
            output Real y;
        algorithm
            x := 5;
            y := x;
        end F;
    "#,
        "F",
        "ER014",
    );
}

// =============================================================================
// FUNC-006: No equations
// "Function shall not have equations, shall not have initial algorithms"
// =============================================================================

#[test]
fn func_006_no_equations_in_function() {
    expect_parse_err_with_code(
        r#"
        function F
            input Real x;
            output Real y;
        equation
            y = x;
        end F;
    "#,
        "EP001",
    );
}

// =============================================================================
// FUNC-007: No when-statements
// "Function body shall not contain when-statements"
// =============================================================================

#[test]
fn func_007_no_when_in_function() {
    expect_resolve_failure_with_code(
        r#"
        function F
            input Real x;
            output Real y;
        algorithm
            when x > 0 then
                y := 1;
            end when;
        end F;
    "#,
        "F",
        "ER015",
    );
}

// =============================================================================
// FUNC-014: Single algorithm/external
// "Function can have at most one algorithm section or one external function interface"
// =============================================================================

#[test]
fn func_014_single_algorithm() {
    expect_success(
        r#"
        function F
            input Real x;
            output Real y;
        algorithm
            y := x * 2;
        end F;
        model Test
            Real z;
        equation
            z = F(2.0);
        end Test;
    "#,
        "Test",
    );
}

// =============================================================================
// FUNC-017: Return in algorithm only
// "Return statement can only be used in an algorithm section of a function"
// =============================================================================

#[test]
fn func_017_return_in_function() {
    expect_success(
        r#"
        function F
            input Real x;
            output Real y;
        algorithm
            if x > 0 then
                y := x;
                return;
            end if;
            y := -x;
        end F;
        model Test
            Real z;
        equation
            z = F(2.0);
        end Test;
    "#,
        "Test",
    );
}

#[test]
fn func_017_return_outside_function_algorithm_fails() {
    expect_parse_err_with_code(
        r#"
        model Test
            Real x;
        equation
            return;
            x = 1.0;
        end Test;
    "#,
        "EP001",
    );
}

// =============================================================================
// Function integration tests
// =============================================================================

#[test]
fn func_basic_call() {
    expect_success(
        r#"
        function Square
            input Real x;
            output Real y;
        algorithm
            y := x * x;
        end Square;
        model Test
            Real x;
            Real y;
        equation
            x = 3.0;
            y = Square(x);
        end Test;
    "#,
        "Test",
    );
}

#[test]
fn func_with_protected() {
    expect_parse_ok(
        r#"
        function F
            input Real x;
            output Real y;
        protected
            Real temp;
        algorithm
            temp := x * x;
            y := temp + 1;
        end F;
    "#,
    );
}

#[test]
fn func_multiple_outputs() {
    expect_parse_ok(
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
fn func_der_class_specifier_short_form_parses() {
    expect_parse_ok(
        r#"
        function f
            input Real x;
            output Real y;
        algorithm
            y := x;
        end f;

        function f_der = der(f, x);
    "#,
    );
}

#[test]
fn func_default_input() {
    expect_parse_ok(
        r#"
        function F
            input Real x;
            input Real scale = 1.0;
            output Real y;
        algorithm
            y := x * scale;
        end F;
    "#,
    );
}
