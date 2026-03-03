//! ALG (Algorithm) contract tests - MLS §11
//!
//! Tests for the 17 algorithm contracts defined in SPEC_0022.

use rumoca_contracts::test_support::{
    expect_parse_err_with_code, expect_resolve_failure_with_code, expect_success,
};

// =============================================================================
// ALG-001: No equation syntax
// "Equation equality = shall not be used in an algorithm section"
// =============================================================================

#[test]
fn alg_001_assignment_in_algorithm() {
    // := is correct in algorithm sections
    expect_success(
        r#"
        model Test
            Real x(start = 0);
            Real y;
        algorithm
            y := x + 1;
        equation
            der(x) = -y;
        end Test;
    "#,
        "Test",
    );
}

#[test]
fn alg_001_no_equality_in_algorithm() {
    expect_parse_err_with_code(
        r#"
        model Test
            Real x;
            Real y;
        algorithm
            y = x + 1;
        equation
            x = 1;
        end Test;
    "#,
        "EP001",
    );
}

// =============================================================================
// ALG-005: While scalar Boolean
// "Expression of while-statement shall be a scalar Boolean expression"
// =============================================================================

#[test]
fn alg_005_while_boolean() {
    expect_success(
        r#"
        function F
            input Integer n;
            output Integer result;
        protected
            Integer i;
        algorithm
            result := 0;
            i := 0;
            while i < n loop
                result := result + i;
                i := i + 1;
            end while;
        end F;
        model Test
            Integer y;
        equation
            y = F(5);
        end Test;
    "#,
        "Test",
    );
}

// =============================================================================
// ALG-007: When not in function
// "When-statement shall not be used inside a function class"
// =============================================================================

#[test]
fn alg_007_no_when_in_function() {
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
// ALG-009: When no nesting
// "When-statement cannot be nested inside another when-statement"
// =============================================================================

#[test]
fn alg_009_no_nested_when_statements() {
    expect_resolve_failure_with_code(
        r#"
        model Test
            discrete Real x(start = 0);
            discrete Real y(start = 0);
        algorithm
            when time > 1 then
                when time > 2 then
                    x := 1;
                end when;
                y := 1;
            end when;
        end Test;
    "#,
        "Test",
        "ER016",
    );
}

// =============================================================================
// ALG-012: break scope
// "break can only be used in while or for loop"
// =============================================================================

#[test]
fn alg_012_break_in_for() {
    expect_success(
        r#"
        function F
            input Integer n;
            output Integer result;
        algorithm
            result := 0;
            for i in 1:n loop
                if i > 5 then
                    break;
                end if;
                result := result + i;
            end for;
        end F;
        model Test
            Integer y;
        equation
            y = F(10);
        end Test;
    "#,
        "Test",
    );
}

// =============================================================================
// ALG-013: return scope
// "return can only be used inside functions"
// =============================================================================

#[test]
fn alg_013_return_in_function() {
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
            Real y;
        equation
            y = F(2.0);
        end Test;
    "#,
        "Test",
    );
}

// =============================================================================
// ALG-016: For range fixed
// "For-statement range expressions are evaluated once before entering loop"
// =============================================================================

#[test]
fn alg_016_for_range() {
    expect_success(
        r#"
        model Test
            Real x[5];
        algorithm
            for i in 1:5 loop
                x[i] := i * 1.0;
            end for;
        equation
        end Test;
    "#,
        "Test",
    );
}

// =============================================================================
// Algorithm integration tests
// =============================================================================

#[test]
fn alg_basic_algorithm_section() {
    expect_success(
        r#"
        model Test
            Real x(start = 0);
            Real y;
        algorithm
            if x > 0 then
                y := 1;
            else
                y := -1;
            end if;
        equation
            der(x) = -y;
        end Test;
    "#,
        "Test",
    );
}

#[test]
fn alg_for_loop_algorithm() {
    expect_success(
        r#"
        model Test
            Real x[3];
            Real total;
        algorithm
            total := 0;
            for i in 1:3 loop
                x[i] := i * 2.0;
                total := total + x[i];
            end for;
        equation
        end Test;
    "#,
        "Test",
    );
}
