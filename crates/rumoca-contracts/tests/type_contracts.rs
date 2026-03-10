//! TYPE (Type/Interface) contract tests - MLS §6
//!
//! Tests for the 35 type contracts defined in SPEC_0022.

use rumoca_contracts::test_support::{
    expect_balanced, expect_failure_in_phase_with_code, expect_resolve_failure_with_code,
    expect_success,
};
use rumoca_session::compile::FailedPhase;

// =============================================================================
// TYPE-009: Variability ordering
// "A compatible with B only if declared variability in A <= variability in B"
// =============================================================================

#[test]
fn type_009_variability_compatible() {
    expect_success(
        r#"
        model Test
            constant Real c = 1.0;
            parameter Real p = c;
            Real x;
        equation
            x = p;
        end Test;
    "#,
        "Test",
    );
}

// =============================================================================
// TYPE-011: Dimension count match
// "Number of array dimensions in A and B must be matched"
// =============================================================================

#[test]
fn type_011_dimension_match() {
    expect_success(
        r#"
        model Test
            Real x[3];
        equation
            x = {1, 2, 3};
        end Test;
    "#,
        "Test",
    );
}

// =============================================================================
// TYPE-013: Enumeration match
// "If B is enumeration type, A must also be"
// =============================================================================

#[test]
fn type_013_enumeration_basic() {
    expect_success(
        r#"
        type Color = enumeration(Red, Green, Blue);
        model Test
            Color c;
        equation
            c = Color.Red;
        end Test;
    "#,
        "Test",
    );
}

#[test]
fn type_013_enumeration_mismatch_fails() {
    expect_failure_in_phase_with_code(
        r#"
        type StateA = enumeration(Off, On);
        type StateB = enumeration(Off, On);
        model Test
            StateA a;
            StateB b;
        equation
            a = b;
        end Test;
    "#,
        "Test",
        FailedPhase::Typecheck,
        "ET002",
    );
}

// =============================================================================
// TYPE-014: Built-in type match
// "If B is built-in type, A must be same built-in type"
// =============================================================================

#[test]
fn type_014_builtin_type_match() {
    expect_balanced(
        r#"
        model Test
            Real x;
            Real y;
        equation
            x = 1.0;
            y = x;
        end Test;
    "#,
        "Test",
    );
}

// =============================================================================
// TYPE-033: Real/Integer coercion
// "If A is Real expression, B must be Real or Integer; result is Real"
// =============================================================================

#[test]
fn type_033_integer_to_real_coercion() {
    expect_balanced(
        r#"
        model Test
            Real x;
        equation
            x = 1 + 2.0;
        end Test;
    "#,
        "Test",
    );
}

// =============================================================================
// TYPE-034: Integer division result
// "For Integer exponentiation and division, result type is Real"
// =============================================================================

#[test]
fn type_034_integer_division_real() {
    expect_success(
        r#"
        model Test
            Real x;
        equation
            x = 7 / 2;
        end Test;
    "#,
        "Test",
    );
}

// =============================================================================
// Type integration tests
// =============================================================================

#[test]
fn type_alias_usage() {
    expect_success(
        r#"
        type Voltage = Real(unit = "V");
        model Test
            Voltage v;
        equation
            v = 1.0;
        end Test;
    "#,
        "Test",
    );
}

#[test]
fn type_extends_basic() {
    expect_balanced(
        r#"
        model Base
            Real x;
        equation
            x = 1;
        end Base;
        model Test
            extends Base;
        end Test;
    "#,
        "Test",
    );
}

#[test]
fn type_enumeration_usage() {
    expect_success(
        r#"
        type State = enumeration(Off, On, Error);
        model Test
            State s;
            Real x;
        equation
            s = State.On;
            x = if s == State.On then 1.0 else 0.0;
        end Test;
    "#,
        "Test",
    );
}

#[test]
fn type_005_class_component_mismatch() {
    expect_resolve_failure_with_code(
        r#"
        model A
            Real x;
        equation
            x = 1;
        end A;
        model Test
            Integer y;
        equation
            y = A;
        end Test;
    "#,
        "Test",
        "ER011",
    );
}

#[test]
fn type_028_record_mismatch_fails() {
    expect_failure_in_phase_with_code(
        r#"
        record PayloadA
            Real x;
        end PayloadA;
        record PayloadB
            Real x;
        end PayloadB;
        model Test
            PayloadA a;
            PayloadB b;
        equation
            a = b;
        end Test;
    "#,
        "Test",
        FailedPhase::Typecheck,
        "ET002",
    );
}

// =============================================================================
// TYPE-030: Modifier element exists
// "Modified element should exist in element being modified"
// =============================================================================

#[test]
fn type_030_existing_modifier_target_is_allowed() {
    expect_success(
        r#"
        model Base
            parameter Real kp = 1.0;
        end Base;

        model PID
            extends Base;
        end PID;

        model Test
            PID pid(kp = 10.0);
        end Test;
    "#,
        "Test",
    );
}

#[test]
fn type_030_missing_modifier_target_fails() {
    expect_failure_in_phase_with_code(
        r#"
        model PID
            parameter Real kp = 1.0;
        end PID;

        model Test
            PID pid(kps = 10.0);
        end Test;
    "#,
        "Test",
        FailedPhase::Typecheck,
        "ET001",
    );
}
