//! UNIT (Unit expressions) contract tests - MLS Chapter 19
//!
//! Unit strings are validated structurally against the §19.2 grammar
//! (ET010); the symbol vocabulary itself stays open per the MLS.

use rumoca_compile::compile::FailedPhase;
use rumoca_contracts::test_support::{expect_failure_in_phase_with_code, expect_success};

fn model_with_unit(unit: &str) -> String {
    format!(
        r#"
        model Test
            Real x(unit = "{unit}", start = 0, fixed = true);
        equation
            der(x) = 1;
        end Test;
    "#
    )
}

// =============================================================================
// UNIT-001: Syntax match
// "Unit string shall match the unit-expression rule"
// =============================================================================

#[test]
fn unit_001_grammar_conformant_unit_accepted() {
    expect_success(&model_with_unit("J/(kg.K)"), "Test");
}

#[test]
fn unit_001_caret_exponent_rejected() {
    expect_failure_in_phase_with_code(
        &model_with_unit("m^2"),
        "Test",
        FailedPhase::Typecheck,
        "ET010",
    );
}

// =============================================================================
// UNIT-002: No whitespace
// "Modelica unit string syntax allows neither comments nor white-space"
// =============================================================================

#[test]
fn unit_002_whitespace_rejected() {
    expect_failure_in_phase_with_code(
        &model_with_unit("N m"),
        "Test",
        FailedPhase::Typecheck,
        "ET010",
    );
}

// =============================================================================
// UNIT-003: No multiple divisors
// "SI standard does not allow multiple units right of division-symbol"
// =============================================================================

#[test]
fn unit_003_parenthesized_denominator_accepted() {
    expect_success(&model_with_unit("W/(m2.K)"), "Test");
}

#[test]
fn unit_003_double_division_rejected() {
    expect_failure_in_phase_with_code(
        &model_with_unit("J/kg/K"),
        "Test",
        FailedPhase::Typecheck,
        "ET010",
    );
}

// =============================================================================
// UNIT-004: No exponent spacing
// "There must be no spacing between unit-operand and possible unit-exponent"
// =============================================================================

#[test]
fn unit_004_spaced_exponent_rejected() {
    expect_failure_in_phase_with_code(
        &model_with_unit("m/s 2"),
        "Test",
        FailedPhase::Typecheck,
        "ET010",
    );
}

// =============================================================================
// UNIT-005: No prefix spacing
// "There must be no spacing between unit-symbol and possible unit-prefix"
// =============================================================================

#[test]
fn unit_005_spaced_prefix_rejected() {
    expect_failure_in_phase_with_code(
        &model_with_unit("k W"),
        "Test",
        FailedPhase::Typecheck,
        "ET010",
    );
}

// =============================================================================
// UNIT-006: Symbol first interpretation
// "Unit-operands should first be interpreted as unit-symbol"
// =============================================================================

#[test]
fn unit_006_min_is_a_symbol_not_milli_in() {
    expect_success(&model_with_unit("1/min"), "Test");
}

// =============================================================================
// UNIT-007: SI unit recognition
// "Tools shall recognize basic and derived units of the SI system"
// =============================================================================

#[test]
fn unit_007_si_units_accepted() {
    for unit in [
        "m", "kg", "s", "A", "K", "mol", "cd", "N", "Pa", "J", "W", "V", "Hz", "rad",
    ] {
        expect_success(&model_with_unit(unit), "Test");
    }
}

// =============================================================================
// UNIT-008: Non-SI unit recognition
// "Tools shall recognize: minute, hour, day, liter, electronvolt, degree"
// =============================================================================

#[test]
fn unit_008_non_si_units_accepted() {
    for unit in ["min", "h", "d", "l", "L", "eV", "deg"] {
        expect_success(&model_with_unit(unit), "Test");
    }
}

// =============================================================================
// UNIT-009: Dot notation required
// (multiplication between unit factors is written with ".")
// =============================================================================

#[test]
fn unit_009_dot_multiplication_accepted() {
    expect_success(&model_with_unit("N.m"), "Test");
}

#[test]
fn unit_009_star_multiplication_rejected() {
    expect_failure_in_phase_with_code(
        &model_with_unit("N*m"),
        "Test",
        FailedPhase::Typecheck,
        "ET010",
    );
}
