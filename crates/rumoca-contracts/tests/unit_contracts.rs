//! UNIT (Unit strings) contract tests - MLS §19.1
//!
//! Tests for the 9 unit-string contracts defined in SPEC_0022.

use rumoca_contracts::test_support::{expect_balanced, expect_failure_in_phase_with_code};
use rumoca_session::FailedPhase;

// =============================================================================
// UNIT-001: Syntax match
// "Unit string shall match the unit-expression rule"
// =============================================================================

#[test]
fn unit_001_unit_string_syntax_match() {
    expect_balanced(
        r#"
        model Test
            Real v(unit="kg.m/s2");
        equation
            v = 1;
        end Test;
    "#,
        "Test",
    );
}

// =============================================================================
// UNIT-002: No whitespace
// "Modelica unit string syntax allows neither comments nor white-space"
// =============================================================================

#[test]
#[ignore = "TODO(UNIT-002): reject whitespace/comments inside unit strings"]
fn unit_002_unit_string_no_whitespace() {
    expect_failure_in_phase_with_code(
        r#"
        model Test
            Real v(unit="m / s");
        equation
            v = 1;
        end Test;
    "#,
        "Test",
        FailedPhase::Typecheck,
        "ET001",
    );
}

// =============================================================================
// UNIT-003: No multiple divisors
// "SI standard does not allow multiple units right of division-symbol (ambiguous)"
// =============================================================================

#[test]
#[ignore = "TODO(UNIT-003): reject unit strings with multiple divisors"]
fn unit_003_unit_string_no_multiple_divisors() {
    expect_failure_in_phase_with_code(
        r#"
        model Test
            Real v(unit="m/s/s");
        equation
            v = 1;
        end Test;
    "#,
        "Test",
        FailedPhase::Typecheck,
        "ET001",
    );
}

// =============================================================================
// UNIT-004: No exponent spacing
// "There must be no spacing between unit-operand and possible unit-exponent"
// =============================================================================

#[test]
#[ignore = "TODO(UNIT-004): reject spacing between unit operand and exponent"]
fn unit_004_unit_exponent_spacing_forbidden() {
    expect_failure_in_phase_with_code(
        r#"
        model Test
            Real area(unit="m ^2");
        equation
            area = 1;
        end Test;
    "#,
        "Test",
        FailedPhase::Typecheck,
        "ET001",
    );
}

// =============================================================================
// UNIT-005: No prefix spacing
// "There must be no spacing between unit-symbol and possible unit-prefix"
// =============================================================================

#[test]
#[ignore = "TODO(UNIT-005): reject spacing between unit prefix and symbol"]
fn unit_005_unit_prefix_spacing_forbidden() {
    expect_failure_in_phase_with_code(
        r#"
        model Test
            Real distance(unit="k m");
        equation
            distance = 1;
        end Test;
    "#,
        "Test",
        FailedPhase::Typecheck,
        "ET001",
    );
}

// =============================================================================
// UNIT-006: Symbol first interpretation
// "Unit-operands should first be interpreted as unit-symbol, only then as prefixed operand"
// =============================================================================

#[test]
fn unit_006_unit_symbol_interpretation_prefers_symbol() {
    expect_balanced(
        r#"
        model Test
            Real p(unit="Pa");
        equation
            p = 1;
        end Test;
    "#,
        "Test",
    );
}

// =============================================================================
// UNIT-007: SI unit recognition
// "Tools shall recognize basic and derived units of the SI system"
// =============================================================================

#[test]
fn unit_007_si_units_recognized() {
    expect_balanced(
        r#"
        model Test
            Real force(unit="N");
            Real torque(unit="N.m");
            Real density(unit="kg/m3");
        equation
            force = 1;
            torque = 1;
            density = 1;
        end Test;
    "#,
        "Test",
    );
}

// =============================================================================
// UNIT-008: Non-SI unit recognition
// "Tools shall recognize: minute, hour, day, liter, electronvolt, degree, debye"
// =============================================================================

#[test]
fn unit_008_non_si_units_recognized() {
    expect_balanced(
        r#"
        model Test
            Real t_min(unit="min");
            Real t_hour(unit="h");
            Real t_day(unit="d");
            Real volume(unit="l");
            Real energy(unit="eV");
            Real angle(unit="deg");
            Real dipole(unit="D");
        equation
            t_min = 1;
            t_hour = 1;
            t_day = 1;
            volume = 1;
            energy = 1;
            angle = 1;
            dipole = 1;
        end Test;
    "#,
        "Test",
    );
}

// =============================================================================
// UNIT-009: Dot notation required
// "Multiplication uses dot notation: 'N.m' for newton-meter, not 'Nm'"
// =============================================================================

#[test]
#[ignore = "TODO(UNIT-009): reject implicit multiplication notation in unit strings (e.g. Nm)"]
fn unit_009_dot_notation_required() {
    expect_failure_in_phase_with_code(
        r#"
        model Test
            Real torque(unit="Nm");
        equation
            torque = 1;
        end Test;
    "#,
        "Test",
        FailedPhase::Typecheck,
        "ET001",
    );
}
