//! CONN (Connection) contract tests - MLS §9
//!
//! Tests for the 29 connection contracts defined in SPEC_0022.

use rumoca_contracts::test_support::{
    expect_balanced, expect_failure_in_phase_with_code, expect_resolve_failure_with_code,
    expect_success,
};
use rumoca_session::FailedPhase;

// =============================================================================
// CONN-001: Homogeneity
// "Connection set shall contain either only flow or only non-flow variables"
// =============================================================================

#[test]
fn conn_001_flow_connects_ok() {
    expect_success(
        r#"
        connector Pin
            Real v;
            flow Real i;
        end Pin;
        model Test
            Pin p1;
            Pin p2;
            Real v_offset;
            Real i_offset;
        equation
            connect(p1, p2);
            p1.v + v_offset = 1.0;
            p2.i + i_offset = 0.0;
        end Test;
    "#,
        "Test",
    );
}

// =============================================================================
// CONN-002: Type matching
// "Matched primitive components must have the same primitive types"
// =============================================================================

#[test]
fn conn_002_same_type_connectors() {
    expect_success(
        r#"
        connector Pin
            Real v;
            flow Real i;
        end Pin;
        model Test
            Pin a;
            Pin b;
            Real v_offset;
            Real i_offset;
        equation
            connect(a, b);
            a.v + v_offset = 1.0;
            b.i + i_offset = 0.0;
        end Test;
    "#,
        "Test",
    );
}

#[test]
fn conn_002_type_mismatch_rejected() {
    expect_failure_in_phase_with_code(
        r#"
        connector RealOutput = output Real;
        connector BoolInput = input Boolean;

        model Test
            RealOutput a;
            BoolInput b;
        equation
            connect(a, b);
        end Test;
    "#,
        "Test",
        FailedPhase::Flatten,
        "EF002",
    );
}

// =============================================================================
// CONN-007: Connector not parameter
// "Connector component shall not be declared with parameter or constant"
// =============================================================================

#[test]
fn conn_007_no_parameter_connector() {
    expect_resolve_failure_with_code(
        r#"
        connector Pin
            Real v;
            flow Real i;
        end Pin;
        model Test
            parameter Pin p;
        equation
        end Test;
    "#,
        "Test",
        "ER027",
    );
}

// =============================================================================
// CONN-017: Balance flow = potential
// "For non-partial non-simple non-expandable connector: number of flow = number of potential"
// =============================================================================

#[test]
fn conn_017_balanced_connector() {
    expect_success(
        r#"
        connector Pin
            Real v;
            flow Real i;
        end Pin;
        model Test
            Pin p;
            Real v_bias;
        equation
            p.v + v_bias = 1.0;
            p.i = 0.0;
        end Test;
    "#,
        "Test",
    );
}

#[test]
fn conn_017_unbalanced_connector_fails() {
    expect_resolve_failure_with_code(
        r#"
        connector BadPin
            Real v;
            Real w;
            flow Real i;
        end BadPin;
        model Test
            BadPin p;
        equation
        end Test;
    "#,
        "Test",
        "ER028",
    );
}

// =============================================================================
// CONN-026: Flow sign convention
// "Flow sign is +1 for inside connectors and -1 for outside connectors"
// =============================================================================

#[test]
fn conn_026_flow_sign_basic() {
    // Basic resistor model: flow conservation is handled by connect
    expect_balanced(
        r#"
        connector Pin
            Real v;
            flow Real i;
        end Pin;
        model Resistor
            Pin p;
            Pin n;
            parameter Real R = 1;
        equation
            p.v - n.v = R * p.i;
            p.i + n.i = 0;
        end Resistor;
    "#,
        "Resistor",
    );
}

// =============================================================================
// CONN-029: Connect arguments are connectors
// "Both arguments of connect must be connector references"
// =============================================================================

#[test]
fn conn_029_connect_requires_connectors() {
    expect_resolve_failure_with_code(
        r#"
        model Test
            Real x;
            Real y;
        equation
            connect(x, y);
        end Test;
    "#,
        "Test",
        "ER009",
    );
}

// =============================================================================
// Connection integration tests
// =============================================================================

#[test]
fn conn_series_resistors() {
    expect_balanced(
        r#"
        connector Pin
            Real v;
            flow Real i;
        end Pin;
        model Resistor
            Pin p;
            Pin n;
            parameter Real R = 1;
        equation
            p.v - n.v = R * p.i;
            p.i + n.i = 0;
        end Resistor;
        model Ground
            Pin p;
        equation
            p.v = 0;
        end Ground;
        model Source
            Pin p;
            Pin n;
            parameter Real V = 1;
        equation
            p.v - n.v = V;
            p.i + n.i = 0;
        end Source;
        model Test
            Resistor r1(R = 100);
            Resistor r2(R = 200);
            Source src(V = 10);
            Ground gnd;
        equation
            connect(src.p, r1.p);
            connect(r1.n, r2.p);
            connect(r2.n, src.n);
            connect(src.n, gnd.p);
        end Test;
    "#,
        "Test",
    );
}

#[test]
fn conn_008_same_dimensions() {
    // Connectors with same structure should connect successfully
    expect_success(
        r#"
        connector Pin
            Real v;
            flow Real i;
        end Pin;
        model Test
            Pin a;
            Pin b;
            Real v_offset;
            Real i_offset;
        equation
            connect(a, b);
            a.v + v_offset = 1;
            b.i + i_offset = 0;
        end Test;
    "#,
        "Test",
    );
}

#[test]
fn conn_008_dimension_mismatch_rejected() {
    expect_failure_in_phase_with_code(
        r#"
        connector Vec2
            Real v[2];
        end Vec2;

        connector Vec3
            Real v[3];
        end Vec3;

        model Test
            Vec2 a;
            Vec3 b;
        equation
            connect(a, b);
        end Test;
    "#,
        "Test",
        FailedPhase::Flatten,
        "EF002",
    );
}
