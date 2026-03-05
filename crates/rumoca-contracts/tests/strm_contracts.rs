//! STRM (Stream) contract tests - MLS §15
//!
//! Tests for the 11 stream contracts defined in SPEC_0022.

use rumoca_contracts::test_support::expect_failure_in_phase_with_code;
use rumoca_session::FailedPhase;

// =============================================================================
// STRM-001: Stream prefix scope
// "The stream prefix can only be used in a connector declaration"
// =============================================================================

#[test]
#[ignore = "TODO(STRM-001): enforce stream-prefix scope to connector declarations only"]
fn strm_001_stream_prefix_scope() {
    expect_failure_in_phase_with_code(
        r#"
        model Test
            stream Real s;
        equation
            s = 1;
        end Test;
    "#,
        "Test",
        FailedPhase::Instantiate,
        "EI000",
    );
}

// =============================================================================
// STRM-002: Exactly one flow
// "A stream connector must have exactly one variable with the flow prefix"
// =============================================================================

#[test]
#[ignore = "TODO(STRM-002): enforce exactly-one-flow rule in stream connectors"]
fn strm_002_exactly_one_flow() {
    expect_failure_in_phase_with_code(
        r#"
        connector C
            flow Real f1;
            flow Real f2;
            stream Real h;
        end C;
    "#,
        "C",
        FailedPhase::Instantiate,
        "EI000",
    );
}

// =============================================================================
// STRM-003: Flow scalar Real
// "Flow variable shall be a scalar that is a subtype of Real"
// =============================================================================

#[test]
#[ignore = "TODO(STRM-003): enforce scalar-Real constraint for stream-connector flow variable"]
fn strm_003_flow_scalar_real() {
    expect_failure_in_phase_with_code(
        r#"
        connector C
            flow Integer f;
            stream Real h;
        end C;
    "#,
        "C",
        FailedPhase::Instantiate,
        "EI000",
    );
}

// =============================================================================
// STRM-004: Outside connector equations
// "Outside connectors generate one equation per stream variable"
// =============================================================================

#[test]
#[ignore = "TODO(STRM-004): generate outside-connector stream equations per MLS §15.1"]
fn strm_004_outside_connector_equations() {
    expect_failure_in_phase_with_code(
        r#"
        connector C
            flow Real f;
            stream Real h;
        end C;

        model Test
            C c1;
            C c2;
        equation
            connect(c1, c2);
        end Test;
    "#,
        "Test",
        FailedPhase::ToDae,
        "ED000",
    );
}

// =============================================================================
// STRM-005: Inside connector no equations
// "Inside connectors with stream vars do not generate connection equations"
// =============================================================================

#[test]
#[ignore = "TODO(STRM-005): suppress stream equations for inside connectors per MLS §15.1"]
fn strm_005_inside_connector_no_equations() {
    expect_failure_in_phase_with_code(
        r#"
        connector C
            flow Real f;
            stream Real h;
        end C;

        model M
            C c;
        end M;

        model Test
            M m;
        equation
            // stream balance behavior for inner/outer connector placement not enforced yet
            m.c.h = 1;
        end Test;
    "#,
        "Test",
        FailedPhase::ToDae,
        "ED000",
    );
}

// =============================================================================
// STRM-006: inStream stream only
// "inStream(v) is only allowed on stream variables v"
// =============================================================================

#[test]
#[ignore = "TODO(STRM-006): enforce inStream argument to be a stream variable"]
fn strm_006_instream_stream_only() {
    expect_failure_in_phase_with_code(
        r#"
        connector C
            flow Real f;
            Real p;
        end C;

        model Test
            C c;
            Real y;
        equation
            y = inStream(c.p);
        end Test;
    "#,
        "Test",
        FailedPhase::Instantiate,
        "EI000",
    );
}

// =============================================================================
// STRM-007: inStream vectorizable
// "Array inStream arguments are handled elementwise"
// =============================================================================

#[test]
#[ignore = "TODO(STRM-007): implement elementwise inStream semantics for array arguments"]
fn strm_007_instream_vectorizable() {
    expect_failure_in_phase_with_code(
        r#"
        connector C
            flow Real f;
            stream Real h[2];
        end C;

        model Test
            C c;
            Real y[2];
        equation
            y = inStream(c.h);
        end Test;
    "#,
        "Test",
        FailedPhase::ToDae,
        "ED000",
    );
}

// =============================================================================
// STRM-008: inStream continuity
// "Implementation must be continuous/differentiable for continuous inputs"
// =============================================================================

#[test]
#[ignore = "TODO(STRM-008): enforce continuity/differentiability guarantees for inStream implementation"]
fn strm_008_instream_continuity() {
    expect_failure_in_phase_with_code(
        r#"
        connector C
            flow Real f;
            stream Real h;
        end C;

        model Test
            C c;
            Real y;
        equation
            y = inStream(c.h);
        end Test;
    "#,
        "Test",
        FailedPhase::ToDae,
        "ED000",
    );
}

// =============================================================================
// STRM-009: No division by zero
// "inStream result is always well-defined (no division-by-zero)"
// =============================================================================

#[test]
#[ignore = "TODO(STRM-009): enforce no-division-by-zero behavior in inStream equation system"]
fn strm_009_no_division_by_zero() {
    expect_failure_in_phase_with_code(
        r#"
        connector C
            flow Real f;
            stream Real h;
        end C;

        model Test
            C c;
            Real y;
        equation
            y = inStream(c.h);
        end Test;
    "#,
        "Test",
        FailedPhase::ToDae,
        "ED000",
    );
}

// =============================================================================
// STRM-010: actualStream argument
// "Only argument of actualStream needs to be a stream variable reference"
// =============================================================================

#[test]
#[ignore = "TODO(STRM-010): enforce actualStream argument typing per MLS §15.3"]
fn strm_010_actualstream_argument() {
    expect_failure_in_phase_with_code(
        r#"
        connector C
            flow Real f;
            Real p;
        end C;

        model Test
            C c;
            Real y;
        equation
            y = actualStream(c.p);
        end Test;
    "#,
        "Test",
        FailedPhase::Instantiate,
        "EI000",
    );
}

// =============================================================================
// STRM-011: Flow/stream same level
// "Flow variable must exist at same hierarchy level as stream variable"
// =============================================================================

#[test]
#[ignore = "TODO(STRM-011): enforce same-level flow/stream pairing in connector hierarchy"]
fn strm_011_flow_stream_same_level() {
    expect_failure_in_phase_with_code(
        r#"
        connector Inner
            flow Real f;
        end Inner;

        connector Outer
            Inner i;
            stream Real h;
        end Outer;
    "#,
        "Outer",
        FailedPhase::Instantiate,
        "EI000",
    );
}
