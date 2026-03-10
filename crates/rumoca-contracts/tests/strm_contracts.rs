//! STRM (Stream) contract tests - MLS §15
//!
//! Tests for stream connector structure and builtin stream operators.

use rumoca_contracts::test_support::{expect_resolve_failure_with_code, expect_success};

// =============================================================================
// STRM-001: Stream prefix scope
// "The stream prefix can only be used in a connector declaration"
// =============================================================================

#[test]
fn strm_001_stream_prefix_inside_connector_ok() {
    expect_success(
        r#"
        connector Port
            flow Real m_flow;
            Real p;
            stream Real h_outflow;
        end Port;

        model Test
            Port a;
            Port b;
        equation
            connect(a, b);
        end Test;
    "#,
        "Test",
    );
}

#[test]
fn strm_001_stream_prefix_outside_connector_rejected() {
    expect_resolve_failure_with_code(
        r#"
        model Test
            stream Real h_outflow;
        end Test;
    "#,
        "Test",
        "ER064",
    );
}

// =============================================================================
// STRM-002: Exactly one flow
// "A stream connector must have exactly one variable with the flow prefix"
// =============================================================================

#[test]
fn strm_002_stream_connector_with_single_flow_ok() {
    expect_success(
        r#"
        connector BasePort
            flow Real m_flow;
            Real p;
        end BasePort;

        connector Port
            extends BasePort;
            stream Real h_outflow;
        end Port;

        model Test
            Port a;
            Port b;
        equation
            connect(a, b);
        end Test;
    "#,
        "Test",
    );
}

#[test]
fn strm_002_stream_connector_without_flow_rejected() {
    expect_resolve_failure_with_code(
        r#"
        connector Port
            stream Real h_outflow;
        end Port;

        model Test
            Port a;
            Port b;
        equation
            connect(a, b);
        end Test;
    "#,
        "Test",
        "ER065",
    );
}

#[test]
fn strm_002_stream_connector_with_multiple_flows_rejected() {
    expect_resolve_failure_with_code(
        r#"
        connector Port
            flow Real m_flow;
            flow Real q_flow;
            stream Real h_outflow;
        end Port;

        model Test
            Port a;
            Port b;
        equation
            connect(a, b);
        end Test;
    "#,
        "Test",
        "ER065",
    );
}

// =============================================================================
// STRM-003: Flow scalar Real
// "Flow variable shall be a scalar that is a subtype of Real"
// =============================================================================

#[test]
fn strm_003_stream_connector_flow_is_scalar_real_ok() {
    expect_success(
        r#"
        type MassFlowRate = Real;

        connector Port
            flow MassFlowRate m_flow;
            Real p;
            stream Real h_outflow;
        end Port;

        model Test
            Port a;
            Port b;
        equation
            connect(a, b);
        end Test;
    "#,
        "Test",
    );
}

#[test]
fn strm_003_stream_connector_flow_must_be_real_rejected() {
    expect_resolve_failure_with_code(
        r#"
        connector Port
            flow Integer m_flow;
            stream Real h_outflow;
        end Port;

        model Test
            Port a;
            Port b;
        equation
            connect(a, b);
        end Test;
    "#,
        "Test",
        "ER066",
    );
}

#[test]
fn strm_003_stream_connector_flow_must_be_scalar_rejected() {
    expect_resolve_failure_with_code(
        r#"
        connector Port
            flow Real m_flow[2];
            stream Real h_outflow;
        end Port;

        model Test
            Port a;
            Port b;
        equation
            connect(a, b);
        end Test;
    "#,
        "Test",
        "ER066",
    );
}

// =============================================================================
// STRM-006: inStream stream only
// "inStream(v) is only allowed on stream variables v"
// =============================================================================

#[test]
fn strm_006_instream_requires_stream_variable() {
    expect_resolve_failure_with_code(
        r#"
        connector Port
            flow Real m_flow;
            Real p;
        end Port;

        model Test
            Port a;
            Real y;
        equation
            y = inStream(a.p);
        end Test;
    "#,
        "Test",
        "ER067",
    );
}

#[test]
fn strm_006_instream_accepts_stream_variable() {
    expect_success(
        r#"
        connector Port
            flow Real m_flow;
            Real p;
            stream Real h_outflow;
        end Port;

        model Test
            Port a;
            Real y;
        equation
            a.p = 1;
            a.h_outflow = 2;
            y = inStream(a.h_outflow);
        end Test;
    "#,
        "Test",
    );
}

// =============================================================================
// STRM-010: actualStream argument
// "Only argument of actualStream needs to be a reference to a stream variable"
// =============================================================================

#[test]
fn strm_010_actualstream_requires_stream_variable() {
    expect_resolve_failure_with_code(
        r#"
        connector Port
            flow Real m_flow;
            Real p;
        end Port;

        model Test
            Port a;
            Real y;
        equation
            y = actualStream(a.p);
        end Test;
    "#,
        "Test",
        "ER068",
    );
}

#[test]
fn strm_010_actualstream_accepts_stream_variable() {
    expect_success(
        r#"
        connector Port
            flow Real m_flow;
            Real p;
            stream Real h_outflow;
        end Port;

        model Test
            Port a;
            Real y;
        equation
            a.p = 1;
            a.h_outflow = 2;
            y = actualStream(a.h_outflow);
        end Test;
    "#,
        "Test",
    );
}

// =============================================================================
// STRM-011: Flow/stream same level
// "Flow variable must exist at same level as stream variable in connector hierarchy"
// =============================================================================

#[test]
fn strm_011_nested_connector_same_level_flow_ok() {
    expect_success(
        r#"
        connector InnerPort
            flow Real m_flow;
            Real p;
            stream Real h_outflow;
        end InnerPort;

        connector OuterPort
            InnerPort port;
        end OuterPort;

        model Test
            OuterPort a;
            OuterPort b;
        equation
            connect(a, b);
        end Test;
    "#,
        "Test",
    );
}

#[test]
fn strm_011_nested_connector_missing_same_level_flow_rejected() {
    expect_resolve_failure_with_code(
        r#"
        connector InnerPort
            Real p;
            stream Real h_outflow;
        end InnerPort;

        connector OuterPort
            flow Real outer_flow;
            InnerPort port;
        end OuterPort;

        model Test
            OuterPort a;
            OuterPort b;
        equation
            connect(a, b);
        end Test;
    "#,
        "Test",
        "ER065",
    );
}
