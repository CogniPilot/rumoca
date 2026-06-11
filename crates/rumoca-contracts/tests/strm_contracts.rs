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

// =============================================================================
// STRM-008: Implementation must be continuous and differentiable given
// continuous inputs
// STRM-009: Division by zero can no longer occur; result is always well-defined
// =============================================================================

#[test]
fn strm_009_instream_finite_through_zero_flow() {
    // m_flow crosses zero at t = 1; inStream must stay finite and bounded by
    // the two mixing enthalpies throughout (regularized stream semantics).
    let trace = rumoca_contracts::test_support::simulate_model(
        r#"
        model M
            connector C
                Real p;
                flow Real m_flow;
                stream Real h;
            end C;
            model Vol
                C port;
                parameter Real h_out = 2;
                Real h_in;
            equation
                port.h = h_out;
                h_in = inStream(port.h);
            end Vol;
            Vol v1(h_out = 2);
            Vol v2(h_out = 4);
            Real q;
            Real t(start = 0, fixed = true);
        equation
            der(t) = 1;
            connect(v1.port, v2.port);
            v1.port.p = 1;
            v1.port.m_flow = time - 1;
            q = v1.h_in;
        end M;
    "#,
        "M",
        2.0,
    );
    let q = trace.channel("q");
    assert!(
        q.iter().all(|v| v.is_finite()),
        "inStream must be well-defined at zero flow: {q:?}"
    );
    assert!(
        q.iter().all(|&v| (2.0..=4.0).contains(&v)),
        "inStream must stay between the mixing enthalpies: {q:?}"
    );
}

#[test]
fn strm_008_instream_continuous_across_flow_reversal() {
    let trace = rumoca_contracts::test_support::simulate_model(
        r#"
        model M
            connector C
                Real p;
                flow Real m_flow;
                stream Real h;
            end C;
            model Vol
                C port;
                parameter Real h_out = 2;
                Real h_in;
            equation
                port.h = h_out;
                h_in = inStream(port.h);
            end Vol;
            Vol v1(h_out = 2);
            Vol v2(h_out = 4);
            Real q;
            Real t(start = 0, fixed = true);
        equation
            der(t) = 1;
            connect(v1.port, v2.port);
            v1.port.p = 1;
            v1.port.m_flow = time - 1;
            q = v1.h_in;
        end M;
    "#,
        "M",
        2.0,
    );
    let q = trace.channel("q");
    let max_jump = q
        .windows(2)
        .map(|w| (w[1] - w[0]).abs())
        .fold(0.0_f64, f64::max);
    assert!(
        max_jump < 1.0,
        "inStream must vary continuously across flow reversal, max jump {max_jump}"
    );
}

// =============================================================================
// STRM-005: For inside connectors, variables with stream prefix do not lead
// to connection equations
// =============================================================================

#[test]
fn strm_005_inside_connector_instream_is_peer_value() {
    // With exactly two inside connectors in the set, inStream(v1.port.h) is
    // the peer's outflow enthalpy with no extra connection equations.
    let trace = rumoca_contracts::test_support::simulate_model(
        r#"
        model M
            connector C
                Real p;
                flow Real m_flow;
                stream Real h;
            end C;
            model Vol
                C port;
                parameter Real h_out = 2;
                Real h_in;
            equation
                port.h = h_out;
                h_in = inStream(port.h);
            end Vol;
            Vol v1(h_out = 2);
            Vol v2(h_out = 4);
            Real q;
            Real t(start = 0, fixed = true);
        equation
            der(t) = 1;
            connect(v1.port, v2.port);
            v1.port.p = 1;
            v1.port.m_flow = time - 1;
            q = v1.h_in;
        end M;
    "#,
        "M",
        2.0,
    );
    let q = trace.channel("q");
    assert!(
        q.iter().all(|&v| v == 4.0),
        "inStream at an inside connector must be the peer's stream value: {q:?}"
    );
}

// =============================================================================
// STRM-004: For every outside connector, one equation is generated for every
// variable with stream prefix
// =============================================================================

#[test]
fn strm_004_outside_singleton_instream_is_own_value() {
    // A standalone top-level connector forms a singleton stream set; the
    // outside-connector rule makes inStream(port.h) = port.h, and the
    // unconnected-flow equation is generated for m_flow.
    let trace = rumoca_contracts::test_support::simulate_model(
        r#"
        model M
            connector C
                Real p;
                flow Real m_flow;
                stream Real h;
            end C;
            C port;
            Real q;
            Real t(start = 0, fixed = true);
        equation
            der(t) = 1;
            port.p = 1;
            port.h = 5;
            q = inStream(port.h);
        end M;
    "#,
        "M",
        1.0,
    );
    let q = trace.channel("q");
    assert!(
        q.iter().all(|&v| v == 5.0),
        "outside singleton inStream must equal its own stream value: {q:?}"
    );
}

// =============================================================================
// STRM-007: If argument of inStream is array, implicit equation system holds
// elementwise
// =============================================================================

#[test]
fn strm_007_array_instream_elementwise_peer_values() {
    let trace = rumoca_contracts::test_support::simulate_model(
        r#"
        model M
            connector C
                Real p;
                flow Real m_flow;
                stream Real h[2];
            end C;
            model Vol
                C port;
                parameter Real h1 = 2;
                parameter Real h2 = 3;
                Real h_in[2];
            equation
                port.h = {h1, h2};
                h_in = inStream(port.h);
            end Vol;
            Vol v1(h1 = 2, h2 = 3);
            Vol v2(h1 = 4, h2 = 5);
            Real t(start = 0, fixed = true);
        equation
            der(t) = 1;
            connect(v1.port, v2.port);
            v1.port.p = 1;
            v1.port.m_flow = time - 1;
        end M;
    "#,
        "M",
        2.0,
    );
    assert_eq!(
        trace.final_value("v1.h_in[1]"),
        4.0,
        "array inStream must take the peer's elementwise values"
    );
    assert_eq!(trace.final_value("v1.h_in[2]"), 5.0);
}
