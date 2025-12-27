//! MLS §15: Medium Priority Stream Connector Tests
//!
//! This module tests medium priority normative requirements:
//! - §15.1: Stream variable declaration and restrictions
//! - §15.2: inStream and actualStream operators
//! - §15.3: Stream connection equation generation
//!
//! Reference: https://specification.modelica.org/master/stream-connectors.html

use crate::spec::{expect_failure, expect_parse_success, expect_success};

// ============================================================================
// §15.1 STREAM VARIABLE DECLARATION
// ============================================================================

/// MLS §15.1: Stream variable declaration rules
mod stream_declaration {
    use super::*;

    /// MLS: "stream prefix declares stream variable"
    #[test]
    fn mls_15_1_stream_variable_syntax() {
        expect_parse_success(
            r#"
            connector FluidPort
                Real p;
                flow Real m_flow;
                stream Real h_outflow;
            end FluidPort;
            "#,
        );
    }

    /// MLS: "Stream variable must have exactly one associated flow variable"
    #[test]
    fn mls_15_1_stream_requires_flow() {
        expect_failure(
            r#"
            connector BadPort
                Real p;
                stream Real h_outflow;
            end BadPort;
            "#,
            "BadPort",
        );
    }

    /// MLS: "Stream variables only allowed in connector"
    #[test]
    fn mls_15_1_stream_only_in_connector() {
        expect_failure(
            r#"
            model BadModel
                stream Real h;
            equation
            end BadModel;
            "#,
            "BadModel",
        );
    }

    /// MLS: "Multiple stream variables with same flow"
    #[test]
    fn mls_15_1_multiple_stream_same_flow() {
        expect_parse_success(
            r#"
            connector FluidPort
                Real p;
                flow Real m_flow;
                stream Real h_outflow;
                stream Real Xi_outflow[2];
            end FluidPort;
            "#,
        );
    }

    /// Valid: Regular connector without stream
    #[test]
    fn mls_15_1_regular_connector() {
        expect_success(
            r#"
            connector Pin
                Real v;
                flow Real i;
            end Pin;

            model Test
                Pin p;
            equation
            end Test;
            "#,
            "Test",
        );
    }
}

// ============================================================================
// §15.2 INSTREAM AND ACTUALSTREAM
// ============================================================================

/// MLS §15.2: inStream and actualStream operators
mod stream_operators {
    use super::*;

    /// MLS: "inStream(v) returns upstream value"
    #[test]
    fn mls_15_2_instream_basic() {
        expect_parse_success(
            r#"
            connector FluidPort
                Real p;
                flow Real m_flow;
                stream Real h_outflow;
            end FluidPort;

            model Pipe
                FluidPort port_a, port_b;
                Real h_a_in, h_b_in;
            equation
                h_a_in = inStream(port_a.h_outflow);
                h_b_in = inStream(port_b.h_outflow);
            end Pipe;
            "#,
        );
    }

    /// MLS: "actualStream(v) returns direction-aware value"
    #[test]
    fn mls_15_2_actualstream_basic() {
        expect_parse_success(
            r#"
            connector FluidPort
                Real p;
                flow Real m_flow;
                stream Real h_outflow;
            end FluidPort;

            model Sensor
                FluidPort port;
                Real h_actual;
            equation
                h_actual = actualStream(port.h_outflow);
            end Sensor;
            "#,
        );
    }

    /// MLS: "inStream only applies to stream variables"
    #[test]
    fn mls_15_2_instream_type_check() {
        expect_failure(
            r#"
            connector Pin
                Real v;
                flow Real i;
            end Pin;

            model BadModel
                Pin p;
                Real x;
            equation
                x = inStream(p.v);
            end BadModel;
            "#,
            "BadModel",
        );
    }

    /// MLS: "actualStream only applies to stream variables"
    #[test]
    fn mls_15_2_actualstream_type_check() {
        expect_failure(
            r#"
            connector Pin
                Real v;
                flow Real i;
            end Pin;

            model BadModel
                Pin p;
                Real x;
            equation
                x = actualStream(p.v);
            end BadModel;
            "#,
            "BadModel",
        );
    }
}

// ============================================================================
// §15.3 STREAM CONNECTION EQUATIONS
// ============================================================================

/// MLS §15.3: Stream connection equation generation
mod stream_connections {
    use super::*;

    /// MLS: "Stream connection generates mixing equations"
    #[test]
    fn mls_15_3_stream_connection() {
        expect_parse_success(
            r#"
            connector FluidPort
                Real p;
                flow Real m_flow;
                stream Real h_outflow;
            end FluidPort;

            model Source
                FluidPort port;
                parameter Real h0 = 1e5;
            equation
                port.h_outflow = h0;
                port.p = 1e5;
            end Source;

            model Sink
                FluidPort port;
                Real h_in;
            equation
                h_in = inStream(port.h_outflow);
                port.h_outflow = h_in;
            end Sink;

            model Test
                Source src;
                Sink snk;
            equation
                connect(src.port, snk.port);
            end Test;
            "#,
        );
    }

    /// MLS: "Three-way junction mixing"
    #[test]
    fn mls_15_3_three_way_junction() {
        expect_parse_success(
            r#"
            connector FluidPort
                Real p;
                flow Real m_flow;
                stream Real h_outflow;
            end FluidPort;

            model Junction
                FluidPort port_a, port_b, port_c;
            equation
                port_a.p = port_b.p;
                port_b.p = port_c.p;
                port_a.m_flow + port_b.m_flow + port_c.m_flow = 0;
            end Junction;
            "#,
        );
    }

    /// MLS: "Zero-flow singularity handling"
    #[test]
    fn mls_15_3_zero_flow_handling() {
        expect_parse_success(
            r#"
            connector FluidPort
                Real p;
                flow Real m_flow;
                stream Real h_outflow;
            end FluidPort;

            model Pipe
                FluidPort port_a, port_b;
            equation
                port_a.m_flow + port_b.m_flow = 0;
                port_a.p = port_b.p;
                port_a.h_outflow = inStream(port_b.h_outflow);
                port_b.h_outflow = inStream(port_a.h_outflow);
            end Pipe;
            "#,
        );
    }
}

// ============================================================================
// §15.1 STREAM VARIABLE RESTRICTIONS
// ============================================================================

/// MLS §15.1: Additional stream restrictions
mod stream_restrictions {
    use super::*;

    /// MLS: "Cannot have input stream"
    #[test]
    fn mls_15_1_no_input_stream() {
        expect_failure(
            r#"
            connector BadPort
                input stream Real h;
                flow Real m_flow;
            end BadPort;
            "#,
            "BadPort",
        );
    }

    /// MLS: "Cannot have output stream"
    #[test]
    fn mls_15_1_no_output_stream() {
        expect_failure(
            r#"
            connector BadPort
                output stream Real h;
                flow Real m_flow;
            end BadPort;
            "#,
            "BadPort",
        );
    }

    /// MLS: "Stream in array of connectors"
    #[test]
    fn mls_15_1_stream_connector_array() {
        expect_parse_success(
            r#"
            connector FluidPort
                Real p;
                flow Real m_flow;
                stream Real h_outflow;
            end FluidPort;

            model MultiPort
                FluidPort ports[3];
            equation
            end MultiPort;
            "#,
        );
    }
}
