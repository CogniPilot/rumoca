//! MLS Chapter 9: High-Priority Edge Case Tests
//!
//! This module contains critical tests for connection edge cases
//! and advanced scenarios in connector behavior.
//!
//! Reference: https://specification.modelica.org/master/connectors-connections.html

use crate::spec::{expect_failure, expect_parse_success, expect_success};

// ============================================================================
// CRITICAL: CONNECTION GRAPH EDGE CASES
// ============================================================================

/// Critical connection graph edge cases
mod connection_critical {
    use super::*;

    /// Critical: Connection with self
    #[test]
    fn critical_self_connection() {
        expect_failure(
            r#"
            connector C
                Real v;
                flow Real i;
            end C;

            model Test
                C c;
            equation
                connect(c, c);
            end Test;
            "#,
            "Test",
        );
    }

    /// Critical: Circular connection chain
    #[test]
    fn critical_circular_chain() {
        expect_parse_success(
            r#"
            connector C
                Real v;
                flow Real i;
            end C;

            model Element
                C a, b;
            equation
                a.v = b.v;
                a.i + b.i = 0;
            end Element;

            model Ring
                Element e1, e2, e3, e4;
            equation
                connect(e1.b, e2.a);
                connect(e2.b, e3.a);
                connect(e3.b, e4.a);
                connect(e4.b, e1.a);
            end Ring;
            "#,
        );
    }

    /// Critical: Star topology
    #[test]
    fn critical_star_topology() {
        expect_parse_success(
            r#"
            connector C
                Real v;
                flow Real i;
            end C;

            model Element
                C port;
            equation
                port.v = 1;
            end Element;

            model Star
                Element center;
                Element leaf1, leaf2, leaf3, leaf4, leaf5;
            equation
                connect(center.port, leaf1.port);
                connect(center.port, leaf2.port);
                connect(center.port, leaf3.port);
                connect(center.port, leaf4.port);
                connect(center.port, leaf5.port);
            end Star;
            "#,
        );
    }

    /// Critical: Diamond connection pattern
    #[test]
    fn critical_diamond_pattern() {
        expect_parse_success(
            r#"
            connector C
                Real v;
                flow Real i;
            end C;

            model Element
                C a, b;
            equation
                a.v = b.v;
                a.i + b.i = 0;
            end Element;

            model Diamond
                Element top;
                Element left, right;
                Element bottom;
            equation
                connect(top.b, left.a);
                connect(top.b, right.a);
                connect(left.b, bottom.a);
                connect(right.b, bottom.a);
            end Diamond;
            "#,
        );
    }

    /// Critical: Deep hierarchy connection
    #[test]
    fn critical_deep_hierarchy() {
        expect_success(
            r#"
            connector Pin
                Real v;
                flow Real i;
            end Pin;

            model Level1
                Pin p;
            equation
                p.v = 1;
            end Level1;

            model Level2
                Level1 sub;
            equation
            end Level2;

            model Level3
                Level2 sub;
            equation
            end Level3;

            model Level4
                Level3 sub;
            equation
            end Level4;

            model Test
                Level4 a;
                Level4 b;
            equation
                connect(a.sub.sub.sub.p, b.sub.sub.sub.p);
            end Test;
            "#,
            "Test",
        );
    }
}

// ============================================================================
// CRITICAL: FLOW BALANCE EDGE CASES
// ============================================================================

/// Critical flow balance edge cases
mod flow_balance_critical {
    use super::*;

    /// Critical: Zero flow at isolated node
    #[test]
    fn critical_isolated_zero_flow() {
        expect_success(
            r#"
            connector C
                Real v;
                flow Real i;
            end C;

            model Source
                C port;
            equation
                port.v = 5;
            end Source;
            "#,
            "Source",
        );
    }

    /// Critical: Multiple flows cancel
    #[test]
    fn critical_flow_cancellation() {
        expect_parse_success(
            r#"
            connector C
                Real v;
                flow Real i;
            end C;

            model Source
                C port;
                parameter Real I = 1;
            equation
                port.i = -I;
                port.v = 5;
            end Source;

            model Sink
                C port;
                parameter Real I = 1;
            equation
                port.i = I;
            end Sink;

            model Test
                Source s1(I = 1);
                Source s2(I = 1);
                Sink sink(I = 2);
            equation
                connect(s1.port, sink.port);
                connect(s2.port, sink.port);
            end Test;
            "#,
        );
    }

    /// Critical: Large connection set
    #[test]
    fn critical_large_connection_set() {
        expect_parse_success(
            r#"
            connector C
                Real v;
                flow Real i;
            end C;

            model Node
                C port;
            equation
                port.v = 1;
            end Node;

            model LargeSet
                Node n1, n2, n3, n4, n5, n6, n7, n8;
            equation
                connect(n1.port, n2.port);
                connect(n1.port, n3.port);
                connect(n1.port, n4.port);
                connect(n1.port, n5.port);
                connect(n1.port, n6.port);
                connect(n1.port, n7.port);
                connect(n1.port, n8.port);
            end LargeSet;
            "#,
        );
    }
}

// ============================================================================
// CRITICAL: CONNECTOR ARRAY EDGE CASES
// ============================================================================

/// Critical connector array edge cases
mod array_critical {
    use super::*;

    /// Critical: Connect array slices
    #[test]
    fn critical_array_slices() {
        expect_parse_success(
            r#"
            connector C
                Real v;
                flow Real i;
            end C;

            model Test
                C a[6], b[3];
            equation
                connect(a[1:3], b);
            end Test;
            "#,
        );
    }

    /// Critical: Parameter-sized connector arrays
    #[test]
    fn critical_param_sized_arrays() {
        expect_parse_success(
            r#"
            connector C
                Real v;
                flow Real i;
            end C;

            model Parallel
                parameter Integer n = 5;
                C a[n], b[n];
            equation
                for i in 1:n loop
                    connect(a[i], b[i]);
                end for;
            end Parallel;
            "#,
        );
    }

    /// Critical: 2D connector grid
    #[test]
    fn critical_2d_grid() {
        expect_parse_success(
            r#"
            connector C
                Real v;
                flow Real i;
            end C;

            model Grid
                C nodes[3, 3];
            equation
                for i in 1:2 loop
                    for j in 1:3 loop
                        connect(nodes[i, j], nodes[i+1, j]);
                    end for;
                end for;
            end Grid;
            "#,
        );
    }
}

// ============================================================================
// CRITICAL: EXPANDABLE CONNECTOR EDGE CASES
// ============================================================================

/// Critical expandable connector edge cases
mod expandable_critical {
    use super::*;

    /// Critical: Dynamic element addition
    #[test]
    fn critical_dynamic_addition() {
        expect_parse_success(
            r#"
            expandable connector Bus
            end Bus;

            connector RealSignal
                Real value;
            end RealSignal;

            model Sensor
                Bus bus;
                RealSignal signal;
            equation
                connect(bus.temperature, signal);
            end Sensor;
            "#,
        );
    }

    /// Critical: Mixed expandable connections
    #[test]
    fn critical_mixed_expandable() {
        expect_parse_success(
            r#"
            expandable connector Bus
                Real speed;
            end Bus;

            connector Signal
                Real value;
            end Signal;

            model Test
                Bus bus1, bus2;
                Signal s;
            equation
                connect(bus1, bus2);
                connect(bus1.extra, s);
            end Test;
            "#,
        );
    }
}

// ============================================================================
// CRITICAL: NESTED CONNECTOR EDGE CASES
// ============================================================================

/// Critical nested connector edge cases
mod nested_critical {
    use super::*;

    /// Critical: Deep nested connector
    #[test]
    fn critical_deep_nested() {
        expect_parse_success(
            r#"
            connector Level1
                Real a;
                flow Real fa;
            end Level1;

            connector Level2
                Level1 sub;
            end Level2;

            connector Level3
                Level2 sub;
            end Level3;

            model Test
                Level3 port1, port2;
            equation
                connect(port1, port2);
            end Test;
            "#,
        );
    }

    /// Critical: Mixed nested and flat
    #[test]
    fn critical_mixed_nested() {
        expect_parse_success(
            r#"
            connector Simple
                Real v;
                flow Real i;
            end Simple;

            connector Compound
                Simple a;
                Simple b;
                Real extra;
            end Compound;

            model Test
                Compound c1, c2;
            equation
                connect(c1, c2);
            end Test;
            "#,
        );
    }

    /// Critical: Partial nested connection
    #[test]
    fn critical_partial_nested() {
        expect_parse_success(
            r#"
            connector Simple
                Real v;
                flow Real i;
            end Simple;

            connector Compound
                Simple a;
                Simple b;
            end Compound;

            model Test
                Compound c;
                Simple s;
            equation
                connect(c.a, s);
            end Test;
            "#,
        );
    }
}

// ============================================================================
// CRITICAL: STREAM CONNECTOR EDGE CASES
// ============================================================================

/// Critical stream connector edge cases
mod stream_critical {
    use super::*;

    /// Critical: Basic stream connector
    #[test]
    fn critical_stream_basic() {
        expect_parse_success(
            r#"
            connector FluidPort
                Real p;
                flow Real m_flow;
                stream Real h_outflow;
            end FluidPort;

            model Tank
                FluidPort port;
            equation
                port.p = 1e5;
                port.h_outflow = 1e6;
            end Tank;
            "#,
        );
    }

    /// Critical: Multiple stream variables
    #[test]
    fn critical_multiple_streams() {
        expect_parse_success(
            r#"
            connector MultiStream
                Real p;
                flow Real m_flow;
                stream Real h_outflow;
                stream Real Xi_outflow[3];
            end MultiStream;

            model Component
                MultiStream port;
            equation
                port.p = 1e5;
                port.h_outflow = 1e6;
                port.Xi_outflow = {0.2, 0.3, 0.5};
            end Component;
            "#,
        );
    }

    /// Critical: Stream connector connection
    #[test]
    fn critical_stream_connection() {
        expect_parse_success(
            r#"
            connector FluidPort
                Real p;
                flow Real m_flow;
                stream Real h_outflow;
            end FluidPort;

            model Pipe
                FluidPort a, b;
            equation
                a.p = b.p;
                a.m_flow + b.m_flow = 0;
                a.h_outflow = b.h_outflow;
            end Pipe;

            model System
                Pipe p1, p2;
            equation
                connect(p1.b, p2.a);
                p1.a.p = 1e5;
                p1.a.h_outflow = 1e6;
                p2.b.p = 0.9e5;
                p2.b.h_outflow = 1e6;
            end System;
            "#,
        );
    }
}

// ============================================================================
// CRITICAL: ERROR DETECTION
// ============================================================================

/// Critical error detection cases
mod error_critical {
    use super::*;

    /// Critical: Incompatible flow/non-flow mix
    #[test]
    fn critical_incompatible_flow_mix() {
        expect_failure(
            r#"
            connector A
                Real v;
                flow Real i;
            end A;

            connector B
                flow Real v;
                Real i;
            end B;

            model Test
                A a;
                B b;
            equation
                connect(a, b);
            end Test;
            "#,
            "Test",
        );
    }

    /// Critical: Connect inside if
    #[test]
    fn critical_connect_in_if() {
        expect_parse_success(
            r#"
            connector C
                Real v;
                flow Real i;
            end C;

            model Test
                C a, b, c;
                parameter Boolean useB = true;
            equation
                if useB then
                    connect(a, b);
                else
                    connect(a, c);
                end if;
            end Test;
            "#,
        );
    }

    /// Critical: Connect in when (should be invalid)
    #[test]
    fn critical_connect_in_when_error() {
        expect_failure(
            r#"
            connector C
                Real v;
                flow Real i;
            end C;

            model Test
                C a, b;
                Real t(start = 0);
            equation
                der(t) = 1;
                when t > 1 then
                    connect(a, b);
                end when;
            end Test;
            "#,
            "Test",
        );
    }
}

// ============================================================================
// COMPLEX SCENARIOS
// ============================================================================

/// Complex connection scenarios
mod complex_scenarios {
    use super::*;

    /// Complex: Multi-domain system
    #[test]
    fn complex_multi_domain() {
        expect_parse_success(
            r#"
            connector ElectricalPin
                Real v;
                flow Real i;
            end ElectricalPin;

            connector HeatPort
                Real T;
                flow Real Q_flow;
            end HeatPort;

            model HeatedResistor
                ElectricalPin p, n;
                HeatPort heatPort;
                parameter Real R = 100;
                Real power;
            equation
                p.v - n.v = R * p.i;
                p.i + n.i = 0;
                power = R * p.i^2;
                heatPort.Q_flow = -power;
            end HeatedResistor;
            "#,
        );
    }

    /// Complex: Component with multiple connector types
    #[test]
    fn complex_multiple_connector_types() {
        expect_parse_success(
            r#"
            connector Flange
                Real phi;
                flow Real tau;
            end Flange;

            connector Pin
                Real v;
                flow Real i;
            end Pin;

            model Motor
                Flange shaft;
                Pin p, n;
                parameter Real k = 1;
            equation
                shaft.tau = k * (p.i);
                p.v - n.v = k * der(shaft.phi);
                p.i + n.i = 0;
            end Motor;
            "#,
        );
    }
}
