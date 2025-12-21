//! MLS §9.2: Flow Variable Semantics
//!
//! Tests for flow variable behavior in connections including:
//! - Flow sum to zero rule
//! - Flow sign convention
//! - Unconnected flows
//! - Multiple flow variables
//!
//! Reference: https://specification.modelica.org/master/connectors-connections.html

use crate::spec::{expect_parse_success, expect_success};

// ============================================================================
// §9.2.1 FLOW SUM TO ZERO
// ============================================================================

/// MLS §9.2: Flow variables sum to zero at connection points
mod flow_zero_sum {
    use super::*;

    /// MLS: Two-way connection flow balance
    #[test]
    fn mls_9_2_two_way_flow() {
        expect_success(
            r#"
            connector C
                Real e;
                flow Real f;
            end C;

            model TwoPort
                C a, b;
            equation
                a.e = b.e;
                a.f + b.f = 0;
            end TwoPort;

            model Test
                TwoPort p1, p2;
            equation
                p1.a.e = 1;
                connect(p1.b, p2.a);
                p2.b.e = 0;
            end Test;
            "#,
            "Test",
        );
    }

    /// MLS: Three-way connection flow balance
    #[test]
    fn mls_9_2_three_way_flow() {
        expect_parse_success(
            r#"
            connector C
                Real e;
                flow Real f;
            end C;

            model Source
                C port;
            equation
                port.f = -1;
            end Source;

            model Sink
                C port;
            equation
                port.f = 0.5;
            end Sink;

            model Test
                Source src;
                Sink sink1, sink2;
            equation
                connect(src.port, sink1.port);
                connect(src.port, sink2.port);
            end Test;
            "#,
        );
    }

    /// MLS: Star connection flow balance
    #[test]
    fn mls_9_2_star_connection() {
        expect_parse_success(
            r#"
            connector C
                Real e;
                flow Real f;
            end C;

            model Element
                C port;
            equation
                port.e = 1;
            end Element;

            model Star
                Element e1, e2, e3, e4;
            equation
                connect(e1.port, e2.port);
                connect(e1.port, e3.port);
                connect(e1.port, e4.port);
            end Star;
            "#,
        );
    }

    /// MLS: Chain connection maintains flow balance
    #[test]
    fn mls_9_2_chain_flow() {
        expect_success(
            r#"
            connector C
                Real e;
                flow Real f;
            end C;

            model Element
                C a, b;
            equation
                a.e = b.e;
                a.f + b.f = 0;
            end Element;

            model Chain
                Element e1, e2, e3;
            equation
                e1.a.e = 1;
                connect(e1.b, e2.a);
                connect(e2.b, e3.a);
                e3.b.e = 0;
            end Chain;
            "#,
            "Chain",
        );
    }
}

// ============================================================================
// §9.2.2 FLOW SIGN CONVENTION
// ============================================================================

/// MLS §9.2: Flow sign convention
mod flow_sign {
    use super::*;

    /// MLS: Positive flow into component
    #[test]
    fn mls_9_2_flow_into_component() {
        expect_success(
            r#"
            connector C
                Real e;
                flow Real f;
            end C;

            model Consumer
                C port;
                Real power;
            equation
                power = port.e * port.f;
                port.e = 1;
            end Consumer;
            "#,
            "Consumer",
        );
    }

    /// MLS: Flow conservation in two-port
    #[test]
    fn mls_9_2_flow_conservation() {
        expect_success(
            r#"
            connector Pin
                Real v;
                flow Real i;
            end Pin;

            model Resistor
                Pin p, n;
                parameter Real R = 1;
            equation
                p.v - n.v = R * p.i;
                p.i + n.i = 0;
            end Resistor;
            "#,
            "Resistor",
        );
    }

    /// MLS: Multiple flow direction handling
    #[test]
    fn mls_9_2_bidirectional_flow() {
        expect_parse_success(
            r#"
            connector FluidPort
                Real p;
                flow Real m_flow;
            end FluidPort;

            model Pipe
                FluidPort a, b;
                parameter Real dp = 0.1;
            equation
                a.p - b.p = dp * a.m_flow;
                a.m_flow + b.m_flow = 0;
            end Pipe;
            "#,
        );
    }
}

// ============================================================================
// §9.2.3 UNCONNECTED FLOWS
// ============================================================================

/// MLS §9.2: Unconnected flow behavior
mod unconnected_flow {
    use super::*;

    /// MLS: Unconnected flow defaults to zero
    #[test]
    fn mls_9_2_unconnected_zero() {
        // A component with external connectors is partial by design
        // This test verifies the pattern is valid syntax and semantics
        expect_success(
            r#"
            connector C
                Real e;
                flow Real f;
            end C;

            model Source
                C port;
            equation
                port.e = 5;
            end Source;
            "#,
            "Source",
        );
    }

    /// MLS: Partially connected model
    #[test]
    fn mls_9_2_partially_connected() {
        expect_success(
            r#"
            connector C
                Real e;
                flow Real f;
            end C;

            model TwoPort
                C a, b;
            equation
                a.e = b.e;
                a.f + b.f = 0;
            end TwoPort;

            model Test
                TwoPort tp;
            equation
                tp.a.e = 1;
            end Test;
            "#,
            "Test",
        );
    }
}

// ============================================================================
// §9.2.4 MULTIPLE FLOW VARIABLES
// ============================================================================

/// MLS §9.2: Multiple flow variables in one connector
mod multiple_flows {
    use super::*;

    /// MLS: Multiple independent flows
    #[test]
    fn mls_9_2_multiple_independent() {
        expect_parse_success(
            r#"
            connector ThermoFluid
                Real p;
                flow Real m_flow;
                flow Real H_flow;
            end ThermoFluid;

            model Component
                ThermoFluid a, b;
            equation
                a.p = b.p;
                a.m_flow + b.m_flow = 0;
                a.H_flow + b.H_flow = 0;
            end Component;
            "#,
        );
    }

    /// MLS: Flow arrays
    #[test]
    fn mls_9_2_flow_arrays() {
        expect_parse_success(
            r#"
            connector MultiPhase
                Real v[3];
                flow Real i[3];
            end MultiPhase;

            model ThreePhaseLoad
                MultiPhase port;
            equation
                port.v = {1, 2, 3};
            end ThreePhaseLoad;
            "#,
        );
    }

    /// MLS: Nested connector flows
    #[test]
    fn mls_9_2_nested_flows() {
        expect_parse_success(
            r#"
            connector Electrical
                Real v;
                flow Real i;
            end Electrical;

            connector Thermal
                Real T;
                flow Real Q;
            end Thermal;

            connector Combined
                Electrical elec;
                Thermal therm;
            end Combined;

            model Device
                Combined port;
            equation
                port.elec.v = 1;
                port.therm.T = 300;
            end Device;
            "#,
        );
    }
}

// ============================================================================
// §9.2.5 POTENTIAL VARIABLE EQUALIZATION
// ============================================================================

/// MLS §9.2: Potential variable equalization
mod potential_equalization {
    use super::*;

    /// MLS: Connected potentials are equal
    #[test]
    fn mls_9_2_potential_equal() {
        expect_success(
            r#"
            connector C
                Real e;
                flow Real f;
            end C;

            model Test
                C a, b, c;
            equation
                a.e = 5;
                a.f = 0;
                connect(a, b);
                connect(b, c);
            end Test;
            "#,
            "Test",
        );
    }

    /// MLS: Multiple potential variables
    #[test]
    fn mls_9_2_multiple_potentials() {
        expect_parse_success(
            r#"
            connector ComplexPin
                Real v_re;
                Real v_im;
                flow Real i_re;
                flow Real i_im;
            end ComplexPin;

            model Test
                ComplexPin a, b;
            equation
                a.v_re = 1;
                a.v_im = 0;
                connect(a, b);
            end Test;
            "#,
        );
    }

    /// MLS: Potential array equalization
    #[test]
    fn mls_9_2_potential_array() {
        expect_parse_success(
            r#"
            connector VectorPort
                Real x[3];
                flow Real f[3];
            end VectorPort;

            model Test
                VectorPort a, b;
            equation
                a.x = {1, 2, 3};
                connect(a, b);
            end Test;
            "#,
        );
    }
}

// ============================================================================
// ELECTRICAL DOMAIN EXAMPLES
// ============================================================================

/// Electrical domain connection examples
mod electrical_examples {
    use super::*;

    /// Kirchhoff's current law via connections
    #[test]
    fn electrical_kcl() {
        expect_success(
            r#"
            connector Pin
                Real v;
                flow Real i;
            end Pin;

            model Resistor
                Pin p, n;
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

            model VoltageSource
                Pin p, n;
                parameter Real V = 1;
            equation
                p.v - n.v = V;
                p.i + n.i = 0;
            end VoltageSource;

            model SimpleCircuit
                VoltageSource vs(V = 5);
                Resistor r1(R = 100);
                Resistor r2(R = 200);
                Ground gnd;
            equation
                connect(vs.p, r1.p);
                connect(r1.n, r2.p);
                connect(r2.n, vs.n);
                connect(vs.n, gnd.p);
            end SimpleCircuit;
            "#,
            "SimpleCircuit",
        );
    }

    /// Parallel resistors
    #[test]
    fn electrical_parallel() {
        expect_success(
            r#"
            connector Pin
                Real v;
                flow Real i;
            end Pin;

            model Resistor
                Pin p, n;
                parameter Real R = 1;
            equation
                p.v - n.v = R * p.i;
                p.i + n.i = 0;
            end Resistor;

            model ParallelCircuit
                Resistor r1(R = 100);
                Resistor r2(R = 200);
            equation
                r1.p.v = 5;
                r1.n.v = 0;
                connect(r1.p, r2.p);
                connect(r1.n, r2.n);
            end ParallelCircuit;
            "#,
            "ParallelCircuit",
        );
    }
}

// ============================================================================
// MECHANICAL DOMAIN EXAMPLES
// ============================================================================

/// Mechanical domain connection examples
mod mechanical_examples {
    use super::*;

    /// Rotational mechanics
    #[test]
    fn mechanical_rotational() {
        expect_parse_success(
            r#"
            connector Flange
                Real phi "Angle";
                flow Real tau "Torque";
            end Flange;

            model Inertia
                Flange a, b;
                parameter Real J = 1;
                Real w "Angular velocity";
                Real alpha "Angular acceleration";
            equation
                a.phi = b.phi;
                w = der(a.phi);
                alpha = der(w);
                J * alpha = a.tau + b.tau;
            end Inertia;
            "#,
        );
    }

    /// Translational mechanics
    #[test]
    fn mechanical_translational() {
        expect_parse_success(
            r#"
            connector Support
                Real s "Position";
                flow Real f "Force";
            end Support;

            model Mass
                Support a, b;
                parameter Real m = 1;
                Real v "Velocity";
            equation
                a.s = b.s;
                v = der(a.s);
                m * der(v) = a.f + b.f;
            end Mass;
            "#,
        );
    }
}

// ============================================================================
// THERMAL DOMAIN EXAMPLES
// ============================================================================

/// Thermal domain connection examples
mod thermal_examples {
    use super::*;

    /// Thermal conduction
    #[test]
    fn thermal_conduction() {
        expect_parse_success(
            r#"
            connector HeatPort
                Real T "Temperature";
                flow Real Q_flow "Heat flow";
            end HeatPort;

            model ThermalConductor
                HeatPort a, b;
                parameter Real G = 1 "Conductance";
            equation
                a.Q_flow = G * (a.T - b.T);
                a.Q_flow + b.Q_flow = 0;
            end ThermalConductor;

            model HeatCapacity
                HeatPort port;
                parameter Real C = 1 "Heat capacity";
            equation
                C * der(port.T) = port.Q_flow;
            end HeatCapacity;
            "#,
        );
    }
}
