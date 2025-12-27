//! MLS §9.3-9.4: Connection Restrictions and Overconstrained Connections
//!
//! Comprehensive tests covering normative statements from MLS §9.3-9.4 including:
//! - §9.3: Restrictions on connections
//! - §9.4: Overconstrained connection-based equation systems
//!
//! Reference: https://specification.modelica.org/master/connectors-connections.html

use crate::spec::{expect_failure, expect_parse_success, expect_success};

// ============================================================================
// §9.1 CONNECTOR RESTRICTIONS
// ============================================================================

/// MLS §9.1: Connector component restrictions
mod connector_restrictions {
    use super::*;

    /// MLS: "A connector shall not be declared with variability prefix constant"
    #[test]
    #[ignore = "Connector variability restriction not yet enforced"]
    fn error_constant_connector() {
        expect_failure(
            r#"
            connector Pin
                Real v;
                flow Real i;
            end Pin;

            model Test
                constant Pin p;
            equation
            end Test;
            "#,
            "Test",
        );
    }

    /// MLS: "A connector shall not be declared with variability prefix parameter"
    #[test]
    #[ignore = "Connector variability restriction not yet enforced"]
    fn error_parameter_connector() {
        expect_failure(
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
        );
    }

    /// Valid connector declarations
    #[test]
    fn mls_9_1_valid_connector_declarations() {
        expect_success(
            r#"
            connector Pin
                Real v;
                flow Real i;
            end Pin;

            model Test
                Pin p1;
                Pin p2;
            equation
                connect(p1, p2);
                p1.v = 0;
            end Test;
            "#,
            "Test",
        );
    }

    /// Input/output connectors
    #[test]
    fn mls_9_1_input_output_connectors() {
        expect_success(
            r#"
            connector RealInput = input Real;
            connector RealOutput = output Real;

            model Test
                RealInput u;
                RealOutput y;
            equation
                y = u;
            end Test;
            "#,
            "Test",
        );
    }
}

// ============================================================================
// §9.2 CONNECTION EQUATION GENERATION
// ============================================================================

/// MLS §9.2: Connection equation generation rules
mod connection_equation_generation {
    use super::*;

    /// MLS: Potential variables are set equal
    #[test]
    fn mls_9_2_potential_equalization() {
        expect_success(
            r#"
            connector Pin
                Real v;
                flow Real i;
            end Pin;

            model Test
                Pin p1;
                Pin p2;
                Pin p3;
            equation
                connect(p1, p2);
                connect(p2, p3);
                p1.v = 1;
                p1.i = 0;
                p3.i = 0;
            end Test;
            "#,
            "Test",
        );
    }

    /// MLS: Flow variables sum to zero in connection set
    #[test]
    fn mls_9_2_flow_zero_sum() {
        expect_success(
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

            model Test
                Resistor r1(R = 1);
                Resistor r2(R = 2);
            equation
                r1.p.v = 1;
                r2.n.v = 0;
                connect(r1.n, r2.p);
            end Test;
            "#,
            "Test",
        );
    }

    /// Multiple connectors in connection set
    #[test]
    fn mls_9_2_multiple_connectors() {
        expect_success(
            r#"
            connector Pin
                Real v;
                flow Real i;
            end Pin;

            model Component
                Pin a;
                Pin b;
            equation
                a.v = b.v;
                a.i + b.i = 0;
            end Component;

            model Test
                Component c1;
                Component c2;
                Component c3;
            equation
                c1.a.v = 1;
                connect(c1.b, c2.a);
                connect(c1.b, c3.a);
                c2.b.v = 0;
                c3.b.v = 0;
            end Test;
            "#,
            "Test",
        );
    }

    /// Array connector connections
    #[test]
    fn mls_9_2_array_connector() {
        expect_parse_success(
            r#"
            connector Pin
                Real v;
                flow Real i;
            end Pin;

            model MultiPin
                Pin p[3];
            equation
            end MultiPin;

            model Test
                MultiPin a;
                MultiPin b;
            equation
                connect(a.p, b.p);
            end Test;
            "#,
        );
    }
}

// ============================================================================
// §9.3 CONNECTION RESTRICTIONS
// ============================================================================

/// MLS §9.3: Restrictions on connect statements
mod connection_restrictions {
    use super::*;

    /// MLS: "Connected components must be of compatible types"
    #[test]
    fn error_incompatible_connector_types() {
        expect_failure(
            r#"
            connector PinA
                Real v;
                flow Real i;
            end PinA;

            connector PinB
                Real voltage;
                flow Real current;
            end PinB;

            model Test
                PinA a;
                PinB b;
            equation
                connect(a, b);
            end Test;
            "#,
            "Test",
        );
    }

    /// MLS: "Connected components must have matching dimensions"
    #[test]
    #[ignore = "Connector dimension matching not yet checked"]
    fn error_dimension_mismatch() {
        expect_failure(
            r#"
            connector Pin
                Real v;
                flow Real i;
            end Pin;

            model A
                Pin p[3];
            end A;

            model B
                Pin p[4];
            end B;

            model Test
                A a;
                B b;
            equation
                connect(a.p, b.p);
            end Test;
            "#,
            "Test",
        );
    }

    /// MLS: "Flow and non-flow variables cannot be connected directly"
    #[test]
    fn error_flow_nonflow_mixing() {
        expect_failure(
            r#"
            connector Pin
                Real v;
                flow Real i;
            end Pin;

            model Test
                Pin p1;
                Pin p2;
            equation
                connect(p1.v, p2.i);
            end Test;
            "#,
            "Test",
        );
    }

    /// Valid connection between same connector types
    #[test]
    fn mls_9_3_same_type_connection() {
        expect_success(
            r#"
            connector Pin
                Real v;
                flow Real i;
            end Pin;

            model Test
                Pin p1;
                Pin p2;
            equation
                connect(p1, p2);
                p1.v = 1;
                p1.i = 0;
            end Test;
            "#,
            "Test",
        );
    }

    /// Connection with inherited connectors
    #[test]
    fn mls_9_3_inherited_connectors() {
        expect_parse_success(
            r#"
            connector BasePin
                Real v;
                flow Real i;
            end BasePin;

            connector ExtendedPin
                extends BasePin;
                Real temperature;
            end ExtendedPin;

            model Test
                BasePin base;
                ExtendedPin ext;
            equation
                connect(base, ext);
            end Test;
            "#,
        );
    }
}

// ============================================================================
// §9.1.3 EXPANDABLE CONNECTORS
// ============================================================================

/// MLS §9.1.3: Expandable connector requirements
mod expandable_connectors {
    use super::*;

    /// Basic expandable connector
    #[test]
    fn mls_9_1_3_basic_expandable() {
        expect_parse_success(
            r#"
            expandable connector Bus
            end Bus;

            model Test
                Bus bus1;
                Bus bus2;
            equation
                connect(bus1, bus2);
            end Test;
            "#,
        );
    }

    /// Expandable connector with predefined elements
    #[test]
    fn mls_9_1_3_expandable_with_elements() {
        expect_parse_success(
            r#"
            expandable connector ControlBus
                Real setpoint;
            end ControlBus;

            model Test
                ControlBus bus;
            equation
                bus.setpoint = 1.0;
            end Test;
            "#,
        );
    }

    /// MLS: "Flow variables cannot appear in expandable connectors"
    #[test]
    fn error_flow_in_expandable() {
        expect_failure(
            r#"
            expandable connector BadBus
                flow Real f;
            end BadBus;
            "#,
            "BadBus",
        );
    }

    /// Connecting to expandable connector
    #[test]
    fn mls_9_1_3_connect_to_expandable() {
        expect_parse_success(
            r#"
            connector RealOutput = output Real;

            expandable connector Bus
            end Bus;

            model Sensor
                RealOutput y;
            equation
                y = 1.0;
            end Sensor;

            model Controller
                Bus bus;
                Sensor sensor;
            equation
                connect(sensor.y, bus.sensorValue);
            end Controller;
            "#,
        );
    }
}

// ============================================================================
// §9.4 OVERCONSTRAINED CONNECTION-BASED EQUATION SYSTEMS
// ============================================================================

/// MLS §9.4: Overconstrained connection handling
mod overconstrained_connections {
    use super::*;

    /// Basic connector with equalityConstraint function
    #[test]
    fn mls_9_4_equality_constraint() {
        expect_parse_success(
            r#"
            operator record Orientation
                Real R[3,3];
                function equalityConstraint
                    input Orientation R1;
                    input Orientation R2;
                    output Real residue[3];
                algorithm
                    residue := {0, 0, 0};
                end equalityConstraint;
            end Orientation;

            connector Frame
                Real r[3];
                Orientation R;
            end Frame;
            "#,
        );
    }

    /// Connections.branch function
    #[test]
    fn mls_9_4_connections_branch() {
        expect_parse_success(
            r#"
            connector Frame
                Real r[3];
                Real R[3,3];
            end Frame;

            model RigidBody
                Frame frame_a;
                Frame frame_b;
            equation
                Connections.branch(frame_a, frame_b);
                frame_b.r = frame_a.r;
                frame_b.R = frame_a.R;
            end RigidBody;
            "#,
        );
    }

    /// Connections.root function
    #[test]
    fn mls_9_4_connections_root() {
        expect_parse_success(
            r#"
            connector Frame
                Real r[3];
                Real R[3,3];
            end Frame;

            model World
                Frame frame_b;
            equation
                Connections.root(frame_b);
                frame_b.r = {0, 0, 0};
            end World;
            "#,
        );
    }

    /// Connections.potentialRoot function
    #[test]
    fn mls_9_4_connections_potential_root() {
        expect_parse_success(
            r#"
            connector Frame
                Real r[3];
                Real R[3,3];
            end Frame;

            model Body
                Frame frame_a;
                parameter Integer priority = 0;
            equation
                Connections.potentialRoot(frame_a, priority);
            end Body;
            "#,
        );
    }

    /// Connections.isRoot function
    #[test]
    fn mls_9_4_connections_is_root() {
        expect_parse_success(
            r#"
            connector Frame
                Real r[3];
                Real R[3,3];
            end Frame;

            model Body
                Frame frame_a;
                Boolean isRoot;
            equation
                Connections.potentialRoot(frame_a);
                isRoot = Connections.isRoot(frame_a);
            end Body;
            "#,
        );
    }

    /// Connections.rooted function
    #[test]
    fn mls_9_4_connections_rooted() {
        expect_parse_success(
            r#"
            connector Frame
                Real r[3];
                Real R[3,3];
            end Frame;

            model Joint
                Frame frame_a;
                Frame frame_b;
            equation
                Connections.branch(frame_a, frame_b);
                if Connections.rooted(frame_a) then
                    frame_b.r = frame_a.r;
                else
                    frame_a.r = frame_b.r;
                end if;
            end Joint;
            "#,
        );
    }
}

// ============================================================================
// CONNECTION SEMANTICS - SIGNAL FLOW
// ============================================================================

/// Signal flow and causality in connections
mod signal_flow {
    use super::*;

    /// MLS: "A connection set may have at most one component with output causality"
    #[test]
    #[ignore = "Connection set signal source validation not yet implemented"]
    fn error_multiple_outputs_connected() {
        expect_failure(
            r#"
            connector RealOutput = output Real;

            model Source1
                RealOutput y = 1;
            end Source1;

            model Source2
                RealOutput y = 2;
            end Source2;

            model Test
                Source1 s1;
                Source2 s2;
            equation
                connect(s1.y, s2.y);
            end Test;
            "#,
            "Test",
        );
    }

    /// Valid: one output to multiple inputs
    #[test]
    fn mls_9_output_to_multiple_inputs() {
        expect_success(
            r#"
            connector RealInput = input Real;
            connector RealOutput = output Real;

            model Source
                RealOutput y = 1;
            end Source;

            model Sink
                RealInput u;
            equation
            end Sink;

            model Test
                Source src;
                Sink sink1;
                Sink sink2;
            equation
                connect(src.y, sink1.u);
                connect(src.y, sink2.u);
            end Test;
            "#,
            "Test",
        );
    }

    /// Input/output in block diagram
    #[test]
    fn mls_9_block_diagram() {
        expect_success(
            r#"
            connector RealInput = input Real;
            connector RealOutput = output Real;

            block Gain
                RealInput u;
                RealOutput y;
                parameter Real k = 1;
            equation
                y = k * u;
            end Gain;

            block Add
                RealInput u1;
                RealInput u2;
                RealOutput y;
            equation
                y = u1 + u2;
            end Add;

            model Test
                Gain g1(k = 2);
                Gain g2(k = 3);
                Add add;
            equation
                g1.u = 1;
                g2.u = 2;
                connect(g1.y, add.u1);
                connect(g2.y, add.u2);
            end Test;
            "#,
            "Test",
        );
    }
}

// ============================================================================
// COMPLEX CONNECTION SCENARIOS
// ============================================================================

/// Complex connection scenarios
mod complex_scenarios {
    use super::*;

    /// Hierarchical connections
    #[test]
    fn complex_hierarchical_connections() {
        expect_success(
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

            model SubCircuit
                Pin p;
                Pin n;
                Resistor r1(R = 1);
                Resistor r2(R = 2);
            equation
                connect(p, r1.p);
                connect(r1.n, r2.p);
                connect(r2.n, n);
            end SubCircuit;

            model Test
                SubCircuit sc;
            equation
                sc.p.v = 1;
                sc.n.v = 0;
            end Test;
            "#,
            "Test",
        );
    }

    /// Parallel resistor network
    #[test]
    fn complex_parallel_network() {
        expect_success(
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

            model ParallelResistors
                Resistor r1(R = 100);
                Resistor r2(R = 200);
                Resistor r3(R = 300);
                Ground gnd;
            equation
                r1.p.v = 5;
                connect(r1.p, r2.p);
                connect(r1.p, r3.p);
                connect(r1.n, gnd.p);
                connect(r2.n, gnd.p);
                connect(r3.n, gnd.p);
            end ParallelResistors;
            "#,
            "ParallelResistors",
        );
    }

    /// Mixed domain connections
    #[test]
    fn complex_electro_thermal() {
        expect_success(
            r#"
            connector Pin
                Real v;
                flow Real i;
            end Pin;

            connector HeatPort
                Real T;
                flow Real Q_flow;
            end HeatPort;

            model HeatingResistor
                Pin p;
                Pin n;
                HeatPort heatPort;
                parameter Real R = 1;
                Real LossPower;
            equation
                p.v - n.v = R * p.i;
                p.i + n.i = 0;
                LossPower = (p.v - n.v) * p.i;
                heatPort.Q_flow = -LossPower;
            end HeatingResistor;

            model ThermalGround
                HeatPort port;
                parameter Real T = 300;
            equation
                port.T = T;
            end ThermalGround;

            model Test
                HeatingResistor r(R = 100);
                ThermalGround thermal;
            equation
                r.p.v = 10;
                r.n.v = 0;
                connect(r.heatPort, thermal.port);
            end Test;
            "#,
            "Test",
        );
    }
}
