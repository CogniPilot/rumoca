//! MLS §9: Medium Priority Connector and Connection Tests
//!
//! This module tests medium priority normative requirements:
//! - §9.1: Connector restrictions (no parameter/constant)
//! - §9.2: Flow singleton sets (unconnected flow)
//! - §9.3: Mixed flow/non-flow forbidden
//! - §9.4: Expandable connectors
//! - §9.5: Overconstrained connections
//!
//! Reference: https://specification.modelica.org/master/connectors-connections.html

use crate::spec::{expect_failure, expect_parse_success, expect_success};

// ============================================================================
// §9.1 CONNECTOR RESTRICTIONS
// ============================================================================

/// MLS §9.1: Connector declaration restrictions
mod connector_restrictions {
    use super::*;

    /// MLS: "Connectors cannot have parameter components with flow/potential"
    #[test]
    #[ignore = "Connector parameter restriction not yet implemented"]
    fn mls_9_1_connector_no_parameter() {
        expect_failure(
            r#"
            connector BadConnector
                parameter Real p = 1.0;
                Real v;
                flow Real i;
            end BadConnector;
            "#,
            "BadConnector",
        );
    }

    /// MLS: "Connectors cannot have constant components"
    #[test]
    #[ignore = "Connector constant restriction not yet implemented"]
    fn mls_9_1_connector_no_constant() {
        expect_failure(
            r#"
            connector BadConnector
                constant Real c = 1.0;
                Real v;
                flow Real i;
            end BadConnector;
            "#,
            "BadConnector",
        );
    }

    /// Valid: Connector with only potential and flow
    #[test]
    fn mls_9_1_valid_connector() {
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

    /// Valid: Connector with nested connector
    #[test]
    fn mls_9_1_nested_connector() {
        expect_success(
            r#"
            connector Inner
                Real x;
                flow Real f;
            end Inner;

            connector Outer
                Inner a;
                Inner b;
            end Outer;

            model Test
                Outer o;
            equation
            end Test;
            "#,
            "Test",
        );
    }
}

// ============================================================================
// §9.2 FLOW VARIABLE RESTRICTIONS
// ============================================================================

/// MLS §9.2: Flow variable handling
mod flow_restrictions {
    use super::*;

    /// MLS: "Unconnected flow variables are set to zero"
    #[test]
    fn mls_9_2_unconnected_flow_zero() {
        expect_success(
            r#"
            connector Pin
                Real v;
                flow Real i;
            end Pin;

            model Test
                Pin p;
            equation
                p.v = 1.0;
            end Test;
            "#,
            "Test",
        );
    }

    /// MLS: "Flow variables sum to zero at connection"
    #[test]
    fn mls_9_2_flow_sum_zero() {
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

            model Test
                Resistor r1, r2;
            equation
                connect(r1.n, r2.p);
            end Test;
            "#,
            "Test",
        );
    }

    /// MLS: "Flow sign convention: positive into component"
    #[test]
    fn mls_9_2_flow_sign_convention() {
        expect_success(
            r#"
            connector Pin
                Real v;
                flow Real i;
            end Pin;

            model Source
                Pin p;
                parameter Real I = 1;
            equation
                p.i = -I;
            end Source;

            model Load
                Pin p;
            equation
                p.i = p.v;
            end Load;

            model Test
                Source src;
                Load load;
            equation
                connect(src.p, load.p);
            end Test;
            "#,
            "Test",
        );
    }
}

// ============================================================================
// §9.3 CONNECTION RESTRICTIONS
// ============================================================================

/// MLS §9.3: Connection restrictions
mod connection_restrictions {
    use super::*;

    /// MLS: "Cannot connect flow to non-flow variable"
    #[test]
    fn mls_9_3_no_flow_nonflow_mix() {
        expect_failure(
            r#"
            connector FlowConnector
                flow Real f;
            end FlowConnector;

            connector PotentialConnector
                Real v;
            end PotentialConnector;

            model Test
                FlowConnector fc;
                PotentialConnector pc;
            equation
                connect(fc.f, pc.v);
            end Test;
            "#,
            "Test",
        );
    }

    /// MLS: "Connectors in connect must be compatible"
    #[test]
    fn mls_9_3_connector_type_mismatch() {
        expect_failure(
            r#"
            connector ElectricalPin
                Real v;
                flow Real i;
            end ElectricalPin;

            connector MechanicalFlange
                Real s;
                flow Real f;
            end MechanicalFlange;

            model Test
                ElectricalPin ep;
                MechanicalFlange mf;
            equation
                connect(ep, mf);
            end Test;
            "#,
            "Test",
        );
    }

    /// Valid: Compatible connector types
    #[test]
    fn mls_9_3_compatible_connectors() {
        expect_success(
            r#"
            connector Pin
                Real v;
                flow Real i;
            end Pin;

            model Component
                Pin p, n;
            equation
            end Component;

            model Test
                Component c1, c2;
            equation
                connect(c1.n, c2.p);
            end Test;
            "#,
            "Test",
        );
    }
}

// ============================================================================
// §9.4 EXPANDABLE CONNECTORS
// ============================================================================

/// MLS §9.4: Expandable connector rules
mod expandable_connectors {
    use super::*;

    /// MLS: "expandable connector syntax"
    #[test]
    fn mls_9_4_expandable_syntax() {
        expect_parse_success(
            r#"
            expandable connector Bus
            end Bus;

            model Test
                Bus bus;
            equation
            end Test;
            "#,
        );
    }

    /// MLS: "expandable connector can have any variables added"
    #[test]
    fn mls_9_4_expandable_dynamic() {
        expect_parse_success(
            r#"
            expandable connector Bus
            end Bus;

            connector Signal
                Real s;
            end Signal;

            model Sender
                Bus bus;
                Signal sig;
            equation
                connect(sig, bus.signal1);
            end Sender;
            "#,
        );
    }

    /// MLS: "flow forbidden in expandable connector declaration"
    #[test]
    fn mls_9_4_expandable_no_flow() {
        expect_failure(
            r#"
            expandable connector BadBus
                flow Real f;
            end BadBus;
            "#,
            "BadBus",
        );
    }

    /// MLS: "expandable only connects to expandable"
    #[test]
    #[ignore = "expandable connector type check not yet implemented"]
    fn mls_9_4_expandable_to_expandable() {
        expect_failure(
            r#"
            expandable connector Bus
            end Bus;

            connector Pin
                Real v;
                flow Real i;
            end Pin;

            model Test
                Bus bus;
                Pin pin;
            equation
                connect(bus, pin);
            end Test;
            "#,
            "Test",
        );
    }
}

// ============================================================================
// §9.5 OVERCONSTRAINED CONNECTIONS
// ============================================================================

/// MLS §9.5: Overconstrained connection equations
mod overconstrained_connections {
    use super::*;

    /// MLS: "Connections.branch creates a spanning tree edge"
    #[test]
    fn mls_9_5_connections_branch() {
        expect_parse_success(
            r#"
            connector Frame
                Real r[3];
                Real T[3,3];
            end Frame;

            model Body
                Frame frame_a, frame_b;
            equation
                Connections.branch(frame_a, frame_b);
            end Body;
            "#,
        );
    }

    /// MLS: "Connections.root specifies the root of spanning tree"
    #[test]
    fn mls_9_5_connections_root() {
        expect_parse_success(
            r#"
            connector Frame
                Real r[3];
            end Frame;

            model World
                Frame frame;
            equation
                Connections.root(frame);
            end World;
            "#,
        );
    }

    /// MLS: "Connections.potentialRoot marks potential root"
    #[test]
    fn mls_9_5_connections_potential_root() {
        expect_parse_success(
            r#"
            connector Frame
                Real r[3];
            end Frame;

            model Body
                Frame frame;
            equation
                Connections.potentialRoot(frame, priority=1);
            end Body;
            "#,
        );
    }

    /// MLS: "Connections.isRoot queries if connector is root"
    #[test]
    fn mls_9_5_connections_is_root() {
        expect_parse_success(
            r#"
            connector Frame
                Real r[3];
            end Frame;

            model Body
                Frame frame;
                Boolean isRoot;
            equation
                isRoot = Connections.isRoot(frame);
            end Body;
            "#,
        );
    }

    /// MLS: "Connections.rooted queries if rooted"
    #[test]
    fn mls_9_5_connections_rooted() {
        expect_parse_success(
            r#"
            connector Frame
                Real r[3];
            end Frame;

            model Body
                Frame frame_a, frame_b;
                Boolean aRooted;
            equation
                Connections.branch(frame_a, frame_b);
                aRooted = Connections.rooted(frame_a);
            end Body;
            "#,
        );
    }

    /// MLS: "equalityConstraint function for overdetermined systems"
    #[test]
    fn mls_9_5_equality_constraint() {
        expect_parse_success(
            r#"
            connector Orientation
                Real T[3,3];

                function equalityConstraint
                    input Orientation oc1;
                    input Orientation oc2;
                    output Real residue[3];
                algorithm
                    residue := {0, 0, 0};
                end equalityConstraint;
            end Orientation;
            "#,
        );
    }
}
