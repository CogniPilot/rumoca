//! MLS §9.1: Connector Restrictions and Details
//!
//! Detailed tests for connector definitions including:
//! - Connector restrictions
//! - Connector variability
//! - Nested connectors
//! - Connector arrays
//!
//! Reference: https://specification.modelica.org/master/connectors-connections.html

use crate::spec::{expect_failure, expect_parse_success, expect_success};

// ============================================================================
// §9.1.1 CONNECTOR RESTRICTIONS
// ============================================================================

/// MLS §9.1: Connector definition restrictions
mod connector_restrictions {
    use super::*;

    /// MLS: Connector can have potential (non-flow) variables
    #[test]
    fn mls_9_1_connector_potential_only() {
        expect_parse_success(
            r#"
            connector PotentialOnly
                Real v;
                Real theta;
            end PotentialOnly;
            "#,
        );
    }

    /// MLS: Connector can have flow variables
    #[test]
    fn mls_9_1_connector_with_flow() {
        expect_parse_success(
            r#"
            connector WithFlow
                Real e;
                flow Real f;
            end WithFlow;
            "#,
        );
    }

    /// MLS: Connector can have stream variables
    #[test]
    fn mls_9_1_connector_with_stream() {
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

    /// MLS: Connector can extend another connector
    #[test]
    fn mls_9_1_connector_extends() {
        expect_parse_success(
            r#"
            connector Base
                Real v;
                flow Real i;
            end Base;

            connector Extended
                extends Base;
                Real temperature;
            end Extended;
            "#,
        );
    }

    /// MLS: Connector can have nested connectors
    #[test]
    fn mls_9_1_connector_nested() {
        expect_parse_success(
            r#"
            connector Simple
                Real v;
                flow Real i;
            end Simple;

            connector Composite
                Simple electrical;
                Real temperature;
            end Composite;
            "#,
        );
    }

    /// MLS: Connector can have constant fields
    #[test]
    fn mls_9_1_connector_with_constant() {
        expect_parse_success(
            r#"
            connector WithConstant
                constant Integer n = 3;
                Real v[n];
                flow Real i[n];
            end WithConstant;
            "#,
        );
    }

    /// MLS: Connector can have parameter fields
    #[test]
    fn mls_9_1_connector_with_parameter() {
        expect_parse_success(
            r#"
            connector Parameterized
                parameter Integer n = 3;
                Real v[n];
                flow Real i[n];
            end Parameterized;
            "#,
        );
    }
}

// ============================================================================
// §9.1.2 CONNECTOR ARRAYS
// ============================================================================

/// MLS §9.1: Arrays of connectors
mod connector_arrays {
    use super::*;

    /// MLS: Simple connector array
    #[test]
    fn mls_9_1_connector_array_simple() {
        expect_parse_success(
            r#"
            connector Pin
                Real v;
                flow Real i;
            end Pin;

            model Test
                Pin pins[4];
            end Test;
            "#,
        );
    }

    /// MLS: Connecting array elements
    #[test]
    fn mls_9_1_connect_array_elements() {
        expect_parse_success(
            r#"
            connector C
                Real v;
                flow Real i;
            end C;

            model Test
                C a[3], b[3];
            equation
                connect(a[1], b[1]);
                connect(a[2], b[2]);
                connect(a[3], b[3]);
            end Test;
            "#,
        );
    }

    /// MLS: Connecting connector arrays
    #[test]
    fn mls_9_1_connect_arrays() {
        expect_parse_success(
            r#"
            connector C
                Real v;
                flow Real i;
            end C;

            model Test
                C a[3], b[3];
            equation
                connect(a, b);
            end Test;
            "#,
        );
    }

    /// MLS: For-loop connector array connection
    #[test]
    fn mls_9_1_connect_for_loop() {
        expect_parse_success(
            r#"
            connector C
                Real v;
                flow Real i;
            end C;

            model Test
                C a[5], b[5];
            equation
                for i in 1:5 loop
                    connect(a[i], b[i]);
                end for;
            end Test;
            "#,
        );
    }

    /// MLS: 2D connector array
    #[test]
    fn mls_9_1_connector_2d_array() {
        expect_parse_success(
            r#"
            connector C
                Real v;
                flow Real i;
            end C;

            model Grid
                C nodes[3, 3];
            end Grid;
            "#,
        );
    }
}

// ============================================================================
// §9.1.3 CONNECTOR COMPONENT ACCESS
// ============================================================================

/// MLS §9.1: Connector component access
mod connector_access {
    use super::*;

    /// MLS: Access connector in component
    #[test]
    fn mls_9_1_access_in_component() {
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

    /// MLS: Hierarchical connector access
    #[test]
    fn mls_9_1_hierarchical_access() {
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

            model Circuit
                Resistor r1(R = 100);
                Resistor r2(R = 200);
            equation
                r1.p.v = 5;
                connect(r1.n, r2.p);
                r2.n.v = 0;
            end Circuit;
            "#,
            "Circuit",
        );
    }

    /// MLS: Nested connector access
    #[test]
    fn mls_9_1_nested_connector_access() {
        expect_parse_success(
            r#"
            connector Simple
                Real v;
                flow Real i;
            end Simple;

            connector Complex
                Simple electrical;
                Real thermal;
            end Complex;

            model Test
                Complex port;
            equation
                port.electrical.v = 1;
                port.thermal = 300;
            end Test;
            "#,
        );
    }
}

// ============================================================================
// §9.1.4 EXPANDABLE CONNECTORS
// ============================================================================

/// MLS §9.1: Expandable connectors
mod expandable_connectors {
    use super::*;

    /// MLS: Basic expandable connector
    #[test]
    fn mls_9_1_expandable_basic() {
        expect_parse_success(
            r#"
            expandable connector ControlBus
            end ControlBus;

            model Controller
                ControlBus bus;
            end Controller;
            "#,
        );
    }

    /// MLS: Expandable connector with predefined elements
    #[test]
    fn mls_9_1_expandable_with_elements() {
        expect_parse_success(
            r#"
            expandable connector SignalBus
                Real speed;
                Boolean enable;
            end SignalBus;

            model Test
                SignalBus bus;
            end Test;
            "#,
        );
    }

    /// MLS: Connect to expandable connector
    #[test]
    fn mls_9_1_connect_expandable() {
        expect_parse_success(
            r#"
            expandable connector Bus
            end Bus;

            connector Signal
                Real value;
            end Signal;

            model Test
                Bus bus;
                Signal s;
            equation
                connect(bus.signal, s);
            end Test;
            "#,
        );
    }

    /// MLS: Multiple expandable connectors connected
    #[test]
    fn mls_9_1_expandable_multiple() {
        expect_parse_success(
            r#"
            expandable connector Bus
            end Bus;

            model Test
                Bus bus1, bus2, bus3;
            equation
                connect(bus1, bus2);
                connect(bus2, bus3);
            end Test;
            "#,
        );
    }
}

// ============================================================================
// NEGATIVE TESTS - Connector errors
// ============================================================================

/// Connector error cases
mod connector_errors {
    use super::*;

    /// Error: Connect incompatible connectors
    #[test]
    fn error_incompatible_connectors() {
        expect_failure(
            r#"
            connector A
                Real v;
                flow Real i;
            end A;

            connector B
                Real voltage;
                flow Real current;
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

    /// Error: Connect connector to non-connector
    #[test]
    fn error_connect_non_connector() {
        expect_failure(
            r#"
            connector C
                Real v;
                flow Real i;
            end C;

            model Test
                C c;
                Real x;
            equation
                connect(c, x);
            end Test;
            "#,
            "Test",
        );
    }

    /// Error: Array dimension mismatch in connection
    #[test]
    fn error_array_dimension_mismatch() {
        expect_failure(
            r#"
            connector C
                Real v;
                flow Real i;
            end C;

            model Test
                C a[3], b[4];
            equation
                connect(a, b);
            end Test;
            "#,
            "Test",
        );
    }
}
