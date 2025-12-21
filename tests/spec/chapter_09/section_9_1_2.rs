//! MLS §9.1-9.2: Connect-Equations, Connectors, and Connection Equations
//!
//! Tests for:
//! - §9.1: Connector definitions and connect equations
//! - §9.2: Generation of connection equations
//!
//! Reference: https://specification.modelica.org/master/connectors-connections.html

use crate::spec::{expect_parse_success, expect_success};

// ============================================================================
// §9.1 CONNECT-EQUATIONS AND CONNECTORS
// ============================================================================

/// MLS §9.1: Connector definitions
mod section_9_1_connectors {
    use super::*;

    #[test]
    fn mls_9_1_connector_basic() {
        expect_parse_success(
            r#"
            connector Pin
                Real v;
                flow Real i;
            end Pin;
            "#,
        );
    }

    #[test]
    fn mls_9_1_connector_multiple_flow() {
        expect_parse_success(
            r#"
            connector MultiFlow
                Real p;
                flow Real m_flow;
                flow Real H_flow;
            end MultiFlow;
            "#,
        );
    }

    #[test]
    fn mls_9_1_connector_expandable() {
        expect_parse_success("expandable connector Bus end Bus;");
    }

    #[test]
    fn mls_9_1_connector_with_record() {
        expect_parse_success(
            r#"
            connector ComplexPin
                Real v_re;
                Real v_im;
                flow Real i_re;
                flow Real i_im;
            end ComplexPin;
            "#,
        );
    }

    #[test]
    fn mls_9_1_connector_with_array() {
        expect_parse_success(
            r#"
            connector MultiPin
                Real v[3];
                flow Real i[3];
            end MultiPin;
            "#,
        );
    }
}

/// MLS §9.1: Connect equations
mod section_9_1_connect {
    use super::*;

    #[test]
    fn mls_9_1_connect_simple() {
        expect_success(
            r#"
            connector C Real v; flow Real i; end C;
            model M C c1; C c2; equation connect(c1, c2); end M;
            "#,
            "M",
        );
    }

    #[test]
    fn mls_9_1_connect_chain() {
        expect_success(
            r#"
            connector C Real v; flow Real i; end C;
            model Comp C a; C b; equation a.v = 1; a.i + b.i = 0; b.v = a.v; end Comp;
            model Chain
                Comp c1;
                Comp c2;
                Comp c3;
            equation
                connect(c1.b, c2.a);
                connect(c2.b, c3.a);
            end Chain;
            "#,
            "Chain",
        );
    }

    #[test]
    fn mls_9_1_connect_branch() {
        expect_success(
            r#"
            connector C Real v; flow Real i; end C;
            model Comp C a; C b; equation a.v = 1; a.i + b.i = 0; b.v = a.v; end Comp;
            model Branch
                Comp c1;
                Comp c2;
                Comp c3;
            equation
                connect(c1.b, c2.a);
                connect(c1.b, c3.a);
            end Branch;
            "#,
            "Branch",
        );
    }
}

// ============================================================================
// §9.2 GENERATION OF CONNECTION EQUATIONS
// ============================================================================

/// MLS §9.2: Connection equation generation
mod section_9_2_connection_equations {
    use super::*;

    #[test]
    fn mls_9_2_potential_equalization() {
        // Test that potential variables are equalized
        expect_success(
            r#"
            connector C Real v; flow Real i; end C;
            model Test
                C c1;
                C c2;
            equation
                c1.v = 1;
                c1.i = 0.1;
                connect(c1, c2);
            end Test;
            "#,
            "Test",
        );
    }

    #[test]
    fn mls_9_2_flow_summation() {
        // Test that flow variables sum to zero
        expect_success(
            r#"
            connector C Real v; flow Real i; end C;
            model Comp C a; C b;
            equation
                a.v - b.v = 0.1 * a.i;
                a.i + b.i = 0;
            end Comp;
            model Test
                Comp r1;
                Comp r2;
            equation
                r1.a.v = 1;
                r2.b.v = 0;
                connect(r1.b, r2.a);
            end Test;
            "#,
            "Test",
        );
    }
}

// ============================================================================
// STREAM CONNECTORS (MLS §15)
// ============================================================================

/// Stream connector tests (preview of Chapter 15)
mod stream_connectors {
    use super::*;

    #[test]
    fn stream_connector_basic() {
        expect_parse_success(
            r#"
            connector FluidPort
                Real p "Pressure";
                flow Real m_flow "Mass flow rate";
                stream Real h_outflow "Specific enthalpy";
            end FluidPort;
            "#,
        );
    }
}
