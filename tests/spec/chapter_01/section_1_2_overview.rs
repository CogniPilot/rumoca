//! MLS §1.2: Overview of Modelica
//!
//! This module tests fundamental Modelica concepts:
//! - Object-oriented modeling
//! - Acausal (equation-based) modeling
//! - Multi-domain modeling
//! - Hybrid modeling (continuous + discrete)
//!
//! Reference: https://specification.modelica.org/master/introduction.html#overview

use crate::spec::{expect_parse_success, expect_success};

// ============================================================================
// §1.2.1 OBJECT-ORIENTED MODELING
// ============================================================================

/// MLS §1.2: Object-oriented features
mod object_oriented {
    use super::*;

    /// MLS: Inheritance using extends
    #[test]
    fn mls_1_2_inheritance() {
        expect_success(
            r#"
            model Base
                Real x = 1;
            equation
            end Base;

            model Derived
                extends Base;
                Real y = 2;
            equation
            end Derived;
            "#,
            "Derived",
        );
    }

    /// MLS: Multiple inheritance
    #[test]
    fn mls_1_2_multiple_inheritance() {
        expect_success(
            r#"
            model A
                Real a = 1;
            equation
            end A;

            model B
                Real b = 2;
            equation
            end B;

            model C
                extends A;
                extends B;
            equation
            end C;
            "#,
            "C",
        );
    }

    /// MLS: Redeclaration of components
    #[test]
    fn mls_1_2_redeclaration() {
        expect_parse_success(
            r#"
            model Base
                replaceable Real x = 1;
            equation
            end Base;

            model Derived
                extends Base(redeclare Real x = 2);
            equation
            end Derived;
            "#,
        );
    }

    /// MLS: Encapsulation with protected
    #[test]
    fn mls_1_2_encapsulation_protected() {
        expect_parse_success(
            r#"
            model Encapsulated
                Real publicVar = 1;
            protected
                Real privateVar = 2;
            equation
            end Encapsulated;
            "#,
        );
    }

    /// MLS: Partial classes cannot be instantiated
    #[test]
    fn mls_1_2_partial_class() {
        expect_parse_success(
            r#"
            partial model PartialBase
                Real x;
            end PartialBase;

            model Complete
                extends PartialBase;
            equation
                x = 1;
            end Complete;
            "#,
        );
    }
}

// ============================================================================
// §1.2.2 ACAUSAL MODELING
// ============================================================================

/// MLS §1.2: Acausal (non-causal) modeling
mod acausal_modeling {
    use super::*;

    /// MLS: Equations can be written in any direction
    #[test]
    fn mls_1_2_acausal_equations() {
        expect_success(
            r#"
            model Acausal
                Real x, y;
            equation
                x + y = 10;  // Neither x nor y is the "output"
                x = 3;
            end Acausal;
            "#,
            "Acausal",
        );
    }

    /// MLS: Connect equations establish constraints
    #[test]
    fn mls_1_2_acausal_connections() {
        expect_success(
            r#"
            connector C
                Real e;
                flow Real f;
            end C;

            model Component
                C port;
            equation
                port.e = 1;
            end Component;

            model System
                Component a;
                Component b;
            equation
                connect(a.port, b.port);
            end System;
            "#,
            "System",
        );
    }

    /// MLS: System of equations solved as a whole
    #[test]
    fn mls_1_2_equation_system() {
        expect_success(
            r#"
            model LinearSystem
                Real x, y, z;
            equation
                2*x + y - z = 8;
                -3*x - y + 2*z = -11;
                -2*x + y + 2*z = -3;
            end LinearSystem;
            "#,
            "LinearSystem",
        );
    }
}

// ============================================================================
// §1.2.3 MULTI-DOMAIN MODELING
// ============================================================================

/// MLS §1.2: Multi-domain physical modeling
mod multi_domain {
    use super::*;

    /// MLS: Electrical domain modeling
    #[test]
    fn mls_1_2_electrical_domain() {
        expect_success(
            r#"
            connector ElectricalPin
                Real v;
                flow Real i;
            end ElectricalPin;

            model Resistor
                ElectricalPin p;
                ElectricalPin n;
                parameter Real R = 1;
            equation
                p.v - n.v = R * p.i;
                p.i + n.i = 0;
            end Resistor;
            "#,
            "Resistor",
        );
    }

    /// MLS: Mechanical domain modeling
    #[test]
    fn mls_1_2_mechanical_domain() {
        expect_success(
            r#"
            connector Flange
                Real phi "Angle";
                flow Real tau "Torque";
            end Flange;

            model Inertia
                Flange flange;
                parameter Real J = 1 "Moment of inertia";
                Real w(start = 0) "Angular velocity";
            equation
                w = der(flange.phi);
                J * der(w) = flange.tau;
            end Inertia;
            "#,
            "Inertia",
        );
    }

    /// MLS: Thermal domain modeling
    #[test]
    fn mls_1_2_thermal_domain() {
        expect_success(
            r#"
            connector HeatPort
                Real T "Temperature";
                flow Real Q_flow "Heat flow";
            end HeatPort;

            model ThermalConductor
                HeatPort port_a;
                HeatPort port_b;
                parameter Real G = 1 "Thermal conductance";
            equation
                port_a.Q_flow = G * (port_a.T - port_b.T);
                port_a.Q_flow + port_b.Q_flow = 0;
            end ThermalConductor;
            "#,
            "ThermalConductor",
        );
    }

    /// MLS: Fluid domain modeling
    #[test]
    fn mls_1_2_fluid_domain() {
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

// ============================================================================
// §1.2.4 HYBRID MODELING
// ============================================================================

/// MLS §1.2: Hybrid (continuous + discrete) modeling
mod hybrid_modeling {
    use super::*;

    /// MLS: Continuous dynamics
    #[test]
    fn mls_1_2_continuous_dynamics() {
        expect_success(
            r#"
            model ContinuousSystem
                Real x(start = 1);
                Real y(start = 0);
            equation
                der(x) = -x;
                der(y) = x;
            end ContinuousSystem;
            "#,
            "ContinuousSystem",
        );
    }

    /// MLS: Discrete events with when
    #[test]
    fn mls_1_2_discrete_events() {
        expect_success(
            r#"
            model DiscreteEvent
                Real x(start = 0);
                discrete Real count(start = 0);
            equation
                der(x) = 1;
                when x > 1 then
                    count = pre(count) + 1;
                end when;
            end DiscreteEvent;
            "#,
            "DiscreteEvent",
        );
    }

    /// MLS: Conditional equations with if
    #[test]
    fn mls_1_2_conditional_equations() {
        expect_success(
            r#"
            model Conditional
                Real x(start = 0);
                Real y;
            equation
                der(x) = 1;
                if x < 5 then
                    y = x;
                else
                    y = 10 - x;
                end if;
            end Conditional;
            "#,
            "Conditional",
        );
    }

    /// MLS: State events with reinit
    #[test]
    fn mls_1_2_state_events() {
        expect_success(
            r#"
            model BouncingBall
                Real h(start = 1) "Height";
                Real v(start = 0) "Velocity";
                parameter Real e = 0.8 "Coefficient of restitution";
                parameter Real g = 9.81 "Gravity";
            equation
                der(h) = v;
                der(v) = -g;
                when h < 0 then
                    reinit(v, -e * pre(v));
                end when;
            end BouncingBall;
            "#,
            "BouncingBall",
        );
    }

    /// MLS: Initial equations for initialization
    #[test]
    fn mls_1_2_initialization() {
        expect_success(
            r#"
            model Initialized
                Real x;
                Real y;
            initial equation
                x = 1;
                y = 2;
            equation
                der(x) = -x;
                der(y) = x;
            end Initialized;
            "#,
            "Initialized",
        );
    }
}

// ============================================================================
// §1.2.5 REUSABLE COMPONENTS
// ============================================================================

/// MLS §1.2: Reusable component libraries
mod reusable_components {
    use super::*;

    /// MLS: Parameterized components for reuse
    #[test]
    fn mls_1_2_parameterized_reuse() {
        expect_success(
            r#"
            model GenericFirstOrder
                parameter Real tau = 1 "Time constant";
                parameter Real k = 1 "Gain";
                Real x(start = 0);
                Real u;
                Real y;
            equation
                tau * der(x) = -x + k * u;
                y = x;
                u = 1;  // Step input
            end GenericFirstOrder;
            "#,
            "GenericFirstOrder",
        );
    }

    /// MLS: Arrays of components
    #[test]
    fn mls_1_2_component_arrays() {
        expect_success(
            r#"
            model Element
                Real x = 1;
            equation
            end Element;

            model ArrayOfComponents
                Element e[3];
                Real sum;
            equation
                sum = e[1].x + e[2].x + e[3].x;
            end ArrayOfComponents;
            "#,
            "ArrayOfComponents",
        );
    }

    /// MLS: Conditional components
    #[test]
    fn mls_1_2_conditional_components() {
        expect_parse_success(
            r#"
            model ConditionalComponent
                parameter Boolean useHeatPort = false;
                Real T = 300 if useHeatPort;
            end ConditionalComponent;
            "#,
        );
    }

    /// MLS: Import for accessing library components
    #[test]
    fn mls_1_2_imports() {
        expect_parse_success(
            r#"
            package MyLib
                model A
                    Real x = 1;
                equation
                end A;
            end MyLib;

            model UseLib
                import MyLib.A;
                A component;
            equation
            end UseLib;
            "#,
        );
    }
}
