//! Connector Examples: Electrical, Mechanical, and Thermal
//!
//! Comprehensive examples of connector usage across domains.
//!
//! Reference: https://specification.modelica.org/master/connectors-connections.html

use crate::spec::{expect_parse_success, expect_success};

// ============================================================================
// ELECTRICAL EXAMPLES
// ============================================================================

/// Electrical circuit examples using connectors
mod electrical_examples {
    use super::*;

    #[test]
    fn electrical_resistor() {
        expect_success(
            r#"
            connector Pin Real v; flow Real i; end Pin;
            model Resistor
                Pin p;
                Pin n;
                parameter Real R = 1;
            equation
                p.v - n.v = R * p.i;
                p.i + n.i = 0;
            end Resistor;
            "#,
            "Resistor",
        );
    }

    #[test]
    fn electrical_capacitor() {
        expect_success(
            r#"
            connector Pin Real v; flow Real i; end Pin;
            model Capacitor
                Pin p;
                Pin n;
                parameter Real C = 1;
                Real v(start = 0);
            equation
                v = p.v - n.v;
                C * der(v) = p.i;
                p.i + n.i = 0;
            end Capacitor;
            "#,
            "Capacitor",
        );
    }

    #[test]
    fn electrical_inductor() {
        expect_success(
            r#"
            connector Pin Real v; flow Real i; end Pin;
            model Inductor
                Pin p;
                Pin n;
                parameter Real L = 1;
                Real i(start = 0);
            equation
                i = p.i;
                L * der(i) = p.v - n.v;
                p.i + n.i = 0;
            end Inductor;
            "#,
            "Inductor",
        );
    }

    #[test]
    fn electrical_ground() {
        expect_success(
            r#"
            connector Pin Real v; flow Real i; end Pin;
            model Ground
                Pin p;
            equation
                p.v = 0;
            end Ground;
            "#,
            "Ground",
        );
    }

    #[test]
    fn electrical_voltage_source() {
        expect_success(
            r#"
            connector Pin Real v; flow Real i; end Pin;
            model VoltageSource
                Pin p;
                Pin n;
                parameter Real V = 1;
            equation
                p.v - n.v = V;
                p.i + n.i = 0;
            end VoltageSource;
            "#,
            "VoltageSource",
        );
    }

    #[test]
    fn electrical_rc_circuit() {
        expect_success(
            r#"
            connector Pin Real v; flow Real i; end Pin;

            model Resistor
                Pin p;
                Pin n;
                parameter Real R = 1;
            equation
                p.v - n.v = R * p.i;
                p.i + n.i = 0;
            end Resistor;

            model Capacitor
                Pin p;
                Pin n;
                parameter Real C = 1;
            equation
                C * der(p.v - n.v) = p.i;
                p.i + n.i = 0;
            end Capacitor;

            model Ground
                Pin p;
            equation
                p.v = 0;
            end Ground;

            model RCCircuit
                Resistor R(R = 1000);
                Capacitor C(C = 1e-6);
                Ground gnd;
            equation
                R.p.v = 5;
                connect(R.n, C.p);
                connect(C.n, gnd.p);
            end RCCircuit;
            "#,
            "RCCircuit",
        );
    }
}

// ============================================================================
// MECHANICAL EXAMPLES
// ============================================================================

/// Mechanical system examples using connectors
mod mechanical_examples {
    use super::*;

    #[test]
    fn mechanical_flange() {
        expect_parse_success(
            r#"
            connector Flange
                Real s "Position";
                flow Real f "Force";
            end Flange;
            "#,
        );
    }

    #[test]
    fn mechanical_spring() {
        expect_success(
            r#"
            connector Flange Real s; flow Real f; end Flange;
            model Spring
                Flange a;
                Flange b;
                parameter Real c = 1 "Spring constant";
            equation
                a.f = c * (a.s - b.s);
                a.f + b.f = 0;
            end Spring;
            "#,
            "Spring",
        );
    }

    #[test]
    fn mechanical_damper() {
        expect_success(
            r#"
            connector Flange Real s; flow Real f; end Flange;
            model Damper
                Flange a;
                Flange b;
                parameter Real d = 1 "Damping coefficient";
            equation
                a.f = d * der(a.s - b.s);
                a.f + b.f = 0;
            end Damper;
            "#,
            "Damper",
        );
    }

    #[test]
    fn mechanical_mass() {
        expect_success(
            r#"
            connector Flange Real s; flow Real f; end Flange;
            model Mass
                Flange flange;
                parameter Real m = 1 "Mass";
                Real v(start = 0) "Velocity";
            equation
                v = der(flange.s);
                m * der(v) = flange.f;
            end Mass;
            "#,
            "Mass",
        );
    }

    #[test]
    fn mechanical_fixed() {
        expect_success(
            r#"
            connector Flange Real s; flow Real f; end Flange;
            model Fixed
                Flange flange;
                parameter Real s0 = 0 "Fixed position";
            equation
                flange.s = s0;
            end Fixed;
            "#,
            "Fixed",
        );
    }
}

// ============================================================================
// THERMAL EXAMPLES
// ============================================================================

/// Thermal system examples using connectors
mod thermal_examples {
    use super::*;

    #[test]
    fn thermal_heatport() {
        expect_parse_success(
            r#"
            connector HeatPort
                Real T "Temperature";
                flow Real Q_flow "Heat flow";
            end HeatPort;
            "#,
        );
    }

    #[test]
    fn thermal_conductor() {
        expect_success(
            r#"
            connector HeatPort Real T; flow Real Q_flow; end HeatPort;
            model ThermalConductor
                HeatPort a;
                HeatPort b;
                parameter Real G = 1 "Thermal conductance";
            equation
                a.Q_flow = G * (a.T - b.T);
                a.Q_flow + b.Q_flow = 0;
            end ThermalConductor;
            "#,
            "ThermalConductor",
        );
    }

    #[test]
    fn thermal_capacitor() {
        expect_success(
            r#"
            connector HeatPort Real T; flow Real Q_flow; end HeatPort;
            model HeatCapacitor
                HeatPort port;
                parameter Real C = 1 "Heat capacity";
            equation
                C * der(port.T) = port.Q_flow;
            end HeatCapacitor;
            "#,
            "HeatCapacitor",
        );
    }

    #[test]
    fn thermal_fixed_temperature() {
        expect_success(
            r#"
            connector HeatPort Real T; flow Real Q_flow; end HeatPort;
            model FixedTemperature
                HeatPort port;
                parameter Real T = 293.15 "Fixed temperature";
            equation
                port.T = T;
            end FixedTemperature;
            "#,
            "FixedTemperature",
        );
    }
}
