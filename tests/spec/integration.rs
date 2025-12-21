//! Integration Tests
//!
//! This module provides comprehensive integration tests that combine multiple
//! language features to validate real-world modeling scenarios.

use super::{expect_balanced, expect_success};

// ============================================================================
// CLASSIC DYNAMICAL SYSTEMS
// ============================================================================

/// Classic mathematical and physical systems
mod dynamical_systems {
    use super::*;

    #[test]
    fn integration_harmonic_oscillator() {
        expect_success(
            r#"
            model HarmonicOscillator
                "Simple harmonic oscillator: m*x'' + k*x = 0"
                parameter Real m = 1.0 "Mass";
                parameter Real k = 1.0 "Spring constant";
                parameter Real c = 0.1 "Damping coefficient";
                Real x(start = 1.0) "Displacement";
                Real v(start = 0.0) "Velocity";
            equation
                der(x) = v;
                m * der(v) = -k * x - c * v;
            end HarmonicOscillator;
            "#,
            "HarmonicOscillator",
        );
    }

    #[test]
    fn integration_van_der_pol() {
        expect_success(
            r#"
            model VanDerPol
                "Van der Pol oscillator: x'' - mu*(1-x^2)*x' + x = 0"
                parameter Real mu = 1.0 "Nonlinearity parameter";
                Real x(start = 2) "Position";
                Real y(start = 0) "Velocity";
            equation
                der(x) = y;
                der(y) = mu * (1 - x^2) * y - x;
            end VanDerPol;
            "#,
            "VanDerPol",
        );
    }

    #[test]
    fn integration_lorenz() {
        expect_success(
            r#"
            model Lorenz
                "Lorenz attractor (chaotic system)"
                parameter Real sigma = 10;
                parameter Real rho = 28;
                parameter Real beta = 8/3;
                Real x(start = 1);
                Real y(start = 1);
                Real z(start = 1);
            equation
                der(x) = sigma * (y - x);
                der(y) = x * (rho - z) - y;
                der(z) = x * y - beta * z;
            end Lorenz;
            "#,
            "Lorenz",
        );
    }

    #[test]
    fn integration_lotka_volterra() {
        expect_success(
            r#"
            model LotkaVolterra
                "Predator-prey model"
                parameter Real alpha = 0.1 "Prey birth rate";
                parameter Real beta = 0.02 "Predation rate";
                parameter Real gamma = 0.4 "Predator death rate";
                parameter Real delta = 0.02 "Predator reproduction rate";
                Real prey(start = 100) "Prey population";
                Real predator(start = 20) "Predator population";
            equation
                der(prey) = alpha * prey - beta * prey * predator;
                der(predator) = delta * prey * predator - gamma * predator;
            end LotkaVolterra;
            "#,
            "LotkaVolterra",
        );
    }

    #[test]
    fn integration_pendulum() {
        expect_success(
            r#"
            model Pendulum
                "Simple pendulum: theta'' + (g/L)*sin(theta) = 0"
                parameter Real L = 1 "Length";
                parameter Real g = 9.81 "Gravity";
                Real theta(start = 0.1) "Angle from vertical";
                Real omega(start = 0) "Angular velocity";
            equation
                der(theta) = omega;
                der(omega) = -(g/L) * sin(theta);
            end Pendulum;
            "#,
            "Pendulum",
        );
    }

    #[test]
    fn integration_double_integrator() {
        expect_success(
            r#"
            model DoubleIntegrator
                "Double integrator: x'' = u"
                parameter Real u = 1 "Input acceleration";
                Real x(start = 0) "Position";
                Real v(start = 0) "Velocity";
            equation
                der(x) = v;
                der(v) = u;
            end DoubleIntegrator;
            "#,
            "DoubleIntegrator",
        );
    }
}

// ============================================================================
// EVENT-DRIVEN SYSTEMS
// ============================================================================

/// Systems with discrete events
mod event_systems {
    use super::*;

    #[test]
    fn integration_bouncing_ball() {
        expect_success(
            r#"
            model BouncingBall
                "Ball bouncing on ground with energy loss"
                parameter Real g = 9.81 "Gravity";
                parameter Real e = 0.8 "Coefficient of restitution";
                Real h(start = 1) "Height";
                Real v(start = 0) "Velocity";
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

    #[test]
    fn integration_counter() {
        expect_success(
            r#"
            model Counter
                "Event counter triggered by time"
                Real x(start = 0);
                discrete Integer count(start = 0);
            equation
                der(x) = 1;
                when x > 1 then
                    count = pre(count) + 1;
                    reinit(x, 0);
                end when;
            end Counter;
            "#,
            "Counter",
        );
    }

    #[test]
    fn integration_sampled_system() {
        expect_success(
            r#"
            model SampledSystem
                "Continuous system with sampled measurements"
                Real x(start = 0);
                discrete Real x_sampled(start = 0);
            equation
                der(x) = 1;
                when sample(0, 0.1) then
                    x_sampled = x;
                end when;
            end SampledSystem;
            "#,
            "SampledSystem",
        );
    }

    #[test]
    fn integration_hysteresis() {
        expect_success(
            r#"
            model Hysteresis
                "Simple hysteresis model"
                Real x(start = 0);
                discrete Boolean state(start = false);
            equation
                der(x) = if time < 5 then 1 else -1;
                when x > 2 then
                    state = true;
                elsewhen x < 1 then
                    state = false;
                end when;
            end Hysteresis;
            "#,
            "Hysteresis",
        );
    }
}

// ============================================================================
// ELECTRICAL CIRCUITS
// ============================================================================

/// Electrical circuit examples
mod electrical_systems {
    use super::*;

    #[test]
    fn integration_rc_circuit() {
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

    #[test]
    fn integration_rlc_circuit() {
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

            model RLCCircuit
                Resistor R(R = 100);
                Inductor L(L = 0.1);
                Capacitor C(C = 1e-6);
                Ground gnd;
            equation
                R.p.v = 5;
                connect(R.n, L.p);
                connect(L.n, C.p);
                connect(C.n, gnd.p);
            end RLCCircuit;
            "#,
            "RLCCircuit",
        );
    }
}

// ============================================================================
// THERMAL SYSTEMS
// ============================================================================

/// Thermal system examples
mod thermal_systems {
    use super::*;

    #[test]
    fn integration_heat_rod() {
        expect_success(
            r#"
            model HeatRod
                "1D heat conduction along a rod"
                parameter Integer n = 5 "Number of nodes";
                parameter Real k = 1.0 "Thermal conductivity";
                parameter Real c = 1.0 "Heat capacity";
                parameter Real dx = 0.1 "Node spacing";
                Real T[n](each start = 0) "Temperature at nodes";
            equation
                // Left boundary: fixed temperature
                c * der(T[1]) = k * (1.0 - T[1]) / dx + k * (T[2] - T[1]) / dx;
                // Interior nodes
                for i in 2:n-1 loop
                    c * der(T[i]) = k * (T[i-1] - T[i]) / dx + k * (T[i+1] - T[i]) / dx;
                end for;
                // Right boundary: insulated
                c * der(T[n]) = k * (T[n-1] - T[n]) / dx;
            end HeatRod;
            "#,
            "HeatRod",
        );
    }
}

// ============================================================================
// CONTROL SYSTEMS
// ============================================================================

/// Control system examples
mod control_systems {
    use super::*;

    #[test]
    fn integration_pid_controller() {
        expect_success(
            r#"
            model PIDController
                "PID controller with first-order process"
                parameter Real Kp = 1 "Proportional gain";
                parameter Real Ki = 0.1 "Integral gain";
                parameter Real Kd = 0.01 "Derivative gain";
                parameter Real setpoint = 1 "Reference value";

                Real err "Error signal";
                Real integ(start = 0) "Integral of error";
                Real deriv "Derivative of error";
                Real u "Control signal";
                Real process(start = 0) "Process variable";
            equation
                err = setpoint - process;
                der(integ) = err;
                deriv = der(err);
                u = Kp * err + Ki * integ + Kd * deriv;
                // Simple first-order process
                der(process) = u - process;
            end PIDController;
            "#,
            "PIDController",
        );
    }

    #[test]
    fn integration_first_order_system() {
        expect_success(
            r#"
            model FirstOrderSystem
                "First-order system: tau*y' + y = K*u"
                parameter Real K = 1 "Gain";
                parameter Real tau = 1 "Time constant";
                parameter Real u = 1 "Input";
                Real y(start = 0) "Output";
            equation
                tau * der(y) + y = K * u;
            end FirstOrderSystem;
            "#,
            "FirstOrderSystem",
        );
    }

    #[test]
    fn integration_second_order_system() {
        expect_success(
            r#"
            model SecondOrderSystem
                "Second-order system: y'' + 2*zeta*wn*y' + wn^2*y = wn^2*u"
                parameter Real wn = 1 "Natural frequency";
                parameter Real zeta = 0.5 "Damping ratio";
                parameter Real u = 1 "Input";
                Real y(start = 0) "Output";
                Real yd(start = 0) "Output derivative";
            equation
                der(y) = yd;
                der(yd) = wn^2 * (u - y) - 2 * zeta * wn * yd;
            end SecondOrderSystem;
            "#,
            "SecondOrderSystem",
        );
    }
}

// ============================================================================
// INHERITANCE AND COMPOSITION
// ============================================================================

/// Tests combining inheritance and composition patterns
mod composition {
    use super::*;

    #[test]
    fn integration_component_hierarchy() {
        expect_success(
            r#"
            model BaseODE
                Real x(start = 0);
            equation
                der(x) = 1;
            end BaseODE;

            model ExtendedODE
                extends BaseODE;
                Real y;
            equation
                y = x * 2;
            end ExtendedODE;

            model System
                ExtendedODE ode;
            end System;
            "#,
            "System",
        );
    }

    #[test]
    fn integration_parameterized_components() {
        expect_success(
            r#"
            model Decay
                parameter Real k = 1;
                Real x(start = 1);
            equation
                der(x) = -k * x;
            end Decay;

            model TwoDecay
                Decay fast(k = 10);
                Decay slow(k = 0.1);
            end TwoDecay;
            "#,
            "TwoDecay",
        );
    }
}

// ============================================================================
// BALANCED MODEL TESTS
// ============================================================================

/// Verify model balancing
mod balanced_models {
    use super::*;

    #[test]
    fn balanced_single_equation() {
        expect_balanced("model Test Real x; equation x = 1; end Test;", "Test");
    }

    #[test]
    fn balanced_ode() {
        expect_balanced(
            "model Test Real x(start=1); equation der(x) = -x; end Test;",
            "Test",
        );
    }

    #[test]
    fn balanced_system() {
        expect_balanced(
            r#"
            model Test
                Real x;
                Real y;
                Real z;
            equation
                x = 1;
                y = x + 1;
                z = y + 1;
            end Test;
            "#,
            "Test",
        );
    }

    #[test]
    fn balanced_for_loop() {
        expect_balanced(
            r#"
            model Test
                Real x[5];
            equation
                for i in 1:5 loop
                    x[i] = i;
                end for;
            end Test;
            "#,
            "Test",
        );
    }

    #[test]
    fn balanced_coupled_odes() {
        expect_balanced(
            r#"
            model Test
                Real x(start = 0);
                Real y(start = 1);
            equation
                der(x) = y;
                der(y) = -x;
            end Test;
            "#,
            "Test",
        );
    }
}

// ============================================================================
// PHASE 10: INFRASTRUCTURE TESTS
// ============================================================================

/// Tests for Phase 10 enhanced test infrastructure
mod infrastructure_tests {
    use crate::spec::{
        expect_balanced_with_count, expect_counts, expect_failure_with_message,
        expect_multi_file_failure, expect_multi_file_success, expect_parse_failure_with_message,
    };

    // ------------------------------------------------------------------------
    // Balance Verification with Counts
    // ------------------------------------------------------------------------

    /// Test: Simple model with 1 equation, 1 unknown
    #[test]
    fn infra_balanced_1_1() {
        expect_balanced_with_count(
            r#"
            model Simple
                Real x;
            equation
                x = 1;
            end Simple;
            "#,
            "Simple",
            1,
            1,
        );
    }

    /// Test: Model with 2 equations, 2 unknowns
    #[test]
    fn infra_balanced_2_2() {
        expect_balanced_with_count(
            r#"
            model TwoVar
                Real x;
                Real y;
            equation
                x = 1;
                y = x + 1;
            end TwoVar;
            "#,
            "TwoVar",
            2,
            2,
        );
    }

    /// Test: Model with differential equation
    #[test]
    fn infra_differential_equation() {
        expect_balanced_with_count(
            r#"
            model Integrator
                Real x(start = 0);
            equation
                der(x) = 1;
            end Integrator;
            "#,
            "Integrator",
            1,
            1,
        );
    }

    /// Test: Model with array (3 equations, 3 unknowns)
    #[test]
    fn infra_array_balanced() {
        expect_balanced_with_count(
            r#"
            model ArrayModel
                Real x[3];
            equation
                x = {1, 2, 3};
            end ArrayModel;
            "#,
            "ArrayModel",
            3,
            3,
        );
    }

    /// Test: expect_counts without balance requirement
    #[test]
    fn infra_counts() {
        expect_counts(
            r#"
            model Counted
                Real a;
                Real b;
            equation
                a = 1;
                b = 2;
            end Counted;
            "#,
            "Counted",
            2,
            2,
        );
    }

    // ------------------------------------------------------------------------
    // Error Message Validation
    // ------------------------------------------------------------------------

    /// Test: Undefined variable error message
    #[test]
    fn infra_undefined_variable_message() {
        expect_failure_with_message(
            r#"
            model UndefinedVar
                Real x;
            equation
                x = undefined_var;
            end UndefinedVar;
            "#,
            "UndefinedVar",
            "undefined_var",
        );
    }

    /// Test: Parse error message contains location info
    #[test]
    fn infra_parse_error_message() {
        expect_parse_failure_with_message(
            r#"
            model BadSyntax
                Real x
            end BadSyntax;
            "#,
            "Syntax",
        );
    }

    // ------------------------------------------------------------------------
    // Multi-File Compilation
    // ------------------------------------------------------------------------

    /// Test: Two files with cross-reference
    #[test]
    fn infra_two_file_import() {
        expect_multi_file_success(
            &[
                (
                    "Constants.mo",
                    r#"
                    package Constants
                        constant Real pi = 3.14159;
                        constant Real e = 2.71828;
                    end Constants;
                    "#,
                ),
                (
                    "Circle.mo",
                    r#"
                    model Circle
                        import Constants.pi;
                        parameter Real r = 1.0;
                        Real area;
                    equation
                        area = pi * r^2;
                    end Circle;
                    "#,
                ),
            ],
            "Circle",
        );
    }

    /// Test: Three files with chain dependency
    #[test]
    fn infra_chain_dependency() {
        expect_multi_file_success(
            &[
                (
                    "Base.mo",
                    r#"
                    package Base
                        constant Real x = 1.0;
                    end Base;
                    "#,
                ),
                (
                    "Middle.mo",
                    r#"
                    package Middle
                        import Base.x;
                        constant Real y = x + 1.0;
                    end Middle;
                    "#,
                ),
                (
                    "Top.mo",
                    r#"
                    model Top
                        import Middle.y;
                        Real z;
                    equation
                        z = y;
                    end Top;
                    "#,
                ),
            ],
            "Top",
        );
    }

    /// Test: Expect failure with undefined reference
    #[test]
    fn infra_undefined_reference_fails() {
        expect_multi_file_failure(
            &[(
                "BadRef.mo",
                r#"
                model BadRef
                    Real x = UndefinedPackage.value;
                equation
                end BadRef;
                "#,
            )],
            "BadRef",
        );
    }

    /// Test: Package with nested model
    #[test]
    fn infra_package_with_model() {
        expect_multi_file_success(
            &[(
                "MyPackage.mo",
                r#"
                package MyPackage
                    model Scaler
                        parameter Real factor = 2.0;
                        Real x;
                        Real y;
                    equation
                        y = factor * x;
                        x = 1;
                    end Scaler;
                end MyPackage;
                "#,
            )],
            "MyPackage.Scaler",
        );
    }
}
