//! MLS §8.1-8.3: Equation Categories and Equation Sections
//!
//! Tests for:
//! - §8.1: Different categories of equations
//! - §8.3.1: For-equations
//! - §8.3.2: If-equations
//! - §8.3.3: When-equations
//! - §8.3.4: Assert equations
//! - §8.3.5: Connect equations
//!
//! Reference: https://specification.modelica.org/master/equations.html

use crate::spec::expect_success;

// ============================================================================
// §8.1 EQUATION CATEGORIES
// ============================================================================

/// MLS §8.1: Different categories of equations
mod section_8_1_categories {
    use super::*;

    #[test]
    fn mls_8_1_simple_equation() {
        expect_success("model Test Real x; equation x = 1; end Test;", "Test");
    }

    #[test]
    fn mls_8_1_differential_equation() {
        expect_success(
            "model Test Real x(start=1); equation der(x) = -x; end Test;",
            "Test",
        );
    }

    #[test]
    fn mls_8_1_algebraic_equation() {
        expect_success(
            "model Test Real x; Real y; equation x + y = 10; x - y = 2; end Test;",
            "Test",
        );
    }

    #[test]
    fn mls_8_1_initial_equation() {
        expect_success(
            r#"
            model Test
                Real x;
            initial equation
                x = 0;
            equation
                der(x) = 1;
            end Test;
            "#,
            "Test",
        );
    }

    #[test]
    fn mls_8_1_connect_equation() {
        expect_success(
            r#"
            connector C Real v; flow Real i; end C;
            model A C c1; C c2; equation connect(c1, c2); end A;
            "#,
            "A",
        );
    }
}

// ============================================================================
// §8.3.1 FOR-EQUATIONS
// ============================================================================

/// MLS §8.3.1: For-equations
mod section_8_3_1_for_equations {
    use super::*;

    #[test]
    fn mls_8_3_1_for_basic() {
        expect_success(
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
    fn mls_8_3_1_for_with_step() {
        expect_success(
            r#"
            model Test
                Real x[3];
            equation
                for i in 1:2:5 loop
                    x[div(i+1,2)] = i;
                end for;
            end Test;
            "#,
            "Test",
        );
    }

    #[test]
    fn mls_8_3_1_for_nested() {
        expect_success(
            r#"
            model Test
                Real A[3,3];
            equation
                for i in 1:3 loop
                    for j in 1:3 loop
                        A[i,j] = i * j;
                    end for;
                end for;
            end Test;
            "#,
            "Test",
        );
    }

    #[test]
    fn mls_8_3_1_for_differential() {
        expect_success(
            r#"
            model Test
                parameter Integer n = 3;
                Real x[n](each start = 0);
            equation
                for i in 1:n loop
                    der(x[i]) = i;
                end for;
            end Test;
            "#,
            "Test",
        );
    }

    #[test]
    fn mls_8_3_1_for_with_expression() {
        expect_success(
            r#"
            model Test
                parameter Integer n = 5;
                Real x[n];
            equation
                for i in 1:n loop
                    x[i] = sin(i * 0.1);
                end for;
            end Test;
            "#,
            "Test",
        );
    }
}

// ============================================================================
// §8.3.2 IF-EQUATIONS
// ============================================================================

/// MLS §8.3.2: If-equations
mod section_8_3_2_if_equations {
    use super::*;

    #[test]
    fn mls_8_3_2_if_basic() {
        expect_success(
            r#"
            model Test
                parameter Boolean condition = true;
                Real x;
            equation
                if condition then
                    x = 1;
                else
                    x = 0;
                end if;
            end Test;
            "#,
            "Test",
        );
    }

    #[test]
    fn mls_8_3_2_if_elseif() {
        expect_success(
            r#"
            model Test
                parameter Integer n = 1;
                Real x;
            equation
                if n == 0 then
                    x = 0;
                elseif n == 1 then
                    x = 1;
                elseif n == 2 then
                    x = 4;
                else
                    x = n * n;
                end if;
            end Test;
            "#,
            "Test",
        );
    }

    #[test]
    fn mls_8_3_2_if_multiple_elseif() {
        expect_success(
            r#"
            model Test
                parameter Integer n = 3;
                Real x;
            equation
                if n == 0 then
                    x = 0;
                elseif n == 1 then
                    x = 1;
                elseif n == 2 then
                    x = 2;
                elseif n == 3 then
                    x = 3;
                else
                    x = -1;
                end if;
            end Test;
            "#,
            "Test",
        );
    }

    #[test]
    fn mls_8_3_2_if_balanced() {
        expect_success(
            r#"
            model Test
                parameter Boolean b = true;
                Real x;
                Real y;
            equation
                if b then
                    x = 1;
                    y = 2;
                else
                    x = 3;
                    y = 4;
                end if;
            end Test;
            "#,
            "Test",
        );
    }
}

// ============================================================================
// §8.3.3 WHEN-EQUATIONS
// ============================================================================

/// MLS §8.3.3: When-equations
mod section_8_3_3_when_equations {
    use super::*;

    #[test]
    fn mls_8_3_3_when_basic() {
        expect_success(
            r#"
            model Test
                Real x(start = 0);
                discrete Real triggered(start = 0);
            equation
                der(x) = 1;
                when x > 1 then
                    triggered = 1;
                end when;
            end Test;
            "#,
            "Test",
        );
    }

    #[test]
    fn mls_8_3_3_when_elsewhen() {
        expect_success(
            r#"
            model Test
                Real x(start = 0);
                discrete Integer state(start = 0);
            equation
                der(x) = 1;
                when x > 1 then
                    state = 1;
                elsewhen x > 2 then
                    state = 2;
                end when;
            end Test;
            "#,
            "Test",
        );
    }

    #[test]
    fn mls_8_3_3_when_with_pre() {
        expect_success(
            r#"
            model Test
                Real x(start = 0);
                discrete Integer count(start = 0);
            equation
                der(x) = 1;
                when x > 1 then
                    count = pre(count) + 1;
                end when;
            end Test;
            "#,
            "Test",
        );
    }

    #[test]
    fn mls_8_3_3_when_sample() {
        expect_success(
            r#"
            model Test
                discrete Real x(start = 0);
            equation
                when sample(0, 0.1) then
                    x = pre(x) + 1;
                end when;
            end Test;
            "#,
            "Test",
        );
    }

    #[test]
    fn mls_8_3_3_when_reinit() {
        expect_success(
            r#"
            model Test
                Real x(start = 0);
            equation
                der(x) = 1;
                when x > 1 then
                    reinit(x, 0);
                end when;
            end Test;
            "#,
            "Test",
        );
    }

    #[test]
    fn mls_8_3_3_when_multiple_equations() {
        expect_success(
            r#"
            model Test
                Real x(start = 0);
                discrete Real a(start = 0);
                discrete Real b(start = 0);
            equation
                der(x) = 1;
                when x > 1 then
                    a = 1;
                    b = 2;
                end when;
            end Test;
            "#,
            "Test",
        );
    }
}

// ============================================================================
// §8.3.4 ASSERT EQUATION
// ============================================================================

/// MLS §8.3.4: Assert equation
mod section_8_3_4_assert {
    use super::*;

    #[test]
    fn mls_8_3_4_assert_basic() {
        expect_success(
            r#"
            model Test
                parameter Real x = 5;
            equation
                assert(x > 0, "x must be positive");
            end Test;
            "#,
            "Test",
        );
    }

    #[test]
    fn mls_8_3_4_assert_with_level() {
        expect_success(
            r#"
            model Test
                parameter Real x = 5;
            equation
                assert(x > 0, "x must be positive", AssertionLevel.warning);
            end Test;
            "#,
            "Test",
        );
    }

    #[test]
    fn mls_8_3_4_assert_error_level() {
        expect_success(
            r#"
            model Test
                parameter Real x = 5;
            equation
                assert(x > 0, "x must be positive", AssertionLevel.error);
            end Test;
            "#,
            "Test",
        );
    }
}

// ============================================================================
// §8.3.5 CONNECT EQUATIONS
// ============================================================================

/// MLS §8.3.5: Connect equations
mod section_8_3_5_connect {
    use super::*;

    #[test]
    fn mls_8_3_5_connect_basic() {
        expect_success(
            r#"
            connector C
                Real e;
                flow Real f;
            end C;

            model Component
                C c1;
                C c2;
            equation
                c1.e = 1;
                c1.f + c2.f = 0;
                c2.e = c1.e;
            end Component;

            model Connected
                Component a;
                Component b;
            equation
                connect(a.c2, b.c1);
            end Connected;
            "#,
            "Connected",
        );
    }

    #[test]
    fn mls_8_3_5_electrical_circuit() {
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

            model Circuit
                Resistor r(R = 100);
                Ground gnd;
            equation
                r.p.v = 5;
                connect(r.n, gnd.p);
            end Circuit;
            "#,
            "Circuit",
        );
    }
}

// ============================================================================
// HIGHER ORDER ODEs
// ============================================================================

/// Tests for higher-order differential equations
mod higher_order_odes {
    use super::*;

    #[test]
    fn second_order_ode() {
        expect_success(
            r#"
            model SecondOrder
                Real x(start = 1);
                Real v(start = 0);
            equation
                der(x) = v;
                der(v) = -x;
            end SecondOrder;
            "#,
            "SecondOrder",
        );
    }

    #[test]
    fn damped_oscillator() {
        expect_success(
            r#"
            model DampedOscillator
                parameter Real m = 1 "mass";
                parameter Real c = 0.1 "damping";
                parameter Real k = 1 "stiffness";
                Real x(start = 1) "position";
                Real v(start = 0) "velocity";
            equation
                der(x) = v;
                m * der(v) = -c * v - k * x;
            end DampedOscillator;
            "#,
            "DampedOscillator",
        );
    }

    #[test]
    fn coupled_odes() {
        expect_success(
            r#"
            model CoupledSystem
                Real x(start = 1);
                Real y(start = 0);
            equation
                der(x) = y;
                der(y) = -x - 0.1 * y;
            end CoupledSystem;
            "#,
            "CoupledSystem",
        );
    }
}
