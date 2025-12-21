//! MLS §16: Low Priority Synchronous Language Element Tests
//!
//! This module tests low priority normative requirements:
//! - §16.1: Clock types and declarations
//! - §16.2: Clock operators (sample, hold, previous, etc.)
//! - §16.3: Clocked equations and algorithms
//! - §16.4: Discretized partitions
//!
//! Reference: https://specification.modelica.org/master/synchronous-language-elements.html

use crate::spec::{expect_failure, expect_parse_success};

// ============================================================================
// §16.1 CLOCK TYPES
// ============================================================================

/// MLS §16.1: Clock type declarations
mod clock_types {
    use super::*;

    /// MLS: "Clock type declaration"
    #[test]
    fn mls_16_1_clock_basic() {
        expect_parse_success(
            r#"
            model Test
                Clock c;
            equation
            end Test;
            "#,
        );
    }

    /// MLS: "Clock constructor with Real interval"
    #[test]
    fn mls_16_1_clock_real_interval() {
        expect_parse_success(
            r#"
            model Test
                Clock c = Clock(0.1);
            equation
            end Test;
            "#,
        );
    }

    /// MLS: "Clock constructor with Integer factor"
    #[test]
    fn mls_16_1_clock_integer_factor() {
        expect_parse_success(
            r#"
            model Test
                Clock c1 = Clock(0.1);
                Clock c2 = Clock(c1, 2);
            equation
            end Test;
            "#,
        );
    }

    /// MLS: "Clock with Boolean condition"
    #[test]
    fn mls_16_1_clock_boolean_condition() {
        expect_parse_success(
            r#"
            model Test
                Boolean trigger;
                Clock c = Clock(trigger);
            equation
                trigger = time > 1;
            end Test;
            "#,
        );
    }

    /// MLS: "Clock cannot have flow prefix"
    #[test]
    #[ignore = "Clock flow restriction not yet implemented"]
    fn mls_16_1_clock_no_flow() {
        expect_failure(
            r#"
            connector BadConnector
                flow Clock c;
            end BadConnector;
            "#,
            "BadConnector",
        );
    }

    /// MLS: "Clock cannot have stream prefix"
    #[test]
    #[ignore = "Clock stream restriction not yet implemented"]
    fn mls_16_1_clock_no_stream() {
        expect_failure(
            r#"
            connector BadConnector
                stream Clock c;
            end BadConnector;
            "#,
            "BadConnector",
        );
    }

    /// MLS: "Clock cannot have discrete prefix"
    #[test]
    #[ignore = "Clock discrete restriction not yet implemented"]
    fn mls_16_1_clock_no_discrete() {
        expect_failure(
            r#"
            model Test
                discrete Clock c;
            equation
            end Test;
            "#,
            "Test",
        );
    }
}

// ============================================================================
// §16.2 CLOCK OPERATORS
// ============================================================================

/// MLS §16.2: Clock operators
mod clock_operators {
    use super::*;

    /// MLS: "sample(u, clock) samples continuous signal"
    #[test]
    fn mls_16_2_sample_operator() {
        expect_parse_success(
            r#"
            model Test
                Real x(start = 0);
                Clock c = Clock(0.1);
                Real xs;
            equation
                der(x) = 1;
                xs = sample(x, c);
            end Test;
            "#,
        );
    }

    /// MLS: "hold(u) converts clocked to continuous"
    #[test]
    fn mls_16_2_hold_operator() {
        expect_parse_success(
            r#"
            model Test
                Clock c = Clock(0.1);
                Real xs;
                Real xh;
            equation
                xs = sample(1.0, c);
                xh = hold(xs);
            end Test;
            "#,
        );
    }

    /// MLS: "previous(u) returns previous clocked value"
    #[test]
    fn mls_16_2_previous_operator() {
        expect_parse_success(
            r#"
            model Test
                Clock c = Clock(0.1);
                Real x(start = 0);
            equation
                x = previous(x) + 1;
            end Test;
            "#,
        );
    }

    /// MLS: "subSample(u, factor) reduces clock rate"
    #[test]
    fn mls_16_2_subsample_operator() {
        expect_parse_success(
            r#"
            model Test
                Clock c1 = Clock(0.1);
                Real x;
                Real y;
            equation
                x = sample(1.0, c1);
                y = subSample(x, 2);
            end Test;
            "#,
        );
    }

    /// MLS: "superSample(u, factor) increases clock rate"
    #[test]
    fn mls_16_2_supersample_operator() {
        expect_parse_success(
            r#"
            model Test
                Clock c1 = Clock(0.1);
                Real x;
                Real y;
            equation
                x = sample(1.0, c1);
                y = superSample(x, 2);
            end Test;
            "#,
        );
    }

    /// MLS: "shiftSample(u, shiftCounter, resolution) shifts clock"
    #[test]
    fn mls_16_2_shiftsample_operator() {
        expect_parse_success(
            r#"
            model Test
                Clock c = Clock(0.1);
                Real x;
                Real y;
            equation
                x = sample(1.0, c);
                y = shiftSample(x, 1, 2);
            end Test;
            "#,
        );
    }

    /// MLS: "backSample(u, backCounter, resolution) back-shifts"
    #[test]
    fn mls_16_2_backsample_operator() {
        expect_parse_success(
            r#"
            model Test
                Clock c = Clock(0.1);
                Real x;
                Real y;
            equation
                x = sample(1.0, c);
                y = backSample(x, 1, 2);
            end Test;
            "#,
        );
    }

    /// MLS: "noClock(u) removes clock association"
    #[test]
    fn mls_16_2_noclock_operator() {
        expect_parse_success(
            r#"
            model Test
                Clock c = Clock(0.1);
                Real x;
                Real y;
            equation
                x = sample(1.0, c);
                y = noClock(x);
            end Test;
            "#,
        );
    }

    /// MLS: "interval() returns clock interval"
    #[test]
    fn mls_16_2_interval_function() {
        expect_parse_success(
            r#"
            model Test
                Clock c = Clock(0.1);
                Real x;
                Real dt;
            equation
                x = sample(1.0, c);
                dt = interval(x);
            end Test;
            "#,
        );
    }

    /// MLS: "firstTick() returns true on first tick"
    #[test]
    fn mls_16_2_first_tick() {
        expect_parse_success(
            r#"
            model Test
                Clock c = Clock(0.1);
                Boolean isFirst;
            equation
                isFirst = firstTick(c);
            end Test;
            "#,
        );
    }
}

// ============================================================================
// §16.3 CLOCKED EQUATIONS
// ============================================================================

/// MLS §16.3: Clocked equations and when-clauses
mod clocked_equations {
    use super::*;

    /// MLS: "Clocked when-clause with Clock condition"
    #[test]
    fn mls_16_3_clocked_when() {
        expect_parse_success(
            r#"
            model Test
                Clock c = Clock(0.1);
                Real x(start = 0);
            equation
                when c then
                    x = previous(x) + 1;
                end when;
            end Test;
            "#,
        );
    }

    /// MLS: "Clock inference from equations"
    #[test]
    fn mls_16_3_clock_inference() {
        expect_parse_success(
            r#"
            model Test
                input Real u;
                output Real y;
                Real x(start = 0);
            equation
                x = previous(x) + u;
                y = x;
            end Test;
            "#,
        );
    }

    /// MLS: "Base-clock partitioning"
    #[test]
    fn mls_16_3_base_clock_partitioning() {
        expect_parse_success(
            r#"
            model Test
                Clock c1 = Clock(0.1);
                Clock c2 = Clock(0.2);
                Real x1, x2;
            equation
                x1 = sample(1.0, c1);
                x2 = sample(2.0, c2);
            end Test;
            "#,
        );
    }
}

// ============================================================================
// §16.4 DISCRETIZED PARTITIONS
// ============================================================================

/// MLS §16.4: Discretized continuous-time partitions
mod discretized_partitions {
    use super::*;

    /// MLS: "Solver method annotation"
    #[test]
    fn mls_16_4_solver_method() {
        expect_parse_success(
            r#"
            model Test
                Real x(start = 0);
            equation
                der(x) = -x;
            annotation(
                __Dymola_discretized = true,
                __Dymola_solver = "ExplicitEuler"
            );
            end Test;
            "#,
        );
    }

    /// MLS: "ExplicitEuler solver"
    #[test]
    fn mls_16_4_explicit_euler() {
        expect_parse_success(
            r#"
            model Test
                Clock c = Clock(solverMethod = "ExplicitEuler");
                Real x(start = 1);
            equation
                when c then
                    x = previous(x) - 0.1 * previous(x);
                end when;
            end Test;
            "#,
        );
    }

    /// MLS: "ImplicitEuler solver"
    #[test]
    fn mls_16_4_implicit_euler() {
        expect_parse_success(
            r#"
            model Test
                Clock c = Clock(solverMethod = "ImplicitEuler");
                Real x(start = 1);
            equation
                when c then
                    x = previous(x) / (1 + 0.1);
                end when;
            end Test;
            "#,
        );
    }
}

// ============================================================================
// §16.5 INITIALIZATION OF CLOCKED PARTITIONS
// ============================================================================

/// MLS §16.5: Clocked partition initialization
mod clocked_initialization {
    use super::*;

    /// MLS: "Clocked variable initialization with start"
    #[test]
    fn mls_16_5_clocked_start() {
        expect_parse_success(
            r#"
            model Test
                Clock c = Clock(0.1);
                Real x(start = 5);
            equation
                x = previous(x) + 1;
            end Test;
            "#,
        );
    }

    /// MLS: "previous() returns start value on first tick"
    #[test]
    fn mls_16_5_previous_start() {
        expect_parse_success(
            r#"
            model Test
                Clock c = Clock(0.1);
                Real x(start = 0);
                Real y;
            equation
                when c then
                    y = previous(x);
                    x = y + 1;
                end when;
            end Test;
            "#,
        );
    }
}
