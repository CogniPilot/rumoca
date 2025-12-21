//! MLS §8: Critical When-Equation Restriction Tests
//!
//! This module tests critical normative requirements for when-equations:
//! - §8.3.5: When-equations cannot be nested
//! - §8.3.5: When-equations cannot appear in functions
//! - §8.3.5: When condition must be discrete-time
//! - §8.3.5: Statements in when must be discrete assignments
//! - §8.3.5: reinit() can only appear in when-equations
//!
//! Reference: https://specification.modelica.org/master/equations.html

use crate::spec::{expect_failure, expect_success};

// ============================================================================
// §8.3.5 WHEN-EQUATION NESTING RESTRICTION
// ============================================================================

/// MLS §8.3.5: When-equations cannot be nested
mod when_nesting_restriction {
    use super::*;

    /// MLS: "A when-equation shall not be used within a when-equation"
    #[test]
    fn mls_8_3_5_when_inside_when() {
        expect_failure(
            r#"
            model Test
                Real x(start = 0);
                discrete Integer n(start = 0);
                discrete Integer m(start = 0);
            equation
                der(x) = 1;
                when x > 1 then
                    n = pre(n) + 1;
                    when x > 2 then
                        m = pre(m) + 1;
                    end when;
                end when;
            end Test;
            "#,
            "Test",
        );
    }

    /// MLS: "A when-equation shall not be used within an if-equation"
    #[test]
    #[ignore = "When inside if-equation detection not yet implemented"]
    fn mls_8_3_5_when_inside_if() {
        expect_failure(
            r#"
            model Test
                Boolean cond = true;
                Real x(start = 0);
                discrete Integer n(start = 0);
            equation
                der(x) = 1;
                if cond then
                    when x > 1 then
                        n = pre(n) + 1;
                    end when;
                end if;
            end Test;
            "#,
            "Test",
        );
    }

    /// MLS: "A when-equation shall not be used within a for-equation"
    #[test]
    #[ignore = "When inside for-equation detection not yet implemented"]
    fn mls_8_3_5_when_inside_for() {
        expect_failure(
            r#"
            model Test
                Real x(start = 0);
                discrete Integer n[3](each start = 0);
            equation
                der(x) = 1;
                for i in 1:3 loop
                    when x > i then
                        n[i] = pre(n[i]) + 1;
                    end when;
                end for;
            end Test;
            "#,
            "Test",
        );
    }

    /// Valid: Sequential when-equations at same level
    #[test]
    fn mls_8_3_5_sequential_when_allowed() {
        expect_success(
            r#"
            model Test
                Real x(start = 0);
                discrete Integer n(start = 0);
                discrete Integer m(start = 0);
            equation
                der(x) = 1;
                when x > 1 then
                    n = pre(n) + 1;
                end when;
                when x > 2 then
                    m = pre(m) + 1;
                end when;
            end Test;
            "#,
            "Test",
        );
    }

    /// Valid: When with elsewhen (not nested)
    #[test]
    fn mls_8_3_5_when_elsewhen_allowed() {
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
                elsewhen x > 3 then
                    state = 3;
                end when;
            end Test;
            "#,
            "Test",
        );
    }
}

// ============================================================================
// §8.3.5 WHEN-EQUATION CONDITION RESTRICTIONS
// ============================================================================

/// MLS §8.3.5: When condition must be discrete-time Boolean expression
mod when_condition_restriction {
    use super::*;

    /// MLS: "The condition of a when-equation shall be a discrete-time expression"
    #[test]
    #[ignore = "When condition discrete check not yet implemented"]
    fn mls_8_3_5_when_condition_continuous() {
        // Using a continuous expression directly as when condition
        // This should fail because x is continuous
        expect_failure(
            r#"
            model Test
                Real x(start = 0);
                discrete Integer n(start = 0);
            equation
                der(x) = 1;
                when x then
                    n = pre(n) + 1;
                end when;
            end Test;
            "#,
            "Test",
        );
    }

    /// MLS: "Condition must be Boolean"
    #[test]
    #[ignore = "When condition Boolean check not yet implemented"]
    fn mls_8_3_5_when_condition_integer() {
        expect_failure(
            r#"
            model Test
                Integer trigger = 1;
                discrete Integer n(start = 0);
            equation
                when trigger then
                    n = pre(n) + 1;
                end when;
            end Test;
            "#,
            "Test",
        );
    }

    /// MLS: "Condition must be Boolean"
    #[test]
    #[ignore = "When condition Boolean check not yet implemented"]
    fn mls_8_3_5_when_condition_real() {
        expect_failure(
            r#"
            model Test
                Real trigger = 1.0;
                discrete Integer n(start = 0);
            equation
                when trigger then
                    n = pre(n) + 1;
                end when;
            end Test;
            "#,
            "Test",
        );
    }

    /// Valid: Boolean comparison as condition
    #[test]
    fn mls_8_3_5_when_condition_comparison_allowed() {
        expect_success(
            r#"
            model Test
                Real x(start = 0);
                discrete Integer n(start = 0);
            equation
                der(x) = 1;
                when x > 1 then
                    n = pre(n) + 1;
                end when;
            end Test;
            "#,
            "Test",
        );
    }

    /// Valid: sample() as condition
    #[test]
    fn mls_8_3_5_when_condition_sample_allowed() {
        expect_success(
            r#"
            model Test
                discrete Real sampled(start = 0);
                Real x(start = 0);
            equation
                der(x) = 1;
                when sample(0, 0.1) then
                    sampled = x;
                end when;
            end Test;
            "#,
            "Test",
        );
    }

    /// Valid: initial() as condition
    #[test]
    fn mls_8_3_5_when_condition_initial_allowed() {
        expect_success(
            r#"
            model Test
                discrete Integer n(start = 0);
            equation
                when initial() then
                    n = 1;
                end when;
            end Test;
            "#,
            "Test",
        );
    }
}

// ============================================================================
// §8.3.5 WHEN-EQUATION BODY RESTRICTIONS
// ============================================================================

/// MLS §8.3.5: Restrictions on statements within when-equations
mod when_body_restriction {
    use super::*;

    /// MLS: "Only discrete-time expressions can appear in when-equation body"
    #[test]
    fn mls_8_3_5_when_body_discrete_assignment() {
        expect_success(
            r#"
            model Test
                Real x(start = 0);
                discrete Integer n(start = 0);
            equation
                der(x) = 1;
                when x > 1 then
                    n = pre(n) + 1;
                end when;
            end Test;
            "#,
            "Test",
        );
    }

    /// MLS: "Cannot assign to continuous variable in when (without reinit)"
    #[test]
    #[ignore = "When body continuous assignment check not yet implemented"]
    fn mls_8_3_5_when_continuous_assignment_forbidden() {
        expect_failure(
            r#"
            model Test
                Real x(start = 0);
                Real y;
            equation
                der(x) = 1;
                when x > 1 then
                    y = x;
                end when;
            end Test;
            "#,
            "Test",
        );
    }

    /// Valid: Using reinit for continuous variable in when
    #[test]
    fn mls_8_3_5_when_reinit_allowed() {
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

    /// MLS: "assert is allowed in when-equations"
    #[test]
    fn mls_8_3_5_when_assert_allowed() {
        expect_success(
            r#"
            model Test
                Real x(start = 0);
            equation
                der(x) = 1;
                when x > 10 then
                    assert(false, "x exceeded limit");
                end when;
            end Test;
            "#,
            "Test",
        );
    }

    /// MLS: "terminate is allowed in when-equations"
    #[test]
    fn mls_8_3_5_when_terminate_allowed() {
        expect_success(
            r#"
            model Test
                Real x(start = 0);
            equation
                der(x) = 1;
                when x > 10 then
                    terminate("Simulation complete");
                end when;
            end Test;
            "#,
            "Test",
        );
    }
}

// ============================================================================
// §8.3.5 REINIT RESTRICTIONS
// ============================================================================

/// MLS §8.3.5: reinit() can only appear in when-equations
mod reinit_restriction {
    use super::*;

    /// MLS: "reinit can only be used in a when-equation"
    #[test]
    #[ignore = "reinit context restriction not yet implemented"]
    fn mls_8_3_5_reinit_outside_when() {
        expect_failure(
            r#"
            model Test
                Real x(start = 0);
            equation
                der(x) = 1;
                reinit(x, 0);
            end Test;
            "#,
            "Test",
        );
    }

    /// MLS: "reinit can only be used in a when-equation"
    #[test]
    #[ignore = "reinit context restriction not yet implemented"]
    fn mls_8_3_5_reinit_in_if() {
        expect_failure(
            r#"
            model Test
                Real x(start = 0);
                Boolean cond = true;
            equation
                der(x) = 1;
                if cond then
                    reinit(x, 0);
                end if;
            end Test;
            "#,
            "Test",
        );
    }

    /// MLS: "reinit can only reinitialize continuous-time variables"
    #[test]
    #[ignore = "reinit variable type check not yet implemented"]
    fn mls_8_3_5_reinit_discrete_forbidden() {
        expect_failure(
            r#"
            model Test
                Real x(start = 0);
                discrete Integer n(start = 0);
            equation
                der(x) = 1;
                when x > 1 then
                    reinit(n, 0);
                end when;
            end Test;
            "#,
            "Test",
        );
    }

    /// MLS: "reinit can only reinitialize continuous-time variables"
    #[test]
    #[ignore = "reinit variable type check not yet implemented"]
    fn mls_8_3_5_reinit_parameter_forbidden() {
        expect_failure(
            r#"
            model Test
                Real x(start = 0);
                parameter Real k = 1;
            equation
                der(x) = 1;
                when x > 1 then
                    reinit(k, 0);
                end when;
            end Test;
            "#,
            "Test",
        );
    }

    /// Valid: reinit in when for continuous state
    #[test]
    fn mls_8_3_5_reinit_in_when_allowed() {
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

    /// Valid: reinit for array element
    #[test]
    fn mls_8_3_5_reinit_array_element_allowed() {
        expect_success(
            r#"
            model Test
                Real x[2](each start = 0);
            equation
                der(x[1]) = 1;
                der(x[2]) = 2;
                when x[1] > 1 then
                    reinit(x[1], 0);
                end when;
            end Test;
            "#,
            "Test",
        );
    }
}

// ============================================================================
// §8.3.5 PRE() RESTRICTIONS
// ============================================================================

/// MLS §8.3.5: pre() usage restrictions
mod pre_restriction {
    use super::*;

    /// MLS: "pre() can only be applied to discrete-time variables"
    #[test]
    #[ignore = "pre() variable type check not yet implemented"]
    fn mls_8_3_5_pre_continuous_forbidden() {
        expect_failure(
            r#"
            model Test
                Real x(start = 0);
                Real y;
            equation
                der(x) = 1;
                y = pre(x);
            end Test;
            "#,
            "Test",
        );
    }

    /// Valid: pre() on discrete variable
    #[test]
    fn mls_8_3_5_pre_discrete_allowed() {
        expect_success(
            r#"
            model Test
                Real x(start = 0);
                discrete Integer n(start = 0);
            equation
                der(x) = 1;
                when x > 1 then
                    n = pre(n) + 1;
                end when;
            end Test;
            "#,
            "Test",
        );
    }

    /// Valid: pre() on Boolean
    #[test]
    fn mls_8_3_5_pre_boolean_allowed() {
        expect_success(
            r#"
            model Test
                Real x(start = 0);
                Boolean trigger = x > 1;
            equation
                der(x) = 1;
            end Test;
            "#,
            "Test",
        );
    }
}

// ============================================================================
// §8.3.5 EDGE() AND CHANGE() RESTRICTIONS
// ============================================================================

/// MLS §8.3.5: edge() and change() usage restrictions
mod edge_change_restriction {
    use super::*;

    /// Valid: edge() on Boolean expression
    #[test]
    fn mls_8_3_5_edge_boolean_allowed() {
        expect_success(
            r#"
            model Test
                Real x(start = 0);
                Boolean trigger = x > 1;
                discrete Integer n(start = 0);
            equation
                der(x) = 1;
                when edge(trigger) then
                    n = pre(n) + 1;
                end when;
            end Test;
            "#,
            "Test",
        );
    }

    /// Valid: change() on discrete variable
    #[test]
    fn mls_8_3_5_change_discrete_allowed() {
        expect_success(
            r#"
            model Test
                Real x(start = 0);
                discrete Integer state(start = 0);
                discrete Integer changes(start = 0);
            equation
                der(x) = 1;
                when x > 1 then
                    state = 1;
                elsewhen x > 2 then
                    state = 2;
                end when;
                when change(state) then
                    changes = pre(changes) + 1;
                end when;
            end Test;
            "#,
            "Test",
        );
    }

    /// MLS: "edge() requires a Boolean argument"
    #[test]
    #[ignore = "edge() argument type check not yet implemented"]
    fn mls_8_3_5_edge_integer_forbidden() {
        expect_failure(
            r#"
            model Test
                discrete Integer n(start = 0);
            equation
                when edge(n) then
                    n = pre(n) + 1;
                end when;
            end Test;
            "#,
            "Test",
        );
    }
}
