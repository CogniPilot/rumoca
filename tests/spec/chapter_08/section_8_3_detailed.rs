//! MLS §8.3: Equation Sections - Detailed Conformance Tests
//!
//! Comprehensive tests covering normative statements from MLS §8.3 including:
//! - §8.3.1: For-equation restrictions
//! - §8.3.2: If-equation branch balance
//! - §8.3.3: When-equation restrictions
//!
//! Reference: https://specification.modelica.org/master/equations.html

use crate::spec::{expect_failure, expect_parse_success, expect_success};

// ============================================================================
// §8.3.1 FOR-EQUATION RESTRICTIONS
// ============================================================================

/// MLS §8.3.1: For-equation normative requirements
mod for_equation_restrictions {
    use super::*;

    /// MLS: "The expression of a for-equation shall be a parameter expression"
    #[test]
    fn mls_8_3_1_parameter_expression_range() {
        expect_success(
            r#"
            model Test
                parameter Integer n = 5;
                Real x[5];
            equation
                for i in 1:n loop
                    x[i] = i;
                end for;
            end Test;
            "#,
            "Test",
        );
    }

    /// For-loop with constant range
    #[test]
    fn mls_8_3_1_constant_range() {
        expect_success(
            r#"
            model Test
                constant Integer n = 3;
                Real x[3];
            equation
                for i in 1:n loop
                    x[i] = i * 2;
                end for;
            end Test;
            "#,
            "Test",
        );
    }

    /// MLS: "The loop-variable is implicitly declared and is local to the for-equation"
    #[test]
    fn mls_8_3_1_loop_variable_local_scope() {
        expect_success(
            r#"
            model Test
                parameter Integer i = 100;
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

    /// MLS: "The loop-variable shall not be modified within the for-equation"
    #[test]
    #[ignore = "Loop variable assignment detection not yet implemented"]
    fn error_loop_variable_assigned() {
        expect_failure(
            r#"
            model Test
                Real x[5];
            equation
                for i in 1:5 loop
                    i = i + 1;
                    x[i] = i;
                end for;
            end Test;
            "#,
            "Test",
        );
    }

    /// For-loop with array literal range
    #[test]
    fn mls_8_3_1_array_literal_range() {
        expect_success(
            r#"
            model Test
                Real x[3];
            equation
                for i in {1, 3, 5} loop
                    x[div(i + 1, 2)] = i;
                end for;
            end Test;
            "#,
            "Test",
        );
    }

    /// For-loop with enumeration range
    #[test]
    fn mls_8_3_1_enumeration_range() {
        expect_parse_success(
            r#"
            type Mode = enumeration(Off, On, Auto);
            model Test
                Real x[3];
            equation
                for m in Mode loop
                    x[Integer(m)] = Integer(m);
                end for;
            end Test;
            "#,
        );
    }

    /// Nested for-loops with independent indices
    #[test]
    fn mls_8_3_1_nested_independent_indices() {
        expect_success(
            r#"
            model Test
                Real A[3, 3];
            equation
                for i in 1:3 loop
                    for j in 1:3 loop
                        A[i, j] = i + j;
                    end for;
                end for;
            end Test;
            "#,
            "Test",
        );
    }

    /// For-loop empty iteration (n = 0)
    #[test]
    fn mls_8_3_1_empty_iteration() {
        expect_success(
            r#"
            model Test
                parameter Integer n = 0;
                Real x;
            equation
                x = 1;
                for i in 1:n loop
                end for;
            end Test;
            "#,
            "Test",
        );
    }
}

// ============================================================================
// §8.3.2 IF-EQUATION BRANCH BALANCE
// ============================================================================

/// MLS §8.3.2: If-equation normative requirements
mod if_equation_balance {
    use super::*;

    /// MLS: "Each branch of an if-equation shall have the same number of equations"
    #[test]
    fn mls_8_3_2_balanced_branches() {
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

    /// Unbalanced if-equation branches
    #[test]
    #[ignore = "If-equation branch balance checking not yet implemented"]
    fn error_unbalanced_branches() {
        expect_failure(
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
                end if;
            end Test;
            "#,
            "Test",
        );
    }

    /// Elseif branches must also be balanced
    #[test]
    fn mls_8_3_2_elseif_balanced() {
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
                else
                    x = 2;
                end if;
            end Test;
            "#,
            "Test",
        );
    }

    /// If-equation with for-equations in branches
    #[test]
    fn mls_8_3_2_for_in_branches() {
        expect_success(
            r#"
            model Test
                parameter Boolean mode = true;
                Real x[3];
            equation
                if mode then
                    for i in 1:3 loop
                        x[i] = i;
                    end for;
                else
                    for i in 1:3 loop
                        x[i] = -i;
                    end for;
                end if;
            end Test;
            "#,
            "Test",
        );
    }

    /// If with parameter condition (static evaluation)
    #[test]
    fn mls_8_3_2_parameter_condition() {
        expect_success(
            r#"
            model Test
                parameter Boolean useSimpleModel = true;
                Real x(start = 0);
            equation
                if useSimpleModel then
                    der(x) = 1;
                else
                    der(x) = sin(x);
                end if;
            end Test;
            "#,
            "Test",
        );
    }
}

// ============================================================================
// §8.3.3 WHEN-EQUATION RESTRICTIONS
// ============================================================================

/// MLS §8.3.3: When-equation normative requirements
mod when_equation_restrictions {
    use super::*;

    /// Basic when-equation functionality
    #[test]
    fn mls_8_3_3_basic_when() {
        expect_success(
            r#"
            model Test
                Real x(start = 0);
                discrete Real y(start = 0);
            equation
                der(x) = 1;
                when x > 1 then
                    y = 1;
                end when;
            end Test;
            "#,
            "Test",
        );
    }

    /// MLS: "When-equations shall not be used in initial equation sections"
    #[test]
    #[ignore = "When in initial equation detection not yet implemented"]
    fn error_when_in_initial_equation() {
        expect_failure(
            r#"
            model Test
                Real x(start = 0);
                discrete Real y(start = 0);
            initial equation
                when true then
                    y = 1;
                end when;
            equation
                der(x) = 1;
            end Test;
            "#,
            "Test",
        );
    }

    /// MLS: "When-equations shall not be nested"
    #[test]
    fn error_nested_when() {
        expect_failure(
            r#"
            model Test
                Real x(start = 0);
                discrete Real y(start = 0);
                discrete Real z(start = 0);
            equation
                der(x) = 1;
                when x > 1 then
                    y = 1;
                    when x > 2 then
                        z = 1;
                    end when;
                end when;
            end Test;
            "#,
            "Test",
        );
    }

    /// MLS: "Two when-equations shall not define the same variable"
    #[test]
    #[ignore = "Duplicate when-equation variable detection not yet implemented"]
    fn error_duplicate_when_variable() {
        expect_failure(
            r#"
            model Test
                Real x(start = 0);
                discrete Real y(start = 0);
            equation
                der(x) = 1;
                when x > 1 then
                    y = 1;
                end when;
                when x > 2 then
                    y = 2;
                end when;
            end Test;
            "#,
            "Test",
        );
    }

    /// MLS: "All branches of when/elsewhen must have same set of LHS variables"
    #[test]
    #[ignore = "When branch LHS consistency check not yet implemented"]
    fn error_inconsistent_when_branches() {
        expect_failure(
            r#"
            model Test
                Real x(start = 0);
                discrete Real y(start = 0);
                discrete Real z(start = 0);
            equation
                der(x) = 1;
                when x > 1 then
                    y = 1;
                elsewhen x > 2 then
                    z = 1;
                end when;
            end Test;
            "#,
            "Test",
        );
    }

    /// MLS: "reinit can only be used in when-equation body"
    #[test]
    #[ignore = "reinit location check not yet implemented"]
    fn error_reinit_outside_when() {
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

    /// MLS: "reinit shall only be applied once to a variable in any when-clause"
    #[test]
    #[ignore = "Duplicate reinit detection not yet implemented"]
    fn error_duplicate_reinit() {
        expect_failure(
            r#"
            model Test
                Real x(start = 0);
            equation
                der(x) = 1;
                when x > 1 then
                    reinit(x, 0);
                    reinit(x, 1);
                end when;
            end Test;
            "#,
            "Test",
        );
    }

    /// Valid when with elsewhen and same LHS
    #[test]
    fn mls_8_3_3_valid_elsewhen() {
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

    /// When with sample trigger
    #[test]
    fn mls_8_3_3_sample_trigger() {
        expect_success(
            r#"
            model Test
                discrete Real sampled(start = 0);
                discrete Integer count(start = 0);
            equation
                when sample(0, 0.1) then
                    sampled = time;
                    count = pre(count) + 1;
                end when;
            end Test;
            "#,
            "Test",
        );
    }

    /// When with edge trigger
    #[test]
    fn mls_8_3_3_edge_trigger() {
        expect_success(
            r#"
            model Test
                Boolean trigger(start = false);
                Real x(start = 0);
                discrete Integer count(start = 0);
            equation
                der(x) = 1;
                trigger = x > 1;
                when edge(trigger) then
                    count = pre(count) + 1;
                end when;
            end Test;
            "#,
            "Test",
        );
    }

    /// When with array condition
    #[test]
    fn mls_8_3_3_array_condition() {
        expect_success(
            r#"
            model Test
                Real x(start = 0);
                Real y(start = 0);
                discrete Real triggered(start = 0);
            equation
                der(x) = 1;
                der(y) = 2;
                when {x > 1, y > 1} then
                    triggered = 1;
                end when;
            end Test;
            "#,
            "Test",
        );
    }
}

// ============================================================================
// §8.4 SYNCHRONOUS DATA-FLOW PRINCIPLE
// ============================================================================

/// MLS §8.4: Synchronous data-flow principle
mod synchronous_dataflow {
    use super::*;

    /// Basic causality: equations define unknowns
    #[test]
    fn mls_8_4_basic_causality() {
        expect_success(
            r#"
            model Test
                Real x;
                Real y;
            equation
                x = 1;
                y = x + 1;
            end Test;
            "#,
            "Test",
        );
    }

    /// Algebraic loop is valid
    #[test]
    fn mls_8_4_algebraic_loop() {
        expect_success(
            r#"
            model Test
                Real x;
                Real y;
            equation
                x + y = 10;
                x - y = 2;
            end Test;
            "#,
            "Test",
        );
    }

    /// Coupled ODEs
    #[test]
    fn mls_8_4_coupled_odes() {
        expect_success(
            r#"
            model Test
                Real x(start = 1);
                Real y(start = 0);
            equation
                der(x) = -y;
                der(y) = x;
            end Test;
            "#,
            "Test",
        );
    }
}

// ============================================================================
// COMPLEX EQUATION SCENARIOS
// ============================================================================

/// Complex equation scenarios combining multiple features
mod complex_scenarios {
    use super::*;

    /// For-equation with conditional inside
    #[test]
    fn complex_for_with_if() {
        expect_success(
            r#"
            model Test
                parameter Boolean usePositive = true;
                Real x[5];
            equation
                for i in 1:5 loop
                    if usePositive then
                        x[i] = i;
                    else
                        x[i] = -i;
                    end if;
                end for;
            end Test;
            "#,
            "Test",
        );
    }

    /// If-equation with when inside
    #[test]
    fn complex_if_with_when() {
        expect_success(
            r#"
            model Test
                parameter Boolean enableEvent = true;
                Real x(start = 0);
                discrete Real triggered(start = 0);
            equation
                der(x) = 1;
                if enableEvent then
                    when x > 1 then
                        triggered = 1;
                    end when;
                else
                    triggered = 0;
                end if;
            end Test;
            "#,
            "Test",
        );
    }

    /// Multiple independent when-equations
    #[test]
    fn complex_multiple_when() {
        expect_success(
            r#"
            model Test
                Real x(start = 0);
                discrete Real a(start = 0);
                discrete Real b(start = 0);
                discrete Real c(start = 0);
            equation
                der(x) = 1;
                when x > 1 then
                    a = 1;
                end when;
                when x > 2 then
                    b = 1;
                end when;
                when x > 3 then
                    c = 1;
                end when;
            end Test;
            "#,
            "Test",
        );
    }

    /// For-equation generating when-equations
    /// MLS §8.3.5: "A when-equation shall not be used within a for-equation"
    /// Even though this pattern is sometimes useful, it's explicitly forbidden
    #[test]
    fn complex_for_generating_when() {
        expect_failure(
            r#"
            model Test
                Real x(start = 0);
                discrete Real triggered[3](each start = 0);
            equation
                der(x) = 1;
                for i in 1:3 loop
                    when x > i then
                        triggered[i] = 1;
                    end when;
                end for;
            end Test;
            "#,
            "Test",
        );
    }

    /// Initial equation with for-loop
    #[test]
    fn complex_initial_for() {
        expect_success(
            r#"
            model Test
                Real x[5];
            initial equation
                for i in 1:5 loop
                    x[i] = i;
                end for;
            equation
                for i in 1:5 loop
                    der(x[i]) = -x[i];
                end for;
            end Test;
            "#,
            "Test",
        );
    }
}
