//! MLS §11: High Priority Statement and Algorithm Tests
//!
//! This module tests high priority normative requirements:
//! - §11.2.1: Assignment statement restrictions
//! - §11.2.4: For-statement restrictions
//! - §11.2.5: While-statement restrictions
//! - §11.2.6: Break and return statement restrictions
//! - §11.2.7: When-statement restrictions (algorithm section)
//!
//! Reference: https://specification.modelica.org/master/statements-and-algorithm-sections.html

use crate::spec::{expect_failure, expect_success};

// ============================================================================
// §11.2.1 ASSIGNMENT STATEMENT RESTRICTIONS
// ============================================================================

/// MLS §11.2.1: Assignment statement restrictions
mod assignment_restrictions {
    use super::*;

    /// MLS: "Input components cannot be assigned in algorithm"
    #[test]
    fn mls_11_2_1_input_assignment_forbidden() {
        expect_failure(
            r#"
            function Test
                input Real x;
                output Real y;
            algorithm
                x := 1.0;
                y := x;
            end Test;
            "#,
            "Test",
        );
    }

    /// MLS: "Parameter cannot be assigned in algorithm"
    #[test]
    fn mls_11_2_1_parameter_assignment_forbidden() {
        expect_failure(
            r#"
            model Test
                parameter Real k = 1;
            algorithm
                k := 2;
            end Test;
            "#,
            "Test",
        );
    }

    /// MLS: "Constant cannot be assigned in algorithm"
    #[test]
    fn mls_11_2_1_constant_assignment_forbidden() {
        expect_failure(
            r#"
            model Test
                constant Real c = 1;
            algorithm
                c := 2;
            end Test;
            "#,
            "Test",
        );
    }

    /// Valid: Regular variable assignment
    #[test]
    fn mls_11_2_1_variable_assignment_allowed() {
        expect_success(
            r#"
            model Test
                Real x(start = 0);
            algorithm
                x := 1.0;
            end Test;
            "#,
            "Test",
        );
    }

    /// Valid: Output assignment in function
    #[test]
    fn mls_11_2_1_output_assignment_allowed() {
        expect_success(
            r#"
            function Test
                input Real x;
                output Real y;
            algorithm
                y := x * 2;
            end Test;
            "#,
            "Test",
        );
    }

    /// Valid: Array element assignment
    #[test]
    fn mls_11_2_1_array_element_assignment_allowed() {
        expect_success(
            r#"
            model Test
                Real x[3];
            algorithm
                x[1] := 1;
                x[2] := 2;
                x[3] := 3;
            end Test;
            "#,
            "Test",
        );
    }
}

// ============================================================================
// §11.2.4 FOR-STATEMENT RESTRICTIONS
// ============================================================================

/// MLS §11.2.4: For-statement restrictions
mod for_statement_restrictions {
    use super::*;

    /// MLS: "Loop variable is implicitly declared and read-only"
    #[test]
    #[ignore = "Loop variable assignment detection not yet implemented"]
    fn mls_11_2_4_loop_variable_assignment_forbidden() {
        expect_failure(
            r#"
            model Test
                Real sum;
            algorithm
                sum := 0;
                for i in 1:10 loop
                    i := i + 1;
                    sum := sum + i;
                end for;
            end Test;
            "#,
            "Test",
        );
    }

    /// MLS: "Loop variable scope is the for-statement body"
    #[test]
    fn mls_11_2_4_loop_variable_outside_scope() {
        expect_failure(
            r#"
            model Test
                Real x;
            algorithm
                for i in 1:10 loop
                    x := i;
                end for;
                x := i;
            end Test;
            "#,
            "Test",
        );
    }

    /// Valid: Basic for loop
    #[test]
    fn mls_11_2_4_basic_for_loop() {
        expect_success(
            r#"
            model Test
                Real sum;
            algorithm
                sum := 0;
                for i in 1:10 loop
                    sum := sum + i;
                end for;
            end Test;
            "#,
            "Test",
        );
    }

    /// Valid: Nested for loops
    #[test]
    fn mls_11_2_4_nested_for_loops() {
        expect_success(
            r#"
            model Test
                Real A[3,3];
            algorithm
                for i in 1:3 loop
                    for j in 1:3 loop
                        A[i, j] := i * j;
                    end for;
                end for;
            end Test;
            "#,
            "Test",
        );
    }

    /// Valid: For loop with step
    #[test]
    fn mls_11_2_4_for_loop_with_step() {
        expect_success(
            r#"
            model Test
                Real sum;
            algorithm
                sum := 0;
                for i in 1:2:10 loop
                    sum := sum + i;
                end for;
            end Test;
            "#,
            "Test",
        );
    }
}

// ============================================================================
// §11.2.5 WHILE-STATEMENT RESTRICTIONS
// ============================================================================

/// MLS §11.2.5: While-statement restrictions
mod while_statement_restrictions {
    use super::*;

    /// MLS: "While condition must be Boolean expression"
    #[test]
    #[ignore = "While condition type check not yet implemented"]
    fn mls_11_2_5_while_condition_integer() {
        expect_failure(
            r#"
            model Test
                Integer n;
            algorithm
                n := 10;
                while n loop
                    n := n - 1;
                end while;
            end Test;
            "#,
            "Test",
        );
    }

    /// MLS: "While condition must be Boolean expression"
    #[test]
    #[ignore = "While condition type check not yet implemented"]
    fn mls_11_2_5_while_condition_real() {
        expect_failure(
            r#"
            model Test
                Real x;
            algorithm
                x := 10.0;
                while x loop
                    x := x - 1;
                end while;
            end Test;
            "#,
            "Test",
        );
    }

    /// Valid: While with Boolean condition
    #[test]
    fn mls_11_2_5_while_boolean_condition() {
        expect_success(
            r#"
            model Test
                Real x;
            algorithm
                x := 10;
                while x > 0 loop
                    x := x - 1;
                end while;
            end Test;
            "#,
            "Test",
        );
    }

    /// Valid: While with Boolean variable
    #[test]
    fn mls_11_2_5_while_boolean_variable() {
        expect_success(
            r#"
            model Test
                Real x;
                Boolean running;
            algorithm
                x := 10;
                running := true;
                while running loop
                    x := x - 1;
                    running := x > 0;
                end while;
            end Test;
            "#,
            "Test",
        );
    }
}

// ============================================================================
// §11.2.6 BREAK AND RETURN RESTRICTIONS
// ============================================================================

/// MLS §11.2.6: Break and return statement restrictions
mod break_return_restrictions {
    use super::*;

    /// MLS: "break can only be used in for or while"
    #[test]
    fn mls_11_2_6_break_outside_loop() {
        expect_failure(
            r#"
            model Test
                Real x;
            algorithm
                x := 1;
                break;
            end Test;
            "#,
            "Test",
        );
    }

    /// MLS: "return can only be used in functions"
    #[test]
    #[ignore = "Return context restriction not yet implemented"]
    fn mls_11_2_6_return_in_model() {
        expect_failure(
            r#"
            model Test
                Real x;
            algorithm
                x := 1;
                return;
            end Test;
            "#,
            "Test",
        );
    }

    /// Valid: Break in for loop
    #[test]
    fn mls_11_2_6_break_in_for() {
        expect_success(
            r#"
            model Test
                Real x;
            algorithm
                x := 0;
                for i in 1:100 loop
                    x := x + i;
                    if x > 50 then
                        break;
                    end if;
                end for;
            end Test;
            "#,
            "Test",
        );
    }

    /// Valid: Break in while loop
    #[test]
    fn mls_11_2_6_break_in_while() {
        expect_success(
            r#"
            model Test
                Real x;
            algorithm
                x := 0;
                while true loop
                    x := x + 1;
                    if x > 10 then
                        break;
                    end if;
                end while;
            end Test;
            "#,
            "Test",
        );
    }

    /// Valid: Return in function
    #[test]
    fn mls_11_2_6_return_in_function() {
        expect_success(
            r#"
            function Test
                input Real x;
                output Real y;
            algorithm
                if x < 0 then
                    y := 0;
                    return;
                end if;
                y := x;
            end Test;
            "#,
            "Test",
        );
    }
}

// ============================================================================
// §11.2.7 WHEN-STATEMENT RESTRICTIONS (ALGORITHM)
// ============================================================================

/// MLS §11.2.7: When-statement restrictions in algorithms
mod when_statement_restrictions {
    use super::*;

    /// MLS: "When-statements cannot be nested"
    #[test]
    fn mls_11_2_7_when_nested_forbidden() {
        expect_failure(
            r#"
            model Test
                Real x(start = 0);
                discrete Integer n(start = 0);
                discrete Integer m(start = 0);
            algorithm
                when x > 1 then
                    n := pre(n) + 1;
                    when x > 2 then
                        m := pre(m) + 1;
                    end when;
                end when;
            equation
                der(x) = 1;
            end Test;
            "#,
            "Test",
        );
    }

    /// MLS: "When-statements cannot appear in functions"
    #[test]
    fn mls_11_2_7_when_in_function_forbidden() {
        expect_failure(
            r#"
            function Test
                input Real x;
                output Integer n;
            algorithm
                n := 0;
                when x > 1 then
                    n := 1;
                end when;
            end Test;
            "#,
            "Test",
        );
    }

    /// MLS: "When-statements cannot be inside if-statements in algorithms"
    #[test]
    fn mls_11_2_7_when_inside_if_forbidden() {
        expect_failure(
            r#"
            model Test
                Boolean cond = true;
                Real x(start = 0);
                discrete Integer n(start = 0);
            algorithm
                if cond then
                    when x > 1 then
                        n := pre(n) + 1;
                    end when;
                end if;
            equation
                der(x) = 1;
            end Test;
            "#,
            "Test",
        );
    }

    /// Valid: When-statement at top level of algorithm
    #[test]
    fn mls_11_2_7_when_top_level_allowed() {
        expect_success(
            r#"
            model Test
                Real x(start = 0);
                discrete Integer n(start = 0);
            algorithm
                when x > 1 then
                    n := pre(n) + 1;
                end when;
            equation
                der(x) = 1;
            end Test;
            "#,
            "Test",
        );
    }

    /// Valid: Multiple sequential when-statements
    #[test]
    fn mls_11_2_7_when_sequential_allowed() {
        expect_success(
            r#"
            model Test
                Real x(start = 0);
                discrete Integer n(start = 0);
                discrete Integer m(start = 0);
            algorithm
                when x > 1 then
                    n := pre(n) + 1;
                end when;
                when x > 2 then
                    m := pre(m) + 1;
                end when;
            equation
                der(x) = 1;
            end Test;
            "#,
            "Test",
        );
    }
}

// ============================================================================
// §11.1 ALGORITHM SECTION RESTRICTIONS
// ============================================================================

/// MLS §11.1: Algorithm section restrictions
mod algorithm_section_restrictions {
    use super::*;

    /// MLS: "Variables assigned in algorithm get start value at initialization"
    #[test]
    fn mls_11_1_algorithm_start_value() {
        expect_success(
            r#"
            model Test
                Real x(start = 5);
            algorithm
                x := x + 1;
            end Test;
            "#,
            "Test",
        );
    }

    /// MLS: "Multiple algorithm sections are executed in order"
    #[test]
    fn mls_11_1_multiple_algorithm_sections() {
        expect_success(
            r#"
            model Test
                Real x;
                Real y;
            algorithm
                x := 1;
            algorithm
                y := x + 1;
            end Test;
            "#,
            "Test",
        );
    }

    /// MLS: "Algorithm and equation sections can be mixed"
    #[test]
    fn mls_11_1_mixed_sections() {
        expect_success(
            r#"
            model Test
                Real x(start = 0);
                Real y;
                Real z;
            equation
                der(x) = 1;
            algorithm
                y := x * 2;
            equation
                z = y + 1;
            end Test;
            "#,
            "Test",
        );
    }
}
