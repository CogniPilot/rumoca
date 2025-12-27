//! MLS §3: Critical Type Restriction Tests
//!
//! This module tests critical normative requirements for operator type checking:
//! - §3.5: Boolean ordering forbidden
//! - §3.5: Logical operators only on Boolean
//! - §3.5: Relational operators type compatibility
//! - §3.6: If-expression branch type matching
//!
//! These are semantic checks that prevent type errors at compile time.
//!
//! Reference: https://specification.modelica.org/master/operatorsandexpressions.html

use crate::spec::{expect_failure, expect_success};

// ============================================================================
// §3.5 BOOLEAN ORDERING RESTRICTION
// ============================================================================

/// MLS §3.5: Boolean values cannot be ordered with <, <=, >, >=
mod boolean_ordering_restriction {
    use super::*;

    /// MLS: "Boolean cannot be used with ordering operators"
    #[test]
    fn mls_3_5_boolean_less_than_forbidden() {
        expect_failure(
            r#"
            model Test
                Boolean a = true;
                Boolean b = false;
                Boolean c = a < b;
            equation
            end Test;
            "#,
            "Test",
        );
    }

    /// MLS: "Boolean cannot be used with ordering operators"
    #[test]
    fn mls_3_5_boolean_less_equal_forbidden() {
        expect_failure(
            r#"
            model Test
                Boolean a = true;
                Boolean b = false;
                Boolean c = a <= b;
            equation
            end Test;
            "#,
            "Test",
        );
    }

    /// MLS: "Boolean cannot be used with ordering operators"
    #[test]
    fn mls_3_5_boolean_greater_than_forbidden() {
        expect_failure(
            r#"
            model Test
                Boolean a = true;
                Boolean b = false;
                Boolean c = a > b;
            equation
            end Test;
            "#,
            "Test",
        );
    }

    /// MLS: "Boolean cannot be used with ordering operators"
    #[test]
    fn mls_3_5_boolean_greater_equal_forbidden() {
        expect_failure(
            r#"
            model Test
                Boolean a = true;
                Boolean b = false;
                Boolean c = a >= b;
            equation
            end Test;
            "#,
            "Test",
        );
    }

    /// Boolean equality IS allowed (should pass)
    #[test]
    fn mls_3_5_boolean_equality_allowed() {
        expect_success(
            r#"
            model Test
                Boolean a = true;
                Boolean b = false;
                Boolean c = a == b;
                Boolean d = a <> b;
            equation
            end Test;
            "#,
            "Test",
        );
    }
}

// ============================================================================
// §3.5 LOGICAL OPERATOR TYPE RESTRICTIONS
// ============================================================================

/// MLS §3.5: Logical operators only work on Boolean operands
mod logical_operator_type_restriction {
    use super::*;

    /// MLS: "'and' requires Boolean operands"
    #[test]
    fn mls_3_5_and_with_integer_forbidden() {
        expect_failure(
            r#"
            model Test
                Integer a = 1;
                Integer b = 2;
                Boolean c = a and b;
            equation
            end Test;
            "#,
            "Test",
        );
    }

    /// MLS: "'and' requires Boolean operands"
    #[test]
    fn mls_3_5_and_with_real_forbidden() {
        expect_failure(
            r#"
            model Test
                Real a = 1.0;
                Real b = 2.0;
                Boolean c = a and b;
            equation
            end Test;
            "#,
            "Test",
        );
    }

    /// MLS: "'or' requires Boolean operands"
    #[test]
    fn mls_3_5_or_with_integer_forbidden() {
        expect_failure(
            r#"
            model Test
                Integer a = 1;
                Integer b = 0;
                Boolean c = a or b;
            equation
            end Test;
            "#,
            "Test",
        );
    }

    /// MLS: "'or' requires Boolean operands"
    #[test]
    fn mls_3_5_or_with_real_forbidden() {
        expect_failure(
            r#"
            model Test
                Real a = 1.0;
                Real b = 0.0;
                Boolean c = a or b;
            equation
            end Test;
            "#,
            "Test",
        );
    }

    /// MLS: "'not' requires Boolean operand"
    #[test]
    fn mls_3_5_not_with_integer_forbidden() {
        expect_failure(
            r#"
            model Test
                Integer a = 1;
                Boolean b = not a;
            equation
            end Test;
            "#,
            "Test",
        );
    }

    /// MLS: "'not' requires Boolean operand"
    #[test]
    fn mls_3_5_not_with_real_forbidden() {
        expect_failure(
            r#"
            model Test
                Real a = 1.0;
                Boolean b = not a;
            equation
            end Test;
            "#,
            "Test",
        );
    }

    /// MLS: "Cannot use 'and' with String"
    #[test]
    fn mls_3_5_and_with_string_forbidden() {
        expect_failure(
            r#"
            model Test
                String a = "hello";
                String b = "world";
                Boolean c = a and b;
            equation
            end Test;
            "#,
            "Test",
        );
    }

    /// Valid: Boolean with logical operators
    #[test]
    fn mls_3_5_logical_with_boolean_allowed() {
        expect_success(
            r#"
            model Test
                Boolean a = true;
                Boolean b = false;
                Boolean c = a and b;
                Boolean d = a or b;
                Boolean e = not a;
            equation
            end Test;
            "#,
            "Test",
        );
    }
}

// ============================================================================
// §3.5 RELATIONAL OPERATOR TYPE COMPATIBILITY
// ============================================================================

/// MLS §3.5: Relational operators require compatible types
mod relational_type_compatibility {
    use super::*;

    /// MLS: "Cannot compare Real to String"
    #[test]
    fn mls_3_5_real_string_comparison_forbidden() {
        expect_failure(
            r#"
            model Test
                Real a = 1.0;
                String b = "hello";
                Boolean c = a == b;
            equation
            end Test;
            "#,
            "Test",
        );
    }

    /// MLS: "Cannot compare Integer to String"
    #[test]
    fn mls_3_5_integer_string_comparison_forbidden() {
        expect_failure(
            r#"
            model Test
                Integer a = 1;
                String b = "hello";
                Boolean c = a == b;
            equation
            end Test;
            "#,
            "Test",
        );
    }

    /// MLS: "Cannot compare Boolean to Integer"
    #[test]
    fn mls_3_5_boolean_integer_comparison_forbidden() {
        expect_failure(
            r#"
            model Test
                Boolean a = true;
                Integer b = 1;
                Boolean c = a == b;
            equation
            end Test;
            "#,
            "Test",
        );
    }

    /// MLS: "Cannot compare Boolean to Real"
    #[test]
    fn mls_3_5_boolean_real_comparison_forbidden() {
        expect_failure(
            r#"
            model Test
                Boolean a = true;
                Real b = 1.0;
                Boolean c = a == b;
            equation
            end Test;
            "#,
            "Test",
        );
    }

    /// Valid: Integer and Real comparison (Integer promoted to Real)
    #[test]
    fn mls_3_5_integer_real_comparison_allowed() {
        expect_success(
            r#"
            model Test
                Integer a = 1;
                Real b = 1.5;
                Boolean c = a < b;
                Boolean d = a == b;
            equation
            end Test;
            "#,
            "Test",
        );
    }

    /// Valid: Same type comparisons
    #[test]
    fn mls_3_5_same_type_comparison_allowed() {
        expect_success(
            r#"
            model Test
                Real a = 1.0;
                Real b = 2.0;
                Integer i = 1;
                Integer j = 2;
                String s1 = "a";
                String s2 = "b";
                Boolean eq1 = a == b;
                Boolean eq2 = i == j;
                Boolean eq3 = s1 == s2;
            equation
            end Test;
            "#,
            "Test",
        );
    }
}

// ============================================================================
// §3.6 IF-EXPRESSION TYPE RESTRICTIONS
// ============================================================================

/// MLS §3.6: If-expression branches must have the same type
mod if_expression_type_restriction {
    use super::*;

    /// MLS §6: Integer can be coerced to Real in if-expression branches
    #[test]
    fn mls_3_6_if_real_vs_integer_allowed() {
        // Real and Integer branches are allowed via Integer->Real promotion (MLS §6)
        expect_success(
            r#"
            model Test
                Boolean c = true;
                Real x = if c then 1.0 else 1;
            equation
            end Test;
            "#,
            "Test",
        );
    }

    /// MLS: "Both branches of if-expression must have same type"
    #[test]
    fn mls_3_6_if_real_vs_string_forbidden() {
        expect_failure(
            r#"
            model Test
                Boolean c = true;
                Real x = if c then 1.0 else "hello";
            equation
            end Test;
            "#,
            "Test",
        );
    }

    /// MLS: "Both branches of if-expression must have same type"
    #[test]
    fn mls_3_6_if_boolean_vs_integer_forbidden() {
        expect_failure(
            r#"
            model Test
                Boolean c = true;
                Boolean x = if c then true else 1;
            equation
            end Test;
            "#,
            "Test",
        );
    }

    /// MLS: "If-expression condition must be Boolean"
    #[test]
    fn mls_3_6_if_condition_not_boolean_forbidden() {
        expect_failure(
            r#"
            model Test
                Integer c = 1;
                Real x = if c then 1.0 else 2.0;
            equation
            end Test;
            "#,
            "Test",
        );
    }

    /// MLS: "If-expression condition must be Boolean"
    #[test]
    fn mls_3_6_if_condition_real_forbidden() {
        expect_failure(
            r#"
            model Test
                Real c = 1.0;
                Real x = if c then 1.0 else 2.0;
            equation
            end Test;
            "#,
            "Test",
        );
    }

    /// Valid: Same type branches with Boolean condition
    #[test]
    fn mls_3_6_if_same_type_branches_allowed() {
        expect_success(
            r#"
            model Test
                Boolean c = true;
                Real x = if c then 1.0 else 2.0;
                Integer n = if c then 1 else 2;
                String s = if c then "yes" else "no";
                Boolean b = if c then true else false;
            equation
            end Test;
            "#,
            "Test",
        );
    }

    /// MLS: Array branches must have same dimensions
    #[test]
    fn mls_3_6_if_array_dimension_mismatch_forbidden() {
        expect_failure(
            r#"
            model Test
                Boolean c = true;
                Real x[3] = if c then {1, 2, 3} else {4, 5};
            equation
            end Test;
            "#,
            "Test",
        );
    }

    /// Valid: Same dimension array branches
    #[test]
    fn mls_3_6_if_array_same_dimension_allowed() {
        expect_success(
            r#"
            model Test
                Boolean c = true;
                Real x[3] = if c then {1, 2, 3} else {4, 5, 6};
            equation
            end Test;
            "#,
            "Test",
        );
    }
}

// ============================================================================
// §3.4 ARITHMETIC OPERATOR TYPE RESTRICTIONS
// ============================================================================

/// MLS §3.4: Arithmetic operators require numeric operands
mod arithmetic_type_restriction {
    use super::*;

    /// MLS: "Cannot add String and Real"
    #[test]
    fn mls_3_4_add_string_real_forbidden() {
        expect_failure(
            r#"
            model Test
                String a = "hello";
                Real b = 1.0;
                String c = a + b;
            equation
            end Test;
            "#,
            "Test",
        );
    }

    /// MLS: "Cannot subtract Boolean"
    #[test]
    fn mls_3_4_subtract_boolean_forbidden() {
        expect_failure(
            r#"
            model Test
                Boolean a = true;
                Boolean b = false;
                Boolean c = a - b;
            equation
            end Test;
            "#,
            "Test",
        );
    }

    /// MLS: "Cannot multiply Boolean"
    #[test]
    fn mls_3_4_multiply_boolean_forbidden() {
        expect_failure(
            r#"
            model Test
                Boolean a = true;
                Boolean b = false;
                Boolean c = a * b;
            equation
            end Test;
            "#,
            "Test",
        );
    }

    /// MLS: "Cannot divide String"
    #[test]
    fn mls_3_4_divide_string_forbidden() {
        expect_failure(
            r#"
            model Test
                String a = "hello";
                String b = "world";
                String c = a / b;
            equation
            end Test;
            "#,
            "Test",
        );
    }

    /// MLS: "Cannot exponentiate String"
    #[test]
    fn mls_3_4_power_string_forbidden() {
        expect_failure(
            r#"
            model Test
                String a = "hello";
                Integer b = 2;
                String c = a ^ b;
            equation
            end Test;
            "#,
            "Test",
        );
    }

    /// Valid: String concatenation with +
    #[test]
    fn mls_3_4_string_concat_allowed() {
        expect_success(
            r#"
            model Test
                String a = "hello";
                String b = " world";
                String c = a + b;
            equation
            end Test;
            "#,
            "Test",
        );
    }

    /// Valid: Numeric arithmetic
    #[test]
    fn mls_3_4_numeric_arithmetic_allowed() {
        expect_success(
            r#"
            model Test
                Real a = 1.0;
                Real b = 2.0;
                Integer i = 3;
                Real c = a + b;
                Real d = a - b;
                Real e = a * b;
                Real f = a / b;
                Real g = a ^ i;
            equation
            end Test;
            "#,
            "Test",
        );
    }
}

// ============================================================================
// EMPTY ARRAY EXPRESSIONS
// ============================================================================

/// MLS §3: Empty array expressions
mod empty_array_expressions {
    use super::*;

    /// MLS: Empty array literal
    #[test]
    #[ignore = "Empty array literal {} syntax not yet supported"]
    fn mls_3_empty_array_expression() {
        expect_success(
            r#"
            model Test
                Real x[:] = {};
            equation
            end Test;
            "#,
            "Test",
        );
    }

    /// MLS: Empty 2D array
    #[test]
    #[ignore = "Empty 2D array not yet supported"]
    fn mls_3_empty_2d_array() {
        expect_success(
            r#"
            model Test
                Real x[:, :] = {{}, {}};
            equation
            end Test;
            "#,
            "Test",
        );
    }

    /// MLS: Empty array with parameter size
    #[test]
    fn mls_3_empty_param_array() {
        expect_success(
            r#"
            model Test
                parameter Integer n = 0;
                Real x[n];
            equation
            end Test;
            "#,
            "Test",
        );
    }

    /// MLS: Empty array from fill
    #[test]
    fn mls_3_empty_fill() {
        expect_success(
            r#"
            model Test
                parameter Integer n = 0;
                Real x[n] = fill(0, n);
            equation
            end Test;
            "#,
            "Test",
        );
    }

    /// MLS: Empty array concatenation
    #[test]
    #[ignore = "Empty array concatenation not yet supported"]
    fn mls_3_empty_concat() {
        expect_success(
            r#"
            model Test
                Real x[:] = cat(1, {}, {1, 2, 3});
            equation
            end Test;
            "#,
            "Test",
        );
    }
}
