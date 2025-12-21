//! MLS §3: Operators and Expressions - Detailed Conformance Tests
//!
//! Comprehensive tests covering normative statements from MLS §3 including:
//! - §3.1: Expression restrictions
//! - §3.2: Precedence edge cases
//! - §3.4: Arithmetic operator restrictions
//! - §3.5: Type restrictions for operators
//! - §3.7: Built-in function restrictions
//!
//! Reference: https://specification.modelica.org/master/operatorsandexpressions.html

use crate::spec::{expect_failure, expect_parse_success, expect_success};

// ============================================================================
// §3.1 EXPRESSION RESTRICTIONS
// ============================================================================

/// MLS §3.1: Expression normative requirements
mod expression_restrictions {
    use super::*;

    /// Range expressions
    #[test]
    fn mls_3_1_range_expression() {
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

    /// Range with step
    #[test]
    fn mls_3_1_range_with_step() {
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

    /// Negative step range
    #[test]
    fn mls_3_1_range_negative_step() {
        expect_parse_success(
            r#"
            function F
                input Integer n;
                output Integer s;
            algorithm
                s := 0;
                for i in n:-1:1 loop
                    s := s + i;
                end for;
            end F;
            "#,
        );
    }

    /// Array constructor expression
    #[test]
    fn mls_3_1_array_constructor() {
        expect_success(
            r#"
            model Test
                Real x[3] = {1, 2, 3};
                Real y[2, 2] = {{1, 2}, {3, 4}};
            equation
            end Test;
            "#,
            "Test",
        );
    }

    /// Array comprehension expression
    #[test]
    fn mls_3_1_array_comprehension() {
        expect_success(
            r#"
            model Test
                Real x[5] = {i for i in 1:5};
            equation
            end Test;
            "#,
            "Test",
        );
    }

    /// Nested array comprehension
    #[test]
    fn mls_3_1_nested_comprehension() {
        expect_success(
            r#"
            model Test
                Real A[3, 3] = {i * j for i in 1:3, j in 1:3};
            equation
            end Test;
            "#,
            "Test",
        );
    }
}

// ============================================================================
// §3.4 ARITHMETIC OPERATOR RESTRICTIONS
// ============================================================================

/// MLS §3.4: Arithmetic operator normative requirements
mod arithmetic_restrictions {
    use super::*;

    /// Element-wise operations on arrays
    #[test]
    fn mls_3_4_elementwise_add() {
        expect_success(
            r#"
            model Test
                Real a[3] = {1, 2, 3};
                Real b[3] = {4, 5, 6};
                Real c[3] = a .+ b;
            equation
            end Test;
            "#,
            "Test",
        );
    }

    /// Element-wise multiplication
    #[test]
    fn mls_3_4_elementwise_mult() {
        expect_success(
            r#"
            model Test
                Real a[3] = {1, 2, 3};
                Real b[3] = {4, 5, 6};
                Real c[3] = a .* b;
            equation
            end Test;
            "#,
            "Test",
        );
    }

    /// Element-wise division
    #[test]
    fn mls_3_4_elementwise_div() {
        expect_success(
            r#"
            model Test
                Real a[3] = {4, 6, 8};
                Real b[3] = {2, 2, 2};
                Real c[3] = a ./ b;
            equation
            end Test;
            "#,
            "Test",
        );
    }

    /// Element-wise power
    #[test]
    fn mls_3_4_elementwise_power() {
        expect_success(
            r#"
            model Test
                Real a[3] = {2, 3, 4};
                Real b[3] = a .^ 2;
            equation
            end Test;
            "#,
            "Test",
        );
    }

    /// Matrix multiplication
    #[test]
    #[ignore = "Matrix multiplication dimension inference not yet implemented"]
    fn mls_3_4_matrix_mult() {
        expect_success(
            r#"
            model Test
                Real A[2, 3] = {{1, 2, 3}, {4, 5, 6}};
                Real B[3, 2] = {{1, 2}, {3, 4}, {5, 6}};
                Real C[2, 2] = A * B;
            equation
            end Test;
            "#,
            "Test",
        );
    }

    /// Vector dot product
    #[test]
    fn mls_3_4_vector_dot() {
        expect_success(
            r#"
            model Test
                Real a[3] = {1, 2, 3};
                Real b[3] = {4, 5, 6};
                Real c = a * b;
            equation
            end Test;
            "#,
            "Test",
        );
    }

    /// Scalar-array multiplication
    #[test]
    fn mls_3_4_scalar_array_mult() {
        expect_success(
            r#"
            model Test
                Real k = 2;
                Real a[3] = {1, 2, 3};
                Real b[3] = k * a;
            equation
            end Test;
            "#,
            "Test",
        );
    }

    /// Array-scalar multiplication
    #[test]
    fn mls_3_4_array_scalar_mult() {
        expect_success(
            r#"
            model Test
                Real a[3] = {1, 2, 3};
                Real k = 2;
                Real b[3] = a * k;
            equation
            end Test;
            "#,
            "Test",
        );
    }
}

// ============================================================================
// §3.5 TYPE RESTRICTIONS FOR OPERATORS
// ============================================================================

/// MLS §3.5: Type restrictions for relational and logical operators
mod type_restrictions {
    use super::*;

    /// MLS: "Relational operators require compatible types"
    #[test]
    fn mls_3_5_real_comparison() {
        expect_success(
            r#"
            model Test
                Real a = 1.5;
                Real b = 2.5;
                Boolean less = a < b;
                Boolean equal = a == b;
            equation
            end Test;
            "#,
            "Test",
        );
    }

    /// Integer comparisons
    #[test]
    fn mls_3_5_integer_comparison() {
        expect_success(
            r#"
            model Test
                Integer a = 1;
                Integer b = 2;
                Boolean less = a < b;
                Boolean greater = a > b;
            equation
            end Test;
            "#,
            "Test",
        );
    }

    /// String equality
    #[test]
    fn mls_3_5_string_equality() {
        expect_success(
            r#"
            model Test
                String a = "hello";
                String b = "world";
                Boolean eq = a == b;
                Boolean neq = a <> b;
            equation
            end Test;
            "#,
            "Test",
        );
    }

    /// Boolean operations
    #[test]
    fn mls_3_5_boolean_operations() {
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

    /// MLS: "Cannot compare Boolean with <, >, etc"
    #[test]
    #[ignore = "Boolean ordering restriction not yet enforced"]
    fn error_boolean_ordering() {
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

    /// MLS: "Cannot use 'and' with Real"
    #[test]
    #[ignore = "Logical operator type restriction not yet enforced"]
    fn error_and_with_real() {
        expect_failure(
            r#"
            model Test
                Real a = 1;
                Real b = 2;
                Boolean c = a and b;
            equation
            end Test;
            "#,
            "Test",
        );
    }
}

// ============================================================================
// §3.6 IF-EXPRESSION RESTRICTIONS
// ============================================================================

/// MLS §3.6: If-expression normative requirements
mod if_expression_restrictions {
    use super::*;

    /// If-expression with Real result
    #[test]
    fn mls_3_6_if_real() {
        expect_success(
            r#"
            model Test
                Real x = 5;
                Real y = if x > 0 then 1 else -1;
            equation
            end Test;
            "#,
            "Test",
        );
    }

    /// If-expression with Integer result
    #[test]
    fn mls_3_6_if_integer() {
        expect_success(
            r#"
            model Test
                Integer x = 5;
                Integer y = if x > 0 then 1 else -1;
            equation
            end Test;
            "#,
            "Test",
        );
    }

    /// If-expression with array result
    #[test]
    fn mls_3_6_if_array() {
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

    /// Multiple elseif branches
    #[test]
    fn mls_3_6_multiple_elseif() {
        expect_success(
            r#"
            model Test
                Integer n = 2;
                Real x = if n == 1 then 1.0
                         elseif n == 2 then 2.0
                         elseif n == 3 then 3.0
                         else 0.0;
            equation
            end Test;
            "#,
            "Test",
        );
    }

    /// MLS: "Both branches must have same type"
    #[test]
    #[ignore = "If-expression branch type checking not yet enforced"]
    fn error_if_mismatched_types() {
        expect_failure(
            r#"
            model Test
                Boolean c = true;
                Real x = if c then 1 else "hello";
            equation
            end Test;
            "#,
            "Test",
        );
    }
}

// ============================================================================
// §3.7 BUILT-IN FUNCTION RESTRICTIONS
// ============================================================================

/// MLS §3.7: Built-in function normative requirements
mod builtin_restrictions {
    use super::*;

    /// pre() function
    #[test]
    fn mls_3_7_pre() {
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

    /// edge() function
    #[test]
    fn mls_3_7_edge() {
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

    /// change() function
    #[test]
    fn mls_3_7_change() {
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

    /// initial() function
    #[test]
    fn mls_3_7_initial() {
        expect_success(
            r#"
            model Test
                Real x(start = 0);
                discrete Integer n(start = 0);
            equation
                der(x) = 1;
                when initial() then
                    n = 1;
                end when;
            end Test;
            "#,
            "Test",
        );
    }

    /// terminal() function
    #[test]
    fn mls_3_7_terminal() {
        expect_parse_success(
            r#"
            model Test
                Real x(start = 0);
            equation
                der(x) = 1;
                when terminal() then
                    // Clean up at simulation end
                end when;
            end Test;
            "#,
        );
    }

    /// noEvent() function
    #[test]
    fn mls_3_7_no_event() {
        expect_success(
            r#"
            model Test
                Real x(start = 0);
                Real y;
            equation
                der(x) = 1;
                y = noEvent(if x > 0 then x else 0);
            end Test;
            "#,
            "Test",
        );
    }

    /// smooth() function
    #[test]
    fn mls_3_7_smooth() {
        expect_success(
            r#"
            model Test
                Real x(start = 0);
                Real y;
            equation
                der(x) = 1;
                y = smooth(0, if x > 0 then x else 0);
            end Test;
            "#,
            "Test",
        );
    }

    /// sample() function
    #[test]
    fn mls_3_7_sample() {
        expect_success(
            r#"
            model Test
                Real x(start = 0);
                discrete Real sampled(start = 0);
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

    /// delay() function
    #[test]
    fn mls_3_7_delay() {
        expect_success(
            r#"
            model Test
                Real x(start = 0);
                Real y;
            equation
                der(x) = 1;
                y = delay(x, 0.1);
            end Test;
            "#,
            "Test",
        );
    }

    /// reinit() function
    #[test]
    fn mls_3_7_reinit() {
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

    /// assert() function
    #[test]
    fn mls_3_7_assert() {
        expect_success(
            r#"
            model Test
                parameter Real k = 1;
            equation
                assert(k > 0, "k must be positive");
            end Test;
            "#,
            "Test",
        );
    }

    /// terminate() function
    #[test]
    fn mls_3_7_terminate() {
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
// §3.7 ARRAY FUNCTIONS
// ============================================================================

/// MLS §3.7: Array built-in functions
mod array_functions {
    use super::*;

    /// size() function
    #[test]
    fn mls_3_7_size() {
        expect_success(
            r#"
            model Test
                Real x[5] = {1, 2, 3, 4, 5};
                Integer n = size(x, 1);
            equation
            end Test;
            "#,
            "Test",
        );
    }

    /// ndims() function
    #[test]
    fn mls_3_7_ndims() {
        expect_success(
            r#"
            model Test
                Real x[2, 3] = {{1, 2, 3}, {4, 5, 6}};
                Integer n = ndims(x);
            equation
            end Test;
            "#,
            "Test",
        );
    }

    /// sum() function
    #[test]
    fn mls_3_7_sum() {
        expect_success(
            r#"
            model Test
                Real x[5] = {1, 2, 3, 4, 5};
                Real s = sum(x);
            equation
            end Test;
            "#,
            "Test",
        );
    }

    /// product() function
    #[test]
    fn mls_3_7_product() {
        expect_success(
            r#"
            model Test
                Real x[4] = {1, 2, 3, 4};
                Real p = product(x);
            equation
            end Test;
            "#,
            "Test",
        );
    }

    /// zeros() function
    #[test]
    fn mls_3_7_zeros() {
        expect_success(
            r#"
            model Test
                Real x[5] = zeros(5);
            equation
            end Test;
            "#,
            "Test",
        );
    }

    /// ones() function
    #[test]
    fn mls_3_7_ones() {
        expect_success(
            r#"
            model Test
                Real x[5] = ones(5);
            equation
            end Test;
            "#,
            "Test",
        );
    }

    /// identity() function
    #[test]
    fn mls_3_7_identity() {
        expect_success(
            r#"
            model Test
                Real I[3, 3] = identity(3);
            equation
            end Test;
            "#,
            "Test",
        );
    }

    /// diagonal() function
    #[test]
    fn mls_3_7_diagonal() {
        expect_success(
            r#"
            model Test
                Real v[3] = {1, 2, 3};
                Real D[3, 3] = diagonal(v);
            equation
            end Test;
            "#,
            "Test",
        );
    }

    /// transpose() function
    #[test]
    #[ignore = "transpose() function dimension inference not yet implemented"]
    fn mls_3_7_transpose() {
        expect_success(
            r#"
            model Test
                Real A[2, 3] = {{1, 2, 3}, {4, 5, 6}};
                Real B[3, 2] = transpose(A);
            equation
            end Test;
            "#,
            "Test",
        );
    }

    /// symmetric() function
    #[test]
    fn mls_3_7_symmetric() {
        expect_parse_success(
            r#"
            model Test
                Real A[3, 3] = {{1, 2, 3}, {2, 4, 5}, {3, 5, 6}};
                Real B[3, 3] = symmetric(A);
            equation
            end Test;
            "#,
        );
    }

    /// cross() function for 3D vectors
    #[test]
    fn mls_3_7_cross() {
        expect_success(
            r#"
            model Test
                Real a[3] = {1, 0, 0};
                Real b[3] = {0, 1, 0};
                Real c[3] = cross(a, b);
            equation
            end Test;
            "#,
            "Test",
        );
    }

    /// skew() function
    #[test]
    #[ignore = "skew() function not yet implemented"]
    fn mls_3_7_skew() {
        expect_success(
            r#"
            model Test
                Real v[3] = {1, 2, 3};
                Real S[3, 3] = skew(v);
            equation
            end Test;
            "#,
            "Test",
        );
    }

    /// cat() function
    #[test]
    #[ignore = "cat() function not yet implemented"]
    fn mls_3_7_cat() {
        expect_success(
            r#"
            model Test
                Real a[3] = {1, 2, 3};
                Real b[2] = {4, 5};
                Real c[5] = cat(1, a, b);
            equation
            end Test;
            "#,
            "Test",
        );
    }

    /// vector() function
    #[test]
    fn mls_3_7_vector() {
        expect_success(
            r#"
            model Test
                Real A[2, 2] = {{1, 2}, {3, 4}};
                Real v[4] = vector(A);
            equation
            end Test;
            "#,
            "Test",
        );
    }

    /// matrix() function
    #[test]
    fn mls_3_7_matrix() {
        expect_success(
            r#"
            model Test
                Real v[3] = {1, 2, 3};
                Real M[3, 1] = matrix(v);
            equation
            end Test;
            "#,
            "Test",
        );
    }

    /// scalar() function
    #[test]
    fn mls_3_7_scalar() {
        expect_success(
            r#"
            model Test
                Real A[1, 1] = {{5}};
                Real x = scalar(A);
            equation
            end Test;
            "#,
            "Test",
        );
    }
}

// ============================================================================
// COMPLEX EXPRESSION SCENARIOS
// ============================================================================

/// Complex expression scenarios
mod complex_scenarios {
    use super::*;

    /// Mixed operators and functions
    #[test]
    fn complex_mixed_expression() {
        expect_success(
            r#"
            model Test
                Real x = 1;
                Real y = sin(x) * cos(x) + sqrt(abs(x));
            equation
            end Test;
            "#,
            "Test",
        );
    }

    /// Conditional with array operations
    #[test]
    fn complex_conditional_array() {
        expect_success(
            r#"
            model Test
                Boolean use_zeros = true;
                Real x[3] = if use_zeros then zeros(3) else ones(3);
            equation
            end Test;
            "#,
            "Test",
        );
    }

    /// Nested function calls
    #[test]
    fn complex_nested_functions() {
        expect_success(
            r#"
            model Test
                Real x = 0.5;
                Real y = sin(cos(tan(x)));
            equation
            end Test;
            "#,
            "Test",
        );
    }

    /// Array reduction operations
    #[test]
    fn complex_array_reductions() {
        expect_success(
            r#"
            model Test
                Real x[5] = {1, 2, 3, 4, 5};
                Real total = sum(x);
                Real prod = product(x);
                Real minVal = min(x);
                Real maxVal = max(x);
            equation
            end Test;
            "#,
            "Test",
        );
    }
}
