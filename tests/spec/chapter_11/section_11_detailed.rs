//! MLS §11: Statements and Algorithm Sections - Detailed Conformance Tests
//!
//! Comprehensive tests covering normative statements from MLS §11 including:
//! - §11.1: Algorithm section restrictions
//! - §11.2.1: Assignment statement restrictions
//! - §11.2.2: If-statement requirements
//! - §11.2.3: For-statement loop variable restrictions
//! - §11.2.4: While-statement requirements
//! - §11.2.5: When-statement restrictions in algorithms
//! - §11.2.6-7: Break and return statement restrictions
//!
//! Reference: https://specification.modelica.org/master/statements-and-algorithm-sections.html

use crate::spec::{expect_failure, expect_parse_success, expect_success};

// ============================================================================
// §11.1 ALGORITHM SECTION RESTRICTIONS
// ============================================================================

/// MLS §11.1: Algorithm section normative requirements
mod algorithm_section_restrictions {
    use super::*;

    /// Algorithm section in function
    #[test]
    fn mls_11_1_algorithm_in_function() {
        expect_success(
            r#"
            function F
                input Real x;
                output Real y;
            algorithm
                y := x * 2;
            end F;

            model Test
                Real y = F(2);
            equation
            end Test;
            "#,
            "Test",
        );
    }

    /// Algorithm section in model
    #[test]
    fn mls_11_1_algorithm_in_model() {
        expect_success(
            r#"
            model Test
                Real x;
                Real y;
            algorithm
                x := 1;
                y := x + 1;
            equation
            end Test;
            "#,
            "Test",
        );
    }

    /// Initial algorithm section
    #[test]
    fn mls_11_1_initial_algorithm() {
        expect_success(
            r#"
            model Test
                Real x(start = 0);
                Real initialized;
            initial algorithm
                initialized := 42;
            equation
                der(x) = 1;
            end Test;
            "#,
            "Test",
        );
    }

    /// Multiple algorithm sections contribute separate equations
    #[test]
    fn mls_11_1_multiple_algorithm_sections() {
        expect_success(
            r#"
            model Test
                Real a;
                Real b;
            algorithm
                a := 1;
            algorithm
                b := 2;
            equation
            end Test;
            "#,
            "Test",
        );
    }

    /// Algorithm section with protected local variables
    #[test]
    fn mls_11_1_algorithm_with_protected() {
        expect_success(
            r#"
            function F
                input Real x;
                output Real y;
            protected
                Real temp;
            algorithm
                temp := x * x;
                y := temp + 1;
            end F;

            model Test
                Real y = F(3);
            equation
            end Test;
            "#,
            "Test",
        );
    }
}

// ============================================================================
// §11.2.1 ASSIGNMENT STATEMENT RESTRICTIONS
// ============================================================================

/// MLS §11.2.1: Assignment statement normative requirements
mod assignment_restrictions {
    use super::*;

    /// MLS: "The left-hand side must be a variable reference"
    #[test]
    fn mls_11_2_1_variable_assignment() {
        expect_success(
            r#"
            function F
                input Real x;
                output Real y;
            algorithm
                y := x + 1;
            end F;

            model Test
                Real y = F(1);
            equation
            end Test;
            "#,
            "Test",
        );
    }

    /// Array element assignment
    #[test]
    fn mls_11_2_1_array_element_assignment() {
        expect_success(
            r#"
            function InitVec
                input Integer n;
                output Real v[3];
            algorithm
                for i in 1:3 loop
                    v[i] := i;
                end for;
            end InitVec;

            model Test
                Real v[3] = InitVec(3);
            equation
            end Test;
            "#,
            "Test",
        );
    }

    /// Slice assignment
    #[test]
    fn mls_11_2_1_slice_assignment() {
        expect_parse_success(
            r#"
            function F
                input Real x[5];
                output Real y[5];
            algorithm
                y := x;
                y[1:3] := {0, 0, 0};
            end F;
            "#,
        );
    }

    /// MLS: "Cannot assign to input variables"
    #[test]
    fn error_assign_to_input() {
        expect_failure(
            r#"
            function F
                input Real x;
                output Real y;
            algorithm
                x := 5;
                y := x;
            end F;
            "#,
            "F",
        );
    }

    /// MLS: "Cannot assign to constant"
    #[test]
    fn error_assign_to_constant() {
        expect_failure(
            r#"
            function F
                input Real x;
                output Real y;
            protected
                constant Real c = 1;
            algorithm
                c := 2;
                y := x + c;
            end F;
            "#,
            "F",
        );
    }

    /// Multiple outputs function call assignment
    #[test]
    fn mls_11_2_1_multiple_output_assignment() {
        expect_success(
            r#"
            function DivMod
                input Integer a;
                input Integer b;
                output Integer q;
                output Integer r;
            algorithm
                q := div(a, b);
                r := mod(a, b);
            end DivMod;

            model Test
                Integer q;
                Integer r;
            algorithm
                (q, r) := DivMod(10, 3);
            equation
            end Test;
            "#,
            "Test",
        );
    }
}

// ============================================================================
// §11.2.2 IF-STATEMENT REQUIREMENTS
// ============================================================================

/// MLS §11.2.2: If-statement normative requirements
mod if_statement_requirements {
    use super::*;

    /// If-statement basic structure
    #[test]
    fn mls_11_2_2_if_basic() {
        expect_success(
            r#"
            function Abs
                input Real x;
                output Real y;
            algorithm
                if x >= 0 then
                    y := x;
                else
                    y := -x;
                end if;
            end Abs;

            model Test
                Real y = Abs(-5);
            equation
            end Test;
            "#,
            "Test",
        );
    }

    /// If-statement with elseif
    #[test]
    fn mls_11_2_2_if_elseif() {
        expect_success(
            r#"
            function Sign
                input Real x;
                output Integer s;
            algorithm
                if x > 0 then
                    s := 1;
                elseif x < 0 then
                    s := -1;
                else
                    s := 0;
                end if;
            end Sign;

            model Test
                Integer s = Sign(-3);
            equation
            end Test;
            "#,
            "Test",
        );
    }

    /// Nested if-statements
    #[test]
    fn mls_11_2_2_nested_if() {
        expect_success(
            r#"
            function Classify
                input Real x;
                input Real y;
                output Integer quadrant;
            algorithm
                if x >= 0 then
                    if y >= 0 then
                        quadrant := 1;
                    else
                        quadrant := 4;
                    end if;
                else
                    if y >= 0 then
                        quadrant := 2;
                    else
                        quadrant := 3;
                    end if;
                end if;
            end Classify;

            model Test
                Integer q = Classify(1, -1);
            equation
            end Test;
            "#,
            "Test",
        );
    }

    /// If-statement without else
    #[test]
    fn mls_11_2_2_if_no_else() {
        expect_success(
            r#"
            function Clamp
                input Real x;
                input Real maxVal;
                output Real y;
            algorithm
                y := x;
                if y > maxVal then
                    y := maxVal;
                end if;
            end Clamp;

            model Test
                Real y = Clamp(10, 5);
            equation
            end Test;
            "#,
            "Test",
        );
    }
}

// ============================================================================
// §11.2.3 FOR-STATEMENT RESTRICTIONS
// ============================================================================

/// MLS §11.2.3: For-statement normative requirements
mod for_statement_restrictions {
    use super::*;

    /// For-loop with range
    #[test]
    fn mls_11_2_3_for_range() {
        expect_success(
            r#"
            function Sum
                input Integer n;
                output Integer s;
            algorithm
                s := 0;
                for i in 1:n loop
                    s := s + i;
                end for;
            end Sum;

            model Test
                Integer s = Sum(10);
            equation
            end Test;
            "#,
            "Test",
        );
    }

    /// For-loop with step
    #[test]
    fn mls_11_2_3_for_step() {
        expect_success(
            r#"
            function SumOdd
                input Integer n;
                output Integer s;
            algorithm
                s := 0;
                for i in 1:2:n loop
                    s := s + i;
                end for;
            end SumOdd;

            model Test
                Integer s = SumOdd(10);
            equation
            end Test;
            "#,
            "Test",
        );
    }

    /// MLS: "Loop variable is local to the for-loop"
    #[test]
    fn mls_11_2_3_loop_variable_scope() {
        expect_parse_success(
            r#"
            function F
                input Integer n;
                output Integer s;
            algorithm
                s := 0;
                for i in 1:n loop
                    s := s + i;
                end for;
            end F;
            "#,
        );
    }

    /// MLS: "Cannot assign to loop variable"
    #[test]
    #[ignore = "Loop variable assignment restriction not yet enforced"]
    fn error_assign_to_loop_variable() {
        expect_failure(
            r#"
            function F
                input Integer n;
                output Integer s;
            algorithm
                s := 0;
                for i in 1:n loop
                    i := i + 1;
                    s := s + i;
                end for;
            end F;
            "#,
            "F",
        );
    }

    /// Nested for-loops
    #[test]
    fn mls_11_2_3_nested_for() {
        expect_success(
            r#"
            function MatrixInit
                input Integer m;
                input Integer n;
                output Real A[3, 3];
            algorithm
                for i in 1:3 loop
                    for j in 1:3 loop
                        A[i, j] := i * 10 + j;
                    end for;
                end for;
            end MatrixInit;

            model Test
                Real A[3, 3] = MatrixInit(3, 3);
            equation
            end Test;
            "#,
            "Test",
        );
    }

    /// For-loop with negative step
    #[test]
    fn mls_11_2_3_for_negative_step() {
        expect_success(
            r#"
            function Countdown
                input Integer n;
                output Integer s;
            algorithm
                s := 0;
                for i in n:-1:1 loop
                    s := s + i;
                end for;
            end Countdown;

            model Test
                Integer s = Countdown(5);
            equation
            end Test;
            "#,
            "Test",
        );
    }
}

// ============================================================================
// §11.2.4 WHILE-STATEMENT REQUIREMENTS
// ============================================================================

/// MLS §11.2.4: While-statement normative requirements
mod while_statement_requirements {
    use super::*;

    /// While-loop basic
    #[test]
    fn mls_11_2_4_while_basic() {
        expect_success(
            r#"
            function Factorial
                input Integer n;
                output Integer f;
            protected
                Integer i;
            algorithm
                f := 1;
                i := n;
                while i > 1 loop
                    f := f * i;
                    i := i - 1;
                end while;
            end Factorial;

            model Test
                Integer f = Factorial(5);
            equation
            end Test;
            "#,
            "Test",
        );
    }

    /// While-loop with compound condition
    #[test]
    fn mls_11_2_4_while_compound_condition() {
        expect_success(
            r#"
            function BoundedSum
                input Integer limit;
                output Integer s;
            protected
                Integer i;
            algorithm
                s := 0;
                i := 1;
                while i <= 100 and s < limit loop
                    s := s + i;
                    i := i + 1;
                end while;
            end BoundedSum;

            model Test
                Integer s = BoundedSum(50);
            equation
            end Test;
            "#,
            "Test",
        );
    }

    /// Nested while-loops
    #[test]
    fn mls_11_2_4_nested_while() {
        expect_parse_success(
            r#"
            function GCD
                input Integer a_in;
                input Integer b_in;
                output Integer g;
            protected
                Integer a;
                Integer b;
                Integer t;
            algorithm
                a := a_in;
                b := b_in;
                while b <> 0 loop
                    t := b;
                    b := mod(a, b);
                    a := t;
                end while;
                g := a;
            end GCD;
            "#,
        );
    }
}

// ============================================================================
// §11.2.5 WHEN-STATEMENT RESTRICTIONS
// ============================================================================

/// MLS §11.2.5: When-statement normative requirements in algorithms
mod when_statement_restrictions {
    use super::*;

    /// When-statement in algorithm section
    #[test]
    fn mls_11_2_5_when_in_algorithm() {
        expect_success(
            r#"
            model Test
                Real x(start = 0);
                discrete Integer count(start = 0);
            algorithm
                when x > 1 then
                    count := pre(count) + 1;
                end when;
            equation
                der(x) = 1;
            end Test;
            "#,
            "Test",
        );
    }

    /// When-statement with elsewhen
    #[test]
    fn mls_11_2_5_when_elsewhen() {
        expect_success(
            r#"
            model Test
                Real x(start = 0);
                discrete Integer state(start = 0);
            algorithm
                when x > 2 then
                    state := 2;
                elsewhen x > 1 then
                    state := 1;
                end when;
            equation
                der(x) = 1;
            end Test;
            "#,
            "Test",
        );
    }

    /// MLS: "When-statement can only assign to discrete variables"
    #[test]
    #[ignore = "When-statement discrete restriction not yet enforced"]
    fn error_when_assigns_continuous() {
        expect_failure(
            r#"
            model Test
                Real x(start = 0);
                Real y;
            algorithm
                when x > 1 then
                    y := 1;
                end when;
            equation
                der(x) = 1;
            end Test;
            "#,
            "Test",
        );
    }

    /// MLS: "When-statement cannot be nested"
    #[test]

    fn error_nested_when() {
        expect_failure(
            r#"
            model Test
                Real x(start = 0);
                discrete Integer a(start = 0);
            algorithm
                when x > 1 then
                    when x > 2 then
                        a := 1;
                    end when;
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
// §11.2.6 BREAK STATEMENT RESTRICTIONS
// ============================================================================

/// MLS §11.2.6: Break statement normative requirements
mod break_statement_restrictions {
    use super::*;

    /// Break in for-loop
    #[test]
    fn mls_11_2_6_break_in_for() {
        expect_success(
            r#"
            function FindFirst
                input Real x[5];
                input Real target;
                output Integer idx;
            algorithm
                idx := 0;
                for i in 1:5 loop
                    if x[i] == target then
                        idx := i;
                        break;
                    end if;
                end for;
            end FindFirst;

            model Test
                Integer idx = FindFirst({1, 2, 3, 4, 5}, 3);
            equation
            end Test;
            "#,
            "Test",
        );
    }

    /// Break in while-loop
    #[test]
    fn mls_11_2_6_break_in_while() {
        expect_success(
            r#"
            function CountTo
                input Integer max;
                output Integer count;
            algorithm
                count := 0;
                while true loop
                    count := count + 1;
                    if count >= max then
                        break;
                    end if;
                end while;
            end CountTo;

            model Test
                Integer c = CountTo(10);
            equation
            end Test;
            "#,
            "Test",
        );
    }

    /// MLS: "Break only valid in for or while loop"
    #[test]
    fn error_break_outside_loop() {
        expect_failure(
            r#"
            function F
                input Real x;
                output Real y;
            algorithm
                y := x;
                break;
            end F;
            "#,
            "F",
        );
    }
}

// ============================================================================
// §11.2.7 RETURN STATEMENT RESTRICTIONS
// ============================================================================

/// MLS §11.2.7: Return statement normative requirements
mod return_statement_restrictions {
    use super::*;

    /// Return in function
    #[test]
    fn mls_11_2_7_return_in_function() {
        expect_success(
            r#"
            function SafeDiv
                input Real x;
                input Real y;
                output Real z;
            algorithm
                if y == 0 then
                    z := 0;
                    return;
                end if;
                z := x / y;
            end SafeDiv;

            model Test
                Real z = SafeDiv(10, 2);
            equation
            end Test;
            "#,
            "Test",
        );
    }

    /// Return with multiple outputs
    #[test]
    fn mls_11_2_7_return_multiple_outputs() {
        expect_success(
            r#"
            function MinMax
                input Real x;
                input Real y;
                output Real minVal;
                output Real maxVal;
            algorithm
                if x <= y then
                    minVal := x;
                    maxVal := y;
                else
                    minVal := y;
                    maxVal := x;
                end if;
                return;
            end MinMax;

            model Test
                Real minV;
                Real maxV;
            algorithm
                (minV, maxV) := MinMax(5, 3);
            equation
            end Test;
            "#,
            "Test",
        );
    }

    /// MLS: "Return only valid in functions"
    #[test]
    #[ignore = "Return in model not yet detected"]
    fn error_return_in_model() {
        expect_failure(
            r#"
            model Test
                Real x;
            algorithm
                x := 1;
                return;
            equation
            end Test;
            "#,
            "Test",
        );
    }
}

// ============================================================================
// §11.2.8 ASSERT STATEMENT IN ALGORITHMS
// ============================================================================

/// MLS §11.2.8: Assert statement in algorithms
mod assert_in_algorithms {
    use super::*;

    /// Assert in algorithm
    #[test]
    fn mls_11_2_8_assert_basic() {
        expect_success(
            r#"
            function Sqrt
                input Real x;
                output Real y;
            algorithm
                assert(x >= 0, "sqrt requires non-negative input");
                y := x ^ 0.5;
            end Sqrt;

            model Test
                Real y = Sqrt(4);
            equation
            end Test;
            "#,
            "Test",
        );
    }

    /// Assert with assertion level
    #[test]
    fn mls_11_2_8_assert_with_level() {
        expect_success(
            r#"
            function Log
                input Real x;
                output Real y;
            algorithm
                assert(x > 0, "log requires positive input", AssertionLevel.error);
                y := log(x);
            end Log;

            model Test
                Real y = Log(2);
            equation
            end Test;
            "#,
            "Test",
        );
    }

    /// Assert warning level
    #[test]
    fn mls_11_2_8_assert_warning() {
        expect_success(
            r#"
            function F
                input Real x;
                output Real y;
            algorithm
                assert(x > 1e-6, "Value very small", AssertionLevel.warning);
                y := 1 / x;
            end F;

            model Test
                Real y = F(0.5);
            equation
            end Test;
            "#,
            "Test",
        );
    }
}

// ============================================================================
// COMPLEX ALGORITHM SCENARIOS
// ============================================================================

/// Complex algorithm scenarios
mod complex_scenarios {
    use super::*;

    /// Algorithm with multiple control structures
    #[test]
    fn complex_mixed_control_flow() {
        expect_success(
            r#"
            function ProcessArray
                input Real x[5];
                output Real sum;
                output Integer count;
            algorithm
                sum := 0;
                count := 0;
                for i in 1:5 loop
                    if x[i] > 0 then
                        sum := sum + x[i];
                        count := count + 1;
                    end if;
                end for;
            end ProcessArray;

            model Test
                Real sum;
                Integer count;
            algorithm
                (sum, count) := ProcessArray({1, -2, 3, -4, 5});
            equation
            end Test;
            "#,
            "Test",
        );
    }

    /// Recursive-like algorithm (iterative)
    #[test]
    fn complex_iterative_fibonacci() {
        expect_success(
            r#"
            function Fibonacci
                input Integer n;
                output Integer f;
            protected
                Integer a;
                Integer b;
                Integer temp;
                Integer i;
            algorithm
                if n <= 1 then
                    f := n;
                    return;
                end if;
                a := 0;
                b := 1;
                i := 2;
                while i <= n loop
                    temp := a + b;
                    a := b;
                    b := temp;
                    i := i + 1;
                end while;
                f := b;
            end Fibonacci;

            model Test
                Integer f = Fibonacci(10);
            equation
            end Test;
            "#,
            "Test",
        );
    }

    /// Algorithm section interacting with equations
    #[test]
    fn complex_algorithm_equation_interaction() {
        expect_success(
            r#"
            model Test
                Real x(start = 0);
                Real y;
                discrete Integer quadrant(start = 1);
            algorithm
                if x >= 0 and y >= 0 then
                    quadrant := 1;
                elseif x < 0 and y >= 0 then
                    quadrant := 2;
                elseif x < 0 and y < 0 then
                    quadrant := 3;
                else
                    quadrant := 4;
                end if;
            equation
                der(x) = 1;
                y = sin(x);
            end Test;
            "#,
            "Test",
        );
    }

    /// Matrix operations in algorithm
    #[test]
    fn complex_matrix_algorithm() {
        expect_success(
            r#"
            function MatrixMult
                input Real A[2, 2];
                input Real B[2, 2];
                output Real C[2, 2];
            algorithm
                for i in 1:2 loop
                    for j in 1:2 loop
                        C[i, j] := 0;
                        for k in 1:2 loop
                            C[i, j] := C[i, j] + A[i, k] * B[k, j];
                        end for;
                    end for;
                end for;
            end MatrixMult;

            model Test
                Real A[2, 2] = {{1, 2}, {3, 4}};
                Real B[2, 2] = {{5, 6}, {7, 8}};
                Real C[2, 2] = MatrixMult(A, B);
            equation
            end Test;
            "#,
            "Test",
        );
    }
}
