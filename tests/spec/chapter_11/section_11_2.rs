//! MLS §11.2: Statements
//!
//! Tests for:
//! - §11.2.1 Assignment statements
//! - §11.2.2 If-statements
//! - §11.2.3 For-statements
//! - §11.2.4 While-statements
//! - §11.2.5 When-statements
//! - §11.2.6 Break statement
//! - §11.2.7 Return statement
//! - §11.2.8 Assert statement
//!
//! Reference: https://specification.modelica.org/master/statements-and-algorithm-sections.html

use crate::spec::expect_parse_success;

// ============================================================================
// §11.2.1 ASSIGNMENT STATEMENTS
// ============================================================================

/// MLS §11.2.1: Assignment statements
mod section_11_2_1_assignment {
    use super::*;

    #[test]
    fn mls_11_2_1_simple_assignment() {
        expect_parse_success(
            r#"
            function F
                input Real x;
                output Real y;
            algorithm
                y := x;
            end F;
            "#,
        );
    }

    #[test]
    fn mls_11_2_1_expression_assignment() {
        expect_parse_success(
            r#"
            function F
                input Real x;
                output Real y;
            algorithm
                y := x * x + 2 * x + 1;
            end F;
            "#,
        );
    }

    #[test]
    fn mls_11_2_1_array_element_assignment() {
        expect_parse_success(
            r#"
            function F
                input Integer n;
                output Real y[3];
            algorithm
                y[1] := n;
                y[2] := n + 1;
                y[3] := n + 2;
            end F;
            "#,
        );
    }

    #[test]
    fn mls_11_2_1_compound_assignment() {
        expect_parse_success(
            r#"
            function F
                input Real x;
                output Real a;
                output Real b;
            algorithm
                a := x;
                b := x;
                a := a + 1;
                b := b * 2;
            end F;
            "#,
        );
    }

    #[test]
    fn mls_11_2_1_chained_assignment() {
        expect_parse_success(
            r#"
            function F
                input Real x;
                output Real y;
            protected
                Real temp;
            algorithm
                temp := x;
                temp := temp + 1;
                temp := temp * 2;
                y := temp;
            end F;
            "#,
        );
    }
}

// ============================================================================
// §11.2.2 IF-STATEMENTS
// ============================================================================

/// MLS §11.2.2: If-statements
mod section_11_2_2_if_statement {
    use super::*;

    #[test]
    fn mls_11_2_2_if_simple() {
        expect_parse_success(
            r#"
            function F
                input Real x;
                output Real y;
            algorithm
                if x > 0 then
                    y := x;
                else
                    y := -x;
                end if;
            end F;
            "#,
        );
    }

    #[test]
    fn mls_11_2_2_if_elseif() {
        expect_parse_success(
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
            "#,
        );
    }

    #[test]
    fn mls_11_2_2_if_multiple_elseif() {
        expect_parse_success(
            r#"
            function Grade
                input Real score;
                output String grade;
            algorithm
                if score >= 90 then
                    grade := "A";
                elseif score >= 80 then
                    grade := "B";
                elseif score >= 70 then
                    grade := "C";
                elseif score >= 60 then
                    grade := "D";
                else
                    grade := "F";
                end if;
            end Grade;
            "#,
        );
    }

    #[test]
    fn mls_11_2_2_if_nested() {
        expect_parse_success(
            r#"
            function F
                input Real x;
                input Real y;
                output Real z;
            algorithm
                if x > 0 then
                    if y > 0 then
                        z := 1;
                    else
                        z := 2;
                    end if;
                else
                    z := 3;
                end if;
            end F;
            "#,
        );
    }

    #[test]
    fn mls_11_2_2_if_without_else() {
        expect_parse_success(
            r#"
            function F
                input Real x;
                output Real y;
            algorithm
                y := 0;
                if x > 0 then
                    y := x;
                end if;
            end F;
            "#,
        );
    }

    #[test]
    fn mls_11_2_2_if_with_multiple_statements() {
        expect_parse_success(
            r#"
            function F
                input Real x;
                output Real a;
                output Real b;
            algorithm
                if x > 0 then
                    a := x;
                    b := x * 2;
                else
                    a := 0;
                    b := 0;
                end if;
            end F;
            "#,
        );
    }
}

// ============================================================================
// §11.2.3 FOR-STATEMENTS
// ============================================================================

/// MLS §11.2.3: For-statements
mod section_11_2_3_for_statement {
    use super::*;

    #[test]
    fn mls_11_2_3_for_simple() {
        expect_parse_success(
            r#"
            function Sum
                input Integer n;
                output Real s;
            algorithm
                s := 0;
                for i in 1:n loop
                    s := s + i;
                end for;
            end Sum;
            "#,
        );
    }

    #[test]
    fn mls_11_2_3_for_with_step() {
        expect_parse_success(
            r#"
            function F
                input Integer n;
                output Real s;
            algorithm
                s := 0;
                for i in 1:2:n loop
                    s := s + i;
                end for;
            end F;
            "#,
        );
    }

    #[test]
    fn mls_11_2_3_for_nested() {
        expect_parse_success(
            r#"
            function MatrixSum
                input Real A[:,:];
                output Real s;
            algorithm
                s := 0;
                for i in 1:size(A,1) loop
                    for j in 1:size(A,2) loop
                        s := s + A[i,j];
                    end for;
                end for;
            end MatrixSum;
            "#,
        );
    }

    #[test]
    fn mls_11_2_3_for_array_iteration() {
        expect_parse_success(
            r#"
            function InitArray
                input Integer n;
                output Real x[n];
            algorithm
                for i in 1:n loop
                    x[i] := i * i;
                end for;
            end InitArray;
            "#,
        );
    }

    #[test]
    fn mls_11_2_3_for_with_negative_step() {
        expect_parse_success(
            r#"
            function ReverseSum
                input Integer n;
                output Real s;
            algorithm
                s := 0;
                for i in n:-1:1 loop
                    s := s + i;
                end for;
            end ReverseSum;
            "#,
        );
    }

    #[test]
    fn mls_11_2_3_for_triple_nested() {
        expect_parse_success(
            r#"
            function TensorSum
                input Real T[:,:,:];
                output Real s;
            algorithm
                s := 0;
                for i in 1:size(T,1) loop
                    for j in 1:size(T,2) loop
                        for k in 1:size(T,3) loop
                            s := s + T[i,j,k];
                        end for;
                    end for;
                end for;
            end TensorSum;
            "#,
        );
    }
}

// ============================================================================
// §11.2.4 WHILE-STATEMENTS
// ============================================================================

/// MLS §11.2.4: While-statements
mod section_11_2_4_while_statement {
    use super::*;

    #[test]
    fn mls_11_2_4_while_simple() {
        expect_parse_success(
            r#"
            function Countdown
                input Integer n;
                output Integer count;
            algorithm
                count := n;
                while count > 0 loop
                    count := count - 1;
                end while;
            end Countdown;
            "#,
        );
    }

    #[test]
    fn mls_11_2_4_while_with_condition() {
        expect_parse_success(
            r#"
            function Sqrt
                input Real x;
                output Real y;
            protected
                Real eps = 1e-10;
                Real prev;
            algorithm
                y := x / 2;
                prev := 0;
                while abs(y - prev) > eps loop
                    prev := y;
                    y := (y + x / y) / 2;
                end while;
            end Sqrt;
            "#,
        );
    }

    #[test]
    fn mls_11_2_4_while_nested() {
        expect_parse_success(
            r#"
            function F
                input Integer m;
                input Integer n;
                output Integer count;
            protected
                Integer i;
                Integer j;
            algorithm
                count := 0;
                i := m;
                while i > 0 loop
                    j := n;
                    while j > 0 loop
                        count := count + 1;
                        j := j - 1;
                    end while;
                    i := i - 1;
                end while;
            end F;
            "#,
        );
    }

    #[test]
    fn mls_11_2_4_while_with_compound_condition() {
        expect_parse_success(
            r#"
            function F
                input Integer n;
                output Integer result;
            protected
                Integer i;
            algorithm
                i := 0;
                result := 0;
                while i < n and result < 100 loop
                    result := result + i;
                    i := i + 1;
                end while;
            end F;
            "#,
        );
    }
}

// ============================================================================
// §11.2.5 WHEN-STATEMENTS
// ============================================================================

/// MLS §11.2.5: When-statements
mod section_11_2_5_when_statement {
    use super::*;

    #[test]
    fn mls_11_2_5_when_in_algorithm() {
        expect_parse_success(
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
        );
    }

    #[test]
    fn mls_11_2_5_when_elsewhen() {
        expect_parse_success(
            r#"
            model Test
                Real x(start = 0);
                discrete Integer state(start = 0);
            algorithm
                when x > 1 then
                    state := 1;
                elsewhen x > 2 then
                    state := 2;
                end when;
            equation
                der(x) = 1;
            end Test;
            "#,
        );
    }

    #[test]
    fn mls_11_2_5_when_multiple_statements() {
        expect_parse_success(
            r#"
            model Test
                Real x(start = 0);
                discrete Real a(start = 0);
                discrete Real b(start = 0);
            algorithm
                when x > 1 then
                    a := 1;
                    b := 2;
                end when;
            equation
                der(x) = 1;
            end Test;
            "#,
        );
    }
}

// ============================================================================
// §11.2.6 BREAK STATEMENT
// ============================================================================

/// MLS §11.2.6: Break statement
mod section_11_2_6_break {
    use super::*;

    #[test]
    fn mls_11_2_6_break_in_for() {
        expect_parse_success(
            r#"
            function FindFirst
                input Real x[:];
                input Real target;
                output Integer idx;
            algorithm
                idx := 0;
                for i in 1:size(x,1) loop
                    if x[i] == target then
                        idx := i;
                        break;
                    end if;
                end for;
            end FindFirst;
            "#,
        );
    }

    #[test]
    fn mls_11_2_6_break_in_while() {
        expect_parse_success(
            r#"
            function F
                input Integer n;
                output Integer count;
            algorithm
                count := 0;
                while true loop
                    count := count + 1;
                    if count >= n then
                        break;
                    end if;
                end while;
            end F;
            "#,
        );
    }

    #[test]
    fn mls_11_2_6_break_in_nested_loop() {
        expect_parse_success(
            r#"
            function FindInMatrix
                input Real A[:,:];
                input Real target;
                output Integer row;
                output Integer col;
            protected
                Boolean found;
            algorithm
                found := false;
                row := 0;
                col := 0;
                for i in 1:size(A,1) loop
                    for j in 1:size(A,2) loop
                        if A[i,j] == target then
                            row := i;
                            col := j;
                            found := true;
                            break;
                        end if;
                    end for;
                    if found then
                        break;
                    end if;
                end for;
            end FindInMatrix;
            "#,
        );
    }
}

// ============================================================================
// §11.2.7 RETURN STATEMENT
// ============================================================================

/// MLS §11.2.7: Return statement
mod section_11_2_7_return {
    use super::*;

    #[test]
    fn mls_11_2_7_return_basic() {
        expect_parse_success(
            r#"
            function F
                input Real x;
                output Real y;
            algorithm
                y := x * 2;
                return;
            end F;
            "#,
        );
    }

    #[test]
    fn mls_11_2_7_return_early() {
        expect_parse_success(
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
            "#,
        );
    }

    #[test]
    fn mls_11_2_7_return_in_loop() {
        expect_parse_success(
            r#"
            function FindIndex
                input Real x[:];
                input Real target;
                output Integer idx;
            algorithm
                for i in 1:size(x,1) loop
                    if x[i] == target then
                        idx := i;
                        return;
                    end if;
                end for;
                idx := 0;
            end FindIndex;
            "#,
        );
    }

    #[test]
    fn mls_11_2_7_return_multiple_outputs() {
        expect_parse_success(
            r#"
            function DivMod
                input Integer a;
                input Integer b;
                output Integer quotient;
                output Integer remainder;
            algorithm
                if b == 0 then
                    quotient := 0;
                    remainder := 0;
                    return;
                end if;
                quotient := div(a, b);
                remainder := mod(a, b);
            end DivMod;
            "#,
        );
    }
}

// ============================================================================
// §11.2.8 ASSERT STATEMENT
// ============================================================================

/// MLS §11.2.8: Assert statement
mod section_11_2_8_assert {
    use super::*;

    #[test]
    fn mls_11_2_8_assert_in_algorithm() {
        expect_parse_success(
            r#"
            function Sqrt
                input Real x;
                output Real y;
            algorithm
                assert(x >= 0, "sqrt requires non-negative input");
                y := x ^ 0.5;
            end Sqrt;
            "#,
        );
    }

    #[test]
    fn mls_11_2_8_assert_with_level() {
        expect_parse_success(
            r#"
            function F
                input Real x;
                output Real y;
            algorithm
                assert(x > 0, "x must be positive", AssertionLevel.warning);
                y := log(x);
            end F;
            "#,
        );
    }

    #[test]
    fn mls_11_2_8_multiple_asserts() {
        expect_parse_success(
            r#"
            function Divide
                input Real a;
                input Real b;
                output Real c;
            algorithm
                assert(b <> 0, "Division by zero");
                assert(abs(b) > 1e-10, "Divisor too small", AssertionLevel.warning);
                c := a / b;
            end Divide;
            "#,
        );
    }
}
