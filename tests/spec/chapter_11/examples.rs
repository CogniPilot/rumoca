//! Chapter 11: Algorithm and Statement Examples
//!
//! Complete function examples demonstrating various algorithmic constructs.
//!
//! Reference: https://specification.modelica.org/master/statements-and-algorithm-sections.html

use crate::spec::expect_parse_success;

// ============================================================================
// COMPLETE FUNCTION EXAMPLES
// ============================================================================

/// Complete function examples demonstrating algorithm features
mod function_examples {
    use super::*;

    #[test]
    fn function_factorial() {
        expect_parse_success(
            r#"
            function Factorial
                input Integer n;
                output Integer result;
            algorithm
                result := 1;
                for i in 2:n loop
                    result := result * i;
                end for;
            end Factorial;
            "#,
        );
    }

    #[test]
    fn function_fibonacci() {
        expect_parse_success(
            r#"
            function Fibonacci
                input Integer n;
                output Integer fib;
            protected
                Integer a;
                Integer b;
                Integer temp;
            algorithm
                a := 0;
                b := 1;
                for i in 1:n loop
                    temp := a + b;
                    a := b;
                    b := temp;
                end for;
                fib := a;
            end Fibonacci;
            "#,
        );
    }

    #[test]
    fn function_bubble_sort() {
        expect_parse_success(
            r#"
            function BubbleSort
                input Real x[:];
                output Real y[size(x,1)];
            protected
                Real temp;
                Integer n;
            algorithm
                y := x;
                n := size(y, 1);
                for i in 1:n-1 loop
                    for j in 1:n-i loop
                        if y[j] > y[j+1] then
                            temp := y[j];
                            y[j] := y[j+1];
                            y[j+1] := temp;
                        end if;
                    end for;
                end for;
            end BubbleSort;
            "#,
        );
    }

    #[test]
    fn function_binary_search() {
        expect_parse_success(
            r#"
            function BinarySearch
                input Real x[:];
                input Real target;
                output Integer idx;
            protected
                Integer lo;
                Integer hi;
                Integer mid;
            algorithm
                lo := 1;
                hi := size(x, 1);
                idx := 0;
                while lo <= hi loop
                    mid := div(lo + hi, 2);
                    if x[mid] == target then
                        idx := mid;
                        return;
                    elseif x[mid] < target then
                        lo := mid + 1;
                    else
                        hi := mid - 1;
                    end if;
                end while;
            end BinarySearch;
            "#,
        );
    }

    #[test]
    fn function_gcd() {
        expect_parse_success(
            r#"
            function GCD
                "Greatest Common Divisor using Euclidean algorithm"
                input Integer a;
                input Integer b;
                output Integer result;
            protected
                Integer x;
                Integer y;
                Integer temp;
            algorithm
                x := abs(a);
                y := abs(b);
                while y <> 0 loop
                    temp := mod(x, y);
                    x := y;
                    y := temp;
                end while;
                result := x;
            end GCD;
            "#,
        );
    }

    #[test]
    fn function_power_integer() {
        expect_parse_success(
            r#"
            function PowerInt
                "Compute base^exp for integer exponent"
                input Real base;
                input Integer exp;
                output Real result;
            protected
                Integer n;
            algorithm
                result := 1;
                n := abs(exp);
                for i in 1:n loop
                    result := result * base;
                end for;
                if exp < 0 then
                    result := 1 / result;
                end if;
            end PowerInt;
            "#,
        );
    }

    #[test]
    fn function_is_prime() {
        expect_parse_success(
            r#"
            function IsPrime
                input Integer n;
                output Boolean prime;
            protected
                Integer i;
            algorithm
                if n < 2 then
                    prime := false;
                    return;
                end if;
                if n == 2 then
                    prime := true;
                    return;
                end if;
                if mod(n, 2) == 0 then
                    prime := false;
                    return;
                end if;
                prime := true;
                i := 3;
                while i * i <= n loop
                    if mod(n, i) == 0 then
                        prime := false;
                        return;
                    end if;
                    i := i + 2;
                end while;
            end IsPrime;
            "#,
        );
    }

    #[test]
    fn function_selection_sort() {
        expect_parse_success(
            r#"
            function SelectionSort
                input Real x[:];
                output Real y[size(x,1)];
            protected
                Integer n;
                Integer minIdx;
                Real temp;
            algorithm
                y := x;
                n := size(y, 1);
                for i in 1:n-1 loop
                    minIdx := i;
                    for j in i+1:n loop
                        if y[j] < y[minIdx] then
                            minIdx := j;
                        end if;
                    end for;
                    if minIdx <> i then
                        temp := y[i];
                        y[i] := y[minIdx];
                        y[minIdx] := temp;
                    end if;
                end for;
            end SelectionSort;
            "#,
        );
    }

    #[test]
    fn function_array_reverse() {
        expect_parse_success(
            r#"
            function ArrayReverse
                input Real x[:];
                output Real y[size(x,1)];
            protected
                Integer n;
            algorithm
                n := size(x, 1);
                for i in 1:n loop
                    y[i] := x[n - i + 1];
                end for;
            end ArrayReverse;
            "#,
        );
    }

    #[test]
    fn function_count_occurrences() {
        expect_parse_success(
            r#"
            function CountOccurrences
                input Real x[:];
                input Real target;
                output Integer count;
            algorithm
                count := 0;
                for i in 1:size(x, 1) loop
                    if x[i] == target then
                        count := count + 1;
                    end if;
                end for;
            end CountOccurrences;
            "#,
        );
    }
}

/// Numerical algorithm examples
mod numerical_algorithms {
    use super::*;

    #[test]
    fn newton_raphson_sqrt() {
        expect_parse_success(
            r#"
            function NewtonSqrt
                "Square root using Newton-Raphson method"
                input Real x;
                output Real result;
            protected
                Real guess;
                Real prev;
                Real eps = 1e-12;
                Integer maxIter = 100;
                Integer iter;
            algorithm
                assert(x >= 0, "Cannot compute sqrt of negative number");
                if x == 0 then
                    result := 0;
                    return;
                end if;
                guess := x / 2;
                iter := 0;
                while iter < maxIter loop
                    prev := guess;
                    guess := (guess + x / guess) / 2;
                    if abs(guess - prev) < eps then
                        break;
                    end if;
                    iter := iter + 1;
                end while;
                result := guess;
            end NewtonSqrt;
            "#,
        );
    }

    #[test]
    fn trapezoidal_integration() {
        expect_parse_success(
            r#"
            function TrapezoidalIntegrate
                "Approximate integral using trapezoidal rule"
                input Real y[:];
                input Real dx;
                output Real integral;
            protected
                Integer n;
            algorithm
                n := size(y, 1);
                if n < 2 then
                    integral := 0;
                    return;
                end if;
                integral := (y[1] + y[n]) / 2;
                for i in 2:n-1 loop
                    integral := integral + y[i];
                end for;
                integral := integral * dx;
            end TrapezoidalIntegrate;
            "#,
        );
    }
}
