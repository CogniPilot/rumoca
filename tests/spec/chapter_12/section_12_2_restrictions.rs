//! MLS §12.2: Function as a Specialized Class - Restrictions
//!
//! This file tests the specific restrictions that apply to functions
//! as specialized classes. Both valid and invalid constructs are tested.
//!
//! Reference: https://specification.modelica.org/master/functions.html#function-as-a-specialized-class

use crate::spec::{expect_failure, expect_parse_failure, expect_parse_success};

// ============================================================================
// §12.2 VALID FUNCTION CONSTRUCTS
// ============================================================================

/// MLS §12.2: Valid function constructs
mod section_12_2_valid {
    use super::*;

    /// MLS §12.2: All public components must be input or output - valid case
    #[test]
    fn mls_12_2_public_input_output_only() {
        expect_parse_success(
            r#"
            function F
                input Real a;
                input Real b;
                output Real c;
            algorithm
                c := a + b;
            end F;
            "#,
        );
    }

    /// MLS §12.2: Protected components need not be input/output
    #[test]
    fn mls_12_2_protected_non_io() {
        expect_parse_success(
            r#"
            function F
                input Real x;
                output Real y;
            protected
                Real temp;
                Integer counter;
            algorithm
                temp := x * 2;
                counter := 1;
                y := temp + counter;
            end F;
            "#,
        );
    }

    /// MLS §12.2: Function with type component (allowed)
    #[test]
    fn mls_12_2_type_component() {
        expect_parse_success(
            r#"
            function F
                input Real x;
                output Real y;
            protected
                type MyReal = Real(unit = "m");
                MyReal temp;
            algorithm
                temp := x;
                y := temp * 2;
            end F;
            "#,
        );
    }

    /// MLS §12.2: Function with record component (allowed)
    #[test]
    fn mls_12_2_record_component() {
        expect_parse_success(
            r#"
            function F
                input Real x;
                input Real y;
                output Real magnitude;
            protected
                record Point
                    Real px;
                    Real py;
                end Point;
                Point p;
            algorithm
                p.px := x;
                p.py := y;
                magnitude := sqrt(p.px^2 + p.py^2);
            end F;
            "#,
        );
    }

    /// MLS §12.2: Function with nested function (allowed)
    #[test]
    fn mls_12_2_nested_function() {
        expect_parse_success(
            r#"
            function Outer
                input Real x;
                output Real y;
            protected
                function Inner
                    input Real a;
                    output Real b;
                algorithm
                    b := a * 2;
                end Inner;
            algorithm
                y := Inner(x) + 1;
            end Outer;
            "#,
        );
    }

    /// MLS §12.2: Recursive function (allowed)
    #[test]
    fn mls_12_2_recursive_function() {
        expect_parse_success(
            r#"
            function Factorial
                input Integer n;
                output Integer result;
            algorithm
                if n <= 1 then
                    result := 1;
                else
                    result := n * Factorial(n - 1);
                end if;
            end Factorial;
            "#,
        );
    }

    /// MLS §12.2: Mutually recursive functions (allowed)
    #[test]
    fn mls_12_2_mutual_recursion() {
        expect_parse_success(
            r#"
            function IsEven
                input Integer n;
                output Boolean result;
            algorithm
                if n == 0 then
                    result := true;
                else
                    result := IsOdd(n - 1);
                end if;
            end IsEven;

            function IsOdd
                input Integer n;
                output Boolean result;
            algorithm
                if n == 0 then
                    result := false;
                else
                    result := IsEven(n - 1);
                end if;
            end IsOdd;
            "#,
        );
    }

    /// MLS §12.2: Single algorithm section (allowed)
    #[test]
    fn mls_12_2_single_algorithm() {
        expect_parse_success(
            r#"
            function F
                input Real x;
                output Real y;
            algorithm
                y := x * 2;
            end F;
            "#,
        );
    }

    /// MLS §12.2: External interface only (allowed)
    #[test]
    fn mls_12_2_external_only() {
        expect_parse_success(
            r#"
            function F
                input Real x;
                output Real y;
            external "C";
            end F;
            "#,
        );
    }
}

// ============================================================================
// §12.2 INVALID FUNCTION CONSTRUCTS (NEGATIVE TESTS)
// ============================================================================

/// MLS §12.2: Invalid function constructs that should fail
mod section_12_2_invalid {
    use super::*;

    /// MLS §12.2: "No equations allowed" - equation section should fail
    #[test]
    fn mls_12_2_no_equations() {
        expect_parse_failure(
            r#"
            function F
                input Real x;
                output Real y;
            equation
                y = x * 2;
            end F;
            "#,
        );
    }

    /// MLS §12.2: "No connections allowed" - connect should fail
    #[test]
    fn mls_12_2_no_connections() {
        expect_parse_failure(
            r#"
            connector C
                Real v;
                flow Real i;
            end C;

            function F
                input Real x;
                output Real y;
            protected
                C c1, c2;
            algorithm
                connect(c1, c2);
                y := x;
            end F;
            "#,
        );
    }

    /// MLS §12.2: "No initial algorithms allowed"
    #[test]
    fn mls_12_2_no_initial_algorithm() {
        expect_parse_failure(
            r#"
            function F
                input Real x;
                output Real y;
            initial algorithm
                y := 0;
            algorithm
                y := x;
            end F;
            "#,
        );
    }

    /// MLS §12.2: "Maximum one algorithm section" - multiple should fail
    #[test]
    fn mls_12_2_max_one_algorithm() {
        expect_failure(
            r#"
            function F
                input Real x;
                output Real y;
            algorithm
                y := x;
            algorithm
                y := y * 2;
            end F;
            "#,
            "F",
        );
    }

    /// MLS §12.2: "No inner/outer prefixes"
    #[test]
    fn mls_12_2_no_inner_outer() {
        expect_failure(
            r#"
            function F
                input Real x;
                output Real y;
            protected
                outer Real globalParam;
            algorithm
                y := x * globalParam;
            end F;
            "#,
            "F",
        );
    }

    /// MLS §12.2: "Input parameters are read-only" - modification should fail
    #[test]
    fn mls_12_2_input_readonly() {
        expect_failure(
            r#"
            function F
                input Real x;
                output Real y;
            algorithm
                x := x + 1;
                y := x;
            end F;
            "#,
            "F",
        );
    }

    /// MLS §12.2: "Output variables must be assigned"
    #[test]
    #[ignore = "Semantic validation not yet implemented for function restrictions"]
    fn mls_12_2_output_must_be_assigned() {
        expect_parse_failure(
            r#"
            function F
                input Real x;
                output Real y;
                output Real z;
            algorithm
                y := x;
            end F;
            "#,
        );
    }

    /// MLS §12.2: Public component without input/output
    #[test]
    fn mls_12_2_public_must_be_io() {
        expect_failure(
            r#"
            function F
                input Real x;
                Real temp;
                output Real y;
            algorithm
                temp := x * 2;
                y := temp;
            end F;
            "#,
            "F",
        );
    }
}

// ============================================================================
// §12.2 FUNCTION ENHANCEMENTS
// ============================================================================

/// MLS §12.2: Function enhancements
mod section_12_2_enhancements {
    use super::*;

    /// MLS §12.2: "Support for recursive definitions"
    #[test]
    fn mls_12_2_fibonacci_recursive() {
        expect_parse_success(
            r#"
            function Fibonacci
                input Integer n;
                output Integer result;
            algorithm
                if n <= 0 then
                    result := 0;
                elseif n == 1 then
                    result := 1;
                else
                    result := Fibonacci(n-1) + Fibonacci(n-2);
                end if;
            end Fibonacci;
            "#,
        );
    }

    /// MLS §12.2: "Dynamic instantiation upon calls"
    #[test]
    fn mls_12_2_dynamic_protected() {
        expect_parse_success(
            r#"
            function F
                input Integer n;
                output Real result;
            protected
                Real temp[n];
            algorithm
                for i in 1:n loop
                    temp[i] := i;
                end for;
                result := sum(temp);
            end F;
            "#,
        );
    }

    /// MLS §12.2: "Return statements permitted"
    #[test]
    fn mls_12_2_return_statement() {
        expect_parse_success(
            r#"
            function EarlyReturn
                input Real x;
                output Real y;
            algorithm
                if x < 0 then
                    y := 0;
                    return;
                end if;
                y := sqrt(x);
            end EarlyReturn;
            "#,
        );
    }

    /// MLS §12.2: "Flexible array resizing for non-input arrays"
    #[test]
    fn mls_12_2_array_resizing() {
        expect_parse_success(
            r#"
            function CreateArray
                input Integer n;
                output Real y[:];
            algorithm
                y := zeros(n);
                for i in 1:n loop
                    y[i] := i;
                end for;
            end CreateArray;
            "#,
        );
    }

    /// MLS §12.2: Protected array with dynamic size
    #[test]
    fn mls_12_2_protected_dynamic_array() {
        expect_parse_success(
            r#"
            function F
                input Real x[:];
                output Real y;
            protected
                Integer n = size(x, 1);
                Real temp[n];
            algorithm
                for i in 1:n loop
                    temp[i] := x[i] * 2;
                end for;
                y := sum(temp);
            end F;
            "#,
        );
    }
}
