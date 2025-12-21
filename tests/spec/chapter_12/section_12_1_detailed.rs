//! MLS §12.1: Detailed Function Declaration Tests
//!
//! This file provides comprehensive tests for every normative statement in MLS §12.1.
//! Each test is annotated with the specific assertion it validates.
//!
//! Reference: https://specification.modelica.org/master/functions.html#function-declaration

use crate::spec::expect_parse_success;

// ============================================================================
// §12.1 FUNCTION DECLARATION - BASIC STRUCTURE
// ============================================================================

/// MLS §12.1: "Functions are specialized classes using the function keyword"
mod section_12_1_basic_structure {
    use super::*;

    /// MLS §12.1: Function with only algorithm section
    #[test]
    fn mls_12_1_algorithm_only() {
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

    /// MLS §12.1: Function with only external interface (no algorithm)
    #[test]
    fn mls_12_1_external_only() {
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

    /// MLS §12.1: Function with empty algorithm section
    #[test]
    fn mls_12_1_empty_algorithm() {
        expect_parse_success(
            r#"
            function F
                output Real y = 1.0;
            algorithm
            end F;
            "#,
        );
    }

    /// MLS §12.1: Input parameters with binding equations (default values)
    #[test]
    fn mls_12_1_input_with_binding() {
        expect_parse_success(
            r#"
            function F
                input Real x = 0.0;
                input Real y = 1.0;
                input Real z = x + y;
                output Real result;
            algorithm
                result := x + y + z;
            end F;
            "#,
        );
    }

    /// MLS §12.1: Output with binding equation
    #[test]
    fn mls_12_1_output_with_binding() {
        expect_parse_success(
            r#"
            function F
                input Real x;
                output Real y = x * 2;
            algorithm
            end F;
            "#,
        );
    }

    /// MLS §12.1: Protected local variables
    #[test]
    fn mls_12_1_protected_variables() {
        expect_parse_success(
            r#"
            function F
                input Real x;
                output Real y;
            protected
                Real temp;
                Integer count = 0;
                Boolean flag = true;
            algorithm
                temp := x * 2;
                y := temp + 1;
            end F;
            "#,
        );
    }

    /// MLS §12.1: Function with constant in protected section
    #[test]
    fn mls_12_1_protected_constant() {
        expect_parse_success(
            r#"
            function F
                input Real x;
                output Real y;
            protected
                constant Real PI = 3.14159;
            algorithm
                y := x * PI;
            end F;
            "#,
        );
    }
}

// ============================================================================
// §12.1.1 ORDERING OF FORMAL PARAMETERS
// ============================================================================

/// MLS §12.1.1: "The relative ordering of input and output declarations is significant"
mod section_12_1_1_ordering {
    use super::*;

    /// MLS §12.1.1: Standard ordering - inputs then outputs
    #[test]
    fn mls_12_1_1_standard_ordering() {
        expect_parse_success(
            r#"
            function F
                input Real a;
                input Real b;
                output Real x;
                output Real y;
            algorithm
                x := a + b;
                y := a - b;
            end F;
            "#,
        );
    }

    /// MLS §12.1.1: Intermixed input/output declarations (allowed but not recommended)
    #[test]
    fn mls_12_1_1_intermixed_declarations() {
        expect_parse_success(
            r#"
            function F
                input Real a;
                output Real x;
                input Real b;
                output Real y;
            algorithm
                x := a + b;
                y := a - b;
            end F;
            "#,
        );
    }

    /// MLS §12.1.1: Multiple intermixed declarations
    #[test]
    fn mls_12_1_1_multiple_intermixed() {
        expect_parse_success(
            r#"
            function F
                output Real r1;
                input Real i1;
                input Real i2;
                output Real r2;
                input Real i3;
                output Real r3;
            algorithm
                r1 := i1;
                r2 := i2;
                r3 := i3;
            end F;
            "#,
        );
    }

    /// MLS §12.1.1: Output before any input
    #[test]
    fn mls_12_1_1_output_first() {
        expect_parse_success(
            r#"
            function F
                output Real y;
                input Real x;
            algorithm
                y := x * 2;
            end F;
            "#,
        );
    }
}

// ============================================================================
// §12.1.2 FUNCTION RETURN-STATEMENTS
// ============================================================================

/// MLS §12.1.2: "The return statement terminates function execution"
mod section_12_1_2_return {
    use super::*;

    /// MLS §12.1.2: Basic return statement
    #[test]
    fn mls_12_1_2_basic_return() {
        expect_parse_success(
            r#"
            function F
                input Real x;
                output Real y;
            algorithm
                y := x;
                return;
            end F;
            "#,
        );
    }

    /// MLS §12.1.2: Return as early exit from conditional
    #[test]
    fn mls_12_1_2_return_in_if() {
        expect_parse_success(
            r#"
            function Abs
                input Real x;
                output Real y;
            algorithm
                if x >= 0 then
                    y := x;
                    return;
                end if;
                y := -x;
            end Abs;
            "#,
        );
    }

    /// MLS §12.1.2: Return as early exit from for-loop
    #[test]
    fn mls_12_1_2_return_in_for() {
        expect_parse_success(
            r#"
            function FindFirst
                input Real x[:];
                input Real target;
                output Integer index;
            algorithm
                index := -1;
                for i in 1:size(x, 1) loop
                    if x[i] == target then
                        index := i;
                        return;
                    end if;
                end for;
            end FindFirst;
            "#,
        );
    }

    /// MLS §12.1.2: Return as early exit from while-loop
    #[test]
    fn mls_12_1_2_return_in_while() {
        expect_parse_success(
            r#"
            function SearchValue
                input Real x[:];
                input Real target;
                output Boolean found;
            protected
                Integer i;
            algorithm
                i := 1;
                found := false;
                while i <= size(x, 1) loop
                    if x[i] == target then
                        found := true;
                        return;
                    end if;
                    i := i + 1;
                end while;
            end SearchValue;
            "#,
        );
    }

    /// MLS §12.1.2: Multiple return statements in different branches
    #[test]
    fn mls_12_1_2_multiple_returns() {
        expect_parse_success(
            r#"
            function Sign
                input Real x;
                output Integer s;
            algorithm
                if x > 0 then
                    s := 1;
                    return;
                elseif x < 0 then
                    s := -1;
                    return;
                else
                    s := 0;
                    return;
                end if;
            end Sign;
            "#,
        );
    }

    /// MLS §12.1.2: Return in nested conditionals
    #[test]
    fn mls_12_1_2_nested_return() {
        expect_parse_success(
            r#"
            function Classify
                input Real x;
                input Real y;
                output Integer category;
            algorithm
                if x > 0 then
                    if y > 0 then
                        category := 1;
                        return;
                    else
                        category := 2;
                        return;
                    end if;
                else
                    if y > 0 then
                        category := 3;
                        return;
                    else
                        category := 4;
                        return;
                    end if;
                end if;
            end Classify;
            "#,
        );
    }
}

// ============================================================================
// §12.1.3 INHERITANCE OF FUNCTIONS
// ============================================================================

/// MLS §12.1.3: "Functions may inherit from other functions"
mod section_12_1_3_inheritance {
    use super::*;

    /// MLS §12.1.3: Function extending another function
    #[test]
    fn mls_12_1_3_function_extends() {
        expect_parse_success(
            r#"
            function BaseFunction
                input Real x;
                output Real y;
            algorithm
                y := x;
            end BaseFunction;

            function DerivedFunction
                extends BaseFunction;
            algorithm
                y := x * 2;
            end DerivedFunction;
            "#,
        );
    }

    /// MLS §12.1.3: Function inheritance with additional inputs
    #[test]
    fn mls_12_1_3_extends_with_additions() {
        expect_parse_success(
            r#"
            function Base
                input Real x;
                output Real y;
            algorithm
                y := x;
            end Base;

            function Extended
                extends Base;
                input Real scale = 1.0;
            algorithm
                y := x * scale;
            end Extended;
            "#,
        );
    }

    /// MLS §12.1.3: Short class function definition
    #[test]
    fn mls_12_1_3_short_class_definition() {
        expect_parse_success(
            r#"
            function Base
                input Real x;
                output Real y;
            algorithm
                y := x;
            end Base;

            function Derived = Base(x = 1.0);
            "#,
        );
    }

    /// MLS §12.1.3: Function extending with modified default
    #[test]
    fn mls_12_1_3_extends_modified_default() {
        expect_parse_success(
            r#"
            function Base
                input Real x = 0;
                output Real y;
            algorithm
                y := x + 1;
            end Base;

            function Modified
                extends Base(x = 10);
            end Modified;
            "#,
        );
    }

    /// MLS §12.1.3: Multiple levels of function inheritance
    #[test]
    fn mls_12_1_3_multilevel_inheritance() {
        expect_parse_success(
            r#"
            function Level1
                input Real x;
                output Real y;
            algorithm
                y := x;
            end Level1;

            function Level2
                extends Level1;
                input Real scale = 1.0;
            algorithm
                y := x * scale;
            end Level2;

            function Level3
                extends Level2;
                input Real offset = 0.0;
            algorithm
                y := x * scale + offset;
            end Level3;
            "#,
        );
    }
}

// ============================================================================
// ADDITIONAL §12.1 TESTS - TYPE SPECIFICATIONS
// ============================================================================

/// MLS §12.1: Type specifications in function parameters
mod section_12_1_types {
    use super::*;

    /// MLS §12.1: Integer input and output
    #[test]
    fn mls_12_1_integer_types() {
        expect_parse_success(
            r#"
            function IntFunc
                input Integer a;
                input Integer b;
                output Integer sum;
                output Integer product;
            algorithm
                sum := a + b;
                product := a * b;
            end IntFunc;
            "#,
        );
    }

    /// MLS §12.1: Boolean input and output
    #[test]
    fn mls_12_1_boolean_types() {
        expect_parse_success(
            r#"
            function BoolFunc
                input Boolean a;
                input Boolean b;
                output Boolean andResult;
                output Boolean orResult;
            algorithm
                andResult := a and b;
                orResult := a or b;
            end BoolFunc;
            "#,
        );
    }

    /// MLS §12.1: String input and output
    #[test]
    fn mls_12_1_string_types() {
        expect_parse_success(
            r#"
            function StringFunc
                input String prefix;
                input String suffix;
                output String combined;
            algorithm
                combined := prefix + suffix;
            end StringFunc;
            "#,
        );
    }

    /// MLS §12.1: Mixed types in function
    #[test]
    fn mls_12_1_mixed_types() {
        expect_parse_success(
            r#"
            function MixedFunc
                input Real value;
                input Integer count;
                input Boolean flag;
                output Real result;
                output String message;
            algorithm
                if flag then
                    result := value * count;
                    message := "Computed";
                else
                    result := 0;
                    message := "Skipped";
                end if;
            end MixedFunc;
            "#,
        );
    }

    /// MLS §12.1: Enumeration types in function
    #[test]
    fn mls_12_1_enumeration_types() {
        expect_parse_success(
            r#"
            type Color = enumeration(Red, Green, Blue);

            function ColorFunc
                input Color c;
                output Integer code;
            algorithm
                if c == Color.Red then
                    code := 1;
                elseif c == Color.Green then
                    code := 2;
                else
                    code := 3;
                end if;
            end ColorFunc;
            "#,
        );
    }

    /// MLS §12.1: Record types in function
    #[test]
    fn mls_12_1_record_types() {
        expect_parse_success(
            r#"
            record Point
                Real x;
                Real y;
            end Point;

            function Distance
                input Point p1;
                input Point p2;
                output Real d;
            algorithm
                d := sqrt((p2.x - p1.x)^2 + (p2.y - p1.y)^2);
            end Distance;
            "#,
        );
    }

    /// MLS §12.1: 2D array types
    #[test]
    fn mls_12_1_2d_array_types() {
        expect_parse_success(
            r#"
            function MatrixFunc
                input Real A[:,:];
                input Real B[size(A,1), size(A,2)];
                output Real C[size(A,1), size(A,2)];
            algorithm
                for i in 1:size(A,1) loop
                    for j in 1:size(A,2) loop
                        C[i,j] := A[i,j] + B[i,j];
                    end for;
                end for;
            end MatrixFunc;
            "#,
        );
    }

    /// MLS §12.1: 3D array types
    #[test]
    fn mls_12_1_3d_array_types() {
        expect_parse_success(
            r#"
            function Tensor3D
                input Real T[:,:,:];
                output Real total;
            algorithm
                total := 0;
                for i in 1:size(T,1) loop
                    for j in 1:size(T,2) loop
                        for k in 1:size(T,3) loop
                            total := total + T[i,j,k];
                        end for;
                    end for;
                end for;
            end Tensor3D;
            "#,
        );
    }
}
