//! MLS Chapter 6: High-Priority Edge Case Tests
//!
//! This module contains critical tests for edge cases and advanced scenarios
//! in type system behavior that are essential for MLS conformance.
//!
//! Reference: https://specification.modelica.org/master/interface-or-type.html

use crate::spec::{expect_failure, expect_parse_success, expect_success};

// ============================================================================
// CRITICAL: SUBTYPE RELATIONSHIP TESTS
// ============================================================================

/// Critical subtype relationship edge cases
mod subtype_critical {
    use super::*;

    /// Critical: Component subset rule
    #[test]
    fn critical_subtype_component_subset() {
        expect_success(
            r#"
            model Base
                Real a;
                Real b;
            equation
            end Base;

            model Extended
                extends Base;
                Real c;
            equation
                a = 1;
                b = 2;
                c = 3;
            end Extended;
            "#,
            "Extended",
        );
    }

    /// Critical: Deep inheritance chain
    #[test]
    fn critical_deep_inheritance() {
        expect_success(
            r#"
            model L1
                Real a;
            equation
            end L1;

            model L2
                extends L1;
                Real b;
            equation
            end L2;

            model L3
                extends L2;
                Real c;
            equation
            end L3;

            model L4
                extends L3;
                Real d;
            equation
            end L4;

            model L5
                extends L4;
            equation
                a = 1;
                b = 2;
                c = 3;
                d = 4;
            end L5;
            "#,
            "L5",
        );
    }

    /// Critical: Diamond inheritance pattern
    #[test]
    fn critical_diamond_inheritance() {
        expect_parse_success(
            r#"
            model Base
                Real x;
            equation
            end Base;

            model Left
                extends Base;
                Real left;
            equation
            end Left;

            model Right
                extends Base;
                Real right;
            equation
            end Right;

            model Diamond
                extends Left;
                extends Right;
            equation
                x = 1;
                left = 2;
                right = 3;
            end Diamond;
            "#,
        );
    }

    /// Critical: Component with subtype variability
    #[test]
    fn critical_variability_subtype() {
        expect_success(
            r#"
            model Test
                constant Real c = 1;
                parameter Real p = c;
                Real x = p;
            equation
            end Test;
            "#,
            "Test",
        );
    }
}

// ============================================================================
// CRITICAL: PLUG COMPATIBILITY EDGE CASES
// ============================================================================

/// Critical plug compatibility edge cases
mod plug_critical {
    use super::*;

    /// Critical: Plug-compatible with additional non-flow
    #[test]
    fn critical_plug_additional_nonflow() {
        expect_parse_success(
            r#"
            connector Base
                Real e;
                flow Real f;
            end Base;

            connector Extended
                extends Base;
                Real extra;
            end Extended;

            model Test
                Extended a, b;
            equation
                connect(a, b);
            end Test;
            "#,
        );
    }

    /// Critical: Stream connector plug compatibility
    #[test]
    fn critical_stream_plug() {
        expect_parse_success(
            r#"
            connector FluidPort
                Real p;
                flow Real m_flow;
                stream Real h_outflow;
            end FluidPort;

            model FluidComponent
                FluidPort a, b;
            equation
                connect(a, b);
            end FluidComponent;
            "#,
        );
    }

    /// Critical: Hierarchical connector matching
    #[test]
    fn critical_hierarchical_connector() {
        expect_parse_success(
            r#"
            connector Pin
                Real v;
                flow Real i;
            end Pin;

            connector TwoPin
                Pin p;
                Pin n;
            end TwoPin;

            model Test
                TwoPin a, b;
            equation
                connect(a.p, b.p);
                connect(a.n, b.n);
            end Test;
            "#,
        );
    }

    /// Critical: Array connector connection
    #[test]
    fn critical_array_connector() {
        expect_parse_success(
            r#"
            connector Pin
                Real v;
                flow Real i;
            end Pin;

            model Test
                Pin p[3];
                Pin n[3];
            equation
                for i in 1:3 loop
                    connect(p[i], n[i]);
                end for;
            end Test;
            "#,
        );
    }
}

// ============================================================================
// CRITICAL: ARRAY TYPE EDGE CASES
// ============================================================================

/// Critical array type edge cases
mod array_critical {
    use super::*;

    /// Critical: Unknown array size (colon)
    #[test]
    fn critical_unknown_array_size() {
        expect_parse_success(
            r#"
            function SumVector
                input Real v[:];
                output Real s;
            algorithm
                s := sum(v);
            end SumVector;

            model Test
                Real s = SumVector({1, 2, 3, 4, 5});
            end Test;
            "#,
        );
    }

    /// Critical: Size-dependent array size
    #[test]
    fn critical_size_dependent() {
        expect_parse_success(
            r#"
            function ProcessPair
                input Real a[:];
                input Real b[size(a, 1)];
                output Real c[size(a, 1)];
            algorithm
                for i in 1:size(a, 1) loop
                    c[i] := a[i] + b[i];
                end for;
            end ProcessPair;
            "#,
        );
    }

    /// Critical: Multi-dimensional flexible arrays
    #[test]
    fn critical_multidim_flexible() {
        expect_parse_success(
            r#"
            function MatrixOp
                input Real A[:, :];
                output Real B[size(A, 1), size(A, 2)];
            algorithm
                for i in 1:size(A, 1) loop
                    for j in 1:size(A, 2) loop
                        B[i, j] := 2 * A[i, j];
                    end for;
                end for;
            end MatrixOp;
            "#,
        );
    }

    /// Critical: Empty array handling
    #[test]
    fn critical_empty_array() {
        expect_parse_success(
            r#"
            model Test
                parameter Integer n = 0;
                Real x[n];
            equation
            end Test;
            "#,
        );
    }

    /// Critical: Array slicing
    #[test]
    fn critical_array_slicing() {
        expect_parse_success(
            r#"
            model Test
                Real x[5] = {1, 2, 3, 4, 5};
                Real y[3] = x[2:4];
            end Test;
            "#,
        );
    }
}

// ============================================================================
// CRITICAL: FUNCTION TYPE EDGE CASES
// ============================================================================

/// Critical function type edge cases
mod function_critical {
    use super::*;

    /// Critical: Recursive function type
    #[test]
    fn critical_recursive_function() {
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

            model Test
                Integer f = Factorial(5);
            end Test;
            "#,
        );
    }

    /// Critical: Function with protected variables
    #[test]
    fn critical_function_protected() {
        expect_success(
            r#"
            function Compute
                input Real x;
                output Real y;
            protected
                Real temp;
            algorithm
                temp := x * 2;
                y := temp + 1;
            end Compute;

            model Test
                Real y = Compute(5);
            equation
            end Test;
            "#,
            "Test",
        );
    }

    /// Critical: Higher-order function (partial function)
    #[test]
    fn critical_higher_order() {
        expect_parse_success(
            r#"
            partial function UnaryOp
                input Real x;
                output Real y;
            end UnaryOp;

            function ApplyTwice
                input UnaryOp f;
                input Real x;
                output Real y;
            algorithm
                y := f(f(x));
            end ApplyTwice;

            function Double
                extends UnaryOp;
            algorithm
                y := 2 * x;
            end Double;

            model Test
                Real x = ApplyTwice(Double, 3);
            end Test;
            "#,
        );
    }

    /// Critical: External function type
    #[test]
    fn critical_external_function() {
        expect_parse_success(
            r#"
            function ExternalSin
                input Real x;
                output Real y;
            external "C" y = sin(x);
            end ExternalSin;
            "#,
        );
    }
}

// ============================================================================
// CRITICAL: TYPE COERCION EDGE CASES
// ============================================================================

/// Critical type coercion edge cases
mod coercion_critical {
    use super::*;

    /// Critical: Integer to Real in if-expression
    #[test]
    fn critical_coercion_if_expr() {
        expect_success(
            r#"
            model Test
                Boolean cond = true;
                Real x = if cond then 1 else 2.5;
            equation
            end Test;
            "#,
            "Test",
        );
    }

    /// Critical: Integer to Real in array construction
    #[test]
    fn critical_coercion_array() {
        expect_success(
            r#"
            model Test
                Real x[4] = {1, 2.0, 3, 4.5};
            equation
            end Test;
            "#,
            "Test",
        );
    }

    /// Critical: Integer to Real in function call
    #[test]
    fn critical_coercion_function() {
        expect_success(
            r#"
            function F
                input Real x;
                output Real y;
            algorithm
                y := x * 2;
            end F;

            model Test
                Real y = F(5);
            equation
            end Test;
            "#,
            "Test",
        );
    }

    /// Critical: Coercion with parameter expressions
    #[test]
    fn critical_coercion_parameter() {
        expect_success(
            r#"
            model Test
                parameter Integer n = 5;
                parameter Real x = n + 0.5;
            equation
            end Test;
            "#,
            "Test",
        );
    }

    /// Critical: No Real to Integer coercion
    #[test]
    fn critical_no_real_to_int() {
        expect_failure(
            r#"
            model Test
                Integer n = 5.5;
            equation
            end Test;
            "#,
            "Test",
        );
    }
}

// ============================================================================
// CRITICAL: ENUMERATION TYPE EDGE CASES
// ============================================================================

/// Critical enumeration type edge cases
mod enum_critical {
    use super::*;

    /// Critical: Enumeration in if-expression
    #[test]
    fn critical_enum_if_expr() {
        expect_success(
            r#"
            type State = enumeration(Off, On);

            model Test
                Boolean active = true;
                State s = if active then State.On else State.Off;
            equation
            end Test;
            "#,
            "Test",
        );
    }

    /// Critical: Enumeration comparison
    #[test]
    fn critical_enum_comparison() {
        expect_success(
            r#"
            type Level = enumeration(Low, Medium, High);

            model Test
                Level current = Level.Medium;
                Boolean isHigh;
            equation
                isHigh = current == Level.High;
            end Test;
            "#,
            "Test",
        );
    }

    /// Critical: Enumeration with descriptions
    #[test]
    fn critical_enum_descriptions() {
        expect_parse_success(
            r#"
            type Mode = enumeration(
                Off "System is off",
                Idle "System is idle",
                Running "System is running",
                Error "System has error"
            );

            model Test
                Mode m = Mode.Idle;
            end Test;
            "#,
        );
    }

    /// Critical: Enumeration in array
    #[test]
    fn critical_enum_array() {
        expect_parse_success(
            r#"
            type Color = enumeration(Red, Green, Blue);

            model Test
                Color palette[3] = {Color.Red, Color.Green, Color.Blue};
            end Test;
            "#,
        );
    }
}

// ============================================================================
// CRITICAL: COMPLEX TYPE SCENARIOS
// ============================================================================

/// Critical complex type scenarios
mod complex_critical {
    use super::*;

    /// Critical: Type alias chain
    #[test]
    fn critical_type_alias_chain() {
        expect_success(
            r#"
            type Level1 = Real;
            type Level2 = Level1(min = 0);
            type Level3 = Level2(max = 100);

            model Test
                Level3 x = 50;
            equation
            end Test;
            "#,
            "Test",
        );
    }

    /// Critical: Package-qualified types
    #[test]
    fn critical_package_types() {
        expect_parse_success(
            r#"
            package Units
                type Voltage = Real(unit = "V");
                type Current = Real(unit = "A");
                type Resistance = Real(unit = "Ohm");
            end Units;

            model Resistor
                Units.Voltage v;
                Units.Current i;
                parameter Units.Resistance R = 1;
            equation
                v = R * i;
                i = 1;
            end Resistor;
            "#,
        );
    }

    /// Critical: Nested package types
    #[test]
    fn critical_nested_package_types() {
        expect_parse_success(
            r#"
            package Outer
                package Inner
                    type Value = Real;
                end Inner;
            end Outer;

            model Test
                Outer.Inner.Value x = 1;
            end Test;
            "#,
        );
    }

    /// Critical: Import with type usage
    #[test]
    fn critical_import_types() {
        expect_parse_success(
            r#"
            package Math
                type Vector3 = Real[3];

                function Magnitude
                    input Vector3 v;
                    output Real m;
                algorithm
                    m := sqrt(v[1]^2 + v[2]^2 + v[3]^2);
                end Magnitude;
            end Math;

            model Test
                import Math.Vector3;
                import Math.Magnitude;

                Vector3 v = {3, 4, 0};
                Real m = Magnitude(v);
            end Test;
            "#,
        );
    }

    /// Critical: Generic-like pattern with replaceable
    #[test]
    fn critical_replaceable_type() {
        expect_parse_success(
            r#"
            model GenericFilter
                replaceable type DataType = Real;
                DataType input_val;
                DataType output_val;
            equation
                output_val = input_val;
            end GenericFilter;

            model IntFilter
                extends GenericFilter(redeclare type DataType = Integer);
            end IntFilter;
            "#,
        );
    }
}

// ============================================================================
// CRITICAL: TYPE ERROR DETECTION
// ============================================================================

/// Critical type error detection
mod error_critical {
    use super::*;

    /// Critical: Detect undefined type early
    #[test]
    fn error_undefined_type_early() {
        expect_failure(
            r#"
            model Test
                NonExistentType x;
            equation
            end Test;
            "#,
            "Test",
        );
    }

    /// Critical: Detect circular type definition
    #[test]

    fn error_circular_type() {
        expect_failure(
            r#"
            type A = B;
            type B = A;

            model Test
                A x;
            equation
            end Test;
            "#,
            "Test",
        );
    }

    /// Critical: Detect incompatible array operations
    #[test]
    fn error_incompatible_array_ops() {
        expect_failure(
            r#"
            model Test
                Real x[3] = {1, 2, 3};
                Real y[4] = {1, 2, 3, 4};
                Real z[3];
            equation
                z = x + y;
            end Test;
            "#,
            "Test",
        );
    }

    /// Critical: Detect wrong enumeration literal
    #[test]
    #[ignore = "Enumeration literal validation not yet implemented"]
    fn error_wrong_enum_literal() {
        expect_failure(
            r#"
            type Color = enumeration(Red, Green, Blue);

            model Test
                Color c = Color.Yellow;
            equation
            end Test;
            "#,
            "Test",
        );
    }
}
