//! MLS §6.3: Type Compatibility
//!
//! Tests for type compatibility rules including:
//! - Plug-compatibility for connectors
//! - Type equivalence
//! - Type compatibility in assignments
//! - Compatibility across arrays
//!
//! Reference: https://specification.modelica.org/master/interface-or-type.html

use crate::spec::{expect_failure, expect_parse_success, expect_success};

// ============================================================================
// §6.3.1 PLUG COMPATIBILITY - CONNECTORS
// ============================================================================

/// MLS §6.3: Plug compatibility for connections
mod plug_compatibility {
    use super::*;

    /// MLS: Identical connectors are plug-compatible
    #[test]
    fn mls_6_3_identical_connectors() {
        expect_success(
            r#"
            connector Pin
                Real v;
                flow Real i;
            end Pin;

            model Test
                Pin p1, p2, p3;
            equation
                connect(p1, p2);
                connect(p2, p3);
            end Test;
            "#,
            "Test",
        );
    }

    /// MLS: Extended connector is plug-compatible with base
    #[test]
    fn mls_6_3_extended_connector_compatible() {
        expect_parse_success(
            r#"
            connector BasePin
                Real v;
                flow Real i;
            end BasePin;

            connector ExtendedPin
                extends BasePin;
                Real temperature;
            end ExtendedPin;

            model Test
                ExtendedPin p1, p2;
            equation
                connect(p1, p2);
            end Test;
            "#,
        );
    }

    /// MLS: Flow variables must match for plug-compatibility
    #[test]
    fn mls_6_3_flow_must_match() {
        expect_success(
            r#"
            connector C
                Real e;
                flow Real f;
            end C;

            model TwoPort
                C p, n;
            equation
                connect(p, n);
            end TwoPort;
            "#,
            "TwoPort",
        );
    }

    /// MLS: Multiple flow variables in connector
    #[test]
    fn mls_6_3_multiple_flows() {
        expect_parse_success(
            r#"
            connector FluidPort
                Real p;
                flow Real m_flow;
                stream Real h_outflow;
            end FluidPort;

            model FluidComponent
                FluidPort inlet, outlet;
            equation
                connect(inlet, outlet);
            end FluidComponent;
            "#,
        );
    }

    /// MLS: Expandable connectors are plug-compatible
    #[test]
    fn mls_6_3_expandable_plug_compatible() {
        expect_parse_success(
            r#"
            expandable connector ControlBus
            end ControlBus;

            model Controller
                ControlBus bus1, bus2;
            equation
                connect(bus1, bus2);
            end Controller;
            "#,
        );
    }

    /// MLS: Nested connectors in compatibility
    #[test]
    fn mls_6_3_nested_connectors() {
        expect_parse_success(
            r#"
            connector SimplePin
                Real v;
                flow Real i;
            end SimplePin;

            connector ComplexPort
                SimplePin electrical;
                Real temperature;
            end ComplexPort;

            model Test
                ComplexPort a, b;
            equation
                connect(a, b);
            end Test;
            "#,
        );
    }
}

// ============================================================================
// §6.3.2 TYPE EQUIVALENCE
// ============================================================================

/// MLS §6.3: Type equivalence rules
mod type_equivalence {
    use super::*;

    /// MLS: Same predefined types are equivalent
    #[test]
    fn mls_6_3_predefined_equivalent() {
        expect_success(
            r#"
            model Test
                Real a = 1;
                Real b = 2;
                Real c;
            equation
                c = a + b;
            end Test;
            "#,
            "Test",
        );
    }

    /// MLS: Type alias equivalent to base type
    #[test]
    fn mls_6_3_type_alias_equivalent() {
        expect_success(
            r#"
            type Voltage = Real;

            model Test
                Voltage v = 5;
                Real r = 3;
                Real sum;
            equation
                sum = v + r;
            end Test;
            "#,
            "Test",
        );
    }

    /// MLS: Modified type alias
    #[test]
    fn mls_6_3_modified_type_alias() {
        expect_success(
            r#"
            type PositiveReal = Real(min = 0);

            model Test
                PositiveReal x = 5;
                Real y = 3;
                Real z;
            equation
                z = x + y;
            end Test;
            "#,
            "Test",
        );
    }

    /// MLS: Array type equivalence
    #[test]
    fn mls_6_3_array_type_equivalent() {
        expect_success(
            r#"
            model Test
                Real a[3] = {1, 2, 3};
                Real b[3] = {4, 5, 6};
                Real c[3];
            equation
                c = a + b;
            end Test;
            "#,
            "Test",
        );
    }

    /// MLS: Record type equivalence
    #[test]
    fn mls_6_3_record_type_equivalent() {
        expect_success(
            r#"
            record Point
                Real x;
                Real y;
            end Point;

            model Test
                Point a, b;
            equation
                a.x = 1;
                a.y = 2;
                b.x = a.x;
                b.y = a.y;
            end Test;
            "#,
            "Test",
        );
    }

    /// MLS: Enumeration type equivalence
    #[test]
    fn mls_6_3_enum_type_equivalent() {
        expect_success(
            r#"
            type State = enumeration(Off, On, Error);

            model Test
                State current = State.Off;
                State next;
            equation
                next = current;
            end Test;
            "#,
            "Test",
        );
    }
}

// ============================================================================
// §6.3.3 ASSIGNMENT COMPATIBILITY
// ============================================================================

/// MLS §6.3: Compatibility in assignments and equations
mod assignment_compatibility {
    use super::*;

    /// MLS: Integer to Real coercion in assignment
    #[test]
    fn mls_6_3_int_to_real_assignment() {
        expect_success(
            r#"
            model Test
                Real x = 5;
            equation
            end Test;
            "#,
            "Test",
        );
    }

    /// MLS: Integer to Real in equations
    #[test]
    fn mls_6_3_int_to_real_equation() {
        expect_success(
            r#"
            model Test
                Real x;
                Integer n = 10;
            equation
                x = n;
            end Test;
            "#,
            "Test",
        );
    }

    /// MLS: Integer to Real in expressions
    #[test]
    fn mls_6_3_int_to_real_expression() {
        expect_success(
            r#"
            model Test
                Real x = 2.5;
                Integer n = 3;
                Real y;
            equation
                y = x + n;
            end Test;
            "#,
            "Test",
        );
    }

    /// MLS: Array element assignment compatibility
    #[test]
    fn mls_6_3_array_element_compatible() {
        expect_success(
            r#"
            model Test
                Real x[3];
            equation
                x[1] = 1;
                x[2] = 2;
                x[3] = 3;
            end Test;
            "#,
            "Test",
        );
    }

    /// MLS: Mixed types in array literal
    #[test]
    fn mls_6_3_mixed_array_literal() {
        expect_success(
            r#"
            model Test
                Real x[4] = {1, 2.5, 3, 4.0};
            equation
            end Test;
            "#,
            "Test",
        );
    }

    /// MLS: Component with modified type
    #[test]
    fn mls_6_3_modified_component_compatible() {
        expect_success(
            r#"
            model Base
                parameter Real p = 1;
            equation
            end Base;

            model Test
                Base b(p = 5);
            equation
            end Test;
            "#,
            "Test",
        );
    }
}

// ============================================================================
// §6.3.4 ARRAY COMPATIBILITY
// ============================================================================

/// MLS §6.3: Array type compatibility rules
mod array_compatibility {
    use super::*;

    /// MLS: Arrays with same dimensions compatible
    #[test]
    fn mls_6_3_same_dimension_compatible() {
        expect_success(
            r#"
            model Test
                Real x[3] = {1, 2, 3};
                Real y[3];
            equation
                y = x;
            end Test;
            "#,
            "Test",
        );
    }

    /// MLS: 2D arrays with same shape
    #[test]
    #[ignore = "2D array equation assignment not yet supported"]
    fn mls_6_3_2d_same_shape() {
        expect_success(
            r#"
            model Test
                Real A[2, 3] = {{1, 2, 3}, {4, 5, 6}};
                Real B[2, 3];
            equation
                B = A;
            end Test;
            "#,
            "Test",
        );
    }

    /// MLS: Parameter-sized arrays
    #[test]
    fn mls_6_3_param_sized_arrays() {
        expect_success(
            r#"
            model Test
                parameter Integer n = 4;
                Real x[n];
                Real y[n];
            equation
                x = fill(1, n);
                y = x;
            end Test;
            "#,
            "Test",
        );
    }

    /// MLS: Arrays in function calls
    #[test]
    fn mls_6_3_array_function_compatible() {
        expect_success(
            r#"
            function SumArray
                input Real x[:];
                output Real s;
            algorithm
                s := sum(x);
            end SumArray;

            model Test
                Real x[5] = {1, 2, 3, 4, 5};
                Real total = SumArray(x);
            equation
            end Test;
            "#,
            "Test",
        );
    }

    /// MLS: Flexible array size (colon notation)
    #[test]
    fn mls_6_3_flexible_size_compatible() {
        expect_parse_success(
            r#"
            function Process
                input Real data[:];
                output Real result[:];
            algorithm
                result := 2 * data;
            end Process;

            model Test
                Real x[3] = {1, 2, 3};
                Real y[3] = Process(x);
            end Test;
            "#,
        );
    }
}

// ============================================================================
// §6.3.5 INPUT/OUTPUT COMPATIBILITY
// ============================================================================

/// MLS §6.3: Input/output compatibility for blocks and functions
mod io_compatibility {
    use super::*;

    /// MLS: Block input/output compatibility
    #[test]
    fn mls_6_3_block_io_compatible() {
        expect_success(
            r#"
            block Gain
                input Real u;
                output Real y;
                parameter Real k = 1;
            equation
                y = k * u;
            end Gain;

            model Test
                Gain g1(k = 2);
                Gain g2(k = 3);
                Real signal = 1;
            equation
                g1.u = signal;
                g2.u = g1.y;
            end Test;
            "#,
            "Test",
        );
    }

    /// MLS: Function input/output compatibility
    #[test]
    fn mls_6_3_function_io_compatible() {
        expect_success(
            r#"
            function Square
                input Real x;
                output Real y;
            algorithm
                y := x * x;
            end Square;

            function Cube
                input Real x;
                output Real y;
            algorithm
                y := x * x * x;
            end Cube;

            model Test
                Real a = 2;
                Real b = Square(a);
                Real c = Cube(b);
            equation
            end Test;
            "#,
            "Test",
        );
    }

    /// MLS: Chained block connections
    #[test]
    fn mls_6_3_chained_blocks() {
        expect_success(
            r#"
            block Scale
                input Real u;
                output Real y;
                parameter Real factor = 1;
            equation
                y = factor * u;
            end Scale;

            model Chain
                Scale s1(factor = 2);
                Scale s2(factor = 3);
                Scale s3(factor = 0.5);
                Real input_signal = 1;
                Real output_signal;
            equation
                s1.u = input_signal;
                s2.u = s1.y;
                s3.u = s2.y;
                output_signal = s3.y;
            end Chain;
            "#,
            "Chain",
        );
    }
}

// ============================================================================
// NEGATIVE TESTS - Compatibility violations
// ============================================================================

/// Type compatibility error cases
mod compatibility_errors {
    use super::*;

    /// MLS: Incompatible connector types
    #[test]
    fn error_incompatible_connectors() {
        expect_failure(
            r#"
            connector PinA
                Real voltage;
                flow Real current;
            end PinA;

            connector PinB
                Real v;
                flow Real i;
            end PinB;

            model Test
                PinA a;
                PinB b;
            equation
                connect(a, b);
            end Test;
            "#,
            "Test",
        );
    }

    /// MLS: Array dimension incompatibility
    #[test]
    fn error_array_dimension_incompatible() {
        expect_failure(
            r#"
            model Test
                Real x[3] = {1, 2, 3};
                Real y[4];
            equation
                y = x;
            end Test;
            "#,
            "Test",
        );
    }

    /// MLS: Array rank incompatibility
    #[test]
    fn error_array_rank_incompatible() {
        expect_failure(
            r#"
            model Test
                Real x[3] = {1, 2, 3};
                Real y[3, 3];
            equation
                y = x;
            end Test;
            "#,
            "Test",
        );
    }

    /// MLS: Type mismatch in assignment
    #[test]
    fn error_type_mismatch_assignment() {
        expect_failure(
            r#"
            model Test
                Boolean b = 5;
            equation
            end Test;
            "#,
            "Test",
        );
    }

    /// MLS: Undefined type reference
    #[test]
    fn error_undefined_type() {
        expect_failure(
            r#"
            model Test
                UndefinedType x;
            equation
            end Test;
            "#,
            "Test",
        );
    }

    /// MLS: Incompatible record types
    #[test]
    #[ignore = "Record type compatibility checking not yet implemented"]
    fn error_incompatible_records() {
        expect_failure(
            r#"
            record Point2D
                Real x;
                Real y;
            end Point2D;

            record Point3D
                Real x;
                Real y;
                Real z;
            end Point3D;

            model Test
                Point2D p2;
                Point3D p3;
            equation
                p2.x = 1;
                p2.y = 2;
                p3 = p2;
            end Test;
            "#,
            "Test",
        );
    }
}

// ============================================================================
// COMPLEX COMPATIBILITY SCENARIOS
// ============================================================================

/// Complex type compatibility scenarios
mod complex_scenarios {
    use super::*;

    /// Complex: Nested component compatibility
    #[test]
    fn complex_nested_component_compatible() {
        expect_success(
            r#"
            model Inner
                Real x = 0;
            equation
            end Inner;

            model Middle
                Inner sub;
            equation
            end Middle;

            model Outer
                Middle m1, m2;
                Real diff;
            equation
                diff = m1.sub.x - m2.sub.x;
            end Outer;
            "#,
            "Outer",
        );
    }

    /// Complex: Arrays of records
    #[test]
    fn complex_array_of_records() {
        expect_parse_success(
            r#"
            record Data
                Real value;
                Integer index;
            end Data;

            model Test
                Data items[3];
            equation
                items[1].value = 1;
                items[1].index = 1;
                items[2].value = 2;
                items[2].index = 2;
                items[3].value = 3;
                items[3].index = 3;
            end Test;
            "#,
        );
    }

    /// Complex: Function with record compatibility
    #[test]
    fn complex_function_record() {
        expect_parse_success(
            r#"
            record Point
                Real x;
                Real y;
            end Point;

            function Distance
                input Point a;
                input Point b;
                output Real d;
            algorithm
                d := sqrt((a.x - b.x)^2 + (a.y - b.y)^2);
            end Distance;

            model Test
                Point p1, p2;
                Real dist;
            equation
                p1.x = 0;
                p1.y = 0;
                p2.x = 3;
                p2.y = 4;
                dist = Distance(p1, p2);
            end Test;
            "#,
        );
    }
}
