//! MLS §6.2: The Modelica Type System
//!
//! Tests for the Modelica type system including:
//! - Predefined types
//! - Type definitions
//! - Array types
//! - Record types
//! - Type coercion
//!
//! Reference: https://specification.modelica.org/master/interface-or-type.html

use crate::spec::{expect_failure, expect_parse_success, expect_success};

// ============================================================================
// §6.2.1 PREDEFINED TYPES
// ============================================================================

/// MLS §6.2: Predefined scalar types
mod predefined_types {
    use super::*;

    /// Real type
    #[test]
    fn mls_6_2_type_real() {
        expect_success(
            r#"
            model Test
                Real x = 1.5;
                Real y = 2.5e-3;
                Real z = -1.0;
            equation
            end Test;
            "#,
            "Test",
        );
    }

    /// Integer type
    #[test]
    fn mls_6_2_type_integer() {
        expect_success(
            r#"
            model Test
                Integer n = 42;
                Integer m = -10;
                Integer k = 0;
            equation
            end Test;
            "#,
            "Test",
        );
    }

    /// Boolean type
    #[test]
    fn mls_6_2_type_boolean() {
        expect_success(
            r#"
            model Test
                Boolean a = true;
                Boolean b = false;
                Boolean c = a and b;
            equation
            end Test;
            "#,
            "Test",
        );
    }

    /// String type
    #[test]
    fn mls_6_2_type_string() {
        expect_parse_success(
            r#"
            model Test
                parameter String name = "test";
                parameter String empty = "";
            end Test;
            "#,
        );
    }

    /// Real with attributes
    #[test]
    fn mls_6_2_real_attributes() {
        expect_success(
            r#"
            model Test
                Real x(unit = "m", min = 0, max = 100, start = 50);
            equation
                x = 25;
            end Test;
            "#,
            "Test",
        );
    }

    /// Integer with attributes
    #[test]
    fn mls_6_2_integer_attributes() {
        expect_success(
            r#"
            model Test
                Integer n(min = 0, max = 10, start = 5);
            equation
                n = 7;
            end Test;
            "#,
            "Test",
        );
    }
}

// ============================================================================
// §6.2.2 TYPE DEFINITIONS
// ============================================================================

/// MLS §6.2: Type definitions
mod type_definitions {
    use super::*;

    /// Simple type alias
    #[test]
    fn mls_6_2_type_alias() {
        expect_success(
            r#"
            type Voltage = Real(unit = "V");

            model Test
                Voltage v = 5;
            equation
            end Test;
            "#,
            "Test",
        );
    }

    /// Type with constraints
    #[test]
    fn mls_6_2_type_with_constraints() {
        expect_success(
            r#"
            type PositiveReal = Real(min = 0);

            model Test
                PositiveReal x = 5;
            equation
            end Test;
            "#,
            "Test",
        );
    }

    /// Nested type definition
    #[test]
    fn mls_6_2_nested_type() {
        expect_parse_success(
            r#"
            package Units
                type Voltage = Real(unit = "V");
                type Current = Real(unit = "A");
                type Resistance = Real(unit = "Ohm");
            end Units;

            model Test
                Units.Voltage v = 5;
                Units.Current i = 1;
                Units.Resistance r = v / i;
            end Test;
            "#,
        );
    }

    /// Enumeration type
    #[test]
    fn mls_6_2_enumeration() {
        expect_success(
            r#"
            type Color = enumeration(Red, Green, Blue);

            model Test
                Color c = Color.Red;
            equation
            end Test;
            "#,
            "Test",
        );
    }

    /// Enumeration with description
    #[test]
    fn mls_6_2_enum_with_description() {
        expect_parse_success(
            r#"
            type State = enumeration(
                Off "System is off",
                Startup "System is starting",
                Running "System is running",
                Shutdown "System is shutting down"
            );

            model Test
                State s = State.Off;
            end Test;
            "#,
        );
    }
}

// ============================================================================
// §6.2.3 ARRAY TYPES
// ============================================================================

/// MLS §6.2: Array types
mod array_types {
    use super::*;

    /// 1D array type
    #[test]
    fn mls_6_2_array_1d() {
        expect_success(
            r#"
            model Test
                Real x[3] = {1, 2, 3};
            equation
            end Test;
            "#,
            "Test",
        );
    }

    /// 2D array type
    #[test]
    fn mls_6_2_array_2d() {
        expect_success(
            r#"
            model Test
                Real A[2, 3] = {{1, 2, 3}, {4, 5, 6}};
            equation
            end Test;
            "#,
            "Test",
        );
    }

    /// Array with parameter size
    #[test]
    fn mls_6_2_array_param_size() {
        expect_success(
            r#"
            model Test
                parameter Integer n = 5;
                Real x[n];
            equation
                x = fill(1, n);
            end Test;
            "#,
            "Test",
        );
    }

    /// Array type alias
    #[test]
    fn mls_6_2_array_type_alias() {
        expect_parse_success(
            r#"
            type Vector3 = Real[3];

            model Test
                Vector3 v = {1, 2, 3};
            end Test;
            "#,
        );
    }

    /// Matrix type alias
    #[test]
    fn mls_6_2_matrix_type_alias() {
        expect_parse_success(
            r#"
            type Matrix3x3 = Real[3, 3];

            model Test
                Matrix3x3 I = identity(3);
            end Test;
            "#,
        );
    }

    /// Array of custom type
    #[test]
    fn mls_6_2_array_of_type() {
        expect_success(
            r#"
            type Voltage = Real(unit = "V");

            model Test
                Voltage v[3] = {1, 2, 3};
            equation
            end Test;
            "#,
            "Test",
        );
    }
}

// ============================================================================
// §6.2.4 RECORD TYPES
// ============================================================================

/// MLS §6.2: Record types
mod record_types {
    use super::*;

    /// Simple record
    #[test]
    fn mls_6_2_simple_record() {
        expect_success(
            r#"
            record Point
                Real x;
                Real y;
            end Point;

            model Test
                Point p;
            equation
                p.x = 1;
                p.y = 2;
            end Test;
            "#,
            "Test",
        );
    }

    /// Record with defaults
    #[test]
    fn mls_6_2_record_defaults() {
        expect_parse_success(
            r#"
            record Config
                Real tolerance = 1e-6;
                Integer maxIter = 100;
                Boolean verbose = false;
            end Config;

            model Test
                Config cfg;
            end Test;
            "#,
        );
    }

    /// Nested record
    #[test]
    fn mls_6_2_nested_record() {
        expect_parse_success(
            r#"
            record Point
                Real x;
                Real y;
            end Point;

            record Line
                Point start;
                Point finish;
            end Line;

            model Test
                Line l;
            equation
                l.start.x = 0;
                l.start.y = 0;
                l.finish.x = 1;
                l.finish.y = 1;
            end Test;
            "#,
        );
    }

    /// Record constructor
    #[test]
    fn mls_6_2_record_constructor() {
        expect_parse_success(
            r#"
            record Point
                Real x;
                Real y;
            end Point;

            model Test
                Point p = Point(1, 2);
            end Test;
            "#,
        );
    }

    /// Record with array field
    #[test]
    fn mls_6_2_record_array_field() {
        expect_parse_success(
            r#"
            record Config
                Real values[3];
                Integer count;
            end Config;

            model Test
                Config c;
            equation
                c.values = {1, 2, 3};
                c.count = 3;
            end Test;
            "#,
        );
    }
}

// ============================================================================
// §6.2.5 TYPE COERCION
// ============================================================================

/// MLS §6.2: Type coercion rules
mod type_coercion {
    use super::*;

    /// Integer to Real coercion
    #[test]
    fn mls_6_2_int_to_real() {
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

    /// Integer in Real expression
    #[test]
    fn mls_6_2_int_in_real_expr() {
        expect_success(
            r#"
            model Test
                Real x = 2.5;
                Real y = x + 1;
            equation
            end Test;
            "#,
            "Test",
        );
    }

    /// Integer array to Real array
    #[test]
    fn mls_6_2_int_array_to_real() {
        expect_success(
            r#"
            model Test
                Real x[3] = {1, 2, 3};
            equation
            end Test;
            "#,
            "Test",
        );
    }

    /// Mixed integer and real in array
    #[test]
    fn mls_6_2_mixed_array() {
        expect_success(
            r#"
            model Test
                Real x[3] = {1, 2.5, 3};
            equation
            end Test;
            "#,
            "Test",
        );
    }

    /// Integer parameter with Real expression
    #[test]
    #[ignore = "Type checking for Integer with Real expression not implemented"]
    fn mls_6_2_no_real_to_int() {
        expect_failure(
            r#"
            model Test
                Integer n = 2.5;
            equation
            end Test;
            "#,
            "Test",
        );
    }
}

// ============================================================================
// NEGATIVE TESTS - Type errors
// ============================================================================

/// Type error cases
mod type_errors {
    use super::*;

    /// Type mismatch in assignment
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

    /// Array dimension mismatch
    #[test]
    fn error_array_dimension_mismatch() {
        expect_failure(
            r#"
            model Test
                Real x[3] = {1, 2};
            equation
            end Test;
            "#,
            "Test",
        );
    }

    /// Incompatible types in expression
    #[test]
    #[ignore = "Type checking in expressions not yet implemented"]
    fn error_incompatible_expression() {
        expect_failure(
            r#"
            model Test
                Real x = 1 + "hello";
            equation
            end Test;
            "#,
            "Test",
        );
    }

    /// Wrong type for Boolean operator
    #[test]
    fn error_boolean_operator_wrong_type() {
        expect_failure(
            r#"
            model Test
                Real x = 1;
                Real y = x and 2;
            equation
            end Test;
            "#,
            "Test",
        );
    }

    /// Undefined type
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
}
