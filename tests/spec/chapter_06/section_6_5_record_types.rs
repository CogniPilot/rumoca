//! MLS §6.5: Record Types and Subtyping
//!
//! Tests for record type semantics including:
//! - Record field ordering
//! - Record inheritance
//! - Record constructors
//! - Record type compatibility
//! - Record array operations
//!
//! Reference: https://specification.modelica.org/master/interface-or-type.html

use crate::spec::{expect_failure, expect_parse_success, expect_success};

// ============================================================================
// §6.5.1 RECORD FIELD SEMANTICS
// ============================================================================

/// MLS §6.5: Record field semantics
mod record_fields {
    use super::*;

    /// MLS: Records with multiple field types
    #[test]
    fn mls_6_5_multiple_field_types() {
        expect_success(
            r#"
            record MixedRecord
                Real value;
                Integer count;
                Boolean active;
            end MixedRecord;

            model Test
                MixedRecord r;
            equation
                r.value = 1.5;
                r.count = 10;
                r.active = true;
            end Test;
            "#,
            "Test",
        );
    }

    /// MLS: Record with array fields
    #[test]
    fn mls_6_5_array_fields() {
        expect_parse_success(
            r#"
            record VectorData
                Real values[3];
                Integer indices[3];
            end VectorData;

            model Test
                VectorData data;
            equation
                data.values = {1, 2, 3};
                data.indices = {1, 2, 3};
            end Test;
            "#,
        );
    }

    /// MLS: Record with nested record fields
    #[test]
    fn mls_6_5_nested_fields() {
        expect_parse_success(
            r#"
            record Inner
                Real x;
                Real y;
            end Inner;

            record Outer
                Inner start;
                Inner finish;
            end Outer;

            model Test
                Outer line;
            equation
                line.start.x = 0;
                line.start.y = 0;
                line.finish.x = 1;
                line.finish.y = 1;
            end Test;
            "#,
        );
    }

    /// MLS: Record with default values
    #[test]
    fn mls_6_5_default_values() {
        expect_parse_success(
            r#"
            record Config
                Real tolerance = 1e-6;
                Integer maxIterations = 100;
                Boolean verbose = false;
            end Config;

            model Test
                Config cfg;
            end Test;
            "#,
        );
    }

    /// MLS: Record with parameter fields
    #[test]
    fn mls_6_5_parameter_fields() {
        expect_parse_success(
            r#"
            record Settings
                parameter Real gain = 1.0;
                parameter Integer samples = 10;
            end Settings;

            model Test
                Settings s;
            end Test;
            "#,
        );
    }

    /// MLS: Record field access in expressions
    #[test]
    fn mls_6_5_field_access_expr() {
        expect_success(
            r#"
            record Point
                Real x;
                Real y;
            end Point;

            model Test
                Point p;
                Real magnitude;
            equation
                p.x = 3;
                p.y = 4;
                magnitude = sqrt(p.x^2 + p.y^2);
            end Test;
            "#,
            "Test",
        );
    }
}

// ============================================================================
// §6.5.2 RECORD INHERITANCE
// ============================================================================

/// MLS §6.5: Record inheritance semantics
mod record_inheritance {
    use super::*;

    /// MLS: Simple record extension
    #[test]
    fn mls_6_5_simple_extension() {
        expect_parse_success(
            r#"
            record Base
                Real a;
            end Base;

            record Extended
                extends Base;
                Real b;
            end Extended;

            model Test
                Extended e;
            equation
                e.a = 1;
                e.b = 2;
            end Test;
            "#,
        );
    }

    /// MLS: Multi-level record inheritance
    #[test]
    fn mls_6_5_multilevel_inheritance() {
        expect_parse_success(
            r#"
            record Level1
                Real a;
            end Level1;

            record Level2
                extends Level1;
                Real b;
            end Level2;

            record Level3
                extends Level2;
                Real c;
            end Level3;

            model Test
                Level3 r;
            equation
                r.a = 1;
                r.b = 2;
                r.c = 3;
            end Test;
            "#,
        );
    }

    /// MLS: Record with modified inheritance
    #[test]
    fn mls_6_5_modified_inheritance() {
        expect_parse_success(
            r#"
            record Base
                Real x = 1;
            end Base;

            record Modified
                extends Base(x = 10);
            end Modified;

            model Test
                Modified m;
            end Test;
            "#,
        );
    }

    /// MLS: Multiple record inheritance
    #[test]
    fn mls_6_5_multiple_inheritance() {
        expect_parse_success(
            r#"
            record Position
                Real x;
                Real y;
            end Position;

            record Velocity
                Real vx;
                Real vy;
            end Velocity;

            record State
                extends Position;
                extends Velocity;
            end State;

            model Test
                State s;
            equation
                s.x = 0;
                s.y = 0;
                s.vx = 1;
                s.vy = 2;
            end Test;
            "#,
        );
    }
}

// ============================================================================
// §6.5.3 RECORD CONSTRUCTORS
// ============================================================================

/// MLS §6.5: Record constructor semantics
mod record_constructors {
    use super::*;

    /// MLS: Positional constructor
    #[test]
    fn mls_6_5_positional_constructor() {
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

    /// MLS: Named constructor arguments
    #[test]
    fn mls_6_5_named_constructor() {
        expect_parse_success(
            r#"
            record Config
                Real tolerance;
                Integer iterations;
            end Config;

            model Test
                Config c = Config(iterations = 100, tolerance = 1e-6);
            end Test;
            "#,
        );
    }

    /// MLS: Constructor with defaults
    #[test]
    fn mls_6_5_constructor_defaults() {
        expect_parse_success(
            r#"
            record Settings
                Real value = 1.0;
                Integer count = 10;
            end Settings;

            model Test
                Settings s1 = Settings();
                Settings s2 = Settings(value = 2.0);
                Settings s3 = Settings(2.0, 20);
            end Test;
            "#,
        );
    }

    /// MLS: Nested record construction
    #[test]
    fn mls_6_5_nested_constructor() {
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
                Line l = Line(Point(0, 0), Point(1, 1));
            end Test;
            "#,
        );
    }

    /// MLS: Constructor in equation
    #[test]
    fn mls_6_5_constructor_in_equation() {
        expect_parse_success(
            r#"
            record Point
                Real x;
                Real y;
            end Point;

            model Test
                Point p;
            equation
                p = Point(time, 2*time);
            end Test;
            "#,
        );
    }
}

// ============================================================================
// §6.5.4 RECORD TYPE COMPATIBILITY
// ============================================================================

/// MLS §6.5: Record type compatibility rules
mod record_compatibility {
    use super::*;

    /// MLS: Same record types are compatible
    #[test]
    fn mls_6_5_same_type_compatible() {
        expect_success(
            r#"
            record Data
                Real value;
            end Data;

            model Test
                Data a, b;
            equation
                a.value = 1;
                b.value = a.value;
            end Test;
            "#,
            "Test",
        );
    }

    /// MLS: Record assignment compatibility
    #[test]
    fn mls_6_5_assignment_compatible() {
        expect_parse_success(
            r#"
            record Point
                Real x;
                Real y;
            end Point;

            model Test
                Point p1, p2;
            equation
                p1.x = 1;
                p1.y = 2;
                p2 = p1;
            end Test;
            "#,
        );
    }

    /// MLS: Extended record assignment
    #[test]
    fn mls_6_5_extended_assignment() {
        expect_parse_success(
            r#"
            record Base
                Real a;
            end Base;

            record Extended
                extends Base;
                Real b;
            end Extended;

            model Test
                Extended e1, e2;
            equation
                e1.a = 1;
                e1.b = 2;
                e2 = e1;
            end Test;
            "#,
        );
    }

    /// MLS: Record in function compatibility
    #[test]
    fn mls_6_5_function_compatible() {
        expect_parse_success(
            r#"
            record Point
                Real x;
                Real y;
            end Point;

            function Magnitude
                input Point p;
                output Real m;
            algorithm
                m := sqrt(p.x^2 + p.y^2);
            end Magnitude;

            model Test
                Point p;
                Real m;
            equation
                p.x = 3;
                p.y = 4;
                m = Magnitude(p);
            end Test;
            "#,
        );
    }
}

// ============================================================================
// §6.5.5 RECORD ARRAY OPERATIONS
// ============================================================================

/// MLS §6.5: Arrays of records
mod record_arrays {
    use super::*;

    /// MLS: Simple array of records
    #[test]
    fn mls_6_5_simple_array() {
        expect_parse_success(
            r#"
            record Point
                Real x;
                Real y;
            end Point;

            model Test
                Point points[3];
            equation
                points[1].x = 0;
                points[1].y = 0;
                points[2].x = 1;
                points[2].y = 0;
                points[3].x = 1;
                points[3].y = 1;
            end Test;
            "#,
        );
    }

    /// MLS: Array of records with constructor
    #[test]
    fn mls_6_5_array_constructor() {
        expect_parse_success(
            r#"
            record Data
                Real value;
            end Data;

            model Test
                Data items[3] = {Data(1), Data(2), Data(3)};
            end Test;
            "#,
        );
    }

    /// MLS: Record field array access
    #[test]
    fn mls_6_5_field_array_access() {
        expect_parse_success(
            r#"
            record Container
                Real values[3];
            end Container;

            model Test
                Container c;
                Real sum;
            equation
                c.values = {1, 2, 3};
                sum = c.values[1] + c.values[2] + c.values[3];
            end Test;
            "#,
        );
    }

    /// MLS: 2D array of records
    #[test]
    fn mls_6_5_2d_array_records() {
        expect_parse_success(
            r#"
            record Cell
                Real value;
            end Cell;

            model Test
                Cell grid[2, 2];
            equation
                grid[1, 1].value = 1;
                grid[1, 2].value = 2;
                grid[2, 1].value = 3;
                grid[2, 2].value = 4;
            end Test;
            "#,
        );
    }
}

// ============================================================================
// §6.5.6 RECORD EXPRESSIONS
// ============================================================================

/// MLS §6.5: Record expressions
mod record_expressions {
    use super::*;

    /// MLS: Record in if-expression
    #[test]
    fn mls_6_5_record_if_expression() {
        expect_parse_success(
            r#"
            record Point
                Real x;
                Real y;
            end Point;

            model Test
                Boolean cond = true;
                Point p1, p2, result;
            equation
                p1.x = 1;
                p1.y = 1;
                p2.x = 2;
                p2.y = 2;
                result = if cond then p1 else p2;
            end Test;
            "#,
        );
    }

    /// MLS: Record comparison
    #[test]

    fn mls_6_5_record_comparison() {
        expect_success(
            r#"
            record Point
                Real x;
                Real y;
            end Point;

            model Test
                Point p1, p2;
                Boolean equal;
            equation
                p1.x = 1;
                p1.y = 2;
                p2.x = 1;
                p2.y = 2;
                equal = p1 == p2;
            end Test;
            "#,
            "Test",
        );
    }

    /// MLS: Record field arithmetic
    #[test]
    fn mls_6_5_field_arithmetic() {
        expect_success(
            r#"
            record Vector
                Real x;
                Real y;
            end Vector;

            model Test
                Vector a, b;
                Real dot;
            equation
                a.x = 1;
                a.y = 2;
                b.x = 3;
                b.y = 4;
                dot = a.x * b.x + a.y * b.y;
            end Test;
            "#,
            "Test",
        );
    }
}

// ============================================================================
// NEGATIVE TESTS - Record type errors
// ============================================================================

/// Record type error cases
mod record_errors {
    use super::*;

    /// MLS: Access undefined field
    #[test]
    fn error_undefined_field() {
        expect_failure(
            r#"
            record Point
                Real x;
                Real y;
            end Point;

            model Test
                Point p;
                Real z;
            equation
                p.x = 1;
                p.y = 2;
                z = p.z;
            end Test;
            "#,
            "Test",
        );
    }

    /// MLS: Incompatible record assignment
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

    /// MLS: Wrong field type
    #[test]
    #[ignore = "Record field type checking not yet implemented"]
    fn error_wrong_field_type() {
        expect_failure(
            r#"
            record Data
                Integer count;
            end Data;

            model Test
                Data d;
            equation
                d.count = true;
            end Test;
            "#,
            "Test",
        );
    }
}
