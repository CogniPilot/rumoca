//! MLS §6.4: Subtyping
//!
//! Tests for subtyping rules including:
//! - Class subtyping
//! - Record subtyping
//! - Connector subtyping
//! - Array subtyping
//!
//! Reference: https://specification.modelica.org/master/interface-or-type.html

use crate::spec::{expect_failure, expect_parse_success, expect_success};

// ============================================================================
// §6.4.1 CLASS SUBTYPING
// ============================================================================

/// MLS §6.4: Class subtyping via inheritance
mod class_subtyping {
    use super::*;

    /// Derived class is subtype of base
    #[test]
    fn mls_6_4_derived_is_subtype() {
        expect_success(
            r#"
            model Base
                Real x;
            equation
                x = 1;
            end Base;

            model Derived
                extends Base;
                Real y;
            equation
                y = x + 1;
            end Derived;
            "#,
            "Derived",
        );
    }

    /// Multiple inheritance subtyping
    #[test]
    fn mls_6_4_multiple_inheritance() {
        expect_success(
            r#"
            model A
                Real a;
            equation
            end A;

            model B
                Real b;
            equation
            end B;

            model C
                extends A;
                extends B;
            equation
                a = 1;
                b = 2;
            end C;
            "#,
            "C",
        );
    }

    /// Chain of inheritance
    #[test]
    fn mls_6_4_inheritance_chain() {
        expect_success(
            r#"
            model A
                Real a;
            equation
            end A;

            model B
                extends A;
                Real b;
            equation
            end B;

            model C
                extends B;
                Real c;
            equation
                a = 1;
                b = 2;
                c = 3;
            end C;
            "#,
            "C",
        );
    }
}

// ============================================================================
// §6.4.2 RECORD SUBTYPING
// ============================================================================

/// MLS §6.4: Record subtyping
mod record_subtyping {
    use super::*;

    /// Extended record is subtype
    #[test]
    fn mls_6_4_extended_record() {
        expect_parse_success(
            r#"
            record Point2D
                Real x;
                Real y;
            end Point2D;

            record Point3D
                extends Point2D;
                Real z;
            end Point3D;

            model Test
                Point3D p;
            equation
                p.x = 1;
                p.y = 2;
                p.z = 3;
            end Test;
            "#,
        );
    }

    /// Record with additional fields
    #[test]
    fn mls_6_4_record_additional_fields() {
        expect_parse_success(
            r#"
            record Config
                Real tolerance;
            end Config;

            record ExtendedConfig
                extends Config;
                Integer maxIterations;
            end ExtendedConfig;

            model Test
                ExtendedConfig cfg;
            equation
                cfg.tolerance = 1e-6;
                cfg.maxIterations = 100;
            end Test;
            "#,
        );
    }
}

// ============================================================================
// §6.4.3 CONNECTOR SUBTYPING
// ============================================================================

/// MLS §6.4: Connector subtyping
mod connector_subtyping {
    use super::*;

    /// Extended connector is subtype
    #[test]
    fn mls_6_4_extended_connector() {
        expect_parse_success(
            r#"
            connector BasicPin
                Real v;
                flow Real i;
            end BasicPin;

            connector ExtendedPin
                extends BasicPin;
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

    /// Connector hierarchy
    #[test]
    fn mls_6_4_connector_hierarchy() {
        expect_parse_success(
            r#"
            connector FluidPort
                Real p;
                flow Real m_flow;
            end FluidPort;

            connector FluidPortA
                extends FluidPort;
            end FluidPortA;

            connector FluidPortB
                extends FluidPort;
            end FluidPortB;

            model Test
                FluidPortA a;
                FluidPortB b;
            equation
                connect(a, b);
            end Test;
            "#,
        );
    }
}

// ============================================================================
// §6.4.4 ARRAY SUBTYPING
// ============================================================================

/// MLS §6.4: Array subtyping
mod array_subtyping {
    use super::*;

    /// Array type compatibility
    #[test]
    fn mls_6_4_array_compatible() {
        expect_success(
            r#"
            model Test
                Real x[3];
                Real y[3];
            equation
                x = {1, 2, 3};
                y = x;
            end Test;
            "#,
            "Test",
        );
    }

    /// Array of subtype elements
    #[test]
    fn mls_6_4_array_of_subtype() {
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

    /// 2D array compatibility
    #[test]
    #[ignore = "2D array literal equation not yet supported"]
    fn mls_6_4_2d_array_compatible() {
        expect_success(
            r#"
            model Test
                Real A[2, 2];
                Real B[2, 2];
            equation
                A = {{1, 2}, {3, 4}};
                B = A;
            end Test;
            "#,
            "Test",
        );
    }
}

// ============================================================================
// §6.5 PLUG COMPATIBILITY
// ============================================================================

/// MLS §6.5: Plug compatibility for connectors
mod plug_compatibility {
    use super::*;

    /// Basic plug compatible connectors
    #[test]
    fn mls_6_5_basic_plug_compatible() {
        expect_success(
            r#"
            connector Pin
                Real v;
                flow Real i;
            end Pin;

            model Test
                Pin p1, p2;
            equation
                connect(p1, p2);
            end Test;
            "#,
            "Test",
        );
    }

    /// Multiple flow variables
    #[test]
    fn mls_6_5_multiple_flow() {
        expect_parse_success(
            r#"
            connector FluidPort
                Real p;
                flow Real m_flow;
                stream Real h_outflow;
            end FluidPort;

            model Test
                FluidPort a, b;
            equation
                connect(a, b);
            end Test;
            "#,
        );
    }

    /// Expandable connector plug compatibility
    #[test]
    fn mls_6_5_expandable_plug() {
        expect_parse_success(
            r#"
            expandable connector Bus
            end Bus;

            model Test
                Bus bus1, bus2;
            equation
                connect(bus1, bus2);
            end Test;
            "#,
        );
    }
}

// ============================================================================
// NEGATIVE TESTS - Subtyping errors
// ============================================================================

/// Subtyping error cases
mod subtyping_errors {
    use super::*;

    /// Array dimension mismatch (not subtypes)
    #[test]
    fn error_array_dimension_mismatch() {
        expect_failure(
            r#"
            model Test
                Real x[3];
                Real y[4];
            equation
                x = {1, 2, 3};
                y = x;
            end Test;
            "#,
            "Test",
        );
    }

    /// Array rank mismatch
    #[test]
    fn error_array_rank_mismatch() {
        expect_failure(
            r#"
            model Test
                Real x[3];
                Real y[3, 3];
            equation
                x = {1, 2, 3};
                y = x;
            end Test;
            "#,
            "Test",
        );
    }

    /// Incompatible connector types
    #[test]
    fn error_incompatible_connectors() {
        expect_failure(
            r#"
            connector PinA
                Real v;
                flow Real i;
            end PinA;

            connector PinB
                Real voltage;
                flow Real current;
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
}

// ============================================================================
// TYPE COMPATIBLE EXPRESSIONS
// ============================================================================

/// MLS §6.7: Type compatible expressions
mod type_compatible_expressions {
    use super::*;

    /// If-expression type compatibility
    #[test]
    fn mls_6_7_if_expression_compatible() {
        expect_success(
            r#"
            model Test
                Boolean cond = true;
                Real x = if cond then 1 else 2;
            equation
            end Test;
            "#,
            "Test",
        );
    }

    /// If-expression with Integer and Real (coercion)
    #[test]
    fn mls_6_7_if_expression_coercion() {
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

    /// Array expression type compatibility
    #[test]
    fn mls_6_7_array_expression_compatible() {
        expect_success(
            r#"
            model Test
                Real x[3] = {1, 2, 3};
                Real y[3] = {4, 5, 6};
                Real z[3] = x + y;
            equation
            end Test;
            "#,
            "Test",
        );
    }

    /// Function result type
    #[test]
    fn mls_6_7_function_result_type() {
        expect_success(
            r#"
            function Square
                input Real x;
                output Real y;
            algorithm
                y := x * x;
            end Square;

            model Test
                Real x = Square(5);
            equation
            end Test;
            "#,
            "Test",
        );
    }
}
