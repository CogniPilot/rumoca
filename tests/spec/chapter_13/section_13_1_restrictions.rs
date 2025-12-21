//! MLS §13.1: Package Restrictions
//!
//! Tests for package-specific restrictions including:
//! - Package cannot have variables
//! - Package cannot have equations
//! - Package encapsulation rules
//! - Package constant handling
//!
//! Reference: https://specification.modelica.org/master/packages.html

use crate::spec::{expect_parse_success, expect_success};

// ============================================================================
// §13.1.1 PACKAGE CONTENT RESTRICTIONS
// ============================================================================

/// MLS §13.1: Package content restrictions
mod package_restrictions {
    use super::*;

    /// MLS: Package can only contain classes and constants
    #[test]
    fn mls_13_1_package_only_constants() {
        expect_parse_success(
            r#"
            package ValidPackage
                constant Real pi = 3.14159;
                constant Integer maxIter = 100;
                constant Boolean debug = false;
            end ValidPackage;
            "#,
        );
    }

    /// MLS: Package can contain type definitions
    #[test]
    fn mls_13_1_package_type_definitions() {
        expect_parse_success(
            r#"
            package Types
                type Voltage = Real(unit = "V");
                type Current = Real(unit = "A");
                type Angle = Real(unit = "rad", displayUnit = "deg");
            end Types;
            "#,
        );
    }

    /// MLS: Package can contain enumerations
    #[test]
    fn mls_13_1_package_enumerations() {
        expect_parse_success(
            r#"
            package Enums
                type State = enumeration(Off, On, Error);
                type Color = enumeration(Red, Green, Blue);
            end Enums;
            "#,
        );
    }

    /// MLS: Package can contain models
    #[test]
    fn mls_13_1_package_models() {
        expect_parse_success(
            r#"
            package Models
                model A
                    Real x;
                equation
                    x = 1;
                end A;

                model B
                    Real y;
                equation
                    y = 2;
                end B;
            end Models;
            "#,
        );
    }

    /// MLS: Package can contain connectors
    #[test]
    fn mls_13_1_package_connectors() {
        expect_parse_success(
            r#"
            package Connectors
                connector Pin
                    Real v;
                    flow Real i;
                end Pin;

                connector Flange
                    Real phi;
                    flow Real tau;
                end Flange;
            end Connectors;
            "#,
        );
    }

    /// MLS: Package can contain blocks
    #[test]
    fn mls_13_1_package_blocks() {
        expect_parse_success(
            r#"
            package Blocks
                block Gain
                    input Real u;
                    output Real y;
                    parameter Real k = 1;
                equation
                    y = k * u;
                end Gain;

                block Integrator
                    input Real u;
                    output Real y(start = 0);
                equation
                    der(y) = u;
                end Integrator;
            end Blocks;
            "#,
        );
    }

    /// MLS: Package can contain functions
    #[test]
    fn mls_13_1_package_functions() {
        expect_parse_success(
            r#"
            package Functions
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
            end Functions;
            "#,
        );
    }

    /// MLS: Package can contain records
    #[test]
    fn mls_13_1_package_records() {
        expect_parse_success(
            r#"
            package Records
                record Point
                    Real x;
                    Real y;
                end Point;

                record Line
                    Point start;
                    Point finish;
                end Line;
            end Records;
            "#,
        );
    }
}

// ============================================================================
// §13.1.2 PACKAGE ENCAPSULATION
// ============================================================================

/// MLS §13.1: Package encapsulation rules
mod package_encapsulation {
    use super::*;

    /// MLS: Encapsulated package
    #[test]
    fn mls_13_1_encapsulated_package() {
        expect_parse_success(
            r#"
            encapsulated package Isolated
                constant Real localConst = 1;
            end Isolated;
            "#,
        );
    }

    /// MLS: Encapsulated class in package
    #[test]
    fn mls_13_1_encapsulated_class() {
        expect_parse_success(
            r#"
            package Container
                encapsulated model Independent
                    Real x;
                equation
                    x = 1;
                end Independent;
            end Container;
            "#,
        );
    }

    /// MLS: Package access via qualified name
    #[test]
    fn mls_13_1_qualified_access() {
        expect_success(
            r#"
            package Lib
                constant Real value = 42;

                model Component
                    Real x = Lib.value;
                equation
                end Component;
            end Lib;
            "#,
            "Lib.Component",
        );
    }

    /// MLS: Nested package access
    #[test]
    fn mls_13_1_nested_access() {
        expect_parse_success(
            r#"
            package Outer
                package Inner
                    package Deep
                        constant Real x = 1;
                    end Deep;
                end Inner;
            end Outer;

            model Test
                Real value = Outer.Inner.Deep.x;
            end Test;
            "#,
        );
    }
}

// ============================================================================
// §13.1.3 PACKAGE WITH PROTECTED
// ============================================================================

/// MLS §13.1: Protected elements in packages
mod package_protected {
    use super::*;

    /// MLS: Package with protected section
    #[test]
    fn mls_13_1_protected_section() {
        expect_parse_success(
            r#"
            package WithProtected
            public
                constant Real publicConst = 1;
            protected
                constant Real privateConst = 2;
            end WithProtected;
            "#,
        );
    }

    /// MLS: Protected model in package
    #[test]
    fn mls_13_1_protected_model() {
        expect_parse_success(
            r#"
            package Library
                model PublicModel
                    Real x;
                equation
                    x = 1;
                end PublicModel;

            protected
                model PrivateModel
                    Real y;
                equation
                    y = 2;
                end PrivateModel;
            end Library;
            "#,
        );
    }

    /// MLS: Mixed visibility
    #[test]
    fn mls_13_1_mixed_visibility() {
        expect_parse_success(
            r#"
            package Mixed
                constant Real a = 1;
            protected
                constant Real b = 2;
            public
                constant Real c = 3;
            protected
                constant Real d = 4;
            end Mixed;
            "#,
        );
    }
}

// ============================================================================
// §13.1.4 PARTIAL PACKAGE
// ============================================================================

/// MLS §13.1: Partial packages
mod partial_package {
    use super::*;

    /// MLS: Partial package definition
    #[test]
    fn mls_13_1_partial_package() {
        expect_parse_success(
            r#"
            partial package BaseLibrary
                constant Real version = 1.0;
            end BaseLibrary;
            "#,
        );
    }

    /// MLS: Extending partial package
    #[test]
    fn mls_13_1_extend_partial_package() {
        expect_parse_success(
            r#"
            partial package BaseLibrary
                constant Real version = 1.0;
            end BaseLibrary;

            package CompleteLibrary
                extends BaseLibrary;
                model Component
                    Real x;
                equation
                    x = version;
                end Component;
            end CompleteLibrary;
            "#,
        );
    }
}

// ============================================================================
// PACKAGE USAGE
// ============================================================================

/// MLS §13.1: Package usage patterns
mod package_usage {
    use super::*;

    /// MLS: Using package contents
    #[test]
    fn mls_13_1_use_package_model() {
        expect_success(
            r#"
            package Lib
                model Component
                    Real x = 1;
                equation
                end Component;
            end Lib;

            model Test
                Lib.Component c;
            equation
            end Test;
            "#,
            "Test",
        );
    }

    /// MLS: Using package constant
    #[test]
    fn mls_13_1_use_package_constant() {
        expect_success(
            r#"
            package Constants
                constant Real PI = 3.14159;
            end Constants;

            model Test
                Real x = Constants.PI;
            equation
            end Test;
            "#,
            "Test",
        );
    }

    /// MLS: Using package type
    #[test]
    fn mls_13_1_use_package_type() {
        expect_parse_success(
            r#"
            package SI
                type Length = Real(unit = "m");
            end SI;

            model Test
                SI.Length distance = 100;
            end Test;
            "#,
        );
    }

    /// MLS: Using package function
    #[test]
    fn mls_13_1_use_package_function() {
        expect_success(
            r#"
            package Math
                function Square
                    input Real x;
                    output Real y;
                algorithm
                    y := x * x;
                end Square;
            end Math;

            model Test
                Real y = Math.Square(5);
            equation
            end Test;
            "#,
            "Test",
        );
    }
}
