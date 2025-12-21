//! MLS §13: Packages - Detailed Conformance Tests
//!
//! Comprehensive tests covering normative statements from MLS §13 including:
//! - §13.1: Package as a specialized class
//! - §13.2: Import statements
//! - §13.3: Package structure
//!
//! Reference: https://specification.modelica.org/master/packages.html

use crate::spec::{expect_failure, expect_parse_success, expect_success};

// ============================================================================
// §13.1 PACKAGE AS SPECIALIZED CLASS
// ============================================================================

/// MLS §13.1: Package normative requirements
mod package_class {
    use super::*;

    /// Basic package declaration
    #[test]
    fn mls_13_1_basic_package() {
        expect_parse_success(
            r#"
            package P
            end P;
            "#,
        );
    }

    /// Package with constants
    #[test]
    fn mls_13_1_package_with_constants() {
        expect_parse_success(
            r#"
            package Constants
                constant Real pi = 3.14159265358979;
                constant Real e = 2.71828182845905;
                constant Real g = 9.81;
            end Constants;
            "#,
        );
    }

    /// Package with nested classes
    #[test]
    fn mls_13_1_package_with_classes() {
        expect_parse_success(
            r#"
            package Electrical
                model Resistor
                    parameter Real R = 1;
                    Real v;
                    Real i;
                equation
                    v = R * i;
                end Resistor;

                model Capacitor
                    parameter Real C = 1;
                    Real v;
                    Real i;
                equation
                    i = C * der(v);
                end Capacitor;
            end Electrical;
            "#,
        );
    }

    /// Nested packages
    #[test]
    fn mls_13_1_nested_packages() {
        expect_parse_success(
            r#"
            package Outer
                package Inner
                    constant Real x = 1;
                end Inner;
            end Outer;
            "#,
        );
    }

    /// Package with type definitions
    #[test]
    fn mls_13_1_package_with_types() {
        expect_parse_success(
            r#"
            package SI
                type Voltage = Real(unit = "V");
                type Current = Real(unit = "A");
                type Resistance = Real(unit = "Ohm");
            end SI;
            "#,
        );
    }

    /// Package with functions
    #[test]
    fn mls_13_1_package_with_functions() {
        expect_parse_success(
            r#"
            package Math
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
            end Math;
            "#,
        );
    }

    /// Package with records
    #[test]
    fn mls_13_1_package_with_records() {
        expect_parse_success(
            r#"
            package Geometry
                record Point
                    Real x;
                    Real y;
                end Point;

                record Rectangle
                    Point corner;
                    Real width;
                    Real height;
                end Rectangle;
            end Geometry;
            "#,
        );
    }

    /// Partial package
    #[test]
    fn mls_13_1_partial_package() {
        expect_parse_success(
            r#"
            partial package PartialMedium
                constant Real R = 8.314;
            end PartialMedium;
            "#,
        );
    }

    /// Encapsulated package
    #[test]
    fn mls_13_1_encapsulated_package() {
        expect_parse_success(
            r#"
            encapsulated package MyPackage
                constant Real x = 1;
            end MyPackage;
            "#,
        );
    }
}

// ============================================================================
// §13.2 IMPORTING DEFINITIONS
// ============================================================================

/// MLS §13.2: Import statement normative requirements
mod import_statements {
    use super::*;

    /// Qualified import
    #[test]
    fn mls_13_2_qualified_import() {
        expect_parse_success(
            r#"
            package P
                constant Real c = 1;
            end P;

            model Test
                import P;
                Real x = P.c;
            equation
            end Test;
            "#,
        );
    }

    /// Single definition import
    #[test]
    fn mls_13_2_single_import() {
        expect_parse_success(
            r#"
            package P
                constant Real c = 1;
            end P;

            model Test
                import P.c;
                Real x = c;
            equation
            end Test;
            "#,
        );
    }

    /// Renamed import
    #[test]
    fn mls_13_2_renamed_import() {
        expect_parse_success(
            r#"
            package LongPackageName
                constant Real x = 1;
            end LongPackageName;

            model Test
                import LP = LongPackageName;
                Real y = LP.x;
            equation
            end Test;
            "#,
        );
    }

    /// Wildcard import
    #[test]
    fn mls_13_2_wildcard_import() {
        expect_parse_success(
            r#"
            package P
                constant Real a = 1;
                constant Real b = 2;
            end P;

            model Test
                import P.*;
                Real x = a + b;
            equation
            end Test;
            "#,
        );
    }

    /// Import from nested package
    #[test]
    fn mls_13_2_nested_import() {
        expect_parse_success(
            r#"
            package Outer
                package Inner
                    constant Real c = 42;
                end Inner;
            end Outer;

            model Test
                import Outer.Inner.c;
                Real x = c;
            equation
            end Test;
            "#,
        );
    }

    /// Multiple imports
    #[test]
    fn mls_13_2_multiple_imports() {
        expect_parse_success(
            r#"
            package A
                constant Real a = 1;
            end A;

            package B
                constant Real b = 2;
            end B;

            model Test
                import A.a;
                import B.b;
                Real x = a + b;
            equation
            end Test;
            "#,
        );
    }

    /// Import in package
    #[test]
    fn mls_13_2_import_in_package() {
        expect_parse_success(
            r#"
            package Constants
                constant Real pi = 3.14159;
            end Constants;

            package Derived
                import Constants.pi;
                constant Real twoPi = 2 * pi;
            end Derived;
            "#,
        );
    }
}

// ============================================================================
// §13.3 PACKAGE STRUCTURE
// ============================================================================

/// MLS §13.3: Package structure requirements
mod package_structure {
    use super::*;

    /// Package with extends
    #[test]
    fn mls_13_3_package_extends() {
        expect_parse_success(
            r#"
            package Base
                constant Real a = 1;
            end Base;

            package Extended
                extends Base;
                constant Real b = a + 1;
            end Extended;
            "#,
        );
    }

    /// Package with public and protected sections
    #[test]
    fn mls_13_3_public_protected() {
        expect_parse_success(
            r#"
            package P
            public
                constant Real publicConst = 1;
            protected
                constant Real internalConst = 2;
            public
                constant Real anotherPublic = publicConst + internalConst;
            end P;
            "#,
        );
    }

    /// Package with replaceable
    #[test]
    fn mls_13_3_replaceable_in_package() {
        expect_parse_success(
            r#"
            package GenericPackage
                replaceable model DefaultModel
                    Real x = 1;
                equation
                end DefaultModel;
            end GenericPackage;
            "#,
        );
    }

    /// Package with constrainedby
    #[test]
    fn mls_13_3_constrainedby_in_package() {
        expect_parse_success(
            r#"
            partial model PartialModel
                Real x;
            end PartialModel;

            model ConcreteModel
                extends PartialModel;
            equation
                x = 1;
            end ConcreteModel;

            package Container
                replaceable model M = ConcreteModel constrainedby PartialModel;
            end Container;
            "#,
        );
    }
}

// ============================================================================
// USING PACKAGES
// ============================================================================

/// Using packages in models
mod using_packages {
    use super::*;

    /// Using package constants
    #[test]
    fn using_package_constants() {
        expect_success(
            r#"
            package Constants
                constant Real pi = 3.14159;
            end Constants;

            model Test
                import Constants.pi;
                Real circumference;
                parameter Real radius = 1;
            equation
                circumference = 2 * pi * radius;
            end Test;
            "#,
            "Test",
        );
    }

    /// Using package types
    #[test]
    fn using_package_types() {
        expect_success(
            r#"
            package Units
                type Voltage = Real(unit = "V");
            end Units;

            model Test
                import Units.Voltage;
                Voltage v = 5;
            equation
            end Test;
            "#,
            "Test",
        );
    }

    /// Using package functions
    #[test]
    fn using_package_functions() {
        expect_success(
            r#"
            package MathUtils
                function Square
                    input Real x;
                    output Real y;
                algorithm
                    y := x * x;
                end Square;
            end MathUtils;

            model Test
                import MathUtils.Square;
                Real y = Square(5);
            equation
            end Test;
            "#,
            "Test",
        );
    }

    /// Using package models
    #[test]
    fn using_package_models() {
        expect_success(
            r#"
            package Components
                model Source
                    parameter Real value = 1;
                    Real y = value;
                equation
                end Source;
            end Components;

            model Test
                import Components.Source;
                Source s(value = 5);
            equation
            end Test;
            "#,
            "Test",
        );
    }

    /// Qualified name access
    #[test]
    fn qualified_name_access() {
        expect_success(
            r#"
            package P
                constant Real x = 42;
            end P;

            model Test
                Real y = P.x;
            equation
            end Test;
            "#,
            "Test",
        );
    }

    /// Deep nesting access
    #[test]
    fn deep_nesting_access() {
        expect_success(
            r#"
            package A
                package B
                    package C
                        constant Real val = 100;
                    end C;
                end B;
            end A;

            model Test
                Real x = A.B.C.val;
            equation
            end Test;
            "#,
            "Test",
        );
    }
}

// ============================================================================
// PACKAGE RESTRICTIONS
// ============================================================================

/// MLS: Package restrictions
mod package_restrictions {
    use super::*;

    /// MLS: "Packages cannot have equations"
    #[test]
    #[ignore = "Package equation restriction not yet enforced"]
    fn error_package_with_equations() {
        expect_failure(
            r#"
            package P
                Real x;
            equation
                x = 1;
            end P;
            "#,
            "P",
        );
    }

    /// MLS: "Packages cannot have algorithm sections"
    #[test]
    #[ignore = "Package algorithm restriction not yet enforced"]
    fn error_package_with_algorithm() {
        expect_failure(
            r#"
            package P
                Real x;
            algorithm
                x := 1;
            end P;
            "#,
            "P",
        );
    }
}
