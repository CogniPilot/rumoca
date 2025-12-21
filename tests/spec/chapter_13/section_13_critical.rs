//! MLS Chapter 13: Critical Edge Case Tests
//!
//! This module contains critical tests for package edge cases
//! and advanced scenarios in package semantics.
//!
//! Reference: https://specification.modelica.org/master/packages.html

use crate::spec::{expect_failure, expect_parse_success, expect_success};

// ============================================================================
// CRITICAL: PACKAGE HIERARCHY EDGE CASES
// ============================================================================

/// Critical package hierarchy edge cases
mod hierarchy_critical {
    use super::*;

    /// Critical: Deep package nesting
    #[test]
    fn critical_deep_nesting() {
        expect_parse_success(
            r#"
            package L1
                package L2
                    package L3
                        package L4
                            package L5
                                constant Real deep = 42;
                            end L5;
                        end L4;
                    end L3;
                end L2;
            end L1;

            model Test
                Real x = L1.L2.L3.L4.L5.deep;
            end Test;
            "#,
        );
    }

    /// Critical: Wide package (many siblings)
    #[test]
    fn critical_wide_package() {
        expect_parse_success(
            r#"
            package Wide
                model M1 Real x = 1; equation end M1;
                model M2 Real x = 2; equation end M2;
                model M3 Real x = 3; equation end M3;
                model M4 Real x = 4; equation end M4;
                model M5 Real x = 5; equation end M5;
                model M6 Real x = 6; equation end M6;
                model M7 Real x = 7; equation end M7;
                model M8 Real x = 8; equation end M8;
            end Wide;
            "#,
        );
    }

    /// Critical: Mixed content package
    #[test]
    fn critical_mixed_content() {
        expect_parse_success(
            r#"
            package Mixed
                constant Real c1 = 1;
                type T1 = Real(min = 0);

                model M1
                    T1 x = c1;
                equation
                end M1;

                function F1
                    input Real x;
                    output Real y;
                algorithm
                    y := x * c1;
                end F1;

                connector C1
                    T1 v;
                    flow Real i;
                end C1;

                record R1
                    T1 value;
                end R1;

                package SubPkg
                    constant Real sub = 2;
                end SubPkg;
            end Mixed;
            "#,
        );
    }
}

// ============================================================================
// CRITICAL: IMPORT EDGE CASES
// ============================================================================

/// Critical import edge cases
mod import_critical {
    use super::*;

    /// Critical: Multiple import sources
    #[test]
    fn critical_multiple_sources() {
        expect_parse_success(
            r#"
            package A
                constant Real x = 1;
            end A;

            package B
                constant Real y = 2;
            end B;

            package C
                constant Real z = 3;
            end C;

            model Test
                import A.x;
                import B.y;
                import C.z;
                Real sum = x + y + z;
            end Test;
            "#,
        );
    }

    /// Critical: Import chain
    #[test]
    fn critical_import_chain() {
        expect_parse_success(
            r#"
            package Source
                constant Real origin = 1;
            end Source;

            package Mid
                import Source.origin;
                constant Real value = origin * 2;
            end Mid;

            model Test
                import Mid.value;
                Real x = value;
            end Test;
            "#,
        );
    }

    /// Critical: Import with extends
    #[test]
    fn critical_import_with_extends() {
        expect_parse_success(
            r#"
            package Lib
                constant Real g = 9.81;
            end Lib;

            model Base
                import Lib.g;
                Real acc = g;
            end Base;

            model Derived
                extends Base;
                import Lib.g;
                Real gravity = g;
            end Derived;
            "#,
        );
    }

    /// Critical: Import renamed element
    #[test]
    fn critical_import_renamed() {
        expect_parse_success(
            r#"
            package LongName
                model VeryLongModelName
                    Real x = 1;
                equation
                end VeryLongModelName;
            end LongName;

            model Test
                import M = LongName.VeryLongModelName;
                M comp;
            equation
            end Test;
            "#,
        );
    }
}

// ============================================================================
// CRITICAL: PACKAGE EXTENDS
// ============================================================================

/// Critical package extends edge cases
mod extends_critical {
    use super::*;

    /// Critical: Package extends another package
    #[test]
    fn critical_package_extends() {
        expect_parse_success(
            r#"
            package Base
                constant Real base = 1;
            end Base;

            package Extended
                extends Base;
                constant Real extra = 2;
            end Extended;
            "#,
        );
    }

    /// Critical: Package extends with modifications
    #[test]
    fn critical_package_extends_mod() {
        expect_parse_success(
            r#"
            package Template
                replaceable constant Real value = 1;
            end Template;

            package Instance
                extends Template(value = 42);
            end Instance;
            "#,
        );
    }

    /// Critical: Multiple package inheritance
    #[test]
    fn critical_multiple_package_extends() {
        expect_parse_success(
            r#"
            package A
                constant Real a = 1;
            end A;

            package B
                constant Real b = 2;
            end B;

            package Combined
                extends A;
                extends B;
            end Combined;
            "#,
        );
    }
}

// ============================================================================
// CRITICAL: ENCAPSULATION EDGE CASES
// ============================================================================

/// Critical encapsulation edge cases
mod encapsulation_critical {
    use super::*;

    /// Critical: Encapsulated nested
    #[test]
    fn critical_encapsulated_nested() {
        expect_parse_success(
            r#"
            encapsulated package Outer
                package Inner
                    encapsulated model Deep
                        Real x;
                    equation
                        x = 1;
                    end Deep;
                end Inner;
            end Outer;
            "#,
        );
    }

    /// Critical: Reference to outer from inner
    #[test]
    fn critical_outer_reference() {
        expect_parse_success(
            r#"
            package Container
                constant Real sharedValue = 10;

                model User
                    Real x = Container.sharedValue;
                equation
                end User;
            end Container;
            "#,
        );
    }
}

// ============================================================================
// CRITICAL: LIBRARY PATTERNS
// ============================================================================

/// Critical library patterns
mod library_critical {
    use super::*;

    /// Critical: Modelica Standard Library pattern
    #[test]
    fn critical_msl_pattern() {
        expect_parse_success(
            r#"
            package Modelica
                package Electrical
                    package Analog
                        package Basic
                            model Resistor
                                parameter Real R = 1;
                            equation
                            end Resistor;

                            model Capacitor
                                parameter Real C = 1;
                            equation
                            end Capacitor;
                        end Basic;

                        package Sensors
                            model VoltageSensor
                            equation
                            end VoltageSensor;
                        end Sensors;
                    end Analog;
                end Electrical;

                package Mechanics
                    package Rotational
                        model Inertia
                            parameter Real J = 1;
                        equation
                        end Inertia;
                    end Rotational;
                end Mechanics;

                package SIunits
                    type Voltage = Real(unit = "V");
                    type Current = Real(unit = "A");
                end SIunits;
            end Modelica;
            "#,
        );
    }

    /// Critical: Library version pattern
    #[test]
    #[ignore = "Annotation with variable reference not yet supported"]
    fn critical_library_version() {
        expect_parse_success(
            r#"
            package Library
                constant String version = "1.0.0";
                constant String versionDate = "2024-01-01";

                annotation(version = version);

                model Component
                    Real x = 1;
                equation
                end Component;
            end Library;
            "#,
        );
    }

    /// Critical: Library with icons
    #[test]
    #[ignore = "Complex annotation parsing not yet supported"]
    fn critical_library_annotation() {
        expect_parse_success(
            r#"
            package Library
                annotation(
                    Documentation(info = "<html>Library documentation</html>")
                );

                model Component
                    annotation(
                        Icon(coordinateSystem(preserveAspectRatio = true))
                    );
                equation
                end Component;
            end Library;
            "#,
        );
    }
}

// ============================================================================
// CRITICAL: CROSS-PACKAGE REFERENCES
// ============================================================================

/// Critical cross-package reference cases
mod cross_package_critical {
    use super::*;

    /// Critical: Component uses type from another package
    #[test]
    fn critical_type_cross_reference() {
        expect_parse_success(
            r#"
            package Types
                type Voltage = Real(unit = "V");
            end Types;

            package Components
                import Types.Voltage;

                model Source
                    Voltage v = 5;
                equation
                end Source;
            end Components;
            "#,
        );
    }

    /// Critical: Function uses record from another package
    #[test]
    fn critical_function_record_reference() {
        expect_parse_success(
            r#"
            package Data
                record Point
                    Real x;
                    Real y;
                end Point;
            end Data;

            package Functions
                import Data.Point;

                function Distance
                    input Point a;
                    input Point b;
                    output Real d;
                algorithm
                    d := sqrt((a.x - b.x)^2 + (a.y - b.y)^2);
                end Distance;
            end Functions;
            "#,
        );
    }

    /// Critical: Model uses connector from another package
    #[test]
    fn critical_connector_cross_reference() {
        expect_success(
            r#"
            package Interfaces
                connector Pin
                    Real v;
                    flow Real i;
                end Pin;
            end Interfaces;

            package Components
                model Resistor
                    Interfaces.Pin p, n;
                    parameter Real R = 1;
                equation
                    p.v - n.v = R * p.i;
                    p.i + n.i = 0;
                end Resistor;
            end Components;
            "#,
            "Components.Resistor",
        );
    }
}

// ============================================================================
// CRITICAL: ERROR CASES
// ============================================================================

/// Critical error detection
mod error_critical {
    use super::*;

    /// Critical: Reference to undefined package
    #[test]
    fn critical_undefined_package() {
        expect_failure(
            r#"
            model Test
                Real x = UndefinedPackage.value;
            equation
            end Test;
            "#,
            "Test",
        );
    }

    /// Critical: Reference to undefined package element
    #[test]
    #[ignore = "Undefined package element detection not yet implemented"]
    fn critical_undefined_element() {
        expect_failure(
            r#"
            package Lib
                constant Real x = 1;
            end Lib;

            model Test
                Real x = Lib.undefined;
            equation
            end Test;
            "#,
            "Test",
        );
    }

    /// Critical: Import undefined element
    #[test]
    fn critical_import_undefined() {
        expect_failure(
            r#"
            package Lib
                constant Real x = 1;
            end Lib;

            model Test
                import Lib.undefined;
            equation
            end Test;
            "#,
            "Test",
        );
    }
}
