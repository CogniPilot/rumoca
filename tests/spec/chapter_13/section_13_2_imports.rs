//! MLS §13.2: Importing Definitions from a Package
//!
//! Tests for import statement semantics including:
//! - Qualified imports
//! - Unqualified imports
//! - Import visibility
//! - Import conflicts
//!
//! Reference: https://specification.modelica.org/master/packages.html

use crate::spec::{expect_failure, expect_parse_success};

// ============================================================================
// §13.2.1 QUALIFIED IMPORTS
// ============================================================================

/// MLS §13.2: Qualified import statements
mod qualified_imports {
    use super::*;

    /// MLS: Simple qualified import
    #[test]
    fn mls_13_2_qualified_simple() {
        expect_parse_success(
            r#"
            package Lib
                model A
                    Real x = 1;
                equation
                end A;
            end Lib;

            model Test
                import Lib.A;
                A comp;
            equation
            end Test;
            "#,
        );
    }

    /// MLS: Multiple qualified imports
    #[test]
    fn mls_13_2_multiple_qualified() {
        expect_parse_success(
            r#"
            package Lib
                model A
                    Real x = 1;
                equation
                end A;

                model B
                    Real y = 2;
                equation
                end B;
            end Lib;

            model Test
                import Lib.A;
                import Lib.B;
                A compA;
                B compB;
            equation
            end Test;
            "#,
        );
    }

    /// MLS: Nested qualified import
    #[test]
    fn mls_13_2_nested_qualified() {
        expect_parse_success(
            r#"
            package Outer
                package Inner
                    model Deep
                        Real x = 1;
                    equation
                    end Deep;
                end Inner;
            end Outer;

            model Test
                import Outer.Inner.Deep;
                Deep comp;
            equation
            end Test;
            "#,
        );
    }

    /// MLS: Import constant
    #[test]
    fn mls_13_2_import_constant() {
        expect_parse_success(
            r#"
            package Constants
                constant Real PI = 3.14159;
            end Constants;

            model Test
                import Constants.PI;
                Real x = PI;
            end Test;
            "#,
        );
    }

    /// MLS: Import function
    #[test]
    fn mls_13_2_import_function() {
        expect_parse_success(
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
                import Math.Square;
                Real y = Square(5);
            end Test;
            "#,
        );
    }

    /// MLS: Import type
    #[test]
    fn mls_13_2_import_type() {
        expect_parse_success(
            r#"
            package Types
                type Voltage = Real(unit = "V");
            end Types;

            model Test
                import Types.Voltage;
                Voltage v = 5;
            end Test;
            "#,
        );
    }
}

// ============================================================================
// §13.2.2 UNQUALIFIED IMPORTS
// ============================================================================

/// MLS §13.2: Unqualified import (import all)
mod unqualified_imports {
    use super::*;

    /// MLS: Wildcard import
    #[test]
    fn mls_13_2_wildcard_import() {
        expect_parse_success(
            r#"
            package Lib
                model A
                    Real x = 1;
                equation
                end A;

                model B
                    Real y = 2;
                equation
                end B;
            end Lib;

            model Test
                import Lib.*;
                A compA;
                B compB;
            equation
            end Test;
            "#,
        );
    }

    /// MLS: Package import alias
    #[test]
    fn mls_13_2_package_alias() {
        expect_parse_success(
            r#"
            package VeryLongPackageName
                constant Real x = 1;
            end VeryLongPackageName;

            model Test
                import P = VeryLongPackageName;
                Real value = P.x;
            end Test;
            "#,
        );
    }

    /// MLS: Import with renaming
    #[test]
    fn mls_13_2_import_rename() {
        expect_parse_success(
            r#"
            package Lib
                model Component
                    Real x = 1;
                equation
                end Component;
            end Lib;

            model Test
                import C = Lib.Component;
                C comp;
            equation
            end Test;
            "#,
        );
    }
}

// ============================================================================
// §13.2.3 IMPORT VISIBILITY
// ============================================================================

/// MLS §13.2: Import visibility rules
mod import_visibility {
    use super::*;

    /// MLS: Import is local to class
    #[test]
    fn mls_13_2_import_local() {
        expect_parse_success(
            r#"
            package Lib
                constant Real x = 1;
            end Lib;

            model A
                import Lib.x;
                Real value = x;
            end A;

            model B
                Real value = Lib.x;
            end B;
            "#,
        );
    }

    /// MLS: Import at package level
    #[test]
    fn mls_13_2_import_package_level() {
        expect_parse_success(
            r#"
            package Source
                constant Real PI = 3.14159;
            end Source;

            package User
                import Source.PI;

                model Test
                    Real x = PI;
                end Test;
            end User;
            "#,
        );
    }

    /// MLS: Import in nested class
    #[test]
    fn mls_13_2_import_nested() {
        expect_parse_success(
            r#"
            package Lib
                constant Real x = 1;
            end Lib;

            model Outer
                model Inner
                    import Lib.x;
                    Real value = x;
                end Inner;

                Inner sub;
            equation
            end Outer;
            "#,
        );
    }
}

// ============================================================================
// §13.2.4 IMPORT NOT INHERITED
// ============================================================================

/// MLS §13.2: Imports are NOT inherited
mod import_not_inherited {
    use super::*;

    /// MLS: Import does not propagate through extends
    #[test]
    fn mls_13_2_import_not_inherited() {
        expect_parse_success(
            r#"
            package Lib
                constant Real x = 1;
            end Lib;

            model Base
                import Lib.x;
                Real baseValue = x;
            end Base;

            model Derived
                extends Base;
                Real derivedValue = Lib.x;
            end Derived;
            "#,
        );
    }

    /// MLS: Each class needs its own import
    #[test]
    fn mls_13_2_separate_imports() {
        expect_parse_success(
            r#"
            package Constants
                constant Real g = 9.81;
            end Constants;

            model A
                import Constants.g;
                Real acc = g;
            end A;

            model B
                import Constants.g;
                Real gravity = g;
            end B;
            "#,
        );
    }
}

// ============================================================================
// IMPORT PATTERNS
// ============================================================================

/// MLS §13.2: Common import patterns
mod import_patterns {
    use super::*;

    /// Pattern: Import standard library items
    #[test]
    fn pattern_standard_library() {
        expect_parse_success(
            r#"
            package SI
                type Voltage = Real(unit = "V");
                type Current = Real(unit = "A");
                type Resistance = Real(unit = "Ohm");
            end SI;

            model Resistor
                import SI.*;
                Voltage v;
                Current i;
                parameter Resistance R = 100;
            equation
                v = R * i;
                i = 1;
            end Resistor;
            "#,
        );
    }

    /// Pattern: Import from nested packages
    #[test]
    fn pattern_nested_import() {
        expect_parse_success(
            r#"
            package Root
                package Electrical
                    connector Pin
                        Real v;
                        flow Real i;
                    end Pin;
                end Electrical;

                package Mechanical
                    connector Flange
                        Real phi;
                        flow Real tau;
                    end Flange;
                end Mechanical;
            end Root;

            model MultiDomain
                import Root.Electrical.Pin;
                import Root.Mechanical.Flange;
                Pin elec;
                Flange mech;
            equation
                elec.v = 1;
                mech.phi = 0;
            end MultiDomain;
            "#,
        );
    }

    /// Pattern: Import functions for use in expressions
    #[test]
    fn pattern_function_import() {
        expect_parse_success(
            r#"
            package MathFunctions
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
            end MathFunctions;

            model Calculator
                import MathFunctions.*;
                Real x = 3;
                Real squared = Square(x);
                Real cubed = Cube(x);
            end Calculator;
            "#,
        );
    }
}

// ============================================================================
// NEGATIVE TESTS
// ============================================================================

/// Import error cases
mod import_errors {
    use super::*;

    /// Error: Import non-existent element
    #[test]
    fn error_import_nonexistent() {
        expect_failure(
            r#"
            package Lib
                constant Real x = 1;
            end Lib;

            model Test
                import Lib.nonexistent;
            equation
            end Test;
            "#,
            "Test",
        );
    }

    /// Error: Import from non-package
    #[test]
    #[ignore = "Import source validation not yet implemented"]
    fn error_import_from_non_package() {
        expect_failure(
            r#"
            model NotAPackage
                Real x = 1;
            equation
            end NotAPackage;

            model Test
                import NotAPackage.x;
            equation
            end Test;
            "#,
            "Test",
        );
    }

    /// Error: Duplicate import name
    #[test]
    #[ignore = "Duplicate import detection not yet implemented"]
    fn error_duplicate_import() {
        expect_failure(
            r#"
            package A
                constant Real x = 1;
            end A;

            package B
                constant Real x = 2;
            end B;

            model Test
                import A.x;
                import B.x;
            equation
            end Test;
            "#,
            "Test",
        );
    }
}
