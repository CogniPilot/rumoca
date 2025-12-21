//! MLS §13: High Priority Package and Import Tests
//!
//! This module tests high priority normative requirements:
//! - §13.1: Package as specialized class restrictions
//! - §13.2: Import statement rules
//! - §13.2.1: Qualified import rules
//! - §13.2.2: Single definition import
//! - §13.2.3: Unqualified import rules
//! - §13.3: Library structure requirements
//!
//! Reference: https://specification.modelica.org/master/packages.html

use crate::spec::{expect_failure, expect_parse_success, expect_success};

// ============================================================================
// §13.1 PACKAGE AS SPECIALIZED CLASS
// ============================================================================

/// MLS §13.1: Package restrictions
mod package_restrictions {
    use super::*;

    /// MLS: "Package cannot have equations"
    #[test]
    #[ignore = "Package equation restriction not yet implemented"]
    fn mls_13_1_package_no_equations() {
        expect_failure(
            r#"
            package Test
                Real x;
            equation
                x = 1;
            end Test;
            "#,
            "Test",
        );
    }

    /// MLS: "Package cannot have algorithm sections"
    #[test]
    #[ignore = "Package algorithm restriction not yet implemented"]
    fn mls_13_1_package_no_algorithm() {
        expect_failure(
            r#"
            package Test
                Real x;
            algorithm
                x := 1;
            end Test;
            "#,
            "Test",
        );
    }

    /// MLS: "Package can only contain class definitions and constants"
    #[test]
    fn mls_13_1_package_with_classes() {
        expect_success(
            r#"
            package Test
                constant Real pi = 3.14159;

                model Inner
                    Real x;
                equation
                    x = 1;
                end Inner;

                function Square
                    input Real x;
                    output Real y;
                algorithm
                    y := x * x;
                end Square;
            end Test;

            model Main
                Test.Inner i;
            equation
            end Main;
            "#,
            "Main",
        );
    }

    /// Valid: Nested packages
    #[test]
    fn mls_13_1_nested_packages() {
        expect_success(
            r#"
            package Outer
                package Inner
                    constant Real c = 1.0;
                end Inner;
            end Outer;

            model Test
                Real x = Outer.Inner.c;
            equation
            end Test;
            "#,
            "Test",
        );
    }

    /// Valid: Package with imports
    #[test]
    fn mls_13_1_package_with_imports() {
        expect_success(
            r#"
            package Lib
                constant Real pi = 3.14159;
            end Lib;

            package Test
                import Lib.pi;
                constant Real twoPi = 2 * pi;
            end Test;

            model Main
                Real x = Test.twoPi;
            equation
            end Main;
            "#,
            "Main",
        );
    }
}

// ============================================================================
// §13.2.1 QUALIFIED IMPORT
// ============================================================================

/// MLS §13.2.1: Qualified import rules
mod qualified_import {
    use super::*;

    /// MLS: "Qualified import must reference existing element"
    #[test]
    fn mls_13_2_1_import_nonexistent() {
        expect_failure(
            r#"
            package Lib
                constant Real pi = 3.14159;
            end Lib;

            model Test
                import Lib.nonexistent;
                Real x = nonexistent;
            equation
            end Test;
            "#,
            "Test",
        );
    }

    /// MLS: "Qualified import must reference a package"
    #[test]
    #[ignore = "Import package checking not yet implemented"]
    fn mls_13_2_1_import_non_package() {
        expect_failure(
            r#"
            model NotAPackage
                Real x;
            equation
            end NotAPackage;

            model Test
                import NotAPackage.x;
                Real y = x;
            equation
            end Test;
            "#,
            "Test",
        );
    }

    /// Valid: Qualified import of constant
    #[test]
    fn mls_13_2_1_import_constant() {
        expect_success(
            r#"
            package Lib
                constant Real pi = 3.14159;
            end Lib;

            model Test
                import Lib.pi;
                Real x = pi;
            equation
            end Test;
            "#,
            "Test",
        );
    }

    /// Valid: Qualified import of model
    #[test]
    fn mls_13_2_1_import_model() {
        expect_success(
            r#"
            package Lib
                model Component
                    Real x;
                equation
                end Component;
            end Lib;

            model Test
                import Lib.Component;
                Component c;
            equation
            end Test;
            "#,
            "Test",
        );
    }
}

// ============================================================================
// §13.2.2 SINGLE DEFINITION IMPORT (RENAMING)
// ============================================================================

/// MLS §13.2.2: Renaming import rules
mod renaming_import {
    use super::*;

    /// MLS: "Renaming import creates an alias"
    #[test]
    fn mls_13_2_2_renaming_import() {
        expect_success(
            r#"
            package Lib
                constant Real pi = 3.14159;
            end Lib;

            model Test
                import P = Lib.pi;
                Real x = P;
            equation
            end Test;
            "#,
            "Test",
        );
    }

    /// MLS: "Renaming import for package"
    #[test]
    fn mls_13_2_2_renaming_package() {
        expect_success(
            r#"
            package LongPackageName
                constant Real c = 1.0;
            end LongPackageName;

            model Test
                import L = LongPackageName;
                Real x = L.c;
            equation
            end Test;
            "#,
            "Test",
        );
    }

    /// MLS: "Alias shadows original name"
    #[test]
    fn mls_13_2_2_alias_shadows() {
        expect_success(
            r#"
            package Lib
                constant Real c = 1.0;
            end Lib;

            model Test
                import MyC = Lib.c;
                Real c = 2.0;
                Real x = MyC;
                Real y = c;
            equation
            end Test;
            "#,
            "Test",
        );
    }
}

// ============================================================================
// §13.2.3 UNQUALIFIED IMPORT
// ============================================================================

/// MLS §13.2.3: Unqualified import rules
mod unqualified_import {
    use super::*;

    /// MLS: "Unqualified import brings all public elements into scope"
    #[test]
    #[ignore = "Unqualified import not fully implemented"]
    fn mls_13_2_3_unqualified_import() {
        expect_success(
            r#"
            package Lib
                constant Real pi = 3.14159;
                constant Real e = 2.71828;
            end Lib;

            model Test
                import Lib.*;
                Real x = pi;
                Real y = e;
            equation
            end Test;
            "#,
            "Test",
        );
    }

    /// MLS: "Name conflict with unqualified import"
    #[test]
    fn mls_13_2_3_import_conflict() {
        expect_failure(
            r#"
            package Lib1
                constant Real c = 1.0;
            end Lib1;

            package Lib2
                constant Real c = 2.0;
            end Lib2;

            model Test
                import Lib1.*;
                import Lib2.*;
                Real x = c;
            equation
            end Test;
            "#,
            "Test",
        );
    }

    /// MLS: "Local declaration shadows unqualified import"
    #[test]
    fn mls_13_2_3_local_shadows_import() {
        expect_success(
            r#"
            package Lib
                constant Real c = 1.0;
            end Lib;

            model Test
                import Lib.*;
                Real c = 2.0;
                Real x = c;
            equation
            end Test;
            "#,
            "Test",
        );
    }

    /// Valid: Unqualified import from nested package
    #[test]
    #[ignore = "Unqualified import not fully implemented"]
    fn mls_13_2_3_unqualified_nested() {
        expect_success(
            r#"
            package Outer
                package Inner
                    constant Real c = 1.0;
                    constant Real d = 2.0;
                end Inner;
            end Outer;

            model Test
                import Outer.Inner.*;
                Real x = c;
                Real y = d;
            equation
            end Test;
            "#,
            "Test",
        );
    }
}

// ============================================================================
// §13.2 IMPORT SCOPE AND VISIBILITY
// ============================================================================

/// MLS §13.2: Import scope and visibility rules
mod import_scope {
    use super::*;

    /// MLS: "Import is only visible in the importing class"
    #[test]
    fn mls_13_2_import_local_scope() {
        expect_success(
            r#"
            package Lib
                constant Real c = 1.0;
            end Lib;

            model Outer
                import Lib.c;
                Real x = c;

                model Inner
                    import Lib.c;
                    Real y = c;
                equation
                end Inner;

                Inner i;
            equation
            end Outer;
            "#,
            "Outer",
        );
    }

    /// MLS: "Import in encapsulated class must use global path"
    #[test]
    #[ignore = "encapsulated model syntax not yet supported"]
    fn mls_13_2_encapsulated_import() {
        expect_parse_success(
            r#"
            package Lib
                constant Real c = 1.0;
            end Lib;

            encapsulated model Test
                import .Lib.c;
                Real x = c;
            equation
            end Test;
            "#,
        );
    }

    /// Valid: Multiple imports in same class
    #[test]
    fn mls_13_2_multiple_imports() {
        expect_success(
            r#"
            package Lib1
                constant Real a = 1.0;
            end Lib1;

            package Lib2
                constant Real b = 2.0;
            end Lib2;

            model Test
                import Lib1.a;
                import Lib2.b;
                Real x = a + b;
            equation
            end Test;
            "#,
            "Test",
        );
    }
}

// ============================================================================
// §13.3 LIBRARY STRUCTURE
// ============================================================================

/// MLS §13.3: Library structure requirements
mod library_structure {
    use super::*;

    /// Valid: Package hierarchy
    #[test]
    fn mls_13_3_package_hierarchy() {
        expect_success(
            r#"
            package Root
                package Sub1
                    constant Real c1 = 1.0;
                end Sub1;

                package Sub2
                    constant Real c2 = 2.0;
                end Sub2;
            end Root;

            model Test
                Real x = Root.Sub1.c1 + Root.Sub2.c2;
            equation
            end Test;
            "#,
            "Test",
        );
    }

    /// Valid: Access via fully qualified name
    #[test]
    fn mls_13_3_fully_qualified_access() {
        expect_success(
            r#"
            package Lib
                package Math
                    constant Real pi = 3.14159;
                end Math;
            end Lib;

            model Test
                Real x = .Lib.Math.pi;
            equation
            end Test;
            "#,
            "Test",
        );
    }

    /// Valid: Partial package (for documentation)
    #[test]
    fn mls_13_3_partial_package() {
        expect_parse_success(
            r#"
            partial package Base
                constant Real c = 1.0;
            end Base;

            package Derived
                extends Base;
                constant Real d = 2.0;
            end Derived;
            "#,
        );
    }
}

// ============================================================================
// §13.2 PROTECTED IMPORTS
// ============================================================================

/// MLS §13.2: Protected import behavior
mod protected_imports {
    use super::*;

    /// MLS: "Protected imports are not visible outside the class"
    #[test]
    fn mls_13_2_protected_import() {
        expect_success(
            r#"
            package Lib
                constant Real c = 1.0;
            end Lib;

            model Test
            protected
                import Lib.c;
            public
                Real x = c;
            equation
            end Test;
            "#,
            "Test",
        );
    }

    /// Valid: Public and protected imports
    #[test]
    fn mls_13_2_mixed_visibility() {
        expect_success(
            r#"
            package Lib
                constant Real a = 1.0;
                constant Real b = 2.0;
            end Lib;

            model Test
            public
                import Lib.a;
            protected
                import Lib.b;
            public
                Real x = a + b;
            equation
            end Test;
            "#,
            "Test",
        );
    }
}
