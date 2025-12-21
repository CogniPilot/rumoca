//! MLS §5.3: Static Name Lookup
//!
//! Tests for name lookup rules including:
//! - Simple name lookup
//! - Composite name lookup
//! - Global name lookup
//! - Import clause lookup
//!
//! Reference: https://specification.modelica.org/master/scoping-name-lookup-and-flattening.html

use crate::spec::{expect_failure, expect_parse_success, expect_success};

// ============================================================================
// §5.3.1 SIMPLE NAME LOOKUP
// ============================================================================

/// MLS §5.3: Simple name lookup in local scope
mod simple_name_lookup {
    use super::*;

    /// Local variable shadows enclosing class variable
    #[test]
    fn mls_5_3_local_variable() {
        expect_success(
            r#"
            model Test
                Real x = 1;
            equation
            end Test;
            "#,
            "Test",
        );
    }

    /// Variable from enclosing class
    #[test]
    #[ignore = "Nested model definitions not yet supported in compiler"]
    fn mls_5_3_enclosing_class_variable() {
        expect_success(
            r#"
            model Outer
                Real x = 1;
                model Inner
                    Real y = x;
                end Inner;
                Inner inner;
            equation
            end Outer;
            "#,
            "Outer",
        );
    }

    /// Parameter reference in nested scope
    #[test]
    fn mls_5_3_parameter_in_nested_scope() {
        expect_success(
            r#"
            model Test
                parameter Real p = 10;
                model Nested
                    Real y = p;
                end Nested;
                Nested n;
            equation
            end Test;
            "#,
            "Test",
        );
    }

    /// Iteration variable lookup takes precedence
    #[test]
    fn mls_5_3_iteration_variable_precedence() {
        expect_success(
            r#"
            model Test
                parameter Integer i = 100;
                Real x[5];
            equation
                for i in 1:5 loop
                    x[i] = i;
                end for;
            end Test;
            "#,
            "Test",
        );
    }

    /// Constant from enclosing class (only allowed reference type)
    #[test]
    fn mls_5_3_constant_from_enclosing() {
        expect_success(
            r#"
            model Test
                constant Real c = 3.14159;
                model Nested
                    Real y = c;
                end Nested;
                Nested n;
            equation
            end Test;
            "#,
            "Test",
        );
    }

    /// Reference to type from enclosing scope
    #[test]
    fn mls_5_3_type_from_enclosing() {
        expect_parse_success(
            r#"
            package P
                type Voltage = Real(unit="V");
                model Test
                    Voltage v = 5;
                end Test;
            end P;
            "#,
        );
    }

    /// Reference to class from enclosing scope
    #[test]
    fn mls_5_3_class_from_enclosing() {
        expect_parse_success(
            r#"
            package P
                record Point
                    Real x;
                    Real y;
                end Point;
                model Test
                    Point p;
                end Test;
            end P;
            "#,
        );
    }
}

// ============================================================================
// §5.3.2 COMPOSITE NAME LOOKUP
// ============================================================================

/// MLS §5.3: Composite name lookup (A.B.C)
mod composite_name_lookup {
    use super::*;

    /// Component member access
    #[test]
    fn mls_5_3_component_member_access() {
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

    /// Nested component member access
    #[test]
    #[ignore = "Nested record member access not yet validated"]
    fn mls_5_3_nested_member_access() {
        expect_parse_success(
            r#"
            record Inner
                Real value;
            end Inner;

            record Outer
                Inner inner;
            end Outer;

            model Test
                Outer o;
            equation
                o.inner.value = 1;
            end Test;
            "#,
        );
    }

    /// Package-qualified class reference
    #[test]
    fn mls_5_3_package_qualified_class() {
        expect_parse_success(
            r#"
            package Lib
                model M
                    Real x;
                end M;
            end Lib;

            model Test
                Lib.M m;
            equation
            end Test;
            "#,
        );
    }

    /// Nested package-qualified reference
    #[test]
    fn mls_5_3_nested_package_qualified() {
        expect_parse_success(
            r#"
            package A
                package B
                    model M
                        Real x;
                    end M;
                end B;
            end A;

            model Test
                A.B.M m;
            equation
            end Test;
            "#,
        );
    }

    /// Package-qualified constant reference
    #[test]
    fn mls_5_3_package_qualified_constant() {
        expect_parse_success(
            r#"
            package Constants
                constant Real pi = 3.14159;
            end Constants;

            model Test
                Real area = Constants.pi * 1;
            equation
            end Test;
            "#,
        );
    }

    /// Package-qualified type reference
    #[test]
    fn mls_5_3_package_qualified_type() {
        expect_parse_success(
            r#"
            package Units
                type Voltage = Real(unit="V");
            end Units;

            model Test
                Units.Voltage v = 5;
            equation
            end Test;
            "#,
        );
    }
}

// ============================================================================
// §5.3.3 GLOBAL NAME LOOKUP
// ============================================================================

/// MLS §5.3: Global name lookup (.A.B)
mod global_name_lookup {
    use super::*;

    /// Global name with dot prefix
    #[test]
    fn mls_5_3_global_prefix() {
        expect_parse_success(
            r#"
            package Lib
                constant Real value = 42;
            end Lib;

            model Test
                Real x = .Lib.value;
            equation
            end Test;
            "#,
        );
    }

    /// Global lookup bypasses local shadowing
    #[test]
    fn mls_5_3_global_bypasses_shadow() {
        expect_parse_success(
            r#"
            package Lib
                constant Real x = 100;
            end Lib;

            model Test
                Real x = 1;
                Real y = .Lib.x;
            equation
            end Test;
            "#,
        );
    }

    /// Global lookup of nested package
    #[test]
    fn mls_5_3_global_nested_package() {
        expect_parse_success(
            r#"
            package A
                package B
                    constant Real c = 1;
                end B;
            end A;

            model Test
                Real x = .A.B.c;
            equation
            end Test;
            "#,
        );
    }
}

// ============================================================================
// §5.3.4 IMPORT CLAUSE LOOKUP
// ============================================================================

/// MLS §5.3: Import clause and lookup
mod import_lookup {
    use super::*;

    /// Qualified import
    #[test]
    fn mls_5_3_import_qualified() {
        expect_parse_success(
            r#"
            package Lib
                model M
                    Real x;
                end M;
            end Lib;

            model Test
                import Lib.M;
                M m;
            equation
            end Test;
            "#,
        );
    }

    /// Renaming import
    #[test]
    fn mls_5_3_import_renaming() {
        expect_parse_success(
            r#"
            package Lib
                model LongModelName
                    Real x;
                end LongModelName;
            end Lib;

            model Test
                import M = Lib.LongModelName;
                M m;
            equation
            end Test;
            "#,
        );
    }

    /// Unqualified import (star import)
    #[test]
    fn mls_5_3_import_unqualified() {
        expect_parse_success(
            r#"
            package Lib
                model M1
                    Real x;
                end M1;
                model M2
                    Real y;
                end M2;
            end Lib;

            model Test
                import Lib.*;
                M1 m1;
                M2 m2;
            equation
            end Test;
            "#,
        );
    }

    /// Multiple imports
    #[test]
    fn mls_5_3_multiple_imports() {
        expect_parse_success(
            r#"
            package Lib1
                constant Real c1 = 1;
            end Lib1;

            package Lib2
                constant Real c2 = 2;
            end Lib2;

            model Test
                import Lib1.c1;
                import Lib2.c2;
                Real x = c1 + c2;
            equation
            end Test;
            "#,
        );
    }
}

// ============================================================================
// §5.3.5 ENCAPSULATED LOOKUP
// ============================================================================

/// MLS §5.3: Encapsulated class lookup restrictions
mod encapsulated_lookup {
    use super::*;

    /// Encapsulated class does not see enclosing scope
    #[test]
    #[ignore = "Encapsulation semantics not yet enforced"]
    fn mls_5_3_encapsulated_no_enclosing() {
        expect_failure(
            r#"
            model Outer
                Real x = 1;
                encapsulated model Inner
                    Real y = x;
                end Inner;
            end Outer;
            "#,
            "Outer",
        );
    }

    /// Encapsulated class can use global lookup
    #[test]
    fn mls_5_3_encapsulated_global_lookup() {
        expect_parse_success(
            r#"
            package Lib
                constant Real c = 42;
            end Lib;

            model Outer
                encapsulated model Inner
                    import Lib.c;
                    Real y = c;
                end Inner;
            end Outer;
            "#,
        );
    }

    /// Encapsulated package with import
    #[test]
    fn mls_5_3_encapsulated_package_import() {
        expect_parse_success(
            r#"
            package External
                type Voltage = Real;
            end External;

            encapsulated package MyLib
                import External.Voltage;
                model Test
                    Voltage v = 5;
                end Test;
            end MyLib;
            "#,
        );
    }
}

// ============================================================================
// NEGATIVE TESTS - Name lookup errors
// ============================================================================

/// Name lookup error cases
mod lookup_errors {
    use super::*;

    /// Undefined variable reference
    #[test]
    fn error_undefined_variable() {
        expect_failure(
            r#"
            model Test
                Real y = undefined_var;
            equation
            end Test;
            "#,
            "Test",
        );
    }

    /// Undefined type reference
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

    /// Undefined package in qualified name
    #[test]
    fn error_undefined_package() {
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

    /// Undefined member in component access
    #[test]
    fn error_undefined_member() {
        expect_failure(
            r#"
            record Point
                Real x;
                Real y;
            end Point;

            model Test
                Point p;
            equation
                p.z = 1;
            end Test;
            "#,
            "Test",
        );
    }

    /// Access to non-existent nested member
    #[test]
    #[ignore = "Deep member access validation not implemented"]
    fn error_undefined_nested_member() {
        expect_failure(
            r#"
            record Point
                Real x;
            end Point;

            model Test
                Point p;
            equation
                p.x.nonexistent = 1;
            end Test;
            "#,
            "Test",
        );
    }
}
