//! PKG (Package/Import) contract tests - MLS §13
//!
//! Tests for the 12 package contracts defined in SPEC_0022.

use rumoca_contracts::test_support::{
    expect_parse_ok, expect_resolve_failure_with_code, expect_success,
};

// =============================================================================
// PKG-001: Unique import names
// "Multiple qualified import-clauses shall not have the same import name"
// =============================================================================

#[test]
fn pkg_001_no_duplicate_imports() {
    expect_resolve_failure_with_code(
        r#"
        package P
            constant Real x = 1;
        end P;
        package Q
            constant Real x = 2;
        end Q;
        model Test
            import P.x;
            import Q.x;
            Real y;
        equation
            y = x;
        end Test;
    "#,
        "Test",
        "ER012",
    );
}

// =============================================================================
// PKG-002: Import not inherited
// "Import clauses are not inherited"
// =============================================================================

#[test]
fn pkg_002_import_basic() {
    expect_success(
        r#"
        package P
            constant Real pi = 3.14159;
        end P;
        model Test
            import P.pi;
            Real x;
        equation
            x = pi;
        end Test;
    "#,
        "Test",
    );
}

#[test]
fn pkg_002_import_not_inherited_fails() {
    expect_resolve_failure_with_code(
        r#"
        package P
            constant Real pi = 3.14159;
        end P;
        model Base
            import P.pi;
        end Base;
        model Child
            extends Base;
            Real x;
        equation
            x = pi;
        end Child;
    "#,
        "Child",
        "ER002",
    );
}

// =============================================================================
// PKG-003: Package-only imports
// "One can only import from packages, not from other kinds of classes"
// =============================================================================

#[test]
fn pkg_003_import_from_package() {
    expect_success(
        r#"
        package P
            constant Real g = 9.81;
        end P;
        model Test
            import P.g;
            Real x;
        equation
            x = g;
        end Test;
    "#,
        "Test",
    );
}

// =============================================================================
// PKG-011: Import fully qualified
// "An imported package or definition should always be referred to by its fully qualified name"
// =============================================================================

#[test]
fn pkg_011_qualified_import() {
    expect_success(
        r#"
        package Outer
            package Inner
                constant Real c = 42;
            end Inner;
        end Outer;
        model Test
            import Outer.Inner.c;
            Real x;
        equation
            x = c;
        end Test;
    "#,
        "Test",
    );
}

// =============================================================================
// PKG-012: Import not modifiable
// "Import-clauses are not named elements and cannot be modified or redeclared"
// =============================================================================

// Note: This is difficult to test directly as the parser would reject
// modification syntax on imports

// =============================================================================
// Package integration tests
// =============================================================================

#[test]
fn pkg_wildcard_import() {
    expect_success(
        r#"
        package Constants
            constant Real pi = 3.14159;
            constant Real e = 2.71828;
        end Constants;
        model Test
            import Constants.*;
            Real x;
            Real y;
        equation
            x = pi;
            y = e;
        end Test;
    "#,
        "Test",
    );
}

#[test]
fn pkg_nested_package() {
    expect_success(
        r#"
        package P
            package Sub
                constant Real val = 1.0;
            end Sub;
        end P;
        model Test
            Real x;
        equation
            x = P.Sub.val;
        end Test;
    "#,
        "Test",
    );
}

#[test]
fn pkg_package_with_class() {
    expect_parse_ok(
        r#"
        package Lib
            model Resistor
                parameter Real R = 1;
                Real v;
                Real i;
            equation
                v = R * i;
            end Resistor;
        end Lib;
    "#,
    );
}
