//! PKG (Package/Import) contract tests - MLS §13
//!
//! Tests for the 12 package contracts defined in SPEC_0022.

use rumoca_contracts::test_support::{
    expect_parse_err_with_code, expect_parse_ok, expect_resolve_failure_with_code, expect_success,
};
use rumoca_session::source_roots::parse_source_root_with_cache_in;
use std::fs;
use std::path::Path;

fn write_source_root_file(root: &Path, relative: &str, content: &str) {
    let path = root.join(relative);
    if let Some(parent) = path.parent() {
        fs::create_dir_all(parent).expect("create package parent");
    }
    fs::write(path, content).expect("write package file");
}

fn expect_source_root_layout_ok(files: &[(&str, &str)], root_relative: &str) {
    let temp = tempfile::tempdir().expect("tempdir");
    let root = temp.path().join(root_relative);
    fs::create_dir_all(&root).expect("create package root");
    for (relative, content) in files {
        write_source_root_file(&root, relative, content);
    }
    parse_source_root_with_cache_in(&root, None).unwrap_or_else(|error| {
        panic!(
            "expected valid package layout under {}: {error}",
            root.display()
        )
    });
}

fn expect_source_root_layout_error(
    files: &[(&str, &str)],
    root_relative: &str,
    expected_fragment: &str,
) {
    let temp = tempfile::tempdir().expect("tempdir");
    let root = temp.path().join(root_relative);
    fs::create_dir_all(&root).expect("create package root");
    for (relative, content) in files {
        write_source_root_file(&root, relative, content);
    }
    let error = parse_source_root_with_cache_in(&root, None)
        .expect_err("expected invalid package layout to fail");
    assert!(
        error.to_string().contains(expected_fragment),
        "expected error fragment `{expected_fragment}`, got: {error}"
    );
}

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

#[test]
fn pkg_003_import_from_non_package_rejected() {
    expect_resolve_failure_with_code(
        r#"
        model Outer
            model Inner
            end Inner;
        end Outer;
        model Test
            import Outer.Inner;
            Inner x;
        end Test;
    "#,
        "Test",
        "ER002",
    );
}

// =============================================================================
// PKG-005: Qualified import target
// "Qualified import-clauses may only refer to packages or elements of packages"
// =============================================================================

#[test]
fn pkg_005_import_element_of_package() {
    expect_success(
        r#"
        package P
            model A
                Real x;
            end A;
        end P;
        model Test
            import P.A;
            A a;
        end Test;
    "#,
        "Test",
    );
}

#[test]
fn pkg_005_import_cannot_traverse_model_members() {
    expect_resolve_failure_with_code(
        r#"
        package P
            model A
                constant Real x = 1;
            end A;
        end P;
        model Test
            import P.A.x;
            Real y;
        equation
            y = x;
        end Test;
    "#,
        "Test",
        "ER002",
    );
}

// =============================================================================
// PKG-006: Directory package.mo
// "Each directory shall contain a node, the file package.mo"
// =============================================================================

#[test]
fn pkg_006_directory_requires_package_mo() {
    expect_source_root_layout_error(
        &[
            ("package.mo", "package Pkg end Pkg;"),
            ("Sub/A.mo", "within Pkg.Sub; model A end A;"),
        ],
        "Pkg",
        "PKG-006",
    );
}

// =============================================================================
// PKG-007: No duplicate class names
// "Two sub-entities shall not define classes with identical names"
// =============================================================================

#[test]
fn pkg_007_duplicate_child_class_names_rejected() {
    expect_source_root_layout_error(
        &[
            ("package.mo", "package Pkg end Pkg;"),
            ("A.mo", "within Pkg; model Same end Same;"),
            ("B.mo", "within Pkg; model Same end Same;"),
        ],
        "Pkg",
        "PKG-007",
    );
}

// =============================================================================
// PKG-008: No dir and file conflict
// "A directory shall not contain both sub-directory A and file A.mo"
// =============================================================================

#[test]
fn pkg_008_dir_and_file_conflict_rejected() {
    expect_source_root_layout_error(
        &[
            ("package.mo", "package Pkg end Pkg;"),
            ("A.mo", "within Pkg; model A end A;"),
            ("A/package.mo", "within Pkg; package A end A;"),
        ],
        "Pkg",
        "PKG-008",
    );
}

// =============================================================================
// PKG-009: Within required
// "A non-top-level entity shall begin with a within-clause"
// =============================================================================

#[test]
fn pkg_009_non_top_level_file_requires_within() {
    expect_source_root_layout_error(
        &[
            ("package.mo", "package Pkg end Pkg;"),
            ("A.mo", "model A end A;"),
        ],
        "Pkg",
        "PKG-009",
    );
}

// =============================================================================
// PKG-010: Within designates enclosing
// "The within-clause shall designate the class of the enclosing entity"
// =============================================================================

#[test]
fn pkg_010_within_must_match_enclosing_package() {
    expect_source_root_layout_error(
        &[
            ("package.mo", "package Pkg end Pkg;"),
            ("Sub/package.mo", "within Pkg; package Sub end Sub;"),
            ("Sub/A.mo", "within Wrong.Sub; model A end A;"),
        ],
        "Pkg",
        "PKG-010",
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

#[test]
fn pkg_012_import_clause_not_modifiable() {
    expect_parse_err_with_code(
        r#"
        package P
            constant Real x = 1;
        end P;
        model Test
            import P.x(a = 1);
        end Test;
    "#,
        "EP001",
    );
}

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
fn pkg_nested_package_layout_is_valid() {
    expect_source_root_layout_ok(
        &[
            ("package.mo", "package Pkg end Pkg;"),
            ("Sub/package.mo", "within Pkg; package Sub end Sub;"),
            ("Sub/A.mo", "within Pkg.Sub; model A end A;"),
        ],
        "Pkg",
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
