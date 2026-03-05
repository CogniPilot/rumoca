//! PKG (Package/Import) contract tests - MLS §13
//!
//! Tests for the 12 package contracts defined in SPEC_0022.

use rumoca_contracts::test_support::{
    expect_failure_in_phase_with_code, expect_parse_err_with_code, expect_parse_ok,
    expect_resolve_failure_with_code, expect_success,
};
use rumoca_session::{FailedPhase, PhaseResult, Session, SessionConfig};

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
// PKG-004: MODELICAPATH search
// "Top-level names lookup starts lexically before searching MODELICAPATH"
// =============================================================================

#[test]
#[ignore = "TODO(PKG-004): model lexical-vs-library root precedence explicitly in session lookup"]
fn pkg_004_top_level_lookup_prefers_lexical_before_library_search() {
    let mut session = Session::new(SessionConfig::default());
    session
        .add_document(
            "local.mo",
            r#"
            package Dup
                model M
                    Real x;
                equation
                    x = 1;
                end M;
            end Dup;

            model Test
                Dup.M m;
            end Test;
        "#,
        )
        .expect("add local.mo");
    session
        .add_document(
            "library::dup.mo",
            r#"
            package Dup
                model M
                    Real y;
                equation
                    y = 2;
                end M;
            end Dup;
        "#,
        )
        .expect("add library::dup.mo");

    // Spec intent: lexical/local top-level name Dup should be preferred over library Dup.
    // Current behavior merges roots and rejects non-identical duplicates.
    session
        .compile_model("Test")
        .expect("local lexical Dup should win over library Dup");
}

// =============================================================================
// PKG-005: Qualified import target
// "Qualified imports may only refer to packages or elements of packages"
// =============================================================================

#[test]
#[ignore = "TODO(PKG-005): reject qualified imports that target non-package classes"]
fn pkg_005_qualified_import_must_target_package_or_package_element() {
    expect_resolve_failure_with_code(
        r#"
        model M
            constant Real c = 1;
        end M;

        model Test
            import M.c;
            Real x;
        equation
            x = c;
        end Test;
    "#,
        "Test",
        "ER002",
    );
}

// =============================================================================
// PKG-006: Directory package.mo
// "Each directory shall contain package.mo"
// =============================================================================

#[test]
#[ignore = "TODO(PKG-006): enforce package.mo requirement for directory-based package loading"]
fn pkg_006_directory_requires_package_mo() {
    let mut session = Session::new(SessionConfig::default());
    session
        .add_document(
            "P/M.mo",
            r#"
            within P;
            model M
                Real x;
            equation
                x = 1;
            end M;
        "#,
        )
        .expect("add P/M.mo");

    let err = session
        .compile_model_phases("P.M")
        .expect_err("directory package without package.mo should be rejected");
    let msg = err.to_string();
    assert!(
        msg.contains("package.mo"),
        "unexpected error for missing package.mo: {msg}"
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
fn pkg_012_plain_import_without_modification_ok() {
    expect_success(
        r#"
        package P
            model M
                constant Real c = 1;
            end M;
        end P;
        model Test
            import P.M;
            Real x;
        equation
            x = M.c;
        end Test;
    "#,
        "Test",
    );
}

#[test]
fn pkg_012_import_not_modifiable_rejected_in_parse() {
    expect_parse_err_with_code(
        r#"
        package P
            model M
                constant Real c = 1;
            end M;
        end P;
        model Test
            import P.M(c = 2);
            Real x;
        equation
            x = M.c;
        end Test;
    "#,
        "EP001",
    );
}

// =============================================================================
// PKG-007: No duplicate class names
// "Two sub-entities shall not define classes with identical names"
// =============================================================================

#[test]
fn pkg_007_identical_duplicate_nested_class_across_files_is_accepted() {
    let mut session = Session::new(SessionConfig::default());
    session
        .add_document(
            "a.mo",
            r#"
            within P;
            model M
                Real x;
            equation
                x = 1;
            end M;
        "#,
        )
        .expect("add a.mo");
    session
        .add_document(
            "b.mo",
            r#"
            within P;
            model M
                Real x;
            equation
                x = 1;
            end M;
        "#,
        )
        .expect("add b.mo");
    session
        .add_document(
            "main.mo",
            r#"
            within P;
            model Top
                M m;
            end Top;
        "#,
        )
        .expect("add main.mo");

    session
        .compile_model("P.Top")
        .expect("identical duplicate nested classes should compile");
}

#[test]
fn pkg_007_non_identical_duplicate_nested_class_across_files_is_rejected() {
    let mut session = Session::new(SessionConfig::default());
    session
        .add_document(
            "a.mo",
            r#"
            within P;
            model M
                Real x;
            equation
                x = 1;
            end M;
        "#,
        )
        .expect("add a.mo");
    session
        .add_document(
            "b.mo",
            r#"
            within P;
            model M
                Real y;
            equation
                y = 2;
            end M;
        "#,
        )
        .expect("add b.mo");
    session
        .add_document(
            "main.mo",
            r#"
            within P;
            model Top
                M m;
            end Top;
        "#,
        )
        .expect("add main.mo");

    let err = session
        .compile_model_phases("P.Top")
        .expect_err("non-identical duplicate nested classes must fail merge");
    let msg = err.to_string();
    assert!(
        msg.contains("Duplicate class 'P.M'"),
        "unexpected error: {msg}"
    );
}

// =============================================================================
// PKG-008: No dir and file conflict
// "A directory shall not contain both sub-directory A and file A.mo"
// =============================================================================

#[test]
fn pkg_008_rejects_directory_and_file_with_same_name_conflict() {
    let mut session = Session::new(SessionConfig::default());
    session
        .add_document(
            "A.mo",
            r#"
            model A
                Real x;
            equation
                x = 1;
            end A;
        "#,
        )
        .expect("add A.mo");
    session
        .add_document(
            "A/package.mo",
            r#"
            package A
                model B
                    Real y;
                equation
                    y = 1;
                end B;
            end A;
        "#,
        )
        .expect("add A/package.mo");

    let err = session
        .compile_model_phases("A.B")
        .expect_err("file-vs-directory class conflict should be rejected");
    let msg = err.to_string();
    assert!(
        msg.contains("Duplicate class 'A'"),
        "unexpected duplicate conflict error: {msg}"
    );
}

// =============================================================================
// PKG-009: Within required
// "A non-top-level entity shall begin with a within-clause"
// =============================================================================

#[test]
fn pkg_009_non_top_level_entity_without_within_is_rejected() {
    expect_failure_in_phase_with_code(
        r#"
        package P
        end P;
        model M
            Real x;
        equation
            x = 1;
        end M;
    "#,
        "P.M",
        FailedPhase::Instantiate,
        "EI001",
    );
}

// =============================================================================
// PKG-010: Within designates enclosing
// "The within-clause shall designate the class of the enclosing entity"
// =============================================================================

#[test]
fn pkg_010_within_must_designate_enclosing_entity() {
    let mut session = Session::new(SessionConfig::default());
    session
        .add_document(
            "P/package.mo",
            r#"
            package P
            end P;
        "#,
        )
        .expect("add P/package.mo");
    session
        .add_document(
            "P/M.mo",
            r#"
            within Q;
            model M
                Real x;
            equation
                x = 1;
            end M;
        "#,
        )
        .expect("add P/M.mo");

    let result = session
        .compile_model_phases("P.M")
        .expect("compile_model_phases should complete for P.M");
    match result {
        PhaseResult::Failed {
            phase, error_code, ..
        } => {
            assert_eq!(phase, FailedPhase::Instantiate, "unexpected failed phase");
            assert_eq!(
                error_code.as_deref(),
                Some("rumoca::instantiate::EI001"),
                "unexpected error code for within mismatch"
            );
        }
        other => panic!("expected phase failure for within mismatch, got {other:?}"),
    }
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
