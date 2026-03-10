//! ANN (Annotation) contract tests - MLS §18

use rumoca_contracts::test_support::{
    expect_parse_err_with_code, expect_resolve_failure_with_code, expect_success,
};

// =============================================================================
// ANN-001: Forbidden constructs
// "final, each, element-redeclaration, element-replaceable shall not be used in annotations"
// =============================================================================

#[test]
fn ann_001_component_annotation_each_rejected() {
    expect_parse_err_with_code(
        r#"
        model Test
            Real x annotation(each Evaluate = true);
        end Test;
    "#,
        "EP001",
    );
}

#[test]
fn ann_001_class_annotation_redeclare_rejected() {
    expect_parse_err_with_code(
        r#"
        model Base
        end Base;

        model Test
            annotation(redeclare model Variant = Base);
        end Test;
    "#,
        "EP001",
    );
}

#[test]
fn ann_001_extends_annotation_replaceable_rejected() {
    expect_parse_err_with_code(
        r#"
        model Base
        end Base;

        model Test
            extends Base annotation(replaceable model Variant = Base);
        end Test;
    "#,
        "EP001",
    );
}

// =============================================================================
// ANN-008: Evaluate scope
// "Annotation Evaluate is only allowed for parameters and constants"
// =============================================================================

#[test]
fn ann_008_parameter_evaluate_ok() {
    expect_success(
        r#"
        model Test
            parameter Real p = 2 annotation(Evaluate = true);
            Real x;
        equation
            x = p;
        end Test;
    "#,
        "Test",
    );
}

#[test]
fn ann_008_variable_evaluate_rejected() {
    expect_resolve_failure_with_code(
        r#"
        model Test
            Real p annotation(Evaluate = true);
        equation
            p = 1;
        end Test;
    "#,
        "Test",
        "ER070",
    );
}

#[test]
fn ann_008_class_annotation_evaluate_rejected() {
    expect_resolve_failure_with_code(
        r#"
        model Test
            annotation(Evaluate = true);
        end Test;
    "#,
        "Test",
        "ER070",
    );
}

#[test]
fn ann_008_extends_annotation_evaluate_rejected() {
    expect_resolve_failure_with_code(
        r#"
        model Base
        end Base;

        model Test
            extends Base annotation(Evaluate = true);
        end Test;
    "#,
        "Test",
        "ER070",
    );
}
