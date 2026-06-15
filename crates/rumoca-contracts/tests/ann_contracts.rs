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
fn ann_008_function_local_evaluate_ignored() {
    expect_success(
        r#"
        function ToVector
            input Real x[:];
            output Real y[size(x, 1)];
        protected
            Integer n = size(x, 1) annotation(Evaluate = true);
        algorithm
            for i in 1:n loop
                y[i] := x[i];
            end for;
        end ToVector;

        model Test
            Real x[2] = ToVector({1.0, 2.0});
        end Test;
    "#,
        "Test",
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

// =============================================================================
// ANN-003: Non-empty unit shall match unit-expression in chapter 19
// =============================================================================

#[test]
fn ann_003_invalid_unit_expression_rejected() {
    rumoca_contracts::test_support::expect_failure_in_phase_with_code(
        r#"
        model M
            Real x(unit = "m/s/s") = 1;
        end M;
    "#,
        "M",
        rumoca_compile::compile::FailedPhase::Typecheck,
        "ET010",
    );
}

// =============================================================================
// ANN-010: If StopTime is set in non-partial model, it is required to be a
// simulation model
// =============================================================================

#[test]
fn ann_010_experiment_stop_time_carried_to_simulation_model() {
    let result = rumoca_contracts::test_support::expect_success(
        r#"
        model M
            Real x(start = 0, fixed = true);
        equation
            der(x) = 1;
            annotation(experiment(StopTime = 2.0));
        end M;
    "#,
        "M",
    );
    assert_eq!(
        result.experiment_stop_time,
        Some(2.0),
        "experiment StopTime must reach the simulation model metadata"
    );
}

// =============================================================================
// ANN-013: Standard annotations shall only be used where their semantics is
// defined
// =============================================================================

#[test]
fn ann_013_experiment_on_function_warns() {
    rumoca_contracts::test_support::expect_compile_warning(
        r#"
        model M
            function F
                input Real u;
                output Real y;
            algorithm
                y := u;
                annotation(experiment(StopTime = 1.0));
            end F;
            Real z = F(1.0);
        end M;
    "#,
        "M",
        "WR003",
    );
}

// =============================================================================
// ANN-014: Class with TestCase annotation shall not be used in other models
// unless those also have TestCase
// =============================================================================

#[test]
fn ann_014_testcase_class_reuse_warns() {
    rumoca_contracts::test_support::expect_compile_warning(
        r#"
        model M
            model Tc
                Real x = 1;
                annotation(TestCase(shouldPass = true));
            end Tc;
            Tc t;
        end M;
    "#,
        "M",
        "WR004",
    );
}

// =============================================================================
// ANN-009: For non-evaluable parameter, Evaluate has no impact; issue warning
// =============================================================================

#[test]
fn ann_009_evaluate_on_unbound_parameter_warns() {
    rumoca_contracts::test_support::expect_compile_warning(
        r#"
        model M
            parameter Real p = 1 annotation(Evaluate = true);
            parameter Real q(start = 1) annotation(Evaluate = true);
            Real x = p + q;
        end M;
    "#,
        "M",
        "WR005",
    );
}
