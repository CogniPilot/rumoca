//! ANN (Annotation) contract tests - MLS §18
//!
//! Tests for the 15 annotation contracts defined in SPEC_0022.

use rumoca_contracts::test_support::expect_failure_in_phase_with_code;
use rumoca_session::FailedPhase;

fn expect_ann_todo(source: &str, model: &str) {
    expect_failure_in_phase_with_code(source, model, FailedPhase::Instantiate, "EI000");
}

// =============================================================================
// ANN-001: Forbidden constructs
// =============================================================================

#[test]
#[ignore = "TODO(ANN-001): reject forbidden constructs in annotations (final/each/redeclare/replaceable)"]
fn ann_001_forbidden_constructs() {
    expect_ann_todo(
        r#"
        model Test
            annotation(final foo = 1);
        end Test;
    "#,
        "Test",
    );
}

// =============================================================================
// ANN-002: No empty modification
// =============================================================================

#[test]
#[ignore = "TODO(ANN-002): diagnose deprecated empty annotation element-modification"]
fn ann_002_no_empty_modification() {
    expect_ann_todo(
        r#"
        model Test
            annotation(foo);
        end Test;
    "#,
        "Test",
    );
}

// =============================================================================
// ANN-003: Unit expression valid
// =============================================================================

#[test]
#[ignore = "TODO(ANN-003): validate annotation units with chapter-19 unit-expression grammar"]
fn ann_003_unit_expression_valid() {
    expect_ann_todo(
        r#"
        model Test
            annotation(Icon(coordinateSystem(preserveAspectRatio=true, extent={{100,100},{-100,-100}})));
            annotation(__Dymola_experimentSetupOutput(events=false, unit="m / s"));
        end Test;
    "#,
        "Test",
    );
}

// =============================================================================
// ANN-004: Plot result scalar
// =============================================================================

#[test]
#[ignore = "TODO(ANN-004): reject plot annotations referencing non-scalar results"]
fn ann_004_plot_result_scalar() {
    expect_ann_todo(
        r#"
        model Test
            Real x[2];
            annotation(experiment(StopTime=1.0));
        equation
            x = {1, 2};
        end Test;
    "#,
        "Test",
    );
}

// =============================================================================
// ANN-005: Derivative limit
// =============================================================================

#[test]
#[ignore = "TODO(ANN-005): enforce derivative-order limits in plot annotations"]
fn ann_005_derivative_limit() {
    expect_ann_todo(
        r#"
        model Test
            Real x;
            annotation(experiment(StopTime=1.0));
        equation
            der(x) = -x;
        end Test;
    "#,
        "Test",
    );
}

// =============================================================================
// ANN-006: Unit compatibility
// =============================================================================

#[test]
#[ignore = "TODO(ANN-006): enforce axis-unit compatibility in plot annotations"]
fn ann_006_unit_compatibility() {
    expect_ann_todo(
        r#"
        model Test
            Real x(unit="m");
            annotation(experiment(StopTime=1.0));
        equation
            x = time;
        end Test;
    "#,
        "Test",
    );
}

// =============================================================================
// ANN-007: Figure identifier unique
// =============================================================================

#[test]
#[ignore = "TODO(ANN-007): enforce uniqueness of figure identifiers in plot setup annotations"]
fn ann_007_figure_identifier_unique() {
    expect_ann_todo(
        r#"
        model Test
            annotation(experiment(StopTime=1.0));
            annotation(__OpenModelica_simulationFlags(lv="LOG_STATS"));
        end Test;
    "#,
        "Test",
    );
}

// =============================================================================
// ANN-008: Evaluate scope
// =============================================================================

#[test]
#[ignore = "TODO(ANN-008): restrict Evaluate annotation to parameters/constants"]
fn ann_008_evaluate_scope() {
    expect_ann_todo(
        r#"
        model Test
            Real x annotation(Evaluate=true);
        equation
            x = 1;
        end Test;
    "#,
        "Test",
    );
}

// =============================================================================
// ANN-009: Evaluate non-evaluable warn
// =============================================================================

#[test]
#[ignore = "TODO(ANN-009): warn when Evaluate annotation has no effect on non-evaluable parameters"]
fn ann_009_evaluate_nonevaluable_warn() {
    expect_ann_todo(
        r#"
        model Test
            parameter Real p = time annotation(Evaluate=true);
            Real x;
        equation
            x = p;
        end Test;
    "#,
        "Test",
    );
}

// =============================================================================
// ANN-010: StopTime simulation model
// =============================================================================

#[test]
#[ignore = "TODO(ANN-010): require simulation-model suitability when StopTime is set"]
fn ann_010_stoptime_simulation_model() {
    expect_ann_todo(
        r#"
        model Test
            parameter Real p = 1;
            annotation(experiment(StopTime=1.0));
        end Test;
    "#,
        "Test",
    );
}

// =============================================================================
// ANN-011: mustBeConnected error
// =============================================================================

#[test]
#[ignore = "TODO(ANN-011): enforce mustBeConnected annotation in connection analysis"]
fn ann_011_must_be_connected_error() {
    expect_ann_todo(
        r#"
        connector C
            Real v annotation(mustBeConnected=true);
            flow Real i;
        end C;

        model Test
            C c;
        equation
        end Test;
    "#,
        "Test",
    );
}

// =============================================================================
// ANN-012: mayOnlyConnectOnce error
// =============================================================================

#[test]
#[ignore = "TODO(ANN-012): enforce mayOnlyConnectOnce cardinality in connect sets"]
fn ann_012_may_only_connect_once_error() {
    expect_ann_todo(
        r#"
        connector C
            Real v annotation(mayOnlyConnectOnce=true);
            flow Real i;
        end C;

        model Test
            C a;
            C b;
            C c;
        equation
            connect(a, b);
            connect(a, c);
        end Test;
    "#,
        "Test",
    );
}

// =============================================================================
// ANN-013: Annotation placement
// =============================================================================

#[test]
#[ignore = "TODO(ANN-013): enforce placement constraints for standard annotations"]
fn ann_013_annotation_placement() {
    expect_ann_todo(
        r#"
        model Test
            Real x;
        equation
            x = 1 annotation(experiment(StopTime=1.0));
        end Test;
    "#,
        "Test",
    );
}

// =============================================================================
// ANN-014: TestCase restriction
// =============================================================================

#[test]
#[ignore = "TODO(ANN-014): enforce TestCase-annotation usage restriction across model dependencies"]
fn ann_014_testcase_restriction() {
    expect_ann_todo(
        r#"
        model CaseModel
            annotation(TestCase=true);
        end CaseModel;

        model Test
            CaseModel c;
        end Test;
    "#,
        "Test",
    );
}

// =============================================================================
// ANN-015: Extent coordinate order
// =============================================================================

#[test]
#[ignore = "TODO(ANN-015): validate extent coordinate ordering in graphical annotations"]
fn ann_015_extent_coordinate_order() {
    expect_ann_todo(
        r#"
        model Test
            annotation(Icon(coordinateSystem(extent={{100,100},{-100,-100}})));
        end Test;
    "#,
        "Test",
    );
}
