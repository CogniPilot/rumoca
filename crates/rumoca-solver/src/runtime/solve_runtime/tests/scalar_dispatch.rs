//! The public runtime surface answers each query through the scalar execution
//! it owns, with the same values the execution computes.

use super::*;
use crate::{SimDiagnostic, WARNING_ASSERTION_CODE};

fn root_model() -> solve::SolveModel {
    solve::SolveModel {
        problem: solve::SolveProblem {
            events: solve::SolveEventPartition {
                root_conditions: spanned_block(
                    vec![param_minus_time_root_row(0), time_plus_one_root_row()],
                    "dispatch_roots.mo",
                ),
                root_relation_memory_targets: vec![None; 2],
                root_zero_domains: vec![solve::RootZeroDomain::Previous; 2],
                ..Default::default()
            },
            solve_layout: solve::SolveLayout {
                parameter_count: 1,
                ..Default::default()
            },
            ..Default::default()
        },
        parameters: vec![2.5].into(),
        ..Default::default()
    }
}

#[test]
fn root_conditions_fill_the_callers_buffer_with_the_allocating_values() {
    let model = root_model();
    let runtime = SolveRuntime::new_fixture(&model).expect("valid runtime should prepare");
    let allocated = runtime
        .eval_root_conditions(1.0, &[], &model.parameters, 1.0e-12, 1)
        .expect("root values should evaluate");
    assert_eq!(allocated, vec![1.5, 2.0]);

    let mut filled = vec![f64::NAN; 2];
    runtime
        .eval_root_conditions_into(1.0, &[], &model.parameters, 1.0e-12, 1, &mut filled)
        .expect("root values should fill the buffer");
    assert_eq!(filled, allocated);
}

#[test]
fn guessed_root_search_agrees_with_the_unguessed_search() {
    let model = root_model();
    let runtime = SolveRuntime::new_fixture(&model).expect("valid runtime should prepare");
    let mut plain = vec![0.0; 2];
    runtime
        .eval_root_search_conditions_into(1.0, &[], &model.parameters, 1.0e-12, 1, &mut plain)
        .expect("search roots should evaluate");

    let mut guess = Vec::new();
    let mut guessed = vec![0.0; 2];
    runtime
        .eval_root_search_conditions_with_guess_into(
            1.0,
            &[],
            &model.parameters,
            &mut guess,
            1.0e-12,
            1,
            &mut guessed,
        )
        .expect("guessed search roots should evaluate");
    assert_eq!(guessed, plain);
    assert_eq!(guessed, vec![1.0, 2.0]);
}

#[test]
fn dynamic_time_event_rows_evaluate_their_deadlines_at_the_given_coordinate() {
    let model = solve::SolveModel {
        problem: solve::SolveProblem {
            events: solve::SolveEventPartition {
                dynamic_time_event_names: vec!["deadline".to_owned()],
                dynamic_time_event_rhs: spanned_block(
                    vec![vec![
                        solve::LinearOp::LoadP { dst: 0, index: 0 },
                        solve::LinearOp::LoadTime { dst: 1 },
                        solve::LinearOp::Binary {
                            dst: 2,
                            op: solve::BinaryOp::Add,
                            lhs: 0,
                            rhs: 1,
                        },
                        solve::LinearOp::StoreOutput { src: 2 },
                    ]],
                    "dispatch_deadline.mo",
                ),
                ..Default::default()
            },
            solve_layout: solve::SolveLayout {
                parameter_count: 1,
                ..Default::default()
            },
            ..Default::default()
        },
        parameters: vec![0.5].into(),
        ..Default::default()
    };
    let runtime = SolveRuntime::new_fixture(&model).expect("valid runtime should prepare");
    let mut deadlines = [f64::NAN];
    runtime
        .eval_dynamic_time_event_rows_into(2.0, &[], &model.parameters, &mut deadlines)
        .expect("the deadline row evaluates");
    assert_eq!(deadlines, [2.5]);
    assert!(
        runtime
            .eval_dynamic_time_event_rows_into(2.0, &[], &model.parameters, &mut [])
            .is_err(),
        "a buffer that does not match the deadline rows is refused"
    );
}

fn assignment_model(
    initialization: bool,
) -> (solve::SolveModel, Vec<solve::LinearOp>, solve::ScalarSlot) {
    let slot = solve::scalar_slot_p(0);
    let row = const_visible_value_row(3.0);
    let mut model = solve::SolveModel {
        problem: solve::SolveProblem {
            solve_layout: solve::SolveLayout {
                parameter_count: 1,
                compiled_parameter_len: 1,
                ..Default::default()
            },
            ..Default::default()
        },
        parameters: vec![0.0].into(),
        ..Default::default()
    };
    if initialization {
        model.problem.initialization =
            solve::InitializationSolveSystem::construct(solve::InitializationSystemInput {
                update_rhs: spanned_block(vec![row.clone()], "dispatch_initial_update.mo"),
                update_targets: vec![slot],
                ..Default::default()
            })
            .expect("an update-only initialization system is checked");
    } else {
        model.problem.discrete.runtime_assignment_rhs =
            spanned_block(vec![row.clone()], "dispatch_runtime_assignment.mo");
        model.problem.discrete.runtime_assignment_targets = vec![slot];
        model.problem.discrete.runtime_assignment_roles =
            vec![solve::RuntimeAssignmentRole::RelationFree];
    }
    (model, row, slot)
}

#[test]
fn initialization_updates_write_their_targets_once_and_then_report_no_change() {
    let (model, _, _) = assignment_model(true);
    let runtime = SolveRuntime::new_fixture(&model).expect("valid runtime should prepare");
    let mut p = model.parameters.to_vec();
    assert!(
        runtime
            .apply_initialization_updates(&mut [], &mut p, 0.0, 1.0e-10, 4)
            .expect("the update row applies")
    );
    assert_eq!(p, [3.0]);
    assert!(
        !runtime
            .apply_initialization_updates(&mut [], &mut p, 0.0, 1.0e-10, 4)
            .expect("the settled update row applies")
    );
}

#[test]
fn runtime_assignments_settle_to_a_fixed_point() {
    let (model, _, _) = assignment_model(false);
    let runtime = SolveRuntime::new_fixture(&model).expect("valid runtime should prepare");
    let mut p = model.parameters.to_vec();
    assert!(
        runtime
            .apply_runtime_assignments_until_stable(&mut [], &mut p, 0.0, 1.0e-10, 4)
            .expect("the assignment row applies")
    );
    assert_eq!(p, [3.0]);
    assert!(
        !runtime
            .apply_runtime_assignments_until_stable(&mut [], &mut p, 0.0, 1.0e-10, 4)
            .expect("the settled assignment row applies")
    );
}

#[test]
fn recorded_visible_samples_append_one_value_per_column() {
    let model = crate::fixture_layout::with_visible_expressions(
        solve::SolveModel::default(),
        vec!["y2".to_string(), "p1".to_string(), "time".to_string()],
        spanned_block(
            vec![
                direct_y_visible_value_row(1),
                direct_param_visible_value_row(0),
                vec![
                    solve::LinearOp::LoadTime { dst: 0 },
                    solve::LinearOp::StoreOutput { src: 0 },
                ],
            ],
            "dispatch_visible.mo",
        ),
        Vec::new(),
    );
    let runtime = SolveRuntime::new_fixture(&model).expect("valid runtime should prepare");
    let mut columns = vec![Vec::new(); 3];
    runtime
        .record_visible_sample(&mut columns, &[10.0, 20.0], &[3.5], 4.25)
        .expect("the first sample records");
    runtime
        .record_visible_sample(&mut columns, &[11.0, 21.0], &[3.5], 5.0)
        .expect("the second sample records");
    assert_eq!(
        columns,
        vec![vec![20.0, 21.0], vec![3.5, 3.5], vec![4.25, 5.0]]
    );
    assert!(
        runtime
            .record_visible_sample(&mut vec![Vec::new(); 2], &[10.0, 20.0], &[3.5], 6.0)
            .is_err(),
        "a column count that differs from the visible outputs is refused"
    );
}

fn warning_model() -> solve::SolveModel {
    solve::SolveModel {
        problem: solve::SolveProblem {
            events: solve::SolveEventPartition {
                action_conditions: spanned_block(
                    vec![const_visible_value_row(1.0)],
                    "dispatch_warning_condition.mo",
                ),
                actions: vec![solve::SolveEventAction {
                    kind: solve::SolveEventActionKind::Warning,
                    message: solve::SolveEventMessage {
                        parts: vec![solve::SolveEventMessagePart::Text("check the input".into())],
                    },
                    span: test_span("dispatch_warning.mo"),
                    origin: "Fixture.check".into(),
                    clock_owner: None,
                    assertion_projection: None,
                }],
                ..Default::default()
            },
            ..Default::default()
        },
        ..Default::default()
    }
}

#[test]
fn diagnostics_since_returns_only_the_entries_after_the_cursor() {
    let runtime =
        SolveRuntime::new_fixture(&warning_model()).expect("valid runtime should prepare");
    assert_eq!(runtime.diagnostics_since(0), (Vec::new(), 0));

    runtime
        .observe_warnings(&[], &[], 0.5)
        .expect("the warning site is observed");
    let (reported, cursor): (Vec<SimDiagnostic>, usize) = runtime.diagnostics_since(0);
    assert_eq!(cursor, 1);
    assert_eq!(reported.len(), 1);
    assert_eq!(reported[0].code, WARNING_ASSERTION_CODE);
    assert_eq!(reported[0].message, "check the input");
    assert_eq!(reported[0].time, 0.5);

    runtime
        .observe_warnings(&[], &[], 0.9)
        .expect("a reported site is not observed again");
    assert_eq!(runtime.diagnostics_since(cursor), (Vec::new(), 1));
    assert_eq!(
        runtime.diagnostics_since(99),
        (Vec::new(), 1),
        "a cursor past the log yields nothing"
    );
    assert_eq!(runtime.diagnostics_since(0).0, runtime.diagnostics());
}

#[test]
fn implicit_residual_row_count_is_the_number_of_implicit_rows() {
    let runtime =
        SolveRuntime::new_fixture(&warm_start_test_model()).expect("valid runtime should prepare");
    assert_eq!(runtime.implicit_residual_row_count(), 2);
}

#[test]
fn admitted_errors_take_the_one_source_span_the_block_issues() {
    let (first, second) = (test_span("first.mo"), test_span("second.mo"));
    let span_of = |spans: &[rumoca_core::Span], error| match admitted_compiled_error(
        "dispatch", spans, error,
    ) {
        RuntimeSolveError::SolveIr { span, .. } | RuntimeSolveError::SourceFault { span, .. } => {
            span
        }
        other => panic!("the span-carrying variants keep their variant: {other}"),
    };
    let bare = || RuntimeSolveError::SolveIr {
        message: "fault".into(),
        span: None,
    };
    assert_eq!(span_of(&[first, first], bare()), Some(first));
    assert_eq!(span_of(&[first, second], bare()), None);
    assert_eq!(span_of(&[], bare()), None);
    assert_eq!(
        span_of(
            &[first],
            RuntimeSolveError::SolveIr {
                message: "fault".into(),
                span: Some(second),
            }
        ),
        Some(second),
        "an error that names its own span keeps it"
    );
    assert_eq!(
        span_of(
            &[first],
            RuntimeSolveError::SourceFault {
                message: "fault".into(),
                span: None,
            }
        ),
        Some(first)
    );
}

#[test]
fn solver_names_fall_back_to_a_placeholder_for_unnamed_slots() {
    let runtime =
        SolveRuntime::new_fixture(&warm_start_test_model()).expect("valid runtime should prepare");
    let view = runtime.scalar_execution().unwrap();
    assert_eq!(view.solver_name(0), "x");
    assert_eq!(view.solver_name(1), "z");
    assert_eq!(view.solver_name(9), "<unnamed>");
}
