use super::*;

struct Backend {
    refused: rumoca_core::Span,
    only_time: bool,
    fault: Option<rumoca_core::Span>,
    executions: Rc<std::cell::Cell<usize>>,
}

struct Expression {
    fault: Option<rumoca_core::Span>,
    inner: Rc<dyn CompiledSolveExpression>,
    executions: Rc<std::cell::Cell<usize>>,
}

impl CompiledSolveExpression for Expression {
    fn call(
        &self,
        y: &[f64],
        p: &[f64],
        t: f64,
        tables: &[rumoca_core::ExternalTableData],
        out: &mut [f64],
    ) -> Result<(), RuntimeSolveError> {
        self.executions.set(self.executions.get() + 1);
        if let Some(span) = self.fault {
            return Err(RuntimeSolveError::SourceFault {
                message: "numeric source operation fault".into(),
                span: Some(span),
            });
        }
        self.inner.call(y, p, t, tables, out)
    }
}

impl SolveExecutionBackend for Backend {
    fn compile_expression(
        &self,
        block: &solve::ScalarProgramBlock,
    ) -> Result<Rc<dyn CompiledSolveExpression>, String> {
        if block.program_spans().contains(&self.refused)
            && (!self.only_time
                || block
                    .programs()
                    .iter()
                    .flatten()
                    .any(|op| matches!(op, solve::LinearOp::LoadTime { .. })))
        {
            return Err("unsupported cold lifecycle program".into());
        }
        Ok(Rc::new(Expression {
            fault: self
                .fault
                .filter(|span| block.program_spans().contains(span)),
            inner: super::super::test_kernels::expression(block)?,
            executions: self.executions.clone(),
        }))
    }
    fn compile_jacobian_expression(
        &self,
        block: &solve::ScalarProgramBlock,
    ) -> Result<Rc<dyn CompiledSolveJacobianExpression>, String> {
        super::super::test_kernels::jacobian(block)
    }
    fn compile_assignment_schedule(
        &self,
        source: &solve::ComputeBlock,
        owners: &solve::ContinuousRefreshOwners,
        schedule: &solve::ExactRefreshAssignmentSchedule,
    ) -> Result<Rc<dyn CompiledSolveAssignmentSchedule>, String> {
        super::super::test_kernels::assignment(source, owners, schedule)
    }
    fn compile_event_transaction(
        &self,
        _: &solve::EventTransactionProgram,
    ) -> Result<Rc<dyn CompiledSolveEventTransaction>, String> {
        Err("fixture has no transaction".into())
    }
}

#[test]
fn unsupported_lifecycle_program_fails_before_any_execution_with_source_span() {
    let mut model = warm_start_test_model();
    let span = test_span("cold_assignment.mo");
    model.problem.discrete.runtime_assignment_rhs = spanned_block(
        vec![vec![
            solve::LinearOp::Const { dst: 0, value: 3.0 },
            solve::LinearOp::StoreOutput { src: 0 },
        ]],
        "cold_assignment.mo",
    );
    model.problem.discrete.runtime_assignment_targets = vec![solve::scalar_slot_y(1)];
    let executions = Rc::new(std::cell::Cell::new(0));
    let backend = Rc::new(Backend {
        refused: span,
        only_time: false,
        fault: None,
        executions: executions.clone(),
    });
    let error = SolveRuntime::new_fixture_with_backend(&model, backend)
        .err()
        .expect("the entire runtime must reject unsupported lifecycle code");
    assert!(
        matches!(error, EvalSolveError::CompiledExecution { span: Some(issued), ref message } if issued == span && message.contains("runtime_assignments[0]")),
        "{error}"
    );
    assert_eq!(
        executions.get(),
        0,
        "preparation must not execute source code"
    );
    let reference =
        SolveRuntime::new_fixture(&model).expect("explicit reference execution stays available");
    let mut y = model.initial_y.to_vec();
    reference
        .apply_runtime_assignments_until_stable(&mut y, &mut [], 0.0, 1e-10, 4)
        .unwrap();
    assert_eq!(y[1], 3.0);
}

#[test]
fn lifecycle_inventory_rejects_a_foreign_prepared_source() {
    let model = warm_start_test_model();
    let runtime = SolveRuntime::new_fixture_with_backend(
        &model,
        Rc::new(super::super::test_kernels::ReferenceBackend),
    )
    .unwrap();
    let scalar = runtime.scalar_execution().unwrap();
    let foreign = scalar.scalar.discrete_rhs.clone();
    assert!(scalar.scalar.lifecycle_programs(&foreign).is_err());
    assert!(
        scalar
            .scalar
            .lifecycle_programs(&scalar.scalar.discrete_rhs)
            .unwrap()
            .is_some()
    );
}

#[test]
fn unsupported_cold_message_program_fails_before_any_execution_with_source_span() {
    let mut model = warm_start_test_model();
    let span = test_span("cold_message.mo");
    model.problem.events.actions = vec![solve::SolveEventAction {
        kind: solve::SolveEventActionKind::Warning,
        message: solve::SolveEventMessage {
            parts: vec![solve::SolveEventMessagePart::Conversion {
                value: vec![
                    solve::LinearOp::Const { dst: 0, value: 3.0 },
                    solve::LinearOp::StoreOutput { src: 0 },
                ],
                source: solve::SolveStringConversionSource::Real,
                format: solve::SolveStringConversionFormat::Options {
                    minimum_length: None,
                    left_justified: None,
                    significant_digits: None,
                },
            }],
        },
        span,
        origin: "cold warning".into(),
        clock_owner: None,
        assertion_projection: None,
    }];
    model.problem.events.action_conditions = spanned_block(
        vec![vec![
            solve::LinearOp::Const { dst: 0, value: 0.0 },
            solve::LinearOp::StoreOutput { src: 0 },
        ]],
        "supported_action_condition.mo",
    );
    let executions = Rc::new(std::cell::Cell::new(0));
    let error = SolveRuntime::new_fixture_with_backend(
        &model,
        Rc::new(Backend {
            refused: span,
            only_time: false,
            fault: None,
            executions: executions.clone(),
        }),
    )
    .err()
    .expect("inactive diagnostic code must still be lowered before admission");
    assert!(
        matches!(error, EvalSolveError::CompiledExecution { span: Some(issued), .. } if issued == span),
        "{error}"
    );
    assert_eq!(executions.get(), 0);
    SolveRuntime::new_fixture(&model).expect("explicit reference remains available");
}

fn numeric_message_model() -> solve::SolveModel {
    let mut model = warm_start_test_model();
    let constant = |value| {
        vec![
            solve::LinearOp::Const { dst: 0, value },
            solve::LinearOp::StoreOutput { src: 0 },
        ]
    };
    let product = |index, factor| {
        vec![
            solve::LinearOp::LoadY { dst: 0, index },
            solve::LinearOp::Const {
                dst: 1,
                value: factor,
            },
            solve::LinearOp::Binary {
                dst: 2,
                op: solve::BinaryOp::Mul,
                lhs: 0,
                rhs: 1,
            },
            solve::LinearOp::StoreOutput { src: 2 },
        ]
    };
    model.problem.events.actions = vec![solve::SolveEventAction {
        kind: solve::SolveEventActionKind::Warning,
        message: solve::SolveEventMessage {
            parts: vec![
                solve::SolveEventMessagePart::Text("value=".into()),
                solve::SolveEventMessagePart::Conversion {
                    value: product(0, 2.0),
                    source: solve::SolveStringConversionSource::Real,
                    format: solve::SolveStringConversionFormat::Options {
                        minimum_length: Some(product(1, 4.0)),
                        left_justified: Some(constant(0.0)),
                        significant_digits: Some(constant(3.0)),
                    },
                },
            ],
        },
        span: test_span("numeric_message.mo"),
        origin: "numeric warning".into(),
        clock_owner: None,
        assertion_projection: None,
    }];
    model.problem.events.action_conditions =
        spanned_block(vec![constant(1.0)], "message_condition.mo");
    model
}

#[test]
fn compiled_messages_execute_prepared_values_and_all_dynamic_options() {
    let model = numeric_message_model();
    let executions = Rc::new(std::cell::Cell::new(0));
    let runtime = SolveRuntime::new_fixture_with_backend(
        &model,
        Rc::new(Backend {
            refused: test_span("unrelated.mo"),
            only_time: false,
            fault: None,
            executions: executions.clone(),
        }),
    )
    .unwrap();
    assert_eq!(
        executions.get(),
        0,
        "preparation does not execute the message"
    );
    let scalar = runtime.scalar_execution().unwrap();
    scalar
        .report_violated_warnings(
            &[1.0],
            &model.initial_y,
            &[],
            0.0,
            runtime.row_eval_context(),
        )
        .unwrap();
    assert_eq!(
        executions.get(),
        4,
        "the value and every authored option execute compiled code"
    );
    assert_eq!(runtime.diagnostics()[0].message, "value=       2");
    let reference = SolveRuntime::new_fixture(&model).unwrap();
    reference
        .scalar_execution()
        .unwrap()
        .report_violated_warnings(
            &[1.0],
            &model.initial_y,
            &[],
            0.0,
            reference.row_eval_context(),
        )
        .unwrap();
    assert_eq!(runtime.diagnostics(), reference.diagnostics());
}

#[test]
fn compiled_message_context_rejects_foreign_action_even_with_identical_content() {
    let model = numeric_message_model();
    let runtime = SolveRuntime::new_fixture_with_backend(
        &model,
        Rc::new(super::super::test_kernels::ReferenceBackend),
    )
    .unwrap();
    let error = solve_eval::eval_event_action_message(
        &model.problem.events.actions[0],
        &model.initial_y,
        &[],
        0.0,
        runtime.row_eval_context(),
    )
    .unwrap_err();
    assert!(matches!(error, EvalSolveError::CompiledExecution { .. }));
    let shared = runtime.clone();
    let message = solve_eval::eval_event_action_message(
        &shared.model.problem.events.actions[0],
        &model.initial_y,
        &[],
        0.0,
        runtime.row_eval_context(),
    )
    .unwrap();
    assert_eq!(message, "value=       2");
}

#[test]
fn unsupported_dynamic_message_options_refuse_during_preparation() {
    for option in 0..3 {
        let mut model = numeric_message_model();
        let action = &mut model.problem.events.actions[0];
        let solve::SolveEventMessagePart::Conversion { format, .. } = &mut action.message.parts[1]
        else {
            unreachable!("fixture conversion");
        };
        let solve::SolveStringConversionFormat::Options {
            minimum_length,
            left_justified,
            significant_digits,
        } = format;
        let slots = [minimum_length, left_justified, significant_digits];
        *slots.into_iter().nth(option).unwrap() = Some(vec![
            solve::LinearOp::LoadTime { dst: 0 },
            solve::LinearOp::StoreOutput { src: 0 },
        ]);
        let span = action.span;
        let executions = Rc::new(std::cell::Cell::new(0));
        let error = SolveRuntime::new_fixture_with_backend(
            &model,
            Rc::new(Backend {
                refused: span,
                only_time: true,
                fault: None,
                executions: executions.clone(),
            }),
        )
        .err()
        .expect("every authored option needs lowering, even on a cold path");
        assert!(
            matches!(error, EvalSolveError::CompiledExecution { span: Some(issued), .. } if issued == span)
        );
        assert_eq!(executions.get(), 0);
    }
}

#[test]
fn malformed_message_outputs_refuse_before_execution() {
    let mut model = numeric_message_model();
    let action = &mut model.problem.events.actions[0];
    let solve::SolveEventMessagePart::Conversion { value, .. } = &mut action.message.parts[1]
    else {
        unreachable!("fixture conversion");
    };
    value.push(solve::LinearOp::StoreOutput { src: 2 });
    let span = action.span;
    let error = SolveRuntime::new_fixture_with_backend(
        &model,
        Rc::new(super::super::test_kernels::ReferenceBackend),
    )
    .err()
    .expect("message scalarity must be proven before admission");
    assert!(
        matches!(error, EvalSolveError::CompiledExecution { span: Some(issued), .. } if issued == span)
    );
}

#[test]
fn compiled_message_source_fault_preserves_category_and_does_not_publish() {
    let model = numeric_message_model();
    let span = model.problem.events.actions[0].span;
    let runtime = SolveRuntime::new_fixture_with_backend(
        &model,
        Rc::new(Backend {
            refused: test_span("unrelated.mo"),
            only_time: false,
            fault: Some(span),
            executions: Rc::new(std::cell::Cell::new(0)),
        }),
    )
    .unwrap();
    let error = runtime
        .scalar_execution()
        .unwrap()
        .report_violated_warnings(
            &[1.0],
            &model.initial_y,
            &[],
            0.0,
            runtime.row_eval_context(),
        )
        .unwrap_err();
    assert!(
        matches!(error, RuntimeSolveError::SourceFault { span: Some(issued), .. } if issued == span)
    );
    assert!(runtime.diagnostics().is_empty());
}

#[test]
fn named_visible_expression_uses_its_prepared_compiled_program() {
    let model = crate::fixture_layout::with_visible_expressions(
        warm_start_test_model(),
        vec!["computed".into()],
        spanned_block(
            vec![vec![
                solve::LinearOp::LoadY { dst: 0, index: 0 },
                solve::LinearOp::Const { dst: 1, value: 2.0 },
                solve::LinearOp::Binary {
                    dst: 2,
                    op: solve::BinaryOp::Mul,
                    lhs: 0,
                    rhs: 1,
                },
                solve::LinearOp::StoreOutput { src: 2 },
            ]],
            "visible_case.mo",
        ),
        vec![],
    );
    let executions = Rc::new(std::cell::Cell::new(0));
    let runtime = SolveRuntime::new_fixture_with_backend(
        &model,
        Rc::new(Backend {
            refused: test_span("unrelated.mo"),
            only_time: false,
            fault: None,
            executions: executions.clone(),
        }),
    )
    .unwrap();
    let value = runtime
        .scalar_execution()
        .unwrap()
        .visible_value_from_row("computed", &model.initial_y, &[], 0.0)
        .unwrap();
    assert_eq!(value, Some(2.0));
    assert_eq!(
        executions.get(),
        1,
        "named reads must use the prepared visible kernel"
    );
}

#[test]
fn prepared_deadline_queries_preserve_compiled_source_faults() {
    let mut model = warm_start_test_model();
    let span = test_span("compiled_deadline.mo");
    model.problem.events.dynamic_time_event_rhs = spanned_block(
        vec![vec![
            solve::LinearOp::Const {
                dst: 0,
                value: 0.25,
            },
            solve::LinearOp::StoreOutput { src: 0 },
        ]],
        "compiled_deadline.mo",
    );
    let executions = Rc::new(std::cell::Cell::new(0));
    let runtime = SolveRuntime::new_fixture_with_backend(
        &model,
        Rc::new(Backend {
            refused: test_span("unused.mo"),
            only_time: false,
            fault: Some(span),
            executions: executions.clone(),
        }),
    )
    .unwrap();
    let y = model.initial_y.to_vec();
    let p = model.parameters.to_vec();
    let mut schedule = SolveStopSchedule::new(&model.problem, 0.0, 1.0);
    let before = schedule.next_stop(0.0, 1.0);
    for error in [
        runtime
            .current_dynamic_time_event_stop(&y, &p, 0.25)
            .unwrap_err(),
        runtime
            .next_runtime_event_stop(&y, &p, &mut schedule, 0.0, 1.0)
            .unwrap_err(),
    ] {
        assert!(
            matches!(error, RuntimeSolveError::SourceFault { span: Some(issued), .. } if issued == span),
            "{error}"
        );
    }
    assert_eq!(
        schedule.next_stop(0.0, 1.0),
        before,
        "a failed deadline query cannot consume a scheduled stop"
    );
    assert_eq!(executions.get(), 2);
    let reference = SolveRuntime::new_fixture(&model).unwrap();
    assert!(
        reference
            .current_dynamic_time_event_stop(&y, &p, 0.25)
            .unwrap()
            .is_some()
    );
    let (time, event) = reference
        .next_runtime_event_stop(&y, &p, &mut schedule, 0.0, 1.0)
        .unwrap();
    assert_eq!(time, 0.25);
    assert!(event.is_some());
}

#[test]
fn prepared_condition_memory_seed_and_pulse_release_preserve_compiled_faults() {
    let span = test_span("compiled_condition_memory.mo");
    let model = solve::SolveModel {
        problem: solve::SolveProblem {
            solve_layout: solve::SolveLayout {
                parameter_count: 1,
                compiled_parameter_len: 1,
                ..Default::default()
            },
            events: solve::SolveEventPartition {
                condition_memory_parameter_indices: vec![0],
                ..Default::default()
            },
            discrete: solve::DiscreteSolveSystem {
                rhs: spanned_block(
                    vec![vec![
                        solve::LinearOp::Const { dst: 0, value: 1.0 },
                        solve::LinearOp::StoreOutput { src: 0 },
                    ]],
                    "compiled_condition_memory.mo",
                ),
                update_targets: vec![solve::scalar_slot_p(0)],
                row_roles: vec![solve::DiscreteRowRole::PulseConditionMemory],
                pre_modes: vec![solve::DiscreteEventPreMode::FollowCurrent],
                observation_refresh: vec![false],
                integrator_history_effects: vec![solve::IntegratorHistoryEffect::Preserve],
                clock_owners: vec![None],
                ..Default::default()
            },
            ..Default::default()
        },
        parameters: vec![0.0].into(),
        ..Default::default()
    };
    let executions = Rc::new(std::cell::Cell::new(0));
    let runtime = SolveRuntime::new_fixture_with_backend(
        &model,
        Rc::new(Backend {
            refused: test_span("unused.mo"),
            only_time: false,
            fault: Some(span),
            executions: executions.clone(),
        }),
    )
    .unwrap();
    let mut p = model.parameters.to_vec();
    let scalar = runtime.scalar_execution().unwrap();
    let seed = scalar
        .seed_condition_memory_for_initialization(&mut [], &mut p, 0.0, 1e-10)
        .err()
        .expect("a compiled seed source fault must fail the seed");
    let pulse = scalar
        .release_condition_pulses(&mut [], &mut p, 0.0)
        .unwrap_err();
    for error in [seed, pulse] {
        assert!(
            matches!(error, RuntimeSolveError::SourceFault { span: Some(issued), .. } if issued == span),
            "{error}"
        );
    }
    assert_eq!(executions.get(), 2);
    assert_eq!(p, [0.0], "faulted condition memory writes cannot commit");
    let reference = SolveRuntime::new_fixture(&model).unwrap();
    reference
        .seed_condition_memory_for_initialization(&mut [], &mut p, 0.0, 1e-10)
        .unwrap();
    assert_eq!(p, [1.0]);
    reference
        .scalar_execution()
        .unwrap()
        .release_condition_pulses(&mut [], &mut p, 0.0)
        .unwrap();
    assert_eq!(p, [1.0]);
}
