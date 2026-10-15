use super::*;
use crate::runtime::solve_runtime::{test_kernels, tests::warm_start_test_model};

fn deadline_span() -> rumoca_core::Span {
    rumoca_core::Span::from_offsets(
        rumoca_core::SourceId::from_source_name("dynamic_deadline.mo"),
        1,
        2,
    )
}

struct Backend {
    calls: Rc<std::cell::Cell<usize>>,
    fault: bool,
}

struct Expression {
    inner: Rc<dyn CompiledSolveExpression>,
    calls: Rc<std::cell::Cell<usize>>,
    fault: bool,
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
        self.calls.set(self.calls.get() + 1);
        if self.fault {
            return Err(RuntimeSolveError::SourceFault {
                message: "dynamic deadline source fault".into(),
                span: Some(deadline_span()),
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
        Ok(Rc::new(Expression {
            inner: test_kernels::expression(block)?,
            calls: self.calls.clone(),
            fault: self.fault && block.program_spans().contains(&deadline_span()),
        }))
    }

    fn compile_jacobian_expression(
        &self,
        block: &solve::ScalarProgramBlock,
    ) -> Result<Rc<dyn CompiledSolveJacobianExpression>, String> {
        test_kernels::jacobian(block)
    }

    fn compile_assignment_schedule(
        &self,
        source: &solve::ComputeBlock,
        owners: &solve::ContinuousRefreshOwners,
        schedule: &solve::ExactRefreshAssignmentSchedule,
    ) -> Result<Rc<dyn CompiledSolveAssignmentSchedule>, String> {
        test_kernels::assignment(source, owners, schedule)
    }
}

fn deadline_model(deadline: f64) -> solve::SolveModel {
    let mut model = warm_start_test_model();
    model.problem.layout = crate::fixture_layout::named_y_layout(
        solve::VarLayout::from_parts(Default::default(), 2, 2),
        vec!["x".into(), "z".into()],
    );
    model.problem.solve_layout.parameter_count = 2;
    model.problem.solve_layout.compiled_parameter_len = 2;
    model.parameters = vec![deadline, 0.0].into();
    model.problem.events.dynamic_time_event_rhs = solve::ScalarProgramBlock::with_program_spans(
        vec![vec![
            solve::LinearOp::LoadP { dst: 0, index: 0 },
            solve::LinearOp::StoreOutput { src: 0 },
        ]],
        vec![deadline_span()],
    )
    .unwrap();
    model
}

fn compiled_runtime(
    model: &solve::SolveModel,
    fault: bool,
) -> (SolveRuntime, Rc<std::cell::Cell<usize>>) {
    let calls = Rc::new(std::cell::Cell::new(0));
    let runtime = SolveRuntime::new_fixture_with_backend(
        model,
        Rc::new(Backend {
            calls: calls.clone(),
            fault,
        }),
    )
    .unwrap();
    assert_eq!(calls.get(), 0, "preparation must not evaluate the source");
    (runtime, calls)
}

#[test]
fn deadline_scheduling_current_stop_executes_prepared_code() {
    let model = deadline_model(0.25);
    let (runtime, calls) = compiled_runtime(&model, false);
    let actual = runtime
        .current_dynamic_time_event_stop(&model.initial_y, &model.parameters, 0.25)
        .unwrap();
    let reference = SolveRuntime::new_fixture(&model).unwrap();
    let expected = reference
        .current_dynamic_time_event_stop(&model.initial_y, &model.parameters, 0.25)
        .unwrap();
    assert_eq!(actual, Some(RuntimeEventStop::dynamic_time_event()));
    assert_eq!(actual, expected);
    assert_eq!(
        calls.get(),
        1,
        "current-stop query must execute prepared deadline code"
    );
}

#[test]
fn deadline_scheduling_next_stop_executes_prepared_code() {
    let model = deadline_model(0.25);
    let (runtime, calls) = compiled_runtime(&model, false);
    let mut schedule = SolveStopSchedule::new(&model.problem, 0.0, 1.0);
    let before = schedule.clone();
    let actual = runtime
        .next_runtime_event_stop(&model.initial_y, &model.parameters, &mut schedule, 0.0, 1.0)
        .unwrap();
    let reference = SolveRuntime::new_fixture(&model).unwrap();
    let expected = reference
        .next_runtime_event_stop(
            &model.initial_y,
            &model.parameters,
            &mut before.clone(),
            0.0,
            1.0,
        )
        .unwrap();
    assert_eq!(actual, (0.25, Some(RuntimeEventStop::dynamic_time_event())));
    assert_eq!(actual, expected);
    assert!(schedule.bit_eq(&before));
    assert_eq!(
        calls.get(),
        1,
        "next-stop query must execute prepared deadline code"
    );
}

#[test]
fn deadline_scheduling_compiled_fault_preserves_schedule_and_input_state() {
    let model = deadline_model(0.25);
    let (runtime, calls) = compiled_runtime(&model, true);
    let y = model.initial_y.to_vec();
    let p = model.parameters.to_vec();
    let mut schedule = SolveStopSchedule::new(&model.problem, 0.0, 1.0);
    let before = schedule.clone();
    let result = runtime.next_runtime_event_stop(&y, &p, &mut schedule, 0.0, 1.0);
    assert!(
        matches!(result, Err(RuntimeSolveError::SourceFault { span: Some(span), .. }) if span == deadline_span()),
        "{result:?}"
    );
    assert!(schedule.bit_eq(&before));
    assert_eq!(y, model.initial_y.as_ref());
    assert_eq!(p, model.parameters.as_ref());
    assert_eq!(calls.get(), 1);
    let result = runtime.current_dynamic_time_event_stop(&y, &p, 0.25);
    assert!(
        matches!(result, Err(RuntimeSolveError::SourceFault { span: Some(span), .. }) if span == deadline_span()),
        "{result:?}"
    );
    assert_eq!(calls.get(), 2);
}

#[test]
fn deadline_scheduling_coincident_cluster_preserves_reached_instant_and_horizon() {
    for terminal in [false, true] {
        let target = if terminal { 0.009_f64 } else { 0.02 };
        let deadline = 0.009_f64.next_up();
        let mut model = deadline_model(deadline);
        model.problem.events.has_terminal_event = terminal;
        model.problem.clocks = solve::SolveClockPartition {
            periodic_event_schedules: vec![
                solve::PeriodicEventSchedule::new(
                    rumoca_core::ClockLattice::from_seconds(0.001, 0.0).unwrap(),
                )
                .unwrap(),
            ],
            activation_parameter_indices: vec![1],
        };
        let (runtime, calls) = compiled_runtime(&model, false);
        let mut schedule = SolveStopSchedule::new(&model.problem, 0.0, target);
        let reference = SolveRuntime::new_fixture(&model).unwrap();
        let mut reference_schedule = schedule.clone();
        let actual = runtime
            .next_runtime_event_stop(
                &model.initial_y,
                &model.parameters,
                &mut schedule,
                0.008,
                target,
            )
            .unwrap();
        let expected = reference
            .next_runtime_event_stop(
                &model.initial_y,
                &model.parameters,
                &mut reference_schedule,
                0.008,
                target,
            )
            .unwrap();
        assert_eq!(actual.0.to_bits(), deadline.min(target).to_bits());
        assert_eq!(actual, expected);
        assert!(
            actual
                .1
                .is_some_and(|event| event.observe_right_limit && event.terminal == terminal)
        );
        assert!(schedule.bit_eq(&reference_schedule));
        assert_eq!(calls.get(), 1);
    }
}

#[cfg(feature = "native-wasm")]
#[test]
fn deadline_scheduling_native_profile_preserves_empty_rows_and_refuses_nonempty_rows() {
    let component = compact_component();
    let root = component
        .runtime_view()
        .retain_assertion_root()
        .resolved_periodic_schedules_at(0.0)
        .unwrap();
    let host =
        rumoca_exec_wasm::compile_retained_native_program_wasm(root.native_schedule().unwrap())
            .unwrap()
            .into_host();
    let native =
        SolveRuntime::new_with_native_owner(SolveRuntimeModel::retained(root), host, 0.0).unwrap();
    assert_event_free_schedule(&native);
    assert!(native.scalar_execution().is_err());
    // Deliberately contradictory test fixture: scalar deadline source with a
    // native-only inventory. Production admission never constructs this state.
    let model = deadline_model(0.25);
    let mut runtime = SolveRuntime::new_fixture(&model).unwrap();
    runtime.execution = native.execution;
    assert!(matches!(
        runtime.current_dynamic_time_event_stop(&model.initial_y, &model.parameters, 0.25),
        Err(RuntimeSolveError::UnsupportedModel { .. })
    ));
    let mut schedule = SolveStopSchedule::new(&model.problem, 0.0, 1.0);
    let before = schedule.clone();
    assert!(matches!(
        runtime.next_runtime_event_stop(
            &model.initial_y,
            &model.parameters,
            &mut schedule,
            0.0,
            1.0
        ),
        Err(RuntimeSolveError::UnsupportedModel { .. })
    ));
    assert!(schedule.bit_eq(&before));
}

#[test]
fn deadline_scheduling_zero_output_program_is_refused_by_construction() {
    let result = solve::ScalarProgramBlock::with_program_spans(
        vec![vec![solve::LinearOp::LoadP { dst: 0, index: 0 }]],
        vec![deadline_span()],
    );
    assert!(matches!(
        result,
        Err(solve::SolveProblemShapeContractError::ScalarProgramMissingOutput {
            span: Some(span), ..
        }) if span == deadline_span()
    ));
}
