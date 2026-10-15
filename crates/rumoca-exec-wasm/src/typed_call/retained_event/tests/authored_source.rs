use super::*;

fn compiled_warning() -> (CompiledRetainedEventWasm, *const solve::SolveEventAction) {
    let mut session = rumoca_compile::Session::default();
    session
        .add_document(
            "retained-warning.mo",
            r#"
function warningValue
  input Real q;
  output Real y;
algorithm
  assert(q < 0, "value=" + String(q), level=AssertionLevel.warning);
  y := q + 1;
end warningValue;
model RetainedWarning
  Real x(start=0, fixed=true);
equation
  der(x) = warningValue(time);
end RetainedWarning;
"#,
        )
        .unwrap();
    let compiled = session.compile_model("RetainedWarning").unwrap();
    let model = rumoca_phase_solve::lower_solve_model_for_profile(
        &compiled.dae,
        &std::collections::HashMap::new(),
        Default::default(),
        solve::RefreshProfile::Scalar,
        |_| {},
    )
    .unwrap()
    .into_model();
    let source = solve::SolveExecutionSource::new(model);
    let program = (0..source.model().problem.events.action_conditions.row_count())
        .filter_map(|index| {
            source.event_program(solve::RetainedAssertionSource::ActionConditions, index)
        })
        .find(solve::RetainedEventProgram::has_observations)
        .unwrap();
    let original_action = program
        .invocations()
        .next()
        .unwrap()
        .projections()
        .next()
        .unwrap()
        .action() as *const _;
    (
        CompiledRetainedEventWasm::compile(program).unwrap(),
        original_action,
    )
    // The compiler, DAE, lowering and caller's execution-source handles all drop here.
}

#[test]
fn retained_event_owns_its_original_action_and_recovers_after_a_failed_call() {
    let (code, original_action) = compiled_warning();
    // Public source access supplies the exact retained interface after every
    // caller-owned source handle has gone away.
    let source = code.source();
    let model = source.source().model();
    let y = vec![0.0; model.problem.layout.y_scalars()];
    let p = vec![0.0; model.problem.layout.p_scalars()];
    let mut out = vec![19.0; solve::ScalarProgramBlock::program_output_count(source.program())];
    assert!(!out.is_empty());
    let invocation = source.invocations().next().unwrap();
    let projection = invocation.projections().next().unwrap();
    let action_index = projection.action_index();
    let expected_span = projection.action().span;
    assert!(std::ptr::eq(projection.action(), original_action));
    assert!(std::ptr::eq(
        &model.problem.events.actions[action_index],
        original_action
    ));

    let mut messages = Vec::new();
    for time in [0.0_f64, 1.0] {
        code.call(&y, &p, time, &mut out, |action, captures, span, owner| {
            assert!(std::ptr::eq(action.action(), original_action));
            assert_eq!(action.action_index(), action_index);
            assert_eq!(span, expected_span);
            assert!(!span.is_dummy());
            assert_eq!(captures.len(), 1);
            assert_eq!(captures[0].1.bits(), &[time.to_bits()]);
            let captures = captures
                .iter()
                .map(|(output, cells)| {
                    let values = cells
                        .bits()
                        .iter()
                        .copied()
                        .map(solve::SolveValueKind::Real64)
                        .collect();
                    (
                        *output,
                        rumoca_eval_solve::TypedValue::construct(
                            cells.value_type().clone(),
                            values,
                        )
                        .unwrap(),
                    )
                })
                .collect::<Vec<_>>();
            messages.push(
                rumoca_eval_solve::render_retained_message(action, &captures, span, owner.id())
                    .unwrap(),
            );
            Ok(())
        })
        .unwrap();
    }
    assert_eq!(messages, ["value=0", "value=1"]);
    out.fill(19.0);
    let invalid_y = vec![0.0; y.len() + 1];
    let mut reports = 0;
    let error = code
        .call(&invalid_y, &p, 0.0, &mut out, |_, _, _, _| {
            reports += 1;
            Ok(())
        })
        .unwrap_err();
    assert!(matches!(error, WasmCompileError::Input(_)));
    assert_eq!(out, vec![19.0; out.len()]);
    assert_eq!(reports, 0);
    code.call(&y, &p, -1.0, &mut out, |_, _, _, _| {
        reports += 1;
        Ok(())
    })
    .unwrap();
    assert_eq!(
        reports, 0,
        "an inactive assertion cannot replay a prior capture"
    );
    assert!(out.iter().all(|value| *value != 19.0));
}
