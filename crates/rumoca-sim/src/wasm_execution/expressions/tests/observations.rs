use super::*;
use rumoca_ir_solve::{RetainedAssertionSource, SolveExecutionSource};
use rumoca_solver::CompiledSolveEventObservation;

#[test]
fn emitted_warning_retains_the_first_actual_nested_frame_and_clears_old_messages() {
    let text = r#"
function child
  input Real q;
  input Integer index;
  output Real y;
algorithm
  assert(q < 0, "frame " + String(q + index, minimumLength=5,
    leftJustified=false, significantDigits=3), level=AssertionLevel.warning);
  y := q + index;
end child;
function accumulateWarnings
  input Real q;
  output Real y;
algorithm
  y := 0;
  for i in 1:2 loop
    for j in 1:2 loop
      y := y + child(q, 10*i+j);
    end for;
  end for;
end accumulateWarnings;
model PortableWarnings
  Real x(start=0, fixed=true);
equation
  der(x)=accumulateWarnings(time);
end PortableWarnings;
"#;
    let mut compiler = rumoca_compile::Session::new(Default::default());
    compiler.update_document("portable_warning_frames.mo", text);
    let compiled = compiler.compile_model("PortableWarnings").unwrap();
    let lowered = crate::lower_correlated_for_simulation_with_overrides(
        &compiled.dae,
        &rumoca_solver::SimOptions::default(),
    )
    .unwrap();
    let source = SolveExecutionSource::new(lowered.model().clone());
    let model = source.model();
    let backend = WasmExecutionBackend {
        layout: model.problem.layout.clone(),
        calls: model.pure_calls.clone(),
    };
    let mut messages = Vec::new();
    let mut compiled_count = 0;
    for (kind, block) in [
        (
            RetainedAssertionSource::RootConditions,
            &model.problem.events.root_conditions,
        ),
        (
            RetainedAssertionSource::ActionConditions,
            &model.problem.events.action_conditions,
        ),
    ] {
        for index in 0..block.row_count() {
            let program = source.event_program(kind, index).unwrap();
            if !program.has_observations() {
                continue;
            }
            let outputs = ScalarProgramBlock::program_output_count(program.program());
            let code = backend.compile_event_observation(program).unwrap();
            compiled_count += 1;
            let y = vec![0.0; model.problem.layout.y_scalars()];
            let p = vec![0.0; model.problem.layout.p_scalars()];
            let mut out = vec![0.0; outputs];
            code.call(&y, &p, 0.0, &[], &mut out).unwrap();
            for action in &model.problem.events.actions {
                messages.extend(code.observed_message(action));
                assert!(code.observed_message(&action.clone()).is_none());
            }
            out.fill(19.0);
            let invalid_y = vec![0.0; y.len() + 1];
            assert!(code.call(&invalid_y, &p, 0.0, &[], &mut out).is_err());
            assert!(out.iter().all(|value| *value == 19.0));
            assert!(
                model
                    .problem
                    .events
                    .actions
                    .iter()
                    .all(|action| code.observed_message(action).is_none())
            );
            code.call(&y, &p, -1.0, &[], &mut out).unwrap();
            assert!(
                model
                    .problem
                    .events
                    .actions
                    .iter()
                    .all(|action| code.observed_message(action).is_none())
            );
        }
    }
    assert!(compiled_count > 0);
    assert!(!messages.is_empty());
    assert!(messages.iter().all(|message| message == "frame    11"));
}

#[test]
fn emitted_message_option_fault_is_failed_only_atomic_and_keeps_original_span() {
    let mut compiler = rumoca_compile::Session::new(Default::default());
    compiler.update_document(
        "portable_bad_message.mo",
        r#"
function badWarning
  input Real q;
  output Real y;
algorithm
  assert(q < 0, "value=" + String(q, minimumLength=-1), level=AssertionLevel.warning);
  y := q;
end badWarning;
model MessageFault
  Real x(start=0, fixed=true);
equation
  der(x)=badWarning(time);
end MessageFault;
"#,
    );
    let compiled = compiler.compile_model("MessageFault").unwrap();
    let lowered = crate::lower_correlated_for_simulation_with_overrides(
        &compiled.dae,
        &rumoca_solver::SimOptions::default(),
    )
    .unwrap();
    let source = SolveExecutionSource::new(lowered.model().clone());
    let model = source.model();
    let mut checked = 0;
    for index in 0..model.problem.events.action_conditions.row_count() {
        let program = source
            .event_program(RetainedAssertionSource::ActionConditions, index)
            .unwrap();
        if !program.has_observations() {
            continue;
        }
        let span = program
            .invocations()
            .next()
            .unwrap()
            .projections()
            .next()
            .unwrap()
            .action()
            .span;
        let outputs = ScalarProgramBlock::program_output_count(program.program());
        let code = super::super::super::observations::WasmObservations::compile(
            program,
            &model.problem.layout,
            &model.pure_calls,
        )
        .unwrap();
        let y = vec![0.0; model.problem.layout.y_scalars()];
        let p = vec![0.0; model.problem.layout.p_scalars()];
        let mut out = vec![19.0; outputs];
        code.call(&y, &p, -1.0, &[], &mut out).unwrap();
        out.fill(19.0);
        let error = code.call(&y, &p, 0.0, &[], &mut out).unwrap_err();
        assert!(matches!(
            &error,
            rumoca_solver::RuntimeSolveError::SolveIr { .. }
        ));
        assert_eq!(error.source_span(), Some(span));
        assert!(error.to_string().contains("minimumLength"));
        assert_eq!(out, vec![19.0; outputs]);
        assert!(
            model
                .problem
                .events
                .actions
                .iter()
                .all(|action| code.observed_message(action).is_none())
        );
        checked += 1;
    }
    assert!(checked > 0);
}

#[test]
fn wasm_failed_input_guard_clears_previous_source_bound_warning_records() {
    let mut compiler = rumoca_compile::Session::new(Default::default());
    compiler.update_document(
        "wasm-warning-guard.mo",
        r#"
function guardCapture
  input Real q;
  output Real y;
algorithm
  assert(q < 0, "value=" + String(q), level=AssertionLevel.warning);
  y := q;
end guardCapture;
model GuardCapture
  Real x(start=0, fixed=true);
equation
  der(x)=guardCapture(time);
end GuardCapture;
"#,
    );
    let compiled = compiler.compile_model("GuardCapture").unwrap();
    let lowered = crate::lower_correlated_for_simulation_with_overrides(
        &compiled.dae,
        &rumoca_solver::SimOptions::default(),
    )
    .unwrap();
    let source = SolveExecutionSource::new(lowered.model().clone());
    let model = source.model();
    let mut checked = 0;
    for index in 0..model.problem.events.action_conditions.row_count() {
        let program = source
            .event_program(RetainedAssertionSource::ActionConditions, index)
            .unwrap();
        if !program.has_observations() {
            continue;
        }
        let action_index = program
            .invocations()
            .next()
            .unwrap()
            .projections()
            .next()
            .unwrap()
            .action_index();
        let action = &model.problem.events.actions[action_index];
        let outputs = ScalarProgramBlock::program_output_count(program.program());
        let code = super::super::super::observations::WasmObservations::compile(
            program,
            &model.problem.layout,
            &model.pure_calls,
        )
        .unwrap();
        let y = vec![0.0; model.problem.layout.y_scalars()];
        let p = vec![0.0; model.problem.layout.p_scalars()];
        let mut out = vec![19.0; outputs];
        code.call(&y, &p, 0.0, &[], &mut out).unwrap();
        assert_eq!(code.observed_message(action), Some("value=0".into()));
        assert!(code.observed_message(&action.clone()).is_none());
        out.fill(19.0);
        let invalid_y = vec![0.0; y.len() + 1];
        let _error = code.call(&invalid_y, &p, 0.0, &[], &mut out).unwrap_err();
        assert_eq!(
            out,
            vec![19.0; outputs],
            "failed guard preserves ordinary caller outputs"
        );
        assert!(
            code.observed_message(action).is_none(),
            "a failed input guard cannot expose a previous invocation's warning record"
        );
        checked += 1;
    }
    assert!(checked > 0);
}
