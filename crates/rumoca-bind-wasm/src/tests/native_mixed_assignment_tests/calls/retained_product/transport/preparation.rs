//! Preparation is reusable source authority, never unresolved execution.

use super::*;

mod storage;

#[test]
fn retained_native_preparation_shares_code_and_gate_across_resolved_instances() {
    let _lock = session_test_guard();
    let mut prepared = None;
    with_fixture_component(|component| {
        let original = component.runtime_view().retain_assertion_root();
        let inputs = original.native_preparation().unwrap();
        assert!(matches!(
            inputs.bind_resolved(original.clone()),
            Err(rumoca_ir_solve::RetainedNativeScheduleError::UnresolvedStartTime)
        ));
        let code = rumoca_exec_wasm::compile_retained_native_preparation_wasm(inputs).unwrap();
        assert!(code.bind_resolved(original.clone()).is_err());
        let first_root = original.resolved_periodic_schedules_at(2.0).unwrap();
        let second_root = original.resolved_periodic_schedules_at(5.0).unwrap();
        let first_host = code.bind_resolved(first_root.clone()).unwrap().into_host();
        let second_host = code.bind_resolved(second_root.clone()).unwrap().into_host();
        assert!(std::ptr::eq(
            first_host.product().compiled(),
            code.compiled()
        ));
        assert!(std::ptr::eq(
            second_host.product().compiled(),
            code.compiled()
        ));
        for root in [&first_root, &second_root] {
            assert!(std::ptr::eq(root.model(), code.inputs().root().model()));
            assert!(std::ptr::eq(
                root.metadata(),
                code.inputs().root().metadata()
            ));
            assert!(std::ptr::eq(
                root.native_schedule().unwrap().schedule(),
                code.inputs().schedule()
            ));
        }
        let artifact = execution_description(first_host.product());
        let mut execution = CallExecution::uninitialized(&artifact);
        let p = original.model().parameters.to_vec();
        let y = vec![13.0; original.model().problem.layout.y_scalars()];
        let mut first = first_host.transport(&y, &p).unwrap();
        let mut second = second_host.transport(&y, &p).unwrap();
        let integer = slot(&artifact, "k", "P");
        let boolean = slot(&artifact, "first", "P");
        let samples = slot(&artifact, "samples", "P");
        first
            .set_float64_input_batches(&[(samples, &[5.0])])
            .unwrap();
        second
            .set_float64_input_batches(&[(samples, &[5.0])])
            .unwrap();
        first
            .set_typed_input(integer, NativeTypedInput::Integer(i64::MAX))
            .unwrap();
        first
            .set_typed_input(boolean, NativeTypedInput::Boolean(true))
            .unwrap();
        second
            .set_typed_input(integer, NativeTypedInput::Integer(1))
            .unwrap();
        second
            .set_typed_input(boolean, NativeTypedInput::Boolean(false))
            .unwrap();
        drop(component);
        let result = slot(&artifact, "result", "Y");
        for _ in 0..2 {
            exercise_resource(
                &mut execution,
                &mut first,
                &mut second,
                1.0,
                result,
                integer,
            );
            exercise_resource(
                &mut execution,
                &mut second,
                &mut first,
                2.0,
                result,
                integer,
            );
        }
        prepared = Some(code);
        Ok(String::new())
    })
    .unwrap();
    refuse_foreign_preparation(&prepared.unwrap());
}

fn refuse_foreign_preparation(prepared: &rumoca_exec_wasm::PreparedNativeWasmCode) {
    // Equal source bytes in an independent component do not share authority.
    with_fixture_component(|component| {
        let foreign = component
            .runtime_view()
            .retain_assertion_root()
            .resolved_periodic_schedules_at(2.0)
            .unwrap();
        assert!(matches!(
            prepared.inputs().bind_resolved(foreign.clone()),
            Err(rumoca_ir_solve::RetainedNativeScheduleError::ForeignPreparation)
        ));
        assert!(prepared.bind_resolved(foreign).is_err());
        Ok(String::new())
    })
    .unwrap();
}
