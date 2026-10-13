//! Preparation preserves resource storage but cannot acquire an execution lease.

use super::*;
use rumoca_exec_wasm::PreparedNativeStorage;
use rumoca_ir_solve::{RetainedAssertionRoot, RetainedNativeScheduleError, SolveValueKind};

#[test]
fn retained_native_preinit_writes_survive_refused_and_resolved_activation() {
    let _lock = session_test_guard();
    let mut retained = None;
    with_fixture_component(|component| {
        let original = component.runtime_view().retain_assertion_root();
        let code = rumoca_exec_wasm::compile_retained_native_preparation_wasm(
            original.native_preparation().unwrap(),
        )
        .unwrap();
        let root = original.resolved_periodic_schedules_at(2.0).unwrap();
        let product = code.bind_resolved(root.clone()).unwrap();
        let artifact = execution_description(&product);
        let y = vec![13.0; original.model().problem.layout.y_scalars()];
        let p = original.model().parameters.to_vec();
        assert!(matches!(
            code.storage(&[], &p),
            Err(NativeTransportError::BufferShape)
        ));
        let mut prepared = code.storage(&y, &p).unwrap();
        let missing = prepared.snapshot().unwrap();
        let integer = slot(&artifact, "k", "P");
        let boolean = slot(&artifact, "first", "P");
        prepared
            .set_float64_input_batches(&[(slot(&artifact, "samples", "P"), &[5.0])])
            .unwrap();
        prepared
            .set_typed_input(integer, NativeTypedInput::Integer(i64::MAX))
            .unwrap();
        prepared
            .set_typed_input(boolean, NativeTypedInput::Boolean(true))
            .unwrap();
        let values = exact_values(&prepared);
        assert!(
            prepared
                .set_typed_input_batches(&[
                    (integer, NativeTypedInputBatch::Integer(&[7])),
                    (usize::MAX, NativeTypedInputBatch::Integer(&[9])),
                ])
                .is_err()
        );
        assert_eq!(exact_values(&prepared), values);
        prepared.set_coordinates(&vec![14.0; y.len()], &p).unwrap();
        let accepted = prepared.snapshot().unwrap();
        prepared = refused_storage(
            prepared,
            original,
            RetainedNativeScheduleError::UnresolvedStartTime,
        );
        assert_eq!(exact_values(&prepared), values);
        drop(component);
        retained = Some((prepared, root, code, missing, accepted));
        Ok(String::new())
    })
    .unwrap();
    let (prepared, root, code, missing, accepted) = retained.unwrap();
    let prepared = refuse_foreign_storage(prepared);
    let mut transport = match prepared.activate(root) {
        Ok(transport) => transport,
        Err(_) => panic!("original resolved root must activate retained storage"),
    };
    assert!(std::ptr::eq(
        transport.product().compiled(),
        code.compiled()
    ));
    assert!(std::ptr::eq(
        transport.product().inputs().root().metadata(),
        code.inputs().root().metadata()
    ));
    assert_eq!(transport.restore(&missing), Ok(()));
    assert!(matches!(
        transport.enter(),
        Err(NativeTransportError::MissingInput { .. })
    ));
    assert_eq!(transport.restore(&accepted), Ok(()));
    {
        let lease = transport.enter().unwrap();
        assert!(lease.y().iter().all(|value| *value == 14.0));
        assert_eq!(
            lease.p(),
            code.inputs().root().model().parameters.as_slice()
        );
    }
    exercise_activated_storage(&mut transport, &code, &accepted);
}

fn exercise_activated_storage(
    transport: &mut NativeTransport,
    code: &rumoca_exec_wasm::PreparedNativeWasmCode,
    accepted: &rumoca_exec_wasm::NativeTransportSnapshot,
) {
    let artifact = execution_description(transport.product());
    let mut execution = CallExecution::uninitialized(&artifact);
    let host = code
        .bind_resolved(
            code.inputs()
                .root()
                .resolved_periodic_schedules_at(5.0)
                .unwrap(),
        )
        .unwrap()
        .into_host();
    let mut second = host
        .transport(
            &vec![13.0; code.inputs().root().model().problem.layout.y_scalars()],
            &code.inputs().root().model().parameters,
        )
        .unwrap();
    let integer = slot(&artifact, "k", "P");
    second
        .set_float64_input_batches(&[(slot(&artifact, "samples", "P"), &[5.0])])
        .unwrap();
    second
        .set_typed_input(integer, NativeTypedInput::Integer(1))
        .unwrap();
    second
        .set_typed_input(
            slot(&artifact, "first", "P"),
            NativeTypedInput::Boolean(false),
        )
        .unwrap();
    assert_eq!(
        second.restore(accepted),
        Err(NativeTransportError::ForeignSnapshot)
    );
    for _ in 0..2 {
        exercise_resource(
            &mut execution,
            transport,
            &mut second,
            1.0,
            slot(&artifact, "result", "Y"),
            integer,
        );
        exercise_resource(
            &mut execution,
            &mut second,
            transport,
            2.0,
            slot(&artifact, "result", "Y"),
            integer,
        );
    }
}

fn exact_values(storage: &PreparedNativeStorage) -> Vec<SolveValueKind> {
    storage
        .input_values()
        .map(|(_, value)| value.unwrap())
        .collect()
}

fn refused_storage(
    storage: PreparedNativeStorage,
    root: RetainedAssertionRoot,
    expected: RetainedNativeScheduleError,
) -> PreparedNativeStorage {
    let message = expected.to_string();
    assert_eq!(
        storage.inputs().bind_resolved(root.clone()).err(),
        Some(expected)
    );
    match storage.activate(root) {
        Ok(_) => panic!("unresolved or foreign root cannot activate storage"),
        Err((storage, error)) => {
            assert!(matches!(
                error,
                rumoca_exec_wasm::WasmCompileError::Input(actual) if actual == message
            ));
            *storage
        }
    }
}

fn refuse_foreign_storage(storage: PreparedNativeStorage) -> PreparedNativeStorage {
    let before = exact_values(&storage);
    let mut retained = Some(storage);
    with_fixture_component(|component| {
        let foreign = component
            .runtime_view()
            .retain_assertion_root()
            .resolved_periodic_schedules_at(2.0)
            .unwrap();
        retained = Some(refused_storage(
            retained.take().unwrap(),
            foreign,
            RetainedNativeScheduleError::ForeignPreparation,
        ));
        assert_eq!(exact_values(retained.as_ref().unwrap()), before);
        Ok(String::new())
    })
    .unwrap();
    retained.unwrap()
}

#[test]
fn retained_native_preinit_source_starts_share_exact_owner_and_missing_presence() {
    let _lock = session_test_guard();
    let source = r#"
model ExactPreinit
  input Integer values[2](start={9007199254740993, 9223372036854775807});
  input Boolean enabled[2](start=fill(true, 2));
  output Integer result;
equation
  result = values[1];
end ExactPreinit;
model MissingPreinit
  input Integer value;
  output Integer result;
equation
  result = value;
end MissingPreinit;
"#;
    with_source_component(source, "ExactPreinit", |component| {
        let original = component.runtime_view().retain_assertion_root();
        let code = rumoca_exec_wasm::compile_retained_native_preparation_wasm(
            original.native_preparation().unwrap(),
        )
        .unwrap();
        let prepared = code
            .storage_from_source(&original.model().initial_y, &original.model().parameters)
            .unwrap();
        let expected = exact_values(&prepared);
        assert!(expected.contains(&SolveValueKind::Integer(9_007_199_254_740_993)));
        assert!(expected.contains(&SolveValueKind::Integer(i64::MAX)));
        assert_eq!(
            expected
                .iter()
                .filter(|value| **value == SolveValueKind::Boolean(true))
                .count(),
            2
        );
        let root = original.resolved_periodic_schedules_at(5.0).unwrap();
        let host = code.bind_resolved(root.clone()).unwrap().into_host();
        let normal = host
            .transport_from_source(&original.model().initial_y, &original.model().parameters)
            .unwrap();
        assert_eq!(
            normal
                .input_values()
                .map(|(_, value)| value.unwrap())
                .collect::<Vec<_>>(),
            expected
        );
        let mut transport = match prepared.activate(root) {
            Ok(value) => value,
            Err(_) => panic!("exact original root"),
        };
        assert_eq!(
            transport
                .input_values()
                .map(|(_, value)| value.unwrap())
                .collect::<Vec<_>>(),
            expected
        );
        assert!(transport.enter().is_ok());
        Ok(String::new())
    })
    .unwrap();
    with_source_component(source, "MissingPreinit", |component| {
        let original = component.runtime_view().retain_assertion_root();
        let code = rumoca_exec_wasm::compile_retained_native_preparation_wasm(
            original.native_preparation().unwrap(),
        )
        .unwrap();
        let prepared = code
            .storage_from_source(&original.model().initial_y, &original.model().parameters)
            .unwrap();
        assert!(
            prepared
                .input_values()
                .all(|(_, value)| matches!(value, Err(NativeTransportError::MissingInput { .. })))
        );
        let root = original.resolved_periodic_schedules_at(5.0).unwrap();
        let mut transport = match prepared.activate(root) {
            Ok(value) => value,
            Err(_) => panic!("exact original root"),
        };
        assert!(matches!(
            transport.enter(),
            Err(NativeTransportError::MissingInput { .. })
        ));
        Ok(String::new())
    })
    .unwrap();
}
