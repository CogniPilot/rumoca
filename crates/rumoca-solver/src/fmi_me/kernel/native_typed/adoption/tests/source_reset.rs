//! Source-only reset ownership controls; no production native entry is executed.

use super::*;
use rumoca_exec_wasm::{PreparedNativeStorage, PreparedNativeWasmCode};
use rumoca_ir_solve::{RetainedAssertionRoot, SolveValueKind};

fn code(component: &rumoca_ir_solve::fmi::FmiComponent) -> PreparedNativeWasmCode {
    rumoca_exec_wasm::compile_retained_native_preparation_wasm(
        component
            .runtime_view()
            .retain_assertion_root()
            .native_preparation()
            .unwrap(),
    )
    .unwrap()
}

fn activate(
    storage: PreparedNativeStorage,
    original: &RetainedAssertionRoot,
    start: f64,
) -> NativeTransport {
    match storage.activate(original.resolved_periodic_schedules_at(start).unwrap()) {
        Ok(resource) => resource,
        Err(_) => panic!("the original source activates at the actual requested time"),
    }
}

fn reset(resource: NativeTransport) -> PreparedNativeStorage {
    match resource.reset_to_preparation() {
        Ok(storage) => storage,
        Err(_) => panic!("original-source storage resets to preparation"),
    }
}

fn values(
    storage: &PreparedNativeStorage,
) -> Vec<(usize, Result<SolveValueKind, NativeTransportError>)> {
    storage
        .input_values()
        .map(|(lane, value)| (lane.p_index(), value))
        .collect()
}

#[test]
fn original_defaults_restore_before_reactivation_at_new_actual_anchor() {
    let component = fixture::reset_component();
    let original = component.runtime_view().retain_assertion_root();
    let code = code(&component);
    let mut storage = code.original_storage_from_source().unwrap();
    let original_values = values(&storage);
    assert_eq!(
        original_values,
        [
            (1, Ok(SolveValueKind::Integer(i64::MAX))),
            (2, Ok(SolveValueKind::Boolean(false)))
        ]
    );
    let source = storage.snapshot().unwrap();
    let addresses = (
        storage.coordinates().0.as_ptr(),
        storage.coordinates().1.as_ptr(),
    );
    assert!(std::ptr::eq(storage.inputs(), code.inputs()));
    storage
        .set_typed_input(1, NativeTypedInput::Integer(17))
        .unwrap();
    storage
        .set_typed_input(2, NativeTypedInput::Boolean(true))
        .unwrap();
    storage.set_coordinates(&[13.0], &[7.25, 0.0, 0.0]).unwrap();
    let mut resource = activate(storage, &original, 2.0);
    let old_product = resource.host();
    let continuation = resource.snapshot().unwrap();
    assert_eq!(resource.restore(&continuation), Ok(()));
    let mut storage = reset(resource);
    assert_eq!(values(&storage), original_values);
    assert_eq!(storage.coordinates(), (&[0.0][..], &[2.0, 0.0, 0.0][..]));
    assert_eq!(
        (
            storage.coordinates().0.as_ptr(),
            storage.coordinates().1.as_ptr()
        ),
        addresses
    );
    assert!(std::ptr::eq(storage.inputs(), code.inputs()));
    assert_eq!(storage.restore(&source), Ok(()));
    assert_eq!(
        storage.restore(&continuation),
        Err(NativeTransportError::SnapshotProductUnsupported)
    );
    let mut resource = activate(storage, &original, 5.0);
    assert_eq!(
        resource.product().inputs().root().resolved_start_time(),
        Some(5.0)
    );
    assert_eq!(
        old_product.product().inputs().root().resolved_start_time(),
        Some(2.0)
    );
    assert_eq!(
        resource.restore(&continuation),
        Err(NativeTransportError::SnapshotProductUnsupported)
    );
    assert_eq!(resource.coordinates(), (&[0.0][..], &[2.0, 0.0, 0.0][..]));
    let storage = reset(resource);
    assert_eq!(values(&storage), original_values);
}

#[test]
fn original_missing_presence_is_restored_without_inferring_boolean_default() {
    let component = fixture::missing_boolean_component();
    let original = component.runtime_view().retain_assertion_root();
    let code = code(&component);
    let mut storage = code.original_storage_from_source().unwrap();
    let before = values(&storage);
    assert_eq!(
        before[1],
        (2, Err(NativeTransportError::MissingInput { p_index: 2 }))
    );
    let snapshot = storage.snapshot().unwrap();
    storage
        .set_typed_input(2, NativeTypedInput::Boolean(false))
        .unwrap();
    storage.set_coordinates(&[21.0], &[8.5, 0.0, 0.0]).unwrap();
    let mut storage = reset(activate(storage, &original, 2.0));
    assert_eq!(values(&storage), before);
    assert_eq!(storage.restore(&snapshot), Ok(()));
    assert_eq!(storage.coordinates(), (&[0.0][..], &[2.0, 0.0, 0.0][..]));
    let mut resource = activate(storage, &original, 5.0);
    assert!(matches!(
        resource.enter(),
        Err(NativeTransportError::MissingInput { p_index: 2 })
    ));
}

#[test]
fn raw_coordinates_and_snapshots_cannot_mint_original_source_reset_authority() {
    let component = fixture::reset_component();
    let original = component.runtime_view().retain_assertion_root();
    let code = code(&component);
    let mut storage = code
        .storage_from_source(&[13.0], &[7.25, 0.0, 0.0])
        .unwrap();
    storage.set_float64_input_batches(&[(0, &[7.25])]).unwrap();
    storage
        .set_typed_input(1, NativeTypedInput::Integer(17))
        .unwrap();
    let mut resource = activate(storage, &original, 2.0);
    let saved = resource.snapshot().unwrap();
    resource.restore(&saved).unwrap();
    resource = match resource.reset_to_preparation() {
        Ok(_) => panic!("caller coordinates cannot supply original default authority"),
        Err((resource, error)) => {
            assert_eq!(error, NativeTransportError::SourcePristineUnavailable);
            *resource
        }
    };
    assert_eq!(resource.coordinates(), (&[13.0][..], &[7.25, 0.0, 0.0][..]));
    assert_eq!(resource.restore(&saved), Ok(()));
    assert_eq!(
        resource
            .input_values()
            .find(|(lane, _)| lane.p_index() == 1)
            .unwrap()
            .1,
        Ok(SolveValueKind::Integer(17))
    );
    let resolved_code = rumoca_exec_wasm::compile_retained_native_preparation_wasm(
        original
            .resolved_periodic_schedules_at(2.0)
            .unwrap()
            .native_preparation()
            .unwrap(),
    )
    .unwrap();
    assert!(matches!(
        resolved_code.original_storage_from_source(),
        Err(NativeTransportError::SourcePristineUnavailable)
    ));
}

#[test]
fn busy_and_foreign_source_refusals_preserve_the_same_original_resource() {
    let component = fixture::reset_component();
    let foreign = fixture::reset_component();
    let original = component.runtime_view().retain_assertion_root();
    let code = code(&component);
    let mut storage = code.original_storage_from_source().unwrap();
    storage
        .set_typed_input(1, NativeTypedInput::Integer(17))
        .unwrap();
    let saved = storage.snapshot().unwrap();
    storage = match storage.activate(
        foreign
            .runtime_view()
            .retain_assertion_root()
            .resolved_periodic_schedules_at(2.0)
            .unwrap(),
    ) {
        Ok(_) => panic!("equal foreign source cannot activate this code"),
        Err((storage, _)) => *storage,
    };
    storage.restore(&saved).unwrap();
    let before = values(&storage);
    let mut resource = activate(storage, &original, 2.0);
    let mut other = activate(code.original_storage_from_source().unwrap(), &original, 2.0);
    let lease = other.enter().unwrap();
    resource = match resource.reset_to_preparation() {
        Ok(_) => panic!("an active shared arena cannot reset another resource"),
        Err((resource, error)) => {
            assert_eq!(error, NativeTransportError::Busy);
            *resource
        }
    };
    drop(lease);
    assert_eq!(
        resource
            .input_values()
            .map(|(lane, value)| (lane.p_index(), value))
            .collect::<Vec<_>>(),
        before
    );
    assert_eq!(resource.restore(&saved), Ok(()));
    let storage = reset(resource);
    assert_eq!(
        values(&storage)[0],
        (1, Ok(SolveValueKind::Integer(i64::MAX)))
    );
}
