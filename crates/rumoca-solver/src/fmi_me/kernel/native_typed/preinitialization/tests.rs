//! Ownership controls use the private CPU constructor, never native execution.

use super::super::owner_fixture as fixture;
use super::*;
mod common_owner;
mod parameters;
mod real_presence;

fn pending(component: &rumoca_ir_solve::fmi::FmiComponent) -> MePendingNativeComponent {
    let root = component.runtime_view().retain_assertion_root();
    let code = rumoca_exec_wasm::compile_retained_native_preparation_wasm(
        root.native_preparation().unwrap(),
    )
    .unwrap();
    MePendingNativeComponent::instantiate(MeModelSource::new(component), &code).unwrap()
}

fn config(start: f64) -> MeInstanceConfig {
    MeInstanceConfig::new("pending-native", 1e-6, start, start + 1.0).unwrap()
}

fn write_inputs(pending: &mut MePendingNativeComponent) {
    pending.set_native_integer(&[3], &[i64::MAX]).unwrap();
    pending.set_boolean(&[4], &[false]).unwrap();
    pending.set_native_float64(&[2], &[7.25]).unwrap();
}

fn assert_inputs(pending: &MePendingNativeComponent) {
    let mut integer = [0];
    let mut boolean = [true];
    let mut real = [0.0];
    pending.get_native_integer(&[3], &mut integer).unwrap();
    pending.get_boolean(&[4], &mut boolean).unwrap();
    pending.get_native_float64(&[2], &mut real).unwrap();
    assert_eq!((integer, boolean, real), ([i64::MAX], [false], [7.25]));
}

#[test]
fn pending_storage_moves_into_original_initialization_with_same_buffers_and_brand() {
    let component = fixture::component();
    let mut pending = pending(&component);
    assert_eq!(
        pending
            .model
            .retained_assertion_root()
            .unwrap()
            .resolved_start_time(),
        None
    );
    write_inputs(&mut pending);
    let saved = pending.snapshot().unwrap();
    let addresses = (
        pending.coordinates().0.as_ptr(),
        pending.coordinates().1.as_ptr(),
    );
    assert!(pending.set_native_integer(&[4], &[17]).is_err());
    assert!(pending.set_native_float64(&[2], &[]).is_err());
    assert_inputs(&pending);
    let mut kernel = match pending.enter_initialization_mode_owned(&config(2.0)) {
        Ok(kernel) => kernel,
        Err(_) => panic!("the original storage adopts at its actual startTime"),
    };
    drop(component);
    assert_eq!(kernel.lifecycle.state(), MeState::InitializationMode);
    let storage = kernel.native_transport.as_ref().unwrap();
    assert_eq!(
        (
            storage.borrow().coordinates().0.as_ptr(),
            storage.borrow().coordinates().1.as_ptr()
        ),
        addresses
    );
    assert_eq!(storage.borrow_mut().restore(&saved.storage), Ok(()));
    assert_eq!(kernel.params, [7.25, 0.0, 0.0]);
    let mut integer = [0];
    kernel.get_native_integer(&[3], &mut integer).unwrap();
    assert_eq!(integer, [i64::MAX]);
    let pristine = kernel.fmu_state().unwrap();
    kernel.set_boolean(&[4], &[true]).unwrap();
    kernel.reset_to_fmu_state(&pristine).unwrap();
    let mut boolean = [true];
    kernel.get_boolean(&[4], &mut boolean).unwrap();
    assert_eq!(boolean, [false]);
}

#[test]
fn entering_initialization_mode_follows_the_engine_admission_of_the_resource() {
    let component = fixture::component();
    let mut resource = pending(&component);
    write_inputs(&mut resource);
    let outcome = resource.enter_initialization_mode(&config(2.0));
    assert_eq!(outcome.is_ok(), rumoca_exec_wasm::HOST_ENGINE_ENTRY);
    match outcome {
        Ok(kernel) => assert_eq!(kernel.lifecycle.state(), MeState::InitializationMode),
        Err((returned, error)) => {
            assert!(matches!(error, MeError::Contract { .. }));
            assert_inputs(&returned);
            assert_eq!(
                returned.facts.as_ref().unwrap().lifecycle().state(),
                MeState::Instantiated
            );
        }
    }
}

#[test]
fn failed_adoption_retains_original_anchor_and_preinit_writes_for_same_time_retry() {
    let component = fixture::component();
    let mut resource = pending(&component);
    write_inputs(&mut resource);
    resource.activate(&config(2.0)).unwrap();
    let saved = resource.snapshot().unwrap();
    let addresses = (
        resource.coordinates().0.as_ptr(),
        resource.coordinates().1.as_ptr(),
    );
    for start in [5.0, f64::from_bits(2.0_f64.to_bits() + 1)] {
        resource = match resource.enter_initialization_mode_owned(&config(start)) {
            Ok(_) => panic!("a resolved owner cannot adopt at another anchor"),
            Err((pending, error)) => {
                assert!(matches!(error, MeError::UnsupportedModel { .. }));
                *pending
            }
        };
        let Some(PendingNativeResource::Resolved(transport)) = resource.resource.as_ref() else {
            panic!("the actual resolved owner survives refusal")
        };
        assert_eq!(
            transport.product().inputs().root().resolved_start_time(),
            Some(2.0)
        );
        assert_eq!(
            resource.facts.as_ref().unwrap().lifecycle().state(),
            MeState::Instantiated
        );
        assert_eq!(
            (
                resource.coordinates().0.as_ptr(),
                resource.coordinates().1.as_ptr()
            ),
            addresses
        );
        resource.restore(&saved).unwrap();
        assert_inputs(&resource);
    }
    assert!(
        resource
            .enter_initialization_mode_owned(&config(2.0))
            .is_ok()
    );
}

#[test]
fn pending_missing_presence_and_foreign_checkpoint_refusal_are_atomic() {
    let component = fixture::missing_boolean_component();
    let mut resource = pending(&component);
    let saved = resource.snapshot().unwrap();
    let foreign = pending(&component).snapshot().unwrap();
    assert!(resource.restore(&foreign).is_err());
    let mut boolean = [true];
    assert!(resource.get_boolean(&[4], &mut boolean).is_err());
    assert_eq!(boolean, [true]);
    resource.set_boolean(&[4], &[false]).unwrap();
    resource.restore(&saved).unwrap();
    assert!(resource.get_boolean(&[4], &mut boolean).is_err());
    assert_eq!(boolean, [true]);
    assert!(matches!(
        resource.resource.as_ref(),
        Some(PendingNativeResource::Prepared(_))
    ));
}
