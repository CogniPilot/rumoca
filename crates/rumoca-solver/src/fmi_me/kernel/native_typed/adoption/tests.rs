//! Owner/lifecycle controls; native-host construction does not grant engine success.

mod source_reset;
use super::super::owner_fixture as fixture;

use super::*;
use rumoca_exec_wasm::{NativeTransportError, NativeTypedInput};
use rumoca_ir_solve::SolveValueKind;

fn prepared_storage(
    component: &rumoca_ir_solve::fmi::FmiComponent,
) -> rumoca_exec_wasm::PreparedNativeStorage {
    let original = component.runtime_view().retain_assertion_root();
    let code = rumoca_exec_wasm::compile_retained_native_preparation_wasm(
        original.native_preparation().unwrap(),
    )
    .unwrap();
    code.storage_from_source(&original.model().initial_y, &original.model().parameters)
        .unwrap()
}

fn transport(component: &rumoca_ir_solve::fmi::FmiComponent, start: f64) -> NativeTransport {
    let original = component.runtime_view().retain_assertion_root();
    let mut prepared = prepared_storage(component);
    prepared
        .set_typed_input(1, NativeTypedInput::Integer(i64::MAX))
        .unwrap();
    prepared
        .set_typed_input(2, NativeTypedInput::Boolean(false))
        .unwrap();
    prepared
        .set_coordinates(&[13.0], &[7.25, 0.0, 0.0])
        .unwrap();
    match prepared.activate(original.resolved_periodic_schedules_at(start).unwrap()) {
        Ok(transport) => transport,
        Err(_) => panic!("the original resolved resource activates"),
    }
}

#[test]
fn adopted_native_resource_retains_buffers_typed_bits_and_checkpoint_brand() {
    let component = fixture::component();
    let transport = transport(&component, 2.0);
    let snapshot = transport.snapshot().unwrap();
    let host = transport.host();
    assert!(std::ptr::eq(host.product(), transport.product()));
    let (y, p) = transport.coordinates();
    let addresses = (y.as_ptr(), p.as_ptr());
    let config = MeInstanceConfig::new("adopted-native", 1e-6, 2.0, 3.0).unwrap();
    let mut kernel = match SolveMeKernel::adopt_native_resource(
        MeModelSource::new(&component),
        &config,
        transport,
    ) {
        Ok(kernel) => kernel,
        Err(_) => panic!("the actual native resource has matching source and startTime"),
    };
    assert!(!kernel.visible_report_is_materialized());
    assert!(kernel.runtime.scalar_execution().is_err());
    let _ = kernel.continuous_state_count();
    let _ = kernel.event_indicator_count();
    assert!(!kernel.visible_report_is_materialized());
    drop(component);
    let storage = kernel.native_transport.as_ref().unwrap().borrow();
    let (y, p) = storage.coordinates();
    assert_eq!((y.as_ptr(), p.as_ptr()), addresses);
    assert!(std::ptr::eq(storage.product(), host.product()));
    drop(storage);
    assert_eq!(kernel.params, [7.25, 0.0, 0.0]);
    assert_eq!(*kernel.solver_y_guess.borrow(), [13.0]);
    assert_eq!(
        kernel
            .native_transport
            .as_ref()
            .unwrap()
            .borrow_mut()
            .restore(&snapshot),
        Ok(())
    );
    let pristine = kernel.fmu_state().unwrap();
    assert_original_inputs(&kernel);
    assert!(!kernel.visible_report_is_materialized());
    kernel.enter_initialization_mode().unwrap();
    assert_original_inputs(&kernel);
    kernel.set_native_integer(&[3], &[17]).unwrap();
    kernel.set_boolean(&[4], &[true]).unwrap();
    kernel.set_native_float64(&[2], &[8.5]).unwrap();
    let before = kernel.fmu_state().unwrap();
    assert!(kernel.set_native_integer(&[4], &[9]).is_err());
    assert!(kernel.verification_matches_snapshot(&before));
    kernel.reset_to_fmu_state(&pristine).unwrap();
    assert_original_inputs(&kernel);
    assert_eq!(kernel.lifecycle.state(), MeState::Instantiated);
    kernel.enter_initialization_mode().unwrap();
    assert_eq!(kernel.lifecycle.state(), MeState::InitializationMode);
    kernel.exit_initialization_mode().unwrap();
    assert_eq!(kernel.lifecycle.state(), MeState::EventMode);
    assert_original_inputs(&kernel);
    kernel.reset_to_fmu_state(&pristine).unwrap();
    assert_original_inputs(&kernel);
    assert_eq!(kernel.lifecycle.state(), MeState::Instantiated);
}

fn assert_original_inputs(kernel: &SolveMeKernel) {
    let mut integer = [0];
    let mut boolean = [true];
    let mut real = [0.0];
    kernel.get_native_integer(&[3], &mut integer).unwrap();
    kernel.get_boolean(&[4], &mut boolean).unwrap();
    kernel.get_native_float64(&[2], &mut real).unwrap();
    assert_eq!(integer, [i64::MAX]);
    assert_eq!(boolean, [false]);
    assert_eq!(real, [7.25]);
}

#[test]
fn native_adoption_refuses_wrong_anchor_and_foreign_source_without_resource_writes() {
    let component = fixture::component();
    let foreign = fixture::component();
    let mut resource = transport(&component, 2.0);
    for (source, start) in [
        (MeModelSource::new(&component), 5.0),
        (
            MeModelSource::new(&component),
            f64::from_bits(2.0_f64.to_bits() + 1),
        ),
        (MeModelSource::new(&foreign), 2.0),
    ] {
        let product = resource.host();
        let before = resource
            .input_values()
            .map(|(_, value)| value.unwrap())
            .collect::<Vec<_>>();
        let snapshot = resource.snapshot().unwrap();
        let addresses = (
            resource.coordinates().0.as_ptr(),
            resource.coordinates().1.as_ptr(),
        );
        let coordinate_bits = (
            resource
                .coordinates()
                .0
                .iter()
                .map(|value| value.to_bits())
                .collect::<Vec<_>>(),
            resource
                .coordinates()
                .1
                .iter()
                .map(|value| value.to_bits())
                .collect::<Vec<_>>(),
        );
        let config = MeInstanceConfig::new("refused-native", 1e-6, start, start + 1.0).unwrap();
        resource = match SolveMeKernel::adopt_native_resource(source, &config, resource) {
            Ok(_) => panic!("wrong source or anchor cannot adopt storage"),
            Err((storage, error)) => {
                if start == 2.0 {
                    assert!(matches!(error, MeError::Contract { reason }
                        if reason == "native adoption source differs from its actual resource"));
                } else {
                    assert!(matches!(error, MeError::UnsupportedModel { reason }
                        if reason == "native execution startTime differs from its original resolved anchors"));
                }
                *storage
            }
        };
        assert_eq!(
            (
                resource.coordinates().0.as_ptr(),
                resource.coordinates().1.as_ptr()
            ),
            addresses
        );
        assert_eq!(
            resource
                .input_values()
                .map(|(_, value)| value.unwrap())
                .collect::<Vec<_>>(),
            before
        );
        assert_eq!(
            (
                resource
                    .coordinates()
                    .0
                    .iter()
                    .map(|value| value.to_bits())
                    .collect::<Vec<_>>(),
                resource
                    .coordinates()
                    .1
                    .iter()
                    .map(|value| value.to_bits())
                    .collect::<Vec<_>>(),
            ),
            coordinate_bits,
        );
        assert!(std::ptr::eq(product.product(), resource.product()));
        assert_eq!(resource.restore(&snapshot), Ok(()));
        assert!(before.contains(&SolveValueKind::Integer(i64::MAX)));
        assert!(before.contains(&SolveValueKind::Boolean(false)));
    }
    let mut other = transport(&component, 2.0);
    assert_eq!(
        other.restore(&resource.snapshot().unwrap()),
        Err(NativeTransportError::ForeignSnapshot)
    );
}

#[test]
fn native_adoption_preserves_unwritten_input_presence_across_initialization_and_reset() {
    let component = fixture::missing_boolean_component();
    let prepared = prepared_storage(&component);
    let root = component
        .runtime_view()
        .retain_assertion_root()
        .resolved_periodic_schedules_at(2.0)
        .unwrap();
    let transport = match prepared.activate(root) {
        Ok(transport) => transport,
        Err(_) => panic!("the original storage activates"),
    };
    let snapshot = transport.snapshot().unwrap();
    let config = MeInstanceConfig::new("absent-native", 1e-6, 2.0, 3.0).unwrap();
    let mut kernel = match SolveMeKernel::adopt_native_resource(
        MeModelSource::new(&component),
        &config,
        transport,
    ) {
        Ok(kernel) => kernel,
        Err(_) => panic!("an unwritten cell does not invalidate storage ownership"),
    };
    let pristine = kernel.fmu_state().unwrap();
    assert_boolean_absent(&kernel);
    kernel.enter_initialization_mode().unwrap();
    assert_boolean_absent(&kernel);
    kernel.set_boolean(&[4], &[true]).unwrap();
    let mut value = [false];
    kernel.get_boolean(&[4], &mut value).unwrap();
    assert_eq!(value, [true]);
    kernel.reset_to_fmu_state(&pristine).unwrap();
    assert_boolean_absent(&kernel);
    assert_eq!(
        kernel
            .native_transport
            .as_ref()
            .unwrap()
            .borrow_mut()
            .restore(&snapshot),
        Ok(())
    );
    assert_boolean_absent(&kernel);
}

fn assert_boolean_absent(kernel: &SolveMeKernel) {
    let transport = kernel.native_transport.as_ref().unwrap().borrow();
    let (_, value) = transport
        .input_values()
        .find(|(lane, _)| lane.p_index() == 2)
        .unwrap();
    assert_eq!(
        value,
        Err(NativeTransportError::MissingInput { p_index: 2 })
    );
    drop(transport);
    let mut result = [true];
    assert!(kernel.get_boolean(&[4], &mut result).is_err());
    assert_eq!(result, [true]);
}
