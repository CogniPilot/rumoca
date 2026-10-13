//! WIT requests use the actual inventory and entered time, not guest facts.

use super::*;
use rumoca_ir_solve::SolveVariableValueKind;

#[test]
fn entered_time_and_cardinality_preserve_request_order_and_atomic_getters() {
    let component = fixture::reset_component();
    let kernel = entered(&component);
    assert_eq!(
        kernel
            .native_value_count(&[0, 2, 0], SolveVariableValueKind::Real)
            .unwrap(),
        3
    );
    assert!(
        kernel
            .native_value_count(&[0], SolveVariableValueKind::Integer)
            .is_err()
    );
    assert!(
        kernel
            .native_value_count(&[2, 999], SolveVariableValueKind::Real)
            .is_err()
    );
    let mut values = [-9.0; 3];
    kernel.get_native_float64(&[0, 2, 0], &mut values).unwrap();
    assert_eq!(values, [2.0, 7.25, 2.0]);
    assert!(
        kernel
            .get_native_float64(&[0, 2, 999], &mut values)
            .is_err()
    );
    assert_eq!(values, [2.0, 7.25, 2.0]);
    assert!(kernel.check_native_float64_inputs(&[0], &[5.0]).is_err());
}

#[test]
fn unresolved_preparation_has_no_time_and_reset_reanchors_original_owner() {
    let component = fixture::reset_component();
    let prepared = pending(&component);
    assert!(
        prepared
            .native_value_count(&[0], SolveVariableValueKind::Real)
            .is_err()
    );
    assert_eq!(
        prepared
            .native_value_count(&[2, 2], SolveVariableValueKind::Real)
            .unwrap(),
        2
    );
    let mut value = [-9.0];
    assert!(prepared.get_native_float64(&[0], &mut value).is_err());
    assert_eq!(value, [-9.0]);
    let owner = MeKernelCell::new(entered(&component));
    owner.reset().unwrap();
    assert!(
        owner
            .native_value_count(&[0], SolveVariableValueKind::Real)
            .is_err()
    );
    owner.activate_for_control(&config(5.0)).unwrap();
    owner
        .borrow()
        .unwrap()
        .get_native_float64(&[0], &mut value)
        .unwrap();
    assert_eq!(value, [5.0]);
}

#[test]
fn initialization_options_and_dynamic_name_are_actual_entered_configuration() {
    let component = fixture::reset_component();
    let owner = MeKernelCell::new(entered(&component));
    let actual = owner.initialization_configuration().unwrap();
    assert_eq!(actual.start_time(), 2.0);
    assert_eq!(actual.stop_time(), Some(3.0));
    let options = crate::fmi_me::driver::fmi_session_options(
        &rumoca_ir_solve::fmi::RootLocationPlan::STANDARD,
        &actual,
    )
    .unwrap();
    assert_eq!(options.start_time(), 2.0);
    assert_eq!(options.stop_time(), Some(3.0));
    assert_eq!(options.relative_tolerance(), actual.relative_tolerance());
    assert!(!options.records_trace());
    owner.reset().unwrap();
    assert!(owner.initialization_configuration().is_err());
    let name = String::from("importer-owned-name");
    let config = MeInstanceConfig::open_ended(name, 1e-6, 5.0).unwrap();
    assert_eq!(config.instance_name(), "importer-owned-name");
    assert_eq!(config.stop_time(), None);
}

#[test]
fn termination_and_reset_apply_to_the_actual_prepared_or_entered_owner() {
    let component = fixture::reset_component();
    let mut prepared = pending(&component);
    prepared.set_native_integer(&[3], &[17]).unwrap();
    prepared.terminate().unwrap();
    assert!(prepared.terminate().is_err());
    let mut integer = [-1];
    assert!(prepared.get_native_integer(&[3], &mut integer).is_err());
    assert_eq!(integer, [-1]);
    assert!(prepared.set_native_integer(&[3], &[23]).is_err());
    assert!(
        prepared
            .native_value_count(&[2], SolveVariableValueKind::Real)
            .is_err()
    );
    prepared.reset().unwrap();
    prepared.get_native_integer(&[3], &mut integer).unwrap();
    assert_eq!(integer, [i64::MAX]);
    let owner = MeKernelCell::new(entered(&component));
    owner.terminate().unwrap();
    assert!(
        owner
            .borrow()
            .unwrap()
            .get_native_float64(&[0], &mut [0.0])
            .is_err()
    );
    owner.reset().unwrap();
    owner.with_prepared_for_control(assert_original);
    owner.activate_for_control(&config(5.0)).unwrap();
    let mut time = [0.0];
    owner
        .borrow()
        .unwrap()
        .get_native_float64(&[0], &mut time)
        .unwrap();
    assert_eq!(time, [5.0]);
}
