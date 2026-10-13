//! Source ownership controls; the private CPU adoption path grants no engine.

use super::*;
use crate::fmi_me::{instance::MeKernelCell, session::MeRetainedComponent};
use std::rc::Rc;
mod input_transaction;
mod wit_projection;

fn entered(component: &rumoca_ir_solve::fmi::FmiComponent) -> SolveMeKernel {
    let mut resource = pending(component);
    resource.set_native_integer(&[3], &[17]).unwrap();
    resource.set_boolean(&[4], &[true]).unwrap();
    resource.set_native_float64(&[2], &[7.25]).unwrap();
    match resource.enter_initialization_mode_owned(&config(2.0)) {
        Ok(kernel) => kernel,
        Err(_) => panic!("the actual pending owner adopts once"),
    }
}

fn addresses(kernel: &SolveMeKernel) -> (*const f64, *const f64) {
    let resource = kernel.native_transport.as_ref().unwrap().borrow();
    let (y, p) = resource.coordinates();
    (y.as_ptr(), p.as_ptr())
}

fn assert_original(pending: &MePendingNativeComponent) {
    let mut integer = [0];
    let mut boolean = [true];
    let mut real = [0.0];
    pending.get_native_integer(&[3], &mut integer).unwrap();
    pending.get_boolean(&[4], &mut boolean).unwrap();
    pending.get_native_float64(&[2], &mut real).unwrap();
    assert_eq!((integer, boolean, real), ([i64::MAX], [false], [2.0]));
}

#[test]
fn retained_instance_reset_restores_source_and_reanchors_same_resource_once() {
    let component = fixture::reset_component();
    let kernel = entered(&component);
    let original_addresses = addresses(&kernel);
    let old = kernel.fmu_state().unwrap();
    let owner = Rc::new(MeKernelCell::new(kernel));
    let observer = Rc::clone(&owner);
    let derivative_owner = Rc::clone(&owner);
    drop(component);
    owner.reset().unwrap();
    assert!(observer.borrow().is_err());
    assert!(derivative_owner.borrow_mut().is_err());
    owner.with_prepared_for_control(|pending| {
        assert_original(pending);
        assert_eq!(
            (
                pending.coordinates().0.as_ptr(),
                pending.coordinates().1.as_ptr()
            ),
            original_addresses,
        );
        assert_eq!(
            pending
                .model
                .retained_assertion_root()
                .unwrap()
                .resolved_start_time(),
            None
        );
    });
    owner.activate_for_control(&config(5.0)).unwrap();
    let mut active = observer.borrow_mut().unwrap();
    assert_eq!(active.initialization_state(), MeState::InitializationMode);
    assert_eq!(active.native_start_time(), Some(5.0));
    assert_eq!(addresses(&active), original_addresses);
    let new = active.fmu_state().unwrap();
    assert!(active.reset_to_fmu_state(&old).is_err());
    assert!(active.verification_matches_snapshot(&new));
    active.reset_to_fmu_state(&new).unwrap();
    assert_eq!(active.params[0], 2.0);
}

#[test]
fn retained_reset_restores_missing_and_failed_activation_preserves_preparation() {
    let component = fixture::missing_boolean_component();
    let owner = MeKernelCell::new(entered(&component));
    owner.reset().unwrap();
    owner.with_prepared_for_control(|pending| {
        assert!(pending.get_boolean(&[4], &mut [true]).is_err());
    });
    assert!(
        owner
            .activate_for_control(&MeInstanceConfig {
                start_time: f64::NAN,
                ..config(5.0)
            })
            .is_err()
    );
    owner.with_prepared_for_control(|pending| {
        assert!(pending.get_boolean(&[4], &mut [false]).is_err());
        assert_eq!(
            pending.facts.as_ref().unwrap().lifecycle().state(),
            MeState::Instantiated
        );
    });
    owner.activate_for_control(&config(5.0)).unwrap();
    assert_eq!(owner.borrow().unwrap().native_start_time(), Some(5.0));
}

#[test]
fn busy_reset_preserves_actual_kernel_and_host_writes() {
    let component = fixture::reset_component();
    let kernel = entered(&component);
    let source_addresses = addresses(&kernel);
    let host = kernel.native_transport.as_ref().unwrap().borrow().host();
    let mut other = {
        let resource = kernel.native_transport.as_ref().unwrap().borrow();
        let (y, p) = resource.coordinates();
        host.transport_from_source(y, p).unwrap()
    };
    let lease = other.enter().unwrap();
    let saved = kernel.fmu_state().unwrap();
    let owner = MeKernelCell::new(kernel);
    assert!(owner.restart(f64::NAN, None).is_err());
    assert!(owner.reset().is_err());
    {
        let active = owner.borrow().unwrap();
        assert_eq!(addresses(&active), source_addresses);
        assert!(active.verification_matches_snapshot(&saved));
    }
    drop(lease);
    owner.reset().unwrap();
    owner.with_prepared_for_control(assert_original);
}

#[test]
fn native_initialization_handoff_preserves_writes_until_explicit_reset() {
    let component = fixture::reset_component();
    let kernel = entered(&component);
    let mut retained = match MeRetainedComponent::from_initializing_native_kernel(kernel) {
        Ok(retained) => retained,
        Err(_) => panic!("the actual Initialization Mode kernel owns the handoff"),
    };
    assert_eq!(retained.lease_count(), 0);
    let mut integer = [0];
    let mut boolean = [false];
    let mut real = [0.0];
    retained.get_native_integer(&[3], &mut integer).unwrap();
    retained.get_boolean(&[4], &mut boolean).unwrap();
    retained.get_native_float64(&[2], &mut real).unwrap();
    assert_eq!((integer, boolean, real), ([17], [true], [7.25]));
    retained.reset().unwrap();
    // The reset resource is the original prepared one, and the engine admits it again.
    {
        let options = crate::fmi_me::driver::live_session_options(
            &rumoca_ir_solve::fmi::RootLocationPlan::STANDARD,
            5.0,
            1e-8,
            1e-8,
            1e-10,
            None,
        )
        .unwrap();
        drop(retained.lease(options).unwrap());
        retained.reset().unwrap();
    }
}

#[test]
fn retained_preparation_uses_no_anchor_and_reset_defaults_ignore_host_writes() {
    let component = fixture::reset_component();
    let root = component.runtime_view().retain_assertion_root();
    let code = rumoca_exec_wasm::compile_retained_native_preparation_wasm(
        root.native_preparation().unwrap(),
    )
    .unwrap();
    let mut retained =
        MeRetainedComponent::prepare_retained_native_wasm(MeModelSource::new(&component), &code)
            .unwrap();
    drop(component);
    retained.set_native_integer(&[3], &[17]).unwrap();
    retained.set_boolean(&[4], &[true]).unwrap();
    retained.set_native_float64(&[2], &[7.25]).unwrap();
    assert!(retained.set_boolean(&[3], &[false]).is_err());
    let mut integer = [0];
    let mut boolean = [false];
    let mut real = [0.0];
    retained.get_native_integer(&[3], &mut integer).unwrap();
    retained.get_boolean(&[4], &mut boolean).unwrap();
    retained.get_native_float64(&[2], &mut real).unwrap();
    assert_eq!((integer, boolean, real), ([17], [true], [7.25]));
    retained.reset().unwrap();
    retained.get_native_integer(&[3], &mut integer).unwrap();
    retained.get_boolean(&[4], &mut boolean).unwrap();
    retained.get_native_float64(&[2], &mut real).unwrap();
    assert_eq!((integer, boolean, real), ([i64::MAX], [false], [2.0]));
    assert_eq!(code.inputs().root().resolved_start_time(), None);
    assert_eq!(retained.lease_count(), 0);
    retained.set_native_integer(&[3], &[23]).unwrap();
    retained.enter_initialization_mode(&config(5.0)).unwrap();
    retained.get_native_integer(&[3], &mut integer).unwrap();
    assert_eq!(
        integer,
        [23],
        "initialization keeps the pre-initialization write"
    );
}

#[test]
fn typed_session_payload_preflight_is_read_only_and_busy_refuses_before_modes_move() {
    let component = fixture::reset_component();
    let kernel = entered(&component);
    let snapshot = kernel.fmu_state().unwrap();
    assert!(kernel.check_native_float64_inputs(&[1], &[0.0]).is_err());
    assert!(
        kernel
            .check_native_float64_inputs(&[2], &[f64::NAN])
            .is_err()
    );
    assert!(kernel.check_native_float64_inputs(&[2], &[]).is_err());
    assert!(
        kernel
            .check_native_integer_inputs(&[3, 4], &[17, 1])
            .is_err()
    );
    assert!(
        kernel
            .check_boolean_inputs(&[4, 999], &[true, false])
            .is_err()
    );
    assert!(kernel.verification_matches_snapshot(&snapshot));
    kernel.check_native_float64_inputs(&[2], &[-0.0]).unwrap();
    kernel
        .check_native_integer_inputs(&[3], &[i64::MIN])
        .unwrap();
    kernel.check_boolean_inputs(&[4], &[false]).unwrap();
    assert!(kernel.verification_matches_snapshot(&snapshot));
    let host = kernel.native_transport.as_ref().unwrap().borrow().host();
    let mut other = {
        let source = kernel.native_transport.as_ref().unwrap().borrow();
        let (y, p) = source.coordinates();
        host.transport_from_source(y, p).unwrap()
    };
    let gate = other.enter().unwrap();
    assert!(kernel.check_native_float64_inputs(&[2], &[7.25]).is_err());
    assert!(kernel.check_native_integer_inputs(&[3], &[17]).is_err());
    assert!(kernel.check_boolean_inputs(&[4], &[true]).is_err());
    assert!(kernel.verification_matches_snapshot(&snapshot));
    drop(gate);
}

#[test]
fn native_active_session_uses_exact_vr_batches_and_consumes_original_reset_owner() {
    let component = fixture::reset_component();
    let kernel = entered(&component);
    let host = kernel.native_transport.as_ref().unwrap().borrow().host();
    let mut other = {
        let resource = kernel.native_transport.as_ref().unwrap().borrow();
        let (y, p) = resource.coordinates();
        host.transport_from_source(y, p).unwrap()
    };
    let original = match MeRetainedComponent::from_initializing_native_kernel(kernel) {
        Ok(component) => component,
        Err(_) => panic!("actual native Initialization Mode handoff"),
    };
    let options = crate::fmi_me::driver::live_session_options(
        &rumoca_ir_solve::fmi::RootLocationPlan::STANDARD,
        2.0,
        1e-8,
        1e-8,
        1e-10,
        None,
    )
    .unwrap();
    let mut session = match original.complete_native_initialization_retaining(options) {
        Ok(session) => session,
        Err(_) => panic!("existing browser engine completes the original checked source"),
    };
    session.set_native_integer(&[3], &[i64::MIN]).unwrap();
    session.set_boolean(&[4], &[false]).unwrap();
    let before = crate::fmi_me::kernel::snapshot_count();
    session.set_native_float64(&[2], &[-0.0]).unwrap();
    assert_eq!(crate::fmi_me::kernel::snapshot_count() - before, 1);
    let before = crate::fmi_me::kernel::snapshot_count();
    session.set_native_float64(&[2], &[-0.0]).unwrap();
    assert!(session.set_native_float64(&[2, 1], &[9.0, 0.0]).is_err());
    assert!(session.set_native_float64(&[2], &[f64::NAN]).is_err());
    assert_eq!(crate::fmi_me::kernel::snapshot_count(), before);
    let gate = other.enter().unwrap();
    let before = crate::fmi_me::kernel::snapshot_count();
    assert!(session.set_native_float64(&[2], &[9.0]).is_err());
    assert_eq!(crate::fmi_me::kernel::snapshot_count(), before);
    drop(gate);
    for fault in [
        super::super::super::real::InputCommitFault::BeforeSync,
        super::super::super::real::InputCommitFault::BeforeDelay,
    ] {
        super::super::super::real::fail_input_commit(fault);
        let before = crate::fmi_me::kernel::snapshot_count();
        assert!(session.set_native_float64(&[2], &[9.0]).is_err());
        assert_eq!(crate::fmi_me::kernel::snapshot_count() - before, 1);
        assert_eq!(session.time(), 2.0);
    }
    let mut integer = [0];
    let mut boolean = [true];
    let mut real = [1.0];
    session.get_native_integer(&[3], &mut integer).unwrap();
    session.get_boolean(&[4], &mut boolean).unwrap();
    session.get_native_float64(&[2], &mut real).unwrap();
    assert_eq!((integer, boolean), ([i64::MIN], [false]));
    assert_eq!(real[0].to_bits(), (-0.0_f64).to_bits());
    assert!(session.set_native_float64(&[2, 1], &[9.0, 0.0]).is_err());
    assert!(session.set_native_integer(&[3, 4], &[1, 0]).is_err());
    session.get_native_integer(&[3], &mut integer).unwrap();
    assert_eq!(integer, [i64::MIN]);
    let mut original = match session.into_retained_component() {
        Ok(original) => original,
        Err(_) => panic!("owned session returns its actual component"),
    };
    original.reset().unwrap();
    original.get_native_integer(&[3], &mut integer).unwrap();
    original.get_boolean(&[4], &mut boolean).unwrap();
    original.get_native_float64(&[2], &mut real).unwrap();
    assert_eq!((integer, boolean, real), ([i64::MAX], [false], [2.0]));
    original.enter_initialization_mode(&config(5.0)).unwrap();
}

#[test]
fn native_exit_initialization_preflight_returns_actual_unresolved_preparation() {
    let component = fixture::reset_component();
    let root = component.runtime_view().retain_assertion_root();
    let code = rumoca_exec_wasm::compile_retained_native_preparation_wasm(
        root.native_preparation().unwrap(),
    )
    .unwrap();
    let mut original =
        MeRetainedComponent::prepare_retained_native_wasm(MeModelSource::new(&component), &code)
            .unwrap();
    original.set_native_integer(&[3], &[i64::MIN]).unwrap();
    original.set_boolean(&[4], &[true]).unwrap();
    original.set_native_float64(&[2], &[-0.0]).unwrap();
    let options = crate::fmi_me::driver::live_session_options(
        &rumoca_ir_solve::fmi::RootLocationPlan::STANDARD,
        2.0,
        1e-8,
        1e-8,
        1e-10,
        None,
    )
    .unwrap();
    let (original, _) = match original.complete_native_initialization_retaining(options) {
        Err(failure) => failure,
        Ok(_) => panic!("prepared source is not native Initialization Mode"),
    };
    let mut integer = [0];
    let mut boolean = [false];
    let mut real = [1.0];
    original.get_native_integer(&[3], &mut integer).unwrap();
    original.get_boolean(&[4], &mut boolean).unwrap();
    original.get_native_float64(&[2], &mut real).unwrap();
    assert_eq!((integer, boolean), ([i64::MIN], [true]));
    assert_eq!(real[0].to_bits(), (-0.0_f64).to_bits());
    assert_eq!(code.inputs().root().resolved_start_time(), None);
}

#[test]
fn a_prepared_native_resource_still_answers_model_queries() {
    let component = fixture::component();
    let cell = MeKernelCell::prepared(pending(&component));
    let expected = cell.with_prepared_for_control(|pending| pending.continuous_state_count());
    assert_eq!(
        cell.with_model(|model| model.state_scalar_count()),
        Some(expected)
    );
}

#[test]
fn restarting_an_active_native_instance_reanchors_it_at_the_new_start() {
    let component = fixture::reset_component();
    let owner = MeKernelCell::new(entered(&component));
    owner.restart(5.0, None).unwrap();
    let active = owner.borrow().unwrap();
    assert_eq!(active.initialization_state(), MeState::InitializationMode);
    assert_eq!(active.native_start_time(), Some(5.0));
}

#[test]
fn integrator_completion_attaches_a_session_and_refuses_an_unprepared_source() {
    let options = || {
        crate::fmi_me::driver::live_session_options(
            &rumoca_ir_solve::fmi::RootLocationPlan::STANDARD,
            2.0,
            1e-8,
            1e-8,
            1e-10,
            None,
        )
        .unwrap()
    };
    let component = fixture::reset_component();
    let initializing =
        match MeRetainedComponent::from_initializing_native_kernel(entered(&component)) {
            Ok(retained) => retained,
            Err(_) => panic!("the actual Initialization Mode kernel owns the handoff"),
        };
    let session = match initializing
        .complete_native_initialization_with_integrator_retaining(options(), |_| {
            unreachable!("a stateless component attaches no plugin")
        }) {
        Ok(session) => session,
        Err(_) => panic!("the hosted engine completes native initialization"),
    };
    assert_eq!(session.time(), 2.0);
    let mut integer = [0];
    session.get_native_integer(&[3], &mut integer).unwrap();
    assert_eq!(integer, [17], "the session keeps the written inputs");

    let code = rumoca_exec_wasm::compile_retained_native_preparation_wasm(
        component
            .runtime_view()
            .retain_assertion_root()
            .native_preparation()
            .unwrap(),
    )
    .unwrap();
    let prepared =
        MeRetainedComponent::prepare_retained_native_wasm(MeModelSource::new(&component), &code)
            .unwrap();
    let (prepared, error) = match prepared
        .complete_native_initialization_with_integrator_retaining(options(), |_| {
            unreachable!("no session attaches without an engine")
        }) {
        Err(failure) => failure,
        Ok(_) => panic!("a prepared source is not native Initialization Mode"),
    };
    assert!(
        error.to_string().contains("native resource is prepared"),
        "{error}"
    );
    drop(prepared);
}

#[test]
fn a_refused_initialization_entry_returns_the_pending_resource_with_its_writes() {
    let component = fixture::reset_component();
    let mut pending = pending(&component);
    pending.set_native_integer(&[3], &[17]).unwrap();
    crate::fmi_me::kernel::fail_initialization_entry();
    let (mut pending, error) = match pending.enter_initialization_mode_owned(&config(2.0)) {
        Err(failure) => failure,
        Ok(_) => panic!("the injected failure refuses Initialization Mode"),
    };
    assert!(
        error
            .to_string()
            .contains("injected Initialization Mode entry failure")
    );
    let mut integer = [0];
    pending.get_native_integer(&[3], &mut integer).unwrap();
    assert_eq!(
        integer,
        [17],
        "the refused entry keeps the pre-initialization write"
    );
    pending.reset().unwrap();
    pending.get_native_integer(&[3], &mut integer).unwrap();
    assert_eq!(
        integer,
        [i64::MAX],
        "the returned resource still resets to source"
    );
    let kernel = match pending.enter_initialization_mode_owned(&config(2.0)) {
        Ok(kernel) => kernel,
        Err(_) => panic!("the next entry succeeds"),
    };
    assert_eq!(kernel.initialization_state(), MeState::InitializationMode);
}

#[test]
fn a_discrete_real_is_read_from_its_typed_lane_after_the_engine_runs() {
    let component = fixture::discrete_real_component();
    let kernel = entered(&component);
    let owner = MeKernelCell::new(kernel);
    let mut stepped = [f64::NAN];
    owner
        .get_native_float64(&[5], &mut stepped)
        .expect("the derived discrete Real publishes through its Real lane");
    assert_eq!(
        stepped,
        [1.5 * 7.25],
        "stepped = 1.5 * real at the written input"
    );
    let mut result = [f64::NAN];
    owner.get_native_float64(&[1], &mut result).unwrap();
    assert_eq!(
        result,
        [2.0 * 7.25],
        "the continuous output reads the same inputs"
    );
}
