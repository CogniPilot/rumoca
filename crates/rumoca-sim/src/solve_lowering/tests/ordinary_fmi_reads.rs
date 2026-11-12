//! Standard ordinary VR reads on a dynamic correlated component.
use super::*;
use rumoca_ir_solve::fmi::FmiComponent;
use rumoca_solver::fmi_me::*;
use std::{cell::RefCell, rc::Rc};

fn component(
    policy: crate::SimExecutionPolicy,
    numerical_fault: bool,
    event: bool,
) -> (FmiComponent, MeRetainedComponent) {
    let source = "function checkedValue input Real x; output Real y; algorithm assert(x>=0,\"original value assertion\"); y:=x; end checkedValue; model OrdinaryRead parameter Real p=-0.0; parameter Integer mode=7; Real x(start=1,fixed=true); output Real pair[2]; equation der(x)=2; pair={checkedValue(x),p}; end OrdinaryRead;";
    let source = if numerical_fault {
        source.replace("y:=x;", "y:=sqrt(x);")
    } else {
        source.into()
    };
    // One located state event inside the run, at t=0.0075.
    let source = if event {
        source.replace(
            "p}; end",
            "p}; when x>1.015 then reinit(x,0.9); end when; end",
        )
    } else {
        source
    };
    let dae = compile(&source, "OrdinaryRead");
    let options = SimOptions {
        execution_policy: policy,
        t_end: 0.02,
        ..Default::default()
    };
    let lowered =
        super::super::lower_correlated_for_simulation_with_overrides(&dae, &options).unwrap();
    let backend =
        crate::native_execution::admitted_native_execution_backend(&options, lowered.model())
            .unwrap();
    if policy == crate::SimExecutionPolicy::NativeRequired {
        assert!(
            backend.is_some(),
            "required compiled execution cannot be an interpreter fallback"
        );
    }
    let component = rumoca_phase_solve::fmi::finish_fmi_component(lowered).unwrap();
    let retained = MeRetainedComponent::instantiate(
        MeModelSource::new(&component),
        &MeInstanceConfig::new("ordinary-read", 1e-9, 0.0, 0.02).unwrap(),
        backend,
    )
    .unwrap();
    (component, retained)
}

fn vr(component: &FmiComponent, name: &str) -> u32 {
    component
        .variables()
        .iter()
        .find(|v| v.name() == name)
        .unwrap()
        .value_reference_fmi3()
}

fn initialize(retained: &mut MeRetainedComponent) {
    drop(retained.lease(options()).unwrap());
}

fn options() -> MeSessionOptions {
    session_options(false)
}

/// Options that record the output trace, for tests that compare trajectories.
fn traced() -> MeSessionOptions {
    session_options(true)
}

fn session_options(records_trace: bool) -> MeSessionOptions {
    MeSessionOptions::new(MeSessionOptionsInput {
        start_time: 0.0,
        stop_time: Some(0.02),
        relative_tolerance: 1e-9,
        absolute_tolerance: 1e-10,
        output_interval: 0.01,
        root_scan_resolution: 0.001,
        root_location_tolerance: 1e-10,
        max_wall_seconds: Some(100.0),
        records_trace,
    })
    .unwrap()
}

#[test]
fn ordinary_fmi_reads_dynamic_arrays_time_types_and_reset() {
    for policy in [
        crate::SimExecutionPolicy::Interpreter,
        crate::SimExecutionPolicy::NativeRequired,
    ] {
        let (component, mut retained) = component(policy, false, false);
        initialize(&mut retained);
        let refs = [
            0,
            vr(&component, "pair"),
            vr(&component, "p"),
            vr(&component, "pair"),
        ];
        let mut values = [42.0; 6];
        retained.get_native_float64(&refs, &mut values).unwrap();
        assert_eq!(values, [0.0, 1.0, -0.0, -0.0, 1.0, -0.0]);
        assert_eq!(values[3].to_bits(), (-0.0_f64).to_bits());
        let before = values.map(f64::to_bits);
        for bad_refs in [vec![refs[1], u32::MAX], vec![vr(&component, "mode")]] {
            assert!(retained.get_native_float64(&bad_refs, &mut values).is_err());
            assert_eq!(values.map(f64::to_bits), before);
        }
        let mut wrong_extent = [73.0];
        assert!(
            retained
                .get_native_float64(&[refs[1]], &mut wrong_extent)
                .is_err()
        );
        assert_eq!(wrong_extent, [73.0]);
        retained.get_native_float64(&[], &mut []).unwrap();
        retained.reset().unwrap();
        initialize(&mut retained);
        retained.get_native_float64(&refs, &mut values).unwrap();
        assert_eq!(values.map(f64::to_bits), before);
    }
}

/// Reads ordinary Float64 values during initialization. Without `inner` it
/// retains the handle for inspection and never steps; with `inner` it passes
/// the handle to that real integrator and forwards every later call.
struct ReadOnInitialize {
    reference: u32,
    handle: Rc<RefCell<Option<MeDerivativeHandle>>>,
    fail_source: bool,
    initializations: usize,
    inner: Option<Box<dyn MeIntegratorBackend>>,
}

impl MeIntegratorBackend for ReadOnInitialize {
    fn initialize(
        &mut self,
        point: &MeContinuousPoint,
        handle: MeDerivativeHandle,
    ) -> Result<(), MeIntegrationError> {
        let mut values = [42.0; 3];
        let states = if self.fail_source {
            &[-1.0][..]
        } else {
            point.states()
        };
        let outcome =
            handle.float64_values_into(point.time(), states, &[0, self.reference], &mut values);
        if self.fail_source {
            assert!(
                outcome.is_err(),
                "unexpected successful domain-failure read: {values:?}"
            );
            assert_eq!(values, [42.0; 3]);
        } else {
            outcome?;
            // The first call is the initial point. A restart after an event
            // supplies its own point, so every read derives from that point.
            if self.initializations == 0 {
                assert_eq!(bits(&values), bits(&[0.0, 1.0, -0.0]));
            }
            let current = [point.time(), point.states()[0], -0.0];
            assert_eq!(bits(&values), bits(&current));
            // SOLVE-C93: the event assertion remains root/action owned; a trial
            // value read does not process that event or replace its timing.
            handle.float64_values_into(point.time(), &[-1.0], &[0, self.reference], &mut values)?;
            assert_eq!(bits(&values), bits(&[point.time(), -1.0, -0.0]));
            handle.float64_values_into(
                point.time(),
                point.states(),
                &[0, self.reference],
                &mut values,
            )?;
            assert_eq!(
                bits(&values),
                bits(&current),
                "the trial read is not retained"
            );
        }
        self.initializations += 1;
        match &mut self.inner {
            Some(inner) => inner.initialize(point, handle),
            None => {
                *self.handle.borrow_mut() = Some(handle);
                Ok(())
            }
        }
    }
    fn advance(
        &mut self,
        request: &MeAdvanceRequest,
    ) -> Result<MeStepCandidate, MeIntegrationError> {
        match &mut self.inner {
            Some(inner) => inner.advance(request),
            None => Err(MeIntegrationError::numerical(
                "ordinary Float64 read control",
                MeNumericalFailure::Construction,
                "read fixture never advances",
            )),
        }
    }
    fn sample(&self, time: f64, states: &mut [f64]) -> Result<(), MeIntegrationError> {
        match &self.inner {
            Some(inner) => inner.sample(time, states),
            None => Err(MeIntegrationError::numerical(
                "ordinary Float64 read control",
                MeNumericalFailure::Construction,
                "read fixture never samples",
            )),
        }
    }
    fn truncate_reset(&mut self, point: &MeContinuousPoint) -> Result<(), MeIntegrationError> {
        match &mut self.inner {
            Some(inner) => inner.truncate_reset(point),
            None => Err(MeIntegrationError::numerical(
                "ordinary Float64 read control",
                MeNumericalFailure::Construction,
                "read fixture never truncates",
            )),
        }
    }
}

#[test]
fn ordinary_fmi_reads_use_actual_host_activation_and_latch_inactive_failure() {
    let (component, mut retained) =
        component(crate::SimExecutionPolicy::NativeRequired, false, false);
    let handle = Rc::new(RefCell::new(None));
    let plugin = ReadOnInitialize {
        reference: vr(&component, "pair"),
        handle: Rc::clone(&handle),
        fail_source: false,
        initializations: 0,
        inner: None,
    };
    let host = retained.lease(options()).unwrap();
    let _session = host.into_session(Some(Box::new(plugin))).unwrap();
    let handle = handle.borrow();
    let handle = handle.as_ref().unwrap();
    let mut values = [73.0; 3];
    assert!(
        handle
            .float64_values_into(0.0, &[1.0], &[0, vr(&component, "pair")], &mut values)
            .is_err()
    );
    assert_eq!(values, [73.0; 3]);
    assert!(handle.has_failed());
}

#[test]
fn ordinary_fmi_reads_cannot_swallow_a_public_evaluation_failure() {
    let (component, mut retained) =
        component(crate::SimExecutionPolicy::NativeRequired, true, false);
    let handle = Rc::new(RefCell::new(None));
    let plugin = ReadOnInitialize {
        reference: vr(&component, "pair"),
        handle,
        fail_source: true,
        initializations: 0,
        inner: None,
    };
    let host = retained.lease(options()).unwrap();
    let outcome = host.into_session(Some(Box::new(plugin)));
    let Err(error) = outcome else {
        panic!("host must retain the caught evaluation failure")
    };
    assert!(
        error.to_string().to_lowercase().contains("finite"),
        "{error}"
    );
}

/// One run of the event variant to its stop time, with the host-default RK45
/// integrator either alone or behind the initialization reads.
fn read_trajectory(reads: bool) -> rumoca_solver::SimResult {
    let (component, mut retained) =
        component(crate::SimExecutionPolicy::NativeRequired, false, true);
    let host = retained.lease(traced()).unwrap();
    let rk45 = rumoca_solver_rk45::model_exchange_integrator(host.numerical_setup(None).unwrap());
    let plugin: Box<dyn MeIntegratorBackend> = if reads {
        Box::new(ReadOnInitialize {
            reference: vr(&component, "pair"),
            handle: Rc::new(RefCell::new(None)),
            fail_source: false,
            initializations: 0,
            inner: Some(rk45),
        })
    } else {
        rk45
    };
    let mut cursor = driver::batch_output_cursor(&traced()).unwrap();
    let mut session = host.into_session(Some(plugin)).unwrap();
    session.run_to_stop(&mut cursor).unwrap();
    session.finish()
}

fn bits(values: &[f64]) -> Vec<u64> {
    values.iter().map(|value| value.to_bits()).collect()
}

#[test]
fn ordinary_fmi_reads_during_initialization_leave_trajectory_and_event_unchanged() {
    // The reads include a trial state the component must not keep (SOLVE-C93).
    let read = read_trajectory(true);
    let plain = read_trajectory(false);
    // Both runs record their traces, so the comparison below is not vacuous.
    assert!(
        plain.names.iter().any(|name| name == "x"),
        "{:?}",
        plain.names
    );
    assert!(!plain.times.is_empty());
    assert_eq!(read.names, plain.names);
    assert_eq!(bits(&read.times), bits(&plain.times));
    assert_eq!(read.data.len(), plain.data.len());
    for (with_reads, without) in read.data.iter().zip(&plain.data) {
        assert_eq!(bits(with_reads), bits(without));
    }
    // Both runs located the reset at t=0.0075: x(0.02) = 0.9 + 2 * 0.0125.
    let x = plain.names.iter().position(|name| name == "x").unwrap();
    assert!((plain.data[x].last().unwrap() - 0.925).abs() < 1e-6);
}
