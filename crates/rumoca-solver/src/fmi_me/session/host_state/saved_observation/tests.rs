use super::*;
use crate::fmi_me as me;
use rumoca_ir_solve as solve;

fn host_with_input() -> MeHostState {
    let mut model = solve::SolveModel::default();
    model.problem.solve_layout.compiled_parameter_len = 1;
    model.problem.solve_layout.input_scalar_names = vec!["u".into()];
    model.parameters = vec![1.0];
    model.visible_names = vec!["u".into()];
    let span = rumoca_core::Span::from_offsets(
        rumoca_core::SourceId::from_source_name("saved-observation.mo"),
        1,
        2,
    );
    model.visible_value_rows = solve::ScalarProgramBlock::with_source_span(
        vec![vec![
            solve::LinearOp::LoadP { dst: 0, index: 0 },
            solve::LinearOp::StoreOutput { src: 0 },
        ]],
        span.require_provenance("saved observation fixture")
            .unwrap(),
    )
    .unwrap();
    model.problem.continuous.refresh_owners =
        rumoca_eval_solve::refresh_plan::build_continuous_refresh_owners(&mut model.problem)
            .unwrap();
    let config = me::MeInstanceConfig::new("saved-observation", 1e-10, 0.0, 1.0).unwrap();
    let retained =
        me::MeRetainedComponent::instantiate(me::MeModelSource::fixture(&model), &config, None)
            .unwrap();
    let options = me::driver::live_session_options(0.0, 1e-8, 1e-8, 1e-10, None).unwrap();
    retained.into_lease(options).unwrap().host
}

fn set_input(host: &MeHostState, value: f64) {
    let reference = host.kernel.borrow().value_reference("u").unwrap();
    host.kernel
        .borrow_mut()
        .set_float64(&[reference], &[value])
        .unwrap();
}

#[test]
fn saved_observation_reads_old_inputs_and_restores_the_complete_current_state() {
    let mut host = host_with_input();
    let saved = host.kernel.borrow().fmu_state();
    host.adopt_point(0.25, &[]).unwrap();
    set_input(&host, 2.0);
    let current = host.kernel.borrow().fmu_state();

    assert_eq!(host.observe_saved_point(&saved, 0.125, &[]).unwrap(), [1.0]);
    assert!(host.kernel.borrow().verification_matches_snapshot(&current));
    assert_eq!(host.observe_current().unwrap(), [2.0]);
    assert!(host.usability_loss().is_none());
}

#[test]
fn failed_saved_observation_restores_current_state_and_preserves_the_error() {
    let mut host = host_with_input();
    let saved = host.kernel.borrow().fmu_state();
    host.adopt_point(0.25, &[]).unwrap();
    set_input(&host, 2.0);
    let current = host.kernel.borrow().fmu_state();

    let error = host
        .with_saved_component_state::<()>(&saved, || {
            set_input(&host, 3.0);
            Err(MeSessionError::Timeout { seconds: 7.0 })
        })
        .unwrap_err();
    assert!(matches!(error, MeSessionError::Timeout { seconds: 7.0 }));
    assert!(host.kernel.borrow().verification_matches_snapshot(&current));
}

#[test]
fn unwinding_saved_observation_restores_current_state_and_the_original_payload() {
    let host = host_with_input();
    let saved = host.kernel.borrow().fmu_state();
    set_input(&host, 2.0);
    let current = host.kernel.borrow().fmu_state();
    let panic = std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| {
        host.with_saved_component_state::<()>(&saved, || {
            set_input(&host, 3.0);
            std::panic::panic_any(73_u32);
        })
    }))
    .unwrap_err();
    assert_eq!(panic.downcast_ref::<u32>(), Some(&73));
    assert!(host.kernel.borrow().verification_matches_snapshot(&current));
    assert_eq!(host.usability_loss(), Some(MeSessionLoss::NumericalStep));
}

#[test]
fn a_foreign_observation_snapshot_is_rejected_without_running_its_body() {
    let host = host_with_input();
    let foreign = host_with_input().kernel.borrow().fmu_state();
    let current = host.kernel.borrow().fmu_state();
    let error = host
        .with_saved_component_state::<()>(&foreign, || panic!("foreign snapshot entered"))
        .unwrap_err();
    assert!(matches!(error, MeSessionError::Component(_)));
    assert!(host.kernel.borrow().verification_matches_snapshot(&current));
}
