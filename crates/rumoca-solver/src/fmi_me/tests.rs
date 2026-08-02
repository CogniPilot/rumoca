//! Contract tests for the ME operations SPEC_0038 phase 2 added.
//!
//! The staging tests are pure: they exercise the value-level rules a host's
//! failure bucketing depends on without instantiating a component. The
//! directional-derivative tests run a real component, because the operation's
//! whole point is that the *component* owns the derivative.

use rumoca_ir_solve as solve;

use super::kernel::{event_update_application_time, frozen_projection_changed};
use super::{
    MeError, MeEventCause, MeEventEntry, MeIndicatorCrossing, MeInstanceConfig, MeModelSource,
    MeNoStateSession, MeStage, MeTime, ModelExchangeKernel, SolveMeKernel, resolve_me_stage,
};

#[test]
fn zero_state_event_continuation_is_independent_of_value_tolerance() {
    let mut model = solve::SolveModel::default();
    model.problem.events.scheduled_time_events = vec![2.5e-9];
    let run = |atol| {
        let mut session = MeNoStateSession::instantiate(
            MeModelSource::new(&model),
            crate::SimOptions {
                t_start: 0.0,
                t_end: 2.0e-8,
                dt: Some(1.0e-10),
                atol,
                rtol: atol,
                ..Default::default()
            },
        )
        .expect("zero-state session should initialize");
        [2.4e-9, 2.5e-9, 2.6e-9, 5.0e-9]
            .into_iter()
            .map(|target| {
                session
                    .advance_to(target)
                    .expect("zero-state session should reach every local boundary");
                session.time().to_bits()
            })
            .collect::<Vec<_>>()
    };

    let loose = run(1.0e-6);
    let tight = run(1.0e-12);
    assert_eq!(loose, tight);
    assert_eq!(loose[2], 2.6e-9_f64.to_bits());
    assert_eq!(loose[3], 5.0e-9_f64.to_bits());
}

#[test]
fn state_event_application_time_preserves_clock_and_numerical_owners() {
    let semantic_root = 0.21500000000000002;
    let snapped_horizon = 0.215;

    assert_eq!(
        event_update_application_time(semantic_root, snapped_horizon, false).to_bits(),
        snapped_horizon.to_bits(),
        "ordinary root rows execute at the host's numerical application point"
    );
    assert_eq!(
        event_update_application_time(semantic_root, snapped_horizon, true).to_bits(),
        semantic_root.to_bits(),
        "a coincident clock pass retains the semantic tick"
    );
}

// -- staging (B5) --------------------------------------------------------

#[test]
fn an_unrecorded_stage_takes_the_incoming_one() {
    assert_eq!(
        resolve_me_stage(None, MeStage::Integration),
        MeStage::Integration
    );
}

#[test]
fn a_recorded_stage_wins_over_a_coarser_outer_boundary() {
    assert_eq!(
        resolve_me_stage(Some(MeStage::EventIteration), MeStage::Integration),
        MeStage::EventIteration
    );
}

#[test]
fn resolving_a_stage_is_idempotent() {
    let resolved = resolve_me_stage(None, MeStage::Initialization);
    assert_eq!(
        resolve_me_stage(Some(resolved), MeStage::Integration),
        resolved
    );
}

#[test]
fn annotating_preserves_the_rendered_message() {
    let raw = MeError::Evaluation {
        message: "projection did not converge".to_string(),
    };
    let rendered = raw.to_string();
    assert_eq!(
        raw.at_stage(MeStage::ManifoldProjection).to_string(),
        rendered
    );
}

#[test]
fn kind_peels_annotations_so_variant_matching_is_unchanged() {
    let staged = MeError::NoContinuousStates
        .at_stage(MeStage::Integration)
        .at_stage(MeStage::Instantiate);
    assert!(matches!(staged.kind(), MeError::NoContinuousStates));
    assert!(matches!(staged.into_kind(), MeError::NoContinuousStates));
}

#[test]
fn the_innermost_stage_survives_an_outer_annotation() {
    let staged = MeError::Contract {
        reason: "buffer length".to_string(),
    }
    .at_stage(MeStage::EventIteration)
    .at_stage(MeStage::Integration);
    assert_eq!(staged.stage(), Some(MeStage::EventIteration));
}

#[test]
fn an_unannotated_failure_reports_no_stage() {
    assert_eq!(MeError::NoContinuousStates.stage(), None);
}

// -- fmi3GetDirectionalDerivative ----------------------------------------

/// `der(x) = v`, `der(v) = -4·x`: a pure ODE whose exact state Jacobian is the
/// constant `[[0, 1], [-4, 0]]`, so the expected directional derivative for any
/// seed is closed form and no tolerance is needed.
fn harmonic_oscillator() -> solve::SolveModel {
    let derivative = block(
        vec![
            vec![
                solve::LinearOp::LoadY { dst: 0, index: 1 },
                solve::LinearOp::StoreOutput { src: 0 },
            ],
            vec![
                solve::LinearOp::LoadY { dst: 0, index: 0 },
                solve::LinearOp::Const {
                    dst: 1,
                    value: -4.0,
                },
                solve::LinearOp::Binary {
                    dst: 2,
                    op: solve::BinaryOp::Mul,
                    lhs: 0,
                    rhs: 1,
                },
                solve::LinearOp::StoreOutput { src: 2 },
            ],
        ],
        "fmi_me_harmonic.mo",
    );
    let jacobian_v = block(
        vec![
            vec![
                solve::LinearOp::LoadSeed { dst: 0, index: 1 },
                solve::LinearOp::StoreOutput { src: 0 },
            ],
            vec![
                solve::LinearOp::LoadSeed { dst: 0, index: 0 },
                solve::LinearOp::Const {
                    dst: 1,
                    value: -4.0,
                },
                solve::LinearOp::Binary {
                    dst: 2,
                    op: solve::BinaryOp::Mul,
                    lhs: 0,
                    rhs: 1,
                },
                solve::LinearOp::StoreOutput { src: 2 },
            ],
        ],
        "fmi_me_harmonic_jvp.mo",
    );
    solve::SolveModel {
        problem: solve::SolveProblem {
            continuous: solve::ContinuousSolveSystem {
                implicit_rhs: solve::ComputeBlock::from_scalar_program_block(derivative.clone()),
                implicit_row_targets: vec![
                    Some(solve::scalar_slot_y(0)),
                    Some(solve::scalar_slot_y(1)),
                ],
                derivative_rhs: solve::ComputeBlock::from_scalar_program_block(derivative),
                ..Default::default()
            },
            solve_layout: solve::SolveLayout {
                solver_maps: solve::SolverNameIndexMaps {
                    names: vec!["x".to_string(), "v".to_string()],
                    ..Default::default()
                },
                state_scalar_count: 2,
                ..Default::default()
            },
            ..Default::default()
        },
        artifacts: solve::SolveArtifacts {
            continuous: solve::ContinuousSolveArtifacts {
                full_jacobian_v: jacobian_v,
                ..Default::default()
            },
            ..Default::default()
        },
        initial_y: vec![1.0, 0.0],
        solver_nominals: vec![1.0, 1.0],
        visible_names: vec!["x".to_string(), "v".to_string()],
        ..Default::default()
    }
}

fn strict_root_relation_memory() -> solve::SolveModel {
    let derivative = block(
        vec![vec![
            solve::LinearOp::Const { dst: 0, value: 1.0 },
            solve::LinearOp::StoreOutput { src: 0 },
        ]],
        "fmi_me_strict_root_derivative.mo",
    );
    let root = block(
        vec![vec![
            solve::LinearOp::LoadY { dst: 0, index: 0 },
            solve::LinearOp::StoreOutput { src: 0 },
        ]],
        "fmi_me_strict_root_indicator.mo",
    );
    let condition_memory = block(
        vec![vec![
            solve::LinearOp::LoadY { dst: 0, index: 0 },
            solve::LinearOp::Const { dst: 1, value: 0.0 },
            solve::LinearOp::Compare {
                dst: 2,
                op: solve::CompareOp::Gt,
                lhs: 0,
                rhs: 1,
            },
            solve::LinearOp::StoreOutput { src: 2 },
        ]],
        "fmi_me_strict_root_memory.mo",
    );
    solve::SolveModel {
        problem: solve::SolveProblem {
            continuous: solve::ContinuousSolveSystem {
                implicit_rhs: solve::ComputeBlock::from_scalar_program_block(derivative.clone()),
                implicit_row_targets: vec![Some(solve::scalar_slot_y(0))],
                derivative_rhs: solve::ComputeBlock::from_scalar_program_block(derivative),
                ..Default::default()
            },
            discrete: solve::DiscreteSolveSystem {
                rhs: condition_memory,
                update_targets: vec![solve::scalar_slot_p(0)],
                row_roles: vec![solve::DiscreteRowRole::ConditionMemory],
                pre_modes: vec![solve::DiscreteEventPreMode::FollowCurrent],
                observation_refresh: vec![false],
                clock_owners: vec![None],
                ..Default::default()
            },
            events: solve::SolveEventPartition {
                root_conditions: root,
                root_relation_memory_targets: vec![Some(solve::scalar_slot_p(0))],
                condition_memory_parameter_indices: vec![0],
                ..Default::default()
            },
            solve_layout: solve::SolveLayout {
                solver_maps: solve::SolverNameIndexMaps {
                    names: vec!["x".to_string()],
                    ..Default::default()
                },
                state_scalar_count: 1,
                compiled_parameter_len: 1,
                ..Default::default()
            },
            ..Default::default()
        },
        initial_y: vec![-1.0],
        solver_nominals: vec![1.0],
        parameters: vec![0.0],
        visible_names: vec!["x".to_string()],
        ..Default::default()
    }
}

fn block(rows: Vec<Vec<solve::LinearOp>>, name: &'static str) -> solve::ScalarProgramBlock {
    let span = rumoca_core::Span::from_offsets(rumoca_core::SourceId::from_source_name(name), 1, 2);
    solve::ScalarProgramBlock::with_source_span(
        rows,
        span.require_provenance("fmi_me fixture")
            .expect("fixture span is source-backed"),
    )
    .expect("fixture program is computable")
}

fn instantiate(model: &solve::SolveModel) -> SolveMeKernel {
    instantiate_with_numerics(model, super::MeNumericsProfile::Component)
}

fn instantiate_with_numerics(
    model: &solve::SolveModel,
    numerics_profile: super::MeNumericsProfile,
) -> SolveMeKernel {
    SolveMeKernel::instantiate(
        MeModelSource::new(model),
        &MeInstanceConfig {
            instance_name: "fmi-me-test",
            tolerance: 1.0e-10,
            start_time: 0.0,
            stop_time: 1.0,
            root_profile: super::MeRootProfile::Component,
            numerics_profile,
        },
    )
    .expect("fixture instantiates")
}

#[test]
fn rejected_lifecycle_transitions_leave_the_legal_path_available() {
    let model = harmonic_oscillator();
    let mut kernel = instantiate(&model);

    let error = kernel
        .exit_initialization_mode()
        .expect_err("initialization cannot be exited before it is entered");
    assert_eq!(error.stage(), Some(MeStage::Initialization));
    assert!(matches!(error.kind(), MeError::Contract { .. }));

    kernel
        .enter_initialization_mode()
        .expect("the rejected transition must not consume Instantiated");
    kernel
        .exit_initialization_mode()
        .expect("the legal transition remains available");
    kernel
        .enter_continuous_time_mode()
        .expect_err("the initial event update must complete first");
    kernel
        .update_discrete_states()
        .expect("the rejected continuous-mode entry must leave Event Mode intact");
    kernel
        .enter_continuous_time_mode()
        .expect("the canonical initialization path reaches Continuous-Time Mode");
}

#[test]
fn terminated_is_fail_closed_until_snapshot_restore() {
    let model = harmonic_oscillator();
    let mut kernel = instantiate(&model);
    let saved = kernel.fmu_state();
    kernel.terminate().expect("termination is legal once");

    assert!(kernel.set_time(super::MeTime::at(0.25)).is_err());
    assert!(kernel.set_continuous_states(&[2.0, 3.0]).is_err());
    assert!(kernel.terminate().is_err());

    kernel
        .reset_to_fmu_state(&saved)
        .expect("snapshot restore is the one exit from Terminated");
    kernel
        .enter_initialization_mode()
        .expect("the saved Instantiated lifecycle is restored exactly");
}

#[test]
fn fmu_state_restore_replays_the_same_scheduled_event_continuation() {
    let mut model = harmonic_oscillator();
    model.problem.events.scheduled_time_events = vec![0.5];
    let mut kernel = instantiate(&model);
    kernel
        .enter_initialization_mode()
        .expect("enter initialization");
    kernel
        .exit_initialization_mode()
        .expect("exit initialization");
    kernel
        .update_discrete_states()
        .expect("initial event update");
    kernel
        .enter_continuous_time_mode()
        .expect("enter continuous time");
    kernel
        .set_time(MeTime::at(0.25))
        .expect("set checkpoint time");
    kernel
        .set_continuous_states(&[2.0, 3.0])
        .expect("set checkpoint state");
    let mut cached_derivative = Vec::new();
    kernel
        .get_continuous_state_derivatives(&mut cached_derivative)
        .expect("populate derivative cache");
    let stop = kernel.next_event_stop(0.75).expect("schedule next event");
    assert_eq!(stop.time.to_bits(), 0.5f64.to_bits());
    assert!(stop.is_event);
    let saved_observable = kernel.verification_observable_state();
    let saved = kernel.fmu_state();

    let first = continue_from_scheduled_event(&mut kernel);
    kernel
        .terminate()
        .expect("terminate after first continuation");
    assert!(!kernel.verification_matches_snapshot(&saved));
    kernel
        .reset_to_fmu_state(&saved)
        .expect("same-instance exact restore");
    assert_eq!(kernel.verification_observable_state(), saved_observable);
    assert!(kernel.verification_matches_snapshot(&saved));
    let second = continue_from_scheduled_event(&mut kernel);

    assert_eq!(first, second);
}

fn continue_from_scheduled_event(kernel: &mut SolveMeKernel) -> (Vec<u64>, Vec<u64>) {
    kernel
        .set_time(MeTime::at(0.5))
        .expect("reach scheduled event");
    kernel
        .capture_pre_event_state()
        .expect("capture event pre-state");
    kernel
        .enter_event_mode(MeEventEntry {
            cause: MeEventCause::TimeEvent,
            event_time: 0.5,
            horizon: 0.75,
        })
        .expect("enter scheduled event");
    kernel
        .update_discrete_states()
        .expect("apply scheduled event");
    kernel
        .enter_continuous_time_mode()
        .expect("resume continuous time");
    let mut states = vec![0.0; 2];
    kernel
        .get_continuous_states(&mut states)
        .expect("read continued state");
    let mut derivatives = Vec::new();
    kernel
        .get_continuous_state_derivatives(&mut derivatives)
        .expect("read continued derivative");
    (
        states.into_iter().map(f64::to_bits).collect(),
        derivatives.into_iter().map(f64::to_bits).collect(),
    )
}

#[test]
fn instance_brands_reject_foreign_capabilities_without_mutation() {
    let mut model = harmonic_oscillator();
    model.problem.solve_layout.compiled_parameter_len = 1;
    model.problem.solve_layout.input_scalar_names = vec!["u".to_string()];
    model.parameters = vec![1.0];
    let mut first = instantiate(&model);
    let mut second = instantiate(&model);
    let first_ref = first
        .value_reference("u")
        .expect("input has a value reference");
    let second_ref = second
        .value_reference("u")
        .expect("the other instance has its own reference");

    let error = first
        .set_float64(&[first_ref, second_ref], &[2.0, 3.0])
        .expect_err("a foreign reference rejects the whole batch");
    assert!(matches!(error.kind(), MeError::Contract { .. }));
    assert_eq!(
        first.verification_observable_state().3,
        vec![1.0f64.to_bits()]
    );

    let foreign_observation = first.observe().expect("first instance observes");
    let mut values = Vec::new();
    second
        .get_outputs(&foreign_observation, 0.0, &mut values)
        .expect_err("observations cannot cross component instances");

    let foreign_state = first.fmu_state();
    second
        .reset_to_fmu_state(&foreign_state)
        .expect_err("saved component state cannot cross instances");
    assert_eq!(
        second.verification_observable_state().3,
        vec![1.0f64.to_bits()]
    );
}

#[test]
fn frozen_full_state_guard_is_not_part_of_the_component_profile() {
    let model = harmonic_oscillator();
    let kernel = instantiate(&model);

    let error = kernel
        .verify_frozen_compatibility_state(
            &model.initial_y,
            &model.parameters,
            MeStage::Initialization,
        )
        .expect_err("the ordinary component profile must not expose a migration dual-run guard");

    assert_eq!(error.stage(), Some(MeStage::Initialization));
    assert!(matches!(error.kind(), MeError::Contract { .. }));
    assert!(error.to_string().contains("requires DiffsolFrozen"));
}

#[test]
fn diffsol_profile_guards_the_settled_full_state_after_initialization() {
    let model = harmonic_oscillator();
    let mut kernel = instantiate_with_numerics(&model, super::MeNumericsProfile::DiffsolFrozen);
    kernel
        .enter_initialization_mode()
        .expect("frozen initialization should start");
    kernel
        .exit_initialization_mode()
        .expect("frozen initialization should settle");
    let mut discrete = kernel
        .update_discrete_states()
        .expect("frozen initial event should run");
    while discrete.discrete_states_need_update {
        discrete = kernel
            .update_discrete_states()
            .expect("frozen initial event iteration should converge");
    }
    kernel
        .enter_continuous_time_mode()
        .expect("settled component should enter continuous-time mode");

    kernel
        .verify_frozen_compatibility_state(
            &model.initial_y,
            &model.parameters,
            MeStage::Initialization,
        )
        .expect("the settled full state should match the frozen host vector bit-for-bit");

    let mut mismatched = model.initial_y.clone();
    mismatched.push(0.0);
    let error = kernel
        .verify_frozen_compatibility_state(&mismatched, &model.parameters, MeStage::Initialization)
        .expect_err("a full-layout mismatch must fail the initialization guard");
    assert_eq!(error.stage(), Some(MeStage::Initialization));
    assert!(error.to_string().contains("solver_mismatch=Some"));
}

#[test]
fn diffsol_profile_retains_the_typed_post_side_of_a_strict_root() {
    let model = strict_root_relation_memory();
    let mut kernel = instantiate_with_numerics(&model, super::MeNumericsProfile::DiffsolFrozen);
    kernel
        .enter_initialization_mode()
        .expect("strict-root initialization should start");
    kernel
        .exit_initialization_mode()
        .expect("strict-root initialization should settle");
    kernel
        .update_discrete_states()
        .expect("strict-root initial event should run");
    kernel
        .enter_continuous_time_mode()
        .expect("strict-root model should enter continuous time");

    kernel
        .set_time(MeTime::at(1.0))
        .expect("component should reach the root instant");
    kernel
        .set_continuous_states(&[0.0])
        .expect("component should use the exact root state");
    kernel
        .capture_pre_event_state()
        .expect("event iteration should retain its entry values");
    kernel
        .arm_state_event(&[MeIndicatorCrossing {
            index: 0,
            post_indicator_value: 1.0,
        }])
        .expect("the typed crossing should arm the post-root side");
    assert!(
        kernel.model_description().needs_completed_integrator_step,
        "the linked kernel declares its accepted-step history requirement"
    );
    let completed = kernel
        .completed_integrator_step(true)
        .expect("the integrator should report the located root");
    assert_eq!(completed, super::MeCompletedIntegratorStep::default());
    kernel
        .enter_event_mode(MeEventEntry {
            cause: MeEventCause::StateEvent,
            event_time: 1.0,
            horizon: 1.0,
        })
        .expect("the component should enter root event iteration");
    kernel
        .update_discrete_states()
        .expect("the strict-root event should settle");

    assert_eq!(
        kernel.verification_observable_state().3,
        vec![1.0f64.to_bits()],
        "the exact-root comparison is false, but the typed crossing owns the post-root value"
    );
    kernel
        .enter_continuous_time_mode()
        .expect("the settled strict-root event should resume integration");
    kernel
        .prepare_frozen_bdf_initial_seed(&[0.0])
        .expect("the frozen reset should accept its post-event seed");
    kernel
        .verify_frozen_compatibility_state(&[0.0], &[1.0], MeStage::EventIteration)
        .expect("the reset seam should observe the same typed post-root relation memory");
}

#[test]
fn the_directional_derivative_is_the_exact_state_jacobian_product() {
    let model = harmonic_oscillator();
    let mut kernel = instantiate(&model);
    kernel
        .set_continuous_states(&[3.0, 5.0])
        .expect("state buffer matches the model description");

    let mut sensitivity = vec![f64::NAN; 2];
    kernel
        .get_directional_derivative(&[1.0, 0.0], &mut sensitivity)
        .expect("a pure ODE has a directional derivative everywhere");
    // First column of [[0, 1], [-4, 0]].
    assert_eq!(sensitivity, vec![0.0, -4.0]);

    kernel
        .get_directional_derivative(&[0.0, 1.0], &mut sensitivity)
        .expect("second seed evaluates too");
    assert_eq!(sensitivity, vec![1.0, 0.0]);
}

/// The operation is a *directional* derivative, not a column extractor: a
/// non-unit seed must come back scaled, or a host's Newton direction would be
/// silently renormalized.
#[test]
fn the_directional_derivative_is_linear_in_the_seed() {
    let model = harmonic_oscillator();
    let mut kernel = instantiate(&model);
    kernel
        .set_continuous_states(&[0.25, -1.5])
        .expect("state buffer matches the model description");

    let mut sensitivity = vec![f64::NAN; 2];
    kernel
        .get_directional_derivative(&[2.0, -3.0], &mut sensitivity)
        .expect("a pure ODE has a directional derivative everywhere");
    assert_eq!(sensitivity, vec![-3.0, -8.0]);
}

#[test]
fn a_mismatched_seed_length_is_a_contract_violation_at_the_integration_stage() {
    let model = harmonic_oscillator();
    let kernel = instantiate(&model);

    let mut sensitivity = vec![0.0; 2];
    let error = kernel
        .get_directional_derivative(&[1.0], &mut sensitivity)
        .expect_err("a seed that is not one entry per continuous state is rejected");

    assert_eq!(error.stage(), Some(MeStage::Integration));
    assert!(matches!(error.kind(), MeError::Contract { .. }));
    assert!(
        error
            .to_string()
            .contains("1 entries for 2 continuous states")
    );
}

#[test]
fn a_mismatched_sensitivity_length_is_rejected_before_evaluation() {
    let model = harmonic_oscillator();
    let kernel = instantiate(&model);

    let mut sensitivity = vec![0.0; 3];
    let error = kernel
        .get_directional_derivative(&[1.0, 0.0], &mut sensitivity)
        .expect_err("a sensitivity buffer that is not one entry per state derivative is rejected");

    assert_eq!(error.stage(), Some(MeStage::Integration));
    assert!(matches!(error.kind(), MeError::Contract { .. }));
}

#[test]
fn frozen_projection_keeps_small_bit_real_manifold_changes_active() {
    let before = [293.15];
    let after = [293.150_01];
    let tolerance = 1.0e-6;

    assert!(!crate::runtime_values_changed(&before, &after, tolerance));
    assert!(frozen_projection_changed(true, &before, &after, tolerance));
}

// -- instantiation staging -----------------------------------------------

/// `NoContinuousStates` routes a host to its zero-state path. Annotating it
/// would make a routing answer look like an instantiation failure in every
/// bucket histogram downstream.
#[test]
fn the_zero_state_routing_answer_carries_no_stage() {
    let model = solve::SolveModel::default();
    let error = SolveMeKernel::instantiate(
        MeModelSource::new(&model),
        &MeInstanceConfig {
            instance_name: "fmi-me-test",
            tolerance: 1.0e-10,
            start_time: 0.0,
            stop_time: 1.0,
            root_profile: super::MeRootProfile::Component,
            numerics_profile: super::MeNumericsProfile::Component,
        },
    )
    .err()
    .expect("a model with no continuous states has no ME component");

    assert!(matches!(error, MeError::NoContinuousStates));
    assert_eq!(error.stage(), None);
}

#[test]
fn a_rejected_model_is_staged_at_instantiation() {
    let mut model = harmonic_oscillator();
    model.initial_y = vec![1.0];
    let error = SolveMeKernel::instantiate(
        MeModelSource::new(&model),
        &MeInstanceConfig {
            instance_name: "fmi-me-test",
            tolerance: 1.0e-10,
            start_time: 0.0,
            stop_time: 1.0,
            root_profile: super::MeRootProfile::Component,
            numerics_profile: super::MeNumericsProfile::Component,
        },
    )
    .err()
    .expect("an initial vector that contradicts the solver layout is rejected");

    assert_eq!(error.stage(), Some(MeStage::Instantiate));
    assert!(matches!(error.kind(), MeError::Evaluation { .. }));
}
