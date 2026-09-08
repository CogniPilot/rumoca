//! Contract tests for the SPEC_0038 ME operations.
//!
//! The staging tests are pure: they exercise the value-level rules a host's
//! failure bucketing depends on without instantiating a component. The
//! directional-derivative tests run a real component, because the operation's
//! whole point is that the *component* owns the derivative.

mod failure_atomicity;

use indexmap::IndexMap;
use rumoca_ir_solve as solve;

use super::kernel::{
    continuous_state_values_changed, event_right_limit_state_derivatives,
    event_update_application_time,
};
use super::session::MeOutputCursor;
use super::{
    MeDirectionalKnownBatch, MeDirectionalUnknownBatch, MeError, MeExecutionSelection,
    MeInstanceConfig, MeModelSource, MeRetainedComponent, MeStage, MeTime, SolveMeKernel,
    driver::live_session_options, resolve_me_stage, session::MeAdvanceOutcome,
};

use crate::test_support::empty_binary64_first_product_model;

fn state_directional_batches(
    kernel: &SolveMeKernel,
) -> (MeDirectionalUnknownBatch, MeDirectionalKnownBatch) {
    let knowns = kernel
        .continuous_state_value_references()
        .and_then(|references| kernel.directional_known_batch(references))
        .expect("state known batch constructs");
    let unknowns = kernel
        .continuous_state_derivative_value_references()
        .and_then(|references| kernel.directional_unknown_batch(references))
        .expect("state-derivative unknown batch constructs");
    (unknowns, knowns)
}

fn with_visible_unit_state(
    model: solve::SolveModel,
    name: &str,
    start: f64,
    source: &'static str,
) -> solve::SolveModel {
    let provenance =
        rumoca_core::Span::from_offsets(rumoca_core::SourceId::from_source_name(source), 1, 2);
    crate::test_support::with_explicit_real_scalar_catalog(
        model,
        vec![crate::test_support::RealScalarVariableFixture::state(
            1, name, 0, start, 1.0, true, provenance,
        )],
        crate::test_support::direct_y_visible_rows([0], provenance),
    )
}

fn zero_state_event_model() -> solve::SolveModel {
    crate::test_support::checked_solve_model! {
        problem: crate::test_support::checked_solve_problem!(
            solve::VarLayout::default(),
            solve::SolveLayout::default(),
                crate::test_support::ContinuousSystemFixture::empty(),
            solve::InitializationSolveSystem::empty(),
            solve::DiscreteSolveSystem::default(),
            solve::SolveEventPartition {
                scheduled_time_events: vec![2.5e-9],
                ..Default::default()
            },
            solve::SolveClockPartition::default(),
        )
        .expect("zero-state event fixture satisfies the checked root contract"),
        ..empty_binary64_first_product_model()
    }
}

#[test]
fn zero_state_event_continuation_is_independent_of_value_tolerance() {
    let run = |atol| {
        let model = zero_state_event_model();
        let retained = MeRetainedComponent::instantiate(
            MeModelSource::fixture(crate::test_support::fmi_component(model)),
            &fixture_instance_config(),
            MeExecutionSelection::Interpreter,
        )
        .expect("zero-state component should instantiate");
        let options = live_session_options(atol, atol, 1.0e-10, None)
            .expect("zero-state session options should construct");
        let host = retained
            .into_lease(options)
            .expect("zero-state component should initialize");
        let mut session = host
            .into_session(None)
            .expect("zero-state session should select the time-only plugin");
        let mut cursor = MeOutputCursor::empty();
        [2.4e-9, 2.5e-9, 2.6e-9, 5.0e-9]
            .into_iter()
            .map(|target| {
                session
                    .advance_to(target, &mut cursor)
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
fn consuming_lease_rewinds_after_a_prior_borrowed_session_mutates_the_instance() {
    let config = MeInstanceConfig::new("consuming-nonzero", 1.0e-10, 0.125, 1.0)
        .expect("the nonzero pristine start is checked");
    let mut retained = MeRetainedComponent::instantiate(
        MeModelSource::fixture(crate::test_support::fmi_component(zero_state_event_model())),
        &config,
        MeExecutionSelection::Interpreter,
    )
    .expect("zero-state component should instantiate");
    let options = live_session_options(1.0e-10, 1.0e-10, 1.0e-10, None)
        .expect("zero-state session options should construct");

    {
        let host = retained
            .lease(options.clone())
            .expect("the borrowed lease should initialize");
        let mut session = host
            .into_session(None)
            .expect("zero-state component selects the time-only plugin");
        let mut cursor = MeOutputCursor::empty();
        session
            .advance_to(0.25, &mut cursor)
            .expect("the borrowed session should mutate the retained instance");
        assert_eq!(session.time().to_bits(), 0.25_f64.to_bits());
    }

    let host = retained
        .into_lease(options)
        .expect("the consuming lease should restore before initialization");
    let session = host
        .into_session(None)
        .expect("the restored zero-state component selects the time-only plugin");
    assert_eq!(session.time().to_bits(), 0.125_f64.to_bits());
    assert_eq!(
        session.verification_component_point().0,
        0.125_f64.to_bits()
    );
}

#[test]
fn leased_host_derives_its_start_and_output_grid_from_the_pristine_component() {
    let model = crate::test_support::empty_binary64_first_product_model();
    let config = MeInstanceConfig::new("nonzero-start", 1.0e-10, 0.25, 1.0)
        .expect("the nonzero-start FMI instance request is checked");
    let mut retained = MeRetainedComponent::instantiate(
        MeModelSource::fixture(crate::test_support::fmi_component(model)),
        &config,
        MeExecutionSelection::Interpreter,
    )
    .expect("the event-free component instantiates at its sole start coordinate");
    let options = super::driver::batch_session_options(1.0, 0.75, 1.0e-10, 1.0e-10, 0.25, None)
        .expect("batch policy carries no independent start coordinate");
    let admission = retained
        .admit_batch(options)
        .expect("the complete component-issued batch is admitted before initialization");
    let batch = admission
        .into_batch()
        .expect("the admitted batch retains its host and cursor together");
    let mut batch = batch
        .into_session(None)
        .expect("the zero-state component selects the time-only plugin");
    batch
        .run_to_stop()
        .expect("the batch consumes its issuer-owned cursor");
    let result = batch.finish();
    assert_eq!(
        result.times.first().map(|time| time.to_bits()),
        Some(0.25_f64.to_bits())
    );
}

#[test]
fn invalid_complete_batch_grids_do_not_touch_the_retained_component() {
    let model = crate::test_support::empty_binary64_first_product_model();
    let config = MeInstanceConfig::new("batch-atomic", 1.0e-10, 1.0e20, 1.0e21)
        .expect("the large-coordinate FMI request is checked");
    let mut retained = MeRetainedComponent::instantiate(
        MeModelSource::fixture(crate::test_support::fmi_component(model)),
        &config,
        MeExecutionSelection::Interpreter,
    )
    .expect("the component instantiates at its pristine coordinate");
    let pristine = retained.verification_observable_state();

    let nonadvancing = super::driver::batch_session_options(
        f64::from_bits(1.0e20_f64.to_bits() + 1),
        1.0,
        1.0e-10,
        1.0e-10,
        1.0,
        None,
    )
    .expect("the locally checked policy cannot know the component start");
    assert!(retained.admit_batch(nonadvancing).is_err());
    assert_eq!(retained.verification_observable_state(), pristine);

    let oversized = super::driver::batch_session_options(
        1.0e21,
        1.0,
        1.0e-10,
        1.0e-10,
        f64::MIN_POSITIVE,
        None,
    )
    .expect("the locally checked policy is finite and positive");
    assert!(retained.admit_batch(oversized).is_err());
    assert_eq!(retained.verification_observable_state(), pristine);
}

#[test]
fn lease_rejects_a_defined_stop_behind_the_component_owned_start() {
    let model = crate::test_support::empty_binary64_first_product_model();
    let config = MeInstanceConfig::new("late-start", 1.0e-10, 0.5, 1.0)
        .expect("the late-start FMI instance request is checked");
    let mut retained = MeRetainedComponent::instantiate(
        MeModelSource::fixture(crate::test_support::fmi_component(model)),
        &config,
        MeExecutionSelection::Interpreter,
    )
    .expect("the event-free component instantiates");
    let mutating_options = live_session_options(1.0e-10, 1.0e-10, 0.1, None)
        .expect("the mutation witness policy is valid");
    {
        let host = retained
            .lease(mutating_options)
            .expect("the first borrowed lease initializes");
        let mut session = host
            .into_session(None)
            .expect("the zero-state component selects its time-only plugin");
        let mut cursor = MeOutputCursor::empty();
        session
            .advance_to(0.75, &mut cursor)
            .expect("the first borrowed lease mutates the retained component");
        assert_eq!(session.time().to_bits(), 0.75_f64.to_bits());
    }
    assert!(
        !retained.verification_is_pristine(),
        "dropping a successful lease does not implicitly reset its component"
    );
    let options = super::driver::batch_session_options(0.25, 0.25, 1.0e-10, 1.0e-10, 0.1, None)
        .expect("the unpaired host policy is locally well-formed");

    let error = match retained.lease(options) {
        Ok(_) => panic!("host construction must reject an impossible component-policy pairing"),
        Err(error) => error,
    };
    assert!(matches!(
        error,
        super::session::MeSessionError::Options { .. }
    ));
    assert!(
        retained.verification_is_pristine(),
        "even an invalid repeat lease restores before refusing its options"
    );

    let valid_options = super::driver::batch_session_options(1.0, 0.5, 1.0e-10, 1.0e-10, 0.1, None)
        .expect("the corrected host policy is locally well-formed");
    let host = retained
        .lease(valid_options)
        .expect("the rejected pairing must not poison the retained component");
    let session = host
        .into_session(None)
        .expect("the corrected zero-state host initializes normally");
    assert_eq!(session.time().to_bits(), 0.5_f64.to_bits());
    assert_eq!(session.verification_component_point().0, 0.5_f64.to_bits());
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

fn with_harmonic_variable_catalog(model: solve::SolveModel) -> solve::SolveModel {
    let provenance = rumoca_core::Span::from_offsets(
        rumoca_core::SourceId::from_source_name("fmi_me_harmonic_variables.mo"),
        1,
        2,
    );
    crate::test_support::with_explicit_real_scalar_catalog(
        model,
        vec![
            crate::test_support::RealScalarVariableFixture::state(
                1, "x", 0, 1.0, 1.0, true, provenance,
            ),
            crate::test_support::RealScalarVariableFixture::state(
                2, "v", 1, 0.0, 1.0, true, provenance,
            ),
        ],
        crate::test_support::direct_y_visible_rows([0, 1], provenance),
    )
}

/// `der(x) = v`, `der(v) = -4·x`: a pure ODE whose exact state Jacobian is the
/// constant `[[0, 1], [-4, 0]]`, so the expected directional derivative for any
/// seed is closed form and no tolerance is needed.
fn harmonic_oscillator() -> solve::SolveModel {
    harmonic_oscillator_with_discrete(solve::DiscreteSolveSystem::default())
}

fn harmonic_oscillator_with_discrete(discrete: solve::DiscreteSolveSystem) -> solve::SolveModel {
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
    let solve_layout = solve::SolveLayout {
        solver_maps: solve::SolverNameIndexMaps {
            names: vec!["x".to_string(), "v".to_string()],
            name_to_idx: IndexMap::from([("x".to_string(), 0), ("v".to_string(), 1)]),
            ..Default::default()
        },
        state_scalar_count: 2,
        ..Default::default()
    };
    let events = solve::SolveEventPartition::default();
    let clocks = solve::SolveClockPartition::default();
    let continuous = crate::test_support::ContinuousSystemFixture {
        derivative_rhs: solve::ComputeBlock::from_scalar_program_block(derivative),
        ..crate::test_support::ContinuousSystemFixture::empty()
    };
    let continuous = continuous.seal(&solve_layout, &discrete, &events, &clocks);
    let model = crate::test_support::custom_artifact_solve_model! {
        problem: crate::test_support::checked_solve_problem!(
            solve::VarLayout::from_parts(IndexMap::new(), 2, 0),
            solve_layout,
            continuous,
            solve::InitializationSolveSystem::empty(),
            discrete,
            events,
            clocks,
        )
        .expect("harmonic fixture satisfies the checked root contract"),
        artifacts: solve::SolveArtifactInputs {
            continuous: solve::ContinuousSolveArtifacts {
                full_jacobian_v: jacobian_v,
                ..Default::default()
            },
            ..solve::SolveArtifactInputs::empty()
        },
        initial_y: vec![1.0, 0.0],
        solver_nominals: vec![1.0, 1.0],
        ..empty_binary64_first_product_model()
    };
    with_harmonic_variable_catalog(model)
}

fn one_reinitializable_harmonic_state() -> solve::SolveModel {
    let provenance = rumoca_core::Span::from_offsets(
        rumoca_core::SourceId::from_source_name("fmi_me_reinit_action.mo"),
        1,
        2,
    )
    .require_provenance("reinit action fixture")
    .expect("reinit fixture span is source-backed");
    let reinit = solve::GuardedAssignmentProgram::checked(solve::GuardedAssignmentProgramInput {
        program: vec![
            solve::LinearOp::Const { dst: 0, value: 0.0 },
            solve::LinearOp::StoreOutput { src: 0 },
        ],
        provenance,
        target_ranges: vec![(solve::scalar_slot_y(0), 1)],
        role: solve::DiscreteRowRole::EventAction,
        pre_mode: solve::DiscreteEventPreMode::FollowCurrent,
        observation_refresh: false,
        integrator_history_effect: solve::IntegratorHistoryEffect::Restart,
        clock_owner: None,
    })
    .expect("reinit fixture has one checked guarded state target");
    let discrete = solve::DiscreteSolveSystem {
        guarded_assignments: vec![reinit],
        ..Default::default()
    };
    harmonic_oscillator_with_discrete(discrete)
}

/// A real two-state component whose checked initial event action requests
/// `terminateSimulation` before Continuous-Time Mode is entered.
fn initialization_termination_model() -> solve::SolveModel {
    let base = harmonic_oscillator();
    let solve_layout = solve::SolveLayout {
        compiled_parameter_len: 1,
        terminal_event_parameter_index: Some(0),
        ..base.problem().solve_layout().clone()
    };
    let discrete = base.problem().discrete().clone();
    let action_span = rumoca_core::Span::from_offsets(
        rumoca_core::SourceId::from_source_name("fmi_me_initial_terminate.mo"),
        1,
        2,
    );
    let events = solve::SolveEventPartition {
        action_conditions: block(
            vec![vec![
                solve::LinearOp::Const { dst: 0, value: 1.0 },
                solve::LinearOp::StoreOutput { src: 0 },
            ]],
            "fmi_me_initial_terminate_condition.mo",
        ),
        actions: vec![solve::SolveEventAction {
            kind: solve::SolveEventActionKind::Terminate,
            message: solve::SolveEventMessage {
                parts: vec![solve::SolveEventMessagePart::Text(
                    "terminated during initialization".to_owned(),
                )],
            },
            span: action_span,
            origin: "initial terminate fixture".to_owned(),
            clock_owner: None,
        }],
        has_terminal_event: true,
        ..base.problem().events().clone()
    };
    let clocks = base.problem().clocks().clone();
    let continuous =
        crate::test_support::ContinuousSystemFixture::from_system(base.problem().continuous());
    let continuous = continuous.seal(&solve_layout, &discrete, &events, &clocks);
    let problem = crate::test_support::checked_solve_problem!(
        solve::VarLayout::from_parts(IndexMap::new(), 2, 1),
        solve_layout,
        continuous,
        base.problem().initialization().clone(),
        discrete,
        events,
        clocks,
    )
    .expect("initial-termination fixture satisfies the checked root contract");
    crate::test_support::checked_solve_model! {
        problem: problem,
        parameters: vec![0.0],
        ..base
    }
}

fn nonlinear_right_limit_implicit_jvp() -> solve::ScalarProgramBlock {
    use solve::LinearOp::{Binary, Const, LoadSeed, LoadY, StoreOutput};
    let span = rumoca_core::Span::from_offsets(
        rumoca_core::SourceId::from_source_name("fmi_me_right_limit_implicit_jvp.mo"),
        1,
        2,
    );
    solve::ScalarProgramBlock::with_output_indices(
        vec![vec![
            Const { dst: 0, value: 2.0 },
            LoadY { dst: 1, index: 1 },
            Binary {
                dst: 2,
                op: solve::BinaryOp::Mul,
                lhs: 0,
                rhs: 1,
            },
            LoadSeed { dst: 3, index: 1 },
            Binary {
                dst: 4,
                op: solve::BinaryOp::Mul,
                lhs: 2,
                rhs: 3,
            },
            LoadSeed { dst: 5, index: 0 },
            Binary {
                dst: 6,
                op: solve::BinaryOp::Sub,
                lhs: 4,
                rhs: 5,
            },
            StoreOutput { src: 6 },
        ]],
        vec![span],
        vec![1],
    )
    .expect("right-limit implicit JVP matches the sparse implicit row owner")
}

fn with_right_limit_variable_catalog(model: solve::SolveModel) -> solve::SolveModel {
    let provenance = rumoca_core::Span::from_offsets(
        rumoca_core::SourceId::from_source_name("fmi_me_right_limit_variables.mo"),
        1,
        2,
    );
    crate::test_support::with_explicit_real_scalar_catalog(
        model,
        vec![
            crate::test_support::RealScalarVariableFixture::state(
                1, "x", 0, 4.0, 1.0, true, provenance,
            ),
            crate::test_support::RealScalarVariableFixture::algebraic(
                2, "a", 1, 2.0, 1.0, provenance,
            ),
        ],
        crate::test_support::direct_y_visible_rows([0, 1], provenance),
    )
}

fn nonlinear_right_limit_seed_model() -> solve::SolveModel {
    use solve::LinearOp::{Binary, LoadSeed, LoadY, StoreOutput};
    let derivative = block(
        vec![vec![LoadY { dst: 0, index: 1 }, StoreOutput { src: 0 }]],
        "fmi_me_right_limit_derivative.mo",
    );
    let implicit_span = rumoca_core::Span::from_offsets(
        rumoca_core::SourceId::from_source_name("fmi_me_right_limit_implicit.mo"),
        1,
        2,
    );
    let implicit = solve::ScalarProgramBlock::with_output_indices(
        vec![vec![
            LoadY { dst: 0, index: 1 },
            Binary {
                dst: 1,
                op: solve::BinaryOp::Mul,
                lhs: 0,
                rhs: 0,
            },
            LoadY { dst: 2, index: 0 },
            Binary {
                dst: 3,
                op: solve::BinaryOp::Sub,
                lhs: 1,
                rhs: 2,
            },
            StoreOutput { src: 3 },
        ]],
        vec![implicit_span],
        vec![1],
    )
    .expect("right-limit implicit program is computable");
    let implicit_jvp = nonlinear_right_limit_implicit_jvp();
    let derivative_jvp = block(
        vec![vec![LoadSeed { dst: 0, index: 1 }, StoreOutput { src: 0 }]],
        "fmi_me_right_limit_derivative_jvp.mo",
    );
    let solve_layout = solve::SolveLayout {
        solver_maps: solve::SolverNameIndexMaps {
            names: vec!["x".to_string(), "a".to_string()],
            name_to_idx: IndexMap::from([("x".to_string(), 0), ("a".to_string(), 1)]),
            ..Default::default()
        },
        state_scalar_count: 1,
        algebraic_scalar_count: 1,
        ..Default::default()
    };
    let discrete = solve::DiscreteSolveSystem::default();
    let events = solve::SolveEventPartition::default();
    let clocks = solve::SolveClockPartition::default();
    let continuous = crate::test_support::ContinuousSystemFixture {
        implicit_rhs: solve::ComputeBlock::from_scalar_program_block(implicit),
        implicit_row_targets: vec![None, Some(solve::scalar_slot_y(1))],
        derivative_rhs: solve::ComputeBlock::from_scalar_program_block(derivative),
        algebraic_projection_plan: solve::AlgebraicProjectionPlan {
            blocks: vec![solve::AlgebraicProjectionBlock {
                rows: vec![1],
                y_indices: vec![1],
                tearing: None,
            }],
        },
        ..crate::test_support::ContinuousSystemFixture::empty()
    };
    let continuous = continuous.seal(&solve_layout, &discrete, &events, &clocks);
    let model = crate::test_support::custom_artifact_solve_model! {
        problem: crate::test_support::checked_solve_problem!(
            solve::VarLayout::from_parts(Default::default(), 2, 0),
            solve_layout,
            continuous,
            solve::InitializationSolveSystem::empty(),
            discrete,
            events,
            clocks,
        )
        .expect("right-limit fixture satisfies the checked root contract"),
        artifacts: solve::SolveArtifactInputs {
            continuous: solve::ContinuousSolveArtifacts {
                implicit_jacobian_v: solve::ComputeBlock::from_scalar_program_block(
                    implicit_jvp.clone(),
                ),
                implicit_jacobian_v_scalar: implicit_jvp,
                full_jacobian_v: derivative_jvp,
                ..Default::default()
            },
            ..solve::SolveArtifactInputs::empty()
        },
        initial_y: vec![4.0, 2.0],
        solver_nominals: vec![1.0, 1.0],
        ..empty_binary64_first_product_model()
    };
    with_right_limit_variable_catalog(model)
}

#[test]
fn event_right_limit_derivative_retains_the_full_algebraic_seed() {
    let model = nonlinear_right_limit_seed_model();
    let model = std::sync::Arc::new(model);
    let runtime = crate::runtime::solve_runtime::SolveRuntime::new(std::sync::Arc::clone(&model))
        .expect("right-limit seed fixture should prepare");
    let settle = crate::runtime::solve_runtime::AlgebraicSettle {
        tol: 1.0e-12,
        max_iters: 32,
    };

    let derivative =
        event_right_limit_state_derivatives(&runtime, model.initial_y(), 0.0, &[4.0], &[], settle)
            .expect("the retained positive algebraic branch should remain solvable");
    assert_eq!(derivative, vec![2.0]);

    let error =
        event_right_limit_state_derivatives(&runtime, &[4.0, 0.0], 0.0, &[4.0], &[], settle)
            .expect_err("a zeroed algebraic seed is singular for a² - x at a = 0");
    assert!(
        error
            .to_string()
            .contains("algebraic projection did not converge"),
        "{error}"
    );
    assert!(error.to_string().contains("target=a"), "{error}");
}

fn strict_root_relation_memory(initial_state: f64) -> solve::SolveModel {
    let derivative = block(
        vec![vec![
            solve::LinearOp::Const { dst: 0, value: 1.0 },
            solve::LinearOp::StoreOutput { src: 0 },
        ]],
        "fmi_me_strict_root_derivative.mo",
    );
    let root = block(
        vec![vec![
            solve::LinearOp::Const { dst: 0, value: 0.0 },
            solve::LinearOp::LoadY { dst: 1, index: 0 },
            solve::LinearOp::Binary {
                dst: 2,
                op: solve::BinaryOp::Sub,
                lhs: 0,
                rhs: 1,
            },
            solve::LinearOp::StoreOutput { src: 2 },
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
    let solve_layout = solve::SolveLayout {
        solver_maps: solve::SolverNameIndexMaps {
            names: vec!["x".to_string()],
            name_to_idx: IndexMap::from([("x".to_string(), 0)]),
            ..Default::default()
        },
        variable_storage_runs: vec![solve::SolveVariableStorageRun {
            base: solve::SolveStorageCoordinate::Y(0),
            scalar_count: 1,
            role: solve::SolveVariableStorageRole::State,
            value_kind: solve::SolveVariableValueKind::Real,
        }],
        variable_declarations: vec![solve::SolveVariableDeclaration::new(
            solve::SolveVariableStorageRole::State,
            solve::SolveVariableValueKind::Real,
        )],
        state_scalar_count: 1,
        compiled_parameter_len: 1,
        ..Default::default()
    };
    let discrete = solve::DiscreteSolveSystem {
        rhs: condition_memory,
        update_targets: vec![solve::scalar_slot_p(0)],
        row_roles: vec![solve::DiscreteRowRole::ConditionMemory],
        pre_modes: vec![solve::DiscreteEventPreMode::FollowCurrent],
        observation_refresh: vec![false],
        integrator_history_effects: vec![solve::IntegratorHistoryEffect::Preserve],
        clock_owners: vec![None],
        ..Default::default()
    };
    let events = solve::SolveEventPartition {
        root_conditions: root,
        root_relation_memory_targets: vec![Some(solve::scalar_slot_p(0))],
        root_zero_domains: vec![solve::RootZeroDomain::Positive],
        root_relation_refresh_roles: vec![solve::RootRelationRefreshRole::Frozen],
        condition_memory_parameter_indices: vec![0],
        ..Default::default()
    };
    let clocks = solve::SolveClockPartition::default();
    let continuous = crate::test_support::ContinuousSystemFixture {
        derivative_rhs: solve::ComputeBlock::from_scalar_program_block(derivative),
        ..crate::test_support::ContinuousSystemFixture::empty()
    };
    let continuous = continuous.seal(&solve_layout, &discrete, &events, &clocks);
    let problem = crate::test_support::checked_solve_problem!(
        solve::VarLayout::from_parts(IndexMap::new(), 1, 1),
        solve_layout,
        continuous,
        solve::InitializationSolveSystem::empty(),
        discrete,
        events,
        clocks,
    )
    .expect("strict-root fixture satisfies the checked root contract");
    let provenance = strict_root_relation_provenance();
    let variable_entries = strict_root_relation_variable_entries(initial_state, provenance);
    crate::test_support::checked_solve_model! {
        problem: problem,
        initial_y: vec![initial_state],
        solver_nominals: vec![1.0],
        parameters: vec![0.0],
        visible_value_rows: crate::test_support::direct_y_visible_rows([0], provenance),
        variable_entries: variable_entries,
        ..empty_binary64_first_product_model()
    }
}

/// The source span every strict-root fixture variable is declared at.
fn strict_root_relation_provenance() -> rumoca_core::Span {
    rumoca_core::Span::from_offsets(
        rumoca_core::SourceId::from_source_name("fmi_me_strict_root_variables.mo"),
        1,
        2,
    )
}

/// The Real scalar catalog entries for the strict-root relation model.
/// Split out for length; provenance and every entry are unchanged.
fn strict_root_relation_variable_entries(
    initial_state: f64,
    provenance: rumoca_core::Span,
) -> Vec<solve::SolveVariableCatalogSourceEntry> {
    crate::test_support::explicit_real_scalar_catalog_entries(vec![
        crate::test_support::RealScalarVariableFixture::state(
            1,
            "x",
            0,
            initial_state,
            1.0,
            true,
            provenance,
        ),
    ])
}

/// One continuous state `x' = 1`, initialized to `0.5`, and one dynamic
/// time-event row whose deadline reads `x`.
///
/// The non-empty dynamic row makes the runtime's next-event evaluation a
/// solver-vector consumer, so every `fmi3UpdateDiscreteStates` on this model
/// reads the construction-reserved event solver workspace.
fn dynamic_deadline_model() -> solve::SolveModel {
    dynamic_deadline_model_with_root(false)
}

fn equal_width_root_deadline_model() -> solve::SolveModel {
    dynamic_deadline_model_with_root(true)
}

fn dynamic_deadline_model_with_root(include_root: bool) -> solve::SolveModel {
    let derivative = block(
        vec![vec![
            solve::LinearOp::Const { dst: 0, value: 1.0 },
            solve::LinearOp::StoreOutput { src: 0 },
        ]],
        "fmi_me_dynamic_deadline_derivative.mo",
    );
    let deadline = block(
        vec![vec![
            solve::LinearOp::LoadY { dst: 0, index: 0 },
            solve::LinearOp::StoreOutput { src: 0 },
        ]],
        "fmi_me_dynamic_deadline_row.mo",
    );
    let (root_conditions, root_relation_memory_targets, root_zero_domains, root_roles) =
        if include_root {
            (
                block(
                    vec![vec![
                        solve::LinearOp::Const { dst: 0, value: 0.0 },
                        solve::LinearOp::LoadY { dst: 1, index: 0 },
                        solve::LinearOp::Binary {
                            dst: 2,
                            op: solve::BinaryOp::Sub,
                            lhs: 0,
                            rhs: 1,
                        },
                        solve::LinearOp::StoreOutput { src: 2 },
                    ]],
                    "fmi_me_equal_width_root_row.mo",
                ),
                vec![None],
                vec![solve::RootZeroDomain::Previous],
                vec![solve::RootRelationRefreshRole::Frozen],
            )
        } else {
            (
                solve::ScalarProgramBlock::with_program_spans(Vec::new(), Vec::new())
                    .expect("an explicitly empty root block constructs"),
                Vec::new(),
                Vec::new(),
                Vec::new(),
            )
        };
    let solve_layout = solve::SolveLayout {
        solver_maps: solve::SolverNameIndexMaps {
            names: vec!["x".to_string()],
            name_to_idx: IndexMap::from([("x".to_string(), 0)]),
            ..Default::default()
        },
        state_scalar_count: 1,
        ..Default::default()
    };
    let discrete = solve::DiscreteSolveSystem::default();
    let events = solve::SolveEventPartition {
        root_conditions,
        root_relation_memory_targets,
        root_zero_domains,
        root_relation_refresh_roles: root_roles,
        dynamic_time_event_rhs: deadline,
        ..Default::default()
    };
    let clocks = solve::SolveClockPartition::default();
    let continuous = crate::test_support::ContinuousSystemFixture {
        derivative_rhs: solve::ComputeBlock::from_scalar_program_block(derivative),
        ..crate::test_support::ContinuousSystemFixture::empty()
    };
    let continuous = continuous.seal(&solve_layout, &discrete, &events, &clocks);
    let model = crate::test_support::checked_solve_model! {
        problem: crate::test_support::checked_solve_problem!(
            solve::VarLayout::from_parts(IndexMap::new(), 1, 0),
            solve_layout,
            continuous,
            solve::InitializationSolveSystem::empty(),
            discrete,
            events,
            clocks,
        )
        .expect("dynamic-deadline fixture satisfies the checked contract"),
        initial_y: vec![0.5],
        solver_nominals: vec![1.0],
        ..empty_binary64_first_product_model()
    };
    with_visible_unit_state(model, "x", 0.5, "fmi_me_dynamic_deadline_variables.mo")
}

fn static_true_relation_memory() -> solve::SolveModel {
    let derivative = zero_derivative("fmi_me_static_relation_derivative.mo");
    let root = block(
        vec![vec![
            solve::LinearOp::Const {
                dst: 0,
                value: -1.0,
            },
            solve::LinearOp::StoreOutput { src: 0 },
        ]],
        "fmi_me_static_relation_root.mo",
    );
    let solve_layout = solve::SolveLayout {
        solver_maps: solve::SolverNameIndexMaps {
            names: vec!["state".to_string()],
            name_to_idx: IndexMap::from([("state".to_string(), 0)]),
            ..Default::default()
        },
        state_scalar_count: 1,
        compiled_parameter_len: 1,
        relation_memory_parameter_indices: vec![0],
        ..Default::default()
    };
    let discrete = solve::DiscreteSolveSystem::default();
    let events = solve::SolveEventPartition {
        root_conditions: root,
        root_relation_memory_targets: vec![Some(solve::scalar_slot_p(0))],
        root_zero_domains: vec![solve::RootZeroDomain::Previous],
        root_relation_refresh_roles: vec![solve::RootRelationRefreshRole::Frozen],
        ..Default::default()
    };
    let clocks = solve::SolveClockPartition::default();
    let continuous = crate::test_support::ContinuousSystemFixture {
        derivative_rhs: solve::ComputeBlock::from_scalar_program_block(derivative),
        ..crate::test_support::ContinuousSystemFixture::empty()
    };
    let continuous = continuous.seal(&solve_layout, &discrete, &events, &clocks);
    let model = crate::test_support::checked_solve_model! {
        problem: crate::test_support::checked_solve_problem!(
            solve::VarLayout::from_parts(IndexMap::new(), 1, 1),
            solve_layout,
            continuous,
            solve::InitializationSolveSystem::empty(),
            discrete,
            events,
            clocks,
        )
        .expect("static-root fixture satisfies the checked root contract"),
        initial_y: vec![0.0],
        solver_nominals: vec![1.0],
        parameters: vec![1.0],
        ..empty_binary64_first_product_model()
    };
    with_visible_unit_state(model, "state", 0.0, "fmi_me_static_relation_variables.mo")
}

fn post_pre_relation_cycle() -> solve::SolveModel {
    let derivative = block(
        vec![vec![
            solve::LinearOp::Const { dst: 0, value: 0.0 },
            solve::LinearOp::StoreOutput { src: 0 },
        ]],
        "fmi_me_post_pre_relation_cycle_derivative.mo",
    );
    let runtime_assignment = block(
        vec![vec![
            solve::LinearOp::LoadP { dst: 0, index: 0 },
            solve::LinearOp::Const { dst: 1, value: 0.5 },
            solve::LinearOp::Compare {
                dst: 2,
                op: solve::CompareOp::Gt,
                lhs: 0,
                rhs: 1,
            },
            solve::LinearOp::Const { dst: 3, value: 1.0 },
            solve::LinearOp::Const {
                dst: 4,
                value: -1.0,
            },
            solve::LinearOp::Select {
                dst: 5,
                cond: 2,
                if_true: 3,
                if_false: 4,
            },
            solve::LinearOp::StoreOutput { src: 5 },
        ]],
        "fmi_me_post_pre_relation_cycle_assignment.mo",
    );
    let root = block(
        vec![vec![
            solve::LinearOp::LoadP { dst: 0, index: 1 },
            solve::LinearOp::StoreOutput { src: 0 },
        ]],
        "fmi_me_post_pre_relation_cycle_root.mo",
    );
    let solve_layout = solve::SolveLayout {
        solver_maps: solve::SolverNameIndexMaps {
            names: vec!["state".to_string()],
            name_to_idx: IndexMap::from([("state".to_string(), 0)]),
            ..Default::default()
        },
        state_scalar_count: 1,
        compiled_parameter_len: 2,
        relation_memory_parameter_indices: vec![0],
        ..Default::default()
    };
    let discrete = solve::DiscreteSolveSystem {
        runtime_assignment_rhs: runtime_assignment,
        runtime_assignment_targets: vec![solve::scalar_slot_p(1)],
        runtime_assignment_roles: vec![solve::RuntimeAssignmentRole::RelationEvaluating],
        ..Default::default()
    };
    let events = solve::SolveEventPartition {
        root_conditions: root,
        root_relation_memory_targets: vec![Some(solve::scalar_slot_p(0))],
        root_zero_domains: vec![solve::RootZeroDomain::Previous],
        root_relation_refresh_roles: vec![solve::RootRelationRefreshRole::Frozen],
        ..Default::default()
    };
    let clocks = solve::SolveClockPartition::default();
    let continuous = crate::test_support::ContinuousSystemFixture {
        derivative_rhs: solve::ComputeBlock::from_scalar_program_block(derivative),
        ..crate::test_support::ContinuousSystemFixture::empty()
    };
    let continuous = continuous.seal(&solve_layout, &discrete, &events, &clocks);
    let model = crate::test_support::checked_solve_model! {
        problem: crate::test_support::checked_solve_problem!(
            solve::VarLayout::from_parts(IndexMap::new(), 1, 2),
            solve_layout,
            continuous,
            solve::InitializationSolveSystem::empty(),
            discrete,
            events,
            clocks,
        )
        .expect("post-pre fixture satisfies the checked root contract"),
        initial_y: vec![0.0],
        solver_nominals: vec![1.0],
        parameters: vec![0.0, 0.0],
        ..empty_binary64_first_product_model()
    };
    with_visible_unit_state(model, "state", 0.0, "fmi_me_post_pre_variables.mo")
}

fn post_commit_alias_with_frozen_parameter_root() -> solve::SolveModel {
    let derivative = zero_derivative("fmi_me_post_commit_frozen_root_derivative.mo");
    let PostCommitFrozenRootPrograms {
        alias,
        runtime_assignments,
        roots,
    } = post_commit_frozen_root_programs();
    let solve_layout = solve::SolveLayout {
        solver_maps: solve::SolverNameIndexMaps {
            names: vec!["state".to_string()],
            name_to_idx: IndexMap::from([("state".to_string(), 0)]),
            ..Default::default()
        },
        state_scalar_count: 1,
        compiled_parameter_len: 4,
        relation_memory_parameter_indices: vec![0, 1],
        ..Default::default()
    };
    let discrete = solve::DiscreteSolveSystem {
        runtime_assignment_rhs: runtime_assignments,
        runtime_assignment_targets: vec![solve::scalar_slot_p(2), solve::scalar_slot_p(3)],
        runtime_assignment_roles: vec![
            solve::RuntimeAssignmentRole::RelationFree,
            solve::RuntimeAssignmentRole::RelationEvaluating,
        ],
        post_commit_assignment_rhs: alias,
        post_commit_assignment_targets: vec![solve::scalar_slot_p(2)],
        post_commit_assignment_runtime_rows: vec![0],
        ..Default::default()
    };
    let events = solve::SolveEventPartition {
        root_conditions: roots,
        root_relation_memory_targets: vec![
            Some(solve::scalar_slot_p(0)),
            Some(solve::scalar_slot_p(1)),
        ],
        root_zero_domains: vec![
            solve::RootZeroDomain::Previous,
            solve::RootZeroDomain::Previous,
        ],
        root_relation_refresh_roles: vec![
            solve::RootRelationRefreshRole::Frozen,
            solve::RootRelationRefreshRole::Frozen,
        ],
        ..Default::default()
    };
    let clocks = solve::SolveClockPartition::default();
    let continuous = crate::test_support::ContinuousSystemFixture {
        derivative_rhs: solve::ComputeBlock::from_scalar_program_block(derivative),
        ..crate::test_support::ContinuousSystemFixture::empty()
    };
    let continuous = continuous.seal(&solve_layout, &discrete, &events, &clocks);
    let model = crate::test_support::checked_solve_model! {
        problem: crate::test_support::checked_solve_problem!(
            solve::VarLayout::from_parts(IndexMap::new(), 1, 4),
            solve_layout,
            continuous,
            solve::InitializationSolveSystem::empty(),
            discrete,
            events,
            clocks,
        )
        .expect("post-commit fixture satisfies the checked root contract"),
        initial_y: vec![0.0],
        solver_nominals: vec![1.0],
        parameters: vec![1.0, 1.0, -1.0, -1.0],
        ..empty_binary64_first_product_model()
    };
    with_visible_unit_state(model, "state", 0.0, "fmi_me_post_commit_variables.mo")
}

struct PostCommitFrozenRootPrograms {
    alias: solve::ScalarProgramBlock,
    runtime_assignments: solve::ScalarProgramBlock,
    roots: solve::ScalarProgramBlock,
}

fn post_commit_frozen_root_programs() -> PostCommitFrozenRootPrograms {
    let alias_program = vec![
        solve::LinearOp::LoadP { dst: 0, index: 0 },
        solve::LinearOp::StoreOutput { src: 0 },
    ];
    let alias = block(
        vec![alias_program.clone()],
        "fmi_me_post_commit_frozen_root_runtime.mo",
    );
    let runtime_assignments = block(
        vec![
            alias_program,
            vec![
                solve::LinearOp::LoadP { dst: 0, index: 2 },
                solve::LinearOp::Const { dst: 1, value: 0.0 },
                solve::LinearOp::Compare {
                    dst: 2,
                    op: solve::CompareOp::Gt,
                    lhs: 0,
                    rhs: 1,
                },
                solve::LinearOp::StoreOutput { src: 2 },
            ],
        ],
        "fmi_me_post_commit_frozen_root_runtime.mo",
    );
    let roots = block(
        vec![
            vec![
                solve::LinearOp::LoadP { dst: 0, index: 3 },
                solve::LinearOp::StoreOutput { src: 0 },
            ],
            vec![
                solve::LinearOp::LoadP { dst: 0, index: 2 },
                solve::LinearOp::StoreOutput { src: 0 },
            ],
        ],
        "fmi_me_post_commit_frozen_root_conditions.mo",
    );
    PostCommitFrozenRootPrograms {
        alias,
        runtime_assignments,
        roots,
    }
}

fn zero_derivative(name: &'static str) -> solve::ScalarProgramBlock {
    block(
        vec![vec![
            solve::LinearOp::Const { dst: 0, value: 0.0 },
            solve::LinearOp::StoreOutput { src: 0 },
        ]],
        name,
    )
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

fn instantiate(model: solve::SolveModel) -> SolveMeKernel {
    SolveMeKernel::instantiate(
        MeModelSource::fixture(crate::test_support::fmi_component(model)),
        &MeInstanceConfig::new("fmi-me-test", 1.0e-10, 0.0, 1.0)
            .expect("fixture instance configuration constructs"),
    )
    .expect("fixture instantiates")
}

#[test]
fn callback_caches_require_the_exact_fmi_coordinate() {
    let model = harmonic_oscillator();
    let kernel = instantiate(model);
    let time = 0.25_f64;
    let adjacent_time = f64::from_bits(time.to_bits() + 1);
    let state = [2.0, 3.0];
    let adjacent_state = [f64::from_bits(2.0f64.to_bits() + 1), 3.0];

    kernel.cache_derivative(time, &state, &[4.0, 5.0]);
    assert_eq!(kernel.cached_derivative(time, &state), Some(vec![4.0, 5.0]));
    assert_eq!(kernel.cached_derivative(adjacent_time, &state), None);
    assert_eq!(kernel.cached_derivative(time, &adjacent_state), None);
}

#[test]
fn repeated_directional_seeds_reuse_only_the_exact_settled_coordinate() {
    let model = nonlinear_right_limit_seed_model();
    let mut kernel = instantiate(model);
    kernel
        .enter_initialization_mode(0.0)
        .expect("enter initialization");
    kernel
        .exit_initialization_mode()
        .expect("exit initialization");
    kernel
        .update_discrete_states()
        .expect("settle the initial event");
    kernel
        .enter_continuous_time_mode()
        .expect("enter continuous time");
    let state = [4.0];
    let mut derivative = vec![0.0; kernel.model_description().continuous_state_count];
    kernel
        .get_continuous_state_derivatives(&mut derivative)
        .expect("the nonlinear algebraic coordinate should settle");
    assert_eq!(derivative, vec![2.0]);
    assert!(kernel.verification_continuous_linearization_cache_matches(0.0, &state, &[]));

    let (unknowns, knowns) = state_directional_batches(&kernel);
    let mut sensitivity = vec![f64::NAN];
    kernel
        .get_directional_derivative(&unknowns, &knowns, &[1.0], &mut sensitivity)
        .expect("the cached exact coordinate should support another seed");
    assert_eq!(sensitivity, vec![0.25]);

    let adjacent_time = f64::from_bits(1);
    kernel
        .set_time(MeTime::at(adjacent_time))
        .expect("the adjacent finite time is a legal FMI coordinate");
    assert!(!kernel.verification_continuous_linearization_cache_matches(
        adjacent_time,
        &state,
        &[]
    ));
    kernel
        .get_directional_derivative(&unknowns, &knowns, &[1.0], &mut sensitivity)
        .expect("a coordinate miss must fall back to checked settling");
    assert_eq!(sensitivity, vec![0.25]);
    assert!(kernel.verification_continuous_linearization_cache_matches(adjacent_time, &state, &[]));
}

#[test]
fn roots_refresh_from_the_exact_derivative_algebraic_branch() {
    let base = nonlinear_right_limit_seed_model();
    let layout = base.problem().layout().clone();
    let solve_layout = base.problem().solve_layout().clone();
    let continuous =
        crate::test_support::ContinuousSystemFixture::from_system(base.problem().continuous());
    let initialization = base.problem().initialization().clone();
    let discrete = base.problem().discrete().clone();
    let events = solve::SolveEventPartition {
        root_conditions: block(
            vec![vec![
                solve::LinearOp::LoadY { dst: 0, index: 1 },
                solve::LinearOp::StoreOutput { src: 0 },
            ]],
            "fmi_me_cached_root_branch.mo",
        ),
        root_relation_memory_targets: vec![None],
        root_zero_domains: vec![solve::RootZeroDomain::Previous],
        root_relation_refresh_roles: vec![solve::RootRelationRefreshRole::AlgebraicDependent],
        ..Default::default()
    };
    let clocks = base.problem().clocks().clone();
    let continuous = continuous.seal(&solve_layout, &discrete, &events, &clocks);
    let problem = crate::test_support::checked_solve_problem!(
        layout,
        solve_layout,
        continuous,
        initialization,
        discrete,
        events,
        clocks,
    )
    .expect("cached-root fixture satisfies the checked root contract");
    let model = crate::test_support::checked_solve_model! { problem: problem, ..base };
    let mut kernel = SolveMeKernel::instantiate(
        MeModelSource::fixture(crate::test_support::fmi_component(model)),
        &MeInstanceConfig::new("fmi-me-cached-root-test", 1.0e-10, 0.0, 1.0)
            .expect("cached-root instance configuration constructs"),
    )
    .expect("cached-root fixture instantiates");
    kernel
        .enter_initialization_mode(0.0)
        .expect("enter initialization");
    kernel
        .exit_initialization_mode()
        .expect("exit initialization");
    kernel
        .update_discrete_states()
        .expect("settle the initial event");
    kernel
        .enter_continuous_time_mode()
        .expect("enter continuous time");
    kernel.verification_cache_continuous_linearization(0.0, &[4.0], &[], &[4.0, -2.0]);
    let mut indicators = vec![0.0; kernel.model_description().event_indicator_count];

    kernel
        .get_event_indicators(&mut indicators)
        .expect("the complete root refresh should retain the derivative's settled branch");

    assert_eq!(indicators, vec![-2.0]);
}

#[test]
fn rejected_lifecycle_transitions_leave_the_legal_path_available() {
    let model = harmonic_oscillator();
    let mut kernel = instantiate(model);

    let error = kernel
        .exit_initialization_mode()
        .expect_err("initialization cannot be exited before it is entered");
    assert_eq!(error.stage(), Some(MeStage::Initialization));
    assert!(matches!(error.kind(), MeError::Contract { .. }));

    kernel
        .enter_initialization_mode(0.0)
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
fn discrete_update_reports_the_complete_fmi_output_set() {
    let base = harmonic_oscillator();
    let solve_layout = base.problem().solve_layout().clone();
    let continuous =
        crate::test_support::ContinuousSystemFixture::from_system(base.problem().continuous());
    let discrete = base.problem().discrete().clone();
    let events = solve::SolveEventPartition {
        scheduled_time_events: vec![0.5],
        ..base.problem().events().clone()
    };
    let clocks = base.problem().clocks().clone();
    let continuous = continuous.seal(&solve_layout, &discrete, &events, &clocks);
    let problem = crate::test_support::checked_solve_problem!(
        base.problem().layout().clone(),
        solve_layout,
        continuous,
        base.problem().initialization().clone(),
        discrete,
        events,
        clocks,
    )
    .expect("scheduled-event fixture satisfies the checked root contract");
    let model = crate::test_support::checked_solve_model! { problem: problem, ..base };
    let mut kernel = instantiate(model);

    kernel
        .enter_initialization_mode(0.0)
        .expect("enter initialization");
    kernel
        .exit_initialization_mode()
        .expect("exit initialization");
    let discrete = kernel
        .update_discrete_states()
        .expect("initial discrete update");

    assert!(!discrete.discrete_states_need_update);
    assert!(discrete.terminate_simulation.is_none());
    assert!(
        !discrete.values_of_continuous_states_changed,
        "an event-free initial update must report that its state vector is unchanged"
    );
    assert!(!discrete.nominals_of_continuous_states_changed);
    assert_eq!(
        discrete.next_event_time.map(f64::to_bits),
        Some(0.5f64.to_bits())
    );
}

#[test]
fn continuous_state_change_flag_uses_the_exact_fmi_state_vector() {
    assert!(!continuous_state_values_changed(&[1.0, -2.0], &[1.0, -2.0]));
    assert!(continuous_state_values_changed(&[1.0, -2.0], &[1.0, -3.0]));
    assert!(continuous_state_values_changed(&[0.0], &[-0.0]));
    assert!(continuous_state_values_changed(&[1.0], &[1.0, 2.0]));
}

fn harmonic_oscillator_with_input() -> solve::SolveModel {
    let base = harmonic_oscillator();
    let solve_layout = solve::SolveLayout {
        compiled_parameter_len: 1,
        input_scalar_names: vec!["u".to_string()],
        ..base.problem().solve_layout().clone()
    };
    let continuous =
        crate::test_support::ContinuousSystemFixture::from_system(base.problem().continuous());
    let discrete = base.problem().discrete().clone();
    let events = base.problem().events().clone();
    let clocks = base.problem().clocks().clone();
    let continuous = continuous.seal(&solve_layout, &discrete, &events, &clocks);
    let problem = crate::test_support::checked_solve_problem!(
        solve::VarLayout::from_parts(IndexMap::new(), 2, 1),
        solve_layout,
        continuous,
        base.problem().initialization().clone(),
        discrete,
        events,
        clocks,
    )
    .expect("input oscillator fixture satisfies the checked root contract");
    let model = crate::test_support::checked_solve_model! {
        problem: problem,
        parameters: vec![1.0],
        ..base
    };
    let provenance = rumoca_core::Span::from_offsets(
        rumoca_core::SourceId::from_source_name("fmi_me_harmonic_input.mo"),
        1,
        2,
    );
    crate::test_support::with_explicit_real_scalar_catalog(
        model,
        vec![
            crate::test_support::RealScalarVariableFixture::state(
                1, "x", 0, 1.0, 1.0, true, provenance,
            ),
            crate::test_support::RealScalarVariableFixture::state(
                2, "v", 1, 0.0, 1.0, true, provenance,
            ),
            crate::test_support::RealScalarVariableFixture::external_input(
                3, "u", 0, 1.0, provenance,
            ),
        ],
        block(
            vec![
                vec![
                    solve::LinearOp::LoadY { dst: 0, index: 0 },
                    solve::LinearOp::StoreOutput { src: 0 },
                ],
                vec![
                    solve::LinearOp::LoadY { dst: 0, index: 1 },
                    solve::LinearOp::StoreOutput { src: 0 },
                ],
                vec![
                    solve::LinearOp::LoadP { dst: 0, index: 0 },
                    solve::LinearOp::StoreOutput { src: 0 },
                ],
            ],
            "fmi_me_harmonic_input_visible.mo",
        ),
    )
}

fn aggregate_float64_input_model() -> solve::SolveModel {
    let provenance = rumoca_core::Span::from_offsets(
        rumoca_core::SourceId::from_source_name("fmi_me_aggregate_input.mo"),
        1,
        2,
    );
    let base = empty_binary64_first_product_model();
    let solve_layout = solve::SolveLayout {
        compiled_parameter_len: 3,
        input_scalar_names: vec!["u[1]".to_string(), "u[2]".to_string(), "s".to_string()],
        variable_storage_runs: vec![
            solve::SolveVariableStorageRun {
                base: solve::SolveStorageCoordinate::P(0),
                scalar_count: 2,
                role: solve::SolveVariableStorageRole::ExternalInput,
                value_kind: solve::SolveVariableValueKind::Real,
            },
            solve::SolveVariableStorageRun {
                base: solve::SolveStorageCoordinate::P(2),
                scalar_count: 1,
                role: solve::SolveVariableStorageRole::ExternalInput,
                value_kind: solve::SolveVariableValueKind::Real,
            },
        ],
        variable_declarations: vec![
            solve::SolveVariableDeclaration::new(
                solve::SolveVariableStorageRole::ExternalInput,
                solve::SolveVariableValueKind::Real,
            ),
            solve::SolveVariableDeclaration::new(
                solve::SolveVariableStorageRole::ExternalInput,
                solve::SolveVariableValueKind::Real,
            ),
        ],
        ..Default::default()
    };
    let discrete = solve::DiscreteSolveSystem::default();
    let events = solve::SolveEventPartition::default();
    let clocks = solve::SolveClockPartition::default();
    let continuous = crate::test_support::ContinuousSystemFixture::empty().seal(
        &solve_layout,
        &discrete,
        &events,
        &clocks,
    );
    let problem = crate::test_support::checked_solve_problem!(
        solve::VarLayout::from_parts(IndexMap::new(), 0, 3),
        solve_layout,
        continuous,
        solve::InitializationSolveSystem::empty(),
        discrete,
        events,
        clocks,
    )
    .expect("aggregate-input fixture satisfies the checked Solve contract");
    let variable_entries = aggregate_float64_input_variable_entries(provenance);
    crate::test_support::checked_solve_model! {
        problem: problem,
        parameters: vec![1.0, 2.0, 3.0],
        visible_value_rows: crate::test_support::direct_p_visible_rows([0, 1, 2], provenance),
        variable_entries: variable_entries,
        ..base
    }
}

/// The Real scalar catalog entries for the aggregate Float64 input model.
/// Split out for length; every entry and evaluated value is unchanged.
fn aggregate_float64_input_variable_entries(
    provenance: rumoca_core::Span,
) -> Vec<solve::SolveVariableCatalogSourceEntry> {
    vec![
        (
            solve::SolveVariableSource::new(
                rumoca_core::SourceOccurrenceId::try_from(rumoca_core::InstanceId::new(1))
                    .expect("fixture source occurrence is explicitly nonzero"),
                "u".to_string(),
                vec![2],
                vec!["u[1]".to_string(), "u[2]".to_string()],
                provenance,
            ),
            solve::SolveVariableSourceAttributes::new(
                solve::SolveVariableCausality::Input,
                solve::SolveVariableVariability::Continuous,
                false,
                None,
                None,
                rumoca_core::Fixity::Free,
            ),
            solve::SolveVariableEvaluatedValues::new(Some(vec![1.0, 2.0]), None, None, None),
        ),
        (
            solve::SolveVariableSource::new(
                rumoca_core::SourceOccurrenceId::try_from(rumoca_core::InstanceId::new(2))
                    .expect("fixture source occurrence is explicitly nonzero"),
                "s".to_string(),
                Vec::new(),
                vec!["s".to_string()],
                provenance,
            ),
            solve::SolveVariableSourceAttributes::new(
                solve::SolveVariableCausality::Input,
                solve::SolveVariableVariability::Continuous,
                false,
                None,
                None,
                rumoca_core::Fixity::Free,
            ),
            solve::SolveVariableEvaluatedValues::new(Some(vec![3.0]), None, None, None),
        ),
    ]
}

fn harmonic_oscillator_with_scheduled_event(event_time: f64) -> solve::SolveModel {
    let base = harmonic_oscillator();
    let solve_layout = base.problem().solve_layout().clone();
    let discrete = base.problem().discrete().clone();
    let events = solve::SolveEventPartition {
        scheduled_time_events: vec![event_time],
        ..base.problem().events().clone()
    };
    let clocks = base.problem().clocks().clone();
    let continuous =
        crate::test_support::ContinuousSystemFixture::from_system(base.problem().continuous())
            .seal(&solve_layout, &discrete, &events, &clocks);
    let problem = crate::test_support::checked_solve_problem!(
        base.problem().layout().clone(),
        solve_layout,
        continuous,
        base.problem().initialization().clone(),
        discrete,
        events,
        clocks,
    )
    .expect("scheduled time-floor fixture satisfies the checked root contract");
    crate::test_support::checked_solve_model! { problem: problem, ..base }
}

fn enter_continuous_mode(kernel: &mut SolveMeKernel) {
    kernel
        .enter_initialization_mode(0.0)
        .expect("enter initialization");
    kernel
        .exit_initialization_mode()
        .expect("exit initialization");
    kernel
        .update_discrete_states()
        .expect("perform initial discrete update");
    kernel
        .enter_continuous_time_mode()
        .expect("enter continuous time");
}

#[test]
fn continuous_state_nominal_is_the_declared_nominal_not_the_solver_scale() {
    let mut kernel = instantiate(strict_root_relation_memory(2.0));
    enter_continuous_mode(&mut kernel);

    let mut nominals = [f64::NAN];
    kernel
        .get_nominals_of_continuous_states(&mut nominals)
        .expect("the continuous-state nominal is available in continuous-time mode");

    assert_eq!(nominals, [1.0]);
}

#[test]
fn set_time_retains_start_and_second_last_completed_step_bounds_across_snapshot_restore() {
    let mut kernel = instantiate(harmonic_oscillator());
    enter_continuous_mode(&mut kernel);
    kernel
        .set_time(MeTime::at(0.25))
        .expect("advance to first completed point");
    kernel
        .completed_integrator_step(true)
        .expect("complete first point");
    let after_first = kernel.fmu_state();
    kernel
        .set_time(MeTime::at(0.5))
        .expect("advance to second completed point");
    kernel
        .completed_integrator_step(true)
        .expect("complete second point");
    let after_second = kernel.fmu_state();

    kernel
        .set_time(MeTime::at(0.1))
        .expect_err("0.1 precedes the retained second-last completed point 0.25");
    assert!(kernel.verification_matches_snapshot(&after_second));

    kernel
        .reset_to_fmu_state(&after_first)
        .expect("restore the exact one-step time-bound continuation");
    kernel
        .set_time(MeTime::at(0.1))
        .expect("with only one completed point, the start time is the lower bound");
    let at_point_one = kernel.fmu_state();
    kernel
        .set_time(MeTime::at(-0.1))
        .expect_err("a time before startTime is always rejected");
    assert!(kernel.verification_matches_snapshot(&at_point_one));
}

#[test]
fn set_time_retains_last_enter_event_mode_bound_across_snapshot_restore() {
    let mut kernel = instantiate(harmonic_oscillator_with_scheduled_event(0.5));
    enter_continuous_mode(&mut kernel);
    kernel
        .set_time(MeTime::at(0.25))
        .expect("retain a pre-event snapshot coordinate");
    let before_event = kernel.fmu_state();
    kernel
        .set_time(MeTime::at(0.5))
        .expect("reach scheduled event time");
    kernel.enter_event_mode().expect("enter Event Mode at 0.5");
    kernel
        .update_discrete_states()
        .expect("apply scheduled event");
    kernel
        .enter_continuous_time_mode()
        .expect("return to Continuous-Time Mode");
    let after_event = kernel.fmu_state();

    kernel
        .set_time(MeTime::at(0.1))
        .expect_err("0.1 precedes the last enterEventMode time 0.5");
    assert!(kernel.verification_matches_snapshot(&after_event));

    kernel
        .reset_to_fmu_state(&before_event)
        .expect("restore the pre-event bound continuation");
    kernel
        .set_time(MeTime::at(0.1))
        .expect("snapshot restore removes the later event-time bound exactly");
}

#[test]
fn float64_batches_serialize_mixed_array_and_scalar_references_in_caller_order() {
    let mut kernel = instantiate(aggregate_float64_input_model());
    assert_eq!(kernel.model_description().input_names, ["u", "s"]);
    assert!(kernel.value_reference("u[1]").is_none());
    assert!(kernel.value_reference("u[2]").is_none());
    let input = kernel
        .value_reference("u")
        .expect("the aggregate declaration owns one FMI 3 value reference");
    assert_eq!(input.value_reference, 1);
    let scalar = kernel
        .value_reference("s")
        .expect("the scalar declaration owns its adjacent exact reference");
    assert_eq!(scalar.value_reference, 2);

    kernel
        .set_float64(&[input.clone(), scalar.clone()], &[4.0, 5.0, 6.0])
        .expect("array then scalar serializes to three input values");
    let mut values = [0.0, 0.0, 0.0];
    kernel
        .get_float64(&[input.clone(), scalar.clone()], &mut values)
        .expect("array then scalar serializes to three result values");
    assert_eq!(values, [4.0, 5.0, 6.0]);
    kernel
        .set_float64(&[scalar.clone(), input.clone()], &[7.0, 8.0, 9.0])
        .expect("scalar then array follows caller reference order");
    kernel
        .get_float64(&[scalar.clone(), input.clone()], &mut values)
        .expect("the reverse getter follows caller reference order");
    assert_eq!(values, [7.0, 8.0, 9.0]);

    for refs in [
        vec![input.clone(), scalar.clone()],
        vec![scalar.clone(), input.clone()],
    ] {
        for rejected in [&[11.0, 12.0][..], &[11.0, 12.0, 13.0, 14.0][..]] {
            let before = kernel.fmu_state();
            kernel
                .set_float64(&refs, rejected)
                .expect_err("the total serialized setter width is exact");
            assert!(kernel.verification_matches_snapshot(&before));
        }

        let mut short = [17.0, 19.0];
        kernel
            .get_float64(&refs, &mut short)
            .expect_err("the serialized getter rejects a short result buffer");
        assert_eq!(short, [17.0, 19.0]);
        let mut long = [23.0, 29.0, 31.0, 37.0];
        kernel
            .get_float64(&refs, &mut long)
            .expect_err("the serialized getter rejects a long result buffer");
        assert_eq!(long, [23.0, 29.0, 31.0, 37.0]);
    }
}

#[test]
fn initial_discrete_update_failure_keeps_latched_inputs_for_exact_retry() {
    let mut kernel = instantiate(harmonic_oscillator());
    kernel
        .enter_initialization_mode(0.0)
        .expect("enter initialization");
    kernel
        .exit_initialization_mode()
        .expect("latch the settled and pre-event vectors");
    let before = kernel.fmu_state();
    kernel.verification_fail_next_update_discrete_states();

    kernel
        .update_discrete_states()
        .expect_err("the injected failure follows detached initial-event evaluation");
    assert!(kernel.verification_matches_snapshot(&before));
    kernel
        .update_discrete_states()
        .expect("retry consumes the same settled and pre-event vectors");
}

#[test]
fn initialization_entry_and_exit_failures_restore_for_identical_retry() {
    let mut kernel = instantiate(harmonic_oscillator());
    let instantiated = kernel.fmu_state();
    kernel.verification_fail_next_enter_initialization();
    kernel
        .enter_initialization_mode(0.0)
        .expect_err("injected entry failure occurs after staged reset");
    assert!(kernel.verification_matches_snapshot(&instantiated));
    kernel
        .enter_initialization_mode(0.0)
        .expect("entry retries from the identical instantiated continuation");

    let initializing = kernel.fmu_state();
    kernel.verification_fail_next_exit_initialization();
    kernel
        .exit_initialization_mode()
        .expect_err("injected exit failure occurs after staged settlement");
    assert!(kernel.verification_matches_snapshot(&initializing));
    kernel
        .exit_initialization_mode()
        .expect("settlement retries from the identical initialization continuation");
}

#[test]
fn failed_delay_commit_and_indicator_seed_restore_event_mode_for_retry() {
    let mut kernel = instantiate(strict_root_relation_memory(-1.0));
    kernel.enter_initialization_mode(0.0).expect("enter init");
    kernel.exit_initialization_mode().expect("exit init");
    kernel.update_discrete_states().expect("settle init event");
    let event_mode = kernel.fmu_state();
    kernel.verification_fail_next_enter_continuous_time_mode();
    kernel
        .enter_continuous_time_mode()
        .expect_err("failure after staged delay commit and indicator seed is atomic");
    assert!(kernel.verification_matches_snapshot(&event_mode));
    kernel
        .enter_continuous_time_mode()
        .expect("the identical staged transition succeeds on retry");
}

#[test]
fn runtime_discrete_update_failure_keeps_event_entry_for_exact_retry() {
    let base = harmonic_oscillator();
    let solve_layout = base.problem().solve_layout().clone();
    let discrete = base.problem().discrete().clone();
    let events = solve::SolveEventPartition {
        scheduled_time_events: vec![0.5],
        ..base.problem().events().clone()
    };
    let clocks = base.problem().clocks().clone();
    let continuous =
        crate::test_support::ContinuousSystemFixture::from_system(base.problem().continuous())
            .seal(&solve_layout, &discrete, &events, &clocks);
    let problem = crate::test_support::checked_solve_problem!(
        base.problem().layout().clone(),
        solve_layout,
        continuous,
        base.problem().initialization().clone(),
        discrete,
        events,
        clocks,
    )
    .expect("scheduled retry fixture satisfies the checked root contract");
    let model = crate::test_support::checked_solve_model! { problem: problem, ..base };
    let mut kernel = instantiate(model);
    kernel.enter_initialization_mode(0.0).expect("enter init");
    kernel.exit_initialization_mode().expect("exit init");
    kernel.update_discrete_states().expect("initial update");
    kernel
        .enter_continuous_time_mode()
        .expect("enter continuous mode");
    kernel.set_time(MeTime::at(0.5)).expect("reach event time");
    kernel.enter_event_mode().expect("latch the event entry");
    let before = kernel.fmu_state();
    kernel.verification_fail_next_update_discrete_states();

    kernel
        .update_discrete_states()
        .expect_err("the injected failure follows detached runtime-event evaluation");
    assert!(kernel.verification_matches_snapshot(&before));
    kernel
        .update_discrete_states()
        .expect("retry consumes the same runtime event entry");
}

#[test]
fn terminated_me_allows_final_getters_but_refuses_mutation_until_snapshot_restore() {
    let model = harmonic_oscillator_with_input();
    let mut kernel = instantiate(model);
    let saved = kernel.fmu_state();
    let input = kernel
        .value_reference("u")
        .expect("the final-value fixture declares one Float64 input");
    kernel
        .enter_initialization_mode(0.0)
        .expect("enter initialization");
    kernel
        .exit_initialization_mode()
        .expect("exit initialization");
    kernel
        .update_discrete_states()
        .expect("settle the initial event");
    kernel
        .enter_continuous_time_mode()
        .expect("enter continuous time");
    kernel.terminate().expect("termination is legal once");

    let mut states = vec![0.0; kernel.model_description().continuous_state_count];
    kernel
        .get_continuous_states(&mut states)
        .expect("Terminated ME permits final continuous states");
    assert_eq!(states.len(), 2);
    let mut nominals = vec![0.0; states.len()];
    kernel
        .get_nominals_of_continuous_states(&mut nominals)
        .expect("Terminated ME permits final state nominals");
    let mut derivatives = vec![0.0; kernel.model_description().continuous_state_count];
    kernel
        .get_continuous_state_derivatives(&mut derivatives)
        .expect("Terminated ME permits final state derivatives");
    assert_eq!(derivatives.len(), states.len());
    let mut indicators = vec![0.0; kernel.model_description().event_indicator_count];
    kernel
        .get_event_indicators(&mut indicators)
        .expect("Terminated ME permits final event indicators");
    assert_eq!(
        indicators.len(),
        kernel.model_description().event_indicator_count
    );
    let output_references = kernel
        .model_description()
        .output_names
        .iter()
        .map(|name| {
            kernel
                .value_reference(name)
                .expect("output value reference")
        })
        .collect::<Vec<_>>();
    let mut outputs = vec![0.0; output_references.len()];
    kernel
        .get_float64(&output_references, &mut outputs)
        .expect("Terminated ME permits final output getters");
    let mut input_value = [0.0];
    kernel
        .get_float64(std::slice::from_ref(&input), &mut input_value)
        .expect("Terminated ME permits final typed Float64 getters");
    assert_eq!(input_value, [1.0]);

    assert!(kernel.set_time(super::MeTime::at(0.25)).is_err());
    assert!(kernel.set_continuous_states(&[2.0, 3.0]).is_err());
    assert!(kernel.set_float64(&[input], &[2.0]).is_err());
    assert!(kernel.terminate().is_err());

    kernel
        .reset_to_fmu_state(&saved)
        .expect("snapshot restore is the one exit from Terminated");
    kernel
        .enter_initialization_mode(0.0)
        .expect("the saved Instantiated lifecycle is restored exactly");
}

#[test]
fn fmu_state_restore_replays_the_same_scheduled_event_continuation() {
    let base = harmonic_oscillator();
    let solve_layout = base.problem().solve_layout().clone();
    let continuous =
        crate::test_support::ContinuousSystemFixture::from_system(base.problem().continuous());
    let discrete = base.problem().discrete().clone();
    let events = solve::SolveEventPartition {
        scheduled_time_events: vec![0.5],
        ..base.problem().events().clone()
    };
    let clocks = base.problem().clocks().clone();
    let continuous = continuous.seal(&solve_layout, &discrete, &events, &clocks);
    let problem = crate::test_support::checked_solve_problem!(
        base.problem().layout().clone(),
        solve_layout,
        continuous,
        base.problem().initialization().clone(),
        discrete,
        events,
        clocks,
    )
    .expect("scheduled snapshot fixture satisfies the checked root contract");
    let model = crate::test_support::checked_solve_model! { problem: problem, ..base };
    let mut kernel = instantiate(model);
    kernel
        .enter_initialization_mode(0.0)
        .expect("enter initialization");
    kernel
        .exit_initialization_mode()
        .expect("exit initialization");
    let discrete = kernel
        .update_discrete_states()
        .expect("initial event update");
    assert_eq!(discrete.next_event_time, Some(0.5));
    kernel
        .enter_continuous_time_mode()
        .expect("enter continuous time");
    kernel
        .set_time(MeTime::at(0.25))
        .expect("set checkpoint time");
    kernel
        .set_continuous_states(&[2.0, 3.0])
        .expect("set checkpoint state");
    let mut cached_derivative = vec![0.0; kernel.model_description().continuous_state_count];
    kernel
        .get_continuous_state_derivatives(&mut cached_derivative)
        .expect("populate derivative cache");
    let saved_observable = kernel.verification_observable_state();
    let saved = kernel.fmu_state();

    let first = continue_from_scheduled_event(&mut kernel);
    assert!(
        !first.2,
        "a scheduled event with no state update must preserve the exact state vector"
    );
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

fn continue_from_scheduled_event(kernel: &mut SolveMeKernel) -> (Vec<u64>, Vec<u64>, bool) {
    kernel
        .set_time(MeTime::at(0.5))
        .expect("reach scheduled event");
    kernel.enter_event_mode().expect("enter scheduled event");
    let discrete = kernel
        .update_discrete_states()
        .expect("apply scheduled event");
    kernel
        .enter_continuous_time_mode()
        .expect("resume continuous time");
    let mut states = vec![0.0; 2];
    kernel
        .get_continuous_states(&mut states)
        .expect("read continued state");
    let mut derivatives = vec![0.0; kernel.model_description().continuous_state_count];
    kernel
        .get_continuous_state_derivatives(&mut derivatives)
        .expect("read continued derivative");
    (
        states.into_iter().map(f64::to_bits).collect(),
        derivatives.into_iter().map(f64::to_bits).collect(),
        discrete.values_of_continuous_states_changed,
    )
}

#[test]
fn instance_brands_reject_foreign_capabilities_without_mutation() {
    let mut first = instantiate(harmonic_oscillator_with_input());
    let mut second = instantiate(harmonic_oscillator_with_input());
    let first_ref = first
        .value_reference("u")
        .expect("input has a value reference");
    let second_ref = second
        .value_reference("u")
        .expect("the other instance has its own reference");

    let error = first
        .set_float64(&[first_ref.clone(), second_ref.clone()], &[2.0, 3.0])
        .expect_err("a foreign reference rejects the whole batch");
    assert!(matches!(error.kind(), MeError::Contract { .. }));
    assert_eq!(
        first.verification_observable_state().3,
        vec![1.0f64.to_bits()]
    );

    let mut values = [17.0, 19.0];
    second
        .get_float64(&[second_ref, first_ref], &mut values)
        .expect_err("a foreign reference rejects the whole read batch");
    assert_eq!(values, [17.0, 19.0]);

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
fn the_component_retains_the_typed_post_side_of_a_strict_root() {
    let model = strict_root_relation_memory(-1.0);
    let mut kernel = instantiate(model);
    kernel
        .enter_initialization_mode(0.0)
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

    let entered_time = f64::from_bits(1.0f64.to_bits() + 1);
    let entered_state = entered_time - 1.0;
    kernel
        .set_time(MeTime::at(entered_time))
        .expect("component should reach the first checked point in the entered domain");
    kernel
        .set_continuous_states(&[entered_state])
        .expect("component should use the least state in the entered relation domain");
    assert!(
        kernel.model_description().needs_completed_integrator_step,
        "the linked kernel declares its accepted-step history requirement"
    );
    let completed = kernel
        .completed_integrator_step(true)
        .expect("the integrator should report the located root");
    assert_eq!(
        completed,
        super::MeCompletedIntegratorStep {
            enter_event_mode: false,
            terminate_simulation: false,
        },
        "a located state event is importer-owned, not a step event"
    );
    kernel
        .enter_event_mode()
        .expect("the component should enter root event iteration");
    kernel
        .update_discrete_states()
        .expect("the strict-root event should settle");

    assert_eq!(
        kernel.verification_observable_state().3,
        vec![1.0f64.to_bits()],
        "the first checked point in the entered domain owns the strict post-root value"
    );
    kernel
        .enter_continuous_time_mode()
        .expect("the settled strict-root event should resume integration");
}

#[test]
fn completed_step_failure_does_not_publish_crossings_or_frozen_domains() {
    let mut kernel = instantiate(strict_root_relation_memory(-1.0));
    kernel.enter_initialization_mode(0.0).expect("enter init");
    kernel.exit_initialization_mode().expect("exit init");
    kernel.update_discrete_states().expect("initial update");
    kernel
        .enter_continuous_time_mode()
        .expect("seed the accepted indicator domain");
    let entered_time = f64::from_bits(1.0f64.to_bits() + 1);
    kernel
        .set_time(MeTime::at(entered_time))
        .expect("reach the accepted post-root coordinate");
    kernel
        .set_continuous_states(&[entered_time - 1.0])
        .expect("cross the strict root");
    let before = kernel.fmu_state();
    kernel.verification_fail_next_completed_integrator_step();

    kernel
        .completed_integrator_step(true)
        .expect_err("the injected failure follows detached crossing detection");
    assert!(kernel.verification_matches_snapshot(&before));
    let retried = kernel
        .completed_integrator_step(true)
        .expect("retry observes the same pre-operation indicator domain");
    assert!(!retried.enter_event_mode);
}

#[test]
fn neutral_static_root_never_fabricates_a_relation_crossing() {
    let model = static_true_relation_memory();
    let mut kernel = instantiate(model);
    kernel
        .enter_initialization_mode(0.0)
        .expect("static-relation initialization should start");
    kernel
        .exit_initialization_mode()
        .expect("static-relation initialization should settle");
    kernel
        .update_discrete_states()
        .expect("static-relation initial event should run");
    kernel
        .enter_continuous_time_mode()
        .expect("static-relation model should enter continuous time");

    let mut indicators = vec![0.0; kernel.model_description().event_indicator_count];
    kernel
        .get_event_indicators(&mut indicators)
        .expect("the parameter-static relation is outside the FMI indicator domain");
    assert!(indicators.is_empty());
    assert_eq!(
        kernel.verification_observable_state().3,
        vec![1.0f64.to_bits()],
        "full event iteration must retain the true relation value"
    );

    let completed = kernel
        .completed_integrator_step(true)
        .expect("the unchanged static root should complete without an event");
    assert!(!completed.enter_event_mode);
    assert_eq!(
        kernel.verification_observable_state().3,
        vec![1.0f64.to_bits()],
        "an excluded parameter-static relation must not overwrite relation memory"
    );
}

#[test]
fn post_pre_canonicalization_holds_parameter_only_relation_memory() {
    let model = post_pre_relation_cycle();
    let mut solver_y = model.initial_y().to_vec();
    let mut kernel = instantiate(model);

    kernel
        .verification_canonicalize_committed_event_view(0.0, &mut solver_y)
        .expect("derived settling must not start a second relation-memory event iteration");

    assert_eq!(
        kernel.verification_observable_state().3,
        vec![0.0f64.to_bits(), 0.0f64.to_bits()],
        "the RelationEvaluating owner is not replayed after pre commits"
    );
    assert_eq!(
        solver_y,
        vec![0.0],
        "derived settling observes the selected side without feeding back into relation memory"
    );
}

#[test]
fn post_commit_relation_free_alias_cannot_flip_a_parameter_only_root() {
    let model = post_commit_alias_with_frozen_parameter_root();
    let mut solver_y = model.initial_y().to_vec();
    let mut kernel = instantiate(model);

    kernel
        .verification_canonicalize_committed_event_view(0.0, &mut solver_y)
        .expect("relation-free alias should settle without reopening frozen roots");

    assert_eq!(
        kernel.verification_observable_state().3,
        vec![
            1.0f64.to_bits(),
            1.0f64.to_bits(),
            1.0f64.to_bits(),
            (-1.0f64).to_bits(),
        ],
        "the alias changes P2, but the separate P2-root relation memory P1 stays frozen"
    );
}

#[test]
fn the_component_preserves_a_nonzero_root_distance_inside_solver_tolerance() {
    let positive_distance = 5.0e-11;
    let model = strict_root_relation_memory(-positive_distance);
    let mut kernel = instantiate(model);
    kernel
        .enter_initialization_mode(0.0)
        .expect("near-root initialization should start");
    kernel
        .exit_initialization_mode()
        .expect("near-root initialization should settle");
    kernel
        .update_discrete_states()
        .expect("near-root initial event should run");
    kernel
        .enter_continuous_time_mode()
        .expect("near-root model should enter continuous time");

    let mut indicators = vec![0.0; kernel.model_description().event_indicator_count];
    kernel
        .get_event_indicators(&mut indicators)
        .expect("the component should expose its signed root distance");

    assert_eq!(indicators, vec![positive_distance]);
}

#[test]
fn the_directional_derivative_is_the_exact_state_jacobian_product() {
    let model = harmonic_oscillator();
    let mut kernel = instantiate(model);
    kernel
        .enter_initialization_mode(0.0)
        .expect("enter initialization");
    kernel
        .exit_initialization_mode()
        .expect("exit initialization");
    kernel
        .update_discrete_states()
        .expect("settle the initial event");
    kernel
        .enter_continuous_time_mode()
        .expect("enter continuous time");
    kernel
        .set_continuous_states(&[3.0, 5.0])
        .expect("state buffer matches the model description");

    let (unknowns, knowns) = state_directional_batches(&kernel);
    let mut sensitivity = vec![f64::NAN; 2];
    kernel
        .get_directional_derivative(&unknowns, &knowns, &[1.0, 0.0], &mut sensitivity)
        .expect("a pure ODE has a directional derivative everywhere");
    // First column of [[0, 1], [-4, 0]].
    assert_eq!(sensitivity, vec![0.0, -4.0]);

    kernel
        .get_directional_derivative(&unknowns, &knowns, &[0.0, 1.0], &mut sensitivity)
        .expect("second seed evaluates too");
    assert_eq!(sensitivity, vec![1.0, 0.0]);
}

/// The operation is a *directional* derivative, not a column extractor: a
/// non-unit seed must come back scaled, or a host's Newton direction would be
/// silently renormalized.
#[test]
fn the_directional_derivative_is_linear_in_the_seed() {
    let model = harmonic_oscillator();
    let mut kernel = instantiate(model);
    kernel
        .enter_initialization_mode(0.0)
        .expect("enter initialization");
    kernel
        .exit_initialization_mode()
        .expect("exit initialization");
    kernel
        .update_discrete_states()
        .expect("settle the initial event");
    kernel
        .enter_continuous_time_mode()
        .expect("enter continuous time");
    kernel
        .set_continuous_states(&[0.25, -1.5])
        .expect("state buffer matches the model description");

    let (unknowns, knowns) = state_directional_batches(&kernel);
    let mut sensitivity = vec![f64::NAN; 2];
    kernel
        .get_directional_derivative(&unknowns, &knowns, &[2.0, -3.0], &mut sensitivity)
        .expect("a pure ODE has a directional derivative everywhere");
    assert_eq!(sensitivity, vec![-3.0, -8.0]);
}

/// A three-state model whose second and third derivative rows carry a runtime
/// linear solve, the fixture image of the flat Modelica model
///
/// ```modelica
/// model DirectionalRowFault
///   Real z(start = 0, fixed = true);
///   Real x(start = 0, fixed = true);
///   Real y(start = 0, fixed = true);
/// equation
///   der(z) = z;
///   der(x) + z*der(y) = 1;
///   z*der(x) + der(y) = 2;
/// end DirectionalRowFault;
/// ```
///
/// The coupled pair `{der(x), der(y)}` is an affine derivative BLT block, so
/// its rows are lowered through `LinSolve` with the runtime coefficient
/// matrix `[[1, z], [z, 1]]`. `checked_solve_model!` binds its derivative JVP
/// to production `rumoca_phase_solve::lower_solve_artifacts`; no fixture JVP
/// is supplied. The matrix is the identity at the start point `z = 0` and
/// singular at `z = 1`, so the production JVP block has a genuine runtime
/// failure that occurs on row 1 after row 0 has already stored its output.
fn affine_derivative_row_fault_model() -> solve::SolveModel {
    let derivative = block(
        vec![
            vec![
                solve::LinearOp::LoadY { dst: 0, index: 0 },
                solve::LinearOp::StoreOutput { src: 0 },
            ],
            affine_value_row(0),
            affine_value_row(1),
        ],
        "fmi_me_row_fault.mo",
    );
    let solve_layout = solve::SolveLayout {
        solver_maps: solve::SolverNameIndexMaps {
            names: vec!["z".to_string(), "x".to_string(), "y".to_string()],
            name_to_idx: IndexMap::from([
                ("z".to_string(), 0),
                ("x".to_string(), 1),
                ("y".to_string(), 2),
            ]),
            ..Default::default()
        },
        state_scalar_count: 3,
        ..Default::default()
    };
    let discrete = solve::DiscreteSolveSystem::default();
    let events = solve::SolveEventPartition::default();
    let clocks = solve::SolveClockPartition::default();
    let continuous = crate::test_support::ContinuousSystemFixture {
        derivative_rhs: solve::ComputeBlock::from_scalar_program_block(derivative),
        ..crate::test_support::ContinuousSystemFixture::empty()
    };
    let continuous = continuous.seal(&solve_layout, &discrete, &events, &clocks);
    let model = crate::test_support::checked_solve_model! {
        problem: crate::test_support::checked_solve_problem!(
            solve::VarLayout::from_parts(IndexMap::new(), 3, 0),
            solve_layout,
            continuous,
            solve::InitializationSolveSystem::empty(),
            discrete,
            events,
            clocks,
        )
        .expect("row-fault fixture satisfies the checked root contract"),
        initial_y: vec![0.0, 0.0, 0.0],
        solver_nominals: vec![1.0, 1.0, 1.0],
        ..empty_binary64_first_product_model()
    };
    let provenance = rumoca_core::Span::from_offsets(
        rumoca_core::SourceId::from_source_name("fmi_me_row_fault_variables.mo"),
        1,
        2,
    );
    crate::test_support::with_explicit_real_scalar_catalog(
        model,
        vec![
            crate::test_support::RealScalarVariableFixture::state(
                1, "z", 0, 0.0, 1.0, true, provenance,
            ),
            crate::test_support::RealScalarVariableFixture::state(
                2, "x", 1, 0.0, 1.0, true, provenance,
            ),
            crate::test_support::RealScalarVariableFixture::state(
                3, "y", 2, 0.0, 1.0, true, provenance,
            ),
        ],
        crate::test_support::direct_y_visible_rows([0, 1, 2], provenance),
    )
}

/// One value row of the affine derivative system
/// `[[1, z], [z, 1]] * [der(x), der(y)] = [1, 2]`.
fn affine_value_row(component: usize) -> Vec<solve::LinearOp> {
    vec![
        solve::LinearOp::Const { dst: 0, value: 1.0 },
        solve::LinearOp::LoadY { dst: 1, index: 0 },
        solve::LinearOp::LoadY { dst: 2, index: 0 },
        solve::LinearOp::Const { dst: 3, value: 1.0 },
        solve::LinearOp::Const { dst: 4, value: 1.0 },
        solve::LinearOp::Const { dst: 5, value: 2.0 },
        solve::LinearOp::LinearSolveComponent {
            dst: 6,
            matrix_start: 0,
            rhs_start: 4,
            n: 2,
            component,
        },
        solve::LinearOp::StoreOutput { src: 6 },
    ]
}

/// A directional evaluation that fails on a later JVP row, after an earlier
/// row already stored its output, must leave every caller slot bit-identical:
/// the JVP block writes its output vector incrementally (zero fill, then one
/// row at a time, each fallible), so the component may only publish to the
/// caller after the whole block has evaluated.
///
/// The failure driven here is inside the JVP row loop itself, not in the
/// validation that precedes it: row 0 (`der(z)`) stores the seed, then row 1
/// hits the singular runtime matrix `[[1, 1], [1, 1]]` inside its
/// `LinearSolveComponent`. A component that evaluated directly into the
/// caller's buffer would pass every pre-JVP validation-failure test and still
/// hand the caller the zero fill plus row 0's value here.
#[test]
fn a_production_derived_directional_row_failure_leaves_the_caller_bits_unchanged() {
    let mut kernel = instantiate(affine_derivative_row_fault_model());
    kernel
        .enter_initialization_mode(0.0)
        .expect("enter initialization");
    kernel
        .exit_initialization_mode()
        .expect("exit initialization");
    kernel
        .update_discrete_states()
        .expect("settle the initial event");
    kernel
        .enter_continuous_time_mode()
        .expect("enter continuous time");

    // At the start point z = 0 the affine matrix is the identity: the value
    // path and the directional path both evaluate, proving the model is
    // well-formed and the failing call below exercises the live JVP rows.
    let mut derivative = vec![f64::NAN; 3];
    kernel
        .get_continuous_state_derivatives(&mut derivative)
        .expect("the identity affine system solves at the start point");
    assert_eq!(derivative, vec![0.0, 1.0, 2.0]);

    // z = 1 makes the affine matrix [[1, 1], [1, 1]]: singular, so the JVP
    // block fails on row 1 after row 0 (the seed of z) has already stored.
    kernel
        .set_continuous_states(&[1.0, 0.0, 0.0])
        .expect("state buffer matches the model description");

    let (unknowns, knowns) = state_directional_batches(&kernel);
    let sentinel = [
        f64::from_bits(0x7ff8_0000_0000_1001),
        f64::from_bits(0x4009_21fb_5444_2d18),
        f64::from_bits(0xbff0_0000_0000_0002),
    ];
    let mut sensitivity = sentinel.to_vec();
    let error = kernel
        .get_directional_derivative(&unknowns, &knowns, &[1.0, 0.0, 0.0], &mut sensitivity)
        .expect_err("the singular affine matrix must fail the JVP row");
    assert_eq!(error.stage(), Some(MeStage::Integration));
    let MeError::Evaluation { message } = error.kind() else {
        panic!("a JVP row failure surfaces as an evaluation failure: {error}");
    };
    assert!(
        message.starts_with(
            "Solve-IR linear solve of size 2 cannot evaluate component 0: singular matrix"
        ),
        "unexpected evaluation failure: {message}"
    );
    // The caller's buffer is bit-identical to the sentinel: neither the zero
    // fill nor row 0's successful store may reach the caller on failure.
    for (slot, expected) in sensitivity.iter().zip(sentinel) {
        assert_eq!(slot.to_bits(), expected.to_bits());
    }

    // The failed getter keeps the instance usable: back at the nonsingular
    // point the same call publishes the exact directional derivative
    // (seed z = 1 flows through der(z) = z and the tangent system
    // [[1, 0], [0, 1]] * d = -[der(y), der(x)] = -[2, 1]).
    kernel
        .set_continuous_states(&[0.0, 0.0, 0.0])
        .expect("state buffer matches the model description");
    kernel
        .get_directional_derivative(&unknowns, &knowns, &[1.0, 0.0, 0.0], &mut sensitivity)
        .expect("the identity affine system has a directional derivative");
    assert_eq!(sensitivity, vec![1.0, -2.0, -1.0]);
}

#[test]
fn directional_reference_batches_support_standard_state_and_derivative_subsets() {
    let mut kernel = instantiate(harmonic_oscillator());
    kernel
        .enter_initialization_mode(0.0)
        .expect("enter initialization");
    kernel
        .exit_initialization_mode()
        .expect("exit initialization");
    kernel
        .update_discrete_states()
        .expect("settle the initial event");
    kernel
        .enter_continuous_time_mode()
        .expect("enter continuous time");

    let mut states = kernel
        .continuous_state_value_references()
        .expect("state references construct");
    let mut derivatives = kernel
        .continuous_state_derivative_value_references()
        .expect("derivative references construct");
    let x = states.remove(0);
    let derivative_v = derivatives.remove(1);
    let knowns = kernel
        .directional_known_batch(vec![x])
        .expect("the host may select one known state");
    let unknowns = kernel
        .directional_unknown_batch(vec![derivative_v])
        .expect("the host may select one unknown derivative");
    let mut sensitivity = [f64::NAN];
    kernel
        .get_directional_derivative(&unknowns, &knowns, &[2.0], &mut sensitivity)
        .expect("the standard subset lists evaluate");
    assert_eq!(sensitivity, [-8.0]);
}

/// The derivative getters keep one construction-reserved caller-publication
/// storage identity across real rhs and Jacobian-action calls, and the reused
/// full seed is re-zeroed on every call.
///
/// The pointer/length rows die if `fmi3GetContinuousStateDerivatives` or
/// `fmi3GetDirectionalDerivative` replaces or regrows any of the four
/// construction-reserved publication buffers. This two-state fixture makes
/// every observed width nonzero; construction reserves the derivative output,
/// full seed, and state-sensitivity buffers at that state width and the
/// serialization buffer at the nonempty issued derivative-table width, so no
/// zero-width pointer identity is treated as evidence. The subset call in the
/// middle is the non-vacuity anchor for the seed refill: after a full-batch
/// call seeded `[5, 7]`, a knowns batch covering only `x` asks for
/// `d(der(x))·v` with a zero `v` seed, so the exact answer is `0.0`; a stale
/// `7.0` leaking from the previous call's full seed is precisely the reuse
/// defect this kills.
/// The exact derivative and Jacobian values prove the calls really evaluated
/// through the reserved caller-publication path rather than short-circuiting.
#[test]
fn the_derivative_getters_keep_one_caller_publication_identity_across_real_calls() {
    let mut kernel = instantiate(harmonic_oscillator());
    kernel
        .enter_initialization_mode(0.0)
        .expect("enter initialization");
    kernel
        .exit_initialization_mode()
        .expect("exit initialization");
    kernel
        .update_discrete_states()
        .expect("settle the initial event");
    kernel
        .enter_continuous_time_mode()
        .expect("enter continuous time");
    kernel
        .set_continuous_states(&[3.0, 5.0])
        .expect("state buffer matches the model description");

    assert_eq!(
        kernel.model_description().continuous_state_count,
        2,
        "the storage-identity witness must have a nonzero constructed state width"
    );
    let identity = kernel.verification_derivative_scratch_identity();

    let mut derivatives = vec![f64::NAN; 2];
    kernel
        .get_continuous_state_derivatives(&mut derivatives)
        .expect("the rhs evaluates");
    assert_eq!(derivatives, vec![5.0, -12.0]);

    let (unknowns, knowns) = state_directional_batches(&kernel);
    let mut sensitivity = vec![f64::NAN; 2];
    kernel
        .get_directional_derivative(&unknowns, &knowns, &[5.0, 7.0], &mut sensitivity)
        .expect("the full-batch Jacobian action evaluates");
    // [[0, 1], [-4, 0]] · [5, 7].
    assert_eq!(sensitivity, vec![7.0, -20.0]);

    let mut states = kernel
        .continuous_state_value_references()
        .expect("state references construct");
    let mut derivative_refs = kernel
        .continuous_state_derivative_value_references()
        .expect("derivative references construct");
    let x = states.remove(0);
    let derivative_x = derivative_refs.remove(0);
    let subset_knowns = kernel
        .directional_known_batch(vec![x])
        .expect("the host may select one known state");
    let subset_unknowns = kernel
        .directional_unknown_batch(vec![derivative_x])
        .expect("the host may select one unknown derivative");
    let mut subset_sensitivity = [f64::NAN];
    kernel
        .get_directional_derivative(
            &subset_unknowns,
            &subset_knowns,
            &[2.0],
            &mut subset_sensitivity,
        )
        .expect("the subset Jacobian action evaluates");
    // `der(x) = v` and the subset seeds no `v`, so a nonzero answer here is a
    // seed leaked from the previous call's reused full-seed storage.
    assert_eq!(subset_sensitivity, [0.0]);

    for _ in 0..8 {
        kernel
            .get_continuous_state_derivatives(&mut derivatives)
            .expect("every residual rhs evaluates");
        kernel
            .get_directional_derivative(&unknowns, &knowns, &[1.0, 0.0], &mut sensitivity)
            .expect("every Newton direction evaluates");
    }
    assert_eq!(derivatives, vec![5.0, -12.0]);
    assert_eq!(sensitivity, vec![0.0, -4.0]);

    assert_eq!(
        kernel.verification_derivative_scratch_identity(),
        identity,
        "a derivative or Jacobian-action call rebuilt or regrew its construction-reserved \
         caller-publication storage"
    );
}

/// SPEC_0038 failure atomicity for `fmi3GetContinuousStateDerivatives`: a
/// post-admission evaluation failure must leave every caller slot exactly as
/// the host wrote it, because the getter evaluates into construction-reserved
/// caller-publication storage and copies into the caller only after a
/// successful evaluation.
///
/// The route is behavioral, not a width or lifecycle admission failure (those
/// leave the caller untouched however the getter is written). The harmonic
/// oscillator's `der(v) = -4x` overflows to `-inf` at `x = f64::MAX`, which
/// `set_continuous_states` admits because `f64::MAX` is finite; the runtime's
/// unconditional `validate_finite_derivatives` then rejects the vector *after*
/// it has already been written into the evaluation buffer. With the publish
/// copy in place the clobbered `[0.0, -inf]` lands in caller-publication
/// storage, and the typed error returns before the copy, so the NaN-sentinel
/// caller buffer is bit-identical afterward.
///
/// Mutant killed: keep the `derivative_output_scratch.borrow_mut()` line,
/// evaluate straight into the caller (`continuous_state_derivatives_into(&mut
/// derivatives[..])?`), and delete the final `copy_from_slice`. That mutant
/// passes the deny-scan (borrow line present, no banned token), the identity
/// rows (reserved publication field untouched), and every success-value test,
/// but here it writes the overflowed `[0.0, -inf]` into `derivatives` before
/// the typed error returns, so the sentinel assertion below fails.
#[test]
fn a_failed_continuous_state_derivative_getter_leaves_the_caller_untouched() {
    let mut kernel = instantiate(harmonic_oscillator());
    kernel
        .enter_initialization_mode(0.0)
        .expect("enter initialization");
    kernel
        .exit_initialization_mode()
        .expect("exit initialization");
    kernel
        .update_discrete_states()
        .expect("settle the initial event");
    kernel
        .enter_continuous_time_mode()
        .expect("enter continuous time");
    kernel
        .set_continuous_states(&[f64::MAX, 0.0])
        .expect("f64::MAX is a finite continuous state the setter admits");

    // A quiet-NaN sentinel with a recognizable payload, compared by bits so a
    // NaN caller value is a real fixed point rather than a value that fails its
    // own equality.
    let sentinel = f64::from_bits(0x7ff8_0000_0bad_0bad);
    let mut derivatives = vec![sentinel; 2];
    let before: Vec<u64> = derivatives.iter().map(|value| value.to_bits()).collect();

    let error = kernel
        .get_continuous_state_derivatives(&mut derivatives)
        .expect_err("der(v) = -4x overflows to -inf at x = f64::MAX");
    assert!(
        matches!(error.kind(), MeError::NonFiniteDerivative { .. }),
        "the overflow must surface as the typed non-finite-derivative failure: {error:?}"
    );
    assert_eq!(error.stage(), Some(MeStage::Integration));

    let after: Vec<u64> = derivatives.iter().map(|value| value.to_bits()).collect();
    assert_eq!(
        after, before,
        "a failing continuous-state-derivative getter must publish nothing into the caller buffer \
         (SPEC_0038)"
    );
}

#[test]
fn directional_reference_batches_reject_foreign_duplicate_and_wrong_role_lists() {
    let first = instantiate(harmonic_oscillator());
    let second = instantiate(harmonic_oscillator());
    let first_states = first
        .continuous_state_value_references()
        .expect("first state references construct");
    let second_states = second
        .continuous_state_value_references()
        .expect("second state references construct");
    let first_derivatives = first
        .continuous_state_derivative_value_references()
        .expect("first derivative references construct");

    first
        .directional_known_batch(vec![first_states[0].clone(), second_states[1].clone()])
        .expect_err("a foreign trailing known reference is rejected");
    first
        .directional_known_batch(vec![first_states[0].clone(), first_states[0].clone()])
        .expect_err("a duplicated known reference is rejected");
    first
        .directional_known_batch(vec![first_derivatives[0].clone()])
        .expect_err("a derivative reference cannot enter a known-state batch");
    first
        .directional_unknown_batch(vec![first_states[0].clone()])
        .expect_err("a state reference cannot enter an unknown-derivative batch");
}

#[test]
fn a_mismatched_seed_length_is_a_contract_violation_at_the_integration_stage() {
    let model = harmonic_oscillator();
    let mut kernel = instantiate(model);
    kernel
        .enter_initialization_mode(0.0)
        .expect("enter initialization");
    kernel
        .exit_initialization_mode()
        .expect("exit initialization");
    kernel
        .update_discrete_states()
        .expect("settle the initial event");
    kernel
        .enter_continuous_time_mode()
        .expect("enter continuous time");
    let (unknowns, knowns) = state_directional_batches(&kernel);

    let mut sensitivity = vec![0.0; 2];
    let error = kernel
        .get_directional_derivative(&unknowns, &knowns, &[1.0], &mut sensitivity)
        .expect_err("a seed that is not one entry per continuous state is rejected");

    assert_eq!(error.stage(), Some(MeStage::Integration));
    assert!(matches!(error.kind(), MeError::Contract { .. }));
    assert!(
        error
            .to_string()
            .contains("1 entries for serialized known width 2")
    );
}

#[test]
fn a_mismatched_sensitivity_length_is_rejected_before_evaluation() {
    let model = harmonic_oscillator();
    let mut kernel = instantiate(model);
    kernel
        .enter_initialization_mode(0.0)
        .expect("enter initialization");
    kernel
        .exit_initialization_mode()
        .expect("exit initialization");
    kernel
        .update_discrete_states()
        .expect("settle the initial event");
    kernel
        .enter_continuous_time_mode()
        .expect("enter continuous time");
    let (unknowns, knowns) = state_directional_batches(&kernel);

    let mut sensitivity = vec![0.0; 3];
    let error = kernel
        .get_directional_derivative(&unknowns, &knowns, &[1.0, 0.0], &mut sensitivity)
        .expect_err("a sensitivity buffer that is not one entry per state derivative is rejected");

    assert_eq!(error.stage(), Some(MeStage::Integration));
    assert!(matches!(error.kind(), MeError::Contract { .. }));
}

#[test]
fn event_mode_state_writes_require_construction_issued_reinit_false_evidence() {
    let mut kernel = instantiate(one_reinitializable_harmonic_state());
    kernel
        .enter_initialization_mode(0.0)
        .expect("enter initialization");
    kernel
        .exit_initialization_mode()
        .expect("exit initialization into Event Mode");
    let reinitializable = kernel.value_reference("x").expect("x value reference");
    let reinit_false = kernel.value_reference("v").expect("v value reference");
    let before = kernel.fmu_state();

    let rejected = kernel
        .set_float64(
            &[reinit_false.clone(), reinitializable.clone()],
            &[3.0, 4.0],
        )
        .expect_err("an Event-Mode batch containing a reinitializable state is rejected");
    assert!(matches!(rejected.kind(), MeError::Contract { .. }));
    assert!(kernel.verification_matches_snapshot(&before));

    kernel
        .set_float64(std::slice::from_ref(&reinit_false), &[3.0])
        .expect("the state proved reinit=false is writable in Event Mode");
    kernel
        .update_discrete_states()
        .expect("the initial event settles");
    kernel
        .enter_continuous_time_mode()
        .expect("enter Continuous-Time Mode");
    kernel
        .set_float64(std::slice::from_ref(&reinitializable), &[4.0])
        .expect("a reinitializable state remains writable in Continuous-Time Mode");
}

// -- instantiation staging -----------------------------------------------

/// ME-ZERO-001: a zero-state model constructs the same component. The common
/// session chooses its time-only numerical plugin from the checked component
/// width; a separate routing error would recreate a second host path.
#[test]
fn a_zero_state_model_constructs_the_common_component() {
    let model = empty_binary64_first_product_model();
    let component = SolveMeKernel::instantiate(
        MeModelSource::fixture(crate::test_support::fmi_component(model)),
        &fixture_instance_config(),
    )
    .expect("a model with no continuous states still has an FMI ME component");

    assert_eq!(component.model_description().continuous_state_count, 0);
}

#[test]
fn an_empty_initial_vector_is_not_completed_by_the_negative_fixture_path() {
    let mut fixture = crate::test_support::SolveModelFixture::from_model(harmonic_oscillator());
    fixture.initial_y.clear();
    let error = fixture
        .try_seal()
        .expect_err("an initial vector that contradicts the solver layout is rejected");

    assert!(matches!(
        *error,
        solve::SolveModelConstructionError::VectorLength {
            field: "initial_y",
            expected: 2,
            actual: 0,
        }
    ));
}

fn constant_delay_model() -> solve::SolveModel {
    let constant_row = |value, source| {
        block(
            vec![vec![
                solve::LinearOp::Const { dst: 0, value },
                solve::LinearOp::StoreOutput { src: 0 },
            ]],
            source,
        )
    };
    let base = empty_binary64_first_product_model();
    let solve_layout = solve::SolveLayout {
        compiled_parameter_len: 1,
        ..Default::default()
    };
    let discrete = solve::DiscreteSolveSystem::default();
    let events = solve::SolveEventPartition {
        delays: solve::SolveDelayPartition {
            source_rhs: constant_row(3.0, "fmi_me_delay_source.mo"),
            delay_time_rhs: constant_row(0.2, "fmi_me_delay_time.mo"),
            delay_max_rhs: constant_row(1.0, "fmi_me_delay_max.mo"),
            value_parameter_indices: vec![0],
            source_is_discrete: vec![false],
        },
        ..Default::default()
    };
    let clocks = solve::SolveClockPartition::default();
    let continuous = crate::test_support::ContinuousSystemFixture::empty();
    let continuous = continuous.seal(&solve_layout, &discrete, &events, &clocks);
    let problem = crate::test_support::checked_solve_problem!(
        solve::VarLayout::from_parts(IndexMap::new(), 0, 1),
        solve_layout,
        continuous,
        solve::InitializationSolveSystem::empty(),
        discrete,
        events,
        clocks,
    )
    .expect("constant-delay fixture satisfies the checked root contract");
    crate::test_support::checked_solve_model! {
        problem: problem,
        parameters: vec![0.0],
        ..base
    }
}

fn continuous_state_delay_model() -> solve::SolveModel {
    let constant_row = |value, source| {
        block(
            vec![vec![
                solve::LinearOp::Const { dst: 0, value },
                solve::LinearOp::StoreOutput { src: 0 },
            ]],
            source,
        )
    };
    let delay_source = block(
        vec![vec![
            solve::LinearOp::LoadY { dst: 0, index: 0 },
            solve::LinearOp::StoreOutput { src: 0 },
        ]],
        "fmi_me_state_delay_source.mo",
    );
    let delay_time = block(
        vec![vec![
            solve::LinearOp::LoadY { dst: 0, index: 1 },
            solve::LinearOp::StoreOutput { src: 0 },
        ]],
        "fmi_me_state_delay_time.mo",
    );
    let derivative = block(
        vec![
            vec![
                solve::LinearOp::LoadP { dst: 0, index: 0 },
                solve::LinearOp::StoreOutput { src: 0 },
            ],
            vec![
                solve::LinearOp::Const { dst: 0, value: 0.0 },
                solve::LinearOp::StoreOutput { src: 0 },
            ],
        ],
        "fmi_me_state_delay_derivative.mo",
    );
    let solve_layout = solve::SolveLayout {
        solver_maps: solve::SolverNameIndexMaps {
            names: vec!["x".to_string(), "delayDuration".to_string()],
            name_to_idx: IndexMap::from([("x".to_string(), 0), ("delayDuration".to_string(), 1)]),
            ..Default::default()
        },
        state_scalar_count: 2,
        compiled_parameter_len: 1,
        ..Default::default()
    };
    let discrete = solve::DiscreteSolveSystem::default();
    let events = solve::SolveEventPartition {
        delays: solve::SolveDelayPartition {
            source_rhs: delay_source,
            delay_time_rhs: delay_time,
            delay_max_rhs: constant_row(1.0, "fmi_me_state_delay_max.mo"),
            value_parameter_indices: vec![0],
            source_is_discrete: vec![false],
        },
        ..Default::default()
    };
    let clocks = solve::SolveClockPartition::default();
    let continuous = crate::test_support::ContinuousSystemFixture {
        derivative_rhs: solve::ComputeBlock::from_scalar_program_block(derivative),
        ..crate::test_support::ContinuousSystemFixture::empty()
    };
    let continuous = continuous.seal(&solve_layout, &discrete, &events, &clocks);
    let problem = crate::test_support::checked_solve_problem!(
        solve::VarLayout::from_parts(IndexMap::new(), 2, 1),
        solve_layout,
        continuous,
        solve::InitializationSolveSystem::empty(),
        discrete,
        events,
        clocks,
    )
    .expect("continuous-state delay fixture satisfies the checked root contract");
    let model = crate::test_support::checked_solve_model! {
        problem: problem,
        initial_y: vec![0.0, 0.1875],
        solver_nominals: vec![1.0, 1.0],
        parameters: vec![0.0],
        ..empty_binary64_first_product_model()
    };
    with_continuous_state_delay_catalog(model)
}

/// Attach the explicit Real scalar catalog for the continuous-state delay
/// model. Split out for length; every fixture field is unchanged.
fn with_continuous_state_delay_catalog(model: solve::SolveModel) -> solve::SolveModel {
    let provenance = rumoca_core::Span::from_offsets(
        rumoca_core::SourceId::from_source_name("fmi_me_state_delay_variables.mo"),
        1,
        2,
    );
    crate::test_support::with_explicit_real_scalar_catalog(
        model,
        vec![
            crate::test_support::RealScalarVariableFixture::state(
                1, "x", 0, 0.0, 1.0, true, provenance,
            ),
            crate::test_support::RealScalarVariableFixture::state(
                2,
                "delayDuration",
                1,
                0.1875,
                1.0,
                true,
                provenance,
            ),
        ],
        crate::test_support::direct_y_visible_rows([0, 1], provenance),
    )
}

fn enter_delay_continuous_mode(kernel: &mut SolveMeKernel) {
    kernel.enter_initialization_mode(0.0).expect("enter init");
    kernel.exit_initialization_mode().expect("exit init");
    kernel.update_discrete_states().expect("initial update");
    kernel
        .enter_continuous_time_mode()
        .expect("enter continuous-time mode");
}

fn accept_delay_point(kernel: &mut SolveMeKernel, time: f64, source: f64) {
    kernel
        .set_time(MeTime::at(time))
        .expect("set accepted time");
    kernel
        .set_continuous_states(&[source, 0.1875])
        .expect("set accepted state");
    kernel
        .completed_integrator_step(true)
        .expect("publish accepted delay point");
}

#[test]
fn float64_trial_refresh_never_enters_accepted_delay_history() {
    let mut control = instantiate(continuous_state_delay_model());
    let mut rejected_trial = instantiate(continuous_state_delay_model());
    enter_delay_continuous_mode(&mut control);
    enter_delay_continuous_mode(&mut rejected_trial);
    for kernel in [&mut control, &mut rejected_trial] {
        accept_delay_point(kernel, 0.25, 2.0);
        accept_delay_point(kernel, 0.5, 8.0);
    }

    rejected_trial
        .set_time(MeTime::at(0.625))
        .expect("reach a speculative integrator coordinate");
    let trial_source = rejected_trial
        .value_reference("x")
        .expect("the continuous source has a Float64 value reference");
    rejected_trial
        .set_float64(std::slice::from_ref(&trial_source), &[50.0])
        .expect("a legal Float64 state write refreshes current derived facts");

    for kernel in [&mut control, &mut rejected_trial] {
        kernel
            .set_time(MeTime::at(0.375))
            .expect("backtrack no earlier than the second-last accepted point");
        kernel
            .set_continuous_states(&[3.0, 0.1875])
            .expect("replace the rejected trial state");
        kernel
            .set_time(MeTime::at(0.75))
            .expect("evaluate the replacement trajectory");
        kernel
            .set_continuous_states(&[4.0, 0.1875])
            .expect("set the replacement trajectory state");
    }
    let mut control_derivative = [0.0; 2];
    let mut trial_derivative = [0.0; 2];
    control
        .get_continuous_state_derivatives(&mut control_derivative)
        .expect("evaluate the control delay");
    rejected_trial
        .get_continuous_state_derivatives(&mut trial_derivative)
        .expect("evaluate delay after rejecting the speculative coordinate");
    assert_eq!(trial_derivative, control_derivative);
    // At t=.75 the delay reads t=.5625. Correct history interpolates the
    // accepted (.5, 8) point and the current (.75, 4) point to 7. Deleting
    // accepted publication yields 3; publishing the speculative (.625, 50)
    // setter point yields 29. All coordinates and values are binary-exact.
    assert_eq!(trial_derivative, [7.0, 0.0]);

    let duration = rejected_trial
        .value_reference("delayDuration")
        .expect("the delay-time state has a Float64 value reference");
    rejected_trial
        .set_float64(&[trial_source.clone(), duration.clone()], &[7.0, 0.125])
        .expect("a valid Float64 batch refreshes the current delay bound");
    let max_step = rejected_trial
        .max_step_duration_value_reference()
        .expect("a delay-bearing component publishes the standard numerical bound");
    let mut refreshed_bound = [0.0];
    rejected_trial
        .get_float64(std::slice::from_ref(&max_step), &mut refreshed_bound)
        .expect("read the setter-refreshed maximum step duration");
    assert_eq!(refreshed_bound, [0.125]);

    let before_failed_batch = rejected_trial.fmu_state();
    rejected_trial
        .set_float64(&[trial_source, duration], &[77.0, -0.1])
        .expect_err("an invalid refreshed delay time rejects the complete Float64 batch");
    assert!(rejected_trial.verification_matches_snapshot(&before_failed_batch));
}

#[test]
fn maximum_step_duration_is_a_checked_standard_float64_read() {
    let mut kernel = SolveMeKernel::instantiate(
        MeModelSource::fixture(crate::test_support::fmi_component(constant_delay_model())),
        &fixture_instance_config(),
    )
    .expect("the checked delay annotation and kernel are correlated");
    kernel
        .enter_initialization_mode(0.0)
        .expect("enter initialization");
    kernel
        .exit_initialization_mode()
        .expect("exit initialization");
    kernel
        .update_discrete_states()
        .expect("settle the initial event");
    kernel
        .enter_continuous_time_mode()
        .expect("enter continuous-time mode");

    let reference = kernel
        .max_step_duration_value_reference()
        .expect("the checked inventory declares the Float64 local");
    let mut values = [0.0];
    kernel
        .get_float64(std::slice::from_ref(&reference), &mut values)
        .expect("the local is readable through the standard batched getter");
    assert_eq!(values, [0.2]);

    let mut undeclared = reference;
    undeclared.value_reference = u32::MAX;
    let error = kernel
        .get_float64(std::slice::from_ref(&undeclared), &mut values)
        .expect_err("an unreadable value reference is rejected");
    assert!(matches!(error.kind(), MeError::Contract { .. }));
}

#[test]
fn completed_step_failure_does_not_publish_delay_history() {
    let mut kernel = instantiate(constant_delay_model());
    kernel.enter_initialization_mode(0.0).expect("enter init");
    kernel.exit_initialization_mode().expect("exit init");
    kernel.update_discrete_states().expect("initial update");
    kernel
        .enter_continuous_time_mode()
        .expect("initialize accepted delay history");
    kernel
        .set_time(MeTime::at(0.1))
        .expect("reach the next accepted point");
    let before = kernel.fmu_state();
    kernel.verification_fail_next_completed_integrator_step();

    kernel
        .completed_integrator_step(true)
        .expect_err("the injected failure precedes detached delay publication");
    assert!(kernel.verification_matches_snapshot(&before));
    kernel
        .completed_integrator_step(true)
        .expect("retry commits delay history from the same accepted point");
}

#[test]
fn delay_history_retiming_replays_the_same_initialized_component_state() {
    let config = MeInstanceConfig::new("delay-retime", 1.0e-10, 0.25, 1.0)
        .expect("the delay component's nonzero start is checked");
    let retained = MeRetainedComponent::instantiate(
        MeModelSource::fixture(crate::test_support::fmi_component(constant_delay_model())),
        &config,
        MeExecutionSelection::Interpreter,
    )
    .expect("the delay-bearing component instantiates");
    let options = live_session_options(1.0e-10, 1.0e-10, 1.0, None)
        .expect("the live delay session options are checked");
    let host = retained
        .into_lease(options)
        .expect("the delay-bearing host initializes");
    let mut session = host
        .into_session(None)
        .expect("the zero-state delay component uses the time-only plugin");
    let mut cursor = MeOutputCursor::empty();

    session
        .retime(0.75)
        .expect("explicit retiming seeds delay history at the new coordinate");
    let retimed = session.verification_component_snapshot();
    session
        .advance_to(0.9, &mut cursor)
        .expect("advancement commits later delay history");
    assert!(!session.verification_matches_component_snapshot(&retimed));
    session
        .retime(0.75)
        .expect("the same retiming rebuilds delay history from pristine again");
    assert!(session.verification_matches_component_snapshot(&retimed));
}

// -- the sole master algorithm, end to end ---------------------------------

#[test]
fn initialization_termination_retains_exact_state_and_final_reads_across_restarts() {
    use super::integrator::conformance::{HermiteStepIntegrator, SamplerQuality};
    use super::session::{MeRetainedComponent, MeSessionOptions, MeSessionOptionsInput};

    let model = initialization_termination_model();
    let mut retained = MeRetainedComponent::instantiate(
        MeModelSource::fixture(crate::test_support::fmi_component(model)),
        &fixture_instance_config(),
        MeExecutionSelection::Interpreter,
    )
    .expect("the checked initial-termination component instantiates");
    let options = MeSessionOptions::new(MeSessionOptionsInput {
        stop_time: Some(1.0),
        relative_tolerance: 1.0e-8,
        absolute_tolerance: 1.0e-8,
        output_interval: 0.1,
        root_scan_resolution: 0.05,
        root_location_tolerance: 1.0e-10,
        max_wall_seconds: None,
        records_trace: true,
    })
    .expect("the initial-termination options are admissible");
    let host = retained
        .lease(options)
        .expect("host construction consumes the real terminateSimulation request");
    assert!(host.is_terminated());
    assert_eq!(host.state_count(), 2);
    assert_eq!(
        host.termination()
            .map(|termination| termination.message.as_str()),
        Some("terminated during initialization")
    );
    let mut session = host
        .into_session(Some(Box::new(HermiteStepIntegrator::new(
            2,
            SamplerQuality::Native,
        ))))
        .expect("a terminal stateful host retains its exact-width plugin aggregate");

    let assert_terminal_point = |session: &super::session::MeSimulationSession<'_, '_>,
                                 time: f64| {
        assert!(session.is_terminated());
        assert_eq!(session.time().to_bits(), time.to_bits());
        assert_eq!(session.output_values().unwrap(), vec![1.0, 0.0]);
        assert_eq!(session.visible_values().unwrap()["x"], 1.0);
        assert_eq!(session.visible_values().unwrap()["v"], 0.0);
        let getters = session.verification_terminal_getter_vectors();
        assert_eq!(getters.states, vec![1.0, 0.0]);
        assert_eq!(getters.nominals, vec![1.0, 1.0]);
        assert_eq!(getters.derivatives, vec![0.0, -4.0]);
        assert_eq!(getters.directional, vec![1.0, -4.0]);
        assert!(getters.indicators.is_empty());
        let component_point = session.verification_component_point();
        assert_eq!(component_point.0, time.to_bits());
        assert_eq!(
            component_point.1,
            vec![1.0_f64.to_bits(), 0.0_f64.to_bits()]
        );
    };

    assert_terminal_point(&session, 0.0);
    session
        .reset()
        .expect("pristine reset replays initialization termination");
    assert_terminal_point(&session, 0.0);
    session
        .retime(0.75)
        .expect("explicit retiming replays initialization termination at the new coordinate");
    assert_terminal_point(&session, 0.75);
    assert_eq!(
        session
            .termination()
            .map(|termination| termination.time.to_bits()),
        Some(0.75_f64.to_bits())
    );
}

/// The first execution of [`super::session::MeSimulationSession`] over a real
/// component.
///
/// Everything below it — the checked options, the lease, the plugin arity
/// check, the accepted-step proof, the unconditional sampler validation, the
/// output cursor, the trace recorder — is exercised by driving one FMI
/// component with the unrelated conformance plugin. The plugin reaches the
/// component *only* through the capability the host lends per call, which is
/// what makes this evidence for ME-INT-001 rather than for a fixture.
#[test]
fn the_common_host_integrates_a_component_through_the_thin_plugin_contract() {
    use super::integrator::conformance::{HermiteStepIntegrator, SamplerQuality};
    use super::session::{
        MeOutputCursor, MeRetainedComponent, MeSessionOptions, MeSessionOptionsInput,
    };

    let model = harmonic_oscillator();
    let mut retained = MeRetainedComponent::instantiate(
        MeModelSource::fixture(crate::test_support::fmi_component(model)),
        &fixture_instance_config(),
        MeExecutionSelection::Interpreter,
    )
    .expect("the fixture component instantiates");

    let options = MeSessionOptions::new(MeSessionOptionsInput {
        stop_time: Some(1.0),
        relative_tolerance: 1.0e-8,
        absolute_tolerance: 1.0e-8,
        output_interval: 0.1,
        root_scan_resolution: 0.05,
        root_location_tolerance: 1.0e-10,
        max_wall_seconds: None,
        records_trace: true,
    })
    .expect("the fixture options are admissible");

    let host = retained.lease(options).expect("the sole lease is granted");
    let state_count = host.state_count();
    assert_eq!(state_count, 2);
    let mut session = host
        .into_session(Some(Box::new(HermiteStepIntegrator::new(
            state_count,
            SamplerQuality::Native,
        ))))
        .expect("a state-carrying component admits a state-carrying plugin");

    let mut cursor = MeOutputCursor::new(
        (0..=10)
            .map(|tenth| f64::from(tenth) / 10.0)
            .collect::<Vec<_>>(),
    )
    .expect("the output schedule is sorted and unique");
    // Yield at each requested coordinate, exactly as an incremental host does.
    // The conformance plugin takes one accepted step per request, so this is
    // also what bounds its step: the contract, not a plugin-owned schedule.
    for tenth in 0..=10 {
        let boundary = f64::from(tenth) / 10.0;
        let outcome = session
            .advance_to(boundary, &mut cursor)
            .expect("the master algorithm reaches each yield boundary");
        assert!(
            matches!(
                outcome,
                MeAdvanceOutcome::Yielded | MeAdvanceOutcome::ReachedStop
            ),
            "an event-free run neither terminates nor stalls at t={boundary}: {outcome:?}"
        );
        assert!((session.time() - boundary).abs() <= 1.0e-12);
    }
    assert!(session.is_terminated());
    assert!(
        session.verification_component_is_terminated(),
        "a successful defined-experiment end must terminate the FMI component, not only the host"
    );
    let result = session.finish();

    // `der(x) = v`, `der(v) = -4 x` from `x(0) = 1`, `v(0) = 0`, so
    // `x(t) = cos(2t)`.
    assert_eq!(result.names, vec!["x".to_string(), "v".to_string()]);
    assert_eq!(result.times.len(), 11);
    assert_eq!(result.data.len(), 2);
    for (index, time) in result.times.iter().copied().enumerate() {
        let expected = (2.0 * time).cos();
        let actual = result.data[0][index];
        assert!(
            (actual - expected).abs() < 1.0e-4,
            "x({time}) = {actual}, expected {expected}"
        );
    }
    assert!(result.termination.is_none());
}

/// A state-carrying component cannot be given the time-only plugin, and the
/// rejection is typed rather than a rendered message (ME-ZERO-001 in reverse).
#[test]
fn a_state_carrying_component_refuses_the_time_only_plugin() {
    use super::session::{
        MePluginArity, MeRetainedComponent, MeSessionError, MeSessionOptions, MeSessionOptionsInput,
    };

    let model = harmonic_oscillator();
    let mut retained = MeRetainedComponent::instantiate(
        MeModelSource::fixture(crate::test_support::fmi_component(model)),
        &fixture_instance_config(),
        MeExecutionSelection::Interpreter,
    )
    .expect("the fixture component instantiates");
    let options = MeSessionOptions::new(MeSessionOptionsInput {
        stop_time: None,
        relative_tolerance: 1.0e-8,
        absolute_tolerance: 1.0e-8,
        output_interval: 0.1,
        root_scan_resolution: 0.05,
        root_location_tolerance: 1.0e-10,
        max_wall_seconds: None,
        records_trace: false,
    })
    .expect("an open live session needs no defined stop");
    let host = retained.lease(options).expect("the sole lease is granted");
    let failure = host
        .into_session(None)
        .err()
        .expect("two continuous states cannot be advanced by the time-only plugin");
    assert!(matches!(
        failure,
        MeSessionError::PluginArity {
            state_count: 2,
            mismatch: MePluginArity::RequiresANumericalPlugin
        }
    ));
    // The diagnostic names the rule, not a solver: ME-INT-001's thin surface
    // has no identity operation to ask, and no common enum lists backends
    assert_eq!(
        failure.to_string(),
        "a component with 2 continuous states requires a numerical plugin, and none was supplied"
    );
}

fn fixture_instance_config() -> MeInstanceConfig {
    MeInstanceConfig::new("fmi-me-session", 1.0e-10, 0.0, 1.0)
        .expect("fixture instance configuration constructs")
}

// -- the indicator inventory is built once (SPEC_0044 ME-EVENT-001) ---------

/// Sources the indicator step path is drawn from, so the properties below are
/// checked against the code that actually runs.
const COMPONENT_SOURCE: &str = include_str!("kernel/component.rs");
const EVENT_STORAGE_SOURCE: &str = include_str!("kernel/component/event_storage.rs");
const EVENT_BOUNDARY_SOURCE: &str = include_str!("kernel/event_boundary.rs");
const INSTANTIATION_SOURCE: &str = include_str!("kernel/component/instantiation.rs");
const KERNEL_SOURCE: &str = include_str!("kernel.rs");
const SESSION_SOURCE: &str = include_str!("session.rs");
const ROOT_SOURCE: &str = include_str!("root.rs");
const HOST_STATE_SOURCE: &str = include_str!("session/host_state.rs");
const INDICATOR_PLAN_SOURCE: &str = include_str!("kernel/indicator_plan.rs");
const LINKED_RUNTIME_FACTS_SOURCE: &str =
    include_str!("../../../rumoca-ir-solve/src/fmi/linked_runtime.rs");
const SOLVE_OPS_SOURCE: &str = include_str!("../runtime/solve_ops.rs");
const SOLVE_RUNTIME_SOURCE: &str = include_str!("../runtime/solve_runtime.rs");
const SOLVE_RUNTIME_PLANS_SOURCE: &str = include_str!("../runtime/solve_runtime/plans.rs");

/// The body of one method declared at `impl` indentation.
fn method_body<'source>(source: &'source str, signature: &str) -> &'source str {
    let start = source
        .find(signature)
        .unwrap_or_else(|| panic!("{signature} must exist"));
    let body = &source[start..];
    let end = body
        .find("\n    }\n")
        .unwrap_or_else(|| panic!("{signature} must be a method at impl indentation"));
    &body[..end]
}

/// The body of the final method with this signature, used when a private
/// implementation follows its trait declaration in the same source file.
fn last_method_body<'source>(source: &'source str, signature: &str) -> &'source str {
    let start = source
        .rfind(signature)
        .unwrap_or_else(|| panic!("{signature} must exist"));
    let body = &source[start..];
    let end = body
        .find("\n    }\n")
        .unwrap_or_else(|| panic!("{signature} must be a method at impl indentation"));
    &body[..end]
}

fn assert_indicator_storage_issuer(signature: &str, role: &str, factory: &str, width: &str) {
    let issuer = last_method_body(EVENT_STORAGE_SOURCE, signature);
    assert!(
        issuer.contains(role) && issuer.contains(factory) && issuer.contains(width),
        "the storage issuer erased or crossed the {role} plan width"
    );
}

/// The name of the function or method one occurrence sits in.
fn enclosing_function<'source>(source: &'source str, needle: &str) -> &'source str {
    let position = source
        .find(needle)
        .unwrap_or_else(|| panic!("{needle} must exist"));
    let header = source[..position]
        .rfind("\nfn ")
        .into_iter()
        .chain(source[..position].rfind("\n    fn "))
        .chain(source[..position].rfind("\n    pub(super) fn "))
        .chain(source[..position].rfind("\n    pub(crate) fn "))
        .max()
        .expect("an occurrence sits inside a function");
    let name = source[header..position]
        .split("fn ")
        .nth(1)
        .expect("a function header names its function");
    name.split(['(', '<', ' ']).next().unwrap_or_default()
}

#[test]
fn the_indicator_inventory_has_one_constructor_the_step_path_cannot_reach() {
    assert_eq!(
        LINKED_RUNTIME_FACTS_SOURCE
            .matches("\nfn indicator_plan(")
            .count(),
        1,
        "checked FMI construction owns the sole indicator-plan constructor"
    );
    assert_eq!(
        KERNEL_SOURCE.matches("FmiIndicatorPlan::derive").count(),
        0,
        "no operation outside the constructor may build an indicator table"
    );
    assert_eq!(
        enclosing_function(
            LINKED_RUNTIME_FACTS_SOURCE,
            "let events = model.problem().events();",
        ),
        "indicator_plan",
        "the sole indicator table is a checked construction product"
    );
    for source in [COMPONENT_SOURCE, KERNEL_SOURCE] {
        assert_eq!(
            source.matches(".indicator_plan =").count(),
            0,
            "the resolved indicator table is never reassigned"
        );
    }
    for source in [INDICATOR_PLAN_SOURCE, LINKED_RUNTIME_FACTS_SOURCE] {
        assert_eq!(
            source.matches("&mut self").count(),
            0,
            "the resolved indicator table exposes no operation that could mutate it"
        );
    }
}

#[test]
fn the_step_path_neither_masks_nor_reprojects_the_root_vector() {
    for name in [
        "filter_scheduled_root_crossings",
        "root_condition_is_search_active",
        "root_search_is_uniformly_inactive",
    ] {
        for source in [
            COMPONENT_SOURCE,
            KERNEL_SOURCE,
            SOLVE_OPS_SOURCE,
            SOLVE_RUNTIME_SOURCE,
            SOLVE_RUNTIME_PLANS_SOURCE,
        ] {
            assert_eq!(
                source.matches(name).count(),
                0,
                "{name} compensated for an unfiltered inventory and has no successor"
            );
        }
    }
    let evaluation = method_body(COMPONENT_SOURCE, "fn evaluate_inventory_indicators(");
    assert_eq!(
        evaluation.matches("full_solver_y").count(),
        1,
        "an indicator read settles the full algebraic coordinate at most once"
    );
    assert!(
        evaluation
            .contains("if self.indicator_plan().reads_deadlines() && settled_guess.is_none() {"),
        "only a dynamic-time deadline needs a settled algebraic coordinate of its own; \
         a root-only inventory keeps the root search's own restricted refresh"
    );
    for signature in [
        "fn evaluate_inventory_indicators(",
        "fn apply_indicator_zero_sides(",
    ] {
        let body = method_body(COMPONENT_SOURCE, signature);
        for reconstruction in [
            "vec![",
            "Vec::new()",
            ".to_vec()",
            "collect::<Vec<",
            "FmiEventIndicatorSource",
        ] {
            assert!(
                !body.contains(reconstruction),
                "{signature} must read the resolved table, not rebuild one with {reconstruction}"
            );
        }
    }
}

/// ME-LIFE-005: the initial `fmi3UpdateDiscreteStates` announces a
/// solver-reading dynamic deadline through the one construction-reserved event
/// solver workspace. The call succeeds only if the taken workspace is restored
/// to its field before that consumer runs, and the storage identity stays
/// pointer-identical, so consuming the buffer while taken fails the update
/// with its exact-width refusal and replacing it with a fresh allocation or a
/// cloned second authority fails the identity comparison.
#[test]
fn dynamic_deadline_storage_survives_initial_event_cache_and_snapshot() {
    let model = dynamic_deadline_model();
    let mut kernel = instantiate(model);
    kernel
        .enter_initialization_mode(0.0)
        .expect("dynamic-deadline fixture initialization should start");
    kernel
        .exit_initialization_mode()
        .expect("dynamic-deadline fixture initialization should settle");
    let indicator_identity = kernel.verification_indicator_storage();
    assert_eq!(indicator_identity.role_widths(), (1, 0, 1, 1));
    let identity = kernel.verification_event_stage_identity();
    let update = kernel
        .update_discrete_states()
        .expect("the initial event evaluates its dynamic deadline against the restored workspace");
    assert_eq!(
        update.next_event_time,
        Some(0.5),
        "the dynamic deadline is announced as the next time event"
    );
    let after = kernel.verification_event_stage_identity();
    assert!(
        identity.same_construction_object(&after),
        "the initial event replaced construction-reserved event storage: {identity:?} -> {after:?}"
    );
    kernel
        .enter_continuous_time_mode()
        .expect("the dynamic-deadline fixture enters continuous time");
    let mut indicators = vec![0.0; kernel.model_description().event_indicator_count];
    assert_eq!(
        indicators.len(),
        1,
        "the deadline contributes one indicator"
    );
    kernel
        .get_event_indicators(&mut indicators)
        .expect("the first read populates the exact-width indicator cache");
    assert_eq!(kernel.verification_indicator_storage(), indicator_identity);
    let first = indicators.clone();
    kernel
        .get_event_indicators(&mut indicators)
        .expect("the second read consumes the exact-width indicator cache");
    assert_eq!(indicators, first);
    assert_eq!(kernel.verification_indicator_storage(), indicator_identity);
    let saved = kernel.fmu_state();
    kernel
        .set_time(MeTime::at(0.25))
        .expect("advance beyond the cached coordinate");
    kernel
        .reset_to_fmu_state(&saved)
        .expect("restore the cache with the component snapshot");
    kernel
        .get_event_indicators(&mut indicators)
        .expect("the restored cache retains its exact indicator width");
    assert_eq!(indicators, first);
    assert_eq!(kernel.verification_indicator_storage(), indicator_identity);
}

#[test]
fn equal_width_root_and_deadline_roles_route_distinct_values_through_cache_and_snapshot() {
    let mut kernel = instantiate(equal_width_root_deadline_model());
    assert_eq!(kernel.model_description().continuous_state_count, 1);
    let indicator_identity = kernel.verification_indicator_storage();
    assert_eq!(
        indicator_identity.role_widths(),
        (2, 1, 1, 2),
        "one state, one root source, and one deadline source publish two FMI indicators"
    );
    let population_buffers = indicator_identity.buffer_identities();
    assert_eq!(
        (*population_buffers).map(|(_, len)| len),
        [1, 1, 2, 2, 2, 2, 2, 1],
        "root, deadline, working, publication, domain-pair, cached values, and cached state retain their issued widths"
    );
    assert!(
        population_buffers.iter().all(|(_, len)| *len != 0),
        "the equal-width fixture must exercise every typed population"
    );
    for (position, (pointer, _)) in population_buffers.iter().enumerate() {
        assert!(
            population_buffers[..position]
                .iter()
                .all(|(earlier, _)| earlier != pointer),
            "each role population must retain distinct construction storage"
        );
    }
    kernel
        .enter_initialization_mode(0.0)
        .expect("equal-width fixture initialization should start");
    kernel
        .exit_initialization_mode()
        .expect("equal-width fixture initialization should settle");
    let update = kernel
        .update_discrete_states()
        .expect("equal-width fixture initial event should settle");
    assert_eq!(update.next_event_time, Some(0.5));
    kernel
        .enter_continuous_time_mode()
        .expect("equal-width fixture should enter continuous time");

    let mut indicators = [0.0; 2];
    kernel
        .get_event_indicators(&mut indicators)
        .expect("the cache miss evaluates both equal-width roles");
    assert_eq!(
        indicators,
        [-0.5, 0.5],
        "root distance and deadline distance have deliberately opposite signs and fixed plan order"
    );
    assert_eq!(kernel.verification_indicator_storage(), indicator_identity);

    indicators.fill(f64::NAN);
    kernel
        .get_event_indicators(&mut indicators)
        .expect("the identical coordinate takes the cache-hit route");
    assert_eq!(indicators, [-0.5, 0.5]);
    assert_eq!(kernel.verification_indicator_storage(), indicator_identity);

    let saved = kernel.fmu_state();
    kernel
        .set_time(MeTime::at(0.25))
        .expect("a distinct coordinate invalidates the old cache key");
    kernel
        .get_event_indicators(&mut indicators)
        .expect("the distinct coordinate evaluates both roles again");
    assert_eq!(indicators, [-0.5, 0.25]);
    kernel
        .set_continuous_states(&[-0.5])
        .expect("the integrator proposes a point on the opposite side of both domains");
    kernel
        .get_event_indicators(&mut indicators)
        .expect("the changed state refreshes working values before publication");
    assert_eq!(indicators, [0.5, -0.75]);
    kernel
        .completed_integrator_step(true)
        .expect("the accepted point freezes the new domains");
    assert_eq!(
        kernel.verification_indicator_domains(),
        (vec![false, true], vec![true, false]),
        "the completed step retains old domains in the working population and new signs in the frozen population"
    );
    kernel
        .reset_to_fmu_state(&saved)
        .expect("snapshot restore reinstates the original typed cache");
    indicators.fill(f64::NAN);
    kernel
        .get_event_indicators(&mut indicators)
        .expect("the restored cache retains role order and values");
    assert_eq!(indicators, [-0.5, 0.5]);
    assert_eq!(
        kernel.verification_indicator_domains(),
        (vec![false, true], vec![false, true]),
        "snapshot restore reinstates the frozen domains without crossing the working population"
    );
    assert_eq!(kernel.verification_indicator_storage(), indicator_identity);
}

#[test]
fn event_indicator_cache_shape_and_aggregate_roles_have_one_construction_authority() {
    let reservation = method_body(EVENT_STORAGE_SOURCE, "fn try_reserved(");
    assert!(reservation.contains("plan.try_cached_values("));
    assert!(reservation.contains("live_state: &[f64]"));
    assert!(reservation.contains("live_state.len()"));
    assert!(!reservation.contains("state_count"));
    for wrong_width in ["root_value_width", "deadline_width"] {
        assert!(
            !reservation.contains(wrong_width),
            "the published FMI cache must not be sized from {wrong_width}"
        );
    }
    let store = method_body(EVENT_STORAGE_SOURCE, "fn store_publication(");
    assert!(store.contains("values: &FmiPublicationIndicatorValues"));
    assert!(!store.contains("values: &[f64]"));
    let aggregate = method_body(EVENT_STORAGE_SOURCE, "fn try_construct(");
    assert!(aggregate.contains("live_state: &[f64]"));
    assert!(!aggregate.contains("state_count"));
    for issuer in [
        "plan.try_root_values()",
        "plan.try_deadline_values()",
        "plan.try_working_values(",
        "plan.try_publication_values(",
        "plan.try_working_domains(",
        "plan.try_frozen_domains(",
    ] {
        assert!(
            aggregate.contains(issuer),
            "the aggregate constructor must obtain each role from its plan issuer: {issuer}"
        );
    }
    for interchangeable_width in [
        "plan.root_value_width()",
        "plan.deadline_width()",
        "plan.len()",
    ] {
        assert!(
            !aggregate.contains(interchangeable_width),
            "the aggregate constructor must not accept interchangeable widths: {interchangeable_width}"
        );
    }
    assert!(INSTANTIATION_SOURCE.contains("indicator_plan, &states, config.start_time"));
}

#[test]
fn event_indicator_plan_widths_reach_only_their_typed_storage_issuers() {
    for (signature, role, field) in [
        (
            "pub const fn root_value_width(",
            "FmiRootValueWidth",
            "self.root_value_width",
        ),
        (
            "pub const fn deadline_width(",
            "FmiDeadlineWidth",
            "self.deadline_width",
        ),
        (
            "pub const fn published_width(",
            "FmiPublishedIndicatorWidth",
            "self.published_width",
        ),
        (
            "pub const fn domain_width(",
            "FmiIndicatorDomainWidth",
            "self.domain_width",
        ),
    ] {
        let issuer = method_body(LINKED_RUNTIME_FACTS_SOURCE, signature);
        assert!(issuer.contains(role));
        assert!(issuer.contains(field));
    }
    for field in [
        "published_width: FmiPublishedIndicatorWidth",
        "root_value_width: FmiRootValueWidth",
        "deadline_width: FmiDeadlineWidth",
        "domain_width: FmiIndicatorDomainWidth",
    ] {
        assert!(
            LINKED_RUNTIME_FACTS_SOURCE.contains(field),
            "the checked plan must store its width as the role capability {field}"
        );
    }
    assert_indicator_storage_issuer(
        "fn try_root_values(",
        "RootIndicatorValues",
        "RootIndicatorValues::try_for_width",
        "self.root_value_width()",
    );
    assert_indicator_storage_issuer(
        "fn try_deadline_values(",
        "DeadlineIndicatorValues",
        "DeadlineIndicatorValues::try_for_width",
        "self.deadline_width()",
    );
    for (signature, role) in [
        ("fn try_working_values(", "WorkingPublishedIndicatorValues"),
        (
            "fn try_publication_values(",
            "FmiPublicationIndicatorValues",
        ),
        ("fn try_cached_values(", "CachedPublishedIndicatorValues"),
    ] {
        assert_indicator_storage_issuer(
            signature,
            role,
            &format!("{role}::try_for_published_width"),
            "self.published_width()",
        );
    }
    for (signature, role) in [
        ("fn try_working_domains(", "WorkingIndicatorDomains"),
        ("fn try_frozen_domains(", "FrozenIndicatorDomains"),
    ] {
        assert_indicator_storage_issuer(
            signature,
            role,
            &format!("{role}::try_for_domain_width"),
            "self.domain_width()",
        );
    }
}

#[test]
fn event_indicator_equal_width_populations_have_distinct_roles_and_transfers() {
    for population in [
        "WorkingPublishedIndicatorRole",
        "FmiPublicationIndicatorRole",
        "CachedPublishedIndicatorRole",
        "WorkingIndicatorDomainRole",
        "FrozenIndicatorDomainRole",
    ] {
        assert!(
            EVENT_STORAGE_SOURCE.contains(population),
            "each equal-width population needs its own compile-time role: {population}"
        );
    }
    for field in [
        "working_values: RefCell<WorkingPublishedIndicatorValues>",
        "publication_values: RefCell<FmiPublicationIndicatorValues>",
        "values: CachedPublishedIndicatorValues",
        "working_domains: WorkingIndicatorDomains",
        "frozen_domains: FrozenIndicatorDomains",
    ] {
        assert!(
            EVENT_STORAGE_SOURCE.contains(field),
            "the aggregate erased a population role at its storage field: {field}"
        );
    }
    assert!(!EVENT_STORAGE_SOURCE.contains("type PublishedIndicatorValues ="));
    assert!(!EVENT_STORAGE_SOURCE.contains("type IndicatorDomains ="));
    for transfer in [
        "fn copy_from_publication(",
        "fn copy_into_publication(",
        "fn copy_into_working(",
        "fn copy_from_working(",
        "fn copy_from_frozen(",
    ] {
        assert!(
            EVENT_STORAGE_SOURCE.contains(transfer),
            "the aggregate lost its explicit typed transfer {transfer}"
        );
    }
    let publication_cache = method_body(EVENT_STORAGE_SOURCE, "fn store_publication_cache(");
    assert!(publication_cache.contains("values: &FmiPublicationIndicatorValues"));
    assert!(!publication_cache.contains("values: &[f64]"));

    let detached = method_body(EVENT_STORAGE_SOURCE, "fn try_detached_stage(");
    assert!(detached.contains("try_same_shape"));
    for foreign_shape in ["plan:", "FmiIndicatorPlan", "Self::try_construct"] {
        assert!(
            !detached.contains(foreign_shape),
            "detached staging must derive its shape from live storage, not {foreign_shape}"
        );
    }
}

#[test]
fn the_indicator_step_path_never_rebuilds_or_regrows_its_storage() {
    let model = strict_root_relation_memory(-0.5);
    let mut kernel = instantiate(model);
    kernel
        .enter_initialization_mode(0.0)
        .expect("stepping fixture initialization should start");
    kernel
        .exit_initialization_mode()
        .expect("stepping fixture initialization should settle");
    kernel
        .update_discrete_states()
        .expect("stepping fixture initial event should run");
    kernel
        .enter_continuous_time_mode()
        .expect("stepping fixture should enter continuous time");

    let mut indicators = vec![0.0; kernel.model_description().event_indicator_count];
    kernel
        .get_event_indicators(&mut indicators)
        .expect("the stepping fixture publishes its root distance");
    assert_eq!(
        indicators.len(),
        1,
        "the fixture must exercise a non-empty inventory"
    );
    let identity = kernel.verification_indicator_storage();
    let event_stage_identity = kernel.verification_event_stage_identity();
    assert!(
        event_stage_identity.shares_construction_facts(),
        "live and staged continuation share the one construction-issued fact object"
    );
    // The sweep crosses the root, so the completed-step callback exercises both
    // the unchanged domains of an ordinary step and the armed crossing of an
    // accepted event point.
    for step in 1..=16u32 {
        let time = f64::from(step) * 0.05;
        kernel
            .set_time(MeTime::at(time))
            .expect("the integrator advances component time");
        kernel
            .set_continuous_states(&[-0.5 + time])
            .expect("the integrator advances the continuous state");
        kernel
            .get_event_indicators(&mut indicators)
            .expect("the host reads indicators at every trial point");
        assert!(
            !kernel
                .completed_integrator_step(true)
                .expect("the host completes every accepted step")
                .enter_event_mode,
            "a state-domain change is not an FMU-owned step event"
        );
        assert_eq!(
            kernel.verification_indicator_storage(),
            identity,
            "step {step} rebuilt or regrew the FMI event-indicator storage"
        );
        let current_event_stage_identity = kernel.verification_event_stage_identity();
        assert!(
            event_stage_identity.same_construction_object(&current_event_stage_identity),
            "step {step} replaced the construction-reserved event stage or linked facts: {event_stage_identity:?} -> {current_event_stage_identity:?}"
        );
        assert_eq!(
            current_event_stage_identity, event_stage_identity,
            "step {step} regrew construction-reserved event storage"
        );
    }

    kernel
        .enter_event_mode()
        .expect("the importer enters Event Mode at the located root boundary");
    kernel
        .update_discrete_states()
        .expect("the construction-reserved stage settles UpdateDiscreteStates");
    assert_eq!(
        kernel.verification_event_stage_identity(),
        event_stage_identity,
        "Event Mode plus UpdateDiscreteStates regrew construction-reserved event storage"
    );
}

#[test]
fn the_host_enters_event_mode_for_a_located_state_event_without_callback_permission() {
    let body = method_body(SESSION_SOURCE, "fn apply_located_root(");
    assert!(body.contains("self.enter_event_mode_and_settle(event_time)"));
    assert!(body.contains("self.host.completed_integrator_step()?"));
    assert!(
        !body.contains("completed.enter_event_mode"),
        "the state-event branch must not ask the completed-step callback for permission"
    );
}

#[test]
fn completed_step_freezes_domains_but_event_mode_owns_state_classification() {
    let completed = method_body(KERNEL_SOURCE, "pub(crate) fn completed_integrator_step(");
    assert!(completed.contains("freeze_completed_indicator_domains()"));
    for forbidden in ["classify_entered_state_event", "capture_event_entry"] {
        assert!(
            !completed.contains(forbidden),
            "CompletedIntegratorStep must not classify a state event through {forbidden}"
        );
    }
    let event_mode = method_body(KERNEL_SOURCE, "pub(crate) fn enter_event_mode(");
    assert!(event_mode.contains("classify_entered_state_event()"));
}

/// SPEC_0036 / SPEC_0043 §8 / SPEC_0044 §6: the linked kernel issues one root
/// scan shape whose state and published-indicator widths remain distinct types
/// through storage reservation. Swapping either capability at an issuer body
/// is therefore a Rust type error, not a runtime mismatch to detect. The host
/// accepts only that aggregate and scan buffers remain fixed-width boxed slices.
#[test]
fn the_root_scan_shape_keeps_width_roles_typed_through_fixed_storage() {
    // The whole production half of the module, before its test submodule.
    let production = ROOT_SOURCE
        .split("#[cfg(test)]\nmod tests")
        .next()
        .expect("the root module has a production half");
    assert!(
        !production.contains("fn copy_scan_values"),
        "the grow-and-shrink scan repair helper must be deleted, not retained"
    );
    assert!(
        !production.contains("require_indicator_width"),
        "the runtime width recheck of a construction-issued fact must be gone"
    );
    // Every scan population has a fixed-width backing and keeps its semantic
    // role in the field type, so equal lengths do not make populations
    // interchangeable.
    for typed in [
        "values: Box<[f64]>",
        "retained: RootScanIndicatorBuffer",
        "states: RootScanStateBuffer",
        "indicators: RootScanIndicatorBuffer",
    ] {
        assert!(
            production.contains(typed),
            "the scan workspace must hold typed fixed-width storage: {typed}"
        );
    }
    let shape_issuer = method_body(KERNEL_SOURCE, "pub(super) fn root_scan_shape(");
    assert!(shape_issuer.contains("RootScanStateWidth::issue(self.body.state_domain)"));
    assert!(
        shape_issuer.contains(
            "RootScanIndicatorWidth::issue(self.body.indicator_plan().published_width())"
        )
    );
    assert!(
        KERNEL_SOURCE
            .contains("const fn issue(published_width: FmiPublishedIndicatorWidth) -> Self")
    );
    assert!(
        KERNEL_SOURCE.contains("const fn issue(state_domain: MeContinuousStateDomain) -> Self")
    );
    assert!(!KERNEL_SOURCE.contains("const fn issue(state_count: usize) -> Self"));

    let workspace = method_body(ROOT_SOURCE, "fn new(shape: RootScanShape)");
    assert!(workspace.contains("RootScanStateBuffer::try_for_width(shape.state_width()"));
    assert!(workspace.contains("RootScanIndicatorBuffer::try_for_width("));
    assert!(!workspace.contains("state_count"));
    assert!(!workspace.contains("indicator_count"));
    let sample = method_body(ROOT_SOURCE, "fn reserved(shape: &RootScanShape)");
    assert!(sample.contains("RootScanStateBuffer::try_for_width("));
    assert!(sample.contains("shape.indicator_width()"));
    assert!(ROOT_SOURCE.contains("width: RootScanStateWidth"));
    assert!(ROOT_SOURCE.contains("width: RootScanIndicatorWidth"));

    let root_owner = method_body(ROOT_SOURCE, "pub(super) fn new(kernel: &SolveMeKernel)");
    assert!(root_owner.contains("kernel.continuous_state_domain()"));
    assert!(root_owner.contains("RootScanWorkspace::new(kernel.root_scan_shape())"));
    let session_production = SESSION_SOURCE
        .split("#[cfg(test)]\npub(super) struct VerificationTerminalGetterVectors")
        .next()
        .expect("the session has a production half");
    assert!(!session_production.contains("RootScanWorkspace::new("));
    assert!(!session_production.contains("root_scan_shape()"));
    assert!(!session_production.contains("state_count: usize"));
    assert!(!HOST_STATE_SOURCE.contains("state_count: usize"));
    assert!(
        !HOST_STATE_SOURCE.contains("pub(super) indicator_count: usize"),
        "the host must not retain a second bare indicator-width authority"
    );
    // The only runtime check the scan keeps on an indicator vector is finiteness.
    assert!(production.contains("fn require_finite_indicators("));
}

/// SPEC_0038: `fmi3GetEventIndicators` evaluates into the construction-reserved
/// caller-publication scratch, and the scanned bodies (the getter, the
/// off-point indicator reads, and the session-level retained refresh) spell
/// out none of the banned allocation tokens. The scan is a substring read of
/// those bodies: it cannot observe transitive evaluation, and shipped
/// transitive paths (`full_solver_y`, the `cached_continuous_solver_y` clone
/// on a linearization-cache hit, the evaluator, delay workspaces) do reserve
/// on their own terms during an indicator read, as does the state-coordinate
/// copy `read_into` makes one call below the scanned off-point bodies. Unlike
/// the derivative and directional getters, the event-indicator getter has no
/// behavioral late-failure witness; that failure-atomicity gap is open.
#[test]
fn the_event_indicator_getters_use_construction_reserved_caller_publication_storage() {
    let getter = method_body(KERNEL_SOURCE, "pub(crate) fn get_event_indicators(");
    assert!(
        getter.contains("self.body.indicator_storage.publication_values_mut()"),
        "the getter evaluates into the construction-reserved scratch"
    );
    for allocation in [
        "reserved_float64_values",
        "me_float_buffer",
        "Vec::new(",
        "Vec::with_capacity(",
        "vec![",
        ".to_vec(",
        "collect::<Vec",
        "try_reserve",
    ] {
        assert!(
            !getter.contains(allocation),
            "fmi3GetEventIndicators reintroduced a per-call allocation via {allocation}"
        );
    }
    // The off-point indicator read (the former per-accepted-step allocation and
    // wholesale replacement) publishes into the caller's indicator buffer
    // without replacing or reallocating that buffer. The scan reads only these
    // two wrapper bodies; the excursion setup one call below (`read_into`)
    // copies the state coordinate and does allocate.
    let off_point = method_body(HOST_STATE_SOURCE, "pub(super) fn indicators_into(");
    let refresh = method_body(
        HOST_STATE_SOURCE,
        "pub(super) fn refresh_retained_indicators_into(",
    );
    for (label, body) in [("indicators_into", off_point), ("refresh", refresh)] {
        for allocation in ["me_float_buffer", "Vec::new(", "vec![", ".to_vec("] {
            assert!(
                !body.contains(allocation),
                "the off-point indicator path {label} reintroduced an allocation via {allocation}"
            );
        }
    }
    // The session-level refresh fills the workspace's retained vector in place;
    // it neither takes-and-replaces nor allocates a second authority.
    let session_refresh = method_body(SESSION_SOURCE, "fn refresh_retained_indicators(");
    assert!(session_refresh.contains("workspace.refresh_retained("));
    for allocation in ["std::mem::take", "Vec::new(", "vec![", ".to_vec("] {
        assert!(
            !session_refresh.contains(allocation),
            "the retained refresh reintroduced a replacement/allocation via {allocation}"
        );
    }
}

/// SPEC_0038: the continuous-state derivative getter (the right-hand side of
/// every integrator residual) and the directional-derivative getter (the
/// Newton Jacobian-action path) evaluate into construction-reserved
/// caller-publication storage and publish to the caller only after success.
/// The scan spans the two getter bodies and keeps the obsolete direct
/// publication-reservation helper out of the kernel.
///
/// This is deliberately limited to caller publication: evaluator, JVP, and
/// delay workspaces retain resizing paths. The scan and the nonzero runtime
/// identity witness cover only the construction-reserved caller-publication
/// storage visible in these getter bodies.
#[test]
fn the_derivative_getters_use_construction_reserved_caller_publication_storage() {
    assert_eq!(
        KERNEL_SOURCE.matches("reserved_float64_values").count(),
        0,
        "the obsolete caller-publication reservation helper must stay deleted from the kernel"
    );
    let derivative = method_body(
        KERNEL_SOURCE,
        "pub(crate) fn get_continuous_state_derivatives(",
    );
    assert!(
        derivative.contains("self.body.derivative_output_scratch.borrow_mut()"),
        "the derivative getter uses construction-reserved caller-publication storage"
    );
    let directional = method_body(KERNEL_SOURCE, "pub(crate) fn get_directional_derivative(");
    for scratch in [
        "self.body.directional_seed_scratch.borrow_mut()",
        "self.body.directional_sensitivity_scratch.borrow_mut()",
        "self.body.directional_serialized_scratch.borrow_mut()",
    ] {
        assert!(
            directional.contains(scratch),
            "the directional getter must use construction-reserved caller-publication storage: \
             {scratch}"
        );
    }
    assert!(
        directional.contains("full_seed.fill(0.0)"),
        "the reused full seed must be refilled with zeros before the scatter"
    );
    for (label, body) in [("derivative", derivative), ("directional", directional)] {
        for allocation in [
            "reserved_float64_values",
            "me_float_buffer",
            "Vec::new(",
            "Vec::with_capacity(",
            "vec![",
            ".to_vec(",
            "collect::<Vec",
            "try_reserve",
        ] {
            assert!(
                !body.contains(allocation),
                "the {label} getter introduced separate caller-publication storage via {allocation}"
            );
        }
    }
}

#[test]
fn event_transactions_reuse_the_construction_reserved_stage_without_plan_clone() {
    assert!(!KERNEL_SOURCE.contains("detached_event_stage"));
    assert!(!INDICATOR_PLAN_SOURCE.contains("derive(Clone"));
    assert!(!LINKED_RUNTIME_FACTS_SOURCE.contains("impl Clone for FmiEventIndicatorPlan"));
    assert_eq!(
        COMPONENT_SOURCE
            .matches("construction_event_stage()")
            .count(),
        1,
        "one staging continuation is reserved by instantiation"
    );
    for signature in [
        "pub(crate) fn update_discrete_states(",
        "pub(crate) fn completed_integrator_step(",
    ] {
        let body = method_body(KERNEL_SOURCE, signature);
        for allocation in [".clone()", "Vec::new()", "vec![", ".to_vec()"] {
            assert!(
                !body.contains(allocation),
                "{signature} must reuse staging rather than allocate via {allocation}"
            );
        }
        assert!(body.contains("self.event_stage"));
    }
}

#[test]
fn deep_event_lifecycle_helpers_do_not_construct_or_clone_vectors() {
    let component_methods = [
        "pub(super) fn capture_event_entry(",
        "pub(super) fn exit_initialization_mode_inner(",
        "fn exit_initialization_mode_with_storage(",
        "pub(super) fn apply_discrete_event_updates(",
        "fn apply_discrete_event_updates_with_storage(",
        "pub(super) fn prepare_event_pre_for_update(",
        "pub(super) fn run_initial_event_boundary(",
        "pub(super) fn run_runtime_event_boundary(",
        "pub(super) fn discrete_states_after_update(",
    ];
    let boundary_methods = [
        "pub(super) fn process_runtime_event_boundary(",
        "fn apply_event_right_limit(",
    ];
    for (source, signature) in component_methods
        .iter()
        .map(|signature| (COMPONENT_SOURCE, *signature))
        .chain(
            boundary_methods
                .iter()
                .map(|signature| (EVENT_BOUNDARY_SOURCE, *signature)),
        )
    {
        let body = method_body(source, signature);
        for allocation in [
            "Vec::new(",
            "Vec::with_capacity(",
            "vec![",
            ".to_vec(",
            "collect::<Vec",
            "self.params.clone(",
            "self.states.clone(",
            "solver_y_guess.borrow().clone(",
        ] {
            assert!(
                !body.contains(allocation),
                "{signature} must reuse construction storage, not {allocation}"
            );
        }
    }
}

#[test]
fn linked_instantiation_consumes_only_opaque_facts_and_the_shared_execution_root() {
    for forbidden in [".variables()", "SolveStorageColumn", "FmiValueBacking"] {
        assert!(
            !INSTANTIATION_SOURCE.contains(forbidden),
            "linked instantiation reopened FMI construction through {forbidden}"
        );
    }
    assert!(INSTANTIATION_SOURCE.contains("float64_descriptors()"));
    assert!(INSTANTIATION_SOURCE.contains("let (runtime_view, configuration)"));
    assert!(INSTANTIATION_SOURCE.contains("SolveRuntime::new_fmi(runtime_view"));
    assert!(!INSTANTIATION_SOURCE.contains("SolveRuntime::new(model"));
}

#[test]
fn completed_step_requirement_is_identical_before_and_after_linking() {
    for model in [harmonic_oscillator(), strict_root_relation_memory(-1.0)] {
        let component = crate::test_support::fmi_component(model);
        let before = component.needs_completed_integrator_step();
        let kernel = SolveMeKernel::instantiate(
            MeModelSource::fixture(component),
            &fixture_instance_config(),
        )
        .expect("checked component links");
        assert_eq!(
            kernel.model_description().needs_completed_integrator_step,
            before
        );
    }
}

#[test]
fn lifecycle_invariant_refusals_have_no_reseed_or_live_clone_repairs() {
    for forbidden in [
        ".is_none_or(|memory|",
        "previous.get(position).copied().unwrap_or",
        ".unwrap_or_else(|| self.params.clone())",
        ".unwrap_or_else(|| self.current_solver_y())",
    ] {
        assert!(
            !COMPONENT_SOURCE.contains(forbidden),
            "lifecycle code retains the semantic repair `{forbidden}`"
        );
    }
    assert!(!include_str!("../fmi_me.rs").contains("MeCompletedIntegratorStep::default"));
    assert!(
        !COMPONENT_SOURCE.contains("differs from constructed width"),
        "runtime code must not re-prove a construction-issued width"
    );
    assert!(COMPONENT_SOURCE.contains("requires a latched pre-event parameter vector"));
}
