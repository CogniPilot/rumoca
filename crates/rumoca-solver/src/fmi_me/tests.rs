//! Contract tests for the ME operations SPEC_0038 phase 2 added.
//!
//! The staging tests are pure: they exercise the value-level rules a host's
//! failure bucketing depends on without instantiating a component. The
//! directional-derivative tests run a real component, because the operation's
//! whole point is that the *component* owns the derivative.

use rumoca_ir_solve as solve;

use super::{
    MeError, MeInstanceConfig, MeModelSource, MeStage, ModelExchangeKernel, SolveMeKernel,
    resolve_me_stage,
};

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
    SolveMeKernel::instantiate(
        MeModelSource::new(model),
        &MeInstanceConfig {
            instance_name: "fmi-me-test",
            tolerance: 1.0e-10,
            start_time: 0.0,
            stop_time: 1.0,
        },
    )
    .expect("fixture instantiates")
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
        },
    )
    .err()
    .expect("an initial vector that contradicts the solver layout is rejected");

    assert_eq!(error.stage(), Some(MeStage::Instantiate));
    assert!(matches!(error.kind(), MeError::Evaluation { .. }));
}
