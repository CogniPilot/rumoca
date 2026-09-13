use super::*;
use crate::{InitializationProjectionBlock, LinearOp, SolveProblem, scalar_slot_y};
use rumoca_core::{SourceId, Span};

fn input() -> InitializationSystemInput {
    let span = Span::from_offsets(SourceId::from_source_name("initialization_owner.mo"), 0, 1);
    InitializationSystemInput {
        residual: ComputeBlock::from_scalar_program_block(
            ScalarProgramBlock::with_source_span(
                vec![vec![
                    LinearOp::LoadY { dst: 0, index: 0 },
                    LinearOp::StoreOutput { src: 0 },
                ]],
                span.require_provenance("initialization owner fixture")
                    .unwrap(),
            )
            .unwrap(),
        ),
        row_roles: vec![InitializationRowRole::Solved],
        projection_plan: InitializationProjectionPlan {
            blocks: vec![InitializationProjectionBlock {
                rows: vec![0],
                unknowns: vec![scalar_slot_y(0)],
            }],
        },
        ..Default::default()
    }
}

#[test]
fn replay_derives_targets_and_unknowns_from_the_same_plan() {
    let owner = InitializationSolveSystem::construct(input()).unwrap();
    let wire = serde_json::to_value(&owner).unwrap();
    assert!(wire.get("row_targets").is_none());
    assert!(wire.get("projection_unknowns").is_none());
    let replay: InitializationSolveSystem = serde_json::from_value(wire).unwrap();
    assert_eq!(replay.row_targets(), [Some(scalar_slot_y(0))]);
    assert_eq!(replay.projection_unknowns(), [scalar_slot_y(0)]);
}

#[test]
fn a_wire_cannot_supply_an_independent_target_inventory() {
    let mut wire =
        serde_json::to_value(InitializationSolveSystem::construct(input()).unwrap()).unwrap();
    wire["row_targets"] = serde_json::json!([null]);
    assert!(serde_json::from_value::<InitializationSolveSystem>(wire).is_err());
}

#[test]
fn a_row_cannot_claim_to_solve_an_unknown_owned_by_another_row() {
    let mut parts = input();
    parts.projection_plan = InitializationProjectionPlan::default();
    assert!(matches!(
        InitializationSolveSystem::construct(parts),
        Err(SolveProblemShapeContractError::InitializationOwnership { .. })
    ));
}

#[test]
fn an_update_cannot_overwrite_a_projected_coordinate() {
    let mut parts = input();
    parts.update_rhs = match &parts.residual.nodes[0] {
        crate::ComputeNode::ScalarPrograms(block) => block.clone(),
        _ => unreachable!(),
    };
    parts.update_targets = vec![scalar_slot_y(0)];
    assert!(matches!(
        InitializationSolveSystem::construct(parts),
        Err(SolveProblemShapeContractError::InitializationOwnership { .. })
    ));
}

#[test]
fn a_manifold_suffix_cannot_exceed_the_complete_initial_system() {
    let mut parts = input();
    parts.manifold_row_count = 2;
    assert!(InitializationSolveSystem::construct(parts).is_err());
}

#[test]
fn root_assembly_requires_every_continuous_manifold_row_at_initialization() {
    let mut problem = SolveProblem::default();
    problem.continuous.manifold_residual = input().residual;
    let error = crate::validate_initialization_system_shape(&problem).unwrap_err();
    assert!(
        error
            .to_string()
            .contains("initialization.manifold_row_count"),
        "{error}"
    );
}

#[test]
fn a_given_initial_state_cannot_be_claimed_by_the_projection() {
    let mut parts = input();
    parts.given_state_indices = vec![0];
    assert!(matches!(
        InitializationSolveSystem::construct(parts),
        Err(SolveProblemShapeContractError::InitializationOwnership { .. })
    ));
}
