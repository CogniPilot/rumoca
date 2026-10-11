use super::*;
use rumoca_core::{SourceId, Span};

fn block(program: Vec<LinearOp>) -> ScalarProgramBlock {
    let span = Span::from_offsets(SourceId::from_source_name("numeric-memory.mo"), 0, 1);
    ScalarProgramBlock::with_program_spans(vec![program], vec![span]).unwrap()
}

fn fixture() -> SolveProblem {
    let prefix = vec![LinearOp::LoadTime { dst: 0 }];
    let root = ratio_indicator(&prefix, 0).unwrap();
    let mut producer = prefix;
    producer.extend([
        LinearOp::Unary {
            dst: 1,
            op: UnaryOp::Floor,
            src: 0,
        },
        LinearOp::StoreOutput { src: 1 },
    ]);
    let rhs = block(producer);
    let mut problem = SolveProblem::default();
    problem.discrete.rhs = rhs.clone();
    problem.discrete.update_targets = vec![scalar_slot_p(0)];
    problem.discrete.row_roles = vec![DiscreteRowRole::QuotientMemory {
        step: UnaryOp::Floor,
        root_index: 0,
    }];
    problem.discrete.clock_owners = vec![None];
    problem.discrete.pre_modes = vec![DiscreteEventPreMode::FollowCurrent];
    problem.discrete.observation_refresh = vec![false];
    problem.events.root_conditions = block(root);
    problem.initialization = InitializationSolveSystem::construct(InitializationSystemInput {
        update_rhs: rhs,
        update_targets: vec![scalar_slot_p(0)],
        ..Default::default()
    })
    .unwrap();
    problem
}

#[test]
fn quotient_certificate_binds_the_exact_live_ratio() {
    let mut problem = fixture();
    validate(&problem).unwrap();
    let wrong_root = ratio_indicator(&[LinearOp::LoadP { dst: 0, index: 0 }], 0).unwrap();
    problem.events.root_conditions = block(wrong_root);
    let error = validate(&problem).unwrap_err().to_string();
    assert!(error.contains("exact live ratio"), "{error}");
}

#[test]
fn quotient_certificate_rejects_missing_or_wrong_rounding_owner() {
    let mut problem = fixture();
    problem.discrete.row_roles[0] = DiscreteRowRole::QuotientMemory {
        step: UnaryOp::Trunc,
        root_index: 0,
    };
    assert!(
        validate(&problem)
            .unwrap_err()
            .to_string()
            .contains("rounding step")
    );
    problem.discrete.row_roles[0] = DiscreteRowRole::QuotientMemory {
        step: UnaryOp::Floor,
        root_index: 1,
    };
    assert!(
        validate(&problem)
            .unwrap_err()
            .to_string()
            .contains("live root owner")
    );
}

#[test]
fn quotient_certificate_rejects_reserved_storage_and_observation_refresh() {
    let mut problem = fixture();
    problem.solve_layout.initial_event_parameter_index = Some(0);
    assert!(validate(&problem).is_err());
    problem.solve_layout.initial_event_parameter_index = None;
    problem.solve_layout.pre_param_bindings = vec![PreParamBinding {
        dest_p_index: 0,
        source: PreParamSource::P { index: 1 },
        clock_schedule: None,
    }];
    assert!(validate(&problem).is_err());
    problem.solve_layout.pre_param_bindings.clear();
    problem.discrete.observation_refresh[0] = true;
    assert!(validate(&problem).is_err());
}

#[test]
fn quotient_certificate_requires_identical_initialization() {
    let mut problem = fixture();
    problem.initialization = InitializationSolveSystem::construct(InitializationSystemInput {
        update_rhs: block(vec![
            LinearOp::Const { dst: 0, value: 0.0 },
            LinearOp::StoreOutput { src: 0 },
        ]),
        update_targets: vec![scalar_slot_p(0)],
        ..Default::default()
    })
    .unwrap();
    assert!(
        validate(&problem)
            .unwrap_err()
            .to_string()
            .contains("initialization does not match")
    );
}
