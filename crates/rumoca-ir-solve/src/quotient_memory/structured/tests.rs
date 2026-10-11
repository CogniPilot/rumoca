use super::*;
use rumoca_core::{
    IndexBinderProvenance, SourceId, Span, StructuredIndexBinder, StructuredIndexDomain,
};

fn fixture(descending: bool) -> SolveProblem {
    let span = Span::from_offsets(SourceId::from_source_name("structured-memory.mo"), 0, 1);
    let (lower, upper, step) = if descending { (2, 1, -1) } else { (1, 2, 1) };
    let domain = StructuredIndexDomain {
        binders: vec![StructuredIndexBinder {
            provenance: IndexBinderProvenance::Generated,
            id: 0,
            display_name: "i".into(),
            lower,
            upper,
            step,
        }],
    };
    let prefix = |index| {
        vec![
            LinearOp::Const {
                dst: 0,
                value: index as f64,
            },
            LinearOp::LoadTime { dst: 1 },
            LinearOp::Binary {
                dst: 2,
                op: BinaryOp::Add,
                lhs: 1,
                rhs: 0,
            },
        ]
    };
    let mut base = prefix(lower);
    base.extend([
        LinearOp::Unary {
            dst: 3,
            op: UnaryOp::Floor,
            arg: 2,
        },
        LinearOp::StoreOutput { src: 3 },
    ]);
    let roots = [lower, upper]
        .into_iter()
        .map(|point| ratio_indicator(&prefix(point), 2).unwrap())
        .collect();
    let mut problem = SolveProblem::default();
    problem.layout = VarLayout::from_parts(Default::default(), 0, 2);
    problem.solve_layout.parameter_count = 2;
    problem.solve_layout.compiled_parameter_len = 2;
    problem.discrete.structured_rhs = ComputeBlock {
        nodes: vec![ComputeNode::Map {
            output_map: TensorOutputMap::dense_contiguous(0, &domain).unwrap(),
            domain: domain.clone(),
            base_ops: base,
            load_strides: Vec::new(),
            const_strides: vec![AffineStencilConstStride {
                op_position: 0,
                terms: vec![AffineStencilConstStrideTerm {
                    dimension: 0,
                    stride: step as f64,
                }],
            }],
            metadata: TensorNodeMetadata::default(),
            span,
        }],
    };
    problem.discrete.structured_updates = vec![StructuredDiscreteUpdate {
        node_index: 0,
        target: StructuredDiscreteTargetMap {
            base: scalar_slot_p(0),
            map: TensorOutputMap::dense_contiguous(0, &domain).unwrap(),
        },
        role: DiscreteRowRole::QuotientMemory {
            step: UnaryOp::Floor,
            root_index: 0,
        },
        pre_mode: DiscreteEventPreMode::FollowCurrent,
        clock_owner: None,
        observation_refresh: false,
        integrator_history_effect: IntegratorHistoryEffect::Restart,
    }];
    problem.events.root_conditions =
        ScalarProgramBlock::with_program_spans(roots, vec![span; 2]).unwrap();
    problem.events.root_relation_memory_targets = vec![None; 2];
    problem.events.root_zero_domains = vec![RootZeroDomain::NonPositive; 2];
    problem.events.root_relation_refresh_roles = vec![RootRelationRefreshRole::Frozen; 2];
    problem
        .validate()
        .expect("constructor-backed compact numeric memory");
    problem
}

#[test]
fn structured_quotient_certificate_replays_both_domain_directions_and_wire() {
    for descending in [false, true] {
        let problem = fixture(descending);
        assert_eq!(problem.discrete.structured_rhs.nodes.len(), 1);
        assert!(problem.initialization.update_rhs().is_empty());
        assert!(crate::solve_has_initialization(&problem));
        let wire = serde_json::to_vec(&problem).unwrap();
        let replay: SolveProblem = serde_json::from_slice(&wire).unwrap();
        assert_eq!(serde_json::to_vec(&replay).unwrap(), wire);
    }
}

#[test]
fn structured_quotient_certificate_rejects_wrong_point_order_and_ratio() {
    let mut problem = fixture(false);
    let roots = problem.events.root_conditions.programs();
    let reordered = vec![roots[1].clone(), roots[0].clone()];
    let spans = problem.events.root_conditions.program_spans().to_vec();
    problem.events.root_conditions =
        ScalarProgramBlock::with_program_spans(reordered, spans).unwrap();
    assert!(
        problem
            .validate()
            .unwrap_err()
            .to_string()
            .contains("exact live ratios")
    );
    let mut problem = fixture(false);
    let ComputeNode::Map { const_strides, .. } = &mut problem.discrete.structured_rhs.nodes[0]
    else {
        unreachable!()
    };
    const_strides.push(const_strides[0].clone());
    assert!(
        problem
            .validate()
            .unwrap_err()
            .to_string()
            .contains("exact live ratios")
    );
}

#[test]
fn structured_quotient_certificate_rejects_forged_initialization_or_reserved_storage() {
    let mut problem = fixture(false);
    let span = problem.events.root_conditions.program_span(0).unwrap();
    problem.initialization = InitializationSolveSystem::construct(InitializationSystemInput {
        update_rhs: ScalarProgramBlock::with_program_spans(
            vec![vec![
                LinearOp::Const {
                    dst: 0,
                    value: 17.0,
                },
                LinearOp::StoreOutput { src: 0 },
            ]],
            vec![span],
        )
        .unwrap(),
        update_targets: vec![scalar_slot_p(1)],
        ..Default::default()
    })
    .unwrap();
    assert!(
        problem
            .validate()
            .unwrap_err()
            .to_string()
            .contains("exclusive private")
    );
    let mut problem = fixture(false);
    problem
        .solve_layout
        .pre_param_bindings
        .push(PreParamBinding {
            dest_p_index: 1,
            source: PreParamSource::P { index: 0 },
            clock_schedule: None,
        });
    assert!(problem.validate().is_err());
}

#[test]
fn structured_quotient_certificate_rejects_incomplete_roots_and_observation_refresh() {
    let mut problem = fixture(false);
    problem.discrete.structured_updates[0].role = DiscreteRowRole::QuotientMemory {
        step: UnaryOp::Floor,
        root_index: 1,
    };
    assert!(
        problem
            .validate()
            .unwrap_err()
            .to_string()
            .contains("root family is incomplete")
    );
    let mut problem = fixture(false);
    problem.discrete.structured_updates[0].observation_refresh = true;
    assert!(
        problem
            .validate()
            .unwrap_err()
            .to_string()
            .contains("exclusive private")
    );
}

#[test]
fn structured_quotient_certificate_rejects_foreign_root_event_policy() {
    let mut problem = fixture(false);
    problem.events.root_zero_domains[1] = RootZeroDomain::Positive;
    assert!(problem.validate().is_err());
    let mut problem = fixture(false);
    problem.events.root_relation_refresh_roles.clear();
    assert!(problem.validate().is_err());
}

#[test]
fn structured_quotient_current_wire_rejects_overlap_between_complete_maps() {
    let mut problem = fixture(false);
    problem.layout = VarLayout::from_parts(Default::default(), 0, 4);
    problem.solve_layout.parameter_count = 4;
    problem.solve_layout.compiled_parameter_len = 4;
    let mut node = problem.discrete.structured_rhs.nodes[0].clone();
    let ComputeNode::Map {
        domain, output_map, ..
    } = &mut node
    else {
        unreachable!()
    };
    *output_map = TensorOutputMap::dense_contiguous(2, domain).unwrap();
    problem.discrete.structured_rhs.nodes.push(node);
    let mut update = problem.discrete.structured_updates[0].clone();
    update.node_index = 1;
    update.target.base = scalar_slot_p(2);
    update.role = DiscreteRowRole::QuotientMemory {
        step: UnaryOp::Floor,
        root_index: 2,
    };
    problem.discrete.structured_updates.push(update);
    let roots = problem.events.root_conditions.programs().to_vec();
    let spans = problem.events.root_conditions.program_spans().to_vec();
    problem.events.root_conditions = ScalarProgramBlock::with_program_spans(
        roots.iter().chain(&roots).cloned().collect(),
        spans.iter().chain(&spans).copied().collect(),
    )
    .unwrap();
    problem.events.root_relation_memory_targets = vec![None; 4];
    problem.events.root_zero_domains = vec![RootZeroDomain::NonPositive; 4];
    problem.events.root_relation_refresh_roles = vec![RootRelationRefreshRole::Frozen; 4];
    problem
        .validate()
        .expect("two complete disjoint numeric Map owners");
    let wire = serde_json::to_vec(&problem).unwrap();
    let replay: SolveProblem = serde_json::from_slice(&wire).unwrap();
    replay.validate().unwrap();
    problem.discrete.structured_updates[1].target.base = scalar_slot_p(0);
    assert!(matches!(
        problem.validate(),
        Err(SolveProblemShapeContractError::DuplicateIndex {
            context: "discrete.structured_updates.target",
            ..
        })
    ));
    let tampered = serde_json::to_vec(&problem).unwrap();
    assert!(serde_json::from_slice::<SolveProblem>(&tampered).is_err());
}
