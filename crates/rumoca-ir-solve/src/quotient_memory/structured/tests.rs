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
    let mut problem = SolveProblem {
        layout: VarLayout::from_parts(Default::default(), 0, 2),
        ..Default::default()
    };
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
fn structured_quotient_memory_excludes_declared_storage_but_allows_adjacency() {
    let mut problem = fixture(false);
    problem.layout = VarLayout::from_parts(Default::default(), 0, 3);
    problem.solve_layout.parameter_count = 3;
    problem.solve_layout.compiled_parameter_len = 3;
    problem.solve_layout.variable_declarations = vec![crate::SolveVariableDeclaration::new(
        crate::SolveVariableStorageRole::Parameter,
        crate::SolveVariableValueKind::Real,
    )];
    problem.solve_layout.variable_storage_runs = vec![crate::SolveVariableStorageRun {
        base: scalar_slot_p(2),
        scalar_count: 1,
        role: crate::SolveVariableStorageRole::Parameter,
        value_kind: crate::SolveVariableValueKind::Real,
    }];
    problem
        .validate()
        .expect("adjacent declared and private storage");
    let wire = serde_json::to_vec(&problem).unwrap();
    let replay: SolveProblem = serde_json::from_slice(&wire).unwrap();
    assert_eq!(serde_json::to_vec(&replay).unwrap(), wire);

    for index in [0, 1] {
        let mut overlapping = problem.clone();
        overlapping.solve_layout.variable_storage_runs[0].base = scalar_slot_p(index);
        assert!(
            overlapping
                .validate()
                .unwrap_err()
                .to_string()
                .contains("exclusive private")
        );
        let wire = serde_json::to_vec(&overlapping).unwrap();
        assert!(serde_json::from_slice::<SolveProblem>(&wire).is_err());
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

fn activated_root_program(condition: &[LinearOp], point: f64) -> Vec<LinearOp> {
    let prefix = [
        LinearOp::Const {
            dst: 0,
            value: point,
        },
        LinearOp::LoadTime { dst: 1 },
        LinearOp::Binary {
            dst: 2,
            op: BinaryOp::Add,
            lhs: 1,
            rhs: 0,
        },
    ];
    let root = FunctionConditionalProgram::checked(
        0,
        vec![1],
        [(condition.to_vec(), ratio_indicator(&prefix, 2).unwrap())],
        vec![
            LinearOp::Const { dst: 0, value: 1.0 },
            LinearOp::StoreOutput { src: 0 },
        ],
    )
    .unwrap();
    vec![
        LinearOp::FunctionConditional {
            dst_start: 0,
            capture_start: 0,
            program: std::sync::Arc::new(root),
        },
        LinearOp::StoreOutput { src: 0 },
    ]
}

fn activated_map_fixture() -> SolveProblem {
    let mut problem = fixture(false);
    let condition = vec![
        LinearOp::LoadTime { dst: 0 },
        LinearOp::Const { dst: 1, value: 0.5 },
        LinearOp::Compare {
            dst: 2,
            op: crate::CompareOp::Lt,
            lhs: 0,
            rhs: 1,
        },
        LinearOp::StoreOutput { src: 2 },
    ];
    let selected_prefix = vec![
        LinearOp::LoadFunctionConditionalCapture { dst: 0, index: 0 },
        LinearOp::LoadTime { dst: 1 },
        LinearOp::Binary {
            dst: 2,
            op: BinaryOp::Add,
            lhs: 1,
            rhs: 0,
        },
    ];
    let mut selected = selected_prefix.clone();
    selected.extend([
        LinearOp::Unary {
            dst: 3,
            op: UnaryOp::Floor,
            arg: 2,
        },
        LinearOp::StoreOutput { src: 3 },
    ]);
    let fallback = vec![
        LinearOp::LoadFunctionConditionalCapture { dst: 0, index: 1 },
        LinearOp::StoreOutput { src: 0 },
    ];
    let program =
        FunctionConditionalProgram::checked(2, vec![1], [(condition.clone(), selected)], fallback)
            .unwrap();
    let ComputeNode::Map {
        base_ops,
        load_strides,
        ..
    } = &mut problem.discrete.structured_rhs.nodes[0]
    else {
        unreachable!()
    };
    *base_ops = vec![
        LinearOp::Const { dst: 0, value: 1.0 },
        LinearOp::LoadP { dst: 1, index: 0 },
        LinearOp::FunctionConditional {
            dst_start: 2,
            capture_start: 0,
            program: std::sync::Arc::new(program),
        },
        LinearOp::StoreOutput { src: 2 },
    ];
    *load_strides = vec![AffineStencilLoadStride {
        op_position: 1,
        terms: vec![AffineStencilIndexStrideTerm {
            dimension: 0,
            stride: 1,
        }],
    }];
    let roots = [1.0, 2.0]
        .into_iter()
        .map(|point| activated_root_program(&condition, point))
        .collect();
    problem.events.root_conditions = ScalarProgramBlock::with_program_spans(
        roots,
        vec![problem.events.root_conditions.program_span(0).unwrap(); 2],
    )
    .unwrap();
    problem
}

#[test]
fn activated_map_requires_its_complete_private_hold_geometry() {
    let problem = activated_map_fixture();
    problem
        .validate()
        .expect("one compact conditional Map with exact ordinal holds");
    for mutation in 0..4 {
        let mut wrong = problem.clone();
        let ComputeNode::Map {
            load_strides,
            base_ops,
            ..
        } = &mut wrong.discrete.structured_rhs.nodes[0]
        else {
            unreachable!()
        };
        match mutation {
            0 => load_strides.clear(),
            1 => load_strides[0].op_position = 0,
            2 => load_strides[0].terms[0].stride = 0,
            _ => base_ops.insert(2, LinearOp::LoadP { dst: 1, index: 0 }),
        }
        assert!(
            wrong.validate().is_err(),
            "hold geometry mutation {mutation} must not pass"
        );
    }
    let value = serde_json::to_value(&problem).unwrap();
    serde_json::from_value::<SolveProblem>(value).expect("current compact owner wire roundtrip");

    // A guard that consumes the held capture must not make the marker proof
    // vacuous. The shared checker retains both branches even when marking the
    // load makes this guard constant; a second unshifted same-base load still
    // cannot supply the fallback's reaching value.
    let mut guarded_by_hold = problem.clone();
    let ComputeNode::Map { base_ops, .. } = &mut guarded_by_hold.discrete.structured_rhs.nodes[0]
    else {
        unreachable!()
    };
    let LinearOp::FunctionConditional { program, .. } = &mut base_ops[2] else {
        unreachable!()
    };
    let original = program.as_ref();
    let condition = vec![
        LinearOp::LoadFunctionConditionalCapture { dst: 0, index: 1 },
        LinearOp::Const { dst: 1, value: 0.0 },
        LinearOp::Compare {
            dst: 2,
            op: crate::CompareOp::Eq,
            lhs: 0,
            rhs: 1,
        },
        LinearOp::StoreOutput { src: 2 },
    ];
    *program = std::sync::Arc::new(
        FunctionConditionalProgram::checked(
            original.capture_count,
            vec![1],
            [(condition, original.arms[0].result.clone())],
            original.fallback.clone(),
        )
        .unwrap(),
    );
    super::super::activation::indicator(0, UnaryOp::Floor, base_ops, 0, Some(1))
        .expect("guard and fallback consume the exact shifted load");
    base_ops.insert(2, LinearOp::LoadP { dst: 1, index: 0 });
    assert!(
        super::super::activation::indicator(0, UnaryOp::Floor, base_ops, 0, Some(1),).is_err(),
        "a constant guard after marking cannot hide the foreign reaching load"
    );
}
