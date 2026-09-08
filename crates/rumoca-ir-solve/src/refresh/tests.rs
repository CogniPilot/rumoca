use super::*;
use crate::SolveLayout;

fn issue_refresh_fixture(
    source: ComputeBlock,
    mut solve_layout: SolveLayout,
    canonical_targets: &[usize],
    inputs: ContinuousRefreshPlanInputs,
) -> Result<ContinuousRefreshOwners, ContinuousRefreshConstructionError> {
    let y_extent = canonical_targets
        .iter()
        .copied()
        .max()
        .map_or(0, |index| index + 1);
    assert!(
        solve_layout.state_scalar_count <= y_extent,
        "fixture state prefix must fit its explicit canonical Y domain"
    );
    solve_layout.algebraic_scalar_count = y_extent - solve_layout.state_scalar_count;
    solve_layout.solver_maps.names = (0..y_extent).map(|index| format!("y{index}")).collect();
    let implicit_row_targets = canonical_targets
        .iter()
        .copied()
        .map(|index| Some(crate::scalar_slot_y(index)))
        .collect::<Vec<_>>();
    let projection = AlgebraicProjectionPlan {
        blocks: canonical_targets
            .iter()
            .copied()
            .enumerate()
            .map(|(equation, target)| crate::AlgebraicProjectionBlock {
                rows: vec![equation],
                y_indices: vec![target],
                tearing: None,
            })
            .collect(),
    };
    crate::ContinuousSolveSystem::construct(
        &solve_layout,
        crate::ContinuousSolveSystemInputs::new(
            source,
            implicit_row_targets,
            projection,
            ComputeBlock::default(),
            (ComputeBlock::default(), AlgebraicProjectionPlan::default()),
            ComputeBlock::default(),
            inputs,
        ),
    )
    .map(|system| system.refresh_owners().clone())
}

fn algebraic_refresh_inputs(plan: RefreshPlan) -> ContinuousRefreshPlanInputs {
    ContinuousRefreshPlanInputs::new(
        plan,
        RefreshPlan::empty(),
        RefreshPlan::empty(),
        RefreshPlan::empty(),
        Vec::new(),
    )
}

fn source_block() -> ComputeBlock {
    let span = rumoca_core::Span::from_offsets(
        rumoca_core::SourceId::from_source_name("refresh_owner_test.mo"),
        0,
        1,
    );
    ComputeBlock::from_scalar_program_block(
        crate::ScalarProgramBlock::with_source_span(
            vec![vec![
                crate::LinearOp::LoadY { dst: 0, index: 0 },
                crate::LinearOp::Const { dst: 1, value: 1.0 },
                crate::LinearOp::Binary {
                    dst: 2,
                    op: crate::BinaryOp::Sub,
                    lhs: 0,
                    rhs: 1,
                },
                crate::LinearOp::StoreOutput { src: 2 },
            ]],
            rumoca_core::ProvenanceSpan::new(span, "refresh owner test").unwrap(),
        )
        .unwrap(),
    )
}

fn one_algebraic_layout() -> SolveLayout {
    SolveLayout {
        algebraic_scalar_count: 1,
        solver_maps: crate::SolverNameIndexMaps {
            names: vec!["a".to_string()],
            name_to_idx: indexmap::IndexMap::from([("a".to_string(), 0)]),
            base_to_indices: indexmap::IndexMap::from([("a".to_string(), vec![0])]),
        },
        variable_storage_runs: vec![crate::SolveVariableStorageRun {
            base: crate::SolveStorageCoordinate::Y(0),
            scalar_count: 1,
            role: crate::SolveVariableStorageRole::Algebraic,
            value_kind: crate::SolveVariableValueKind::Real,
        }],
        variable_declarations: vec![crate::SolveVariableDeclaration::new(
            crate::SolveVariableStorageRole::Algebraic,
            crate::SolveVariableValueKind::Real,
        )],
        ..SolveLayout::default()
    }
}

fn issued_parameter_static_fixture(
    program: Vec<LinearOp>,
    solve_layout: &SolveLayout,
    caller_claims_static: bool,
) -> ContinuousRefreshOwners {
    let source = ComputeBlock::from_scalar_program_block(
        ScalarProgramBlock::with_source_span(
            vec![program],
            rumoca_core::ProvenanceSpan::new(
                rumoca_core::Span::from_offsets(
                    rumoca_core::SourceId::from_source_name("refresh_static_owner_test.mo"),
                    0,
                    1,
                ),
                "refresh static owner test",
            )
            .unwrap(),
        )
        .unwrap(),
    );
    let row = AlgebraicRefreshRow::checked(AlgebraicRefreshRowDraft {
        owner_id: RefreshRowOwnerId::checked(0).unwrap(),
        source: RefreshScalarProgramSource::checked(0, 0).unwrap(),
        equation_index: 0,
        output_offset: 0,
        target_index: 10,
        assignment_target: None,
        assignment_shape: None,
        direct_assignment_certified: false,
        exact_assignment_certified: false,
    })
    .unwrap();
    let selected = selection(1, [0]);
    let empty = RefreshRowSelection::empty();
    let (static_rows, dynamic_rows) = if caller_claims_static {
        (selected.clone(), empty)
    } else {
        (empty, selected.clone())
    };
    let plan = RefreshPlan {
        rows: vec![row],
        causal_seed_rows: selected,
        static_causal_seed_rows: static_rows.clone(),
        dynamic_causal_seed_rows: dynamic_rows.clone(),
        value_stages: vec![RefreshStage::CausalSeedSweep {
            static_rows,
            dynamic_rows,
        }],
        ..RefreshPlan::empty()
    };
    issue_refresh_fixture(
        source,
        solve_layout.clone(),
        &[10],
        ContinuousRefreshPlanInputs::new(
            plan,
            RefreshPlan::empty(),
            RefreshPlan::empty(),
            RefreshPlan::empty(),
            Vec::new(),
        ),
    )
    .unwrap()
}

#[test]
fn issuing_owner_rederives_static_partition_instead_of_trusting_the_draft() {
    let layout = SolveLayout {
        state_scalar_count: 2,
        parameter_count: 8,
        ..SolveLayout::default()
    };
    let static_owner = issued_parameter_static_fixture(
        vec![
            LinearOp::LoadP { dst: 0, index: 7 },
            LinearOp::StoreOutput { src: 0 },
        ],
        &layout,
        false,
    );
    assert_eq!(static_owner.algebraic().static_causal_rows().len(), 1);
    assert_eq!(static_owner.static_parameter_indices(), [7]);
    assert!(matches!(
        static_owner.algebraic().value_stages(),
        [IssuedRefreshStage::CausalSeedSweep { static_rows, dynamic_rows, .. }]
            if static_rows.indices() == [0] && dynamic_rows.is_empty()
    ));

    let dynamic_owner = issued_parameter_static_fixture(
        vec![
            LinearOp::LoadP { dst: 0, index: 8 },
            LinearOp::StoreOutput { src: 0 },
        ],
        &layout,
        true,
    );
    assert_eq!(dynamic_owner.algebraic().dynamic_causal_rows().len(), 1);
    assert!(dynamic_owner.algebraic().static_causal_rows().is_empty());
    assert!(dynamic_owner.static_parameter_indices().is_empty());
}

#[test]
fn static_partition_and_parameter_snapshot_select_the_exact_multi_output_row() {
    let layout = SolveLayout {
        state_scalar_count: 2,
        parameter_count: 8,
        ..SolveLayout::default()
    };
    let owners = issued_parameter_static_fixture(
        vec![
            LinearOp::LoadP { dst: 0, index: 7 },
            LinearOp::StoreOutput { src: 0 },
            LinearOp::LoadP { dst: 1, index: 8 },
            LinearOp::StoreOutput { src: 1 },
        ],
        &layout,
        false,
    );

    assert_eq!(owners.algebraic().static_causal_rows().len(), 1);
    assert!(owners.algebraic().dynamic_causal_rows().is_empty());
    assert_eq!(owners.static_parameter_indices(), [7]);
}

#[test]
fn containing_system_wire_replay_rederives_static_partition_with_layout() {
    let source = source_block();
    let layout = one_algebraic_layout();
    let projection = crate::AlgebraicProjectionBlock {
        rows: vec![0],
        y_indices: vec![0],
        tearing: None,
    };
    let plan = RefreshPlan {
        simultaneous_plan: AlgebraicProjectionPlan {
            blocks: vec![projection.clone()],
        },
        simultaneous_block_indices: vec![0],
        rows: vec![row(0)],
        causal_seed_rows: selection(1, [0]),
        dynamic_causal_seed_rows: selection(1, [0]),
        ..RefreshPlan::empty()
    };
    let system = crate::ContinuousSolveSystem::construct(
        &layout,
        crate::ContinuousSolveSystemInputs::new(
            source,
            vec![Some(crate::scalar_slot_y(0))],
            AlgebraicProjectionPlan {
                blocks: vec![projection],
            },
            ComputeBlock::default(),
            (ComputeBlock::default(), AlgebraicProjectionPlan::default()),
            ComputeBlock::default(),
            ContinuousRefreshPlanInputs::new(
                plan,
                RefreshPlan::empty(),
                RefreshPlan::empty(),
                RefreshPlan::empty(),
                Vec::new(),
            ),
        ),
    )
    .unwrap();
    let problem = crate::SolveProblem::construct(
        crate::VarLayout::from_parts(Default::default(), 1, 0),
        layout,
        system,
        crate::InitializationSolveSystem::empty(),
        crate::DiscreteSolveSystem::default(),
        crate::SolveEventPartition::default(),
        crate::SolveClockPartition::default(),
    )
    .unwrap();
    let mut wire = serde_json::to_value(problem).unwrap();
    wire["continuous"]["refresh_owners"]["algebraic"]["static_causal_seed_rows"] =
        serde_json::json!([]);
    wire["continuous"]["refresh_owners"]["algebraic"]["dynamic_causal_seed_rows"] =
        serde_json::json!([0]);
    let replay = serde_json::from_value::<crate::SolveProblem>(wire).unwrap();
    assert_eq!(
        replay
            .continuous()
            .refresh_owners()
            .algebraic()
            .static_causal_seed_rows()
            .indices(),
        [0]
    );
}

#[test]
fn containing_system_wire_refuses_a_coordinated_target_at_the_y_extent() {
    let source = source_block();
    let layout = one_algebraic_layout();
    let projection = crate::AlgebraicProjectionBlock {
        rows: vec![0],
        y_indices: vec![0],
        tearing: None,
    };
    let plan = RefreshPlan {
        simultaneous_plan: AlgebraicProjectionPlan {
            blocks: vec![projection.clone()],
        },
        simultaneous_block_indices: vec![0],
        rows: vec![row(0)],
        causal_seed_rows: selection(1, [0]),
        dynamic_causal_seed_rows: selection(1, [0]),
        ..RefreshPlan::empty()
    };
    let system = crate::ContinuousSolveSystem::construct(
        &layout,
        crate::ContinuousSolveSystemInputs::new(
            source,
            vec![Some(crate::scalar_slot_y(0))],
            AlgebraicProjectionPlan {
                blocks: vec![projection],
            },
            ComputeBlock::default(),
            (ComputeBlock::default(), AlgebraicProjectionPlan::default()),
            ComputeBlock::default(),
            algebraic_refresh_inputs(plan),
        ),
    )
    .unwrap();
    let problem = crate::SolveProblem::construct(
        crate::VarLayout::from_parts(Default::default(), 1, 0),
        layout,
        system,
        crate::InitializationSolveSystem::empty(),
        crate::DiscreteSolveSystem::default(),
        crate::SolveEventPartition::default(),
        crate::SolveClockPartition::default(),
    )
    .unwrap();

    let mut wire = serde_json::to_value(problem).unwrap();
    wire["continuous"]["implicit_row_targets"][0]["Y"]["index"] = serde_json::json!(1);
    wire["continuous"]["algebraic_projection_plan"]["blocks"][0]["y_indices"][0] =
        serde_json::json!(1);
    let algebraic = &mut wire["continuous"]["refresh_owners"]["algebraic"];
    algebraic["simultaneous_plan"]["blocks"][0]["y_indices"][0] = serde_json::json!(1);
    algebraic["rows"][0]["target_index"] = serde_json::json!(1);
    algebraic["rows"][0]["assignment_target"] = serde_json::json!(1);
    algebraic["rows"][0]["assignment_shape"]["Direct"]["target_y_index"] = serde_json::json!(1);

    let error = serde_json::from_value::<crate::SolveProblem>(wire)
        .expect_err("a coordinated target at the exact Y extent must refuse during replay");
    assert!(
        error
            .to_string()
            .contains("canonical target 1 leaves algebraic solver Y range 0..1"),
        "unexpected error: {error}"
    );
}

#[test]
fn static_partition_proof_covers_tensor_and_nested_dependencies() {
    let layout = SolveLayout {
        state_scalar_count: 2,
        parameter_count: 8,
        ..SolveLayout::default()
    };
    let tensor_tail = issued_parameter_static_fixture(
        vec![
            LinearOp::TensorLoad {
                dst_start: 0,
                input: crate::TensorInputKind::P,
                input_start: 7,
                count: 2,
                seed_start: None,
                lanes: 1,
            },
            LinearOp::StoreOutput { src: 1 },
        ],
        &layout,
        true,
    );
    assert_eq!(tensor_tail.algebraic().dynamic_causal_rows().len(), 1);

    let conditional = crate::FunctionConditionalProgram::checked(
        0,
        [1],
        [(
            vec![
                LinearOp::LoadTime { dst: 0 },
                LinearOp::StoreOutput { src: 0 },
            ],
            vec![
                LinearOp::Const { dst: 0, value: 1.0 },
                LinearOp::StoreOutput { src: 0 },
            ],
        )],
        vec![
            LinearOp::Const { dst: 0, value: 0.0 },
            LinearOp::StoreOutput { src: 0 },
        ],
    )
    .unwrap();
    let nested_time = issued_parameter_static_fixture(
        vec![
            LinearOp::FunctionConditional {
                dst_start: 0,
                capture_start: 0,
                program: std::sync::Arc::new(conditional),
            },
            LinearOp::StoreOutput { src: 0 },
        ],
        &layout,
        true,
    );
    assert_eq!(nested_time.algebraic().dynamic_causal_rows().len(), 1);
}

#[test]
fn homotopy_endpoint_is_the_only_static_parameter_beyond_the_prefix() {
    let dynamic_layout = SolveLayout {
        state_scalar_count: 2,
        parameter_count: 8,
        ..SolveLayout::default()
    };
    let static_layout = SolveLayout {
        initial_homotopy_parameter_index: Some(8),
        ..dynamic_layout.clone()
    };
    let program = || {
        vec![
            LinearOp::LoadP { dst: 0, index: 8 },
            LinearOp::StoreOutput { src: 0 },
        ]
    };
    assert_eq!(
        issued_parameter_static_fixture(program(), &dynamic_layout, true)
            .algebraic()
            .dynamic_causal_rows()
            .len(),
        1
    );
    assert_eq!(
        issued_parameter_static_fixture(program(), &static_layout, false)
            .algebraic()
            .static_causal_rows()
            .len(),
        1
    );
    assert_eq!(
        issued_parameter_static_fixture(program(), &static_layout, false)
            .static_parameter_indices(),
        [8]
    );
}

#[test]
fn canonical_assignment_shape_preserves_cross_algebraic_dependencies() {
    let program = vec![
        crate::LinearOp::LoadY { dst: 0, index: 1 },
        crate::LinearOp::LoadY { dst: 1, index: 0 },
        crate::LinearOp::Binary {
            dst: 2,
            op: crate::BinaryOp::Sub,
            lhs: 0,
            rhs: 1,
        },
        crate::LinearOp::StoreOutput { src: 2 },
    ];
    let shape = canonical_assignment_shape_for_output(&program, 0, 1)
        .expect("the row is locally isolable before system-level validation");
    assert!(matches!(shape, TargetAssignmentShape::Direct { .. }));
}

#[test]
fn guarded_fold_dependency_includes_its_activation() {
    let domain = rumoca_core::StructuredIndexDomain {
        binders: vec![rumoca_core::StructuredIndexBinder {
            id: rumoca_core::StructuredIndexBinderId::new(0),
            display_name: "i".to_string(),
            lower: 1,
            upper: 1,
            step: 1,
        }],
    };
    let fold = crate::FunctionFoldProgram::checked(
        domain,
        1,
        0,
        vec![
            crate::LinearOp::LoadFoldCarried { dst: 0, index: 0 },
            crate::LinearOp::StoreOutput { src: 0 },
        ],
    )
    .expect("fixture fold is valid");
    let program = vec![
        crate::LinearOp::LoadY { dst: 0, index: 7 },
        crate::LinearOp::Const { dst: 1, value: 1.0 },
        crate::LinearOp::GuardedFunctionFold {
            dst_start: 2,
            initial_start: 1,
            capture_start: 1,
            activation: 0,
            program: std::sync::Arc::new(fold),
        },
    ];

    assert!(ScalarProgramYDependency::new(&program).depends_on(2, 7));
}

#[test]
fn matrix_multiply_dependency_tracks_the_selected_output_element() {
    let program = vec![
        crate::LinearOp::TensorLoad {
            dst_start: 0,
            input: crate::TensorInputKind::Y,
            input_start: 10,
            count: 6,
            seed_start: None,
            lanes: 1,
        },
        crate::LinearOp::TensorLoad {
            dst_start: 6,
            input: crate::TensorInputKind::Y,
            input_start: 20,
            count: 6,
            seed_start: None,
            lanes: 1,
        },
        crate::LinearOp::MatrixMultiply {
            dst_start: 12,
            lhs_start: 0,
            rhs_start: 6,
            rows: 2,
            inner: 3,
            columns: 2,
            lanes: 1,
        },
    ];
    let dependencies = ScalarProgramYDependency::new(&program);

    for input in [10, 11, 12, 20, 22, 24] {
        assert!(dependencies.depends_on(12, input));
    }
    for input in [13, 14, 15, 21, 23, 25] {
        assert!(!dependencies.depends_on(12, input));
    }
}

fn row(target: usize) -> AlgebraicRefreshRow {
    AlgebraicRefreshRow {
        owner_id: RefreshRowOwnerId::checked(target).unwrap(),
        source: RefreshScalarProgramSource::checked(0, target).unwrap(),
        equation_index: target,
        output_offset: 0,
        target_index: target,
        assignment_target: Some(target),
        assignment_shape: Some(TargetAssignmentShape::Direct {
            target_y_index: target,
            expr_reg: 1,
            target_scale: 1.0,
            expr_eval_len: 2,
        }),
        direct_assignment_certified: true,
        exact_assignment_certified: true,
    }
}

#[test]
fn algebraic_refresh_row_wire_requires_optional_assignment_keys() {
    let value = serde_json::to_value(row(0)).expect("serialize current refresh row");
    let _: AlgebraicRefreshRowWire =
        serde_json::from_value(value.clone()).expect("explicit optional keys replay");
    for field in ["assignment_target", "assignment_shape"] {
        let mut omitted = value.clone();
        assert!(
            omitted
                .as_object_mut()
                .expect("refresh row wire is an object")
                .remove(field)
                .is_some(),
            "fixture must carry `{field}`"
        );
        let error = match serde_json::from_value::<AlgebraicRefreshRowWire>(omitted) {
            Ok(_) => panic!("omitted current optional assignment key `{field}` must fail"),
            Err(error) => error,
        };
        assert!(
            error
                .to_string()
                .contains(&format!("missing field `{field}`")),
            "unexpected omission error for {field}: {error}"
        );
    }
}

fn selection(row_count: usize, indices: impl IntoIterator<Item = usize>) -> RefreshRowSelection {
    RefreshRowSelection::checked(row_count, indices).unwrap()
}

fn two_output_source(second_reads_first_target: bool) -> ComputeBlock {
    let span = rumoca_core::Span::from_offsets(
        rumoca_core::SourceId::from_source_name("refresh_group_test.mo"),
        0,
        1,
    );
    let second_value = if second_reads_first_target {
        crate::LinearOp::LoadY { dst: 4, index: 0 }
    } else {
        crate::LinearOp::Const { dst: 4, value: 2.0 }
    };
    ComputeBlock::from_scalar_program_block(
        crate::ScalarProgramBlock::with_source_span(
            vec![vec![
                crate::LinearOp::LoadY { dst: 0, index: 0 },
                crate::LinearOp::Const { dst: 1, value: 1.0 },
                crate::LinearOp::Binary {
                    dst: 2,
                    op: crate::BinaryOp::Sub,
                    lhs: 0,
                    rhs: 1,
                },
                crate::LinearOp::StoreOutput { src: 2 },
                crate::LinearOp::LoadY { dst: 3, index: 1 },
                second_value,
                crate::LinearOp::Binary {
                    dst: 5,
                    op: crate::BinaryOp::Sub,
                    lhs: 3,
                    rhs: 4,
                },
                crate::LinearOp::StoreOutput { src: 5 },
            ]],
            rumoca_core::ProvenanceSpan::new(span, "refresh group test").unwrap(),
        )
        .unwrap(),
    )
}

fn cyclic_two_output_source() -> ComputeBlock {
    let span = rumoca_core::Span::from_offsets(
        rumoca_core::SourceId::from_source_name("refresh_static_cycle_test.mo"),
        0,
        1,
    );
    ComputeBlock::from_scalar_program_block(
        crate::ScalarProgramBlock::with_source_span(
            vec![vec![
                crate::LinearOp::LoadY { dst: 0, index: 0 },
                crate::LinearOp::LoadY { dst: 1, index: 1 },
                crate::LinearOp::Binary {
                    dst: 2,
                    op: crate::BinaryOp::Sub,
                    lhs: 0,
                    rhs: 1,
                },
                crate::LinearOp::StoreOutput { src: 2 },
                crate::LinearOp::LoadY { dst: 3, index: 1 },
                crate::LinearOp::LoadY { dst: 4, index: 0 },
                crate::LinearOp::Binary {
                    dst: 5,
                    op: crate::BinaryOp::Sub,
                    lhs: 3,
                    rhs: 4,
                },
                crate::LinearOp::StoreOutput { src: 5 },
            ]],
            rumoca_core::ProvenanceSpan::new(span, "refresh static cycle test").unwrap(),
        )
        .unwrap(),
    )
}

fn grouped_row(target: usize, output_offset: usize, expr_reg: u32) -> AlgebraicRefreshRow {
    AlgebraicRefreshRow {
        owner_id: RefreshRowOwnerId::checked(target).unwrap(),
        source: RefreshScalarProgramSource::checked(0, 0).unwrap(),
        equation_index: output_offset,
        output_offset,
        target_index: target,
        assignment_target: Some(target),
        assignment_shape: Some(TargetAssignmentShape::Direct {
            target_y_index: target,
            expr_reg,
            target_scale: 1.0,
            expr_eval_len: if output_offset == 0 { 2 } else { 6 },
        }),
        direct_assignment_certified: true,
        exact_assignment_certified: true,
    }
}

fn two_row_dynamic_plan() -> RefreshPlan {
    let rows = vec![grouped_row(0, 0, 1), grouped_row(1, 1, 4)];
    RefreshPlan {
        causal_seed_rows: selection(rows.len(), 0..rows.len()),
        dynamic_causal_seed_rows: selection(rows.len(), 0..rows.len()),
        rows,
        ..RefreshPlan::empty()
    }
}

fn two_row_causal_plan(reverse: bool) -> RefreshPlan {
    let mut rows = vec![grouped_row(0, 0, 1), grouped_row(1, 1, 4)];
    if reverse {
        rows.reverse();
    }
    RefreshPlan {
        simultaneous_plan: AlgebraicProjectionPlan {
            blocks: vec![
                crate::AlgebraicProjectionBlock {
                    rows: vec![0],
                    y_indices: vec![0],
                    tearing: None,
                },
                crate::AlgebraicProjectionBlock {
                    rows: vec![1],
                    y_indices: vec![1],
                    tearing: None,
                },
            ],
        },
        simultaneous_block_indices: vec![0, 1],
        causal_seed_rows: selection(2, [0, 1]),
        dynamic_causal_seed_rows: selection(2, [0, 1]),
        rows,
        ..RefreshPlan::empty()
    }
}

#[test]
fn causal_certificate_uses_the_issuer_derived_static_prefix() {
    let source = two_output_source(true);
    let issue = |plan| {
        issue_refresh_fixture(
            source.clone(),
            SolveLayout::default(),
            &[0, 1],
            algebraic_refresh_inputs(plan),
        )
        .unwrap()
    };
    assert!(
        issue(two_row_causal_plan(false))
            .algebraic()
            .causal_solution_certified()
    );
    let reordered = issue(two_row_causal_plan(true));
    assert_eq!(
        reordered.algebraic().static_causal_seed_rows().indices(),
        [1],
        "the independent row becomes the static prefix without bootstrapping its dependent"
    );
    assert!(reordered.algebraic().causal_solution_certified());
}

#[test]
fn cyclic_causal_pair_cannot_bootstrap_parameter_static_ownership() {
    let owners = issue_refresh_fixture(
        cyclic_two_output_source(),
        SolveLayout::default(),
        &[0, 1],
        algebraic_refresh_inputs(two_row_causal_plan(false)),
    )
    .expect("a cyclic residual pair remains a dynamic projection owner");

    assert!(owners.algebraic().static_causal_rows().is_empty());
    assert_eq!(owners.algebraic().dynamic_causal_rows().len(), 2);
    assert!(owners.static_parameter_indices().is_empty());
    assert!(!owners.algebraic().causal_solution_certified());
}

#[test]
fn wire_cannot_claim_a_causal_solution_certificate() {
    let owners = issue_refresh_fixture(
        two_output_source(true),
        SolveLayout::default(),
        &[0, 1],
        algebraic_refresh_inputs(two_row_causal_plan(false)),
    )
    .unwrap();
    let mut wire = serde_json::to_value(owners).unwrap();
    wire["algebraic"]["causal_solution_certified"] = serde_json::json!(true);
    let error = serde_json::from_value::<ContinuousRefreshOwnersWire>(wire)
        .expect_err("wire cannot supply a construction-derived certificate");
    assert!(error.to_string().contains("causal_solution_certified"));
}

fn mixed_projection_exact_plan(projection_first: bool) -> RefreshPlan {
    let rows = vec![grouped_row(0, 0, 1), grouped_row(1, 1, 4)];
    let projection_block = crate::AlgebraicProjectionBlock {
        rows: vec![0],
        y_indices: vec![0],
        tearing: None,
    };
    let seed = RefreshStage::CausalSeedSweep {
        static_rows: RefreshRowSelection::empty(),
        dynamic_rows: selection(rows.len(), [0, 1]),
    };
    let projection = RefreshStage::ProjectionBlock {
        block_index: 0,
        plan: AlgebraicProjectionPlan {
            blocks: vec![projection_block.clone()],
        },
        seed_rows: selection(rows.len(), [0]),
    };
    let exact = RefreshStage::ExactAssignments {
        static_rows: RefreshRowSelection::empty(),
        dynamic_rows: selection(rows.len(), [1]),
    };
    let value_stages = if projection_first {
        vec![seed, projection, exact]
    } else {
        vec![seed, exact, projection]
    };
    RefreshPlan {
        simultaneous_plan: AlgebraicProjectionPlan {
            blocks: vec![
                projection_block,
                crate::AlgebraicProjectionBlock {
                    rows: vec![1],
                    y_indices: vec![1],
                    tearing: None,
                },
            ],
        },
        simultaneous_block_indices: vec![0, 1],
        rows,
        value_stages,
        ..RefreshPlan::empty()
    }
}

#[test]
fn projection_requirement_distinguishes_exact_and_residual_stages() {
    let exact = RefreshPlan {
        value_stages: vec![RefreshStage::ExactAssignments {
            static_rows: RefreshRowSelection::empty(),
            dynamic_rows: RefreshRowSelection::empty(),
        }],
        ..RefreshPlan::empty()
    };
    let exact = RefreshSequenceAllocator::for_owner(0)
        .unwrap()
        .issue_plan(exact)
        .unwrap();
    assert!(!exact.requires_projection());

    let residual = RefreshPlan {
        value_stages: vec![RefreshStage::ProjectionBlock {
            block_index: 0,
            plan: AlgebraicProjectionPlan::default(),
            seed_rows: RefreshRowSelection::empty(),
        }],
        ..RefreshPlan::empty()
    };
    let residual = RefreshSequenceAllocator::for_owner(0)
        .unwrap()
        .issue_plan(residual)
        .unwrap();
    assert!(residual.requires_projection());
}

#[test]
fn exact_assignment_stage_rejects_a_non_exact_row() {
    let mut non_exact = row(0);
    non_exact.assignment_shape = None;
    non_exact.direct_assignment_certified = false;
    non_exact.exact_assignment_certified = false;
    let plan = RefreshPlan {
        rows: vec![non_exact],
        value_stages: vec![RefreshStage::ExactAssignments {
            static_rows: RefreshRowSelection::empty(),
            dynamic_rows: selection(1, [0]),
        }],
        ..RefreshPlan::empty()
    };

    let error = issue_refresh_fixture(
        source_block(),
        SolveLayout::default(),
        &[0],
        algebraic_refresh_inputs(plan),
    )
    .expect_err("an exact stage cannot select a non-exact row");
    assert!(error.to_string().contains("selects a non-exact row"));
}

#[test]
fn exact_assignment_completeness_requires_full_blt_coverage() {
    let incomplete = RefreshPlan {
        rows: vec![row(0)],
        causal_seed_rows: selection(1, [0]),
        dynamic_causal_seed_rows: selection(1, [0]),
        ..RefreshPlan::empty()
    };
    let incomplete = issue_refresh_fixture(
        source_block(),
        SolveLayout::default(),
        &[0],
        algebraic_refresh_inputs(incomplete),
    )
    .expect("the incomplete schedule remains a valid residual plan");
    assert!(!incomplete.algebraic_exact_assignment_stages_cover());

    let complete = RefreshPlan {
        simultaneous_plan: AlgebraicProjectionPlan {
            blocks: vec![crate::AlgebraicProjectionBlock {
                rows: vec![0],
                y_indices: vec![0],
                tearing: None,
            }],
        },
        simultaneous_block_indices: vec![0],
        rows: vec![row(0)],
        causal_seed_rows: selection(1, [0]),
        dynamic_causal_seed_rows: selection(1, [0]),
        value_stages: vec![RefreshStage::ExactAssignments {
            static_rows: RefreshRowSelection::empty(),
            dynamic_rows: selection(1, [0]),
        }],
        ..RefreshPlan::empty()
    };
    let complete = issue_refresh_fixture(
        source_block(),
        SolveLayout::default(),
        &[0],
        algebraic_refresh_inputs(complete),
    )
    .expect("the exact singleton schedule is valid");
    assert!(complete.algebraic_exact_assignment_stages_cover());
}

#[test]
fn projection_output_settles_a_following_exact_assignment_dependency() {
    let owners = issue_refresh_fixture(
        two_output_source(true),
        SolveLayout::default(),
        &[0, 1],
        algebraic_refresh_inputs(mixed_projection_exact_plan(true)),
    )
    .expect("the preceding projection settles the exact assignment dependency");

    assert!(owners.algebraic().requires_projection());
    assert!(!owners.algebraic_exact_assignment_stages_cover());
}

#[test]
fn exact_assignment_rejects_a_dependency_settled_by_a_later_projection() {
    let error = issue_refresh_fixture(
        two_output_source(true),
        SolveLayout::default(),
        &[0, 1],
        algebraic_refresh_inputs(mixed_projection_exact_plan(false)),
    )
    .expect_err("a causal seed does not settle a dependency before its projection block");

    assert!(error.to_string().contains("non-causal"));
}

#[test]
fn refresh_owner_rejects_a_stage_row_without_a_canonical_owner() {
    let plan = RefreshPlan {
        rows: vec![row(0)],
        value_stages: vec![RefreshStage::ExactAssignments {
            static_rows: RefreshRowSelection::empty(),
            dynamic_rows: RefreshRowSelection(vec![1].into_boxed_slice()),
        }],
        ..RefreshPlan::empty()
    };
    let error = issue_refresh_fixture(
        source_block(),
        SolveLayout::default(),
        &[0],
        algebraic_refresh_inputs(plan),
    )
    .expect_err("a stage cannot forge another row owner");
    assert!(error.to_string().contains("unowned canonical identity"));
}

#[test]
fn refresh_owner_rejects_an_isolator_for_another_target() {
    let mut forged = row(0);
    forged.assignment_shape = Some(TargetAssignmentShape::Direct {
        target_y_index: 1,
        expr_reg: 0,
        target_scale: 1.0,
        expr_eval_len: 1,
    });
    let plan = RefreshPlan {
        rows: vec![forged],
        ..RefreshPlan::empty()
    };
    let error = issue_refresh_fixture(
        source_block(),
        SolveLayout::default(),
        &[0],
        algebraic_refresh_inputs(plan),
    )
    .expect_err("an isolator cannot change target ownership");
    assert!(error.to_string().contains("another target"));
}

#[test]
fn refresh_owner_rejects_a_caller_authored_target_for_another_canonical_row() {
    let mut forged = row(0);
    forged.target_index = 1;
    forged.assignment_target = Some(1);
    forged.assignment_shape = Some(TargetAssignmentShape::Direct {
        target_y_index: 1,
        expr_reg: 1,
        target_scale: 1.0,
        expr_eval_len: 2,
    });
    let plan = RefreshPlan {
        rows: vec![forged],
        ..RefreshPlan::empty()
    };
    let error = issue_refresh_fixture(
        source_block(),
        SolveLayout::default(),
        &[0],
        algebraic_refresh_inputs(plan),
    )
    .expect_err("the issuer derives its target from the exact implicit row");
    assert!(
        error
            .to_string()
            .contains("target 1 disagrees with canonical implicit target 0"),
        "unexpected construction error: {error}"
    );
}

#[test]
fn refresh_owner_rejects_a_source_outside_the_canonical_compute_block() {
    let mut forged = row(0);
    forged.source = RefreshScalarProgramSource::checked(0, 1).unwrap();
    let plan = RefreshPlan {
        rows: vec![forged],
        ..RefreshPlan::empty()
    };
    let error = issue_refresh_fixture(
        source_block(),
        SolveLayout::default(),
        &[0],
        algebraic_refresh_inputs(plan),
    )
    .expect_err("a refresh row cannot forge a source program");
    assert!(
        error
            .to_string()
            .contains("missing canonical scalar-program output"),
        "unexpected construction error: {error}"
    );
}

#[test]
fn refresh_owner_rejects_a_source_output_for_another_equation() {
    let mut forged = row(0);
    forged.equation_index = 1;
    let plan = RefreshPlan {
        rows: vec![forged],
        ..RefreshPlan::empty()
    };
    let error = issue_refresh_fixture(
        source_block(),
        SolveLayout::default(),
        &[0],
        algebraic_refresh_inputs(plan),
    )
    .expect_err("a refresh row cannot claim another equation");
    assert!(error.to_string().contains("does not own equation 1"));
}

#[test]
fn exact_assignment_program_is_constructed_once_and_not_serialized() {
    let refresh_row = row(0);
    let plan = RefreshPlan {
        rows: vec![refresh_row],
        causal_seed_rows: selection(1, [0]),
        dynamic_causal_seed_rows: selection(1, [0]),
        ..RefreshPlan::empty()
    };
    let owners = issue_refresh_fixture(
        source_block(),
        SolveLayout::default(),
        &[0],
        algebraic_refresh_inputs(plan),
    )
    .unwrap();
    let sequence = owners.algebraic().static_causal_sequence();
    let schedule = owners
        .exact_assignment_schedule(sequence)
        .expect("exact row order must own one frozen assignment schedule");
    let [program_id] = schedule.program_ids() else {
        panic!("one exact row should issue one program identity");
    };
    let program = owners
        .exact_assignment_program(*program_id)
        .expect("exact row must own its constructed assignment program");
    assert_eq!(program.target_indices(), &[0]);
    assert_eq!(
        program.row_owners(),
        &[RefreshRowOwnerId::checked(0).unwrap()]
    );
    assert_eq!(program.assignment_shapes().len(), 1);
    let final_program = program.final_program();
    assert_eq!(final_program.programs().len(), 1);
    assert_eq!(final_program.output_count(), 1);

    let wire = serde_json::to_value(&owners).unwrap();
    assert!(wire.get("exact_assignment_programs").is_none());
    assert!(wire.get("exact_assignment_schedules").is_none());
    assert_eq!(
        wire["algebraic"]["static_causal_seed_rows"],
        serde_json::json!([0])
    );
    assert!(
        wire["algebraic"]["static_causal_seed_rows"][0]
            .as_object()
            .is_none()
    );
}

#[test]
fn independent_outputs_from_one_source_keep_one_checked_program_owner() {
    let owners = issue_refresh_fixture(
        two_output_source(false),
        SolveLayout::default(),
        &[0, 1],
        algebraic_refresh_inputs(two_row_dynamic_plan()),
    )
    .unwrap();
    let sequence = owners.algebraic().static_causal_sequence();
    let schedule = owners.exact_assignment_schedule(sequence).unwrap();
    let [program_id] = schedule.program_ids() else {
        panic!("independent outputs from one issued source must remain one program");
    };
    let program = owners.exact_assignment_program(*program_id).unwrap();
    assert_eq!(program.target_indices(), &[0, 1]);
    assert_eq!(program.assignment_shapes().len(), 2);
    assert_eq!(program.final_program().output_count(), 2);
}

#[test]
fn dependent_outputs_from_one_source_preserve_sequential_program_owners() {
    let owners = issue_refresh_fixture(
        two_output_source(true),
        SolveLayout::default(),
        &[0, 1],
        algebraic_refresh_inputs(two_row_dynamic_plan()),
    )
    .unwrap();
    let sequence = owners.algebraic().static_causal_sequence();
    let schedule = owners.exact_assignment_schedule(sequence).unwrap();
    assert_eq!(schedule.program_ids().len(), 2);
    let targets = schedule
        .program_ids()
        .iter()
        .map(|id| {
            owners
                .exact_assignment_program(*id)
                .unwrap()
                .target_indices()
        })
        .collect::<Vec<_>>();
    assert_eq!(targets, vec![&[0][..], &[1][..]]);
}

#[test]
fn wire_reconstructs_the_root_remainder_relation() {
    let owners = issue_refresh_fixture(
        ComputeBlock::default(),
        SolveLayout::default(),
        &[],
        ContinuousRefreshPlanInputs::empty(),
    )
    .unwrap();
    let wire = serde_json::to_value(&owners).unwrap();
    assert!(wire.get("root_after_derivative").is_none());
    let wire: ContinuousRefreshOwnersWire = serde_json::from_value(wire).unwrap();
    let replayed = issue_refresh_fixture(
        ComputeBlock::default(),
        SolveLayout::default(),
        &[],
        wire.into_inputs(),
    )
    .unwrap();
    assert!(
        replayed
            .root_after_derivative()
            .remainder()
            .rows()
            .is_empty()
    );
}

#[test]
fn wire_reconstructs_clock_remainders_after_event_coverage() {
    let first_block = crate::AlgebraicProjectionBlock {
        rows: vec![0],
        y_indices: vec![0],
        tearing: None,
    };
    let second_block = crate::AlgebraicProjectionBlock {
        rows: vec![1],
        y_indices: vec![1],
        tearing: None,
    };
    let event = RefreshPlan {
        simultaneous_plan: AlgebraicProjectionPlan {
            blocks: vec![first_block.clone()],
        },
        simultaneous_block_indices: vec![0],
        rows: vec![grouped_row(0, 0, 1)],
        causal_seed_rows: selection(1, [0]),
        dynamic_causal_seed_rows: selection(1, [0]),
        ..RefreshPlan::empty()
    };
    let clock = RefreshPlan {
        simultaneous_plan: AlgebraicProjectionPlan {
            blocks: vec![first_block, second_block],
        },
        simultaneous_block_indices: vec![0, 1],
        rows: vec![grouped_row(0, 0, 1), grouped_row(1, 1, 4)],
        causal_seed_rows: selection(2, [0, 1]),
        dynamic_causal_seed_rows: selection(2, [0, 1]),
        ..RefreshPlan::empty()
    };
    let owners = issue_refresh_fixture(
        two_output_source(false),
        SolveLayout::default(),
        &[0, 1],
        ContinuousRefreshPlanInputs::new(
            RefreshPlan::empty(),
            RefreshPlan::empty(),
            RefreshPlan::empty(),
            event,
            vec![clock],
        ),
    )
    .unwrap();

    let [relation] = owners.clock_events_after_event() else {
        panic!("one clock owner must issue one event-settled relation")
    };
    assert_eq!(
        relation
            .remainder()
            .causal_rows()
            .iter()
            .map(AlgebraicRefreshRow::target_index)
            .collect::<Vec<_>>(),
        [1]
    );
    assert_eq!(
        relation
            .remainder()
            .static_causal_rows()
            .iter()
            .map(AlgebraicRefreshRow::target_index)
            .collect::<Vec<_>>(),
        [1]
    );
    assert_ne!(
        relation.remainder().static_causal_sequence(),
        owners.clock_events()[0].static_causal_sequence()
    );

    let source = two_output_source(false);
    let wire = serde_json::to_value(&owners).unwrap();
    assert!(wire.get("clock_events_after_event").is_none());
    let wire: ContinuousRefreshOwnersWire = serde_json::from_value(wire).unwrap();
    let replayed =
        issue_refresh_fixture(source, SolveLayout::default(), &[0, 1], wire.into_inputs()).unwrap();
    let [relation] = replayed.clock_events_after_event() else {
        panic!("wire replay must reissue one event-settled clock relation")
    };
    assert_eq!(
        relation
            .remainder()
            .static_causal_rows()
            .iter()
            .map(AlgebraicRefreshRow::target_index)
            .collect::<Vec<_>>(),
        [1]
    );
}

#[test]
fn construction_issues_distinct_sequence_identities() {
    let owners = issue_refresh_fixture(
        ComputeBlock::default(),
        SolveLayout::default(),
        &[],
        ContinuousRefreshPlanInputs::empty(),
    )
    .unwrap();
    assert_ne!(
        owners.algebraic().static_causal_sequence(),
        owners.algebraic().dynamic_causal_sequence()
    );
    assert_ne!(
        owners.root().static_causal_sequence(),
        owners
            .root_after_derivative()
            .remainder()
            .static_causal_sequence()
    );
}

#[test]
fn sequence_allocator_issues_exact_first_and_last_identities_and_refuses_overflow() {
    let mut first = RefreshSequenceAllocator::for_owner(0).unwrap();
    assert_eq!(first.allocate().unwrap().0.get(), 1);

    let mut last = RefreshSequenceAllocator {
        owner: u32::MAX,
        next: u32::MAX - 1,
    };
    assert_eq!(last.allocate().unwrap().0.get(), u64::MAX);
    let error = last
        .allocate()
        .expect_err("an ordinal beyond the exact u32 boundary must refuse");
    assert!(error.to_string().contains("sequence count exceeds u32"));
    assert_eq!(
        last.next,
        u32::MAX,
        "overflow must not mutate the allocator"
    );
}

#[cfg(target_pointer_width = "64")]
#[test]
fn sequence_allocator_refuses_the_first_owner_beyond_u32() {
    let last = RefreshSequenceAllocator::for_owner(u32::MAX as usize)
        .expect("the last u32 owner is representable");
    assert_eq!(last.owner, u32::MAX);

    let error = RefreshSequenceAllocator::for_owner(u32::MAX as usize + 1)
        .expect_err("an owner beyond the exact u32 boundary must refuse");
    assert!(error.to_string().contains("owner count exceeds u32"));
}

#[test]
fn derivative_settle_relation_keeps_only_uncovered_root_stages() {
    let first_block = crate::AlgebraicProjectionBlock {
        rows: vec![0],
        y_indices: vec![0],
        tearing: None,
    };
    let second_block = crate::AlgebraicProjectionBlock {
        rows: vec![1],
        y_indices: vec![1],
        tearing: None,
    };
    let root = RefreshPlan {
        simultaneous_plan: AlgebraicProjectionPlan {
            blocks: vec![first_block.clone(), second_block],
        },
        simultaneous_block_indices: vec![10, 11],
        rows: vec![grouped_row(0, 0, 1), grouped_row(1, 1, 4)],
        causal_seed_rows: selection(2, [0, 1]),
        dynamic_causal_seed_rows: selection(2, [0, 1]),
        value_stages: vec![exact_stage(2, 0), exact_stage(2, 1)],
        ..RefreshPlan::empty()
    };
    let derivative = RefreshPlan {
        simultaneous_plan: AlgebraicProjectionPlan {
            blocks: vec![first_block],
        },
        simultaneous_block_indices: vec![10],
        rows: vec![grouped_row(0, 0, 1)],
        causal_seed_rows: selection(1, [0]),
        dynamic_causal_seed_rows: selection(1, [0]),
        value_stages: vec![exact_stage(1, 0)],
        ..RefreshPlan::empty()
    };

    let owners = issue_refresh_fixture(
        two_output_source(false),
        SolveLayout::default(),
        &[0, 1],
        ContinuousRefreshPlanInputs::new(
            RefreshPlan::empty(),
            derivative,
            root,
            RefreshPlan::empty(),
            Vec::new(),
        ),
    )
    .unwrap();
    let relation = owners.root_after_derivative();
    assert!(matches!(
        relation.remainder().value_stages(),
        [IssuedRefreshStage::ExactAssignments { static_rows, .. }]
            if static_rows.indices() == [1]
    ));
    assert!(!relation.remainder().causal_solution_certified());
    assert_eq!(relation.remainder().causal_seed_rows().indices(), [1]);
    assert_eq!(
        relation.remainder().static_causal_seed_rows().indices(),
        [1]
    );
}

fn exact_stage(row_count: usize, index: usize) -> RefreshStage {
    RefreshStage::ExactAssignments {
        static_rows: RefreshRowSelection::empty(),
        dynamic_rows: selection(row_count, [index]),
    }
}
