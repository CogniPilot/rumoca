use super::*;

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
            id: 0,
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
        dynamic_causal_seed_rows: selection(rows.len(), 0..rows.len()),
        rows,
        ..RefreshPlan::default()
    }
}

fn mixed_projection_exact_plan(projection_first: bool) -> RefreshPlan {
    let rows = vec![grouped_row(0, 0, 1), grouped_row(1, 1, 4)];
    let projection_block = crate::AlgebraicProjectionBlock {
        rows: vec![0],
        y_indices: vec![0],
    };
    let seed = RefreshStage::CausalSeedSweep {
        static_sequence: Default::default(),
        dynamic_sequence: Default::default(),
        static_rows: RefreshRowSelection::default(),
        dynamic_rows: selection(rows.len(), [0, 1]),
    };
    let projection = RefreshStage::ProjectionBlock {
        seed_sequence: Default::default(),
        block_index: 0,
        plan: AlgebraicProjectionPlan {
            blocks: vec![projection_block.clone()],
        },
        seed_rows: selection(rows.len(), [0]),
    };
    let exact = RefreshStage::ExactAssignments {
        static_sequence: Default::default(),
        dynamic_sequence: Default::default(),
        static_rows: RefreshRowSelection::default(),
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
                },
            ],
        },
        simultaneous_block_indices: vec![0, 1],
        rows,
        value_stages,
        ..RefreshPlan::default()
    }
}

#[test]
fn projection_requirement_distinguishes_exact_and_residual_stages() {
    let exact = RefreshPlan {
        value_stages: vec![RefreshStage::ExactAssignments {
            static_sequence: Default::default(),
            dynamic_sequence: Default::default(),
            static_rows: RefreshRowSelection::default(),
            dynamic_rows: RefreshRowSelection::default(),
        }],
        ..RefreshPlan::default()
    };
    assert!(!exact.requires_projection());

    let residual = RefreshPlan {
        value_stages: vec![RefreshStage::ProjectionBlock {
            seed_sequence: Default::default(),
            block_index: 0,
            plan: AlgebraicProjectionPlan::default(),
            seed_rows: RefreshRowSelection::default(),
        }],
        ..RefreshPlan::default()
    };
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
            static_sequence: Default::default(),
            dynamic_sequence: Default::default(),
            static_rows: RefreshRowSelection::default(),
            dynamic_rows: selection(1, [0]),
        }],
        ..RefreshPlan::default()
    };

    let error = ContinuousRefreshOwners::checked_for_source(
        &source_block(),
        plan,
        RefreshPlan::default(),
        RefreshPlan::default(),
        RefreshPlan::default(),
        Vec::new(),
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
        ..RefreshPlan::default()
    };
    let incomplete = ContinuousRefreshOwners::checked_for_source(
        &source_block(),
        incomplete,
        RefreshPlan::default(),
        RefreshPlan::default(),
        RefreshPlan::default(),
        Vec::new(),
    )
    .expect("the incomplete schedule remains a valid residual plan");
    assert!(!incomplete.algebraic_exact_assignment_stages_cover());

    let complete = RefreshPlan {
        simultaneous_plan: AlgebraicProjectionPlan {
            blocks: vec![crate::AlgebraicProjectionBlock {
                rows: vec![0],
                y_indices: vec![0],
            }],
        },
        simultaneous_block_indices: vec![0],
        rows: vec![row(0)],
        causal_seed_rows: selection(1, [0]),
        dynamic_causal_seed_rows: selection(1, [0]),
        value_stages: vec![RefreshStage::ExactAssignments {
            static_sequence: Default::default(),
            dynamic_sequence: Default::default(),
            static_rows: RefreshRowSelection::default(),
            dynamic_rows: selection(1, [0]),
        }],
        ..RefreshPlan::default()
    };
    let complete = ContinuousRefreshOwners::checked_for_source(
        &source_block(),
        complete,
        RefreshPlan::default(),
        RefreshPlan::default(),
        RefreshPlan::default(),
        Vec::new(),
    )
    .expect("the exact singleton schedule is valid");
    assert!(complete.algebraic_exact_assignment_stages_cover());
}

#[test]
fn projection_output_settles_a_following_exact_assignment_dependency() {
    let owners = ContinuousRefreshOwners::checked_for_source(
        &two_output_source(true),
        mixed_projection_exact_plan(true),
        RefreshPlan::default(),
        RefreshPlan::default(),
        RefreshPlan::default(),
        Vec::new(),
    )
    .expect("the preceding projection settles the exact assignment dependency");

    assert!(owners.algebraic().requires_projection());
    assert!(!owners.algebraic_exact_assignment_stages_cover());
}

#[test]
fn exact_assignment_rejects_a_dependency_settled_by_a_later_projection() {
    let error = ContinuousRefreshOwners::checked_for_source(
        &two_output_source(true),
        mixed_projection_exact_plan(false),
        RefreshPlan::default(),
        RefreshPlan::default(),
        RefreshPlan::default(),
        Vec::new(),
    )
    .expect_err("a causal seed does not settle a dependency before its projection block");

    assert!(error.to_string().contains("non-causal"));
}

#[test]
fn refresh_owner_rejects_a_stage_row_without_a_canonical_owner() {
    let plan = RefreshPlan {
        rows: vec![row(0)],
        value_stages: vec![RefreshStage::ExactAssignments {
            static_sequence: Default::default(),
            dynamic_sequence: Default::default(),
            static_rows: RefreshRowSelection::default(),
            dynamic_rows: RefreshRowSelection(vec![1].into_boxed_slice()),
        }],
        ..RefreshPlan::default()
    };
    let error = ContinuousRefreshOwners::checked(
        plan,
        RefreshPlan::default(),
        RefreshPlan::default(),
        RefreshPlan::default(),
        Vec::new(),
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
        ..RefreshPlan::default()
    };
    let error = ContinuousRefreshOwners::checked(
        plan,
        RefreshPlan::default(),
        RefreshPlan::default(),
        RefreshPlan::default(),
        Vec::new(),
    )
    .expect_err("an isolator cannot change target ownership");
    assert!(error.to_string().contains("another target"));
}

#[test]
fn refresh_owner_rejects_a_source_outside_the_canonical_compute_block() {
    let mut forged = row(0);
    forged.source = RefreshScalarProgramSource::checked(0, 1).unwrap();
    let plan = RefreshPlan {
        rows: vec![forged],
        ..RefreshPlan::default()
    };
    let error = ContinuousRefreshOwners::checked_for_source(
        &source_block(),
        plan,
        RefreshPlan::default(),
        RefreshPlan::default(),
        RefreshPlan::default(),
        Vec::new(),
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
        ..RefreshPlan::default()
    };
    let error = ContinuousRefreshOwners::checked_for_source(
        &source_block(),
        plan,
        RefreshPlan::default(),
        RefreshPlan::default(),
        RefreshPlan::default(),
        Vec::new(),
    )
    .expect_err("a refresh row cannot claim another equation");
    assert!(error.to_string().contains("does not own equation 1"));
}

#[test]
fn exact_assignment_program_is_constructed_once_and_not_serialized() {
    let refresh_row = row(0);
    let plan = RefreshPlan {
        rows: vec![refresh_row],
        dynamic_causal_seed_rows: selection(1, [0]),
        ..RefreshPlan::default()
    };
    let owners = ContinuousRefreshOwners::checked_for_source(
        &source_block(),
        plan,
        RefreshPlan::default(),
        RefreshPlan::default(),
        RefreshPlan::default(),
        Vec::new(),
    )
    .unwrap();
    let sequence = owners.algebraic().dynamic_causal_sequence;
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
    let final_program = program.final_scalar_program(&source_block()).unwrap();
    assert_eq!(final_program.programs().len(), 1);
    assert_eq!(final_program.output_count(), 1);

    let wire = serde_json::to_value(&owners).unwrap();
    assert!(wire.get("exact_assignment_programs").is_none());
    assert!(wire.get("exact_assignment_schedules").is_none());
    assert_eq!(
        wire["algebraic"]["dynamic_causal_seed_rows"],
        serde_json::json!([0])
    );
    assert!(
        wire["algebraic"]["dynamic_causal_seed_rows"][0]
            .as_object()
            .is_none()
    );
}

#[test]
fn independent_outputs_from_one_source_keep_one_checked_program_owner() {
    let owners = ContinuousRefreshOwners::checked_for_source(
        &two_output_source(false),
        two_row_dynamic_plan(),
        RefreshPlan::default(),
        RefreshPlan::default(),
        RefreshPlan::default(),
        Vec::new(),
    )
    .unwrap();
    let sequence = owners.algebraic().dynamic_causal_sequence;
    let schedule = owners.exact_assignment_schedule(sequence).unwrap();
    let [program_id] = schedule.program_ids() else {
        panic!("independent outputs from one issued source must remain one program");
    };
    let program = owners.exact_assignment_program(*program_id).unwrap();
    assert_eq!(program.target_indices(), &[0, 1]);
    assert_eq!(program.assignment_shapes().len(), 2);
    assert_eq!(
        program
            .final_scalar_program(&two_output_source(false))
            .unwrap()
            .output_count(),
        2
    );
}

#[test]
fn dependent_outputs_from_one_source_preserve_sequential_program_owners() {
    let owners = ContinuousRefreshOwners::checked_for_source(
        &two_output_source(true),
        two_row_dynamic_plan(),
        RefreshPlan::default(),
        RefreshPlan::default(),
        RefreshPlan::default(),
        Vec::new(),
    )
    .unwrap();
    let sequence = owners.algebraic().dynamic_causal_sequence;
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
    let owners = ContinuousRefreshOwners::checked(
        RefreshPlan::default(),
        RefreshPlan::default(),
        RefreshPlan::default(),
        RefreshPlan::default(),
        Vec::new(),
    )
    .unwrap();
    let wire = serde_json::to_value(&owners).unwrap();
    assert!(wire.get("root_after_derivative").is_none());
    let replayed: ContinuousRefreshOwners = serde_json::from_value(wire).unwrap();
    assert!(replayed.is_issued());
    assert!(replayed.root_after_derivative().is_some());
}

#[test]
fn wire_reconstructs_clock_remainders_after_event_coverage() {
    let event = RefreshPlan {
        rows: vec![row(0)],
        causal_seed_rows: selection(1, [0]),
        dynamic_causal_seed_rows: selection(1, [0]),
        causal_solution_certified: true,
        ..RefreshPlan::default()
    };
    let clock = RefreshPlan {
        rows: vec![row(0), row(1)],
        causal_seed_rows: selection(2, [0, 1]),
        dynamic_causal_seed_rows: selection(2, [0, 1]),
        causal_solution_certified: true,
        ..RefreshPlan::default()
    };
    let owners = ContinuousRefreshOwners::checked(
        RefreshPlan::default(),
        RefreshPlan::default(),
        RefreshPlan::default(),
        event,
        vec![clock],
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
            .dynamic_causal_rows()
            .iter()
            .map(AlgebraicRefreshRow::target_index)
            .collect::<Vec<_>>(),
        [1]
    );
    assert_ne!(
        relation.remainder().dynamic_causal_sequence,
        owners.clock_events()[0].dynamic_causal_sequence
    );

    let wire = serde_json::to_value(&owners).unwrap();
    assert!(wire.get("clock_events_after_event").is_none());
    let replayed: ContinuousRefreshOwners = serde_json::from_value(wire).unwrap();
    let [relation] = replayed.clock_events_after_event() else {
        panic!("wire replay must reissue one event-settled clock relation")
    };
    assert_eq!(
        relation
            .remainder()
            .dynamic_causal_rows()
            .iter()
            .map(AlgebraicRefreshRow::target_index)
            .collect::<Vec<_>>(),
        [1]
    );
}

#[test]
fn construction_issues_distinct_sequence_identities() {
    let owners = ContinuousRefreshOwners::checked(
        RefreshPlan::default(),
        RefreshPlan::default(),
        RefreshPlan::default(),
        RefreshPlan::default(),
        Vec::new(),
    )
    .unwrap();
    assert_ne!(
        owners.algebraic().static_causal_sequence,
        owners.algebraic().dynamic_causal_sequence
    );
    assert_ne!(
        owners.root().static_causal_sequence,
        owners
            .root_after_derivative()
            .unwrap()
            .remainder()
            .static_causal_sequence
    );
}

#[test]
fn derivative_settle_relation_keeps_only_uncovered_root_stages() {
    let projection = RefreshStage::ProjectionBlock {
        seed_sequence: Default::default(),
        block_index: 11,
        plan: AlgebraicProjectionPlan {
            blocks: vec![crate::AlgebraicProjectionBlock {
                rows: vec![1, 2],
                y_indices: vec![1, 2],
            }],
        },
        seed_rows: RefreshRowSelection::default(),
    };
    let root = RefreshPlan {
        rows: vec![row(0), row(3)],
        causal_seed_rows: selection(2, [0, 1]),
        dynamic_causal_seed_rows: selection(2, [0, 1]),
        value_stages: vec![exact_stage(2, 0), projection.clone(), exact_stage(2, 1)],
        causal_solution_certified: true,
        ..RefreshPlan::default()
    };
    let derivative = RefreshPlan {
        rows: vec![row(0), row(9)],
        causal_seed_rows: selection(2, [0, 1]),
        dynamic_causal_seed_rows: selection(2, [0, 1]),
        value_stages: vec![exact_stage(2, 0), exact_stage(2, 1), projection],
        causal_solution_certified: true,
        ..RefreshPlan::default()
    };

    let relation = root.issue_value_remainder_after(&derivative);
    assert!(matches!(
        relation.remainder().value_stages.as_slice(),
        [RefreshStage::ExactAssignments { dynamic_rows, .. }]
            if dynamic_rows.indices() == [1]
    ));
    assert!(relation.remainder().causal_solution_certified);
    assert_eq!(relation.remainder().causal_seed_rows.indices(), [1]);
    assert_eq!(relation.remainder().dynamic_causal_seed_rows.indices(), [1]);
}

fn exact_stage(row_count: usize, index: usize) -> RefreshStage {
    RefreshStage::ExactAssignments {
        static_sequence: Default::default(),
        dynamic_sequence: Default::default(),
        static_rows: RefreshRowSelection::default(),
        dynamic_rows: selection(row_count, [index]),
    }
}
