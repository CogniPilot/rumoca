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
                crate::LinearOp::Const { dst: 0, value: 1.0 },
                crate::LinearOp::StoreOutput { src: 0 },
            ]],
            rumoca_core::ProvenanceSpan::new(span, "refresh owner test").unwrap(),
        )
        .unwrap(),
    )
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
            expr_reg: 0,
            target_scale: 1.0,
            expr_eval_len: 1,
        }),
        direct_assignment_certified: true,
        exact_assignment_certified: true,
    }
}

fn two_output_source(second_reads_first_target: bool) -> ComputeBlock {
    let span = rumoca_core::Span::from_offsets(
        rumoca_core::SourceId::from_source_name("refresh_group_test.mo"),
        0,
        1,
    );
    let second = if second_reads_first_target {
        crate::LinearOp::LoadY { dst: 1, index: 0 }
    } else {
        crate::LinearOp::Const { dst: 1, value: 2.0 }
    };
    ComputeBlock::from_scalar_program_block(
        crate::ScalarProgramBlock::with_source_span(
            vec![vec![
                crate::LinearOp::Const { dst: 0, value: 1.0 },
                crate::LinearOp::StoreOutput { src: 0 },
                second,
                crate::LinearOp::StoreOutput { src: 1 },
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
            expr_eval_len: if output_offset == 0 { 1 } else { 3 },
        }),
        direct_assignment_certified: true,
        exact_assignment_certified: true,
    }
}

fn two_row_dynamic_plan() -> RefreshPlan {
    let rows = vec![grouped_row(0, 0, 0), grouped_row(1, 1, 1)];
    RefreshPlan {
        rows: rows.clone(),
        dynamic_causal_seed_rows: rows,
        ..RefreshPlan::default()
    }
}

#[test]
fn refresh_owner_rejects_a_stage_row_without_a_canonical_owner() {
    let plan = RefreshPlan {
        rows: vec![row(0)],
        value_stages: vec![RefreshStage::ExactAssignments {
            static_sequence: Default::default(),
            dynamic_sequence: Default::default(),
            static_rows: Box::new([]),
            dynamic_rows: vec![row(1)].into_boxed_slice(),
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
        rows: vec![refresh_row.clone()],
        dynamic_causal_seed_rows: vec![refresh_row],
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
        seed_rows: Box::new([]),
    };
    let root = RefreshPlan {
        value_stages: vec![exact_stage(0), projection.clone(), exact_stage(3)],
        ..RefreshPlan::default()
    };
    let derivative = RefreshPlan {
        value_stages: vec![exact_stage(0), exact_stage(9), projection],
        ..RefreshPlan::default()
    };

    let relation = root.issue_value_remainder_after(&derivative);
    assert!(matches!(
        relation.remainder().value_stages.as_slice(),
        [RefreshStage::ExactAssignments { dynamic_rows, .. }]
            if dynamic_rows.len() == 1 && dynamic_rows[0].target_index == 3
    ));
}

fn exact_stage(index: usize) -> RefreshStage {
    RefreshStage::ExactAssignments {
        static_sequence: Default::default(),
        dynamic_sequence: Default::default(),
        static_rows: Box::new([]),
        dynamic_rows: vec![row(index)].into_boxed_slice(),
    }
}
