use super::*;

fn issued(source: &ComputeBlock, plan: RefreshPlan) -> ContinuousRefreshOwners {
    ContinuousRefreshOwners::checked_for_source(
        source,
        plan,
        RefreshPlan::default(),
        RefreshPlan::default(),
        RefreshPlan::default(),
        Vec::new(),
    )
    .unwrap()
}

fn assignment(owners: &ContinuousRefreshOwners) -> &ExactRefreshAssignmentProgram {
    let schedule = owners
        .exact_assignment_schedule(owners.algebraic().dynamic_causal_sequence)
        .unwrap();
    owners
        .exact_assignment_program(schedule.program_ids()[0])
        .unwrap()
}

#[test]
fn binding_borrows_the_original_complete_program_and_tuple() {
    let source = two_output_source(false);
    let owners = issued(&source, two_row_dynamic_plan());
    let owner = assignment(&owners);
    let bound = owner.bind_source(&source).unwrap();
    let ComputeNode::ScalarPrograms(original) = &source.nodes[0] else {
        panic!("fixture owns one scalar program");
    };
    assert!(std::ptr::eq(bound.assignment(), owner));
    assert!(std::ptr::eq(
        bound.source_operations(),
        original.programs()[0].as_slice()
    ));
    assert_eq!(bound.provenance(), original.program_span(0).unwrap());
    assert_eq!(bound.assignment().target_indices(), &[0, 1]);
    assert!(bound.assignment().assignment_leaf_dependencies(0).is_some());
    assert!(bound.assignment().assignment_leaf_dependencies(1).is_some());
    let materialized = bound.final_scalar_program().unwrap();
    assert_eq!(materialized.output_count(), 2);
    assert_eq!(materialized.program_span(0), Some(bound.provenance()));
}

#[test]
fn equal_independent_and_wire_replayed_sources_cannot_acquire_the_original_tuple() {
    let source = two_output_source(false);
    let owners = issued(&source, two_row_dynamic_plan());
    let owner = assignment(&owners);
    let shared = source.clone();
    assert!(owner.bind_source(&shared).is_ok());
    let independent = two_output_source(false);
    let replayed: ComputeBlock =
        serde_json::from_value(serde_json::to_value(&source).unwrap()).unwrap();
    for foreign in [&independent, &replayed] {
        assert_eq!(
            serde_json::to_value(foreign).unwrap(),
            serde_json::to_value(&source).unwrap()
        );
        let error = owner.bind_source(foreign).unwrap_err();
        assert!(error.to_string().contains("no longer has its issued owner"));
    }
    let missing = ComputeBlock::default();
    assert!(owner.bind_source(&missing).is_err());
}

#[test]
fn binding_does_not_merge_dependent_tuple_commits() {
    let source = two_output_source(true);
    let owners = issued(&source, two_row_dynamic_plan());
    let schedule = owners
        .exact_assignment_schedule(owners.algebraic().dynamic_causal_sequence)
        .unwrap();
    assert_eq!(schedule.program_ids().len(), 2);
    let targets = schedule
        .program_ids()
        .iter()
        .map(|&id| {
            let bound = owners
                .exact_assignment_program(id)
                .unwrap()
                .bind_source(&source)
                .unwrap();
            let program = bound.final_scalar_program().unwrap();
            assert_eq!(program.output_count(), 1);
            bound.assignment().target_indices().to_vec()
        })
        .collect::<Vec<_>>();
    assert_eq!(targets, [vec![0], vec![1]]);
}

/// One program of two independent residual outputs, `y[0] - 2 * y[2]` and
/// `y[1] + y[2]`. Before its second value the program also loads both
/// targets, forms the first residual and its negation, and solves a linear
/// system nothing reads.
fn source_with_unread_prefix_operations() -> Vec<LinearOp> {
    vec![
        LinearOp::LoadY { dst: 0, index: 0 },
        LinearOp::LoadY { dst: 1, index: 2 },
        LinearOp::Const { dst: 2, value: 2.0 },
        LinearOp::Binary {
            dst: 3,
            op: crate::BinaryOp::Mul,
            lhs: 1,
            rhs: 2,
        },
        LinearOp::Binary {
            dst: 4,
            op: crate::BinaryOp::Sub,
            lhs: 0,
            rhs: 3,
        },
        LinearOp::StoreOutput { src: 4 },
        LinearOp::LoadY { dst: 5, index: 1 },
        LinearOp::Unary {
            dst: 6,
            op: crate::UnaryOp::Neg,
            arg: 4,
        },
        LinearOp::LinearSolveComponent {
            dst: 7,
            matrix_start: 2,
            rhs_start: 1,
            n: 1,
            component: 0,
        },
        LinearOp::Unary {
            dst: 8,
            op: crate::UnaryOp::Neg,
            arg: 1,
        },
        LinearOp::Binary {
            dst: 9,
            op: crate::BinaryOp::Sub,
            lhs: 5,
            rhs: 8,
        },
        LinearOp::StoreOutput { src: 9 },
    ]
}

fn direct_row(target: usize, expr_reg: u32, expr_eval_len: usize) -> AlgebraicRefreshRow {
    AlgebraicRefreshRow {
        owner_id: RefreshRowOwnerId::checked(target).unwrap(),
        source: RefreshScalarProgramSource::checked(0, 0).unwrap(),
        equation_index: target,
        output_offset: target,
        target_index: target,
        assignment_target: Some(target),
        assignment_shape: Some(TargetAssignmentShape::Direct {
            target_y_index: target,
            expr_reg,
            target_scale: 1.0,
            expr_eval_len,
        }),
        direct_assignment_certified: true,
        exact_assignment_certified: true,
    }
}

#[test]
fn materialization_drops_unread_pure_prefix_operations_and_keeps_the_rest_in_order() {
    let operations = source_with_unread_prefix_operations();
    let span = rumoca_core::Span::from_offsets(
        rumoca_core::SourceId::from_source_name("refresh_unread_prefix_test.mo"),
        0,
        1,
    );
    let source = ComputeBlock::from_scalar_program_block(
        crate::ScalarProgramBlock::with_source_span(
            vec![operations.clone()],
            rumoca_core::ProvenanceSpan::new(span, "refresh unread prefix test").unwrap(),
        )
        .unwrap(),
    );
    let rows = vec![direct_row(0, 3, 4), direct_row(1, 8, 10)];
    let owners = issued(
        &source,
        RefreshPlan {
            dynamic_causal_seed_rows: selection(rows.len(), 0..rows.len()),
            rows,
            ..RefreshPlan::default()
        },
    );
    let owner = assignment(&owners);
    assert_eq!(owner.target_indices(), &[0, 1]);
    let materialized = owner.final_scalar_program(&source).unwrap();
    let [program] = materialized.programs() else {
        panic!("one exact assignment materializes one program");
    };

    // Kept, in source order: the load both values read, the first value's
    // operands, the unread solve (it can fail), and the second value. Dropped:
    // both target loads and the residual chain, which the isolators replace.
    let mut expected = [1, 2, 3, 8, 9]
        .map(|position| operations[position].clone())
        .to_vec();
    expected.extend([
        LinearOp::StoreOutput { src: 3 },
        LinearOp::StoreOutput { src: 8 },
    ]);
    assert_eq!(program, &expected);
    assert!(!operations[8].fault_class().is_prunable());
    for dropped in [0, 4, 6, 7] {
        assert!(operations[dropped].fault_class().is_prunable());
    }
    assert_eq!(materialized.output_count(), 2);
    assert!(crate::ScalarProgramRegisterFlow::derive(program).is_ok());
}
