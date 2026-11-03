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
