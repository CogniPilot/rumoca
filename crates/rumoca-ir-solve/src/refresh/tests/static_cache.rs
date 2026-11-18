use super::*;

#[test]
fn global_cache_keys_keep_other_family_and_complete_original_program_reads() {
    let original = two_output_source(false);
    let ComputeNode::ScalarPrograms(block) = &original.nodes[0] else {
        unreachable!()
    };
    let mut operations = block.programs()[0].clone();
    // This read lies after both stored values, outside the assignment prefix.
    // The original runtime still validates and keys the complete source.
    operations.push(LinearOp::LoadP { dst: 6, index: 7 });
    let source = ComputeBlock::from_scalar_program_block(
        ScalarProgramBlock::with_program_spans(
            vec![operations],
            vec![block.program_span(0).unwrap()],
        )
        .unwrap(),
    );
    let rows = vec![grouped_row(0, 0, 1), grouped_row(1, 1, 4)];
    let root_plan = RefreshPlan {
        static_causal_seed_rows: selection(rows.len(), [0]),
        dynamic_causal_seed_rows: selection(rows.len(), [1]),
        rows,
        ..Default::default()
    };
    let owners = ContinuousRefreshOwners::checked_for_source(
        &source,
        RefreshPlan::default(),
        RefreshPlan::default(),
        root_plan,
        RefreshPlan::default(),
        Vec::new(),
    )
    .unwrap();
    assert!(owners.derivative().rows.is_empty());
    assert_eq!(&*owners.static_cache_parameters(&source).unwrap(), [7]);
}
