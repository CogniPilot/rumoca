use super::*;
use solve::LinearOp as Op;

fn unused_guard(y: usize) -> Vec<Op> {
    let profile = solve::SolvePureCallTable::default().arithmetic();
    let real = solve::SolveValueType::scalar(solve::SolveScalarType::real(profile));
    let span = solve::source_span_from_offsets(814, 1, 2);
    let table = solve::SolvePureCallTable::construct(profile, |table| {
        table.add_owner(
            solve::SolvePureCallIdentity::issued(std::num::NonZeroU64::new(814).unwrap()),
            vec![real.clone(), real.clone()],
            vec![solve::SolvePureCallOutput::result(real)],
            span,
            |builder, inputs, outputs| {
                let x = builder.load(inputs[0], span)?;
                let guard = builder.load(inputs[1], span)?;
                builder.check_finite(guard, solve::SolveFiniteCheck::Derivative, span)?;
                builder.store(outputs[0], x, span)
            },
        )?;
        Ok(())
    })
    .unwrap();
    vec![
        Op::Const { dst: 0, value: 1.0 },
        Op::LoadY { dst: 1, index: y },
        Op::PureCall {
            dst_start: 2,
            input_starts: vec![0, 1].into(),
            site: table.owners()[0].call_site(),
        },
        Op::StoreOutput { src: 0 },
    ]
}

fn block(programs: Vec<Vec<Op>>, outputs: Vec<usize>) -> solve::ScalarProgramBlock {
    let spans = vec![solve::source_span_from_offsets(815, 1, 2); programs.len()];
    solve::ScalarProgramBlock::with_output_indices(programs, spans, outputs).unwrap()
}

#[test]
fn initialization_execution_cones_keep_unused_call_guards_and_sparse_output_placement() {
    let residual = block(vec![unused_guard(2)], vec![1]);
    let implicit = block(
        vec![
            unused_guard(3),
            vec![Op::Const { dst: 0, value: 2.0 }, Op::StoreOutput { src: 0 }],
        ],
        vec![0, 2],
    );
    let blocks = [
        solve::AlgebraicProjectionBlock {
            rows: vec![0],
            y_indices: vec![2],
            ..Default::default()
        },
        solve::AlgebraicProjectionBlock {
            rows: vec![2],
            y_indices: vec![3],
            ..Default::default()
        },
        // An unrelated block, potentially singular, must remain outside both cones.
        solve::AlgebraicProjectionBlock {
            rows: vec![1],
            y_indices: vec![4],
            ..Default::default()
        },
    ];
    let values = read_cones(
        &row_reads(&residual, 2, false).unwrap(),
        &row_reads(&implicit, 3, false).unwrap(),
        &blocks,
        None,
    )
    .unwrap();
    let execution = read_cones(
        &row_reads(&residual, 2, true).unwrap(),
        &row_reads(&implicit, 3, true).unwrap(),
        &blocks,
        None,
    )
    .unwrap();
    assert!(
        values.iter().all(|cone| cone.is_empty()),
        "tangent/value cones stay uncoarsened"
    );
    assert!(execution[0].is_empty());
    assert_eq!(
        &*execution[1],
        &[0, 1],
        "guard-only producer and its guard closure in issued order"
    );
    assert!(
        execution_cones_need_settlement(Some(&execution)),
        "guard-only reads activate settlement independently of value row roles"
    );
    assert!(!execution_cones_need_settlement(Some(&values)));
    assert!(
        execution_cones_need_settlement(None),
        "unproved closure activates the complete path"
    );
    assert_eq!(blocks_read_by(&execution, &[1]), Some(vec![0, 1]));
    assert_eq!(blocks_read_by(&values, &[1]), Some(vec![]));
    assert!(
        blocks_read_by(&execution, &[2]).is_none(),
        "unproved row requests every block"
    );
}

#[test]
fn execution_read_cones_are_bound_to_each_replacement_source() {
    let first = block(vec![unused_guard(2)], vec![0]);
    let replaced = block(vec![unused_guard(3)], vec![0]);
    let first_reads = row_reads(&first, 1, true).unwrap();
    let replaced_reads = row_reads(&replaced, 1, true).unwrap();
    assert_eq!(first_reads[0], std::collections::BTreeSet::from([2]));
    assert_eq!(replaced_reads[0], std::collections::BTreeSet::from([3]));
    assert_eq!(
        row_reads(&first, 1, false).unwrap(),
        row_reads(&replaced, 1, false).unwrap()
    );
}
