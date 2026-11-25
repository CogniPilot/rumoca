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
    assert_eq!(first_reads[0].indices(), [2]);
    assert_eq!(replaced_reads[0].indices(), [3]);
    assert_eq!(
        row_reads(&first, 1, false).unwrap(),
        row_reads(&replaced, 1, false).unwrap()
    );
}

/// `guards` unused guards over consecutive solver coordinates from `first_y`,
/// then `outputs` constant results: one full-shape loop whose every output
/// carries the program's execution obligations.
fn guarded_loop(first_y: usize, guards: usize, outputs: usize) -> solve::ScalarProgramBlock {
    let mut ops = Vec::new();
    for offset in 0..guards {
        let base = u32::try_from(3 * offset).unwrap();
        for op in unused_guard(first_y + offset) {
            ops.push(match op {
                Op::Const { dst, value } => Op::Const {
                    dst: dst + base,
                    value,
                },
                Op::LoadY { dst, index } => Op::LoadY {
                    dst: dst + base,
                    index,
                },
                Op::PureCall {
                    dst_start,
                    input_starts,
                    site,
                } => Op::PureCall {
                    dst_start: dst_start + base,
                    input_starts: input_starts.iter().map(|start| start + base).collect(),
                    site,
                },
                Op::StoreOutput { .. } => continue,
                other => panic!("unexpected guard op {other:?}"),
            });
        }
    }
    ops.extend((0..outputs).map(|_| Op::StoreOutput { src: 0 }));
    block(vec![ops], (0..outputs).collect())
}

/// A row's reads as the test sees them: the indices read, and the entries held
/// to represent them.
trait StoredReads {
    fn indices(&self) -> Vec<usize>;
    fn stored_entries(&self) -> usize;
}

impl StoredReads for solve::IndexIntervals {
    fn indices(&self) -> Vec<usize> {
        self.iter().collect()
    }

    fn stored_entries(&self) -> usize {
        self.intervals().count()
    }
}

/// #683: a full-shape loop's outputs each read the loop's obligations, so the
/// rows must hold them as one run per output however wide the loop is, not as
/// one entry per coordinate per output; and the cones must not change.
#[test]
fn a_full_shape_loops_execution_reads_are_stored_per_run_not_per_coordinate() {
    let (guards, outputs) = (64, 48);
    let residual = guarded_loop(2, guards, outputs);
    let rows = row_reads(&residual, outputs, true).unwrap();
    assert_eq!(rows.len(), outputs);
    let expected: Vec<usize> = (2..2 + guards).collect();
    for row in &rows {
        assert_eq!(row.indices(), expected);
    }
    let stored: usize = rows.iter().map(StoredReads::stored_entries).sum();
    assert!(
        stored <= 2 * outputs,
        "{stored} entries for {outputs} rows reading one run of {guards}"
    );
    assert!(
        row_reads(&residual, outputs, false)
            .unwrap()
            .iter()
            .all(|row| row.indices().is_empty()),
        "value reads see no guard"
    );
}

/// The cones over the full-shape loop do not depend on how the rows hold
/// their reads: block `p` solves coordinate `2 + p` from a row that reads the
/// whole loop shifted by one, so the closure of the residual reads is every
/// block, the last one through the shifted rows alone.
#[test]
fn a_full_shape_loops_cones_are_every_block_over_a_shifted_loop() {
    let (guards, outputs) = (64, 48);
    let rows = row_reads(&guarded_loop(2, guards, outputs), outputs, true).unwrap();
    let implicit = guarded_loop(3, guards, guards);
    let blocks: Vec<_> = (0..guards + 1)
        .map(|position| solve::AlgebraicProjectionBlock {
            rows: vec![position],
            y_indices: vec![2 + position],
            ..Default::default()
        })
        .collect();
    let implicit_rows = row_reads(&implicit, guards + 1, true).unwrap();
    let cones = read_cones(&rows, &implicit_rows, &blocks, None).unwrap();
    let every_block: Vec<usize> = (0..=guards).collect();
    assert!(cones.iter().all(|cone| **cone == *every_block), "{cones:?}");
}
