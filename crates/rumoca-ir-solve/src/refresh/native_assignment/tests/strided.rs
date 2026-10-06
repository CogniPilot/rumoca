//! Exact column ownership and compact progression intersection controls.

use super::*;

fn columns(rows: usize, columns: usize) -> (ComputeBlock, Vec<Option<ScalarSlot>>, VarLayout) {
    let mut nodes = Vec::new();
    let mut targets = Vec::new();
    for column in 0..columns {
        let mut node = family(
            rows,
            column * rows,
            column,
            LinearOp::LoadP {
                dst: 1,
                index: column * rows,
            },
        );
        let ComputeNode::Map { load_strides, .. } = &mut node else {
            unreachable!()
        };
        load_strides[0].terms[0].stride = columns as isize;
        nodes.push(node);
        targets.extend((0..rows).map(|row| Some(scalar_slot_y(row * columns + column))));
    }
    (
        ComputeBlock { nodes },
        targets,
        VarLayout::from_parts(Default::default(), rows * columns, rows * columns),
    )
}

#[test]
fn interleaved_columns_own_only_their_exact_slots_and_replay_source_inventory() {
    for (rows, column_count) in [(2, 3), (6, 16), (90, 160)] {
        let (source, targets, layout) = columns(rows, column_count);
        let owner = derive(&source, &targets, &layout).unwrap();
        let schedule = &owner;
        assert_eq!(schedule.stages().len(), column_count);
        for (column, stage) in schedule.stages().iter().enumerate() {
            assert_eq!(stage.target_range(), None);
            assert_eq!(stage.target_count(), rows);
            assert_eq!(stage.target_stride(), column_count);
            assert_eq!(
                stage.target_span(),
                column..(rows - 1) * column_count + column + 1
            );
            for other in &schedule.stages()[..column] {
                assert!(!stage.targets_overlap(other).unwrap());
            }
        }
        let mut changed = targets.clone();
        changed.swap(0, rows);
    }
}

#[test]
fn aliased_missing_non_affine_and_coupled_column_ownership_is_refused() {
    let (source, targets, layout) = columns(6, 16);
    let mut aliased = targets.clone();
    aliased[6] = aliased[0];
    let mut missing = targets.clone();
    missing[0] = None;
    let mut irregular = targets.clone();
    irregular.swap(0, 1);
    for bad in [aliased, missing, irregular] {
        assert!(derive(&source, &bad, &layout).is_err());
    }
    let mut coupled = source.clone();
    let ComputeNode::Map {
        base_ops,
        load_strides,
        ..
    } = &mut coupled.nodes[0]
    else {
        unreachable!()
    };
    base_ops[1] = LinearOp::LoadY { dst: 1, index: 0 };
    load_strides[1].terms[0].stride = 16;
    assert!(derive(&coupled, &targets, &layout).is_err());
}

#[test]
fn dependency_order_uses_exact_column_sets_and_real_cycles_still_refuse() {
    let (mut source, targets, layout) = columns(6, 3);
    let ComputeNode::Map {
        base_ops,
        load_strides,
        ..
    } = &mut source.nodes[0]
    else {
        unreachable!()
    };
    base_ops[1] = LinearOp::LoadY { dst: 1, index: 1 };
    load_strides[1].terms[0].stride = 3;
    let owner = derive(&source, &targets, &layout).unwrap();
    assert_eq!(
        owner
            .stages()
            .iter()
            .map(continuous_node)
            .collect::<Vec<_>>(),
        [1, 0, 2]
    );
    let ComputeNode::Map {
        base_ops,
        load_strides,
        ..
    } = &mut source.nodes[1]
    else {
        unreachable!()
    };
    base_ops[1] = LinearOp::LoadY { dst: 1, index: 0 };
    load_strides[1].terms[0].stride = 3;
    assert_eq!(
        derive(&source, &targets, &layout).unwrap_err().to_string(),
        "native assignment dependency cycle"
    );
}

#[test]
fn compact_progression_intersections_match_an_independent_finite_set_oracle() {
    for start_a in 0..7 {
        for start_b in 0..7 {
            check_steps(start_a, start_b);
        }
    }
}

fn check_steps(start_a: usize, start_b: usize) {
    for stride_a in 1..9 {
        for stride_b in 1..9 {
            let a = coverage::Coverage {
                span: start_a..start_a + 4 * stride_a + 1,
                count: 5,
                stride: stride_a,
                width: 1,
            };
            let b = coverage::Coverage {
                span: start_b..start_b + 3 * stride_b + 1,
                count: 4,
                stride: stride_b,
                width: 1,
            };
            let expected =
                (0..5).any(|i| (0..4).any(|j| start_a + i * stride_a == start_b + j * stride_b));
            assert_eq!(a.overlaps(&b).unwrap(), expected, "{a:?} / {b:?}");
            assert_eq!(b.overlaps(&a).unwrap(), expected);
        }
    }
}
