//! Complementary rectangular slices retain compact exact ownership.

use super::*;

fn slice(rows: usize, columns: usize, start: usize, width: usize, output: usize) -> ComputeNode {
    let mut node = family(
        rows,
        output,
        start,
        LinearOp::LoadP {
            dst: 1,
            index: start,
        },
    );
    let ComputeNode::Map {
        domain,
        output_map,
        load_strides,
        ..
    } = &mut node
    else {
        unreachable!()
    };
    domain.binders.push(StructuredIndexBinder {
        id: 1,
        display_name: "column".into(),
        lower: 1,
        upper: width as i64,
        step: 1,
    });
    *output_map = TensorOutputMap::dense_contiguous(output, domain).unwrap();
    for load in load_strides {
        load.terms = vec![
            AffineStencilIndexStrideTerm {
                dimension: 0,
                stride: columns as isize,
            },
            AffineStencilIndexStrideTerm {
                dimension: 1,
                stride: 1,
            },
        ];
    }
    node
}

#[test]
fn complementary_rectangular_slices_prove_exact_coverage_and_replay() {
    for (rows, columns, width) in [(15, 15, 6), (90, 160, 64)] {
        let source = ComputeBlock {
            nodes: vec![
                slice(rows, columns, 0, width, 0),
                slice(rows, columns, width, columns - width, rows * width),
            ],
        };
        let targets = [0..width, width..columns]
            .into_iter()
            .flat_map(|range| {
                (0..rows).flat_map(move |row| {
                    range
                        .clone()
                        .map(move |column| Some(scalar_slot_y(row * columns + column)))
                })
            })
            .collect::<Vec<_>>();
        let layout = VarLayout::from_parts(Default::default(), rows * columns, rows * columns);
        let mut owner = crate::ContinuousRefreshOwners::default();
        owner
            .issue_native_assignment_schedule(&source, &targets, &layout)
            .unwrap();
        let stages = owner.native_assignment_schedule().unwrap().stages();
        assert_eq!(stages.len(), 2);
        assert_eq!(stages[0].target_range(), None);
        assert_eq!(stages[0].target_count(), rows * width);
        assert_eq!(stages[0].target_block_width(), width);
        assert_eq!(stages[0].target_stride(), columns);
        assert_eq!(stages[0].target_span(), 0..(rows - 1) * columns + width);
        assert!(!stages[0].targets_overlap(&stages[1]).unwrap());
        owner
            .validate_native_assignment_schedule(&source, &targets, &layout)
            .unwrap();
        let mut altered = targets.clone();
        altered.swap(0, rows * width);
        assert!(
            owner
                .validate_native_assignment_schedule(&source, &altered, &layout)
                .is_err()
        );
    }
}

#[test]
fn block_intersections_match_an_independent_scalar_set_oracle() {
    for start_a in 0..3 {
        for start_b in 0..3 {
            check_widths(start_a, start_b);
        }
    }
}

fn check_widths(start_a: usize, start_b: usize) {
    for width_a in 1..5 {
        for width_b in 1..5 {
            check_strides(start_a, start_b, width_a, width_b);
        }
    }
}

fn check_strides(start_a: usize, start_b: usize, width_a: usize, width_b: usize) {
    for stride_a in width_a..width_a + 4 {
        for stride_b in width_b..width_b + 4 {
            let a = coverage::Coverage {
                span: start_a..start_a + 2 * stride_a + width_a,
                count: 3 * width_a,
                stride: stride_a,
                width: width_a,
            };
            let b = coverage::Coverage {
                span: start_b..start_b + stride_b + width_b,
                count: 2 * width_b,
                stride: stride_b,
                width: width_b,
            };
            let a_slots = (0..3)
                .flat_map(|i| (0..width_a).map(move |j| start_a + i * stride_a + j))
                .collect::<Vec<_>>();
            let b_slots = (0..2)
                .flat_map(|i| (0..width_b).map(move |j| start_b + i * stride_b + j))
                .collect::<Vec<_>>();
            let expected = a_slots.iter().any(|slot| b_slots.contains(slot));
            assert_eq!(a.overlaps(&b).unwrap(), expected, "{a:?} / {b:?}");
            assert_eq!(b.overlaps(&a).unwrap(), expected);
        }
    }
}
