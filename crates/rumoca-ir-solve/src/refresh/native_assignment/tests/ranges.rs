use super::*;
use crate::{ScalarProgramBlock, TensorInputKind, UnaryOp};

fn span() -> Span {
    Span::from_offsets(SourceId::from_source_name("NativeArray.mo"), 10, 20)
}

fn program(
    count: usize,
    target: usize,
    input: TensorInputKind,
    input_start: usize,
) -> Vec<LinearOp> {
    let n = count as u32;
    vec![
        LinearOp::TensorLoad {
            dst_start: 0,
            input: TensorInputKind::Y,
            input_start: target,
            count,
            seed_start: None,
            lanes: 1,
        },
        LinearOp::TensorLoad {
            dst_start: n,
            input,
            input_start,
            count,
            seed_start: None,
            lanes: 1,
        },
        LinearOp::TensorBinary {
            dst_start: 2 * n,
            op: BinaryOp::Sub,
            lhs_start: 0,
            rhs_start: n,
            count,
            lhs_stride: 1,
            rhs_stride: 1,
            lanes: 1,
        },
        LinearOp::StoreOutputRange {
            start: 2 * n,
            count,
            stride: 1,
        },
    ]
}

fn source(programs: Vec<Vec<LinearOp>>) -> ComputeBlock {
    let spans = vec![span(); programs.len()];
    ComputeBlock {
        nodes: vec![ComputeNode::ScalarPrograms(
            ScalarProgramBlock::with_program_spans(programs, spans).unwrap(),
        )],
    }
}

fn fixture(count: usize) -> (ComputeBlock, Vec<Option<ScalarSlot>>, VarLayout) {
    (
        source(vec![
            program(count, 0, TensorInputKind::Y, count),
            program(count, count, TensorInputKind::P, 0),
        ]),
        (0..2 * count)
            .map(|index| Some(scalar_slot_y(index)))
            .collect(),
        VarLayout::from_parts(Default::default(), 2 * count, count),
    )
}

#[test]
fn complete_array_programs_keep_two_compact_stages_and_exact_dependency_order() {
    for count in [1, 9, 225, 14_400, 57_600] {
        let (block, targets, layout) = fixture(count);
        let schedule = derive(&block, &targets, &layout).unwrap();
        assert_eq!(schedule.stages.len(), 2);
        assert_eq!(schedule.stages[0].target_range().unwrap(), count..2 * count);
        assert_eq!(schedule.stages[1].target_range().unwrap(), 0..count);
        let ComputeNode::ScalarPrograms(original) = &block.nodes[0] else {
            panic!("source")
        };
        for (stage, original_program) in schedule.stages.iter().zip([1, 0]) {
            let ComputeNode::ScalarPrograms(value) = &stage.value_kernel().nodes[0] else {
                panic!("value")
            };
            assert_eq!(value.programs().len(), 1);
            assert_eq!(value.programs()[0].len(), 4);
            assert!(operations_match(
                &value.programs()[0][..3],
                &original.programs()[original_program][..3]
            ));
            assert_eq!(
                value.programs()[0][3],
                LinearOp::StoreOutputRange {
                    start: count as u32,
                    count,
                    stride: 1
                }
            );
        }
    }
}

#[test]
fn reversed_residual_and_broadcast_rhs_use_the_independent_original_range() {
    for reverse in [false, true] {
        let count = 9;
        let mut ops = program(count, 0, TensorInputKind::P, 0);
        ops[1] = LinearOp::LoadP { dst: 9, index: 0 };
        let LinearOp::TensorBinary {
            lhs_start,
            rhs_start,
            lhs_stride,
            rhs_stride,
            ..
        } = &mut ops[2]
        else {
            panic!("residual")
        };
        *rhs_stride = 0;
        if reverse {
            std::mem::swap(lhs_start, rhs_start);
            std::mem::swap(lhs_stride, rhs_stride);
        }
        let targets = (0..count)
            .map(|i| Some(scalar_slot_y(i)))
            .collect::<Vec<_>>();
        let layout = VarLayout::from_parts(Default::default(), count, 1);
        let schedule = derive(&source(vec![ops]), &targets, &layout).unwrap();
        let ComputeNode::ScalarPrograms(value) = &schedule.stages[0].value_kernel().nodes[0] else {
            panic!("value")
        };
        assert_eq!(
            value.programs()[0].last(),
            Some(&LinearOp::StoreOutputRange {
                start: 27,
                count,
                stride: 1
            })
        );
        assert_eq!(
            value.programs()[0][3],
            LinearOp::TensorFill {
                dst_start: 27,
                value_start: 9,
                count,
                lanes: 1
            }
        );
    }
}

#[test]
fn compact_matrix_value_and_unused_target_dependent_prefix_remain_intact() {
    let count = 9;
    let mut ops = program(count, 0, TensorInputKind::P, 0);
    ops.insert(
        2,
        LinearOp::TensorIdentity {
            dst_start: 27,
            size: 3,
            lanes: 1,
        },
    );
    ops.insert(
        3,
        LinearOp::MatrixMultiply {
            dst_start: 36,
            lhs_start: 9,
            rhs_start: 27,
            rows: 3,
            inner: 3,
            columns: 3,
            lanes: 1,
        },
    );
    if let LinearOp::TensorBinary { rhs_start, .. } = &mut ops[4] {
        *rhs_start = 36;
    }
    ops.insert(5, LinearOp::LoadY { dst: 45, index: 0 });
    ops.insert(
        6,
        LinearOp::Unary {
            dst: 46,
            op: UnaryOp::Sqrt,
            arg: 45,
        },
    );
    let block = source(vec![ops.clone()]);
    let targets = (0..count)
        .map(|i| Some(scalar_slot_y(i)))
        .collect::<Vec<_>>();
    let schedule = derive(
        &block,
        &targets,
        &VarLayout::from_parts(Default::default(), count, count),
    )
    .unwrap();
    let ComputeNode::ScalarPrograms(value) = &schedule.stages[0].value_kernel().nodes[0] else {
        panic!("value")
    };
    assert!(operations_match(
        &value.programs()[0][..ops.len() - 1],
        &ops[..ops.len() - 1]
    ));
    assert_eq!(
        value.programs()[0].last(),
        Some(&LinearOp::StoreOutputRange {
            start: 36,
            count,
            stride: 1
        })
    );
}

#[test]
fn whole_range_independence_rejects_scalar_and_matrix_target_coupling() {
    for matrix in [false, true] {
        let count = 9;
        let mut ops = program(count, 0, TensorInputKind::P, 0);
        ops.insert(2, LinearOp::LoadY { dst: 27, index: 4 });
        ops.insert(
            3,
            LinearOp::TensorBinary {
                dst_start: 28,
                op: BinaryOp::Mul,
                lhs_start: 9,
                rhs_start: 27,
                count,
                lhs_stride: 1,
                rhs_stride: 0,
                lanes: 1,
            },
        );
        let rhs = if matrix {
            ops.insert(
                4,
                LinearOp::MatrixMultiply {
                    dst_start: 37,
                    lhs_start: 28,
                    rhs_start: 9,
                    rows: 3,
                    inner: 3,
                    columns: 3,
                    lanes: 1,
                },
            );
            37
        } else {
            28
        };
        let residual = ops
            .iter_mut()
            .find(|op| {
                matches!(
                    op,
                    LinearOp::TensorBinary {
                        op: BinaryOp::Sub,
                        ..
                    }
                )
            })
            .unwrap();
        if let LinearOp::TensorBinary { rhs_start, .. } = residual {
            *rhs_start = rhs;
        }
        let targets = (0..count)
            .map(|i| Some(scalar_slot_y(i)))
            .collect::<Vec<_>>();
        assert_eq!(
            derive(
                &source(vec![ops]),
                &targets,
                &VarLayout::from_parts(Default::default(), count, count)
            )
            .unwrap_err()
            .0,
            "native tensor residual couples its owned target range"
        );
    }
}

#[test]
fn compact_ranges_refuse_cycles_target_aliases_and_register_overwrites() {
    let (block, mut targets, layout) = fixture(9);
    targets[1] = targets[0];
    assert!(derive(&block, &targets, &layout).is_err());
    let (_, targets, layout) = fixture(9);
    let cycle = source(vec![
        program(9, 0, TensorInputKind::Y, 9),
        program(9, 9, TensorInputKind::Y, 0),
    ]);
    assert_eq!(
        derive(&cycle, &targets, &layout).unwrap_err().0,
        "native assignment dependency cycle"
    );
    let mut overwrite = program(9, 0, TensorInputKind::P, 0);
    overwrite.insert(3, LinearOp::Const { dst: 0, value: 0. });
    let targets = targets[..9].to_vec();
    assert_eq!(
        derive(
            &source(vec![overwrite]),
            &targets,
            &VarLayout::from_parts(Default::default(), 9, 9)
        )
        .unwrap_err()
        .0,
        "native tensor program has overlapping destination versions"
    );
}

/// Two output segments of one program (a record-valued source: a tensor
/// field, then a scalar field), each after its own residual (SOLVE-C68).
fn segmented(scalar_value: LinearOp) -> Vec<LinearOp> {
    vec![
        LinearOp::TensorLoad {
            dst_start: 0,
            input: TensorInputKind::Y,
            input_start: 0,
            count: 2,
            seed_start: None,
            lanes: 1,
        },
        LinearOp::TensorLoad {
            dst_start: 2,
            input: TensorInputKind::P,
            input_start: 0,
            count: 2,
            seed_start: None,
            lanes: 1,
        },
        LinearOp::TensorBinary {
            dst_start: 4,
            op: BinaryOp::Sub,
            lhs_start: 0,
            rhs_start: 2,
            count: 2,
            lhs_stride: 1,
            rhs_stride: 1,
            lanes: 1,
        },
        LinearOp::StoreOutputRange {
            start: 4,
            count: 2,
            stride: 1,
        },
        LinearOp::LoadY { dst: 6, index: 2 },
        scalar_value,
        LinearOp::Binary {
            dst: 8,
            op: BinaryOp::Sub,
            lhs: 6,
            rhs: 7,
        },
        LinearOp::StoreOutput { src: 8 },
    ]
}

#[test]
fn segmented_record_stores_issue_one_stage_over_their_adjacent_targets() {
    let targets = (0..3)
        .map(|index| Some(scalar_slot_y(index)))
        .collect::<Vec<_>>();
    let layout = VarLayout::from_parts(Default::default(), 3, 3);
    let block = source(vec![segmented(LinearOp::LoadP { dst: 7, index: 2 })]);
    let schedule = derive(&block, &targets, &layout).unwrap();
    assert_eq!(schedule.stages.len(), 1);
    assert_eq!(schedule.stages[0].target_range().unwrap(), 0..3);
    // A segment value that reads a target the stage owns is coupled.
    let coupled = source(vec![segmented(LinearOp::LoadY { dst: 7, index: 0 })]);
    assert_eq!(
        derive(&coupled, &targets, &layout).unwrap_err().0,
        "native tensor residual couples its owned target range"
    );
    // Segment targets that are not one dense range are refused.
    let layout = VarLayout::from_parts(Default::default(), 4, 3);
    let sparse = vec![
        Some(scalar_slot_y(0)),
        Some(scalar_slot_y(1)),
        Some(scalar_slot_y(3)),
    ];
    assert!(derive(&block, &sparse, &layout).is_err());
}
