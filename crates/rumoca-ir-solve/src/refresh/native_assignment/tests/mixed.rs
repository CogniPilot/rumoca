use super::*;
use crate::ScalarProgramBlock;

fn span(offset: usize) -> Span {
    Span::from_offsets(
        SourceId::from_source_name("MixedNative.mo"),
        offset,
        offset + 1,
    )
}

fn gain(target: usize, input: LinearOp) -> Vec<LinearOp> {
    vec![
        LinearOp::LoadY {
            dst: 0,
            index: target,
        },
        input,
        LinearOp::Const { dst: 2, value: 0.5 },
        LinearOp::Binary {
            dst: 3,
            op: BinaryOp::Add,
            lhs: 1,
            rhs: 2,
        },
        LinearOp::Binary {
            dst: 4,
            op: BinaryOp::Sub,
            lhs: 0,
            rhs: 3,
        },
        // An unused source-prefix operation still binds replay and emission.
        LinearOp::Const { dst: 5, value: 0.0 },
        LinearOp::StoreOutput { src: 4 },
    ]
}

fn mixed(count: usize) -> (ComputeBlock, Vec<Option<ScalarSlot>>, VarLayout) {
    let mut consumer = family(
        count - 2,
        count + 1,
        count + 1,
        LinearOp::LoadY { dst: 1, index: 1 },
    );
    if let ComputeNode::Map { base_ops, .. } = &mut consumer {
        base_ops[2] = LinearOp::LoadY {
            dst: 2,
            index: count,
        };
    }
    let boundaries = ScalarProgramBlock::with_output_indices(
        vec![
            vec![
                LinearOp::LoadY {
                    dst: 0,
                    index: count * 2 - 1,
                },
                LinearOp::StoreOutput { src: 0 },
            ],
            gain(count, LinearOp::LoadP { dst: 1, index: 0 }),
        ],
        vec![span(0), span(1)],
        vec![count * 2 - 1, count],
    )
    .unwrap();
    (
        ComputeBlock {
            nodes: vec![
                consumer,
                family(count, 0, 0, LinearOp::LoadP { dst: 1, index: 0 }),
                ComputeNode::ScalarPrograms(boundaries),
            ],
        },
        (0..count * 2).map(|i| Some(scalar_slot_y(i))).collect(),
        VarLayout::from_parts(Default::default(), count * 2, count),
    )
}

fn replace_boundary(
    source: &mut ComputeBlock,
    edit: impl FnOnce(&mut Vec<Vec<LinearOp>>, &mut Vec<Span>),
) {
    let ComputeNode::ScalarPrograms(block) = &source.nodes[2] else {
        panic!("scalar source")
    };
    let mut programs = block.programs().to_vec();
    let mut spans = block.program_spans().to_vec();
    let indices = block.output_indices().to_vec();
    edit(&mut programs, &mut spans);
    source.nodes[2] = ComputeNode::ScalarPrograms(
        ScalarProgramBlock::with_output_indices(programs, spans, indices).unwrap(),
    );
}

#[test]
fn mixed_scalar_sparse_outputs_keep_compact_order_and_complete_source_prefix() {
    for count in [16, 160 * 90, 320 * 180] {
        let (source, targets, layout) = mixed(count);
        let schedule = derive(&source, &targets, &layout).unwrap();
        assert_eq!(schedule.stages().len(), 4);
        assert_eq!(
            schedule
                .stages()
                .iter()
                .map(|s| s.source_node())
                .collect::<Vec<_>>(),
            [1, 2, 2, 0]
        );
        assert_eq!(
            schedule.stages()[1].target_range().unwrap(),
            count * 2 - 1..count * 2
        );
        assert_eq!(
            schedule.stages()[2].target_range().unwrap(),
            count..count + 1
        );
        let ComputeNode::ScalarPrograms(original) = &source.nodes[2] else {
            panic!("source")
        };
        let ComputeNode::ScalarPrograms(value) = &schedule.stages()[2].value_kernel().nodes[0]
        else {
            panic!("value")
        };
        assert!(operations_match(
            &original.programs()[1][..6],
            &value.programs()[0][..6]
        ));
        assert_eq!(
            value.programs()[0].last(),
            Some(&LinearOp::StoreOutput { src: 3 })
        );
        let ComputeNode::ScalarPrograms(zero) = &schedule.stages()[1].value_kernel().nodes[0]
        else {
            panic!("zero")
        };
        assert_eq!(zero.programs()[0][0], original.programs()[0][0]);
        assert!(matches!(
            zero.programs()[0][1],
            LinearOp::Const { value: 0.0, .. }
        ));
    }
}

#[test]
fn mixed_scalar_dependencies_reject_cycles_bounds_and_aliases() {
    let (mut source, targets, layout) = mixed(16);
    replace_boundary(&mut source, |programs, _| {
        programs[1][1] = LinearOp::LoadY { dst: 1, index: 17 }
    });
    assert_eq!(
        derive(&source, &targets, &layout).unwrap_err().0,
        "native assignment dependency cycle"
    );
    let (mut source, targets, layout) = mixed(16);
    replace_boundary(&mut source, |programs, _| {
        programs[1][1] = LinearOp::LoadP { dst: 1, index: 16 }
    });
    assert_eq!(
        derive(&source, &targets, &layout).unwrap_err().0,
        "native scalar load exceeds its owned variable layout"
    );
    let (mut source, mut targets, layout) = mixed(16);
    targets[16] = Some(scalar_slot_y(0));
    replace_boundary(&mut source, |programs, _| {
        programs[1][0] = LinearOp::LoadY { dst: 0, index: 0 }
    });
    assert!(derive(&source, &targets, &layout).is_err());
}

#[test]
fn scalar_source_replay_retains_signed_zero_and_provenance() {
    for change_span in [false, true] {
        let (mut source, targets, layout) = mixed(16);
        let mut owner = crate::ContinuousRefreshOwners::default();
        owner
            .issue_native_assignment_schedule(&source, &targets, &layout)
            .unwrap();
        replace_boundary(&mut source, |programs, spans| {
            if change_span {
                spans[1] = span(10);
            } else {
                programs[1][5] = LinearOp::Const {
                    dst: 5,
                    value: -0.0,
                };
            }
        });
        assert!(
            owner
                .validate_native_assignment_schedule(&source, &targets, &layout)
                .is_err()
        );
        owner
            .issue_native_assignment_schedule(&source, &targets, &layout)
            .unwrap();
        owner
            .validate_native_assignment_schedule(&source, &targets, &layout)
            .unwrap();
    }
}

#[test]
fn scalar_output_mapping_replacement_revokes_equivalent_values() {
    let (mut source, targets, layout) = mixed(16);
    let mut owner = crate::ContinuousRefreshOwners::default();
    owner
        .issue_native_assignment_schedule(&source, &targets, &layout)
        .unwrap();
    let ComputeNode::ScalarPrograms(block) = &source.nodes[2] else {
        panic!("source")
    };
    let mut programs = block.programs().to_vec();
    programs.reverse();
    let mut spans = block.program_spans().to_vec();
    spans.reverse();
    let mut indices = block.output_indices().to_vec();
    indices.reverse();
    source.nodes[2] = ComputeNode::ScalarPrograms(
        ScalarProgramBlock::with_output_indices(programs, spans, indices).unwrap(),
    );
    assert!(derive(&source, &targets, &layout).is_ok());
    assert!(
        owner
            .validate_native_assignment_schedule(&source, &targets, &layout)
            .is_err()
    );
}

#[test]
fn scalar_prefix_effects_and_nonterminal_outputs_do_not_receive_certificates() {
    let (mut source, targets, layout) = mixed(16);
    replace_boundary(&mut source, |programs, _| {
        programs[1].insert(5, LinearOp::LoadSeed { dst: 6, index: 0 })
    });
    assert_eq!(
        derive(&source, &targets, &layout).unwrap_err().0,
        "native scalar program contains unsupported or effectful operations"
    );
    let (mut source, targets, layout) = mixed(16);
    replace_boundary(&mut source, |programs, _| {
        let last = programs[1].pop().unwrap();
        programs[1].insert(5, last);
    });
    assert_eq!(
        derive(&source, &targets, &layout).unwrap_err().0,
        "native scalar program requires one terminal scalar output"
    );
}

#[test]
fn native_map_zero_values_use_the_same_exact_owner_materializer() {
    let (mut source, targets, layout) = fixture(16);
    if let ComputeNode::Map {
        base_ops,
        load_strides,
        ..
    } = &mut source.nodes[1]
    {
        *base_ops = vec![
            LinearOp::LoadY { dst: 0, index: 0 },
            LinearOp::StoreOutput { src: 0 },
        ];
        load_strides.truncate(1);
    }
    let schedule = derive(&source, &targets, &layout).unwrap();
    let ComputeNode::Map { base_ops, .. } = &schedule.stages()[0].value_kernel().nodes[0] else {
        panic!("zero map")
    };
    assert!(matches!(base_ops[1], LinearOp::Const { value: 0.0, .. }));
}

#[test]
fn scalar_coupled_values_and_multiple_outputs_remain_outside_native_profile() {
    let (mut source, targets, layout) = mixed(16);
    replace_boundary(&mut source, |programs, _| {
        programs[1][3] = LinearOp::Binary {
            dst: 3,
            op: BinaryOp::Mul,
            lhs: 0,
            rhs: 0,
        };
    });
    assert!(derive(&source, &targets, &layout).is_err());
    let source = ComputeBlock {
        nodes: vec![ComputeNode::ScalarPrograms(
            ScalarProgramBlock::with_program_spans(
                vec![vec![
                    LinearOp::LoadY { dst: 0, index: 0 },
                    LinearOp::StoreOutput { src: 0 },
                    LinearOp::LoadY { dst: 1, index: 1 },
                    LinearOp::StoreOutput { src: 1 },
                ]],
                vec![span(0)],
            )
            .unwrap(),
        )],
    };
    let targets = vec![Some(scalar_slot_y(0)), Some(scalar_slot_y(1))];
    let layout = VarLayout::from_parts(Default::default(), 2, 0);
    assert_eq!(
        derive(&source, &targets, &layout).unwrap_err().0,
        "native scalar program requires one terminal scalar output"
    );
}
