//! Source-ordered rectangular/reversed domains with exact affine target maps.

use super::*;

fn fixture(rows: usize, columns: usize) -> (ComputeBlock, Vec<Option<ScalarSlot>>, VarLayout) {
    let domain = StructuredIndexDomain {
        binders: vec![
            StructuredIndexBinder {
                id: 0,
                display_name: "column".into(),
                lower: 1,
                upper: columns as i64,
                step: 1,
            },
            StructuredIndexBinder {
                id: 1,
                display_name: "row".into(),
                lower: 1,
                upper: rows as i64,
                step: 1,
            },
        ],
    };
    let dense = TensorOutputMap::dense_contiguous(0, &domain).unwrap();
    let target = vec![
        AffineStencilIndexStrideTerm {
            dimension: 0,
            stride: 1,
        },
        AffineStencilIndexStrideTerm {
            dimension: 1,
            stride: columns as isize,
        },
    ];
    let mut node = family(rows * columns, 0, 0, LinearOp::LoadP { dst: 1, index: 0 });
    let ComputeNode::Map {
        domain: body_domain,
        output_map,
        load_strides,
        ..
    } = &mut node
    else {
        unreachable!()
    };
    *body_domain = domain;
    *output_map = dense.clone();
    load_strides[0].terms = target;
    load_strides[1].terms = dense.strides;
    let targets = (0..rows * columns)
        .map(|i| Some(scalar_slot_y(i % rows * columns + i / rows)))
        .collect();
    (
        ComputeBlock { nodes: vec![node] },
        targets,
        VarLayout::from_parts(Default::default(), rows * columns, rows * columns),
    )
}

#[test]
fn transposed_rectangular_target_maps_keep_compact_prefix_and_source_order() {
    for (rows, columns) in [(2, 3), (6, 16), (90, 160)] {
        let (source, targets, layout) = fixture(rows, columns);
        let owner = derive(&source, &targets, &layout).unwrap();
        let stage = &owner.stages()[0];
        assert_eq!(stage.target_range().unwrap(), 0..rows * columns);
        let ComputeNode::Map {
            domain,
            output_map,
            base_ops,
            ..
        } = &stage.value_kernel().nodes[0]
        else {
            unreachable!()
        };
        let ComputeNode::Map {
            domain: original,
            base_ops: prefix,
            ..
        } = &source.nodes[0]
        else {
            unreachable!()
        };
        assert_eq!(domain, original);
        assert_eq!(&base_ops[..5], &prefix[..5]);
        assert_eq!(
            output_map.strides,
            [
                AffineStencilIndexStrideTerm {
                    dimension: 0,
                    stride: 1
                },
                AffineStencilIndexStrideTerm {
                    dimension: 1,
                    stride: columns as isize
                },
            ]
        );
        let mut altered = targets.clone();
        altered.swap(0, 1);
    }
}

#[test]
fn reversed_target_map_has_bounded_offset_and_preserves_source_domain() {
    let mut source = ComputeBlock {
        nodes: vec![family(7, 0, 6, LinearOp::LoadP { dst: 1, index: 0 })],
    };
    let ComputeNode::Map { load_strides, .. } = &mut source.nodes[0] else {
        unreachable!()
    };
    load_strides[0].terms[0].stride = -1;
    let targets = (0..7)
        .rev()
        .map(|i| Some(scalar_slot_y(i)))
        .collect::<Vec<_>>();
    let layout = VarLayout::from_parts(Default::default(), 7, 7);
    let owner = derive(&source, &targets, &layout).unwrap();
    let ComputeNode::Map { output_map, .. } = &owner.stages()[0].value_kernel().nodes[0] else {
        unreachable!()
    };
    assert_eq!(output_map.start, 6);
    assert_eq!(output_map.strides[0].stride, -1);
}

#[test]
fn target_holes_aliases_non_affine_permutations_and_coupled_reads_are_refused() {
    let (source, targets, layout) = fixture(2, 3);
    let mut alias = targets.clone();
    alias[1] = alias[0];
    let mut hole = targets.clone();
    hole[1] = Some(scalar_slot_y(6));
    let mut non_affine = targets.clone();
    non_affine.swap(0, 1);
    for wrong in [alias, hole, non_affine] {
        assert!(derive(&source, &wrong, &layout).is_err());
    }
    let mut coupled = source.clone();
    let ComputeNode::Map { base_ops, .. } = &mut coupled.nodes[0] else {
        unreachable!()
    };
    base_ops[1] = LinearOp::LoadY { dst: 1, index: 0 };
    assert!(derive(&coupled, &targets, &layout).is_err());
}
