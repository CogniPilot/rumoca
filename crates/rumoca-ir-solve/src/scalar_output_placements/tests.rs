use crate::{
    LinearOp, ScalarOutputPlacementView, ScalarProgramBlock, ScalarProgramOutputPlacement,
};
use rumoca_core::{SourceId, Span};

fn span() -> Span {
    Span::from_offsets(SourceId::from_source_name("CompactOutputs.mo"), 1, 2)
}

fn program(count: usize) -> Vec<LinearOp> {
    vec![
        LinearOp::Const { dst: 0, value: 1.0 },
        LinearOp::TensorFill {
            dst_start: 1,
            value_start: 0,
            count,
            lanes: 1,
        },
        LinearOp::StoreOutputRange {
            start: 1,
            count,
            stride: 1,
        },
    ]
}

fn assert_unmaterialized(block: &ScalarProgramBlock) {
    assert!(block.data.materialized_output_indices.get().is_none());
}

#[test]
fn compact_output_placement_queries_and_wire_do_not_materialize_large_ranges() {
    for placement in [
        ScalarProgramOutputPlacement::Range(0..131_072),
        ScalarProgramOutputPlacement::Range(7..131_079),
    ] {
        let source = ScalarProgramBlock::with_program_output_placements(
            vec![program(131_072)],
            vec![span()],
            vec![placement],
        )
        .unwrap();
        let wire = serde_json::to_vec(&source).unwrap();
        assert!(wire.len() < 1_000);
        let replay: ScalarProgramBlock = serde_json::from_slice(&wire).unwrap();
        let binary = bincode::serialize(&source).unwrap();
        let binary_replay: ScalarProgramBlock = bincode::deserialize(&binary).unwrap();
        for owner in [&source, &replay, &binary_replay] {
            assert_eq!(owner.stored_output_count(), 131_072);
            let first = owner.output_index_at(0).unwrap();
            assert_eq!(owner.output_index_at(131_071), Some(first + 131_071));
            assert_eq!(owner.output_index_at(131_072), None);
            assert_eq!(owner.output_position(first + 131_071), Some((0, 131_071)));
            assert_eq!(
                owner.program_contiguous_output_range(0),
                Some(first..first + 131_072)
            );
            let store = owner.program_output_span(0, 2).unwrap();
            assert_eq!(store.count(), 131_072);
            assert_eq!(store.start(), first);
            assert_eq!(owner.output_count(), first + 131_072);
            assert_eq!(
                owner
                    .advance_compute_block_output_cursor("compact", 0, 0)
                    .unwrap(),
                first + 131_072
            );
            assert_unmaterialized(owner);
        }
    }
}

#[test]
fn mixed_output_placements_preserve_explicit_order_duplicate_first_owner_and_store_spans() {
    let programs = vec![program(4), program(2)];
    let source = ScalarProgramBlock::with_program_output_placements(
        programs.clone(),
        vec![span(); 2],
        vec![
            ScalarProgramOutputPlacement::Range(8..12),
            ScalarProgramOutputPlacement::Explicit(vec![2, 8]),
        ],
    )
    .unwrap();
    let explicit = ScalarProgramBlock::with_output_indices(
        programs,
        vec![span(); 2],
        vec![8, 9, 10, 11, 2, 8],
    )
    .unwrap();
    assert!(
        matches!(source.output_placements(), ScalarOutputPlacementView::ProgramRanges(entries) if entries.len() == 2)
    );
    for output in 0..15 {
        assert_eq!(
            source.output_position(output),
            explicit.output_position(output)
        );
    }
    assert_eq!(source.output_position(8), Some((0, 0)));
    assert_eq!(
        source.program_output_span(0, 2),
        explicit.program_output_span(0, 2)
    );
    assert_eq!(
        source.program_output_span(1, 2),
        explicit.program_output_span(1, 2)
    );
    assert_unmaterialized(&source);
    assert_eq!(source.output_indices(), explicit.output_indices());
    assert_eq!(
        source.output_indices().as_ptr(),
        source.clone().output_indices().as_ptr()
    );
    let replay: ScalarProgramBlock =
        serde_json::from_value(serde_json::to_value(&source).unwrap()).unwrap();
    assert_unmaterialized(&replay);
    assert_eq!(replay.output_position(8), Some((0, 0)));
}

#[test]
fn fused_program_placements_keep_each_native_store_source_range() {
    let mut ops = program(4);
    ops.push(LinearOp::StoreOutputRange {
        start: 1,
        count: 2,
        stride: 1,
    });
    let placement = ScalarProgramOutputPlacement::Range(8..12)
        .concatenate(ScalarProgramOutputPlacement::Range(20..22));
    let source = ScalarProgramBlock::with_program_output_placements(
        vec![ops],
        vec![span()],
        vec![placement],
    )
    .unwrap();
    assert_eq!(source.output_position(20), Some((0, 4)));
    assert_eq!(source.output_index_at(5), Some(21));
    assert_eq!(source.program_output_span(0, 2).unwrap().start(), 8);
    assert_eq!(source.program_output_span(0, 3).unwrap().start(), 20);
    assert_eq!(source.program_contiguous_output_range(0), None);
    assert_unmaterialized(&source);
}

#[test]
fn compact_output_wire_rejects_old_fields_forged_counts_programs_and_reversed_ranges() {
    let source = ScalarProgramBlock::with_program_output_placements(
        vec![program(2)],
        vec![span()],
        vec![ScalarProgramOutputPlacement::Range(7..9)],
    )
    .unwrap();
    let valid = serde_json::to_value(&source).unwrap();
    let mutations = [
        serde_json::json!({"Dense": {"count": 3}}),
        serde_json::json!({"ProgramRanges": []}),
        serde_json::json!({"ProgramRanges": [{"Range": {"start": 7, "end": 9, "output_indices": [7, 8]}}]}),
        serde_json::json!({"ProgramRanges": [{"Range": {"start": 9, "end": 7}}]}),
        serde_json::json!({"ProgramRanges": [{"Range": {"start": 7, "end": 8}}]}),
        serde_json::json!({"ProgramRanges": [{"Range": {"start": 7, "end": 9}}, {"Explicit": [1]}]}),
    ];
    for placement in mutations {
        let mut wire = valid.clone();
        wire["output_placements"] = placement;
        assert!(serde_json::from_value::<ScalarProgramBlock>(wire).is_err());
    }
    let mut old = valid.clone();
    old.as_object_mut().unwrap().remove("output_placements");
    old["output_indices"] = serde_json::json!([7, 8]);
    assert!(serde_json::from_value::<ScalarProgramBlock>(old).is_err());
    let mut extra = valid;
    extra["output_indices"] = serde_json::json!([7, 8]);
    assert!(serde_json::from_value::<ScalarProgramBlock>(extra).is_err());
}

#[test]
fn segmented_native_store_spans_match_original_explicit_order_and_fault_bounds() {
    use crate::ScalarOutputSegment as Segment;
    let cases = [
        (
            vec![Segment::Range(7..9), Segment::Range(9..11)],
            vec![7, 8, 9, 10],
        ),
        (
            vec![
                Segment::Range(7..8),
                Segment::Range(7..8),
                Segment::Range(7..8),
            ],
            vec![7, 7, 7],
        ),
        (
            vec![
                Segment::Range(7..8),
                Segment::Range(5..6),
                Segment::Range(3..4),
            ],
            vec![7, 5, 3],
        ),
        (
            vec![Segment::Explicit(vec![7, 9]), Segment::Range(11..12)],
            vec![7, 9, 11],
        ),
        (
            vec![Segment::Range(7..9), Segment::Range(11..13)],
            vec![7, 8, 11, 12],
        ),
    ];
    for (segments, indices) in cases {
        let ops = program(indices.len());
        let compact = ScalarProgramBlock::with_program_output_placements(
            vec![ops.clone()],
            vec![span()],
            vec![ScalarProgramOutputPlacement::Segments(segments)],
        )
        .unwrap();
        let explicit =
            ScalarProgramBlock::with_output_indices(vec![ops], vec![span()], indices.clone())
                .unwrap();
        assert_eq!(
            compact.program_output_span(0, 2),
            explicit.program_output_span(0, 2)
        );
        for output in 0..15 {
            assert_eq!(
                compact.output_position(output),
                explicit.output_position(output)
            );
        }
        assert_eq!(compact.output_index_at(indices.len()), None);
        assert_unmaterialized(&compact);
    }
}

#[test]
fn produced_output_image_unions_every_placement_kind_sorted_without_duplicates() {
    use crate::{ComputeBlock, ComputeNode, ScalarOutputSegment as Segment};
    let block = ScalarProgramBlock::with_program_output_placements(
        vec![program(3), program(2), program(4)],
        vec![span(); 3],
        vec![
            ScalarProgramOutputPlacement::Range(8..11),
            ScalarProgramOutputPlacement::Explicit(vec![9, 2]),
            ScalarProgramOutputPlacement::Segments(vec![
                Segment::Range(20..22),
                Segment::Explicit(vec![5, 8]),
            ]),
        ],
    )
    .unwrap();
    let source = ComputeBlock {
        nodes: vec![ComputeNode::ScalarPrograms(block)],
    };
    assert_eq!(
        source.produced_output_indices("placement union").unwrap(),
        [2, 5, 8, 9, 10, 20, 21]
    );
}
