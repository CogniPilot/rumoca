use crate::{LinearOp, ScalarProgramBlock};

fn block(indices: Vec<usize>, ranged: bool) -> ScalarProgramBlock {
    let span = rumoca_core::Span::from_offsets(
        rumoca_core::SourceId::from_source_name("OutputSpan.mo"),
        1,
        2,
    );
    let mut ops = vec![LinearOp::Const { dst: 0, value: 2.0 }];
    if ranged {
        ops.push(LinearOp::TensorFill {
            dst_start: 1,
            value_start: 0,
            count: indices.len(),
            lanes: 1,
        });
        ops.push(LinearOp::StoreOutputRange {
            start: 1,
            count: indices.len(),
            stride: 1,
        });
    } else {
        ops.extend(indices.iter().map(|_| LinearOp::StoreOutput { src: 0 }));
    }
    ScalarProgramBlock::with_output_indices(vec![ops], vec![span], indices).unwrap()
}

#[test]
fn output_spans_follow_checked_native_store_mapping_and_wire_replay() {
    for indices in [vec![7, 9, 11], vec![7, 5, 3], vec![7, 7, 7]] {
        let source = block(indices.clone(), true);
        let span = source.program_output_span(0, 2).unwrap();
        assert_eq!(span.count(), indices.len());
        assert_eq!(span.start(), indices[0]);
        assert_eq!(span.stride(), indices[0].abs_diff(indices[1]));
        assert_eq!(span.descending(), indices[1] < indices[0]);
        assert_eq!(
            (0..span.count())
                .map(|i| span.index(i).unwrap())
                .collect::<Vec<_>>(),
            indices
        );
        assert_eq!(span.index(span.count()), None);
        let wire = serde_json::to_value(&source).unwrap();
        assert!(wire.get("output_spans").is_none());
        let replay: ScalarProgramBlock = serde_json::from_value(wire).unwrap();
        assert_eq!(replay.program_output_span(0, 2), Some(span));
    }
}

#[test]
fn output_spans_never_reassemble_scalar_stores_or_guess_irregular_offsets() {
    for ranged in [false, true] {
        let owner = block(vec![7, 9, 12], ranged);
        assert_eq!(
            owner.program_output_span(0, if ranged { 2 } else { 1 }),
            None
        );
    }
    let scalar = block(vec![7, 9, 11], false);
    assert_eq!(scalar.program_output_span(0, 1), None);
}
