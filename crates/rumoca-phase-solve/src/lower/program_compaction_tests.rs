use super::*;

#[test]
fn optional_fusion_keeps_checked_programs_when_the_combination_exceeds_metadata_budget() {
    let mut sources = rumoca_core::SourceMap::new();
    let source = sources.add("SharedOutputs.mo", "7.0");
    let span = Span::from_offsets(source, 0, 3);
    let program = vec![
        solve::LinearOp::Const { dst: 0, value: 7.0 },
        solve::LinearOp::StoreOutput { src: 0 },
    ];
    let evaluate = |programs: Vec<Vec<solve::LinearOp>>| {
        let spans = vec![span; programs.len()];
        let block = solve::ScalarProgramBlock::with_program_spans(programs, spans).unwrap();
        let mut out = vec![0.0; block.output_count()];
        rumoca_eval_solve::eval_scalar_program_block(&block, &[], &[], 0.0, None, &mut out)
            .unwrap();
        out
    };
    let mut separate = vec![program.clone(), program.clone()];
    let budget = scalar::MetadataBudget::with_bytes(256);
    assert!(
        separate
            .iter()
            .all(|program| budget.permits_operations(program.len()))
    );
    compact_identical_program_prefixes(
        &mut separate,
        &mut vec![span; 2],
        &mut vec![
            solve::ScalarProgramOutputPlacement::Range(0..1),
            solve::ScalarProgramOutputPlacement::Range(1..2),
        ],
        budget,
    );
    assert_eq!(separate.len(), 2);
    assert_eq!(evaluate(separate), vec![7.0, 7.0]);

    let mut fused = vec![program.clone(), program];
    compact_identical_program_prefixes(
        &mut fused,
        &mut vec![span; 2],
        &mut vec![
            solve::ScalarProgramOutputPlacement::Range(0..1),
            solve::ScalarProgramOutputPlacement::Range(1..2),
        ],
        scalar::MetadataBudget::with_bytes(1024),
    );
    assert_eq!(fused.len(), 1);
    assert_eq!(evaluate(fused), vec![7.0, 7.0]);
}

#[test]
fn source_range_append_preserves_large_native_stores_through_prefix_fusion() {
    let source = rumoca_core::SourceId::from_source_name("CompactFamilyAppend.mo");
    let span = Span::from_offsets(source, 0, 1);
    let count = 131_072;
    let program = vec![
        solve::LinearOp::Const { dst: 0, value: 7.0 },
        solve::LinearOp::TensorFill {
            dst_start: 1,
            value_start: 0,
            count,
            lanes: 1,
        },
        solve::LinearOp::StoreOutputRange {
            start: 1,
            count,
            stride: 1,
        },
    ];
    let mut rows = ScalarRows::default();
    rows.push_output_range(program.clone(), span, 7..7 + count);
    rows.push_output_range(program, span, 300_000..300_000 + count);
    let compute = rows.into_compute_block().unwrap();
    let [solve::ComputeNode::ScalarPrograms(block)] = compute.nodes.as_slice() else {
        panic!("compact scalar source owner was changed");
    };
    assert_eq!(block.row_count(), 1);
    assert_eq!(block.programs()[0].len(), 4);
    assert_eq!(block.stored_output_count(), 2 * count);
    assert_eq!(block.output_position(300_000), Some((0, count)));
    assert_eq!(block.program_output_span(0, 2).unwrap().start(), 7);
    assert_eq!(block.program_output_span(0, 3).unwrap().start(), 300_000);
    assert!(serde_json::to_vec(block).unwrap().len() < 1_000);
}

#[test]
fn source_range_append_checks_cardinality_against_the_actual_program() {
    let span = Span::from_offsets(
        rumoca_core::SourceId::from_source_name("BadRangeAppend.mo"),
        0,
        1,
    );
    let mut rows = ScalarRows::default();
    rows.push_output_range(
        vec![
            solve::LinearOp::Const { dst: 0, value: 1.0 },
            solve::LinearOp::StoreOutput { src: 0 },
        ],
        span,
        7..9,
    );
    let error = rows.into_scalar_block().unwrap_err();
    assert!(matches!(error, LowerError::ContractViolation { .. }));
}

#[test]
fn event_cursor_counts_stored_ranges_and_empty_programs_without_point_metadata() {
    let span = Span::from_offsets(
        rumoca_core::SourceId::from_source_name("RootCursor.mo"),
        0,
        1,
    );
    let mut rows = ScalarRows::default();
    rows.push_output_range(Vec::new(), span, 0..0);
    rows.push_output_range(
        vec![solve::LinearOp::StoreOutputRange {
            start: 0,
            count: 131_072,
            stride: 1,
        }],
        span,
        0..131_072,
    );
    rows.push_outputs(
        vec![
            solve::LinearOp::StoreOutput { src: 0 },
            solve::LinearOp::StoreOutput { src: 0 },
        ],
        span,
        [7, 7],
    );
    assert_eq!(rows.len(), 3);
    assert_eq!(rows.stored_output_count(span).unwrap(), 131_074);
    rows.push_output_range(
        vec![solve::LinearOp::StoreOutputRange {
            start: 0,
            count: usize::MAX,
            stride: 1,
        }],
        span,
        0..usize::MAX,
    );
    assert!(rows.stored_output_count(span).is_err());
    assert_eq!(
        solve::ScalarProgramBlock::checked_program_output_count(&[
            solve::LinearOp::StoreOutputRange {
                start: 0,
                count: usize::MAX,
                stride: 1
            },
            solve::LinearOp::StoreOutput { src: 0 },
        ]),
        None
    );
}
