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
    compact_identical_program_prefixes(&mut separate, &mut vec![span; 2], budget);
    assert_eq!(separate.len(), 2);
    assert_eq!(evaluate(separate), vec![7.0, 7.0]);

    let mut fused = vec![program.clone(), program];
    compact_identical_program_prefixes(
        &mut fused,
        &mut vec![span; 2],
        scalar::MetadataBudget::with_bytes(1024),
    );
    assert_eq!(fused.len(), 1);
    assert_eq!(evaluate(fused), vec![7.0, 7.0]);
}
