use super::*;

#[test]
fn emitted_indexed_seeds_match_canonical_rounding_and_nonfinite_coordinates() {
    let span = rumoca_core::Span::from_offsets(
        rumoca_core::SourceId::from_source_name("indexed_seed.mo"),
        0,
        53,
    );
    let source = ScalarProgramBlock::with_program_spans(
        vec![vec![
            LinearOp::LoadY { dst: 0, index: 0 },
            LinearOp::LoadIndexedSeed {
                dst: 1,
                base: 0,
                count: 2,
                index: 0,
            },
            LinearOp::StoreOutput { src: 1 },
        ]],
        vec![span],
    )
    .unwrap();
    let layout = VarLayout::from_parts(Default::default(), 2, 0);
    let code = WasmJacobian::compile(&source, &layout, &SolvePureCallTable::default()).unwrap();
    let prepared = rumoca_eval_solve::PreparedScalarProgramBlock::new(source).unwrap();
    let seed = [11.0, 23.0];
    let mut failures = Vec::new();
    for index in [
        0.5_f64.next_down(),
        0.5,
        0.5_f64.next_up(),
        -0.5,
        1.5,
        f64::NAN,
        f64::INFINITY,
        f64::NEG_INFINITY,
    ] {
        let y = [index, 0.0];
        let mut expected = [0.0];
        prepared
            .eval_with_context(
                &y,
                &[],
                0.0,
                rumoca_eval_solve::RowEvalContext {
                    seed: Some(&seed),
                    ..Default::default()
                },
                &mut expected,
            )
            .unwrap();
        let mut actual = [19.0];
        match code.call(&y, &[], 0.0, &seed, &[], &mut actual) {
            Ok(()) if actual[0].to_bits() == expected[0].to_bits() => {}
            Ok(()) => failures.push(format!("index {index:?}: {} != {}", actual[0], expected[0])),
            Err(error) => failures.push(format!("index {index:?}: {error}")),
        }
    }
    assert!(failures.is_empty(), "{}", failures.join("\n"));
}
