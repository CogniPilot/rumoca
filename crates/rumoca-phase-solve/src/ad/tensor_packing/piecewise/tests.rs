use super::*;

#[test]
fn piecewise_packing_preserves_the_exact_ordered_register_tuple() {
    let source = rumoca_core::SourceId::from_source_name("register_runs.mo");
    let span = rumoca_core::Span::from_offsets(source, 0, 1)
        .require_provenance("register run fixture")
        .unwrap();
    let y = [
        0.0,
        -0.0,
        2.0,
        -3.0,
        4.0,
        f64::INFINITY,
        6.0,
        7.0,
        f64::from_bits(0x7ff8_0000_0000_0011),
        9.0,
    ];
    let cases = [
        vec![0, 1, 2, 8, 8, 8, 5, 6, 7],
        vec![0, 9, 1, 9, 2, 9, 5, 9, 6, 9, 7, 9],
        vec![7, 1, 8, 3, 9, 0, 2, 6],
    ];
    for registers in cases {
        let mut builder = AdBuilder::new_with_span(SeedMode::SolverYOnly, span.span());
        for index in 0..y.len() {
            builder.emit_load_y(index).unwrap();
        }
        let start = builder.pack_registers(&registers).unwrap();
        builder.ops.push(LinearOp::StoreOutputRange {
            start,
            count: registers.len(),
            stride: 1,
        });
        let block = ScalarProgramBlock::with_source_span(vec![builder.ops], span).unwrap();
        let mut output = vec![0.0; registers.len()];
        rumoca_eval_solve::eval_scalar_program_block(&block, &y, &[], 0.0, None, &mut output)
            .unwrap();
        let expected = registers
            .iter()
            .map(|&index| y[index as usize].to_bits())
            .collect::<Vec<_>>();
        assert_eq!(
            output.into_iter().map(f64::to_bits).collect::<Vec<_>>(),
            expected
        );
    }
}
