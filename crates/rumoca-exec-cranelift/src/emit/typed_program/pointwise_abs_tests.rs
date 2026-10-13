use super::*;

#[test]
fn native_pointwise_abs_preserves_canonical_kinks_and_seed_cells() {
    let arithmetic = solve::SolveArithmeticProfile::construct(
        solve::SolveRealFormat::Binary64,
        solve::SolveIntegerDomain::FULL,
    );
    let tensor =
        solve::SolveValueType::tensor(solve::SolveScalarType::real(arithmetic), vec![2, 3])
            .unwrap();
    let at = rumoca_core::Span::from_offsets(
        rumoca_core::SourceId::from_source_name("native_pointwise_abs.mo"),
        1,
        2,
    );
    let table = solve::SolvePureCallTable::construct(arithmetic, |table| {
        table.add_owner(
            solve::SolvePureCallIdentity::issued(std::num::NonZeroU64::new(904).unwrap()),
            vec![tensor.clone()],
            vec![solve::SolvePureCallOutput::result(tensor)],
            at,
            |builder, inputs, outputs| {
                let input = builder.load(inputs[0], at)?;
                let result = builder.unary(solve::SolveUnaryOperator::Abs, input, at)?;
                builder.store(outputs[0], result, at)
            },
        )?;
        Ok(())
    })
    .unwrap();
    let compiled = CompiledPureCallTable::compile(&table).unwrap();
    let x = [-3.0_f64, -0.0, 0.0, 4.0, f64::INFINITY, f64::NAN];
    let seed = [0.3, -0.0, 2.0, 0.7, -3.0, 4.0];
    let input = x
        .into_iter()
        .chain(seed)
        .map(f64::to_bits)
        .collect::<Vec<_>>();
    let mut output = vec![0; 12];
    compiled
        .call_cells(
            rumoca_eval_solve::PureCallInvocation::Directional(
                table.owners()[0].call_site().directional().unwrap(),
            ),
            &input,
            &mut output,
        )
        .unwrap();
    for i in 0..x.len() {
        if x[i].is_nan() {
            assert!(f64::from_bits(output[i]).is_nan());
        } else {
            assert_eq!(output[i], x[i].abs().to_bits());
        }
        let tangent = if x[i] >= 0.0 { seed[i] } else { -seed[i] };
        assert_eq!(output[x.len() + i], tangent.to_bits());
    }
}
