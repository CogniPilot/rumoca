use super::*;

#[test]
fn native_pointwise_power_preserves_both_seed_terms_and_operand_orders() {
    let arithmetic = solve::SolveArithmeticProfile::construct(
        solve::SolveRealFormat::Binary64,
        solve::SolveIntegerDomain::FULL,
    );
    let element = solve::SolveScalarType::real(arithmetic);
    let scalar = solve::SolveValueType::scalar(element);
    let tensor = solve::SolveValueType::tensor(element, vec![2, 2]).unwrap();
    let at = rumoca_core::Span::from_offsets(
        rumoca_core::SourceId::from_source_name("native_pointwise_power.mo"),
        1,
        2,
    );
    for scalar_on_lhs in [false, true] {
        let table = solve::SolvePureCallTable::construct(arithmetic, |table| {
            table.add_owner(
                solve::SolvePureCallIdentity::issued(std::num::NonZeroU64::new(903).unwrap()),
                vec![tensor.clone(), scalar.clone()],
                vec![solve::SolvePureCallOutput::result(tensor.clone())],
                at,
                |builder, inputs, outputs| {
                    let aggregate = builder.load(inputs[0], at)?;
                    let scalar = builder.load(inputs[1], at)?;
                    let result = builder.broadcast_binary(
                        solve::SolveBinaryOperator::Power,
                        aggregate,
                        scalar,
                        scalar_on_lhs,
                        at,
                    )?;
                    builder.store(outputs[0], result, at)
                },
            )?;
            Ok(())
        })
        .unwrap();
        let compiled = CompiledPureCallTable::compile(&table).unwrap();
        let site = table.owners()[0].call_site();
        let aggregate = [1.5_f64, 2.0, 3.0, 4.0];
        let seed = [0.3, -0.2, 0.4, 0.7];
        let scalar = 2.5_f64;
        let scalar_seed = 0.6;
        let input = aggregate
            .into_iter()
            .chain(seed)
            .chain([scalar, scalar_seed])
            .map(f64::to_bits)
            .collect::<Vec<_>>();
        let mut output = vec![0; 8];
        compiled
            .call_cells(
                rumoca_eval_solve::PureCallInvocation::Directional(site.directional().unwrap()),
                &input,
                &mut output,
            )
            .unwrap();
        for i in 0..aggregate.len() {
            let (primal, tangent) = if scalar_on_lhs {
                let primal = scalar.powf(aggregate[i]);
                (
                    primal,
                    primal * (seed[i] * scalar.ln() + aggregate[i] * scalar_seed / scalar),
                )
            } else {
                let primal = aggregate[i].powf(scalar);
                (
                    primal,
                    scalar * aggregate[i].powf(scalar - 1.0) * seed[i]
                        + primal * aggregate[i].ln() * scalar_seed,
                )
            };
            assert!((f64::from_bits(output[i]) - primal).abs() < 1e-12);
            assert!((f64::from_bits(output[4 + i]) - tangent).abs() < 1e-12);
        }
    }
}
