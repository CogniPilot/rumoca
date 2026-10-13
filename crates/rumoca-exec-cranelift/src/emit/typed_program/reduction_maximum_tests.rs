use super::*;

fn maximum_table(
    format: solve::SolveRealFormat,
    dimensions: Vec<u32>,
    parameter: bool,
) -> solve::SolvePureCallTable {
    let arithmetic =
        solve::SolveArithmeticProfile::construct(format, solve::SolveIntegerDomain::FULL);
    let scalar = solve::SolveValueType::scalar(solve::SolveScalarType::real(arithmetic));
    let tensor = solve::SolveValueType::tensor(scalar.element_type(), dimensions).unwrap();
    let mut inputs = vec![tensor];
    if parameter {
        inputs.push(scalar.clone());
    }
    let at = rumoca_core::Span::from_offsets(
        rumoca_core::SourceId::from_source_name("native_maximum.mo"),
        1,
        2,
    );
    solve::SolvePureCallTable::construct(arithmetic, |table| {
        table.add_owner(
            solve::SolvePureCallIdentity::issued(std::num::NonZeroU64::new(905).unwrap()),
            inputs,
            vec![solve::SolvePureCallOutput::result(scalar)],
            at,
            |builder, inputs, outputs| {
                let input = builder.load(inputs[0], at)?;
                let mut result =
                    builder.reduce(solve::SolveReductionOperator::Maximum, input, at)?;
                if parameter {
                    let offset = builder.load(inputs[1], at)?;
                    result = builder.binary(solve::SolveBinaryOperator::Add, result, offset, at)?;
                }
                builder.store(outputs[0], result, at)
            },
        )?;
        Ok(())
    })
    .unwrap()
}

#[test]
fn native_maximum_retains_ordered_kink_policy_and_parameter_seed() {
    let table = maximum_table(solve::SolveRealFormat::Binary64, vec![2, 2], false);
    let compiled = CompiledPureCallTable::compile(&table).unwrap();
    for (x, tangent) in [
        ([3.0_f64, 3.0, 1.0, 0.0], 11.0_f64),
        ([2.0, f64::NAN, 1.0, 0.0], 22.0),
        ([f64::NAN, 2.0, 1.0, 0.0], 22.0),
        ([2.0, 1.0, 0.0, f64::NAN], 44.0),
        ([-0.0, 0.0, -0.0, 0.0], 11.0),
        ([f64::INFINITY, f64::INFINITY, f64::NEG_INFINITY, 0.0], 11.0),
    ] {
        let input = x
            .into_iter()
            .chain([11.0, 22.0, 33.0, 44.0])
            .map(f64::to_bits)
            .collect::<Vec<_>>();
        let mut output = [0; 2];
        compiled
            .call_cells(
                rumoca_eval_solve::PureCallInvocation::Directional(
                    table.owners()[0].call_site().directional().unwrap(),
                ),
                &input,
                &mut output,
            )
            .unwrap();
        assert_eq!(output[1], tangent.to_bits());
        assert_eq!(
            f64::from_bits(output[0]),
            x.into_iter().reduce(f64::max).unwrap()
        );
    }
    let table = maximum_table(solve::SolveRealFormat::Binary64, vec![2, 2], true);
    let compiled = CompiledPureCallTable::compile(&table).unwrap();
    let input = [-3.0_f64, 2.0, 7.0, 1.0, 0.3, -0.2, 0.4, 0.7, 5.0, 0.6].map(f64::to_bits);
    let mut output = [0; 2];
    compiled
        .call_cells(
            rumoca_eval_solve::PureCallInvocation::Directional(
                table.owners()[0].call_site().directional().unwrap(),
            ),
            &input,
            &mut output,
        )
        .unwrap();
    assert_eq!(output, [12.0_f64.to_bits(), 1.0_f64.to_bits()]);
}

#[test]
fn native_binary32_maximum_preserves_nan_order_and_zero_seed_bits() {
    let table = maximum_table(solve::SolveRealFormat::Binary32, vec![3], false);
    let compiled = CompiledPureCallTable::compile(&table).unwrap();
    for (x, seed, primal, tangent) in [
        (
            [2.0_f32, f32::NAN, 1.0],
            [11.0, 22.0, 33.0],
            2.0_f32,
            22.0_f32,
        ),
        ([-0.0, 0.0, -0.0], [-0.0, 22.0, 33.0], 0.0, -0.0),
    ] {
        let input = x
            .into_iter()
            .chain(seed)
            .map(|v| u64::from(v.to_bits()))
            .collect::<Vec<_>>();
        let mut output = [0; 2];
        compiled
            .call_cells(
                rumoca_eval_solve::PureCallInvocation::Directional(
                    table.owners()[0].call_site().directional().unwrap(),
                ),
                &input,
                &mut output,
            )
            .unwrap();
        assert_eq!(f32::from_bits(output[0] as u32), primal);
        assert_eq!(output[1], u64::from(tangent.to_bits()));
    }
    let table = maximum_table(solve::SolveRealFormat::Binary32, vec![1], false);
    let compiled = CompiledPureCallTable::compile(&table).unwrap();
    for x in [-0.0_f32, f32::from_bits(0x7fc0_1234)] {
        let mut output = [0; 2];
        compiled
            .call_cells(
                rumoca_eval_solve::PureCallInvocation::Directional(
                    table.owners()[0].call_site().directional().unwrap(),
                ),
                &[u64::from(x.to_bits()), u64::from((-0.0_f32).to_bits())],
                &mut output,
            )
            .unwrap();
        assert_eq!(output[0], u64::from(x.to_bits()));
        assert_eq!(output[1], u64::from((-0.0_f32).to_bits()));
    }
}
