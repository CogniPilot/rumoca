use super::*;

#[test]
fn native_local_stores_preserve_values_without_output_aliasing() {
    let at = rumoca_core::Span::from_offsets(
        rumoca_core::SourceId::from_source_name("native_locals.mo"),
        1,
        2,
    );
    let arithmetic = solve::SolveArithmeticProfile::construct(
        solve::SolveRealFormat::Binary64,
        solve::SolveIntegerDomain::construct(i64::MIN, i64::MAX).unwrap(),
    );
    let real = solve::SolveValueType::scalar(solve::SolveScalarType::real(arithmetic));
    let table = solve::SolvePureCallTable::construct(arithmetic, |table| {
        table.add_owner(
            solve::SolvePureCallIdentity::issued(std::num::NonZeroU64::new(1).unwrap()),
            vec![real.clone()],
            vec![solve::SolvePureCallOutput::result(real.clone())],
            at,
            |builder, inputs, outputs| {
                let local = builder.declare_slot(
                    real,
                    solve::SolveStorageClass::MethodLocal,
                    solve::SolveSlotAccess::ReadWrite,
                    at,
                )?;
                let x = builder.load(inputs[0], at)?;
                let square = builder.binary(solve::SolveBinaryOperator::Multiply, x, x, at)?;
                builder.store(local, square, at)?;
                let saved = builder.load(local, at)?;
                let one = builder.constant(solve::SolveValue::real(arithmetic, 1.0), at)?;
                let shifted = builder.binary(solve::SolveBinaryOperator::Add, saved, one, at)?;
                builder.store(local, shifted, at)?;
                let result = builder.load(local, at)?;
                builder.store(outputs[0], result, at)
            },
        )?;
        Ok(())
    })
    .unwrap();
    let compiled = CompiledPureCallTable::compile(&table).unwrap();
    let site = table.owners()[0].call_site();
    for x in [2.0_f64, -3.0, 0.0] {
        let input = [x.to_bits()];
        let mut output = [0_u64];
        compiled.call_cells(&site, &input, &mut output).unwrap();
        assert_eq!(f64::from_bits(output[0]), x * x + 1.0);
        assert_eq!(input, [x.to_bits()]);
    }
}
