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
        compiled
            .call_cells(
                rumoca_eval_solve::PureCallInvocation::Primal(&site),
                &input,
                &mut output,
            )
            .unwrap();
        assert_eq!(f64::from_bits(output[0]), x * x + 1.0);
        assert_eq!(input, [x.to_bits()]);
    }
}

#[test]
fn full_width_typed_integer_tapes_preserve_exact_cells_on_default_stack() {
    let at = rumoca_core::Span::from_offsets(
        rumoca_core::SourceId::from_source_name("native_heap_integer.mo"),
        1,
        2,
    );
    let arithmetic = solve::SolveArithmeticProfile::construct(
        solve::SolveRealFormat::Binary64,
        solve::SolveIntegerDomain::FULL,
    );
    let tensor =
        solve::SolveValueType::tensor(solve::SolveScalarType::integer(arithmetic), vec![512, 512])
            .unwrap();
    let table = solve::SolvePureCallTable::construct(arithmetic, |table| {
        table.add_owner(
            solve::SolvePureCallIdentity::issued(std::num::NonZeroU64::new(1001).unwrap()),
            vec![tensor.clone()],
            vec![solve::SolvePureCallOutput::result(tensor)],
            at,
            |builder, inputs, outputs| {
                let value = builder.load(inputs[0], at)?;
                let negated = builder.unary(solve::SolveUnaryOperator::Negate, value, at)?;
                builder.store(outputs[0], negated, at)
            },
        )?;
        Ok(())
    })
    .unwrap();
    let compiled = CompiledPureCallTable::compile(&table).unwrap();
    let site = table.owners()[0].call_site();
    let mut output = vec![0; 512 * 512];
    for offset in [0_i64, 17] {
        let input = (0..output.len())
            .map(|index| ([9_007_199_254_740_993_i64, -7, 0][index % 3] + offset) as u64)
            .collect::<Vec<_>>();
        compiled
            .call_cells(
                rumoca_eval_solve::PureCallInvocation::Primal(&site),
                &input,
                &mut output,
            )
            .unwrap();
        assert!(
            output
                .iter()
                .zip(&input)
                .all(|(&actual, &input)| actual == (-(input as i64)) as u64)
        );
        // A repeat reaches the existing complete-coordinate cache-hit return.
        output.fill(0);
        compiled
            .call_cells(
                rumoca_eval_solve::PureCallInvocation::Primal(&site),
                &input,
                &mut output,
            )
            .unwrap();
        assert!(
            output
                .iter()
                .zip(&input)
                .all(|(&actual, &input)| actual == (-(input as i64)) as u64)
        );
    }
    // Checked Integer failure must return through every acquired native frame.
    let mut input = vec![0; output.len()];
    input[0] = i64::MIN as u64;
    let error = compiled
        .call_cells(
            rumoca_eval_solve::PureCallInvocation::Primal(&site),
            &input,
            &mut output,
        )
        .unwrap_err();
    assert!(
        error
            .to_string()
            .contains("Integer arithmetic left the checked target domain"),
        "{error}"
    );
}
