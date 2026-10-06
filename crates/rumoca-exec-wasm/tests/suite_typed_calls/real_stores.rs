//! Real numeric stores retain raw cells and success-only tuple publication.
use super::*;

fn mixed_numeric_call() -> (solve::SolvePureCallTable, solve::SolvePureCallSite) {
    let p = profile();
    let real_type = solve::SolveValueType::scalar(solve::SolveScalarType::real(p));
    let integer = solve::SolveValueType::scalar(solve::SolveScalarType::integer(p));
    let boolean = solve::SolveValueType::scalar(solve::SolveScalarType::Boolean);
    let mut table = solve::SolvePureCallTable::builder(p);
    let owner = table
        .add_owner(
            identity(9010),
            vec![real_type.clone(), integer.clone()],
            [
                real_type.clone(),
                real_type.clone(),
                real_type,
                boolean,
                integer,
            ]
            .into_iter()
            .map(solve::SolvePureCallOutput::result)
            .collect(),
            span(9010),
            |b, inputs, outputs| {
                let value = b.load(inputs[0], span(9011))?;
                let integer = b.load(inputs[1], span(9012))?;
                let negative = b.unary(solve::SolveUnaryOperator::Negate, value, span(9013))?;
                let absolute = b.unary(solve::SolveUnaryOperator::Abs, value, span(9014))?;
                let converted = b.convert(
                    solve::SolveConversionOperator::IntegerToReal,
                    integer,
                    span(9015),
                )?;
                let equal =
                    b.compare(solve::SolveCompareOperator::Equal, value, value, span(9016))?;
                b.store(outputs[0], negative, span(9017))?;
                b.store(outputs[1], absolute, span(9018))?;
                b.store(outputs[2], converted, span(9019))?;
                b.store(outputs[3], equal, span(9020))?;
                let one = b.constant(solve::SolveValue::integer(p, 1).unwrap(), span(9021))?;
                let incremented =
                    b.binary(solve::SolveBinaryOperator::Add, integer, one, span(9022))?;
                b.store(outputs[4], incremented, span(9023))
            },
        )
        .unwrap();
    let site = table.call_site(owner).unwrap();
    (table.finish(), site)
}

#[test]
fn real_numeric_stores_preserve_ieee_payloads_integer_conversions_and_boolean_results() {
    let (table, site) = mixed_numeric_call();
    let compiled = compile_pure_call_wasm(&table, &site).unwrap();
    let mut runner = Runner::new(&compiled);
    let patterns: [u64; 14] = [
        0,
        0x8000_0000_0000_0000,
        1,
        0x8000_0000_0000_0001,
        1.0f64.to_bits(),
        (-1.0f64).to_bits(),
        f64::MAX.to_bits(),
        (-f64::MAX).to_bits(),
        f64::INFINITY.to_bits(),
        f64::NEG_INFINITY.to_bits(),
        0x7ff8_dead_beef_1234,
        0xfff8_dead_beef_1234,
        0x7ff0_0000_0000_0001,
        0xfff0_0000_0000_0042,
    ];
    for bits in patterns {
        for integer in [i64::MIN, i64::MAX - 1, -1, 0, (1i64 << 53) + 1, 7] {
            let inputs = cells([
                real(f64::from_bits(bits)),
                solve::SolveValueKind::Integer(integer),
            ]);
            let (status, output) = runner.run(&inputs);
            assert_eq!(status, 0);
            let expected = cells([
                real(f64::from_bits(bits ^ (1 << 63))),
                real(f64::from_bits(bits & !(1 << 63))),
                real(integer as f64),
                solve::SolveValueKind::Boolean(!f64::from_bits(bits).is_nan()),
                solve::SolveValueKind::Integer(integer + 1),
            ]);
            assert_eq!(output, expected);
            assert_eq!(&runner.memory.data(&runner.store)[..inputs.len()], inputs);
        }
    }
}

#[test]
fn later_integer_fault_keeps_earlier_real_stores_private_and_next_call_recovers() {
    let (table, site) = mixed_numeric_call();
    let compiled = compile_pure_call_wasm(&table, &site).unwrap();
    let mut runner = Runner::new(&compiled);
    let inputs = cells([real(-0.0), solve::SolveValueKind::Integer(i64::MAX)]);
    let (status, output) = runner.run(&inputs);
    let fault = compiled
        .faults()
        .iter()
        .find(|fault| fault.status == status as u32)
        .unwrap();
    assert_eq!(fault.kind, TypedCallFaultKind::IntegerArithmetic);
    assert_eq!(fault.provenance, span(9022));
    assert_eq!(output, vec![0xa5; compiled.layout().output_bytes as usize]);
    assert_eq!(&runner.memory.data(&runner.store)[..inputs.len()], inputs);
    assert_eq!(
        runner
            .run(&cells([real(-0.0), solve::SolveValueKind::Integer(7)]))
            .0,
        0
    );
}
