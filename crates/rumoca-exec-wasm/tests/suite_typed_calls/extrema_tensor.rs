use super::*;

#[test]
fn real_extrema_ignore_single_nan_and_preserve_admissible_equal_operand_bits() {
    let p = profile();
    let ty = solve::SolveValueType::scalar(solve::SolveScalarType::real(p));
    let mut table = solve::SolvePureCallTable::builder(p);
    let owner = table
        .add_owner(
            identity(92),
            vec![ty.clone(); 2],
            vec![solve::SolvePureCallOutput::result(ty); 2],
            span(530),
            |b, inputs, outputs| {
                let lhs = b.load(inputs[0], span(531))?;
                let rhs = b.load(inputs[1], span(532))?;
                let min = b.binary(solve::SolveBinaryOperator::Min, lhs, rhs, span(533))?;
                let max = b.binary(solve::SolveBinaryOperator::Max, lhs, rhs, span(534))?;
                b.store(outputs[0], min, span(535))?;
                b.store(outputs[1], max, span(536))
            },
        )
        .unwrap();
    let site = table.call_site(owner).unwrap();
    let table = table.finish();
    let compiled = compile_pure_call_wasm(&table, &site).unwrap();
    let mut runner = Runner::new(&compiled);
    let nan = f64::from_bits(0x7ff8_0000_0000_0042);
    let signaling = f64::from_bits(0x7ff0_0000_0000_0013);
    for (lhs, rhs) in [
        (2.0, 3.0),
        (3.0, 2.0),
        (-4.0, -9.0),
        (f64::INFINITY, f64::NEG_INFINITY),
        (nan, 2.0),
        (2.0, nan),
        (signaling, -3.0),
        (-3.0, signaling),
        (-0.0, 0.0),
        (0.0, -0.0),
        (nan, signaling),
    ] {
        let inputs = vec![vec![real(lhs)], vec![real(rhs)]];
        let (status, output) = runner.run(&cells([real(lhs), real(rhs)]));
        assert_eq!(status, 0);
        let canonical = oracle(&table, &site, &inputs).unwrap();
        for ((actual, expected), operator) in output
            .chunks_exact(8)
            .zip(canonical.chunks_exact(8))
            .zip(["min", "max"])
        {
            let actual = f64::from_le_bytes(actual.try_into().unwrap());
            let expected = f64::from_le_bytes(expected.try_into().unwrap());
            if lhs.is_nan() && rhs.is_nan() {
                assert!(actual.is_nan() && expected.is_nan());
            } else if lhs == rhs {
                assert!(
                    actual.to_bits() == lhs.to_bits() || actual.to_bits() == rhs.to_bits(),
                    "{operator} equal tie must retain an input"
                );
                assert!(expected.to_bits() == lhs.to_bits() || expected.to_bits() == rhs.to_bits());
            } else {
                assert_eq!(
                    actual.to_bits(),
                    expected.to_bits(),
                    "{operator} must ignore a single NaN and retain exact finite values"
                );
            }
        }
    }
}

#[test]
fn aggregate_rows_copy_every_cell_in_source_order_without_changing_bits() {
    let p = profile();
    let vector = solve::SolveValueType::tensor(solve::SolveScalarType::real(p), vec![3]).unwrap();
    let matrix =
        solve::SolveValueType::tensor(solve::SolveScalarType::real(p), vec![2, 3]).unwrap();
    let mut table = solve::SolvePureCallTable::builder(p);
    let owner = table
        .add_owner(
            identity(112),
            vec![vector; 2],
            vec![solve::SolvePureCallOutput::result(matrix)],
            span(660),
            |b, inputs, outputs| {
                let first = b.load(inputs[0], span(661))?;
                let second = b.load(inputs[1], span(662))?;
                let matrix = b.construct_aggregate(&[first, second], vec![2, 3], span(663))?;
                b.store(outputs[0], matrix, span(664))
            },
        )
        .unwrap();
    let site = table.call_site(owner).unwrap();
    let table = table.finish();
    let compiled = compile_pure_call_wasm(&table, &site).unwrap();
    let inputs = vec![
        vec![
            real(-0.0),
            real(1.5),
            real(f64::from_bits(0x7ff8_1234_5678_9abc)),
        ],
        vec![real(9.0), real(-8.0), real(f64::MIN_POSITIVE)],
    ];
    let expected = cells(inputs.iter().flatten().copied());
    let (status, actual) = Runner::new(&compiled).run(&expected);
    assert_eq!(status, 0);
    assert_eq!(actual, expected);
    assert_eq!(actual, oracle(&table, &site, &inputs).unwrap());
}

#[test]
fn real_scale_and_identity_retain_row_major_values_signed_zero_and_checked_shapes() {
    assert!(
        matches!(
            solve::SolveValueType::tensor(solve::SolveScalarType::real(profile()), vec![0]),
            Err(solve::SolveTypeConstructionError::ZeroTensorExtent)
        ),
        "zero tensor extent remains a canonical construction refusal"
    );
    for extent in [1, 3] {
        let p = profile();
        let scalar = solve::SolveScalarType::real(p);
        let vector = solve::SolveValueType::tensor(scalar, vec![extent]).unwrap();
        let matrix = solve::SolveValueType::tensor(scalar, vec![extent, extent]).unwrap();
        let mut table = solve::SolvePureCallTable::builder(p);
        let owner = table
            .add_owner(
                identity(93),
                vec![vector.clone(), solve::SolveValueType::scalar(scalar)],
                vec![
                    solve::SolvePureCallOutput::result(vector),
                    solve::SolvePureCallOutput::result(matrix),
                ],
                span(540),
                |b, inputs, outputs| {
                    let vector = b.load(inputs[0], span(541))?;
                    let factor = b.load(inputs[1], span(542))?;
                    let scaled = b.scale(vector, factor, span(543))?;
                    let identity = b.identity(scalar, extent, span(544))?;
                    b.store(outputs[0], scaled, span(545))?;
                    b.store(outputs[1], identity, span(546))
                },
            )
            .unwrap();
        let site = table.call_site(owner).unwrap();
        let table = table.finish();
        let compiled = compile_pure_call_wasm(&table, &site).unwrap();
        let values = [-0.0, 1.5, f64::MIN_POSITIVE];
        let inputs = vec![
            values[..extent as usize]
                .iter()
                .copied()
                .map(real)
                .collect(),
            vec![real(-2.0)],
        ];
        let (status, output) = Runner::new(&compiled).run(&cells(inputs.iter().flatten().copied()));
        assert_eq!(status, 0);
        assert_eq!(output, oracle(&table, &site, &inputs).unwrap());
        let mut expected = values[..extent as usize]
            .iter()
            .map(|v| real(*v * -2.0))
            .collect::<Vec<_>>();
        for index in 0..extent * extent {
            let row = index / extent;
            let column = index % extent;
            expected.push(real(f64::from(u8::from(row == column))));
        }
        assert_eq!(output, cells(expected));
    }
}
