//! Original static slice geometry, private copies, and bitwise update semantics.
use super::*;

fn table(
    scalar: solve::SolveScalarType,
    shape: Vec<u32>,
    origin: Vec<u32>,
    dimensions: Vec<u32>,
) -> Result<
    (solve::SolvePureCallTable, solve::SolvePureCallSite),
    solve::SolveProgramConstructionError,
> {
    let aggregate = solve::SolveValueType::tensor(scalar, shape).unwrap();
    let value = solve::SolveValueType::tensor(scalar, dimensions).unwrap();
    let mut builder = solve::SolvePureCallTable::builder(profile());
    let owner = builder.add_owner(
        identity(180),
        vec![aggregate.clone(), value.clone()],
        vec![
            solve::SolvePureCallOutput::result(aggregate.clone()),
            solve::SolvePureCallOutput::result(aggregate),
            solve::SolvePureCallOutput::result(value),
        ],
        span(18000),
        |b, inputs, outputs| {
            let original = b.load(inputs[0], span(18001))?;
            let value = b.load(inputs[1], span(18002))?;
            let updated = b.update_slice(original, value, origin, span(18003))?;
            b.store(outputs[0], updated, span(18004))?;
            b.store(outputs[1], original, span(18005))?;
            b.store(outputs[2], value, span(18006))
        },
    )?;
    let site = builder.call_site(owner).unwrap();
    Ok((builder.finish(), site))
}

fn literal(
    original: &[solve::SolveValueKind],
    value: &[solve::SolveValueKind],
    shape: &[u32],
    origin: &[u32],
    dimensions: &[u32],
) -> Vec<solve::SolveValueKind> {
    let mut output = original.to_vec();
    for (ordinal, element) in value.iter().enumerate() {
        let mut remainder = ordinal;
        let mut offset = 0;
        let mut stride = 1;
        for axis in (0..shape.len()).rev() {
            offset += (origin[axis] as usize + remainder % dimensions[axis] as usize) * stride;
            remainder /= dimensions[axis] as usize;
            stride *= shape[axis] as usize;
        }
        output[offset] = *element;
    }
    output
}

#[test]
fn slice_update_multiaxis_and_full14400_preserve_real_bits_and_inputs() {
    for (shape, origin, dimensions) in [
        (vec![3], vec![1], vec![2]),
        (vec![4, 5], vec![1, 2], vec![2, 3]),
        (vec![3, 4, 5], vec![1, 1, 2], vec![2, 3, 2]),
        (vec![2, 3, 4, 5], vec![1, 1, 1, 2], vec![1, 2, 3, 3]),
        (vec![120, 120], vec![0, 0], vec![120, 120]),
    ] {
        let bits = [
            0.,
            -0.,
            f64::from_bits(0x7ff8_1234_5678_abcd),
            f64::INFINITY,
            f64::NEG_INFINITY,
            1.25,
        ];
        let aggregate = (0..shape.iter().map(|n| *n as usize).product())
            .map(|i| real(bits[i % 6]))
            .collect::<Vec<_>>();
        let value = (0..dimensions.iter().map(|n| *n as usize).product())
            .map(|i| real(bits[(i + 3) % 6]))
            .collect::<Vec<_>>();
        let mut expected = literal(&aggregate, &value, &shape, &origin, &dimensions);
        expected.extend_from_slice(&aggregate);
        expected.extend_from_slice(&value);
        let (table, site) = table(
            solve::SolveScalarType::real(profile()),
            shape,
            origin,
            dimensions,
        )
        .unwrap();
        let compiled = compile_pure_call_wasm(&table, &site).unwrap();
        let input = [aggregate, value];
        let bytes = cells(input.iter().flatten().copied());
        let mut runner = Runner::new(&compiled);
        let (status, actual) = runner.run(&bytes);
        assert_eq!(status, 0);
        assert_eq!(actual, cells(expected));
        assert_eq!(actual, oracle(&table, &site, &input).unwrap());
        let mut retained = vec![0; bytes.len()];
        runner.memory.read(&runner.store, 0, &mut retained).unwrap();
        assert_eq!(retained, bytes);
        assert!(compiled.module_bytes().len() < 4096);
    }
}

#[test]
fn slice_update_integer_boolean_bits_match_canonical() {
    for (scalar, aggregate, value) in [
        (
            solve::SolveScalarType::integer(profile()),
            vec![i64::MIN, -1, 0, 1, i64::MAX, 42]
                .into_iter()
                .map(solve::SolveValueKind::Integer)
                .collect::<Vec<_>>(),
            vec![i64::MAX, i64::MIN]
                .into_iter()
                .map(solve::SolveValueKind::Integer)
                .collect::<Vec<_>>(),
        ),
        (
            solve::SolveScalarType::Boolean,
            vec![false, true, false, true, false, true]
                .into_iter()
                .map(solve::SolveValueKind::Boolean)
                .collect(),
            vec![true, false]
                .into_iter()
                .map(solve::SolveValueKind::Boolean)
                .collect(),
        ),
    ] {
        let (table, site) = table(scalar, vec![2, 3], vec![1, 1], vec![1, 2]).unwrap();
        let compiled = compile_pure_call_wasm(&table, &site).unwrap();
        let input = [aggregate, value];
        let (status, actual) = Runner::new(&compiled).run(&cells(input.iter().flatten().copied()));
        assert_eq!(status, 0);
        assert_eq!(actual, oracle(&table, &site, &input).unwrap());
    }
}

#[test]
fn slice_update_rank_bounds_overflow_refuse_at_original_constructor_span() {
    for (origin, dimensions) in [
        (vec![0], vec![2, 2]),
        (vec![0, 0], vec![2]),
        (vec![1, 2], vec![2, 2]),
        (vec![u32::MAX, 0], vec![1, 1]),
    ] {
        assert_eq!(
            table(
                solve::SolveScalarType::real(profile()),
                vec![2, 3],
                origin,
                dimensions
            )
            .unwrap_err(),
            solve::SolveProgramConstructionError::InvalidProjection {
                provenance: span(18003)
            }
        );
    }
}

#[test]
fn slice_update_invalid_abi_is_atomic_and_recovers() {
    let (table, site) = table(
        solve::SolveScalarType::real(profile()),
        vec![2, 3],
        vec![1, 1],
        vec![1, 2],
    )
    .unwrap();
    let compiled = compile_pure_call_wasm(&table, &site).unwrap();
    let mut runner = Runner::new(&compiled);
    let input = [
        (0..6).map(|i| real(i as f64)).collect::<Vec<_>>(),
        vec![real(-0.), real(42.)],
    ];
    let bytes = cells(input.iter().flatten().copied());
    runner.run(&bytes);
    runner
        .memory
        .write(
            &mut runner.store,
            runner.output,
            &vec![0xa5; runner.output_bytes],
        )
        .unwrap();
    let status = runner
        .call
        .call(
            &mut runner.store,
            (1, runner.output as i32, runner.scratch as i32),
        )
        .unwrap();
    let fault = compiled
        .faults()
        .iter()
        .find(|f| f.status == status as u32)
        .unwrap();
    assert_eq!(fault.kind, TypedCallFaultKind::InvalidBuffer);
    assert_eq!(fault.provenance, span(18000));
    let mut output = vec![0; runner.output_bytes];
    runner
        .memory
        .read(&runner.store, runner.output, &mut output)
        .unwrap();
    assert_eq!(output, vec![0xa5; runner.output_bytes]);
    let (status, actual) = runner.run(&bytes);
    assert_eq!(status, 0);
    assert_eq!(actual, oracle(&table, &site, &input).unwrap());
}
