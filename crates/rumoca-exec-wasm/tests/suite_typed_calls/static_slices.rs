//! Static rank-preserving slices use original checked geometry and exact bits.
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
    let input = solve::SolveValueType::tensor(scalar, shape).unwrap();
    let output = solve::SolveValueType::tensor(scalar, dimensions.clone()).unwrap();
    let mut builder = solve::SolvePureCallTable::builder(profile());
    let owner = builder.add_owner(
        identity(160),
        vec![input.clone()],
        vec![
            solve::SolvePureCallOutput::result(output),
            solve::SolvePureCallOutput::result(input),
        ],
        span(16000),
        |b, inputs, outputs| {
            let original = b.load(inputs[0], span(16001))?;
            let result = b.project_slice(original, origin, dimensions, span(16002))?;
            b.store(outputs[0], result, span(16003))?;
            b.store(outputs[1], original, span(16004))
        },
    )?;
    let site = builder.call_site(owner).unwrap();
    Ok((builder.finish(), site))
}

fn literal(
    source: &[solve::SolveValueKind],
    shape: &[u32],
    origin: &[u32],
    extents: &[u32],
) -> Vec<solve::SolveValueKind> {
    let count = extents.iter().map(|n| *n as usize).product::<usize>();
    (0..count)
        .map(|ordinal| {
            let mut remainder = ordinal;
            let mut offset = 0;
            let mut stride = 1;
            for axis in (0..shape.len()).rev() {
                offset += (origin[axis] as usize + remainder % extents[axis] as usize) * stride;
                remainder /= extents[axis] as usize;
                stride *= shape[axis] as usize;
            }
            source[offset]
        })
        .collect()
}

#[test]
fn slice_nonzero_multiaxis_origins_real_bits_and_original_are_exact() {
    for (shape, origin, dimensions) in [
        (vec![3], vec![0], vec![3]),
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
        let input = (0..shape.iter().map(|n| *n as usize).product())
            .map(|i| real(bits[i % bits.len()]))
            .collect::<Vec<_>>();
        let mut expected = literal(&input, &shape, &origin, &dimensions);
        expected.extend_from_slice(&input);
        let (table, site) = table(
            solve::SolveScalarType::real(profile()),
            shape,
            origin,
            dimensions,
        )
        .unwrap();
        let compiled = compile_pure_call_wasm(&table, &site).unwrap();
        let mut runner = Runner::new(&compiled);
        let bytes = cells(input.iter().copied());
        let (status, actual) = runner.run(&bytes);
        assert_eq!(status, 0);
        assert_eq!(actual, cells(expected));
        assert_eq!(actual, oracle(&table, &site, &[input]).unwrap());
        let mut retained = vec![0; bytes.len()];
        runner.memory.read(&runner.store, 0, &mut retained).unwrap();
        assert_eq!(retained, bytes);
        assert!(compiled.module_bytes().len() < 4096);
    }
}

#[test]
fn slice_integer_boolean_payloads_preserved() {
    for (scalar, input) in [
        (
            solve::SolveScalarType::integer(profile()),
            vec![i64::MIN, -1, 0, 1, i64::MAX, 42]
                .into_iter()
                .map(solve::SolveValueKind::Integer)
                .collect::<Vec<_>>(),
        ),
        (
            solve::SolveScalarType::Boolean,
            vec![true, false, true, false, true, false]
                .into_iter()
                .map(solve::SolveValueKind::Boolean)
                .collect(),
        ),
    ] {
        let (table, site) = table(scalar, vec![2, 3], vec![1, 1], vec![1, 2]).unwrap();
        let compiled = compile_pure_call_wasm(&table, &site).unwrap();
        let (status, actual) = Runner::new(&compiled).run(&cells(input.iter().copied()));
        assert_eq!(status, 0);
        assert_eq!(actual, oracle(&table, &site, &[input]).unwrap());
    }
}

#[test]
fn slice_constructor_rejects_rank_bounds_and_overflow_at_original_span() {
    for (origin, extents) in [
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
                extents
            )
            .unwrap_err(),
            solve::SolveProgramConstructionError::InvalidProjection {
                provenance: span(16002)
            }
        );
    }
}

#[test]
fn slice_invalid_abi_is_atomic_and_same_instance_recovers() {
    let (table, site) = table(
        solve::SolveScalarType::real(profile()),
        vec![2, 3],
        vec![1, 1],
        vec![1, 2],
    )
    .unwrap();
    let compiled = compile_pure_call_wasm(&table, &site).unwrap();
    let mut runner = Runner::new(&compiled);
    let input = (0..6).map(|i| real(i as f64)).collect::<Vec<_>>();
    let bytes = cells(input.iter().copied());
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
        .find(|fault| fault.status == status as u32)
        .unwrap();
    assert_eq!(fault.kind, TypedCallFaultKind::InvalidBuffer);
    assert_eq!(fault.provenance, span(16000));
    let mut output = vec![0; runner.output_bytes];
    runner
        .memory
        .read(&runner.store, runner.output, &mut output)
        .unwrap();
    assert_eq!(output, vec![0xa5; runner.output_bytes]);
    let (status, actual) = runner.run(&bytes);
    assert_eq!(status, 0);
    assert_eq!(actual, oracle(&table, &site, &[input]).unwrap());
}
