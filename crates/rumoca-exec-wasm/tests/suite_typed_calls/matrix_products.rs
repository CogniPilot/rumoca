//! Actual WASM products against the original canonical ordered evaluator.
use super::*;

fn table(
    left: Vec<u32>,
    right: Vec<u32>,
    scalar: solve::SolveScalarType,
) -> (solve::SolvePureCallTable, solve::SolvePureCallSite) {
    let l = solve::SolveValueType::tensor(scalar, left.clone()).unwrap();
    let r = solve::SolveValueType::tensor(scalar, right.clone()).unwrap();
    let result_shape = match (left.as_slice(), right.as_slice()) {
        ([_], [_]) => vec![],
        ([rows, _], [_]) => vec![*rows],
        ([_], [_, columns]) => vec![*columns],
        ([rows, _], [_, columns]) => vec![*rows, *columns],
        _ => unreachable!(),
    };
    let result = if result_shape.is_empty() {
        solve::SolveValueType::scalar(scalar)
    } else {
        solve::SolveValueType::tensor(scalar, result_shape).unwrap()
    };
    let mut builder = solve::SolvePureCallTable::builder(profile());
    let owner = builder
        .add_owner(
            identity(150),
            vec![l.clone(), r],
            vec![
                solve::SolvePureCallOutput::result(result),
                solve::SolvePureCallOutput::result(l),
            ],
            span(15000),
            |b, inputs, outputs| {
                let lhs = b.load(inputs[0], span(15001))?;
                let rhs = b.load(inputs[1], span(15002))?;
                let product = b.matrix_multiply(lhs, rhs, span(15003))?;
                b.store(outputs[0], product, span(15004))?;
                b.store(outputs[1], lhs, span(15005))
            },
        )
        .unwrap();
    let site = builder.call_site(owner).unwrap();
    (builder.finish(), site)
}

#[test]
fn real_matrix_products_all_four_rank_pairs_match_canonical_order() {
    for (left, right) in [
        (vec![3], vec![3]),
        (vec![2, 3], vec![3]),
        (vec![3], vec![3, 2]),
        (vec![2, 3], vec![3, 2]),
        (vec![1], vec![1]),
    ] {
        let count = |shape: &[u32]| shape.iter().map(|n| *n as usize).product::<usize>();
        let a = (0..count(&left))
            .map(|i| real([1e16, 1., -1e16, -0., 2., 3.][i % 6]))
            .collect::<Vec<_>>();
        let b = (0..count(&right))
            .map(|i| real([1., -0., 1., 1., 2., -3.][i % 6]))
            .collect::<Vec<_>>();
        let (table, site) = table(left, right, solve::SolveScalarType::real(profile()));
        let compiled = compile_pure_call_wasm(&table, &site).unwrap();
        let inputs = [a, b];
        let expected = oracle(&table, &site, &inputs).unwrap();
        let (status, actual) = Runner::new(&compiled).run(&cells(inputs.iter().flatten().copied()));
        assert_eq!(status, 0);
        assert_eq!(actual, expected);
        assert!(compiled.module_bytes().len() < 4096);
    }
}

#[test]
fn one_term_product_keeps_negative_zero_and_infinities() {
    let (table, site) = table(vec![1], vec![1], solve::SolveScalarType::real(profile()));
    let compiled = compile_pure_call_wasm(&table, &site).unwrap();
    let mut runner = Runner::new(&compiled);
    for (a, b) in [(-0., 1.), (f64::INFINITY, -2.), (f64::NEG_INFINITY, -2.)] {
        let input = [vec![real(a)], vec![real(b)]];
        let (status, actual) = runner.run(&cells(input.iter().flatten().copied()));
        assert_eq!(status, 0);
        assert_eq!(actual, oracle(&table, &site, &input).unwrap());
    }
}

#[test]
fn integer_matrix_owner_is_refused_by_existing_directional_constructor() {
    let ty =
        solve::SolveValueType::tensor(solve::SolveScalarType::integer(profile()), vec![2]).unwrap();
    let mut builder = solve::SolvePureCallTable::builder(profile());
    let result = builder.add_owner(
        identity(151),
        vec![ty.clone(), ty],
        vec![solve::SolvePureCallOutput::result(
            solve::SolveValueType::scalar(solve::SolveScalarType::integer(profile())),
        )],
        span(15100),
        |b, inputs, outputs| {
            let lhs = b.load(inputs[0], span(15101))?;
            let rhs = b.load(inputs[1], span(15102))?;
            let value = b.matrix_multiply(lhs, rhs, span(15103))?;
            b.store(outputs[0], value, span(15104))
        },
    );
    assert_eq!(
        result.unwrap_err(),
        solve::SolveProgramConstructionError::InvalidTensorAlgebra {
            provenance: span(15103)
        }
    );
}

#[test]
fn zero_extent_tensor_is_not_an_admitted_matrix_operand() {
    assert!(
        solve::SolveValueType::tensor(solve::SolveScalarType::real(profile()), vec![2, 0]).is_err()
    );
}

#[test]
fn matrix_invalid_abi_is_atomic_and_same_instance_recovers() {
    let (table, site) = table(
        vec![2, 2],
        vec![2, 2],
        solve::SolveScalarType::real(profile()),
    );
    let compiled = compile_pure_call_wasm(&table, &site).unwrap();
    let mut runner = Runner::new(&compiled);
    let input = [
        vec![real(1.), real(2.), real(3.), real(4.)],
        vec![real(2.), real(0.), real(0.), real(2.)],
    ];
    runner.run(&cells(input.iter().flatten().copied()));
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
    assert_eq!(fault.provenance, span(15000));
    let mut output = vec![0; runner.output_bytes];
    runner
        .memory
        .read(&runner.store, runner.output, &mut output)
        .unwrap();
    assert_eq!(output, vec![0xa5; runner.output_bytes]);
    let (status, actual) = runner.run(&cells(input.iter().flatten().copied()));
    assert_eq!(status, 0);
    assert_eq!(actual, oracle(&table, &site, &input).unwrap());
}
