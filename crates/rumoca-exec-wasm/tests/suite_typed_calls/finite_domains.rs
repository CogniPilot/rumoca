use super::*;

fn cartesian(axes: &[(i64, i64, i64)]) -> StructuredIndexDomain {
    StructuredIndexDomain {
        binders: axes
            .iter()
            .enumerate()
            .map(|(id, (lower, upper, step))| StructuredIndexBinder {
                id,
                display_name: format!("i{id}"),
                lower: *lower,
                upper: *upper,
                step: *step,
            })
            .collect(),
    }
}

#[test]
fn zero_rank_fold_retains_constructor_refusal_and_exact_source_span() {
    let p = profile();
    let integer = solve::SolveValueType::scalar(solve::SolveScalarType::integer(p));
    let mut table = solve::SolvePureCallTable::builder(p);
    let result = table.add_owner(
        identity(121),
        vec![],
        vec![solve::SolvePureCallOutput::result(integer)],
        span(680),
        |b, _, outputs| {
            let initial = b.constant(solve::SolveValue::integer(p, 0).unwrap(), span(681))?;
            let result = b.fold(
                cartesian(&[]),
                &[initial],
                &[],
                span(682),
                |r, carried, _, _, outputs| {
                    let value = r.load(carried[0], span(683))?;
                    r.store(outputs[0], value, span(684))
                },
            )?;
            b.store(outputs[0], result[0], span(685))
        },
    );
    assert_eq!(
        result,
        Err(solve::SolveProgramConstructionError::InvalidFold {
            provenance: span(682)
        })
    );
}

fn tuple_fold(
    domain: StructuredIndexDomain,
) -> (solve::SolvePureCallTable, solve::SolvePureCallSite) {
    let p = profile();
    let integer = solve::SolveValueType::scalar(solve::SolveScalarType::integer(p));
    let mut table = solve::SolvePureCallTable::builder(p);
    let rank = domain.binders.len();
    let owner = table
        .add_owner(
            identity(90),
            vec![],
            vec![solve::SolvePureCallOutput::result(integer); rank],
            span(500),
            |b, _, outputs| {
                let zero = b.constant(solve::SolveValue::integer(p, 0).unwrap(), span(501))?;
                let initial = vec![zero; rank];
                let result = b.fold(
                    domain,
                    &initial,
                    &[],
                    span(502),
                    |r, _, _, binders, outputs| {
                        for (binder, output) in binders.iter().zip(outputs) {
                            let value = r.load(*binder, span(503))?;
                            r.store(*output, value, span(504))?;
                        }
                        Ok(())
                    },
                )?;
                for (result, output) in result.iter().zip(outputs) {
                    b.store(*output, *result, span(505))?;
                }
                Ok(())
            },
        )
        .unwrap();
    let site = table.call_site(owner).unwrap();
    (table.finish(), site)
}

#[test]
fn finite_domains_preserve_empty_negative_nonattained_and_extreme_endpoints() {
    for (axes, expected) in [
        (vec![(1, 0, 1), (3, 1, -1)], vec![0, 0]),
        (vec![(2, 0, -1), (5, -4, -4)], vec![0, -3]),
        (
            vec![(1, 2, 1), (i64::MAX - 1, i64::MAX, 1)],
            vec![2, i64::MAX],
        ),
        (
            vec![(i64::MIN + 1, i64::MIN, -1), (1, 2, 1)],
            vec![i64::MIN, 2],
        ),
        (
            vec![(1, 2, 1), (-i64::MAX, i64::MAX, i64::MAX), (9, 1, -3)],
            vec![2, i64::MAX, 3],
        ),
        (vec![(i64::MIN, -1, i64::MAX), (1, 2, 1)], vec![-1, 2]),
    ] {
        let (table, site) = tuple_fold(cartesian(&axes));
        let compiled = compile_pure_call_wasm(&table, &site).unwrap();
        let (status, output) = Runner::new(&compiled).run(&[]);
        assert_eq!(status, 0);
        assert_eq!(
            output,
            cells(expected.into_iter().map(solve::SolveValueKind::Integer))
        );
        assert_eq!(output, oracle(&table, &site, &[]).unwrap());
    }
}

#[test]
fn finite_two_binder_fold_preserves_order_simultaneous_carries_and_recovery() {
    let p = profile();
    let integer = solve::SolveValueType::scalar(solve::SolveScalarType::integer(p));
    let mut table = solve::SolvePureCallTable::builder(p);
    let owner = table
        .add_owner(
            identity(91),
            vec![integer.clone()],
            vec![solve::SolvePureCallOutput::result(integer); 2],
            span(510),
            |b, inputs, outputs| {
                let initial = b.load(inputs[0], span(511))?;
                let result = b.fold(
                    cartesian(&[(1, 2, 1), (1, 3, 1)]),
                    &[initial, initial],
                    &[],
                    span(512),
                    |r, carried, _, binders, outputs| {
                        let old = r.load(carried[0], span(513))?;
                        let hundred =
                            r.constant(solve::SolveValue::integer(p, 100).unwrap(), span(514))?;
                        let ten =
                            r.constant(solve::SolveValue::integer(p, 10).unwrap(), span(515))?;
                        let first = r.load(binders[0], span(516))?;
                        let second = r.load(binders[1], span(517))?;
                        let prefix = r.binary(
                            solve::SolveBinaryOperator::Multiply,
                            old,
                            hundred,
                            span(518),
                        )?;
                        let first =
                            r.binary(solve::SolveBinaryOperator::Multiply, first, ten, span(519))?;
                        let coordinate =
                            r.binary(solve::SolveBinaryOperator::Add, first, second, span(520))?;
                        let next = r.binary(
                            solve::SolveBinaryOperator::Add,
                            prefix,
                            coordinate,
                            span(521),
                        )?;
                        r.store(outputs[0], next, span(522))?;
                        r.store(outputs[1], old, span(523))
                    },
                )?;
                b.store(outputs[0], result[0], span(524))?;
                b.store(outputs[1], result[1], span(525))
            },
        )
        .unwrap();
    let site = table.call_site(owner).unwrap();
    let table = table.finish();
    let compiled = compile_pure_call_wasm(&table, &site).unwrap();
    let mut runner = Runner::new(&compiled);
    let mut expected = 0;
    let mut previous = 0;
    for point in [11, 12, 13, 21, 22, 23] {
        previous = expected;
        expected = 100 * expected + point;
    }
    let inputs = vec![vec![solve::SolveValueKind::Integer(0)]];
    let (status, output) = runner.run(&cells([solve::SolveValueKind::Integer(0)]));
    assert_eq!(status, 0);
    assert_eq!(
        output,
        cells([expected, previous].map(solve::SolveValueKind::Integer))
    );
    assert_eq!(output, oracle(&table, &site, &inputs).unwrap());
    let (status, output) = runner.run(&cells([solve::SolveValueKind::Integer(10_000_000)]));
    assert_ne!(status, 0);
    assert_eq!(output, vec![0xa5; 16]);
    let failure = oracle(
        &table,
        &site,
        &[vec![solve::SolveValueKind::Integer(10_000_000)]],
    )
    .unwrap_err();
    let fault = compiled
        .faults()
        .iter()
        .find(|fault| fault.status == status as u32)
        .unwrap();
    assert_eq!(fault.provenance, span(518));
    assert_eq!(failure.source_span(), Some(fault.provenance));
    let (status, output) = runner.run(&cells([solve::SolveValueKind::Integer(0)]));
    assert_eq!(status, 0);
    assert_eq!(output, oracle(&table, &site, &inputs).unwrap());
}
