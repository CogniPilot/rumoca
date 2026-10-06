use super::*;

fn tuple_map(
    domain: StructuredIndexDomain,
) -> (solve::SolvePureCallTable, solve::SolvePureCallSite) {
    let p = profile();
    let integer = solve::SolveScalarType::integer(p);
    let rank = domain.binders.len();
    let body_type = solve::SolveValueType::tensor(integer, vec![rank as u32]).unwrap();
    let mut shape = domain
        .extents()
        .unwrap()
        .into_iter()
        .map(|v| v as u32)
        .collect::<Vec<_>>();
    shape.push(rank as u32);
    let output_type = solve::SolveValueType::tensor(integer, shape).unwrap();
    let mut table = solve::SolvePureCallTable::builder(p);
    let owner = table
        .add_owner(
            identity(16020),
            vec![],
            vec![solve::SolvePureCallOutput::result(output_type)],
            span(16020),
            |b, _, outputs| {
                let result = b.map(
                    domain,
                    &[],
                    body_type,
                    span(16021),
                    |r, _, binders, output| {
                        let mut values = Vec::new();
                        for binder in binders {
                            values.push(r.load(*binder, span(16022))?);
                        }
                        let result =
                            r.construct_aggregate(&values, vec![rank as u32], span(16023))?;
                        r.store(output, result, span(16024))
                    },
                )?;
                b.store(outputs[0], result, span(16025))
            },
        )
        .unwrap();
    let site = table.call_site(owner).unwrap();
    (table.finish(), site)
}

#[test]
fn typed_map_array_results_follow_cartesian_order_and_extreme_endpoints() {
    for axes in [
        vec![(1, 2, 1), (9, 1, -3)],
        vec![(i64::MIN, -1, i64::MAX), (1, 2, 1)],
        vec![(1, 2, 1), (i64::MAX - 1, i64::MAX, 1)],
        vec![(2, 0, -1), (-3, 4, 3), (1, 2, 1)],
    ] {
        let domain = cartesian(&axes);
        let expected = domain
            .index_tuple_iter()
            .unwrap()
            .flatten()
            .map(solve::SolveValueKind::Integer)
            .collect::<Vec<_>>();
        let (table, site) = tuple_map(domain);
        let compiled = compile_pure_call_wasm(&table, &site).unwrap();
        let actual = Runner::new(&compiled).run(&[]);
        assert_eq!(actual, (0, cells(expected)));
        assert_eq!(actual.1, oracle(&table, &site, &[]).unwrap());
    }
}

#[test]
fn typed_map_empty_or_malformed_domains_preserve_checked_constructor_refusal() {
    let p = profile();
    let scalar = solve::SolveValueType::scalar(solve::SolveScalarType::real(p));
    for axes in [
        vec![(1, 0, 1)],
        vec![],
        vec![(1, 3, 0)],
        vec![(i64::MIN, i64::MAX, 1)],
    ] {
        let mut table = solve::SolvePureCallTable::builder(p);
        let result = table.add_owner(
            identity(16026),
            vec![],
            vec![solve::SolvePureCallOutput::result(scalar.clone())],
            span(16026),
            |b, _, _| {
                b.map(
                    cartesian(&axes),
                    &[],
                    scalar.clone(),
                    span(16027),
                    |r, _, _, output| {
                        let value = r.constant(solve::SolveValue::real(p, 0.), span(16028))?;
                        r.store(output, value, span(16029))
                    },
                )?;
                Ok(())
            },
        );
        assert_eq!(
            result,
            Err(solve::SolveProgramConstructionError::InvalidMap {
                provenance: span(16027)
            })
        );
    }
}
