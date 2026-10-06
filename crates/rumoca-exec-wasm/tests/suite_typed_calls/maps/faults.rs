use super::*;

pub(crate) fn table(count: i64) -> (solve::SolvePureCallTable, solve::SolvePureCallSite) {
    let p = profile();
    let real_type = solve::SolveValueType::scalar(solve::SolveScalarType::real(p));
    let integer = solve::SolveValueType::scalar(solve::SolveScalarType::integer(p));
    let boolean = solve::SolveValueType::scalar(solve::SolveScalarType::Boolean);
    let raster = solve::SolveValueType::tensor(real_type.element_type(), vec![4]).unwrap();
    let result =
        solve::SolveValueType::tensor(real_type.element_type(), vec![count as u32]).unwrap();
    let mut table = solve::SolvePureCallTable::builder(p);
    let owner = table
        .add_owner(
            identity(16040),
            vec![raster, integer, boolean],
            vec![
                solve::SolvePureCallOutput::result(real_type.clone()),
                solve::SolvePureCallOutput::result(result),
            ],
            span(16040),
            |b, inputs, outputs| {
                let prefix = b.constant(solve::SolveValue::real(p, 42.), span(16041))?;
                b.store(outputs[0], prefix, span(16042))?;
                let raster = b.load(inputs[0], span(16043))?;
                let target = b.load(inputs[1], span(16044))?;
                let enabled = b.load(inputs[2], span(16045))?;
                let result = b.map(
                    cartesian(&[(1, count, 1)]),
                    &[raster, target, enabled],
                    real_type.clone(),
                    span(16046),
                    |r, captures, binders, output| {
                        let raster = r.load(captures[0], span(16047))?;
                        let target = r.load(captures[1], span(16048))?;
                        let enabled = r.load(captures[2], span(16049))?;
                        let index = r.load(binders[0], span(16050))?;
                        let last =
                            r.constant(solve::SolveValue::integer(p, count).unwrap(), span(16051))?;
                        let at_end = r.compare(
                            solve::SolveCompareOperator::Equal,
                            index,
                            last,
                            span(16052),
                        )?;
                        let selected = r.select(at_end, target, index, span(16053))?;
                        let result = r.conditional(
                            enabled,
                            &[raster, selected],
                            vec![real_type],
                            span(16054),
                            |r, inputs, outputs| {
                                let raster = r.load(inputs[0], span(16055))?;
                                let index = r.load(inputs[1], span(16056))?;
                                let value =
                                    r.project_element_dynamic(raster, &[index], span(16057))?;
                                r.store(outputs[0], value, span(16058))
                            },
                            |r, _, outputs| {
                                let value =
                                    r.constant(solve::SolveValue::real(p, -0.), span(16059))?;
                                r.store(outputs[0], value, span(16060))
                            },
                        )?;
                        r.store(output, result[0], span(16061))
                    },
                )?;
                b.store(outputs[1], result, span(16062))
            },
        )
        .unwrap();
    let site = table.call_site(owner).unwrap();
    (table.finish(), site)
}

#[test]
fn typed_map_late_bounds_fault_rolls_back_all_outputs_lazy_arm_and_recovers() {
    let (table, site) = table(3);
    let compiled = compile_pure_call_wasm(&table, &site).unwrap();
    let mut runner = Runner::new(&compiled);
    for (target, enabled) in [(3, true), (5, false), (5, true), (3, true)] {
        let inputs = vec![
            vec![real(-0.), real(2.), real(3.), real(4.)],
            vec![solve::SolveValueKind::Integer(target)],
            vec![solve::SolveValueKind::Boolean(enabled)],
        ];
        let actual = runner.run(&cells(inputs.iter().flatten().copied()));
        match oracle(&table, &site, &inputs) {
            Ok(expected) => assert_eq!(actual, (0, expected)),
            Err(error) => {
                assert!(actual.0 > 0);
                assert_eq!(actual.1, vec![0xa5; 32]);
                let fault = compiled
                    .faults()
                    .iter()
                    .find(|f| f.status == actual.0 as u32)
                    .unwrap();
                assert_eq!(fault.provenance, span(16057));
                assert_eq!(error.source_span(), Some(fault.provenance));
                assert_eq!(fault.kind, TypedCallFaultKind::IndexBounds);
                assert_eq!(fault.region_path, [(5, 0), (7, 0)]);
            }
        }
    }
}

#[test]
fn typed_map_empty_domain_with_faulting_body_remains_explicitly_unconstructible() {
    let p = profile();
    let real_type = solve::SolveValueType::scalar(solve::SolveScalarType::real(p));
    let mut table = solve::SolvePureCallTable::builder(p);
    let result = table.add_owner(
        identity(16064),
        vec![],
        vec![solve::SolvePureCallOutput::result(real_type.clone())],
        span(16064),
        |b, _, _| {
            b.map(
                cartesian(&[(1, 0, 1)]),
                &[],
                real_type,
                span(16065),
                |r, _, _, output| {
                    let value =
                        r.constant(solve::SolveValue::real(p, f64::INFINITY), span(16066))?;
                    let integer = r.convert(
                        solve::SolveConversionOperator::RealToIntegerTowardZero,
                        value,
                        span(16067),
                    )?;
                    let value = r.convert(
                        solve::SolveConversionOperator::IntegerToReal,
                        integer,
                        span(16068),
                    )?;
                    r.store(output, value, span(16069))
                },
            )?;
            Ok(())
        },
    );
    assert_eq!(
        result,
        Err(solve::SolveProgramConstructionError::InvalidMap {
            provenance: span(16065)
        })
    );
}
