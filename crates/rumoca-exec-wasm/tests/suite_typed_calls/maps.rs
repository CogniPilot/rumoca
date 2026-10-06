mod domains;
pub(crate) mod faults;
mod nested;
mod raster;
use super::*;

fn cartesian(axes: &[(i64, i64, i64)]) -> StructuredIndexDomain {
    StructuredIndexDomain {
        binders: axes
            .iter()
            .enumerate()
            .map(|(id, &(lower, upper, step))| StructuredIndexBinder {
                id,
                display_name: format!("i{id}"),
                lower,
                upper,
                step,
            })
            .collect(),
    }
}

fn fast_table() -> (solve::SolvePureCallTable, solve::SolvePureCallSite) {
    let p = profile();
    let real_type = solve::SolveValueType::scalar(solve::SolveScalarType::real(p));
    let patch = solve::SolveValueType::tensor(real_type.element_type(), vec![24]).unwrap();
    let result = solve::SolveValueType::tensor(real_type.element_type(), vec![22]).unwrap();
    let mut table = solve::SolvePureCallTable::builder(p);
    let owner = table
        .add_owner(
            identity(16000),
            vec![patch.clone()],
            vec![
                solve::SolvePureCallOutput::result(result),
                solve::SolvePureCallOutput::result(patch),
            ],
            span(16000),
            |b, inputs, outputs| {
                let patch = b.load(inputs[0], span(16001))?;
                let low = b.map(
                    cartesian(&[(1, 22, 1)]),
                    &[patch],
                    real_type.clone(),
                    span(16002),
                    |r, captures, binders, output| {
                        let patch = r.load(captures[0], span(16003))?;
                        let i = r.load(binders[0], span(16004))?;
                        let one =
                            r.constant(solve::SolveValue::integer(p, 1).unwrap(), span(16005))?;
                        let next =
                            r.binary(solve::SolveBinaryOperator::Add, i, one, span(16006))?;
                        let a = r.project_element_dynamic(patch, &[i], span(16007))?;
                        let c = r.project_element_dynamic(patch, &[next], span(16008))?;
                        let less =
                            r.compare(solve::SolveCompareOperator::Less, a, c, span(16009))?;
                        let chosen = r.conditional(
                            less,
                            &[a, c],
                            vec![real_type],
                            span(16010),
                            |r, inputs, outputs| {
                                let a = r.load(inputs[0], span(16011))?;
                                r.store(outputs[0], a, span(16012))
                            },
                            |r, inputs, outputs| {
                                let c = r.load(inputs[1], span(16013))?;
                                r.store(outputs[0], c, span(16014))
                            },
                        )?;
                        r.store(output, chosen[0], span(16015))
                    },
                )?;
                b.store(outputs[0], low, span(16016))?;
                b.store(outputs[1], patch, span(16017))
            },
        )
        .unwrap();
    let site = table.call_site(owner).unwrap();
    (table.finish(), site)
}

#[test]
fn typed_map_fast_22_point_lazy_comparison_preserves_ieee_bits_and_capture() {
    let (table, site) = fast_table();
    let compiled = compile_pure_call_wasm(&table, &site).unwrap();
    let mut runner = Runner::new(&compiled);
    let values = [
        0.,
        -0.,
        f64::from_bits(1),
        2.,
        -3.,
        f64::INFINITY,
        f64::NEG_INFINITY,
        f64::from_bits(0x7ff8_dead_beef_1234),
    ];
    for frame in 0..8 {
        let patch = (0..24)
            .map(|i| values[(i + frame) % values.len()])
            .collect::<Vec<_>>();
        let expected = patch[..22]
            .iter()
            .zip(&patch[1..23])
            .map(|(&a, &b)| real(if a < b { a } else { b }))
            .chain(patch.iter().copied().map(real))
            .collect::<Vec<_>>();
        let inputs = vec![patch.into_iter().map(real).collect()];
        let actual = runner.run(&cells(inputs.iter().flatten().copied()));
        assert_eq!(actual, (0, cells(expected)));
        assert_eq!(actual.1, oracle(&table, &site, &inputs).unwrap());
    }
}
