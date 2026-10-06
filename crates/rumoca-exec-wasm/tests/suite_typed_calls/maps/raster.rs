use super::*;

#[test]
fn typed_map_full_14400_values_keep_raw_bits_and_compact_region_storage() {
    let p = profile();
    let scalar = solve::SolveValueType::scalar(solve::SolveScalarType::real(p));
    let raster = solve::SolveValueType::tensor(scalar.element_type(), vec![14400]).unwrap();
    let mut table = solve::SolvePureCallTable::builder(p);
    let owner = table
        .add_owner(
            identity(16140),
            vec![raster.clone()],
            vec![solve::SolvePureCallOutput::result(raster)],
            span(16140),
            |b, inputs, outputs| {
                let input = b.load(inputs[0], span(16141))?;
                let result = b.map(
                    cartesian(&[(1, 14400, 1)]),
                    &[input],
                    scalar,
                    span(16142),
                    |r, captures, binders, output| {
                        let input = r.load(captures[0], span(16143))?;
                        let index = r.load(binders[0], span(16144))?;
                        let value = r.project_element_dynamic(input, &[index], span(16145))?;
                        r.store(output, value, span(16146))
                    },
                )?;
                b.store(outputs[0], result, span(16147))
            },
        )
        .unwrap();
    let site = table.call_site(owner).unwrap();
    let table = table.finish();
    let compiled = compile_pure_call_wasm(&table, &site).unwrap();
    assert!(compiled.module_bytes().len() < 4096);
    assert!(compiled.layout().scratch_bytes < 14400 * 8 * 3);
    let mut runner = Runner::new(&compiled);
    for pattern in [
        [
            0.,
            -0.,
            f64::from_bits(1),
            f64::from_bits(0x7ff8_dead_beef_1234),
        ],
        [1e16, 1., -1e16, f64::INFINITY],
    ] {
        let input = (0..14400).map(|i| real(pattern[i % 4])).collect::<Vec<_>>();
        let actual = runner.run(&cells(input.iter().copied()));
        assert_eq!(actual, (0, cells(input.iter().copied())));
        assert_eq!(actual.1, oracle(&table, &site, &[input]).unwrap());
    }
}
