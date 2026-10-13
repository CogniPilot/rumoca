//! Scalar temporaries of a loop pass live in wasm locals, and a binder-bounded
//! index needs no run-time check while an unbounded one keeps its fault.
use super::*;

/// `y[i] = 2 * real(i) + 1` for `i in 1:last`, into a vector of `extent`.
fn ramp(extent: u32, last: i64) -> (solve::SolvePureCallTable, solve::SolvePureCallSite) {
    let p = profile();
    let vector =
        solve::SolveValueType::tensor(solve::SolveScalarType::real(p), vec![extent]).unwrap();
    let mut table = solve::SolvePureCallTable::builder(p);
    let owner = table
        .add_owner(
            identity(90),
            vec![],
            vec![solve::SolvePureCallOutput::result(vector)],
            span(900),
            |b, _, outputs| {
                let zero = b.constant(solve::SolveValue::real(p, 0.0), span(901))?;
                let initial = b.fill(zero, vec![extent], span(902))?;
                let result = b.fold(
                    domain(1, last, 1),
                    &[initial],
                    &[],
                    span(903),
                    |r, carried, _, binders, outputs| {
                        let old = r.load(carried[0], span(904))?;
                        let index = r.load(binders[0], span(905))?;
                        let as_real = r.convert(
                            solve::SolveConversionOperator::IntegerToReal,
                            index,
                            span(906),
                        )?;
                        let two = r.constant(solve::SolveValue::real(p, 2.0), span(907))?;
                        let scaled = r.binary(
                            solve::SolveBinaryOperator::Multiply,
                            as_real,
                            two,
                            span(908),
                        )?;
                        let one = r.constant(solve::SolveValue::real(p, 1.0), span(909))?;
                        let value =
                            r.binary(solve::SolveBinaryOperator::Add, scaled, one, span(910))?;
                        let next = r.update_element(old, value, &[index], span(911))?;
                        r.store(outputs[0], next, span(912))
                    },
                )?;
                b.store(outputs[0], result[0], span(913))
            },
        )
        .unwrap();
    let site = table.call_site(owner).unwrap();
    (table.finish(), site)
}

#[test]
fn scalar_temporaries_of_a_pass_take_no_scratch_and_match_the_oracle() {
    let (table, site) = ramp(8, 8);
    let compiled = compile_pure_call_wasm(&table, &site).unwrap();
    let (status, actual) = Runner::new(&compiled).run(&[]);
    assert_eq!(status, 0);
    assert_eq!(actual, oracle(&table, &site, &[]).unwrap());
    assert_eq!(
        actual,
        cells((1..=8).map(|i| real(2.0 * f64::from(i) + 1.0)))
    );
    // Output 64, the zero constant 8 and the carried vector 64 (rewritten in
    // place), then the pass's binder 8 and loop counter 8. The converted index,
    // the scaled value and the sum live in wasm locals.
    assert_eq!(compiled.layout().scratch_bytes, 64 + 8 + 64 + 8 + 8);
}

#[test]
fn a_binder_bounded_index_needs_no_check_and_an_unbounded_one_faults() {
    let (bounded, bounded_site) = ramp(8, 8);
    let bounded_wasm = compile_pure_call_wasm(&bounded, &bounded_site).unwrap();
    let (status, _) = Runner::new(&bounded_wasm).run(&[]);
    assert_eq!(status, 0);

    // One pass past the vector: the binder bound no longer proves the index.
    let (unbounded, unbounded_site) = ramp(8, 9);
    let unbounded_wasm = compile_pure_call_wasm(&unbounded, &unbounded_site).unwrap();
    let (status, _) = Runner::new(&unbounded_wasm).run(&[]);
    assert_ne!(status, 0);
    let fault = unbounded_wasm
        .faults()
        .iter()
        .find(|fault| fault.status == status as u32)
        .unwrap();
    assert_eq!(fault.kind, TypedCallFaultKind::IndexBounds);
    assert_eq!(fault.provenance, span(911));
    assert!(oracle(&unbounded, &unbounded_site, &[]).is_err());
    // The proven index carries no bounds-check code.
    assert!(bounded_wasm.module_bytes().len() < unbounded_wasm.module_bytes().len());
}

/// `fill(source, extent)` where the source is a literal or the input.
fn fill_of(
    extent: u32,
    source: Option<f64>,
) -> (solve::SolvePureCallTable, solve::SolvePureCallSite) {
    let p = profile();
    let scalar = solve::SolveValueType::scalar(solve::SolveScalarType::real(p));
    let vector =
        solve::SolveValueType::tensor(solve::SolveScalarType::real(p), vec![extent]).unwrap();
    let mut table = solve::SolvePureCallTable::builder(p);
    let owner = table
        .add_owner(
            identity(91),
            vec![scalar],
            vec![solve::SolvePureCallOutput::result(vector)],
            span(920),
            |b, inputs, outputs| {
                let value = match source {
                    Some(literal) => b.constant(solve::SolveValue::real(p, literal), span(921))?,
                    None => b.load(inputs[0], span(922))?,
                };
                let filled = b.fill(value, vec![extent], span(923))?;
                b.store(outputs[0], filled, span(924))
            },
        )
        .unwrap();
    let site = table.call_site(owner).unwrap();
    (table.finish(), site)
}

#[test]
fn fills_of_every_size_and_source_match_the_oracle() {
    for extent in [1, 2, 8, 9, 100, 1000] {
        for source in [Some(0.0), Some(-0.0), Some(3.5), None] {
            let (table, site) = fill_of(extent, source);
            let compiled = compile_pure_call_wasm(&table, &site).unwrap();
            let mut runner = Runner::new(&compiled);
            for input in [-0.0, 2.25] {
                let inputs = vec![vec![real(input)]];
                let (status, actual) = runner.run(&cells(inputs.iter().flatten().copied()));
                assert_eq!(status, 0);
                assert_eq!(actual, oracle(&table, &site, &inputs).unwrap());
            }
        }
    }
}
