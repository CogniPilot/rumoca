//! A pass of a loop keeps its counter and a single binder out of memory, and a
//! Map pass writes its element where it belongs instead of copying it there.
use super::*;

/// Loads and stores executed in the body of the first loop of `module`.
pub(super) fn loop_memory_accesses(module: &[u8]) -> (usize, usize) {
    use wasmparser::Operator as O;
    let (mut loads, mut stores, mut depth) = (0, 0, 0usize);
    for payload in wasmparser::Parser::new(0).parse_all(module) {
        let wasmparser::Payload::CodeSectionEntry(body) = payload.unwrap() else {
            continue;
        };
        let mut blocks: Vec<bool> = Vec::new();
        for operator in body.get_operators_reader().unwrap() {
            match operator.unwrap() {
                O::Loop { .. } => {
                    blocks.push(true);
                    depth += 1;
                }
                O::Block { .. } | O::If { .. } => blocks.push(false),
                O::End => depth -= usize::from(blocks.pop() == Some(true)),
                O::I32Load { .. } | O::I64Load { .. } | O::F64Load { .. } if depth > 0 => {
                    loads += 1;
                }
                O::I32Store { .. } | O::I64Store { .. } | O::F64Store { .. } if depth > 0 => {
                    stores += 1;
                }
                _ => {}
            }
        }
    }
    (loads, stores)
}

/// `y[i] = 2 * real(i)` for `i in first:last:step` as one Map.
pub(super) fn doubled_binder(
    first: i64,
    last: i64,
    step: i64,
    extent: u32,
) -> (solve::SolvePureCallTable, solve::SolvePureCallSite) {
    let p = profile();
    let real = solve::SolveValueType::scalar(solve::SolveScalarType::real(p));
    let vector =
        solve::SolveValueType::tensor(solve::SolveScalarType::real(p), vec![extent]).unwrap();
    let mut table = solve::SolvePureCallTable::builder(p);
    let owner = table
        .add_owner(
            identity(96),
            vec![],
            vec![solve::SolvePureCallOutput::result(vector)],
            span(1200),
            |b, _, outputs| {
                let doubled = b.map(
                    domain(first, last, step),
                    &[],
                    real.clone(),
                    span(1201),
                    |r, _, binders, output| {
                        let index = r.load(binders[0], span(1202))?;
                        let index = r.convert(
                            solve::SolveConversionOperator::IntegerToReal,
                            index,
                            span(1203),
                        )?;
                        let two = r.constant(solve::SolveValue::real(p, 2.0), span(1204))?;
                        let value =
                            r.binary(solve::SolveBinaryOperator::Multiply, index, two, span(1205))?;
                        r.store(output, value, span(1206))
                    },
                )?;
                b.store(outputs[0], doubled, span(1207))
            },
        )
        .unwrap();
    let site = table.call_site(owner).unwrap();
    (table.finish(), site)
}

#[test]
fn a_map_pass_reads_no_counter_or_binder_and_writes_its_element_in_place() {
    // More passes than the unrolling bound, so the passes are a loop.
    for (first, last, step) in [(1, 40, 1), (100, -20, -3), (-60, 60, 3)] {
        let expected = std::iter::successors(Some(first), |i| Some(i + step))
            .take_while(|i| if step > 0 { *i <= last } else { *i >= last })
            .map(|i| 2 * i)
            .collect::<Vec<i64>>();
        let extent = expected.len() as u32;
        let (table, site) = doubled_binder(first, last, step, extent);
        let compiled = compile_pure_call_wasm(&table, &site).unwrap();
        let (status, actual) = Runner::new(&compiled).run(&[]);
        assert_eq!(status, 0);
        assert_eq!(actual, oracle(&table, &site, &[]).unwrap());
        assert_eq!(
            actual,
            cells(expected.iter().map(|value| real(*value as f64)))
        );
        // The element is the only memory access of a pass.
        assert_eq!(
            loop_memory_accesses(compiled.module_bytes()),
            (0, 1),
            "domain {first}:{last}:{step}"
        );
    }
}

/// `sum(2 * real(i) for i in first:last)` as one Fold carrying a Real.
fn doubled_sum(first: i64, last: i64) -> (solve::SolvePureCallTable, solve::SolvePureCallSite) {
    let p = profile();
    let real = solve::SolveValueType::scalar(solve::SolveScalarType::real(p));
    let mut table = solve::SolvePureCallTable::builder(p);
    let owner = table
        .add_owner(
            identity(98),
            vec![],
            vec![solve::SolvePureCallOutput::result(real)],
            span(1400),
            |b, _, outputs| {
                let zero = b.constant(solve::SolveValue::real(p, 0.0), span(1401))?;
                let sum = b.fold(
                    domain(first, last, 1),
                    &[zero],
                    &[],
                    span(1402),
                    |r, carried, _, binders, outputs| {
                        let old = r.load(carried[0], span(1403))?;
                        let index = r.load(binders[0], span(1404))?;
                        let index = r.convert(
                            solve::SolveConversionOperator::IntegerToReal,
                            index,
                            span(1405),
                        )?;
                        let two = r.constant(solve::SolveValue::real(p, 2.0), span(1406))?;
                        let doubled =
                            r.binary(solve::SolveBinaryOperator::Multiply, index, two, span(1407))?;
                        let next =
                            r.binary(solve::SolveBinaryOperator::Add, old, doubled, span(1408))?;
                        r.store(outputs[0], next, span(1409))
                    },
                )?;
                b.store(outputs[0], sum[0], span(1410))
            },
        )
        .unwrap();
    let site = table.call_site(owner).unwrap();
    (table.finish(), site)
}

#[test]
fn a_fold_pass_loads_its_carried_scalar_once_and_stores_it_in_place() {
    // More passes than the unrolling bound, so the passes are a loop.
    for (first, last, expected) in [(1, 40, 1640.0), (-30, 40, 710.0), (-40, 40, 0.0)] {
        let (table, site) = doubled_sum(first, last);
        let compiled = compile_pure_call_wasm(&table, &site).unwrap();
        let (status, actual) = Runner::new(&compiled).run(&[]);
        assert_eq!(status, 0);
        assert_eq!(actual, oracle(&table, &site, &[]).unwrap());
        assert_eq!(actual, cells([real(expected)]));
        // One load of the carried value and one store of its successor.
        assert_eq!(
            loop_memory_accesses(compiled.module_bytes()),
            (1, 1),
            "domain {first}:{last}"
        );
    }
}
