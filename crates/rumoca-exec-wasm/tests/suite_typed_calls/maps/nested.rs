use super::*;

#[test]
fn typed_map_array_update_keeps_duplicate_captures_immutable_between_points() {
    let p = profile();
    let element = solve::SolveScalarType::real(p);
    let vector = solve::SolveValueType::tensor(element, vec![3]).unwrap();
    let result_type = solve::SolveValueType::tensor(element, vec![3, 3]).unwrap();
    let mut table = solve::SolvePureCallTable::builder(p);
    let owner = table
        .add_owner(
            identity(16080),
            vec![vector.clone()],
            vec![
                solve::SolvePureCallOutput::result(result_type),
                solve::SolvePureCallOutput::result(vector.clone()),
            ],
            span(16080),
            |b, inputs, outputs| {
                let original = b.load(inputs[0], span(16081))?;
                let result = b.map(
                    cartesian(&[(1, 3, 1)]),
                    &[original, original],
                    vector,
                    span(16082),
                    |r, captures, binders, output| {
                        let input = r.load(captures[0], span(16083))?;
                        let index = r.load(binders[0], span(16084))?;
                        let value = r.constant(solve::SolveValue::real(p, 99.), span(16085))?;
                        let changed = r.update_element(input, value, &[index], span(16086))?;
                        // The aliased old input must remain readable after update.
                        let old = r.load(captures[1], span(16087))?;
                        let zero = r.constant(solve::SolveValue::real(p, 0.), span(16088))?;
                        let ignored = r.scale(old, zero, span(16089))?;
                        let result = r.binary(
                            solve::SolveBinaryOperator::Add,
                            changed,
                            ignored,
                            span(16090),
                        )?;
                        r.store(output, result, span(16091))
                    },
                )?;
                b.store(outputs[0], result, span(16092))?;
                b.store(outputs[1], original, span(16093))
            },
        )
        .unwrap();
    let site = table.call_site(owner).unwrap();
    let table = table.finish();
    let compiled = compile_pure_call_wasm(&table, &site).unwrap();
    let inputs = vec![vec![real(-0.), real(2.), real(3.)]];
    let actual = Runner::new(&compiled).run(&cells(inputs.iter().flatten().copied()));
    assert_eq!(actual, (0, oracle(&table, &site, &inputs).unwrap()));
}

fn nested_call(assertion: bool) -> (solve::SolvePureCallTable, solve::SolvePureCallSite) {
    let p = profile();
    let scalar = solve::SolveValueType::scalar(solve::SolveScalarType::real(p));
    let boolean = solve::SolveValueType::scalar(solve::SolveScalarType::Boolean);
    let mut table = solve::SolvePureCallTable::builder(p);
    let child = table
        .add_owner(
            identity(16100),
            vec![scalar.clone()],
            vec![
                solve::SolvePureCallOutput::result(scalar.clone()),
                if assertion {
                    solve::SolvePureCallOutput::assertion_predicate()
                } else {
                    solve::SolvePureCallOutput::result(boolean)
                },
            ],
            span(16100),
            |b, inputs, outputs| {
                let value = b.load(inputs[0], span(16101))?;
                let value = b.unary(solve::SolveUnaryOperator::Cos, value, span(16102))?;
                let predicate = b.constant(solve::SolveValue::boolean(false), span(16103))?;
                b.store(outputs[0], value, span(16104))?;
                b.store(outputs[1], predicate, span(16105))
            },
        )
        .unwrap();
    let result_type = solve::SolveValueType::tensor(scalar.element_type(), vec![2, 3]).unwrap();
    let root = table
        .add_owner(
            identity(16101),
            vec![scalar.clone()],
            vec![solve::SolvePureCallOutput::result(result_type)],
            span(16110),
            |b, inputs, outputs| {
                let value = b.load(inputs[0], span(16111))?;
                let row_type =
                    solve::SolveValueType::tensor(scalar.element_type(), vec![3]).unwrap();
                let result = b.map(
                    cartesian(&[(1, 2, 1)]),
                    &[value],
                    row_type,
                    span(16112),
                    |r, captures, _, output| {
                        let value = r.load(captures[0], span(16113))?;
                        let result = r.map(
                            cartesian(&[(3, 1, -1)]),
                            &[value],
                            scalar,
                            span(16114),
                            |r, captures, _, output| {
                                let value = r.load(captures[0], span(16115))?;
                                let result = r.call(child, &[value], span(16116))?;
                                r.store(output, result[0], span(16117))
                            },
                        )?;
                        r.store(output, result, span(16118))
                    },
                )?;
                b.store(outputs[0], result, span(16119))
            },
        )
        .unwrap();
    let site = table.call_site(root).unwrap();
    (table.finish(), site)
}

#[test]
fn typed_map_nested_maps_discover_complete_child_interface_and_math_imports() {
    let (table, site) = nested_call(false);
    let compiled = compile_pure_call_wasm(&table, &site).unwrap();
    let mut runner = Runner::new(&compiled);
    for value in [0., -0., 1.5, f64::INFINITY] {
        let inputs = vec![vec![real(value)]];
        assert_eq!(
            runner.run(&cells([real(value)])),
            (0, oracle(&table, &site, &inputs).unwrap())
        );
    }
}

#[test]
fn typed_map_nested_assertion_interface_stays_refused_even_when_output_unused() {
    let (table, site) = nested_call(true);
    assert_eq!(
        compile_pure_call_wasm(&table, &site).unwrap_err(),
        TypedCallCompileError::UnsupportedOutputKind {
            owner: table.owners()[0].id(),
            output: 1,
            kind: solve::SolvePureCallOutputKind::AssertionPredicate,
            provenance: span(16100)
        }
    );
}
