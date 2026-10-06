use super::*;

fn conversion_owner(table: &mut solve::SolvePureCallTableBuilder) -> solve::SolvePureCallOwnerId {
    let p = profile();
    let real_type = solve::SolveValueType::scalar(solve::SolveScalarType::real(p));
    let integer = solve::SolveValueType::scalar(solve::SolveScalarType::integer(p));
    table
        .add_owner(
            identity(100),
            vec![real_type.clone()],
            vec![
                solve::SolvePureCallOutput::result(integer),
                solve::SolvePureCallOutput::result(real_type),
            ],
            span(600),
            |b, inputs, outputs| {
                let value = b.load(inputs[0], span(601))?;
                let integer = b.convert(
                    solve::SolveConversionOperator::RealToIntegerTowardZero,
                    value,
                    span(602),
                )?;
                b.store(outputs[0], integer, span(603))?;
                b.store(outputs[1], value, span(604))
            },
        )
        .unwrap()
}

#[test]
fn early_private_store_and_late_call_failure_preserve_atomic_tuple_publication() {
    let p = profile();
    let integer = solve::SolveValueType::scalar(solve::SolveScalarType::integer(p));
    let real_type = solve::SolveValueType::scalar(solve::SolveScalarType::real(p));
    let mut table = solve::SolvePureCallTable::builder(p);
    let child = conversion_owner(&mut table);
    let root = table
        .add_owner(
            identity(122),
            vec![real_type],
            vec![solve::SolvePureCallOutput::result(integer); 2],
            span(690),
            |b, inputs, outputs| {
                let prefix = b.constant(solve::SolveValue::integer(p, 7).unwrap(), span(691))?;
                b.store(outputs[0], prefix, span(692))?;
                let value = b.load(inputs[0], span(693))?;
                let result = b.call(child, &[value], span(694))?;
                b.store(outputs[1], result[0], span(695))
            },
        )
        .unwrap();
    let site = table.call_site(root).unwrap();
    let table = table.finish();
    let compiled = compile_pure_call_wasm(&table, &site).unwrap();
    let mut runner = Runner::new(&compiled);
    let (status, output) = runner.run(&cells([real(f64::INFINITY)]));
    assert!(status > 0);
    assert_eq!(output, vec![0xa5; 16]);
    assert!(oracle(&table, &site, &[vec![real(f64::INFINITY)]]).is_err());
    let actual = runner.run(&cells([real(4.5)]));
    assert_eq!(
        actual,
        (
            0,
            cells([
                solve::SolveValueKind::Integer(7),
                solve::SolveValueKind::Integer(4)
            ])
        )
    );
    assert_eq!(actual.1, oracle(&table, &site, &[vec![real(4.5)]]).unwrap());
}

#[test]
fn nested_calls_preserve_tuple_order_child_fault_identity_atomicity_and_recovery() {
    let p = profile();
    let real_type = solve::SolveValueType::scalar(solve::SolveScalarType::real(p));
    let integer = solve::SolveValueType::scalar(solve::SolveScalarType::integer(p));
    let mut table = solve::SolvePureCallTable::builder(p);
    let child = conversion_owner(&mut table);
    let parent = table
        .add_owner(
            identity(101),
            vec![real_type],
            vec![solve::SolvePureCallOutput::result(integer)],
            span(610),
            |b, inputs, outputs| {
                let value = b.load(inputs[0], span(611))?;
                let first = b.call(child, &[value], span(612))?;
                let second = b.call(child, &[first[1]], span(613))?;
                b.store(outputs[0], second[0], span(614))
            },
        )
        .unwrap();
    let root = table
        .add_owner(
            identity(102),
            vec![solve::SolveValueType::scalar(solve::SolveScalarType::real(
                p,
            ))],
            vec![solve::SolvePureCallOutput::result(
                solve::SolveValueType::scalar(solve::SolveScalarType::integer(p)),
            )],
            span(620),
            |b, inputs, outputs| {
                let value = b.load(inputs[0], span(621))?;
                let result = b.call(parent, &[value], span(622))?;
                b.store(outputs[0], result[0], span(623))
            },
        )
        .unwrap();
    let site = table.call_site(root).unwrap();
    let table = table.finish();
    let compiled = compile_pure_call_wasm(&table, &site).unwrap();
    let mut runner = Runner::new(&compiled);
    for value in [3.75, f64::INFINITY, -4.5] {
        let inputs = vec![vec![real(value)]];
        let actual = runner.run(&cells([real(value)]));
        match oracle(&table, &site, &inputs) {
            Ok(expected) => assert_eq!(actual, (0, expected)),
            Err(error) => {
                assert_eq!(actual.1, vec![0xa5; 8]);
                let fault = compiled
                    .faults()
                    .iter()
                    .find(|f| f.status == actual.0 as u32)
                    .unwrap();
                assert_eq!(fault.owner, child);
                assert_eq!(fault.provenance, span(602));
                assert_eq!(error.source_span(), Some(fault.provenance));
            }
        }
    }
    // Each exact owner is emitted once despite two calls to the same child.
    assert_eq!(
        compiled
            .faults()
            .iter()
            .filter(|f| f.owner == child && f.kind == TypedCallFaultKind::IntegerConversion)
            .count(),
        1
    );
}

#[test]
fn nested_call_is_lazy_inside_a_checked_conditional() {
    let p = profile();
    let integer = solve::SolveValueType::scalar(solve::SolveScalarType::integer(p));
    let mut table = solve::SolvePureCallTable::builder(p);
    let child = conversion_owner(&mut table);
    let root = table
        .add_owner(
            identity(103),
            vec![
                solve::SolveValueType::scalar(solve::SolveScalarType::real(p)),
                solve::SolveValueType::scalar(solve::SolveScalarType::Boolean),
            ],
            vec![solve::SolvePureCallOutput::result(integer.clone())],
            span(630),
            |b, inputs, outputs| {
                let value = b.load(inputs[0], span(631))?;
                let condition = b.load(inputs[1], span(632))?;
                let result = b.conditional(
                    condition,
                    &[value],
                    vec![integer],
                    span(633),
                    |r, inputs, outputs| {
                        let value = r.load(inputs[0], span(634))?;
                        let result = r.call(child, &[value], span(635))?;
                        r.store(outputs[0], result[0], span(636))
                    },
                    |r, _, outputs| {
                        let zero =
                            r.constant(solve::SolveValue::integer(p, 0).unwrap(), span(637))?;
                        r.store(outputs[0], zero, span(638))
                    },
                )?;
                b.store(outputs[0], result[0], span(639))
            },
        )
        .unwrap();
    let site = table.call_site(root).unwrap();
    let table = table.finish();
    let compiled = compile_pure_call_wasm(&table, &site).unwrap();
    let mut runner = Runner::new(&compiled);
    for active in [false, true, false] {
        let inputs = vec![
            vec![real(f64::INFINITY)],
            vec![solve::SolveValueKind::Boolean(active)],
        ];
        let (status, output) = runner.run(&cells(inputs.iter().flatten().copied()));
        if active {
            assert!(status > 0);
            assert_eq!(output, vec![0xa5; 8]);
        } else {
            assert_eq!(status, 0);
            assert_eq!(output, oracle(&table, &site, &inputs).unwrap());
        }
    }
}

#[test]
fn nested_assertion_predicate_interface_is_refused_even_when_result_is_unused() {
    let p = profile();
    let integer = solve::SolveValueType::scalar(solve::SolveScalarType::integer(p));
    let mut table = solve::SolvePureCallTable::builder(p);
    let child = table
        .add_owner(
            identity(104),
            vec![],
            vec![solve::SolvePureCallOutput::assertion_predicate()],
            span(640),
            |b, _, outputs| {
                let value = b.constant(solve::SolveValue::boolean(false), span(641))?;
                b.store(outputs[0], value, span(642))
            },
        )
        .unwrap();
    let root = table
        .add_owner(
            identity(105),
            vec![],
            vec![solve::SolvePureCallOutput::result(integer)],
            span(643),
            |b, _, outputs| {
                b.call(child, &[], span(644))?;
                let value = b.constant(solve::SolveValue::integer(p, 1).unwrap(), span(645))?;
                b.store(outputs[0], value, span(646))
            },
        )
        .unwrap();
    let site = table.call_site(root).unwrap();
    let table = table.finish();
    assert_eq!(
        compile_pure_call_wasm(&table, &site).unwrap_err(),
        TypedCallCompileError::UnsupportedOutputKind {
            owner: child,
            output: 0,
            kind: solve::SolvePureCallOutputKind::AssertionPredicate,
            provenance: span(640),
        }
    );
}
