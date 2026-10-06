//! Exact i64 quotient semantics with independent i128 expected values.
use super::*;

fn quotient_call(
    operator: solve::SolveBinaryOperator,
    domain: solve::SolveIntegerDomain,
    count: u32,
) -> (solve::SolvePureCallTable, solve::SolvePureCallSite) {
    let p = solve::SolveArithmeticProfile::construct(solve::SolveRealFormat::Binary64, domain);
    let value_type =
        solve::SolveValueType::tensor(solve::SolveScalarType::integer(p), vec![count]).unwrap();
    let mut table = solve::SolvePureCallTable::builder(p);
    let owner = table
        .add_owner(
            identity(9400),
            vec![value_type.clone(), value_type.clone()],
            vec![solve::SolvePureCallOutput::result(value_type)],
            span(9400),
            |b, inputs, outputs| {
                let lhs = b.load(inputs[0], span(9401))?;
                let rhs = b.load(inputs[1], span(9402))?;
                let value = b.binary(operator, lhs, rhs, span(9403))?;
                b.store(outputs[0], value, span(9404))
            },
        )
        .unwrap();
    let site = table.call_site(owner).unwrap();
    (table.finish(), site)
}

fn independent(operator: solve::SolveBinaryOperator, lhs: i64, rhs: i64) -> Option<i64> {
    use solve::SolveBinaryOperator as B;
    if rhs == 0 {
        return None;
    }
    let (lhs, rhs) = (i128::from(lhs), i128::from(rhs));
    let truncated = lhs / rhs;
    let remainder = lhs - truncated * rhs;
    let value = match operator {
        B::IntegerQuotient => truncated,
        B::IntegerRemainder => remainder,
        B::IntegerModulo => {
            let floor = if remainder != 0 && (lhs < 0) != (rhs < 0) {
                truncated - 1
            } else {
                truncated
            };
            lhs - floor * rhs
        }
        _ => panic!("fixture owns only exact Integer quotients"),
    };
    i64::try_from(value).ok()
}

fn integers(values: &[i64]) -> Vec<solve::SolveValueKind> {
    values
        .iter()
        .copied()
        .map(solve::SolveValueKind::Integer)
        .collect()
}

#[test]
fn integer_quotients_preserve_exact_large_signed_values_and_reload() {
    use solve::SolveBinaryOperator as B;
    let cases = [
        (7, 3),
        (-7, 3),
        (7, -3),
        (-7, -3),
        (0, -1),
        (9_007_199_254_740_993, 2),
        (-9_007_199_254_740_993, 7),
        (i64::MAX, 1),
        (i64::MIN, 1),
        (i64::MIN, 3),
        (i64::MAX, i64::MIN),
        (1, i64::MIN),
        (-1, i64::MAX),
        (i64::MIN, i64::MAX),
        (i64::MIN, i64::MIN),
        (i64::MAX, -1),
    ];
    for operator in [B::IntegerQuotient, B::IntegerModulo, B::IntegerRemainder] {
        let (table, site) = quotient_call(operator, solve::SolveIntegerDomain::FULL, 1);
        let json = serde_json::to_string(&table).unwrap();
        let reloaded: solve::SolvePureCallTable = serde_json::from_str(&json).unwrap();
        assert_eq!(serde_json::to_string(&reloaded).unwrap(), json);
        let compiled = compile_pure_call_wasm(&reloaded, &site).unwrap();
        let mut runner = Runner::new(&compiled);
        for (lhs, rhs) in cases {
            let input = [integers(&[lhs]), integers(&[rhs])];
            let expected = independent(operator, lhs, rhs).unwrap();
            let expected = cells(integers(&[expected]));
            assert_eq!(oracle(&reloaded, &site, &input).unwrap(), expected);
            assert_eq!(
                runner.run(&cells(input.into_iter().flatten())),
                (0, expected)
            );
        }
    }
}

fn check_fault(
    operator: solve::SolveBinaryOperator,
    domain: solve::SolveIntegerDomain,
    pair: (i64, i64),
) {
    let (table, site) = quotient_call(operator, domain, 14400);
    let compiled = compile_pure_call_wasm(&table, &site).unwrap();
    let mut runner = Runner::new(&compiled);
    let mut lhs = vec![7; 14400];
    let mut rhs = vec![3; 14400];
    lhs[14399] = pair.0;
    rhs[14399] = pair.1;
    let inputs = [integers(&lhs), integers(&rhs)];
    assert!(matches!(
        oracle(&table, &site, &inputs),
        Err(rumoca_eval_solve::TypedProgramEvalError::IntegerArithmetic { .. })
    ));
    let (status, output) = runner.run(&cells(inputs.into_iter().flatten()));
    assert!(
        compiled
            .faults()
            .iter()
            .any(|fault| fault.status == status as u32
                && fault.kind == TypedCallFaultKind::IntegerArithmetic)
    );
    assert_eq!(
        output,
        vec![0xa5; 14400 * 8],
        "late failure published a partial tuple"
    );
    lhs[14399] = 7;
    rhs[14399] = 3;
    let inputs = [integers(&lhs), integers(&rhs)];
    let expected = cells(integers(&vec![independent(operator, 7, 3).unwrap(); 14400]));
    assert_eq!(oracle(&table, &site, &inputs).unwrap(), expected);
    assert_eq!(
        runner.run(&cells(inputs.into_iter().flatten())),
        (0, expected)
    );
}

#[test]
fn integer_quotients_full_raster_late_faults_are_atomic_and_recover() {
    use solve::SolveBinaryOperator as B;
    for operator in [B::IntegerQuotient, B::IntegerModulo, B::IntegerRemainder] {
        check_fault(operator, solve::SolveIntegerDomain::FULL, (1, 0));
    }
    check_fault(
        B::IntegerQuotient,
        solve::SolveIntegerDomain::FULL,
        (i64::MIN, -1),
    );
    let narrow = solve::SolveIntegerDomain::construct(-8, 7).unwrap();
    check_fault(B::IntegerQuotient, narrow, (-8, -1));
    let positive = solve::SolveIntegerDomain::construct(1, 8).unwrap();
    check_fault(B::IntegerModulo, positive, (6, 3));
    check_fault(B::IntegerRemainder, positive, (6, 3));
}

#[test]
fn integer_remainders_do_not_construct_an_overflowing_quotient() {
    for operator in [
        solve::SolveBinaryOperator::IntegerModulo,
        solve::SolveBinaryOperator::IntegerRemainder,
    ] {
        let (table, site) = quotient_call(operator, solve::SolveIntegerDomain::FULL, 1);
        let compiled = compile_pure_call_wasm(&table, &site).unwrap();
        let inputs = [integers(&[i64::MIN]), integers(&[-1])];
        let expected = cells(integers(&[0]));
        assert_eq!(oracle(&table, &site, &inputs).unwrap(), expected);
        assert_eq!(
            Runner::new(&compiled).run(&cells(inputs.into_iter().flatten())),
            (0, expected)
        );
    }
}
