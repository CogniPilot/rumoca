//! Exact i64 negation, magnitude and sign with checked domain faults.
use super::*;

fn unary_call(
    operator: solve::SolveUnaryOperator,
    domain: solve::SolveIntegerDomain,
) -> (solve::SolvePureCallTable, solve::SolvePureCallSite) {
    let p = solve::SolveArithmeticProfile::construct(solve::SolveRealFormat::Binary64, domain);
    let value_type = solve::SolveValueType::scalar(solve::SolveScalarType::integer(p));
    let mut table = solve::SolvePureCallTable::builder(p);
    let owner = table
        .add_owner(
            identity(9500),
            vec![value_type.clone()],
            vec![solve::SolvePureCallOutput::result(value_type)],
            span(9500),
            |b, inputs, outputs| {
                let operand = b.load(inputs[0], span(9501))?;
                let value = b.unary(operator, operand, span(9502))?;
                b.store(outputs[0], value, span(9503))
            },
        )
        .unwrap();
    let site = table.call_site(owner).unwrap();
    (table.finish(), site)
}

fn independent(operator: solve::SolveUnaryOperator, value: i64) -> i128 {
    use solve::SolveUnaryOperator as U;
    let value = i128::from(value);
    match operator {
        U::Negate => -value,
        U::Abs => value.abs(),
        U::Sign => value.signum(),
        _ => panic!("fixture owns only Integer unaries"),
    }
}

fn integer(value: i64) -> Vec<solve::SolveValueKind> {
    vec![solve::SolveValueKind::Integer(value)]
}

fn check(operator: solve::SolveUnaryOperator, domain: solve::SolveIntegerDomain) {
    let (table, site) = unary_call(operator, domain);
    let compiled = compile_pure_call_wasm(&table, &site).unwrap();
    let mut runner = Runner::new(&compiled);
    let (low, high) = (domain.minimum(), domain.maximum());
    for value in [low, low + 1, -7, -1, 0, 1, 7, high] {
        let input = [integer(value)];
        let (status, output) = runner.run(&cells(input[0].clone()));
        let expected = i64::try_from(independent(operator, value))
            .ok()
            .filter(|result| domain.contains(*result));
        let Some(expected) = expected else {
            assert!(oracle(&table, &site, &input).is_err());
            assert!(compiled.faults().iter().any(|fault| {
                fault.status == status as u32 && fault.kind == TypedCallFaultKind::IntegerArithmetic
            }));
            assert_eq!(output, vec![0xa5; 8], "fault published its output");
            continue;
        };
        let expected = cells(integer(expected));
        assert_eq!(oracle(&table, &site, &input).unwrap(), expected);
        assert_eq!((status, output), (0, expected));
    }
}

#[test]
fn integer_unaries_match_independent_values_and_fault_outside_their_domain() {
    use solve::SolveUnaryOperator as U;
    let narrow = solve::SolveIntegerDomain::construct(-8, 7).unwrap();
    for domain in [solve::SolveIntegerDomain::FULL, narrow] {
        for operator in [U::Negate, U::Abs, U::Sign] {
            check(operator, domain);
        }
    }
}
