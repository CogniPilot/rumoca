//! An Integer the bounds pin to one value is a constant: no overflow check
//! guards arithmetic over it.
use super::*;

/// `sum((i - 1) * stride + 1 for i in 1:3)` where `stride` is `div(16, 4)`, or
/// the Integer input when `from_input`.
fn strided_sum(from_input: bool) -> (solve::SolvePureCallTable, solve::SolvePureCallSite) {
    let p = profile();
    let integer = solve::SolveValueType::scalar(solve::SolveScalarType::integer(p));
    let mut table = solve::SolvePureCallTable::builder(p);
    let owner = table
        .add_owner(
            identity(97),
            if from_input {
                vec![integer.clone()]
            } else {
                vec![]
            },
            vec![solve::SolvePureCallOutput::result(integer)],
            span(1300),
            |b, inputs, outputs| {
                let stride = if from_input {
                    b.load(inputs[0], span(1301))?
                } else {
                    let total =
                        b.constant(solve::SolveValue::integer(p, 16).unwrap(), span(1302))?;
                    let parts =
                        b.constant(solve::SolveValue::integer(p, 4).unwrap(), span(1303))?;
                    b.binary(
                        solve::SolveBinaryOperator::IntegerQuotient,
                        total,
                        parts,
                        span(1304),
                    )?
                };
                let zero = b.constant(solve::SolveValue::integer(p, 0).unwrap(), span(1305))?;
                let sum = b.fold(
                    domain(1, 3, 1),
                    &[zero],
                    &[stride],
                    span(1306),
                    |r, carried, captures, binders, outputs| {
                        let old = r.load(carried[0], span(1307))?;
                        let index = r.load(binders[0], span(1308))?;
                        let stride = r.load(captures[0], span(1309))?;
                        let one =
                            r.constant(solve::SolveValue::integer(p, 1).unwrap(), span(1310))?;
                        let steps =
                            r.binary(solve::SolveBinaryOperator::Subtract, index, one, span(1311))?;
                        let scaled = r.binary(
                            solve::SolveBinaryOperator::Multiply,
                            steps,
                            stride,
                            span(1312),
                        )?;
                        let shifted =
                            r.binary(solve::SolveBinaryOperator::Add, scaled, one, span(1313))?;
                        let next =
                            r.binary(solve::SolveBinaryOperator::Add, old, shifted, span(1314))?;
                        r.store(outputs[0], next, span(1315))
                    },
                )?;
                b.store(outputs[0], sum[0], span(1316))
            },
        )
        .unwrap();
    let site = table.call_site(owner).unwrap();
    (table.finish(), site)
}

/// How many fault exits `module` contains.
fn fault_exits(module: &[u8]) -> usize {
    use wasmparser::Operator as O;
    let mut exits = 0;
    for payload in wasmparser::Parser::new(0).parse_all(module) {
        let wasmparser::Payload::CodeSectionEntry(body) = payload.unwrap() else {
            continue;
        };
        for operator in body.get_operators_reader().unwrap() {
            exits += usize::from(matches!(operator.unwrap(), O::Return));
        }
    }
    exits
}

#[test]
fn arithmetic_over_a_quotient_of_constants_is_unchecked() {
    let (constant, constant_site) = strided_sum(false);
    let (input, input_site) = strided_sum(true);
    let constant_wasm = compile_pure_call_wasm(&constant, &constant_site).unwrap();
    let input_wasm = compile_pure_call_wasm(&input, &input_site).unwrap();

    let (status, actual) = Runner::new(&constant_wasm).run(&[]);
    assert_eq!(status, 0);
    assert_eq!(actual, oracle(&constant, &constant_site, &[]).unwrap());
    assert_eq!(actual, cells([solve::SolveValueKind::Integer(15)]));
    let four = 4i64.to_le_bytes();
    let (status, actual) = Runner::new(&input_wasm).run(&four);
    assert_eq!(status, 0);
    let expected = oracle(
        &input,
        &input_site,
        &[vec![solve::SolveValueKind::Integer(4)]],
    );
    assert_eq!(actual, expected.unwrap());

    let constant_exits = fault_exits(constant_wasm.module_bytes());
    let input_exits = fault_exits(input_wasm.module_bytes());
    assert!(
        constant_exits < input_exits,
        "{constant_exits} vs {input_exits}"
    );
}
