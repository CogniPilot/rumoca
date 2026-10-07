//! Cross products, reductions, diagonals, concatenations and guarded element
//! selections match the canonical typed evaluator bit for bit, and fail with
//! the source-bound Integer fault wherever the evaluator refuses.
use super::*;

/// A one-output owner over `inputs` whose body is `body` of the loaded inputs.
fn owner(
    inputs: Vec<solve::SolveValueType>,
    output: solve::SolveValueType,
    body: impl for<'p> Fn(
        &mut solve::TypedProgramBuilder<'p>,
        &[solve::ProgramRegister<'p>],
    )
        -> Result<solve::ProgramRegister<'p>, solve::SolveProgramConstructionError>,
) -> (solve::SolvePureCallTable, solve::SolvePureCallSite) {
    let mut table = solve::SolvePureCallTable::builder(profile());
    let owner = table
        .add_owner(
            identity(9300),
            inputs,
            vec![solve::SolvePureCallOutput::result(output)],
            span(9300),
            |b, inputs, outputs| {
                let loaded = inputs
                    .iter()
                    .enumerate()
                    .map(|(ordinal, input)| b.load(*input, span(9301 + ordinal)))
                    .collect::<Result<Vec<_>, _>>()?;
                let result = body(b, &loaded)?;
                b.store(outputs[0], result, span(9399))
            },
        )
        .unwrap();
    let site = table.call_site(owner).unwrap();
    (table.finish(), site)
}

/// Every case runs in Wasmi and in the canonical evaluator: equal cells on
/// success, an Integer fault and untouched outputs on a refusal.
fn agrees(
    (table, site): &(solve::SolvePureCallTable, solve::SolvePureCallSite),
    cases: &[Vec<Vec<solve::SolveValueKind>>],
) {
    let compiled = compile_pure_call_wasm(table, site).unwrap();
    let mut runner = Runner::new(&compiled);
    for inputs in cases {
        let (status, actual) = runner.run(&cells(inputs.iter().flatten().copied()));
        match oracle(table, site, inputs) {
            Ok(expected) => {
                assert_eq!(status, 0, "{inputs:?}");
                assert_eq!(actual, expected, "{inputs:?}");
            }
            Err(_) => {
                let fault = compiled
                    .faults()
                    .iter()
                    .find(|fault| fault.status == status as u32)
                    .unwrap_or_else(|| panic!("{inputs:?} must fault, status {status}"));
                assert_eq!(fault.kind, TypedCallFaultKind::IntegerArithmetic);
                assert_eq!(actual, vec![0xa5; actual.len()]);
            }
        }
    }
}

fn reals(values: &[f64]) -> Vec<solve::SolveValueKind> {
    values.iter().copied().map(real).collect()
}

fn integers(values: &[i64]) -> Vec<solve::SolveValueKind> {
    values
        .iter()
        .copied()
        .map(solve::SolveValueKind::Integer)
        .collect()
}

fn tensor(scalar: solve::SolveScalarType, dimensions: &[u32]) -> solve::SolveValueType {
    solve::SolveValueType::tensor(scalar, dimensions.to_vec()).unwrap()
}

fn real_scalar() -> solve::SolveScalarType {
    solve::SolveScalarType::real(profile())
}

fn integer_scalar() -> solve::SolveScalarType {
    solve::SolveScalarType::integer(profile())
}

const SPECIAL: [f64; 6] = [-0.0, 1.5, -2.25, f64::INFINITY, f64::MAX, f64::NAN];

#[test]
fn cross_products_match_the_canonical_evaluator() {
    let vector = tensor(real_scalar(), &[3]);
    let call = owner(vec![vector.clone(); 2], vector, |b, v| {
        b.cross(v[0], v[1], span(9310))
    });
    let mut cases = Vec::new();
    for a in SPECIAL {
        for c in SPECIAL {
            cases.push(vec![reals(&[a, 2.0, c]), reals(&[c, -0.0, a])]);
        }
    }
    agrees(&call, &cases);
}

#[test]
fn reductions_fold_from_the_first_element_like_the_canonical_evaluator() {
    let vector = tensor(real_scalar(), &[4]);
    for operator in [
        solve::SolveReductionOperator::Sum,
        solve::SolveReductionOperator::Product,
        solve::SolveReductionOperator::Minimum,
        solve::SolveReductionOperator::Maximum,
    ] {
        let call = owner(
            vec![vector.clone()],
            solve::SolveValueType::scalar(real_scalar()),
            move |b, v| b.reduce(operator, v[0], span(9320)),
        );
        agrees(
            &call,
            &[
                vec![reals(&[1.0, -0.0, 2.5, 1e308])],
                vec![reals(&[f64::NAN, 2.0, -3.0, 0.0])],
                vec![reals(&[-0.0, 0.0, f64::INFINITY, f64::NEG_INFINITY])],
            ],
        );
    }
    let vector = tensor(integer_scalar(), &[3]);
    for operator in [
        solve::SolveReductionOperator::Sum,
        solve::SolveReductionOperator::Product,
        solve::SolveReductionOperator::Minimum,
        solve::SolveReductionOperator::Maximum,
    ] {
        let call = owner(
            vec![vector.clone()],
            solve::SolveValueType::scalar(integer_scalar()),
            move |b, v| b.reduce(operator, v[0], span(9321)),
        );
        agrees(
            &call,
            &[
                vec![integers(&[(1 << 53) + 1, -2, 7])],
                vec![integers(&[i64::MAX, 1, -5])],
                vec![integers(&[i64::MIN, -1, 1])],
            ],
        );
    }
    let flags = tensor(solve::SolveScalarType::Boolean, &[3]);
    for operator in [
        solve::SolveReductionOperator::All,
        solve::SolveReductionOperator::Minimum,
        solve::SolveReductionOperator::Maximum,
    ] {
        let call = owner(
            vec![flags.clone()],
            solve::SolveValueType::scalar(solve::SolveScalarType::Boolean),
            move |b, v| b.reduce(operator, v[0], span(9322)),
        );
        let cases = (0..8)
            .map(|bits: u8| {
                vec![
                    (0..3)
                        .map(|bit| solve::SolveValueKind::Boolean(bits >> bit & 1 == 1))
                        .collect(),
                ]
            })
            .collect::<Vec<_>>();
        agrees(&call, &cases);
    }
}

#[test]
fn diagonals_place_each_element_on_the_main_diagonal() {
    let call = owner(
        vec![tensor(real_scalar(), &[3])],
        tensor(real_scalar(), &[3, 3]),
        |b, v| b.diagonal(v[0], span(9330)),
    );
    agrees(&call, &[vec![reals(&[-0.0, f64::NAN, 7.5])]]);
    let call = owner(
        vec![tensor(integer_scalar(), &[2])],
        tensor(integer_scalar(), &[2, 2]),
        |b, v| b.diagonal(v[0], span(9331)),
    );
    agrees(&call, &[vec![integers(&[(1 << 53) + 1, -(1 << 53) - 1])]]);
}

#[test]
fn concatenations_interleave_operand_blocks_in_row_major_order() {
    let row = tensor(real_scalar(), &[1, 3]);
    let block = tensor(real_scalar(), &[2, 3]);
    let call = owner(
        vec![row.clone(), block.clone()],
        tensor(real_scalar(), &[3, 3]),
        |b, v| b.concatenate(0, &[v[0], v[1]], span(9340)),
    );
    agrees(
        &call,
        &[vec![
            reals(&[11.0, 12.0, 13.0]),
            reals(&[21.0, -0.0, f64::NAN, 31.0, 32.0, 33.0]),
        ]],
    );
    let left = tensor(integer_scalar(), &[2, 1]);
    let right = tensor(integer_scalar(), &[2, 2]);
    let call = owner(
        vec![left, right],
        tensor(integer_scalar(), &[2, 3]),
        |b, v| b.concatenate(1, &[v[0], v[1]], span(9341)),
    );
    agrees(
        &call,
        &[vec![
            integers(&[(1 << 53) + 1, -9]),
            integers(&[1, 2, 3, i64::MIN]),
        ]],
    );
}

#[test]
fn element_selection_returns_the_fallback_outside_every_axis() {
    let integer = solve::SolveValueType::scalar(integer_scalar());
    let call = owner(
        vec![
            tensor(integer_scalar(), &[2, 3]),
            integer.clone(),
            integer.clone(),
            integer,
        ],
        solve::SolveValueType::scalar(integer_scalar()),
        |b, v| b.select_element(v[0], &[v[1], v[2]], v[3], span(9350)),
    );
    let matrix = integers(&[1, 2, 3, (1 << 53) + 1, 5, 6]);
    let mut cases = Vec::new();
    for row in [i64::MIN, -1, 0, 1, 2, 3, i64::MAX] {
        for column in [0, 1, 3, 4] {
            cases.push(vec![
                matrix.clone(),
                integers(&[row]),
                integers(&[column]),
                integers(&[-42]),
            ]);
        }
    }
    agrees(&call, &cases);
}
