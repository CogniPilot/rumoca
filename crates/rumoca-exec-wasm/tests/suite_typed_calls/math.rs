//! Source-owned math calls preserve binary64 reference evaluation and checked cells.
mod bindings;
use super::*;
pub(crate) use bindings::bind;

const UNARY: [solve::SolveUnaryOperator; 12] = [
    solve::SolveUnaryOperator::Sin,
    solve::SolveUnaryOperator::Cos,
    solve::SolveUnaryOperator::Tan,
    solve::SolveUnaryOperator::Asin,
    solve::SolveUnaryOperator::Acos,
    solve::SolveUnaryOperator::Atan,
    solve::SolveUnaryOperator::Sinh,
    solve::SolveUnaryOperator::Cosh,
    solve::SolveUnaryOperator::Tanh,
    solve::SolveUnaryOperator::Exp,
    solve::SolveUnaryOperator::Log,
    solve::SolveUnaryOperator::Log10,
];

fn vector_math(count: u32) -> (solve::SolvePureCallTable, solve::SolvePureCallSite) {
    let p = profile();
    let tensor =
        solve::SolveValueType::tensor(solve::SolveScalarType::real(p), vec![count]).unwrap();
    let mut table = solve::SolvePureCallTable::builder(p);
    let owner = table
        .add_owner(
            identity(11001),
            vec![tensor.clone(); 2],
            vec![solve::SolvePureCallOutput::result(tensor); 14],
            span(11001),
            |b, inputs, outputs| {
                let lhs = b.load(inputs[0], span(11002))?;
                let rhs = b.load(inputs[1], span(11003))?;
                for (i, operator) in UNARY.into_iter().enumerate() {
                    let result = b.unary(operator, lhs, span(11004 + i))?;
                    b.store(outputs[i], result, span(11030 + i))?;
                }
                let power = b.binary(solve::SolveBinaryOperator::Power, lhs, rhs, span(11050))?;
                let angle = b.binary(solve::SolveBinaryOperator::Atan2, lhs, rhs, span(11051))?;
                b.store(outputs[12], power, span(11052))?;
                b.store(outputs[13], angle, span(11053))
            },
        )
        .unwrap();
    let site = table.call_site(owner).unwrap();
    (table.finish(), site)
}

#[test]
fn typed_real_math_imports_match_canonical_full_ieee_cells_and_declared_arities() {
    let patterns = [
        0,
        0x8000_0000_0000_0000,
        1,
        0x8000_0000_0000_0001,
        (-2.0f64).to_bits(),
        (-1.0f64).to_bits(),
        (-0.5f64).to_bits(),
        0.5f64.to_bits(),
        1.0f64.to_bits(),
        2.0f64.to_bits(),
        f64::MAX.to_bits(),
        f64::INFINITY.to_bits(),
        f64::NEG_INFINITY.to_bits(),
        0x7ff8_dead_beef_1234,
        0xfff8_dead_beef_1234,
        0x7ff0_0000_0000_0001,
    ];
    for count in [1, 16, 14400] {
        let (table, site) = vector_math(count);
        let compiled = compile_pure_call_wasm(&table, &site).unwrap();
        assert_eq!(
            compiled.math_imports(),
            [
                "sin", "cos", "tan", "asin", "acos", "atan", "sinh", "cosh", "tanh", "exp", "log",
                "log10", "pow", "atan2"
            ]
        );
        let mut runner = Runner::new(&compiled);
        let lhs = (0..count as usize)
            .map(|i| real(f64::from_bits(patterns[i % patterns.len()])))
            .collect::<Vec<_>>();
        let rhs = (0..count as usize)
            .map(|i| real(f64::from_bits(patterns[(i + 3) % patterns.len()])))
            .collect::<Vec<_>>();
        let inputs = vec![lhs, rhs];
        let actual = runner.run(&cells(inputs.iter().flatten().copied()));
        assert_eq!(actual, (0, oracle(&table, &site, &inputs).unwrap()));
    }
}

/// Unreachable Tan is excluded; reachable Fold/Conditional and nested call imports are retained.
pub(crate) fn nested_math() -> (solve::SolvePureCallTable, solve::SolvePureCallSite) {
    let p = profile();
    let real_type = solve::SolveValueType::scalar(solve::SolveScalarType::real(p));
    let integer = solve::SolveValueType::scalar(solve::SolveScalarType::integer(p));
    let mut table = solve::SolvePureCallTable::builder(p);
    add_unary_owner(&mut table, 11100, solve::SolveUnaryOperator::Tan);
    let child = add_unary_owner(&mut table, 11101, solve::SolveUnaryOperator::Cos);
    let root = table
        .add_owner(
            identity(11102),
            vec![real_type.clone()],
            vec![
                solve::SolvePureCallOutput::result(real_type),
                solve::SolvePureCallOutput::result(integer),
            ],
            span(11102),
            |b, inputs, outputs| {
                let value = b.load(inputs[0], span(11103))?;
                let initial = b.constant(solve::SolveValue::real(p, 0.0), span(11104))?;
                let folded = b.fold(
                    domain(1, 2, 1),
                    &[initial],
                    &[value],
                    span(11105),
                    |r, carried, captures, _, outputs| {
                        let old = r.load(carried[0], span(11106))?;
                        let value = r.load(captures[0], span(11107))?;
                        let result = r.call(child, &[value], span(11108))?;
                        let sum =
                            r.binary(solve::SolveBinaryOperator::Add, old, result[0], span(11109))?;
                        r.store(outputs[0], sum, span(11110))
                    },
                )?;
                let log = b.unary(solve::SolveUnaryOperator::Log, value, span(11111))?;
                let power =
                    b.binary(solve::SolveBinaryOperator::Power, value, value, span(11112))?;
                let sum = b.binary(solve::SolveBinaryOperator::Add, folded[0], log, span(11113))?;
                let sum = b.binary(solve::SolveBinaryOperator::Add, sum, power, span(11114))?;
                b.store(outputs[0], sum, span(11115))?;
                let converted = b.convert(
                    solve::SolveConversionOperator::RealToIntegerTowardZero,
                    value,
                    span(11116),
                )?;
                b.store(outputs[1], converted, span(11117))
            },
        )
        .unwrap();
    let site = table.call_site(root).unwrap();
    (table.finish(), site)
}

fn add_unary_owner(
    table: &mut solve::SolvePureCallTableBuilder,
    id: u64,
    operator: solve::SolveUnaryOperator,
) -> solve::SolvePureCallOwnerId {
    let p = profile();
    let real_type = solve::SolveValueType::scalar(solve::SolveScalarType::real(p));
    table
        .add_owner(
            identity(id),
            vec![real_type.clone()],
            vec![solve::SolvePureCallOutput::result(real_type)],
            span(id as usize),
            |b, inputs, outputs| {
                let value = b.load(inputs[0], span(id as usize + 1))?;
                let output = b.unary(operator, value, span(id as usize + 2))?;
                b.store(outputs[0], output, span(id as usize + 3))
            },
        )
        .unwrap()
}

#[test]
fn reachable_nested_math_catalog_fault_provenance_atomicity_and_recovery() {
    let (table, site) = nested_math();
    let compiled = compile_pure_call_wasm(&table, &site).unwrap();
    assert_eq!(compiled.math_imports(), ["cos", "log", "pow"]);
    let mut runner = Runner::new(&compiled);
    for value in [0.5, f64::INFINITY, f64::NAN, -0.0, 2.0] {
        let inputs = vec![vec![real(value)]];
        let (status, actual) = runner.run(&cells([real(value)]));
        match oracle(&table, &site, &inputs) {
            Ok(expected) => assert_eq!((status, actual), (0, expected)),
            Err(error) => {
                let fault = compiled
                    .faults()
                    .iter()
                    .find(|f| f.status == status as u32)
                    .unwrap();
                assert_eq!(fault.kind, TypedCallFaultKind::IntegerConversion);
                assert_eq!(fault.provenance, span(11116));
                assert_eq!(error.source_span(), Some(fault.provenance));
                assert_eq!(actual, vec![0xa5; compiled.layout().output_bytes as usize]);
            }
        }
    }
}

#[test]
fn both_conditional_regions_issue_math_imports_without_eager_execution() {
    let p = profile();
    let real_type = solve::SolveValueType::scalar(solve::SolveScalarType::real(p));
    let mut table = solve::SolvePureCallTable::builder(p);
    let owner = table
        .add_owner(
            identity(11300),
            vec![real_type.clone()],
            vec![solve::SolvePureCallOutput::result(real_type.clone())],
            span(11300),
            |b, inputs, outputs| {
                let value = b.load(inputs[0], span(11301))?;
                let zero = b.constant(solve::SolveValue::real(p, 0.0), span(11302))?;
                let positive = b.compare(
                    solve::SolveCompareOperator::Greater,
                    value,
                    zero,
                    span(11303),
                )?;
                let result = b.conditional(
                    positive,
                    &[value],
                    vec![real_type],
                    span(11304),
                    |r, inputs, outputs| {
                        conditional_unary(r, inputs, outputs, solve::SolveUnaryOperator::Exp)
                    },
                    |r, inputs, outputs| {
                        conditional_unary(r, inputs, outputs, solve::SolveUnaryOperator::Tanh)
                    },
                )?;
                b.store(outputs[0], result[0], span(11305))
            },
        )
        .unwrap();
    let site = table.call_site(owner).unwrap();
    let table = table.finish();
    let compiled = compile_pure_call_wasm(&table, &site).unwrap();
    assert_eq!(compiled.math_imports(), ["tanh", "exp"]);
    let mut runner = Runner::new(&compiled);
    for value in [-1.0, -0.0, 0.0, 1.0, f64::INFINITY, f64::NAN] {
        let inputs = vec![vec![real(value)]];
        assert_eq!(
            runner.run(&cells([real(value)])),
            (0, oracle(&table, &site, &inputs).unwrap())
        );
    }
}

fn conditional_unary<'program>(
    b: &mut solve::TypedProgramBuilder<'program>,
    inputs: &[solve::ProgramSlot<'program>],
    outputs: &[solve::ProgramSlot<'program>],
    operator: solve::SolveUnaryOperator,
) -> Result<(), solve::SolveProgramConstructionError> {
    let value = b.load(inputs[0], span(11306))?;
    let result = b.unary(operator, value, span(11307))?;
    b.store(outputs[0], result, span(11308))
}
