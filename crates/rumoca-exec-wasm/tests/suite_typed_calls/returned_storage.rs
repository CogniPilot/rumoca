//! Region returns keep simultaneous tuples and earlier output snapshots.
use super::*;

#[test]
fn same_ordinal_private_mask_return_matches_full_domain_and_keeps_public_input() {
    let p = profile();
    let tensor =
        solve::SolveValueType::tensor(solve::SolveScalarType::real(p), vec![14400]).unwrap();
    let mut table = solve::SolvePureCallTable::builder(p);
    let owner = table
        .add_owner(
            identity(82),
            vec![tensor.clone()],
            vec![solve::SolvePureCallOutput::result(tensor.clone()); 2],
            span(850),
            |b, inputs, outputs| {
                let original = b.load(inputs[0], span(851))?;
                let result = b.fold(
                    domain(1, 14400, 1),
                    &[original],
                    &[],
                    span(852),
                    |r, carried, _, binders, outputs| {
                        let old = r.load(carried[0], span(853))?;
                        let index = r.load(binders[0], span(854))?;
                        let two =
                            r.constant(solve::SolveValue::integer(p, 2).unwrap(), span(855))?;
                        let active = r.compare(
                            solve::SolveCompareOperator::LessEqual,
                            index,
                            two,
                            span(856),
                        )?;
                        let next = r.conditional(
                            active,
                            &[old, index],
                            vec![tensor.clone()],
                            span(857),
                            |r, inputs, outputs| {
                                let old = r.load(inputs[0], span(858))?;
                                let index = r.load(inputs[1], span(859))?;
                                let value =
                                    r.constant(solve::SolveValue::real(p, 99.0), span(860))?;
                                let changed = r.update_element(old, value, &[index], span(861))?;
                                r.store(outputs[0], changed, span(862))
                            },
                            |r, inputs, outputs| {
                                let old = r.load(inputs[0], span(863))?;
                                r.store(outputs[0], old, span(864))
                            },
                        )?;
                        r.store(outputs[0], next[0], span(865))
                    },
                )?;
                b.store(outputs[0], result[0], span(866))?;
                b.store(outputs[1], original, span(867))
            },
        )
        .unwrap();
    let site = table.call_site(owner).unwrap();
    let table = table.finish();
    let compiled = compile_pure_call_wasm(&table, &site).unwrap();
    assert_eq!(loop_full_mask_copies(compiled.module_bytes()), 0);
    let mut input = vec![real(-0.0); 14400];
    input[3] = solve::SolveValueKind::Real64(0x7ff8_0000_0000_1234);
    let mut changed = input.clone();
    changed[..2].fill(real(99.0));
    let (status, actual) = Runner::new(&compiled).run(&cells(input.iter().copied()));
    assert_eq!(status, 0);
    assert_eq!(actual, cells(changed.iter().chain(&input).copied()));
    assert_eq!(actual, oracle(&table, &site, &[input]).unwrap());
}

pub(super) fn loop_full_mask_copies(module: &[u8]) -> usize {
    let mut copies = 0;
    for payload in wasmparser::Parser::new(0).parse_all(module) {
        let wasmparser::Payload::CodeSectionEntry(body) = payload.unwrap() else {
            continue;
        };
        let mut blocks = Vec::new();
        let mut previous_size = None;
        for operation in body.get_operators_reader().unwrap() {
            let operation = operation.unwrap();
            if matches!(operation, wasmparser::Operator::MemoryCopy { .. })
                && previous_size == Some(115200)
                && blocks.contains(&true)
            {
                copies += 1;
            }
            previous_size = match operation {
                wasmparser::Operator::I32Const { value } => Some(value),
                _ => None,
            };
            match operation {
                wasmparser::Operator::Loop { .. } => blocks.push(true),
                wasmparser::Operator::Block { .. } | wasmparser::Operator::If { .. } => {
                    blocks.push(false)
                }
                wasmparser::Operator::End => {
                    blocks.pop();
                }
                _ => {}
            }
        }
    }
    copies
}

#[test]
fn cross_ordinal_full_tensor_returns_keep_simultaneous_swap_snapshots() {
    let p = profile();
    let tensor =
        solve::SolveValueType::tensor(solve::SolveScalarType::real(p), vec![14400]).unwrap();
    let mut table = solve::SolvePureCallTable::builder(p);
    let owner = table
        .add_owner(
            identity(83),
            vec![tensor.clone(); 2],
            vec![solve::SolvePureCallOutput::result(tensor); 2],
            span(870),
            |b, inputs, outputs| {
                let a = b.load(inputs[0], span(871))?;
                let z = b.load(inputs[1], span(872))?;
                let result = b.fold(
                    domain(1, 3, 1),
                    &[a, z],
                    &[],
                    span(873),
                    |r, carried, _, _, outputs| {
                        let a = r.load(carried[0], span(874))?;
                        let z = r.load(carried[1], span(875))?;
                        r.store(outputs[0], z, span(876))?;
                        r.store(outputs[1], a, span(877))
                    },
                )?;
                b.store(outputs[0], result[0], span(878))?;
                b.store(outputs[1], result[1], span(879))
            },
        )
        .unwrap();
    let site = table.call_site(owner).unwrap();
    let table = table.finish();
    let compiled = compile_pure_call_wasm(&table, &site).unwrap();
    let a = vec![real(-0.0); 14400];
    let z = vec![solve::SolveValueKind::Real64(0x7ff8_0000_0000_4321); 14400];
    let (status, actual) = Runner::new(&compiled).run(&cells(a.iter().chain(&z).copied()));
    assert_eq!(status, 0);
    assert_eq!(actual, cells(z.iter().chain(&a).copied()));
    assert_eq!(actual, oracle(&table, &site, &[a, z]).unwrap());
}

#[test]
fn earlier_output_snapshot_survives_a_later_nested_private_write() {
    let p = profile();
    let tensor =
        solve::SolveValueType::tensor(solve::SolveScalarType::real(p), vec![14400]).unwrap();
    let mut table = solve::SolvePureCallTable::builder(p);
    let owner = table
        .add_owner(
            identity(84),
            vec![tensor.clone()],
            vec![solve::SolvePureCallOutput::result(tensor.clone()); 2],
            span(880),
            |b, inputs, outputs| {
                let original = b.load(inputs[0], span(881))?;
                let result = b.fold(
                    domain(1, 1, 1),
                    &[original, original],
                    &[],
                    span(882),
                    |r, carried, _, _, outputs| {
                        let old = r.load(carried[0], span(883))?;
                        r.store(outputs[0], old, span(884))?;
                        let active = r.constant(solve::SolveValue::boolean(true), span(885))?;
                        let next = r.conditional(
                            active,
                            &[old],
                            vec![tensor.clone()],
                            span(886),
                            |r, inputs, outputs| {
                                let old = r.load(inputs[0], span(887))?;
                                let index = r.constant(
                                    solve::SolveValue::integer(p, 1).unwrap(),
                                    span(888),
                                )?;
                                let value =
                                    r.constant(solve::SolveValue::real(p, 99.0), span(889))?;
                                let changed = r.update_element(old, value, &[index], span(890))?;
                                r.store(outputs[0], changed, span(891))
                            },
                            |r, inputs, outputs| {
                                let old = r.load(inputs[0], span(892))?;
                                r.store(outputs[0], old, span(893))
                            },
                        )?;
                        r.store(outputs[1], next[0], span(894))
                    },
                )?;
                b.store(outputs[0], result[0], span(895))?;
                b.store(outputs[1], result[1], span(896))
            },
        )
        .unwrap();
    let site = table.call_site(owner).unwrap();
    let table = table.finish();
    let compiled = compile_pure_call_wasm(&table, &site).unwrap();
    let input = vec![real(-0.0); 14400];
    let mut changed = input.clone();
    changed[0] = real(99.0);
    let (status, actual) = Runner::new(&compiled).run(&cells(input.iter().copied()));
    assert_eq!(status, 0);
    assert_eq!(actual, cells(input.iter().chain(&changed).copied()));
    assert_eq!(actual, oracle(&table, &site, &[input]).unwrap());
}
