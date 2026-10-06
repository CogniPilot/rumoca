//! Whole-tuple region snapshots before any transition storage is published.
use super::*;

fn alias_table(count: i64) -> (solve::SolvePureCallTable, solve::SolvePureCallSite) {
    let p = profile();
    let tensor =
        solve::SolveValueType::tensor(solve::SolveScalarType::real(p), vec![14400]).unwrap();
    let integer = solve::SolveValueType::scalar(solve::SolveScalarType::integer(p));
    let mut table = solve::SolvePureCallTable::builder(p);
    let owner = table
        .add_owner(
            identity(40),
            vec![tensor.clone(), integer],
            vec![solve::SolvePureCallOutput::result(tensor); 3],
            span(400),
            |b, inputs, outputs| {
                let original = b.load(inputs[0], span(401))?;
                let final_index = b.load(inputs[1], span(402))?;
                let result = b.fold(
                    domain(1, count, 1),
                    &[original, original],
                    &[original, final_index],
                    span(403),
                    |r, carried, captures, binders, outputs| {
                        let old = r.load(carried[0], span(404))?;
                        let index = r.load(binders[0], span(405))?;
                        let final_index = r.load(captures[1], span(406))?;
                        let three =
                            r.constant(solve::SolveValue::integer(p, 3).unwrap(), span(407))?;
                        let final_step =
                            r.compare(solve::SolveCompareOperator::Equal, index, three, span(408))?;
                        let target = r.select(final_step, final_index, index, span(409))?;
                        let value = r.constant(solve::SolveValue::real(p, 99.0), span(410))?;
                        let changed = r.update_element(old, value, &[target], span(411))?;
                        r.store(outputs[0], changed, span(412))?;
                        // Output zero cannot mutate the old first tuple element.
                        let old_again = r.load(carried[0], span(413))?;
                        r.store(outputs[1], old_again, span(414))
                    },
                )?;
                b.store(outputs[0], result[0], span(415))?;
                b.store(outputs[1], result[1], span(416))?;
                b.store(outputs[2], original, span(417))
            },
        )
        .unwrap();
    let site = table.call_site(owner).unwrap();
    (table.finish(), site)
}

#[test]
fn full_raster_carried_alias_capture_late_fault_atomicity_and_recovery() {
    let (table, site) = alias_table(3);
    let compiled = compile_pure_call_wasm(&table, &site).unwrap();
    let mut runner = Runner::new(&compiled);
    let original = (0..14400)
        .map(|i| real(if i == 0 { -0.0 } else { i as f64 }))
        .collect::<Vec<_>>();
    for target in [3, 14401, 3] {
        let inputs = vec![
            original.clone(),
            vec![solve::SolveValueKind::Integer(target)],
        ];
        let (status, actual) = runner.run(&cells(inputs.iter().flatten().copied()));
        match oracle(&table, &site, &inputs) {
            Ok(expected) => {
                assert_eq!(status, 0);
                assert_eq!(actual, expected);
            }
            Err(_) => {
                assert_eq!(actual, vec![0xa5; 14400 * 8 * 3]);
                let fault = compiled
                    .faults()
                    .iter()
                    .find(|f| f.status == status as u32)
                    .unwrap();
                assert_eq!(fault.provenance, span(411));
                assert_eq!(fault.kind, TypedCallFaultKind::IndexBounds);
                assert_eq!(fault.region_path, [(2, 0)]);
            }
        }
    }
    assert!(compiled.layout().scratch_bytes < 14400 * 8 * 12);
    assert!(compiled.module_bytes().len() < 4096);
}

#[test]
fn empty_full_raster_carried_fold_does_not_evaluate_transition() {
    let (table, site) = alias_table(0);
    let compiled = compile_pure_call_wasm(&table, &site).unwrap();
    let inputs = vec![
        vec![real(-0.0); 14400],
        vec![solve::SolveValueKind::Integer(14401)],
    ];
    let (status, actual) = Runner::new(&compiled).run(&cells(inputs.iter().flatten().copied()));
    assert_eq!(status, 0);
    assert_eq!(actual, oracle(&table, &site, &inputs).unwrap());
}

fn moments_table() -> (solve::SolvePureCallTable, solve::SolvePureCallSite) {
    let p = profile();
    let raster =
        solve::SolveValueType::tensor(solve::SolveScalarType::real(p), vec![14400]).unwrap();
    let moments = solve::SolveValueType::tensor(solve::SolveScalarType::real(p), vec![3]).unwrap();
    let mut table = solve::SolvePureCallTable::builder(p);
    let owner = table
        .add_owner(
            identity(41),
            vec![raster, moments.clone()],
            vec![solve::SolvePureCallOutput::result(moments)],
            span(420),
            |b, inputs, outputs| {
                let raster = b.load(inputs[0], span(421))?;
                let initial = b.load(inputs[1], span(422))?;
                let result = b.fold(
                    domain(1, 14400, 1),
                    &[initial],
                    &[raster],
                    span(423),
                    |r, carried, captures, binders, outputs| {
                        let old = r.load(carried[0], span(424))?;
                        let raster = r.load(captures[0], span(425))?;
                        let index = r.load(binders[0], span(426))?;
                        let value = r.project_element_dynamic(raster, &[index], span(427))?;
                        let one =
                            r.constant(solve::SolveValue::integer(p, 1).unwrap(), span(428))?;
                        let two =
                            r.constant(solve::SolveValue::integer(p, 2).unwrap(), span(429))?;
                        let three =
                            r.constant(solve::SolveValue::integer(p, 3).unwrap(), span(430))?;
                        let previous = r.project_element(old, vec![0], span(431))?;
                        let total =
                            r.binary(solve::SolveBinaryOperator::Add, previous, value, span(432))?;
                        let changed = r.update_element(old, previous, &[two], span(433))?;
                        let changed = r.update_element(changed, total, &[one], span(434))?;
                        let secondary = r.project_element(old, vec![2], span(435))?;
                        let sum =
                            r.binary(solve::SolveBinaryOperator::Add, secondary, value, span(436))?;
                        let changed = r.update_element(changed, sum, &[three], span(437))?;
                        r.store(outputs[0], changed, span(438))
                    },
                )?;
                b.store(outputs[0], result[0], span(439))
            },
        )
        .unwrap();
    let site = table.call_site(owner).unwrap();
    (table.finish(), site)
}

#[test]
fn full_14400_raster_capture_small_tensor_carry_preserves_order_and_old_snapshots() {
    let (table, site) = moments_table();
    let compiled = compile_pure_call_wasm(&table, &site).unwrap();
    let mut runner = Runner::new(&compiled);
    for pattern in [[1e16, 1.0, -1e16, 3.0], [-0.0; 4], [0.5, 1.0, 2.0, -4.0]] {
        let values = (0..14400).map(|i| pattern[i % 4]).collect::<Vec<_>>();
        let mut expected = [-0.0, -0.0, -0.0];
        for value in &values {
            expected[1] = expected[0];
            expected[0] += value;
            expected[2] += value;
        }
        let inputs = vec![values.into_iter().map(real).collect(), vec![real(-0.0); 3]];
        let (status, actual) = runner.run(&cells(inputs.iter().flatten().copied()));
        assert_eq!(status, 0);
        assert_eq!(actual, cells(expected.into_iter().map(real)));
        assert_eq!(actual, oracle(&table, &site, &inputs).unwrap());
    }
    assert!(compiled.layout().scratch_bytes < 1024);
    assert!(compiled.module_bytes().len() < 4096);
    eprintln!(
        "AGGREGATE_FULL14400 module_bytes={} scratch_bytes={}",
        compiled.module_bytes().len(),
        compiled.layout().scratch_bytes
    );
}

#[test]
fn nested_aggregate_fold_and_lazy_branch_keep_outer_snapshot() {
    let p = profile();
    let tensor = solve::SolveValueType::tensor(solve::SolveScalarType::real(p), vec![3]).unwrap();
    let mut table = solve::SolvePureCallTable::builder(p);
    let owner = table
        .add_owner(
            identity(42),
            vec![tensor.clone()],
            vec![solve::SolvePureCallOutput::result(tensor.clone()); 2],
            span(440),
            |b, inputs, outputs| {
                let input = b.load(inputs[0], span(441))?;
                let result = b.fold(
                    domain(1, 2, 1),
                    &[input],
                    &[input],
                    span(442),
                    |r, carried, captures, _, outputs| {
                        let old = r.load(carried[0], span(443))?;
                        let capture = r.load(captures[0], span(444))?;
                        let inner = r.fold(
                            domain(1, 3, 1),
                            &[old],
                            &[capture],
                            span(445),
                            |r, carried, captures, binders, outputs| {
                                let old = r.load(carried[0], span(446))?;
                                let snapshot = r.load(captures[0], span(447))?;
                                let index = r.load(binders[0], span(448))?;
                                let value =
                                    r.project_element_dynamic(snapshot, &[index], span(449))?;
                                let condition =
                                    r.constant(solve::SolveValue::boolean(true), span(450))?;
                                let changed = r.conditional(
                                    condition,
                                    &[old, value, index],
                                    vec![tensor.clone()],
                                    span(451),
                                    selected_tensor_update,
                                    refused_tensor_update,
                                )?;
                                r.store(outputs[0], changed[0], span(462))
                            },
                        )?;
                        r.store(outputs[0], inner[0], span(463))
                    },
                )?;
                b.store(outputs[0], result[0], span(464))?;
                b.store(outputs[1], input, span(465))
            },
        )
        .unwrap();
    let site = table.call_site(owner).unwrap();
    let table = table.finish();
    let compiled = compile_pure_call_wasm(&table, &site).unwrap();
    let inputs = vec![vec![real(-0.0), real(7.5), real(-3.0)]];
    let (status, actual) = Runner::new(&compiled).run(&cells(inputs.iter().flatten().copied()));
    assert_eq!(status, 0);
    assert_eq!(actual, oracle(&table, &site, &inputs).unwrap());
    assert_eq!(
        actual,
        cells(
            inputs
                .iter()
                .flatten()
                .chain(inputs.iter().flatten())
                .copied()
        )
    );
}

fn selected_tensor_update<'region>(
    r: &mut solve::TypedProgramBuilder<'region>,
    inputs: &[solve::ProgramSlot<'region>],
    outputs: &[solve::ProgramSlot<'region>],
) -> Result<(), solve::SolveProgramConstructionError> {
    let old = r.load(inputs[0], span(452))?;
    let value = r.load(inputs[1], span(453))?;
    let index = r.load(inputs[2], span(454))?;
    let changed = r.update_element(old, value, &[index], span(455))?;
    r.store(outputs[0], changed, span(456))
}

fn refused_tensor_update<'region>(
    r: &mut solve::TypedProgramBuilder<'region>,
    inputs: &[solve::ProgramSlot<'region>],
    outputs: &[solve::ProgramSlot<'region>],
) -> Result<(), solve::SolveProgramConstructionError> {
    let old = r.load(inputs[0], span(457))?;
    let invalid = r.constant(solve::SolveValue::integer(profile(), 4).unwrap(), span(458))?;
    let value = r.load(inputs[1], span(459))?;
    let changed = r.update_element(old, value, &[invalid], span(460))?;
    r.store(outputs[0], changed, span(461))
}
