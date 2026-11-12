use super::*;
use crate::SolveCompareOperator;
use rumoca_core::{IndexBinderProvenance, StructuredIndexBinder, StructuredIndexDomain};

#[test]
fn fixed_coordinate_survives_fold_and_conditional() {
    let vector = real_shape(vec![3]);
    let real = real_shape(vec![]);
    let table = SolvePureCallTable::construct(profile(), |table| {
        table.add_owner(
            identity(1100),
            vec![vector],
            vec![SolvePureCallOutput::result(real.clone())],
            span(1100),
            |builder, inputs, outputs| {
                let p = builder.load(inputs[0], span(1101))?;
                let zero = builder.constant(SolveValue::real(profile(), 0.0), span(1102))?;
                let values = builder.fold(
                    StructuredIndexDomain {
                        binders: vec![StructuredIndexBinder {
                            provenance: IndexBinderProvenance::Generated,
                            id: 0,
                            display_name: "i".into(),
                            lower: 1,
                            upper: 2,
                            step: 1,
                        }],
                    },
                    &[zero],
                    &[p],
                    span(1103),
                    |region, carried, captures, _, result| {
                        let p = region.load(captures[0], span(1104))?;
                        let carry = region.load(carried[0], span(1105))?;
                        let z = region.project_element(p, vec![2], span(1106))?;
                        let zero = region.constant(SolveValue::real(profile(), 0.0), span(1107))?;
                        let condition =
                            region.compare(SolveCompareOperator::Greater, z, zero, span(1108))?;
                        let values = region.conditional(
                            condition,
                            &[p, carry],
                            vec![real.clone()],
                            span(1109),
                            active_branch,
                            carry_branch,
                        )?;
                        region.store(result[0], values[0], span(1117))
                    },
                )?;
                builder.store(outputs[0], values[0], span(1118))
            },
        )?;
        Ok(())
    })
    .unwrap();
    let actual = output_coordinates(table.owners()[0].call_site(), false);
    assert_eq!(actual, vec![BTreeSet::from([2])]);
}

#[test]
fn fold_keeps_a_distinct_conditional_predicate_coordinate() {
    let vector = real_shape(vec![3]);
    let real = real_shape(vec![]);
    let table = SolvePureCallTable::construct(profile(), |table| {
        table.add_owner(
            identity(1100),
            vec![vector],
            vec![SolvePureCallOutput::result(real.clone())],
            span(1100),
            |builder, inputs, outputs| {
                let p = builder.load(inputs[0], span(1101))?;
                let zero = builder.constant(SolveValue::real(profile(), 0.0), span(1102))?;
                let values = builder.fold(
                    StructuredIndexDomain {
                        binders: vec![StructuredIndexBinder {
                            provenance: IndexBinderProvenance::Generated,
                            id: 0,
                            display_name: "i".into(),
                            lower: 1,
                            upper: 2,
                            step: 1,
                        }],
                    },
                    &[zero],
                    &[p],
                    span(1103),
                    |region, carried, captures, _, result| {
                        let p = region.load(captures[0], span(1104))?;
                        let carry = region.load(carried[0], span(1105))?;
                        let z = region.project_element(p, vec![0], span(1106))?;
                        let zero = region.constant(SolveValue::real(profile(), 0.0), span(1107))?;
                        let condition =
                            region.compare(SolveCompareOperator::Greater, z, zero, span(1108))?;
                        let values = region.conditional(
                            condition,
                            &[p, carry],
                            vec![real.clone()],
                            span(1109),
                            active_branch,
                            carry_branch,
                        )?;
                        region.store(result[0], values[0], span(1117))
                    },
                )?;
                builder.store(outputs[0], values[0], span(1118))
            },
        )?;
        Ok(())
    })
    .unwrap();
    let actual = output_coordinates(table.owners()[0].call_site(), false);
    assert_eq!(actual, vec![BTreeSet::from([0, 2])]);
}

#[test]
fn fold_keeps_both_conditional_branch_read_sets() {
    let vector = real_shape(vec![3]);
    let real = real_shape(vec![]);
    let table = SolvePureCallTable::construct(profile(), |table| {
        table.add_owner(
            identity(1100),
            vec![vector],
            vec![SolvePureCallOutput::result(real.clone())],
            span(1100),
            |builder, inputs, outputs| {
                let p = builder.load(inputs[0], span(1101))?;
                let zero = builder.constant(SolveValue::real(profile(), 0.0), span(1102))?;
                let values = builder.fold(
                    StructuredIndexDomain {
                        binders: vec![StructuredIndexBinder {
                            provenance: IndexBinderProvenance::Generated,
                            id: 0,
                            display_name: "i".into(),
                            lower: 1,
                            upper: 2,
                            step: 1,
                        }],
                    },
                    &[zero],
                    &[p],
                    span(1103),
                    |region, carried, captures, _, result| {
                        let p = region.load(captures[0], span(1104))?;
                        let carry = region.load(carried[0], span(1105))?;
                        let z = region.project_element(p, vec![2], span(1106))?;
                        let zero = region.constant(SolveValue::real(profile(), 0.0), span(1107))?;
                        let condition =
                            region.compare(SolveCompareOperator::Greater, z, zero, span(1108))?;
                        let values = region.conditional(
                            condition,
                            &[p, carry],
                            vec![real.clone()],
                            span(1109),
                            active_branch,
                            alternate_branch,
                        )?;
                        region.store(result[0], values[0], span(1117))
                    },
                )?;
                builder.store(outputs[0], values[0], span(1118))
            },
        )?;
        Ok(())
    })
    .unwrap();
    let actual = output_coordinates(table.owners()[0].call_site(), false);
    assert_eq!(actual, vec![BTreeSet::from([1, 2])]);
}

#[test]
fn fold_keeps_full_reduction_read_set() {
    let vector = real_shape(vec![3]);
    let real = real_shape(vec![]);
    let table = SolvePureCallTable::construct(profile(), |table| {
        table.add_owner(
            identity(1100),
            vec![vector],
            vec![SolvePureCallOutput::result(real.clone())],
            span(1100),
            |builder, inputs, outputs| {
                let p = builder.load(inputs[0], span(1101))?;
                let zero = builder.constant(SolveValue::real(profile(), 0.0), span(1102))?;
                let values = builder.fold(
                    StructuredIndexDomain {
                        binders: vec![StructuredIndexBinder {
                            provenance: IndexBinderProvenance::Generated,
                            id: 0,
                            display_name: "i".into(),
                            lower: 1,
                            upper: 2,
                            step: 1,
                        }],
                    },
                    &[zero],
                    &[p],
                    span(1103),
                    |region, carried, captures, _, result| {
                        let p = region.load(captures[0], span(1104))?;
                        let carry = region.load(carried[0], span(1105))?;
                        let z = region.reduce(crate::SolveReductionOperator::Sum, p, span(1106))?;
                        let zero = region.constant(SolveValue::real(profile(), 0.0), span(1107))?;
                        let condition =
                            region.compare(SolveCompareOperator::Greater, z, zero, span(1108))?;
                        let values = region.conditional(
                            condition,
                            &[p, carry],
                            vec![real.clone()],
                            span(1109),
                            active_branch,
                            carry_branch,
                        )?;
                        region.store(result[0], values[0], span(1117))
                    },
                )?;
                builder.store(outputs[0], values[0], span(1118))
            },
        )?;
        Ok(())
    })
    .unwrap();
    let actual = output_coordinates(table.owners()[0].call_site(), false);
    assert_eq!(actual, vec![BTreeSet::from([0, 1, 2])]);
}

#[test]
fn nested_scalar_call_preserves_projected_source_coordinate() {
    let real = real_shape(vec![]);
    let table = SolvePureCallTable::construct(profile(), |table| {
        let inner = table.add_owner(
            identity(1200),
            vec![real.clone()],
            vec![SolvePureCallOutput::result(real.clone())],
            span(1200),
            |builder, inputs, outputs| {
                let x = builder.load(inputs[0], span(1201))?;
                builder.store(outputs[0], x, span(1203))
            },
        )?;
        table.add_owner(
            identity(1201),
            vec![real_shape(vec![3])],
            vec![SolvePureCallOutput::result(real.clone())],
            span(1204),
            |builder, inputs, outputs| {
                let p = builder.load(inputs[0], span(1205))?;
                let z = builder.project_element(p, vec![2], span(1206))?;
                let values = builder.call(inner, &[z], span(1207))?;
                builder.store(outputs[0], values[0], span(1208))
            },
        )?;
        Ok(())
    })
    .unwrap();
    assert_eq!(
        output_coordinates(table.owners()[1].call_site(), false),
        vec![BTreeSet::from([2])]
    );
}

fn active_branch<'p>(
    branch: &mut crate::TypedProgramBuilder<'p>,
    captures: &[crate::ProgramSlot<'p>],
    result: &[crate::ProgramSlot<'p>],
) -> Result<(), SolveProgramConstructionError> {
    let p = branch.load(captures[0], span(1110))?;
    let carry = branch.load(captures[1], span(1111))?;
    let z = branch.project_element(p, vec![2], span(1112))?;
    let sum = branch.binary(SolveBinaryOperator::Add, carry, z, span(1113))?;
    branch.store(result[0], sum, span(1114))
}
fn carry_branch<'p>(
    branch: &mut crate::TypedProgramBuilder<'p>,
    captures: &[crate::ProgramSlot<'p>],
    result: &[crate::ProgramSlot<'p>],
) -> Result<(), SolveProgramConstructionError> {
    let carry = branch.load(captures[1], span(1115))?;
    branch.store(result[0], carry, span(1116))
}
fn alternate_branch<'p>(
    branch: &mut crate::TypedProgramBuilder<'p>,
    captures: &[crate::ProgramSlot<'p>],
    result: &[crate::ProgramSlot<'p>],
) -> Result<(), SolveProgramConstructionError> {
    let p = branch.load(captures[0], span(1115))?;
    let carry = branch.project_element(p, vec![1], span(1115))?;
    branch.store(result[0], carry, span(1116))
}

fn three_passes() -> StructuredIndexDomain {
    StructuredIndexDomain {
        binders: vec![StructuredIndexBinder {
            provenance: IndexBinderProvenance::Generated,
            id: 0,
            display_name: "i".into(),
            lower: 1,
            upper: 3,
            step: 1,
        }],
    }
}

#[test]
fn fold_carry_reads_only_the_carries_it_reaches_and_their_captures() {
    // a' = a + x, b' = b + y, c' = c + a: `c` reaches `a` and so reads `x`;
    // nothing reaches `b`, so no other carry reads `y`.
    let real = real_shape(vec![]);
    let table = SolvePureCallTable::construct(profile(), |table| {
        table.add_owner(
            identity(1300),
            vec![real.clone(), real.clone()],
            vec![
                SolvePureCallOutput::result(real.clone()),
                SolvePureCallOutput::result(real.clone()),
                SolvePureCallOutput::result(real.clone()),
            ],
            span(1300),
            |builder, inputs, outputs| {
                let x = builder.load(inputs[0], span(1301))?;
                let y = builder.load(inputs[1], span(1302))?;
                let zero = builder.constant(SolveValue::real(profile(), 0.0), span(1303))?;
                let values = builder.fold(
                    three_passes(),
                    &[zero, zero, zero],
                    &[x, y],
                    span(1304),
                    |region, carried, captures, _, result| {
                        let a = region.load(carried[0], span(1305))?;
                        let b = region.load(carried[1], span(1306))?;
                        let c = region.load(carried[2], span(1307))?;
                        let x = region.load(captures[0], span(1308))?;
                        let y = region.load(captures[1], span(1309))?;
                        let a_next = region.binary(SolveBinaryOperator::Add, a, x, span(1310))?;
                        let b_next = region.binary(SolveBinaryOperator::Add, b, y, span(1311))?;
                        let c_next = region.binary(SolveBinaryOperator::Add, c, a, span(1312))?;
                        region.store(result[0], a_next, span(1313))?;
                        region.store(result[1], b_next, span(1314))?;
                        region.store(result[2], c_next, span(1315))
                    },
                )?;
                builder.store(outputs[0], values[0], span(1316))?;
                builder.store(outputs[1], values[1], span(1317))?;
                builder.store(outputs[2], values[2], span(1318))
            },
        )?;
        Ok(())
    })
    .unwrap();
    let leaves = input_leaves(table.owners()[0].call_site().output_dependencies());
    assert_eq!(
        leaves,
        vec![
            vec![0].into_boxed_slice(),
            vec![1].into_boxed_slice(),
            vec![0].into_boxed_slice()
        ]
    );
}

#[test]
fn fold_carry_that_reads_every_carry_reads_every_capture() {
    // c' = c + a + b reaches both other carries, so it reads both captures.
    let real = real_shape(vec![]);
    let table = SolvePureCallTable::construct(profile(), |table| {
        table.add_owner(
            identity(1320),
            vec![real.clone(), real.clone()],
            vec![
                SolvePureCallOutput::result(real.clone()),
                SolvePureCallOutput::result(real.clone()),
                SolvePureCallOutput::result(real.clone()),
            ],
            span(1320),
            |builder, inputs, outputs| {
                let x = builder.load(inputs[0], span(1321))?;
                let y = builder.load(inputs[1], span(1322))?;
                let zero = builder.constant(SolveValue::real(profile(), 0.0), span(1323))?;
                let values = builder.fold(
                    three_passes(),
                    &[zero, zero, zero],
                    &[x, y],
                    span(1324),
                    |region, carried, captures, _, result| {
                        let a = region.load(carried[0], span(1325))?;
                        let b = region.load(carried[1], span(1326))?;
                        let c = region.load(carried[2], span(1327))?;
                        let x = region.load(captures[0], span(1328))?;
                        let y = region.load(captures[1], span(1329))?;
                        let a_next = region.binary(SolveBinaryOperator::Add, a, x, span(1330))?;
                        let b_next = region.binary(SolveBinaryOperator::Add, b, y, span(1331))?;
                        let ab = region.binary(SolveBinaryOperator::Add, a, b, span(1332))?;
                        let c_next = region.binary(SolveBinaryOperator::Add, c, ab, span(1333))?;
                        region.store(result[0], a_next, span(1334))?;
                        region.store(result[1], b_next, span(1335))?;
                        region.store(result[2], c_next, span(1336))
                    },
                )?;
                builder.store(outputs[0], values[0], span(1337))?;
                builder.store(outputs[1], values[1], span(1338))?;
                builder.store(outputs[2], values[2], span(1339))
            },
        )?;
        Ok(())
    })
    .unwrap();
    let leaves = input_leaves(table.owners()[0].call_site().output_dependencies());
    assert_eq!(leaves[2], vec![0, 1].into_boxed_slice());
}

fn passes(upper: i64) -> StructuredIndexDomain {
    StructuredIndexDomain {
        binders: vec![StructuredIndexBinder {
            provenance: IndexBinderProvenance::Generated,
            id: 0,
            display_name: "i".into(),
            lower: 1,
            upper,
            step: 1,
        }],
    }
}

/// `r := y; for i in 1:count loop r[i] := x[i]; r[2 + i] := r[2 + i] * x[i];
/// end for` over `x[2]` (scalars 0, 1) and `y[4]` (scalars 2..5).
fn rows_written_at_binder_offsets(count: i64) -> Vec<BTreeSet<usize>> {
    let table = SolvePureCallTable::construct(profile(), |table| {
        table.add_owner(
            identity(1400),
            vec![real_shape(vec![2]), real_shape(vec![4])],
            vec![SolvePureCallOutput::result(real_shape(vec![4]))],
            span(1400),
            |builder, inputs, outputs| {
                let x = builder.load(inputs[0], span(1401))?;
                let y = builder.load(inputs[1], span(1402))?;
                let values = builder.fold(
                    passes(count),
                    &[y],
                    &[x],
                    span(1403),
                    |region, carried, captures, binders, result| {
                        let rows = region.load(carried[0], span(1404))?;
                        let x = region.load(captures[0], span(1405))?;
                        let i = region.load(binders[0], span(1406))?;
                        let two = region
                            .constant(SolveValue::integer(profile(), 2).unwrap(), span(1407))?;
                        let row = region.binary(SolveBinaryOperator::Add, two, i, span(1408))?;
                        let value = region.project_element_dynamic(x, &[i], span(1409))?;
                        let rows = region.update_element(rows, value, &[i], span(1410))?;
                        let old = region.project_element_dynamic(rows, &[row], span(1411))?;
                        let scaled =
                            region.binary(SolveBinaryOperator::Multiply, old, value, span(1412))?;
                        let rows = region.update_element(rows, scaled, &[row], span(1413))?;
                        region.store(result[0], rows, span(1414))
                    },
                )?;
                builder.store(outputs[0], values[0], span(1415))
            },
        )?;
        Ok(())
    })
    .unwrap();
    output_coordinates(table.owners()[0].call_site(), false)
}

#[test]
fn fold_element_written_at_a_binder_index_reads_only_what_its_pass_stored() {
    assert_eq!(
        rows_written_at_binder_offsets(2),
        vec![
            BTreeSet::from([0]),
            BTreeSet::from([1]),
            BTreeSet::from([0, 4]),
            BTreeSet::from([1, 5]),
        ]
    );
}

#[test]
fn fold_followed_for_one_pass_leaves_the_unwritten_elements_on_their_initial_value() {
    assert_eq!(
        rows_written_at_binder_offsets(1),
        vec![
            BTreeSet::from([0]),
            BTreeSet::from([3]),
            BTreeSet::from([0, 4]),
            BTreeSet::from([5]),
        ]
    );
}

/// `k := 0; a := y; for i in 1:count loop k := k + 1; a[k] := x[k]; end for`
/// over `x[n]` and `y[n]`: the counter is a carried Integer the passes fix.
fn rows_written_at_a_carried_counter(count: i64, size: u32) -> Vec<BTreeSet<usize>> {
    let table = SolvePureCallTable::construct(profile(), |table| {
        table.add_owner(
            identity(1420),
            vec![real_shape(vec![size]), real_shape(vec![size])],
            vec![SolvePureCallOutput::result(real_shape(vec![size]))],
            span(1420),
            |builder, inputs, outputs| {
                let x = builder.load(inputs[0], span(1421))?;
                let y = builder.load(inputs[1], span(1422))?;
                let zero =
                    builder.constant(SolveValue::integer(profile(), 0).unwrap(), span(1423))?;
                let values = builder.fold(
                    passes(count),
                    &[zero, y],
                    &[x],
                    span(1424),
                    |region, carried, captures, _, result| {
                        let k = region.load(carried[0], span(1425))?;
                        let rows = region.load(carried[1], span(1426))?;
                        let x = region.load(captures[0], span(1427))?;
                        let one = region
                            .constant(SolveValue::integer(profile(), 1).unwrap(), span(1428))?;
                        let k = region.binary(SolveBinaryOperator::Add, k, one, span(1429))?;
                        let value = region.project_element_dynamic(x, &[k], span(1430))?;
                        let rows = region.update_element(rows, value, &[k], span(1431))?;
                        region.store(result[0], k, span(1432))?;
                        region.store(result[1], rows, span(1433))
                    },
                )?;
                builder.store(outputs[0], values[1], span(1434))
            },
        )?;
        Ok(())
    })
    .unwrap();
    output_coordinates(table.owners()[0].call_site(), false)
}

#[test]
fn fold_element_written_at_a_carried_counter_is_followed_through_the_counter() {
    assert_eq!(
        rows_written_at_a_carried_counter(2, 3),
        vec![
            BTreeSet::from([0]),
            BTreeSet::from([1]),
            BTreeSet::from([5]),
        ]
    );
}

#[test]
fn fold_longer_than_the_followed_budget_keeps_the_summary_over_all_passes() {
    // 65 passes are not followed: every element may be written from any
    // element of `x`, and keeps its own initial value's array.
    let size = 65;
    let every = (0..2 * size as usize).collect::<BTreeSet<_>>();
    assert_eq!(
        rows_written_at_a_carried_counter(65, size),
        vec![every; size as usize]
    );
}

/// One level of a nest of folds around `sum := sum + x`: `levels` more
/// folds of `count` passes, then the sum itself.
fn sum_level<'p>(
    builder: &mut crate::TypedProgramBuilder<'p>,
    carried: &[crate::ProgramSlot<'p>],
    captures: &[crate::ProgramSlot<'p>],
    result: &[crate::ProgramSlot<'p>],
    count: i64,
    levels: usize,
) -> Result<(), SolveProgramConstructionError> {
    let sum = builder.load(carried[0], span(1504))?;
    let x = builder.load(captures[0], span(1505))?;
    let next = if levels == 0 {
        builder.binary(SolveBinaryOperator::Add, sum, x, span(1506))?
    } else {
        builder.fold(
            passes(count),
            &[sum],
            &[x],
            span(1507),
            |region, carried, captures, _, result| {
                sum_level(region, carried, captures, result, count, levels - 1)
            },
        )?[0]
    };
    builder.store(result[0], next, span(1508))
}

/// `s := 0; for .. loop .. s := s + x; .. end for` with `count` passes at
/// each of `depth` levels: the leaves its result reads and the most passes
/// one derivation of it followed.
fn nested_sum(count: i64, depth: usize) -> (Vec<Box<[usize]>>, usize) {
    use crate::typed_program::call::dependency::followed_work;
    let real = real_shape(vec![]);
    followed_work::reset();
    let table = SolvePureCallTable::construct(profile(), |table| {
        table.add_owner(
            identity(1500),
            vec![real.clone()],
            vec![SolvePureCallOutput::result(real.clone())],
            span(1500),
            |builder, inputs, outputs| {
                let x = builder.load(inputs[0], span(1501))?;
                let zero = builder.constant(SolveValue::real(profile(), 0.0), span(1502))?;
                let values = builder.fold(
                    passes(count),
                    &[zero],
                    &[x],
                    span(1503),
                    |region, carried, captures, _, result| {
                        sum_level(region, carried, captures, result, count, depth - 1)
                    },
                )?;
                builder.store(outputs[0], values[0], span(1509))
            },
        )?;
        Ok(())
    })
    .unwrap();
    let leaves = input_leaves(table.owners()[0].call_site().output_dependencies());
    (leaves, followed_work::most_passes())
}

#[test]
fn a_nest_of_folds_within_the_work_budget_is_followed_in_every_pass() {
    // 33 outer passes, each following 33 inner passes: more than a backend
    // unrolls, and well inside the analysis budget.
    let (leaves, passes) = nested_sum(33, 2);
    assert_eq!(leaves, vec![vec![0].into_boxed_slice()]);
    assert_eq!(passes, 33 + 33 * 33);
}

#[test]
fn nested_followed_folds_draw_on_one_work_budget() {
    use crate::typed_program::call::dependency::followed_work;
    // 33 * 33 * 33 followed passes would walk the innermost body 35,937
    // times and 64 * 64 * 64 would walk it 262,144 times. Every pass costs
    // at least one operation of the owner's one budget, so the following
    // stops there, and the folds that could not be paid for keep the summary
    // over all passes, which names the same leaf here.
    for (count, unbounded) in [
        (33_usize, 33 + 33 * 33 + 33 * 33 * 33),
        (64, 64 + 64 * 64 + 64 * 64 * 64),
    ] {
        let (leaves, passes) = nested_sum(count as i64, 3);
        assert_eq!(leaves, vec![vec![0].into_boxed_slice()]);
        assert!(passes <= followed_work::BUDGET, "{count}: {passes}");
        assert!(passes < unbounded, "{count}: {passes} of {unbounded}");
        assert!(
            passes > 0,
            "{count}: the nest is followed until the budget runs out"
        );
    }
}
