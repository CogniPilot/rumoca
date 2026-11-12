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
