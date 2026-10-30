//! Coordinate-selection regressions motivated by actual walking-model blockers.
use super::*;
use rumoca_core::{IndexBinderProvenance, StructuredIndexBinder, StructuredIndexDomain};

#[test]
fn coordinate_precision_aggregate_keeps_the_selected_component() {
    let table = SolvePureCallTable::construct(profile(), |table| {
        table.add_owner(
            identity(1200),
            vec![real_shape(vec![3])],
            vec![SolvePureCallOutput::result(real_shape(vec![]))],
            span(1200),
            |builder, inputs, outputs| {
                let input = builder.load(inputs[0], span(1201))?;
                let x = builder.project_element(input, vec![0], span(1202))?;
                let y = builder.project_element(input, vec![1], span(1203))?;
                let z = builder.project_element(input, vec![2], span(1204))?;
                let vector = builder.construct_aggregate(&[x, y, z], vec![3], span(1205))?;
                let selected = builder.project_element(vector, vec![2], span(1206))?;
                builder.store(outputs[0], selected, span(1207))
            },
        )?;
        Ok(())
    })
    .unwrap();
    assert_eq!(
        output_coordinates(table.owners()[0].call_site(), false),
        vec![BTreeSet::from([2])]
    );
}

#[test]
fn coordinate_precision_update_does_not_taint_an_unmodified_component() {
    let table = SolvePureCallTable::construct(profile(), |table| {
        table.add_owner(
            identity(1220),
            vec![real_shape(vec![2])],
            vec![SolvePureCallOutput::result(real_shape(vec![]))],
            span(1220),
            |builder, inputs, outputs| {
                let input = builder.load(inputs[0], span(1221))?;
                let second = builder.project_element(input, vec![1], span(1222))?;
                let two =
                    builder.constant(SolveValue::integer(profile(), 2).unwrap(), span(1223))?;
                let updated = builder.update_element(input, second, &[two], span(1224))?;
                let first = builder.project_element(updated, vec![0], span(1225))?;
                builder.store(outputs[0], first, span(1226))
            },
        )?;
        Ok(())
    })
    .unwrap();
    assert_eq!(
        output_coordinates(table.owners()[0].call_site(), false),
        vec![BTreeSet::from([0])]
    );
}

#[test]
fn coordinate_precision_independent_fold_carries_keep_separate_reads() {
    let real = real_shape(vec![]);
    let table = SolvePureCallTable::construct(profile(), |table| {
        table.add_owner(
            identity(1240),
            vec![real_shape(vec![3])],
            vec![
                SolvePureCallOutput::result(real.clone()),
                SolvePureCallOutput::result(real.clone()),
            ],
            span(1240),
            |builder, inputs, outputs| {
                let input = builder.load(inputs[0], span(1241))?;
                let zero = builder.constant(SolveValue::real(profile(), 0.0), span(1242))?;
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
                    &[zero, zero],
                    &[input],
                    span(1243),
                    |region, carried, captures, _, result| {
                        let input = region.load(captures[0], span(1244))?;
                        let z = region.project_element(input, vec![2], span(1245))?;
                        let x = region.project_element(input, vec![0], span(1246))?;
                        let first = region.load(carried[0], span(1247))?;
                        let second = region.load(carried[1], span(1248))?;
                        let first =
                            region.binary(SolveBinaryOperator::Add, first, z, span(1249))?;
                        let second =
                            region.binary(SolveBinaryOperator::Add, second, x, span(1250))?;
                        region.store(result[0], first, span(1251))?;
                        region.store(result[1], second, span(1252))
                    },
                )?;
                builder.store(outputs[0], values[0], span(1253))?;
                builder.store(outputs[1], values[1], span(1254))
            },
        )?;
        Ok(())
    })
    .unwrap();
    assert_eq!(
        output_coordinates(table.owners()[0].call_site(), false),
        vec![BTreeSet::from([2]), BTreeSet::from([0])]
    );
}

#[test]
fn coordinate_precision_nested_aggregate_keeps_the_selected_tensor_element() {
    let table = SolvePureCallTable::construct(profile(), |table| {
        table.add_owner(
            identity(1260),
            vec![real_shape(vec![2, 2])],
            vec![SolvePureCallOutput::result(real_shape(vec![]))],
            span(1260),
            |builder, inputs, outputs| {
                let input = builder.load(inputs[0], span(1261))?;
                let assembled = builder.construct_aggregate(&[input], vec![1, 2, 2], span(1262))?;
                let selected = builder.project_element(assembled, vec![0, 1, 0], span(1263))?;
                builder.store(outputs[0], selected, span(1264))
            },
        )?;
        Ok(())
    })
    .unwrap();
    assert_eq!(
        output_coordinates(table.owners()[0].call_site(), false),
        vec![BTreeSet::from([2])]
    );
}

#[test]
fn coordinate_precision_dynamic_update_retains_the_conservative_reads() {
    let integer = SolveValueType::scalar(SolveScalarType::integer(profile()));
    let table = SolvePureCallTable::construct(profile(), |table| {
        table.add_owner(
            identity(1280),
            vec![real_shape(vec![2]), integer],
            vec![SolvePureCallOutput::result(real_shape(vec![]))],
            span(1280),
            |builder, inputs, outputs| {
                let input = builder.load(inputs[0], span(1281))?;
                let index = builder.load(inputs[1], span(1282))?;
                let replacement = builder.project_element(input, vec![1], span(1283))?;
                let updated = builder.update_element(input, replacement, &[index], span(1284))?;
                let first = builder.project_element(updated, vec![0], span(1285))?;
                builder.store(outputs[0], first, span(1286))
            },
        )?;
        Ok(())
    })
    .unwrap();
    assert_eq!(
        output_coordinates(table.owners()[0].call_site(), false),
        vec![BTreeSet::from([0, 1, 2])]
    );
}

fn two_iterations() -> StructuredIndexDomain {
    StructuredIndexDomain {
        binders: vec![StructuredIndexBinder {
            provenance: IndexBinderProvenance::Generated,
            id: 0,
            display_name: "i".into(),
            lower: 1,
            upper: 2,
            step: 1,
        }],
    }
}

#[test]
fn coordinate_precision_cross_carry_retains_transitive_iteration_reads() {
    let real = real_shape(vec![]);
    let table = SolvePureCallTable::construct(profile(), |table| {
        table.add_owner(
            identity(1300),
            vec![real_shape(vec![2])],
            vec![
                SolvePureCallOutput::result(real.clone()),
                SolvePureCallOutput::result(real),
            ],
            span(1300),
            |builder, inputs, outputs| {
                let input = builder.load(inputs[0], span(1301))?;
                let x = builder.project_element(input, vec![0], span(1302))?;
                let y = builder.project_element(input, vec![1], span(1303))?;
                let values = builder.fold(
                    two_iterations(),
                    &[x, y],
                    &[],
                    span(1304),
                    |region, carried, _, _, result| {
                        let first = region.load(carried[0], span(1305))?;
                        let second = region.load(carried[1], span(1306))?;
                        let sum =
                            region.binary(SolveBinaryOperator::Add, first, second, span(1307))?;
                        region.store(result[0], second, span(1308))?;
                        region.store(result[1], sum, span(1309))
                    },
                )?;
                builder.store(outputs[0], values[0], span(1310))?;
                builder.store(outputs[1], values[1], span(1311))
            },
        )?;
        Ok(())
    })
    .unwrap();
    assert_eq!(
        output_coordinates(table.owners()[0].call_site(), false),
        vec![BTreeSet::from([0, 1]), BTreeSet::from([0, 1])]
    );
}

#[test]
fn coordinate_precision_permuted_carry_retains_other_coordinate_reads() {
    let vector = real_shape(vec![2]);
    let table = SolvePureCallTable::construct(profile(), |table| {
        table.add_owner(
            identity(1320),
            vec![vector.clone()],
            vec![SolvePureCallOutput::result(vector)],
            span(1320),
            |builder, inputs, outputs| {
                let input = builder.load(inputs[0], span(1321))?;
                let values = builder.fold(
                    two_iterations(),
                    &[input],
                    &[],
                    span(1322),
                    |region, carried, _, _, result| {
                        let input = region.load(carried[0], span(1323))?;
                        let first = region.project_element(input, vec![0], span(1324))?;
                        let second = region.project_element(input, vec![1], span(1325))?;
                        let swapped =
                            region.construct_aggregate(&[second, first], vec![2], span(1326))?;
                        region.store(result[0], swapped, span(1327))
                    },
                )?;
                builder.store(outputs[0], values[0], span(1328))
            },
        )?;
        Ok(())
    })
    .unwrap();
    assert_eq!(
        output_coordinates(table.owners()[0].call_site(), false),
        vec![BTreeSet::from([0, 1]), BTreeSet::from([0, 1])]
    );
}
