use super::*;
use std::collections::BTreeSet;

fn output_coordinates(site: SolvePureCallSite, directional: bool) -> Vec<BTreeSet<usize>> {
    use crate::{LinearOp, StructuralPattern, TensorInputKind};
    let inputs = if directional {
        site.directional().unwrap().inputs()
    } else {
        site.inputs()
    };
    let mut count = 0;
    let starts = inputs
        .iter()
        .map(|input| {
            let start = count;
            count += input.scalar_count();
            start
        })
        .collect::<Vec<_>>()
        .into_boxed_slice();
    let (call, output_count) = if directional {
        let site = site.directional().unwrap().clone();
        let output_count = site.output_scalar_count().unwrap();
        (
            LinearOp::PureCallDirectional {
                dst_start: count,
                input_starts: starts,
                site,
            },
            output_count,
        )
    } else {
        let output_count = site.output_scalar_count().unwrap();
        (
            LinearOp::PureCall {
                dst_start: count,
                input_starts: starts,
                site,
            },
            output_count,
        )
    };
    StructuralPattern::derive_output_y_dependencies(
        &[
            LinearOp::TensorLoad {
                dst_start: 0,
                input: TensorInputKind::Y,
                input_start: 0,
                count: count as usize,
                seed_start: None,
                lanes: 1,
            },
            call,
            LinearOp::StoreOutputRange {
                start: count,
                count: output_count,
                stride: 1,
            },
        ],
        Some(span(0)),
    )
    .unwrap()
}

fn real_shape(dimensions: Vec<u32>) -> SolveValueType {
    if dimensions.is_empty() {
        SolveValueType::scalar(SolveScalarType::real(profile()))
    } else {
        SolveValueType::tensor(SolveScalarType::real(profile()), dimensions).unwrap()
    }
}

fn input_leaves(outputs: &[Box<[crate::SolveCallDependency]>]) -> Vec<Box<[usize]>> {
    outputs
        .iter()
        .map(|dependencies| {
            dependencies
                .iter()
                .map(crate::SolveCallDependency::input_index)
                .collect::<std::collections::BTreeSet<_>>()
                .into_iter()
                .collect()
        })
        .collect()
}

pub(super) fn separated_outputs(width: u32) -> SolvePureCallTable {
    let vector = SolveValueType::tensor(SolveScalarType::real(profile()), vec![width]).unwrap();
    let inputs = vec![vector.clone(), vector.clone()];
    let outputs = vec![
        SolvePureCallOutput::result(vector.clone()),
        SolvePureCallOutput::result(vector),
        SolvePureCallOutput::assertion_predicate(),
    ];
    SolvePureCallTable::construct(profile(), |table| {
        let inner = table.add_owner(
            identity(1),
            inputs.clone(),
            outputs.clone(),
            span(0),
            |builder, inputs, outputs| {
                let first = builder.load(inputs[0], span(1))?;
                let second = builder.load(inputs[1], span(2))?;
                let safe = builder.constant(SolveValue::boolean(true), span(3))?;
                builder.store(outputs[0], first, span(4))?;
                builder.store(outputs[1], second, span(5))?;
                builder.store(outputs[2], safe, span(6))
            },
        )?;
        table.add_owner(
            identity(2),
            inputs,
            outputs,
            span(7),
            |builder, inputs, outputs| {
                let first = builder.load(inputs[0], span(8))?;
                let second = builder.load(inputs[1], span(9))?;
                let values = builder.call(inner, &[second, first], span(10))?;
                for (&output, &value) in outputs.iter().zip(&values) {
                    builder.store(output, value, span(11))?;
                }
                Ok(())
            },
        )?;
        Ok(())
    })
    .unwrap()
}

#[test]
fn issued_call_sites_share_immutable_owner_interfaces() {
    let table = separated_outputs(32);
    let owner = &table.owners()[1];
    let first = owner.call_site();
    for site in [owner.call_site(), first.clone()] {
        assert!(table.matches_site(&site));
        assert!(std::ptr::eq(owner.inputs(), site.inputs()));
        assert!(std::ptr::eq(owner.outputs(), site.outputs()));
        assert!(std::ptr::eq(
            first.output_dependencies(),
            site.output_dependencies(),
        ));
        let directional_owner = owner.directional().unwrap();
        let directional_site = site.directional().unwrap();
        assert!(table.matches_directional_site(directional_site));
        assert!(std::ptr::eq(
            directional_owner.inputs(),
            directional_site.inputs(),
        ));
        assert!(std::ptr::eq(
            directional_owner.outputs(),
            directional_site.outputs(),
        ));
        assert!(std::ptr::eq(
            first.directional().unwrap().output_dependencies(),
            directional_site.output_dependencies(),
        ));
    }
}

#[test]
fn nested_vector_copy_has_no_cross_component_dependencies() {
    use crate::{LinearOp, StructuralPattern, TensorInputKind};
    let table = separated_outputs(3);
    let site = table.owners()[1].call_site();
    let program = vec![
        LinearOp::TensorLoad {
            dst_start: 0,
            input: TensorInputKind::Y,
            input_start: 0,
            count: 6,
            seed_start: None,
            lanes: 1,
        },
        LinearOp::PureCall {
            dst_start: 6,
            input_starts: vec![0, 3].into_boxed_slice(),
            site,
        },
        LinearOp::StoreOutputRange {
            start: 6,
            count: 7,
            stride: 1,
        },
    ];
    let actual = StructuralPattern::derive_output_y_dependencies(&program, Some(span(0))).unwrap();
    let expected = [Some(3), Some(4), Some(5), Some(0), Some(1), Some(2), None]
        .into_iter()
        .map(|index| index.into_iter().collect())
        .collect::<Vec<_>>();
    assert_eq!(actual, expected);
}

#[test]
fn vector_copy_directional_outputs_preserve_each_primal_and_tangent_coordinate() {
    let table = separated_outputs(3);
    let actual = output_coordinates(table.owners()[1].call_site(), true);
    let expected = (6..12)
        .chain(0..6)
        .map(|index| BTreeSet::from([index]))
        .chain(std::iter::once(BTreeSet::new()))
        .collect::<Vec<_>>();
    assert_eq!(actual, expected);
}

#[test]
fn matrix_product_dependencies_follow_rows_columns_and_the_inner_axis() {
    for (lhs, rhs, output, expected) in [
        (vec![2], vec![2], vec![], vec![vec![0, 1, 2, 3]]),
        (
            vec![2, 2],
            vec![2],
            vec![2],
            vec![vec![0, 1, 4, 5], vec![2, 3, 4, 5]],
        ),
        (
            vec![2],
            vec![2, 2],
            vec![2],
            vec![vec![0, 1, 2, 4], vec![0, 1, 3, 5]],
        ),
        (
            vec![2, 2],
            vec![2, 3],
            vec![2, 3],
            vec![
                vec![0, 1, 4, 7],
                vec![0, 1, 5, 8],
                vec![0, 1, 6, 9],
                vec![2, 3, 4, 7],
                vec![2, 3, 5, 8],
                vec![2, 3, 6, 9],
            ],
        ),
    ] {
        let table = SolvePureCallTable::construct(profile(), |table| {
            table.add_owner(
                identity(1),
                vec![real_shape(lhs), real_shape(rhs)],
                vec![SolvePureCallOutput::result(real_shape(output))],
                span(0),
                |builder, inputs, outputs| {
                    let lhs = builder.load(inputs[0], span(1))?;
                    let rhs = builder.load(inputs[1], span(2))?;
                    let result = builder.matrix_multiply(lhs, rhs, span(3))?;
                    builder.store(outputs[0], result, span(4))
                },
            )?;
            Ok(())
        })
        .unwrap();
        assert_eq!(
            output_coordinates(table.owners()[0].call_site(), false),
            expected
                .into_iter()
                .map(|indices| indices.into_iter().collect())
                .collect::<Vec<_>>()
        );
    }
}

#[test]
fn transpose_slice_and_nested_call_maps_compose_without_expanding_tensors() {
    let table = SolvePureCallTable::construct(profile(), |table| {
        let inner = table.add_owner(
            identity(1),
            vec![real_shape(vec![3, 2])],
            vec![SolvePureCallOutput::result(real_shape(vec![2, 3]))],
            span(0),
            |builder, inputs, outputs| {
                let input = builder.load(inputs[0], span(1))?;
                let transposed = builder.transpose(input, span(2))?;
                builder.store(outputs[0], transposed, span(3))
            },
        )?;
        table.add_owner(
            identity(2),
            vec![real_shape(vec![2, 3])],
            vec![SolvePureCallOutput::result(real_shape(vec![2, 2]))],
            span(4),
            |builder, inputs, outputs| {
                let input = builder.load(inputs[0], span(5))?;
                let transposed = builder.transpose(input, span(6))?;
                let results = builder.call(inner, &[transposed], span(7))?;
                let slice = builder.project_slice(results[0], vec![0, 1], vec![2, 2], span(8))?;
                builder.store(outputs[0], slice, span(9))
            },
        )?;
        Ok(())
    })
    .unwrap();
    let site = table.owners()[1].call_site();
    assert_eq!(
        output_coordinates(site.clone(), false),
        [1, 2, 4, 5].map(|index| BTreeSet::from([index]))
    );
    assert_eq!(
        output_coordinates(site, true),
        [1, 2, 4, 5, 7, 8, 10, 11].map(|index| BTreeSet::from([index]))
    );
}

#[test]
fn nested_matrix_products_compose_both_contraction_domains() {
    let table = SolvePureCallTable::construct(profile(), |table| {
        let inner = table.add_owner(
            identity(1),
            vec![real_shape(vec![2, 2]), real_shape(vec![2, 2])],
            vec![SolvePureCallOutput::result(real_shape(vec![2, 2]))],
            span(0),
            |builder, inputs, outputs| {
                let lhs = builder.load(inputs[0], span(1))?;
                let rhs = builder.load(inputs[1], span(2))?;
                let result = builder.matrix_multiply(lhs, rhs, span(3))?;
                builder.store(outputs[0], result, span(4))
            },
        )?;
        table.add_owner(
            identity(2),
            vec![
                real_shape(vec![2, 3]),
                real_shape(vec![3, 2]),
                real_shape(vec![2, 2]),
            ],
            vec![SolvePureCallOutput::result(real_shape(vec![2, 2]))],
            span(5),
            |builder, inputs, outputs| {
                let a = builder.load(inputs[0], span(6))?;
                let b = builder.load(inputs[1], span(7))?;
                let c = builder.load(inputs[2], span(8))?;
                let ab = builder.matrix_multiply(a, b, span(9))?;
                let abc = builder.call(inner, &[ab, c], span(10))?;
                builder.store(outputs[0], abc[0], span(11))
            },
        )?;
        Ok(())
    })
    .unwrap();
    let expected = (0..2)
        .flat_map(|row| {
            (0..2).map(move |column| {
                (row * 3..row * 3 + 3)
                    .chain(6..12)
                    .chain([12 + column, 14 + column])
                    .collect::<BTreeSet<_>>()
            })
        })
        .collect::<Vec<_>>();
    assert_eq!(
        output_coordinates(table.owners()[1].call_site(), false),
        expected
    );
}

#[test]
fn nested_outputs_substitute_only_their_primal_and_tangent_arguments() {
    let table = separated_outputs(3);
    let site = table.owners()[1].call_site();
    assert_eq!(
        input_leaves(site.output_dependencies()),
        &[
            vec![1].into_boxed_slice(),
            vec![0].into_boxed_slice(),
            Box::new([])
        ]
    );
    assert_eq!(
        input_leaves(site.directional().unwrap().output_dependencies()),
        &[
            vec![2].into_boxed_slice(),
            vec![3].into_boxed_slice(),
            vec![0].into_boxed_slice(),
            vec![1].into_boxed_slice(),
            Box::new([]),
        ]
    );
    let json = serde_json::to_value(&table).unwrap();
    assert!(json["owners"][0].get("dependencies").is_none());
    let replayed: SolvePureCallTable = serde_json::from_value(json).unwrap();
    assert_eq!(table, replayed);
    assert!(replayed.matches_site(&site));
}

#[test]
fn typed_leaf_dependency_metadata_does_not_scale_with_tensor_extent() {
    let small = separated_outputs(3);
    let large = separated_outputs(1_000_000);
    for (small, large) in small.owners().iter().zip(large.owners()) {
        let small = small.call_site();
        let large = large.call_site();
        assert_eq!(small.output_dependencies(), large.output_dependencies());
        assert_eq!(
            small.directional().unwrap().output_dependencies(),
            large.directional().unwrap().output_dependencies()
        );
    }
}

#[test]
fn owner_replay_rejects_changed_primal_and_directional_site_summaries() {
    let table = separated_outputs(3);
    let site = table.owners()[1].call_site();
    let original = serde_json::to_value(&site).unwrap();
    for path in ["/dependencies/0", "/directional/dependencies/0"] {
        let mut missing = original.clone();
        *missing.pointer_mut(path).unwrap() = serde_json::json!([]);
        let mut wrong_input = original.clone();
        wrong_input.pointer_mut(path).unwrap()[0]["input"] = serde_json::json!(99);
        let mut wrong_coordinate = original.clone();
        wrong_coordinate.pointer_mut(path).unwrap()[0]["coordinates"]["subscripts"][0]["coeffs"]
            [0] = serde_json::json!(0);
        for forged in [missing, wrong_input, wrong_coordinate] {
            let forged: SolvePureCallSite = serde_json::from_value(forged).unwrap();
            assert!(!table.matches_site(&forged));
            if path.starts_with("/directional") {
                assert!(!table.matches_directional_site(forged.directional().unwrap()));
            }
        }
    }
    let mut missing = original;
    missing.as_object_mut().unwrap().remove("dependencies");
    assert!(serde_json::from_value::<SolvePureCallSite>(missing).is_err());
}

#[test]
fn conditional_output_keeps_condition_and_both_branch_dependencies() {
    let real = SolveValueType::scalar(SolveScalarType::real(profile()));
    let table = SolvePureCallTable::construct(profile(), |table| {
        table.add_owner(
            identity(1),
            vec![
                SolveValueType::scalar(SolveScalarType::Boolean),
                real.clone(),
                real.clone(),
            ],
            vec![SolvePureCallOutput::result(real.clone())],
            span(0),
            |builder, inputs, outputs| {
                let condition = builder.load(inputs[0], span(1))?;
                let a = builder.load(inputs[1], span(2))?;
                let b = builder.load(inputs[2], span(3))?;
                let selected = builder.conditional(
                    condition,
                    &[a, b],
                    vec![real],
                    span(4),
                    |branch, captures, outputs| {
                        let a = branch.load(captures[0], span(5))?;
                        branch.store(outputs[0], a, span(6))
                    },
                    |branch, captures, outputs| {
                        let b = branch.load(captures[1], span(7))?;
                        branch.store(outputs[0], b, span(8))
                    },
                )?;
                builder.store(outputs[0], selected[0], span(9))
            },
        )?;
        Ok(())
    })
    .unwrap();
    let site = table.owners()[0].call_site();
    assert_eq!(
        input_leaves(site.output_dependencies()),
        &[vec![0, 1, 2].into_boxed_slice()]
    );
    let directional = input_leaves(site.directional().unwrap().output_dependencies());
    assert!(
        directional
            .iter()
            .all(|dependencies| dependencies.contains(&0))
    );
    assert!(directional[1].contains(&2) && directional[1].contains(&4));
}

#[test]
fn a_replaced_local_store_does_not_retain_the_old_value_dependency() {
    let real = SolveValueType::scalar(SolveScalarType::real(profile()));
    let table = SolvePureCallTable::construct(profile(), |table| {
        table.add_owner(
            identity(1),
            vec![real.clone(), real.clone()],
            vec![SolvePureCallOutput::result(real.clone())],
            span(0),
            |builder, inputs, outputs| {
                let local = builder.declare_slot(
                    real,
                    SolveStorageClass::MethodLocal,
                    SolveSlotAccess::ReadWrite,
                    span(1),
                )?;
                let old = builder.load(inputs[0], span(1))?;
                builder.store(local, old, span(2))?;
                let final_value = builder.load(inputs[1], span(3))?;
                builder.store(local, final_value, span(4))?;
                let value = builder.load(local, span(5))?;
                builder.store(outputs[0], value, span(6))
            },
        )?;
        Ok(())
    })
    .unwrap();
    assert_eq!(
        input_leaves(table.owners()[0].call_site().output_dependencies()),
        &[vec![1].into_boxed_slice()]
    );
}
