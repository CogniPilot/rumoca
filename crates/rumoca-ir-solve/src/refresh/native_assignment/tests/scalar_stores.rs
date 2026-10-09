use super::*;
use crate::{ScalarProgramBlock, TensorInputKind};

fn span() -> Span {
    Span::from_offsets(SourceId::from_source_name("ScalarStores.mo"), 7, 19)
}

fn program(
    count: usize,
    target: usize,
    input: TensorInputKind,
    input_start: usize,
) -> Vec<LinearOp> {
    let width = u32::try_from(count).unwrap();
    let mut operations = vec![
        LinearOp::TensorLoad {
            dst_start: 0,
            input: TensorInputKind::Y,
            input_start: target,
            count,
            seed_start: None,
            lanes: 1,
        },
        LinearOp::TensorLoad {
            dst_start: width,
            input,
            input_start,
            count,
            seed_start: None,
            lanes: 1,
        },
        LinearOp::TensorBinary {
            dst_start: 2 * width,
            op: BinaryOp::Sub,
            lhs_start: 0,
            rhs_start: width,
            count,
            lhs_stride: 1,
            rhs_stride: 1,
            lanes: 1,
        },
        // The unused prefix remains part of the original evaluation obligation.
        LinearOp::Const {
            dst: 3 * width,
            value: -0.0,
        },
    ];
    operations.extend((0..width).map(|i| LinearOp::StoreOutput { src: 2 * width + i }));
    operations
}

fn source(programs: Vec<Vec<LinearOp>>, indices: Option<Vec<usize>>) -> ComputeBlock {
    let spans = vec![span(); programs.len()];
    let block = match indices {
        Some(indices) => ScalarProgramBlock::with_output_indices(programs, spans, indices),
        None => ScalarProgramBlock::with_program_spans(programs, spans),
    }
    .unwrap();
    ComputeBlock {
        nodes: vec![ComputeNode::ScalarPrograms(block)],
    }
}

fn targets(count: usize) -> Vec<Option<ScalarSlot>> {
    (0..count).map(|i| Some(scalar_slot_y(i))).collect()
}

#[test]
fn terminal_scalar_stores_keep_one_compact_stage_per_original_program() {
    for count in [2, 3, 9, 14_400] {
        let original = program(count, 0, TensorInputKind::Y, count);
        let block = source(
            vec![
                original.clone(),
                program(count, count, TensorInputKind::P, 0),
            ],
            None,
        );
        let layout = VarLayout::from_parts(Default::default(), 2 * count, count);
        let schedule = derive(&block, &targets(2 * count), &layout).unwrap();
        assert_eq!(schedule.stages.len(), 2);
        assert_eq!(schedule.stages[0].target_range(), Some(count..2 * count));
        assert_eq!(schedule.stages[1].target_range(), Some(0..count));
        let stage = &schedule.stages[1];
        let ComputeNode::ScalarPrograms(value) = &stage.value_kernel().nodes[0] else {
            panic!("value")
        };
        assert_eq!(value.programs()[0].len(), 5);
        assert!(operations_match(&value.programs()[0][..4], &original[..4]));
        assert_eq!(value.program_spans(), [span()]);
        assert_eq!(
            value.programs()[0][4],
            LinearOp::StoreOutputRange {
                start: count as u32,
                count,
                stride: 1
            }
        );
    }
}

// Independent scalar reference for this finite test profile. It deliberately
// executes every original operation and reads each source store in order.
fn dense_execute(operations: &[LinearOp], y: &[f64], p: &[f64]) -> Vec<f64> {
    let mut registers = vec![0.0; 4 * y.len() + 1];
    let mut output = Vec::new();
    for op in operations {
        match *op {
            LinearOp::TensorLoad {
                dst_start,
                input,
                input_start,
                count,
                ..
            } => {
                let values = match input {
                    TensorInputKind::Y => y,
                    TensorInputKind::P => p,
                };
                for i in 0..count {
                    registers[dst_start as usize + i] = values[input_start + i];
                }
            }
            LinearOp::TensorBinary {
                dst_start,
                op: BinaryOp::Sub,
                lhs_start,
                rhs_start,
                count,
                ..
            } => {
                for i in 0..count {
                    registers[dst_start as usize + i] =
                        registers[lhs_start as usize + i] - registers[rhs_start as usize + i];
                }
            }
            LinearOp::Const { dst, value } => registers[dst as usize] = value,
            LinearOp::LoadY { dst, index } => registers[dst as usize] = y[index],
            LinearOp::StoreOutput { src } => output.push(registers[src as usize]),
            LinearOp::StoreOutputRange {
                start,
                count,
                stride,
            } => {
                output.extend((0..count).map(|i| registers[start as usize + i * stride]));
            }
            _ => panic!("test profile"),
        }
    }
    output
}

fn scalar_target_loads(count: usize) -> Vec<LinearOp> {
    let mut operations = program(count, 0, TensorInputKind::P, 0);
    operations.splice(
        0..1,
        (0..count).map(|i| LinearOp::LoadY {
            dst: i as u32,
            index: i,
        }),
    );
    operations
}

#[test]
fn scalar_target_producers_keep_exact_register_identity_and_source_order() {
    for reverse_definitions in [false, true] {
        let mut operations = scalar_target_loads(3);
        if reverse_definitions {
            operations.swap(0, 2);
        }
        let block = source(vec![operations.clone()], None);
        let layout = VarLayout::from_parts(Default::default(), 3, 3);
        let schedule = derive(&block, &targets(3), &layout).unwrap();
        let ComputeNode::ScalarPrograms(value) = &schedule.stages[0].value_kernel().nodes[0] else {
            panic!("value")
        };
        assert!(operations_match(
            &value.programs()[0][..6],
            &operations[..6]
        ));
        let y = [9.0, -3.0, 1e100];
        let p = [-0.0, 0.0, 0.25];
        let residual = dense_execute(&operations, &y, &p);
        let result = dense_execute(&value.programs()[0], &y, &p);
        for i in 0..3 {
            assert_eq!(residual[i].to_bits(), (y[i] - p[i]).to_bits());
            assert_eq!(result[i].to_bits(), p[i].to_bits());
        }
    }
}

#[test]
fn scalar_target_producers_reject_wrong_duplicate_partial_or_mixed_coordinates() {
    let layout = VarLayout::from_parts(Default::default(), 3, 3);
    for mutation in 0..4 {
        let mut operations = scalar_target_loads(3);
        match mutation {
            0 => {
                operations[0] = LinearOp::LoadY { dst: 0, index: 1 };
                operations[1] = LinearOp::LoadY { dst: 1, index: 0 };
            }
            1 => operations[1] = LinearOp::LoadY { dst: 1, index: 0 },
            2 => operations[2] = LinearOp::LoadP { dst: 2, index: 2 },
            _ => {
                operations[0] = LinearOp::TensorLoad {
                    dst_start: 0,
                    input: TensorInputKind::Y,
                    input_start: 0,
                    count: 2,
                    seed_start: None,
                    lanes: 1,
                };
                operations.remove(1);
            }
        }
        assert!(derive(&source(vec![operations], None), &targets(3), &layout).is_err());
    }
}

#[test]
fn dense_scalar_reference_proves_ordered_values_and_signed_zero_without_residual_cancellation() {
    let count = 9;
    let original = program(count, 0, TensorInputKind::P, 0);
    let block = source(vec![original.clone()], None);
    let layout = VarLayout::from_parts(Default::default(), count, count);
    let schedule = derive(&block, &targets(count), &layout).unwrap();
    let ComputeNode::ScalarPrograms(value) = &schedule.stages[0].value_kernel().nodes[0] else {
        panic!("value")
    };
    let parameters = [
        -0.0,
        0.0,
        1.25,
        -3.0,
        1e-300,
        1e300,
        f64::INFINITY,
        f64::NEG_INFINITY,
        f64::NAN,
    ];
    let y = [0.0; 9];
    let residual = dense_execute(&original, &y, &parameters);
    let actual = dense_execute(&value.programs()[0], &y, &parameters);
    for ((actual, expected), residual) in actual.iter().zip(parameters).zip(residual) {
        assert_eq!(actual.to_bits(), expected.to_bits());
        assert_eq!(residual.to_bits(), (0.0 - expected).to_bits());
    }
}

#[test]
fn scalar_store_register_order_and_interleaved_outputs_refuse() {
    let layout = VarLayout::from_parts(Default::default(), 3, 3);
    for mutation in 0..3 {
        let mut operations = program(3, 0, TensorInputKind::P, 0);
        match mutation {
            0 => operations.swap(4, 5),
            1 => operations[5] = operations[4].clone(),
            _ => operations.insert(
                5,
                LinearOp::Const {
                    dst: 10,
                    value: 1.0,
                },
            ),
        }
        assert!(derive(&source(vec![operations], None), &targets(3), &layout).is_err());
    }
}

#[test]
fn scalar_store_logical_rows_targets_aliases_and_effects_stay_checked() {
    let layout = VarLayout::from_parts(Default::default(), 3, 3);
    let original = program(3, 0, TensorInputKind::P, 0);
    assert!(
        derive(
            &source(vec![original.clone()], Some(vec![0, 2, 1])),
            &targets(3),
            &layout
        )
        .is_err()
    );
    let mut repeated = targets(3);
    repeated[2] = repeated[1];
    assert!(derive(&source(vec![original.clone()], None), &repeated, &layout).is_err());
    let mut invalid = targets(3);
    invalid[2] = Some(scalar_slot_y(3));
    assert!(derive(&source(vec![original.clone()], None), &invalid, &layout).is_err());
    let coupled = program(3, 0, TensorInputKind::Y, 0);
    assert!(derive(&source(vec![coupled], None), &targets(3), &layout).is_err());
    let mut overlap = original.clone();
    overlap.insert(3, LinearOp::Const { dst: 0, value: 1.0 });
    assert!(derive(&source(vec![overlap], None), &targets(3), &layout).is_err());
    let mut effect = original;
    effect.insert(3, LinearOp::LoadSeed { dst: 10, index: 0 });
    assert!(derive(&source(vec![effect], None), &targets(3), &layout).is_err());
}

#[test]
fn unused_typed_call_inputs_remain_independent_of_every_owned_target() {
    use crate::{
        SolveArithmeticProfile, SolveIntegerDomain, SolvePureCallIdentity, SolvePureCallOutput,
        SolvePureCallTable, SolveRealFormat, SolveScalarType, SolveValueType,
    };
    let arithmetic = SolveArithmeticProfile::construct(
        SolveRealFormat::Binary64,
        SolveIntegerDomain::construct(i64::MIN, i64::MAX).unwrap(),
    );
    let tensor = SolveValueType::tensor(SolveScalarType::real(arithmetic), vec![3]).unwrap();
    let mut table = SolvePureCallTable::builder(arithmetic);
    let owner = table
        .add_owner(
            SolvePureCallIdentity::issued(std::num::NonZeroU64::new(1).unwrap()),
            vec![tensor.clone()],
            vec![SolvePureCallOutput::result(tensor)],
            span(),
            |builder, inputs, outputs| {
                let value = builder.load(inputs[0], span())?;
                builder.store(outputs[0], value, span())
            },
        )
        .unwrap();
    let site = table.call_site(owner).unwrap();
    let layout = VarLayout::from_parts(Default::default(), 3, 3);
    for input_start in [0, 3] {
        let mut operations = program(3, 0, TensorInputKind::P, 0);
        operations.insert(
            4,
            LinearOp::PureCall {
                dst_start: 10,
                input_starts: vec![input_start].into_boxed_slice(),
                site: site.clone(),
            },
        );
        let result = derive(&source(vec![operations], None), &targets(3), &layout);
        if input_start == 0 {
            assert_eq!(
                result.unwrap_err().0,
                "native call inputs depend on its own assignment target"
            );
        } else {
            let stage = result.unwrap();
            let ComputeNode::ScalarPrograms(value) = &stage.stages[0].value_kernel().nodes[0]
            else {
                panic!("value")
            };
            assert!(matches!(value.programs()[0][4], LinearOp::PureCall { .. }));
        }
    }
}

/// A discrete program that stores several rows (SOLVE-C82) yields, per row, the
/// program with only that row's store, whether the store is scalar or one
/// element of a range.
#[test]
fn a_row_of_a_multi_output_discrete_program_keeps_only_its_own_store() {
    use super::super::derived_discrete::row_program;

    let span = Span::from_offsets(SourceId::from_source_name("RowProgram.mo"), 0, 1);
    let program = vec![
        LinearOp::Const { dst: 0, value: 5.0 },
        LinearOp::Const { dst: 1, value: 6.0 },
        LinearOp::Const { dst: 2, value: 7.0 },
        LinearOp::StoreOutput { src: 0 },
        LinearOp::StoreOutputRange {
            start: 1,
            count: 2,
            stride: 1,
        },
    ];
    let block =
        ScalarProgramBlock::with_output_indices(vec![program], vec![span], vec![0, 1, 2]).unwrap();
    for (row, register) in [(0, 0), (1, 1), (2, 2)] {
        let (index, operations, src) = row_program(&block, row).unwrap();
        assert_eq!(index, 0);
        assert_eq!(src, register);
        assert_eq!(operations.len(), 4);
        assert_eq!(
            operations.last(),
            Some(&LinearOp::StoreOutput { src: register })
        );
        assert_eq!(ScalarProgramBlock::program_output_count(&operations), 1);
    }
}
