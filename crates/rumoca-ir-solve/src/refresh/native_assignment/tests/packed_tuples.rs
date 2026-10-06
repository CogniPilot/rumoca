use super::*;
use crate::{ScalarProgramBlock, SolvePureCallSite};

fn span() -> Span {
    Span::from_offsets(SourceId::from_source_name("PackedTuple.mo"), 4, 19)
}

fn operations(count: usize, reverse: bool, scalar_stores: bool) -> Vec<LinearOp> {
    let n = u32::try_from(count).unwrap();
    let mut ops = Vec::new();
    for i in 0..n {
        let r = i * 4;
        ops.extend([
            LinearOp::LoadY {
                dst: r,
                index: i as usize,
            },
            LinearOp::LoadP {
                dst: r + 1,
                index: i as usize,
            },
            LinearOp::Move { dst: r + 2, src: r },
            LinearOp::Binary {
                dst: r + 3,
                op: BinaryOp::Sub,
                lhs: if reverse { r + 1 } else { r + 2 },
                rhs: if reverse { r + 2 } else { r + 1 },
            },
        ]);
    }
    ops.extend((0..n).map(|i| LinearOp::Move {
        dst: 4 * n + i,
        src: 4 * i + 3,
    }));
    if scalar_stores {
        ops.extend((0..n).map(|i| LinearOp::StoreOutput { src: 4 * n + i }));
    } else {
        ops.push(LinearOp::StoreOutputRange {
            start: 4 * n,
            count,
            stride: 1,
        });
    }
    ops
}

fn source(ops: Vec<LinearOp>) -> ComputeBlock {
    ComputeBlock {
        nodes: vec![ComputeNode::ScalarPrograms(
            ScalarProgramBlock::with_program_spans(vec![ops], vec![span()]).unwrap(),
        )],
    }
}

fn layout(count: usize) -> VarLayout {
    VarLayout::from_parts(Default::default(), count, count)
}

fn targets(count: usize) -> Vec<Option<ScalarSlot>> {
    (0..count).map(|i| Some(scalar_slot_y(i))).collect()
}

#[test]
fn move_packed_tuples_keep_one_original_stage_and_ordered_prefix() {
    for (count, reverse, stores) in [(3, false, false), (160, true, false), (14_400, false, true)] {
        let original = operations(count, reverse, stores);
        let split = if stores {
            original.len() - count
        } else {
            original.len() - 1
        };
        let schedule = derive(&source(original.clone()), &targets(count), &layout(count)).unwrap();
        assert_eq!(schedule.stages.len(), 1);
        let stage = &schedule.stages[0];
        assert_eq!(stage.target_range(), Some(0..count));
        let SourceProjection::Scalar {
            program,
            output,
            stores,
        } = &stage.source_projection
        else {
            panic!("original scalar tuple owner")
        };
        assert_eq!((*program, *output), (0, 0));
        assert_eq!(stores, &original[split..]);
        let ComputeNode::ScalarPrograms(block) = &stage.value_kernel().nodes[0] else {
            panic!("one tuple value program")
        };
        assert_eq!(block.program_spans(), [span()]);
        assert!(operations_match(
            &block.programs()[0][..split],
            &original[..split]
        ));
        let projection = &block.programs()[0][split..];
        assert_eq!(projection.len(), count + 1);
        for (i, operation) in projection[..count].iter().enumerate() {
            assert_eq!(
                *operation,
                LinearOp::Move {
                    dst: (5 * count + i) as u32,
                    src: (4 * i + 1) as u32
                }
            );
        }
        assert_eq!(
            projection[count],
            LinearOp::StoreOutputRange {
                start: (5 * count) as u32,
                count,
                stride: 1
            }
        );
    }
}

#[test]
fn move_packed_tuples_refuse_cross_target_values_and_wrong_target_coordinates() {
    for mutation in 0..3 {
        let mut ops = operations(3, false, false);
        match mutation {
            0 => ops[1] = LinearOp::LoadY { dst: 1, index: 2 },
            1 => ops[0] = LinearOp::LoadY { dst: 0, index: 1 },
            _ => {
                ops[3] = LinearOp::Binary {
                    dst: 3,
                    op: BinaryOp::Mul,
                    lhs: 2,
                    rhs: 1,
                }
            }
        }
        assert!(derive(&source(ops), &targets(3), &layout(3)).is_err());
    }
}

#[test]
fn move_packed_tuples_refuse_bounds_effects_overwrites_and_mixed_producers() {
    for mutation in 0..4 {
        let mut ops = operations(3, false, false);
        match mutation {
            0 => ops.insert(12, LinearOp::LoadP { dst: 20, index: 3 }),
            1 => ops.insert(12, LinearOp::LoadSeed { dst: 20, index: 0 }),
            2 => ops.insert(12, LinearOp::Const { dst: 0, value: 1. }),
            _ => ops[13] = LinearOp::Const { dst: 13, value: 0. },
        }
        assert!(derive(&source(ops), &targets(3), &layout(3)).is_err());
    }
}

#[test]
fn move_packed_tuple_invalid_registers_refuse() {
    let mut undefined = operations(3, false, false);
    undefined[12] = LinearOp::Move { dst: 12, src: 21 };
    assert!(ScalarProgramBlock::with_program_spans(vec![undefined], vec![span()]).is_err());
}

fn ignored_input_site() -> SolvePureCallSite {
    use crate::{
        SolveArithmeticProfile, SolveIntegerDomain, SolvePureCallIdentity, SolvePureCallOutput,
        SolvePureCallTable, SolveRealFormat, SolveScalarType, SolveValue, SolveValueType,
    };
    let p = SolveArithmeticProfile::construct(
        SolveRealFormat::Binary64,
        SolveIntegerDomain::construct(i64::MIN, i64::MAX).unwrap(),
    );
    let ty = SolveValueType::scalar(SolveScalarType::real(p));
    let mut table = SolvePureCallTable::builder(p);
    let owner = table
        .add_owner(
            SolvePureCallIdentity::issued(std::num::NonZeroU64::new(1).unwrap()),
            vec![ty.clone()],
            vec![SolvePureCallOutput::result(ty)],
            span(),
            |b, _, out| {
                let zero = b.constant(SolveValue::real(p, 0.), span())?;
                b.store(out[0], zero, span())
            },
        )
        .unwrap();
    table.call_site(owner).unwrap()
}

#[test]
fn move_packed_range_store_checks_even_unused_call_arguments_against_whole_tuple() {
    let site = ignored_input_site();
    for input in [1, 8] {
        let mut ops = operations(3, false, false);
        ops.insert(
            12,
            LinearOp::PureCall {
                dst_start: 20,
                input_starts: vec![input].into_boxed_slice(),
                site: site.clone(),
            },
        );
        let result = derive(&source(ops), &targets(3), &layout(3));
        if input == 8 {
            assert_eq!(
                result.unwrap_err().0,
                "native call inputs depend on its own assignment target"
            );
        } else {
            assert!(result.is_ok());
        }
    }
}

#[test]
fn move_packed_tuple_source_replacement_revokes_original_store_and_literal_owner() {
    let ops = operations(3, false, false);
    let block = source(ops.clone());
    let mut owner = crate::ContinuousRefreshOwners::default();
    owner
        .issue_native_assignment_schedule(&block, &targets(3), &layout(3))
        .unwrap();
    owner
        .validate_native_assignment_schedule(&block, &targets(3), &layout(3))
        .unwrap();
    for mutation in 0..2 {
        let mut edited = ops.clone();
        match mutation {
            0 => {
                edited.insert(
                    12,
                    LinearOp::Const {
                        dst: 20,
                        value: -0.,
                    },
                );
            }
            _ => {
                edited[15] = LinearOp::StoreOutputRange {
                    start: 12,
                    count: 2,
                    stride: 1,
                };
            }
        }
        assert!(
            owner
                .validate_native_assignment_schedule(&source(edited), &targets(3), &layout(3))
                .is_err()
        );
    }
}
