use super::*;

fn ranged_source(count: usize) -> ComputeBlock {
    ranged_source_with_fill(count, false)
}

fn ranged_source_with_fill(count: usize, repeated: bool) -> ComputeBlock {
    let width = u32::try_from(count).unwrap();
    let span = rumoca_core::Span::from_offsets(
        rumoca_core::SourceId::from_source_name("NativeRefreshRange.mo"),
        0,
        1,
    );
    ComputeBlock::from_scalar_program_block(
        ScalarProgramBlock::with_program_spans(
            vec![vec![
                LinearOp::TensorLoad {
                    dst_start: 0,
                    input: crate::TensorInputKind::Y,
                    input_start: 0,
                    count,
                    seed_start: None,
                    lanes: 1,
                },
                if repeated {
                    LinearOp::Const {
                        dst: width,
                        value: -0.0,
                    }
                } else {
                    LinearOp::TensorLoad {
                        dst_start: width,
                        input: crate::TensorInputKind::P,
                        input_start: 0,
                        count,
                        seed_start: None,
                        lanes: 1,
                    }
                },
                LinearOp::TensorBinary {
                    dst_start: if repeated { width + 1 } else { 2 * width },
                    op: crate::BinaryOp::Sub,
                    lhs_start: 0,
                    rhs_start: width,
                    count,
                    lhs_stride: 1,
                    rhs_stride: usize::from(!repeated),
                    lanes: 1,
                },
                LinearOp::StoreOutputRange {
                    start: if repeated { width + 1 } else { 2 * width },
                    count,
                    stride: 1,
                },
            ]],
            vec![span],
        )
        .unwrap(),
    )
}

#[test]
fn exact_refresh_issues_one_checked_fill_for_a_broadcast_direct_value() {
    let source = ranged_source_with_fill(120, true);
    let owners = range_owners(&source, &(0..120).collect::<Vec<_>>());
    let schedule = owners
        .exact_assignment_schedule(owners.algebraic().dynamic_causal_sequence)
        .unwrap();
    let shared = schedule.shared_segments(&source, &owners).unwrap();
    let segment = &shared.segments().segments()[0];
    assert_eq!(segment.ops().len(), 4);
    assert!(
        matches!(segment.ops()[1], LinearOp::Const { value, .. } if value.to_bits() == (-0.0f64).to_bits())
    );
    assert_eq!(
        segment.ops()[2],
        LinearOp::TensorFill {
            dst_start: 121,
            value_start: 120,
            count: 120,
            lanes: 1
        }
    );
    assert_eq!(
        segment.ops()[3],
        LinearOp::StoreOutputRange {
            start: 121,
            count: 120,
            stride: 1
        }
    );
}

fn range_owners(source: &ComputeBlock, order: &[usize]) -> ContinuousRefreshOwners {
    let (operations, _) =
        scalar_source_program(source, RefreshScalarProgramSource::checked(0, 0).unwrap())
            .unwrap()
            .unwrap();
    let rows = order
        .iter()
        .map(|&offset| AlgebraicRefreshRow {
            owner_id: RefreshRowOwnerId::checked(offset).unwrap(),
            source: RefreshScalarProgramSource::checked(0, 0).unwrap(),
            equation_index: offset,
            output_offset: offset,
            target_index: offset,
            assignment_target: Some(offset),
            assignment_shape: canonical_assignment_shape_for_output(operations, offset, offset),
            direct_assignment_certified: true,
            exact_assignment_certified: true,
        })
        .collect::<Vec<_>>();
    let plan = RefreshPlan {
        dynamic_causal_seed_rows: selection(rows.len(), 0..rows.len()),
        rows,
        ..RefreshPlan::default()
    };
    ContinuousRefreshOwners::checked_for_source(
        source,
        plan,
        RefreshPlan::default(),
        RefreshPlan::default(),
        RefreshPlan::default(),
        Vec::new(),
    )
    .unwrap()
}

#[test]
fn exact_refresh_preserves_source_range_through_shared_schedule() {
    for count in [12, 120] {
        let source = ranged_source(count);
        let owners = range_owners(&source, &(0..count).collect::<Vec<_>>());
        let schedule = owners
            .exact_assignment_schedule(owners.algebraic().dynamic_causal_sequence)
            .unwrap();
        let shared = schedule.shared_segments(&source, &owners).unwrap();
        let [segment] = shared.segments().segments() else {
            panic!("one source range must retain one correlated assignment program");
        };
        assert_eq!(segment.ops().len(), 3, "operation count follows the source");
        assert_eq!(
            segment.ops().last(),
            Some(&LinearOp::StoreOutputRange {
                start: u32::try_from(count).unwrap(),
                count,
                stride: 1,
            }),
            "the shared schedule must retain its original source range"
        );
        assert_eq!(segment.targets(), (0..count).collect::<Vec<_>>());
    }
}

#[test]
fn exact_refresh_retains_selected_source_subrange_and_row_order() {
    let source = ranged_source(6);
    for order in [vec![1, 2, 3], vec![3, 1, 2]] {
        let owners = range_owners(&source, &order);
        let schedule = owners
            .exact_assignment_schedule(owners.algebraic().dynamic_causal_sequence)
            .unwrap();
        let shared = schedule.shared_segments(&source, &owners).unwrap();
        let segment = &shared.segments().segments()[0];
        assert_eq!(segment.targets(), order);
        let ranges = segment
            .ops()
            .iter()
            .filter_map(|op| match op {
                LinearOp::StoreOutputRange {
                    start,
                    count,
                    stride,
                } => Some((*start, *count, *stride)),
                _ => None,
            })
            .collect::<Vec<_>>();
        if order == [1, 2, 3] {
            assert_eq!(ranges, [(7, 3, 1)]);
        } else {
            assert_eq!(ranges, [(7, 2, 1)]);
            assert!(segment.ops().contains(&LinearOp::StoreOutput { src: 9 }));
        }
    }
}
