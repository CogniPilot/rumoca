//! One stage from a program that stores several output segments
//! (SPEC_0040 SOLVE-C68): the fields of one record-valued pure call are
//! stored as consecutive segments, each after its own residual operations.

use super::*;

/// One store of a program: `count` outputs from register `start` on.
#[derive(Clone, Copy)]
pub(super) struct Segment {
    pub(super) start: Reg,
    pub(super) count: usize,
    pub(super) stride: usize,
}

/// Every output store of `operations`, in program order.
pub(super) fn store_segments(operations: &[LinearOp]) -> Vec<Segment> {
    operations
        .iter()
        .filter_map(|operation| match *operation {
            LinearOp::StoreOutput { src } => Some(Segment {
                start: src,
                count: 1,
                stride: 1,
            }),
            LinearOp::StoreOutputRange {
                start,
                count,
                stride,
            } => Some(Segment {
                start,
                count,
                stride,
            }),
            _ => None,
        })
        .collect()
}

/// Whether the stores are the packed scalar tuple form: scalar stores of
/// consecutive registers that end the program.
pub(super) fn trailing_scalar_tuple(operations: &[LinearOp], segments: &[Segment]) -> bool {
    let trailing = operations
        .iter()
        .rev()
        .take_while(|op| matches!(op, LinearOp::StoreOutput { .. }))
        .count();
    trailing == segments.len()
        && segments
            .windows(2)
            .all(|pair| pair[0].start.checked_add(1) == Some(pair[1].start))
}

/// The stage of a multi-segment program whose segment targets, in store
/// order, form one dense injective Y range.
pub(super) fn derive_segments(
    source: RefreshScalarProgramSource,
    outputs: Range<usize>,
    operations: &[LinearOp],
    segments: &[Segment],
    span: rumoca_core::Span,
    layout: &VarLayout,
    target: impl FnOnce() -> Checked<Range<usize>>,
) -> Checked<Family> {
    derive_paired_segments(source, outputs, operations, segments, span, layout, || {
        let owned = target()?;
        let mut first = owned.start;
        Ok(segments
            .iter()
            .map(|segment| {
                let run = first..first + segment.count;
                first = run.end;
                run
            })
            .collect())
    })
}

pub(super) fn derive_paired_segments(
    source: RefreshScalarProgramSource,
    outputs: Range<usize>,
    operations: &[LinearOp],
    segments: &[Segment],
    span: rumoca_core::Span,
    layout: &VarLayout,
    targets: impl FnOnce() -> Checked<Vec<Range<usize>>>,
) -> Checked<Family> {
    let total = segments
        .iter()
        .try_fold(0usize, |total, segment| total.checked_add(segment.count))
        .ok_or(NativeRefreshAssignmentRefusal(
            "native segment count overflows",
        ))?;
    if total != outputs.len() || segments.iter().any(|segment| segment.count == 0) {
        return refused("native segmented program does not store its owned outputs once");
    }
    // Stores define no register, so the value prefix is the program without
    // them, with every register numbered as in the source.
    let prefix = operations
        .iter()
        .filter(|op| {
            !matches!(
                op,
                LinearOp::StoreOutput { .. } | LinearOp::StoreOutputRange { .. }
            )
        })
        .cloned()
        .collect::<Vec<_>>();
    crate::ScalarProgramRegisterFlow::derive(&prefix).map_err(|_| {
        NativeRefreshAssignmentRefusal("native segmented prefix has invalid register flow")
    })?;
    let targets = targets()?;
    let owned = checked_segment_targets(&targets, segments, layout)?;
    let producers = UniqueProgram::new(&prefix).ok_or(NativeRefreshAssignmentRefusal(
        "native segmented program has overlapping destination versions",
    ))?;
    let dependencies = dependency::derive(producers.view(), &owned, layout)?;
    independent_call_inputs(producers.view(), &dependencies)?;
    let mut values = prefix.clone();
    let mut stores = Vec::with_capacity(segments.len());
    for (segment, target) in segments.iter().zip(&targets) {
        if segment.stride != 1 {
            return refused("native tensor residual requires a contiguous terminal range");
        }
        let (value, stride) = isolated_value(
            producers.view(),
            segment.start,
            segment.count,
            target,
            &dependencies,
        )?;
        let value = if stride == 0 {
            let start = next_register(&values)?;
            values.push(LinearOp::TensorFill {
                dst_start: start,
                value_start: value,
                count: segment.count,
                lanes: 1,
            });
            start
        } else {
            value
        };
        stores.push(LinearOp::StoreOutputRange {
            start: value,
            count: segment.count,
            stride: stride.max(1),
        });
    }
    values.extend(stores);
    let values = crate::prune_unread_pure_operations(values);
    let value_kernel = ScalarProgramBlock::with_program_output_placements(
        vec![values],
        vec![span],
        vec![crate::ScalarProgramOutputPlacement::Segments(
            targets
                .iter()
                .map(|target| {
                    crate::ScalarOutputSegment::Range(
                        target.start - owned.start..target.end - owned.start,
                    )
                })
                .collect(),
        )],
    )
    .map_err(|_| NativeRefreshAssignmentRefusal("malformed native segmented value projection"))?;
    Ok(Family {
        stage: NativeRefreshAssignmentStage {
            integer_bindings: Default::default(),
            source: NativeStageSource::Continuous {
                node: source.node() as usize,
            },
            targets: coverage::Coverage::dense(owned),
            value_kernel: ComputeBlock {
                nodes: vec![ComputeNode::ScalarPrograms(value_kernel)],
            },
        },
        reads: dependencies
            .reads
            .into_iter()
            .map(coverage::Coverage::dense)
            .collect(),
        outputs,
    })
}

fn checked_segment_targets(
    targets: &[Range<usize>],
    segments: &[Segment],
    layout: &VarLayout,
) -> Checked<Range<usize>> {
    if targets.len() != segments.len()
        || targets.iter().zip(segments).any(|(target, segment)| {
            target.start > target.end
                || target.len() != segment.count
                || target.end > layout.y_scalars()
        })
    {
        return refused("native tensor targets are not one bounded dense injective range");
    }
    let mut ordered = targets.to_vec();
    ordered.sort_by_key(|target| target.start);
    let Some(first) = ordered.first() else {
        return refused("native segmented target inventory is empty");
    };
    let start = first.start;
    let mut end = start;
    for target in ordered {
        if target.start != end {
            return refused("native tensor targets are not one bounded dense injective range");
        }
        end = target.end;
    }
    Ok(start..end)
}
