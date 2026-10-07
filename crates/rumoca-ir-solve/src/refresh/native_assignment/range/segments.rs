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
    targets: &[Option<ScalarSlot>],
    layout: &VarLayout,
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
    let owned = target_range(targets, outputs.clone(), layout)?;
    let producers = UniqueProgram::new(&prefix).ok_or(NativeRefreshAssignmentRefusal(
        "native segmented program has overlapping destination versions",
    ))?;
    let dependencies = dependency::derive(producers.view(), &owned, layout)?;
    independent_call_inputs(producers.view(), &dependencies)?;
    let mut values = prefix.clone();
    let mut stores = Vec::with_capacity(segments.len());
    let mut first = owned.start;
    for segment in segments {
        if segment.stride != 1 {
            return refused("native tensor residual requires a contiguous terminal range");
        }
        let target = first..first + segment.count;
        let (value, stride) = isolated_value(
            producers.view(),
            segment.start,
            segment.count,
            &target,
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
        first = target.end;
    }
    values.extend(stores);
    let value_kernel =
        ScalarProgramBlock::with_program_spans(vec![values], vec![span]).map_err(|_| {
            NativeRefreshAssignmentRefusal("malformed native segmented value projection")
        })?;
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
