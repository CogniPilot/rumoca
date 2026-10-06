//! Native value stages for canonical scalar programs, alongside ranged owners.

use super::super::source_outputs::SourceOutputs;
use super::*;
use crate::{RefreshScalarProgramSource, ScalarProgramBlock, ScalarSlot};

pub(super) fn derive(
    source_node: usize,
    block: &ScalarProgramBlock,
    outputs: &SourceOutputs<'_>,
    targets: &[Option<ScalarSlot>],
    layout: &VarLayout,
) -> Checked<Vec<Family>> {
    block
        .programs()
        .iter()
        .enumerate()
        .map(|(program, operations)| {
            let source = RefreshScalarProgramSource::checked(source_node, program).ok_or(
                NativeRefreshAssignmentRefusal("native scalar source identity overflows"),
            )?;
            let output = outputs
                .get(source, 0)
                .ok_or(NativeRefreshAssignmentRefusal(
                    "native scalar output has no source owner",
                ))?;
            let span = block.program_spans().get(program).copied().ok_or(
                NativeRefreshAssignmentRefusal("native scalar program has no provenance"),
            )?;
            if matches!(operations.last(), Some(LinearOp::StoreOutputRange { .. }))
                || ScalarProgramBlock::program_output_count(operations) > 1
            {
                let output_range =
                    outputs
                        .contiguous_range(source)
                        .ok_or(NativeRefreshAssignmentRefusal(
                            "native tensor outputs are not one contiguous owned range",
                        ))?;
                return super::range::derive(
                    source,
                    output_range,
                    operations,
                    span,
                    targets,
                    layout,
                );
            }
            derive_program(source, output, operations, span, targets, layout)
        })
        .collect()
}

fn derive_program(
    source: RefreshScalarProgramSource,
    output: usize,
    operations: &[LinearOp],
    span: rumoca_core::Span,
    targets: &[Option<ScalarSlot>],
    layout: &VarLayout,
) -> Checked<Family> {
    let Some(LinearOp::StoreOutput { .. }) = operations.last() else {
        return refused("native scalar program requires one terminal scalar output");
    };
    if ScalarProgramBlock::program_output_count(operations) != 1 {
        return refused("native scalar program requires one terminal scalar output");
    }
    let target = match targets.get(output).copied().flatten() {
        Some(ScalarSlot::Y { index, byte_offset })
            if index.checked_mul(8) == Some(byte_offset) && index < layout.y_scalars() =>
        {
            index
        }
        _ => return refused("native scalar output has no bounded owned Y target"),
    };
    let prefix = &operations[..operations.len() - 1];
    crate::ScalarProgramRegisterFlow::derive(prefix).map_err(|_| {
        NativeRefreshAssignmentRefusal("native scalar prefix has invalid register flow")
    })?;
    let reads = if prefix
        .iter()
        .any(|operation| matches!(operation, LinearOp::FunctionConditional { .. }))
    {
        super::range::checked_conditional_inputs(prefix, target..target + 1, layout)?
    } else {
        reads(prefix, target, layout)?
    };
    if prefix.iter().any(|operation| {
        matches!(
            operation,
            LinearOp::PureCall { .. } | LinearOp::LoadIndexedRegister { .. }
        )
    }) {
        super::range::checked_call_inputs(prefix, target..target + 1, layout)?;
    }
    let shape = derive_target_assignment_shape_for_output(operations, 0, target).ok_or(
        NativeRefreshAssignmentRefusal("native scalar program has no exact target isolator"),
    )?;
    if !matches!(
        shape,
        TargetAssignmentShape::Direct { .. } | TargetAssignmentShape::Zero { .. }
    ) {
        return refused("native scalar program requires an unsupported non-direct isolator");
    }
    let mut values = prefix.to_vec();
    let (value, _) = materialize_target_assignment(&shape, &mut values).ok_or(
        NativeRefreshAssignmentRefusal("native scalar materialization overflows"),
    )?;
    values.push(LinearOp::StoreOutput { src: value });
    let value_kernel = ScalarProgramBlock::with_program_spans(vec![values], vec![span])
        .map_err(|_| NativeRefreshAssignmentRefusal("malformed native scalar value projection"))?;
    Ok(Family {
        stage: NativeRefreshAssignmentStage {
            source: NativeStageSource::Continuous {
                node: source.node() as usize,
            },
            targets: coverage::Coverage::dense(target..target + 1),
            value_kernel: ComputeBlock {
                nodes: vec![ComputeNode::ScalarPrograms(value_kernel)],
            },
        },
        reads: reads.into_iter().map(coverage::Coverage::dense).collect(),
        outputs: output
            ..output.checked_add(1).ok_or(NativeRefreshAssignmentRefusal(
                "native scalar output range overflows",
            ))?,
    })
}

fn reads(operations: &[LinearOp], target: usize, layout: &VarLayout) -> Checked<Vec<Range<usize>>> {
    let mut reads = Vec::new();
    for operation in operations {
        match operation {
            LinearOp::LoadY { index, .. } if *index < layout.y_scalars() => {
                if *index != target {
                    reads.push(*index..*index + 1);
                }
            }
            LinearOp::LoadP { index, .. } if *index < layout.p_scalars() => {}
            LinearOp::TensorLoad {
                input,
                input_start,
                count,
                seed_start: None,
                lanes: 1,
                ..
            } => {
                reads.extend(tensor_reads(*input, *input_start, *count, target, layout)?);
            }
            LinearOp::PureCall {
                input_starts, site, ..
            } if input_starts.len() == site.inputs().len()
                && site.output_scalar_count().is_some()
                && input_starts
                    .iter()
                    .zip(site.inputs())
                    .all(|(&start, value)| {
                        u64::from(start) + u64::from(value.scalar_count())
                            <= u64::from(crate::Reg::MAX) + 1
                    }) => {}
            LinearOp::LoadY { .. } | LinearOp::LoadP { .. } => {
                return refused("native scalar load exceeds its owned variable layout");
            }
            LinearOp::Const { value, .. } if value.is_finite() => {}
            LinearOp::LoadIndexedRegister { .. }
            | LinearOp::Move { .. }
            | LinearOp::LoadTime { .. }
            | LinearOp::Unary { .. }
            | LinearOp::Binary { .. }
            | LinearOp::Compare { .. }
            | LinearOp::Select { .. } => {}
            _ => {
                return refused(
                    "native scalar program contains unsupported or effectful operations",
                );
            }
        }
    }
    Ok(reads)
}

fn tensor_reads(
    input: crate::TensorInputKind,
    start: usize,
    count: usize,
    target: usize,
    layout: &VarLayout,
) -> Checked<Vec<Range<usize>>> {
    let end = start
        .checked_add(count)
        .ok_or(NativeRefreshAssignmentRefusal(
            "native scalar tensor load overflows",
        ))?;
    let limit = match input {
        crate::TensorInputKind::Y => layout.y_scalars(),
        crate::TensorInputKind::P => layout.p_scalars(),
    };
    if end > limit {
        return refused("native scalar tensor load exceeds its owned variable layout");
    }
    if input == crate::TensorInputKind::P {
        return Ok(Vec::new());
    }
    let mut reads = Vec::new();
    if start < target.min(end) {
        reads.push(start..target.min(end));
    }
    let upper = target.saturating_add(1).max(start);
    if end > upper {
        reads.push(upper..end);
    }
    Ok(reads)
}

/// The value stage of one derived-discrete output: its discrete row program,
/// every derived read rebound, publishing the stored register into the
/// output's private work slot. A discrete row is already a value program, so
/// no isolator is involved.
pub(super) fn derive_discrete(
    output: &NativeDerivedOutput,
    rhs: &ScalarProgramBlock,
    rebinding: &std::collections::BTreeMap<usize, usize>,
    layout: &VarLayout,
) -> Result<Family, NativeScheduleRefusal> {
    let program = rhs
        .program_index_for_output(output.row)
        .ok_or(NativeEvaluationRefusal::UnownedDiscreteRow)?;
    let source = rhs
        .program(program)
        .ok_or(NativeEvaluationRefusal::UnownedDiscreteRow)?;
    let span = rhs
        .program_span(program)
        .ok_or(NativeRefreshAssignmentRefusal(
            "native discrete program has no provenance",
        ))?;
    let operations = super::rebinding::rebind_operations(source, rebinding)?;
    let Some(LinearOp::StoreOutput { .. }) = operations.last() else {
        return Err(NativeEvaluationRefusal::MultiOutputDiscreteProgram.into());
    };
    let target = output.work_index;
    let prefix = &operations[..operations.len() - 1];
    crate::ScalarProgramRegisterFlow::derive(prefix).map_err(|_| {
        NativeRefreshAssignmentRefusal("native discrete prefix has invalid register flow")
    })?;
    let reads = if prefix
        .iter()
        .any(|operation| matches!(operation, LinearOp::FunctionConditional { .. }))
    {
        super::range::checked_conditional_inputs(prefix, target..target + 1, layout)?
    } else {
        reads(prefix, target, layout)?
    };
    if prefix.iter().any(|operation| {
        matches!(
            operation,
            LinearOp::PureCall { .. } | LinearOp::LoadIndexedRegister { .. }
        )
    }) {
        super::range::checked_call_inputs(prefix, target..target + 1, layout)?;
    }
    let value_kernel = ScalarProgramBlock::with_program_spans(vec![operations], vec![span])
        .map_err(|_| NativeRefreshAssignmentRefusal("malformed native discrete value program"))?;
    Ok(Family {
        stage: NativeRefreshAssignmentStage {
            source: NativeStageSource::Discrete { row: output.row },
            targets: coverage::Coverage::dense(target..target + 1),
            value_kernel: ComputeBlock {
                nodes: vec![ComputeNode::ScalarPrograms(value_kernel)],
            },
        },
        reads: reads.into_iter().map(coverage::Coverage::dense).collect(),
        outputs: 0..0,
    })
}
