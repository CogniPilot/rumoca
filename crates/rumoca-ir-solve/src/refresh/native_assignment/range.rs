//! Compact direct assignments from one exact tensor residual program.

mod dependency;
mod scalar_tuple;
mod segments;

use super::*;
use crate::refresh::assignment_shape::producers::{ProgramPrefix, UniqueProgram};
use crate::{
    BinaryOp, RefreshScalarProgramSource, Reg, ScalarProgramBlock, ScalarSlot, TensorInputKind,
};
use segments::{derive_segments, store_segments, trailing_scalar_tuple};

/// Returned values alone do not prove a call's numerical-failure dependence.
/// Every complete input cell must be independent of this stage's unknown target.
pub(super) fn checked_call_inputs(
    operations: &[LinearOp],
    target: Range<usize>,
    layout: &VarLayout,
) -> Checked<()> {
    crate::ScalarProgramRegisterFlow::derive(operations).map_err(|_| {
        NativeRefreshAssignmentRefusal("native call prefix has invalid register flow")
    })?;
    let producers = UniqueProgram::new(operations).ok_or(NativeRefreshAssignmentRefusal(
        "native call prefix has overlapping destination versions",
    ))?;
    let dependencies = dependency::derive(producers.view(), &target, layout)?;
    independent_call_inputs(producers.view(), &dependencies)
}

/// A checked conditional has independent region register namespaces. Its
/// complete captures and every region's fault-sensitive prefix are checked,
/// while the canonical dependency owner retains the exact target isolator.
pub(super) fn checked_conditional_inputs(
    operations: &[LinearOp],
    target: Range<usize>,
    layout: &VarLayout,
) -> Checked<Vec<Range<usize>>> {
    crate::ScalarProgramRegisterFlow::derive(operations).map_err(|_| {
        NativeRefreshAssignmentRefusal("native conditional prefix has invalid register flow")
    })?;
    let producers = UniqueProgram::new(operations).ok_or(NativeRefreshAssignmentRefusal(
        "native conditional prefix has overlapping destination versions",
    ))?;
    let dependencies = dependency::derive(producers.view(), &target, layout)?;
    independent_call_inputs(producers.view(), &dependencies)?;
    Ok(dependencies.reads)
}

/// Full-domain load dependence is established by the family address/coverage
/// owner, rather than treating the template point as a scalar source program.
pub(super) fn checked_family_call_inputs(
    operations: &[LinearOp],
    load_dependencies: &[Option<bool>],
) -> Checked<()> {
    crate::ScalarProgramRegisterFlow::derive(operations).map_err(|_| {
        NativeRefreshAssignmentRefusal("native family call prefix has invalid register flow")
    })?;
    let producers = UniqueProgram::new(operations).ok_or(NativeRefreshAssignmentRefusal(
        "native call prefix has overlapping destination versions",
    ))?;
    let dependencies = dependency::derive_family(producers.view(), load_dependencies)?;
    independent_call_inputs(producers.view(), &dependencies)
}

fn independent_call_inputs(
    prefix: ProgramPrefix<'_>,
    dependencies: &dependency::Dependencies,
) -> Checked<()> {
    for position in 0..prefix.len() {
        let operation = prefix
            .operation(position)
            .ok_or(NativeRefreshAssignmentRefusal("invalid native call prefix"))?;
        let before = prefix
            .before(position)
            .ok_or(NativeRefreshAssignmentRefusal("invalid native call prefix"))?;
        if let LinearOp::LoadIndexedRegister { indices, .. } = operation {
            independent_gather_indices(before, dependencies, indices)?;
        }
        let LinearOp::PureCall {
            input_starts, site, ..
        } = operation
        else {
            continue;
        };
        let before = prefix
            .before(position)
            .ok_or(NativeRefreshAssignmentRefusal("invalid native call prefix"))?;
        for (&start, value) in input_starts.iter().zip(site.inputs()) {
            let count = value.scalar_count() as usize;
            if count != 0 && dependencies.depends_on(before, start, count, 1)? {
                return refused("native call inputs depend on its own assignment target");
            }
        }
    }
    Ok(())
}

fn independent_gather_indices(
    before: ProgramPrefix<'_>,
    dependencies: &dependency::Dependencies,
    indices: &[crate::TensorIndex],
) -> Checked<()> {
    for index in indices {
        if let crate::TensorIndex::Runtime(register) = index
            && dependencies.depends_on(before, *register, 1, 1)?
        {
            return refused("native gather address depends on its own assignment target");
        }
    }
    Ok(())
}

pub(super) fn derive(
    source: RefreshScalarProgramSource,
    outputs: Range<usize>,
    operations: &[LinearOp],
    span: rumoca_core::Span,
    targets: &[Option<ScalarSlot>],
    layout: &VarLayout,
) -> Checked<Family> {
    let segments = store_segments(operations);
    if segments.len() > 1 && !trailing_scalar_tuple(operations, &segments) {
        return derive_segments(
            source, outputs, operations, &segments, span, targets, layout,
        );
    }
    let (prefix, stores, start, count) = terminal_outputs(operations)?;
    crate::ScalarProgramRegisterFlow::derive(prefix).map_err(|_| {
        NativeRefreshAssignmentRefusal("native tensor prefix has invalid register flow")
    })?;
    if count == 0 || outputs.len() != count {
        return refused("native tensor program has multiple or empty output ranges");
    }
    let target_range = target_range(targets, outputs.clone(), layout)?;
    if stores.len() > 1
        && prefix
            .iter()
            .any(|op| matches!(op, LinearOp::PureCall { .. }))
    {
        checked_call_inputs(prefix, target_range.clone(), layout)?;
    }
    let producers = UniqueProgram::new(prefix).ok_or(NativeRefreshAssignmentRefusal(
        "native tensor program has overlapping destination versions",
    ))?;
    let dependencies = dependency::derive(producers.view(), &target_range, layout)?;
    independent_call_inputs(producers.view(), &dependencies)?;
    // Retain every original operation, even unused/fault-sensitive prefixes.
    // Only the final output switches from the residual to its exact value.
    let mut values = prefix.to_vec();
    let (value, stride) = if scalar_tuple::is_packed(producers.view(), start) {
        crate::ScalarProgramRegisterFlow::derive(prefix).map_err(|_| {
            NativeRefreshAssignmentRefusal("native scalar tuple has invalid register flow")
        })?;
        independent_call_inputs(producers.view(), &dependencies)?;
        let registers =
            scalar_tuple::derive(producers.view(), start, &target_range, &dependencies)?;
        (scalar_tuple::pack(&mut values, &registers)?, 1)
    } else {
        isolated_value(producers.view(), start, count, &target_range, &dependencies)?
    };
    let value = if stride == 0 {
        let start = next_register(prefix)?;
        values.push(LinearOp::TensorFill {
            dst_start: start,
            value_start: value,
            count,
            lanes: 1,
        });
        start
    } else {
        value
    };
    values.push(LinearOp::StoreOutputRange {
        start: value,
        count,
        stride: stride.max(1),
    });
    let value_kernel = ScalarProgramBlock::with_program_spans(vec![values], vec![span])
        .map_err(|_| NativeRefreshAssignmentRefusal("malformed native tensor value projection"))?;
    Ok(Family {
        stage: NativeRefreshAssignmentStage {
            integer_bindings: Default::default(),
            source: NativeStageSource::Continuous {
                node: source.node() as usize,
            },
            targets: coverage::Coverage::dense(target_range),
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

/// One original program may end in a range store or consecutive scalar stores.
/// This derives only their register projection, retaining every original store.
fn terminal_outputs(operations: &[LinearOp]) -> Checked<(&[LinearOp], &[LinearOp], Reg, usize)> {
    if let Some(LinearOp::StoreOutputRange {
        start,
        count,
        stride,
    }) = operations.last()
    {
        if *stride != 1 {
            return refused("native tensor residual requires a contiguous terminal range");
        }
        let split = operations.len() - 1;
        return Ok((&operations[..split], &operations[split..], *start, *count));
    }
    let count = operations
        .iter()
        .rev()
        .take_while(|op| matches!(op, LinearOp::StoreOutput { .. }))
        .count();
    if count < 2 || ScalarProgramBlock::program_output_count(operations) != count {
        return refused("native scalar program requires one terminal scalar output");
    }
    let split = operations.len() - count;
    let stores = &operations[split..];
    let LinearOp::StoreOutput { src: start } = stores[0] else {
        return refused("native scalar terminal output projection is malformed");
    };
    for (offset, store) in stores.iter().enumerate() {
        let expected = u32::try_from(offset)
            .ok()
            .and_then(|i| start.checked_add(i));
        if !matches!(store, LinearOp::StoreOutput { src } if Some(*src) == expected) {
            return refused("native scalar terminal outputs are not consecutive registers");
        }
    }
    Ok((&operations[..split], stores, start, count))
}

fn next_register(operations: &[LinearOp]) -> Checked<Reg> {
    let end = operations
        .iter()
        .try_fold(0u64, |end, operation| {
            let Some(start) = operation.dst_register() else {
                return Some(end);
            };
            Some(
                end.max(
                    u64::from(start)
                        .checked_add(u64::try_from(operation.dst_register_count()).ok()?)?,
                ),
            )
        })
        .and_then(|end| Reg::try_from(end).ok());
    end.ok_or(NativeRefreshAssignmentRefusal(
        "native tensor broadcast register range overflows",
    ))
}

fn target_range(
    targets: &[Option<ScalarSlot>],
    outputs: Range<usize>,
    layout: &VarLayout,
) -> Checked<Range<usize>> {
    let selected = targets.get(outputs).ok_or(NativeRefreshAssignmentRefusal(
        "native tensor outputs exceed the target inventory",
    ))?;
    let index = |slot| match slot {
        Some(ScalarSlot::Y { index, byte_offset }) if index.checked_mul(8) == Some(byte_offset) => {
            Some(index)
        }
        _ => None,
    };
    let start = selected
        .first()
        .copied()
        .and_then(index)
        .ok_or(NativeRefreshAssignmentRefusal(
            "native tensor output has no owned Y target",
        ))?;
    let end = start
        .checked_add(selected.len())
        .ok_or(NativeRefreshAssignmentRefusal(
            "native tensor target range overflows",
        ))?;
    if end > layout.y_scalars()
        || selected
            .iter()
            .enumerate()
            .any(|(offset, &slot)| index(slot) != start.checked_add(offset))
    {
        return refused("native tensor targets are not one bounded dense injective range");
    }
    Ok(start..end)
}

fn isolated_value(
    prefix: ProgramPrefix<'_>,
    output: Reg,
    count: usize,
    target: &Range<usize>,
    dependencies: &dependency::Dependencies,
) -> Checked<(Reg, usize)> {
    let position = prefix
        .producer_position(output)
        .ok_or(NativeRefreshAssignmentRefusal(
            "native tensor residual has no exact producer",
        ))?;
    // An elementwise residual `load - value` (either order): a tensor
    // difference over the whole store, or one scalar difference.
    let (lhs, lhs_stride, rhs, rhs_stride) = match prefix.operation(position) {
        Some(LinearOp::TensorBinary {
            dst_start,
            op: BinaryOp::Sub,
            lhs_start,
            rhs_start,
            count: residual_count,
            lhs_stride,
            rhs_stride,
            lanes: 1,
        }) => {
            if *dst_start != output || *residual_count != count {
                return refused("native tensor residual selects a partial producer range");
            }
            (*lhs_start, *lhs_stride, *rhs_start, *rhs_stride)
        }
        Some(LinearOp::Binary {
            dst,
            op: BinaryOp::Sub,
            lhs,
            rhs,
        }) if *dst == output && count == 1 => (*lhs, 1, *rhs, 1),
        _ => return refused("native tensor residual has no direct elementwise isolator"),
    };
    let before = prefix
        .before(position)
        .ok_or(NativeRefreshAssignmentRefusal(
            "invalid native tensor prefix",
        ))?;
    for (load, load_stride, value, value_stride) in [
        (lhs, lhs_stride, rhs, rhs_stride),
        (rhs, rhs_stride, lhs, lhs_stride),
    ] {
        if load_stride == 1
            && exact_target_load(before, load, target)
            && !dependencies.depends_on(before, value, count, value_stride)?
        {
            return Ok((value, value_stride));
        }
    }
    refused("native tensor residual couples its owned target range")
}

fn exact_target_load(prefix: ProgramPrefix<'_>, register: Reg, target: &Range<usize>) -> bool {
    if target.is_empty() {
        return false;
    }
    let producer = prefix
        .producer_position(register)
        .and_then(|position| prefix.operation(position));
    if let Some(LinearOp::TensorLoad { .. }) = producer {
        return matches!(producer, Some(LinearOp::TensorLoad { dst_start, input: TensorInputKind::Y,
            input_start, count, seed_start: None, lanes: 1 }
            ) if *dst_start == register && *input_start == target.start && *count == target.len());
    }
    // This only inspects already-issued scalar producers. It creates no load
    // range or scalar owners, and the complete original prefix stays required.
    (0..target.len()).all(|offset| {
        let Some(reg) = u32::try_from(offset)
            .ok()
            .and_then(|i| register.checked_add(i))
        else {
            return false;
        };
        prefix
            .producer_position(reg)
            .and_then(|position| prefix.operation(position))
            .is_some_and(|op| {
                matches!(op, LinearOp::LoadY { dst, index }
                if *dst == reg && target.start.checked_add(offset) == Some(*index))
            })
    })
}
