//! Conservative whole-operation dependence over immutable producer ranges.

mod conditional;

use super::*;

pub(super) struct Dependencies {
    dependent: Vec<bool>,
    pub(super) reads: Vec<Range<usize>>,
}

impl Dependencies {
    pub(super) fn depends_on(
        &self,
        prefix: ProgramPrefix<'_>,
        start: Reg,
        count: usize,
        stride: usize,
    ) -> Checked<bool> {
        let width = count
            .checked_sub(1)
            .and_then(|last| last.checked_mul(stride))
            .and_then(|last| last.checked_add(1))
            .ok_or(NativeRefreshAssignmentRefusal(
                "native tensor operand range overflows",
            ))?;
        prefix
            .any_producer_in_range(start, width, |position| self.dependent[position])
            .ok_or(NativeRefreshAssignmentRefusal(
                "native tensor register range overflows",
            ))
    }
}

pub(super) fn derive(
    prefix: ProgramPrefix<'_>,
    target: &Range<usize>,
    layout: &VarLayout,
) -> Checked<Dependencies> {
    derive_scoped(prefix, target, layout, None)
}

fn derive_scoped(
    prefix: ProgramPrefix<'_>,
    target: &Range<usize>,
    layout: &VarLayout,
    captures: Option<usize>,
) -> Checked<Dependencies> {
    // Reuse canonical shape and source-range validation before admitting gathers.
    // ProgramPrefix owns only producer facts, not tensor-rank or static bounds facts.
    let mut result = Dependencies {
        dependent: Vec::with_capacity(prefix.len()),
        reads: Vec::new(),
    };
    for position in 0..prefix.len() {
        let op = prefix
            .operation(position)
            .ok_or(NativeRefreshAssignmentRefusal(
                "invalid native tensor prefix",
            ))?;
        let before = prefix
            .before(position)
            .ok_or(NativeRefreshAssignmentRefusal(
                "invalid native tensor prefix",
            ))?;
        let dependent = match op {
            LinearOp::LoadY { index, .. } => input(
                &mut result.reads,
                TensorInputKind::Y,
                *index,
                1,
                target,
                layout,
            )?,
            LinearOp::LoadP { index, .. } => input(
                &mut result.reads,
                TensorInputKind::P,
                *index,
                1,
                target,
                layout,
            )?,
            LinearOp::TensorLoad {
                input: kind,
                input_start,
                count,
                seed_start: None,
                lanes: 1,
                ..
            } => input(
                &mut result.reads,
                *kind,
                *input_start,
                *count,
                target,
                layout,
            )?,
            LinearOp::LoadFunctionConditionalCapture { index, .. } => {
                conditional::capture(captures, *index, 1)?
            }
            LinearOp::LoadFunctionConditionalCaptureRange {
                index_start, count, ..
            } => conditional::capture(captures, *index_start, *count)?,
            LinearOp::FunctionConditional {
                capture_start,
                program,
                ..
            } => conditional::derive(&mut result, before, *capture_start, program, target, layout)?,
            LinearOp::StoreOutput { src } => result.depends_on(before, *src, 1, 1)?,
            LinearOp::StoreOutputRange {
                start,
                count,
                stride,
            } if captures.is_none() => result.depends_on(before, *start, *count, *stride)?,
            _ => operation_dependency(&result, before, op)?,
        };
        result.dependent.push(dependent);
    }
    Ok(result)
}

/// Consume only the exact per-load facts checked over the complete family.
pub(super) fn derive_family(
    prefix: ProgramPrefix<'_>,
    loads: &[Option<bool>],
) -> Checked<Dependencies> {
    if loads.len() != prefix.len() {
        return refused("native family load dependency inventory has invalid arity");
    }
    let mut result = Dependencies {
        dependent: Vec::with_capacity(prefix.len()),
        reads: Vec::new(),
    };
    for (position, load) in loads.iter().enumerate() {
        let op = prefix
            .operation(position)
            .ok_or(NativeRefreshAssignmentRefusal(
                "invalid native family call prefix",
            ))?;
        let before = prefix
            .before(position)
            .ok_or(NativeRefreshAssignmentRefusal(
                "invalid native family call prefix",
            ))?;
        let dependent = match (op, load) {
            (LinearOp::LoadY { .. } | LinearOp::LoadP { .. }, Some(dependent)) => *dependent,
            (LinearOp::LoadY { .. } | LinearOp::LoadP { .. }, None) | (_, Some(_)) => {
                return refused("native family load dependency inventory is not exact");
            }
            (_, None) => operation_dependency(&result, before, op)?,
        };
        result.dependent.push(dependent);
    }
    Ok(result)
}

fn input(
    reads: &mut Vec<Range<usize>>,
    kind: TensorInputKind,
    start: usize,
    count: usize,
    target: &Range<usize>,
    layout: &VarLayout,
) -> Checked<bool> {
    let end = start
        .checked_add(count)
        .ok_or(NativeRefreshAssignmentRefusal(
            "native tensor load range overflows",
        ))?;
    let capacity = match kind {
        TensorInputKind::Y => layout.y_scalars(),
        TensorInputKind::P => layout.p_scalars(),
    };
    if end > capacity {
        return refused("native tensor load exceeds its owned variable layout");
    }
    if kind == TensorInputKind::P {
        return Ok(false);
    }
    let range = start..end;
    if !overlaps(&range, target) {
        reads.push(range);
        return Ok(false);
    }
    if start < target.start {
        reads.push(start..target.start);
    }
    if end > target.end {
        reads.push(target.end..end);
    }
    Ok(true)
}

fn operation_dependency(
    dependencies: &Dependencies,
    prefix: ProgramPrefix<'_>,
    op: &LinearOp,
) -> Checked<bool> {
    let read = |start, count, stride| dependencies.depends_on(prefix, start, count, stride);
    match op {
        LinearOp::Const { value, .. } if value.is_finite() => Ok(false),
        LinearOp::LoadTime { .. } | LinearOp::TensorIdentity { lanes: 1, .. } => Ok(false),
        LinearOp::Move { src, .. } | LinearOp::Unary { arg: src, .. } => read(*src, 1, 1),
        LinearOp::Binary { lhs, rhs, .. } | LinearOp::Compare { lhs, rhs, .. } => {
            Ok(read(*lhs, 1, 1)? | read(*rhs, 1, 1)?)
        }
        LinearOp::Select {
            cond,
            if_true,
            if_false,
            ..
        } => Ok(read(*cond, 1, 1)? | read(*if_true, 1, 1)? | read(*if_false, 1, 1)?),
        LinearOp::LoadIndexedRegister {
            base,
            stride,
            dimensions,
            indices,
            ..
        } => gather_dependency(dependencies, prefix, *base, *stride, dimensions, indices),
        LinearOp::PureCall {
            input_starts, site, ..
        } if input_starts.len() == site.inputs().len() => {
            let mut dependent = false;
            for (&start, value) in input_starts.iter().zip(site.inputs()) {
                let count = value.scalar_count() as usize;
                if count != 0 {
                    dependent |= read(start, count, 1)?;
                }
            }
            Ok(dependent)
        }
        LinearOp::TensorFill {
            value_start,
            lanes: 1,
            ..
        } => read(*value_start, 1, 1),
        LinearOp::TensorBinary {
            lhs_start,
            rhs_start,
            count,
            lhs_stride,
            rhs_stride,
            lanes: 1,
            ..
        } => Ok(read(*lhs_start, *count, *lhs_stride)? | read(*rhs_start, *count, *rhs_stride)?),
        LinearOp::TensorCross {
            lhs_start,
            rhs_start,
            lanes: 1,
            ..
        } => Ok(read(*lhs_start, 3, 1)? | read(*rhs_start, 3, 1)?),
        LinearOp::MatrixMultiply {
            lhs_start,
            rhs_start,
            rows,
            inner,
            columns,
            lanes: 1,
            ..
        } => Ok(read(*lhs_start, product(&[*rows, *inner])?, 1)?
            | read(*rhs_start, product(&[*inner, *columns])?, 1)?),
        LinearOp::TensorTranspose {
            src_start,
            rows,
            columns,
            element_width,
            lanes: 1,
            ..
        } => read(*src_start, product(&[*rows, *columns, *element_width])?, 1),
        LinearOp::TensorConcatenate {
            sources, lanes: 1, ..
        } => {
            let mut dependent = false;
            for source in sources {
                let count = source
                    .dimensions
                    .iter()
                    .try_fold(1usize, |count, &extent| count.checked_mul(extent as usize))
                    .ok_or(NativeRefreshAssignmentRefusal(
                        "native tensor concatenate extent overflows",
                    ))?;
                dependent |= read(source.start, count, 1)?;
            }
            Ok(dependent)
        }
        _ => refused("native tensor program contains unsupported or effectful operations"),
    }
}

fn gather_dependency(
    dependencies: &Dependencies,
    prefix: ProgramPrefix<'_>,
    base: Reg,
    stride: usize,
    dimensions: &[u32],
    indices: &[crate::TensorIndex],
) -> Checked<bool> {
    let count = dimensions
        .iter()
        .try_fold(1usize, |count, &extent| count.checked_mul(extent as usize))
        .ok_or(NativeRefreshAssignmentRefusal(
            "native gather extent overflows",
        ))?;
    let mut dependent = dependencies.depends_on(prefix, base, count, stride)?;
    for index in indices {
        if let crate::TensorIndex::Runtime(register) = index {
            dependent |= dependencies.depends_on(prefix, *register, 1, 1)?;
        }
    }
    Ok(dependent)
}

fn product(extents: &[usize]) -> Checked<usize> {
    extents
        .iter()
        .try_fold(1usize, |count, &extent| count.checked_mul(extent))
        .ok_or(NativeRefreshAssignmentRefusal(
            "native tensor operand extent overflows",
        ))
}
