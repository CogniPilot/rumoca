use super::*;

pub(super) fn eval_prepared_affine_node(
    node: &PreparedComputeNode,
    y: &[f64],
    p: &[f64],
    t: f64,
    context: RowEvalContext<'_>,
    out: &mut [f64],
    scratch: &mut RowEvalScratch,
) -> Result<(), EvalSolveError> {
    let PreparedComputeNode::Affine {
        program,
        scalar_count,
        extents,
        ordinal_strides,
        output_start,
        output_strides,
        load_adjustments,
        const_adjustments,
        span,
        ..
    } = node
    else {
        return Err(invalid_prepared_row(
            "prepared affine evaluator received a non-affine node",
        ));
    };
    eval_prepared_affine(
        PreparedAffineEval {
            program,
            scalar_count: *scalar_count,
            extents,
            ordinal_strides,
            output_start: *output_start,
            output_strides,
            load_adjustments,
            const_adjustments,
            span: *span,
        },
        y,
        p,
        t,
        context,
        out,
        scratch,
    )
}

struct PreparedAffineEval<'a> {
    program: &'a PreparedLinearOps,
    scalar_count: usize,
    extents: &'a [usize],
    ordinal_strides: &'a [usize],
    output_start: usize,
    output_strides: &'a [i128],
    load_adjustments: &'a [PreparedAffineLoadAdjustment],
    const_adjustments: &'a [PreparedAffineConstAdjustment],
    span: rumoca_core::Span,
}

fn eval_prepared_affine(
    affine: PreparedAffineEval<'_>,
    y: &[f64],
    p: &[f64],
    t: f64,
    context: RowEvalContext<'_>,
    out: &mut [f64],
    scratch: &mut RowEvalScratch,
) -> Result<(), EvalSolveError> {
    let mut ops = std::mem::take(&mut scratch.affine_ops);
    ops.clear();
    ops.extend_from_slice(&affine.program.ops);
    let result = (|| {
        for ordinal in 0..affine.scalar_count {
            apply_prepared_affine_adjustments(&affine, ordinal, &mut ops)?;
            let output_index = prepared_affine_output_index(&affine, ordinal)?;
            let output_indices = [output_index];
            let mut sink = OutputCursor::with_output_indices(out, &output_indices);
            eval_row_prepared_maybe_fast(
                PreparedRowEval::new(&ops, affine.program.register_count, y, p, t, context)
                    .with_source_span(Some(affine.span)),
                affine.program.register_safe,
                scratch,
                &mut sink,
            )?;
        }
        Ok(())
    })();
    scratch.affine_ops = ops;
    result
}

fn apply_prepared_affine_adjustments(
    affine: &PreparedAffineEval<'_>,
    ordinal: usize,
    ops: &mut [LinearOp],
) -> Result<(), EvalSolveError> {
    for adjustment in affine.load_adjustments {
        let base_op = affine
            .program
            .ops
            .get(adjustment.op_position)
            .ok_or_else(|| {
                prepared_affine_op_error(
                    "load",
                    adjustment.op_position,
                    affine.program.ops.len(),
                    affine.span,
                )
            })?;
        let op_count = ops.len();
        let adjusted_op = ops.get_mut(adjustment.op_position).ok_or_else(|| {
            prepared_affine_op_error("load", adjustment.op_position, op_count, affine.span)
        })?;
        let base_index = match *base_op {
            LinearOp::LoadY { index, .. }
            | LinearOp::LoadP { index, .. }
            | LinearOp::LoadSeed { index, .. } => index,
            _ => {
                return Err(prepared_affine_arithmetic_error(
                    "load adjustment targets an invalid operation",
                    affine.span,
                ));
            }
        };
        let index =
            prepared_affine_index(base_index, &adjustment.strides, affine, ordinal, "load")?;
        match adjusted_op {
            LinearOp::LoadY {
                index: adjusted, ..
            }
            | LinearOp::LoadP {
                index: adjusted, ..
            }
            | LinearOp::LoadSeed {
                index: adjusted, ..
            } => *adjusted = index,
            _ => {
                return Err(prepared_affine_arithmetic_error(
                    "load adjustment targets an invalid scratch operation",
                    affine.span,
                ));
            }
        }
    }
    apply_prepared_affine_const_adjustments(affine, ordinal, ops)
}

fn apply_prepared_affine_const_adjustments(
    affine: &PreparedAffineEval<'_>,
    ordinal: usize,
    ops: &mut [LinearOp],
) -> Result<(), EvalSolveError> {
    for adjustment in affine.const_adjustments {
        let base_value = match affine.program.ops.get(adjustment.op_position) {
            Some(LinearOp::Const { value, .. }) => *value,
            _ => {
                return Err(prepared_affine_arithmetic_error(
                    "constant adjustment targets an invalid operation",
                    affine.span,
                ));
            }
        };
        let mut value = base_value;
        for (dimension, stride) in adjustment.strides.iter().copied().enumerate() {
            value += prepared_affine_position(affine, ordinal, dimension)? as f64 * stride;
        }
        if !value.is_finite() && base_value.is_finite() {
            return Err(prepared_affine_arithmetic_error(
                "constant adjustment produced a non-finite value",
                affine.span,
            ));
        }
        match ops.get_mut(adjustment.op_position) {
            Some(LinearOp::Const {
                value: adjusted, ..
            }) => *adjusted = value,
            _ => {
                return Err(prepared_affine_arithmetic_error(
                    "constant adjustment targets an invalid scratch operation",
                    affine.span,
                ));
            }
        }
    }
    Ok(())
}

fn prepared_affine_output_index(
    affine: &PreparedAffineEval<'_>,
    ordinal: usize,
) -> Result<usize, EvalSolveError> {
    prepared_affine_index(
        affine.output_start,
        affine.output_strides,
        affine,
        ordinal,
        "output",
    )
}

fn prepared_affine_index(
    base_index: usize,
    strides: &[i128],
    affine: &PreparedAffineEval<'_>,
    ordinal: usize,
    kind: &'static str,
) -> Result<usize, EvalSolveError> {
    let mut value = i128::try_from(base_index).map_err(|_| {
        prepared_affine_arithmetic_error("base index exceeds arithmetic range", affine.span)
    })?;
    for (dimension, stride) in strides.iter().copied().enumerate() {
        let position = prepared_affine_position(affine, ordinal, dimension)?;
        let offset = i128::try_from(position)
            .ok()
            .and_then(|position| position.checked_mul(stride))
            .ok_or_else(|| {
                prepared_affine_arithmetic_error("index stride arithmetic overflows", affine.span)
            })?;
        value = value.checked_add(offset).ok_or_else(|| {
            prepared_affine_arithmetic_error("index accumulation overflows", affine.span)
        })?;
    }
    if value < 0 {
        return Err(EvalSolveError::Scalarization {
            message: format!("prepared affine {kind} produced negative index {value}"),
            span: Some(affine.span),
        });
    }
    usize::try_from(value)
        .map_err(|_| prepared_affine_arithmetic_error("index exceeds host range", affine.span))
}

fn prepared_affine_position(
    affine: &PreparedAffineEval<'_>,
    ordinal: usize,
    dimension: usize,
) -> Result<usize, EvalSolveError> {
    let extent = affine.extents.get(dimension).copied().ok_or_else(|| {
        prepared_affine_dimension_error("position", dimension, affine.extents.len(), affine.span)
    })?;
    let stride = affine
        .ordinal_strides
        .get(dimension)
        .copied()
        .ok_or_else(|| {
            prepared_affine_dimension_error(
                "ordinal",
                dimension,
                affine.ordinal_strides.len(),
                affine.span,
            )
        })?;
    Ok((ordinal / stride) % extent)
}

pub(super) fn prepared_domain_scalar_count(
    domain: &rumoca_core::StructuredIndexDomain,
    span: rumoca_core::Span,
) -> Result<usize, EvalSolveError> {
    domain
        .scalar_count()
        .map_err(|err| EvalSolveError::ShapeContract {
            message: format!("prepared affine structured index domain is invalid: {err}"),
            span: Some(span),
        })
}
