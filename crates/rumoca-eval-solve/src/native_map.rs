//! Native, allocation-bounded evaluation of structured Solve-IR Maps.

use super::*;

/// Execute a compact Solve-IR `Map` directly over its structured domain.
///
/// This is intentionally distinct from scalarization: it keeps the map's
/// single `base_ops` owner and applies affine load/constant offsets while each
/// element is evaluated.  The callback sees a borrowed ordinal that is reused
/// for every element, so this path never creates a scalar-row vector or a
/// per-cell `LinearOp` clone.
pub fn eval_map_elements_with_context(
    node: &ComputeNode,
    y: &mut [f64],
    p: &[f64],
    t: f64,
    context: RowEvalContext<'_>,
    mut visit: impl FnMut(&[usize], f64, &mut [f64]) -> Result<(), EvalSolveError>,
) -> Result<MapEvaluationMetrics, EvalSolveError> {
    let ComputeNode::Map {
        domain,
        base_ops,
        load_strides,
        const_strides,
        span,
        ..
    } = node
    else {
        return Err(EvalSolveError::InvalidRow {
            message: "native map evaluation requires a ComputeNode::Map".to_string(),
            span: None,
        });
    };
    let local_runtime_state;
    let context = match context.runtime_state {
        Some(_) => context,
        None => {
            local_runtime_state = SimulationRuntimeState::new();
            context.with_runtime_state(&local_runtime_state)
        }
    };
    validate_affine_map_metadata(domain, base_ops, load_strides, const_strides, *span)?;
    let counts = map_domain_counts(domain, *span)?;
    if counts.contains(&0) {
        return Ok(MapEvaluationMetrics::default());
    }
    let register_count = required_registers(base_ops)?.max(1);
    let mut scratch = RowEvalScratch::default();
    let mut ordinal = vec![0usize; counts.len()];
    let mut metrics = MapEvaluationMetrics {
        temporary_values: counts
            .len()
            .saturating_mul(2)
            .saturating_add(register_count),
        ..Default::default()
    };
    loop {
        let mut output = [0.0f64];
        let mut sink = OutputCursor::new(&mut output);
        let input = PreparedRowEval::new(base_ops, register_count, y, p, t, context)
            .with_source_span(Some(*span));
        eval_affine_map_row(
            input,
            &mut scratch,
            &mut sink,
            AffineMapOffsets {
                ordinal: &ordinal,
                load_strides,
                const_strides,
                span: *span,
            },
        )?;
        visit(&ordinal, output[0], y)?;
        metrics.elements = metrics.elements.saturating_add(1);
        if increment_map_ordinal(&mut ordinal, &counts) {
            break;
        }
    }
    Ok(metrics)
}

/// Deterministic resource counters for native map evaluation.  They are used
/// by compact-runtime callers to assert linear traversal rather than relying
/// on host timing.
#[derive(Clone, Copy, Debug, Default, Eq, PartialEq)]
pub struct MapEvaluationMetrics {
    pub elements: usize,
    pub temporary_values: usize,
}

pub(super) fn affine_load_offset(
    position: usize,
    offsets: AffineMapOffsets<'_>,
) -> Result<isize, EvalSolveError> {
    offsets
        .load_strides
        .iter()
        .filter(|stride| stride.op_position == position)
        .try_fold(0isize, |total, terms| {
            checked_affine_index_offset(total, &terms.terms, offsets.ordinal, offsets.span)
        })
}

pub(super) fn affine_const_offset(
    position: usize,
    offsets: AffineMapOffsets<'_>,
) -> Result<f64, EvalSolveError> {
    offsets
        .const_strides
        .iter()
        .filter(|stride| stride.op_position == position)
        .try_fold(0.0f64, |total, terms| {
            checked_affine_const_offset(total, &terms.terms, offsets.ordinal, offsets.span)
        })
}

#[derive(Clone, Copy)]
pub(super) struct AffineMapOffsets<'a> {
    pub(super) ordinal: &'a [usize],
    pub(super) load_strides: &'a [rumoca_ir_solve::AffineStencilLoadStride],
    pub(super) const_strides: &'a [rumoca_ir_solve::AffineStencilConstStride],
    pub(super) span: rumoca_core::Span,
}

fn eval_affine_map_row(
    input: PreparedRowEval<'_, '_>,
    scratch: &mut RowEvalScratch,
    sink: &mut OutputCursor<'_>,
    offsets: AffineMapOffsets<'_>,
) -> Result<(), EvalSolveError> {
    scratch.regs.resize(input.register_count, 0.0);
    scratch.initialized.resize(input.register_count, false);
    scratch.regs.fill(0.0);
    scratch.initialized.fill(false);
    let mut evaluator = CheckedRowEvaluator {
        regs: &mut scratch.regs,
        initialized: &mut scratch.initialized,
        input,
        sink,
    };
    for (position, op) in evaluator.input.row.iter().copied().enumerate() {
        evaluator.eval_affine_op(position, op, offsets)?;
    }
    Ok(())
}

fn checked_affine_index_offset(
    total: isize,
    terms: &[rumoca_ir_solve::AffineStencilIndexStrideTerm],
    ordinal: &[usize],
    span: rumoca_core::Span,
) -> Result<isize, EvalSolveError> {
    terms.iter().try_fold(total, |sum, term| {
        let coordinate = *ordinal.get(term.dimension).ok_or_else(|| {
            affine_map_error(
                "affine load stride references a missing domain dimension",
                span,
            )
        })?;
        let coordinate = isize::try_from(coordinate)
            .map_err(|_| affine_map_error("affine load ordinal overflows isize", span))?;
        let offset = term
            .stride
            .checked_mul(coordinate)
            .ok_or_else(|| affine_map_error("affine load stride overflows isize", span))?;
        sum.checked_add(offset)
            .ok_or_else(|| affine_map_error("affine load offset overflows isize", span))
    })
}

fn checked_affine_const_offset(
    total: f64,
    terms: &[rumoca_ir_solve::AffineStencilConstStrideTerm],
    ordinal: &[usize],
    span: rumoca_core::Span,
) -> Result<f64, EvalSolveError> {
    terms.iter().try_fold(total, |sum, term| {
        let coordinate = *ordinal.get(term.dimension).ok_or_else(|| {
            affine_map_error(
                "affine constant stride references a missing domain dimension",
                span,
            )
        })?;
        let offset = term.stride * coordinate as f64;
        if !offset.is_finite() {
            return Err(affine_map_error(
                "affine constant offset is non-finite",
                span,
            ));
        }
        let next = sum + offset;
        next.is_finite()
            .then_some(next)
            .ok_or_else(|| affine_map_error("affine constant offset is non-finite", span))
    })
}

fn validate_affine_map_metadata(
    domain: &rumoca_core::StructuredIndexDomain,
    base_ops: &[LinearOp],
    load_strides: &[rumoca_ir_solve::AffineStencilLoadStride],
    const_strides: &[rumoca_ir_solve::AffineStencilConstStride],
    span: rumoca_core::Span,
) -> Result<(), EvalSolveError> {
    for stride in load_strides {
        validate_affine_dimensions(&stride.terms, domain, span)?;
        if !matches!(
            base_ops.get(stride.op_position),
            Some(LinearOp::LoadY { .. } | LinearOp::LoadP { .. })
        ) {
            return Err(affine_map_error(
                "affine load stride does not point at LoadY or LoadP",
                span,
            ));
        }
    }
    for stride in const_strides {
        validate_affine_dimensions(&stride.terms, domain, span)?;
        if !matches!(
            base_ops.get(stride.op_position),
            Some(LinearOp::Const { .. })
        ) {
            return Err(affine_map_error(
                "affine constant stride does not point at Const",
                span,
            ));
        }
    }
    Ok(())
}

fn validate_affine_dimensions<T: AffineDimension>(
    terms: &[T],
    domain: &rumoca_core::StructuredIndexDomain,
    span: rumoca_core::Span,
) -> Result<(), EvalSolveError> {
    if terms
        .iter()
        .any(|term| term.dimension() >= domain.binders.len())
    {
        return Err(affine_map_error(
            "affine stride references a missing domain dimension",
            span,
        ));
    }
    Ok(())
}

trait AffineDimension {
    fn dimension(&self) -> usize;
}

impl AffineDimension for rumoca_ir_solve::AffineStencilIndexStrideTerm {
    fn dimension(&self) -> usize {
        self.dimension
    }
}

impl AffineDimension for rumoca_ir_solve::AffineStencilConstStrideTerm {
    fn dimension(&self) -> usize {
        self.dimension
    }
}

fn map_domain_counts(
    domain: &rumoca_core::StructuredIndexDomain,
    span: rumoca_core::Span,
) -> Result<Vec<usize>, EvalSolveError> {
    domain
        .binders
        .iter()
        .map(|binder| {
            if binder.step == 0 {
                return Err(affine_map_error("structured map binder step is zero", span));
            }
            let distance = if binder.step > 0 {
                binder.upper.checked_sub(binder.lower)
            } else {
                binder.lower.checked_sub(binder.upper)
            }
            .ok_or_else(|| {
                affine_map_error("structured map binder bounds contradict step", span)
            })?;
            let step = i64::try_from(binder.step.unsigned_abs())
                .map_err(|_| affine_map_error("structured map binder step overflows i64", span))?;
            let count = distance
                .checked_div(step)
                .and_then(|count| count.checked_add(1))
                .ok_or_else(|| affine_map_error("structured map binder count overflows", span))?;
            usize::try_from(count).map_err(|_| {
                affine_map_error("structured map binder count exceeds host range", span)
            })
        })
        .collect()
}

fn increment_map_ordinal(ordinal: &mut [usize], counts: &[usize]) -> bool {
    for dimension in (0..ordinal.len()).rev() {
        ordinal[dimension] += 1;
        if ordinal[dimension] < counts[dimension] {
            return false;
        }
        ordinal[dimension] = 0;
    }
    true
}

pub(super) fn affine_map_error(
    message: impl Into<String>,
    span: rumoca_core::Span,
) -> EvalSolveError {
    EvalSolveError::InvalidRow {
        message: message.into(),
        span: Some(span),
    }
}
