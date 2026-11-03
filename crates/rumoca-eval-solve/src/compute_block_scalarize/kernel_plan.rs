//! The point plan of one affine tensor node (SPEC_0032 §4).
//!
//! A `Map` or `AffineStencil` node is a base program plus affine metadata over
//! a compact domain. [`AffineKernelPlan`] is the single owner of the relation
//! between a domain point and that point's scalar row: the scalar view
//! evaluates it point by point, and a native backend evaluates it inside one
//! loop nest. Construction proves every bound once over the whole domain, so
//! neither consumer re-checks a point.

use rumoca_ir_solve::{
    AffineStencilConstStride, AffineStencilLoadStride, LinearOp, TensorOutputMap,
};

use super::ScalarizeError;
use super::affine::validate_affine_stride_metadata;

/// One strided load of the base program: its index at a point is
/// `base_index + sum_d strides[d] * k_d`, with `k_d` the point's ordinal along
/// binder `d`.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct AffineKernelLoad {
    op_position: usize,
    base_index: usize,
    strides: Vec<i64>,
    max_index: usize,
}

impl AffineKernelLoad {
    #[must_use]
    pub const fn op_position(&self) -> usize {
        self.op_position
    }

    #[must_use]
    pub const fn base_index(&self) -> usize {
        self.base_index
    }

    /// Combined index stride per binder ordinal, in binder order.
    #[must_use]
    pub fn strides(&self) -> &[i64] {
        &self.strides
    }

    /// The largest index this load reads at any domain point.
    #[must_use]
    pub const fn max_index(&self) -> usize {
        self.max_index
    }

    /// The load index at `ordinals`, which must lie inside the plan's domain.
    #[must_use]
    pub fn index_at(&self, ordinals: &[usize]) -> usize {
        let mut value = self.base_index as i64;
        for (stride, ordinal) in self.strides.iter().zip(ordinals) {
            value += stride * *ordinal as i64;
        }
        value as usize
    }
}

/// One strided constant of the base program. Its value at a point is
/// `base_value`, then `value += (k_d as f64) * strides[d]` for every binder in
/// order: these exact IEEE operations, in this order, define the value.
#[derive(Clone, Debug, PartialEq)]
pub struct AffineKernelConst {
    op_position: usize,
    base_value: f64,
    strides: Vec<f64>,
}

impl AffineKernelConst {
    #[must_use]
    pub const fn op_position(&self) -> usize {
        self.op_position
    }

    #[must_use]
    pub const fn base_value(&self) -> f64 {
        self.base_value
    }

    /// Combined constant stride per binder ordinal, in binder order.
    #[must_use]
    pub fn strides(&self) -> &[f64] {
        &self.strides
    }

    /// The constant at `ordinals`, which must lie inside the plan's domain.
    #[must_use]
    pub fn value_at(&self, ordinals: &[usize]) -> f64 {
        let mut value = self.base_value;
        for (stride, ordinal) in self.strides.iter().zip(ordinals) {
            value += *ordinal as f64 * stride;
        }
        value
    }
}

/// The checked point plan of one affine tensor node.
#[derive(Clone, Debug, PartialEq)]
pub struct AffineKernelPlan {
    extents: Vec<usize>,
    point_count: usize,
    loads: Vec<AffineKernelLoad>,
    consts: Vec<AffineKernelConst>,
    output_start: usize,
    output_strides: Vec<i64>,
    output_count: usize,
}

/// The node fields a plan is constructed from.
#[derive(Clone, Copy)]
pub struct AffineKernelNode<'a> {
    pub domain: &'a rumoca_core::StructuredIndexDomain,
    pub output_map: Option<&'a TensorOutputMap>,
    pub base_ops: &'a [LinearOp],
    pub load_strides: &'a [AffineStencilLoadStride],
    pub const_strides: &'a [AffineStencilConstStride],
    pub kind: &'static str,
    pub span: rumoca_core::Span,
}

impl AffineKernelPlan {
    /// Check the node once and derive its point plan.
    ///
    /// Every load index is affine in the binder ordinals and every partial sum
    /// of a strided constant is monotone in each ordinal (IEEE rounding is
    /// monotone), so the extremes over the domain box are attained at its
    /// corners: checking the two extreme corners proves every point.
    pub fn new(node: AffineKernelNode<'_>) -> Result<Self, ScalarizeError> {
        let AffineKernelNode {
            domain,
            output_map,
            base_ops,
            load_strides,
            const_strides,
            kind,
            span,
        } = node;
        validate_affine_stride_metadata(domain, base_ops, load_strides, const_strides, kind, span)?;
        let extents = domain
            .extents()
            .map_err(|err| ScalarizeError::ShapeContract {
                message: format!("structured index domain is invalid: {err}"),
                span: Some(span),
            })?;
        let point_count = domain
            .scalar_count()
            .map_err(|err| ScalarizeError::ShapeContract {
                message: format!("structured index domain is invalid: {err}"),
                span: Some(span),
            })?;
        let rank = extents.len();
        let loads = combined_loads(base_ops, load_strides, rank, kind, span)?
            .into_iter()
            .map(|(op_position, base_index, strides)| {
                checked_load(
                    op_position,
                    base_index,
                    strides,
                    &extents,
                    point_count,
                    kind,
                    span,
                )
            })
            .collect::<Result<Vec<_>, _>>()?;
        let consts = combined_consts(base_ops, const_strides, rank, kind, span)?;
        if point_count > 0 {
            for constant in &consts {
                check_const_corners(constant, &extents, kind, span)?;
            }
        }
        let (output_start, output_strides, output_count) = match output_map {
            Some(map) => checked_output(map, domain, rank, kind, span)?,
            None => (0, vec![0; rank], 0),
        };
        Ok(Self {
            extents,
            point_count,
            loads,
            consts,
            output_start,
            output_strides,
            output_count,
        })
    }

    /// Ordinal extent of each binder, in binder order.
    #[must_use]
    pub fn extents(&self) -> &[usize] {
        &self.extents
    }

    #[must_use]
    pub const fn point_count(&self) -> usize {
        self.point_count
    }

    #[must_use]
    pub fn loads(&self) -> &[AffineKernelLoad] {
        &self.loads
    }

    #[must_use]
    pub fn consts(&self) -> &[AffineKernelConst] {
        &self.consts
    }

    #[must_use]
    pub const fn output_start(&self) -> usize {
        self.output_start
    }

    /// Output index stride per binder ordinal, in binder order.
    #[must_use]
    pub fn output_strides(&self) -> &[i64] {
        &self.output_strides
    }

    /// One past the largest output index the node writes (0 when empty).
    #[must_use]
    pub const fn output_count(&self) -> usize {
        self.output_count
    }

    /// The output index at `ordinals`, which must lie inside the domain.
    #[must_use]
    pub fn output_index_at(&self, ordinals: &[usize]) -> usize {
        let mut value = self.output_start as i64;
        for (stride, ordinal) in self.output_strides.iter().zip(ordinals) {
            value += stride * *ordinal as i64;
        }
        value as usize
    }

    /// The scalar row of the point at `ordinals`: the base program with every
    /// strided load and constant evaluated there.
    #[must_use]
    pub fn row_at(&self, base_ops: &[LinearOp], ordinals: &[usize]) -> Vec<LinearOp> {
        let mut ops = base_ops.to_vec();
        self.patch(
            &mut ops,
            |load| load.index_at(ordinals),
            |constant| constant.value_at(ordinals),
        );
        ops
    }

    /// The base program with every strided load reading its largest index:
    /// its input requirements bound every point's.
    #[must_use]
    pub fn max_index_row(&self, base_ops: &[LinearOp]) -> Vec<LinearOp> {
        let mut ops = base_ops.to_vec();
        self.patch(
            &mut ops,
            AffineKernelLoad::max_index,
            AffineKernelConst::base_value,
        );
        ops
    }

    /// Row-major ordinals of every point, innermost binder fastest.
    pub fn for_each_point(&self, mut visit: impl FnMut(&[usize])) {
        if self.point_count == 0 {
            return;
        }
        let mut ordinals = vec![0usize; self.extents.len()];
        loop {
            visit(&ordinals);
            if !advance_row_major(&mut ordinals, &self.extents) {
                return;
            }
        }
    }

    fn patch(
        &self,
        ops: &mut [LinearOp],
        load_index: impl Fn(&AffineKernelLoad) -> usize,
        const_value: impl Fn(&AffineKernelConst) -> f64,
    ) {
        for load in &self.loads {
            match &mut ops[load.op_position] {
                LinearOp::LoadY { index, .. }
                | LinearOp::LoadP { index, .. }
                | LinearOp::LoadSeed { index, .. } => *index = load_index(load),
                _ => unreachable!("plan construction checked the strided load kind"),
            }
        }
        for constant in &self.consts {
            match &mut ops[constant.op_position] {
                LinearOp::Const { value, .. } => *value = const_value(constant),
                _ => unreachable!("plan construction checked the strided constant kind"),
            }
        }
    }
}

/// Step `ordinals` to the next row-major point; `false` after the last one.
fn advance_row_major(ordinals: &mut [usize], extents: &[usize]) -> bool {
    for dimension in (0..ordinals.len()).rev() {
        ordinals[dimension] += 1;
        if ordinals[dimension] < extents[dimension] {
            return true;
        }
        ordinals[dimension] = 0;
    }
    false
}

type CombinedLoad = (usize, usize, Vec<i128>);

fn combined_loads(
    base_ops: &[LinearOp],
    strides: &[AffineStencilLoadStride],
    rank: usize,
    kind: &'static str,
    span: rumoca_core::Span,
) -> Result<Vec<CombinedLoad>, ScalarizeError> {
    let mut by_op = vec![None::<Vec<i128>>; base_ops.len()];
    for stride in strides {
        let dimension_strides = by_op[stride.op_position].get_or_insert_with(|| vec![0; rank]);
        for term in &stride.terms {
            let slot = &mut dimension_strides[term.dimension];
            *slot = slot.checked_add(term.stride as i128).ok_or_else(|| {
                ScalarizeError::ShapeContract {
                    message: format!("native {kind} family load stride accumulation overflowed"),
                    span: Some(span),
                }
            })?;
        }
    }
    let mut loads = Vec::new();
    for (op_position, dimension_strides) in by_op.into_iter().enumerate() {
        let Some(dimension_strides) = dimension_strides else {
            continue;
        };
        let base_index = match &base_ops[op_position] {
            LinearOp::LoadY { index, .. }
            | LinearOp::LoadP { index, .. }
            | LinearOp::LoadSeed { index, .. } => *index,
            _ => {
                // Metadata validation already proved the strided op kind.
                return Err(ScalarizeError::ShapeContract {
                    message: format!(
                        "native {kind} family load stride at op {op_position} targets no load"
                    ),
                    span: Some(span),
                });
            }
        };
        loads.push((op_position, base_index, dimension_strides));
    }
    Ok(loads)
}

fn checked_load(
    op_position: usize,
    base_index: usize,
    strides: Vec<i128>,
    extents: &[usize],
    point_count: usize,
    kind: &'static str,
    span: rumoca_core::Span,
) -> Result<AffineKernelLoad, ScalarizeError> {
    let overflow = || ScalarizeError::ShapeContract {
        message: format!("native {kind} family load index accumulation overflowed"),
        span: Some(span),
    };
    let base = i128::try_from(base_index).map_err(|_| overflow())?;
    let (mut minimum, mut maximum) = (base, base);
    if point_count > 0 {
        for (stride, extent) in strides.iter().zip(extents) {
            let last = i128::try_from(*extent - 1).map_err(|_| overflow())?;
            let offset = stride.checked_mul(last).ok_or_else(overflow)?;
            if offset < 0 {
                minimum = minimum.checked_add(offset).ok_or_else(overflow)?;
            } else {
                maximum = maximum.checked_add(offset).ok_or_else(overflow)?;
            }
        }
    }
    if minimum < 0 {
        return Err(ScalarizeError::NegativeLoadIndex {
            kind,
            value: minimum,
            span,
        });
    }
    let max_index = i64::try_from(maximum)
        .ok()
        .and_then(|value| usize::try_from(value).ok())
        .ok_or_else(|| ScalarizeError::ShapeContract {
            message: format!("native {kind} family load index exceeds host range"),
            span: Some(span),
        })?;
    let strides = strides
        .into_iter()
        .map(|stride| i64::try_from(stride).map_err(|_| overflow()))
        .collect::<Result<Vec<_>, _>>()?;
    Ok(AffineKernelLoad {
        op_position,
        base_index,
        strides,
        max_index,
    })
}

fn combined_consts(
    base_ops: &[LinearOp],
    strides: &[AffineStencilConstStride],
    rank: usize,
    kind: &'static str,
    span: rumoca_core::Span,
) -> Result<Vec<AffineKernelConst>, ScalarizeError> {
    let mut by_op = vec![None::<Vec<f64>>; base_ops.len()];
    for stride in strides {
        let dimension_strides = by_op[stride.op_position].get_or_insert_with(|| vec![0.0; rank]);
        for term in &stride.terms {
            let slot = &mut dimension_strides[term.dimension];
            *slot += term.stride;
            if !slot.is_finite() {
                return Err(ScalarizeError::ShapeContract {
                    message: format!(
                        "native {kind} family constant stride accumulation is non-finite"
                    ),
                    span: Some(span),
                });
            }
        }
    }
    let mut consts = Vec::new();
    for (op_position, dimension_strides) in by_op.into_iter().enumerate() {
        let Some(strides) = dimension_strides else {
            continue;
        };
        let LinearOp::Const { value, .. } = &base_ops[op_position] else {
            // Metadata validation already proved the strided op kind.
            return Err(ScalarizeError::ShapeContract {
                message: format!(
                    "native {kind} family constant stride at op {op_position} targets no Const"
                ),
                span: Some(span),
            });
        };
        consts.push(AffineKernelConst {
            op_position,
            base_value: *value,
            strides,
        });
    }
    Ok(consts)
}

/// Every partial sum of a strided constant is finite at every point when it
/// is finite at the two corners that extremize each term.
fn check_const_corners(
    constant: &AffineKernelConst,
    extents: &[usize],
    kind: &'static str,
    span: rumoca_core::Span,
) -> Result<(), ScalarizeError> {
    if !constant.base_value.is_finite() {
        return Ok(());
    }
    for maximize in [false, true] {
        let mut value = constant.base_value;
        for (stride, extent) in constant.strides.iter().zip(extents) {
            let last = *extent - 1;
            let ordinal = if (*stride > 0.0) == maximize { last } else { 0 };
            value += ordinal as f64 * stride;
            if !value.is_finite() {
                return Err(ScalarizeError::ShapeContract {
                    message: format!("native {kind} family constant adjustment is non-finite"),
                    span: Some(span),
                });
            }
        }
    }
    Ok(())
}

fn checked_output(
    map: &TensorOutputMap,
    domain: &rumoca_core::StructuredIndexDomain,
    rank: usize,
    kind: &'static str,
    span: rumoca_core::Span,
) -> Result<(usize, Vec<i64>, usize), ScalarizeError> {
    let output_count = map
        .output_count(domain)
        .map_err(|err| ScalarizeError::ShapeContract {
            message: format!("native {kind} family output map is invalid: {err:?}"),
            span: Some(span),
        })?;
    let overflow = || ScalarizeError::ShapeContract {
        message: format!("native {kind} family output stride exceeds host range"),
        span: Some(span),
    };
    let mut strides = vec![0i64; rank];
    for term in &map.strides {
        let slot =
            strides
                .get_mut(term.dimension)
                .ok_or(ScalarizeError::InvalidStrideDimension {
                    kind,
                    dimension: term.dimension,
                    dimension_count: rank,
                    span,
                })?;
        *slot = slot
            .checked_add(i64::try_from(term.stride).map_err(|_| overflow())?)
            .ok_or_else(overflow)?;
    }
    i64::try_from(output_count).map_err(|_| overflow())?;
    Ok((map.start, strides, output_count))
}

#[cfg(test)]
mod tests;
