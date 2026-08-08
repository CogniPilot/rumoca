//! C and MLIR rendering for solver-facing row IR.
//!
//! SPEC_0021 file-size exception: solve rendering still shares scalar, tensor,
//! and runtime helper objects. split plan: move C, MLIR, and tensor object
//! renderers into separate modules behind this facade.

use std::sync::Arc;

use crate::errors::render_err;
use minijinja::Value;
use minijinja::value::{Enumerator, Object, ObjectRepr};
use rumoca_ir_solve as solve;

use super::render_expr::get_field;
use super::render_solve_ops::{
    render_solve_binary_wgsl, render_solve_compare_wgsl, render_solve_unary_wgsl,
};
use super::{RenderResult, render_vec_with_capacity, reserve_render_capacity, value_to_string};

mod dense_solve_render;
mod mlir_family;
#[cfg(test)]
#[path = "render_solve_tests.rs"]
mod render_solve_tests;
mod template_partition;
#[cfg(test)]
pub(super) use dense_solve_render::{LinSolveRenderShape, MatMulRenderShape};
pub(super) use dense_solve_render::{
    mlir_native_dense_node_supported, render_linsolve_mlir_function, render_matmul_mlir_function,
    render_solve_row_output_wgsl_function, render_solve_row_wgsl_function, required_bool_field,
    required_string_field, required_usize_field,
};
pub(super) use mlir_family::render_solve_native_family_mlir_function;
pub(super) use template_partition::{
    RenderNativeAffineFamily, SolveNativeDenseNodesValue, SolveNativeFamiliesValue, SolveRowsValue,
    SolveScalarFallbackRowsValue, native_family_template_partition,
    render_solve_native_family_output_index_wgsl_function,
    render_solve_native_family_output_map_start_function, render_solve_native_family_wgsl_function,
    render_wgsl_kernel_schedule_json_function, render_wgsl_kernel_workgroup_total_function,
    render_wgsl_native_family_inventory_json_function,
};
#[cfg(test)]
pub(super) use template_partition::{
    kernel_workgroup_count, scalar_kernel_chunk_count, scalar_program_block_source_span,
    wgsl_kernel_schedule_entry_count, wgsl_kernel_workgroup_total,
};

// ─── Typed scalar-program rows ───────────────────────────────────────────────
//
// Templates receive scalar-program rows as these objects instead of
// serde-bridged values: per-row value bridging dominated render time on
// large models (each access re-serialized op subtrees). The row renderers
// downcast and walk the typed ops directly; plain-value rows (tests,
// custom contexts) keep the original walk.

#[derive(Debug)]

pub(super) struct SolveRowValue {
    rows: Arc<Vec<Vec<solve::LinearOp>>>,
    index: usize,
}

impl SolveRowValue {
    pub(in crate::codegen) fn new(rows: Arc<Vec<Vec<solve::LinearOp>>>, index: usize) -> Self {
        Self { rows, index }
    }

    pub(in crate::codegen) fn ops(&self) -> &[solve::LinearOp] {
        &self.rows[self.index]
    }
}

impl Object for SolveRowValue {
    fn repr(self: &Arc<Self>) -> ObjectRepr {
        ObjectRepr::Seq
    }

    fn get_value(self: &Arc<Self>, key: &Value) -> Option<Value> {
        let index = key.as_usize()?;
        self.ops().get(index).map(Value::from_serialize)
    }

    fn enumerate(self: &Arc<Self>) -> Enumerator {
        Enumerator::Seq(self.ops().len())
    }
}

fn render_solve_row_for(
    row: &Value,
    cfg: &SolveRowCConfig,
    dialect: SolveRowDialect,
) -> RenderResult {
    if let Some(typed) = row.downcast_object_ref::<SolveRowValue>() {
        return render_solve_row_typed(typed.ops(), cfg, dialect);
    }
    let mut regs = Vec::<String>::new();
    let mut output = None;
    let iter = row
        .try_iter()
        .map_err(|_| render_err("solve row must be an array of LinearOp values"))?;
    for op in iter {
        output = render_solve_op_for(&op, cfg, dialect, &mut regs, output)?;
    }
    output.ok_or_else(|| render_err("solve row did not contain StoreOutput"))
}

fn render_solve_row_output_for(
    row: &Value,
    output_ordinal: usize,
    cfg: &SolveRowCConfig,
    dialect: SolveRowDialect,
) -> RenderResult {
    if let Some(typed) = row.downcast_object_ref::<SolveRowValue>() {
        return render_solve_row_typed_output(typed.ops(), output_ordinal, cfg, dialect);
    }
    let mut regs = Vec::<String>::new();
    let mut seen_outputs = 0usize;
    let iter = row
        .try_iter()
        .map_err(|_| render_err("solve row must be an array of LinearOp values"))?;
    for op in iter {
        let output = render_solve_op_for(&op, cfg, dialect, &mut regs, None)?;
        if get_field(&op, "StoreOutput").is_ok() {
            if seen_outputs == output_ordinal {
                return output.ok_or_else(|| render_err("solve StoreOutput did not yield a value"));
            }
            seen_outputs = next_store_output_ordinal(seen_outputs)?;
        }
    }
    missing_store_output_ordinal(output_ordinal)
}

/// Render an affine row family as one WGSL expression parametric in the
/// compact-domain row offset `r` (a `u32` in the emitting kernel).
fn render_native_family_expr_wgsl(
    family: &RenderNativeAffineFamily,
    config: &Value,
) -> RenderResult {
    validate_native_family_metadata(family)?;
    let cfg = SolveRowCConfig::from_value(config);
    let mut overrides = std::collections::HashMap::new();
    for (op_position, op) in family.base_ops.iter().enumerate() {
        let terms = family_load_terms(family, op_position)?;
        if terms.is_empty() {
            continue;
        }
        let (source, base_index) = match op {
            solve::LinearOp::LoadY { index, .. } => (NativeFamilyLoadSource::Y, *index),
            solve::LinearOp::LoadP { index, .. } => (NativeFamilyLoadSource::P, *index),
            solve::LinearOp::LoadSeed { index, .. } => (NativeFamilyLoadSource::Seed, *index),
            other => {
                return Err(render_err(format!(
                    "native family stride targets a non-load op: {}",
                    other.kind_name()
                )));
            }
        };
        let index_expr = affine_load_index_expr(base_index, &terms, &family.domain)?;
        let access = match source {
            NativeFamilyLoadSource::Y => cfg.y_access_expr(&index_expr),
            NativeFamilyLoadSource::P => cfg.p_access_expr(&index_expr),
            NativeFamilyLoadSource::Seed => cfg.seed_access_expr(&index_expr).ok_or_else(|| {
                render_err("native family LoadSeed requires a `seed` access pattern")
            })?,
        };
        overrides.insert(op_position, access);
    }
    for (op_position, op) in family.base_ops.iter().enumerate() {
        let terms = family_const_terms(family, op_position)?;
        if terms.is_empty() {
            continue;
        }
        let base_value = match op {
            solve::LinearOp::Const { value, .. } => *value,
            other => {
                return Err(render_err(format!(
                    "native family const stride targets a non-const op: {}",
                    other.kind_name()
                )));
            }
        };
        let value_expr = affine_const_expr(base_value, &terms, &family.domain)?;
        overrides.insert(op_position, value_expr);
    }
    render_solve_row_typed_with_overrides(&family.base_ops, &overrides, &cfg, SolveRowDialect::Wgsl)
}

enum NativeFamilyLoadSource {
    Y,
    P,
    Seed,
}

fn validate_native_family_metadata(
    family: &RenderNativeAffineFamily,
) -> Result<(), minijinja::Error> {
    let rank = family.domain.binders.len();
    let scalar_count = family
        .domain
        .scalar_count()
        .map_err(|error| render_err(format!("invalid native-family domain: {error}")))?;
    if family.count != scalar_count {
        return Err(render_err(format!(
            "native-family row count {} does not match domain cardinality {scalar_count}",
            family.count
        )));
    }
    family
        .output_map
        .output_count(&family.domain)
        .map_err(|error| render_err(format!("invalid native-family output map: {error:?}")))?;
    for stride in &family.load_strides {
        match family.base_ops.get(stride.op_position) {
            Some(
                solve::LinearOp::LoadY { .. }
                | solve::LinearOp::LoadP { .. }
                | solve::LinearOp::LoadSeed { .. },
            ) => {}
            Some(op) => {
                return Err(render_err(format!(
                    "native family stride targets a non-load op: {}",
                    op.kind_name()
                )));
            }
            None => {
                return Err(render_err(format!(
                    "native family load stride targets missing op {} of {}",
                    stride.op_position,
                    family.base_ops.len()
                )));
            }
        }
        validate_native_family_dimensions(
            stride.terms.iter().map(|term| term.dimension),
            rank,
            "load",
        )?;
    }
    for stride in &family.const_strides {
        match family.base_ops.get(stride.op_position) {
            Some(solve::LinearOp::Const { .. }) => {}
            Some(op) => {
                return Err(render_err(format!(
                    "native family const stride targets a non-const op: {}",
                    op.kind_name()
                )));
            }
            None => {
                return Err(render_err(format!(
                    "native family constant stride targets missing op {} of {}",
                    stride.op_position,
                    family.base_ops.len()
                )));
            }
        }
        validate_native_family_dimensions(
            stride.terms.iter().map(|term| term.dimension),
            rank,
            "constant",
        )?;
        if stride.terms.iter().any(|term| !term.stride.is_finite()) {
            return Err(render_err("native family constant stride must be finite"));
        }
    }
    Ok(())
}

fn validate_native_family_dimensions(
    dimensions: impl Iterator<Item = usize>,
    rank: usize,
    kind: &str,
) -> Result<(), minijinja::Error> {
    for dimension in dimensions {
        if dimension >= rank {
            return Err(render_err(format!(
                "native family {kind} stride dimension {dimension} exceeds domain rank {rank}"
            )));
        }
    }
    Ok(())
}

fn affine_load_index_expr(
    base_index: usize,
    terms: &[solve::AffineStencilIndexStrideTerm],
    domain: &rumoca_core::StructuredIndexDomain,
) -> Result<String, minijinja::Error> {
    let terms = combined_affine_index_terms(terms, domain)?;
    validate_wgsl_affine_index_range(base_index, &terms, domain, "load")?;
    let mut expr = format!("i32({base_index}u)");
    for term in &terms {
        let contribution = domain_coordinate_contribution(term.dimension, term.stride, domain)?;
        expr = format!("{expr} + {contribution}");
    }
    Ok(format!("u32({expr})"))
}

fn affine_output_index_expr(
    output_map: &solve::TensorOutputMap,
    domain: &rumoca_core::StructuredIndexDomain,
) -> Result<String, minijinja::Error> {
    let terms = combined_affine_index_terms(&output_map.strides, domain)?;
    validate_wgsl_affine_index_range(output_map.start, &terms, domain, "output")?;
    let mut expr = format!("i32({}u)", output_map.start);
    for term in &terms {
        let contribution = domain_coordinate_contribution(term.dimension, term.stride, domain)?;
        expr = format!("{expr} + {contribution}");
    }
    Ok(format!("u32({expr})"))
}

fn affine_const_expr(
    base_value: f64,
    terms: &[solve::AffineStencilConstStrideTerm],
    domain: &rumoca_core::StructuredIndexDomain,
) -> Result<String, minijinja::Error> {
    let terms = combined_affine_const_terms(terms, domain)?;
    let mut expr = format!("{base_value:?}");
    for term in &terms {
        let contribution = domain_coordinate_contribution_f32(term.dimension, term.stride, domain)?;
        expr = format!("{expr} + {contribution}");
    }
    Ok(expr)
}

fn family_load_terms(
    family: &RenderNativeAffineFamily,
    op_position: usize,
) -> Result<Vec<solve::AffineStencilIndexStrideTerm>, minijinja::Error> {
    let count = family
        .load_strides
        .iter()
        .filter(|stride| stride.op_position == op_position)
        .try_fold(0usize, |count, stride| {
            count.checked_add(stride.terms.len())
        })
        .ok_or_else(|| render_err("native family load stride count overflows"))?;
    let mut terms = render_vec_with_capacity(count, "native family load stride count")?;
    for stride in family
        .load_strides
        .iter()
        .filter(|stride| stride.op_position == op_position)
    {
        terms.extend(stride.terms.iter().cloned());
    }
    Ok(terms)
}

fn family_const_terms(
    family: &RenderNativeAffineFamily,
    op_position: usize,
) -> Result<Vec<solve::AffineStencilConstStrideTerm>, minijinja::Error> {
    let count = family
        .const_strides
        .iter()
        .filter(|stride| stride.op_position == op_position)
        .try_fold(0usize, |count, stride| {
            count.checked_add(stride.terms.len())
        })
        .ok_or_else(|| render_err("native family constant stride count overflows"))?;
    let mut terms = render_vec_with_capacity(count, "native family constant stride count")?;
    for stride in family
        .const_strides
        .iter()
        .filter(|stride| stride.op_position == op_position)
    {
        terms.extend(stride.terms.iter().cloned());
    }
    Ok(terms)
}

fn combined_affine_index_terms(
    terms: &[solve::AffineStencilIndexStrideTerm],
    domain: &rumoca_core::StructuredIndexDomain,
) -> Result<Vec<solve::AffineStencilIndexStrideTerm>, minijinja::Error> {
    let extents = domain
        .extents()
        .map_err(|error| render_err(format!("invalid native-family domain: {error}")))?;
    let mut strides = render_vec_with_capacity(
        domain.binders.len(),
        "native family affine dimension stride count",
    )?;
    strides.resize(domain.binders.len(), 0i128);
    for term in terms {
        let Some(stride) = strides.get_mut(term.dimension) else {
            return Err(render_err(format!(
                "affine stride dimension {} out of bounds",
                term.dimension
            )));
        };
        *stride = stride
            .checked_add(term.stride as i128)
            .ok_or_else(|| render_err("affine stride accumulation overflows"))?;
    }
    let mut combined =
        render_vec_with_capacity(strides.len(), "native family combined affine strides")?;
    for (dimension, (stride, extent)) in strides.into_iter().zip(extents).enumerate() {
        if stride == 0 || extent <= 1 {
            continue;
        }
        let stride = isize::try_from(stride)
            .map_err(|_| render_err("combined affine stride exceeds Solve-IR index range"))?;
        combined.push(solve::AffineStencilIndexStrideTerm { dimension, stride });
    }
    Ok(combined)
}

fn combined_affine_const_terms(
    terms: &[solve::AffineStencilConstStrideTerm],
    domain: &rumoca_core::StructuredIndexDomain,
) -> Result<Vec<solve::AffineStencilConstStrideTerm>, minijinja::Error> {
    let extents = domain
        .extents()
        .map_err(|error| render_err(format!("invalid native-family domain: {error}")))?;
    let mut strides = render_vec_with_capacity(
        domain.binders.len(),
        "native family constant dimension stride count",
    )?;
    strides.resize(domain.binders.len(), 0.0f64);
    for term in terms {
        if !term.stride.is_finite() {
            return Err(render_err("native family constant stride must be finite"));
        }
        let Some(stride) = strides.get_mut(term.dimension) else {
            return Err(render_err(format!(
                "affine constant stride dimension {} out of bounds",
                term.dimension
            )));
        };
        *stride += term.stride;
        if !stride.is_finite() {
            return Err(render_err(
                "native family constant stride accumulation is non-finite",
            ));
        }
    }
    let mut combined =
        render_vec_with_capacity(strides.len(), "native family combined constant strides")?;
    for (dimension, (stride, extent)) in strides.into_iter().zip(extents).enumerate() {
        if stride == 0.0 || extent <= 1 {
            continue;
        }
        combined.push(solve::AffineStencilConstStrideTerm { dimension, stride });
    }
    Ok(combined)
}

fn validate_wgsl_affine_index_range(
    base: usize,
    terms: &[solve::AffineStencilIndexStrideTerm],
    domain: &rumoca_core::StructuredIndexDomain,
    context: &str,
) -> Result<(), minijinja::Error> {
    let extents = domain
        .extents()
        .map_err(|error| render_err(format!("invalid native-family domain: {error}")))?;
    let mut minimum = i128::try_from(base)
        .map_err(|_| render_err(format!("WGSL native-family {context} base overflows")))?;
    let mut maximum = minimum;
    for term in terms {
        let extent = extents.get(term.dimension).copied().ok_or_else(|| {
            render_err(format!(
                "WGSL native-family {context} dimension {} is out of bounds",
                term.dimension
            ))
        })?;
        let last = i128::try_from(extent - 1)
            .map_err(|_| render_err(format!("WGSL native-family {context} extent overflows")))?;
        let offset = last
            .checked_mul(term.stride as i128)
            .ok_or_else(|| render_err(format!("WGSL native-family {context} stride overflows")))?;
        if offset < 0 {
            minimum = minimum.checked_add(offset).ok_or_else(|| {
                render_err(format!("WGSL native-family {context} minimum overflows"))
            })?;
        } else {
            maximum = maximum.checked_add(offset).ok_or_else(|| {
                render_err(format!("WGSL native-family {context} maximum overflows"))
            })?;
        }
    }
    if minimum < 0 || maximum > i128::from(i32::MAX) {
        return Err(render_err(format!(
            "WGSL native-family {context} index range {minimum}..={maximum} exceeds i32"
        )));
    }
    Ok(())
}

fn domain_coordinate_contribution(
    dimension: usize,
    stride: isize,
    domain: &rumoca_core::StructuredIndexDomain,
) -> Result<String, minijinja::Error> {
    if domain.binders.get(dimension).is_none() {
        return Err(render_err(format!(
            "affine stride dimension {dimension} out of bounds"
        )));
    }
    let scale = stride;
    Ok(format!(
        "i32({}) * {}",
        domain_ordinal_expr(dimension, domain)?,
        scale
    ))
}

fn domain_coordinate_contribution_f32(
    dimension: usize,
    stride: f64,
    domain: &rumoca_core::StructuredIndexDomain,
) -> Result<String, minijinja::Error> {
    if domain.binders.get(dimension).is_none() {
        return Err(render_err(format!(
            "affine stride dimension {dimension} out of bounds"
        )));
    }
    let scale = stride;
    Ok(format!(
        "f32({}) * {:?}",
        domain_ordinal_expr(dimension, domain)?,
        scale
    ))
}

fn domain_ordinal_expr(
    dimension: usize,
    domain: &rumoca_core::StructuredIndexDomain,
) -> Result<String, minijinja::Error> {
    let binder = domain
        .binders
        .get(dimension)
        .ok_or_else(|| render_err(format!("affine stride dimension {dimension} out of bounds")))?;
    let len = binder.value_count_for_render()?;
    let later = domain.binders[dimension + 1..]
        .iter()
        .try_fold(1usize, |acc, binder| {
            let count = binder.value_count_for_render()?;
            acc.checked_mul(count).ok_or_else(|| {
                render_err("structured domain suffix count overflows host index range")
            })
        })?;
    let base = if later == 1 {
        "r".to_string()
    } else {
        format!("r / {later}u")
    };
    Ok(if len == 1 {
        "0u".to_string()
    } else {
        format!("({base}) % {len}u")
    })
}

trait RenderStructuredIndexBinderExt {
    fn value_count_for_render(&self) -> Result<usize, minijinja::Error>;
}

impl RenderStructuredIndexBinderExt for rumoca_core::StructuredIndexBinder {
    fn value_count_for_render(&self) -> Result<usize, minijinja::Error> {
        if self.step == 0 {
            return Err(render_err("structured domain step cannot be zero"));
        }
        let count = if self.step > 0 {
            structured_binder_value_count(self.lower, self.upper, self.step)?
        } else if self.lower < self.upper {
            0
        } else {
            let step = self
                .step
                .checked_neg()
                .ok_or_else(|| render_err("structured domain step magnitude overflows"))?;
            structured_binder_value_count(self.upper, self.lower, step)?
        };
        usize::try_from(count)
            .map_err(|_| render_err("structured domain count exceeds host index range"))
    }
}

fn structured_binder_value_count(
    lower: i64,
    upper: i64,
    positive_step: i64,
) -> Result<i64, minijinja::Error> {
    if lower > upper {
        return Ok(0);
    }
    let distance = upper
        .checked_sub(lower)
        .ok_or_else(|| render_err("structured domain extent overflows"))?;
    distance
        .checked_div(positive_step)
        .and_then(|steps| steps.checked_add(1))
        .ok_or_else(|| render_err("structured domain count overflows"))
}

fn render_solve_row_typed(
    ops: &[solve::LinearOp],
    cfg: &SolveRowCConfig,
    dialect: SolveRowDialect,
) -> RenderResult {
    render_solve_row_typed_with_overrides(ops, &std::collections::HashMap::new(), cfg, dialect)
}

fn render_solve_row_typed_output(
    ops: &[solve::LinearOp],
    output_ordinal: usize,
    cfg: &SolveRowCConfig,
    dialect: SolveRowDialect,
) -> RenderResult {
    let mut regs = Vec::<String>::new();
    let mut seen_outputs = 0usize;
    for op in ops {
        let output = render_solve_op_typed(op, cfg, dialect, &mut regs, None)?;
        if matches!(op, solve::LinearOp::StoreOutput { .. }) {
            if seen_outputs == output_ordinal {
                return output.ok_or_else(|| render_err("solve StoreOutput did not yield a value"));
            }
            seen_outputs = next_store_output_ordinal(seen_outputs)?;
        }
    }
    missing_store_output_ordinal(output_ordinal)
}

fn next_store_output_ordinal(value: usize) -> Result<usize, minijinja::Error> {
    value
        .checked_add(1)
        .ok_or_else(|| render_err("solve StoreOutput ordinal overflows host range"))
}

fn missing_store_output_ordinal(output_ordinal: usize) -> RenderResult {
    Err(render_err(format!(
        "solve row did not contain StoreOutput #{output_ordinal}"
    )))
}

/// Typed row render where selected load ops (by op position) use a caller
/// provided access expression instead of their literal index — the
/// mechanism behind parametric native-kernel emission.
fn render_solve_row_typed_with_overrides(
    ops: &[solve::LinearOp],
    access_overrides: &std::collections::HashMap<usize, String>,
    cfg: &SolveRowCConfig,
    dialect: SolveRowDialect,
) -> RenderResult {
    let mut regs = Vec::<String>::new();
    let mut output = None;
    for (position, op) in ops.iter().enumerate() {
        if let Some(access) = access_overrides.get(&position) {
            let dst = match op {
                solve::LinearOp::LoadY { dst, .. }
                | solve::LinearOp::LoadP { dst, .. }
                | solve::LinearOp::Const { dst, .. } => *dst,
                other => {
                    return Err(render_err(format!(
                        "native family override targets a non-load/non-const op: {}",
                        other.kind_name()
                    )));
                }
            };
            store_solve_reg(
                regs.as_mut(),
                solve_reg_index(dst, "native override destination register")?,
                access.clone(),
            )?;
            continue;
        }
        output = render_solve_op_typed(op, cfg, dialect, &mut regs, output)?;
    }
    output.ok_or_else(|| render_err("solve row did not contain StoreOutput"))
}

/// Typed twin of `render_solve_op_for`. The textual output must stay
/// byte-identical to the value-walk path (`{:?}` on f64 prints the same
/// shortest round-trip form serde_json uses; non-finite constants render
/// as the dialect infinity, matching serde_json's null for non-finite).
// SPEC_0021: Exception - typed solve op rendering mirrors the serialized
// value-walk renderer variant-for-variant for byte-identical template output.
#[allow(clippy::too_many_lines)]
fn render_solve_op_typed(
    op: &solve::LinearOp,
    cfg: &SolveRowCConfig,
    dialect: SolveRowDialect,
    regs: &mut Vec<String>,
    output: Option<String>,
) -> Result<Option<String>, minijinja::Error> {
    use solve::LinearOp;
    // Runtime-indexed parameter/seed loads are handled by a shared renderer.
    if render_indexed_load_op(op, cfg, dialect, regs)? {
        return Ok(output);
    }
    match op {
        LinearOp::Const { dst, value } => {
            let text = if value.is_finite() {
                format!("{value:?}")
            } else {
                dialect.infinity().to_string()
            };
            store_solve_reg(
                regs,
                solve_reg_index(*dst, "const destination register")?,
                dialect.format_const(text),
            )?;
        }
        LinearOp::LoadTime { dst } => {
            store_solve_reg(
                regs,
                solve_reg_index(*dst, "time destination register")?,
                cfg.time.clone(),
            )?;
        }
        LinearOp::LoadY { dst, index } => {
            store_solve_reg(
                regs,
                solve_reg_index(*dst, "Y-load destination register")?,
                cfg.y_access(*index),
            )?;
        }
        LinearOp::LoadP { dst, index } => {
            store_solve_reg(
                regs,
                solve_reg_index(*dst, "parameter-load destination register")?,
                cfg.p_access(*index),
            )?;
        }
        LinearOp::LoadSeed { dst, index } => {
            let Some(seed) = cfg.seed_access(*index) else {
                return Err(render_err(
                    "LoadSeed requires a `seed` access pattern in solve-row C output",
                ));
            };
            store_solve_reg(
                regs,
                solve_reg_index(*dst, "seed-load destination register")?,
                seed,
            )?;
        }
        LinearOp::Move { dst, src } => {
            let value = solve_reg(regs, solve_reg_index(*src, "move source register")?)?;
            store_solve_reg(
                regs,
                solve_reg_index(*dst, "move destination register")?,
                value,
            )?;
        }
        LinearOp::LinearSolveComponent {
            dst,
            matrix_start,
            rhs_start,
            n,
            component,
        } => {
            let expr = dialect.render_linear_solve_component(
                regs,
                solve_reg_index(*matrix_start, "linear-solve matrix register")?,
                solve_reg_index(*rhs_start, "linear-solve RHS register")?,
                *n,
                *component,
            )?;
            store_solve_reg(
                regs,
                solve_reg_index(*dst, "linear-solve destination register")?,
                expr,
            )?;
        }
        LinearOp::Unary { dst, op, arg } => {
            let arg = solve_reg(regs, solve_reg_index(*arg, "unary argument register")?)?;
            store_solve_reg(
                regs,
                solve_reg_index(*dst, "unary destination register")?,
                dialect.render_unary(op.kind_name(), arg)?,
            )?;
        }
        LinearOp::Binary { dst, op, lhs, rhs } => {
            let lhs = solve_reg(regs, solve_reg_index(*lhs, "binary lhs register")?)?;
            let rhs = solve_reg(regs, solve_reg_index(*rhs, "binary rhs register")?)?;
            store_solve_reg(
                regs,
                solve_reg_index(*dst, "binary destination register")?,
                dialect.render_binary(op.kind_name(), lhs, rhs)?,
            )?;
        }
        LinearOp::Compare { dst, op, lhs, rhs } => {
            let lhs = solve_reg(regs, solve_reg_index(*lhs, "compare lhs register")?)?;
            let rhs = solve_reg(regs, solve_reg_index(*rhs, "compare rhs register")?)?;
            store_solve_reg(
                regs,
                solve_reg_index(*dst, "compare destination register")?,
                dialect.render_compare(op.kind_name(), lhs, rhs)?,
            )?;
        }
        LinearOp::Select {
            dst,
            cond,
            if_true,
            if_false,
        } => {
            let cond = solve_reg(regs, solve_reg_index(*cond, "select condition register")?)?;
            let if_true = solve_reg(regs, solve_reg_index(*if_true, "select true register")?)?;
            let if_false = solve_reg(regs, solve_reg_index(*if_false, "select false register")?)?;
            store_solve_reg(
                regs,
                solve_reg_index(*dst, "select destination register")?,
                dialect.render_select(cond, if_true, if_false),
            )?;
        }
        LinearOp::StoreOutput { src } => {
            return Ok(Some(solve_reg(
                regs,
                solve_reg_index(*src, "store-output source register")?,
            )?));
        }
        other => {
            return Err(render_err(format!(
                "unsupported solve LinearOp: {}",
                other.kind_name()
            )));
        }
    }
    Ok(output)
}

/// Render the array-access expression for a runtime-indexed parameter (or seed)
/// load: `array[base + clamp(round(index_expr), 0, count-1)]`.
fn render_indexed_access(
    cfg: &SolveRowCConfig,
    dialect: SolveRowDialect,
    index_expr: &str,
    base: usize,
    count: usize,
    is_seed: bool,
) -> Result<String, minijinja::Error> {
    let slot = dialect.render_indexed_index(index_expr, base, count);
    if is_seed {
        cfg.seed_access_expr(&slot).ok_or_else(|| {
            render_err("LoadIndexedSeed requires a `seed` access pattern in solve-row output")
        })
    } else {
        Ok(cfg.p_access_expr(&slot))
    }
}

/// Typed-renderer handler for runtime-indexed parameter/seed loads: stores the
/// access expression into `regs` and returns `true` when `op` is such a load.
fn render_indexed_load_op(
    op: &solve::LinearOp,
    cfg: &SolveRowCConfig,
    dialect: SolveRowDialect,
    regs: &mut Vec<String>,
) -> Result<bool, minijinja::Error> {
    use solve::LinearOp;
    let (dst, base, count, index, is_seed) = match op {
        LinearOp::LoadIndexedP {
            dst,
            base,
            count,
            index,
        } => (*dst, *base, *count, *index, false),
        LinearOp::LoadIndexedSeed {
            dst,
            base,
            count,
            index,
        } => (*dst, *base, *count, *index, true),
        _ => return Ok(false),
    };
    let index_expr = solve_reg(regs, index as usize)?;
    let expr = render_indexed_access(cfg, dialect, &index_expr, base, count, is_seed)?;
    store_solve_reg(regs, dst as usize, expr)?;
    Ok(true)
}

/// Value-renderer (`get_field`) handler for a runtime-indexed load, mirroring
/// [`render_indexed_load_op`] but producing a [`SolveOpEffect`] from the lazy op
/// `Value`. The caller dispatches on op kind, so this always handles an indexed
/// load (no optional-miss return).
/// Value-renderer handler for a `LinearSolveComponent` op, extracted from the
/// `solve_op_expr` chain to keep it within the complexity budget.
fn solve_linsolve_effect(
    value: &Value,
    dialect: SolveRowDialect,
    regs: &[String],
) -> Result<SolveOpEffect, minijinja::Error> {
    let dst = solve_field_usize(value, "dst")?;
    let matrix_start = solve_field_usize(value, "matrix_start")?;
    let rhs_start = solve_field_usize(value, "rhs_start")?;
    let n = solve_field_usize(value, "n")?;
    let component = solve_field_usize(value, "component")?;
    let expr =
        dialect.render_linear_solve_component(regs, matrix_start, rhs_start, n, component)?;
    Ok(SolveOpEffect::Compute { dst, expr })
}

fn solve_indexed_effect(
    value: &Value,
    cfg: &SolveRowCConfig,
    dialect: SolveRowDialect,
    regs: &[String],
    is_seed: bool,
) -> Result<SolveOpEffect, minijinja::Error> {
    let dst = solve_field_usize(value, "dst")?;
    let base = solve_field_usize(value, "base")?;
    let count = solve_field_usize(value, "count")?;
    let index = solve_field_usize(value, "index")?;
    let index_expr = solve_reg(regs, index)?;
    let expr = render_indexed_access(cfg, dialect, &index_expr, base, count, is_seed)?;
    Ok(SolveOpEffect::Compute { dst, expr })
}

/// The effect of a single `LinearOp` when rendering a solve program: either it
/// computes a value into register `dst`, or it stores a register to an output.
enum SolveOpEffect {
    Compute { dst: usize, expr: String },
    Store { src: usize },
}

/// Compute the rendered expression (and destination register) for one solve op,
/// reading operand registers from `regs` (their already-rendered expressions or
/// temp names). Shared by the single-output expression renderer
/// ([`render_solve_op_for`]) and the multi-output block renderer
/// ([`render_solve_block_for`]).
fn solve_op_expr(
    op: &Value,
    cfg: &SolveRowCConfig,
    dialect: SolveRowDialect,
    regs: &[String],
) -> Result<SolveOpEffect, minijinja::Error> {
    if let Ok(value) = get_field(op, "Const") {
        let dst = solve_field_usize(&value, "dst")?;
        let expr = dialect.format_const(solve_const_value_string(&value, dialect.infinity())?);
        return Ok(SolveOpEffect::Compute { dst, expr });
    }
    if let Ok(value) = get_field(op, "LoadTime") {
        let dst = solve_field_usize(&value, "dst")?;
        return Ok(SolveOpEffect::Compute {
            dst,
            expr: cfg.time.clone(),
        });
    }
    if let Ok(value) = get_field(op, "LoadY") {
        let dst = solve_field_usize(&value, "dst")?;
        let index = solve_field_usize(&value, "index")?;
        return Ok(SolveOpEffect::Compute {
            dst,
            expr: cfg.y_access(index),
        });
    }
    if let Ok(value) = get_field(op, "LoadP") {
        let dst = solve_field_usize(&value, "dst")?;
        let index = solve_field_usize(&value, "index")?;
        return Ok(SolveOpEffect::Compute {
            dst,
            expr: cfg.p_access(index),
        });
    }
    if let Ok(value) = get_field(op, "LoadIndexedP") {
        return solve_indexed_effect(&value, cfg, dialect, regs, false);
    }
    if let Ok(value) = get_field(op, "LoadIndexedSeed") {
        return solve_indexed_effect(&value, cfg, dialect, regs, true);
    }
    if let Ok(value) = get_field(op, "LoadSeed") {
        let dst = solve_field_usize(&value, "dst")?;
        let index = solve_field_usize(&value, "index")?;
        let Some(seed) = cfg.seed_access(index) else {
            return Err(render_err(
                "LoadSeed requires a `seed` access pattern in solve-row C output",
            ));
        };
        return Ok(SolveOpEffect::Compute { dst, expr: seed });
    }
    if let Ok(value) = get_field(op, "Move") {
        let dst = solve_field_usize(&value, "dst")?;
        let src = solve_reg(regs, solve_field_usize(&value, "src")?)?;
        return Ok(SolveOpEffect::Compute { dst, expr: src });
    }
    if let Ok(value) = get_field(op, "LinearSolveComponent") {
        return solve_linsolve_effect(&value, dialect, regs);
    }
    if let Ok(value) = get_field(op, "Unary") {
        let dst = solve_field_usize(&value, "dst")?;
        let op = solve_variant_name(&get_field(&value, "op")?)?;
        let arg = solve_reg(regs, solve_field_usize(&value, "arg")?)?;
        return Ok(SolveOpEffect::Compute {
            dst,
            expr: dialect.render_unary(&op, arg)?,
        });
    }
    if let Ok(value) = get_field(op, "Binary") {
        let dst = solve_field_usize(&value, "dst")?;
        let op = solve_variant_name(&get_field(&value, "op")?)?;
        let lhs = solve_reg(regs, solve_field_usize(&value, "lhs")?)?;
        let rhs = solve_reg(regs, solve_field_usize(&value, "rhs")?)?;
        return Ok(SolveOpEffect::Compute {
            dst,
            expr: dialect.render_binary(&op, lhs, rhs)?,
        });
    }
    if let Ok(value) = get_field(op, "Compare") {
        let dst = solve_field_usize(&value, "dst")?;
        let op = solve_variant_name(&get_field(&value, "op")?)?;
        let lhs = solve_reg(regs, solve_field_usize(&value, "lhs")?)?;
        let rhs = solve_reg(regs, solve_field_usize(&value, "rhs")?)?;
        return Ok(SolveOpEffect::Compute {
            dst,
            expr: dialect.render_compare(&op, lhs, rhs)?,
        });
    }
    if let Ok(value) = get_field(op, "Select") {
        let dst = solve_field_usize(&value, "dst")?;
        let cond = solve_reg(regs, solve_field_usize(&value, "cond")?)?;
        let if_true = solve_reg(regs, solve_field_usize(&value, "if_true")?)?;
        let if_false = solve_reg(regs, solve_field_usize(&value, "if_false")?)?;
        return Ok(SolveOpEffect::Compute {
            dst,
            expr: dialect.render_select(cond, if_true, if_false),
        });
    }
    if let Ok(value) = get_field(op, "StoreOutput") {
        let src = solve_field_usize(&value, "src")?;
        return Ok(SolveOpEffect::Store { src });
    }
    Err(render_err(format!("unsupported solve LinearOp: {op}")))
}

fn render_solve_op_for(
    op: &Value,
    cfg: &SolveRowCConfig,
    dialect: SolveRowDialect,
    regs: &mut Vec<String>,
    output: Option<String>,
) -> Result<Option<String>, minijinja::Error> {
    match solve_op_expr(op, cfg, dialect, regs)? {
        SolveOpEffect::Compute { dst, expr } => {
            store_solve_reg(regs, dst, expr)?;
            Ok(output)
        }
        SolveOpEffect::Store { src } => Ok(Some(solve_reg(regs, src)?)),
    }
}

#[derive(Clone, Copy)]
enum SolveRowDialect {
    /// WGSL compute-shader dialect (WebGPU). f32 baseline precision; boolean
    /// values keep the numeric 0.0/1.0 encoding via `select`.
    Wgsl,
}

impl SolveRowDialect {
    fn infinity(self) -> &'static str {
        // WGSL has no portable infinity literal; f32::MAX approximates it.
        "3.4028235e38"
    }

    /// WGSL rejects abstract-int/float mixing in some positions.
    fn format_const(self, value: String) -> String {
        let looks_integer = !value.is_empty()
            && !value.contains(['.', 'e', 'E'])
            && value
                .strip_prefix('-')
                .unwrap_or(&value)
                .chars()
                .all(|c| c.is_ascii_digit());
        if looks_integer {
            format!("{value}.0")
        } else {
            value
        }
    }

    fn render_compare(self, op: &str, lhs: String, rhs: String) -> RenderResult {
        render_solve_compare_wgsl(op, lhs, rhs)
    }

    fn render_select(self, cond: String, if_true: String, if_false: String) -> String {
        format!("select(({if_false}), ({if_true}), ({cond}) != 0.0)")
    }

    /// Render the integer array index for a runtime-indexed load:
    /// `base + clamp(round(index_expr), 0, count-1)`, matching
    /// [`rumoca_ir_solve::resolve_indexed_slot`]. The result is an integer-typed
    /// expression suitable to substitute into a `p[...]` / `seed[...]` access.
    fn render_indexed_index(self, index_expr: &str, base: usize, count: usize) -> String {
        let last = if count == 0 { 0 } else { count - 1 };
        format!("({base}u + u32(clamp(round({index_expr}), 0.0, f32({last}))))")
    }

    fn render_linear_solve_component(
        self,
        _regs: &[String],
        _matrix_start: usize,
        _rhs_start: usize,
        _n: usize,
        _component: usize,
    ) -> RenderResult {
        Err(render_err(
            "LinearSolveComponent is not supported by the WGSL dialect yet; \
             models with implicit linear blocks cannot target wgsl-ode",
        ))
    }

    fn render_unary(self, op: &str, arg: String) -> RenderResult {
        render_solve_unary_wgsl(op, arg)
    }

    fn render_binary(self, op: &str, lhs: String, rhs: String) -> RenderResult {
        render_solve_binary_wgsl(op, lhs, rhs)
    }
}

fn solve_reg_index(reg: solve::Reg, context: &'static str) -> Result<usize, minijinja::Error> {
    usize::try_from(reg).map_err(|_| render_err(format!("{context} exceeds host usize")))
}

fn store_solve_reg(
    regs: &mut Vec<String>,
    dst: usize,
    expr: String,
) -> Result<(), minijinja::Error> {
    if regs.len() <= dst {
        let target_len = dst
            .checked_add(1)
            .ok_or_else(|| render_err("solve row register index overflows"))?;
        reserve_render_capacity(
            regs,
            target_len - regs.len(),
            "solve row register file size",
        )?;
        regs.resize(target_len, String::new());
    }
    regs[dst] = expr;
    Ok(())
}

fn solve_reg(regs: &[String], reg: usize) -> RenderResult {
    regs.get(reg)
        .filter(|expr| !expr.is_empty())
        .cloned()
        .ok_or_else(|| render_err(format!("solve row references unset register {reg}")))
}

fn solve_field_usize(value: &Value, field: &str) -> Result<usize, minijinja::Error> {
    solve_field_string(value, field)?
        .parse::<usize>()
        .map_err(|_| render_err(format!("solve field `{field}` is not a usize")))
}

fn solve_field_string(value: &Value, field: &str) -> Result<String, minijinja::Error> {
    Ok(value_to_string(&get_field(value, field)?))
}

fn solve_const_value_string(value: &Value, infinity: &str) -> Result<String, minijinja::Error> {
    let field = value
        .get_attr("value")
        .or_else(|_| value.get_item(&Value::from("value")))?;
    if field.is_none() {
        return Ok(infinity.to_string());
    }
    if field.is_undefined() {
        return Err(render_err("solve Const op is missing `value`"));
    }
    Ok(value_to_string(&field))
}

fn solve_variant_name(value: &Value) -> RenderResult {
    let raw = value_to_string(value);
    if !raw.is_empty() && raw != "{}" {
        return Ok(raw);
    }
    for name in [
        "Neg", "Not", "Abs", "Sign", "Sqrt", "Floor", "Ceil", "Trunc", "Sin", "Cos", "Tan", "Asin",
        "Acos", "Atan", "Sinh", "Cosh", "Tanh", "Exp", "Log", "Log10", "Add", "Sub", "Mul", "Div",
        "Pow", "And", "Or", "Atan2", "Min", "Max", "Lt", "Le", "Gt", "Ge", "Eq", "Ne",
    ] {
        if get_field(value, name).is_ok() {
            return Ok(name.to_string());
        }
    }
    Err(render_err(format!("unsupported solve op variant: {value}")))
}

struct SolveRowCConfig {
    time: String,
    y_pattern: String,
    p_pattern: String,
    seed_pattern: Option<String>,
}

impl SolveRowCConfig {
    fn from_value(value: &Value) -> Self {
        Self {
            time: config_string(value, "time").unwrap_or_else(|| "m->time".to_string()),
            y_pattern: config_string(value, "y")
                .unwrap_or_else(|| "__rumoca_solve_y(m, {})".to_string()),
            p_pattern: config_string(value, "p")
                .unwrap_or_else(|| "__rumoca_solve_p(m, {})".to_string()),
            seed_pattern: config_string(value, "seed"),
        }
    }

    fn y_access(&self, index: usize) -> String {
        format_solve_access(&self.y_pattern, index)
    }

    fn y_access_expr(&self, index_expr: &str) -> String {
        self.y_pattern.replace("{}", index_expr)
    }

    fn p_access_expr(&self, index_expr: &str) -> String {
        self.p_pattern.replace("{}", index_expr)
    }

    fn p_access(&self, index: usize) -> String {
        format_solve_access(&self.p_pattern, index)
    }

    fn seed_access(&self, index: usize) -> Option<String> {
        self.seed_pattern
            .as_ref()
            .map(|pattern| format_solve_access(pattern, index))
    }

    fn seed_access_expr(&self, index_expr: &str) -> Option<String> {
        self.seed_pattern
            .as_ref()
            .map(|pattern| pattern.replace("{}", index_expr))
    }
}

fn format_solve_access(pattern: &str, index: usize) -> String {
    if pattern.contains("{}") {
        return pattern.replacen("{}", &index.to_string(), 1);
    }
    format!("{pattern}[{index}]")
}

fn config_string(value: &Value, field: &str) -> Option<String> {
    get_field(value, field)
        .ok()
        .map(|field| value_to_string(&field))
        .filter(|field| !field.is_empty())
}
