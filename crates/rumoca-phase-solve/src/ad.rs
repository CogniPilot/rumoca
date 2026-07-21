//! AD lowering from primal linear ops to forward-mode J·v ops.
//!
//! SPEC_0021 file-size exception: AD lowering still keeps scalar row AD,
//! tensor-node JVP lowering, and regression tests together while Solve IR
//! multi-output programs are being stabilized. split plan: move tensor-node JVP
//! lowering and indexed-load AD helpers into sibling modules behind this facade.

use crate::lower::{LowerError, lower_initial_residual, lower_residual};
use rumoca_ir_solve::{
    AffineStencilConstStride, AffineStencilLoadStride, BinaryOp, CompareOp, ComputeBlock,
    ComputeNode, LinearOp, RandomGenerator, Reg, ScalarProgramBlock, UnaryOp, VarLayout,
};
use std::collections::HashMap;

#[derive(Debug, Clone, Copy)]
struct DualReg {
    re: Reg,
    du: Reg,
}

pub fn lower_residual_ad(
    dae_model: &rumoca_ir_dae::Dae,
    layout: &VarLayout,
) -> Result<Vec<Vec<LinearOp>>, LowerError> {
    let primal_rows = lower_residual(dae_model, layout)?;
    lower_scalar_program_block_ad(&primal_rows)
}

pub fn lower_initial_residual_ad(
    dae_model: &rumoca_ir_dae::Dae,
    layout: &VarLayout,
) -> Result<Vec<Vec<LinearOp>>, LowerError> {
    let primal_rows = lower_initial_residual(dae_model, layout)?;
    lower_scalar_program_block_ad(&primal_rows)
}

pub fn lower_residual_full_ad(
    dae_model: &rumoca_ir_dae::Dae,
    layout: &VarLayout,
) -> Result<Vec<Vec<LinearOp>>, LowerError> {
    let primal_rows = lower_residual(dae_model, layout)?;
    lower_scalar_program_block_full_ad(&primal_rows, layout)
}

pub fn lower_initial_residual_full_ad(
    dae_model: &rumoca_ir_dae::Dae,
    layout: &VarLayout,
) -> Result<Vec<Vec<LinearOp>>, LowerError> {
    let primal_rows = lower_initial_residual(dae_model, layout)?;
    lower_scalar_program_block_full_ad(&primal_rows, layout)
}

/// Compute the forward-mode JVP of a `ComputeBlock`, preserving tensor structure.
///
/// For `ScalarPrograms` nodes: applies the existing scalar AD pass row-by-row.
/// For `MatMul` nodes: preserves tensor structure and uses a block-product form
/// for the product rule when both operands depend on solver-y.
/// For `LinSolve` nodes: emits another `LinSolve` using
/// `dx = A^{-1}(db - dA * x)` so coupled systems keep tensor solve structure.
/// For `Map` and `AffineStencil` nodes: transforms the compact base program once
/// and remaps its affine load/constant strides to the generated dual program.
pub fn lower_compute_block_jvp(block: &ComputeBlock) -> Result<ComputeBlock, LowerError> {
    let span = compute_block_context_span(block);
    let mut nodes = ad_vec_with_capacity(block.nodes.len(), "compute block JVP node count", span)?;
    for node in &block.nodes {
        nodes.push(lower_compute_node_jvp(node)?);
    }
    Ok(ComputeBlock { nodes })
}

fn lower_compute_node_jvp(node: &ComputeNode) -> Result<ComputeNode, LowerError> {
    match node {
        ComputeNode::ScalarPrograms(rows) => {
            let jvp_rows =
                lower_scalar_program_block_ad_with_spans(&rows.programs, &rows.program_spans)?;
            Ok(ComputeNode::ScalarPrograms(
                ScalarProgramBlock::with_output_indices(
                    jvp_rows,
                    rows.program_spans.clone(),
                    rows.output_indices.clone(),
                )?,
            ))
        }
        ComputeNode::MatMul { .. } => lower_matmul_jvp_node(node),
        ComputeNode::LinSolve {
            setup_ops,
            matrix_start,
            rhs_start,
            n,
            metadata,
            span,
            ..
        } => lower_linsolve_jvp_node(
            setup_ops,
            *matrix_start,
            *rhs_start,
            *n,
            metadata.clone(),
            *span,
        ),
        ComputeNode::Map {
            domain,
            output_map,
            base_ops,
            load_strides,
            const_strides,
            metadata,
            span,
        } => {
            let lowered = lower_affine_jvp_ops(base_ops, load_strides, const_strides, *span)?;
            Ok(ComputeNode::Map {
                domain: domain.clone(),
                output_map: output_map.clone(),
                base_ops: lowered.ops,
                load_strides: lowered.load_strides,
                const_strides: lowered.const_strides,
                metadata: metadata.clone(),
                span: *span,
            })
        }
        ComputeNode::AffineStencil {
            domain,
            output_map,
            base_ops,
            load_strides,
            const_strides,
            metadata,
            span,
        } => {
            let lowered = lower_affine_jvp_ops(base_ops, load_strides, const_strides, *span)?;
            Ok(ComputeNode::AffineStencil {
                domain: domain.clone(),
                output_map: output_map.clone(),
                base_ops: lowered.ops,
                load_strides: lowered.load_strides,
                const_strides: lowered.const_strides,
                metadata: metadata.clone(),
                span: *span,
            })
        }
    }
}

struct AffineJvpOps {
    ops: Vec<LinearOp>,
    load_strides: Vec<AffineStencilLoadStride>,
    const_strides: Vec<AffineStencilConstStride>,
}

fn lower_affine_jvp_ops(
    base_ops: &[LinearOp],
    load_strides: &[AffineStencilLoadStride],
    const_strides: &[AffineStencilConstStride],
    span: rumoca_core::Span,
) -> Result<AffineJvpOps, LowerError> {
    validate_affine_jvp_stride_targets(base_ops, load_strides, const_strides, span)?;
    let mut builder = AdBuilder::new_with_span(SeedMode::SolverYOnly, span);
    let mut lowered_load_strides = Vec::new();
    let mut lowered_const_strides = Vec::new();

    for (op_position, op) in base_ops.iter().copied().enumerate() {
        let generated_start = builder.ops.len();
        builder.lower_op(op)?;
        let generated_end = builder.ops.len();

        for stride in load_strides
            .iter()
            .filter(|stride| stride.op_position == op_position)
        {
            append_generated_load_strides(
                &builder.ops,
                generated_start,
                generated_end,
                stride,
                &mut lowered_load_strides,
            );
        }
        for stride in const_strides
            .iter()
            .filter(|stride| stride.op_position == op_position)
        {
            lowered_const_strides.push(AffineStencilConstStride {
                op_position: generated_start,
                terms: stride.terms.clone(),
            });
        }
    }

    Ok(AffineJvpOps {
        ops: builder.ops,
        load_strides: lowered_load_strides,
        const_strides: lowered_const_strides,
    })
}

fn append_generated_load_strides(
    ops: &[LinearOp],
    generated_start: usize,
    generated_end: usize,
    stride: &AffineStencilLoadStride,
    lowered: &mut Vec<AffineStencilLoadStride>,
) {
    for (generated_position, op) in ops
        .iter()
        .enumerate()
        .take(generated_end)
        .skip(generated_start)
    {
        if matches!(
            op,
            LinearOp::LoadY { .. } | LinearOp::LoadP { .. } | LinearOp::LoadSeed { .. }
        ) {
            lowered.push(AffineStencilLoadStride {
                op_position: generated_position,
                terms: stride.terms.clone(),
            });
        }
    }
}

fn validate_affine_jvp_stride_targets(
    base_ops: &[LinearOp],
    load_strides: &[AffineStencilLoadStride],
    const_strides: &[AffineStencilConstStride],
    span: rumoca_core::Span,
) -> Result<(), LowerError> {
    for stride in load_strides {
        if !matches!(
            base_ops.get(stride.op_position),
            Some(LinearOp::LoadY { .. } | LinearOp::LoadP { .. })
        ) {
            return Err(ad_contract_violation(
                format!(
                    "affine JVP load stride targets invalid op position {}",
                    stride.op_position
                ),
                span,
            ));
        }
    }
    for stride in const_strides {
        if !matches!(
            base_ops.get(stride.op_position),
            Some(LinearOp::Const { .. })
        ) {
            return Err(ad_contract_violation(
                format!(
                    "affine JVP const stride targets invalid op position {}",
                    stride.op_position
                ),
                span,
            ));
        }
    }
    Ok(())
}

fn lower_matmul_jvp_node(node: &ComputeNode) -> Result<ComputeNode, LowerError> {
    let ComputeNode::MatMul {
        lhs_ops,
        lhs_start,
        rhs_ops,
        rhs_start,
        m,
        k,
        n,
        lhs_sparsity,
        rhs_sparsity,
        metadata,
        span,
    } = node
    else {
        let span = compute_node_span(node)?;
        return Err(ad_contract_violation(
            format!(
                "MatMul JVP lowering received {} node",
                compute_node_kind(node)
            ),
            span,
        ));
    };

    let lhs_len = checked_ad_product(*m, *k, *span, "MatMul JVP lhs value count")?;
    let rhs_len = checked_ad_product(*k, *n, *span, "MatMul JVP rhs value count")?;
    let lhs_depends_on_y = ops_reference_y(lhs_ops);
    let rhs_depends_on_y = ops_reference_y(rhs_ops);

    let (mut lhs_builder, lhs) = lower_tensor_operand(lhs_ops, *lhs_start, lhs_len, 0, *span)?;
    let (jvp_lhs_start, jvp_k, jvp_lhs_sparsity) = if lhs_depends_on_y && rhs_depends_on_y {
        let regs = block_product_lhs_regs(&lhs, *m, *k, *span)?;
        (
            lhs_builder.pack_registers(&regs)?,
            checked_ad_product(2, *k, *span, "MatMul JVP block inner dimension")?,
            rumoca_ir_solve::SparsityPattern::Dense,
        )
    } else {
        let regs = if lhs_depends_on_y {
            dual_regs(&lhs, DualPart::Tangent, "MatMul JVP lhs tangent", *span)?
        } else {
            dual_regs(&lhs, DualPart::Primal, "MatMul JVP lhs primal", *span)?
        };
        (lhs_builder.pack_registers(&regs)?, *k, lhs_sparsity.clone())
    };

    let rhs_next_reg = lhs_builder.next_reg;
    let (mut rhs_builder, rhs) =
        lower_tensor_operand(rhs_ops, *rhs_start, rhs_len, rhs_next_reg, *span)?;
    let (jvp_rhs_start, jvp_rhs_sparsity) = if lhs_depends_on_y && rhs_depends_on_y {
        let regs = block_product_rhs_regs(&rhs, *span)?;
        (
            rhs_builder.pack_registers(&regs)?,
            rumoca_ir_solve::SparsityPattern::Dense,
        )
    } else {
        let regs = if rhs_depends_on_y || !lhs_depends_on_y {
            dual_regs(&rhs, DualPart::Tangent, "MatMul JVP rhs tangent", *span)?
        } else {
            dual_regs(&rhs, DualPart::Primal, "MatMul JVP rhs primal", *span)?
        };
        (rhs_builder.pack_registers(&regs)?, rhs_sparsity.clone())
    };

    Ok(ComputeNode::MatMul {
        lhs_ops: lhs_builder.ops,
        lhs_start: jvp_lhs_start,
        rhs_ops: rhs_builder.ops,
        rhs_start: jvp_rhs_start,
        m: *m,
        k: jvp_k,
        n: *n,
        lhs_sparsity: jvp_lhs_sparsity,
        rhs_sparsity: jvp_rhs_sparsity,
        metadata: metadata.clone(),
        span: *span,
    })
}

fn lower_tensor_operand(
    ops: &[LinearOp],
    value_start: Reg,
    value_count: usize,
    next_reg: Reg,
    span: rumoca_core::Span,
) -> Result<(AdBuilder, Vec<DualReg>), LowerError> {
    let mut builder = AdBuilder::new_with_span(SeedMode::SolverYOnly, span);
    builder.next_reg = next_reg;
    for op in ops {
        builder.lower_op(*op)?;
    }
    let values = collect_dual_range(
        &builder,
        value_start,
        value_count,
        span,
        "MatMul JVP operand value count",
        "operand range",
    )?;
    Ok((builder, values))
}

#[derive(Clone, Copy)]
enum DualPart {
    Primal,
    Tangent,
}

fn dual_regs(
    values: &[DualReg],
    part: DualPart,
    context: &'static str,
    span: rumoca_core::Span,
) -> Result<Vec<Reg>, LowerError> {
    let mut regs = ad_vec_with_capacity(values.len(), context, span)?;
    regs.extend(values.iter().map(|value| match part {
        DualPart::Primal => value.re,
        DualPart::Tangent => value.du,
    }));
    Ok(regs)
}

fn block_product_lhs_regs(
    lhs: &[DualReg],
    m: usize,
    k: usize,
    span: rumoca_core::Span,
) -> Result<Vec<Reg>, LowerError> {
    let count = checked_ad_product(lhs.len(), 2, span, "MatMul JVP block lhs value count")?;
    let mut regs = ad_vec_with_capacity(count, "MatMul JVP block lhs value count", span)?;
    for row in 0..m {
        let start = checked_ad_product(row, k, span, "MatMul JVP block lhs row")?;
        let end = start.checked_add(k).ok_or_else(|| {
            ad_contract_violation("MatMul JVP block lhs row overflow".to_string(), span)
        })?;
        regs.extend(lhs[start..end].iter().map(|value| value.du));
        regs.extend(lhs[start..end].iter().map(|value| value.re));
    }
    Ok(regs)
}

fn block_product_rhs_regs(
    rhs: &[DualReg],
    span: rumoca_core::Span,
) -> Result<Vec<Reg>, LowerError> {
    let count = checked_ad_product(rhs.len(), 2, span, "MatMul JVP block rhs value count")?;
    let mut regs = ad_vec_with_capacity(count, "MatMul JVP block rhs value count", span)?;
    regs.extend(rhs.iter().map(|value| value.re));
    regs.extend(rhs.iter().map(|value| value.du));
    Ok(regs)
}

fn compute_node_kind(node: &ComputeNode) -> &'static str {
    match node {
        ComputeNode::ScalarPrograms(_) => "ScalarPrograms",
        ComputeNode::MatMul { .. } => "MatMul",
        ComputeNode::LinSolve { .. } => "LinSolve",
        ComputeNode::Map { .. } => "Map",
        ComputeNode::AffineStencil { .. } => "AffineStencil",
    }
}

fn compute_node_span(node: &ComputeNode) -> Result<rumoca_core::Span, LowerError> {
    match node {
        ComputeNode::ScalarPrograms(block) => block.program_span(0).ok_or_else(|| {
            ad_optional_contract_violation(
                "ScalarPrograms node has no program span for AD error context".to_string(),
                first_non_dummy_span(&block.program_spans),
            )
        }),
        ComputeNode::MatMul { span, .. }
        | ComputeNode::LinSolve { span, .. }
        | ComputeNode::Map { span, .. }
        | ComputeNode::AffineStencil { span, .. } => Ok(*span),
    }
}

fn compute_block_context_span(block: &ComputeBlock) -> Option<rumoca_core::Span> {
    for node in &block.nodes {
        if let Some(span) = compute_node_context_span(node) {
            return Some(span);
        }
    }
    None
}

fn compute_node_context_span(node: &ComputeNode) -> Option<rumoca_core::Span> {
    match node {
        ComputeNode::ScalarPrograms(block) => first_non_dummy_span(&block.program_spans),
        ComputeNode::MatMul { span, .. }
        | ComputeNode::LinSolve { span, .. }
        | ComputeNode::Map { span, .. }
        | ComputeNode::AffineStencil { span, .. } => (!span.is_dummy()).then_some(*span),
    }
}

/// Returns true if any op in the slice is `LoadY`.
fn ops_reference_y(ops: &[LinearOp]) -> bool {
    ops.iter().any(|op| matches!(op, LinearOp::LoadY { .. }))
}

fn lower_linsolve_jvp_node(
    setup_ops: &[LinearOp],
    matrix_start: Reg,
    rhs_start: Reg,
    n: usize,
    metadata: rumoca_ir_solve::TensorNodeMetadata,
    span: rumoca_core::Span,
) -> Result<ComputeNode, LowerError> {
    if n == 0 {
        return Err(unsupported("invalid zero-sized LinSolve in JVP"));
    }

    let mut builder = AdBuilder::new_with_span(SeedMode::SolverYOnly, span);
    for op in setup_ops {
        builder.lower_op(*op)?;
    }

    let matrix_len = checked_ad_product(n, n, span, "LinSolve JVP matrix range")?;
    let matrix = collect_dual_range(
        &builder,
        matrix_start,
        matrix_len,
        span,
        "LinSolve JVP matrix value count",
        "matrix range",
    )?;
    let rhs = collect_dual_range(
        &builder,
        rhs_start,
        n,
        span,
        "LinSolve JVP RHS value count",
        "rhs range",
    )?;

    let matrix_re = real_regs_from_duals(&matrix, "LinSolve JVP matrix real value count", span)?;
    let rhs_re = real_regs_from_duals(&rhs, "LinSolve JVP RHS real value count", span)?;
    let matrix_re_start = builder.pack_registers(&matrix_re)?;
    let rhs_re_start = builder.pack_registers(&rhs_re)?;

    let mut solution = ad_vec_with_capacity(n, "LinSolve JVP solution value count", span)?;
    for component in 0..n {
        let dst = builder.alloc_reg()?;
        builder.ops.push(LinearOp::LinearSolveComponent {
            dst,
            matrix_start: matrix_re_start,
            rhs_start: rhs_re_start,
            n,
            component,
        });
        solution.push(dst);
    }

    let mut tangent_rhs = ad_vec_with_capacity(n, "LinSolve JVP tangent RHS value count", span)?;
    for row in 0..n {
        let mut acc = rhs[row].du;
        for col in 0..n {
            let matrix_du = matrix[row * n + col].du;
            let product = builder.emit_binary(BinaryOp::Mul, matrix_du, solution[col])?;
            acc = builder.emit_binary(BinaryOp::Sub, acc, product)?;
        }
        tangent_rhs.push(acc);
    }
    let tangent_rhs_start = builder.pack_registers(&tangent_rhs)?;
    let next_reg = builder.next_reg;

    Ok(ComputeNode::LinSolve {
        setup_ops: builder.ops,
        matrix_start: matrix_re_start,
        rhs_start: tangent_rhs_start,
        n,
        next_reg,
        metadata,
        span,
    })
}

pub fn lower_scalar_program_block_ad(
    primal_rows: &[Vec<LinearOp>],
) -> Result<Vec<Vec<LinearOp>>, LowerError> {
    lower_scalar_program_rows_ad(
        primal_rows,
        &[],
        SeedMode::SolverYOnly,
        "scalar program AD row count",
    )
}

pub fn lower_scalar_program_block_full_ad(
    primal_rows: &[Vec<LinearOp>],
    layout: &VarLayout,
) -> Result<Vec<Vec<LinearOp>>, LowerError> {
    lower_scalar_program_block_full_ad_with_spans(primal_rows, &[], layout)
}

pub fn lower_scalar_program_block_full_ad_with_spans(
    primal_rows: &[Vec<LinearOp>],
    row_spans: &[rumoca_core::Span],
    layout: &VarLayout,
) -> Result<Vec<Vec<LinearOp>>, LowerError> {
    lower_scalar_program_rows_ad(
        primal_rows,
        row_spans,
        SeedMode::SolverYAndP {
            p_seed_offset: layout.y_scalars(),
        },
        "full scalar program AD row count",
    )
}

fn lower_scalar_program_block_ad_with_spans(
    primal_rows: &[Vec<LinearOp>],
    row_spans: &[rumoca_core::Span],
) -> Result<Vec<Vec<LinearOp>>, LowerError> {
    lower_scalar_program_rows_ad(
        primal_rows,
        row_spans,
        SeedMode::SolverYOnly,
        "spanned scalar program AD row count",
    )
}

fn lower_scalar_program_rows_ad(
    primal_rows: &[Vec<LinearOp>],
    row_spans: &[rumoca_core::Span],
    seed_mode: SeedMode,
    context: &'static str,
) -> Result<Vec<Vec<LinearOp>>, LowerError> {
    let span = first_non_dummy_span(row_spans);
    if !row_spans.is_empty() && row_spans.len() != primal_rows.len() {
        return Err(ad_optional_contract_violation(
            format!(
                "AD scalar row span count {} does not match row count {}",
                row_spans.len(),
                primal_rows.len()
            ),
            span,
        ));
    }

    let mut rows = ad_vec_with_capacity(primal_rows.len(), context, span)?;
    for (row_index, row) in primal_rows.iter().enumerate() {
        let row_span = row_span(row_spans, row_index)?;
        rows.push(lower_row_ad_with_span(row, seed_mode, row_span)?);
    }
    Ok(rows)
}

fn row_span(
    row_spans: &[rumoca_core::Span],
    row_index: usize,
) -> Result<Option<rumoca_core::Span>, LowerError> {
    if row_spans.is_empty() {
        return Ok(None);
    }
    row_spans
        .get(row_index)
        .copied()
        .map(|span| (!span.is_dummy()).then_some(span))
        .ok_or_else(|| {
            ad_optional_contract_violation(
                format!("AD scalar row {row_index} has no source span"),
                first_non_dummy_span(row_spans),
            )
        })
}

fn first_non_dummy_span(spans: &[rumoca_core::Span]) -> Option<rumoca_core::Span> {
    for span in spans {
        if !span.is_dummy() {
            return Some(*span);
        }
    }
    None
}

fn lower_row_ad_with_span(
    primal_ops: &[LinearOp],
    seed_mode: SeedMode,
    span: Option<rumoca_core::Span>,
) -> Result<Vec<LinearOp>, LowerError> {
    let mut builder = AdBuilder::new_with_optional_span(seed_mode, span);
    for op in primal_ops {
        builder.lower_op(*op)?;
    }
    Ok(builder.ops)
}

#[derive(Clone, Copy, Default)]
enum SeedMode {
    #[default]
    SolverYOnly,
    SolverYAndP {
        p_seed_offset: usize,
    },
}

#[derive(Default)]
struct AdBuilder {
    ops: Vec<LinearOp>,
    next_reg: Reg,
    map: HashMap<Reg, DualReg>,
    cached_zero: Option<Reg>,
    cached_one: Option<Reg>,
    cached_ln10: Option<Reg>,
    cached_two: Option<Reg>,
    seed_mode: SeedMode,
    span: Option<rumoca_core::Span>,
}

impl AdBuilder {
    fn new_with_span(seed_mode: SeedMode, span: rumoca_core::Span) -> Self {
        Self::new_with_optional_span(seed_mode, Some(span))
    }

    fn new_with_optional_span(seed_mode: SeedMode, span: Option<rumoca_core::Span>) -> Self {
        Self {
            seed_mode,
            span,
            ..Self::default()
        }
    }

    fn lower_op(&mut self, op: LinearOp) -> Result<(), LowerError> {
        match op {
            LinearOp::Const { dst, value } => self.lower_const(dst, value),
            LinearOp::LoadTime { dst } => self.lower_load_time(dst),
            LinearOp::LoadY { dst, index } => self.lower_load_y(dst, index),
            LinearOp::LoadP { dst, index } => self.lower_load_p(dst, index),
            LinearOp::LoadIndexedP {
                dst,
                base,
                count,
                index,
            } => self.lower_load_indexed_p(dst, base, count, index),
            LinearOp::LoadSeed { .. } => Err(unsupported("unexpected LoadSeed in primal row")),
            LinearOp::LoadIndexedSeed { .. } => {
                Err(unsupported("unexpected LoadIndexedSeed in primal row"))
            }
            LinearOp::Move { dst, src } => self.lower_move(dst, src),
            LinearOp::LinearSolveComponent {
                dst,
                matrix_start,
                rhs_start,
                n,
                component,
            } => self.lower_linear_solve_component(dst, matrix_start, rhs_start, n, component),
            LinearOp::TableBounds { dst, table_id, max } => {
                self.lower_table_bounds(dst, table_id, max)
            }
            LinearOp::TableLookup {
                dst,
                table_id,
                column,
                input,
            } => self.lower_table_lookup(dst, table_id, column, input),
            LinearOp::TableLookupSlope { .. } => {
                Err(unsupported("unexpected TableLookupSlope in primal row"))
            }
            LinearOp::TableNextEvent {
                dst,
                table_id,
                time,
            } => self.lower_table_next_event(dst, table_id, time),
            LinearOp::RandomInitialState {
                dst,
                generator,
                local_seed,
                global_seed,
                state_len,
                state_index,
            } => self.lower_random_initial_state(
                dst,
                generator,
                local_seed,
                global_seed,
                state_len,
                state_index,
            ),
            LinearOp::RandomResult {
                dst,
                generator,
                state_start,
                state_len,
            } => self.lower_random_result(dst, generator, state_start, state_len),
            LinearOp::RandomState {
                dst,
                generator,
                state_start,
                state_len,
                state_index,
            } => self.lower_random_state(dst, generator, state_start, state_len, state_index),
            LinearOp::ImpureRandomInit { .. }
            | LinearOp::ImpureRandom { .. }
            | LinearOp::ImpureRandomInteger { .. } => {
                Err(unsupported("impure random solve-IR ops are discrete-only"))
            }
            LinearOp::Unary { dst, op, arg } => self.lower_unary(dst, op, arg),
            LinearOp::Binary { dst, op, lhs, rhs } => self.lower_binary(dst, op, lhs, rhs),
            LinearOp::Compare { dst, op, lhs, rhs } => self.lower_compare(dst, op, lhs, rhs),
            LinearOp::Select {
                dst,
                cond,
                if_true,
                if_false,
            } => self.lower_select(dst, cond, if_true, if_false),
            LinearOp::StoreOutput { src } => self.lower_store(src),
        }
    }

    fn lower_const(&mut self, dst: Reg, value: f64) -> Result<(), LowerError> {
        let re = self.emit_const(value)?;
        let du = self.zero_reg()?;
        self.bind(dst, DualReg { re, du })
    }

    fn lower_load_time(&mut self, dst: Reg) -> Result<(), LowerError> {
        let re = self.emit_load_time()?;
        let du = self.zero_reg()?;
        self.bind(dst, DualReg { re, du })
    }

    fn lower_random_initial_state(
        &mut self,
        dst: Reg,
        generator: RandomGenerator,
        local_seed: Reg,
        global_seed: Reg,
        state_len: usize,
        state_index: usize,
    ) -> Result<(), LowerError> {
        self.ensure_solver_y_random_ad()?;
        let local_seed = self.lookup(local_seed)?.re;
        let global_seed = self.lookup(global_seed)?.re;
        let re = self.alloc_reg()?;
        self.ops.push(LinearOp::RandomInitialState {
            dst: re,
            generator,
            local_seed,
            global_seed,
            state_len,
            state_index,
        });
        let du = self.zero_reg()?;
        self.bind(dst, DualReg { re, du })
    }

    fn lower_random_result(
        &mut self,
        dst: Reg,
        generator: RandomGenerator,
        state_start: Reg,
        state_len: usize,
    ) -> Result<(), LowerError> {
        self.ensure_solver_y_random_ad()?;
        let state_start = self.pack_random_primal_state(state_start, state_len)?;
        let re = self.alloc_reg()?;
        self.ops.push(LinearOp::RandomResult {
            dst: re,
            generator,
            state_start,
            state_len,
        });
        let du = self.zero_reg()?;
        self.bind(dst, DualReg { re, du })
    }

    fn lower_random_state(
        &mut self,
        dst: Reg,
        generator: RandomGenerator,
        state_start: Reg,
        state_len: usize,
        state_index: usize,
    ) -> Result<(), LowerError> {
        self.ensure_solver_y_random_ad()?;
        let state_start = self.pack_random_primal_state(state_start, state_len)?;
        let re = self.alloc_reg()?;
        self.ops.push(LinearOp::RandomState {
            dst: re,
            generator,
            state_start,
            state_len,
            state_index,
        });
        let du = self.zero_reg()?;
        self.bind(dst, DualReg { re, du })
    }

    fn ensure_solver_y_random_ad(&self) -> Result<(), LowerError> {
        match self.seed_mode {
            SeedMode::SolverYOnly => Ok(()),
            SeedMode::SolverYAndP { .. } => Err(unsupported(
                "deterministic random ops do not define parameter sensitivities",
            )),
        }
    }

    fn pack_random_primal_state(
        &mut self,
        state_start: Reg,
        state_len: usize,
    ) -> Result<Reg, LowerError> {
        let values = collect_dual_range(
            self,
            state_start,
            state_len,
            self.span,
            "random AD state value count",
            "random state range",
        )?;
        let values = real_regs_from_duals(&values, "random AD primal state count", self.span)?;
        self.pack_registers(&values)
    }

    fn lower_move(&mut self, dst: Reg, src: Reg) -> Result<(), LowerError> {
        let value = self.lookup(src)?;
        self.bind(dst, value)
    }

    fn lower_linear_solve_component(
        &mut self,
        dst: Reg,
        matrix_start: Reg,
        rhs_start: Reg,
        n: usize,
        component: usize,
    ) -> Result<(), LowerError> {
        if n == 0 || component >= n {
            return Err(unsupported("invalid LinearSolveComponent shape in AD row"));
        }

        let span = self.span;
        let matrix_len = checked_ad_product(n, n, span, "LinearSolveComponent AD matrix range")?;
        let matrix = collect_dual_range(
            self,
            matrix_start,
            matrix_len,
            span,
            "LinearSolveComponent AD matrix value count",
            "matrix range",
        )?;
        let rhs = collect_dual_range(
            self,
            rhs_start,
            n,
            span,
            "LinearSolveComponent AD RHS value count",
            "rhs range",
        )?;

        let matrix_re = real_regs_from_duals(
            &matrix,
            "LinearSolveComponent AD matrix real value count",
            span,
        )?;
        let rhs_re =
            real_regs_from_duals(&rhs, "LinearSolveComponent AD RHS real value count", span)?;
        let matrix_start_re = self.pack_registers(&matrix_re)?;
        let rhs_start_re = self.pack_registers(&rhs_re)?;

        let mut solution_regs =
            ad_vec_with_capacity(n, "LinearSolveComponent AD solution value count", span)?;
        for solution_component in 0..n {
            let solution = self.alloc_reg()?;
            self.ops.push(LinearOp::LinearSolveComponent {
                dst: solution,
                matrix_start: matrix_start_re,
                rhs_start: rhs_start_re,
                n,
                component: solution_component,
            });
            solution_regs.push(solution);
        }

        let mut tangent_rhs =
            ad_vec_with_capacity(n, "LinearSolveComponent AD tangent RHS value count", span)?;
        for row in 0..n {
            let mut acc = rhs[row].du;
            for col in 0..n {
                let matrix_du = matrix[row * n + col].du;
                let product = self.emit_binary(BinaryOp::Mul, matrix_du, solution_regs[col])?;
                acc = self.emit_binary(BinaryOp::Sub, acc, product)?;
            }
            tangent_rhs.push(acc);
        }

        let tangent_rhs_start = self.pack_registers(&tangent_rhs)?;
        let du = self.alloc_reg()?;
        self.ops.push(LinearOp::LinearSolveComponent {
            dst: du,
            matrix_start: matrix_start_re,
            rhs_start: tangent_rhs_start,
            n,
            component,
        });

        self.bind(
            dst,
            DualReg {
                re: solution_regs[component],
                du,
            },
        )
    }

    fn lower_load_y(&mut self, dst: Reg, index: usize) -> Result<(), LowerError> {
        let re = self.emit_load_y(index)?;
        let du = self.emit_load_seed(index)?;
        self.bind(dst, DualReg { re, du })
    }

    fn lower_load_p(&mut self, dst: Reg, index: usize) -> Result<(), LowerError> {
        let re = self.emit_load_p(index)?;
        let du = match self.seed_mode {
            SeedMode::SolverYOnly => self.zero_reg()?,
            SeedMode::SolverYAndP { .. } => self.emit_load_seed(self.p_seed_index(index)?)?,
        };
        self.bind(dst, DualReg { re, du })
    }

    /// Forward-mode dual of a runtime-indexed parameter load. The value is
    /// loaded at the same runtime offset; its tangent is zero under solver-y AD
    /// and, under parameter-seed AD, the seed at the matching offset shifted
    /// into the seed region (`p_seed_index` is affine, so the whole run shifts
    /// by `p_seed_offset` while the index register is reused unchanged).
    fn lower_load_indexed_p(
        &mut self,
        dst: Reg,
        base: usize,
        count: usize,
        index: Reg,
    ) -> Result<(), LowerError> {
        let idx = self.lookup(index)?;
        let re = self.emit_load_indexed_p(base, count, idx.re)?;
        let du = match self.seed_mode {
            SeedMode::SolverYOnly => self.zero_reg()?,
            SeedMode::SolverYAndP { .. } => {
                self.emit_load_indexed_seed(self.p_seed_index(base)?, count, idx.re)?
            }
        };
        self.bind(dst, DualReg { re, du })
    }

    fn lower_table_bounds(&mut self, dst: Reg, table_id: Reg, max: bool) -> Result<(), LowerError> {
        let table = self.lookup(table_id)?;
        let re = self.emit_table_bounds(table.re, max)?;
        let du = self.zero_reg()?;
        self.bind(dst, DualReg { re, du })
    }

    fn lower_table_lookup(
        &mut self,
        dst: Reg,
        table_id: Reg,
        column: Reg,
        input: Reg,
    ) -> Result<(), LowerError> {
        let table = self.lookup(table_id)?;
        let column = self.lookup(column)?;
        let input = self.lookup(input)?;
        let re = self.emit_table_lookup(table.re, column.re, input.re)?;
        let slope = self.emit_table_lookup_slope(table.re, column.re, input.re)?;
        let du = self.emit_binary(BinaryOp::Mul, slope, input.du)?;
        self.bind(dst, DualReg { re, du })
    }

    fn lower_table_next_event(
        &mut self,
        dst: Reg,
        table_id: Reg,
        time: Reg,
    ) -> Result<(), LowerError> {
        let table = self.lookup(table_id)?;
        let time = self.lookup(time)?;
        let re = self.emit_table_next_event(table.re, time.re)?;
        let du = self.zero_reg()?;
        self.bind(dst, DualReg { re, du })
    }

    fn lower_unary(&mut self, dst: Reg, op: UnaryOp, arg: Reg) -> Result<(), LowerError> {
        let x = self.lookup(arg)?;
        let out = self.unary_dual(op, x)?;
        self.bind(dst, out)
    }

    fn lower_binary(
        &mut self,
        dst: Reg,
        op: BinaryOp,
        lhs: Reg,
        rhs: Reg,
    ) -> Result<(), LowerError> {
        let l = self.lookup(lhs)?;
        let r = self.lookup(rhs)?;
        let out = self.binary_dual(op, l, r)?;
        self.bind(dst, out)
    }

    fn lower_compare(
        &mut self,
        dst: Reg,
        op: CompareOp,
        lhs: Reg,
        rhs: Reg,
    ) -> Result<(), LowerError> {
        let l = self.lookup(lhs)?;
        let r = self.lookup(rhs)?;
        let re = self.emit_compare(op, l.re, r.re)?;
        let du = self.zero_reg()?;
        self.bind(dst, DualReg { re, du })
    }

    fn lower_select(
        &mut self,
        dst: Reg,
        cond: Reg,
        if_true: Reg,
        if_false: Reg,
    ) -> Result<(), LowerError> {
        let c = self.lookup(cond)?;
        let t = self.lookup(if_true)?;
        let f = self.lookup(if_false)?;
        let re = self.emit_select(c.re, t.re, f.re)?;
        let du = self.emit_select(c.re, t.du, f.du)?;
        self.bind(dst, DualReg { re, du })
    }

    fn lower_store(&mut self, src: Reg) -> Result<(), LowerError> {
        let d = self.lookup(src)?;
        self.ops.push(LinearOp::StoreOutput { src: d.du });
        Ok(())
    }

    fn unary_dual(&mut self, op: UnaryOp, x: DualReg) -> Result<DualReg, LowerError> {
        let zero = self.zero_reg()?;
        let out = match op {
            UnaryOp::Neg => {
                let re = self.emit_unary(UnaryOp::Neg, x.re)?;
                let du = self.emit_unary(UnaryOp::Neg, x.du)?;
                DualReg { re, du }
            }
            UnaryOp::Not => {
                let re = self.emit_unary(UnaryOp::Not, x.re)?;
                DualReg { re, du: zero }
            }
            UnaryOp::Abs => {
                let re = self.emit_unary(UnaryOp::Abs, x.re)?;
                let neg_du = self.emit_unary(UnaryOp::Neg, x.du)?;
                let cond = self.emit_compare(CompareOp::Ge, x.re, zero)?;
                let du = self.emit_select(cond, x.du, neg_du)?;
                DualReg { re, du }
            }
            UnaryOp::Sign | UnaryOp::Floor | UnaryOp::Ceil | UnaryOp::Trunc => {
                let re = self.emit_unary(op, x.re)?;
                DualReg { re, du: zero }
            }
            UnaryOp::Sin => self.unary_mul_chain(UnaryOp::Sin, UnaryOp::Cos, x)?,
            UnaryOp::Cos => {
                let re = self.emit_unary(UnaryOp::Cos, x.re)?;
                let sinx = self.emit_unary(UnaryOp::Sin, x.re)?;
                let neg_sinx = self.emit_unary(UnaryOp::Neg, sinx)?;
                let du = self.emit_binary(BinaryOp::Mul, x.du, neg_sinx)?;
                DualReg { re, du }
            }
            UnaryOp::Tan => {
                let re = self.emit_unary(UnaryOp::Tan, x.re)?;
                let cosx = self.emit_unary(UnaryOp::Cos, x.re)?;
                let cos_sq = self.emit_binary(BinaryOp::Mul, cosx, cosx)?;
                let du = self.emit_binary(BinaryOp::Div, x.du, cos_sq)?;
                DualReg { re, du }
            }
            UnaryOp::Asin => self.lower_asin_or_acos(x, false)?,
            UnaryOp::Acos => self.lower_asin_or_acos(x, true)?,
            UnaryOp::Atan => {
                let re = self.emit_unary(UnaryOp::Atan, x.re)?;
                let one = self.one_reg()?;
                let x_sq = self.emit_binary(BinaryOp::Mul, x.re, x.re)?;
                let denom = self.emit_binary(BinaryOp::Add, one, x_sq)?;
                let du = self.emit_binary(BinaryOp::Div, x.du, denom)?;
                DualReg { re, du }
            }
            UnaryOp::Sinh => self.unary_mul_chain(UnaryOp::Sinh, UnaryOp::Cosh, x)?,
            UnaryOp::Cosh => self.unary_mul_chain(UnaryOp::Cosh, UnaryOp::Sinh, x)?,
            UnaryOp::Tanh => {
                let re = self.emit_unary(UnaryOp::Tanh, x.re)?;
                let cosh = self.emit_unary(UnaryOp::Cosh, x.re)?;
                let cosh_sq = self.emit_binary(BinaryOp::Mul, cosh, cosh)?;
                let du = self.emit_binary(BinaryOp::Div, x.du, cosh_sq)?;
                DualReg { re, du }
            }
            UnaryOp::Exp => {
                let re = self.emit_unary(UnaryOp::Exp, x.re)?;
                let du = self.emit_binary(BinaryOp::Mul, x.du, re)?;
                DualReg { re, du }
            }
            UnaryOp::Log => self.lower_log_like(x, false)?,
            UnaryOp::Log10 => self.lower_log_like(x, true)?,
            UnaryOp::Sqrt => self.lower_sqrt(x)?,
        };
        Ok(out)
    }

    fn unary_mul_chain(
        &mut self,
        re_op: UnaryOp,
        deriv_op: UnaryOp,
        x: DualReg,
    ) -> Result<DualReg, LowerError> {
        let re = self.emit_unary(re_op, x.re)?;
        let deriv_term = self.emit_unary(deriv_op, x.re)?;
        let du = self.emit_binary(BinaryOp::Mul, x.du, deriv_term)?;
        Ok(DualReg { re, du })
    }

    fn lower_asin_or_acos(&mut self, x: DualReg, is_acos: bool) -> Result<DualReg, LowerError> {
        let re = self.emit_unary(
            if is_acos {
                UnaryOp::Acos
            } else {
                UnaryOp::Asin
            },
            x.re,
        )?;
        let one = self.one_reg()?;
        let x_sq = self.emit_binary(BinaryOp::Mul, x.re, x.re)?;
        let denom_sq = self.emit_binary(BinaryOp::Sub, one, x_sq)?;
        let denom = self.emit_unary(UnaryOp::Sqrt, denom_sq)?;
        let safe = self.emit_binary(BinaryOp::Div, x.du, denom)?;
        let signed = if is_acos {
            self.emit_unary(UnaryOp::Neg, safe)?
        } else {
            safe
        };
        let zero = self.zero_reg()?;
        let du_zero = self.emit_compare(CompareOp::Eq, x.du, zero)?;
        let du = self.emit_select(du_zero, zero, signed)?;
        Ok(DualReg { re, du })
    }

    fn lower_log_like(&mut self, x: DualReg, is_log10: bool) -> Result<DualReg, LowerError> {
        let op = if is_log10 {
            UnaryOp::Log10
        } else {
            UnaryOp::Log
        };
        let re = self.emit_unary(op, x.re)?;
        let zero = self.zero_reg()?;
        let nonzero = self.emit_compare(CompareOp::Ne, x.re, zero)?;
        let denom = if is_log10 {
            let ln10 = self.ln10_reg()?;
            self.emit_binary(BinaryOp::Mul, x.re, ln10)?
        } else {
            x.re
        };
        let safe = self.emit_binary(BinaryOp::Div, x.du, denom)?;
        let du = self.emit_select(nonzero, safe, zero)?;
        Ok(DualReg { re, du })
    }

    fn lower_sqrt(&mut self, x: DualReg) -> Result<DualReg, LowerError> {
        let re = self.emit_unary(UnaryOp::Sqrt, x.re)?;
        let zero = self.zero_reg()?;
        let nonzero = self.emit_compare(CompareOp::Ne, x.re, zero)?;
        let two = self.two_reg()?;
        let denom = self.emit_binary(BinaryOp::Mul, two, re)?;
        let safe = self.emit_binary(BinaryOp::Div, x.du, denom)?;
        let du = self.emit_select(nonzero, safe, zero)?;
        Ok(DualReg { re, du })
    }

    fn binary_dual(
        &mut self,
        op: BinaryOp,
        lhs: DualReg,
        rhs: DualReg,
    ) -> Result<DualReg, LowerError> {
        let out = match op {
            BinaryOp::Add => self.binary_add(lhs, rhs)?,
            BinaryOp::Sub => self.binary_sub(lhs, rhs)?,
            BinaryOp::Mul => self.binary_mul(lhs, rhs)?,
            BinaryOp::Div => self.binary_div(lhs, rhs)?,
            BinaryOp::Pow => self.binary_pow(lhs, rhs)?,
            BinaryOp::And | BinaryOp::Or => self.binary_bool(op, lhs, rhs)?,
            BinaryOp::Atan2 => self.binary_atan2(lhs, rhs)?,
            BinaryOp::Min => self.binary_minmax(lhs, rhs, false)?,
            BinaryOp::Max => self.binary_minmax(lhs, rhs, true)?,
        };
        Ok(out)
    }

    fn binary_add(&mut self, lhs: DualReg, rhs: DualReg) -> Result<DualReg, LowerError> {
        let re = self.emit_binary(BinaryOp::Add, lhs.re, rhs.re)?;
        let du = self.emit_binary(BinaryOp::Add, lhs.du, rhs.du)?;
        Ok(DualReg { re, du })
    }

    fn binary_sub(&mut self, lhs: DualReg, rhs: DualReg) -> Result<DualReg, LowerError> {
        let re = self.emit_binary(BinaryOp::Sub, lhs.re, rhs.re)?;
        let du = self.emit_binary(BinaryOp::Sub, lhs.du, rhs.du)?;
        Ok(DualReg { re, du })
    }

    fn binary_mul(&mut self, lhs: DualReg, rhs: DualReg) -> Result<DualReg, LowerError> {
        let re = self.emit_binary(BinaryOp::Mul, lhs.re, rhs.re)?;
        let term1 = self.emit_binary(BinaryOp::Mul, lhs.du, rhs.re)?;
        let term2 = self.emit_binary(BinaryOp::Mul, lhs.re, rhs.du)?;
        let du = self.emit_binary(BinaryOp::Add, term1, term2)?;
        Ok(DualReg { re, du })
    }

    fn binary_div(&mut self, lhs: DualReg, rhs: DualReg) -> Result<DualReg, LowerError> {
        let zero = self.zero_reg()?;
        let denom_zero = self.emit_compare(CompareOp::Eq, rhs.re, zero)?;
        let numer_zero = self.emit_compare(CompareOp::Eq, lhs.re, zero)?;

        let safe_re = self.emit_binary(BinaryOp::Div, lhs.re, rhs.re)?;
        let denom_zero_re = self.emit_select(numer_zero, zero, safe_re)?;
        let re = self.emit_select(denom_zero, denom_zero_re, safe_re)?;

        let term1 = self.emit_binary(BinaryOp::Mul, lhs.du, rhs.re)?;
        let term2 = self.emit_binary(BinaryOp::Mul, lhs.re, rhs.du)?;
        let numer_du = self.emit_binary(BinaryOp::Sub, term1, term2)?;
        let rhs_sq = self.emit_binary(BinaryOp::Mul, rhs.re, rhs.re)?;
        let safe_du = self.emit_binary(BinaryOp::Div, numer_du, rhs_sq)?;
        let du = self.emit_select(denom_zero, zero, safe_du)?;

        Ok(DualReg { re, du })
    }

    fn binary_pow(&mut self, lhs: DualReg, rhs: DualReg) -> Result<DualReg, LowerError> {
        let re = self.emit_binary(BinaryOp::Pow, lhs.re, rhs.re)?;
        let du = self.lower_pow_du(lhs, rhs, re)?;
        Ok(DualReg { re, du })
    }

    fn lower_pow_du(&mut self, lhs: DualReg, rhs: DualReg, re: Reg) -> Result<Reg, LowerError> {
        let zero = self.zero_reg()?;
        let one = self.one_reg()?;
        let rhs_du_zero = self.emit_compare(CompareOp::Eq, rhs.du, zero)?;

        let lhs_re_zero = self.emit_compare(CompareOp::Eq, lhs.re, zero)?;
        let rhs_re_one = self.emit_compare(CompareOp::Eq, rhs.re, one)?;
        let rhs_minus_one = self.emit_binary(BinaryOp::Sub, rhs.re, one)?;
        let x_pow_n_minus_1 = self.emit_binary(BinaryOp::Pow, lhs.re, rhs_minus_one)?;
        let n_times = self.emit_binary(BinaryOp::Mul, rhs.re, x_pow_n_minus_1)?;
        let const_exp_safe = self.emit_binary(BinaryOp::Mul, n_times, lhs.du)?;
        let lhs_zero_branch = self.emit_select(rhs_re_one, lhs.du, zero)?;
        let const_exp_du = self.emit_select(lhs_re_zero, lhs_zero_branch, const_exp_safe)?;

        let lhs_positive = self.emit_compare(CompareOp::Gt, lhs.re, zero)?;
        let ln_x = self.emit_unary(UnaryOp::Log, lhs.re)?;
        let term1 = self.emit_binary(BinaryOp::Mul, rhs.du, ln_x)?;
        let xprime_over_x = self.emit_binary(BinaryOp::Div, lhs.du, lhs.re)?;
        let term2 = self.emit_binary(BinaryOp::Mul, rhs.re, xprime_over_x)?;
        let sum = self.emit_binary(BinaryOp::Add, term1, term2)?;
        let var_exp_safe = self.emit_binary(BinaryOp::Mul, re, sum)?;
        let var_exp_du = self.emit_select(lhs_positive, var_exp_safe, zero)?;

        self.emit_select(rhs_du_zero, const_exp_du, var_exp_du)
    }

    fn binary_bool(
        &mut self,
        op: BinaryOp,
        lhs: DualReg,
        rhs: DualReg,
    ) -> Result<DualReg, LowerError> {
        let re = self.emit_binary(op, lhs.re, rhs.re)?;
        let du = self.zero_reg()?;
        Ok(DualReg { re, du })
    }

    fn binary_atan2(&mut self, lhs: DualReg, rhs: DualReg) -> Result<DualReg, LowerError> {
        let re = self.emit_binary(BinaryOp::Atan2, lhs.re, rhs.re)?;
        let term1 = self.emit_binary(BinaryOp::Mul, lhs.du, rhs.re)?;
        let term2 = self.emit_binary(BinaryOp::Mul, lhs.re, rhs.du)?;
        let numer = self.emit_binary(BinaryOp::Sub, term1, term2)?;
        let lhs_sq = self.emit_binary(BinaryOp::Mul, lhs.re, lhs.re)?;
        let rhs_sq = self.emit_binary(BinaryOp::Mul, rhs.re, rhs.re)?;
        let denom = self.emit_binary(BinaryOp::Add, lhs_sq, rhs_sq)?;
        let du = self.emit_binary(BinaryOp::Div, numer, denom)?;
        Ok(DualReg { re, du })
    }

    fn binary_minmax(
        &mut self,
        lhs: DualReg,
        rhs: DualReg,
        is_max: bool,
    ) -> Result<DualReg, LowerError> {
        let cmp = if is_max { CompareOp::Ge } else { CompareOp::Le };
        let cond = self.emit_compare(cmp, lhs.re, rhs.re)?;
        let re = self.emit_select(cond, lhs.re, rhs.re)?;
        let du = self.emit_select(cond, lhs.du, rhs.du)?;
        Ok(DualReg { re, du })
    }

    fn bind(&mut self, src: Reg, dual: DualReg) -> Result<(), LowerError> {
        if self.map.insert(src, dual).is_some() {
            return Err(unsupported("duplicate destination register in primal row"));
        }
        Ok(())
    }

    fn lookup(&self, reg: Reg) -> Result<DualReg, LowerError> {
        self.map
            .get(&reg)
            .copied()
            .ok_or_else(|| unsupported("missing source register in primal row"))
    }

    fn alloc_reg(&mut self) -> Result<Reg, LowerError> {
        let reg = self.next_reg;
        self.next_reg = self.next_reg.checked_add(1).ok_or_else(|| {
            ad_optional_contract_violation(
                format!("AD register allocation overflow after r{reg}"),
                self.span,
            )
        })?;
        Ok(reg)
    }

    fn emit_const(&mut self, value: f64) -> Result<Reg, LowerError> {
        let dst = self.alloc_reg()?;
        self.ops.push(LinearOp::Const { dst, value });
        Ok(dst)
    }

    fn pack_registers(&mut self, regs: &[Reg]) -> Result<Reg, LowerError> {
        let start = self.next_reg;
        for &src in regs {
            let dst = self.alloc_reg()?;
            self.ops.push(LinearOp::Move { dst, src });
        }
        Ok(start)
    }

    fn emit_load_time(&mut self) -> Result<Reg, LowerError> {
        let dst = self.alloc_reg()?;
        self.ops.push(LinearOp::LoadTime { dst });
        Ok(dst)
    }

    fn emit_load_y(&mut self, index: usize) -> Result<Reg, LowerError> {
        let dst = self.alloc_reg()?;
        self.ops.push(LinearOp::LoadY { dst, index });
        Ok(dst)
    }

    fn emit_load_p(&mut self, index: usize) -> Result<Reg, LowerError> {
        let dst = self.alloc_reg()?;
        self.ops.push(LinearOp::LoadP { dst, index });
        Ok(dst)
    }

    fn emit_load_seed(&mut self, index: usize) -> Result<Reg, LowerError> {
        let dst = self.alloc_reg()?;
        self.ops.push(LinearOp::LoadSeed { dst, index });
        Ok(dst)
    }

    fn emit_load_indexed_p(
        &mut self,
        base: usize,
        count: usize,
        index: Reg,
    ) -> Result<Reg, LowerError> {
        let dst = self.alloc_reg()?;
        self.ops.push(LinearOp::LoadIndexedP {
            dst,
            base,
            count,
            index,
        });
        Ok(dst)
    }

    fn emit_load_indexed_seed(
        &mut self,
        base: usize,
        count: usize,
        index: Reg,
    ) -> Result<Reg, LowerError> {
        let dst = self.alloc_reg()?;
        self.ops.push(LinearOp::LoadIndexedSeed {
            dst,
            base,
            count,
            index,
        });
        Ok(dst)
    }

    fn p_seed_index(&self, index: usize) -> Result<usize, LowerError> {
        match self.seed_mode {
            SeedMode::SolverYOnly => Ok(index),
            SeedMode::SolverYAndP { p_seed_offset } => {
                p_seed_offset.checked_add(index).ok_or_else(|| {
                    ad_optional_contract_violation(
                        format!(
                        "parameter seed index offset {p_seed_offset} plus index {index} overflows"
                    ),
                        self.span,
                    )
                })
            }
        }
    }

    fn emit_table_bounds(&mut self, table_id: Reg, max: bool) -> Result<Reg, LowerError> {
        let dst = self.alloc_reg()?;
        self.ops.push(LinearOp::TableBounds { dst, table_id, max });
        Ok(dst)
    }

    fn emit_table_lookup(
        &mut self,
        table_id: Reg,
        column: Reg,
        input: Reg,
    ) -> Result<Reg, LowerError> {
        let dst = self.alloc_reg()?;
        self.ops.push(LinearOp::TableLookup {
            dst,
            table_id,
            column,
            input,
        });
        Ok(dst)
    }

    fn emit_table_lookup_slope(
        &mut self,
        table_id: Reg,
        column: Reg,
        input: Reg,
    ) -> Result<Reg, LowerError> {
        let dst = self.alloc_reg()?;
        self.ops.push(LinearOp::TableLookupSlope {
            dst,
            table_id,
            column,
            input,
        });
        Ok(dst)
    }

    fn emit_table_next_event(&mut self, table_id: Reg, time: Reg) -> Result<Reg, LowerError> {
        let dst = self.alloc_reg()?;
        self.ops.push(LinearOp::TableNextEvent {
            dst,
            table_id,
            time,
        });
        Ok(dst)
    }

    fn emit_unary(&mut self, op: UnaryOp, arg: Reg) -> Result<Reg, LowerError> {
        let dst = self.alloc_reg()?;
        self.ops.push(LinearOp::Unary { dst, op, arg });
        Ok(dst)
    }

    fn emit_binary(&mut self, op: BinaryOp, lhs: Reg, rhs: Reg) -> Result<Reg, LowerError> {
        let dst = self.alloc_reg()?;
        self.ops.push(LinearOp::Binary { dst, op, lhs, rhs });
        Ok(dst)
    }

    fn emit_compare(&mut self, op: CompareOp, lhs: Reg, rhs: Reg) -> Result<Reg, LowerError> {
        let dst = self.alloc_reg()?;
        self.ops.push(LinearOp::Compare { dst, op, lhs, rhs });
        Ok(dst)
    }

    fn emit_select(&mut self, cond: Reg, if_true: Reg, if_false: Reg) -> Result<Reg, LowerError> {
        let dst = self.alloc_reg()?;
        self.ops.push(LinearOp::Select {
            dst,
            cond,
            if_true,
            if_false,
        });
        Ok(dst)
    }

    fn zero_reg(&mut self) -> Result<Reg, LowerError> {
        if let Some(reg) = self.cached_zero {
            return Ok(reg);
        }
        let reg = self.emit_const(0.0)?;
        self.cached_zero = Some(reg);
        Ok(reg)
    }

    fn one_reg(&mut self) -> Result<Reg, LowerError> {
        if let Some(reg) = self.cached_one {
            return Ok(reg);
        }
        let reg = self.emit_const(1.0)?;
        self.cached_one = Some(reg);
        Ok(reg)
    }

    fn ln10_reg(&mut self) -> Result<Reg, LowerError> {
        if let Some(reg) = self.cached_ln10 {
            return Ok(reg);
        }
        let reg = self.emit_const(std::f64::consts::LN_10)?;
        self.cached_ln10 = Some(reg);
        Ok(reg)
    }

    fn two_reg(&mut self) -> Result<Reg, LowerError> {
        if let Some(reg) = self.cached_two {
            return Ok(reg);
        }
        let reg = self.emit_const(2.0)?;
        self.cached_two = Some(reg);
        Ok(reg)
    }
}

fn unsupported(reason: &str) -> LowerError {
    LowerError::Unsupported {
        reason: reason.to_string(),
    }
}

fn ad_contract_violation(reason: String, span: rumoca_core::Span) -> LowerError {
    ad_optional_contract_violation(reason, Some(span))
}

fn ad_optional_contract_violation(reason: String, span: Option<rumoca_core::Span>) -> LowerError {
    match span.filter(|span| !span.is_dummy()) {
        Some(span) => LowerError::ContractViolation { reason, span },
        None => LowerError::UnspannedContractViolation { reason },
    }
}

fn checked_ad_product(
    lhs: usize,
    rhs: usize,
    span: impl Into<Option<rumoca_core::Span>>,
    context: &'static str,
) -> Result<usize, LowerError> {
    let span = span.into();
    lhs.checked_mul(rhs).ok_or_else(|| {
        ad_optional_contract_violation(format!("{context} overflow for {lhs} * {rhs}"), span)
    })
}

fn reserve_ad_capacity<T>(
    values: &mut Vec<T>,
    capacity: usize,
    context: &'static str,
    span: impl Into<Option<rumoca_core::Span>>,
) -> Result<(), LowerError> {
    let span = span.into();
    values.try_reserve_exact(capacity).map_err(|_| {
        ad_optional_contract_violation(
            format!("{context} capacity exceeds host memory limits"),
            span,
        )
    })
}

fn ad_vec_with_capacity<T>(
    capacity: usize,
    context: &'static str,
    span: impl Into<Option<rumoca_core::Span>>,
) -> Result<Vec<T>, LowerError> {
    let mut values = Vec::new();
    reserve_ad_capacity(&mut values, capacity, context, span)?;
    Ok(values)
}

fn collect_dual_range(
    builder: &AdBuilder,
    start: Reg,
    len: usize,
    span: impl Into<Option<rumoca_core::Span>>,
    capacity_context: &'static str,
    offset_context: &'static str,
) -> Result<Vec<DualReg>, LowerError> {
    let span = span.into();
    let mut values = ad_vec_with_capacity(len, capacity_context, span)?;
    for idx in 0..len {
        values.push(builder.lookup(checked_ad_reg_offset(start, idx, span, offset_context)?)?);
    }
    Ok(values)
}

fn real_regs_from_duals(
    duals: &[DualReg],
    context: &'static str,
    span: impl Into<Option<rumoca_core::Span>>,
) -> Result<Vec<Reg>, LowerError> {
    let span = span.into();
    let mut regs = ad_vec_with_capacity(duals.len(), context, span)?;
    for dual in duals {
        regs.push(dual.re);
    }
    Ok(regs)
}

fn checked_ad_reg_offset(
    start: Reg,
    offset: usize,
    span: impl Into<Option<rumoca_core::Span>>,
    context: &'static str,
) -> Result<Reg, LowerError> {
    let span = span.into();
    let offset = Reg::try_from(offset).map_err(|_| {
        ad_optional_contract_violation(
            format!("{context} offset {offset} exceeds register index type"),
            span,
        )
    })?;
    start.checked_add(offset).ok_or_else(|| {
        ad_optional_contract_violation(
            format!("{context} starting at {start} overflows at offset {offset}"),
            span,
        )
    })
}

#[cfg(test)]
mod tests;
