//! AD lowering from primal linear ops to forward-mode J·v ops.
//!
//! Where an operation is not differentiable, the emitted tangent follows the
//! kink rules stated once in `crate::reverse`, so a forward product
//! certifies a matrix assembled from reverse rows; `kink_rule_tests` pins the
//! two together with the typed directional owners.
//!
mod calls;
#[cfg(test)]
mod discrete_call_tangent_tests;
mod dispatch;
mod error;
#[cfg(test)]
mod inactive_tangent_tests;
mod inactive_tangents;
#[cfg(test)]
mod kink_rule_tests;
mod loads;
mod registers;
mod rules;
mod seed_domain;
#[cfg(test)]
mod seed_domain_tests;
#[cfg(test)]
mod structured_lowering_tests;
mod tensor_ops;
mod tensor_packing;
#[cfg(test)]
mod variability_tests;
pub(crate) use seed_domain::lower_projection_domain;

pub use error::SolveDerivationError;
use rumoca_ir_solve::{
    AffineStencilConstStride, AffineStencilLoadStride, BinaryOp, CompareOp, ComputeBlock,
    ComputeNode, FoldTensorUpdateStore, LinearOp, MatrixProductShape, RandomGenerator, Reg,
    ScalarProgramBlock, StridedOperand, UnaryOp, VarLayout,
};
use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;
use std::sync::Arc;

#[derive(Debug, Clone, Copy)]
struct DualReg {
    re: Reg,
    du: Reg,
}

type FoldAdCache = Rc<RefCell<HashMap<usize, Arc<rumoca_ir_solve::FunctionFoldProgram>>>>;
type ConditionalAdCache =
    Rc<RefCell<HashMap<usize, Arc<rumoca_ir_solve::FunctionConditionalProgram>>>>;

/// Compute the forward-mode JVP of a `ComputeBlock`, preserving tensor structure.
///
/// For `ScalarPrograms` nodes: applies the existing scalar AD pass row-by-row.
/// For `MatMul` nodes: preserves tensor structure and uses a block-product form
/// for the product rule when both operands depend on solver-y.
/// For `LinSolve` nodes: emits another `LinSolve` using
/// `dx = A^{-1}(db - dA * x)` so coupled systems keep tensor solve structure.
/// For `Map` and `AffineStencil` nodes: transforms the compact base program once
/// and remaps its affine load/constant strides to the generated dual program.
pub fn lower_compute_block_jvp(block: &ComputeBlock) -> Result<ComputeBlock, SolveDerivationError> {
    lower_compute_block_jvp_with_seed_mode(block, SeedMode::SolverYOnly)
}

/// Compute a JVP whose seed vector contains solver Y followed by P.
///
/// Initialization uses this form because fixed=false parameters are genuine
/// initialization unknowns and must contribute Jacobian columns.
pub fn lower_compute_block_full_jvp(
    block: &ComputeBlock,
    p_seed_offset: usize,
) -> Result<ComputeBlock, SolveDerivationError> {
    lower_compute_block_jvp_with_seed_mode(block, SeedMode::SolverYAndP { p_seed_offset })
}

fn lower_compute_block_jvp_with_seed_mode(
    block: &ComputeBlock,
    seed_mode: SeedMode<'_>,
) -> Result<ComputeBlock, SolveDerivationError> {
    let span = compute_block_context_span(block);
    let mut nodes = ad_vec_with_capacity(block.nodes.len(), "compute block JVP node count", span)?;
    for node in &block.nodes {
        nodes.push(lower_compute_node_jvp(node, seed_mode)?);
    }
    Ok(ComputeBlock { nodes })
}

fn lower_scalar_programs_jvp(
    rows: &ScalarProgramBlock,
    seed_mode: SeedMode<'_>,
) -> Result<ComputeNode, SolveDerivationError> {
    let jvp_rows = lower_scalar_program_rows_ad(
        rows.programs(),
        rows.program_spans(),
        seed_mode,
        "spanned compute block AD row count",
    )?;
    Ok(ComputeNode::ScalarPrograms(
        ScalarProgramBlock::with_output_indices(
            jvp_rows,
            rows.program_spans().to_vec(),
            rows.output_indices().to_vec(),
        )?,
    ))
}

fn lower_compute_node_jvp(
    node: &ComputeNode,
    seed_mode: SeedMode<'_>,
) -> Result<ComputeNode, SolveDerivationError> {
    match node {
        ComputeNode::ScalarPrograms(rows) => lower_scalar_programs_jvp(rows, seed_mode),
        ComputeNode::MatMul {
            lhs_ops,
            lhs_start,
            rhs_ops,
            rhs_start,
            m,
            k,
            n,
            lhs_pattern,
            rhs_pattern,
            metadata,
            span,
        } => lower_matmul_jvp_node(
            MatMulJvpInput {
                lhs: (lhs_ops, *lhs_start),
                rhs: (rhs_ops, *rhs_start),
                shape: (*m, *k, *n),
                patterns: (lhs_pattern, rhs_pattern),
                metadata,
                span: *span,
            },
            seed_mode,
        ),
        ComputeNode::LinSolve {
            setup_ops,
            matrix_start,
            rhs_start,
            n,
            matrix_pattern,
            metadata,
            span,
            ..
        } => lower_linsolve_jvp_node(LinSolveJvpInput {
            setup_ops,
            matrix_start: *matrix_start,
            rhs_start: *rhs_start,
            n: *n,
            matrix_pattern: matrix_pattern.clone(),
            metadata: metadata.clone(),
            span: *span,
            seed_mode,
        }),
        ComputeNode::Map {
            domain,
            output_map,
            base_ops,
            load_strides,
            const_strides,
            metadata,
            span,
        } => {
            let lowered =
                lower_affine_jvp_ops(base_ops, load_strides, const_strides, *span, seed_mode)?;
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
            let lowered =
                lower_affine_jvp_ops(base_ops, load_strides, const_strides, *span, seed_mode)?;
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
    seed_mode: SeedMode<'_>,
) -> Result<AffineJvpOps, SolveDerivationError> {
    validate_affine_jvp_stride_targets(base_ops, load_strides, const_strides, span)?;
    let mut builder = AdBuilder::new_with_span(seed_mode, span);
    let mut lowered_load_strides = Vec::new();
    let mut lowered_const_strides = Vec::new();

    for (op_position, op) in base_ops.iter().cloned().enumerate() {
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
) -> Result<(), SolveDerivationError> {
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

struct MatMulJvpInput<'node> {
    lhs: (&'node [LinearOp], Reg),
    rhs: (&'node [LinearOp], Reg),
    shape: (usize, usize, usize),
    patterns: (
        &'node rumoca_ir_solve::StructuralPattern,
        &'node rumoca_ir_solve::StructuralPattern,
    ),
    metadata: &'node rumoca_ir_solve::TensorNodeMetadata,
    span: rumoca_core::Span,
}

fn lower_matmul_jvp_node(
    input: MatMulJvpInput<'_>,
    seed_mode: SeedMode<'_>,
) -> Result<ComputeNode, SolveDerivationError> {
    let MatMulJvpInput {
        lhs: (lhs_ops, lhs_start),
        rhs: (rhs_ops, rhs_start),
        shape: (m, k, n),
        patterns: (lhs_pattern, rhs_pattern),
        metadata,
        span,
    } = input;

    let lhs_len = checked_ad_product(m, k, span, "MatMul JVP lhs value count")?;
    let rhs_len = checked_ad_product(k, n, span, "MatMul JVP rhs value count")?;
    let lhs_depends_on_seed = ops_reference_seeded_inputs(lhs_ops, seed_mode);
    let rhs_depends_on_seed = ops_reference_seeded_inputs(rhs_ops, seed_mode);

    let (mut lhs_builder, lhs) =
        lower_tensor_operand(lhs_ops, lhs_start, lhs_len, 0, span, seed_mode)?;
    let (jvp_lhs_start, jvp_k, jvp_lhs_pattern) = if lhs_depends_on_seed && rhs_depends_on_seed {
        let regs = block_product_lhs_regs(&lhs, m, k, span)?;
        let block_k = checked_ad_product(2, k, span, "MatMul JVP block inner dimension")?;
        (
            lhs_builder.pack_registers(&regs)?,
            block_k,
            full_tensor_pattern(m, block_k, span)?,
        )
    } else {
        let regs = if lhs_depends_on_seed {
            dual_regs(&lhs, DualPart::Tangent, "MatMul JVP lhs tangent", span)?
        } else {
            dual_regs(&lhs, DualPart::Primal, "MatMul JVP lhs primal", span)?
        };
        (lhs_builder.pack_registers(&regs)?, k, lhs_pattern.clone())
    };

    let rhs_next_reg = lhs_builder.next_reg;
    let (mut rhs_builder, rhs) =
        lower_tensor_operand(rhs_ops, rhs_start, rhs_len, rhs_next_reg, span, seed_mode)?;
    let (jvp_rhs_start, jvp_rhs_pattern) = if lhs_depends_on_seed && rhs_depends_on_seed {
        let regs = block_product_rhs_regs(&rhs, span)?;
        (
            rhs_builder.pack_registers(&regs)?,
            full_tensor_pattern(jvp_k, n, span)?,
        )
    } else {
        let regs = if rhs_depends_on_seed || !lhs_depends_on_seed {
            dual_regs(&rhs, DualPart::Tangent, "MatMul JVP rhs tangent", span)?
        } else {
            dual_regs(&rhs, DualPart::Primal, "MatMul JVP rhs primal", span)?
        };
        (rhs_builder.pack_registers(&regs)?, rhs_pattern.clone())
    };

    Ok(ComputeNode::MatMul {
        lhs_ops: lhs_builder.ops,
        lhs_start: jvp_lhs_start,
        rhs_ops: rhs_builder.ops,
        rhs_start: jvp_rhs_start,
        m,
        k: jvp_k,
        n,
        lhs_pattern: jvp_lhs_pattern,
        rhs_pattern: jvp_rhs_pattern,
        metadata: metadata.clone(),
        span,
    })
}

fn full_tensor_pattern(
    rows: usize,
    columns: usize,
    span: rumoca_core::Span,
) -> Result<rumoca_ir_solve::StructuralPattern, SolveDerivationError> {
    let provenance = rumoca_ir_solve::PatternProvenance::derived(
        rumoca_ir_solve::PatternDerivation::DependencyPropagation,
        span,
    )
    .map_err(|error| ad_contract_violation(error.to_string(), span))?;
    rumoca_ir_solve::StructuralPattern::full(rows, columns, provenance)
        .map_err(|error| ad_contract_violation(error.to_string(), span))
}

fn lower_tensor_operand<'seed>(
    ops: &[LinearOp],
    value_start: Reg,
    value_count: usize,
    next_reg: Reg,
    span: rumoca_core::Span,
    seed_mode: SeedMode<'seed>,
) -> Result<(AdBuilder<'seed>, Vec<DualReg>), SolveDerivationError> {
    let mut builder = AdBuilder::new_with_span(seed_mode, span);
    builder.next_reg = next_reg;
    for op in ops {
        builder.lower_op(op.clone())?;
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
) -> Result<Vec<Reg>, SolveDerivationError> {
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
) -> Result<Vec<Reg>, SolveDerivationError> {
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
) -> Result<Vec<Reg>, SolveDerivationError> {
    let count = checked_ad_product(rhs.len(), 2, span, "MatMul JVP block rhs value count")?;
    let mut regs = ad_vec_with_capacity(count, "MatMul JVP block rhs value count", span)?;
    regs.extend(rhs.iter().map(|value| value.re));
    regs.extend(rhs.iter().map(|value| value.du));
    Ok(regs)
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
        ComputeNode::ScalarPrograms(block) => block.program_span(0),
        ComputeNode::MatMul { span, .. }
        | ComputeNode::LinSolve { span, .. }
        | ComputeNode::Map { span, .. }
        | ComputeNode::AffineStencil { span, .. } => (!span.is_dummy()).then_some(*span),
    }
}

fn ops_reference_seeded_inputs(ops: &[LinearOp], seed_mode: SeedMode<'_>) -> bool {
    ops.iter().any(|op| {
        matches!(op, LinearOp::LoadY { .. })
            || matches!(seed_mode, SeedMode::SolverYAndP { .. })
                && matches!(op, LinearOp::LoadP { .. } | LinearOp::LoadIndexedP { .. })
            || matches!(op, LinearOp::FunctionFold { program, .. }
                | LinearOp::GuardedFunctionFold { program, .. }
                if program.regions().any(|region| ops_reference_seeded_inputs(region, seed_mode)))
            || matches!(op, LinearOp::FunctionConditional { program, .. }
                if program
                    .arms
                    .iter()
                    .flat_map(|arm| [&arm.condition, &arm.result])
                    .chain(std::iter::once(&program.fallback))
                    .any(|region| ops_reference_seeded_inputs(region, seed_mode)))
    })
}

struct LinSolveJvpInput<'ops> {
    setup_ops: &'ops [LinearOp],
    matrix_start: Reg,
    rhs_start: Reg,
    n: usize,
    matrix_pattern: rumoca_ir_solve::StructuralPattern,
    metadata: rumoca_ir_solve::TensorNodeMetadata,
    span: rumoca_core::Span,
    seed_mode: SeedMode<'ops>,
}

fn lower_linsolve_jvp_node(
    input: LinSolveJvpInput<'_>,
) -> Result<ComputeNode, SolveDerivationError> {
    let LinSolveJvpInput {
        setup_ops,
        matrix_start,
        rhs_start,
        n,
        matrix_pattern,
        metadata,
        span,
        seed_mode,
    } = input;
    if n == 0 {
        return Err(unsupported("invalid zero-sized LinSolve in JVP"));
    }

    let mut builder = AdBuilder::new_with_span(seed_mode, span);
    for op in setup_ops {
        builder.lower_op(op.clone())?;
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
        matrix_pattern,
        metadata,
        span,
    })
}

pub fn lower_scalar_program_block_ad(
    primal_rows: &[Vec<LinearOp>],
) -> Result<Vec<Vec<LinearOp>>, SolveDerivationError> {
    lower_scalar_program_rows_ad(
        primal_rows,
        &[],
        SeedMode::SolverYOnly,
        "scalar program AD row count",
    )
}

/// The forward-mode JVP of `block` over `[solver-y | parameter]` seeds whose
/// parameter seeds start at `p_seed_offset`, row-aligned with `block`.
pub fn lower_scalar_program_block_full_jvp(
    block: &rumoca_ir_solve::ScalarProgramBlock,
    p_seed_offset: usize,
) -> Result<rumoca_ir_solve::ScalarProgramBlock, SolveDerivationError> {
    let rows = lower_scalar_program_rows_ad(
        block.programs(),
        block.program_spans(),
        SeedMode::SolverYAndP { p_seed_offset },
        "full scalar program JVP row count",
    )?;
    rumoca_ir_solve::ScalarProgramBlock::with_output_indices(
        rows,
        block.program_spans().to_vec(),
        block.output_indices().to_vec(),
    )
    .map_err(SolveDerivationError::from)
}

pub fn lower_scalar_program_block_full_ad_with_spans(
    primal_rows: &[Vec<LinearOp>],
    row_spans: &[rumoca_core::Span],
    layout: &VarLayout,
) -> Result<Vec<Vec<LinearOp>>, SolveDerivationError> {
    lower_scalar_program_rows_ad(
        primal_rows,
        row_spans,
        SeedMode::SolverYAndP {
            p_seed_offset: layout.y_scalars(),
        },
        "full scalar program AD row count",
    )
}

fn lower_scalar_program_rows_ad(
    primal_rows: &[Vec<LinearOp>],
    row_spans: &[rumoca_core::Span],
    seed_mode: SeedMode<'_>,
    context: &'static str,
) -> Result<Vec<Vec<LinearOp>>, SolveDerivationError> {
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
    // A fold body is an aggregate program owner. Preserve that ownership in
    // the derived JVP: every reference to the same primal body must reference
    // the same derived body instead of recursively rebuilding an expression
    // tree at each call site.
    let fold_program_cache = Rc::new(RefCell::new(HashMap::new()));
    for (row_index, row) in primal_rows.iter().enumerate() {
        let row_span = row_span(row_spans, row_index)?;
        rows.push(lower_row_ad_with_span(
            row,
            seed_mode,
            row_span,
            fold_program_cache.clone(),
        )?);
    }
    Ok(rows)
}

fn row_span(
    row_spans: &[rumoca_core::Span],
    row_index: usize,
) -> Result<Option<rumoca_core::Span>, SolveDerivationError> {
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
    seed_mode: SeedMode<'_>,
    span: Option<rumoca_core::Span>,
    fold_program_cache: FoldAdCache,
) -> Result<Vec<LinearOp>, SolveDerivationError> {
    let mut builder =
        AdBuilder::new_with_optional_span_and_fold_cache(seed_mode, span, fold_program_cache);
    for op in primal_ops {
        builder.lower_op(op.clone())?;
    }
    Ok(builder.ops)
}

#[derive(Clone, Copy, Default)]
enum SeedMode<'a> {
    #[default]
    SolverYOnly,
    SolverYSubset {
        active: &'a [usize],
    },
    SolverYAndP {
        p_seed_offset: usize,
    },
}

#[derive(Clone, Copy, Debug, Default)]
enum StoreOutputMode {
    #[default]
    Derivative,
    Primal,
    Dual,
}

/// The dual pair of each primal register, indexed by register. Primal rows
/// number their registers densely from zero, so a slot per register replaces
/// hashing every bind and lookup.
#[derive(Debug, Default)]
struct DualMap(Vec<Option<DualReg>>);

impl DualMap {
    /// Record `dual` for `src`; `false` when `src` was already bound.
    fn bind(&mut self, src: Reg, dual: DualReg) -> bool {
        let index = src as usize;
        if index >= self.0.len() {
            self.0.resize(index + 1, None);
        }
        self.0[index].replace(dual).is_none()
    }

    fn get(&self, src: Reg) -> Option<DualReg> {
        self.0.get(src as usize).copied().flatten()
    }
}

struct AdBuilder<'a> {
    ops: Vec<LinearOp>,
    next_reg: Reg,
    map: DualMap,
    unary_values: HashMap<(UnaryOp, Reg), Reg>,
    tangent_planes: HashMap<(Reg, usize), Reg>,
    cached_zero: Option<Reg>,
    cached_one: Option<Reg>,
    cached_ln10: Option<Reg>,
    cached_half: Option<Reg>,
    seed_mode: SeedMode<'a>,
    span: Option<rumoca_core::Span>,
    store_output_mode: StoreOutputMode,
    fold_program_cache: FoldAdCache,
    conditional_program_cache: ConditionalAdCache,
}

impl Default for AdBuilder<'_> {
    fn default() -> Self {
        Self {
            ops: Vec::new(),
            next_reg: 0,
            map: DualMap::default(),
            unary_values: HashMap::new(),
            tangent_planes: HashMap::new(),
            cached_zero: None,
            cached_one: None,
            cached_ln10: None,
            cached_half: None,
            seed_mode: SeedMode::default(),
            span: None,
            store_output_mode: StoreOutputMode::Derivative,
            fold_program_cache: Rc::new(RefCell::new(HashMap::new())),
            conditional_program_cache: Rc::new(RefCell::new(HashMap::new())),
        }
    }
}

impl<'a> AdBuilder<'a> {
    fn new_with_span(seed_mode: SeedMode<'a>, span: rumoca_core::Span) -> Self {
        Self::new_with_optional_span(seed_mode, Some(span))
    }

    fn new_with_optional_span(seed_mode: SeedMode<'a>, span: Option<rumoca_core::Span>) -> Self {
        Self {
            seed_mode,
            span,
            ..Self::default()
        }
    }

    fn new_with_optional_span_and_fold_cache(
        seed_mode: SeedMode<'a>,
        span: Option<rumoca_core::Span>,
        fold_program_cache: FoldAdCache,
    ) -> Self {
        Self {
            seed_mode,
            span,
            fold_program_cache,
            ..Self::default()
        }
    }
}

fn unsupported(reason: &str) -> SolveDerivationError {
    SolveDerivationError::UnspannedContractViolation {
        reason: reason.to_string(),
    }
}

fn ad_contract_violation(reason: String, span: rumoca_core::Span) -> SolveDerivationError {
    ad_optional_contract_violation(reason, Some(span))
}

fn ad_optional_contract_violation(
    reason: String,
    span: Option<rumoca_core::Span>,
) -> SolveDerivationError {
    match span.filter(|span| !span.is_dummy()) {
        Some(span) => SolveDerivationError::ContractViolation { reason, span },
        None => SolveDerivationError::UnspannedContractViolation { reason },
    }
}

fn checked_ad_product(
    lhs: usize,
    rhs: usize,
    span: impl Into<Option<rumoca_core::Span>>,
    context: &'static str,
) -> Result<usize, SolveDerivationError> {
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
) -> Result<(), SolveDerivationError> {
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
) -> Result<Vec<T>, SolveDerivationError> {
    let mut values = Vec::new();
    reserve_ad_capacity(&mut values, capacity, context, span)?;
    Ok(values)
}

fn collect_dual_range(
    builder: &AdBuilder<'_>,
    start: Reg,
    len: usize,
    span: impl Into<Option<rumoca_core::Span>>,
    capacity_context: &'static str,
    offset_context: &'static str,
) -> Result<Vec<DualReg>, SolveDerivationError> {
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
) -> Result<Vec<Reg>, SolveDerivationError> {
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
) -> Result<Reg, SolveDerivationError> {
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
mod output_projection_tests {
    use super::*;

    #[test]
    fn dual_registers_bind_once_and_report_missing_sources() {
        let mut builder = AdBuilder::default();
        builder.bind(3, DualReg { re: 1, du: 2 }).unwrap();
        builder.bind(0, DualReg { re: 4, du: 5 }).unwrap();
        let three = builder.lookup(3).unwrap();
        assert_eq!((three.re, three.du), (1, 2));
        assert!(builder.lookup(1).is_err(), "gap below a bound register");
        assert!(builder.lookup(9).is_err(), "beyond the bound registers");
        assert!(builder.bind(3, DualReg { re: 6, du: 7 }).is_err());
    }

    #[test]
    fn non_affine_dual_outputs_are_packed_before_compact_projection() {
        let mut builder = AdBuilder {
            next_reg: 20,
            store_output_mode: StoreOutputMode::Dual,
            ..AdBuilder::default()
        };
        builder.bind(0, DualReg { re: 5, du: 9 }).unwrap();
        builder.bind(1, DualReg { re: 7, du: 12 }).unwrap();

        builder.lower_store_range(0, 2, 1).unwrap();

        assert_eq!(
            &builder.ops,
            &[
                LinearOp::Move { dst: 20, src: 5 },
                LinearOp::Move { dst: 21, src: 9 },
                LinearOp::Move { dst: 22, src: 7 },
                LinearOp::Move { dst: 23, src: 12 },
                LinearOp::StoreOutputRange {
                    start: 20,
                    count: 4,
                    stride: 1,
                },
            ]
        );
    }
}
