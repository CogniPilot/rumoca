//! AD lowering from primal linear ops to forward-mode J·v ops.
//!
//! SPEC_0021 file-size exception: AD lowering still keeps scalar row AD,
//! tensor-node JVP lowering, and regression tests together while Solve IR
//! multi-output programs are being stabilized. split plan: move tensor-node JVP
//! lowering and indexed-load AD helpers into sibling modules behind this facade.

use crate::LowerError;
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
pub fn lower_compute_block_jvp(block: &ComputeBlock) -> Result<ComputeBlock, LowerError> {
    lower_compute_block_jvp_with_seed_mode(block, SeedMode::SolverYOnly)
}

/// Compute a JVP whose seed vector contains solver Y followed by P.
///
/// Initialization uses this form because fixed=false parameters are genuine
/// initialization unknowns and must contribute Jacobian columns.
pub fn lower_compute_block_full_jvp(
    block: &ComputeBlock,
    p_seed_offset: usize,
) -> Result<ComputeBlock, LowerError> {
    lower_compute_block_jvp_with_seed_mode(block, SeedMode::SolverYAndP { p_seed_offset })
}

fn lower_compute_block_jvp_with_seed_mode(
    block: &ComputeBlock,
    seed_mode: SeedMode,
) -> Result<ComputeBlock, LowerError> {
    let span = compute_block_context_span(block);
    let mut nodes = ad_vec_with_capacity(block.nodes.len(), "compute block JVP node count", span)?;
    for node in &block.nodes {
        nodes.push(lower_compute_node_jvp(node, seed_mode)?);
    }
    Ok(ComputeBlock { nodes })
}

fn lower_compute_node_jvp(
    node: &ComputeNode,
    seed_mode: SeedMode,
) -> Result<ComputeNode, LowerError> {
    match node {
        ComputeNode::ScalarPrograms(rows) => {
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
        ComputeNode::MatMul { .. } => lower_matmul_jvp_node(node, seed_mode),
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
    seed_mode: SeedMode,
) -> Result<AffineJvpOps, LowerError> {
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

fn lower_matmul_jvp_node(
    node: &ComputeNode,
    seed_mode: SeedMode,
) -> Result<ComputeNode, LowerError> {
    let ComputeNode::MatMul {
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
    let lhs_depends_on_seed = ops_reference_seeded_inputs(lhs_ops, seed_mode);
    let rhs_depends_on_seed = ops_reference_seeded_inputs(rhs_ops, seed_mode);

    let (mut lhs_builder, lhs) =
        lower_tensor_operand(lhs_ops, *lhs_start, lhs_len, 0, *span, seed_mode)?;
    let (jvp_lhs_start, jvp_k, jvp_lhs_pattern) = if lhs_depends_on_seed && rhs_depends_on_seed {
        let regs = block_product_lhs_regs(&lhs, *m, *k, *span)?;
        let block_k = checked_ad_product(2, *k, *span, "MatMul JVP block inner dimension")?;
        (
            lhs_builder.pack_registers(&regs)?,
            block_k,
            full_tensor_pattern(*m, block_k, *span)?,
        )
    } else {
        let regs = if lhs_depends_on_seed {
            dual_regs(&lhs, DualPart::Tangent, "MatMul JVP lhs tangent", *span)?
        } else {
            dual_regs(&lhs, DualPart::Primal, "MatMul JVP lhs primal", *span)?
        };
        (lhs_builder.pack_registers(&regs)?, *k, lhs_pattern.clone())
    };

    let rhs_next_reg = lhs_builder.next_reg;
    let (mut rhs_builder, rhs) =
        lower_tensor_operand(rhs_ops, *rhs_start, rhs_len, rhs_next_reg, *span, seed_mode)?;
    let (jvp_rhs_start, jvp_rhs_pattern) = if lhs_depends_on_seed && rhs_depends_on_seed {
        let regs = block_product_rhs_regs(&rhs, *span)?;
        (
            rhs_builder.pack_registers(&regs)?,
            full_tensor_pattern(jvp_k, *n, *span)?,
        )
    } else {
        let regs = if rhs_depends_on_seed || !lhs_depends_on_seed {
            dual_regs(&rhs, DualPart::Tangent, "MatMul JVP rhs tangent", *span)?
        } else {
            dual_regs(&rhs, DualPart::Primal, "MatMul JVP rhs primal", *span)?
        };
        (rhs_builder.pack_registers(&regs)?, rhs_pattern.clone())
    };

    Ok(ComputeNode::MatMul {
        lhs_ops: lhs_builder.ops,
        lhs_start: jvp_lhs_start,
        rhs_ops: rhs_builder.ops,
        rhs_start: jvp_rhs_start,
        m: *m,
        k: jvp_k,
        n: *n,
        lhs_pattern: jvp_lhs_pattern,
        rhs_pattern: jvp_rhs_pattern,
        metadata: metadata.clone(),
        span: *span,
    })
}

fn full_tensor_pattern(
    rows: usize,
    columns: usize,
    span: rumoca_core::Span,
) -> Result<rumoca_ir_solve::StructuralPattern, LowerError> {
    let provenance = rumoca_ir_solve::PatternProvenance::derived(
        rumoca_ir_solve::PatternDerivation::DependencyPropagation,
        span,
    )
    .map_err(|error| ad_contract_violation(error.to_string(), span))?;
    rumoca_ir_solve::StructuralPattern::full(rows, columns, provenance)
        .map_err(|error| ad_contract_violation(error.to_string(), span))
}

fn lower_tensor_operand(
    ops: &[LinearOp],
    value_start: Reg,
    value_count: usize,
    next_reg: Reg,
    span: rumoca_core::Span,
    seed_mode: SeedMode,
) -> Result<(AdBuilder, Vec<DualReg>), LowerError> {
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
                None,
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
        ComputeNode::ScalarPrograms(block) => block.program_span(0),
        ComputeNode::MatMul { span, .. }
        | ComputeNode::LinSolve { span, .. }
        | ComputeNode::Map { span, .. }
        | ComputeNode::AffineStencil { span, .. } => (!span.is_dummy()).then_some(*span),
    }
}

fn ops_reference_seeded_inputs(ops: &[LinearOp], seed_mode: SeedMode) -> bool {
    ops.iter().any(|op| {
        matches!(op, LinearOp::LoadY { .. })
            || matches!(seed_mode, SeedMode::SolverYAndP { .. })
                && matches!(op, LinearOp::LoadP { .. })
            || matches!(op, LinearOp::FunctionFold { program, .. }
                | LinearOp::GuardedFunctionFold { program, .. }
                if ops_reference_seeded_inputs(program.update(), seed_mode))
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
    seed_mode: SeedMode,
}

fn lower_linsolve_jvp_node(input: LinSolveJvpInput<'_>) -> Result<ComputeNode, LowerError> {
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
) -> Result<Vec<Vec<LinearOp>>, LowerError> {
    lower_scalar_program_rows_ad(
        primal_rows,
        &[],
        SeedMode::SolverYOnly,
        "scalar program AD row count",
    )
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
    fold_program_cache: FoldAdCache,
) -> Result<Vec<LinearOp>, LowerError> {
    let mut builder =
        AdBuilder::new_with_optional_span_and_fold_cache(seed_mode, span, fold_program_cache);
    for op in primal_ops {
        builder.lower_op(op.clone())?;
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

#[derive(Clone, Copy, Debug, Default)]
enum StoreOutputMode {
    #[default]
    Derivative,
    Primal,
    Dual,
}

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
    store_output_mode: StoreOutputMode,
    fold_program_cache: FoldAdCache,
    conditional_program_cache: ConditionalAdCache,
}

impl Default for AdBuilder {
    fn default() -> Self {
        Self {
            ops: Vec::new(),
            next_reg: 0,
            map: HashMap::new(),
            cached_zero: None,
            cached_one: None,
            cached_ln10: None,
            cached_two: None,
            seed_mode: SeedMode::default(),
            span: None,
            store_output_mode: StoreOutputMode::Derivative,
            fold_program_cache: Rc::new(RefCell::new(HashMap::new())),
            conditional_program_cache: Rc::new(RefCell::new(HashMap::new())),
        }
    }
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

    fn new_with_optional_span_and_fold_cache(
        seed_mode: SeedMode,
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

    // SPEC_0021: Exception - exhaustive AD lowering dispatch over every LinearOp variant.
    #[allow(clippy::too_many_lines)]
    fn lower_op(&mut self, op: LinearOp) -> Result<(), LowerError> {
        match op {
            LinearOp::Const { dst, value } => self.lower_const(dst, value),
            LinearOp::LoadTime { dst } => self.lower_load_time(dst),
            LinearOp::LoadY { dst, index } => self.lower_load_y(dst, index),
            LinearOp::LoadP { dst, index } => self.lower_load_p(dst, index),
            LinearOp::LoadIndexedRegister {
                dst,
                base,
                stride,
                dimensions,
                indices,
            } => self.lower_load_indexed_register(dst, base, stride, dimensions, &indices),
            LinearOp::LoadIndexedFoldCarried {
                dst,
                base,
                stride,
                dimensions,
                indices,
            } => self.lower_load_indexed_fold_carried(dst, base, stride, dimensions, &indices),
            LinearOp::LoadIndexedFoldCapture {
                dst,
                base,
                stride,
                dimensions,
                indices,
            } => self.lower_load_indexed_fold_capture(dst, base, stride, dimensions, &indices),
            LinearOp::LoadSeed { .. } => Err(unsupported("unexpected LoadSeed in primal row")),
            LinearOp::LoadFoldCarried { dst, index } => self.lower_load_fold_carried(dst, index),
            LinearOp::LoadFoldIndex { dst, dimension } => {
                self.lower_load_fold_index(dst, dimension)
            }
            LinearOp::LoadFoldCapture { dst, index } => self.lower_load_fold_capture(dst, index),
            LinearOp::LoadFunctionConditionalCapture { dst, index } => {
                self.lower_load_function_conditional_capture(dst, index)
            }
            LinearOp::LoadFunctionConditionalCaptureRange {
                dst_start,
                index_start,
                count,
            } => self.lower_load_function_conditional_capture_range(dst_start, index_start, count),
            LinearOp::Move { dst, src } => self.lower_move(dst, src),
            LinearOp::LinearSolveComponent {
                dst,
                matrix_start,
                rhs_start,
                n,
                component,
            } => self.lower_linear_solve_component(dst, matrix_start, rhs_start, n, component),
            LinearOp::DotProduct {
                dst,
                lhs_start,
                rhs_start,
                count,
                lhs_stride,
                rhs_stride,
            } => self.lower_dot_product(
                dst,
                (
                    StridedOperand {
                        start: lhs_start,
                        stride: lhs_stride,
                    },
                    StridedOperand {
                        start: rhs_start,
                        stride: rhs_stride,
                    },
                ),
                count,
            ),
            LinearOp::MatrixMultiply {
                dst_start,
                lhs_start,
                rhs_start,
                rows,
                inner,
                columns,
                lanes,
            } => self.lower_matrix_multiply(
                dst_start,
                lhs_start,
                rhs_start,
                MatrixProductShape {
                    rows,
                    inner,
                    columns,
                    lanes,
                },
            ),
            LinearOp::TensorBinary {
                dst_start,
                op,
                lhs_start,
                rhs_start,
                count,
                lhs_stride,
                rhs_stride,
                lanes,
            } => self.lower_tensor_binary(
                dst_start,
                op,
                (
                    StridedOperand {
                        start: lhs_start,
                        stride: lhs_stride,
                    },
                    StridedOperand {
                        start: rhs_start,
                        stride: rhs_stride,
                    },
                ),
                count,
                lanes,
            ),
            LinearOp::TensorCross {
                dst_start,
                lhs_start,
                rhs_start,
                lanes,
            } => self.lower_tensor_cross(dst_start, lhs_start, rhs_start, lanes),
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
            LinearOp::FunctionFold {
                dst_start,
                initial_start,
                capture_start,
                program,
            } => self.lower_function_fold(dst_start, initial_start, capture_start, program),
            LinearOp::GuardedFunctionFold {
                dst_start,
                initial_start,
                capture_start,
                activation,
                program,
            } => self.lower_guarded_function_fold(
                dst_start,
                initial_start,
                capture_start,
                activation,
                program,
            ),
            LinearOp::FunctionConditional {
                dst_start,
                capture_start,
                program,
            } => self.lower_function_conditional(dst_start, capture_start, program),
            LinearOp::PureCall {
                dst_start,
                input_starts,
                site,
            } => self.lower_pure_call(dst_start, &input_starts, site),
            LinearOp::PureCallDirectional { .. } => {
                Err(unsupported("unexpected directional call in primal row"))
            }
            LinearOp::StoreOutputFoldTensorUpdate {
                source_base,
                source_stride,
                dimensions,
                updates,
                nodes,
                result,
                lanes,
            } => self.lower_store_fold_tensor_update(FoldTensorUpdateStore {
                source_base,
                source_stride,
                dimensions: &dimensions,
                updates: &updates,
                nodes: &nodes,
                result,
                lanes,
            }),
            LinearOp::TensorTranspose {
                dst_start,
                src_start,
                rows,
                columns,
                element_width,
                lanes,
            } => self.lower_tensor_transpose(
                dst_start,
                src_start,
                rows,
                columns,
                element_width,
                lanes,
            ),
            LinearOp::TensorConcatenate {
                dst_start,
                sources,
                dimensions,
                axis,
                lanes,
            } => self.lower_tensor_concatenate(dst_start, &sources, dimensions, axis, lanes),
            LinearOp::TensorUpdate {
                dst_start,
                base_start,
                value_start,
                dimensions,
                subscripts,
                lanes,
            } => self.lower_tensor_update(
                dst_start,
                base_start,
                value_start,
                dimensions,
                &subscripts,
                lanes,
            ),
            LinearOp::TensorFill {
                dst_start,
                value_start,
                count,
                lanes,
            } => self.lower_tensor_fill(dst_start, value_start, count, lanes),
            LinearOp::TensorIdentity {
                dst_start,
                size,
                lanes,
            } => self.lower_tensor_identity(dst_start, size, lanes),
            LinearOp::TensorLoad {
                dst_start,
                input,
                input_start,
                count,
                seed_start,
                lanes,
            } => self.lower_tensor_load(dst_start, input, input_start, count, seed_start, lanes),
            LinearOp::StoreOutputFunctionFold {
                initial,
                capture_start,
                program,
                result_base,
                count,
                condition,
                nested_when_true,
            } => self.lower_store_output_function_fold(
                initial,
                capture_start,
                program,
                (result_base, count),
                condition,
                nested_when_true,
            ),
            LinearOp::StoreOutputRange {
                start,
                count,
                stride,
            } => self.lower_store_range(start, count, stride),
            LinearOp::StoreOutput { src } => self.lower_store(src),
        }
    }

    // SPEC_0021: Exception - exhaustive directional-ABI lowering for typed pure calls.
    #[expect(
        clippy::too_many_lines,
        reason = "all primal and tangent input/output forms are validated in one dispatcher"
    )]
    fn lower_pure_call(
        &mut self,
        dst_start: Reg,
        input_starts: &[Reg],
        site: rumoca_ir_solve::SolvePureCallSite,
    ) -> Result<(), LowerError> {
        let directional = site.directional().cloned().ok_or_else(|| {
            unsupported("typed pure-call directional owner has not been constructed")
        })?;
        if input_starts.len() != site.inputs().len() {
            return Err(unsupported("typed pure-call AD input interface mismatch"));
        }
        let mut directional_inputs = Vec::with_capacity(directional.inputs().len());
        for (&start, value_type) in input_starts.iter().zip(site.inputs()) {
            let count = value_type.scalar_count() as usize;
            let mut primals = Vec::with_capacity(count);
            let mut tangents = Vec::with_capacity(count);
            for offset in 0..count {
                let register =
                    checked_ad_reg_offset(start, offset, self.span, "typed pure-call AD input")?;
                let value = self.lookup(register)?;
                primals.push(value.re);
                tangents.push(value.du);
            }
            directional_inputs.push(self.pack_registers(&primals)?);
            if matches!(
                value_type.element_type(),
                rumoca_ir_solve::SolveScalarType::Real { .. }
            ) {
                directional_inputs.push(self.pack_registers(&tangents)?);
            }
        }
        if directional_inputs.len() != directional.inputs().len() {
            return Err(unsupported(
                "typed pure-call directional input ABI does not match primal owner",
            ));
        }
        let output_count = directional
            .output_scalar_count()
            .ok_or_else(|| unsupported("typed pure-call directional output width overflows"))?;
        let directional_start = self.next_reg;
        for _ in 0..output_count {
            self.alloc_reg()?;
        }
        self.ops.push(LinearOp::PureCallDirectional {
            dst_start: directional_start,
            input_starts: directional_inputs.into_boxed_slice(),
            site: directional,
        });

        let mut primal_offset = 0usize;
        let mut directional_offset = 0usize;
        for output in site.outputs() {
            let count = output.value_type().scalar_count() as usize;
            let has_tangent = output.kind() == rumoca_ir_solve::SolvePureCallOutputKind::Result
                && matches!(
                    output.value_type().element_type(),
                    rumoca_ir_solve::SolveScalarType::Real { .. }
                );
            let primal_start = checked_ad_reg_offset(
                directional_start,
                directional_offset,
                self.span,
                "typed pure-call directional primal output",
            )?;
            directional_offset = directional_offset
                .checked_add(count)
                .ok_or_else(|| unsupported("typed pure-call directional output overflow"))?;
            let tangent_start = has_tangent
                .then(|| {
                    checked_ad_reg_offset(
                        directional_start,
                        directional_offset,
                        self.span,
                        "typed pure-call directional tangent output",
                    )
                })
                .transpose()?;
            if has_tangent {
                directional_offset = directional_offset
                    .checked_add(count)
                    .ok_or_else(|| unsupported("typed pure-call tangent output overflow"))?;
            }
            for element in 0..count {
                let primal_register = checked_ad_reg_offset(
                    dst_start,
                    primal_offset + element,
                    self.span,
                    "typed pure-call primal result",
                )?;
                let re = checked_ad_reg_offset(
                    primal_start,
                    element,
                    self.span,
                    "typed pure-call directional primal element",
                )?;
                let du = match tangent_start {
                    Some(start) => checked_ad_reg_offset(
                        start,
                        element,
                        self.span,
                        "typed pure-call directional tangent element",
                    )?,
                    None => self.zero_reg()?,
                };
                self.bind(primal_register, DualReg { re, du })?;
            }
            primal_offset = primal_offset
                .checked_add(count)
                .ok_or_else(|| unsupported("typed pure-call primal output overflow"))?;
        }
        if directional_offset != output_count {
            return Err(unsupported(
                "typed pure-call directional output ABI does not match primal owner",
            ));
        }
        Ok(())
    }

    fn lower_load_fold_carried(&mut self, dst: Reg, index: usize) -> Result<(), LowerError> {
        let primal_index = index
            .checked_mul(2)
            .ok_or_else(|| unsupported("function-fold AD carried index overflow"))?;
        let re = self.alloc_reg()?;
        self.ops.push(LinearOp::LoadFoldCarried {
            dst: re,
            index: primal_index,
        });
        let du = self.alloc_reg()?;
        self.ops.push(LinearOp::LoadFoldCarried {
            dst: du,
            index: primal_index + 1,
        });
        self.bind(dst, DualReg { re, du })
    }

    fn lower_load_fold_index(&mut self, dst: Reg, dimension: usize) -> Result<(), LowerError> {
        let re = self.alloc_reg()?;
        self.ops
            .push(LinearOp::LoadFoldIndex { dst: re, dimension });
        let du = self.zero_reg()?;
        self.bind(dst, DualReg { re, du })
    }

    fn lower_load_fold_capture(&mut self, dst: Reg, index: usize) -> Result<(), LowerError> {
        let primal_index = index
            .checked_mul(2)
            .ok_or_else(|| unsupported("function-fold AD capture index overflow"))?;
        let re = self.alloc_reg()?;
        self.ops.push(LinearOp::LoadFoldCapture {
            dst: re,
            index: primal_index,
        });
        let du = self.alloc_reg()?;
        self.ops.push(LinearOp::LoadFoldCapture {
            dst: du,
            index: primal_index + 1,
        });
        self.bind(dst, DualReg { re, du })
    }

    fn lower_load_function_conditional_capture(
        &mut self,
        dst: Reg,
        index: usize,
    ) -> Result<(), LowerError> {
        let primal_index = index
            .checked_mul(2)
            .ok_or_else(|| unsupported("function-conditional AD capture index overflow"))?;
        let re = self.alloc_reg()?;
        self.ops.push(LinearOp::LoadFunctionConditionalCapture {
            dst: re,
            index: primal_index,
        });
        let du = self.alloc_reg()?;
        self.ops.push(LinearOp::LoadFunctionConditionalCapture {
            dst: du,
            index: primal_index + 1,
        });
        self.bind(dst, DualReg { re, du })
    }

    fn lower_load_function_conditional_capture_range(
        &mut self,
        dst_start: Reg,
        index_start: usize,
        count: usize,
    ) -> Result<(), LowerError> {
        let dual_index_start = index_start
            .checked_mul(2)
            .ok_or_else(|| unsupported("function-conditional AD capture range index overflow"))?;
        let dual_count = count
            .checked_mul(2)
            .ok_or_else(|| unsupported("function-conditional AD capture range count overflow"))?;
        let dual_start = self.next_reg;
        for _ in 0..dual_count {
            self.alloc_reg()?;
        }
        self.ops
            .push(LinearOp::LoadFunctionConditionalCaptureRange {
                dst_start: dual_start,
                index_start: dual_index_start,
                count: dual_count,
            });
        for offset in 0..count {
            let primal = checked_ad_reg_offset(
                dst_start,
                offset,
                self.span,
                "function conditional capture range primal",
            )?;
            let dual = checked_ad_reg_offset(
                dual_start,
                checked_ad_product(
                    offset,
                    2,
                    self.span,
                    "function conditional capture range dual lane",
                )?,
                self.span,
                "function conditional capture range dual",
            )?;
            self.bind(
                primal,
                DualReg {
                    re: dual,
                    du: dual + 1,
                },
            )?;
        }
        Ok(())
    }

    fn derived_fold_program(
        &mut self,
        primal: &Arc<rumoca_ir_solve::FunctionFoldProgram>,
    ) -> Result<Arc<rumoca_ir_solve::FunctionFoldProgram>, LowerError> {
        let key = Arc::as_ptr(primal) as usize;
        if let Some(program) = self.fold_program_cache.borrow().get(&key).cloned() {
            return Ok(program);
        }

        let mut update = Self::new_with_optional_span_and_fold_cache(
            self.seed_mode,
            self.span,
            self.fold_program_cache.clone(),
        );
        update.conditional_program_cache = self.conditional_program_cache.clone();
        update.store_output_mode = StoreOutputMode::Dual;
        for operation in primal.update() {
            update.lower_op(operation.clone())?;
        }
        let carried_count = primal
            .carried_count()
            .checked_mul(2)
            .ok_or_else(|| unsupported("function-fold AD carried count overflow"))?;
        let capture_count = primal
            .capture_count()
            .checked_mul(2)
            .ok_or_else(|| unsupported("function-fold AD capture count overflow"))?;
        let program = Arc::new(
            rumoca_ir_solve::FunctionFoldProgram::checked(
                primal.domain().clone(),
                carried_count,
                capture_count,
                update.ops,
            )
            .map_err(|error| {
                unsupported(&format!("function-fold AD register proof failed: {error}"))
            })?,
        );
        self.fold_program_cache
            .borrow_mut()
            .insert(key, program.clone());
        Ok(program)
    }

    // SPEC_0021: Exception - exhaustive derivative dispatch over conditional region operations.
    #[allow(clippy::excessive_nesting)]
    fn derive_conditional_region(
        &self,
        primal: &[LinearOp],
        store_output_mode: StoreOutputMode,
    ) -> Result<Vec<LinearOp>, LowerError> {
        let mut region = Self::new_with_optional_span_and_fold_cache(
            self.seed_mode,
            self.span,
            self.fold_program_cache.clone(),
        );
        region.conditional_program_cache = self.conditional_program_cache.clone();
        region.store_output_mode = store_output_mode;
        for operation in primal {
            region.lower_op(operation.clone())?;
        }
        Ok(region.ops)
    }

    fn derived_conditional_program(
        &mut self,
        primal: &Arc<rumoca_ir_solve::FunctionConditionalProgram>,
    ) -> Result<Arc<rumoca_ir_solve::FunctionConditionalProgram>, LowerError> {
        let key = Arc::as_ptr(primal) as usize;
        if let Some(program) = self.conditional_program_cache.borrow().get(&key).cloned() {
            return Ok(program);
        }

        let capture_count = checked_ad_product(
            primal.capture_count(),
            2,
            self.span,
            "function-conditional AD captures",
        )?;
        let target_widths = primal
            .target_widths()
            .iter()
            .map(|width| checked_ad_product(*width, 2, self.span, "function-conditional AD target"))
            .collect::<Result<Vec<_>, _>>()?;
        let mut arms = ad_vec_with_capacity(
            primal.arms().len(),
            "function-conditional AD arms",
            self.span,
        )?;
        for arm in primal.arms() {
            arms.push((
                self.derive_conditional_region(arm.condition(), StoreOutputMode::Primal)?,
                self.derive_conditional_region(arm.result(), StoreOutputMode::Dual)?,
            ));
        }
        let fallback = self.derive_conditional_region(primal.fallback(), StoreOutputMode::Dual)?;
        let checked = match primal.owner() {
            Some(owner) => rumoca_ir_solve::FunctionConditionalProgram::checked_owned(
                owner,
                capture_count,
                target_widths,
                arms,
                fallback,
            ),
            None => rumoca_ir_solve::FunctionConditionalProgram::checked(
                capture_count,
                target_widths,
                arms,
                fallback,
            ),
        }
        .map_err(|error| {
            unsupported(&format!(
                "function-conditional AD register proof failed: {error}"
            ))
        })?;
        let program = Arc::new(checked);
        self.conditional_program_cache
            .borrow_mut()
            .insert(key, program.clone());
        Ok(program)
    }

    fn lower_function_fold(
        &mut self,
        dst_start: Reg,
        initial_start: Reg,
        capture_start: Reg,
        program: Arc<rumoca_ir_solve::FunctionFoldProgram>,
    ) -> Result<(), LowerError> {
        self.lower_function_fold_with_activation(
            dst_start,
            initial_start,
            capture_start,
            None,
            program,
        )
    }

    fn lower_function_conditional(
        &mut self,
        dst_start: Reg,
        capture_start: Reg,
        program: Arc<rumoca_ir_solve::FunctionConditionalProgram>,
    ) -> Result<(), LowerError> {
        let packed_captures =
            self.pack_dual_register_range(capture_start, program.capture_count())?;
        let derived_program = self.derived_conditional_program(&program)?;
        let result_start = self.next_reg;
        for _ in 0..derived_program.result_count() {
            self.alloc_reg()?;
        }
        self.ops.push(LinearOp::FunctionConditional {
            dst_start: result_start,
            capture_start: packed_captures,
            program: derived_program,
        });
        for offset in 0..program.result_count() {
            let primal_dst =
                checked_ad_reg_offset(dst_start, offset, self.span, "function conditional output")?;
            let lane =
                checked_ad_product(offset, 2, self.span, "function conditional AD result lane")?;
            let re = checked_ad_reg_offset(
                result_start,
                lane,
                self.span,
                "function conditional AD primal result",
            )?;
            self.bind(primal_dst, DualReg { re, du: re + 1 })?;
        }
        Ok(())
    }

    fn lower_guarded_function_fold(
        &mut self,
        dst_start: Reg,
        initial_start: Reg,
        capture_start: Reg,
        activation: Reg,
        program: Arc<rumoca_ir_solve::FunctionFoldProgram>,
    ) -> Result<(), LowerError> {
        let activation = self.lookup(activation)?.re;
        self.lower_function_fold_with_activation(
            dst_start,
            initial_start,
            capture_start,
            Some(activation),
            program,
        )
    }

    fn lower_function_fold_with_activation(
        &mut self,
        dst_start: Reg,
        initial_start: Reg,
        capture_start: Reg,
        activation: Option<Reg>,
        program: Arc<rumoca_ir_solve::FunctionFoldProgram>,
    ) -> Result<(), LowerError> {
        let packed_initial =
            self.pack_dual_register_range(initial_start, program.carried_count())?;
        let packed_captures =
            self.pack_dual_register_range(capture_start, program.capture_count())?;
        let derived_program = self.derived_fold_program(&program)?;
        let carried_count = derived_program.carried_count();
        let folded_start = self.next_reg;
        for _ in 0..carried_count {
            self.alloc_reg()?;
        }
        self.ops.push(match activation {
            Some(activation) => LinearOp::GuardedFunctionFold {
                dst_start: folded_start,
                initial_start: packed_initial,
                capture_start: packed_captures,
                activation,
                program: derived_program,
            },
            None => LinearOp::FunctionFold {
                dst_start: folded_start,
                initial_start: packed_initial,
                capture_start: packed_captures,
                program: derived_program,
            },
        });
        for offset in 0..program.carried_count() {
            let offset_reg = Reg::try_from(offset)
                .map_err(|_| unsupported("function-fold AD destination offset overflow"))?;
            let primal_dst = dst_start
                .checked_add(offset_reg)
                .ok_or_else(|| unsupported("function-fold AD destination overflow"))?;
            let lane = offset_reg
                .checked_mul(2)
                .ok_or_else(|| unsupported("function-fold AD lane overflow"))?;
            self.bind(
                primal_dst,
                DualReg {
                    re: folded_start + lane,
                    du: folded_start + lane + 1,
                },
            )?;
        }
        Ok(())
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

    fn lower_load_indexed_register(
        &mut self,
        dst: Reg,
        base: Reg,
        stride: usize,
        dimensions: Box<[u32]>,
        indices: &[rumoca_ir_solve::TensorIndex],
    ) -> Result<(), LowerError> {
        let count = dimensions
            .iter()
            .try_fold(1usize, |count, &extent| count.checked_mul(extent as usize))
            .ok_or_else(|| unsupported("runtime tensor projection AD extent overflow"))?;
        let mut real =
            ad_vec_with_capacity(count, "runtime tensor projection AD real count", self.span)?;
        let mut tangent = ad_vec_with_capacity(
            count,
            "runtime tensor projection AD tangent count",
            self.span,
        )?;
        for offset in 0..count {
            let source = base
                .checked_add(
                    Reg::try_from(offset.checked_mul(stride).ok_or_else(|| {
                        unsupported("runtime tensor projection AD stride overflow")
                    })?)
                    .map_err(|_| unsupported("runtime tensor projection AD stride overflow"))?,
                )
                .ok_or_else(|| unsupported("runtime tensor projection AD source overflow"))?;
            let dual = self.lookup(source)?;
            real.push(dual.re);
            tangent.push(dual.du);
        }
        let indices = indices
            .iter()
            .map(|index| match *index {
                rumoca_ir_solve::TensorIndex::Constant(coordinate) => {
                    Ok(rumoca_ir_solve::TensorIndex::Constant(coordinate))
                }
                rumoca_ir_solve::TensorIndex::Runtime(register) => Ok(
                    rumoca_ir_solve::TensorIndex::Runtime(self.lookup(register)?.re),
                ),
            })
            .collect::<Result<Vec<_>, LowerError>>()?
            .into_boxed_slice();
        let real_base = self.pack_registers(&real)?;
        let re = self.alloc_reg()?;
        self.ops.push(LinearOp::LoadIndexedRegister {
            dst: re,
            base: real_base,
            stride: 1,
            dimensions: dimensions.clone(),
            indices: indices.clone(),
        });
        let tangent_base = self.pack_registers(&tangent)?;
        let du = self.alloc_reg()?;
        self.ops.push(LinearOp::LoadIndexedRegister {
            dst: du,
            base: tangent_base,
            stride: 1,
            dimensions,
            indices,
        });
        self.bind(dst, DualReg { re, du })
    }

    fn lower_load_indexed_fold_carried(
        &mut self,
        dst: Reg,
        base: usize,
        stride: usize,
        dimensions: Box<[u32]>,
        indices: &[rumoca_ir_solve::TensorIndex],
    ) -> Result<(), LowerError> {
        let indices = indices
            .iter()
            .map(|index| match *index {
                rumoca_ir_solve::TensorIndex::Constant(coordinate) => {
                    Ok(rumoca_ir_solve::TensorIndex::Constant(coordinate))
                }
                rumoca_ir_solve::TensorIndex::Runtime(register) => Ok(
                    rumoca_ir_solve::TensorIndex::Runtime(self.lookup(register)?.re),
                ),
            })
            .collect::<Result<Vec<_>, LowerError>>()?
            .into_boxed_slice();
        let dual_base = base
            .checked_mul(2)
            .ok_or_else(|| unsupported("indexed function-fold AD base overflow"))?;
        let dual_stride = stride
            .checked_mul(2)
            .ok_or_else(|| unsupported("indexed function-fold AD stride overflow"))?;
        let re = self.alloc_reg()?;
        self.ops.push(LinearOp::LoadIndexedFoldCarried {
            dst: re,
            base: dual_base,
            stride: dual_stride,
            dimensions: dimensions.clone(),
            indices: indices.clone(),
        });
        let du = self.alloc_reg()?;
        self.ops.push(LinearOp::LoadIndexedFoldCarried {
            dst: du,
            base: dual_base + 1,
            stride: dual_stride,
            dimensions,
            indices,
        });
        self.bind(dst, DualReg { re, du })
    }

    fn lower_load_indexed_fold_capture(
        &mut self,
        dst: Reg,
        base: usize,
        stride: usize,
        dimensions: Box<[u32]>,
        indices: &[rumoca_ir_solve::TensorIndex],
    ) -> Result<(), LowerError> {
        let indices = indices
            .iter()
            .map(|index| match *index {
                rumoca_ir_solve::TensorIndex::Constant(coordinate) => {
                    Ok(rumoca_ir_solve::TensorIndex::Constant(coordinate))
                }
                rumoca_ir_solve::TensorIndex::Runtime(register) => Ok(
                    rumoca_ir_solve::TensorIndex::Runtime(self.lookup(register)?.re),
                ),
            })
            .collect::<Result<Vec<_>, LowerError>>()?
            .into_boxed_slice();
        let dual_base = base
            .checked_mul(2)
            .ok_or_else(|| unsupported("indexed function-fold AD capture base overflow"))?;
        let dual_stride = stride
            .checked_mul(2)
            .ok_or_else(|| unsupported("indexed function-fold AD capture stride overflow"))?;
        let re = self.alloc_reg()?;
        self.ops.push(LinearOp::LoadIndexedFoldCapture {
            dst: re,
            base: dual_base,
            stride: dual_stride,
            dimensions: dimensions.clone(),
            indices: indices.clone(),
        });
        let du = self.alloc_reg()?;
        self.ops.push(LinearOp::LoadIndexedFoldCapture {
            dst: du,
            base: dual_base + 1,
            stride: dual_stride,
            dimensions,
            indices,
        });
        self.bind(dst, DualReg { re, du })
    }

    // SPEC_0021: Exception - exhaustive tensor-subscript AD dispatch.
    #[allow(clippy::excessive_nesting)]
    fn lower_store_fold_tensor_update(
        &mut self,
        store: FoldTensorUpdateStore<'_>,
    ) -> Result<(), LowerError> {
        let FoldTensorUpdateStore {
            source_base,
            source_stride,
            dimensions,
            updates,
            nodes,
            result,
            lanes,
        } = store;
        if source_stride != 1 || lanes != 1 {
            return Err(unsupported("nested dual-lane function-fold tensor update"));
        }
        let mut dual_updates = Vec::with_capacity(updates.len());
        for update in updates {
            if update.value_stride != 1 {
                return Err(unsupported("nested dual-lane function-fold tensor patch"));
            }
            let subscripts = update
                .subscripts
                .iter()
                .map(|subscript| match *subscript {
                    rumoca_ir_solve::TensorSubscript::Whole => {
                        Ok(rumoca_ir_solve::TensorSubscript::Whole)
                    }
                    rumoca_ir_solve::TensorSubscript::Index(
                        rumoca_ir_solve::TensorIndex::Constant(coordinate),
                    ) => Ok(rumoca_ir_solve::TensorSubscript::Index(
                        rumoca_ir_solve::TensorIndex::Constant(coordinate),
                    )),
                    rumoca_ir_solve::TensorSubscript::Index(
                        rumoca_ir_solve::TensorIndex::Runtime(register),
                    ) => Ok(rumoca_ir_solve::TensorSubscript::Index(
                        rumoca_ir_solve::TensorIndex::Runtime(self.lookup(register)?.re),
                    )),
                })
                .collect::<Result<Vec<_>, LowerError>>()?
                .into_boxed_slice();
            let condition = update
                .condition
                .map(|condition| self.lookup(condition).map(|dual| dual.re))
                .transpose()?;
            let value_count = dimensions
                .iter()
                .zip(subscripts.iter())
                .try_fold(1usize, |count, (&extent, subscript)| {
                    if matches!(subscript, rumoca_ir_solve::TensorSubscript::Whole) {
                        count.checked_mul(extent as usize)
                    } else {
                        Some(count)
                    }
                })
                .ok_or_else(|| unsupported("function-fold tensor update AD extent overflow"))?;
            let mut values = Vec::with_capacity(value_count.saturating_mul(2));
            for element in 0..value_count {
                let element = Reg::try_from(element)
                    .map_err(|_| unsupported("function-fold tensor update AD register overflow"))?;
                let source = update
                    .value_start
                    .checked_add(element)
                    .ok_or_else(|| unsupported("function-fold tensor update AD value overflow"))?;
                let dual = self.lookup(source)?;
                values.extend([dual.re, dual.du]);
            }
            let value_start = self.pack_registers(&values)?;
            dual_updates.push(rumoca_ir_solve::FoldTensorUpdate {
                subscripts,
                condition,
                value_start,
                value_stride: 2,
            });
        }
        let nodes = nodes
            .iter()
            .map(|node| match *node {
                rumoca_ir_solve::FoldTensorNode::Update { base, update } => {
                    Ok(rumoca_ir_solve::FoldTensorNode::Update { base, update })
                }
                rumoca_ir_solve::FoldTensorNode::Select {
                    condition,
                    if_true,
                    if_false,
                } => Ok(rumoca_ir_solve::FoldTensorNode::Select {
                    condition: self.lookup(condition)?.re,
                    if_true,
                    if_false,
                }),
            })
            .collect::<Result<Vec<_>, LowerError>>()?;
        self.ops.push(LinearOp::StoreOutputFoldTensorUpdate {
            source_base: source_base
                .checked_mul(2)
                .ok_or_else(|| unsupported("function-fold tensor update AD base overflow"))?,
            source_stride: 2,
            dimensions: dimensions.into(),
            updates: dual_updates.into_boxed_slice(),
            nodes: nodes.into_boxed_slice(),
            result,
            lanes: 2,
        });
        Ok(())
    }

    // SPEC_0021: Exception - exhaustive nested-fold initial-source AD dispatch.
    #[allow(clippy::excessive_nesting)]
    fn lower_store_output_function_fold(
        &mut self,
        initial: Box<[rumoca_ir_solve::FoldInitialSource]>,
        capture_start: Reg,
        program: Arc<rumoca_ir_solve::FunctionFoldProgram>,
        result: (usize, usize),
        condition: Option<Reg>,
        nested_when_true: bool,
    ) -> Result<(), LowerError> {
        let (result_base, count) = result;
        let mut dual_initial = Vec::with_capacity(initial.len());
        for source in initial {
            match source {
                rumoca_ir_solve::FoldInitialSource::ParentCarried { base, count } => {
                    dual_initial.push(rumoca_ir_solve::FoldInitialSource::ParentCarried {
                        base: base.checked_mul(2).ok_or_else(|| {
                            unsupported("nested fold AD parent initial base overflow")
                        })?,
                        count: count.checked_mul(2).ok_or_else(|| {
                            unsupported("nested fold AD parent initial count overflow")
                        })?,
                    });
                }
                rumoca_ir_solve::FoldInitialSource::Registers { start, count } => {
                    let mut values = Vec::with_capacity(count.saturating_mul(2));
                    for offset in 0..count {
                        let source = start
                            .checked_add(Reg::try_from(offset).map_err(|_| {
                                unsupported("nested fold AD initial offset overflow")
                            })?)
                            .ok_or_else(|| {
                                unsupported("nested fold AD initial register overflow")
                            })?;
                        let dual = self.lookup(source)?;
                        values.extend([dual.re, dual.du]);
                    }
                    dual_initial.push(rumoca_ir_solve::FoldInitialSource::Registers {
                        start: self.pack_registers(&values)?,
                        count: count
                            .checked_mul(2)
                            .ok_or_else(|| unsupported("nested fold AD initial count overflow"))?,
                    });
                }
            }
        }
        let mut captures = Vec::with_capacity(program.capture_count().saturating_mul(2));
        for offset in 0..program.capture_count() {
            let source = capture_start
                .checked_add(
                    Reg::try_from(offset)
                        .map_err(|_| unsupported("nested fold AD capture offset overflow"))?,
                )
                .ok_or_else(|| unsupported("nested fold AD capture register overflow"))?;
            let dual = self.lookup(source)?;
            captures.extend([dual.re, dual.du]);
        }
        let capture_start = self.pack_registers(&captures)?;
        let program = self.derived_fold_program(&program)?;
        let condition = condition
            .map(|condition| self.lookup(condition).map(|value| value.re))
            .transpose()?;
        self.ops.push(LinearOp::StoreOutputFunctionFold {
            initial: dual_initial.into_boxed_slice(),
            capture_start,
            program,
            result_base: result_base
                .checked_mul(2)
                .ok_or_else(|| unsupported("nested fold AD result base overflow"))?,
            count: count
                .checked_mul(2)
                .ok_or_else(|| unsupported("nested fold AD result count overflow"))?,
            condition,
            nested_when_true,
        });
        Ok(())
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

    fn lower_dot_product(
        &mut self,
        dst: Reg,
        operands: (StridedOperand, StridedOperand),
        count: usize,
    ) -> Result<(), LowerError> {
        let (lhs_operand, rhs_operand) = operands;
        let mut sum: Option<DualReg> = None;
        for term in 0..count {
            let lhs = self.lookup(lhs_operand.start + (term * lhs_operand.stride) as Reg)?;
            let rhs = self.lookup(rhs_operand.start + (term * rhs_operand.stride) as Reg)?;
            let re = self.emit_binary(BinaryOp::Mul, lhs.re, rhs.re)?;
            let lhs_du = self.emit_binary(BinaryOp::Mul, lhs.du, rhs.re)?;
            let rhs_du = self.emit_binary(BinaryOp::Mul, lhs.re, rhs.du)?;
            let du = self.emit_binary(BinaryOp::Add, lhs_du, rhs_du)?;
            sum = Some(match sum {
                Some(sum) => DualReg {
                    re: self.emit_binary(BinaryOp::Add, sum.re, re)?,
                    du: self.emit_binary(BinaryOp::Add, sum.du, du)?,
                },
                None => DualReg { re, du },
            });
        }
        let sum = match sum {
            Some(sum) => sum,
            None => {
                let zero = self.zero_reg()?;
                DualReg { re: zero, du: zero }
            }
        };
        self.bind(dst, sum)
    }

    fn lower_matrix_multiply(
        &mut self,
        dst_start: Reg,
        lhs_start: Reg,
        rhs_start: Reg,
        shape: MatrixProductShape,
    ) -> Result<(), LowerError> {
        let MatrixProductShape {
            rows,
            inner,
            columns,
            lanes,
        } = shape;
        if lanes != 1 {
            return Err(unsupported(
                "forward AD expects a primal matrix multiply with one lane",
            ));
        }
        let lhs_count = checked_ad_product(rows, inner, self.span, "matrix multiply lhs")?;
        let rhs_count = checked_ad_product(inner, columns, self.span, "matrix multiply rhs")?;
        let output_count = checked_ad_product(rows, columns, self.span, "matrix multiply output")?;
        let lhs_start = self.pack_dual_register_range(lhs_start, lhs_count)?;
        let rhs_start = self.pack_dual_register_range(rhs_start, rhs_count)?;
        let dual_start = self.next_reg;
        for _ in 0..checked_ad_product(output_count, 2, self.span, "matrix multiply dual output")? {
            self.alloc_reg()?;
        }
        self.ops.push(LinearOp::MatrixMultiply {
            dst_start: dual_start,
            lhs_start,
            rhs_start,
            rows,
            inner,
            columns,
            lanes: 2,
        });
        for offset in 0..output_count {
            let primal =
                checked_ad_reg_offset(dst_start, offset, self.span, "matrix multiply output")?;
            let dual = checked_ad_reg_offset(
                dual_start,
                checked_ad_product(offset, 2, self.span, "matrix multiply dual lane")?,
                self.span,
                "matrix multiply dual output",
            )?;
            self.bind(
                primal,
                DualReg {
                    re: dual,
                    du: dual + 1,
                },
            )?;
        }
        Ok(())
    }

    fn lower_tensor_binary(
        &mut self,
        dst_start: Reg,
        op: BinaryOp,
        operands: (StridedOperand, StridedOperand),
        count: usize,
        lanes: usize,
    ) -> Result<(), LowerError> {
        let (
            StridedOperand {
                start: lhs_start,
                stride: lhs_stride,
            },
            StridedOperand {
                start: rhs_start,
                stride: rhs_stride,
            },
        ) = operands;
        if lanes != 1 || lhs_stride > 1 || rhs_stride > 1 {
            return Err(unsupported(
                "forward AD expects a primal compact tensor binary with zero-or-one strides",
            ));
        }
        let lhs_count = if lhs_stride == 0 { 1 } else { count };
        let rhs_count = if rhs_stride == 0 { 1 } else { count };
        let lhs_start = self.pack_dual_register_range(lhs_start, lhs_count)?;
        let rhs_start = self.pack_dual_register_range(rhs_start, rhs_count)?;
        let dual_start = self.next_reg;
        for _ in 0..checked_ad_product(count, 2, self.span, "tensor binary dual output")? {
            self.alloc_reg()?;
        }
        self.ops.push(LinearOp::TensorBinary {
            dst_start: dual_start,
            op,
            lhs_start,
            rhs_start,
            count,
            lhs_stride,
            rhs_stride,
            lanes: 2,
        });
        for offset in 0..count {
            let primal =
                checked_ad_reg_offset(dst_start, offset, self.span, "tensor binary output")?;
            let dual = checked_ad_reg_offset(
                dual_start,
                checked_ad_product(offset, 2, self.span, "tensor binary dual lane")?,
                self.span,
                "tensor binary dual output",
            )?;
            self.bind(
                primal,
                DualReg {
                    re: dual,
                    du: dual + 1,
                },
            )?;
        }
        Ok(())
    }

    fn lower_tensor_cross(
        &mut self,
        dst_start: Reg,
        lhs_start: Reg,
        rhs_start: Reg,
        lanes: usize,
    ) -> Result<(), LowerError> {
        if lanes != 1 {
            return Err(unsupported(
                "forward AD expects a primal tensor cross product with one lane",
            ));
        }
        let lhs_start = self.pack_dual_register_range(lhs_start, 3)?;
        let rhs_start = self.pack_dual_register_range(rhs_start, 3)?;
        let dual_start = self.next_reg;
        for _ in 0..6 {
            self.alloc_reg()?;
        }
        self.ops.push(LinearOp::TensorCross {
            dst_start: dual_start,
            lhs_start,
            rhs_start,
            lanes: 2,
        });
        for offset in 0..3 {
            let primal =
                checked_ad_reg_offset(dst_start, offset, self.span, "tensor cross product output")?;
            let dual = checked_ad_reg_offset(
                dual_start,
                checked_ad_product(offset, 2, self.span, "tensor cross product dual lane")?,
                self.span,
                "tensor cross product dual output",
            )?;
            self.bind(
                primal,
                DualReg {
                    re: dual,
                    du: dual + 1,
                },
            )?;
        }
        Ok(())
    }

    fn lower_tensor_transpose(
        &mut self,
        dst_start: Reg,
        src_start: Reg,
        rows: usize,
        columns: usize,
        element_width: usize,
        lanes: usize,
    ) -> Result<(), LowerError> {
        if lanes != 1 {
            return Err(unsupported(
                "forward AD expects a primal tensor transpose with one lane",
            ));
        }
        let count = checked_ad_product(rows, columns, self.span, "tensor transpose")?;
        let count = checked_ad_product(
            count,
            element_width,
            self.span,
            "tensor transpose element width",
        )?;
        let src_start = self.pack_dual_register_range(src_start, count)?;
        let dual_start = self.next_reg;
        for _ in 0..checked_ad_product(count, 2, self.span, "tensor transpose dual output")? {
            self.alloc_reg()?;
        }
        self.ops.push(LinearOp::TensorTranspose {
            dst_start: dual_start,
            src_start,
            rows,
            columns,
            element_width,
            lanes: 2,
        });
        for offset in 0..count {
            let primal =
                checked_ad_reg_offset(dst_start, offset, self.span, "tensor transpose output")?;
            let dual = checked_ad_reg_offset(
                dual_start,
                checked_ad_product(offset, 2, self.span, "tensor transpose dual lane")?,
                self.span,
                "tensor transpose dual output",
            )?;
            self.bind(
                primal,
                DualReg {
                    re: dual,
                    du: dual + 1,
                },
            )?;
        }
        Ok(())
    }

    fn lower_tensor_concatenate(
        &mut self,
        dst_start: Reg,
        sources: &[rumoca_ir_solve::TensorConcatenateSource],
        dimensions: Box<[u32]>,
        axis: usize,
        lanes: usize,
    ) -> Result<(), LowerError> {
        if lanes != 1 {
            return Err(unsupported(
                "forward AD expects a primal tensor concatenate with one lane",
            ));
        }
        let mut dual_sources = Vec::with_capacity(sources.len());
        for source in sources.iter() {
            let count = source.dimensions.iter().try_fold(1usize, |count, extent| {
                checked_ad_product(
                    count,
                    *extent as usize,
                    self.span,
                    "tensor concatenate source",
                )
            })?;
            dual_sources.push(rumoca_ir_solve::TensorConcatenateSource {
                start: self.pack_dual_register_range(source.start, count)?,
                dimensions: source.dimensions.clone(),
            });
        }
        let count = dimensions.iter().try_fold(1usize, |count, extent| {
            checked_ad_product(
                count,
                *extent as usize,
                self.span,
                "tensor concatenate output",
            )
        })?;
        let dual_start = self.next_reg;
        for _ in 0..checked_ad_product(count, 2, self.span, "tensor concatenate dual output")? {
            self.alloc_reg()?;
        }
        self.ops.push(LinearOp::TensorConcatenate {
            dst_start: dual_start,
            sources: dual_sources.into_boxed_slice(),
            dimensions,
            axis,
            lanes: 2,
        });
        for offset in 0..count {
            let primal =
                checked_ad_reg_offset(dst_start, offset, self.span, "tensor concatenate output")?;
            let dual = checked_ad_reg_offset(
                dual_start,
                checked_ad_product(offset, 2, self.span, "tensor concatenate dual lane")?,
                self.span,
                "tensor concatenate dual output",
            )?;
            self.bind(
                primal,
                DualReg {
                    re: dual,
                    du: dual + 1,
                },
            )?;
        }
        Ok(())
    }

    // SPEC_0021: Exception - exhaustive tensor-update subscript AD dispatch.
    #[allow(clippy::excessive_nesting)]
    fn lower_tensor_update(
        &mut self,
        dst_start: Reg,
        base_start: Reg,
        value_start: Reg,
        dimensions: Box<[u32]>,
        subscripts: &[rumoca_ir_solve::TensorUpdateSubscript],
        lanes: usize,
    ) -> Result<(), LowerError> {
        if lanes != 1 {
            return Err(unsupported(
                "forward AD expects a primal tensor update with one lane",
            ));
        }
        let count = dimensions.iter().try_fold(1usize, |count, extent| {
            checked_ad_product(count, *extent as usize, self.span, "tensor update output")
        })?;
        let base_start = self.pack_dual_register_range(base_start, count)?;
        let mut value_count = 1usize;
        let mut dual_subscripts = Vec::with_capacity(subscripts.len());
        for (&extent, subscript) in dimensions.iter().zip(subscripts.iter()) {
            match subscript {
                rumoca_ir_solve::TensorUpdateSubscript::Whole => {
                    value_count = checked_ad_product(
                        value_count,
                        extent as usize,
                        self.span,
                        "tensor update value",
                    )?;
                    dual_subscripts.push(rumoca_ir_solve::TensorUpdateSubscript::Whole);
                }
                rumoca_ir_solve::TensorUpdateSubscript::Index(
                    rumoca_ir_solve::TensorIndex::Constant(coordinate),
                ) => dual_subscripts.push(rumoca_ir_solve::TensorUpdateSubscript::Index(
                    rumoca_ir_solve::TensorIndex::Constant(*coordinate),
                )),
                rumoca_ir_solve::TensorUpdateSubscript::Index(
                    rumoca_ir_solve::TensorIndex::Runtime(register),
                ) => dual_subscripts.push(rumoca_ir_solve::TensorUpdateSubscript::Index(
                    rumoca_ir_solve::TensorIndex::Runtime(self.lookup(*register)?.re),
                )),
                rumoca_ir_solve::TensorUpdateSubscript::Slice { start, dimensions } => {
                    let slice_count = dimensions.iter().try_fold(1usize, |count, extent| {
                        checked_ad_product(
                            count,
                            *extent as usize,
                            self.span,
                            "tensor update slice",
                        )
                    })?;
                    value_count = checked_ad_product(
                        value_count,
                        slice_count,
                        self.span,
                        "tensor update sliced value",
                    )?;
                    let mut primal = Vec::with_capacity(slice_count);
                    for offset in 0..slice_count {
                        let register = checked_ad_reg_offset(
                            *start,
                            offset,
                            self.span,
                            "tensor update slice",
                        )?;
                        primal.push(self.lookup(register)?.re);
                    }
                    dual_subscripts.push(rumoca_ir_solve::TensorUpdateSubscript::Slice {
                        start: self.pack_registers(&primal)?,
                        dimensions: dimensions.clone(),
                    });
                }
            }
        }
        let value_start = self.pack_dual_register_range(value_start, value_count)?;
        let dual_start = self.next_reg;
        for _ in 0..checked_ad_product(count, 2, self.span, "tensor update dual output")? {
            self.alloc_reg()?;
        }
        self.ops.push(LinearOp::TensorUpdate {
            dst_start: dual_start,
            base_start,
            value_start,
            dimensions,
            subscripts: dual_subscripts.into_boxed_slice(),
            lanes: 2,
        });
        for offset in 0..count {
            let primal =
                checked_ad_reg_offset(dst_start, offset, self.span, "tensor update output")?;
            let dual = checked_ad_reg_offset(
                dual_start,
                checked_ad_product(offset, 2, self.span, "tensor update dual lane")?,
                self.span,
                "tensor update dual output",
            )?;
            self.bind(
                primal,
                DualReg {
                    re: dual,
                    du: dual + 1,
                },
            )?;
        }
        Ok(())
    }

    fn lower_tensor_fill(
        &mut self,
        dst_start: Reg,
        value_start: Reg,
        count: usize,
        lanes: usize,
    ) -> Result<(), LowerError> {
        if lanes != 1 {
            return Err(unsupported(
                "forward AD expects a primal tensor fill with one lane",
            ));
        }
        let value = self.lookup(value_start)?;
        let value_start = self.pack_registers(&[value.re, value.du])?;
        let dual_start = self.next_reg;
        for _ in 0..checked_ad_product(count, 2, self.span, "tensor fill dual output")? {
            self.alloc_reg()?;
        }
        self.ops.push(LinearOp::TensorFill {
            dst_start: dual_start,
            value_start,
            count,
            lanes: 2,
        });
        for offset in 0..count {
            let primal = checked_ad_reg_offset(dst_start, offset, self.span, "tensor fill output")?;
            let dual = checked_ad_reg_offset(
                dual_start,
                checked_ad_product(offset, 2, self.span, "tensor fill dual lane")?,
                self.span,
                "tensor fill dual output",
            )?;
            self.bind(
                primal,
                DualReg {
                    re: dual,
                    du: dual + 1,
                },
            )?;
        }
        Ok(())
    }

    fn lower_tensor_identity(
        &mut self,
        dst_start: Reg,
        size: usize,
        lanes: usize,
    ) -> Result<(), LowerError> {
        if lanes != 1 {
            return Err(unsupported(
                "forward AD expects a primal tensor identity with one lane",
            ));
        }
        let count = checked_ad_product(size, size, self.span, "tensor identity")?;
        let dual_start = self.next_reg;
        for _ in 0..checked_ad_product(count, 2, self.span, "tensor identity dual output")? {
            self.alloc_reg()?;
        }
        self.ops.push(LinearOp::TensorIdentity {
            dst_start: dual_start,
            size,
            lanes: 2,
        });
        for offset in 0..count {
            let primal =
                checked_ad_reg_offset(dst_start, offset, self.span, "tensor identity output")?;
            let dual = checked_ad_reg_offset(
                dual_start,
                checked_ad_product(offset, 2, self.span, "tensor identity dual lane")?,
                self.span,
                "tensor identity dual output",
            )?;
            self.bind(
                primal,
                DualReg {
                    re: dual,
                    du: dual + 1,
                },
            )?;
        }
        Ok(())
    }

    fn lower_tensor_load(
        &mut self,
        dst_start: Reg,
        input: rumoca_ir_solve::TensorInputKind,
        input_start: usize,
        count: usize,
        seed_start: Option<usize>,
        lanes: usize,
    ) -> Result<(), LowerError> {
        if lanes != 1 || seed_start.is_some() {
            return Err(unsupported(
                "forward AD expects a primal tensor load with one lane and no seed",
            ));
        }
        let seed_start = match (input, self.seed_mode) {
            (rumoca_ir_solve::TensorInputKind::Y, _) => Some(input_start),
            (rumoca_ir_solve::TensorInputKind::P, SeedMode::SolverYOnly) => None,
            (rumoca_ir_solve::TensorInputKind::P, SeedMode::SolverYAndP { .. }) => {
                Some(self.p_seed_index(input_start)?)
            }
        };
        let dual_start = self.next_reg;
        for _ in 0..checked_ad_product(count, 2, self.span, "tensor load dual output")? {
            self.alloc_reg()?;
        }
        self.ops.push(LinearOp::TensorLoad {
            dst_start: dual_start,
            input,
            input_start,
            count,
            seed_start,
            lanes: 2,
        });
        for offset in 0..count {
            let primal = checked_ad_reg_offset(dst_start, offset, self.span, "tensor load output")?;
            let dual = checked_ad_reg_offset(
                dual_start,
                checked_ad_product(offset, 2, self.span, "tensor load dual lane")?,
                self.span,
                "tensor load dual output",
            )?;
            self.bind(
                primal,
                DualReg {
                    re: dual,
                    du: dual + 1,
                },
            )?;
        }
        Ok(())
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
        match self.store_output_mode {
            StoreOutputMode::Derivative => {
                self.ops.push(LinearOp::StoreOutput { src: d.du });
            }
            StoreOutputMode::Primal => {
                self.ops.push(LinearOp::StoreOutput { src: d.re });
            }
            StoreOutputMode::Dual => {
                self.ops.push(LinearOp::StoreOutput { src: d.re });
                self.ops.push(LinearOp::StoreOutput { src: d.du });
            }
        }
        Ok(())
    }

    // SPEC_0021: Exception - exhaustive output-range AD source dispatch.
    #[allow(clippy::excessive_nesting)]
    fn lower_store_range(
        &mut self,
        start: Reg,
        count: usize,
        stride: usize,
    ) -> Result<(), LowerError> {
        let mut sources = Vec::with_capacity(match self.store_output_mode {
            StoreOutputMode::Dual => count.saturating_mul(2),
            StoreOutputMode::Derivative | StoreOutputMode::Primal => count,
        });
        for ordinal in 0..count {
            let offset = ordinal
                .checked_mul(stride)
                .and_then(|offset| Reg::try_from(offset).ok())
                .ok_or_else(|| unsupported("function-conditional AD output range overflow"))?;
            let dual = self.lookup(start.checked_add(offset).ok_or_else(|| {
                unsupported("function-conditional AD output register overflow")
            })?)?;
            match self.store_output_mode {
                StoreOutputMode::Derivative => sources.push(dual.du),
                StoreOutputMode::Primal => sources.push(dual.re),
                StoreOutputMode::Dual => sources.extend([dual.re, dual.du]),
            }
        }
        let (&first, rest) = sources
            .split_first()
            .ok_or_else(|| unsupported("function-conditional AD output range is empty"))?;
        let derived_stride = rest
            .first()
            .map_or(1, |second| second.checked_sub(first).unwrap_or(0) as usize);
        if derived_stride == 0
            || sources
                .iter()
                .copied()
                .enumerate()
                .any(|(ordinal, register)| {
                    usize::try_from(first)
                        .ok()
                        .and_then(|base| base.checked_add(ordinal.saturating_mul(derived_stride)))
                        != usize::try_from(register).ok()
                })
        {
            let packed_start = self.next_reg;
            for source in &sources {
                let destination = self.alloc_reg()?;
                self.ops.push(LinearOp::Move {
                    dst: destination,
                    src: *source,
                });
            }
            self.ops.push(LinearOp::StoreOutputRange {
                start: packed_start,
                count: sources.len(),
                stride: 1,
            });
            return Ok(());
        }
        self.ops.push(LinearOp::StoreOutputRange {
            start: first,
            count: sources.len(),
            stride: derived_stride,
        });
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
        if let Some(&start) = regs.first()
            && regs.iter().copied().enumerate().all(|(offset, register)| {
                u32::try_from(offset)
                    .ok()
                    .and_then(|offset| start.checked_add(offset))
                    == Some(register)
            })
        {
            return Ok(start);
        }
        let start = self.next_reg;
        for &src in regs {
            let dst = self.alloc_reg()?;
            self.ops.push(LinearOp::Move { dst, src });
        }
        Ok(start)
    }

    fn pack_dual_register_range(
        &mut self,
        primal_start: Reg,
        count: usize,
    ) -> Result<Reg, LowerError> {
        if count == 0 {
            return Ok(self.next_reg);
        }
        let first = self.lookup(primal_start)?;
        let already_interleaved = first.du == first.re.saturating_add(1)
            && (0..count).all(|offset| {
                let Ok(primal) =
                    checked_ad_reg_offset(primal_start, offset, self.span, "dual register range")
                else {
                    return false;
                };
                let Ok(value) = self.lookup(primal) else {
                    return false;
                };
                let Ok(lane) = u32::try_from(offset.saturating_mul(2)) else {
                    return false;
                };
                value.re == first.re.saturating_add(lane)
                    && value.du == first.re.saturating_add(lane).saturating_add(1)
            });
        if already_interleaved {
            return Ok(first.re);
        }
        let capacity = checked_ad_product(count, 2, self.span, "dual register range")?;
        let mut registers = ad_vec_with_capacity(capacity, "dual register range", self.span)?;
        for offset in 0..count {
            let primal =
                checked_ad_reg_offset(primal_start, offset, self.span, "dual register range")?;
            let value = self.lookup(primal)?;
            registers.extend([value.re, value.du]);
        }
        self.pack_registers(&registers)
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
    LowerError::UnspannedContractViolation {
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
mod output_projection_tests {
    use super::*;

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
