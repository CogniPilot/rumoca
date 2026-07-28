use super::*;

#[derive(Clone)]
pub(super) struct PreparedLinearOps {
    pub(super) ops: Vec<LinearOp>,
    pub(super) register_count: usize,
    pub(super) register_safe: bool,
    pub(super) requirements: RowInputRequirements,
}

impl PreparedLinearOps {
    pub(super) fn new(ops: Vec<LinearOp>) -> Result<Self, EvalSolveError> {
        let requirements = row_input_requirements(&ops)?;
        Self::new_with_requirements(ops, requirements)
    }

    pub(super) fn new_with_requirements(
        ops: Vec<LinearOp>,
        requirements: RowInputRequirements,
    ) -> Result<Self, EvalSolveError> {
        Ok(Self {
            register_count: required_registers(&ops)?,
            register_safe: row_register_flow_is_valid(&ops)?,
            requirements,
            ops,
        })
    }

    pub(super) fn eval(
        &self,
        y: &[f64],
        p: &[f64],
        t: f64,
        context: RowEvalContext<'_>,
        scratch: &mut RowEvalScratch,
    ) -> Result<(), EvalSolveError> {
        // Operand setup ops compute matrix/rhs entries into the register file
        // and contain no `StoreOutput`; the matmul/linsolve kernel reads the
        // registers afterward. The single-output helper drives the op loop and
        // its (unused) return value is discarded.
        eval_program_single(
            PreparedRowEval::new(&self.ops, self.register_count, y, p, t, context),
            self.register_safe,
            scratch,
        )?;
        Ok(())
    }
}

#[derive(Clone, Copy)]
pub(super) struct MatMulEvalSpec {
    pub(super) lhs_start: usize,
    pub(super) rhs_start: usize,
    pub(super) m: usize,
    pub(super) k: usize,
    pub(super) n: usize,
    pub(super) kernel: MatMulKernel,
}

pub(super) fn eval_matmul_with_policy(
    regs: &[f64],
    spec: MatMulEvalSpec,
    out: &mut [f64],
) -> Result<(), EvalSolveError> {
    let MatMulEvalSpec {
        lhs_start,
        rhs_start,
        m,
        k,
        n,
        kernel,
    } = spec;
    let output_len = m
        .checked_mul(n)
        .ok_or_else(|| EvalSolveError::Scalarization {
            message: format!("matmul output shape {m}x{n} overflows output vector length"),
            span: None,
        })?;
    validate_output_len(out, output_len)?;
    match kernel {
        MatMulKernel::DiagonalLeft => {
            return eval_left_diagonal_matmul(regs, lhs_start, rhs_start, m, n, out);
        }
        MatMulKernel::DiagonalRight => {
            return eval_right_diagonal_matmul(regs, lhs_start, rhs_start, m, k, out);
        }
        MatMulKernel::SmallDense | MatMulKernel::Dense | MatMulKernel::SparseCandidate => {}
    }
    for row in 0..m {
        for col in 0..n {
            let mut sum = 0.0;
            for inner in 0..k {
                sum += regs[lhs_start + row * k + inner] * regs[rhs_start + inner * n + col];
            }
            out[row * n + col] = sum;
        }
    }
    Ok(())
}

pub(super) fn checked_product(
    lhs: usize,
    rhs: usize,
    kind: &'static str,
    span: rumoca_core::Span,
) -> Result<usize, crate::ScalarizeError> {
    lhs.checked_mul(rhs)
        .ok_or(crate::ScalarizeError::ProductOverflow {
            kind,
            lhs,
            rhs,
            span,
        })
}

pub(super) fn eval_left_diagonal_matmul(
    regs: &[f64],
    lhs_start: usize,
    rhs_start: usize,
    m: usize,
    n: usize,
    out: &mut [f64],
) -> Result<(), EvalSolveError> {
    for row in 0..m {
        let scale = regs[lhs_start + row * m + row];
        for col in 0..n {
            out[row * n + col] = scale * regs[rhs_start + row * n + col];
        }
    }
    Ok(())
}

pub(super) fn eval_right_diagonal_matmul(
    regs: &[f64],
    lhs_start: usize,
    rhs_start: usize,
    m: usize,
    k: usize,
    out: &mut [f64],
) -> Result<(), EvalSolveError> {
    for row in 0..m {
        for col in 0..k {
            out[row * k + col] = regs[lhs_start + row * k + col] * regs[rhs_start + col * k + col];
        }
    }
    Ok(())
}

pub(super) fn ensure_register_range(
    regs: &[f64],
    access: &'static str,
    start: u32,
    len: usize,
) -> Result<(), EvalSolveError> {
    let start_index = start as usize;
    if start_index
        .checked_add(len)
        .is_some_and(|end| end <= regs.len())
    {
        return Ok(());
    }
    Err(EvalSolveError::RegisterOutOfBounds {
        access,
        register: checked_register_range_last(start, len)?,
        len: regs.len(),
        span: None,
    })
}

pub(super) fn checked_required_row_count(row_idx: usize) -> Result<usize, EvalSolveError> {
    row_idx
        .checked_add(1)
        .ok_or_else(|| invalid_prepared_row("row index overflows row count"))
}

pub(super) fn checked_register_range_last(start: u32, len: usize) -> Result<u32, EvalSolveError> {
    let Some(offset) = len.checked_sub(1) else {
        return Ok(start);
    };
    let offset = u32::try_from(offset).map_err(|_| {
        invalid_prepared_row(format!(
            "register range offset {offset} exceeds register index type"
        ))
    })?;
    start.checked_add(offset).ok_or_else(|| {
        invalid_prepared_row(format!("register range starting at {start} overflows"))
    })
}

pub(super) fn prepare_row_output_metadata(
    block: &ScalarProgramBlock,
    output_count: usize,
) -> Result<PreparedRowOutputMetadata, EvalSolveError> {
    let span = block.program_span(0);
    let offset_count = checked_prepared_sum(
        block.row_count(),
        1,
        "prepared row output offset count",
        span,
    )?;
    let mut offsets =
        prepared_vec_with_capacity(offset_count, "prepared row output offsets", span)?;
    offsets.push(0usize);
    for row in &block.programs {
        let next = checked_prepared_sum(
            *offsets.last().unwrap_or(&0),
            ScalarProgramBlock::program_output_count(row),
            "prepared row stored output count",
            span,
        )?;
        offsets.push(next);
    }

    if offsets.last().copied() != Some(block.output_indices.len()) {
        return Err(invalid_prepared_row_with_span(
            format!(
                "prepared block has {} stored outputs but {} logical output indices",
                offsets.last().copied().unwrap_or(0),
                block.output_indices.len()
            ),
            span,
        ));
    }
    let mut owners =
        prepared_vec_with_capacity(output_count, "prepared single-output ownership", span)?;
    owners.resize(output_count, PreparedOutputOwner::Unseen);
    for (row_idx, range) in offsets.windows(2).enumerate() {
        let row_output_count = range[1] - range[0];
        for stored_ordinal in range[0]..range[1] {
            let output_index = *block.output_indices.get(stored_ordinal).ok_or_else(|| {
                invalid_prepared_row_with_span(
                    format!(
                        "prepared row output ordinal {stored_ordinal} has no logical output index"
                    ),
                    block.program_span(row_idx),
                )
            })?;
            let owner = owners.get_mut(output_index).ok_or_else(|| {
                invalid_prepared_row_with_span(
                    format!("logical output index {output_index} exceeds prepared output count"),
                    block.program_span(row_idx),
                )
            })?;
            *owner = match (*owner, row_output_count) {
                (PreparedOutputOwner::Unseen, 1) => PreparedOutputOwner::Single(row_idx),
                _ => PreparedOutputOwner::Ambiguous,
            };
        }
    }
    let single_output_rows = owners
        .into_iter()
        .map(|owner| match owner {
            PreparedOutputOwner::Single(row_idx) => Some(row_idx),
            PreparedOutputOwner::Unseen | PreparedOutputOwner::Ambiguous => None,
        })
        .collect();
    Ok(PreparedRowOutputMetadata {
        offsets,
        single_rows: single_output_rows,
    })
}

#[derive(Clone)]
pub(super) struct PreparedRowOutputMetadata {
    pub(super) offsets: Vec<usize>,
    pub(super) single_rows: Vec<Option<usize>>,
}

#[derive(Clone, Copy)]
enum PreparedOutputOwner {
    Unseen,
    Single(usize),
    Ambiguous,
}

pub(super) fn checked_prepared_output_count(
    block: &ScalarProgramBlock,
) -> Result<usize, EvalSolveError> {
    block
        .output_indices
        .iter()
        .copied()
        .max()
        .map_or(Ok(0), |index| {
            checked_prepared_sum(
                index,
                1,
                "prepared logical output count",
                block.program_span(0),
            )
        })
}

pub(super) fn non_causal_linear_op(op: &LinearOp) -> bool {
    matches!(
        op,
        LinearOp::LoadSeed { .. }
            | LinearOp::LoadIndexedSeed { .. }
            | LinearOp::RandomInitialState { .. }
            | LinearOp::RandomResult { .. }
            | LinearOp::RandomState { .. }
            | LinearOp::ImpureRandomInit { .. }
            | LinearOp::ImpureRandom { .. }
            | LinearOp::ImpureRandomInteger { .. }
    )
}

pub(super) fn prepared_seed_loads(
    row: &[LinearOp],
    span: Option<rumoca_core::Span>,
) -> Result<Box<[PreparedSeedLoad]>, EvalSolveError> {
    let count = row
        .iter()
        .filter(|op| {
            matches!(
                op,
                LinearOp::LoadSeed { .. } | LinearOp::LoadIndexedSeed { .. }
            )
        })
        .count();
    let mut loads = prepared_vec_with_capacity(count, "prepared row seed loads", span)?;
    for op in row {
        match *op {
            LinearOp::LoadSeed { index, .. } => loads.push(PreparedSeedLoad::Direct(index)),
            LinearOp::LoadIndexedSeed { base, count, .. } => {
                loads.push(PreparedSeedLoad::Indexed { base, count });
            }
            _ => {}
        }
    }
    Ok(loads.into_boxed_slice())
}

pub(super) fn prepared_vec_with_capacity<T>(
    capacity: usize,
    context: &'static str,
    span: Option<rumoca_core::Span>,
) -> Result<Vec<T>, EvalSolveError> {
    let mut values = Vec::new();
    values.try_reserve_exact(capacity).map_err(|_| {
        invalid_prepared_row_with_span(format!("{context} exceeds host memory limits"), span)
    })?;
    Ok(values)
}

pub(super) fn reserve_prepared_vec_capacity<T>(
    values: &mut Vec<T>,
    capacity: usize,
    context: &'static str,
    span: Option<rumoca_core::Span>,
) -> Result<(), EvalSolveError> {
    if values.capacity() >= capacity {
        return Ok(());
    }
    values
        .try_reserve_exact(capacity - values.capacity())
        .map_err(|_| {
            invalid_prepared_row_with_span(format!("{context} exceeds host memory limits"), span)
        })
}

pub(super) fn checked_prepared_sum(
    lhs: usize,
    rhs: usize,
    context: &'static str,
    span: Option<rumoca_core::Span>,
) -> Result<usize, EvalSolveError> {
    lhs.checked_add(rhs).ok_or_else(|| {
        invalid_prepared_row_with_span(format!("{context} overflows host index range"), span)
    })
}

pub(super) fn first_compute_node_span(block: &ComputeBlock) -> Option<rumoca_core::Span> {
    block.nodes.iter().find_map(compute_node_span)
}

pub(super) fn compute_node_span(node: &ComputeNode) -> Option<rumoca_core::Span> {
    match node {
        ComputeNode::ScalarPrograms(block) => block.program_span(0),
        ComputeNode::MatMul { span, .. }
        | ComputeNode::LinSolve { span, .. }
        | ComputeNode::Map { span, .. }
        | ComputeNode::AffineStencil { span, .. } => Some(*span),
    }
}

pub(super) fn invalid_prepared_row(message: impl Into<String>) -> EvalSolveError {
    invalid_prepared_row_with_span(message, None)
}

pub(super) fn invalid_prepared_row_with_span(
    message: impl Into<String>,
    span: Option<rumoca_core::Span>,
) -> EvalSolveError {
    EvalSolveError::InvalidRow {
        message: message.into(),
        span,
    }
}
