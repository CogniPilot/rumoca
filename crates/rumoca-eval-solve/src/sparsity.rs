use std::collections::BTreeSet;

use rumoca_core::Span;
use rumoca_ir_solve::{
    BinaryOp, ComputeBlock, LinearOp, PatternDerivation, PatternProvenance, Reg,
    ScalarProgramBlock, StructuralPattern,
};

use crate::{EvalSolveError, to_scalar_program_block};

#[derive(Clone, Debug, PartialEq, Eq)]
enum DependencyState {
    Known(BTreeSet<usize>),
    Unknown,
}

#[derive(Clone, Copy)]
enum DependencySource {
    Seed,
    SolverY,
}

impl DependencyState {
    fn empty() -> Self {
        Self::Known(BTreeSet::new())
    }

    fn singleton(index: usize) -> Self {
        Self::Known(BTreeSet::from([index]))
    }

    fn union(self, other: Self) -> Self {
        match (self, other) {
            (Self::Known(mut lhs), Self::Known(rhs)) => {
                lhs.extend(rhs);
                Self::Known(lhs)
            }
            (Self::Known(_), Self::Unknown)
            | (Self::Unknown, Self::Known(_))
            | (Self::Unknown, Self::Unknown) => Self::Unknown,
        }
    }

    fn into_conservative_set(self, columns: usize) -> BTreeSet<usize> {
        match self {
            Self::Known(indices) => indices,
            Self::Unknown => (0..columns).collect(),
        }
    }
}

/// Derive structural Jacobian dependencies from a JVP compute block.
pub fn derive_jacobian_pattern_from_jvp(
    block: &ComputeBlock,
    rows: usize,
    columns: usize,
    owner_span: Span,
) -> Result<StructuralPattern, EvalSolveError> {
    let scalar = to_scalar_program_block(block)?;
    derive_jacobian_pattern_from_scalar_jvp(&scalar, rows, columns, owner_span)
}

/// Derive structural Jacobian dependencies from a checked scalar JVP block.
pub fn derive_jacobian_pattern_from_scalar_jvp(
    block: &ScalarProgramBlock,
    rows: usize,
    columns: usize,
    owner_span: Span,
) -> Result<StructuralPattern, EvalSolveError> {
    if owner_span.is_dummy() {
        return Err(sparsity_error(
            "Jacobian sparsity requires source-backed owner provenance",
            None,
        ));
    }
    if block.output_count() != rows {
        return Err(sparsity_error(
            format!(
                "Jacobian row extent {rows} does not match checked sparse output extent {}",
                block.output_count()
            ),
            Some(owner_span),
        ));
    }

    let mut row_dependencies = vec![None; rows];
    let mut output_ordinal = 0usize;
    for (program_index, program) in block.programs().iter().enumerate() {
        let span = block.program_span(program_index).or(Some(owner_span));
        for dependencies in program_output_dependencies(program, span)? {
            let output_index = *block.output_indices().get(output_ordinal).ok_or_else(|| {
                sparsity_error(
                    format!(
                        "Jacobian sparsity output {output_ordinal} has no checked output identity"
                    ),
                    span,
                )
            })?;
            let slot = row_dependencies.get_mut(output_index).ok_or_else(|| {
                sparsity_error(
                    format!("Jacobian sparsity output index {output_index} is outside 0..{rows}"),
                    span,
                )
            })?;
            if slot.is_some() {
                return Err(sparsity_error(
                    format!("Jacobian sparsity output index {output_index} is produced twice"),
                    span,
                ));
            }
            let dependencies = dependencies.into_conservative_set(columns);
            if let Some(index) = dependencies.iter().find(|index| **index >= columns) {
                return Err(sparsity_error(
                    format!("Jacobian seed index {index} is outside 0..{columns}"),
                    span,
                ));
            }
            *slot = Some(dependencies.into_iter().collect());
            output_ordinal = output_ordinal.checked_add(1).ok_or_else(|| {
                sparsity_error("Jacobian output ordinal overflows host index range", span)
            })?;
        }
    }
    if output_ordinal != block.output_indices().len() {
        return Err(sparsity_error(
            format!(
                "Jacobian emitted {output_ordinal} outputs but carries {} output identities",
                block.output_indices().len()
            ),
            Some(owner_span),
        ));
    }
    let row_dependencies = row_dependencies
        .into_iter()
        // An interior hole is explicitly identified by the checked sparse
        // output map and therefore has no producing operation or edge.
        .map(Option::unwrap_or_default)
        .collect::<Vec<_>>();
    let provenance =
        PatternProvenance::derived(PatternDerivation::DependencyPropagation, owner_span)
            .map_err(|error| sparsity_error(error.to_string(), Some(owner_span)))?;
    StructuralPattern::from_row_dependencies(rows, columns, &row_dependencies, provenance)
        .map_err(|error| sparsity_error(error.to_string(), Some(owner_span)))
}

/// Derive a deterministic greedy coloring from a checked pattern.
pub fn derive_column_coloring(pattern: &StructuralPattern) -> rumoca_ir_solve::ColumnColoring {
    pattern.column_coloring()
}

/// Reconstruct all derived structural artifacts from canonical Solve programs.
///
/// This is the sole decode/runtime reconstruction path; canonical wire data
/// does not carry the derived patterns themselves.
pub fn derive_solve_structural_artifacts(
    problem: &rumoca_ir_solve::SolveProblem,
    artifacts: &rumoca_ir_solve::SolveArtifacts,
) -> Result<
    (
        rumoca_ir_solve::ContinuousStructuralArtifacts,
        rumoca_ir_solve::InitializationStructuralArtifacts,
    ),
    EvalSolveError,
> {
    let solver_columns = problem.solve_layout.solver_scalar_count();
    let full_columns = problem
        .layout
        .y_scalars()
        .checked_add(problem.layout.p_scalars())
        .ok_or_else(|| {
            sparsity_error(
                "continuous full Jacobian column count overflows host index range",
                None,
            )
        })?;
    let implicit = derive_optional_compute_pattern(
        &artifacts.continuous.implicit_jacobian_v,
        problem.continuous.implicit_rhs.len()?,
        solver_columns,
    )?;
    let manifold = derive_optional_compute_pattern(
        &artifacts.continuous.manifold_jacobian_v,
        problem.continuous.manifold_residual.len()?,
        solver_columns,
    )?;
    let algebraic_projection = derive_y_projection_patterns(
        implicit.as_ref(),
        &problem.continuous.algebraic_projection_plan,
    )?;
    let algebraic_invalidates_earlier = derive_algebraic_reverse_invalidations(
        implicit.as_ref(),
        &problem.continuous.algebraic_projection_plan,
    )?;
    let manifold_projection = derive_y_projection_patterns(
        manifold.as_ref(),
        &problem.continuous.manifold_projection_plan,
    )?;
    let continuous = rumoca_ir_solve::ContinuousStructuralArtifacts::derived(
        implicit,
        algebraic_projection,
        algebraic_invalidates_earlier,
        manifold,
        manifold_projection,
        derive_optional_scalar_pattern(
            &artifacts.continuous.full_jacobian_v,
            problem.continuous.derivative_rhs.len()?,
            full_columns,
        )?,
    );
    let initialization_columns = solver_columns
        .checked_add(problem.layout.p_scalars())
        .ok_or_else(|| {
            sparsity_error(
                "initialization Jacobian column count overflows host index range",
                None,
            )
        })?;
    let initialization_residual = derive_optional_compute_pattern(
        &artifacts.initialization.residual_jacobian_v,
        problem.initialization.residual.len()?,
        initialization_columns,
    )?;
    let initialization_projection = derive_initial_projection_patterns(
        initialization_residual.as_ref(),
        &problem.initialization.projection_plan,
        solver_columns,
    )?;
    let initialization = rumoca_ir_solve::InitializationStructuralArtifacts::derived(
        initialization_residual,
        initialization_projection,
    );
    Ok((continuous, initialization))
}

fn derive_algebraic_reverse_invalidations(
    source: Option<&StructuralPattern>,
    plan: &rumoca_ir_solve::AlgebraicProjectionPlan,
) -> Result<Vec<bool>, EvalSolveError> {
    let Some(source) = source else {
        return Ok(Vec::new());
    };
    let column_rows = source.column_rows();
    let mut earlier_rows = vec![false; source.rows() as usize];
    let mut invalidations = Vec::with_capacity(plan.blocks.len());
    for block in &plan.blocks {
        let invalidates =
            block
                .y_indices
                .iter()
                .copied()
                .try_fold(false, |invalidates, column| {
                    let affected_rows = column_rows.get(column).ok_or_else(|| {
                        sparsity_error(
                            format!(
                                "projection invalidation column {column} is outside 0..{}",
                                source.columns()
                            ),
                            Some(source.provenance().span()),
                        )
                    })?;
                    Ok::<_, EvalSolveError>(
                        invalidates || affected_rows.iter().any(|&row| earlier_rows[row]),
                    )
                })?;
        invalidations.push(invalidates);
        for &row in &block.rows {
            let row_count = earlier_rows.len();
            let Some(earlier) = earlier_rows.get_mut(row) else {
                return Err(sparsity_error(
                    format!("projection invalidation row {row} is outside 0..{row_count}"),
                    Some(source.provenance().span()),
                ));
            };
            *earlier = true;
        }
    }
    Ok(invalidations)
}

fn derive_y_projection_patterns(
    source: Option<&StructuralPattern>,
    plan: &rumoca_ir_solve::AlgebraicProjectionPlan,
) -> Result<Vec<StructuralPattern>, EvalSolveError> {
    if source.is_none() && !plan.blocks.is_empty() {
        return Err(sparsity_error(
            "projection plan has blocks but its Jacobian structure is unavailable",
            None,
        ));
    }
    derive_projection_patterns(
        source,
        plan.blocks
            .iter()
            .map(|block| (block.rows.as_slice(), block.y_indices.clone())),
    )
}

fn derive_initial_projection_patterns(
    source: Option<&StructuralPattern>,
    plan: &rumoca_ir_solve::InitializationProjectionPlan,
    solver_columns: usize,
) -> Result<Vec<StructuralPattern>, EvalSolveError> {
    if source.is_none() && !plan.blocks.is_empty() {
        return Err(sparsity_error(
            "initial projection plan has blocks but its Jacobian structure is unavailable",
            None,
        ));
    }
    let Some(source) = source else {
        return Ok(Vec::new());
    };
    plan.blocks
        .iter()
        .map(|block| {
            let columns = initial_projection_columns(&block.unknowns, solver_columns, source)?;
            derive_projection_pattern(source, &block.rows, &columns)
        })
        .collect()
}

fn initial_projection_columns(
    unknowns: &[rumoca_ir_solve::ScalarSlot],
    solver_columns: usize,
    source: &StructuralPattern,
) -> Result<Vec<usize>, EvalSolveError> {
    let span = source.provenance().span();
    unknowns
        .iter()
        .map(|slot| match *slot {
            rumoca_ir_solve::ScalarSlot::Y { index, .. } => Ok(index),
            rumoca_ir_solve::ScalarSlot::P { index, .. } => solver_columns
                .checked_add(index)
                .ok_or_else(|| sparsity_error("initial projection column overflows", Some(span))),
            _ => Err(sparsity_error(
                format!("initial projection unknown {slot:?} is neither Y nor P storage"),
                Some(span),
            )),
        })
        .collect()
}

fn derive_projection_patterns<'a>(
    source: Option<&StructuralPattern>,
    blocks: impl Iterator<Item = (&'a [usize], Vec<usize>)>,
) -> Result<Vec<StructuralPattern>, EvalSolveError> {
    let Some(source) = source else {
        return Ok(Vec::new());
    };
    blocks
        .map(|(rows, columns)| derive_projection_pattern(source, rows, &columns))
        .collect()
}

fn derive_projection_pattern(
    source: &StructuralPattern,
    rows: &[usize],
    columns: &[usize],
) -> Result<StructuralPattern, EvalSolveError> {
    let span = source.provenance().span();
    let mut dependencies = Vec::with_capacity(rows.len());
    for &row in rows {
        let row = u32::try_from(row)
            .map_err(|_| sparsity_error("projection row exceeds u32", Some(span)))?;
        if row >= source.rows() {
            return Err(sparsity_error(
                format!("projection row {row} is outside 0..{}", source.rows()),
                Some(span),
            ));
        }
        let mut local = Vec::new();
        for (local_column, &source_column) in columns.iter().enumerate() {
            let source_column = u32::try_from(source_column)
                .map_err(|_| sparsity_error("projection column exceeds u32", Some(span)))?;
            if source_column >= source.columns() {
                return Err(sparsity_error(
                    format!(
                        "projection column {source_column} is outside 0..{}",
                        source.columns()
                    ),
                    Some(span),
                ));
            }
            if source.contains(row, source_column) {
                local.push(local_column);
            }
        }
        dependencies.push(local);
    }
    StructuralPattern::from_row_dependencies(
        rows.len(),
        columns.len(),
        &dependencies,
        source.provenance(),
    )
    .map_err(|error| sparsity_error(error.to_string(), Some(span)))
}

fn derive_optional_compute_pattern(
    block: &ComputeBlock,
    rows: usize,
    columns: usize,
) -> Result<Option<StructuralPattern>, EvalSolveError> {
    if rows == 0 {
        return Ok(None);
    }
    let span = compute_block_span(block).ok_or_else(|| {
        sparsity_error(
            "non-empty Jacobian program has no source-backed semantic owner",
            None,
        )
    })?;
    derive_jacobian_pattern_from_jvp(block, rows, columns, span).map(Some)
}

fn derive_optional_scalar_pattern(
    block: &ScalarProgramBlock,
    rows: usize,
    columns: usize,
) -> Result<Option<StructuralPattern>, EvalSolveError> {
    if rows == 0 {
        return Ok(None);
    }
    let span = block.first_source_span().ok_or_else(|| {
        sparsity_error(
            "non-empty scalar Jacobian program has no source-backed semantic owner",
            None,
        )
    })?;
    derive_jacobian_pattern_from_scalar_jvp(block, rows, columns, span).map(Some)
}

fn compute_block_span(block: &ComputeBlock) -> Option<Span> {
    block.nodes.iter().find_map(|node| match node {
        rumoca_ir_solve::ComputeNode::ScalarPrograms(programs) => programs.first_source_span(),
        rumoca_ir_solve::ComputeNode::MatMul { span, .. }
        | rumoca_ir_solve::ComputeNode::LinSolve { span, .. }
        | rumoca_ir_solve::ComputeNode::Map { span, .. }
        | rumoca_ir_solve::ComputeNode::AffineStencil { span, .. } => {
            (!span.is_dummy()).then_some(*span)
        }
    })
}

/// Dependencies of the sole output of a scalar row.
pub fn row_seed_dependencies(program: &[LinearOp]) -> Result<Vec<usize>, EvalSolveError> {
    let outputs = program_output_dependencies(program, None)?;
    let [dependencies] = outputs.as_slice() else {
        return Err(sparsity_error(
            format!(
                "scalar row must produce exactly one output for dependency analysis, found {}",
                outputs.len()
            ),
            None,
        ));
    };
    match dependencies {
        DependencyState::Known(indices) => Ok(indices.iter().copied().collect()),
        DependencyState::Unknown => Err(sparsity_error(
            "scalar row has an opaque dependency without a known column bound",
            None,
        )),
    }
}

fn program_output_dependencies(
    program: &[LinearOp],
    span: Option<Span>,
) -> Result<Vec<DependencyState>, EvalSolveError> {
    program_output_dependencies_with_fold(program, span, None, None, None, DependencySource::Seed)
}

pub(crate) fn program_output_y_dependencies(
    program: &[LinearOp],
    span: Option<Span>,
) -> Result<Vec<BTreeSet<usize>>, EvalSolveError> {
    program_output_dependencies_with_fold(
        program,
        span,
        None,
        None,
        None,
        DependencySource::SolverY,
    )?
    .into_iter()
    .map(|dependencies| match dependencies {
        DependencyState::Known(indices) => Ok(indices),
        DependencyState::Unknown => Err(sparsity_error(
            "scalar output has an opaque solver-Y dependency",
            span,
        )),
    })
    .collect()
}

fn program_output_dependencies_with_fold(
    program: &[LinearOp],
    span: Option<Span>,
    fold_carried: Option<&[DependencyState]>,
    fold_captures: Option<&[DependencyState]>,
    conditional_captures: Option<&[DependencyState]>,
    source: DependencySource,
) -> Result<Vec<DependencyState>, EvalSolveError> {
    let mut registers: Vec<Option<DependencyState>> = Vec::new();
    let mut outputs = Vec::new();
    for op in program.iter().cloned() {
        match op {
            LinearOp::Const { dst, .. }
            | LinearOp::LoadTime { dst }
            | LinearOp::LoadP { dst, .. } => set_empty_dependency(&mut registers, dst),
            LinearOp::LoadY { dst, index } => match source {
                DependencySource::Seed => set_empty_dependency(&mut registers, dst),
                DependencySource::SolverY => set_seed_dependency(&mut registers, dst, index),
            },
            LinearOp::LoadSeed { dst, index } => match source {
                DependencySource::Seed => set_seed_dependency(&mut registers, dst, index),
                DependencySource::SolverY => set_empty_dependency(&mut registers, dst),
            },
            LinearOp::LoadFoldCarried { dst, index } => {
                let dependency = fold_carried
                    .and_then(|values| values.get(index))
                    .cloned()
                    .ok_or_else(|| sparsity_error("invalid function-fold carried load", span))?;
                set_register(&mut registers, dst, dependency);
            }
            LinearOp::LoadFoldIndex { dst, .. } => {
                set_empty_dependency(&mut registers, dst);
            }
            LinearOp::LoadFoldCapture { dst, index } => {
                let dependency = fold_captures
                    .and_then(|values| values.get(index))
                    .cloned()
                    .ok_or_else(|| sparsity_error("invalid function-fold capture load", span))?;
                set_register(&mut registers, dst, dependency);
            }
            LinearOp::LoadFunctionConditionalCapture { dst, index } => {
                let dependency = conditional_captures
                    .and_then(|values| values.get(index))
                    .cloned()
                    .ok_or_else(|| {
                        sparsity_error("invalid function-conditional capture load", span)
                    })?;
                set_register(&mut registers, dst, dependency);
            }
            LinearOp::LoadFunctionConditionalCaptureRange {
                dst_start,
                index_start,
                count,
            } => {
                let captures = conditional_captures.ok_or_else(|| {
                    sparsity_error("invalid function-conditional capture range load", span)
                })?;
                for offset in 0..count {
                    let dependency =
                        captures.get(index_start + offset).cloned().ok_or_else(|| {
                            sparsity_error("invalid function-conditional capture range load", span)
                        })?;
                    set_register(&mut registers, dst_start + offset as Reg, dependency);
                }
            }
            LinearOp::LoadIndexedP { dst, index, .. } => {
                copy_dependency(&mut registers, dst, index, span)?;
            }
            LinearOp::LoadIndexedRegister {
                dst,
                base,
                stride,
                dimensions,
                indices,
            } => {
                let count = dimensions
                    .iter()
                    .try_fold(1usize, |count, &extent| count.checked_mul(extent as usize))
                    .ok_or_else(|| {
                        sparsity_error("runtime tensor projection extent overflow", span)
                    })?;
                let mut dependencies = DependencyState::empty();
                for offset in 0..count {
                    dependencies = dependencies.union(register(
                        &registers,
                        base + (offset * stride) as Reg,
                        span,
                    )?);
                }
                for index in indices {
                    if let rumoca_ir_solve::TensorIndex::Runtime(register_id) = index {
                        dependencies = dependencies.union(register(&registers, register_id, span)?);
                    }
                }
                set_register(&mut registers, dst, dependencies);
            }
            LinearOp::LoadIndexedFoldCarried {
                dst,
                base,
                stride,
                dimensions,
                indices,
            } => {
                let carried = fold_carried.ok_or_else(|| {
                    sparsity_error("invalid indexed function-fold carried load", span)
                })?;
                let count = dimensions
                    .iter()
                    .try_fold(1usize, |count, &extent| count.checked_mul(extent as usize))
                    .ok_or_else(|| {
                        sparsity_error("indexed function-fold carried extent overflow", span)
                    })?;
                let mut dependencies = DependencyState::empty();
                for offset in 0..count {
                    let dependency = carried.get(base + offset * stride).ok_or_else(|| {
                        sparsity_error("indexed function-fold carried range is invalid", span)
                    })?;
                    dependencies = dependencies.union(dependency.clone());
                }
                for index in indices {
                    if let rumoca_ir_solve::TensorIndex::Runtime(register_id) = index {
                        dependencies = dependencies.union(register(&registers, register_id, span)?);
                    }
                }
                set_register(&mut registers, dst, dependencies);
            }
            LinearOp::LoadIndexedFoldCapture {
                dst,
                base,
                stride,
                dimensions,
                indices,
            } => {
                let captures = fold_captures.ok_or_else(|| {
                    sparsity_error("invalid indexed function-fold capture load", span)
                })?;
                let count = dimensions
                    .iter()
                    .try_fold(1usize, |count, &extent| count.checked_mul(extent as usize))
                    .ok_or_else(|| {
                        sparsity_error("indexed function-fold capture extent overflow", span)
                    })?;
                let mut dependencies = DependencyState::empty();
                for offset in 0..count {
                    let dependency = captures.get(base + offset * stride).ok_or_else(|| {
                        sparsity_error("indexed function-fold capture range is invalid", span)
                    })?;
                    dependencies = dependencies.union(dependency.clone());
                }
                for index in indices {
                    if let rumoca_ir_solve::TensorIndex::Runtime(register_id) = index {
                        dependencies = dependencies.union(register(&registers, register_id, span)?);
                    }
                }
                set_register(&mut registers, dst, dependencies);
            }
            LinearOp::LoadIndexedSeed {
                dst,
                base,
                count,
                index,
            } => set_indexed_seed_dependency(
                &mut registers,
                IndexedSeedDependency {
                    dst,
                    base,
                    count,
                    index,
                },
                span,
            )?,
            LinearOp::Move { dst, src } | LinearOp::Unary { dst, arg: src, .. } => {
                copy_dependency(&mut registers, dst, src, span)?;
            }
            LinearOp::Binary { dst, lhs, rhs, .. } | LinearOp::Compare { dst, lhs, rhs, .. } => {
                set_union_dependency(&mut registers, dst, [lhs, rhs], span)?;
            }
            LinearOp::Select {
                dst,
                cond,
                if_true,
                if_false,
            } => {
                set_union_dependency(&mut registers, dst, [cond, if_true, if_false], span)?;
            }
            LinearOp::LinearSolveComponent {
                dst,
                matrix_start,
                rhs_start,
                n,
                ..
            } => set_linear_solve_dependency(
                &mut registers,
                LinearSolveDependency {
                    dst,
                    matrix_start,
                    rhs_start,
                    n,
                },
                span,
            )?,
            LinearOp::DotProduct {
                dst,
                lhs_start,
                rhs_start,
                count,
                lhs_stride,
                rhs_stride,
            } => {
                let mut sources = Vec::with_capacity(count.saturating_mul(2));
                for term in 0..count {
                    sources.push(lhs_start + (term * lhs_stride) as Reg);
                    sources.push(rhs_start + (term * rhs_stride) as Reg);
                }
                let mut dependencies = DependencyState::empty();
                for source in sources {
                    dependencies = dependencies.union(register(&registers, source, span)?);
                }
                set_register(&mut registers, dst, dependencies);
            }
            LinearOp::MatrixMultiply {
                dst_start,
                lhs_start,
                rhs_start,
                rows,
                inner,
                columns,
                lanes,
            } => {
                for row in 0..rows {
                    for column in 0..columns {
                        let output = (row * columns + column) * lanes;
                        for lane in 0..lanes {
                            let mut dependencies = DependencyState::empty();
                            for term in 0..inner {
                                let lhs = (row * inner + term) * lanes;
                                let rhs = (term * columns + column) * lanes;
                                dependencies = dependencies.union(register(
                                    &registers,
                                    lhs_start + (lhs + lane) as Reg,
                                    span,
                                )?);
                                dependencies = dependencies.union(register(
                                    &registers,
                                    rhs_start + (rhs + lane) as Reg,
                                    span,
                                )?);
                                if lanes == 2 && lane == 1 {
                                    dependencies = dependencies.union(register(
                                        &registers,
                                        lhs_start + lhs as Reg,
                                        span,
                                    )?);
                                    dependencies = dependencies.union(register(
                                        &registers,
                                        rhs_start + rhs as Reg,
                                        span,
                                    )?);
                                }
                            }
                            set_register(
                                &mut registers,
                                dst_start + (output + lane) as Reg,
                                dependencies,
                            );
                        }
                    }
                }
            }
            LinearOp::TensorBinary {
                dst_start,
                op,
                lhs_start,
                rhs_start,
                count,
                lhs_stride,
                rhs_stride,
                lanes,
            } => {
                for element in 0..count {
                    let lhs = lhs_start + (element * lhs_stride * lanes) as Reg;
                    let rhs = rhs_start + (element * rhs_stride * lanes) as Reg;
                    let output = dst_start + (element * lanes) as Reg;
                    let primal =
                        register(&registers, lhs, span)?.union(register(&registers, rhs, span)?);
                    set_register(&mut registers, output, primal.clone());
                    if lanes == 2 {
                        let mut tangent = register(&registers, lhs + 1, span)?.union(register(
                            &registers,
                            rhs + 1,
                            span,
                        )?);
                        if matches!(op, BinaryOp::Mul | BinaryOp::Div) {
                            tangent = tangent.union(primal);
                        }
                        set_register(&mut registers, output + 1, tangent);
                    }
                }
            }
            LinearOp::TensorCross {
                dst_start,
                lhs_start,
                rhs_start,
                lanes,
            } => {
                for (component, (first, second)) in
                    [(1usize, 2usize), (2, 0), (0, 1)].into_iter().enumerate()
                {
                    let lhs_first = lhs_start + (first * lanes) as Reg;
                    let lhs_second = lhs_start + (second * lanes) as Reg;
                    let rhs_first = rhs_start + (first * lanes) as Reg;
                    let rhs_second = rhs_start + (second * lanes) as Reg;
                    let primal = register(&registers, lhs_first, span)?
                        .union(register(&registers, lhs_second, span)?)
                        .union(register(&registers, rhs_first, span)?)
                        .union(register(&registers, rhs_second, span)?);
                    let output = dst_start + (component * lanes) as Reg;
                    set_register(&mut registers, output, primal.clone());
                    if lanes == 2 {
                        let tangent = primal
                            .union(register(&registers, lhs_first + 1, span)?)
                            .union(register(&registers, lhs_second + 1, span)?)
                            .union(register(&registers, rhs_first + 1, span)?)
                            .union(register(&registers, rhs_second + 1, span)?);
                        set_register(&mut registers, output + 1, tangent);
                    }
                }
            }
            LinearOp::TensorTranspose {
                dst_start,
                src_start,
                rows,
                columns,
                element_width,
                lanes,
            } => {
                let value_width = element_width * lanes;
                for row in 0..rows {
                    for column in 0..columns {
                        for value in 0..value_width {
                            let dst = (row * columns + column) * value_width + value;
                            let src = (column * rows + row) * value_width + value;
                            let dependencies = register(&registers, src_start + src as Reg, span)?;
                            set_register(&mut registers, dst_start + dst as Reg, dependencies);
                        }
                    }
                }
            }
            LinearOp::TensorConcatenate {
                dst_start,
                sources,
                dimensions,
                axis,
                lanes,
            } => {
                super::visit_tensor_concatenate(
                    &sources,
                    &dimensions,
                    axis,
                    lanes,
                    |source, destination| {
                        let dependencies = register(&registers, source, span)?;
                        set_register(&mut registers, dst_start + destination as Reg, dependencies);
                        Ok::<(), EvalSolveError>(())
                    },
                )?;
            }
            LinearOp::TensorUpdate {
                dst_start,
                base_start,
                value_start,
                dimensions,
                subscripts,
                lanes,
            } => {
                let count = dimensions.iter().fold(1usize, |count, extent| {
                    count.saturating_mul(*extent as usize)
                });
                let mut value_count = lanes;
                let mut selector = DependencyState::empty();
                for (&extent, subscript) in dimensions.iter().zip(subscripts.iter()) {
                    match subscript {
                        rumoca_ir_solve::TensorUpdateSubscript::Whole => {
                            value_count = value_count.saturating_mul(extent as usize);
                        }
                        rumoca_ir_solve::TensorUpdateSubscript::Index(
                            rumoca_ir_solve::TensorIndex::Runtime(register_id),
                        ) => {
                            selector = selector.union(register(&registers, *register_id, span)?);
                        }
                        rumoca_ir_solve::TensorUpdateSubscript::Index(
                            rumoca_ir_solve::TensorIndex::Constant(_),
                        ) => {}
                        rumoca_ir_solve::TensorUpdateSubscript::Slice { start, dimensions } => {
                            let slice_count = dimensions.iter().fold(1usize, |count, extent| {
                                count.saturating_mul(*extent as usize)
                            });
                            selector = selector.union(register_range(
                                &registers,
                                *start,
                                slice_count,
                                span,
                            )?);
                            value_count = value_count.saturating_mul(slice_count);
                        }
                    }
                }
                let patch =
                    register_range(&registers, value_start, value_count, span)?.union(selector);
                for element in 0..count {
                    for lane in 0..lanes {
                        let offset = element * lanes + lane;
                        let dependencies = register(&registers, base_start + offset as Reg, span)?
                            .union(patch.clone());
                        set_register(&mut registers, dst_start + offset as Reg, dependencies);
                    }
                }
            }
            LinearOp::TensorFill {
                dst_start,
                value_start,
                count,
                lanes,
            } => {
                for element in 0..count {
                    for lane in 0..lanes {
                        let dependencies = register(&registers, value_start + lane as Reg, span)?;
                        set_register(
                            &mut registers,
                            dst_start + (element * lanes + lane) as Reg,
                            dependencies,
                        );
                    }
                }
            }
            LinearOp::TensorIdentity {
                dst_start,
                size,
                lanes,
            } => {
                for offset in 0..size * size * lanes {
                    set_empty_dependency(&mut registers, dst_start + offset as Reg);
                }
            }
            LinearOp::TensorLoad {
                dst_start,
                count,
                seed_start,
                lanes,
                ..
            } => {
                for element in 0..count {
                    set_empty_dependency(&mut registers, dst_start + (element * lanes) as Reg);
                    if lanes == 2 {
                        let dependency = seed_start
                            .map_or_else(DependencyState::empty, |seed_start| {
                                DependencyState::singleton(seed_start + element)
                            });
                        set_register(
                            &mut registers,
                            dst_start + (element * lanes + 1) as Reg,
                            dependency,
                        );
                    }
                }
            }
            op @ (LinearOp::TableBounds { .. }
            | LinearOp::TableLookup { .. }
            | LinearOp::TableLookupSlope { .. }
            | LinearOp::TableNextEvent { .. }
            | LinearOp::RandomInitialState { .. }
            | LinearOp::RandomResult { .. }
            | LinearOp::RandomState { .. }
            | LinearOp::ImpureRandomInit { .. }
            | LinearOp::ImpureRandom { .. }
            | LinearOp::ImpureRandomInteger { .. }) => {
                apply_runtime_dependency(&mut registers, op, span)?;
            }
            LinearOp::FunctionFold {
                dst_start,
                initial_start,
                capture_start,
                program,
            } => {
                let carried = (0..program.carried_count)
                    .map(|offset| register(&registers, initial_start + offset as Reg, span))
                    .collect::<Result<Vec<_>, _>>()?;
                let captures = (0..program.capture_count)
                    .map(|offset| register(&registers, capture_start + offset as Reg, span))
                    .collect::<Result<Vec<_>, _>>()?;
                let carried =
                    function_fold_dependencies(&program, carried, &captures, span, source)?;
                for (offset, dependency) in carried.into_iter().enumerate() {
                    set_register(&mut registers, dst_start + offset as Reg, dependency);
                }
            }
            LinearOp::GuardedFunctionFold {
                dst_start,
                initial_start,
                capture_start,
                activation,
                program,
            } => {
                let activation = register(&registers, activation, span)?;
                let carried = (0..program.carried_count)
                    .map(|offset| register(&registers, initial_start + offset as Reg, span))
                    .collect::<Result<Vec<_>, _>>()?;
                let captures = (0..program.capture_count)
                    .map(|offset| register(&registers, capture_start + offset as Reg, span))
                    .collect::<Result<Vec<_>, _>>()?;
                let carried =
                    function_fold_dependencies(&program, carried, &captures, span, source)?;
                for (offset, dependency) in carried.into_iter().enumerate() {
                    set_register(
                        &mut registers,
                        dst_start + offset as Reg,
                        dependency.union(activation.clone()),
                    );
                }
            }
            LinearOp::FunctionConditional {
                dst_start,
                capture_start,
                program,
            } => {
                let captures = (0..program.capture_count)
                    .map(|offset| register(&registers, capture_start + offset as Reg, span))
                    .collect::<Result<Vec<_>, _>>()?;
                let mut condition_dependency = DependencyState::empty();
                let mut result = vec![DependencyState::empty(); program.result_count];
                for arm in &program.arms {
                    let condition = program_output_dependencies_with_fold(
                        &arm.condition,
                        span,
                        fold_carried,
                        fold_captures,
                        Some(&captures),
                        source,
                    )?;
                    condition_dependency =
                        condition_dependency.union(condition.first().cloned().ok_or_else(
                            || sparsity_error("missing conditional condition", span),
                        )?);
                    let branch = program_output_dependencies_with_fold(
                        &arm.result,
                        span,
                        fold_carried,
                        fold_captures,
                        Some(&captures),
                        source,
                    )?;
                    union_conditional_results(&mut result, branch, span)?;
                }
                let fallback = program_output_dependencies_with_fold(
                    &program.fallback,
                    span,
                    fold_carried,
                    fold_captures,
                    Some(&captures),
                    source,
                )?;
                union_conditional_results(&mut result, fallback, span)?;
                for (offset, dependency) in result.into_iter().enumerate() {
                    set_register(
                        &mut registers,
                        dst_start + offset as Reg,
                        dependency.union(condition_dependency.clone()),
                    );
                }
            }
            LinearOp::PureCall {
                dst_start,
                input_starts,
                site,
            } => {
                let mut dependency = DependencyState::empty();
                for (start, value_type) in input_starts.iter().zip(site.inputs()) {
                    for offset in 0..value_type.scalar_count() as usize {
                        dependency =
                            dependency.union(register(&registers, start + offset as Reg, span)?);
                    }
                }
                let output_count = site
                    .output_scalar_count()
                    .ok_or_else(|| sparsity_error("pure-call output width overflows", span))?;
                for offset in 0..output_count {
                    set_register(
                        &mut registers,
                        dst_start + offset as Reg,
                        dependency.clone(),
                    );
                }
            }
            LinearOp::PureCallDirectional {
                dst_start,
                input_starts,
                site,
            } => {
                let mut dependency = DependencyState::empty();
                for (start, value_type) in input_starts.iter().zip(site.inputs()) {
                    for offset in 0..value_type.scalar_count() as usize {
                        dependency =
                            dependency.union(register(&registers, start + offset as Reg, span)?);
                    }
                }
                let output_count = site.output_scalar_count().ok_or_else(|| {
                    sparsity_error("directional pure-call output width overflows", span)
                })?;
                for offset in 0..output_count {
                    set_register(
                        &mut registers,
                        dst_start + offset as Reg,
                        dependency.clone(),
                    );
                }
            }
            LinearOp::StoreOutputFoldTensorUpdate {
                source_base,
                source_stride,
                dimensions,
                updates,
                nodes,
                lanes,
                ..
            } => {
                let carried = fold_carried.ok_or_else(|| {
                    sparsity_error(
                        "aggregate output escaped its function-fold update body",
                        span,
                    )
                })?;
                let count = dimensions
                    .iter()
                    .try_fold(1usize, |count, &extent| count.checked_mul(extent as usize))
                    .ok_or_else(|| sparsity_error("tensor update extent overflow", span))?;
                let mut update_dependency = DependencyState::empty();
                for update in &updates {
                    let value_count = dimensions
                        .iter()
                        .zip(update.subscripts.iter())
                        .try_fold(1usize, |count, (&extent, subscript)| {
                            if matches!(subscript, rumoca_ir_solve::TensorSubscript::Whole) {
                                count.checked_mul(extent as usize)
                            } else {
                                Some(count)
                            }
                        })
                        .ok_or_else(|| {
                            sparsity_error("tensor update value extent overflow", span)
                        })?;
                    if let Some(condition) = update.condition {
                        update_dependency =
                            update_dependency.union(register(&registers, condition, span)?);
                    }
                    for element in 0..value_count {
                        for lane in 0..lanes {
                            update_dependency = update_dependency.union(register(
                                &registers,
                                update.value_start + (element * update.value_stride + lane) as Reg,
                                span,
                            )?);
                        }
                    }
                    for subscript in &update.subscripts {
                        if let rumoca_ir_solve::TensorSubscript::Index(
                            rumoca_ir_solve::TensorIndex::Runtime(register_id),
                        ) = subscript
                        {
                            update_dependency =
                                update_dependency.union(register(&registers, *register_id, span)?);
                        }
                    }
                }
                for node in &nodes {
                    if let rumoca_ir_solve::FoldTensorNode::Select { condition, .. } = *node {
                        update_dependency =
                            update_dependency.union(register(&registers, condition, span)?);
                    }
                }
                for element in 0..count {
                    for lane in 0..lanes {
                        let unchanged = carried
                            .get(source_base + element * source_stride + lane)
                            .cloned()
                            .ok_or_else(|| {
                                sparsity_error("tensor update carried source is invalid", span)
                            })?;
                        outputs.push(unchanged.union(update_dependency.clone()));
                    }
                }
            }
            LinearOp::StoreOutputFunctionFold {
                initial,
                capture_start,
                program,
                result_base,
                count,
                condition,
                ..
            } => {
                let parent = fold_carried.ok_or_else(|| {
                    sparsity_error("nested aggregate fold escaped its parent update body", span)
                })?;
                let mut carried = Vec::with_capacity(program.carried_count);
                for source in initial.iter() {
                    match *source {
                        rumoca_ir_solve::FoldInitialSource::Registers { start, count } => {
                            for offset in 0..count {
                                carried.push(register(&registers, start + offset as Reg, span)?);
                            }
                        }
                        rumoca_ir_solve::FoldInitialSource::ParentCarried { base, count } => {
                            let end = base.checked_add(count).ok_or_else(|| {
                                sparsity_error("nested fold carried range overflow", span)
                            })?;
                            let values = parent.get(base..end).ok_or_else(|| {
                                sparsity_error("nested fold carried range is invalid", span)
                            })?;
                            carried.extend_from_slice(values);
                        }
                    }
                }
                let captures = (0..program.capture_count)
                    .map(|offset| register(&registers, capture_start + offset as Reg, span))
                    .collect::<Result<Vec<_>, _>>()?;
                let carried =
                    function_fold_dependencies(&program, carried, &captures, span, source)?;
                let end = result_base
                    .checked_add(count)
                    .ok_or_else(|| sparsity_error("nested fold result range overflow", span))?;
                let result = carried
                    .get(result_base..end)
                    .ok_or_else(|| sparsity_error("nested fold result range is invalid", span))?;
                if let Some(condition) = condition {
                    let condition = register(&registers, condition, span)?;
                    let output_base = outputs.len();
                    for (offset, nested) in result.iter().cloned().enumerate() {
                        let unchanged =
                            parent.get(output_base + offset).cloned().ok_or_else(|| {
                                sparsity_error(
                                    "conditional nested fold parent range is invalid",
                                    span,
                                )
                            })?;
                        outputs.push(nested.union(unchanged).union(condition.clone()));
                    }
                } else {
                    outputs.extend_from_slice(result);
                }
            }
            LinearOp::StoreOutputRange {
                start,
                count,
                stride,
            } => {
                for ordinal in 0..count {
                    let offset = ordinal.checked_mul(stride).ok_or_else(|| {
                        sparsity_error("conditional output range offset overflows", span)
                    })?;
                    let offset = Reg::try_from(offset).map_err(|_| {
                        sparsity_error("conditional output range exceeds registers", span)
                    })?;
                    let source = start.checked_add(offset).ok_or_else(|| {
                        sparsity_error("conditional output register overflows", span)
                    })?;
                    outputs.push(register(&registers, source, span)?);
                }
            }
            LinearOp::StoreOutput { src } => outputs.push(register(&registers, src, span)?),
        }
    }
    Ok(outputs)
}

fn union_conditional_results(
    accumulated: &mut [DependencyState],
    branch: Vec<DependencyState>,
    span: Option<Span>,
) -> Result<(), EvalSolveError> {
    if accumulated.len() != branch.len() {
        return Err(sparsity_error(
            "function-conditional result dependency count mismatch",
            span,
        ));
    }
    for (accumulated, branch) in accumulated.iter_mut().zip(branch) {
        *accumulated = accumulated.clone().union(branch);
    }
    Ok(())
}

fn function_fold_dependencies(
    program: &rumoca_ir_solve::FunctionFoldProgram,
    mut carried: Vec<DependencyState>,
    captures: &[DependencyState],
    span: Option<Span>,
    source: DependencySource,
) -> Result<Vec<DependencyState>, EvalSolveError> {
    if carried.len() != program.carried_count {
        return Err(sparsity_error(
            "function-fold initial dependency count mismatch",
            span,
        ));
    }
    if program
        .domain
        .scalar_count()
        .map_err(|error| sparsity_error(format!("invalid function-fold domain: {error}"), span))?
        == 0
    {
        return Ok(carried);
    }
    loop {
        let updates = program_output_dependencies_with_fold(
            &program.update,
            span,
            Some(&carried),
            Some(captures),
            None,
            source,
        )?;
        if updates.len() != carried.len() {
            return Err(sparsity_error(
                "function-fold update output count mismatch",
                span,
            ));
        }
        let next = carried
            .iter()
            .cloned()
            .zip(updates)
            .map(|(old, new)| old.union(new))
            .collect::<Vec<_>>();
        if next == carried {
            return Ok(carried);
        }
        carried = next;
    }
}

struct IndexedSeedDependency {
    dst: Reg,
    base: usize,
    count: usize,
    index: Reg,
}

struct LinearSolveDependency {
    dst: Reg,
    matrix_start: Reg,
    rhs_start: Reg,
    n: usize,
}

fn apply_runtime_dependency(
    registers: &mut Vec<Option<DependencyState>>,
    operation: LinearOp,
    span: Option<Span>,
) -> Result<(), EvalSolveError> {
    match operation {
        LinearOp::TableBounds { dst, table_id, .. } => {
            copy_dependency(registers, dst, table_id, span)
        }
        LinearOp::TableLookup {
            dst,
            table_id,
            column,
            input,
        }
        | LinearOp::TableLookupSlope {
            dst,
            table_id,
            column,
            input,
        } => set_union_dependency(registers, dst, [table_id, column, input], span),
        LinearOp::TableNextEvent {
            dst,
            table_id,
            time,
        } => set_union_dependency(registers, dst, [table_id, time], span),
        LinearOp::RandomInitialState {
            dst,
            local_seed,
            global_seed,
            ..
        } => set_union_dependency(registers, dst, [local_seed, global_seed], span),
        LinearOp::RandomResult {
            dst,
            state_start,
            state_len,
            ..
        }
        | LinearOp::RandomState {
            dst,
            state_start,
            state_len,
            ..
        } => set_range_dependency(registers, dst, state_start, state_len, span),
        LinearOp::ImpureRandomInit { dst, seed } => copy_dependency(registers, dst, seed, span),
        LinearOp::ImpureRandom { dst, id, .. } => copy_dependency(registers, dst, id, span),
        LinearOp::ImpureRandomInteger {
            dst,
            id,
            imin,
            imax,
            ..
        } => set_union_dependency(registers, dst, [id, imin, imax], span),
        _ => unreachable!("runtime dependency operation is classified by the exhaustive caller"),
    }
}

fn set_empty_dependency(registers: &mut Vec<Option<DependencyState>>, dst: Reg) {
    set_register(registers, dst, DependencyState::empty());
}

fn set_seed_dependency(registers: &mut Vec<Option<DependencyState>>, dst: Reg, index: usize) {
    set_register(registers, dst, DependencyState::singleton(index));
}

fn copy_dependency(
    registers: &mut Vec<Option<DependencyState>>,
    dst: Reg,
    src: Reg,
    span: Option<Span>,
) -> Result<(), EvalSolveError> {
    let dependencies = register(registers, src, span)?;
    set_register(registers, dst, dependencies);
    Ok(())
}

fn set_union_dependency<const N: usize>(
    registers: &mut Vec<Option<DependencyState>>,
    dst: Reg,
    sources: [Reg; N],
    span: Option<Span>,
) -> Result<(), EvalSolveError> {
    let dependencies = union_registers(registers, sources, span)?;
    set_register(registers, dst, dependencies);
    Ok(())
}

fn set_range_dependency(
    registers: &mut Vec<Option<DependencyState>>,
    dst: Reg,
    start: Reg,
    len: usize,
    span: Option<Span>,
) -> Result<(), EvalSolveError> {
    let dependencies = register_range(registers, start, len, span)?;
    set_register(registers, dst, dependencies);
    Ok(())
}

fn set_indexed_seed_dependency(
    registers: &mut Vec<Option<DependencyState>>,
    dependency: IndexedSeedDependency,
    span: Option<Span>,
) -> Result<(), EvalSolveError> {
    let mut dependencies = register(registers, dependency.index, span)?;
    let end = checked_indexed_seed_end(dependency.base, dependency.count, span)?;
    dependencies = dependencies.union(DependencyState::Known((dependency.base..end).collect()));
    set_register(registers, dependency.dst, dependencies);
    Ok(())
}

fn set_linear_solve_dependency(
    registers: &mut Vec<Option<DependencyState>>,
    dependency: LinearSolveDependency,
    span: Option<Span>,
) -> Result<(), EvalSolveError> {
    let matrix_len = checked_product(dependency.n, dependency.n, "linear solve matrix", span)?;
    let dependencies = register_range(registers, dependency.matrix_start, matrix_len, span)?.union(
        register_range(registers, dependency.rhs_start, dependency.n, span)?,
    );
    set_register(registers, dependency.dst, dependencies);
    Ok(())
}

fn set_register(
    registers: &mut Vec<Option<DependencyState>>,
    register: Reg,
    dependencies: DependencyState,
) {
    let index = register as usize;
    if registers.len() <= index {
        registers.resize_with(index + 1, || None);
    }
    registers[index] = Some(dependencies);
}

fn register(
    registers: &[Option<DependencyState>],
    register: Reg,
    span: Option<Span>,
) -> Result<DependencyState, EvalSolveError> {
    registers
        .get(register as usize)
        .and_then(Clone::clone)
        .ok_or(EvalSolveError::UninitializedRegister { register, span })
}

fn register_range(
    registers: &[Option<DependencyState>],
    start: Reg,
    len: usize,
    span: Option<Span>,
) -> Result<DependencyState, EvalSolveError> {
    let mut dependencies = DependencyState::empty();
    for offset in 0..len {
        dependencies = dependencies.union(register(
            registers,
            checked_reg_offset(start, offset, span)?,
            span,
        )?);
    }
    Ok(dependencies)
}

fn union_registers<const N: usize>(
    registers: &[Option<DependencyState>],
    operands: [Reg; N],
    span: Option<Span>,
) -> Result<DependencyState, EvalSolveError> {
    operands
        .into_iter()
        .try_fold(DependencyState::empty(), |dependencies, register_id| {
            Ok(dependencies.union(register(registers, register_id, span)?))
        })
}

fn checked_product(
    lhs: usize,
    rhs: usize,
    operation: &'static str,
    span: Option<Span>,
) -> Result<usize, EvalSolveError> {
    lhs.checked_mul(rhs).ok_or_else(|| {
        sparsity_error(
            format!("{operation} shape product {lhs} * {rhs} overflows register range"),
            span,
        )
    })
}

fn checked_indexed_seed_end(
    base: usize,
    count: usize,
    span: Option<Span>,
) -> Result<usize, EvalSolveError> {
    let width = count.max(1);
    base.checked_add(width).ok_or_else(|| {
        sparsity_error(
            format!("indexed seed range base {base} plus count {count} overflows"),
            span,
        )
    })
}

fn checked_reg_offset(
    start: Reg,
    offset: usize,
    span: Option<Span>,
) -> Result<Reg, EvalSolveError> {
    let offset = u32::try_from(offset)
        .map_err(|_| sparsity_error(format!("register offset {offset} exceeds u32"), span))?;
    start.checked_add(offset).ok_or_else(|| {
        sparsity_error(
            format!("register range start {start} plus offset {offset} overflows"),
            span,
        )
    })
}

fn sparsity_error(message: impl Into<String>, span: Option<Span>) -> EvalSolveError {
    EvalSolveError::ShapeContract {
        message: message.into(),
        span,
    }
}

#[cfg(test)]
mod tests {
    use rumoca_ir_solve::{BinaryOp, ComputeBlock, ScalarProgramBlock, StructuralPatternView};

    use super::*;

    fn span() -> Span {
        Span::from_offsets(
            rumoca_core::SourceId::from_source_name("jvp_sparsity.mo"),
            1,
            2,
        )
    }

    #[test]
    fn row_seed_dependencies_track_arithmetic_flow() {
        let row = vec![
            LinearOp::LoadSeed { dst: 0, index: 2 },
            LinearOp::Const { dst: 1, value: 4.0 },
            LinearOp::Binary {
                dst: 2,
                op: BinaryOp::Mul,
                lhs: 0,
                rhs: 1,
            },
            LinearOp::StoreOutput { src: 2 },
        ];
        assert_eq!(row_seed_dependencies(&row).unwrap(), vec![2]);
    }

    #[test]
    fn register_holes_are_not_silently_independent() {
        let row = vec![
            LinearOp::Const { dst: 7, value: 1.0 },
            LinearOp::Move { dst: 8, src: 0 },
            LinearOp::StoreOutput { src: 8 },
        ];
        assert!(matches!(
            row_seed_dependencies(&row),
            Err(EvalSolveError::UninitializedRegister { register: 0, .. })
        ));
    }

    #[test]
    fn derivation_handles_multiple_outputs_and_colors_them() {
        let block = ScalarProgramBlock::with_source_span(
            vec![vec![
                LinearOp::LoadSeed { dst: 0, index: 1 },
                LinearOp::StoreOutput { src: 0 },
                LinearOp::Const { dst: 1, value: 0.0 },
                LinearOp::StoreOutput { src: 1 },
            ]],
            span()
                .require_provenance("sparsity fixture")
                .expect("fixture span is source-backed"),
        )
        .expect("sparsity fixture is computable");
        let pattern = derive_jacobian_pattern_from_jvp(
            &ComputeBlock::from_scalar_program_block(block),
            2,
            3,
            span(),
        )
        .unwrap();
        assert!(matches!(pattern.view(), StructuralPatternView::Csr { .. }));
        assert!(pattern.contains(0, 1));
        assert!(!pattern.contains(1, 1));
        let coloring = derive_column_coloring(&pattern);
        assert_eq!(coloring.column_count(), 3);
        assert_eq!(coloring.compressed_seed_count(), 1);
    }

    #[test]
    fn out_of_range_seed_is_an_error_not_a_dropped_dependency() {
        let block = ScalarProgramBlock::with_source_span(
            vec![vec![
                LinearOp::LoadSeed { dst: 0, index: 3 },
                LinearOp::StoreOutput { src: 0 },
            ]],
            span()
                .require_provenance("sparsity fixture")
                .expect("fixture span is source-backed"),
        )
        .expect("sparsity fixture is computable");
        let error = derive_jacobian_pattern_from_scalar_jvp(&block, 1, 3, span()).unwrap_err();
        assert!(error.to_string().contains("outside 0..3"));
    }

    #[test]
    fn checked_sparse_output_holes_are_structurally_empty() {
        let block = ScalarProgramBlock::with_output_indices(
            vec![vec![
                LinearOp::LoadSeed { dst: 0, index: 1 },
                LinearOp::StoreOutput { src: 0 },
            ]],
            vec![span()],
            vec![2],
        )
        .unwrap();
        let pattern = derive_jacobian_pattern_from_scalar_jvp(&block, 3, 2, span()).unwrap();
        assert!(!pattern.contains(0, 1));
        assert!(!pattern.contains(1, 1));
        assert!(pattern.contains(2, 1));
    }

    #[test]
    fn row_extent_cannot_claim_trailing_unproduced_rows() {
        let block = ScalarProgramBlock::with_source_span(
            vec![vec![
                LinearOp::LoadSeed { dst: 0, index: 0 },
                LinearOp::StoreOutput { src: 0 },
            ]],
            span()
                .require_provenance("sparsity fixture")
                .expect("fixture span is source-backed"),
        )
        .expect("sparsity fixture is computable");
        let error = derive_jacobian_pattern_from_scalar_jvp(&block, 2, 1, span()).unwrap_err();
        assert!(error.to_string().contains("row extent 2"));
    }
}
