use std::collections::BTreeSet;

use rumoca_core::Span;
use rumoca_ir_solve::{
    ComputeBlock, LinearOp, PatternDerivation, PatternProvenance, Reg, ScalarProgramBlock,
    StructuralPattern,
};

use crate::{EvalSolveError, to_scalar_program_block};

#[derive(Clone, Debug, PartialEq, Eq)]
enum DependencyState {
    Known(BTreeSet<usize>),
    Unknown,
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
    let mut registers: Vec<Option<DependencyState>> = Vec::new();
    let mut outputs = Vec::new();
    for &op in program {
        match op {
            LinearOp::Const { dst, .. }
            | LinearOp::LoadTime { dst }
            | LinearOp::LoadY { dst, .. }
            | LinearOp::LoadP { dst, .. } => set_empty_dependency(&mut registers, dst),
            LinearOp::LoadSeed { dst, index } => set_seed_dependency(&mut registers, dst, index),
            LinearOp::LoadIndexedP { dst, index, .. } => {
                copy_dependency(&mut registers, dst, index, span)?;
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
            LinearOp::StoreOutput { src } => outputs.push(register(&registers, src, span)?),
        }
    }
    Ok(outputs)
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
