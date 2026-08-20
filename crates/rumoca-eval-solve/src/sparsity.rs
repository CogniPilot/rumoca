use std::collections::BTreeSet;

use rumoca_core::Span;
use rumoca_ir_solve::{
    ComputeBlock, ComputeNode, LinearOp, PatternDerivation, PatternProvenance, ScalarProgramBlock,
    StructuralPattern, StructuralPatternError,
};

use crate::{EvalSolveError, to_scalar_program_block};

/// Lift a pattern-authority failure into this crate's error taxonomy without
/// losing the owner span the authority reported it against.
fn from_pattern_error(error: StructuralPatternError, owner_span: Option<Span>) -> EvalSolveError {
    match error {
        StructuralPatternError::UninitializedRegister { register, span } => {
            EvalSolveError::UninitializedRegister {
                register,
                span: span.or(owner_span),
            }
        }
        StructuralPatternError::DependencyContract { message, span } => {
            EvalSolveError::ShapeContract {
                message,
                span: span.or(owner_span),
            }
        }
        other => sparsity_error(other.to_string(), owner_span),
    }
}

/// Derive structural Jacobian dependencies from a JVP compute block.
pub fn derive_jacobian_pattern_from_jvp(
    block: &ComputeBlock,
    rows: usize,
    columns: usize,
    owner_span: Span,
) -> Result<StructuralPattern, EvalSolveError> {
    if let [node] = block.nodes.as_slice() {
        if let ComputeNode::Map {
            domain,
            output_map,
            base_ops,
            load_strides,
            ..
        }
        | ComputeNode::AffineStencil {
            domain,
            output_map,
            base_ops,
            load_strides,
            ..
        } = node
        {
            return StructuralPattern::derive_from_affine_jvp(
                domain,
                output_map,
                base_ops,
                load_strides,
                rows,
                columns,
                owner_span,
            )
            .map_err(|error| from_pattern_error(error, Some(owner_span)));
        }
    }
    if block.nodes.iter().any(|node| {
        matches!(
            node,
            ComputeNode::Map { .. } | ComputeNode::AffineStencil { .. }
        )
    }) {
        // A mixed compact block has no single affine owner representation yet.
        // Preserve the no-materialization boundary and fail conservatively to
        // Full: it may cost compressed AD opportunities but cannot omit an edge.
        let provenance =
            PatternProvenance::derived(PatternDerivation::ConservativeFull, owner_span)
                .map_err(|error| from_pattern_error(error, Some(owner_span)))?;
        return StructuralPattern::full(rows, columns, provenance)
            .map_err(|error| from_pattern_error(error, Some(owner_span)));
    }
    let scalar = to_scalar_program_block(block)?;
    derive_jacobian_pattern_from_scalar_jvp(&scalar, rows, columns, owner_span)
}

/// Derive structural Jacobian dependencies from a checked scalar JVP block.
///
/// SPEC_0039 / SOLVE-C17: the checked block is the semantic owner, and the
/// pattern authority in `rumoca-ir-solve` performs the exhaustive derivation
/// itself. This crate supplies the owner and the extents, never a dependency
/// row.
pub fn derive_jacobian_pattern_from_scalar_jvp(
    block: &ScalarProgramBlock,
    rows: usize,
    columns: usize,
    owner_span: Span,
) -> Result<StructuralPattern, EvalSolveError> {
    StructuralPattern::derive_from_scalar_jvp(block, rows, columns, owner_span)
        .map_err(|error| from_pattern_error(error, Some(owner_span)))
}

/// Dependencies of the sole output of a scalar row.
pub fn row_seed_dependencies(program: &[LinearOp]) -> Result<Vec<usize>, EvalSolveError> {
    StructuralPattern::derive_row_seed_dependencies(program)
        .map_err(|error| from_pattern_error(error, None))
}

pub(crate) fn program_output_y_dependencies(
    program: &[LinearOp],
    span: Option<Span>,
) -> Result<Vec<BTreeSet<usize>>, EvalSolveError> {
    StructuralPattern::derive_output_y_dependencies(program, span)
        .map_err(|error| from_pattern_error(error, span))
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

/// SPEC_0039 / SOLVE-C17: the sub-block relation is read out of the certified
/// source pattern by the pattern authority. This crate selects the rows and
/// columns the projection covers; it cannot state what they depend on.
fn derive_projection_pattern(
    source: &StructuralPattern,
    rows: &[usize],
    columns: &[usize],
) -> Result<StructuralPattern, EvalSolveError> {
    let span = source.provenance().span();
    source
        .project(rows, columns)
        .map_err(|error| from_pattern_error(error, Some(span)))
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

fn sparsity_error(message: impl Into<String>, span: Option<Span>) -> EvalSolveError {
    EvalSolveError::ShapeContract {
        message: message.into(),
        span,
    }
}

#[cfg(test)]
mod tests {
    use rumoca_core::{StructuredIndexBinder, StructuredIndexDomain};
    use rumoca_ir_solve::{
        AffineStencilIndexStrideTerm, AffineStencilLoadStride, BinaryOp, ComputeBlock, ComputeNode,
        ScalarProgramBlock, StructuralPatternView, TensorNodeMetadata, TensorOutputMap,
    };

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

    #[test]
    fn affine_jvp_sparsity_stays_compact_and_exact() {
        let domain = StructuredIndexDomain {
            binders: vec![StructuredIndexBinder {
                id: 0,
                display_name: "i".into(),
                lower: 1,
                upper: 100_000,
                step: 1,
            }],
        };
        let block = ComputeBlock {
            nodes: vec![ComputeNode::AffineStencil {
                output_map: TensorOutputMap::dense_contiguous(0, &domain).unwrap(),
                domain,
                base_ops: vec![
                    LinearOp::LoadSeed { dst: 0, index: 1 },
                    LinearOp::StoreOutput { src: 0 },
                ],
                load_strides: vec![AffineStencilLoadStride {
                    op_position: 0,
                    terms: vec![AffineStencilIndexStrideTerm {
                        dimension: 0,
                        stride: 1,
                    }],
                }],
                const_strides: Vec::new(),
                metadata: TensorNodeMetadata::default(),
                span: span(),
            }],
        };
        let pattern = derive_jacobian_pattern_from_jvp(&block, 100_000, 100_001, span()).unwrap();
        assert!(matches!(
            pattern.view(),
            StructuralPatternView::Affine {
                domain_rank: 1,
                access_count: 1
            }
        ));
        assert!(pattern.contains(0, 1));
        assert!(pattern.contains(99_999, 100_000));
        assert!(!pattern.contains(99_999, 99_999));
        assert_eq!(pattern.nonzero_upper_bound(), Some(100_000));
    }
}
