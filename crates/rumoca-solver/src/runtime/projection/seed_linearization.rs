//! One immutable block matrix shared by its directional right-hand sides.
//!
//! A small or dense block factors its matrix densely once. A sparse candidate
//! block solves through its issued torn affine elimination when the shared
//! admission (`projection_policy::affine_elimination_capacity`) accepts it,
//! and otherwise through faer's sparse LU of its pattern. Either factorization
//! is built on the first solve and reused while the matrix bits are unchanged;
//! a solve it declines is answered by the dense factorization and counted as
//! a [`ProjectionFallback::SeedDense`] fallback at the block's site.

use std::cell::{OnceCell, RefCell};

use super::*;
use nalgebra::{Dyn, LU};
use rumoca_eval_solve::tensor_policy::{LinearSolveKernel, select_linear_solve_kernel};

pub(crate) struct SeedBlockLinearization {
    solver: SeedSolver,
    jacobian: BlockJacobian,
}

enum SeedSolver {
    Dense(LU<f64, Dyn, Dyn>),
    Structured(Box<StructuredSeed>),
}

struct StructuredSeed {
    kind: StructuredSolve,
    cache: RefCell<SparseNewtonCache>,
    /// Unit row and variable scales: the sensitivity system is unscaled.
    unit: Box<[f64]>,
    /// The dense factorization, formed only once a structured solve declines.
    dense: OnceCell<LU<f64, Dyn, Dyn>>,
    site: Option<usize>,
}

enum StructuredSolve {
    Torn(solve::AffineEliminationLayout),
    Sparse(solve::StructuralPattern),
}

impl StructuredSolve {
    /// The structured solve of a sparse-candidate block of `n` rows.
    fn select(structure: Option<&solve::JacobianStructure>, n: usize) -> Option<Self> {
        let structure = structure?;
        let pattern = structure.pattern();
        if pattern.rows() as usize != n
            || !matches!(
                select_linear_solve_kernel(n, pattern),
                Ok(LinearSolveKernel::SparseCandidate)
            )
        {
            return None;
        }
        let torn = structure.affine_elimination().filter(|layout| {
            layout.pattern() == pattern
                && rumoca_eval_solve::projection_policy::affine_elimination_capacity(layout)
                    .is_some()
        });
        Some(match torn {
            Some(layout) => Self::Torn(layout.clone()),
            None => Self::Sparse(pattern.clone()),
        })
    }
}

impl SeedBlockLinearization {
    pub(crate) fn build(
        model: &dyn ImplicitProjectionModel,
        (block_index, site): (usize, Option<usize>),
        block: &solve::AlgebraicProjectionBlock,
        y: &[f64],
        args: AlgebraicProjectionArgs<'_>,
    ) -> Result<Self, RuntimeSolveError> {
        let structure = model.algebraic_projection_block_structure(block_index);
        let jacobian = algebraic_block_jacobian(
            model,
            y,
            args.parameters,
            args.time,
            &block.rows,
            &block.y_indices,
            structure,
        )?;
        let solver = match StructuredSolve::select(structure, jacobian.nrows()) {
            Some(kind) if jacobian.is_square() => {
                SeedSolver::Structured(Box::new(StructuredSeed {
                    kind,
                    cache: RefCell::default(),
                    unit: vec![1.0; jacobian.nrows()].into_boxed_slice(),
                    dense: OnceCell::new(),
                    site,
                }))
            }
            _ => SeedSolver::Dense(jacobian.as_dense().into_owned().lu()),
        };
        Ok(Self { solver, jacobian })
    }

    pub(super) fn row_scales(
        &self,
        model: &dyn ImplicitProjectionModel,
        block_index: usize,
        block: &solve::AlgebraicProjectionBlock,
        seed: &[f64],
    ) -> Vec<f64> {
        let structure = model.algebraic_projection_block_structure(block_index);
        algebraic_block_scales(
            model,
            seed,
            block,
            &self.jacobian,
            structure.map(solve::JacobianStructure::pattern),
        )
        .0
    }

    pub(super) fn solve(&self, rhs: &DVector<f64>) -> Option<DVector<f64>> {
        let seed = match &self.solver {
            SeedSolver::Dense(factor) => return factor.solve(rhs),
            SeedSolver::Structured(seed) => seed,
        };
        let StructuredSeed {
            kind,
            cache,
            unit,
            dense,
            site,
        } = &**seed;
        let _call = begin_block_call(*site, self.jacobian.nrows());
        let mut cache = cache.borrow_mut();
        let structured = match kind {
            StructuredSolve::Torn(layout) => {
                cache.solve_torn_scaled(&self.jacobian, rhs, (unit, unit), layout, None)
            }
            StructuredSolve::Sparse(pattern) => {
                cache.solve_scaled(&self.jacobian, rhs, unit, unit, pattern)
            }
        };
        structured.or_else(|| {
            note_block_fallback(*site, ProjectionFallback::SeedDense);
            dense
                .get_or_init(|| self.jacobian.as_dense().into_owned().lu())
                .solve(rhs)
        })
    }

    pub(super) fn trace_singular(
        &self,
        model: &dyn ImplicitProjectionModel,
        block_index: usize,
        block: &solve::AlgebraicProjectionBlock,
        y: &[f64],
        args: AlgebraicProjectionArgs<'_>,
        rhs: &DVector<f64>,
    ) {
        if !tracing::enabled!(target: "rumoca_solver::projection", tracing::Level::DEBUG) {
            return;
        }
        let variables = block
            .y_indices
            .iter()
            .map(|&index| model.variable_name_for_y_index(index))
            .collect::<Vec<_>>();
        tracing::debug!(
            target: "rumoca_solver::projection",
            block_index,
            rows = ?block.rows,
            y_indices = ?block.y_indices,
            variables = ?variables,
            matrix_rows = self.jacobian.nrows(),
            matrix_columns = self.jacobian.ncols(),
            matrix_column_major = ?self.jacobian.as_dense().as_slice(),
            rhs = ?rhs.as_slice(),
            point_y = ?y,
            parameters = ?args.parameters,
            time = args.time,
            "singular algebraic sensitivity block"
        );
    }
}

pub(super) fn certify_with_refinement<M: ImplicitProjectionModel>(
    model: &M,
    plan: &solve::AlgebraicProjectionPlan,
    y: &[f64],
    args: AlgebraicProjectionArgs<'_>,
    seed: &mut [f64],
    mut row_scales: Vec<f64>,
) -> Result<(), RuntimeSolveError> {
    let rows = projection_rows(plan);
    for iteration in 0..=2 {
        let residual = implicit_selected_jacobian_v_rows(
            model,
            y,
            args.parameters,
            args.time,
            seed,
            &rows,
            "algebraic projection sensitivity",
        )?;
        if scaled_residual_converged(&residual, &row_scales, args.tolerance) {
            return Ok(());
        }
        if iteration == 2 {
            return Err(projection_error_for_rows(
                model,
                "algebraic projection sensitivity did not satisfy the selected residual system",
                &rows,
                &residual,
                &row_scales,
                args.tolerance,
            ));
        }
        row_scales.clear();
        for (index, block) in plan.blocks.iter().enumerate() {
            let linearization = model.algebraic_seed_linearization(index, block, y, args)?;
            refine_direction(model, block, &linearization, y, args, seed)?;
            row_scales.extend(linearization.row_scales(model, index, block, seed));
        }
    }
    unreachable!("bounded refinement returns its final residual check")
}

fn refine_direction<M: ImplicitProjectionModel>(
    model: &M,
    block: &solve::AlgebraicProjectionBlock,
    linearization: &SeedBlockLinearization,
    y: &[f64],
    args: AlgebraicProjectionArgs<'_>,
    seed: &mut [f64],
) -> Result<(), RuntimeSolveError> {
    let mut inputs = seed.to_vec();
    for &index in &block.y_indices {
        inputs[index] = 0.0;
    }
    // Refine the same assembled linear system, not an inconsistent combined
    // JVP. Later blocks see corrected predecessor directions in their RHS.
    let base = DVector::from_vec(implicit_selected_jacobian_v_rows(
        model,
        y,
        args.parameters,
        args.time,
        &inputs,
        &block.rows,
        "algebraic sensitivity refinement rhs",
    )?);
    let values = DVector::from_iterator(
        block.y_indices.len(),
        block.y_indices.iter().map(|&index| seed[index]),
    );
    let residual = linearization.jacobian.mul_vector(&values) + base;
    if residual.iter().all(|&value| value == 0.0) {
        return Ok(());
    }
    let correction = linearization.solve(&(-residual)).ok_or_else(|| {
        RuntimeSolveError::DirectionalDerivativeUnavailable {
            reason: "algebraic projection sensitivity matrix is singular during refinement".into(),
        }
    })?;
    for (&index, correction) in block.y_indices.iter().zip(correction.iter()) {
        let value = seed[index] + correction;
        if !value.is_finite() {
            return Err(RuntimeSolveError::DirectionalDerivativeUnavailable {
                reason: format!(
                    "algebraic projection produced a non-finite sensitivity for y[{index}]"
                ),
            });
        }
        seed[index] = value;
    }
    Ok(())
}
