//! One immutable block matrix shared by its directional right-hand sides.

use super::*;
#[cfg(test)]
use std::cell::Cell;
use std::cell::{OnceCell, RefCell};

type DenseFactor = nalgebra::linalg::LU<f64, nalgebra::Dyn, nalgebra::Dyn>;

#[cfg(test)]
thread_local! {
    static DENSE_FACTORIZATIONS: Cell<usize> = const { Cell::new(0) };
    static TORN_ATTEMPTS: RefCell<Vec<solve::TearingCandidate>> = const { RefCell::new(Vec::new()) };
}

#[cfg(test)]
pub(crate) fn seed_dense_factorization_count() -> usize {
    DENSE_FACTORIZATIONS.get()
}

#[cfg(test)]
pub(super) fn seed_torn_attempts() -> Vec<solve::TearingCandidate> {
    TORN_ATTEMPTS.with(|attempts| attempts.borrow().clone())
}

struct SeedTornFactor {
    // Capture only layouts issued by the structure that assembled this J.
    candidates: Box<[(solve::TearingCandidate, solve::AffineEliminationLayout)]>,
    unit_scales: Box<[f64]>,
    cache: RefCell<SparseNewtonCache>,
}

impl SeedTornFactor {
    fn solve(&self, jacobian: &DMatrix<f64>, rhs: &DVector<f64>) -> Option<DVector<f64>> {
        let mut cache = self.cache.try_borrow_mut().ok()?;
        let residual = -rhs;
        for (candidate, layout) in &self.candidates {
            #[cfg(test)]
            TORN_ATTEMPTS.with(|attempts| attempts.borrow_mut().push(*candidate));
            let system = ScaledNewtonSystem {
                jacobian,
                residual: residual.as_slice(),
                row_scales: &self.unit_scales,
                variable_scales: &self.unit_scales,
                structure: Some(layout.pattern()),
                tolerance: 0.0,
            };
            if let Some(solution) =
                scaled_newton_delta_with_tearing(system, &mut cache, *candidate, layout)
            {
                return Some(solution);
            }
        }
        None
    }
}

pub(crate) struct SeedBlockLinearization {
    factor: OnceCell<DenseFactor>,
    torn: Option<SeedTornFactor>,
    jacobian: DMatrix<f64>,
}

impl SeedBlockLinearization {
    pub(crate) fn build(
        model: &dyn ImplicitProjectionModel,
        block_index: usize,
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
        let torn = structure.and_then(|structure| {
            let candidates = block
                .tearing_candidates()
                .filter_map(|(candidate, _)| {
                    structure
                        .affine_elimination_candidate(candidate)
                        .filter(|layout| layout.pattern() == structure.pattern())
                        .map(|layout| (candidate, layout.clone()))
                })
                .collect::<Vec<_>>();
            (!candidates.is_empty()).then(|| SeedTornFactor {
                candidates: candidates.into_boxed_slice(),
                unit_scales: vec![1.0; jacobian.nrows()].into_boxed_slice(),
                cache: RefCell::new(SparseNewtonCache::default()),
            })
        });
        Ok(Self {
            factor: OnceCell::new(),
            torn,
            jacobian,
        })
    }

    pub(super) fn row_scales(
        &self,
        model: &dyn ImplicitProjectionModel,
        block_index: usize,
        block: &solve::AlgebraicProjectionBlock,
        seed: &[f64],
    ) -> Result<Vec<f64>, RuntimeSolveError> {
        let structure = model.algebraic_projection_block_structure(block_index);
        algebraic_block_scales(
            model,
            seed,
            block,
            &self.jacobian,
            structure.map(solve::JacobianStructure::pattern),
        )
        .map(|scales| scales.0)
    }

    pub(super) fn solve(&self, rhs: &DVector<f64>) -> Option<DVector<f64>> {
        if rhs.iter().all(|&value| value == 0.0) {
            return Some(DVector::zeros(rhs.len()));
        }
        if let Some(solution) = self
            .torn
            .as_ref()
            .and_then(|torn| torn.solve(&self.jacobian, rhs))
        {
            return Some(solution);
        }
        self.factor
            .get_or_init(|| {
                #[cfg(test)]
                DENSE_FACTORIZATIONS.set(DENSE_FACTORIZATIONS.get() + 1);
                self.jacobian.clone().lu()
            })
            .solve(rhs)
    }

    #[cfg(test)]
    pub(super) fn with_busy_torn_cache<R>(&self, f: impl FnOnce() -> R) -> R {
        let _busy = self.torn.as_ref().unwrap().cache.borrow_mut();
        f()
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
            matrix_column_major = ?self.jacobian.as_slice(),
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
            return Err(RuntimeSolveError::solve_ir(projection_failure_message(
                model,
                "algebraic projection sensitivity did not satisfy the selected residual system",
                &rows,
                &residual,
                &row_scales,
                args.tolerance,
            )));
        }
        row_scales.clear();
        for (index, block) in plan.blocks.iter().enumerate() {
            let linearization = model.algebraic_seed_linearization(index, block, y, args)?;
            refine_direction(model, block, &linearization, y, args, seed)?;
            row_scales.extend(linearization.row_scales(model, index, block, seed)?);
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
    let residual = &linearization.jacobian * values + base;
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
