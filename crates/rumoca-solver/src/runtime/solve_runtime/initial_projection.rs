use crate::RuntimeSolveError;
use crate::runtime::projection::{
    AlgebraicProjectionModel, ImplicitProjectionModel, InitialHomotopySystem, ScaledNewtonSystem,
    project_initial_variables_with_homotopy,
};
use nalgebra::DVector;

use super::initial_continuation::InitialContinuationCoverage;

use super::*;

struct InitialProjectionModel<'a> {
    runtime: &'a SolveRuntime,
    tol: f64,
    max_iters: usize,
    refreshes_algebraic_reads: bool,
}

impl ImplicitProjectionModel for InitialProjectionModel<'_> {
    fn eval_residual(
        &self,
        y: &[f64],
        p: &[f64],
        t: f64,
        out: &mut [f64],
    ) -> Result<(), RuntimeSolveError> {
        if let Some(compiled) = self.runtime.compiled_implicit_rhs.as_ref()
            && compiled
                .call(y, p, t, self.runtime.model.external_tables.as_slice(), out)
                .is_ok()
        {
            self.runtime
                .report_nonfinite_implicit_residual_inputs(t, y, out);
            return Ok(());
        }
        self.runtime
            .implicit_rhs
            .eval_with_context(y, p, t, self.runtime.row_eval_context(), out)
            .map_err(RuntimeSolveError::from)?;
        self.runtime
            .report_nonfinite_implicit_residual_inputs(t, y, out);
        Ok(())
    }

    fn eval_jacobian_v(
        &self,
        y: &[f64],
        p: &[f64],
        t: f64,
        v: &[f64],
        out: &mut [f64],
    ) -> Result<(), RuntimeSolveError> {
        if let Some(compiled) = self
            .runtime
            .compiled_implicit_projection_jacobian_v
            .as_ref()
            && compiled
                .call(
                    y,
                    p,
                    t,
                    v,
                    self.runtime.model.external_tables.as_slice(),
                    out,
                )
                .is_ok()
        {
            return Ok(());
        }
        self.runtime
            .implicit_projection_jacobian_v
            .eval_with_context(
                y,
                p,
                t,
                RowEvalContext {
                    seed: Some(v),
                    ..self.runtime.row_eval_context()
                },
                out,
            )
            .map_err(Into::into)
    }

    fn implicit_target(&self, row_idx: usize) -> Option<solve::ScalarSlot> {
        self.runtime
            .model
            .problem
            .continuous
            .implicit_row_targets
            .get(row_idx)
            .copied()
            .flatten()
    }

    #[cfg(test)]
    fn algebraic_projection_plan(&self) -> &solve::AlgebraicProjectionPlan {
        &self
            .runtime
            .model
            .problem
            .continuous
            .algebraic_projection_plan
    }

    fn algebraic_projection_plan_is_validated(&self) -> bool {
        false
    }

    fn algebraic_projection_block_structure(
        &self,
        block_index: usize,
    ) -> Option<&solve::JacobianStructure> {
        self.runtime
            .continuous_structural
            .algebraic_projection()
            .get(block_index)
    }

    fn algebraic_projection_block_invalidates_earlier(&self, block_index: usize) -> bool {
        self.runtime
            .continuous_structural
            .algebraic_invalidates_earlier(block_index)
            .unwrap_or(true)
    }

    fn solve_algebraic_newton_delta(
        &self,
        block_index: usize,
        system: ScaledNewtonSystem<'_>,
    ) -> Option<DVector<f64>> {
        self.runtime
            .algebraic_newton_caches
            .get(block_index)
            .map_or_else(
                || crate::runtime::projection::scaled_newton_delta(system),
                |cache| {
                    crate::runtime::projection::scaled_newton_delta_with_cache(
                        system,
                        &mut cache.borrow_mut(),
                    )
                },
            )
    }

    fn target_name_for_row(&self, row_idx: usize) -> Option<&str> {
        self.runtime
            .model
            .problem
            .continuous
            .implicit_row_targets
            .get(row_idx)
            .copied()
            .flatten()
            .and_then(|slot| match slot {
                solve::ScalarSlot::Y { index, .. } => Some(index),
                _ => None,
            })
            .and_then(|index| {
                self.runtime
                    .model
                    .problem
                    .solve_layout
                    .solver_maps
                    .names
                    .get(index)
            })
            .map(String::as_str)
    }

    fn implicit_target_assignment_is_exact(&self, row_idx: usize, target_y_index: usize) -> bool {
        self.runtime
            .implicit_scalar_rhs
            .row_output_position(row_idx)
            .is_some_and(|(program_idx, output_offset)| {
                self.runtime
                    .implicit_scalar_rhs
                    .certifies_exact_target_assignment_output(
                        program_idx,
                        output_offset,
                        target_y_index,
                    )
            })
    }

    fn variable_name_for_y_index(&self, y_index: usize) -> Option<&str> {
        self.runtime
            .model
            .problem
            .solve_layout
            .solver_maps
            .names
            .get(y_index)
            .map(String::as_str)
    }

    fn variable_scale_for_y_index(&self, y_index: usize) -> f64 {
        self.runtime.model.solver_variable_scale(y_index)
    }
}

impl AlgebraicProjectionModel for InitialProjectionModel<'_> {
    fn eval_initial_residual(
        &self,
        y: &[f64],
        p: &[f64],
        t: f64,
        rows: Option<&[usize]>,
        out: &mut [f64],
    ) -> Result<(), RuntimeSolveError> {
        // An initialization row observes the simultaneous continuous system at
        // the initial instant (MLS 3.6 §8.6), never the declaration seeds of
        // algebraic/output coordinates.  Refresh those derived coordinates on
        // an evaluation-local view: rows that the projection can solve remain
        // the only writers of `y`, while the complete-residual certificate sees
        // the values the continuous equations actually determine.
        let settled = self
            .refreshes_algebraic_reads
            .then(|| {
                let blocks = rows.and_then(|rows| self.runtime.settled_blocks_read_by(rows));
                self.settled_initial_coordinates(y, p, t, blocks.as_deref())
            })
            .transpose()?;
        let (residual_y, residual_p) = match &settled {
            Some((settled_y, settled_p)) => (settled_y.as_slice(), settled_p.as_slice()),
            None => (y, p),
        };
        if let Some(compiled) = self.runtime.compiled_initial_residual.as_ref()
            && compiled
                .call(
                    residual_y,
                    residual_p,
                    t,
                    self.runtime.model.external_tables.as_slice(),
                    out,
                )
                .is_ok()
        {
            return Ok(());
        }
        self.runtime
            .initial_residual
            .eval_with_context(
                residual_y,
                residual_p,
                t,
                self.runtime.row_eval_context(),
                out,
            )
            .map_err(Into::into)
    }

    fn initial_residual_len(&self) -> usize {
        self.runtime.initial_residual.len()
    }

    fn initial_target(&self, row_idx: usize) -> Option<solve::ScalarSlot> {
        self.runtime
            .model
            .problem
            .initialization
            .row_targets()
            .get(row_idx)
            .copied()
            .flatten()
    }

    fn initial_projection_block_structure(
        &self,
        block_index: usize,
    ) -> Option<&solve::JacobianStructure> {
        self.runtime
            .initialization_structural
            .projection()
            .get(block_index)
    }

    fn initial_row_role(&self, row_idx: usize) -> Option<solve::InitializationRowRole> {
        self.runtime
            .model
            .problem
            .initialization
            .row_roles()
            .get(row_idx)
            .copied()
    }

    fn eval_initial_jacobian_v(
        &self,
        y: &[f64],
        p: &[f64],
        t: f64,
        v: &[f64],
        rows: Option<&[usize]>,
        out: &mut [f64],
    ) -> Result<(), RuntimeSolveError> {
        if self.refreshes_algebraic_reads {
            return self.eval_settled_initial_jacobian_v((y, p, t), v, rows, out);
        }
        self.eval_partial_initial_jacobian_v(y, p, t, v, out)
    }

    fn eval_initial_target_value(
        &self,
        output_index: usize,
        target_y_index: usize,
        y: &[f64],
        p: &[f64],
        t: f64,
    ) -> Result<Option<f64>, RuntimeSolveError> {
        if self.refreshes_algebraic_reads {
            return Ok(None);
        }
        let Some(row_idx) = self
            .runtime
            .initial_scalar_residual
            .single_output_row_for_output_index(output_index)
        else {
            return Ok(None);
        };
        self.runtime
            .initial_scalar_residual
            .eval_target_assignment_row_unchecked_with_context(
                row_idx,
                target_y_index,
                y,
                p,
                t,
                self.runtime.row_eval_context(),
            )
            .map_err(Into::into)
    }

    fn eval_initial_residual_row(
        &self,
        output_index: usize,
        y: &[f64],
        p: &[f64],
        t: f64,
    ) -> Result<Option<f64>, RuntimeSolveError> {
        if self.refreshes_algebraic_reads {
            return Ok(None);
        }
        let Some(row_idx) = self
            .runtime
            .initial_scalar_residual
            .single_output_row_for_output_index(output_index)
        else {
            return Ok(None);
        };
        self.runtime
            .initial_scalar_residual
            .eval_row_output_unchecked_with_context(
                row_idx,
                0,
                y,
                p,
                t,
                self.runtime.row_eval_context(),
            )
            .map(Some)
            .map_err(Into::into)
    }
}

impl InitialProjectionModel<'_> {
    /// Construct the simultaneous initialization view seen by residual rows.
    ///
    /// Parameter bindings and other construction-issued initialization updates
    /// are part of the same MLS §8.6 system as the continuous algebraics. A
    /// projection probe therefore has to re-apply those updates to its local
    /// parameter vector before refreshing algebraics; otherwise an outer
    /// `fixed=false` parameter can move while a nested bound parameter remains
    /// at its declaration seed, making the residual spuriously insensitive.
    ///
    /// With `blocks` (positions in the algebraic refresh's simultaneous plan,
    /// the construction-issued read cone of the rows being evaluated, see
    /// [`SolveRuntime::settled_blocks_read_by`]), only those blocks are
    /// reconstructed. The cone is closed over the blocks it reads, so the rows
    /// see exactly the values the complete view gives them, while a block
    /// outside it that is undefined at the point (an enthalpy `h` of
    /// `H = m*h` at a startless zero mass `m`, before the block that projects
    /// `m` has run) never fails their evaluation.
    fn settled_initial_coordinates(
        &self,
        y: &[f64],
        p: &[f64],
        t: f64,
        blocks: Option<&[usize]>,
    ) -> Result<(Vec<f64>, Vec<f64>), RuntimeSolveError> {
        let mut settled_y = y.to_vec();
        let mut settled_p = p.to_vec();
        let subset = blocks.map(|blocks| self.runtime.refresh_block_subset(blocks));
        for pass in 0..self.max_iters {
            let changed = self.runtime.apply_initialization_updates(
                &mut settled_y,
                &mut settled_p,
                t,
                self.tol,
                self.max_iters,
            )?;
            match &subset {
                Some((plan, indices)) => self.runtime.refresh_algebraic_block_subset(
                    (plan, indices),
                    t,
                    &mut settled_y,
                    &settled_p,
                    (self.tol, self.max_iters),
                )?,
                None => self.runtime.refresh_algebraic_and_output_slots(
                    t,
                    &mut settled_y,
                    &settled_p,
                    self.tol,
                    self.max_iters,
                )?,
            }
            if pass > 0 && !changed {
                return Ok((settled_y, settled_p));
            }
        }
        Err(RuntimeSolveError::solve_ir(format!(
            "initial residual evaluation view did not converge at t={t}"
        )))
    }

    /// Total directional derivative of the initialization residual after the
    /// simultaneous continuous algebraics have been reconstructed.
    ///
    /// The compiled residual JVP differentiates stored coordinates and so
    /// cannot see `d settled(y, p) / d(y, p)`. The direction is carried through
    /// the fixed point that settles the view ([`SolveRuntime::settled_initial_tangent`])
    /// and the residual JVP then reads that tangent at the settled point.
    ///
    /// With `rows`, only those rows' entries are needed, so the tangent carries
    /// the direction through only the algebraic blocks they read
    /// ([`SolveRuntime::settled_blocks_read_by`]). A block outside that set
    /// cannot move them, and linearizing it anyway would refuse the whole
    /// derivative wherever that block alone is singular.
    fn eval_settled_initial_jacobian_v(
        &self,
        (y, p, t): (&[f64], &[f64], f64),
        v: &[f64],
        rows: Option<&[usize]>,
        out: &mut [f64],
    ) -> Result<(), RuntimeSolveError> {
        let expected = y.len().checked_add(p.len()).ok_or_else(|| {
            RuntimeSolveError::solve_ir(
                "initial total-sensitivity vector length exceeds host index range".to_string(),
            )
        })?;
        if v.len() != expected {
            return Err(RuntimeSolveError::solve_ir(format!(
                "initial total-sensitivity seed has {} values, expected {expected}",
                v.len()
            )));
        }
        let blocks = rows.and_then(|rows| self.runtime.settled_blocks_read_by(rows));
        let (settled_y, settled_p) =
            self.settled_initial_coordinates(y, p, t, blocks.as_deref())?;
        let settle = AlgebraicSettle {
            tol: self.tol,
            max_iters: self.max_iters,
        };
        let seed = self.runtime.settled_initial_tangent(
            (&settled_y, &settled_p, t),
            v,
            blocks.as_deref(),
            settle,
        )?;
        self.eval_partial_initial_jacobian_v(&settled_y, &settled_p, t, &seed, out)
    }

    /// The initialization residual's JVP in the stored coordinates.
    fn eval_partial_initial_jacobian_v(
        &self,
        y: &[f64],
        p: &[f64],
        t: f64,
        v: &[f64],
        out: &mut [f64],
    ) -> Result<(), RuntimeSolveError> {
        if let Some(compiled) = self.runtime.compiled_initial_residual_jacobian_v.as_ref()
            && compiled
                .call(
                    y,
                    p,
                    t,
                    v,
                    self.runtime.model.external_tables.as_slice(),
                    out,
                )
                .is_ok()
        {
            return Ok(());
        }
        self.runtime
            .initial_residual_jacobian_v
            .eval_with_context(
                y,
                p,
                t,
                RowEvalContext {
                    seed: Some(v),
                    ..self.runtime.row_eval_context()
                },
                out,
            )
            .map_err(Into::into)
    }
}

impl SolveRuntime {
    /// The initialization residual's directional derivative along `v` (over
    /// `[solver-y | parameter]`) at `(y, p, t)`, as the initialization
    /// projection forms it: through the settled view when a row reads a
    /// continuous algebraic, in the stored coordinates otherwise.
    pub fn eval_initial_residual_jacobian_v(
        &self,
        (y, p, t): (&[f64], &[f64], f64),
        settle: AlgebraicSettle,
        v: &[f64],
        out: &mut [f64],
    ) -> Result<(), RuntimeSolveError> {
        InitialProjectionModel {
            runtime: self,
            tol: settle.tol,
            max_iters: settle.max_iters,
            refreshes_algebraic_reads: self.initial_rows_read_settled_view(),
        }
        .eval_initial_jacobian_v(y, p, t, v, None, out)
    }

    /// Whether an initialization row reads a continuous algebraic, so the
    /// projection evaluates the rows on the settled view.
    fn initial_rows_read_settled_view(&self) -> bool {
        self.model
            .problem
            .initialization
            .row_roles()
            .iter()
            .any(|role| {
                matches!(
                    role,
                    solve::InitializationRowRole::UnownedCoordinate(
                        solve::InitializationCoordinateKind::Algebraic
                    ) | solve::InitializationRowRole::SolvedThroughAlgebraicRefresh
                        | solve::InitializationRowRole::SurplusAlgebraicCheck
                )
            })
    }

    /// Positions, in the algebraic refresh's simultaneous plan, of the blocks
    /// the initialization rows `rows` read: the union of their construction-
    /// issued cones. `None` when every block is needed.
    pub(super) fn settled_blocks_read_by(&self, rows: &[usize]) -> Option<Vec<usize>> {
        let cones = self.settled_read_cones.as_deref()?;
        let mut blocks = Vec::new();
        for &row in rows {
            blocks.extend_from_slice(cones.get(row)?);
        }
        blocks.sort_unstable();
        blocks.dedup();
        Some(blocks)
    }

    /// The blocks at `blocks` (positions in the algebraic refresh's
    /// simultaneous plan) as a plan of their own, in plan order, with each
    /// block's index among the continuous structural projection blocks.
    fn refresh_block_subset(
        &self,
        blocks: &[usize],
    ) -> (solve::AlgebraicProjectionPlan, Vec<usize>) {
        let refresh = &self.algebraic_refresh;
        let plan = solve::AlgebraicProjectionPlan {
            blocks: blocks
                .iter()
                .map(|&block| refresh.simultaneous_plan.blocks[block].clone())
                .collect(),
        };
        let indices = blocks
            .iter()
            .map(|&block| refresh.simultaneous_block_indices[block])
            .collect();
        (plan, indices)
    }

    /// Reconstruct the coordinates of a subset of the algebraic refresh
    /// ([`Self::refresh_block_subset`]) in `solver_y`, by projecting its blocks
    /// in plan order; coordinates outside the subset keep their values.
    fn refresh_algebraic_block_subset(
        &self,
        (plan, block_indices): (&solve::AlgebraicProjectionPlan, &[usize]),
        t: f64,
        solver_y: &mut [f64],
        params: &[f64],
        (tol, max_iters): (f64, usize),
    ) -> Result<(), RuntimeSolveError> {
        let projection_model = RefreshProjectionModel {
            runtime: self,
            seed_linearizations: None,
            #[cfg(test)]
            plan,
            block_indices,
            plan_validated: false,
            jacobian_v: ProjectionJacobian::SolverY {
                block: &self.implicit_projection_jacobian_v,
                scalar: &self.implicit_projection_scalar_jacobian_v,
            },
        };
        crate::runtime::projection::project_algebraics_with_plan(
            &projection_model,
            plan,
            solver_y,
            crate::runtime::projection::AlgebraicProjectionArgs {
                parameters: params,
                time: t,
                state_count: self.state_count,
                tolerance: tol,
            },
            max_iters,
        )
    }

    /// The tangent along `v` (over `[solver-y | parameter]`) of the settled
    /// initialization view at its settled point `(y, p)`.
    ///
    /// The view alternates the initialization updates and the algebraic
    /// refresh until the updates stop changing, so its tangent is the fixed
    /// point of the same alternation, linearized: the update rows' JVP writes
    /// the tangents of their targets, and the refresh's seed linearization
    /// writes the tangents of every coordinate it solves, pass after pass
    /// until the update tangents stop changing. With `blocks`, only those
    /// positions of the refresh plan carry the direction.
    fn settled_initial_tangent(
        &self,
        (y, p, t): (&[f64], &[f64], f64),
        v: &[f64],
        blocks: Option<&[usize]>,
        settle: AlgebraicSettle,
    ) -> Result<Vec<f64>, RuntimeSolveError> {
        let initialization = &self.model.problem.initialization;
        let targets = initialization.update_targets();
        let updates = match (
            targets.is_empty(),
            &self.model.artifacts.initialization.update_jacobian_v,
        ) {
            (true, _) => None,
            (false, Some(block)) => Some(block),
            (false, None) => {
                return Err(RuntimeSolveError::DirectionalDerivativeUnavailable {
                    reason: "an initialization update row has no directional derivative"
                        .to_string(),
                });
            }
        };
        let seed_len = (y.len() + p.len()).max(self.implicit_jacobian_v.requirements().seed_len);
        let mut seed = vec![0.0; seed_len];
        seed[..v.len()].copy_from_slice(v);
        let lin = AlgebraicLinearization {
            t,
            params: p,
            settle,
        };
        let refresh = &self.algebraic_refresh;
        let subset = blocks.map(|blocks| self.refresh_block_subset(blocks));
        let plan = match &subset {
            Some((plan, indices)) => (plan, indices.as_slice()),
            None => (
                &refresh.simultaneous_plan,
                refresh.simultaneous_block_indices.as_slice(),
            ),
        };
        let mut values = vec![0.0; targets.len()];
        for pass in 0..settle.max_iters {
            let mut changed = false;
            if let Some(block) = updates {
                solve_eval::eval_scalar_program_block_with_context(
                    block,
                    y,
                    p,
                    t,
                    RowEvalContext {
                        seed: Some(&seed),
                        ..self.row_eval_context()
                    },
                    &mut values,
                )?;
                changed = write_update_tangents(targets, &values, y.len(), &mut seed);
            }
            self.seed_refresh_blocks(plan, lin, y, &mut seed)?;
            if pass > 0 && !changed {
                return Ok(seed);
            }
        }
        Err(RuntimeSolveError::solve_ir(format!(
            "initial residual tangent did not converge at t={t}"
        )))
    }
}

/// Per initialization residual row, the positions in `plan` of the algebraic
/// refresh blocks the row reads directly or through other blocks, closed over
/// the construction's structural patterns, issued once when the runtime is
/// built. `None` when a block Jacobian must carry its direction through every
/// block: a pattern is unrecorded, or an update row reads an algebraic slot,
/// so the updates' tangents may carry any block's.
pub(super) fn settled_read_cones(
    model: &solve::SolveModel,
    continuous: &solve::ContinuousStructuralArtifacts,
    plan: &solve::AlgebraicProjectionPlan,
) -> Option<Box<[Box<[usize]>]>> {
    let residual = model
        .artifacts
        .initialization
        .structural
        .residual()?
        .pattern();
    let implicit = continuous.implicit()?.pattern();
    if updates_read_algebraic_slots(model) {
        return None;
    }
    Some(read_cones(residual, implicit, &plan.blocks))
}

/// [`settled_read_cones`] over the patterns: `residual` has one row per
/// initialization row and `implicit` one per continuous row, both over
/// `[solver-y | parameter]` columns.
fn read_cones(
    residual: &solve::StructuralPattern,
    implicit: &solve::StructuralPattern,
    blocks: &[solve::AlgebraicProjectionBlock],
) -> Box<[Box<[usize]>]> {
    let columns = residual.columns().max(implicit.columns()) as usize;
    (0..residual.rows() as usize)
        .map(|row| {
            let mut read = vec![false; columns];
            residual.visit_row_columns(row, &mut |column| read[column] = true);
            let mut needed = vec![false; blocks.len()];
            // Plan order puts most readers after the blocks they read, so a
            // sweep from the last block back usually closes the set in one
            // pass; the loop stops at the first pass that adds no block.
            while add_blocks_read(blocks, implicit, &mut read, &mut needed) {}
            (0..blocks.len())
                .filter(|&position| needed[position])
                .collect()
        })
        .collect()
}

/// One sweep, last block first, marking every block that solves a coordinate
/// in `read` and adding the columns its rows read; whether any block was added.
fn add_blocks_read(
    blocks: &[solve::AlgebraicProjectionBlock],
    implicit: &solve::StructuralPattern,
    read: &mut [bool],
    needed: &mut [bool],
) -> bool {
    let mut grew = false;
    for (position, block) in blocks.iter().enumerate().rev() {
        let solves_a_read = block.y_indices.iter().any(|&y| read.get(y) == Some(&true));
        if needed[position] || !solves_a_read {
            continue;
        }
        needed[position] = true;
        grew = true;
        for &row in &block.rows {
            implicit.visit_row_columns(row, &mut |column| read[column] = true);
        }
    }
    grew
}

/// Write the update rows' tangents `values` into the seed entries of their
/// `targets` (parameters follow the `y_len` solver coordinates); whether any
/// entry changed.
pub(super) fn write_update_tangents(
    targets: &[solve::ScalarSlot],
    values: &[f64],
    y_len: usize,
    seed: &mut [f64],
) -> bool {
    let mut changed = false;
    for (target, value) in targets.iter().zip(values.iter().copied()) {
        let index = match *target {
            solve::ScalarSlot::Y { index, .. } => index,
            solve::ScalarSlot::P { index, .. } => y_len + index,
            solve::ScalarSlot::Time | solve::ScalarSlot::Constant(_) => continue,
        };
        if seed[index].to_bits() != value.to_bits() {
            seed[index] = value;
            changed = true;
        }
    }
    changed
}

impl SolveRuntime {
    /// Project the initialization unknowns, sweeping the homotopy continuation
    /// parameter when the model has a solve for it to steer.
    ///
    /// The sweep drives exactly the solves named by the runtime's certified
    /// `InitialContinuationCoverage`: the initialization projection plan and —
    /// whenever that coverage includes implicit rows — the algebraic refresh.
    /// The algebraic refresh has to run *inside* the sweep because that is where
    /// the Solve lowering parks a `homotopy(...)` written in an `equation`
    /// section; refreshing only after the sweep leaves the algebraic solve to
    /// pick a root from a cold guess, which is how
    /// `Modelica.Electrical.Analog.Examples.OpAmps.SignalGenerator` settled onto
    /// the trivial all-zero equilibrium instead of the ±15 V branch the
    /// simplified expression selects.
    ///
    /// When the coverage names no steered solve — every λ read sits in a
    /// derivative, discrete, or unowned initialization row — there is nothing to
    /// sweep and λ stays at its seeded `1.0`, so those rows evaluate to `actual`
    /// (MLS 3.6 §3.7.4.3's trivial implementation).
    pub fn project_initial_variables(
        &self,
        y: &mut [f64],
        p: &mut [f64],
        t: f64,
        tol: f64,
        max_iters: usize,
    ) -> Result<(), RuntimeSolveError> {
        let drives_algebraic_refresh = self
            .initial_continuation
            .as_ref()
            .is_some_and(InitialContinuationCoverage::drives_algebraic_refresh);
        project_initial_variables_with_homotopy(
            InitialHomotopySystem {
                model: &InitialProjectionModel {
                    runtime: self,
                    tol,
                    max_iters,
                    refreshes_algebraic_reads: self.initial_rows_read_settled_view(),
                },
                t,
                plan: self.model.problem.initialization.projection_plan(),
                homotopy_parameter_index: self
                    .initial_continuation
                    .as_ref()
                    .and_then(InitialContinuationCoverage::sweep_parameter_index),
                tol,
                max_iters,
            },
            y,
            p,
            |y, p| {
                if drives_algebraic_refresh {
                    self.refresh_algebraic_and_output_slots_certified(t, y, p, tol, max_iters)?;
                } else {
                    self.refresh_event_dependency_slots_certified(t, y, p, tol, max_iters)?;
                }
                let roots = self.eval_root_conditions_from_solver_y(t, y, p)?;
                self.update_root_relation_memory_from_values(&roots, p, &[])?;
                self.apply_runtime_assignments_until_stable(y, p, t, tol, max_iters)?;
                Ok(())
            },
        )
    }

    /// Settle the complete initialization system, including direct
    /// initialization updates such as generated condition-memory equations.
    ///
    /// Updates run once before projection so the first residual evaluation sees
    /// the initial-time branch, then again afterward because a condition may
    /// depend on a projected algebraic. All solver backends use this shared
    /// fixed-point so state and zero-state models observe identical MLS §8.6
    /// initialization semantics.
    pub fn settle_initialization_system(
        &self,
        y: &mut [f64],
        p: &mut [f64],
        t: f64,
        tol: f64,
        max_iters: usize,
    ) -> Result<(), RuntimeSolveError> {
        // MLS 3.6 §8.6 solves the continuous equations, the initial equations,
        // and the `fixed = false` parameters as one simultaneous system, so a
        // parameter update that reads an algebraic coordinate must observe the
        // value the continuous equations determine, not the algebraic's
        // declaration seed. When an initialization update row reads such a
        // coordinate, reconstruct the algebraics into the coordinate vector
        // after the projection has settled the states each pass, before the
        // update rows re-read them; otherwise a `fixed = false` parameter whose
        // definition depends on an algebraic solved by a nonlinear block (never
        // reconstructed as an explicit assignment) freezes at the branch the
        // seed selects instead of the branch the initialized geometry selects.
        let refresh_algebraics_for_updates = self.initialization_updates_read_algebraic_slots();
        for _ in 0..max_iters {
            // MLS 3.6 §3.7.2 defines delay(u, ...) = u throughout
            // initialization. The delayed-value P slots are runtime storage,
            // so keep them synchronized with the current initialization
            // coordinate before either updates or residuals observe them.
            self.refresh_delay_values(t, y, p)?;
            self.apply_initialization_updates(y, p, t, tol, max_iters)?;
            self.project_initial_variables(y, p, t, tol, max_iters)?;
            // Projection can move a delay source. Refresh its identity value
            // before deciding that the coupled initialization fixed point has
            // converged; otherwise declaration starts can leak into the first
            // event even though the source itself was settled correctly.
            let before_delay_refresh = p.to_vec();
            self.refresh_delay_values(t, y, p)?;
            let delay_changed = crate::runtime_values_changed(&before_delay_refresh, p, tol);
            if refresh_algebraics_for_updates {
                self.refresh_algebraic_and_output_slots(t, y, p, tol, max_iters)?;
            }
            // A relation reads its event memory between events; at the
            // initialization instant that memory must be the relation evaluated
            // at the solution itself (MLS 3.7 §8.6, every equation holds at
            // once), so a change sends the projection round again.
            let state = y[..self.model.state_scalar_count().min(y.len())].to_vec();
            let relations_changed =
                self.update_relation_memory_from_state(t, &state, p, tol, max_iters)?;
            let update_changed = self.apply_initialization_updates(y, p, t, tol, max_iters)?;
            if !delay_changed && !update_changed && !relations_changed {
                return Ok(());
            }
        }
        Err(RuntimeSolveError::solve_ir(format!(
            "initial algebraic/update projection did not converge at t={t}"
        )))
    }

    /// Whether any initialization update row reads an algebraic coordinate.
    ///
    /// States occupy the first `state_scalar_count` solver-vector slots and
    /// algebraic/output coordinates occupy the remainder, so an update program
    /// that loads a solver slot at or beyond the state count depends on a
    /// coordinate the continuous equations determine rather than a state the
    /// projection owns. Such an update needs the algebraics reconstructed into
    /// the coordinate vector before it re-reads them (see the caller). Both the
    /// scalar solver-vector load and the packed tensor load reach the algebraic
    /// range; a vector-valued binding (an analytic-loop geometry read, say) is
    /// lowered to the tensor form, so testing only the scalar load would miss
    /// exactly the reads that select an assembly branch.
    fn initialization_updates_read_algebraic_slots(&self) -> bool {
        updates_read_algebraic_slots(&self.model)
    }
}

/// Whether an initialization update row of `model` reads an algebraic slot.
fn updates_read_algebraic_slots(model: &solve::SolveModel) -> bool {
    let state_count = model.state_scalar_count();
    model
        .problem
        .initialization
        .update_rhs()
        .programs()
        .iter()
        .flatten()
        .any(|op| match op {
            solve::LinearOp::LoadY { index, .. } => *index >= state_count,
            solve::LinearOp::TensorLoad {
                input: solve::TensorInputKind::Y,
                input_start,
                count,
                ..
            } => input_start.saturating_add(*count) > state_count,
            _ => false,
        })
}

#[cfg(test)]
mod read_cone_tests {
    use super::*;

    fn pattern(rows: &[Vec<usize>], columns: usize) -> solve::StructuralPattern {
        let span = rumoca_core::Span::from_offsets(
            rumoca_core::SourceId::from_source_name("read_cone_tests.mo"),
            0,
            1,
        );
        let provenance = solve::PatternProvenance::derived(
            solve::PatternDerivation::DependencyPropagation,
            span,
        )
        .expect("fixture provenance");
        solve::StructuralPattern::from_row_dependencies(rows.len(), columns, rows, provenance)
            .expect("fixture pattern")
    }

    fn block(row: usize, y: usize) -> solve::AlgebraicProjectionBlock {
        solve::AlgebraicProjectionBlock {
            rows: vec![row],
            y_indices: vec![y],
            ..Default::default()
        }
    }

    /// TwoTanks in miniature over `[m, H, level, h, T]`: block 0 solves `level`
    /// from `m`, block 1 solves `h` from `H = m*h` (singular at `m = 0`), and
    /// block 2 solves `T` from `h`. The level row reads only block 0, so the
    /// singular block is outside its cone; the temperature row reads block 2
    /// and, through it, block 1.
    #[test]
    fn an_unread_singular_block_is_outside_the_level_rows_cone() {
        let implicit = pattern(&[vec![0, 2], vec![0, 1, 3], vec![3, 4]], 5);
        let residual = pattern(&[vec![2], vec![4]], 5);
        let cones = read_cones(
            &residual,
            &implicit,
            &[block(0, 2), block(1, 3), block(2, 4)],
        );
        assert_eq!(&*cones[0], &[0]);
        assert_eq!(&*cones[1], &[1, 2]);
    }

    /// A block that reads a later block of the plan still enters the cone: the
    /// sweep repeats until it adds nothing.
    #[test]
    fn a_block_reading_a_later_block_is_closed_over() {
        let implicit = pattern(&[vec![1], vec![2], vec![]], 3);
        let residual = pattern(&[vec![0]], 3);
        let cones = read_cones(
            &residual,
            &implicit,
            &[block(2, 2), block(0, 0), block(1, 1)],
        );
        assert_eq!(&*cones[0], &[0, 1, 2]);
    }
}

#[cfg(test)]
mod tests {
    use indexmap::IndexMap;
    use rumoca_core::{BytePos, SourceId, Span};

    use super::*;

    fn test_span() -> Span {
        Span::new(
            SourceId::from_source_name("initial_homotopy_runtime.mo"),
            BytePos(0),
            BytePos(1),
        )
    }

    fn scalar_block(programs: Vec<Vec<solve::LinearOp>>) -> solve::ScalarProgramBlock {
        solve::ScalarProgramBlock::with_source_span(
            programs,
            test_span()
                .require_provenance("initial-projection runtime fixture")
                .expect("fixture span is source-backed"),
        )
        .expect("fixture program is computable")
    }

    fn block(programs: Vec<Vec<solve::LinearOp>>) -> solve::ComputeBlock {
        solve::ComputeBlock::from_scalar_program_block(scalar_block(programs))
    }

    #[test]
    fn runtime_initial_projection_finishes_with_actual_homotopy_system() {
        let residual = block(vec![vec![
            solve::LinearOp::LoadY { dst: 0, index: 0 },
            solve::LinearOp::LoadP { dst: 1, index: 0 },
            solve::LinearOp::Binary {
                dst: 2,
                op: solve::BinaryOp::Sub,
                lhs: 0,
                rhs: 1,
            },
            solve::LinearOp::StoreOutput { src: 2 },
        ]]);
        let jacobian = block(vec![vec![
            solve::LinearOp::LoadSeed { dst: 0, index: 0 },
            solve::LinearOp::StoreOutput { src: 0 },
        ]]);
        let implicit = block(vec![vec![
            solve::LinearOp::LoadY { dst: 0, index: 0 },
            solve::LinearOp::StoreOutput { src: 0 },
        ]]);
        let model = solve::SolveModel {
            problem: solve::SolveProblem {
                solve_layout: solve::SolveLayout {
                    solver_maps: solve::SolverNameIndexMaps {
                        names: vec!["x".to_string()],
                        name_to_idx: IndexMap::from([("x".to_string(), 0)]),
                        base_to_indices: IndexMap::from([("x".to_string(), vec![0])]),
                    },
                    state_scalar_count: 1,
                    compiled_parameter_len: 1,
                    initial_homotopy_parameter_index: Some(0),
                    ..Default::default()
                },
                continuous: solve::ContinuousSolveSystem {
                    implicit_rhs: implicit,
                    implicit_row_targets: vec![Some(solve::scalar_slot_y(0))],
                    ..Default::default()
                },
                initialization: solve::InitializationSolveSystem::construct(
                    solve::InitializationSystemInput {
                        row_roles: vec![solve::InitializationRowRole::Solved],
                        residual,
                        projection_plan: solve::InitializationProjectionPlan {
                            iterates_discretes: false,
                            blocks: vec![solve::InitializationProjectionBlock {
                                rows: vec![0],
                                unknowns: vec![solve::scalar_slot_y(0)],
                                scales: vec![solve::InitializationUnknownScale::Solver],
                            }],
                        },
                        ..Default::default()
                    },
                )
                .expect("initialization fixture has one checked owner per coordinate"),
                ..Default::default()
            },
            artifacts: solve::SolveArtifacts {
                discrete: Default::default(),
                continuous: solve::ContinuousSolveArtifacts {
                    implicit_jacobian_v: jacobian.clone(),
                    implicit_jacobian_v_scalar: to_scalar_program_block(&jacobian)
                        .expect("test Jacobian should scalarize"),
                    ..Default::default()
                },
                initialization: solve::InitializationSolveArtifacts {
                    residual_jacobian_v: jacobian,
                    ..Default::default()
                },
            },
            initial_y: vec![0.0],
            parameters: vec![0.0],
            ..Default::default()
        };
        let runtime = SolveRuntime::new_fixture(&model).expect("runtime should prepare");
        let mut y = model.initial_y.clone();
        let mut p = model.parameters.clone();

        runtime
            .project_initial_variables(&mut y, &mut p, 0.0, 1.0e-10, 8)
            .expect("continuation should project the actual system");

        assert_eq!(p, vec![1.0]);
        assert!((y[0] - 1.0).abs() <= 1.0e-10);
    }

    #[test]
    fn settled_initialization_refreshes_delay_identity_from_the_projected_source() {
        let initial = block(vec![vec![
            solve::LinearOp::LoadY { dst: 0, index: 0 },
            solve::LinearOp::Const { dst: 1, value: 2.0 },
            solve::LinearOp::Binary {
                dst: 2,
                op: solve::BinaryOp::Sub,
                lhs: 0,
                rhs: 1,
            },
            solve::LinearOp::StoreOutput { src: 2 },
        ]]);
        let jacobian = block(vec![vec![
            solve::LinearOp::LoadSeed { dst: 0, index: 0 },
            solve::LinearOp::StoreOutput { src: 0 },
        ]]);
        let delay_time = scalar_block(vec![vec![
            solve::LinearOp::Const { dst: 0, value: 0.1 },
            solve::LinearOp::StoreOutput { src: 0 },
        ]]);
        let model = solve::SolveModel {
            problem: solve::SolveProblem {
                solve_layout: solve::SolveLayout {
                    solver_maps: solve::SolverNameIndexMaps {
                        names: vec!["source".to_string()],
                        name_to_idx: IndexMap::from([("source".to_string(), 0)]),
                        base_to_indices: IndexMap::from([("source".to_string(), vec![0])]),
                    },
                    state_scalar_count: 1,
                    compiled_parameter_len: 1,
                    ..Default::default()
                },
                initialization: solve::InitializationSolveSystem::construct(
                    solve::InitializationSystemInput {
                        row_roles: vec![solve::InitializationRowRole::Solved],
                        residual: initial,
                        projection_plan: solve::InitializationProjectionPlan {
                            iterates_discretes: false,
                            blocks: vec![solve::InitializationProjectionBlock {
                                rows: vec![0],
                                unknowns: vec![solve::scalar_slot_y(0)],
                                scales: vec![solve::InitializationUnknownScale::Solver],
                            }],
                        },
                        ..Default::default()
                    },
                )
                .expect("initialization fixture has one checked owner per coordinate"),
                events: solve::SolveEventPartition {
                    delays: solve::SolveDelayPartition {
                        source_rhs: scalar_block(vec![vec![
                            solve::LinearOp::LoadY { dst: 0, index: 0 },
                            solve::LinearOp::StoreOutput { src: 0 },
                        ]]),
                        delay_time_rhs: delay_time.clone(),
                        delay_max_rhs: delay_time,
                        value_parameter_indices: vec![0],
                        source_is_discrete: vec![false],
                    },
                    ..Default::default()
                },
                ..Default::default()
            },
            artifacts: solve::SolveArtifacts {
                discrete: Default::default(),
                initialization: solve::InitializationSolveArtifacts {
                    residual_jacobian_v: jacobian,
                    ..Default::default()
                },
                ..Default::default()
            },
            initial_y: vec![0.0],
            parameters: vec![0.0],
            ..Default::default()
        };
        let runtime = SolveRuntime::new_fixture(&model).expect("runtime should prepare");
        let mut y = model.initial_y.clone();
        let mut p = model.parameters.clone();
        runtime
            .initialize_delay_history(0.0, &y, &mut p)
            .expect("delay history should seed from the declaration start");

        runtime
            .settle_initialization_system(&mut y, &mut p, 0.0, 1.0e-10, 8)
            .expect("initialization and delay identity should settle together");

        assert!((y[0] - 2.0).abs() <= 1.0e-10);
        assert!((p[0] - 2.0).abs() <= 1.0e-10);
    }

    /// The full implicit JVP over `[a | q, nested_q]` seeds and the update
    /// row's JVP, which carry a `q` direction through `nested_q` into `a`.
    fn fixed_algebraic_tangent_programs() -> (solve::ScalarProgramBlock, solve::ScalarProgramBlock)
    {
        let implicit_full_jacobian = block(vec![vec![
            solve::LinearOp::LoadSeed { dst: 0, index: 0 },
            solve::LinearOp::LoadSeed { dst: 1, index: 2 },
            solve::LinearOp::Binary {
                dst: 2,
                op: solve::BinaryOp::Sub,
                lhs: 0,
                rhs: 1,
            },
            solve::LinearOp::StoreOutput { src: 2 },
        ]]);
        let update_jacobian = block(vec![vec![
            solve::LinearOp::LoadSeed { dst: 0, index: 1 },
            solve::LinearOp::StoreOutput { src: 0 },
        ]]);
        let scalar = |block: &solve::ComputeBlock| {
            to_scalar_program_block(block).expect("fixture tangent programs scalarize")
        };
        (scalar(&implicit_full_jacobian), scalar(&update_jacobian))
    }

    fn fixed_algebraic_parameter_model() -> solve::SolveModel {
        // Continuous a = nested_q - 49; initialization update nested_q = q; initial
        // equation a = 0. The compiled partial JVP of the initial row w.r.t. q is
        // zero (it reads stored `a`); the settled total derivative is one.
        let implicit = block(vec![vec![
            solve::LinearOp::LoadY { dst: 0, index: 0 },
            solve::LinearOp::LoadP { dst: 1, index: 1 },
            solve::LinearOp::Const {
                dst: 2,
                value: 49.0,
            },
            solve::LinearOp::Binary {
                dst: 3,
                op: solve::BinaryOp::Sub,
                lhs: 1,
                rhs: 2,
            },
            solve::LinearOp::Binary {
                dst: 4,
                op: solve::BinaryOp::Sub,
                lhs: 0,
                rhs: 3,
            },
            solve::LinearOp::StoreOutput { src: 4 },
        ]]);
        let initial = block(vec![vec![
            solve::LinearOp::LoadY { dst: 0, index: 0 },
            solve::LinearOp::StoreOutput { src: 0 },
        ]]);
        let implicit_jacobian = block(vec![vec![
            solve::LinearOp::LoadSeed { dst: 0, index: 0 },
            solve::LinearOp::StoreOutput { src: 0 },
        ]]);
        // The initial row's partial JVP is the same identity seed program.
        let partial_initial_jacobian = implicit_jacobian.clone();
        let (implicit_full_jacobian, update_jacobian) = fixed_algebraic_tangent_programs();
        let dependent_update = block(vec![vec![
            solve::LinearOp::LoadP { dst: 0, index: 0 },
            solve::LinearOp::StoreOutput { src: 0 },
        ]]);
        solve::SolveModel {
            problem: solve::SolveProblem {
                solve_layout: solve::SolveLayout {
                    solver_maps: solve::SolverNameIndexMaps {
                        names: vec!["a".to_string()],
                        name_to_idx: IndexMap::from([("a".to_string(), 0)]),
                        base_to_indices: IndexMap::from([("a".to_string(), vec![0])]),
                    },
                    compiled_parameter_len: 2,
                    ..Default::default()
                },
                continuous: solve::ContinuousSolveSystem {
                    implicit_rhs: implicit,
                    implicit_row_targets: vec![Some(solve::scalar_slot_y(0))],
                    algebraic_projection_plan: solve::AlgebraicProjectionPlan {
                        blocks: vec![solve::AlgebraicProjectionBlock {
                            rows: vec![0],
                            y_indices: vec![0],
                            tearing: None,
                            alternate_charts: Vec::new(),
                        }],
                    },
                    ..Default::default()
                },
                initialization: solve::InitializationSolveSystem::construct(
                    solve::InitializationSystemInput {
                        residual: initial,
                        row_roles: vec![
                            solve::InitializationRowRole::SolvedThroughAlgebraicRefresh,
                        ],
                        projection_plan: solve::InitializationProjectionPlan {
                            iterates_discretes: false,
                            blocks: vec![solve::InitializationProjectionBlock {
                                rows: vec![0],
                                unknowns: vec![solve::scalar_slot_p(0)],
                                scales: vec![solve::InitializationUnknownScale::GuessMagnitude],
                            }],
                        },
                        update_rhs: to_scalar_program_block(&dependent_update)
                            .expect("dependent parameter update should scalarize"),
                        update_targets: vec![solve::scalar_slot_p(1)],
                        manifold_row_count: 0,
                        given_state_indices: Vec::new(),
                    },
                )
                .expect("initialization fixture has one checked owner per coordinate"),
                ..Default::default()
            },
            artifacts: solve::SolveArtifacts {
                discrete: Default::default(),
                continuous: solve::ContinuousSolveArtifacts {
                    implicit_jacobian_v: implicit_jacobian.clone(),
                    implicit_jacobian_v_scalar: implicit_full_jacobian,
                    ..Default::default()
                },
                initialization: solve::InitializationSolveArtifacts {
                    residual_jacobian_v: partial_initial_jacobian,
                    update_jacobian_v: Some(update_jacobian),
                    ..Default::default()
                },
            },
            initial_y: vec![51.0],
            parameters: vec![100.0, 100.0],
            ..Default::default()
        }
    }

    #[test]
    fn fixed_algebraic_row_uses_total_sensitivity_of_continuous_refresh() {
        let model = fixed_algebraic_parameter_model();
        let runtime = SolveRuntime::new_fixture(&model).expect("runtime should prepare");
        let mut y = model.initial_y.clone();
        let mut p = model.parameters.clone();

        runtime
            .settle_initialization_system(&mut y, &mut p, 0.0, 1.0e-9, 12)
            .expect("the settled algebraic equation should determine q");

        assert!((p[0] - 49.0).abs() <= 1.0e-7, "q={}", p[0]);
        assert!((p[1] - 49.0).abs() <= 1.0e-7, "nested_q={}", p[1]);
    }
}
