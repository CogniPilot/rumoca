use crate::runtime::projection::{ScaledNewtonSystem, per_row_torn_block_sweep};
use nalgebra::DVector;
use rumoca_eval_solve::{PreparedTornSweep, TornSweepStatus};

use super::*;

/// Prepared batched torn-block sweeps, keyed by the address of the plan's
/// `BlockTearing`. The tearing lives inside the runtime's immutable
/// `SolveModel`, so its address is stable for the runtime's lifetime; the
/// cache is dropped on clone because a clone owns a different model
/// allocation, so inherited keys could collide with unrelated tearings.
#[derive(Default)]
pub(super) struct TornSweepCache(RefCell<FxHashMap<usize, Option<Rc<PreparedTornSweepEntry>>>>);

impl Clone for TornSweepCache {
    fn clone(&self) -> Self {
        Self::default()
    }
}

/// One torn block's prepared batched sweep and, when the execution backend
/// accepts the composite, its compiled form.
pub(super) struct PreparedTornSweepEntry {
    sweep: PreparedTornSweep,
    compiled: Option<CompiledTornSweep>,
}

/// Compiled composite of one torn sweep: the causal chain as one assignment
/// schedule (later rows observe earlier writes, exactly as back-substitution
/// does) and the reduced residual rows as one expression block.
struct CompiledTornSweep {
    schedule: Rc<dyn CompiledSolveAssignmentSchedule>,
    residual_block: Rc<dyn CompiledSolveExpression>,
    /// Flat output index of each residual row in the block's output order;
    /// `None` marks a row with no scalar view.
    residual_outputs: Box<[Option<usize>]>,
    /// Total outputs of `residual_block`, sizing its dense output buffer.
    residual_len: usize,
}

pub(super) struct RefreshSlotArgs<'a> {
    pub(super) t: f64,
    pub(super) solver_y: &'a mut [f64],
    pub(super) params: &'a [f64],
    pub(super) tol: f64,
    pub(super) max_iters: usize,
    pub(super) certify_coordinates: bool,
}

struct ProjectionStageSeed<'a> {
    sequence: solve::RefreshSequenceId,
    rows: solve::RefreshRows<'a>,
}

#[derive(Clone, Default)]
pub(super) struct StaticRefreshCache {
    pub(super) valid: bool,
    pub(super) params: Vec<f64>,
    pub(super) values: Vec<Option<f64>>,
}

#[derive(Clone, Default)]
pub(super) struct ParameterStaticGradientCache {
    rows: Vec<CachedParameterStaticGradient>,
}

#[derive(Clone, Default)]
struct CachedParameterStaticGradient {
    parameter_bits: Vec<u64>,
    gradient: Vec<f64>,
    valid: bool,
}

impl ParameterStaticGradientCache {
    fn dot_solver_y_seed(
        &self,
        row: usize,
        parameter_indices: &[usize],
        params: &[f64],
        seed: &[f64],
    ) -> Option<f64> {
        let cached = self.valid_row(row, parameter_indices, params)?;
        (seed.len() >= cached.gradient.len()).then(|| {
            cached
                .gradient
                .iter()
                .zip(seed)
                .map(|(gradient, seed)| gradient * seed)
                .sum()
        })
    }

    fn copy_into(
        &self,
        row: usize,
        parameter_indices: &[usize],
        params: &[f64],
        gradient: &mut [f64],
    ) -> bool {
        let Some(cached) = self.valid_row(row, parameter_indices, params) else {
            return false;
        };
        if cached.gradient.len() != gradient.len() {
            return false;
        }
        gradient.copy_from_slice(&cached.gradient);
        true
    }

    fn valid_row(
        &self,
        row: usize,
        parameter_indices: &[usize],
        params: &[f64],
    ) -> Option<&CachedParameterStaticGradient> {
        let cached = self.rows.get(row).filter(|cached| cached.valid)?;
        (cached.parameter_bits.len() == parameter_indices.len()
            && parameter_indices
                .iter()
                .zip(&cached.parameter_bits)
                .all(|(&index, &bits)| {
                    params
                        .get(index)
                        .is_some_and(|value| value.to_bits() == bits)
                }))
        .then_some(cached)
    }

    fn store(&mut self, row: usize, parameter_indices: &[usize], params: &[f64], gradient: &[f64]) {
        if parameter_indices.iter().any(|&index| index >= params.len()) {
            return;
        }
        if self.rows.len() <= row {
            self.rows.resize_with(
                row.saturating_add(1),
                CachedParameterStaticGradient::default,
            );
        }
        let cached = &mut self.rows[row];
        cached.parameter_bits.clear();
        cached.parameter_bits.extend(
            parameter_indices
                .iter()
                .map(|&index| params[index].to_bits()),
        );
        cached.gradient.clear();
        cached.gradient.extend_from_slice(gradient);
        cached.valid = true;
    }
}

pub(super) fn cached_static_refresh_value(
    cache: &StaticRefreshCache,
    target_index: usize,
) -> Result<f64, RuntimeSolveError> {
    cache
        .values
        .get(target_index)
        .copied()
        .flatten()
        .ok_or_else(|| {
            RuntimeSolveError::solve_ir(
                "parameter-static refresh cache inventory changed during reuse".to_string(),
            )
        })
}

pub(super) fn trace_reverse_projection_coverage(
    model: &solve::SolveModel,
    implicit: &PreparedScalarProgramBlock,
) {
    if !tracing::enabled!(target: "rumoca_eval_solve::refresh", tracing::Level::DEBUG) {
        return;
    }
    let mut coupled_rows = 0usize;
    let mut reverse_rows = 0usize;
    let mut unsupported_kinds = BTreeSet::new();
    for row in model
        .problem
        .continuous
        .algebraic_projection_plan
        .blocks
        .iter()
        .filter(|block| block.rows.len() > 1)
        .flat_map(|block| block.rows.iter().copied())
    {
        coupled_rows += 1;
        let Some(program_idx) = implicit.single_output_row_for_output_index(row) else {
            unsupported_kinds.insert("MissingScalarRow");
            continue;
        };
        if implicit.reverse_row_y_gradient_supported(program_idx) {
            reverse_rows += 1;
        } else {
            unsupported_kinds.extend(implicit.reverse_row_unsupported_op_kinds(program_idx));
        }
    }
    tracing::debug!(
        target: "rumoca_eval_solve::refresh",
        coupled_rows,
        reverse_rows,
        forward_fallback_rows = coupled_rows.saturating_sub(reverse_rows),
        unsupported_kinds = ?unsupported_kinds,
        "coupled projection reverse-row coverage"
    );
}

pub(super) struct RefreshProjectionModel<'a> {
    pub(super) runtime: &'a SolveRuntime,
    pub(super) plan: &'a solve::AlgebraicProjectionPlan,
    pub(super) block_indices: &'a [usize],
    pub(super) plan_validated: bool,
    pub(super) jacobian_v: ProjectionJacobian<'a>,
}

pub(super) struct RuntimeManifoldProjection<'a> {
    pub(super) runtime: &'a SolveRuntime,
}

impl ManifoldProjectionModel for RuntimeManifoldProjection<'_> {
    fn eval_manifold_residual(
        &self,
        y: &[f64],
        p: &[f64],
        t: f64,
        out: &mut [f64],
    ) -> Result<(), RuntimeSolveError> {
        self.runtime
            .manifold_residual
            .eval_with_context(y, p, t, self.runtime.row_eval_context(), out)
            .map_err(Into::into)
    }

    fn eval_manifold_jacobian_v(
        &self,
        y: &[f64],
        p: &[f64],
        t: f64,
        v: &[f64],
        out: &mut [f64],
    ) -> Result<(), RuntimeSolveError> {
        self.runtime
            .manifold_jacobian_v
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

    fn manifold_residual_len(&self) -> usize {
        self.runtime.manifold_residual.len()
    }

    fn manifold_projection_plan(&self) -> &solve::AlgebraicProjectionPlan {
        &self
            .runtime
            .model
            .problem
            .continuous
            .manifold_projection_plan
    }

    fn manifold_projection_block_structure(
        &self,
        block_index: usize,
    ) -> Option<&solve::JacobianStructure> {
        self.runtime
            .continuous_structural
            .manifold_projection()
            .get(block_index)
    }

    fn manifold_variable_scale(&self, y_index: usize) -> f64 {
        self.runtime.model.solver_variable_scale(y_index)
    }
}

#[derive(Clone, Copy)]
pub(super) enum ProjectionJacobian<'a> {
    SolverY {
        block: &'a PreparedComputeBlock,
        scalar: &'a PreparedScalarProgramBlock,
    },
    SolverYAndParameters(&'a PreparedScalarProgramBlock),
}

impl<'a> ProjectionJacobian<'a> {
    fn eval(
        self,
        y: &[f64],
        p: &[f64],
        t: f64,
        context: RowEvalContext<'_>,
        out: &mut [f64],
    ) -> Result<(), EvalSolveError> {
        match self {
            Self::SolverY { block, .. } => block.eval_with_context(y, p, t, context, out),
            Self::SolverYAndParameters(block) => block.eval_with_context(y, p, t, context, out),
        }
    }

    fn scalar(self) -> &'a PreparedScalarProgramBlock {
        match self {
            Self::SolverY { scalar, .. } | Self::SolverYAndParameters(scalar) => scalar,
        }
    }

    fn is_solver_y_only(self) -> bool {
        matches!(self, Self::SolverY { .. })
    }
}

impl ImplicitProjectionModel for RefreshProjectionModel<'_> {
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
        let compiled = match self.jacobian_v {
            ProjectionJacobian::SolverY { .. } => self
                .runtime
                .compiled_implicit_projection_jacobian_v
                .as_ref(),
            ProjectionJacobian::SolverYAndParameters(_) => {
                self.runtime.compiled_implicit_full_jacobian_v.as_ref()
            }
        };
        if let Some(compiled) = compiled
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
        self.jacobian_v
            .eval(
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

    fn eval_implicit_residual_row(
        &self,
        row_idx: usize,
        y: &[f64],
        p: &[f64],
        t: f64,
    ) -> Result<Option<f64>, RuntimeSolveError> {
        let Some((program_idx, output_offset)) = self
            .runtime
            .implicit_scalar_rhs
            .row_output_position(row_idx)
        else {
            return Ok(None);
        };
        let value = self
            .runtime
            .implicit_scalar_rhs
            .eval_row_output_unchecked_with_context(
                program_idx,
                output_offset,
                y,
                p,
                t,
                self.runtime.row_eval_context(),
            )
            .map_err(RuntimeSolveError::from)?;
        self.runtime
            .report_nonfinite_implicit_residual_row_inputs(t, y, row_idx, value);
        Ok(Some(value))
    }

    fn eval_implicit_jacobian_v_row(
        &self,
        row_idx: usize,
        y: &[f64],
        p: &[f64],
        t: f64,
        v: &[f64],
    ) -> Result<Option<f64>, RuntimeSolveError> {
        let block = self.jacobian_v.scalar();
        let Some((jvp_program_idx, output_offset)) = block.row_output_position(row_idx) else {
            return Ok(None);
        };
        let implicit_program_idx = self
            .runtime
            .implicit_scalar_rhs
            .row_output_position(row_idx)
            .map(|(program_idx, _)| program_idx);
        if self.jacobian_v.is_solver_y_only()
            && let Some(implicit_program_idx) = implicit_program_idx
            && let Some(parameter_indices) = self
                .runtime
                .implicit_scalar_rhs
                .parameter_static_y_gradient_params(implicit_program_idx)
            && let Some(value) = self
                .runtime
                .parameter_static_gradient_cache
                .borrow()
                .dot_solver_y_seed(implicit_program_idx, parameter_indices, p, v)
        {
            return Ok(Some(value));
        }
        block
            .eval_row_output_unchecked_with_context(
                jvp_program_idx,
                output_offset,
                y,
                p,
                t,
                RowEvalContext {
                    seed: Some(v),
                    ..self.runtime.row_eval_context()
                },
            )
            .map(Some)
            .map_err(Into::into)
    }

    fn eval_implicit_jacobian_row(
        &self,
        row_idx: usize,
        y: &[f64],
        p: &[f64],
        t: f64,
        gradient: &mut [f64],
    ) -> Result<bool, RuntimeSolveError> {
        let Some((program_idx, _)) = self
            .runtime
            .implicit_scalar_rhs
            .row_output_position(row_idx)
        else {
            return Ok(false);
        };
        let gradient_params = self
            .runtime
            .implicit_scalar_rhs
            .parameter_static_y_gradient_params(program_idx);
        if let Some(parameter_indices) = gradient_params
            && self
                .runtime
                .parameter_static_gradient_cache
                .borrow()
                .copy_into(program_idx, parameter_indices, p, gradient)
        {
            return Ok(true);
        }
        let evaluated = self
            .runtime
            .implicit_scalar_rhs
            .reverse_row_y_gradient(
                program_idx,
                &rumoca_eval_solve::reverse::ReverseInputs {
                    y,
                    p,
                    t,
                    context: self.runtime.row_eval_context(),
                },
                gradient,
                &mut self.runtime.reverse_scratch.borrow_mut(),
            )
            .map_err(RuntimeSolveError::from)?;
        if evaluated && let Some(parameter_indices) = gradient_params {
            self.runtime
                .parameter_static_gradient_cache
                .borrow_mut()
                .store(program_idx, parameter_indices, p, gradient);
        }
        Ok(evaluated)
    }

    fn implicit_jacobian_v_row_depends_on(&self, row_idx: usize, seed_index: usize) -> bool {
        self.runtime
            .continuous_structural
            .implicit()
            .is_none_or(|structure| {
                structure
                    .pattern()
                    .contains(row_idx as u32, seed_index as u32)
            })
    }

    fn algebraic_projection_block_structure(
        &self,
        block_index: usize,
    ) -> Option<&solve::JacobianStructure> {
        let block_index = self.block_indices.get(block_index).copied()?;
        self.runtime
            .continuous_structural
            .algebraic_projection()
            .get(block_index)
    }

    fn algebraic_projection_block_invalidates_earlier(&self, block_index: usize) -> bool {
        let Some(block_index) = self.block_indices.get(block_index).copied() else {
            return true;
        };
        self.runtime
            .continuous_structural
            .algebraic_invalidates_earlier(block_index)
            .unwrap_or(true)
    }

    fn algebraic_projection_block_is_affine(&self, block_index: usize) -> bool {
        self.plan.blocks.get(block_index).is_some_and(|block| {
            block
                .rows
                .iter()
                .all(|&row| self.implicit_row_is_affine(row))
        })
    }

    fn solve_algebraic_newton_delta(
        &self,
        block_index: usize,
        system: ScaledNewtonSystem<'_>,
    ) -> Option<DVector<f64>> {
        let block_index = self.block_indices.get(block_index).copied()?;
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

    fn eval_implicit_target_value(
        &self,
        row_idx: usize,
        target_y_index: usize,
        y: &[f64],
        p: &[f64],
        t: f64,
    ) -> Result<Option<f64>, RuntimeSolveError> {
        let Some((program_idx, output_offset)) = self
            .runtime
            .implicit_scalar_rhs
            .row_output_position(row_idx)
        else {
            return Ok(None);
        };
        self.runtime
            .implicit_scalar_rhs
            .eval_target_assignment_output_unchecked_with_context(
                rumoca_eval_solve::TargetAssignmentOutputRequest {
                    row_idx: program_idx,
                    output_offset,
                    target_y_index,
                    y,
                    p,
                    t,
                    context: self.runtime.row_eval_context(),
                },
            )
            .map_err(Into::into)
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

    /// Batched torn sweep: the compiled composite (one assignment-schedule
    /// call plus one residual-block call) when the backend accepts it, and
    /// otherwise one prepared-block call per sweep, instead of one model call
    /// per causal step and residual row. Every form is built from the same
    /// certified isolators and program outputs the per-row path resolves on
    /// every call, so all paths produce bit-identical values and decline
    /// decisions; the debug agreement check below enforces that.
    fn torn_block_sweep(
        &self,
        tearing: &solve::BlockTearing,
        y: &mut [f64],
        p: &[f64],
        t: f64,
        residual_out: &mut Vec<f64>,
    ) -> Result<bool, RuntimeSolveError> {
        let Some(entry) = self.runtime.prepared_torn_sweep(tearing) else {
            return per_row_torn_block_sweep(self, tearing, y, p, t, residual_out);
        };
        #[cfg(debug_assertions)]
        let entry_y = y.to_vec();
        let mut raw = Vec::with_capacity(tearing.residual_rows.len());
        let compiled_status = entry.compiled.as_ref().and_then(|compiled| {
            self.eval_compiled_torn_sweep(compiled, tearing, y, p, t, &mut raw)
        });
        let status = match compiled_status {
            Some(status) => status,
            None => self
                .runtime
                .implicit_scalar_rhs
                .eval_torn_sweep_unchecked_with_context(
                    &entry.sweep,
                    y,
                    p,
                    t,
                    self.runtime.row_eval_context(),
                    &mut raw,
                )
                .map_err(RuntimeSolveError::from)?,
        };
        let completed = status == TornSweepStatus::Completed;
        if completed {
            residual_out.clear();
            for (&row, value) in tearing.residual_rows.iter().zip(&raw) {
                residual_out.push(self.torn_residual_value(t, y, row, *value));
            }
        }
        #[cfg(debug_assertions)]
        self.debug_assert_torn_sweep_agrees(tearing, &entry_y, p, t, completed, y, residual_out)?;
        Ok(completed)
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
        self.plan
    }

    fn algebraic_projection_plan_is_validated(&self) -> bool {
        self.plan_validated
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

    fn variable_scale_for_y_index(&self, y_index: usize) -> f64 {
        self.runtime.model.solver_variable_scale(y_index)
    }
}

impl RefreshProjectionModel<'_> {
    /// Debug-only strict-refinement guard: replay the sweep through the
    /// per-row reference path from the same entry point and require the same
    /// decline decision and, on completion, bit-identical unknowns and
    /// residual values (NaN compared by bit pattern).
    #[cfg(debug_assertions)]
    // SPEC_0021: Exception - the agreement check compares every input and output of one sweep.
    #[allow(clippy::too_many_arguments)]
    fn debug_assert_torn_sweep_agrees(
        &self,
        tearing: &solve::BlockTearing,
        entry_y: &[f64],
        p: &[f64],
        t: f64,
        batched_completed: bool,
        batched_y: &[f64],
        batched_residual: &[f64],
    ) -> Result<(), RuntimeSolveError> {
        let mut reference_y = entry_y.to_vec();
        let mut reference_residual = Vec::new();
        let reference_completed = per_row_torn_block_sweep(
            self,
            tearing,
            &mut reference_y,
            p,
            t,
            &mut reference_residual,
        )?;
        debug_assert_eq!(
            reference_completed, batched_completed,
            "batched torn sweep disagrees with the per-row sweep on declining"
        );
        if reference_completed && batched_completed {
            let bits_equal = |reference: &[f64], batched: &[f64]| {
                reference.len() == batched.len()
                    && reference
                        .iter()
                        .zip(batched)
                        .all(|(a, b)| a.to_bits() == b.to_bits())
            };
            debug_assert!(
                bits_equal(&reference_y, batched_y),
                "batched torn sweep diverged from the per-row sweep in solver values"
            );
            debug_assert!(
                bits_equal(&reference_residual, batched_residual),
                "batched torn sweep diverged from the per-row sweep in residual values"
            );
        }
        Ok(())
    }

    /// Run one compiled torn sweep, or `None` to fall back to the interpreted
    /// batch. A failed compiled call may leave causal targets partially
    /// written; that needs no restore, because the fallback rewrites every
    /// causal target in order from the untouched tear values before anything
    /// reads them.
    fn eval_compiled_torn_sweep(
        &self,
        compiled: &CompiledTornSweep,
        tearing: &solve::BlockTearing,
        y: &mut [f64],
        p: &[f64],
        t: f64,
        raw: &mut Vec<Option<f64>>,
    ) -> Option<TornSweepStatus> {
        let tables = self.runtime.model.external_tables.as_slice();
        compiled.schedule.call(y, p, t, tables).ok()?;
        // Mirror the per-row decline decision in causal order: the isolator
        // programs poison a singular step to a non-finite value, so the first
        // non-finite target is exactly where the per-row path declines.
        for step in &tearing.causal_steps {
            if !y.get(step.y_index).copied().unwrap_or(f64::NAN).is_finite() {
                return Some(TornSweepStatus::Declined);
            }
        }
        let mut out = vec![0.0; compiled.residual_len];
        compiled
            .residual_block
            .call(y, p, t, tables, &mut out)
            .ok()?;
        raw.clear();
        for position in compiled.residual_outputs.iter() {
            raw.push(position.and_then(|index| out.get(index).copied()));
        }
        Some(TornSweepStatus::Completed)
    }

    /// One residual row's sweep value under the per-row policy: report the
    /// raw value for the non-finite diagnostics, then record NaN for a row
    /// with no scalar view or a non-finite value.
    fn torn_residual_value(&self, t: f64, y: &[f64], row: usize, value: Option<f64>) -> f64 {
        let Some(value) = value else {
            return f64::NAN;
        };
        self.runtime
            .report_nonfinite_implicit_residual_row_inputs(t, y, row, value);
        if value.is_finite() { value } else { f64::NAN }
    }

    fn implicit_row_is_affine(&self, row_idx: usize) -> bool {
        let block = &self.runtime.implicit_scalar_rhs;
        block
            .row_output_position(row_idx)
            .map(|(program_idx, _)| program_idx)
            .is_some_and(|program_idx| block.certifies_parameter_static_y_gradient(program_idx))
    }
}

impl SolveRuntime {
    /// Prepared batched sweep for one torn block, resolved once from the same
    /// certified isolators the per-row path re-resolves on every call, with
    /// its compiled composite when the execution backend accepts it. `None`
    /// is cached too, so an unbatchable block keeps the per-row path without
    /// repeating the resolution.
    pub(super) fn prepared_torn_sweep(
        &self,
        tearing: &solve::BlockTearing,
    ) -> Option<Rc<PreparedTornSweepEntry>> {
        let key = std::ptr::from_ref(tearing) as usize;
        if let Some(prepared) = self.torn_sweep_cache.0.borrow().get(&key) {
            return prepared.clone();
        }
        let prepared = self.build_torn_sweep_entry(tearing).map(Rc::new);
        self.torn_sweep_cache
            .0
            .borrow_mut()
            .insert(key, prepared.clone());
        prepared
    }

    fn build_torn_sweep_entry(
        &self,
        tearing: &solve::BlockTearing,
    ) -> Option<PreparedTornSweepEntry> {
        let causal_steps = tearing
            .causal_steps
            .iter()
            .map(|step| (step.row, step.y_index))
            .collect::<Vec<_>>();
        let sweep = self
            .implicit_scalar_rhs
            .prepare_torn_sweep(&causal_steps, &tearing.residual_rows)?;
        let compiled = self.execution_backend.as_ref().and_then(|backend| {
            let composite = self.implicit_scalar_rhs.torn_sweep_composite(&sweep)?;
            let schedule = optional_compiled(
                "torn_assignment_schedule",
                backend.compile_torn_assignment_rows(
                    &composite.assignment_rows,
                    &composite.assignment_targets,
                ),
            )?;
            let residual_block = optional_compiled(
                "torn_residual_block",
                backend.compile_expression(&composite.residual_block),
            )?;
            Some(CompiledTornSweep {
                schedule,
                residual_block,
                residual_len: composite.residual_block.stored_output_count(),
                residual_outputs: composite.residual_outputs.into_boxed_slice(),
            })
        });
        Some(PreparedTornSweepEntry { sweep, compiled })
    }

    pub(super) fn value_stage_schedule_is_certified(&self, plan: &solve::RefreshPlan) -> bool {
        let structural = self.continuous_structural.algebraic_projection();
        plan.simultaneous_block_indices.len() == plan.simultaneous_plan.blocks.len()
            && !plan.value_stages.is_empty()
            && value_stage_seed_coverage_is_complete(plan)
            && plan
                .simultaneous_block_indices
                .iter()
                .all(|&index| structural.get(index).is_some())
            && plan
                .simultaneous_block_indices
                .iter()
                .skip(1)
                .all(|&index| {
                    self.continuous_structural
                        .algebraic_invalidates_earlier(index)
                        == Some(false)
                })
    }

    pub(super) fn refresh_slots_with_stages(
        &self,
        plan: &solve::RefreshPlan,
        args: &mut RefreshSlotArgs<'_>,
        incoming: &[f64],
    ) -> Result<(), RuntimeSolveError> {
        self.prepare_static_refresh_cache(args.params, args.solver_y.len());
        for stage in &plan.value_stages {
            if !self.execute_refresh_stage(stage, plan, args, incoming)? {
                return Ok(());
            }
        }
        Ok(())
    }

    fn execute_refresh_stage(
        &self,
        stage: &solve::RefreshStage,
        complete_plan: &solve::RefreshPlan,
        args: &mut RefreshSlotArgs<'_>,
        incoming: &[f64],
    ) -> Result<bool, RuntimeSolveError> {
        match stage {
            solve::RefreshStage::CausalSeedSweep {
                static_sequence,
                dynamic_sequence,
                static_rows,
                dynamic_rows,
            } => {
                let seeded = self.refresh_stage_seed_sweep(
                    *static_sequence,
                    *dynamic_sequence,
                    complete_plan.selected_rows(static_rows),
                    complete_plan.selected_rows(dynamic_rows),
                    args,
                    incoming,
                )?;
                self.continue_or_project_complete(seeded, complete_plan, args)
            }
            solve::RefreshStage::ExactAssignments {
                static_sequence,
                dynamic_sequence,
                static_rows,
                dynamic_rows,
            } => {
                let assigned = self.refresh_stage_seed_sweep(
                    *static_sequence,
                    *dynamic_sequence,
                    complete_plan.selected_rows(static_rows),
                    complete_plan.selected_rows(dynamic_rows),
                    args,
                    incoming,
                )?;
                self.continue_or_project_complete(assigned, complete_plan, args)
            }
            solve::RefreshStage::ProjectionBlock {
                seed_sequence,
                block_index,
                plan,
                seed_rows,
            } => self.refresh_projection_stage_with_seed(
                ProjectionStageSeed {
                    sequence: *seed_sequence,
                    rows: complete_plan.selected_rows(seed_rows),
                },
                *block_index,
                plan,
                complete_plan,
                args,
                incoming,
            ),
        }
    }

    fn continue_or_project_complete(
        &self,
        seeded: bool,
        complete_plan: &solve::RefreshPlan,
        args: &mut RefreshSlotArgs<'_>,
    ) -> Result<bool, RuntimeSolveError> {
        if seeded {
            return Ok(true);
        }
        self.project_refresh_slots(complete_plan, args, true)?;
        Ok(false)
    }

    fn refresh_projection_stage_with_seed(
        &self,
        seed: ProjectionStageSeed<'_>,
        block_index: usize,
        plan: &solve::AlgebraicProjectionPlan,
        complete_plan: &solve::RefreshPlan,
        args: &mut RefreshSlotArgs<'_>,
        incoming: &[f64],
    ) -> Result<bool, RuntimeSolveError> {
        let result =
            self.refresh_slots_once(seed.rows, seed.sequence, args.t, args.solver_y, args.params);
        if let Err(error) = result {
            restore_after_causal_seed_error(error, args.solver_y, incoming)?;
            self.project_refresh_slots(complete_plan, args, true)?;
            return Ok(false);
        }
        self.project_refresh_stage(block_index, plan, args)?;
        Ok(true)
    }

    fn refresh_stage_seed_sweep(
        &self,
        static_sequence: solve::RefreshSequenceId,
        dynamic_sequence: solve::RefreshSequenceId,
        static_rows: solve::RefreshRows<'_>,
        dynamic_rows: solve::RefreshRows<'_>,
        args: &mut RefreshSlotArgs<'_>,
        incoming: &[f64],
    ) -> Result<bool, RuntimeSolveError> {
        let result = self
            .refresh_prepared_static_rows(
                static_rows,
                static_sequence,
                args.t,
                args.solver_y,
                args.params,
            )
            .and_then(|()| {
                self.refresh_slots_once(
                    dynamic_rows,
                    dynamic_sequence,
                    args.t,
                    args.solver_y,
                    args.params,
                )
            });
        if let Err(error) = result {
            restore_after_causal_seed_error(error, args.solver_y, incoming)?;
            return Ok(false);
        }
        Ok(true)
    }

    fn project_refresh_stage(
        &self,
        block_index: usize,
        plan: &solve::AlgebraicProjectionPlan,
        args: &mut RefreshSlotArgs<'_>,
    ) -> Result<(), RuntimeSolveError> {
        let model = RefreshProjectionModel {
            runtime: self,
            plan,
            block_indices: std::slice::from_ref(&block_index),
            plan_validated: true,
            jacobian_v: ProjectionJacobian::SolverY {
                block: &self.implicit_projection_jacobian_v,
                scalar: &self.implicit_projection_scalar_jacobian_v,
            },
        };
        let projection_args = crate::runtime::projection::AlgebraicProjectionArgs {
            parameters: args.params,
            time: args.t,
            state_count: self.state_count,
            tolerance: args.tol,
        };
        if args.certify_coordinates {
            project_algebraics_with_plan_certified(
                &model,
                plan,
                args.solver_y,
                projection_args,
                args.max_iters,
            )
        } else {
            project_algebraics_with_plan(
                &model,
                plan,
                args.solver_y,
                projection_args,
                args.max_iters,
            )
        }
    }
}

pub(super) fn value_stage_seed_coverage_is_complete(plan: &solve::RefreshPlan) -> bool {
    plan.value_stages.iter().all(|stage| match stage {
        solve::RefreshStage::ProjectionBlock {
            plan: projection,
            seed_rows,
            ..
        } => projection.blocks.iter().all(|block| {
            block.y_indices.iter().all(|target| {
                plan.selected_rows(seed_rows)
                    .iter()
                    .any(|row| row.target_index() == *target)
            })
        }),
        _ => true,
    })
}

pub(super) fn seed_error_allows_projection(error: &RuntimeSolveError) -> bool {
    matches!(
        error,
        RuntimeSolveError::NonFiniteValue { .. }
            | RuntimeSolveError::RefreshTargetUnassignable { .. }
            | RuntimeSolveError::RefreshTargetSingular { .. }
    )
}
