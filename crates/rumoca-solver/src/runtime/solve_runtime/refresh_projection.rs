#[cfg(test)]
mod admitted_tests;

mod grouped_jacobian;
mod grouped_residual;
mod prepared_jacobian;
mod torn_preparation;

pub(super) use torn_preparation::TornSweepCache;

pub(super) use prepared_jacobian::{
    prepare_projection_jacobians, projection_jacobian_demand, projection_jacobian_source,
    validate_projection_primal_source,
};

use crate::runtime::projection::{
    ImplicitProjectionModel, ScaledNewtonSystem, per_row_torn_block_sweep,
};
use nalgebra::DVector;
use rumoca_eval_solve::dense_basis::{DenseStageMatrix, DependentConditioning};
use rumoca_eval_solve::{PreparedTornSweep, TornSweepStatus};

use super::*;
use crate::runtime::fallbacks::{self, ProjectionFallback, ProjectionSite};

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
    source_spans: Box<[rumoca_core::Span]>,
    affected_y: Box<[usize]>,
    rollback: RefCell<Vec<f64>>,
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

    /// Copy only the requested columns of a valid cached gradient. A block
    /// Jacobian reads just its own unknowns, so copying the complete solver-Y
    /// gradient per row moves memory no consumer reads.
    fn copy_columns_into(
        &self,
        row: usize,
        parameter_indices: &[usize],
        params: &[f64],
        columns: &[usize],
        gradient: &mut [f64],
    ) -> bool {
        let Some(cached) = self.valid_row(row, parameter_indices, params) else {
            return false;
        };
        if cached.gradient.len() != gradient.len()
            || columns.iter().any(|&column| column >= gradient.len())
        {
            return false;
        }
        for &column in columns {
            gradient[column] = cached.gradient[column];
        }
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
    implicit: ProgramAnalysis<'_>,
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
        .scalar_blocks()
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
    pub(super) runtime: ScalarExecutionView<'a>,
    #[cfg(test)]
    pub(super) plan: &'a solve::AlgebraicProjectionPlan,
    pub(super) block_indices: &'a [usize],
    pub(super) plan_validated: bool,
    pub(super) jacobian_v: ProjectionJacobian<'a>,
    pub(super) seed_linearizations: Option<RefCell<std::cell::RefMut<'a, SeedProjectionCache>>>,
}

pub(super) struct RuntimeManifoldProjection<'a> {
    pub(super) runtime: ScalarExecutionView<'a>,
}

impl ManifoldProjectionModel for RuntimeManifoldProjection<'_> {
    fn eval_manifold_jacobian_outputs(
        &self,
        selection: &solve::ProjectionOutputSelection,
        inputs: solve_eval::JacobianEvalInputs<'_>,
        out: &mut [f64],
    ) -> Result<bool, RuntimeSolveError> {
        self.runtime.scalar.manifold.eval_selected_directional(
            selection,
            inputs,
            self.runtime.row_eval_context(),
            out,
        )?;
        Ok(true)
    }

    fn eval_manifold_residual(
        &self,
        y: &[f64],
        p: &[f64],
        t: f64,
        out: &mut [f64],
    ) -> Result<(), RuntimeSolveError> {
        self.runtime
            .scalar
            .manifold
            .eval_residual(y, p, t, self.runtime.row_eval_context(), out)
    }

    fn eval_manifold_jacobian_v(
        &self,
        y: &[f64],
        p: &[f64],
        t: f64,
        v: &[f64],
        out: &mut [f64],
    ) -> Result<(), RuntimeSolveError> {
        self.runtime.scalar.manifold.eval_directional(
            y,
            p,
            t,
            self.runtime.row_eval_context(),
            v,
            out,
        )
    }

    fn manifold_residual_len(&self) -> usize {
        self.runtime.scalar.manifold.len()
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
    ) -> Option<&solve::ColoredJacobianStructure> {
        self.runtime
            .scalar
            .continuous_structural
            .manifold_projection()
            .get(block_index)
    }

    fn manifold_variable_scale(&self, y_index: usize) -> f64 {
        self.runtime.solver_variable_scale(y_index)
    }
}

#[derive(Clone, Copy)]
pub(super) enum ProjectionJacobian<'a> {
    SolverY(&'a DirectionalProgram),
    SolverYAndParameters(&'a DirectionalProgram),
}

impl<'a> ProjectionJacobian<'a> {
    fn owner(self) -> &'a DirectionalProgram {
        match self {
            Self::SolverY(owner) | Self::SolverYAndParameters(owner) => owner,
        }
    }

    fn source(self) -> ProgramAnalysis<'a> {
        self.owner().source()
    }

    fn is_solver_y_only(self) -> bool {
        matches!(self, Self::SolverY(_))
    }
}

impl ImplicitProjectionModel for RefreshProjectionModel<'_> {
    fn linked_kernel(
        &self,
        request: crate::runtime::projection::KernelRequest<'_>,
    ) -> Result<crate::runtime::projection::KernelAnswer, RuntimeSolveError> {
        use crate::runtime::projection::{KernelAnswer, KernelRequest};
        Ok(match request {
            KernelRequest::BeginBlock {
                block_index,
                point: (y, p, t),
            } => {
                if let Some(&index) = self.block_indices.get(block_index) {
                    self.runtime.begin_block_residual_split(index, y, p, t)?;
                }
                KernelAnswer::Done
            }
            KernelRequest::EndBlock => {
                self.runtime.end_block_residual_split();
                KernelAnswer::Done
            }
            KernelRequest::ColoredEntries {
                structure,
                coordinates,
                point,
            } => self
                .eval_colored_tangent_entries(structure, coordinates, point)?
                .map_or(KernelAnswer::Declined, KernelAnswer::ColoredEntries),
            KernelRequest::TornJacobian {
                block,
                point: (y, p, t),
            } if self.jacobian_v.is_solver_y_only() => self
                .block_indices
                .get(block.index)
                .map(|&index| {
                    self.runtime
                        .torn_tangent_jacobian(index, block.tearing, y, p, t)
                })
                .transpose()?
                .flatten()
                .map_or(KernelAnswer::Declined, |jacobian| {
                    jacobian.map_or(
                        KernelAnswer::TornJacobianSingular,
                        KernelAnswer::TornJacobian,
                    )
                }),
            KernelRequest::TornJacobian { .. } => KernelAnswer::Declined,
        })
    }

    fn eval_prepared_implicit_jacobian(
        &self,
        structure: &solve::ColoredJacobianStructure,
        coordinates: (&[usize], &[usize]),
        y: &[f64],
        p: &[f64],
        t: f64,
        out: &mut [f64],
    ) -> Result<bool, RuntimeSolveError> {
        self.eval_prepared_jacobian(structure, coordinates, y, p, t, out)
    }

    fn eval_implicit_residual_outputs(
        &self,
        selection: &solve::ProjectionOutputSelection,
        y: &[f64],
        p: &[f64],
        t: f64,
        out: &mut [f64],
    ) -> Result<bool, RuntimeSolveError> {
        self.eval_grouped_residual_outputs(selection, y, p, t, out)?;
        Ok(true)
    }

    fn algebraic_seed_linearization(
        &self,
        block_index: usize,
        block: &solve::AlgebraicProjectionBlock,
        y: &[f64],
        args: crate::runtime::projection::AlgebraicProjectionArgs<'_>,
    ) -> Result<Rc<crate::runtime::projection::SeedBlockLinearization>, RuntimeSolveError> {
        self.seed_block_linearization(block_index, block, y, args)
    }

    fn eval_implicit_jacobian_v_outputs(
        &self,
        selection: &solve::ProjectionJacobianOutputs,
        inputs: solve_eval::JacobianEvalInputs<'_>,
        enabled_rows: &[bool],
        out: &mut [f64],
    ) -> Result<bool, RuntimeSolveError> {
        let selection = if self.jacobian_v.is_solver_y_only() {
            selection.solver_y()
        } else {
            selection.solver_y_and_parameters()
        };
        let Some(selection) = selection else {
            return Ok(false);
        };
        self.eval_grouped_jacobian_outputs(selection, inputs, enabled_rows, out)?;
        Ok(true)
    }

    fn eval_residual(
        &self,
        y: &[f64],
        p: &[f64],
        t: f64,
        out: &mut [f64],
    ) -> Result<(), RuntimeSolveError> {
        admitted_output_call(
            &self.runtime.scalar.compiled_output_scratch,
            out,
            |output| {
                self.runtime.scalar.implicit_scalar_rhs.eval(
                    RowEvalPoint { y, p, t },
                    self.runtime.row_eval_context(),
                    output,
                )
            },
        )?;
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
        admitted_output_call(
            &self.runtime.scalar.compiled_output_scratch,
            out,
            |output| {
                self.jacobian_v.owner().eval(
                    solve_eval::JacobianEvalInputs { y, p, t, seed: v },
                    self.runtime.row_eval_context(),
                    output,
                )
            },
        )
    }

    fn eval_implicit_residual_rows(
        &self,
        rows: &[usize],
        point: (&[f64], &[f64], f64),
        out: &mut [f64],
    ) -> Result<bool, RuntimeSolveError> {
        self.runtime.eval_compiled_residual_rows(rows, point, out)
    }

    fn eval_implicit_residual_row(
        &self,
        row_idx: usize,
        y: &[f64],
        p: &[f64],
        t: f64,
    ) -> Result<Option<f64>, RuntimeSolveError> {
        let Some(coordinate) = self
            .runtime
            .scalar
            .implicit_scalar_rhs
            .source()
            .row_output_position(row_idx)
        else {
            return Ok(None);
        };
        let value = match self
            .runtime
            .eval_split_residual_row(coordinate.0, (y, p, t))?
        {
            Some(value) => value,
            None => self.runtime.scalar.implicit_scalar_rhs.eval_output(
                coordinate,
                RowEvalPoint { y, p, t },
                self.runtime.row_eval_context(),
            )?,
        };
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
        let block = self.jacobian_v.source();
        let Some((jvp_program_idx, output_offset)) = block.row_output_position(row_idx) else {
            return Ok(None);
        };
        let implicit_program_idx = self
            .runtime
            .scalar
            .implicit_scalar_rhs
            .source()
            .row_output_position(row_idx)
            .map(|(program_idx, _)| program_idx);
        if self.jacobian_v.is_solver_y_only()
            && let Some(implicit_program_idx) = implicit_program_idx
            && let Some(parameter_indices) = self
                .runtime
                .scalar
                .implicit_scalar_rhs
                .source()
                .parameter_static_y_gradient_params(implicit_program_idx)
            && let Some(value) = self
                .runtime
                .scalar
                .parameter_static_gradient_cache
                .borrow()
                .dot_solver_y_seed(implicit_program_idx, parameter_indices, p, v)
        {
            return Ok(Some(value));
        }
        self.jacobian_v
            .owner()
            .eval_output(
                (jvp_program_idx, output_offset),
                solve_eval::JacobianEvalInputs { y, p, t, seed: v },
                self.runtime.row_eval_context(),
            )
            .map(Some)
    }

    fn eval_implicit_jacobian_row(
        &self,
        row_idx: usize,
        y: &[f64],
        p: &[f64],
        t: f64,
        gradient: &mut [f64],
    ) -> Result<bool, RuntimeSolveError> {
        // The optional reverse evaluator is reference execution. The compiled
        // runtime uses its prepared forward JVP when this optimization declines.
        if self.jacobian_v.owner().is_compiled() {
            return Ok(false);
        }
        let Some((program_idx, _)) = self
            .runtime
            .scalar
            .implicit_scalar_rhs
            .source()
            .row_output_position(row_idx)
        else {
            return Ok(false);
        };
        let gradient_params = self
            .runtime
            .scalar
            .implicit_scalar_rhs
            .source()
            .parameter_static_y_gradient_params(program_idx);
        if let Some(parameter_indices) = gradient_params
            && self
                .runtime
                .scalar
                .parameter_static_gradient_cache
                .borrow()
                .copy_into(program_idx, parameter_indices, p, gradient)
        {
            return Ok(true);
        }
        let Some(reference) = self.runtime.scalar.implicit_scalar_rhs.reference() else {
            return Ok(false);
        };
        let evaluated = reference.reverse_row_y_gradient(
            program_idx,
            &rumoca_eval_solve::reverse::ReverseInputs {
                y,
                p,
                t,
                context: self.runtime.row_eval_context(),
            },
            gradient,
            &mut self.runtime.scalar.reverse_scratch.borrow_mut(),
        )?;
        if evaluated && let Some(parameter_indices) = gradient_params {
            self.runtime
                .scalar
                .parameter_static_gradient_cache
                .borrow_mut()
                .store(program_idx, parameter_indices, p, gradient);
        }
        Ok(evaluated)
    }

    fn eval_implicit_jacobian_row_columns(
        &self,
        row_idx: usize,
        y: &[f64],
        p: &[f64],
        t: f64,
        columns: &[usize],
        gradient: &mut [f64],
    ) -> Result<bool, RuntimeSolveError> {
        if let Some((program_idx, _)) = self
            .runtime
            .scalar
            .implicit_scalar_rhs
            .source()
            .row_output_position(row_idx)
            && let Some(parameter_indices) = self
                .runtime
                .scalar
                .implicit_scalar_rhs
                .source()
                .parameter_static_y_gradient_params(program_idx)
            && self
                .runtime
                .scalar
                .parameter_static_gradient_cache
                .borrow()
                .copy_columns_into(program_idx, parameter_indices, p, columns, gradient)
        {
            return Ok(true);
        }
        self.eval_implicit_jacobian_row(row_idx, y, p, t, gradient)
    }

    fn implicit_jacobian_v_row_depends_on(&self, row_idx: usize, seed_index: usize) -> bool {
        self.runtime
            .scalar
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
    ) -> Option<&solve::ColoredJacobianStructure> {
        let block_index = self.block_indices.get(block_index).copied()?;
        self.runtime
            .scalar
            .continuous_structural
            .algebraic_projection()
            .get(block_index)
    }

    fn algebraic_projection_block_invalidates_earlier(&self, block_index: usize) -> bool {
        let Some(block_index) = self.block_indices.get(block_index).copied() else {
            return true;
        };
        self.runtime
            .scalar
            .continuous_structural
            .algebraic_invalidates_earlier(block_index)
            .unwrap_or(true)
    }

    fn algebraic_projection_block_is_affine(&self, block_index: usize) -> bool {
        self.block_indices.get(block_index).is_some_and(|&index| {
            self.runtime
                .model
                .problem
                .continuous
                .refresh_owners
                .algebraic_projection_block_is_affine(index)
        })
    }

    fn projection_site(&self, block_index: usize) -> Option<usize> {
        self.block_indices.get(block_index).copied()
    }

    fn solve_affine_torn_delta(
        &self,
        block_index: usize,
        system: ScaledNewtonSystem<'_>,
    ) -> Option<DVector<f64>> {
        let block_index = self.block_indices.get(block_index).copied()?;
        let structure = self
            .runtime
            .scalar
            .continuous_structural
            .algebraic_projection()
            .get(block_index)?;
        let mut cache = self
            .runtime
            .scalar
            .algebraic_newton_caches
            .get(block_index)?
            .borrow_mut();
        // The block's affine solve passes this structure's pattern, the one
        // its issued torn system was admitted under.
        let delta = if cache.has_issued_torn() {
            debug_assert!(system.structure == Some(structure.pattern()));
            crate::runtime::projection::scaled_newton_delta_issued_torn(system, &mut cache)
        } else {
            crate::runtime::projection::scaled_newton_delta_with_tearing(
                system,
                &mut cache,
                structure.affine_elimination()?,
            )
        };
        if delta.is_none() {
            fallbacks::note_fallback(
                ProjectionSite::Block(block_index),
                ProjectionFallback::AffineFullSystem,
            );
        }
        delta
    }

    fn affine_jacobian_cache(
        &self,
        block_index: usize,
    ) -> Option<&std::cell::RefCell<crate::runtime::projection::SparseNewtonCache>> {
        let index = self.block_indices.get(block_index).copied()?;
        self.runtime.scalar.algebraic_newton_caches.get(index)
    }

    fn eval_block_residual(
        &self,
        block_index: usize,
        point: (&[f64], &[f64], f64),
        out: &mut [f64],
    ) -> bool {
        self.block_kernels(block_index).is_some_and(|kernels| {
            kernels.eval_residual(point, self.runtime.model.external_tables.as_slice(), out)
        })
    }

    fn eval_block_jacobian_v(
        &self,
        block_index: usize,
        point: (&[f64], &[f64], f64),
        seed: &[f64],
        out: &mut [f64],
    ) -> bool {
        // The kernels hold the rows' programs of the complete directional
        // owner, the one a sensitivity seed projection evaluates per row.
        matches!(
            self.jacobian_v,
            ProjectionJacobian::SolverYAndParameters(owner)
                if std::ptr::eq(owner, &self.runtime.scalar.implicit_jacobian_v)
        ) && self.block_kernels(block_index).is_some_and(|kernels| {
            kernels.eval_jacobian_v(
                point,
                seed,
                self.runtime.model.external_tables.as_slice(),
                out,
            )
        })
    }

    fn implicit_row_static_gradient_parameters(&self, row_idx: usize) -> Option<&[usize]> {
        let (program_idx, _) = self
            .runtime
            .scalar
            .implicit_scalar_rhs
            .source()
            .row_output_position(row_idx)?;
        self.runtime
            .scalar
            .implicit_scalar_rhs
            .source()
            .parameter_static_y_gradient_params(program_idx)
    }

    fn solve_algebraic_newton_delta(
        &self,
        block_index: usize,
        system: ScaledNewtonSystem<'_>,
    ) -> Option<DVector<f64>> {
        let block_index = self.block_indices.get(block_index).copied()?;
        self.runtime
            .scalar
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
            .scalar
            .implicit_scalar_rhs
            .source()
            .row_output_position(row_idx)
        else {
            return Ok(None);
        };
        if let Some(queries) = self.runtime.scalar.compiled_targets(false) {
            return queries.call(
                (program_idx, output_offset, target_y_index),
                (y, p, t),
                self.runtime.row_eval_context(),
            );
        }
        if let Some((compiled, selected)) = self.runtime.compiled_projection_target_values(
            program_idx,
            output_offset,
            target_y_index,
        )? {
            return compiled
                .call(selected, y, p, t, self.runtime.row_eval_context())
                .map(Some)
                .map_err(|error| {
                    admitted_compiled_error(
                        "projection target",
                        self.runtime
                            .scalar
                            .implicit_scalar_rhs
                            .source()
                            .program_span(program_idx)
                            .as_slice(),
                        error,
                    )
                });
        }
        if output_offset == 0
            && let Some(compiled) = self
                .runtime
                .compiled_projection_assignment(program_idx, target_y_index)?
        {
            let mut output = [0.0];
            compiled
                .call(
                    y,
                    p,
                    t,
                    self.runtime.model.external_tables.as_slice(),
                    &mut output,
                )
                .map_err(|error| {
                    admitted_compiled_error(
                        "projection target",
                        self.runtime
                            .scalar
                            .implicit_scalar_rhs
                            .source()
                            .program_span(program_idx)
                            .as_slice(),
                        error,
                    )
                })?;
            return Ok(Some(output[0]));
        }
        let Some(reference) = self.runtime.scalar.implicit_scalar_rhs.reference() else {
            return Ok(None);
        };
        reference.eval_target_assignment_output_unchecked_with_context(
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
    }

    fn implicit_target_assignment_is_exact(&self, row_idx: usize, target_y_index: usize) -> bool {
        self.runtime
            .scalar
            .implicit_scalar_rhs
            .source()
            .row_output_position(row_idx)
            .is_some_and(|(program_idx, output_offset)| {
                self.runtime
                    .scalar
                    .implicit_scalar_rhs
                    .source()
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
        if self.runtime.scalar.implicit_scalar_rhs.is_compiled() && entry.compiled.is_none() {
            return per_row_torn_block_sweep(self, tearing, y, p, t, residual_out);
        }
        #[cfg(debug_assertions)]
        let entry_y = y.to_vec();
        let mut raw = Vec::with_capacity(tearing.residual_rows.len());
        let compiled_status = entry
            .compiled
            .as_ref()
            .map(|compiled| self.eval_compiled_torn_sweep(compiled, tearing, y, p, t, &mut raw))
            .transpose()?;
        let status = match compiled_status {
            Some(status) => status,
            None => self
                .runtime
                .scalar
                .implicit_scalar_rhs
                .reference()
                .ok_or_else(|| {
                    RuntimeSolveError::solve_ir("reference execution capability is unavailable")
                })?
                .eval_torn_sweep_unchecked_with_context(
                    &entry.sweep,
                    y,
                    p,
                    t,
                    self.runtime.row_eval_context(),
                    &mut raw,
                )?,
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
            .target_at(row_idx)
    }

    #[cfg(test)]
    fn algebraic_projection_plan(&self) -> &solve::AlgebraicProjectionPlan {
        self.plan
    }

    fn algebraic_projection_plan_is_validated(&self) -> bool {
        self.plan_validated
    }

    fn unlocalizable_guards(&self) -> &[solve::UnlocalizableGuard] {
        &self.runtime.model.problem.continuous.unlocalizable_guards
    }

    fn target_name_for_row(&self, row_idx: usize) -> Option<String> {
        self.runtime
            .model
            .problem
            .continuous
            .implicit_row_targets
            .target_at(row_idx)
            .and_then(|slot| match slot {
                solve::ScalarSlot::Y { index, .. } => Some(index),
                _ => None,
            })
            .and_then(|index| self.runtime.model.problem.solver_name(index))
    }

    fn variable_name_for_y_index(&self, y_index: usize) -> Option<String> {
        self.runtime.model.problem.solver_name(y_index)
    }

    fn variable_scale_for_y_index(&self, y_index: usize) -> f64 {
        self.runtime.solver_variable_scale(y_index)
    }
}

impl RefreshProjectionModel<'_> {
    /// The fused kernels of plan block `block_index`.
    fn block_kernels(&self, block_index: usize) -> Option<&super::block_kernels::BlockKernels> {
        let index = self.block_indices.get(block_index).copied()?;
        self.runtime.scalar.block_kernels.get(index)?.as_deref()
    }

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

    /// The admitted sweep owns its mutable target transaction. Only a
    /// successful schedule and residual call publish values; mathematical
    /// singular-target decline keeps the ordinary torn-solve policy.
    fn eval_compiled_torn_sweep(
        &self,
        compiled: &CompiledTornSweep,
        tearing: &solve::BlockTearing,
        y: &mut [f64],
        p: &[f64],
        t: f64,
        raw: &mut Vec<Option<f64>>,
    ) -> Result<TornSweepStatus, RuntimeSolveError> {
        let mut incoming = compiled.rollback.borrow_mut();
        resize_runtime_values(
            &mut incoming,
            compiled.affected_y.len(),
            0.0,
            "compiled torn target rollback",
        )?;
        for (&index, value) in compiled.affected_y.iter().zip(incoming.iter_mut()) {
            *value = y[index];
        }
        let result = self.call_compiled_torn_sweep(compiled, tearing, y, p, t, raw);
        if result.is_err() {
            for (&index, &value) in compiled.affected_y.iter().zip(incoming.iter()) {
                y[index] = value;
            }
        }
        result
    }

    fn call_compiled_torn_sweep(
        &self,
        compiled: &CompiledTornSweep,
        tearing: &solve::BlockTearing,
        y: &mut [f64],
        p: &[f64],
        t: f64,
        raw: &mut Vec<Option<f64>>,
    ) -> Result<TornSweepStatus, RuntimeSolveError> {
        let tables = self.runtime.model.external_tables.as_slice();
        compiled.schedule.call(y, p, t, tables).map_err(|error| {
            admitted_compiled_error("torn assignment schedule", &compiled.source_spans, error)
        })?;
        for step in &tearing.causal_steps {
            if !y.get(step.y_index).copied().unwrap_or(f64::NAN).is_finite() {
                return Ok(TornSweepStatus::Declined);
            }
        }
        let mut out = self.runtime.scalar.compiled_output_scratch.borrow_mut();
        resize_runtime_values(
            &mut out,
            compiled.residual_len,
            0.0,
            "compiled torn residual outputs",
        )?;
        compiled
            .residual_block
            .call(y, p, t, tables, &mut out)
            .map_err(|error| {
                admitted_compiled_error("torn residual", &compiled.source_spans, error)
            })?;
        support::reserve_runtime_vec_capacity(
            raw,
            compiled.residual_outputs.len().saturating_sub(raw.len()),
            "compiled torn residual commit",
        )?;
        raw.clear();
        raw.extend(
            compiled
                .residual_outputs
                .iter()
                .map(|position| position.and_then(|index| out.get(index).copied())),
        );
        Ok(TornSweepStatus::Completed)
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
}

impl ScalarExecutionView<'_> {
    pub(super) fn value_stage_schedule_is_certified(&self, plan: &solve::RefreshPlan) -> bool {
        plan.value_stage_schedule_is_certified(&self.scalar.continuous_structural)
    }

    pub(super) fn refresh_slots_with_stages(
        &self,
        plan: &solve::RefreshPlan,
        args: &mut RefreshSlotArgs<'_>,
        incoming: &[f64],
    ) -> Result<(), RuntimeSolveError> {
        self.prepare_static_refresh_cache(args.params, args.solver_y.len());
        let _call = fallbacks::begin_call(ProjectionSite::CompletePlan, 0);
        for stage in &plan.value_stages {
            if !self.execute_refresh_stage(stage, plan, args, incoming)? {
                return Ok(());
            }
        }
        Ok(())
    }

    pub(super) fn execute_refresh_stage(
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
        fallbacks::note_fallback(
            ProjectionSite::CompletePlan,
            ProjectionFallback::CompletePlan,
        );
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
        let Err(error) = result else {
            self.project_refresh_stage(block_index, plan, args)?;
            return Ok(true);
        };
        if !seed_error_allows_projection(&error) {
            args.solver_y.copy_from_slice(incoming);
            return Err(error);
        }
        // A seed is only a warm start. Restore the seed targets and the block
        // unknowns to their pre-stage values and project this block; the
        // certified earlier stages keep their values. Only when the block's
        // own projection fails does the complete simultaneous plan run.
        if let Some(block) = plan.blocks.scalar_blocks().first() {
            for index in solve::projection_seed_rescue_targets(seed.rows, block) {
                args.solver_y[index] = incoming[index];
            }
            fallbacks::note_fallback(
                ProjectionSite::Block(block_index),
                ProjectionFallback::SeedRescue,
            );
            tracing::debug!(
                target: "rumoca_eval_solve::refresh",
                block_index,
                "projection-stage seed was unavailable; projecting its block from the incoming coordinate: {error}"
            );
            match self.project_refresh_stage(block_index, plan, args) {
                Ok(()) => return Ok(true),
                Err(error @ RuntimeSolveError::CompiledExecution { .. }) => {
                    args.solver_y.copy_from_slice(incoming);
                    return Err(error);
                }
                Err(_) => {}
            }
        }
        fallbacks::note_fallback(
            ProjectionSite::Block(block_index),
            ProjectionFallback::CompletePlan,
        );
        restore_after_causal_seed_error(error, args.solver_y, incoming)?;
        self.project_refresh_slots(complete_plan, args, true)?;
        Ok(false)
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
            runtime: *self,
            seed_linearizations: None,
            #[cfg(test)]
            plan,
            block_indices: std::slice::from_ref(&block_index),
            plan_validated: true,
            jacobian_v: ProjectionJacobian::SolverY(
                &self.scalar.implicit_projection_scalar_jacobian_v,
            ),
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

pub(super) fn seed_error_allows_projection(error: &RuntimeSolveError) -> bool {
    matches!(
        error,
        RuntimeSolveError::NonFiniteValue { .. }
            | RuntimeSolveError::RefreshTargetUnassignable { .. }
            | RuntimeSolveError::RefreshTargetSingular { .. }
    )
}

impl ScalarExecutionView<'_> {
    /// A value-projection model view over this runtime's algebraic refresh plan,
    /// used to evaluate individual implicit residual rows and their Jacobian
    /// rows for reduced-chart conditioning and re-seeding.
    fn reduced_chart_projection_model(&self) -> RefreshProjectionModel<'_> {
        RefreshProjectionModel {
            runtime: *self,
            seed_linearizations: None,
            #[cfg(test)]
            plan: &self.scalar.algebraic_refresh.value_projection_plan,
            block_indices: &self.scalar.algebraic_refresh.simultaneous_block_indices,
            plan_validated: true,
            jacobian_v: ProjectionJacobian::SolverY(
                &self.scalar.implicit_projection_scalar_jacobian_v,
            ),
        }
    }

    /// Evaluate the dependent-Jacobian conditioning of each reduced state
    /// selection chart at a settled solver coordinate.
    ///
    /// `folding_rows` are the implicit residual rows of the folding first
    /// integral (`g = 0`); `group_cols` are the solver-Y columns of the
    /// constrained coordinate group; `chart_dependent_positions[j]` are the
    /// positions within `group_cols` that chart `j` reconstructs (its dependent
    /// coordinates). The returned conditioning per chart estimates `1/cond` of
    /// that chart's `g_d` in the same relative pivot scale
    /// [`rumoca_eval_solve::dense_basis::DenseStageMatrix::is_full_column_rank`]
    /// applies, so a chart whose `rcond` falls to its `singular_threshold` is
    /// the one folding at this coordinate.
    pub fn reduced_chart_dependent_conditioning(
        &self,
        t: f64,
        solver_y: &[f64],
        params: &[f64],
        folding_rows: &[usize],
        group_cols: &[usize],
        chart_dependent_positions: &[Vec<usize>],
    ) -> Result<Vec<DependentConditioning>, RuntimeSolveError> {
        if folding_rows.is_empty() || group_cols.is_empty() {
            return Err(RuntimeSolveError::solve_ir(
                "reduced-chart conditioning needs a non-empty folding stage",
            ));
        }
        let model = self.reduced_chart_projection_model();
        let mut seed = vec![0.0; self.solver_count];
        let mut values = Vec::with_capacity(folding_rows.len() * group_cols.len());
        for &row in folding_rows {
            for &col in group_cols {
                values.push(folding_jacobian_entry(
                    &model, row, col, solver_y, params, t, &mut seed,
                )?);
            }
        }
        let stage = DenseStageMatrix::new(folding_rows.len(), group_cols.len(), &values).map_err(
            |error| {
                RuntimeSolveError::solve_ir(format!("folding stage matrix rejected: {error:?}"))
            },
        )?;
        chart_dependent_positions
            .iter()
            .map(|dependent| {
                stage.dependent_conditioning(dependent).map_err(|error| {
                    RuntimeSolveError::solve_ir(format!(
                        "reduced-chart dependent conditioning failed: {error:?}"
                    ))
                })
            })
            .collect()
    }

    /// Evaluate selected implicit residual rows at `solver_y`.
    pub fn evaluate_implicit_residual_rows(
        &self,
        t: f64,
        solver_y: &[f64],
        params: &[f64],
        rows: &[usize],
    ) -> Result<Vec<f64>, RuntimeSolveError> {
        let model = self.reduced_chart_projection_model();
        rows.iter()
            .map(|&row| {
                model
                    .eval_implicit_residual_row(row, solver_y, params, t)?
                    .ok_or_else(|| {
                        RuntimeSolveError::solve_ir("implicit residual row has no scalar view")
                    })
            })
            .collect()
    }

    /// The number of scalar implicit residual rows, i.e. the number of reduced
    /// reconstruction targets the continuous projection drives to zero.
    pub fn implicit_residual_row_count(&self) -> usize {
        self.model.problem.continuous.implicit_row_targets.len()
    }

    /// The one solver-Y coordinate other than `state` that implicit residual
    /// `row` reads, when it reads exactly one: the source coordinate a
    /// state-binding row `state - source` integrates.
    pub(crate) fn binding_row_source(&self, row: usize, state: usize) -> Option<usize> {
        let source = self.scalar.implicit_scalar_rhs.source();
        let (program, _) = source.row_output_position(row)?;
        let mut sources = source
            .direct_y_reads(program)?
            .filter(|&index| index != state)
            .collect::<Vec<_>>();
        sources.sort_unstable();
        sources.dedup();
        match sources.as_slice() {
            [source] => Some(*source),
            _ => None,
        }
    }

    /// For each generated state coordinate (solver-Y `0..state_count`), the
    /// unique implicit residual row whose value is its identity `state - source`,
    /// i.e. the row whose Jacobian with respect to that state column is a unit
    /// pivot. The row's residual reconstructs the integrated source coordinate,
    /// so `state[k] - residual[binding_row[k]]` recovers that source's value.
    pub fn implicit_state_binding_rows(
        &self,
        t: f64,
        solver_y: &[f64],
        params: &[f64],
        state_count: usize,
    ) -> Result<Vec<usize>, RuntimeSolveError> {
        let model = self.reduced_chart_projection_model();
        let row_count = self.implicit_residual_row_count();
        let mut seed = vec![0.0; self.solver_count];
        (0..state_count)
            .map(|state| {
                let slot = seed.get_mut(state).ok_or_else(|| {
                    RuntimeSolveError::solve_ir("state coordinate is out of solver range")
                })?;
                *slot = 1.0;
                let binding = unit_binding_row(&model, row_count, solver_y, params, t, &seed);
                seed[state] = 0.0;
                binding
            })
            .collect()
    }
}

/// One entry of the folding stage Jacobian: the derivative of implicit residual
/// `row` with respect to solver-Y column `col`, evaluated by a unit seed. `seed`
/// is left zeroed for reuse.
fn folding_jacobian_entry(
    model: &RefreshProjectionModel<'_>,
    row: usize,
    col: usize,
    solver_y: &[f64],
    params: &[f64],
    t: f64,
    seed: &mut [f64],
) -> Result<f64, RuntimeSolveError> {
    // The compiler-derived implicit pattern proves most entries structurally
    // zero; only the rest need a Jacobian evaluation.
    if !model.implicit_jacobian_v_row_depends_on(row, col) {
        return Ok(0.0);
    }
    let slot = seed.get_mut(col).ok_or_else(|| {
        RuntimeSolveError::solve_ir("folding group column is out of solver range")
    })?;
    *slot = 1.0;
    let entry = model
        .eval_implicit_jacobian_v_row(row, solver_y, params, t, seed)?
        .ok_or_else(|| {
            RuntimeSolveError::solve_ir("folding residual row has no scalar Jacobian view")
        })?;
    seed[col] = 0.0;
    Ok(entry)
}

/// The unique implicit residual row whose Jacobian with respect to the state
/// column seeded in `seed` is a unit pivot: that state coordinate's identity
/// row. Fails when no row or more than one row carries that pivot.
fn unit_binding_row(
    model: &RefreshProjectionModel<'_>,
    row_count: usize,
    solver_y: &[f64],
    params: &[f64],
    t: f64,
    seed: &[f64],
) -> Result<usize, RuntimeSolveError> {
    let mut binding = None;
    for row in 0..row_count {
        let Some(value) = model.eval_implicit_jacobian_v_row(row, solver_y, params, t, seed)?
        else {
            continue;
        };
        if value.abs() > 0.5 {
            if binding.is_some() {
                return Err(RuntimeSolveError::solve_ir(
                    "state coordinate binds more than one implicit residual row",
                ));
            }
            binding = Some(row);
        }
    }
    binding
        .ok_or_else(|| RuntimeSolveError::solve_ir("state coordinate has no identity residual row"))
}
