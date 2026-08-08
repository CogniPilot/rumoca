use nalgebra::{DMatrix, DVector};

use super::*;

pub(super) struct RefreshSlotArgs<'a> {
    pub(super) t: f64,
    pub(super) solver_y: &'a mut [f64],
    pub(super) params: &'a [f64],
    pub(super) tol: f64,
    pub(super) max_iters: usize,
    pub(super) certify_coordinates: bool,
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

#[cfg(test)]
impl StaticRefreshCache {
    pub(super) fn bit_eq(&self, other: &Self) -> bool {
        self.valid == other.valid
            && float_slice_bit_eq(&self.params, &other.params)
            && self.values.len() == other.values.len()
            && self
                .values
                .iter()
                .zip(&other.values)
                .all(|(left, right)| option_float_bit_eq(*left, *right))
    }
}

#[cfg(test)]
fn float_slice_bit_eq(left: &[f64], right: &[f64]) -> bool {
    left.len() == right.len()
        && left
            .iter()
            .zip(right)
            .all(|(left, right)| left.to_bits() == right.to_bits())
}

#[cfg(test)]
fn option_float_bit_eq(left: Option<f64>, right: Option<f64>) -> bool {
    match (left, right) {
        (Some(left), Some(right)) => left.to_bits() == right.to_bits(),
        (None, None) => true,
        _ => false,
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
        self.runtime
            .implicit_rhs
            .eval_with_context(y, p, t, self.runtime.row_eval_context(), out)
            .map_err(Into::into)
    }

    fn eval_jacobian_v(
        &self,
        y: &[f64],
        p: &[f64],
        t: f64,
        v: &[f64],
        out: &mut [f64],
    ) -> Result<(), RuntimeSolveError> {
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
        let Some(program_idx) = self
            .runtime
            .implicit_scalar_rhs
            .single_output_row_for_output_index(row_idx)
        else {
            return Ok(None);
        };
        self.runtime
            .implicit_scalar_rhs
            .eval_row_unchecked_with_context(program_idx, y, p, t, self.runtime.row_eval_context())
            .map(Some)
            .map_err(Into::into)
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
        let Some(jvp_program_idx) = block.single_output_row_for_output_index(row_idx) else {
            return Ok(None);
        };
        let implicit_program_idx = self
            .runtime
            .implicit_scalar_rhs
            .single_output_row_for_output_index(row_idx);
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
            .eval_row_unchecked_with_context(
                jvp_program_idx,
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
        let Some(program_idx) = self
            .runtime
            .implicit_scalar_rhs
            .single_output_row_for_output_index(row_idx)
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
        jacobian: &DMatrix<f64>,
        residual: &[f64],
        row_scales: &[f64],
        variable_scales: &[f64],
        structure: Option<&solve::StructuralPattern>,
        tolerance: f64,
    ) -> Option<DVector<f64>> {
        let block_index = self.block_indices.get(block_index).copied()?;
        self.runtime
            .algebraic_newton_caches
            .get(block_index)
            .map_or_else(
                || {
                    crate::runtime::projection::scaled_newton_delta(
                        jacobian,
                        residual,
                        row_scales,
                        variable_scales,
                        structure,
                        tolerance,
                    )
                },
                |cache| {
                    crate::runtime::projection::scaled_newton_delta_with_cache(
                        jacobian,
                        residual,
                        row_scales,
                        variable_scales,
                        structure,
                        tolerance,
                        &mut cache.borrow_mut(),
                    )
                },
            )
    }

    fn solve_algebraic_sensitivity_delta(
        &self,
        block_index: usize,
        jacobian: &DMatrix<f64>,
        residual: &[f64],
        row_scales: &[f64],
        variable_scales: &[f64],
        structure: Option<&solve::StructuralPattern>,
        tolerance: f64,
    ) -> Option<DVector<f64>> {
        let block_index = self.block_indices.get(block_index).copied()?;
        let cache = self.runtime.algebraic_newton_caches.get(block_index);
        match cache {
            Some(cache) => crate::runtime::projection::scaled_unique_delta(
                jacobian,
                residual,
                row_scales,
                variable_scales,
                structure,
                tolerance,
                Some(&mut cache.borrow_mut()),
            ),
            None => crate::runtime::projection::scaled_unique_delta(
                jacobian,
                residual,
                row_scales,
                variable_scales,
                structure,
                tolerance,
                None,
            ),
        }
    }

    fn eval_implicit_target_value(
        &self,
        row_idx: usize,
        target_y_index: usize,
        y: &[f64],
        p: &[f64],
        t: f64,
    ) -> Result<Option<f64>, RuntimeSolveError> {
        let Some(program_idx) = self
            .runtime
            .implicit_scalar_rhs
            .single_output_row_for_output_index(row_idx)
        else {
            return Ok(None);
        };
        self.runtime
            .implicit_scalar_rhs
            .eval_target_assignment_row_unchecked_with_context(
                program_idx,
                target_y_index,
                y,
                p,
                t,
                self.runtime.row_eval_context(),
            )
            .map_err(Into::into)
    }

    fn implicit_target_assignment_is_exact(&self, row_idx: usize, target_y_index: usize) -> bool {
        self.runtime
            .implicit_scalar_rhs
            .single_output_row_for_output_index(row_idx)
            .is_some_and(|program_idx| {
                self.runtime
                    .implicit_scalar_rhs
                    .certifies_exact_target_assignment(program_idx, target_y_index)
            })
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
            .solve_layout
            .solver_maps
            .names
            .get(row_idx)
            .map(String::as_str)
    }

    fn variable_scale_for_y_index(&self, y_index: usize) -> f64 {
        self.runtime.model.solver_variable_scale(y_index)
    }
}

impl RefreshProjectionModel<'_> {
    fn implicit_row_is_affine(&self, row_idx: usize) -> bool {
        let block = &self.runtime.implicit_scalar_rhs;
        block
            .single_output_row_for_output_index(row_idx)
            .is_some_and(|program_idx| block.certifies_parameter_static_y_gradient(program_idx))
    }
}

impl SolveRuntime {
    pub(super) fn value_stage_schedule_is_certified(&self, plan: &RefreshPlan) -> bool {
        let structural = self.continuous_structural.algebraic_projection();
        plan.simultaneous_block_indices.len() == plan.simultaneous_plan.blocks.len()
            && !plan.value_stages.is_empty()
            && value_stage_seed_coverage_is_complete(&plan.value_stages)
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
        plan: &RefreshPlan,
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
        stage: &RefreshStage,
        complete_plan: &RefreshPlan,
        args: &mut RefreshSlotArgs<'_>,
        incoming: &[f64],
    ) -> Result<bool, RuntimeSolveError> {
        match stage {
            RefreshStage::CausalSeedSweep {
                static_rows,
                dynamic_rows,
            } => {
                let seeded =
                    self.refresh_stage_seed_sweep(static_rows, dynamic_rows, args, incoming)?;
                self.continue_or_project_complete(seeded, complete_plan, args)
            }
            RefreshStage::ExactAssignments {
                static_rows,
                dynamic_rows,
            } => {
                let assigned =
                    self.refresh_stage_seed_sweep(static_rows, dynamic_rows, args, incoming)?;
                self.continue_or_project_complete(assigned, complete_plan, args)
            }
            RefreshStage::ProjectionBlock {
                block_index,
                plan,
                seed_rows,
            } => self.refresh_projection_stage_with_seed(
                *block_index,
                plan,
                seed_rows,
                complete_plan,
                args,
                incoming,
            ),
        }
    }

    fn continue_or_project_complete(
        &self,
        seeded: bool,
        complete_plan: &RefreshPlan,
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
        block_index: usize,
        plan: &solve::AlgebraicProjectionPlan,
        seed_rows: &[AlgebraicRefreshRow],
        complete_plan: &RefreshPlan,
        args: &mut RefreshSlotArgs<'_>,
        incoming: &[f64],
    ) -> Result<bool, RuntimeSolveError> {
        let result = self.refresh_slots_once(seed_rows, args.t, args.solver_y, args.params);
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
        static_rows: &[AlgebraicRefreshRow],
        dynamic_rows: &[AlgebraicRefreshRow],
        args: &mut RefreshSlotArgs<'_>,
        incoming: &[f64],
    ) -> Result<bool, RuntimeSolveError> {
        let result = self
            .refresh_prepared_static_rows(static_rows, args.t, args.solver_y, args.params)
            .and_then(|()| {
                self.refresh_slots_once(dynamic_rows, args.t, args.solver_y, args.params)
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
        let projection_args = crate::AlgebraicProjectionArgs {
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

pub(super) fn value_stage_seed_coverage_is_complete(stages: &[RefreshStage]) -> bool {
    stages.iter().all(|stage| match stage {
        RefreshStage::ProjectionBlock {
            plan, seed_rows, ..
        } => plan.blocks.iter().all(|block| {
            block
                .y_indices
                .iter()
                .all(|target| seed_rows.iter().any(|row| row.target_index == *target))
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
