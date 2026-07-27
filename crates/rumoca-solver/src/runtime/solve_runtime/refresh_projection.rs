use super::*;

pub(super) struct RefreshSlotArgs<'a> {
    pub(super) t: f64,
    pub(super) solver_y: &'a mut [f64],
    pub(super) params: &'a [f64],
    pub(super) tol: f64,
    pub(super) max_iters: usize,
}

#[derive(Clone, Default)]
pub(super) struct StaticRefreshCache {
    pub(super) valid: bool,
    pub(super) params: Vec<f64>,
    pub(super) values: Vec<Option<f64>>,
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
    pub(super) jacobian_v: ProjectionJacobian<'a>,
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
        let Some(program_idx) = block.single_output_row_for_output_index(row_idx) else {
            return Ok(None);
        };
        block
            .eval_row_unchecked_with_context(
                program_idx,
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
        self.runtime
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
            .map_err(Into::into)
    }

    fn implicit_jacobian_v_row_depends_on(&self, row_idx: usize, seed_index: usize) -> bool {
        let block = self.jacobian_v.scalar();
        block
            .single_output_row_for_output_index(row_idx)
            .is_none_or(|program_idx| block.row_seed_depends_on(program_idx, seed_index))
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

pub(super) fn seed_error_allows_projection(error: &RuntimeSolveError) -> bool {
    matches!(
        error,
        RuntimeSolveError::NonFiniteValue { .. }
            | RuntimeSolveError::RefreshTargetUnassignable { .. }
    )
}
