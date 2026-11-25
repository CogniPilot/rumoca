//! The combined initialization model: the unknowns Y and the parameters P
//! projected as one vector `[Y | P]` over an algebraic projection model.

use super::*;

pub(super) struct CombinedInitializationProjectionModel<'a, M> {
    pub(super) model: &'a M,
    pub(super) y_len: usize,
    pub(super) parameter_scales: Vec<f64>,
}

impl<M> CombinedInitializationProjectionModel<'_, M> {
    fn split_values<'a>(
        &self,
        values: &'a [f64],
    ) -> Result<(&'a [f64], &'a [f64]), RuntimeSolveError> {
        if values.len() < self.y_len {
            return Err(RuntimeSolveError::solve_ir(format!(
                "combined initialization vector has {} values, but Y requires {}",
                values.len(),
                self.y_len
            )));
        }
        Ok(values.split_at(self.y_len))
    }
}

impl<M: AlgebraicProjectionModel> ImplicitProjectionModel
    for CombinedInitializationProjectionModel<'_, M>
{
    fn eval_residual(
        &self,
        values: &[f64],
        _p: &[f64],
        t: f64,
        out: &mut [f64],
    ) -> Result<(), RuntimeSolveError> {
        let (y, p) = self.split_values(values)?;
        self.model.eval_residual(y, p, t, out)
    }

    fn eval_jacobian_v(
        &self,
        values: &[f64],
        _p: &[f64],
        t: f64,
        v: &[f64],
        out: &mut [f64],
    ) -> Result<(), RuntimeSolveError> {
        let (y, p) = self.split_values(values)?;
        let y_seed = v.get(..self.y_len).ok_or_else(|| {
            RuntimeSolveError::solve_ir(format!(
                "combined initialization seed has {} values, but Y requires {}",
                v.len(),
                self.y_len
            ))
        })?;
        self.model.eval_jacobian_v(y, p, t, y_seed, out)
    }

    fn implicit_target(&self, row_idx: usize) -> Option<solve::ScalarSlot> {
        self.model.implicit_target(row_idx)
    }

    #[cfg(test)]
    fn algebraic_projection_plan(&self) -> &solve::AlgebraicProjectionPlan {
        self.model.algebraic_projection_plan()
    }

    fn target_name_for_row(&self, row_idx: usize) -> Option<String> {
        self.model.target_name_for_row(row_idx)
    }

    fn variable_name_for_y_index(&self, index: usize) -> Option<String> {
        (index < self.y_len)
            .then(|| self.model.variable_name_for_y_index(index))
            .flatten()
    }

    fn variable_scale_for_y_index(&self, index: usize) -> f64 {
        if index < self.y_len {
            self.model.variable_scale_for_y_index(index)
        } else {
            self.parameter_scales
                .get(index - self.y_len)
                .copied()
                .unwrap_or(1.0)
        }
    }

    fn algebraic_projection_block_structure(
        &self,
        block_index: usize,
    ) -> Option<&solve::ColoredJacobianStructure> {
        self.model.algebraic_projection_block_structure(block_index)
    }

    fn algebraic_projection_plan_is_validated(&self) -> bool {
        self.model.algebraic_projection_plan_is_validated()
    }

    fn algebraic_projection_block_invalidates_earlier(&self, block_index: usize) -> bool {
        self.model
            .algebraic_projection_block_invalidates_earlier(block_index)
    }

    fn algebraic_projection_block_is_affine(&self, block_index: usize) -> bool {
        self.model.algebraic_projection_block_is_affine(block_index)
    }

    fn solve_algebraic_newton_delta(
        &self,
        block_index: usize,
        system: ScaledNewtonSystem<'_>,
    ) -> Option<DVector<f64>> {
        self.model.solve_algebraic_newton_delta(block_index, system)
    }

    fn implicit_target_assignment_is_exact(&self, row_idx: usize, target_y_index: usize) -> bool {
        self.model
            .implicit_target_assignment_is_exact(row_idx, target_y_index)
    }

    fn solve_affine_torn_delta(
        &self,
        block_index: usize,
        system: ScaledNewtonSystem<'_>,
    ) -> Option<DVector<f64>> {
        self.model.solve_affine_torn_delta(block_index, system)
    }
}

impl<M: AlgebraicProjectionModel> AlgebraicProjectionModel
    for CombinedInitializationProjectionModel<'_, M>
{
    fn eval_initial_residual(
        &self,
        values: &[f64],
        _p: &[f64],
        t: f64,
        rows: Option<&[usize]>,
        out: &mut [f64],
    ) -> Result<(), RuntimeSolveError> {
        let (y, p) = self.split_values(values)?;
        self.model.eval_initial_residual(y, p, t, rows, out)
    }

    fn initial_residual_len(&self) -> usize {
        self.model.initial_residual_len()
    }

    fn initial_target(&self, row_idx: usize) -> Option<solve::ScalarSlot> {
        self.model.initial_target(row_idx)
    }

    fn initial_projection_block_structure(
        &self,
        block_index: usize,
    ) -> Option<&solve::ColoredJacobianStructure> {
        self.model.initial_projection_block_structure(block_index)
    }

    fn initial_row_role(&self, row_idx: usize) -> Option<solve::InitializationRowRole> {
        self.model.initial_row_role(row_idx)
    }

    fn eval_initial_jacobian_v(
        &self,
        values: &[f64],
        _p: &[f64],
        t: f64,
        v: &[f64],
        rows: Option<&[usize]>,
        out: &mut [f64],
    ) -> Result<(), RuntimeSolveError> {
        let (y, p) = self.split_values(values)?;
        self.model.eval_initial_jacobian_v(y, p, t, v, rows, out)
    }

    fn eval_initial_jacobian_columns(
        &self,
        (values, _p, t): (&[f64], &[f64], f64),
        seed_len: usize,
        selection: (Option<&[usize]>, &[usize]),
        column: &mut JacobianColumnSink<'_>,
    ) -> Result<(), RuntimeSolveError> {
        let (y, p) = self.split_values(values)?;
        self.model
            .eval_initial_jacobian_columns((y, p, t), seed_len, selection, column)
    }

    fn eval_initial_target_value(
        &self,
        row_idx: usize,
        target_index: usize,
        values: &[f64],
        _p: &[f64],
        t: f64,
    ) -> Result<Option<f64>, RuntimeSolveError> {
        if target_index >= self.y_len {
            return Ok(None);
        }
        let (y, p) = self.split_values(values)?;
        self.model
            .eval_initial_target_value(row_idx, target_index, y, p, t)
    }

    fn eval_initial_residual_row(
        &self,
        row_idx: usize,
        values: &[f64],
        _p: &[f64],
        t: f64,
    ) -> Result<Option<f64>, RuntimeSolveError> {
        let (y, p) = self.split_values(values)?;
        self.model.eval_initial_residual_row(row_idx, y, p, t)
    }
}
