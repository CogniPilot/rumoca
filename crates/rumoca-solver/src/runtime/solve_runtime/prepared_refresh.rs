use super::*;

/// Immutable refresh layout admitted against one runtime coordinate partition.
#[derive(Clone)]
pub(super) struct PreparedRefreshPlan {
    plan: solve::RefreshPlan,
    state_count: usize,
    solver_count: usize,
}

impl PreparedRefreshPlan {
    pub(super) fn new(
        plan: solve::RefreshPlan,
        state_count: usize,
        solver_count: usize,
    ) -> Result<Self, EvalSolveError> {
        let stages = plan.value_stages.iter().filter_map(|stage| match stage {
            solve::RefreshStage::ProjectionBlock { plan, .. } => Some(plan),
            _ => None,
        });
        for projection in [&plan.simultaneous_plan, &plan.value_projection_plan]
            .into_iter()
            .chain(stages)
        {
            crate::runtime::projection::validate_algebraic_projection_plan(
                projection,
                state_count,
                solver_count,
            )
            .map_err(|error| EvalSolveError::InvalidRow {
                message: error.to_string(),
                span: None,
            })?;
        }
        Ok(Self {
            plan,
            state_count,
            solver_count,
        })
    }

    pub(super) fn validated_for(
        &self,
        state_count: usize,
        solver_count: usize,
        y_len: usize,
    ) -> Result<bool, RuntimeSolveError> {
        if (state_count, solver_count, y_len)
            != (self.state_count, self.solver_count, self.solver_count)
        {
            return Err(RuntimeSolveError::solve_ir(format!(
                "prepared refresh layout mismatch: admitted states/solver {}/{}, got {}/{}, y length {}",
                self.state_count, self.solver_count, state_count, solver_count, y_len
            )));
        }
        Ok(true)
    }
}

impl std::ops::Deref for PreparedRefreshPlan {
    type Target = solve::RefreshPlan;

    fn deref(&self) -> &Self::Target {
        &self.plan
    }
}
