//! The consistent initial sensitivity `dx(t0)/dp` of a lowered model.
//!
//! The initialization system is solved for its unknowns `u` (the projection
//! plan's own) and the targets `w` of its update rows; the equations are the
//! plan's residual rows `R(u, w, p) = 0` and the updates `w = U(u, w, p)`.
//! Differentiating both along `p_j` gives one dense linear system per parameter
//! whose entries are directional derivatives of the checked residual and update
//! rows. A state that is neither an unknown nor an update target keeps the
//! constant it was lowered with and has sensitivity zero.

use rumoca_ir_solve::{self as solve, SensitivityParameter};

use crate::runtime::jacobian::solve_steady_state_sensitivity;
use crate::runtime::solve_runtime::{AlgebraicSettle, SolveRuntime};
use crate::runtime::trajectory::TrajectoryError;

fn initialization_error(reason: impl Into<String>) -> TrajectoryError {
    TrajectoryError::Initialization {
        reason: reason.into(),
    }
}

/// The stacked initialization system at one settled point.
struct InitialSystem<'a> {
    runtime: &'a SolveRuntime,
    at: (&'a [f64], &'a [f64], f64),
    settle: AlgebraicSettle,
    slots: Vec<solve::ScalarSlot>,
    /// Seed index of each slot over `[solver-y | parameter]`.
    seeds: Vec<usize>,
    /// Residual rows the projection plan solves.
    rows: Vec<usize>,
    unknown_count: usize,
    y_scalars: usize,
}

impl<'a> InitialSystem<'a> {
    fn new(
        runtime: &'a SolveRuntime,
        at: (&'a [f64], &'a [f64], f64),
        settle: AlgebraicSettle,
        plan: &solve::InitialSensitivityPlan,
    ) -> Self {
        Self {
            runtime,
            at,
            settle,
            slots: plan.slots.clone(),
            seeds: plan.seeds.clone(),
            rows: plan.rows.clone(),
            unknown_count: plan.unknown_count,
            y_scalars: runtime.model.problem.layout.y_scalars(),
        }
    }

    fn size(&self) -> usize {
        self.slots.len()
    }

    /// One column of the stacked system for a unit seed at `seed_index`: the
    /// solved residual rows, then the update rows.
    fn column(&self, seed_index: usize) -> Result<Vec<f64>, TrajectoryError> {
        let init = &self.runtime.model.problem.initialization;
        let layout = &self.runtime.model.problem.layout;
        let mut seed = vec![0.0; self.y_scalars + layout.p_scalars()];
        seed[seed_index] = 1.0;
        let residual_len = init
            .residual()
            .len()
            .map_err(|error| initialization_error(error.to_string()))?;
        let mut residual = vec![0.0; residual_len];
        self.runtime.eval_initial_residual_jacobian_v(
            self.at,
            self.settle,
            &seed,
            &mut residual,
        )?;
        let mut stacked: Vec<f64> = self.rows.iter().map(|&row| residual[row]).collect();
        if !init.update_targets().is_empty() {
            let mut updates = vec![0.0; init.update_targets().len()];
            self.runtime
                .eval_initialization_update_jacobian_v(self.at, &seed, &mut updates)?;
            stacked.extend_from_slice(&updates);
        }
        Ok(stacked)
    }

    /// The system matrix over the unknowns: `∂R/∂v` for a residual row and
    /// `I - ∂U/∂v` for an update row, which states `w = U`.
    fn matrix(&self) -> Result<Vec<Vec<f64>>, TrajectoryError> {
        let size = self.size();
        let mut matrix = vec![vec![0.0; size]; size];
        for (col, seed) in self.seeds.iter().enumerate() {
            for (row, value) in self.column(*seed)?.into_iter().enumerate() {
                matrix[row][col] = value;
            }
        }
        for (k, row) in (self.unknown_count..size).enumerate() {
            for entry in &mut matrix[row] {
                *entry = -*entry;
            }
            matrix[row][self.unknown_count + k] += 1.0;
        }
        Ok(matrix)
    }

    /// `+1` for a residual row and `-1` for an update row of the right-hand side.
    fn rhs_sign(&self, row: usize) -> f64 {
        if row < self.unknown_count { 1.0 } else { -1.0 }
    }

    /// The right-hand sides in the solver's `J X = -P` form: the residual rows
    /// enter as `∂R/∂p` and the update rows as `-∂U/∂p`.
    fn right_hand_sides(
        &self,
        parameters: &[SensitivityParameter],
    ) -> Result<Vec<Vec<f64>>, TrajectoryError> {
        let mut rhs = vec![vec![0.0; parameters.len()]; self.size()];
        for (col, parameter) in parameters.iter().enumerate() {
            let column = self.column(self.y_scalars + parameter.slot)?;
            for (row, value) in column.into_iter().enumerate() {
                rhs[row][col] = self.rhs_sign(row) * value;
            }
        }
        Ok(rhs)
    }

    /// Refuse a defined parameter that moves with a requested one and that a
    /// continuous row reads: the variational seed carries tangents for the
    /// requested parameters alone.
    fn refuse_moving_parameters(&self, solved: &[Vec<f64>]) -> Result<(), TrajectoryError> {
        let reads = solve::read_continuous_parameter_slots(&self.runtime.model.problem);
        for (slot, tangents) in self.slots.iter().zip(solved) {
            let solve::ScalarSlot::P { index, .. } = slot else {
                continue;
            };
            if reads.contains(index) && tangents.iter().any(|tangent| *tangent != 0.0) {
                return Err(initialization_error(format!(
                    "the parameter `{}` is defined by the initialization from a requested \
                     parameter and the continuous rows read it",
                    parameter_name(self.runtime, *index)
                )));
            }
        }
        Ok(())
    }
}

/// `dx(t0)/dp_j` for each requested parameter, indexed `[parameter][state]`.
pub(crate) fn initial_state_sensitivity(
    runtime: &SolveRuntime,
    at: (&[f64], &[f64], f64),
    construction: &solve::SensitivityProblem,
    settle: AlgebraicSettle,
) -> Result<Vec<Vec<f64>>, TrajectoryError> {
    let parameters = construction.parameters();
    let system = InitialSystem::new(runtime, at, settle, construction.initial());
    let solved = if system.size() == 0 {
        Vec::new()
    } else {
        solve_steady_state_sensitivity(
            &system.matrix()?,
            &system.right_hand_sides(parameters)?,
            system.size(),
            parameters.len(),
        )
        .map_err(initialization_error)?
    };
    system.refuse_moving_parameters(&solved)?;
    let n = runtime.state_count;
    let mut result = vec![vec![0.0; n]; parameters.len()];
    for (seed, tangents) in system.seeds.iter().zip(&solved) {
        if *seed < n {
            for (column, tangent) in result.iter_mut().zip(tangents) {
                column[*seed] = *tangent;
            }
        }
    }
    Ok(result)
}

/// The name bound to parameter slot `index`, for a diagnostic.
fn parameter_name(runtime: &SolveRuntime, index: usize) -> String {
    runtime
        .model
        .problem
        .layout
        .bindings()
        .iter()
        .find_map(|(name, slot)| match slot {
            solve::ScalarSlot::P { index: bound, .. } if *bound == index => Some(name.to_string()),
            _ => None,
        })
        .unwrap_or_else(|| format!("p[{index}]"))
}
