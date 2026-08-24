//! Order-robustness of the torn algebraic projection.
//!
//! The `Modelica.Magnetic.FluxTubes` saturating-reluctance loop diverges under a
//! dense block Newton when it is handed a distant warm start, and *which* start
//! the integrator hands it depends on the flattened variable order. The torn
//! solve iterates only the tear variable and back-substitutes the rest causally,
//! so it converges to the same physical point no matter how the loop's unknowns
//! and equations are numbered. These tests pin that: the same loop, relabelled
//! by several solver-index permutations, projects to the same solution, and it
//! holds the physical branch even with the branch step limit switched off (the
//! band-aid the dense solve needs and the torn solve does not).

use super::super::*;

const MU_I: f64 = 1210.0;
const B_MY_MAX: f64 = 1.16;
const C_A: f64 = 24630.0;
const C_B: f64 = 2.44;
const N: f64 = 14.0;
const MU_0: f64 = 4.0 * std::f64::consts::PI * 1.0e-7;
const AREA: f64 = 6.25e-4;
const LENGTH: f64 = 0.26;
const AIR_GAP_RELUCTANCE: f64 = 127_323.954_404_204_18;
const LEAKAGE_RELUCTANCE: f64 = 1.2e6;

const KNEE_TOTAL_FLUX: f64 = 0.001_334_04;
const KNEE_GUESS_B: f64 = 1.383_918;
const KNEE_GUESS_MU: f64 = 1944.049;
const KNEE_FLUX_DENSITY: f64 = 1.493_653_72;
const KNEE_PERMEABILITY: f64 = 854.297_327_7;

fn permeability(flux_density: f64) -> f64 {
    let b_n = (flux_density / B_MY_MAX).abs();
    1.0 + (MU_I - 1.0 + C_A * b_n) / (1.0 + C_B * b_n + b_n.powf(N))
}

/// The saturating-reluctance loop with its five logical unknowns
/// `[B, mu_r, G_m, R_m, V_m]` and five equations placed at caller-chosen solver
/// slots and residual rows, so one physical system can be projected under many
/// index orderings.
struct PermutedReluctance {
    total_flux: f64,
    /// Solver slot holding each logical unknown.
    slots: [usize; 5],
    /// Residual row of each logical equation. Logical equation `e` is matched to
    /// logical unknown `e`.
    rows: [usize; 5],
    plan: solve::AlgebraicProjectionPlan,
}

impl PermutedReluctance {
    fn new(total_flux: f64, slots: [usize; 5], rows: [usize; 5]) -> Self {
        let block = solve::AlgebraicProjectionBlock {
            rows: rows.to_vec(),
            y_indices: slots.to_vec(),
            tearing: Some(solve::BlockTearing {
                // Tear on B (logical unknown 0); the flux-split equation
                // (logical equation 0) closes the loop as the lone residual.
                tear_y_indices: vec![slots[0]],
                residual_rows: vec![rows[0]],
                causal_steps: vec![
                    solve::CausalStep {
                        row: rows[1],
                        y_index: slots[1],
                    },
                    solve::CausalStep {
                        row: rows[2],
                        y_index: slots[2],
                    },
                    solve::CausalStep {
                        row: rows[3],
                        y_index: slots[3],
                    },
                    solve::CausalStep {
                        row: rows[4],
                        y_index: slots[4],
                    },
                ],
            }),
        };
        Self {
            total_flux,
            slots,
            rows,
            plan: solve::AlgebraicProjectionPlan {
                blocks: vec![block],
            },
        }
    }

    fn logical_residuals(&self, y: &[f64]) -> [f64; 5] {
        let flux_density = y[self.slots[0]];
        let permeability_value = y[self.slots[1]];
        let permeance = y[self.slots[2]];
        let reluctance = y[self.slots[3]];
        let magnetic_potential = y[self.slots[4]];
        let iron_flux = flux_density * AREA;
        [
            (self.total_flux - iron_flux) * LEAKAGE_RELUCTANCE
                - (iron_flux * AIR_GAP_RELUCTANCE + magnetic_potential),
            permeability_value - permeability(flux_density),
            permeance - MU_0 * permeability_value * AREA / LENGTH,
            reluctance - 1.0 / permeance,
            magnetic_potential - iron_flux * reluctance,
        ]
    }

    fn logical_equation_of_row(&self, row_idx: usize) -> Option<usize> {
        self.rows.iter().position(|&row| row == row_idx)
    }

    /// A distant-but-consistent warm start: `B` and `mu_r` sit far past the knee,
    /// the rest are back-substituted from them, exactly what a neighbouring
    /// integrator step leaves behind.
    fn distant_warm_start(&self) -> Vec<f64> {
        let permeance = MU_0 * KNEE_GUESS_MU * AREA / LENGTH;
        let reluctance = 1.0 / permeance;
        let magnetic_potential = KNEE_GUESS_B * AREA * reluctance;
        let mut y = vec![0.0; 5];
        y[self.slots[0]] = KNEE_GUESS_B;
        y[self.slots[1]] = KNEE_GUESS_MU;
        y[self.slots[2]] = permeance;
        y[self.slots[3]] = reluctance;
        y[self.slots[4]] = magnetic_potential;
        y
    }

    /// The same coupled block without a tearing, so the projection solves it as a
    /// dense block Newton over all five unknowns. Used to contrast the dense
    /// solve against the torn solve from the identical warm start.
    fn dense_plan(&self) -> solve::AlgebraicProjectionPlan {
        solve::AlgebraicProjectionPlan {
            blocks: vec![solve::AlgebraicProjectionBlock {
                rows: self.rows.to_vec(),
                y_indices: self.slots.to_vec(),
                tearing: None,
            }],
        }
    }
}

impl ImplicitProjectionModel for PermutedReluctance {
    fn eval_residual(
        &self,
        y: &[f64],
        _p: &[f64],
        _t: f64,
        out: &mut [f64],
    ) -> Result<(), RuntimeSolveError> {
        let residuals = self.logical_residuals(y);
        for (equation, &row) in self.rows.iter().enumerate() {
            out[row] = residuals[equation];
        }
        Ok(())
    }

    fn eval_jacobian_v(
        &self,
        y: &[f64],
        _p: &[f64],
        _t: f64,
        v: &[f64],
        out: &mut [f64],
    ) -> Result<(), RuntimeSolveError> {
        // A directional finite difference of the residual. The torn solve never
        // asks for this (it differences the reduced residual itself); it exists
        // only so the dense fallback the trait promises stays well defined.
        let base = self.logical_residuals(y);
        let norm = v.iter().fold(0.0_f64, |acc, value| acc.max(value.abs()));
        let step = if norm > 0.0 { 1.0e-7 / norm } else { 1.0e-7 };
        let bumped: Vec<f64> = y
            .iter()
            .zip(v.iter())
            .map(|(value, direction)| value + step * direction)
            .collect();
        let bumped = self.logical_residuals(&bumped);
        for (equation, &row) in self.rows.iter().enumerate() {
            out[row] = (bumped[equation] - base[equation]) / step;
        }
        Ok(())
    }

    fn eval_implicit_residual_row(
        &self,
        row_idx: usize,
        y: &[f64],
        _p: &[f64],
        _t: f64,
    ) -> Result<Option<f64>, RuntimeSolveError> {
        Ok(self
            .logical_equation_of_row(row_idx)
            .map(|equation| self.logical_residuals(y)[equation]))
    }

    // Logical equations 1..=4 are exact explicit assignments for their unknown,
    // exactly as the compiler certifies the real saturating-reluctance loop:
    // `mu_r = permeability(B)`, `G_m = mu_0*mu_r*A/l`, `R_m = 1/G_m`, and
    // `V_m = iron_flux*R_m`. Back-substitution recovers each by evaluating that
    // assignment; the flux-split equation 0 stays the tear residual.
    fn implicit_target_assignment_is_exact(&self, row_idx: usize, target_y_index: usize) -> bool {
        match self.logical_equation_of_row(row_idx) {
            Some(equation) if equation >= 1 => self.slots[equation] == target_y_index,
            _ => false,
        }
    }

    fn eval_implicit_target_value(
        &self,
        row_idx: usize,
        target_y_index: usize,
        y: &[f64],
        _p: &[f64],
        _t: f64,
    ) -> Result<Option<f64>, RuntimeSolveError> {
        let Some(equation) = self.logical_equation_of_row(row_idx) else {
            return Ok(None);
        };
        if self.slots[equation] != target_y_index {
            return Ok(None);
        }
        let flux_density = y[self.slots[0]];
        let permeability_value = y[self.slots[1]];
        let permeance = y[self.slots[2]];
        let reluctance = y[self.slots[3]];
        let iron_flux = flux_density * AREA;
        Ok(match equation {
            1 => Some(permeability(flux_density)),
            2 => Some(MU_0 * permeability_value * AREA / LENGTH),
            3 => Some(1.0 / permeance),
            4 => Some(iron_flux * reluctance),
            _ => None,
        })
    }

    fn implicit_target(&self, row_idx: usize) -> Option<solve::ScalarSlot> {
        self.logical_equation_of_row(row_idx)
            .map(|equation| solve::scalar_slot_y(self.slots[equation]))
    }

    fn algebraic_projection_plan(&self) -> &solve::AlgebraicProjectionPlan {
        &self.plan
    }

    fn target_name_for_row(&self, row_idx: usize) -> Option<&str> {
        self.logical_equation_of_row(row_idx).map(|equation| {
            [
                "r_mFe.flux_split",
                "r_mFe.mu_r",
                "r_mFe.G_m",
                "r_mFe.R_m",
                "r_mFe.V_m",
            ][equation]
        })
    }
}

fn knee_args<'a>() -> AlgebraicProjectionArgs<'a> {
    AlgebraicProjectionArgs {
        parameters: &[],
        time: 0.027,
        state_count: 0,
        tolerance: 1.0e-9,
    }
}

fn assert_physical_solution(model: &PermutedReluctance, y: &[f64]) {
    let flux_density = y[model.slots[0]];
    let permeability_value = y[model.slots[1]];
    let reluctance = y[model.slots[3]];
    assert!(
        (flux_density - KNEE_FLUX_DENSITY).abs() <= 1.0e-6,
        "flux density settled at {flux_density}, expected {KNEE_FLUX_DENSITY}"
    );
    assert!(
        (permeability_value - KNEE_PERMEABILITY).abs() <= 1.0e-3,
        "relative permeability settled at {permeability_value}, expected {KNEE_PERMEABILITY}"
    );
    assert!(
        permeability_value >= 1.0,
        "relative permeability {permeability_value} is not physical"
    );
    assert!(reluctance > 0.0, "reluctance {reluctance} is not physical");
}

/// The torn solve reaches the same physical point from the distant warm start
/// under every solver-index permutation of the loop's unknowns and equations.
#[test]
fn torn_projection_is_invariant_to_solver_variable_order() {
    let orderings: [([usize; 5], [usize; 5]); 4] = [
        ([0, 1, 2, 3, 4], [0, 1, 2, 3, 4]),
        ([4, 3, 2, 1, 0], [4, 3, 2, 1, 0]),
        ([2, 0, 4, 1, 3], [1, 4, 0, 3, 2]),
        ([3, 1, 4, 0, 2], [2, 0, 3, 4, 1]),
    ];
    for (slots, rows) in orderings {
        let model = PermutedReluctance::new(KNEE_TOTAL_FLUX, slots, rows);
        let mut y = model.distant_warm_start();
        project_algebraics_with_plan(
            &model,
            model.algebraic_projection_plan(),
            &mut y,
            knee_args(),
            ALGEBRAIC_PROJECTION_MAX_ITERS,
        )
        .unwrap_or_else(|error| {
            panic!("torn projection failed for slots {slots:?}, rows {rows:?}: {error}")
        });
        assert_physical_solution(&model, &y);
    }
}

/// The torn solve holds the physical branch with the branch step limit switched
/// off, where the dense block Newton over the same block leaves it. Both paths
/// start from the identical inconsistent warm start; the only difference is the
/// tearing. With the step limit off the dense tangent carries `mu_r` through
/// zero into a non-physical negative, while the torn solve iterates only the
/// tear variable `B` and recovers the rest by exact back-substitution, so it
/// stays on the branch and reaches the physical solution.
///
/// This runs both projections rather than describing the contrast: it fails if
/// a future change breaks torn branch-holding (the torn solve would then miss
/// the physical solution) and it fails if the dense path stops diverging (the
/// control would no longer establish that tearing is what holds the branch).
#[test]
fn torn_projection_holds_the_branch_without_a_step_limit() {
    let model = PermutedReluctance::new(KNEE_TOTAL_FLUX, [0, 1, 2, 3, 4], [0, 1, 2, 3, 4]);
    let start = model.distant_warm_start();

    // Dense block Newton over all five unknowns, unlimited, from the warm start.
    let mut dense = start.clone();
    let dense_outcome = project_algebraics_with_plan_inner(
        &model,
        &model.dense_plan(),
        &mut dense,
        knee_args(),
        ALGEBRAIC_PROJECTION_MAX_ITERS,
        StepLimit::None,
        false,
    );
    let dense_permeability = dense[model.slots[1]];
    let dense_flux_density = dense[model.slots[0]];
    eprintln!(
        "dense (tearing=None, unlimited): outcome={:?}, B={dense_flux_density}, mu_r={dense_permeability}",
        dense_outcome.as_ref().map(|_| "converged")
    );
    // The dense path leaves the physical branch (a relative permeability the MSL
    // `1 + non-negative/positive` form can never produce) or fails outright.
    assert!(
        dense_outcome.is_err() || dense_permeability < 1.0,
        "the unlimited dense Newton must leave the physical branch or fail; \
         instead it returned {dense_outcome:?} with mu_r = {dense_permeability}"
    );
    let dense_reached_physical =
        dense_outcome.is_ok() && (dense_flux_density - KNEE_FLUX_DENSITY).abs() <= 1.0e-6;
    assert!(
        !dense_reached_physical,
        "control invalidated: the dense Newton also reached the physical branch, \
         so this guard no longer isolates torn branch-holding"
    );

    // Torn solve over the same block, unlimited, from the identical warm start.
    let mut torn = start.clone();
    project_algebraics_with_plan_inner(
        &model,
        model.algebraic_projection_plan(),
        &mut torn,
        knee_args(),
        ALGEBRAIC_PROJECTION_MAX_ITERS,
        StepLimit::None,
        false,
    )
    .expect("the torn solve converges on the physical branch without a step limit");
    eprintln!(
        "torn (tearing=Some, unlimited): converged, B={}, mu_r={}",
        torn[model.slots[0]], torn[model.slots[1]]
    );
    assert_physical_solution(&model, &torn);
}
