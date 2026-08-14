//! The saturating-reluctance projection block of
//! `Modelica.Magnetic.FluxTubes.Examples.BasicExamples.SaturatedInductor`.
//!
//! Split from `projection/tests.rs` under the SPEC_0021 file-size gate.

use super::super::*;

/// The `Modelica.Magnetic.FluxTubes` saturating-reluctance block of
/// `Examples.BasicExamples.SaturatedInductor`, reduced to the five unknowns that
/// carry its nonlinearity.
///
/// `BaseClasses.FixedShape` (MSL 4.1.0) writes the iron path as
/// `B_N = |B/B_myMax|`,
/// `mu_r = 1 + (mu_i - 1 + c_a*B_N)/(1 + c_b*B_N + B_N^n)`,
/// `R_m = 1/G_m`, `V_m = Phi*R_m`, `B = Phi/A`, with `G_m = mu_0*mu_r*A/l` from
/// `Shapes.FixedShape.Cuboid`. The example puts a constant leakage reluctance and a
/// parasitic air gap around it, so the flux the coil carries splits between the two
/// branches. Coefficients are `Material.SoftMagnetic.ElectricSheet.M350_50A`;
/// geometry is the example's core.
///
/// Two properties of this block defeat an unlimited damped Newton together. Past the
/// permeability peak (`B_N > 1`) `mu_r` falls by an order of magnitude over a tenth
/// of a tesla, so the tangent extrapolates it through zero into a negative — and
/// physically impossible — relative permeability. And `R_m = 1/G_m` has a Jacobian
/// entry of `1/G_m^2 ~ 1e11`, which scales that row so hard that a reluctance wrong
/// by `1e8` still reads as a small residual. The step that leaves the branch
/// therefore *lowers* the scaled residual, and the line search takes it.
struct SaturatingReluctanceModel {
    /// Flux the coil carries into the parallel leakage/iron split.
    total_flux: f64,
    plan: solve::AlgebraicProjectionPlan,
}

impl SaturatingReluctanceModel {
    const MU_I: f64 = 1210.0;
    const B_MY_MAX: f64 = 1.16;
    const C_A: f64 = 24630.0;
    const C_B: f64 = 2.44;
    const N: f64 = 14.0;
    const MU_0: f64 = 4.0 * std::f64::consts::PI * 1.0e-7;
    /// Core cross-section `a*b` and mean path length `4*0.065 m`.
    const AREA: f64 = 6.25e-4;
    const LENGTH: f64 = 0.26;
    /// `r_mAirPar.R_m` and `r_mLeak.R_m` of the example.
    const AIR_GAP_RELUCTANCE: f64 = 127_323.954_404_204_18;
    const LEAKAGE_RELUCTANCE: f64 = 1.2e6;

    fn permeability(flux_density: f64) -> f64 {
        let b_n = (flux_density / Self::B_MY_MAX).abs();
        1.0 + (Self::MU_I - 1.0 + Self::C_A * b_n) / (1.0 + Self::C_B * b_n + b_n.powf(Self::N))
    }

    /// `d(mu_r)/dB`.
    fn permeability_slope(flux_density: f64) -> f64 {
        let b_n = (flux_density / Self::B_MY_MAX).abs();
        let numerator = Self::MU_I - 1.0 + Self::C_A * b_n;
        let denominator = 1.0 + Self::C_B * b_n + b_n.powf(Self::N);
        let denominator_slope = Self::C_B + Self::N * b_n.powf(Self::N - 1.0);
        (Self::C_A * denominator - numerator * denominator_slope) / (denominator * denominator)
            * flux_density.signum()
            / Self::B_MY_MAX
    }

    /// A guess that satisfies every row except the flux split, i.e. the algebraic
    /// solution the integrator carried in from a neighbouring step.
    fn consistent_guess(flux_density: f64, permeability: f64) -> Vec<f64> {
        let permeance = Self::MU_0 * permeability * Self::AREA / Self::LENGTH;
        let reluctance = 1.0 / permeance;
        vec![
            flux_density,
            permeability,
            permeance,
            reluctance,
            flux_density * Self::AREA * reluctance,
        ]
    }
}

impl ImplicitProjectionModel for SaturatingReluctanceModel {
    fn eval_residual(
        &self,
        y: &[f64],
        _p: &[f64],
        _t: f64,
        out: &mut [f64],
    ) -> Result<(), RuntimeSolveError> {
        let (flux_density, permeability, permeance, reluctance, magnetic_potential) =
            (y[0], y[1], y[2], y[3], y[4]);
        let iron_flux = flux_density * Self::AREA;
        out[0] = (self.total_flux - iron_flux) * Self::LEAKAGE_RELUCTANCE
            - (iron_flux * Self::AIR_GAP_RELUCTANCE + magnetic_potential);
        out[1] = permeability - Self::permeability(flux_density);
        out[2] = permeance - Self::MU_0 * permeability * Self::AREA / Self::LENGTH;
        out[3] = reluctance - 1.0 / permeance;
        out[4] = magnetic_potential - iron_flux * reluctance;
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
        let (flux_density, permeance, reluctance) = (y[0], y[2], y[3]);
        out[0] = -Self::AREA * (Self::LEAKAGE_RELUCTANCE + Self::AIR_GAP_RELUCTANCE) * v[0] - v[4];
        out[1] = v[1] - Self::permeability_slope(flux_density) * v[0];
        out[2] = v[2] - Self::MU_0 * Self::AREA / Self::LENGTH * v[1];
        out[3] = v[3] + v[2] / (permeance * permeance);
        out[4] = v[4] - Self::AREA * reluctance * v[0] - flux_density * Self::AREA * v[3];
        Ok(())
    }

    fn implicit_target(&self, row_idx: usize) -> Option<solve::ScalarSlot> {
        Some(solve::scalar_slot_y(row_idx))
    }

    fn algebraic_projection_plan(&self) -> &solve::AlgebraicProjectionPlan {
        &self.plan
    }

    fn target_name_for_row(&self, row_idx: usize) -> Option<&str> {
        Some(match row_idx {
            0 => "r_mFe.B",
            1 => "r_mFe.mu_r",
            2 => "r_mFe.G_m",
            3 => "r_mFe.R_m",
            _ => "r_mFe.V_m",
        })
    }
}

fn saturating_reluctance_model(total_flux: f64) -> SaturatingReluctanceModel {
    SaturatingReluctanceModel {
        total_flux,
        plan: solve::AlgebraicProjectionPlan {
            blocks: vec![solve::AlgebraicProjectionBlock {
                rows: vec![0, 1, 2, 3, 4],
                y_indices: vec![0, 1, 2, 3, 4],
            }],
        },
    }
}

/// `SaturatedInductor` at `t = 0.027 s`, an output sample the projection stalls on:
/// the guess is the algebraic solution the integrator carried in from a neighbouring
/// step, the reference is OMC 4.1.0 at that instant (`r_mFe.B = 1.49365`,
/// `r_mFe.mu_r = 854.292`). Which step sequence hands the block a guess this far out
/// depends on the output grid and on whether a start-instant stop restarts the
/// integrator; that the block stalls once handed one does not.
const SATURATION_KNEE_TOTAL_FLUX: f64 = 0.001_334_04;
const SATURATION_KNEE_GUESS: [f64; 2] = [1.383_918, 1944.049];
const SATURATION_KNEE_FLUX_DENSITY: f64 = 1.493_653_72;
const SATURATION_KNEE_PERMEABILITY: f64 = 854.297_327_7;

fn saturation_knee_args<'a>() -> AlgebraicProjectionArgs<'a> {
    AlgebraicProjectionArgs {
        parameters: &[],
        time: 0.027,
        state_count: 0,
        tolerance: 1.0e-9,
    }
}

#[test]
fn unlimited_newton_leaves_the_physical_branch_of_a_saturating_reluctance() {
    let model = saturating_reluctance_model(SATURATION_KNEE_TOTAL_FLUX);
    let mut y = SaturatingReluctanceModel::consistent_guess(
        SATURATION_KNEE_GUESS[0],
        SATURATION_KNEE_GUESS[1],
    );

    let outcome = project_algebraics_with_plan_inner(
        &model,
        model.algebraic_projection_plan(),
        &mut y,
        saturation_knee_args(),
        ALGEBRAIC_PROJECTION_MAX_ITERS,
        StepLimit::None,
        false,
    );

    // Documents why the branch limit exists: the unlimited pass ends off the branch it
    // started on, at a negative relative permeability no soft-magnetic material has.
    assert!(
        outcome.is_err(),
        "expected the unlimited damped Newton to fail on the saturation knee, got y={y:?}"
    );
    assert!(
        y[1] < 0.0,
        "expected the unlimited pass to end at a non-physical permeability, got {}",
        y[1]
    );
}

#[test]
fn step_limited_projection_solves_the_saturation_knee_on_the_physical_branch() {
    let model = saturating_reluctance_model(SATURATION_KNEE_TOTAL_FLUX);
    let mut y = SaturatingReluctanceModel::consistent_guess(
        SATURATION_KNEE_GUESS[0],
        SATURATION_KNEE_GUESS[1],
    );

    project_algebraics_with_plan(
        &model,
        model.algebraic_projection_plan(),
        &mut y,
        saturation_knee_args(),
        ALGEBRAIC_PROJECTION_MAX_ITERS,
    )
    .expect("the saturating reluctance block has a solution reachable from this guess");

    assert!(
        (y[0] - SATURATION_KNEE_FLUX_DENSITY).abs() <= 1.0e-6,
        "flux density settled at {}, expected {SATURATION_KNEE_FLUX_DENSITY}",
        y[0]
    );
    assert!(
        (y[1] - SATURATION_KNEE_PERMEABILITY).abs() <= 1.0e-3,
        "relative permeability settled at {}, expected {SATURATION_KNEE_PERMEABILITY}",
        y[1]
    );
    // The MSL approximation is `1 + non-negative/positive`, so a solve that reports
    // less than 1 — or a negative reluctance — has left the branch.
    assert!(
        y[1] >= 1.0,
        "relative permeability {} is not physical",
        y[1]
    );
    assert!(y[3] > 0.0, "reluctance {} is not physical", y[3]);
}

/// Two independent 1x1 blocks: `y[0]` is solvable (`y[0] = 3`), `y[1]` is not
/// (a constant nonzero residual with a zero Jacobian).
///
/// The solvable half is what gives the rollback test its teeth. A fixture whose only
/// block is unsolvable never accepts a step, so `y` is never written and "the failure
/// restored every slot" holds for free; here the projection provably moves `y[0]` off
/// its starting value before the other block fails.
struct PartlySolvableProjectionModel {
    plan: solve::AlgebraicProjectionPlan,
}

impl PartlySolvableProjectionModel {
    const SOLUTION: f64 = 3.0;

    fn new(blocks: Vec<solve::AlgebraicProjectionBlock>) -> Self {
        Self {
            plan: solve::AlgebraicProjectionPlan { blocks },
        }
    }

    fn solvable_block() -> solve::AlgebraicProjectionBlock {
        solve::AlgebraicProjectionBlock {
            rows: vec![0],
            y_indices: vec![0],
        }
    }

    fn unsolvable_block() -> solve::AlgebraicProjectionBlock {
        solve::AlgebraicProjectionBlock {
            rows: vec![1],
            y_indices: vec![1],
        }
    }
}

impl ImplicitProjectionModel for PartlySolvableProjectionModel {
    fn eval_residual(
        &self,
        y: &[f64],
        _p: &[f64],
        _t: f64,
        out: &mut [f64],
    ) -> Result<(), RuntimeSolveError> {
        out[0] = y[0] - Self::SOLUTION;
        out[1] = 1.0;
        Ok(())
    }

    fn eval_jacobian_v(
        &self,
        _y: &[f64],
        _p: &[f64],
        _t: f64,
        v: &[f64],
        out: &mut [f64],
    ) -> Result<(), RuntimeSolveError> {
        out[0] = v[0];
        out[1] = 0.0;
        Ok(())
    }

    fn implicit_target(&self, row_idx: usize) -> Option<solve::ScalarSlot> {
        Some(solve::scalar_slot_y(row_idx))
    }

    fn algebraic_projection_plan(&self) -> &solve::AlgebraicProjectionPlan {
        &self.plan
    }

    fn target_name_for_row(&self, row_idx: usize) -> Option<&str> {
        Some(if row_idx == 0 {
            "solvable"
        } else {
            "unsolvable"
        })
    }
}

fn partly_solvable_args<'a>() -> AlgebraicProjectionArgs<'a> {
    AlgebraicProjectionArgs {
        parameters: &[],
        time: 0.0,
        state_count: 0,
        tolerance: 1.0e-12,
    }
}

/// A projection with no solution must still fail, and must leave every unknown
/// exactly where it found it: branch preservation is never permission to accept
/// an unsolved residual system.
#[test]
fn a_step_limited_projection_that_cannot_converge_still_fails_loudly() {
    let original = vec![-11.0, 7.0];

    // Control: on its own the solvable block moves `y[0]` from -11 to 3, so the
    // rollback assertion below is about a slot the projection really does write.
    let solvable =
        PartlySolvableProjectionModel::new(vec![PartlySolvableProjectionModel::solvable_block()]);
    let mut moved = original.clone();
    project_algebraics_with_plan(
        &solvable,
        solvable.algebraic_projection_plan(),
        &mut moved,
        partly_solvable_args(),
        ALGEBRAIC_PROJECTION_MAX_ITERS,
    )
    .expect("the solvable block alone has a solution");
    assert!(
        (moved[0] - PartlySolvableProjectionModel::SOLUTION).abs() <= 1.0e-9,
        "control: expected the solvable block to drive y[0] to 3, got {}",
        moved[0]
    );
    assert_ne!(
        moved[0], original[0],
        "control: the fixture must observably mutate y[0]"
    );

    // Combined: the same solvable block plus one that cannot be settled.
    let combined = PartlySolvableProjectionModel::new(vec![
        PartlySolvableProjectionModel::solvable_block(),
        PartlySolvableProjectionModel::unsolvable_block(),
    ]);
    let mut y = original.clone();
    let error = project_algebraics_with_plan(
        &combined,
        combined.algebraic_projection_plan(),
        &mut y,
        partly_solvable_args(),
        ALGEBRAIC_PROJECTION_MAX_ITERS,
    )
    .expect_err("a constant nonzero residual has no solution");

    assert!(
        error.to_string().contains("did not converge"),
        "unexpected projection failure: {error}"
    );
    assert_eq!(
        y, original,
        "a failed projection must restore every Y slot, including the one the \
         solvable block had already moved"
    );
}

#[test]
fn step_limit_bounds_each_unknown_by_a_fraction_of_its_own_magnitude() {
    let limit = StepLimit::Fraction(0.25);
    let y = [4.0, 1000.0];
    // The second unknown asks for a step of 2.5x its magnitude; the cap admits
    // 0.25*1000 = 250, i.e. one tenth of the requested delta.
    let alpha = limit.initial_alpha(&y, &[0, 1], &[0.5, 2500.0], &[1.0, 1.0]);
    assert!(
        (alpha - 0.1).abs() <= 1.0e-12,
        "expected the tightest per-unknown bound to set alpha, got {alpha}"
    );
    assert!(
        (StepLimit::None.initial_alpha(&y, &[0, 1], &[0.5, 2500.0], &[1.0, 1.0]) - 1.0).abs()
            <= 1.0e-12,
        "an unlimited step must start at the full Newton length"
    );
    // A near-zero unknown falls back to its declared scale rather than pinning
    // alpha at zero.
    let alpha_at_zero = limit.initial_alpha(&[0.0], &[0], &[8.0], &[2.0]);
    assert!(
        (alpha_at_zero - 0.0625).abs() <= 1.0e-12,
        "expected the declared scale to bound a step from zero, got {alpha_at_zero}"
    );
}
