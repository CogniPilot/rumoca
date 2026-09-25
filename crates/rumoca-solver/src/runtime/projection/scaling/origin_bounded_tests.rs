//! The origin-bounded residual certificate decides exactly as the full row
//! scales at the candidate do.

use nalgebra::DMatrix;
use rumoca_ir_solve as solve;

use super::super::{ImplicitProjectionModel, RuntimeSolveError};
use super::{
    OriginRowScales, algebraic_block_scales, jacobian_row_derived, jacobian_row_magnitudes,
    origin_bounded_residual_converged, scaled_residual_converged,
};

/// A block over `y[0..n]` whose rows target the unknowns, a coordinate
/// outside the block, or nothing, with per-coordinate nominal scales.
struct ScaledBlock {
    plan: solve::AlgebraicProjectionPlan,
    targets: Vec<Option<usize>>,
    nominals: Vec<f64>,
}

impl ImplicitProjectionModel for ScaledBlock {
    fn eval_residual(
        &self,
        _y: &[f64],
        _p: &[f64],
        _t: f64,
        _out: &mut [f64],
    ) -> Result<(), RuntimeSolveError> {
        Ok(())
    }

    fn eval_jacobian_v(
        &self,
        _y: &[f64],
        _p: &[f64],
        _t: f64,
        _v: &[f64],
        _out: &mut [f64],
    ) -> Result<(), RuntimeSolveError> {
        Ok(())
    }

    fn implicit_target(&self, row: usize) -> Option<solve::ScalarSlot> {
        self.targets
            .get(row)
            .copied()
            .flatten()
            .map(solve::scalar_slot_y)
    }

    fn algebraic_projection_plan(&self) -> &solve::AlgebraicProjectionPlan {
        &self.plan
    }

    fn target_name_for_row(&self, _row: usize) -> Option<&str> {
        None
    }

    fn variable_scale_for_y_index(&self, index: usize) -> f64 {
        self.nominals[index]
    }
}

struct Random(u64);

impl Random {
    fn unit(&mut self) -> f64 {
        self.0 ^= self.0 << 13;
        self.0 ^= self.0 >> 7;
        self.0 ^= self.0 << 17;
        (self.0 % 1_000_000) as f64 / 1_000_000.0
    }

    fn choose(&mut self, count: usize) -> usize {
        (self.unit() * count as f64) as usize % count
    }
}

fn magnitude(random: &mut Random) -> f64 {
    [
        0.0,
        5e-324,
        1e-310,
        1e-8,
        0.3,
        2.0,
        1e6,
        1e300,
        f64::INFINITY,
    ][random.choose(9)]
}

#[test]
fn origin_bounded_certificate_decides_as_the_full_row_scales() {
    let mut random = Random(0x2545_f491_4f6c_dd1d);
    let mut disagreements = 0;
    let (mut accepted, mut rejected) = (0, 0);
    for _ in 0..4000 {
        let n = 1 + random.choose(5);
        let block = solve::AlgebraicProjectionBlock {
            rows: (0..n).collect(),
            y_indices: (0..n).collect(),
            tearing: None,
            alternate_charts: Vec::new(),
        };
        let model = ScaledBlock {
            plan: solve::AlgebraicProjectionPlan {
                blocks: vec![block.clone()],
            },
            targets: (0..n)
                .map(|row| [Some(row), Some(n), None][random.choose(3)])
                .collect(),
            nominals: (0..=n).map(|_| [1.0, 0.5, 1e3][random.choose(3)]).collect(),
        };
        let jacobian = DMatrix::from_fn(n, n, |_, _| {
            let sign = if random.unit() < 0.5 { -1.0 } else { 1.0 };
            sign * magnitude(&mut random) * random.unit()
        });
        let mut origin_y = (0..=n)
            .map(|_| (random.unit() - 0.5) * magnitude(&mut random).min(1e12))
            .collect::<Vec<_>>();
        let mut y = origin_y.clone();
        for index in 0..n {
            origin_y[index] = 0.0;
            y[index] = (random.unit() - 0.5) * [1.0, 1e3, 1e10][random.choose(3)];
        }
        let (origin_scales, origin_variable_scales) =
            algebraic_block_scales(&model, &origin_y, &block, &jacobian, None);
        let magnitudes = jacobian_row_magnitudes(&jacobian, None);
        let derived = jacobian_row_derived(&jacobian, &origin_variable_scales, None);
        let (scales, _) = algebraic_block_scales(&model, &y, &block, &jacobian, None);
        let tol = 1e-8;
        let residual = scales
            .iter()
            .map(|scale| tol * scale * [0.2, 0.9, 1.0, 1.1, 5.0][random.choose(5)])
            .collect::<Vec<_>>();
        let origin = OriginRowScales {
            jacobian: &jacobian,
            structure: None,
            scales: &origin_scales,
            magnitudes: &magnitudes,
            derived: &derived,
        };
        let bounded =
            origin_bounded_residual_converged(&model, &y, &block, &origin, &residual, tol);
        let full = scaled_residual_converged(&residual, &scales, tol);
        disagreements += usize::from(bounded != full);
        if full {
            accepted += 1;
        } else {
            rejected += 1;
        }
    }
    assert_eq!(disagreements, 0);
    assert!(
        accepted > 100 && rejected > 100,
        "{accepted} accepted, {rejected} rejected"
    );
}

/// A contribution underflowing to zero at the origin puts the row on its
/// fallback scale there, while the refined coordinate forms a smaller scale
/// from the same entry: the origin shortcut must not accept that row.
#[test]
fn an_underflowed_origin_contribution_does_not_bound_the_row_scale() {
    let block = solve::AlgebraicProjectionBlock {
        rows: vec![0],
        y_indices: vec![0],
        tearing: None,
        alternate_charts: Vec::new(),
    };
    let model = ScaledBlock {
        plan: solve::AlgebraicProjectionPlan {
            blocks: vec![block.clone()],
        },
        targets: vec![Some(0)],
        nominals: vec![0.5, 1.0],
    };
    let jacobian = DMatrix::from_element(1, 1, 5e-324);
    let (origin_y, y) = (vec![0.0, 0.0], vec![100.0, 0.0]);
    let (origin_scales, origin_variable_scales) =
        algebraic_block_scales(&model, &origin_y, &block, &jacobian, None);
    assert_eq!(
        5e-324 * 0.5,
        0.0,
        "the origin contribution underflows to zero"
    );
    let magnitudes = jacobian_row_magnitudes(&jacobian, None);
    let derived = jacobian_row_derived(&jacobian, &origin_variable_scales, None);
    assert_eq!(derived, [false]);
    let (scales, _) = algebraic_block_scales(&model, &y, &block, &jacobian, None);
    let (residual, tol) = ([1e-12], 1e-8);
    let origin = OriginRowScales {
        jacobian: &jacobian,
        structure: None,
        scales: &origin_scales,
        magnitudes: &magnitudes,
        derived: &derived,
    };
    let full = scaled_residual_converged(&residual, &scales, tol);
    assert!(!full, "the full row scale at y rejects the residual");
    assert_eq!(
        origin_bounded_residual_converged(&model, &y, &block, &origin, &residual, tol),
        full
    );
}
