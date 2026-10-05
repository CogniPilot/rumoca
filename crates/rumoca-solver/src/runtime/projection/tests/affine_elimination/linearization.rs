//! Reuse of an affine block's origin linearization under its certified
//! parameter snapshot.

use super::*;
use std::cell::RefCell;

/// The cycle scaled by parameter `p[0]`: every row's gradient is `p[0]` times
/// the cycle's row. Every row but the first carries the parameter-static
/// gradient certificate on slot 0; the first has none.
struct ParameterScaledCycle {
    cycle: CyclicAffine,
    retains: bool,
    gradient_rows: RefCell<Vec<usize>>,
}

const CERTIFIED_SLOTS: &[usize] = &[0];

impl ParameterScaledCycle {
    fn new(retains: bool) -> Self {
        let expected = DVector::from_fn(DIMENSION, |row, _| 1.0 + row as f64 / 8.0);
        Self {
            cycle: CyclicAffine::new(&[], &expected),
            retains,
            gradient_rows: RefCell::new(Vec::new()),
        }
    }

    fn project(&self, scale: f64) -> Vec<f64> {
        let mut y = vec![-10.0; DIMENSION];
        project_algebraics_with_plan_certified(
            self,
            &self.cycle.plan,
            &mut y,
            AlgebraicProjectionArgs {
                parameters: &[scale],
                time: 0.0,
                state_count: 0,
                tolerance: 1e-10,
            },
            ALGEBRAIC_PROJECTION_MAX_ITERS,
        )
        .unwrap();
        y
    }
}

impl ImplicitProjectionModel for ParameterScaledCycle {
    fn eval_residual(
        &self,
        y: &[f64],
        p: &[f64],
        t: f64,
        out: &mut [f64],
    ) -> Result<(), RuntimeSolveError> {
        self.cycle.eval_residual(y, p, t, out)?;
        out.iter_mut().for_each(|value| *value *= p[0]);
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
        self.cycle.eval_jacobian_v(y, p, t, v, out)?;
        out.iter_mut().for_each(|value| *value *= p[0]);
        Ok(())
    }
    fn eval_implicit_jacobian_row(
        &self,
        row: usize,
        _: &[f64],
        p: &[f64],
        _: f64,
        gradient: &mut [f64],
    ) -> Result<bool, RuntimeSolveError> {
        self.gradient_rows.borrow_mut().push(row);
        for (column, entry) in gradient.iter_mut().enumerate() {
            *entry = p[0] * self.cycle.matrix[(row, column)];
        }
        Ok(true)
    }
    fn implicit_row_static_gradient_parameters(&self, row: usize) -> Option<&[usize]> {
        (row > 0).then_some(CERTIFIED_SLOTS)
    }
    fn affine_jacobian_cache(&self, _: usize) -> Option<&RefCell<SparseNewtonCache>> {
        self.retains.then_some(&self.cycle.cache)
    }
    fn implicit_target(&self, row: usize) -> Option<solve::ScalarSlot> {
        self.cycle.implicit_target(row)
    }
    fn algebraic_projection_plan(&self) -> &solve::AlgebraicProjectionPlan {
        &self.cycle.plan
    }
    fn target_name_for_row(&self, _: usize) -> Option<&str> {
        None
    }
    fn algebraic_projection_block_is_affine(&self, _: usize) -> bool {
        true
    }
    fn algebraic_projection_block_structure(&self, _: usize) -> Option<&solve::JacobianStructure> {
        self.cycle.algebraic_projection_block_structure(0)
    }
}

#[test]
fn a_certified_affine_linearization_is_reused_under_its_parameter_snapshot() {
    let retaining = ParameterScaledCycle::new(true);
    let forming = ParameterScaledCycle::new(false);
    let bits = |values: Vec<f64>| values.into_iter().map(f64::to_bits).collect::<Vec<_>>();
    let mut refilled = Vec::new();
    for scale in [2.0, 2.0, 2.0, 3.0, 3.0] {
        retaining.gradient_rows.borrow_mut().clear();
        assert_eq!(
            bits(retaining.project(scale)),
            bits(forming.project(scale)),
            "a reused linearization projects exactly as a formed one"
        );
        refilled.push(retaining.gradient_rows.borrow().clone());
    }
    let all = (0..DIMENSION).collect::<Vec<_>>();
    // Debug builds also form every reused linearization afresh to check it.
    let reused = std::iter::once(0)
        .chain(all.iter().copied().filter(|_| cfg!(debug_assertions)))
        .collect::<Vec<_>>();
    assert_eq!(refilled[0], all, "the first projection forms every row");
    assert_eq!(refilled[1], reused, "only the uncertified row is refilled");
    assert_eq!(refilled[2], reused);
    assert_eq!(refilled[3], all, "a changed snapshot forms every row again");
    assert_eq!(refilled[4], reused);
    // The fixture answers the rest of the projection model contract from its
    // cycle, scaled by the parameter where the residual is.
    assert_eq!(retaining.algebraic_projection_plan().blocks.len(), 1);
    assert_eq!(retaining.target_name_for_row(0), None);
    let (y, p, v) = (vec![0.0; DIMENSION], [2.0], vec![1.0; DIMENSION]);
    let (mut scaled, mut plain) = (vec![0.0; DIMENSION], vec![0.0; DIMENSION]);
    retaining
        .eval_jacobian_v(&y, &p, 0.0, &v, &mut scaled)
        .unwrap();
    retaining
        .cycle
        .eval_jacobian_v(&y, &p, 0.0, &v, &mut plain)
        .unwrap();
    assert_eq!(
        scaled,
        plain.iter().map(|value| 2.0 * value).collect::<Vec<_>>()
    );
    // A model without the certificate names no parameter snapshot.
    assert_eq!(
        retaining.cycle.implicit_row_static_gradient_parameters(1),
        None
    );
}
