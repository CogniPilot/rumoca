//! Reuse of an affine block's origin linearization when a row's coefficient is
//! a solver coordinate the block does not solve (`v = R_actual*i`).

use super::*;
use rumoca_eval_solve::YGradientColumn;
use std::cell::{Cell, RefCell};

/// The coordinate after the block's unknowns: the coefficient every row is
/// scaled by.
const COEFFICIENT: usize = DIMENSION;

/// The cycle scaled by the solver coordinate `y[COEFFICIENT]`. Row 0 reports
/// no gradient inputs; every other row reports that the columns of its two
/// unknowns read `reads`: the outside coefficient, or a block unknown.
struct CoordinateScaledCycle {
    cycle: CyclicAffine,
    retains: bool,
    columns: Vec<Vec<YGradientColumn>>,
    gradient_rows: RefCell<Vec<usize>>,
    /// How often row 1 was asked for its gradient inputs: once per derivation
    /// of the block's key layout.
    layouts: Cell<usize>,
}

impl CoordinateScaledCycle {
    fn new(retains: bool, reads: usize) -> Self {
        let expected = DVector::from_fn(DIMENSION, |row, _| 1.0 + row as f64 / 8.0);
        let columns = (0..DIMENSION)
            .map(|row| {
                let mut unknowns = [row, (row + 1) % DIMENSION];
                unknowns.sort_unstable();
                unknowns
                    .into_iter()
                    .map(|column| YGradientColumn {
                        column,
                        y_inputs: [reads].into(),
                        reads_time: false,
                    })
                    .collect()
            })
            .collect();
        Self {
            cycle: CyclicAffine::new(&[], &expected),
            retains,
            columns,
            gradient_rows: RefCell::new(Vec::new()),
            layouts: Cell::new(0),
        }
    }

    fn project(&self, coefficient: f64) -> Vec<f64> {
        let mut y = vec![-10.0; DIMENSION];
        y.push(coefficient);
        project_algebraics_with_plan_certified(
            self,
            &self.cycle.plan,
            &mut y,
            AlgebraicProjectionArgs {
                parameters: &[],
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

impl ImplicitProjectionModel for CoordinateScaledCycle {
    fn eval_residual(
        &self,
        y: &[f64],
        p: &[f64],
        t: f64,
        out: &mut [f64],
    ) -> Result<(), RuntimeSolveError> {
        // the residual vector is as long as the solver vector; the
        // coefficient has no row of its own
        out.fill(0.0);
        let block = &mut out[..DIMENSION];
        self.cycle.eval_residual(&y[..DIMENSION], p, t, block)?;
        block.iter_mut().for_each(|value| *value *= y[COEFFICIENT]);
        Ok(())
    }
    // Required by the trait; the projection under test never calls it.
    #[cfg_attr(coverage_nightly, coverage(off))]
    fn eval_jacobian_v(
        &self,
        y: &[f64],
        p: &[f64],
        t: f64,
        v: &[f64],
        out: &mut [f64],
    ) -> Result<(), RuntimeSolveError> {
        out.fill(0.0);
        let block = &mut out[..DIMENSION];
        self.cycle
            .eval_jacobian_v(&y[..DIMENSION], p, t, &v[..DIMENSION], block)?;
        block.iter_mut().for_each(|value| *value *= y[COEFFICIENT]);
        Ok(())
    }
    fn eval_implicit_jacobian_row(
        &self,
        row: usize,
        y: &[f64],
        _: &[f64],
        _: f64,
        gradient: &mut [f64],
    ) -> Result<bool, RuntimeSolveError> {
        self.gradient_rows.borrow_mut().push(row);
        for (column, entry) in gradient.iter_mut().enumerate().take(DIMENSION) {
            *entry = y[COEFFICIENT] * self.cycle.matrix[(row, column)];
        }
        Ok(true)
    }
    fn implicit_row_gradient_inputs(&self, row: usize) -> Option<(&[usize], Vec<YGradientColumn>)> {
        self.layouts.set(self.layouts.get() + usize::from(row == 1));
        (row > 0).then(|| (&[][..], self.columns[row].clone()))
    }
    fn affine_jacobian_cache(&self, _: usize) -> Option<&RefCell<SparseNewtonCache>> {
        self.retains.then_some(&self.cycle.cache)
    }
    fn implicit_target(&self, row: usize) -> Option<solve::ScalarSlot> {
        self.cycle.implicit_target(row)
    }
    // Required by the trait; the projection under test never calls it.
    #[cfg_attr(coverage_nightly, coverage(off))]
    fn algebraic_projection_plan(&self) -> &solve::AlgebraicProjectionPlan {
        &self.cycle.plan
    }
    fn algebraic_projection_block_is_affine(&self, _: usize) -> bool {
        true
    }
    fn algebraic_projection_block_structure(
        &self,
        _: usize,
    ) -> Option<&solve::ColoredJacobianStructure> {
        self.cycle.algebraic_projection_block_structure(0)
    }
}

/// The rows whose gradients each projection evaluated, checking on the way
/// that the retaining model projects bit for bit as one that always forms.
fn refilled_rows(reads: usize, coefficients: &[f64]) -> Vec<Vec<usize>> {
    let retaining = CoordinateScaledCycle::new(true, reads);
    let forming = CoordinateScaledCycle::new(false, reads);
    let bits = |values: Vec<f64>| values.into_iter().map(f64::to_bits).collect::<Vec<_>>();
    coefficients
        .iter()
        .map(|&coefficient| {
            retaining.gradient_rows.borrow_mut().clear();
            assert_eq!(
                bits(retaining.project(coefficient)),
                bits(forming.project(coefficient)),
                "a reused linearization projects exactly as a formed one"
            );
            retaining.gradient_rows.borrow().clone()
        })
        .collect()
}

#[test]
fn a_row_scaled_by_an_outside_coordinate_is_reused_while_that_coordinate_holds_its_bits() {
    let one_ulp_up = f64::from_bits(2.0_f64.to_bits() + 1);
    let refilled = refilled_rows(COEFFICIENT, &[2.0, 2.0, one_ulp_up, one_ulp_up, 2.0]);
    let all = (0..DIMENSION).collect::<Vec<_>>();
    // Debug builds also form every reused linearization afresh to check it.
    let reused = std::iter::once(0)
        .chain(all.iter().copied().filter(|_| cfg!(debug_assertions)))
        .collect::<Vec<_>>();
    assert_eq!(refilled[0], all, "the first projection forms every row");
    assert_eq!(
        refilled[1], reused,
        "only the row without inputs is refilled"
    );
    assert_eq!(
        refilled[2], all,
        "a coefficient one ulp away forms every row again"
    );
    assert_eq!(refilled[3], reused);
    assert_eq!(refilled[4], all, "and so does the way back");
}

#[test]
fn a_row_scaled_by_a_block_unknown_is_not_certified() {
    // Every row but the first names a block unknown as its coefficient, so no
    // row is keyed: a reuse refills all of them from their gradients.
    let refilled = refilled_rows(3, &[2.0, 2.0]);
    let all = (0..DIMENSION).collect::<Vec<_>>();
    let refilled_all = all
        .iter()
        .copied()
        .chain(all.iter().copied().filter(|_| cfg!(debug_assertions)))
        .collect::<Vec<_>>();
    assert_eq!(refilled[0], all);
    assert_eq!(
        refilled[1], refilled_all,
        "every row is refilled, none keyed"
    );
}

#[test]
fn the_gradient_inputs_of_a_block_are_derived_once_for_all_its_formations() {
    let one_ulp_up = f64::from_bits(2.0_f64.to_bits() + 1);
    let retaining = CoordinateScaledCycle::new(true, COEFFICIENT);
    // Three formations (first, one ulp up, back) and two reuses.
    for coefficient in [2.0, 2.0, one_ulp_up, one_ulp_up, 2.0] {
        retaining.project(coefficient);
    }
    assert_eq!(
        retaining.layouts.get(),
        1,
        "the key layout belongs to the block, not to a formation"
    );
}
