//! Row scaling reads the row targets bound to the projection plan's owner
//! instead of querying each row's implicit target on every linearization.

use super::*;

struct CountedTargets {
    plan: solve::AlgebraicProjectionPlan,
    targets: Option<solve::ImplicitRowTargets>,
    target_queries: Cell<usize>,
}

impl CountedTargets {
    fn new(bound: bool) -> Self {
        let block = || solve::AlgebraicProjectionBlock {
            rows: vec![0, 1],
            y_indices: vec![0, 1],
            tearing: None,
            alternate_charts: Vec::new(),
        };
        // The plan and the targets come from distinct owners, as in the
        // runtime's refresh plans.
        let canonical: solve::ProjectionBlocks = vec![block()].into();
        Self {
            plan: solve::AlgebraicProjectionPlan {
                blocks: vec![block()].into(),
            },
            targets: bound.then(|| solve::ImplicitRowTargets::from_projection(&canonical)),
            target_queries: Cell::new(0),
        }
    }
}

impl ImplicitProjectionModel for CountedTargets {
    fn eval_residual(
        &self,
        y: &[f64],
        _p: &[f64],
        _t: f64,
        out: &mut [f64],
    ) -> Result<(), RuntimeSolveError> {
        out[0] = y[0].powi(3) + y[1] - 2.0;
        out[1] = 4.0 * y[1] - 4.0;
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
        out[0] = 3.0 * y[0].powi(2) * v[0] + v[1];
        out[1] = 4.0 * v[1];
        Ok(())
    }

    fn implicit_target(&self, row_idx: usize) -> Option<solve::ScalarSlot> {
        self.target_queries.set(self.target_queries.get() + 1);
        Some(solve::scalar_slot_y(row_idx))
    }

    fn implicit_row_targets(&self) -> Option<&solve::ImplicitRowTargets> {
        self.targets.as_ref()
    }

    fn algebraic_projection_plan(&self) -> &solve::AlgebraicProjectionPlan {
        &self.plan
    }
}

fn projected(model: &CountedTargets) -> Vec<f64> {
    let mut y = vec![0.5, 3.0];
    let args = AlgebraicProjectionArgs {
        parameters: &[],
        time: 0.0,
        state_count: 0,
        tolerance: 1.0e-10,
    };
    project_algebraics_with_plan(
        model,
        &model.plan,
        &mut y,
        args,
        ALGEBRAIC_PROJECTION_MAX_ITERS,
    )
    .expect("the coupled block should converge");
    y
}

#[test]
fn plan_row_scales_read_bound_targets_without_per_row_queries() {
    let y = [0.5, 3.0];
    let bound = CountedTargets::new(true);
    let unbound = CountedTargets::new(false);
    let scales = algebraic_plan_row_scales(&bound, &y, &[], 0.0, &bound.plan).unwrap();
    assert_eq!(bound.target_queries.get(), 0);
    assert_eq!(
        scales,
        algebraic_plan_row_scales(&unbound, &y, &[], 0.0, &unbound.plan).unwrap()
    );
    assert_eq!(
        unbound.target_queries.get(),
        2,
        "without bound targets each row is queried"
    );
}

#[test]
fn projection_reads_bound_targets_without_per_row_queries() {
    let bound = CountedTargets::new(true);
    let unbound = CountedTargets::new(false);
    let y = projected(&bound);
    assert_eq!(bound.target_queries.get(), 0);
    assert_eq!(y, projected(&unbound));
    assert!(
        unbound.target_queries.get() > 0,
        "without bound targets the projection queries rows"
    );
    assert!((y[0] - 1.0).abs() < 1.0e-8 && (y[1] - 1.0).abs() < 1.0e-8);
}
