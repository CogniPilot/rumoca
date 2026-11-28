use super::super::*;

/// Two equal resistances share a current source: each current must be 0.05.
/// Recovering currents from the common voltage loses accuracy for small R.
struct ParallelResistances {
    resistance: f64,
    offset: f64,
    equation_scale: f64,
    sweeps: std::cell::Cell<usize>,
    plan: solve::AlgebraicProjectionPlan,
}

impl ParallelResistances {
    fn new(resistance: f64, offset: f64, equation_scale: f64) -> Self {
        Self {
            resistance,
            offset,
            equation_scale,
            sweeps: std::cell::Cell::new(0),
            plan: solve::AlgebraicProjectionPlan {
                blocks: (vec![solve::AlgebraicProjectionBlock {
                    rows: vec![0, 1, 2],
                    y_indices: vec![0, 1, 2],
                    tearing: Some(solve::BlockTearing {
                        tear_y_indices: vec![0],
                        residual_rows: vec![2],
                        causal_steps: vec![
                            solve::CausalStep {
                                row: 0,
                                y_index: 1,
                                ..Default::default()
                            },
                            solve::CausalStep {
                                row: 1,
                                y_index: 2,
                                ..Default::default()
                            },
                        ],
                    }),
                    alternate_charts: Vec::new(),
                }])
                .into(),
            },
        }
    }
}

impl ImplicitProjectionModel for ParallelResistances {
    fn torn_block_sweep(
        &self,
        tearing: &solve::BlockTearing,
        y: &mut [f64],
        p: &[f64],
        t: f64,
        out: &mut Vec<f64>,
    ) -> Result<bool, RuntimeSolveError> {
        self.sweeps.set(self.sweeps.get() + 1);
        tearing::per_row_torn_block_sweep(self, tearing, y, p, t, out)
    }

    fn eval_residual(
        &self,
        y: &[f64],
        _p: &[f64],
        _t: f64,
        out: &mut [f64],
    ) -> Result<(), RuntimeSolveError> {
        out[0] = y[0] - self.offset - self.resistance * y[1];
        out[1] = y[0] - self.offset - self.resistance * y[2];
        out[2] = self.equation_scale * (y[1] + y[2] - 0.1);
        Ok(())
    }

    fn eval_implicit_residual_row(
        &self,
        row: usize,
        y: &[f64],
        p: &[f64],
        t: f64,
    ) -> Result<Option<f64>, RuntimeSolveError> {
        let mut residual = [0.0; 3];
        self.eval_residual(y, p, t, &mut residual)?;
        Ok(residual.get(row).copied())
    }

    fn eval_jacobian_v(
        &self,
        _y: &[f64],
        _p: &[f64],
        _t: f64,
        v: &[f64],
        out: &mut [f64],
    ) -> Result<(), RuntimeSolveError> {
        out[0] = v[0] - self.resistance * v[1];
        out[1] = v[0] - self.resistance * v[2];
        out[2] = self.equation_scale * (v[1] + v[2]);
        Ok(())
    }

    fn implicit_target(&self, row: usize) -> Option<solve::ScalarSlot> {
        [1, 2, 0].get(row).copied().map(solve::scalar_slot_y)
    }

    fn algebraic_projection_plan(&self) -> &solve::AlgebraicProjectionPlan {
        &self.plan
    }

    fn algebraic_projection_block_is_affine(&self, _block: usize) -> bool {
        true
    }

    fn implicit_target_assignment_is_exact(&self, row: usize, target: usize) -> bool {
        matches!((row, target), (0, 1) | (1, 2))
    }

    fn eval_implicit_target_value(
        &self,
        row: usize,
        target: usize,
        y: &[f64],
        _p: &[f64],
        _t: f64,
    ) -> Result<Option<f64>, RuntimeSolveError> {
        Ok(self
            .implicit_target_assignment_is_exact(row, target)
            .then(|| (y[0] - self.offset) / self.resistance))
    }
}

#[test]
fn rounded_tear_step_declines_and_restores_incoming_values() {
    let model = ParallelResistances::new(1e-10, 10.0, 1.0);
    let incoming = [10.0, 0.02, 0.08];
    let mut y = incoming;
    let result = tearing::project_torn_algebraic_block(
        &model,
        &mut y,
        &[],
        0.0,
        TornBlock {
            index: 0,
            tearing: model.plan.blocks.scalar_blocks()[0]
                .tearing
                .as_ref()
                .unwrap(),
        },
        1e-8,
        true,
    )
    .unwrap();
    assert!(
        result.is_none(),
        "stagnation accepted incorrect currents: {y:?}"
    );
    assert_eq!(y, incoming, "dense recovery must retain the incoming seed");
}

#[test]
fn certified_projection_preserves_current_balance_after_tear_stagnation() {
    for offset in [-10.0, 10.0] {
        for equation_scale in [1e-8, 1.0, 1e8] {
            let model = ParallelResistances::new(1e-10, offset, equation_scale);
            let mut y = [offset, 0.0, 0.0];
            project_algebraics_with_plan_certified(
                &model,
                &model.plan,
                &mut y,
                AlgebraicProjectionArgs {
                    parameters: &[],
                    time: 0.0,
                    state_count: 0,
                    tolerance: 1e-8,
                },
                ALGEBRAIC_PROJECTION_MAX_ITERS,
            )
            .unwrap();
            for current in &y[1..] {
                assert!((current - 0.05).abs() < 1e-8, "incorrect current: {y:?}");
            }
            assert!(
                (y[1] + y[2] - 0.1).abs() < 1e-10,
                "current imbalance: {y:?}"
            );
        }
    }
}

#[test]
fn well_conditioned_torn_block_still_settles_without_dense_recovery() {
    let model = ParallelResistances::new(2.0, 10.0, 1.0);
    let mut y = [10.0, 0.0, 0.0];
    let update = tearing::project_torn_algebraic_block(
        &model,
        &mut y,
        &[],
        0.0,
        TornBlock {
            index: 0,
            tearing: model.plan.blocks.scalar_blocks()[0]
                .tearing
                .as_ref()
                .unwrap(),
        },
        1e-8,
        true,
    )
    .unwrap()
    .expect("well-conditioned tearing should retain its reduced solve");
    assert!(update.settled);
    assert!((y[0] - 10.1).abs() < 1e-8);
    assert!(y[1..].iter().all(|current| (current - 0.05).abs() < 1e-8));
}

#[test]
fn certification_reuses_only_an_identical_sweep() {
    let model = ParallelResistances::new(2.0, 0.0, 1.0);
    let tearing = model.plan.blocks.scalar_blocks()[0]
        .tearing
        .as_ref()
        .unwrap();
    for (initial_current, expected_sweeps) in [(0.05, 1), (0.0, 2)] {
        model.sweeps.set(0);
        let mut y = [0.1, initial_current, initial_current];
        let update = tearing::project_torn_algebraic_block(
            &model,
            &mut y,
            &[],
            0.0,
            TornBlock { index: 0, tearing },
            1e-8,
            true,
        )
        .unwrap()
        .unwrap();
        assert!(update.settled);
        assert_eq!(y.map(f64::to_bits), [0.1_f64, 0.05, 0.05].map(f64::to_bits));
        assert_eq!(model.sweeps.get(), expected_sweeps);
    }
}
