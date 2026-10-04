use super::*;

/// An open tank in miniature over `[m, H]`: row 0 is the level row `m - 2`,
/// row 1 the temperature row `H/m - 3`. Evaluating the complete residual
/// reconstructs the enthalpy `H/m` and fails while `m = 0`, as the settled
/// view does at a startless seed; evaluating only row 0 never reads it.
struct OpenTankRowsModel {
    plan: solve::AlgebraicProjectionPlan,
    complete_reads: Cell<usize>,
}

impl OpenTankRowsModel {
    fn new() -> Self {
        Self {
            plan: solve::AlgebraicProjectionPlan {
                blocks: vec![
                    solve::AlgebraicProjectionBlock {
                        rows: vec![0],
                        y_indices: vec![0],
                        ..Default::default()
                    },
                    solve::AlgebraicProjectionBlock {
                        rows: vec![1],
                        y_indices: vec![1],
                        ..Default::default()
                    },
                ],
            },
            complete_reads: Cell::new(0),
        }
    }

    fn rows(
        &self,
        y: &[f64],
        rows: Option<&[usize]>,
        out: &mut [f64],
    ) -> Result<(), RuntimeSolveError> {
        let reads_enthalpy = rows.is_none_or(|rows| rows.contains(&1));
        if rows.is_none() {
            self.complete_reads.set(self.complete_reads.get() + 1);
        }
        out[0] = y[0] - 2.0;
        if reads_enthalpy {
            if y[0] == 0.0 {
                return Err(RuntimeSolveError::NonFiniteValue {
                    name: "h".to_string(),
                    kind: "NaN",
                    span: None,
                });
            }
            out[1] = y[1] / y[0] - 3.0;
        }
        Ok(())
    }
}

impl ImplicitProjectionModel for OpenTankRowsModel {
    fn eval_residual(
        &self,
        y: &[f64],
        _p: &[f64],
        _t: f64,
        out: &mut [f64],
    ) -> Result<(), RuntimeSolveError> {
        self.rows(y, None, out)
    }

    fn eval_jacobian_v(
        &self,
        y: &[f64],
        _p: &[f64],
        _t: f64,
        v: &[f64],
        out: &mut [f64],
    ) -> Result<(), RuntimeSolveError> {
        out[0] = v[0];
        out[1] = v[1] / y[0] - y[1] * v[0] / (y[0] * y[0]);
        Ok(())
    }

    fn implicit_target(&self, row_idx: usize) -> Option<solve::ScalarSlot> {
        Some(solve::scalar_slot_y(row_idx))
    }

    fn algebraic_projection_plan(&self) -> &solve::AlgebraicProjectionPlan {
        &self.plan
    }

    fn target_name_for_row(&self, _row_idx: usize) -> Option<&str> {
        None
    }
}

impl AlgebraicProjectionModel for OpenTankRowsModel {
    fn eval_initial_residual(
        &self,
        y: &[f64],
        _p: &[f64],
        _t: f64,
        rows: Option<&[usize]>,
        out: &mut [f64],
    ) -> Result<(), RuntimeSolveError> {
        self.rows(y, rows, out)
    }

    fn initial_residual_len(&self) -> usize {
        2
    }

    fn initial_target(&self, row_idx: usize) -> Option<solve::ScalarSlot> {
        Some(solve::scalar_slot_y(row_idx))
    }

    fn eval_initial_jacobian_v(
        &self,
        y: &[f64],
        _p: &[f64],
        _t: f64,
        v: &[f64],
        rows: Option<&[usize]>,
        out: &mut [f64],
    ) -> Result<(), RuntimeSolveError> {
        out[0] = v[0];
        if rows.is_none_or(|rows| rows.contains(&1)) {
            out[1] = v[1] / y[0] - y[1] * v[0] / (y[0] * y[0]);
        }
        Ok(())
    }
}

#[test]
fn the_first_pass_evaluates_each_block_on_its_own_rows_only() {
    let model = OpenTankRowsModel::new();
    let plan = model.plan.clone();
    let mut y = [0.0, 0.0];
    project_initial_variables_by_plan(&model, &mut y, &[], 0.0, &plan, 1e-10)
        .expect("the level block projects the mass before the enthalpy is read");
    assert!((y[0] - 2.0).abs() <= 1e-12, "mass {}", y[0]);
    assert!((y[1] - 6.0).abs() <= 1e-9, "enthalpy {}", y[1]);
    assert!(
        model.complete_reads.get() >= 1,
        "the complete residual certifies the projected point"
    );
}

#[test]
fn a_plan_without_blocks_reads_the_complete_residual_at_the_seed() {
    // With no projection block, nothing moves the mass off zero, so the
    // complete residual is read at the seed and reports the undefined enthalpy.
    let model = OpenTankRowsModel::new();
    let mut y = [0.0, 0.0];
    let empty = solve::AlgebraicProjectionPlan::default();
    let error = project_initial_variables_by_plan(&model, &mut y, &[], 0.0, &empty, 1e-10)
        .expect_err("an empty plan reads the complete residual at the seed");
    assert!(matches!(error, RuntimeSolveError::NonFiniteValue { .. }));
}

/// The continuous view of the fixture is the same two rows the initialization
/// reads: the complete residual reconstructs the enthalpy, and the Jacobian
/// is that of `[m - 2, H/m - 3]`.
#[test]
fn the_open_tank_fixture_states_one_continuous_system() {
    let model = OpenTankRowsModel::new();
    let mut out = [0.0; 2];
    model
        .eval_residual(&[4.0, 8.0], &[], 0.0, &mut out)
        .unwrap();
    assert_eq!(out, [2.0, -1.0]);
    assert_eq!(model.complete_reads.get(), 1);
    model
        .eval_jacobian_v(&[4.0, 8.0], &[], 0.0, &[1.0, 1.0], &mut out)
        .unwrap();
    assert_eq!(out, [1.0, 0.25 - 0.5]);
    assert_eq!(model.implicit_target(1), Some(solve::scalar_slot_y(1)));
    assert_eq!(model.algebraic_projection_plan().blocks.len(), 2);
    assert_eq!(model.target_name_for_row(0), None);
}
