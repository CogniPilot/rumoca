use super::*;

struct NominalScaledProjectionModel {
    plan: solve::AlgebraicProjectionPlan,
    coefficient: f64,
    rhs: f64,
    variable_scale: f64,
}

impl ImplicitProjectionModel for NominalScaledProjectionModel {
    fn eval_residual(
        &self,
        y: &[f64],
        _p: &[f64],
        _t: f64,
        out: &mut [f64],
    ) -> Result<(), RuntimeSolveError> {
        out[0] = self.coefficient * y[0] - self.rhs;
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
        out[0] = self.coefficient * v[0];
        Ok(())
    }

    fn implicit_target(&self, row_idx: usize) -> Option<solve::ScalarSlot> {
        Some(solve::scalar_slot_y(row_idx))
    }

    fn algebraic_projection_plan(&self) -> &solve::AlgebraicProjectionPlan {
        &self.plan
    }

    fn target_name_for_row(&self, _row_idx: usize) -> Option<&str> {
        Some("small")
    }

    fn variable_scale_for_y_index(&self, _y_index: usize) -> f64 {
        self.variable_scale
    }
}

#[test]
fn nominal_scaled_projection_corrects_small_physical_residual() {
    let model = NominalScaledProjectionModel {
        plan: solve::AlgebraicProjectionPlan {
            blocks: vec![solve::AlgebraicProjectionBlock {
                rows: vec![0],
                y_indices: vec![0],
            }],
        },
        coefficient: 1.0,
        rhs: 0.0,
        variable_scale: 1.0e-9,
    };
    let mut y = vec![5.0e-13];

    project_algebraics(&model, &mut y, &[], 0.0, 0, 1.0e-6)
        .expect("nominal-scaled tolerance should require the small residual correction");

    assert!(
        y[0].abs() <= 1.0e-15,
        "scaled residual was not corrected: {y:?}"
    );
}

#[test]
fn projection_row_scale_includes_jacobian_coefficient() {
    let model = NominalScaledProjectionModel {
        plan: solve::AlgebraicProjectionPlan {
            blocks: vec![solve::AlgebraicProjectionBlock {
                rows: vec![0],
                y_indices: vec![0],
            }],
        },
        coefficient: 1.0e-12,
        rhs: 1.0e-3,
        variable_scale: 1.0e12,
    };
    let mut y = vec![0.0];

    project_algebraics(&model, &mut y, &[], 0.0, 0, 1.0e-6)
        .expect("Jacobian-scaled residual should not accept the unprojected initial value");

    assert!(
        (y[0] - 1.0e9).abs() <= 1.0e-6,
        "unexpected projection: {y:?}"
    );
}

#[test]
fn scaled_newton_system_normalizes_mixed_magnitude_columns() {
    let jacobian = DMatrix::from_diagonal(&DVector::from_vec(vec![1.0e-12, 1.0e12]));
    let delta = scaled_newton_delta(
        &jacobian,
        &[-1.0, -1.0],
        &[1.0, 1.0],
        &[1.0e12, 1.0e-12],
        None,
        1.0e-12,
    )
    .expect("scaled diagonal system should solve");

    assert!((delta[0] - 1.0e12).abs() <= 1.0e-4);
    assert!((delta[1] - 1.0e-12).abs() <= f64::EPSILON);
}

#[test]
fn sparse_newton_system_matches_dense_solution() {
    let dimension = 32;
    let jacobian = DMatrix::from_fn(dimension, dimension, |row, column| {
        if row == column {
            4.0
        } else if row.abs_diff(column) == 1 {
            -1.0
        } else {
            0.0
        }
    });
    let rhs = DVector::from_element(dimension, 1.0);
    let dependencies = (0..dimension)
        .map(|row| {
            (0..dimension)
                .filter(|column| row.abs_diff(*column) <= 1)
                .collect::<Vec<_>>()
        })
        .collect::<Vec<_>>();
    let provenance = solve::PatternProvenance::derived(
        solve::PatternDerivation::DependencyPropagation,
        rumoca_core::Span::from_offsets(
            rumoca_core::SourceId::from_source_name("SparseNewton.mo"),
            0,
            1,
        ),
    )
    .expect("fixture provenance");
    let structure = solve::StructuralPattern::from_row_dependencies(
        dimension,
        dimension,
        &dependencies,
        provenance,
    )
    .expect("fixture structure");
    let sparse = scaling::sparse_newton_delta(&jacobian, &rhs, &structure)
        .expect("sparse system should solve");
    let dense = jacobian
        .lu()
        .solve(&rhs)
        .expect("dense system should solve");

    assert!((&sparse - dense).amax() <= 1.0e-12);
}

/// `eps = 100*Modelica.Constants.eps` feeding a guarded division, the shape
/// `Modelica.Magnetic.FluxTubes.Basic.ElectroMagneticConverter` uses for
/// `L_stat = noEvent(if abs(i) > eps then abs(Psi/i) else abs(Psi/eps))`.
///
/// Row 0 assigns the tiny constant; its residual at the `0.0` seed is already
/// far below any usable tolerance, so a "must strictly improve the residual"
/// rule rejects the assignment and leaves the divisor at zero. Row 1 then
/// evaluates `0/0`.
struct TinyConstantDivisorInitialModel {
    divisor: f64,
    plan: solve::AlgebraicProjectionPlan,
}

impl TinyConstantDivisorInitialModel {
    fn residual_row(&self, row_idx: usize, y: &[f64]) -> Option<f64> {
        match row_idx {
            0 => Some(y[0] - self.divisor),
            1 => Some(y[1] - 0.0 / y[0]),
            _ => None,
        }
    }
}

impl ImplicitProjectionModel for TinyConstantDivisorInitialModel {
    fn eval_residual(
        &self,
        y: &[f64],
        _p: &[f64],
        _t: f64,
        out: &mut [f64],
    ) -> Result<(), RuntimeSolveError> {
        for (row, slot) in out.iter_mut().enumerate() {
            *slot = self.residual_row(row, y).unwrap_or(0.0);
        }
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
        out.copy_from_slice(v);
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

impl AlgebraicProjectionModel for TinyConstantDivisorInitialModel {
    fn eval_initial_residual(
        &self,
        y: &[f64],
        p: &[f64],
        t: f64,
        out: &mut [f64],
    ) -> Result<(), RuntimeSolveError> {
        self.eval_residual(y, p, t, out)
    }

    fn eval_initial_jacobian_v(
        &self,
        y: &[f64],
        p: &[f64],
        t: f64,
        v: &[f64],
        out: &mut [f64],
    ) -> Result<(), RuntimeSolveError> {
        self.eval_jacobian_v(y, p, t, v, out)
    }

    fn initial_residual_len(&self) -> usize {
        2
    }

    fn initial_target(&self, row_idx: usize) -> Option<solve::ScalarSlot> {
        Some(solve::scalar_slot_y(row_idx))
    }

    fn eval_initial_target_value(
        &self,
        row_idx: usize,
        _target_y_index: usize,
        y: &[f64],
        _p: &[f64],
        _t: f64,
    ) -> Result<Option<f64>, RuntimeSolveError> {
        Ok(match row_idx {
            0 => Some(self.divisor),
            1 => Some(0.0 / y[0]),
            _ => None,
        })
    }

    fn eval_initial_residual_row(
        &self,
        row_idx: usize,
        y: &[f64],
        _p: &[f64],
        _t: f64,
    ) -> Result<Option<f64>, RuntimeSolveError> {
        Ok(self.residual_row(row_idx, y))
    }
}

#[test]
fn initial_singleton_assignment_writes_sub_tolerance_constant_divisor() {
    let model = TinyConstantDivisorInitialModel {
        divisor: 2.220_446_049_250_313e-14,
        plan: solve::AlgebraicProjectionPlan {
            blocks: vec![
                solve::AlgebraicProjectionBlock {
                    rows: vec![0],
                    y_indices: vec![0],
                },
                solve::AlgebraicProjectionBlock {
                    rows: vec![1],
                    y_indices: vec![1],
                },
            ],
        },
    };
    let mut y = vec![0.0, 0.0];

    project_initial_y_plan(&model, &mut y, &[], 0.0, &model.plan, 1.0e-6)
        .expect("a sub-tolerance constant assignment must still be written");

    assert!(
        (y[0] - model.divisor).abs() <= f64::EPSILON * model.divisor,
        "divisor left at its seed: {y:?}"
    );
    assert!(
        y[1].is_finite(),
        "guarded quotient stayed non-finite: {y:?}"
    );
}
