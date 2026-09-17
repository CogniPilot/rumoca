use std::cell::RefCell;

use rumoca_core::{SourceId, Span};

use super::*;

struct ColoredRows {
    plan: solve::AlgebraicProjectionPlan,
    structure: solve::JacobianStructure,
    selected_calls: RefCell<Vec<usize>>,
    full_calls: Cell<usize>,
    missing_row: Option<usize>,
    failing_row: Option<usize>,
    reverse_row: Option<usize>,
}

impl ColoredRows {
    fn new() -> Self {
        let provenance = solve::PatternProvenance::derived(
            solve::PatternDerivation::DependencyPropagation,
            Span::from_offsets(SourceId::from_source_name("colored_rows.mo"), 1, 2),
        )
        .unwrap();
        let pattern = solve::StructuralPattern::from_row_dependencies(
            3,
            3,
            &[vec![0, 1], vec![2], vec![1]],
            provenance,
        )
        .unwrap();
        Self {
            plan: solve::AlgebraicProjectionPlan {
                blocks: vec![solve::AlgebraicProjectionBlock {
                    rows: vec![4, 1, 3],
                    y_indices: vec![2, 0, 1],
                    tearing: None,
                    alternate_charts: Vec::new(),
                }],
            },
            structure: solve::JacobianStructure::derived(pattern),
            selected_calls: RefCell::new(Vec::new()),
            full_calls: Cell::new(0),
            missing_row: None,
            failing_row: None,
            reverse_row: None,
        }
    }

    fn jvp(row: usize, seed: &[f64]) -> f64 {
        match row {
            4 => 2.0 * seed[2] + 3.0 * seed[0] + 11.0 * seed[3],
            1 => 5.0 * seed[1] + 13.0 * seed[4],
            3 => 7.0 * seed[0],
            _ => panic!("unrelated row {row} must not be selected"),
        }
    }

    fn jacobian(&self) -> Result<DMatrix<f64>, RuntimeSolveError> {
        let block = &self.plan.blocks[0];
        initial::algebraic_block_jacobian(
            self,
            &[0.0; 5],
            &[11.0],
            0.0,
            &block.rows,
            &block.y_indices,
            Some(&self.structure),
        )
    }
}

impl ImplicitProjectionModel for ColoredRows {
    fn eval_residual(
        &self,
        _y: &[f64],
        _p: &[f64],
        _t: f64,
        _out: &mut [f64],
    ) -> Result<(), RuntimeSolveError> {
        panic!("Jacobian construction must not evaluate primal residuals")
    }

    fn eval_jacobian_v(
        &self,
        _y: &[f64],
        _p: &[f64],
        _t: f64,
        seed: &[f64],
        out: &mut [f64],
    ) -> Result<(), RuntimeSolveError> {
        self.full_calls.set(self.full_calls.get() + 1);
        out.fill(f64::NAN);
        for row in [4, 1, 3] {
            out[row] = Self::jvp(row, seed);
        }
        Ok(())
    }

    fn eval_implicit_jacobian_v_row(
        &self,
        row: usize,
        _y: &[f64],
        _p: &[f64],
        _t: f64,
        seed: &[f64],
    ) -> Result<Option<f64>, RuntimeSolveError> {
        assert_eq!(seed.len(), 6);
        assert_eq!(seed[5], 0.0, "projection must leave parameter seeds zero");
        self.selected_calls.borrow_mut().push(row);
        if self.failing_row == Some(row) {
            return Err(RuntimeSolveError::solve_ir("selected row failed"));
        }
        Ok((self.missing_row != Some(row)).then(|| Self::jvp(row, seed)))
    }

    fn eval_implicit_jacobian_row(
        &self,
        row: usize,
        _y: &[f64],
        _p: &[f64],
        _t: f64,
        gradient: &mut [f64],
    ) -> Result<bool, RuntimeSolveError> {
        if self.reverse_row != Some(row) {
            return Ok(false);
        }
        assert_eq!(row, 3);
        gradient.fill(0.0);
        gradient[0] = 7.0;
        Ok(true)
    }

    fn implicit_target(&self, _row: usize) -> Option<solve::ScalarSlot> {
        None
    }

    fn algebraic_projection_plan(&self) -> &solve::AlgebraicProjectionPlan {
        &self.plan
    }

    fn target_name_for_row(&self, _row: usize) -> Option<&str> {
        None
    }

    fn algebraic_projection_block_structure(
        &self,
        _index: usize,
    ) -> Option<&solve::JacobianStructure> {
        Some(&self.structure)
    }
}

fn expected_jacobian() -> DMatrix<f64> {
    DMatrix::from_row_slice(3, 3, &[2.0, 3.0, 0.0, 0.0, 0.0, 5.0, 0.0, 7.0, 0.0])
}

#[test]
fn colored_row_scaling_uses_only_selected_rows_with_nonidentity_layout() {
    let model = ColoredRows::new();
    assert_eq!(model.structure.coloring().groups().len(), 2);
    let scales = algebraic_plan_row_scales(&model, &[0.0; 5], &[11.0], 0.0, &model.plan).unwrap();
    assert_eq!(scales, vec![3.0, 5.0, 7.0]);
    assert_eq!(
        model.full_calls.get(),
        0,
        "available row projections must be used"
    );
    let mut calls = model.selected_calls.borrow().clone();
    calls.sort_unstable();
    assert_eq!(calls, vec![1, 3, 4, 4]);
}

#[test]
fn colored_rows_preserve_local_matrix_placement_and_reverse_rows() {
    let mut model = ColoredRows::new();
    model.reverse_row = Some(3);
    assert_eq!(model.jacobian().unwrap(), expected_jacobian());
    assert_eq!(model.full_calls.get(), 0);
    let mut calls = model.selected_calls.borrow().clone();
    calls.sort_unstable();
    assert_eq!(calls, vec![1, 4, 4]);
}

#[test]
fn unavailable_colored_row_uses_one_full_evaluation_for_its_color() {
    let mut model = ColoredRows::new();
    model.missing_row = Some(1);
    assert_eq!(model.jacobian().unwrap(), expected_jacobian());
    assert_eq!(model.full_calls.get(), 1);
}

#[test]
fn colored_row_error_propagates_without_full_evaluation() {
    let mut model = ColoredRows::new();
    model.failing_row = Some(1);
    let error = model
        .jacobian()
        .expect_err("a failing selected row is not unavailable");
    assert!(error.to_string().contains("selected row failed"));
    assert_eq!(model.full_calls.get(), 0);
}

#[test]
fn seed_projection_rejects_mismatched_structure_and_restores_seed() {
    let mut model = ColoredRows::new();
    model.plan.blocks[0].rows = vec![4, 1];
    model.plan.blocks[0].y_indices = vec![2, 1];
    let mut seed = [0.0, 8.0, 9.0, 0.0, 0.0, 0.0];
    let original = seed;
    let error = project_algebraic_seed_with_plan(
        &model,
        &model.plan,
        &[0.0; 5],
        AlgebraicProjectionArgs {
            parameters: &[11.0],
            time: 0.0,
            state_count: 0,
            tolerance: 1.0e-12,
        },
        &mut seed,
    )
    .expect_err("a checked structure from another block must be rejected");
    assert!(
        error.to_string().contains("structure is 3x3, expected 2x2"),
        "{error}"
    );
    assert_eq!(seed, original, "failed projection restores unknown seeds");
}

#[test]
fn seed_projection_uses_coloring_and_preserves_known_directions() {
    let model = ColoredRows::new();
    let mut seed = [9.0, 8.0, 7.0, 2.0, 3.0, 0.0];
    project_algebraic_seed_with_plan(
        &model,
        &model.plan,
        &[0.0; 5],
        AlgebraicProjectionArgs {
            parameters: &[11.0],
            time: 0.0,
            state_count: 0,
            tolerance: 1.0e-12,
        },
        &mut seed,
    )
    .unwrap();
    assert_eq!(seed, [0.0, -7.8, -11.0, 2.0, 3.0, 0.0]);
    assert_eq!(model.full_calls.get(), 0);
    assert_eq!(
        model.selected_calls.borrow().len(),
        10,
        "three RHS rows, four colored entries, and three certification rows"
    );
}
