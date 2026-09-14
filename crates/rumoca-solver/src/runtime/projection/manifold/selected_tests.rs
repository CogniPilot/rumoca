use super::*;
use rumoca_eval_solve::{JacobianEvalInputs, PreparedScalarProgramBlock, RowEvalContext};
use std::cell::{Cell, RefCell};

struct Model {
    plan: solve::AlgebraicProjectionPlan,
    structure: solve::JacobianStructure,
    directional: PreparedScalarProgramBlock,
    programs: RefCell<Vec<usize>>,
    full_calls: Cell<usize>,
    decline: bool,
    fail: bool,
}

fn program() -> solve::ScalarProgramBlock {
    use solve::{BinaryOp as B, LinearOp as L};
    let shared = vec![
        L::LoadSeed { dst: 0, index: 4 },
        L::LoadSeed { dst: 1, index: 1 },
        L::LoadP { dst: 2, index: 0 },
        L::LoadTime { dst: 3 },
        L::Binary {
            dst: 4,
            op: B::Mul,
            lhs: 2,
            rhs: 0,
        },
        L::Binary {
            dst: 5,
            op: B::Mul,
            lhs: 3,
            rhs: 1,
        },
        L::Binary {
            dst: 6,
            op: B::Add,
            lhs: 2,
            rhs: 3,
        },
        L::Binary {
            dst: 7,
            op: B::Add,
            lhs: 4,
            rhs: 5,
        },
        L::Binary {
            dst: 8,
            op: B::Mul,
            lhs: 6,
            rhs: 1,
        },
        L::StoreOutputRange {
            start: 7,
            count: 2,
            stride: 1,
        },
    ];
    let singleton = vec![
        L::LoadSeed { dst: 0, index: 3 },
        L::LoadP { dst: 1, index: 0 },
        L::LoadTime { dst: 2 },
        L::Binary {
            dst: 3,
            op: B::Add,
            lhs: 1,
            rhs: 1,
        },
        L::Binary {
            dst: 4,
            op: B::Sub,
            lhs: 3,
            rhs: 2,
        },
        L::Binary {
            dst: 5,
            op: B::Mul,
            lhs: 4,
            rhs: 0,
        },
        L::StoreOutput { src: 5 },
    ];
    let span = rumoca_core::Span::from_offsets(
        rumoca_core::SourceId::from_source_name("selected_manifold.mo"),
        1,
        2,
    );
    solve::ScalarProgramBlock::with_output_indices(
        vec![shared, singleton],
        vec![span; 2],
        vec![2, 0, 1],
    )
    .unwrap()
}

impl Model {
    fn new() -> Self {
        let block = program();
        let mut model = solve::SolveModel::default();
        model.problem.solve_layout.state_scalar_count = 5;
        model.problem.solve_layout.solver_maps.names =
            (0..5).map(|index| format!("x{index}")).collect();
        model.problem.layout = solve::VarLayout::from_parts(Default::default(), 5, 1);
        model.initial_y = vec![0.0; 5];
        model.parameters = vec![2.0];
        model.problem.continuous.manifold_residual =
            solve::ComputeBlock::from_scalar_program_block(block.clone());
        model.artifacts.continuous.manifold_jacobian_v =
            solve::ComputeBlock::from_scalar_program_block(block.clone());
        let plan = solve::AlgebraicProjectionPlan {
            blocks: vec![solve::AlgebraicProjectionBlock {
                rows: vec![2, 1, 0],
                y_indices: vec![4, 3, 1],
                tearing: None,
            }],
        };
        model.problem.continuous.manifold_projection_plan = plan.clone();
        let (artifacts, _) =
            rumoca_eval_solve::derive_solve_structural_artifacts(&model.problem, &model.artifacts)
                .unwrap();
        Self {
            plan,
            structure: artifacts.manifold_projection()[0].clone(),
            directional: PreparedScalarProgramBlock::new(block).unwrap(),
            programs: RefCell::default(),
            full_calls: Cell::default(),
            decline: false,
            fail: false,
        }
    }

    fn jacobian(&self, k: f64, t: f64) -> Result<DMatrix<f64>, RuntimeSolveError> {
        manifold_block_jacobian(self, &[0.0; 5], &[k], t, &self.plan.blocks[0], 0)
    }
}

impl ManifoldProjectionModel for Model {
    fn eval_manifold_residual(
        &self,
        _: &[f64],
        _: &[f64],
        _: f64,
        _: &mut [f64],
    ) -> Result<(), RuntimeSolveError> {
        unreachable!("this fixture evaluates the derivative matrix only")
    }

    fn eval_manifold_jacobian_v(
        &self,
        y: &[f64],
        p: &[f64],
        t: f64,
        seed: &[f64],
        out: &mut [f64],
    ) -> Result<(), RuntimeSolveError> {
        self.full_calls.set(self.full_calls.get() + 1);
        self.directional
            .eval_with_context(
                y,
                p,
                t,
                RowEvalContext {
                    seed: Some(seed),
                    ..Default::default()
                },
                out,
            )
            .map_err(Into::into)
    }

    fn eval_manifold_jacobian_outputs(
        &self,
        selection: &solve::JacobianOutputSelection,
        inputs: JacobianEvalInputs<'_>,
        out: &mut [f64],
    ) -> Result<bool, RuntimeSolveError> {
        if self.fail {
            return Err(RuntimeSolveError::solve_ir("selected manifold failed"));
        }
        if self.decline {
            return Ok(false);
        }
        out.fill(0.0);
        let mut values = Vec::new();
        for program in selection.programs() {
            self.programs.borrow_mut().push(program.program());
            self.directional.eval_row_outputs_unchecked_with_context(
                program.program(),
                inputs.y,
                inputs.p,
                inputs.t,
                RowEvalContext {
                    seed: Some(inputs.seed),
                    ..Default::default()
                },
                &mut values,
            )?;
            for &(offset, row) in program.placements() {
                out[row] = values[offset];
            }
        }
        Ok(true)
    }

    fn manifold_residual_len(&self) -> usize {
        3
    }
    fn manifold_projection_plan(&self) -> &solve::AlgebraicProjectionPlan {
        &self.plan
    }
    fn manifold_projection_block_structure(&self, _: usize) -> Option<&solve::JacobianStructure> {
        Some(&self.structure)
    }
}

#[test]
fn selected_tensor_outputs_preserve_colors_placements_and_changing_coefficients() {
    for decline in [false, true] {
        let mut model = Model::new();
        model.decline = decline;
        assert_eq!(model.structure.coloring().groups().len(), 2);
        for (k, t) in [(2.0, 0.5), (-3.0, 2.0), (0.0, -1.0)] {
            model.programs.borrow_mut().clear();
            model.full_calls.set(0);
            let expected =
                DMatrix::from_row_slice(3, 3, &[k, 0.0, t, 0.0, 2.0 * k - t, 0.0, 0.0, 0.0, k + t]);
            assert_eq!(model.jacobian(k, t).unwrap(), expected);
            assert_eq!(model.full_calls.get(), if decline { 3 } else { 0 });
            if !decline {
                assert_eq!(&*model.programs.borrow(), &[0, 1, 0]);
            }
        }
    }
}

#[test]
fn selected_failure_propagates_without_a_full_retry() {
    let mut model = Model::new();
    model.fail = true;
    assert!(
        model
            .jacobian(2.0, 0.5)
            .unwrap_err()
            .to_string()
            .contains("selected manifold failed")
    );
    assert_eq!(model.full_calls.get(), 0);
}
