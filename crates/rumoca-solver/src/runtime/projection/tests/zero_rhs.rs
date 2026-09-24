//! Exact-zero tangent handling for singular but consistent seed blocks.

use super::super::seed_linearization::{seed_dense_factorization_count, seed_torn_attempts};
use super::super::*;
use super::affine_elimination::source_programs;

struct SingularSeedModel {
    state_rhs: [f64; 2],
    second_y2: f64,
    plan: solve::AlgebraicProjectionPlan,
    structures: Option<solve::ContinuousStructuralArtifacts>,
}

impl SingularSeedModel {
    fn new(inconsistent_state_rhs: bool) -> Self {
        Self {
            state_rhs: [f64::from(inconsistent_state_rhs), 0.0],
            second_y2: 2.0,
            plan: solve::AlgebraicProjectionPlan {
                blocks: vec![
                    solve::AlgebraicProjectionBlock {
                        rows: vec![0, 1],
                        y_indices: vec![1, 2],
                        tearing: None,
                        guarded_tearing: None,
                        alternate_charts: Vec::new(),
                    },
                    solve::AlgebraicProjectionBlock {
                        rows: vec![2],
                        y_indices: vec![3],
                        tearing: None,
                        guarded_tearing: None,
                        alternate_charts: Vec::new(),
                    },
                ],
            },
            structures: None,
        }
    }

    fn issued(state_rhs: [f64; 2]) -> Self {
        Self::issued_with_second_y2(state_rhs, 2.0)
    }

    fn issued_with_second_y2(state_rhs: [f64; 2], second_y2: f64) -> Self {
        let mut model = Self::new(false);
        model.state_rhs = state_rhs;
        model.second_y2 = second_y2;
        model.plan.blocks[0].tearing = Some(solve::BlockTearing {
            tear_y_indices: vec![2],
            residual_rows: vec![1],
            causal_steps: vec![solve::CausalStep { row: 0, y_index: 1 }],
        });
        let span = rumoca_core::Span::from_offsets(
            rumoca_core::SourceId::from_source_name("singular_seed_issued.mo"),
            0,
            1,
        );
        let pattern = |rows, columns, dependencies: &[Vec<usize>]| {
            solve::StructuralPattern::from_row_dependencies(
                rows,
                columns,
                dependencies,
                solve::PatternProvenance::derived(
                    solve::PatternDerivation::DependencyPropagation,
                    span,
                )
                .unwrap(),
            )
            .unwrap()
        };
        let matrix = DMatrix::from_row_slice(
            3,
            4,
            &[
                state_rhs[0],
                1.0,
                1.0,
                0.0,
                state_rhs[1],
                2.0,
                second_y2,
                0.0,
                -1.0,
                -1.0,
                0.0,
                1.0,
            ],
        );
        let zeros = DVector::zeros(3);
        let primal = source_programs(&matrix, &zeros, false, span);
        let directional = source_programs(&matrix, &zeros, true, span);
        model.structures = Some(
            solve::ContinuousStructuralArtifacts::derived(
                None,
                vec![
                    pattern(2, 2, &[vec![0, 1], vec![0, 1]]),
                    pattern(1, 1, &[vec![0]]),
                ],
                vec![false, false],
                None,
                vec![],
                None,
            )
            .with_algebraic_output_evaluations(
                &model.plan,
                &primal,
                &directional,
                &directional,
            ),
        );
        assert!(
            model.structures.as_ref().unwrap().algebraic_projection()[0]
                .affine_elimination()
                .is_some()
        );
        model
    }
}

impl ImplicitProjectionModel for SingularSeedModel {
    fn eval_residual(
        &self,
        y: &[f64],
        _p: &[f64],
        _t: f64,
        out: &mut [f64],
    ) -> Result<(), RuntimeSolveError> {
        out[0] = y[1] + y[2] + self.state_rhs[0] * y[0];
        out[1] = 2.0 * y[1] + self.second_y2 * y[2] + self.state_rhs[1] * y[0];
        out[2] = y[3] - y[1] - y[0];
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
        out[0] = v[1] + v[2] + self.state_rhs[0] * v[0];
        out[1] = 2.0 * v[1] + self.second_y2 * v[2] + self.state_rhs[1] * v[0];
        out[2] = v[3] - v[1] - v[0];
        Ok(())
    }

    fn eval_implicit_jacobian_v_row(
        &self,
        row_idx: usize,
        _y: &[f64],
        _p: &[f64],
        _t: f64,
        v: &[f64],
    ) -> Result<Option<f64>, RuntimeSolveError> {
        Ok(Some(match row_idx {
            0 => v[1] + v[2] + self.state_rhs[0] * v[0],
            1 => 2.0 * v[1] + self.second_y2 * v[2] + self.state_rhs[1] * v[0],
            2 => v[3] - v[1] - v[0],
            _ => return Ok(None),
        }))
    }

    fn implicit_target(&self, row_idx: usize) -> Option<solve::ScalarSlot> {
        Some(solve::scalar_slot_y(match row_idx {
            0 | 1 => 1,
            2 => 3,
            _ => return None,
        }))
    }

    fn algebraic_projection_plan(&self) -> &solve::AlgebraicProjectionPlan {
        &self.plan
    }

    fn algebraic_projection_block_structure(
        &self,
        index: usize,
    ) -> Option<&solve::JacobianStructure> {
        self.structures.as_ref()?.algebraic_projection().get(index)
    }

    fn target_name_for_row(&self, row_idx: usize) -> Option<&str> {
        Some(match row_idx {
            0 | 1 => "gauge_enthalpy",
            2 => "temperature_alias",
            _ => return None,
        })
    }
}

#[test]
fn singular_consistent_zero_rhs_recovers_downstream_alias_through_seed_pipeline() {
    let model = SingularSeedModel::new(false);
    let mut seed = [1.0, 0.0, 0.0, 0.0];

    project_algebraic_seed_with_plan(
        &model,
        &model.plan,
        &[0.0; 4],
        AlgebraicProjectionArgs {
            parameters: &[],
            time: 0.0,
            state_count: 1,
            tolerance: 1.0e-12,
        },
        &mut seed,
    )
    .expect("an exact zero RHS has a zero particular tangent");

    assert_eq!(seed, [1.0, 0.0, 0.0, 1.0]);
}

#[test]
fn singular_inconsistent_rhs_remains_a_typed_directional_refusal() {
    let model = SingularSeedModel::new(true);
    let mut seed = [1.0, 0.0, 0.0, 0.0];

    let error = project_algebraic_seed_with_plan(
        &model,
        &model.plan,
        &[0.0; 4],
        AlgebraicProjectionArgs {
            parameters: &[],
            time: 0.0,
            state_count: 1,
            tolerance: 1.0e-12,
        },
        &mut seed,
    )
    .expect_err("a nonzero inconsistent RHS still requires a solvable block");

    assert!(matches!(
        error,
        RuntimeSolveError::DirectionalDerivativeUnavailable { .. }
    ));
    assert_eq!(seed, [1.0, 0.0, 0.0, 0.0]);
}

#[test]
fn issued_torn_singular_rhs_keeps_zero_success_and_nonzero_refusal() {
    let zero_model = SingularSeedModel::issued([0.0, 0.0]);
    let before_factor = seed_dense_factorization_count();
    let first = SeedBlockLinearization::build(
        &zero_model,
        0,
        &zero_model.plan.blocks[0],
        &[0.0; 4],
        AlgebraicProjectionArgs {
            parameters: &[],
            time: 0.0,
            state_count: 1,
            tolerance: 1e-12,
        },
    )
    .unwrap();
    let before_attempts = seed_torn_attempts().len();
    assert_eq!(
        first.solve(&DVector::zeros(2)).unwrap().as_slice(),
        &[0.0, 0.0]
    );
    assert_eq!(seed_dense_factorization_count(), before_factor);
    assert_eq!(seed_torn_attempts().len(), before_attempts);
    let mut seed = [1.0, 42.0, 43.0, 44.0];
    project_algebraic_seed_with_plan(
        &zero_model,
        &zero_model.plan,
        &[0.0; 4],
        AlgebraicProjectionArgs {
            parameters: &[],
            time: 0.0,
            state_count: 1,
            tolerance: 1e-12,
        },
        &mut seed,
    )
    .unwrap();
    assert_eq!(seed, [1.0, 0.0, 0.0, 1.0]);

    for state_rhs in [[1.0, 2.0], [1.0, 3.0]] {
        let model = SingularSeedModel::issued(state_rhs);
        let mut seed = [1.0, 42.0, 43.0, 44.0];
        let original = seed;
        let error = project_algebraic_seed_with_plan(
            &model,
            &model.plan,
            &[0.0; 4],
            AlgebraicProjectionArgs {
                parameters: &[],
                time: 0.0,
                state_count: 1,
                tolerance: 1e-12,
            },
            &mut seed,
        )
        .unwrap_err();
        assert!(matches!(
            error,
            RuntimeSolveError::DirectionalDerivativeUnavailable { .. }
        ));
        assert!(error.to_string().contains("sensitivity matrix is singular"));
        assert_eq!(seed, original);
    }
}

#[test]
fn issued_torn_seed_satisfies_original_selected_jvp_certificate() {
    let model = SingularSeedModel::issued_with_second_y2([1.0, 0.0], 3.0);
    let mut seed = [1.0, 99.0, 99.0, 99.0];
    let before_attempts = seed_torn_attempts().len();
    project_algebraic_seed_with_plan(
        &model,
        &model.plan,
        &[0.0; 4],
        AlgebraicProjectionArgs {
            parameters: &[],
            time: 0.0,
            state_count: 1,
            tolerance: 1e-12,
        },
        &mut seed,
    )
    .unwrap();
    assert_eq!(seed, [1.0, -3.0, 2.0, -2.0]);
    assert!(seed_torn_attempts()[before_attempts..].contains(&solve::TearingCandidate::Primary));
    let mut residual = [f64::NAN; 3];
    model
        .eval_jacobian_v(&[0.0; 4], &[], 0.0, &seed, &mut residual)
        .unwrap();
    assert_eq!(residual, [0.0; 3]);
}
