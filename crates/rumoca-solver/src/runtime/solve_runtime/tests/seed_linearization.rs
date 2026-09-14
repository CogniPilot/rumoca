//! Fixed-coordinate algebraic sensitivities share their matrix, never a seed.

use super::*;
use std::cell::Cell;

struct CountedJacobian {
    matrix_calls: Cell<usize>,
}

impl CompiledSolveJacobianExpression for CountedJacobian {
    fn call(
        &self,
        _: &[f64],
        _: &[f64],
        _: f64,
        _: &[f64],
        _: &[rumoca_core::ExternalTableData],
        _: &mut [f64],
    ) -> Result<(), String> {
        panic!("the fixture has one selected algebraic residual")
    }

    fn call_program_output(
        &self,
        coordinate: (usize, usize),
        y: &[f64],
        p: &[f64],
        t: f64,
        seed: &[f64],
        _: &[rumoca_core::ExternalTableData],
    ) -> Result<Option<f64>, String> {
        assert_eq!(coordinate, (1, 0));
        if seed[0] == 0.0 && seed[1] == 1.0 && seed.get(2).copied().unwrap_or(0.0) == 0.0 {
            self.matrix_calls.set(self.matrix_calls.get() + 1);
        }
        Ok(Some(
            2.0 * (p[0] + t) * y[1] * seed[1] - seed[0]
                + y[1] * y[1] * seed.get(2).copied().unwrap_or(0.0),
        ))
    }
}

fn primal_program() -> Vec<solve::LinearOp> {
    use solve::{BinaryOp as B, LinearOp as L};
    vec![
        L::LoadY { dst: 0, index: 1 },
        L::LoadY { dst: 1, index: 0 },
        L::LoadP { dst: 2, index: 0 },
        L::LoadTime { dst: 3 },
        L::Binary {
            dst: 4,
            op: B::Add,
            lhs: 2,
            rhs: 3,
        },
        L::Binary {
            dst: 5,
            op: B::Mul,
            lhs: 0,
            rhs: 0,
        },
        L::Binary {
            dst: 6,
            op: B::Mul,
            lhs: 4,
            rhs: 5,
        },
        L::Binary {
            dst: 7,
            op: B::Sub,
            lhs: 6,
            rhs: 1,
        },
        L::StoreOutputRange {
            start: 7,
            count: 1,
            stride: 1,
        },
    ]
}

fn solver_y_tangent_program() -> Vec<solve::LinearOp> {
    use solve::{BinaryOp as B, LinearOp as L};
    vec![
        L::LoadY { dst: 0, index: 1 },
        L::LoadP { dst: 1, index: 0 },
        L::LoadTime { dst: 2 },
        L::LoadSeed { dst: 3, index: 1 },
        L::LoadSeed { dst: 4, index: 0 },
        L::Const { dst: 5, value: 2.0 },
        L::Binary {
            dst: 6,
            op: B::Add,
            lhs: 1,
            rhs: 2,
        },
        L::Binary {
            dst: 7,
            op: B::Mul,
            lhs: 5,
            rhs: 6,
        },
        L::Binary {
            dst: 8,
            op: B::Mul,
            lhs: 7,
            rhs: 0,
        },
        L::Binary {
            dst: 9,
            op: B::Mul,
            lhs: 8,
            rhs: 3,
        },
        L::Binary {
            dst: 10,
            op: B::Sub,
            lhs: 9,
            rhs: 4,
        },
        L::StoreOutput { src: 10 },
    ]
}

fn model() -> solve::SolveModel {
    use solve::{BinaryOp as B, LinearOp as L};
    let mut model = warm_start_test_model();
    model.problem.solve_layout.parameter_count = 1;
    model.problem.solve_layout.compiled_parameter_len = 1;
    model.problem.layout = solve::VarLayout::from_parts(Default::default(), 2, 1);
    model.parameters = vec![1.0];
    model.problem.continuous.derivative_rhs =
        solve::ComputeBlock::from_scalar_program_block(spanned_block(
            vec![vec![
                L::LoadY { dst: 0, index: 1 },
                L::StoreOutput { src: 0 },
            ]],
            "seed_derivative.mo",
        ));
    model.artifacts.continuous.full_jacobian_v = spanned_block(
        vec![vec![
            L::LoadSeed { dst: 0, index: 1 },
            L::StoreOutput { src: 0 },
        ]],
        "seed_derivative_jvp.mo",
    );
    let primal = primal_program();
    model.problem.continuous.implicit_rhs =
        solve::ComputeBlock::from_scalar_program_block(spanned_block(
            vec![derivative_placeholder_row(0), primal],
            "seed_linearization.mo",
        ));
    let mut tangent = solver_y_tangent_program();
    let state_tangent = vec![L::LoadSeed { dst: 0, index: 0 }, L::StoreOutput { src: 0 }];
    model.artifacts.continuous.implicit_jacobian_v =
        solve::ComputeBlock::from_scalar_program_block(spanned_block(
            vec![state_tangent.clone(), tangent.clone()],
            "seed_linearization_y.mo",
        ));
    tangent.pop();
    tangent.extend([
        L::LoadSeed { dst: 11, index: 2 },
        L::Binary {
            dst: 12,
            op: B::Mul,
            lhs: 0,
            rhs: 0,
        },
        L::Binary {
            dst: 13,
            op: B::Mul,
            lhs: 12,
            rhs: 11,
        },
        L::Binary {
            dst: 14,
            op: B::Add,
            lhs: 10,
            rhs: 13,
        },
        L::StoreOutput { src: 14 },
    ]);
    model.artifacts.continuous.implicit_jacobian_v_scalar =
        spanned_block(vec![state_tangent, tangent], "seed_linearization_full.mo");
    derive_test_structural_artifacts(&mut model);
    model
}

#[test]
fn algebraic_seed_requests_reuse_one_fixed_coordinate_linearization() {
    let mut runtime = SolveRuntime::new_fixture(&model()).unwrap();
    let native = Rc::new(CountedJacobian {
        matrix_calls: Cell::new(0),
    });
    runtime.compiled_implicit_full_jacobian_v = Some(native.clone());
    for (sx, sp) in [(1.0, 0.0), (-3.0, 2.0), (5.0, -1.0)] {
        let mut seed = [sx, 99.0, sp];
        runtime
            .seed_refresh_with_plan(
                &runtime.algebraic_refresh,
                AlgebraicLinearization {
                    t: 0.0,
                    params: &[1.0],
                    settle: AlgebraicSettle {
                        tol: 1e-10,
                        max_iters: 4,
                    },
                },
                &[4.0, 2.0],
                &mut seed,
            )
            .unwrap();
        assert_eq!(seed, [sx, (sx - 4.0 * sp) / 4.0, sp]);
        assert_eq!(
            native.matrix_calls.get(),
            1,
            "the same matrix was rebuilt for another seed"
        );
    }
}

fn project(
    runtime: &SolveRuntime,
    y: &[f64],
    p: &[f64],
    time: f64,
    seed: &mut [f64],
) -> Result<(), RuntimeSolveError> {
    runtime.seed_refresh_with_plan(
        &runtime.algebraic_refresh,
        AlgebraicLinearization {
            t: time,
            params: p,
            settle: AlgebraicSettle {
                tol: 1e-10,
                max_iters: 4,
            },
        },
        y,
        seed,
    )
}

fn counted_runtime() -> (SolveRuntime, Rc<CountedJacobian>) {
    let mut runtime = SolveRuntime::new_fixture(&model()).unwrap();
    let native = Rc::new(CountedJacobian {
        matrix_calls: Cell::new(0),
    });
    runtime.compiled_implicit_full_jacobian_v = Some(native.clone());
    (runtime, native)
}

#[test]
fn seed_linearization_key_covers_every_y_parameter_and_time_bit() {
    let (runtime, native) = counted_runtime();
    let adjacent_x = f64::from_bits(4.0_f64.to_bits() + 1);
    for (index, (y, parameter, time)) in [
        ([4.0, 2.0], 1.0, 0.0),
        ([adjacent_x, 2.0], 1.0, 0.0),
        ([4.0, 3.0], 1.0, 0.0),
        ([4.0, 2.0], 2.0, 0.0),
        ([4.0, 2.0], 1.0, 1.0),
        ([4.0, 2.0], 1.0, 0.0),
        ([4.0, 2.0], 1.0, -0.0),
    ]
    .into_iter()
    .enumerate()
    {
        for sx in [1.0, -3.0] {
            let mut seed = [sx, 99.0, 0.5];
            project(&runtime, &y, &[parameter], time, &mut seed).unwrap();
            let expected = (sx - y[1] * y[1] * 0.5) / (2.0 * (parameter + time) * y[1]);
            assert!((seed[1] - expected).abs() < 1e-12);
            assert_eq!(native.matrix_calls.get(), index + 1);
        }
    }
}

#[test]
fn a_singular_coordinate_restores_seed_and_cannot_reuse_a_previous_matrix() {
    let (runtime, native) = counted_runtime();
    project(&runtime, &[4.0, 2.0], &[1.0], 0.0, &mut [1.0, 99.0, 0.0]).unwrap();
    for count in [2, 3] {
        let mut seed = [1.0, 99.0, 0.0];
        let error = project(&runtime, &[4.0, 2.0], &[1.0], -1.0, &mut seed).unwrap_err();
        assert!(error.to_string().contains("sensitivity matrix is singular"));
        assert_eq!(seed, [1.0, 99.0, 0.0]);
        assert_eq!(
            native.matrix_calls.get(),
            count,
            "failed projections retain no matrix"
        );
    }
    let mut seed = [1.0, 99.0, 0.0];
    project(&runtime, &[4.0, 2.0], &[1.0], 0.0, &mut seed).unwrap();
    assert_eq!(seed, [1.0, 0.25, 0.0]);
    assert_eq!(native.matrix_calls.get(), 4);
}

#[test]
fn seed_linearizations_without_construction_proofs_are_not_retained() {
    let (mut runtime, native) = counted_runtime();
    runtime.continuous_structural = solve::ContinuousStructuralArtifacts::default();
    for count in 1..=3 {
        let mut seed = [1.0, 99.0, 0.0];
        project(&runtime, &[4.0, 2.0], &[1.0], 0.0, &mut seed).unwrap();
        assert_eq!(seed, [1.0, 0.25, 0.0]);
        assert_eq!(native.matrix_calls.get(), count);
    }
}

#[test]
fn cloned_runtimes_start_with_independent_numerical_linearizations() {
    let (runtime, native) = counted_runtime();
    project(&runtime, &[4.0, 2.0], &[1.0], 0.0, &mut [1.0, 99.0, 0.0]).unwrap();
    let cloned = runtime.clone();
    project(&cloned, &[4.0, 2.0], &[1.0], 0.0, &mut [1.0, 99.0, 0.0]).unwrap();
    assert_eq!(native.matrix_calls.get(), 2);
    project(&runtime, &[4.0, 2.0], &[1.0], 0.0, &mut [1.0, 99.0, 0.0]).unwrap();
    assert_eq!(native.matrix_calls.get(), 2);
}
