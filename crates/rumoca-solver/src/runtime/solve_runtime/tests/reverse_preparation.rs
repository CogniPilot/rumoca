use super::*;

fn model() -> solve::SolveModel {
    let mut model = warm_start_test_model();
    model.problem.solve_layout.state_scalar_count = 2;
    model.problem.solve_layout.algebraic_scalar_count = 0;
    model.problem.continuous.algebraic_projection_plan = Default::default();
    model.problem.continuous.derivative_rhs =
        solve::ComputeBlock::from_scalar_program_block(spanned_block(
            vec![derivative_placeholder_row(0), derivative_placeholder_row(1)],
            "reverse_product.mo",
        ));
    model.artifacts.continuous.full_jacobian_v = spanned_block(
        vec![affine_seed_row(&[(0, 1.0)]), affine_seed_row(&[(1, 1.0)])],
        "reverse_product_jvp.mo",
    );
    derive_test_structural_artifacts(&mut model);
    model
}

#[test]
fn compiled_implicit_vjp_cannot_enter_the_reference_interpreter() {
    let runtime = SolveRuntime::new_fixture_with_backend(
        &model(),
        Rc::new(super::super::test_kernels::ReferenceBackend),
    )
    .unwrap();
    let mut out = [11.0, -0.0];
    let bits = out.map(f64::to_bits);
    assert!(matches!(
        runtime.reverse_implicit_residual_vjp(0.0, &[1.0, 2.0], &[], &[3.0, 4.0], &mut out),
        Err(RuntimeSolveError::CompiledExecution { .. })
    ));
    assert_eq!(out.map(f64::to_bits), bits);
}

#[test]
fn compiled_reverse_request_fails_at_product_preparation() {
    let runtime = SolveRuntime::new_fixture_with_backend(
        &model(),
        Rc::new(super::super::test_kernels::ReferenceBackend),
    )
    .unwrap();
    let error = runtime
        .prepare_reverse_sensitivity()
        .err()
        .expect("a compiled request cannot construct a reference product");
    assert!(matches!(
        error,
        RuntimeSolveError::CompiledExecution {
            context: "reverse sensitivity preparation",
            ..
        }
    ));
}

#[test]
fn compiled_state_vjp_cannot_enter_the_reference_interpreter() {
    let runtime = SolveRuntime::new_fixture_with_backend(
        &model(),
        Rc::new(super::super::test_kernels::ReferenceBackend),
    )
    .unwrap();
    let mut out = [11.0, -0.0];
    let bits = out.map(f64::to_bits);
    let lin = AlgebraicLinearization {
        t: 0.0,
        params: &[],
        settle: AlgebraicSettle {
            tol: 1e-10,
            max_iters: 4,
        },
    };
    assert!(matches!(
        runtime.reverse_state_derivative_vjp(lin, &[1.0, 2.0], &[3.0, 4.0], &mut out),
        Err(RuntimeSolveError::CompiledExecution { .. })
    ));
    assert_eq!(out.map(f64::to_bits), bits);
}

#[test]
fn compiled_steady_transpose_cannot_enter_the_reference_interpreter() {
    let runtime = SolveRuntime::new_fixture_with_backend(
        &model(),
        Rc::new(super::super::test_kernels::ReferenceBackend),
    )
    .unwrap();
    let mut out = [11.0, -0.0];
    let bits = out.map(f64::to_bits);
    assert!(matches!(
        runtime.apply_steady_residual_transpose(0.0, &[1.0, 2.0], &[], &[3.0, 4.0], &mut out),
        Err(RuntimeSolveError::CompiledExecution { .. })
    ));
    assert_eq!(out.map(f64::to_bits), bits);
}

#[test]
fn explicit_reference_reverse_products_preserve_their_results() {
    let runtime = SolveRuntime::new_fixture(&model()).unwrap();
    let reverse = runtime.prepare_reverse_sensitivity().unwrap();
    let mut out = [11.0, -0.0];
    reverse
        .reverse_implicit_residual_vjp(0.0, &[1.0, 2.0], &[], &[3.0, 4.0], &mut out)
        .unwrap();
    assert_eq!(out, [3.0, 4.0]);
    reverse
        .apply_steady_residual_transpose(0.0, &[1.0, 2.0], &[], &[3.0, 4.0], &mut out)
        .unwrap();
    assert_eq!(out, [3.0, 4.0]);
    let lin = AlgebraicLinearization {
        t: 0.0,
        params: &[],
        settle: AlgebraicSettle {
            tol: 1e-10,
            max_iters: 4,
        },
    };
    reverse
        .reverse_state_derivative_vjp(lin, &[1.0, 2.0], &[3.0, 4.0], &mut out)
        .unwrap();
    assert_eq!(out, [3.0, 4.0]);
}
