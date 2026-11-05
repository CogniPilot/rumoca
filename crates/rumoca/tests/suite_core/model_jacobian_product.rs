//! Actual Modelica -> Solve -> shared Jacobian -> native product correspondence.
mod order;
mod structural;

use rumoca::Compiler;
use rumoca_exec_cranelift::CompiledModelStateJacobian;
use rumoca_solver::{AlgebraicSettle, SolveRuntime};

const SOURCE: &str = r#"
model DenseProduct
  parameter Real a = 1.25;
  Real x[2](each start = 0.25);
equation
  der(x[1]) = a*x[1]^3 + sin(x[2]) + time;
  der(x[2]) = x[1]*x[2];
end DenseProduct;
"#;

#[test]
fn native_full_model_jacobian_matches_existing_product_and_analytic_values() {
    let compiled = Compiler::new()
        .model("DenseProduct")
        .compile_str(SOURCE, "DenseProduct.mo")
        .unwrap();
    let model = rumoca_sim::lower_dae_for_simulation(&compiled.dae, &Default::default()).unwrap();
    let reference = SolveRuntime::new(&model).unwrap();
    let mut native = CompiledModelStateJacobian::prepare(&model).unwrap();
    assert_eq!(native.states(), 2);
    let left = native
        .state_labels()
        .iter()
        .position(|name| name == "x[1]")
        .unwrap();
    let right = native
        .state_labels()
        .iter()
        .position(|name| name == "x[2]")
        .unwrap();
    assert_ne!(left, right);
    let settle = AlgebraicSettle {
        tol: 1e-12,
        max_iters: 64,
    };
    for point in [[0.25, -0.5], [-1.0, 0.75], [1.5, 0.125]] {
        let mut state = vec![0.0; 2];
        state[left] = point[0];
        state[right] = point[1];
        let mut output = vec![0.0; 4];
        native
            .eval(&state, &model.parameters, 0.375, &mut output)
            .unwrap();
        let expected = reference.eval_state_jacobian(0.375, &state, &model.parameters, settle);
        assert!(expected.error.is_none(), "{:?}", expected.error);
        assert_eq!(expected.state_labels, native.state_labels());
        for row in 0..2 {
            for column in 0..2 {
                let actual = output[row * 2 + column];
                assert!((actual - expected.matrix[row][column]).abs() < 1e-12);
            }
        }
        assert!((output[left * 2 + left] - 3.75 * point[0] * point[0]).abs() < 1e-12);
        assert!((output[left * 2 + right] - point[1].cos()).abs() < 1e-12);
        assert!((output[right * 2 + left] - point[1]).abs() < 1e-12);
        assert!((output[right * 2 + right] - point[0]).abs() < 1e-12);
    }
    let mut untouched = [123.0; 4];
    assert!(
        native
            .eval(&[1.0], &model.parameters, 0.0, &mut untouched)
            .is_err()
    );
    assert_eq!(untouched, [123.0; 4]);
}

#[test]
fn actual_algebraic_model_is_refused_by_supported_subset() {
    let source = "model HasAlgebraic Real x(start=1); Real y; equation y=sin(x); der(x)=y; end HasAlgebraic;";
    let compiled = Compiler::new()
        .model("HasAlgebraic")
        .compile_str(source, "HasAlgebraic.mo")
        .unwrap();
    let model = rumoca_sim::lower_dae_for_simulation(&compiled.dae, &Default::default()).unwrap();
    assert!(CompiledModelStateJacobian::prepare(&model).is_err());
}
