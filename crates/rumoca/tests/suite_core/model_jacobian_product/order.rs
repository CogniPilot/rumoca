use super::*;
use rumoca_eval_solve::model_jacobian::ModelStateJacobian;
use rumoca_ir_solve::StoredStateJacobianOutputOrder;

fn assert_ordered_native_cells(
    output: &[f64; 4],
    expected: &[Vec<f64>],
    rows: [usize; 2],
    columns: [usize; 2],
) {
    for r in 0..2 {
        for c in 0..2 {
            assert!((output[r * 2 + c] - expected[rows[r]][columns[c]]).abs() < 1e-12);
        }
    }
}

#[test]
fn ordered_native_product_uses_requested_axes_and_canonical_state_inputs() {
    let compiled = Compiler::new()
        .model("DenseProduct")
        .compile_str(SOURCE, "Ordered.mo")
        .unwrap();
    let model = rumoca_sim::lower_dae_for_simulation(&compiled.dae, &Default::default()).unwrap();
    let labels = ModelStateJacobian::construct(&model)
        .unwrap()
        .state_labels()
        .to_vec();
    let reference = SolveRuntime::new(&model).unwrap();
    let settle = AlgebraicSettle {
        tol: 1e-12,
        max_iters: 64,
    };
    for rows in [[0, 1], [1, 0]] {
        for columns in [[0, 1], [1, 0]] {
            let row_names = rows.map(|i| labels[i].clone());
            let column_names = columns.map(|i| labels[i].clone());
            let order =
                StoredStateJacobianOutputOrder::construct(&model, &row_names, &column_names)
                    .unwrap();
            let graph = ModelStateJacobian::construct_with_output_order(order).unwrap();
            let mut native = CompiledModelStateJacobian::compile(graph).unwrap();
            assert_eq!(native.state_labels(), labels);
            assert_eq!(
                native.source_product().output_order().row_labels(),
                row_names
            );
            assert_eq!(
                native.source_product().output_order().column_labels(),
                column_names
            );
            for (state, parameter, time) in [([0.25, -0.5], 1.25, 0.375), ([-1.5, 0.75], -2.0, 2.0)]
            {
                let mut output = [123.0; 4];
                native
                    .eval(&state, &[parameter], time, &mut output)
                    .unwrap();
                let expected = reference.eval_state_jacobian(time, &state, &[parameter], settle);
                assert!(expected.error.is_none(), "{:?}", expected.error);
                assert_ordered_native_cells(&output, &expected.matrix, rows, columns);
            }
        }
    }
}

#[test]
fn ordered_native_faults_preserve_caller_output_and_original_primal_diagnostic() {
    let source = "model OrderedFault Real x(start=1); Real y(start=1); equation der(x)=log(x); der(y)=y; end OrderedFault;";
    let compiled = Compiler::new()
        .model("OrderedFault")
        .compile_str(source, "OrderedFault.mo")
        .unwrap();
    let model = rumoca_sim::lower_dae_for_simulation(&compiled.dae, &Default::default()).unwrap();
    let labels = ModelStateJacobian::construct(&model)
        .unwrap()
        .state_labels()
        .to_vec();
    let rows = vec![labels[1].clone(), labels[0].clone()];
    let order = StoredStateJacobianOutputOrder::construct(&model, &rows, &labels).unwrap();
    let graph = ModelStateJacobian::construct_with_output_order(order).unwrap();
    let mut native = CompiledModelStateJacobian::compile(graph).unwrap();
    let x = labels.iter().position(|s| s == "x").unwrap();
    let mut state = [1.0; 2];
    state[x] = -1.0;
    let mut output = [11.0, -0.0, 13.0, 14.0];
    let before = output.map(f64::to_bits);
    assert!(matches!(
        native.eval(&state, &model.parameters, 0.0, &mut output),
        Err(rumoca_exec_cranelift::CompileError::NonFinite {
            check: rumoca_ir_solve::SolveFiniteCheck::Primal { .. },
            span: Some(_),
            ..
        })
    ));
    assert_eq!(output.map(f64::to_bits), before);
    state[x] = 1.0;
    assert!(matches!(
        native.eval(&state, &model.parameters, f64::NAN, &mut output),
        Err(rumoca_exec_cranelift::CompileError::NonFinite {
            check: rumoca_ir_solve::SolveFiniteCheck::Input { .. },
            ..
        })
    ));
    assert_eq!(output.map(f64::to_bits), before);
    native
        .eval(&state, &model.parameters, 0.0, &mut output)
        .unwrap();
    assert_eq!(output, [0.0, 1.0, 1.0, 0.0]);
}

#[test]
fn ordered_native_derivative_overflow_is_transactional() {
    let source = "model OrderedOverflow Real x(start=0); Real y(start=1); equation der(x)=2*exp(x*y); der(y)=y; end OrderedOverflow;";
    let compiled = Compiler::new()
        .model("OrderedOverflow")
        .compile_str(source, "OrderedOverflow.mo")
        .unwrap();
    let model = rumoca_sim::lower_dae_for_simulation(&compiled.dae, &Default::default()).unwrap();
    let labels = ModelStateJacobian::construct(&model)
        .unwrap()
        .state_labels()
        .to_vec();
    let rows = vec![labels[1].clone(), labels[0].clone()];
    let order = StoredStateJacobianOutputOrder::construct(&model, &rows, &labels).unwrap();
    let graph = ModelStateJacobian::construct_with_output_order(order).unwrap();
    let mut native = CompiledModelStateJacobian::compile(graph).unwrap();
    let y = labels.iter().position(|s| s == "y").unwrap();
    let mut state = [0.0; 2];
    state[y] = 1e308;
    let mut output = [21.0, -0.0, 23.0, 24.0];
    let before = output.map(f64::to_bits);
    assert!(matches!(
        native.eval(&state, &model.parameters, 0.0, &mut output),
        Err(rumoca_exec_cranelift::CompileError::NonFinite {
            check: rumoca_ir_solve::SolveFiniteCheck::Derivative,
            ..
        })
    ));
    assert_eq!(output.map(f64::to_bits), before);
}
