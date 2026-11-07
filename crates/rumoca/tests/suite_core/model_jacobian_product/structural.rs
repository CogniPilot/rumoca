use super::*;
use rumoca_exec_cranelift::CompileError;
use rumoca_ir_solve as solve;

fn lower(text: &str, name: &str) -> solve::SolveModel {
    let compiled = Compiler::new()
        .model(name)
        .compile_str(text, "Structural.mo")
        .unwrap();
    rumoca_sim::lower_dae_for_simulation(&compiled.dae, &Default::default()).unwrap()
}

fn coordinate(native: &CompiledModelStateJacobian<'_>, name: &str) -> usize {
    native
        .state_labels()
        .iter()
        .position(|label| label == name)
        .unwrap()
}

#[test]
fn structural_native_sampled_parameter_zero_and_signed_zero_keep_present_derivatives() {
    let source = "model StructuralParameter parameter Real p=1; Real x(start=0); Real y(start=0); equation der(x)=p*x; der(y)=y; end StructuralParameter;";
    let model = lower(source, "StructuralParameter");
    let mut native = CompiledModelStateJacobian::prepare(&model).unwrap();
    assert_eq!(native.states(), 2);
    assert_eq!(native.parameters(), 1);
    let x = coordinate(&native, "x");
    let y = coordinate(&native, "y");
    let mut out = [77.0; 4];
    for p in [-0.0, 3.0, 0.0, -2.0] {
        native.eval(&[0.0, 0.0], &[p], 0.0, &mut out).unwrap();
        assert_eq!(out[x * 2 + x].to_bits(), p.to_bits());
        assert_eq!(out[x * 2 + y].to_bits(), 0.0_f64.to_bits());
        assert_eq!(out[y * 2 + x].to_bits(), 0.0_f64.to_bits());
        assert_eq!(out[y * 2 + y], 1.0);
    }
    let before = out.map(f64::to_bits);
    assert!(matches!(
        native.eval(&[1.0, 1.0], &[1.0], f64::NAN, &mut out),
        Err(CompileError::NonFinite {
            check: solve::SolveFiniteCheck::Input { .. },
            ..
        })
    ));
    assert_eq!(out.map(f64::to_bits), before);
}

#[test]
fn structural_native_invalid_primal_reports_error_without_publishing_zeros() {
    let model = lower(
        "model StructuralLog Real x(start=1); Real y(start=1); equation der(x)=log(x); der(y)=y; end StructuralLog;",
        "StructuralLog",
    );
    let mut native = CompiledModelStateJacobian::prepare(&model).unwrap();
    let x = coordinate(&native, "x");
    let mut point = [1.0; 2];
    point[x] = -1.0;
    let mut out = [11.0, -0.0, 13.0, 14.0];
    let before = out.map(f64::to_bits);
    assert!(matches!(
        native.eval(&point, &model.parameters, 0.0, &mut out),
        Err(CompileError::NonFinite {
            check: solve::SolveFiniteCheck::Primal { .. },
            span: Some(_),
            ..
        })
    ));
    assert_eq!(out.map(f64::to_bits), before);
    point[x] = 1.0;
    native
        .eval(&point, &model.parameters, 0.0, &mut out)
        .unwrap();
    assert_eq!(out[x * 2 + x], 1.0);
}

#[test]
fn structural_native_derivative_overflow_is_distinct_from_finite_kink_rule() {
    let model = lower(
        "model StructuralOverflow Real x(start=0); Real y(start=1); equation der(x)=2*exp(x*y); der(y)=y; end StructuralOverflow;",
        "StructuralOverflow",
    );
    let mut native = CompiledModelStateJacobian::prepare(&model).unwrap();
    let y = coordinate(&native, "y");
    let mut point = [0.0; 2];
    point[y] = 1e308;
    let mut out = [21.0, 22.0, -0.0, 24.0];
    let before = out.map(f64::to_bits);
    assert!(matches!(
        native.eval(&point, &model.parameters, 0.0, &mut out),
        Err(CompileError::NonFinite {
            check: solve::SolveFiniteCheck::Derivative,
            ..
        })
    ));
    assert_eq!(out.map(f64::to_bits), before);
    let model = lower(
        "model StructuralKink Real x(start=0); Real y(start=1); equation der(x)=sqrt(x); der(y)=y; end StructuralKink;",
        "StructuralKink",
    );
    let mut native = CompiledModelStateJacobian::prepare(&model).unwrap();
    let x = coordinate(&native, "x");
    native
        .eval(&[0.0, 0.0], &model.parameters, 0.0, &mut out)
        .unwrap();
    assert_eq!(out[x * 2 + x], 0.0); // Existing canonical partial-at-kink convention.
}

fn linear(a: f64, b: f64) -> Vec<solve::LinearOp> {
    use solve::{BinaryOp as B, LinearOp as O};
    vec![
        O::LoadY { dst: 0, index: 0 },
        O::LoadY { dst: 1, index: 1 },
        O::Const { dst: 2, value: a },
        O::Const { dst: 3, value: b },
        O::Binary {
            dst: 4,
            op: B::Mul,
            lhs: 0,
            rhs: 2,
        },
        O::Binary {
            dst: 5,
            op: B::Mul,
            lhs: 1,
            rhs: 3,
        },
        O::Binary {
            dst: 6,
            op: B::Add,
            lhs: 4,
            rhs: 5,
        },
        O::StoreOutput { src: 6 },
    ]
}

#[test]
fn structural_native_asymmetric_rows_and_axes_ignore_reversed_program_order() {
    let mut model = lower(SOURCE, "DenseProduct");
    model.problem.continuous.derivative_rhs = solve::ComputeBlock::from_scalar_program_block(
        solve::ScalarProgramBlock::with_output_indices(
            vec![linear(5.0, 7.0), linear(2.0, 3.0)],
            vec![model.problem.layout.shape_span("x").unwrap(); 2],
            vec![1, 0],
        )
        .unwrap(),
    );
    let mut native = CompiledModelStateJacobian::prepare(&model).unwrap();
    let expected_labels = (0..2)
        .map(|i| model.problem.solver_name(i).unwrap())
        .collect::<Vec<_>>();
    assert_eq!(native.state_labels(), expected_labels.as_slice());
    for point in [[11.0, 13.0], [-2.0, 5.0]] {
        let mut out = [31.0, 32.0, 33.0, 34.0];
        native
            .eval(&point, &model.parameters, 0.0, &mut out)
            .unwrap();
        assert_eq!(out, [2.0, 3.0, 5.0, 7.0]);
    }
}

#[test]
fn structural_native_underflow_coefficient_does_not_change_dependency() {
    let model = lower(
        "model StructuralUnderflow parameter Real p=1; parameter Real q=1; Real x(start=0); Real y(start=0); equation der(x)=(p*q)*x; der(y)=y; end StructuralUnderflow;",
        "StructuralUnderflow",
    );
    let mut native = CompiledModelStateJacobian::prepare(&model).unwrap();
    assert_eq!(native.parameters(), 2);
    let x = coordinate(&native, "x");
    let mut output = [0.0; 4];
    for (coefficient, expected) in [(1e-200, 0.0), (2.0, 4.0)] {
        native
            .eval(&[0.0, 0.0], &[coefficient, coefficient], 0.0, &mut output)
            .unwrap();
        assert_eq!(output[x * 2 + x], expected);
    }
}

#[test]
fn structural_native_absent_state_partial_does_not_evaluate_unrequested_overflow() {
    let model = lower(
        "model StructuralAbsentPartial parameter Real p=1; parameter Real q=1; Real x(start=0); Real y(start=1); equation der(x)=2*exp(p*q); der(y)=y; end StructuralAbsentPartial;",
        "StructuralAbsentPartial",
    );
    let mut native = CompiledModelStateJacobian::prepare(&model).unwrap();
    assert_eq!(native.parameters(), 2);
    let x = coordinate(&native, "x");
    let y = coordinate(&native, "y");
    let mut output = [31.0; 4];
    native
        .eval(&[1.0, 1.0], &[0.0, 1e308], 0.0, &mut output)
        .unwrap();
    assert_eq!(output[x * 2 + x].to_bits(), 0.0_f64.to_bits());
    assert_eq!(output[x * 2 + y].to_bits(), 0.0_f64.to_bits());
    assert_eq!(output[y * 2 + x].to_bits(), 0.0_f64.to_bits());
    assert_eq!(output[y * 2 + y], 1.0);
}

/// Both executors run the constructor's one Derivative guard over the cells it
/// cannot prove finite, so they report the same check and guarded-cell ordinal.
#[test]
fn structural_native_and_interpreter_report_the_same_guarded_derivative_cell() {
    let model = lower(
        "model GuardedCell Real x(start=1); Real y(start=1); equation der(x)=1.5e308*y*y; der(y)=y; end GuardedCell;",
        "GuardedCell",
    );
    let mut native = CompiledModelStateJacobian::prepare(&model).unwrap();
    let y = coordinate(&native, "y");
    let mut point = [1.0; 2];
    point[y] = 0.9;
    let mut out = [0.0; 4];
    let native_element = match native.eval(&point, &model.parameters, 0.0, &mut out) {
        Err(CompileError::NonFinite {
            check: solve::SolveFiniteCheck::Derivative,
            element,
            ..
        }) => element,
        other => panic!("expected a Derivative failure, got {other:?}"),
    };
    let product = rumoca_eval_solve::model_jacobian::ModelStateJacobian::construct(&model).unwrap();
    let labels = product.state_labels();
    let inputs = product.call_site().inputs();
    let arguments = labels
        .iter()
        .map(|label| point[coordinate(&native, label)])
        .chain(model.parameters.iter().copied())
        .chain([0.0])
        .zip(inputs)
        .map(|(value, ty)| {
            rumoca_eval_solve::TypedValue::construct(
                ty.clone(),
                vec![solve::SolveValueKind::Real64(value.to_bits())],
            )
            .unwrap()
        })
        .collect::<Vec<_>>();
    assert_eq!(arguments.len(), inputs.len());
    let interpreted = rumoca_eval_solve::eval_pure_call_checked(
        product.table(),
        product.call_site().owner(),
        &arguments,
    );
    assert!(
        matches!(
            interpreted,
            Err(rumoca_eval_solve::TypedProgramEvalError::NonFinite {
                check: solve::SolveFiniteCheck::Derivative,
                element,
                ..
            }) if element == native_element
        ),
        "native reported element {native_element}, interpreter reported {interpreted:?}"
    );
}
