mod compact;
mod order;

use super::*;
use crate::{TypedValue, eval_pure_call_checked};
use rumoca_core::{ProvenanceSpan, SourceId};
use rumoca_ir_solve::*;

fn span() -> Span {
    Span::from_offsets(SourceId::from_source_name("ProductJacobian.mo"), 1, 2)
}

fn model(outputs: Vec<usize>) -> SolveModel {
    let ops = vec![
        LinearOp::LoadY { dst: 0, index: 0 },
        LinearOp::LoadY { dst: 1, index: 1 },
        LinearOp::Binary {
            dst: 2,
            op: BinaryOp::Mul,
            lhs: 0,
            rhs: 0,
        },
        LinearOp::Unary {
            dst: 3,
            op: UnaryOp::Sin,
            arg: 1,
        },
        LinearOp::Binary {
            dst: 4,
            op: BinaryOp::Add,
            lhs: 2,
            rhs: 3,
        },
        LinearOp::StoreOutput { src: 4 },
        LinearOp::Binary {
            dst: 5,
            op: BinaryOp::Mul,
            lhs: 0,
            rhs: 1,
        },
        LinearOp::StoreOutput { src: 5 },
    ];
    let rows = ScalarProgramBlock::with_output_indices(vec![ops], vec![span()], outputs).unwrap();
    SolveModel {
        problem: SolveProblem {
            layout: VarLayout::from_parts_with_shapes_and_spans(
                [("x".into(), scalar_slot_y(0))].into(),
                [("x".into(), vec![2])].into(),
                [("x".into(), span())].into(),
                2,
                0,
            )
            .unwrap(),
            solve_layout: SolveLayout {
                state_scalar_count: 2,
                variable_storage_runs: vec![SolveVariableStorageRun {
                    base: scalar_slot_y(0),
                    scalar_count: 2,
                    role: SolveVariableStorageRole::State,
                    value_kind: SolveVariableValueKind::Real,
                }],
                variable_declarations: vec![SolveVariableDeclaration::new(
                    SolveVariableStorageRole::State,
                    SolveVariableValueKind::Real,
                )],
                ..Default::default()
            },
            continuous: ContinuousSolveSystem {
                derivative_rhs: ComputeBlock::from_scalar_program_block(rows),
                ..Default::default()
            },
            ..Default::default()
        },
        ..Default::default()
    }
}

fn evaluate(product: &ModelStateJacobian<'_>, x: [f64; 2]) -> Vec<f64> {
    let input = [x[0], x[1], 0.0];
    let arguments = product
        .call_site()
        .inputs()
        .iter()
        .zip(input)
        .map(|(ty, value)| {
            TypedValue::construct(ty.clone(), vec![SolveValueKind::Real64(value.to_bits())])
                .unwrap()
        })
        .collect::<Vec<_>>();
    let values =
        eval_pure_call_checked(product.table(), product.call_site().owner(), &arguments).unwrap();
    values[0]
        .elements()
        .iter()
        .map(|value| match value {
            SolveValueKind::Real64(bits) => f64::from_bits(*bits),
            _ => panic!("Binary64 matrix expected"),
        })
        .collect()
}

#[test]
fn model_product_matches_analytic_and_finite_difference_on_all_dense_entries() {
    let model = model(vec![0, 1]);
    let graph = ModelStateJacobian::construct(&model).unwrap();
    for x in [[0.25, -0.5], [-1.5, 0.75], [2.0, 1.25]] {
        let actual = evaluate(&graph, x);
        let analytic = [2.0 * x[0], x[1].cos(), x[1], x[0]];
        let primal = |v: [f64; 2]| [v[0] * v[0] + v[1].sin(), v[0] * v[1]];
        for column in 0..2 {
            let mut plus = x;
            plus[column] += 1e-6;
            let mut minus = x;
            minus[column] -= 1e-6;
            for row in 0..2 {
                let index = row * 2 + column;
                let fd = (primal(plus)[row] - primal(minus)[row]) / 2e-6;
                assert!((actual[index] - analytic[index]).abs() < 1e-12);
                assert!((actual[index] - fd).abs() < 1e-8);
            }
        }
    }
}

#[test]
fn output_permutation_comes_from_canonical_placements_not_program_order() {
    let model = model(vec![1, 0]);
    let graph = ModelStateJacobian::construct(&model).unwrap();
    let values = evaluate(&graph, [0.25, 0.5]);
    assert_eq!(values[0], 0.5);
    assert_eq!(values[1], 0.25);
    assert_eq!(values[2], 0.5);
    assert_eq!(values[3], 0.5f64.cos());
}

#[test]
fn detached_derivative_artifact_is_not_model_jacobian_authority() {
    let mut model = model(vec![0, 1]);
    model.artifacts.continuous.full_jacobian_v = ScalarProgramBlock::with_source_span(
        vec![vec![
            LinearOp::Const {
                dst: 0,
                value: 99.0,
            },
            LinearOp::StoreOutput { src: 0 },
        ]],
        ProvenanceSpan::new(span(), "unrelated derivative").unwrap(),
    )
    .unwrap();
    assert_eq!(
        evaluate(&ModelStateJacobian::construct(&model).unwrap(), [2.0, 0.5])[0],
        4.0
    );
}

#[test]
fn unsupported_history_and_manifold_sources_refuse_before_entry_issuance() {
    let mut history = model(vec![0, 1]);
    history.problem.events.has_terminal_event = true;
    assert!(matches!(
        ModelStateJacobian::construct(&history),
        Err(ModelStateJacobianError::Unsupported { .. })
    ));
    let mut manifold = model(vec![0, 1]);
    manifold.problem.continuous.manifold_residual =
        manifold.problem.continuous.derivative_rhs.clone();
    assert!(matches!(
        ModelStateJacobian::construct(&manifold),
        Err(ModelStateJacobianError::Unsupported { .. })
    ));
}

#[test]
fn unsupported_lazy_source_refuses_instead_of_eager_evaluation() {
    let mut model = model(vec![0, 1]);
    let rows = ScalarProgramBlock::with_program_spans(
        vec![vec![
            LinearOp::Const { dst: 0, value: 1.0 },
            LinearOp::Select {
                dst: 1,
                cond: 0,
                if_true: 0,
                if_false: 0,
            },
            LinearOp::StoreOutput { src: 1 },
            LinearOp::StoreOutput { src: 1 },
        ]],
        vec![span()],
    )
    .unwrap();
    model.problem.continuous.derivative_rhs = ComputeBlock::from_scalar_program_block(rows);
    assert!(matches!(
        ModelStateJacobian::construct(&model),
        Err(ModelStateJacobianError::Unsupported {
            reason: "Select",
            ..
        })
    ));
}

#[test]
fn shared_coefficient_occurrence_is_issued_once_across_dense_directions() {
    let model = single_expression(vec![
        LinearOp::LoadY { dst: 0, index: 0 },
        LinearOp::LoadY { dst: 1, index: 1 },
        LinearOp::Binary {
            dst: 2,
            op: BinaryOp::Mul,
            lhs: 0,
            rhs: 1,
        },
        LinearOp::Unary {
            dst: 4,
            op: UnaryOp::Sin,
            arg: 2,
        },
    ]);
    let product = ModelStateJacobian::construct(&model).unwrap();
    let body = product
        .table()
        .owner(product.call_site().owner())
        .unwrap()
        .body();
    let cosine = body
        .operations()
        .iter()
        .filter(|operation| {
            matches!(
                operation.operation(),
                SolveOperation::Unary {
                    operator: SolveUnaryOperator::Cos,
                    ..
                }
            )
        })
        .count();
    assert_eq!(
        cosine, 1,
        "the sine coefficient is shared across both present state directions"
    );
}

#[test]
fn equal_independent_model_constructions_keep_distinct_numerical_owners() {
    let first = model(vec![0, 1]);
    let second = model(vec![0, 1]);
    let first_product = ModelStateJacobian::construct(&first).unwrap();
    let second_product = ModelStateJacobian::construct(&second).unwrap();
    assert!(
        !first_product
            .table()
            .shares_table_owner(second_product.table())
    );
    assert!(std::ptr::eq(
        first_product.source().continuous(),
        &first.problem.continuous
    ));
    assert!(std::ptr::eq(
        second_product.source().continuous(),
        &second.problem.continuous
    ));
    assert!(!std::ptr::eq(
        first_product.source().continuous(),
        second_product.source().continuous()
    ));
}

#[test]
fn compact_tensor_source_refuses_instead_of_expanding_coordinates() {
    let mut model = model(vec![0, 1]);
    let rows = ScalarProgramBlock::with_program_spans(
        vec![vec![
            LinearOp::TensorLoad {
                dst_start: 0,
                input: TensorInputKind::Y,
                input_start: 0,
                count: 2,
                seed_start: None,
                lanes: 1,
            },
            LinearOp::StoreOutputRange {
                start: 0,
                count: 2,
                stride: 1,
            },
        ]],
        vec![span()],
    )
    .unwrap();
    model.problem.continuous.derivative_rhs = ComputeBlock::from_scalar_program_block(rows);
    assert!(matches!(
        ModelStateJacobian::construct(&model),
        Err(ModelStateJacobianError::Unsupported {
            reason: "TensorLoad",
            ..
        })
    ));
}

fn linear_program(a: f64, b: f64) -> Vec<LinearOp> {
    vec![
        LinearOp::LoadY { dst: 0, index: 0 },
        LinearOp::LoadY { dst: 1, index: 1 },
        LinearOp::Const { dst: 2, value: a },
        LinearOp::Const { dst: 3, value: b },
        LinearOp::Binary {
            dst: 4,
            op: BinaryOp::Mul,
            lhs: 0,
            rhs: 2,
        },
        LinearOp::Binary {
            dst: 5,
            op: BinaryOp::Mul,
            lhs: 1,
            rhs: 3,
        },
        LinearOp::Binary {
            dst: 6,
            op: BinaryOp::Add,
            lhs: 4,
            rhs: 5,
        },
        LinearOp::StoreOutput { src: 6 },
    ]
}

#[test]
fn structural_product_asymmetric_rows_preserve_canonical_order_on_repeated_calls() {
    let mut source = model(vec![0, 1]);
    source.problem.continuous.derivative_rhs = ComputeBlock::from_scalar_program_block(
        ScalarProgramBlock::with_output_indices(
            vec![linear_program(5.0, 7.0), linear_program(2.0, 3.0)],
            vec![span(), span()],
            vec![1, 0],
        )
        .unwrap(),
    );
    let product = ModelStateJacobian::construct(&source).unwrap();
    assert_eq!(product.state_labels().len(), 2);
    for point in [[11.0, 13.0], [-2.0, 5.0]] {
        assert_eq!(evaluate(&product, point), vec![2.0, 3.0, 5.0, 7.0]);
    }
}

fn single_expression(ops: Vec<LinearOp>) -> SolveModel {
    let mut source = model(vec![0, 1]);
    let mut first = ops;
    first.push(LinearOp::StoreOutput { src: 4 });
    let second = vec![
        LinearOp::Const { dst: 0, value: 0.0 },
        LinearOp::StoreOutput { src: 0 },
    ];
    source.problem.continuous.derivative_rhs = ComputeBlock::from_scalar_program_block(
        ScalarProgramBlock::with_output_indices(
            vec![first, second],
            vec![span(), span()],
            vec![0, 1],
        )
        .unwrap(),
    );
    source
}

fn evaluate_checked(
    product: &ModelStateJacobian<'_>,
    point: [f64; 2],
) -> Result<Vec<crate::TypedValue>, crate::TypedProgramEvalError> {
    let arguments = point
        .into_iter()
        .chain([0.0])
        .map(|x| {
            TypedValue::construct(
                SolveValueType::scalar(SolveScalarType::real(product.table().arithmetic())),
                vec![SolveValueKind::Real64(x.to_bits())],
            )
            .unwrap()
        })
        .collect::<Vec<_>>();
    eval_pure_call_checked(product.table(), product.call_site().owner(), &arguments)
}

#[test]
fn structural_product_reports_raw_zero_quotient_before_directional_masking() {
    let source = single_expression(vec![
        LinearOp::LoadY { dst: 0, index: 0 },
        LinearOp::Binary {
            dst: 4,
            op: BinaryOp::Div,
            lhs: 0,
            rhs: 0,
        },
    ]);
    let product = ModelStateJacobian::construct(&source).unwrap();
    assert!(matches!(
        evaluate_checked(&product, [0.0, 1.0]),
        Err(crate::TypedProgramEvalError::NonFinite {
            check: SolveFiniteCheck::Primal {
                program: 0,
                operation: 1
            },
            element: 0,
            ..
        })
    ));
    assert!(evaluate_checked(&product, [2.0, 1.0]).is_ok());
}

#[test]
fn structural_product_validates_unused_inputs_before_primal_occurrences() {
    let source = single_expression(vec![LinearOp::Const { dst: 4, value: 2.0 }]);
    let product = ModelStateJacobian::construct(&source).unwrap();
    assert_eq!(
        evaluate(&product, [1.0, 2.0])
            .iter()
            .map(|x| x.to_bits())
            .collect::<Vec<_>>(),
        vec![0; 4]
    );
    assert!(matches!(
        evaluate_checked(&product, [1.0, f64::NAN]),
        Err(crate::TypedProgramEvalError::NonFinite {
            check: SolveFiniteCheck::Input { coordinate: 1 },
            ..
        })
    ));
}

#[test]
fn structural_product_distinguishes_finite_primal_from_derivative_overflow() {
    let source = single_expression(vec![
        LinearOp::LoadY { dst: 0, index: 0 },
        LinearOp::LoadY { dst: 1, index: 1 },
        LinearOp::Binary {
            dst: 2,
            op: BinaryOp::Mul,
            lhs: 0,
            rhs: 1,
        },
        LinearOp::Unary {
            dst: 3,
            op: UnaryOp::Exp,
            arg: 2,
        },
        LinearOp::Const { dst: 5, value: 2.0 },
        LinearOp::Binary {
            dst: 4,
            op: BinaryOp::Mul,
            lhs: 5,
            rhs: 3,
        },
    ]);
    let product = ModelStateJacobian::construct(&source).unwrap();
    assert!(matches!(
        evaluate_checked(&product, [0.0, 1e308]),
        Err(crate::TypedProgramEvalError::NonFinite {
            check: SolveFiniteCheck::Derivative,
            element: 0,
            ..
        })
    ));
    assert!(matches!(
        evaluate_checked(&product, [1000.0, 1.0]),
        Err(crate::TypedProgramEvalError::NonFinite {
            check: SolveFiniteCheck::Primal {
                program: 0,
                operation: 3
            },
            ..
        })
    ));
}

#[test]
fn structural_product_independent_invalid_primal_cannot_disappear() {
    let fixtures = [
        vec![
            LinearOp::Const {
                dst: 0,
                value: -1.0,
            },
            LinearOp::Unary {
                dst: 4,
                op: UnaryOp::Log,
                arg: 0,
            },
        ],
        vec![
            LinearOp::LoadY { dst: 0, index: 0 },
            LinearOp::Unary {
                dst: 1,
                op: UnaryOp::Exp,
                arg: 0,
            },
            LinearOp::Const { dst: 2, value: 0.0 },
            LinearOp::Binary {
                dst: 4,
                op: BinaryOp::Mul,
                lhs: 2,
                rhs: 1,
            },
        ],
        vec![
            LinearOp::LoadY { dst: 0, index: 0 },
            LinearOp::Binary {
                dst: 4,
                op: BinaryOp::Mul,
                lhs: 0,
                rhs: 0,
            },
        ],
    ];
    for (ops, point) in fixtures
        .into_iter()
        .zip([[1.0, 1.0], [1000.0, 1.0], [1e308, 1.0]])
    {
        let source = single_expression(ops);
        let product = ModelStateJacobian::construct(&source).unwrap();
        assert!(matches!(
            evaluate_checked(&product, point),
            Err(crate::TypedProgramEvalError::NonFinite {
                check: SolveFiniteCheck::Primal {
                    program: 0,
                    operation: 1
                },
                ..
            })
        ));
    }
}

#[test]
fn structural_product_absent_partial_and_subtraction_keep_their_exact_roles() {
    let absent = single_expression(vec![
        LinearOp::Const { dst: 0, value: 0.0 },
        LinearOp::Unary {
            dst: 4,
            op: UnaryOp::Sqrt,
            arg: 0,
        },
    ]);
    let product = ModelStateJacobian::construct(&absent).unwrap();
    assert_eq!(
        evaluate(&product, [2.0, 3.0])
            .iter()
            .map(|x| x.to_bits())
            .collect::<Vec<_>>(),
        vec![0; 4]
    );
    let subtraction = single_expression(vec![
        LinearOp::Const { dst: 0, value: 2.0 },
        LinearOp::LoadY { dst: 1, index: 0 },
        LinearOp::Binary {
            dst: 4,
            op: BinaryOp::Sub,
            lhs: 0,
            rhs: 1,
        },
    ]);
    let product = ModelStateJacobian::construct(&subtraction).unwrap();
    assert_eq!(evaluate(&product, [2.0, 3.0]), vec![-1.0, 0.0, 0.0, 0.0]);
}

#[test]
fn structural_product_register_overwrites_follow_the_reaching_definition() {
    let erase = single_expression(vec![
        LinearOp::LoadY { dst: 0, index: 0 },
        LinearOp::Const { dst: 0, value: 3.0 },
        LinearOp::Move { dst: 4, src: 0 },
    ]);
    let product = ModelStateJacobian::construct(&erase).unwrap();
    assert_eq!(evaluate(&product, [2.0, 3.0]), vec![0.0; 4]);
    let restore = single_expression(vec![
        LinearOp::Const { dst: 0, value: 3.0 },
        LinearOp::LoadY { dst: 0, index: 0 },
        LinearOp::Move { dst: 4, src: 0 },
    ]);
    let product = ModelStateJacobian::construct(&restore).unwrap();
    assert_eq!(evaluate(&product, [2.0, 3.0]), vec![1.0, 0.0, 0.0, 0.0]);
}
