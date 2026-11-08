//! Public consumer controls for the staged IR data API, without runtime cutover.
use rumoca_core::{SourceId, Span};
use rumoca_eval_solve::{TypedValue, eval_pure_call_checked};
use rumoca_ir_solve::*;

fn span() -> Span {
    Span::from_offsets(SourceId::from_source_name("IssuedJacobian.mo"), 1, 2)
}

fn model() -> SolveModel {
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
            ..Default::default()
        },
        ..Default::default()
    }
}

fn check(operation: u32) -> (Span, SolveFiniteCheck) {
    (
        span(),
        SolveFiniteCheck::Primal {
            program: 0,
            operation,
        },
    )
}

fn evaluate(product: &StateJacobianProduct<'_>, state: [f64; 2]) -> Option<Vec<u64>> {
    let arguments = product
        .call_site()
        .inputs()
        .iter()
        .zip([state[0], state[1], 0.0])
        .map(|(ty, value)| {
            TypedValue::construct(ty.clone(), vec![SolveValueKind::Real64(value.to_bits())])
                .unwrap()
        })
        .collect::<Vec<_>>();
    let outputs =
        eval_pure_call_checked(product.table(), product.call_site().owner(), &arguments).ok()?;
    Some(
        outputs[0]
            .elements()
            .iter()
            .map(|value| match value {
                SolveValueKind::Real64(bits) => *bits,
                _ => panic!("expected Binary64"),
            })
            .collect(),
    )
}

#[test]
fn cancellation_is_numeric_zero_but_keeps_its_structural_edge() {
    let model = model();
    let product = StateJacobianProduct::construct(
        model.continuous_numerical_source().unwrap(),
        StoredStateJacobianOutputOrder::canonical(&model).unwrap(),
        &SolveTargetProfile::current_dense_host(),
        span(),
        |c| {
            let x = c.read(StateJacobianProductInput::State(0), check(0))?;
            let cancelled = c.binary(SolveBinaryOperator::Subtract, [&x, &x], check(1))?;
            let copied = c.copy(&cancelled, check(2))?;
            let constant = c.constant(4.0, check(2))?;
            c.store_row(0, &copied, span())?;
            c.store_row(1, &constant, span())
        },
    )
    .unwrap();
    assert_eq!(product.parameters(), 0);
    assert_eq!(product.state_labels().len(), 2);
    assert!(std::ptr::eq(
        product.source().layout(),
        &model.problem.layout
    ));
    assert!(product.canonical_dependencies().contains(0, 0));
    assert!(!product.canonical_dependencies().contains(1, 0));
    assert_eq!(evaluate(&product, [2.0, 3.0]).unwrap(), vec![0; 4]);
}

#[test]
fn public_consumer_retains_the_existing_abs_kink_rule() {
    let model = model();
    let product = StateJacobianProduct::construct(
        model.continuous_numerical_source().unwrap(),
        StoredStateJacobianOutputOrder::canonical(&model).unwrap(),
        &SolveTargetProfile::current_dense_host(),
        span(),
        |c| {
            let x = c.read(StateJacobianProductInput::State(0), check(0))?;
            let absolute = c.unary(SolveUnaryOperator::Abs, &x, check(1))?;
            let constant = c.constant(4.0, check(2))?;
            c.store_row(0, &absolute, span())?;
            c.store_row(1, &constant, span())
        },
    )
    .unwrap();
    assert!(product.canonical_dependencies().contains(0, 0));
    for (x, derivative) in [(-2.0, -1.0_f64), (0.0, 1.0), (2.0, 1.0)] {
        assert_eq!(
            evaluate(&product, [x, 3.0]).unwrap()[0],
            derivative.to_bits()
        );
    }
}

#[test]
fn absent_direction_does_not_hide_a_nonfinite_primal() {
    let model = model();
    let product = StateJacobianProduct::construct(
        model.continuous_numerical_source().unwrap(),
        StoredStateJacobianOutputOrder::canonical(&model).unwrap(),
        &SolveTargetProfile::current_dense_host(),
        span(),
        |c| {
            let negative = c.constant(-1.0, check(0))?;
            let invalid = c.unary(SolveUnaryOperator::Sqrt, &negative, check(1))?;
            c.store_row(0, &invalid, span())?;
            c.store_row(1, &invalid, span())
        },
    )
    .unwrap();
    assert!(!product.canonical_dependencies().contains(0, 0));
    assert!(evaluate(&product, [2.0, 3.0]).is_none());
}
