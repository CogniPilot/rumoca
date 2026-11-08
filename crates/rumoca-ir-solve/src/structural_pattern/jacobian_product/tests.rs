use super::*;
use crate::*;
use rumoca_core::SourceId;

fn span() -> Span {
    Span::from_offsets(SourceId::from_source_name("JacobianProduct.mo"), 1, 2)
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

fn check() -> (Span, SolveFiniteCheck) {
    (
        span(),
        SolveFiniteCheck::Primal {
            program: 0,
            operation: 0,
        },
    )
}

fn asymmetric(
    c: &mut StateJacobianProductConstruction<'_, '_>,
) -> Result<(), SolveProgramConstructionError> {
    let x = c.read(StateJacobianProductInput::State(1), check())?;
    let zero = c.constant(0.0, check())?;
    c.store_row(0, &x, span())?;
    c.store_row(1, &zero, span())
}

#[test]
fn opaque_rows_issue_asymmetric_relation_and_bind_target() {
    let model = model();
    let product = StateJacobianProduct::construct(
        model.continuous_numerical_source().unwrap(),
        StoredStateJacobianOutputOrder::canonical(&model).unwrap(),
        &SolveTargetProfile::current_dense_host(),
        span(),
        asymmetric,
    )
    .unwrap();
    assert!(std::ptr::eq(product.source_model(), &model));
    assert_eq!(product.states(), 2);
    assert!(product.canonical_dependencies().contains(0, 1));
    assert!(!product.canonical_dependencies().contains(1, 0));
    assert!(!product.canonical_dependencies().contains(0, 0));
    assert!(!product.canonical_dependencies().contains(1, 1));
    assert_eq!(
        product.domain(),
        StateJacobianProductDomain::FiniteStoredState
    );
    let _ = product.specialization();
}

#[test]
fn same_shaped_foreign_source_refuses_before_producer() {
    let model = model();
    let foreign = model.clone();
    let called = std::cell::Cell::new(false);
    let result = StateJacobianProduct::construct(
        model.continuous_numerical_source().unwrap(),
        StoredStateJacobianOutputOrder::canonical(&foreign).unwrap(),
        &SolveTargetProfile::current_dense_host(),
        span(),
        |_| {
            called.set(true);
            Ok(())
        },
    );
    assert!(matches!(
        result,
        Err(StateJacobianProductConstructionError::ForeignSource)
    ));
    assert!(!called.get());
}

#[test]
fn cancellation_keeps_presence_and_constants_have_no_directions() {
    let model = model();
    let product = StateJacobianProduct::construct(
        model.continuous_numerical_source().unwrap(),
        StoredStateJacobianOutputOrder::canonical(&model).unwrap(),
        &SolveTargetProfile::current_dense_host(),
        span(),
        |c| {
            let x = c.read(StateJacobianProductInput::State(0), check())?;
            let cancelled = c.binary(SolveBinaryOperator::Subtract, [&x, &x], check())?;
            let zero = c.constant(0.0, check())?;
            c.store_row(0, &cancelled, span())?;
            c.store_row(1, &zero, span())
        },
    )
    .unwrap();
    assert!(product.canonical_dependencies().contains(0, 0));
    assert!(!product.canonical_dependencies().contains(1, 0));
}

#[test]
fn original_rows_must_be_written_once_and_completely() {
    let model = model();
    for duplicate in [false, true] {
        let result = StateJacobianProduct::construct(
            model.continuous_numerical_source().unwrap(),
            StoredStateJacobianOutputOrder::canonical(&model).unwrap(),
            &SolveTargetProfile::current_dense_host(),
            span(),
            |c| {
                let zero = c.constant(0.0, check())?;
                c.store_row(0, &zero, span())?;
                if duplicate {
                    c.store_row(0, &zero, span())?;
                }
                Ok(())
            },
        );
        assert!(matches!(
            result,
            Err(StateJacobianProductConstructionError::Program(_))
        ));
    }
}

#[test]
fn output_axes_project_without_changing_canonical_relation() {
    let model = model();
    let order = StoredStateJacobianOutputOrder::canonical(&model).unwrap();
    let rows = order
        .input_labels()
        .iter()
        .rev()
        .cloned()
        .collect::<Vec<_>>();
    let columns = order.input_labels().to_vec();
    let order = StoredStateJacobianOutputOrder::construct(&model, &rows, &columns).unwrap();
    let product = StateJacobianProduct::construct(
        model.continuous_numerical_source().unwrap(),
        order,
        &SolveTargetProfile::current_dense_host(),
        span(),
        asymmetric,
    )
    .unwrap();
    assert!(product.canonical_dependencies().contains(0, 1));
    assert!(product.dependencies().contains(1, 1));
    assert!(!product.dependencies().contains(0, 1));
}

#[test]
fn copied_output_keeps_its_source_contract_and_direction_presence() {
    let model = model();
    let order = StoredStateJacobianOutputOrder::canonical(&model).unwrap();
    let labels = order.input_labels().to_vec();
    let product = StateJacobianProduct::construct(
        model.continuous_numerical_source().unwrap(),
        order,
        &SolveTargetProfile::current_dense_host(),
        span(),
        |c| {
            let x = c.read(StateJacobianProductInput::State(1), check())?;
            let copied = c.copy(&x, check())?;
            let zero = c.constant(0.0, check())?;
            c.store_row(0, &copied, span())?;
            c.store_row(1, &zero, span())
        },
    )
    .unwrap();
    assert_eq!(product.parameters(), model.problem.layout.p_scalars());
    assert_eq!(product.state_labels(), labels);
    assert!(std::ptr::eq(
        product.source().layout(),
        &model.problem.layout
    ));
    assert!(std::ptr::eq(
        product.source().coordinates(),
        &model.problem.solve_layout
    ));
    assert_eq!(
        product.call_site().owner(),
        product.table().owners()[0].id()
    );
    assert_eq!(product.call_site().inputs().len(), 3);
    assert!(product.canonical_dependencies().contains(0, 1));
    assert!(!product.canonical_dependencies().contains(0, 0));
    assert!(!product.canonical_dependencies().contains(1, 1));
}
