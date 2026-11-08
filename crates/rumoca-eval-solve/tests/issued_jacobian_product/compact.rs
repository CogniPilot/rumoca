//! Execute the actual joined graph; compare source-ordered scalar controls.
use super::*;
use rumoca_core::{IndexBinderProvenance, StructuredIndexBinder, StructuredIndexDomain};
use rumoca_eval_solve::TypedProgramEvalError;

fn six_states() -> SolveModel {
    let mut m = model();
    m.problem.layout = VarLayout::from_parts_with_shapes_and_spans(
        [("x".into(), scalar_slot_y(0))].into(),
        [("x".into(), vec![6])].into(),
        [("x".into(), span())].into(),
        6,
        0,
    )
    .unwrap();
    m.problem.solve_layout.state_scalar_count = 6;
    m.problem.solve_layout.variable_storage_runs[0].scalar_count = 6;
    m
}

fn grid() -> StructuredIndexDomain {
    StructuredIndexDomain {
        binders: [2, 3]
            .into_iter()
            .enumerate()
            .map(|(id, upper)| StructuredIndexBinder {
                provenance: IndexBinderProvenance::Generated,
                id,
                display_name: format!("k{id}"),
                lower: 1,
                upper,
                step: 1,
            })
            .collect(),
    }
}

fn order(m: &SolveModel, reversed: bool) -> StoredStateJacobianOutputOrder<'_> {
    let order = StoredStateJacobianOutputOrder::canonical(m).unwrap();
    if !reversed {
        return order;
    }
    let rows = order
        .input_labels()
        .iter()
        .rev()
        .cloned()
        .collect::<Vec<_>>();
    StoredStateJacobianOutputOrder::construct(m, &rows, order.input_labels()).unwrap()
}

fn mapped(m: &SolveModel, reversed_axes: bool, sqrt: bool) -> StateJacobianProduct<'_> {
    let placement =
        CompactJacobianPlacement::new(vec![2, 3], (5, vec![-3, -1]), vec![(0, vec![3, 1])], 6)
            .unwrap();
    placement
        .with_dependency_receipt(span(), |receipt| {
            StateJacobianProduct::construct(
                m.continuous_numerical_source().unwrap(),
                order(m, reversed_axes),
                &SolveTargetProfile::current_dense_host(),
                span(),
                |c| {
                    c.store_compact(receipt, grid(), 40, span(), |b| {
                        let x = b.read(StateJacobianCompactInput::Seed(0), (span(), 0))?;
                        let x = b.copy(&x, (span(), 1))?;
                        if sqrt {
                            b.unary(SolveUnaryOperator::Sqrt, &x, (span(), 7))
                        } else {
                            b.binary(SolveBinaryOperator::Multiply, [&x, &x], (span(), 7))
                        }
                    })
                },
            )
        })
        .unwrap()
        .unwrap()
}

fn scalarized(m: &SolveModel, reversed_axes: bool, sqrt: bool) -> StateJacobianProduct<'_> {
    StateJacobianProduct::construct(
        m.continuous_numerical_source().unwrap(),
        order(m, reversed_axes),
        &SolveTargetProfile::current_dense_host(),
        span(),
        |c| {
            for point in 0..6 {
                let at = |operation| {
                    (
                        span(),
                        SolveFiniteCheck::Primal {
                            program: 40 + point as u32,
                            operation,
                        },
                    )
                };
                let x = c.read(StateJacobianProductInput::State(point), at(0))?;
                let x = c.copy(&x, at(1))?;
                let y = if sqrt {
                    c.unary(SolveUnaryOperator::Sqrt, &x, at(7))?
                } else {
                    c.binary(SolveBinaryOperator::Multiply, [&x, &x], at(7))?
                };
                c.store_row(5 - point, &y, span())?;
            }
            Ok(())
        },
    )
    .unwrap()
}

fn run(
    product: &StateJacobianProduct<'_>,
    states: &[f64],
) -> Result<Vec<u64>, TypedProgramEvalError> {
    let args = product
        .call_site()
        .inputs()
        .iter()
        .zip(states.iter().copied().chain([0.0]))
        .map(|(ty, x)| {
            TypedValue::construct(ty.clone(), vec![SolveValueKind::Real64(x.to_bits())]).unwrap()
        })
        .collect::<Vec<_>>();
    let output = eval_pure_call_checked(product.table(), product.call_site().owner(), &args)?;
    Ok(output[0]
        .elements()
        .iter()
        .map(|x| match x {
            SolveValueKind::Real64(bits) => *bits,
            _ => panic!("expected Real64"),
        })
        .collect())
}

#[test]
fn reversed_grid_graph_and_dependency_axes_agree_with_scalarized_source() {
    let m = six_states();
    for axes in [false, true] {
        let map = mapped(&m, axes, false);
        let scalar = scalarized(&m, axes, false);
        assert!(
            map.table().owners()[0]
                .body()
                .operations()
                .iter()
                .any(|op| matches!(op.operation(), SolveOperation::Map { .. }))
        );
        for y in [
            [0.5, -1.25, 2.0, 0.75, -3.0, 1.0],
            [-3.0, 0.0, 0.001, 7.5, 2.0, -0.5],
        ] {
            let got = run(&map, &y).unwrap();
            assert_eq!(got, run(&scalar, &y).unwrap());
            for row in 0..6 {
                for column in 0..6 {
                    let source_column = if axes { row } else { 5 - row };
                    let expected = if column == source_column {
                        2.0 * y[column]
                    } else {
                        0.0
                    };
                    assert_eq!(f64::from_bits(got[row * 6 + column]), expected);
                    assert_eq!(
                        map.dependencies().contains(row as u32, column as u32),
                        column == source_column
                    );
                    assert_eq!(
                        map.dependencies().contains(row as u32, column as u32),
                        scalar.dependencies().contains(row as u32, column as u32)
                    );
                }
            }
        }
    }
}

#[test]
fn later_mapped_primal_failure_is_the_same_first_scalarized_fault() {
    let m = six_states();
    let map = mapped(&m, false, true);
    let scalar = scalarized(&m, false, true);
    let y = [1.0, 4.0, 9.0, 16.0, -1.0, -4.0];
    let fault = run(&map, &y).unwrap_err();
    assert_eq!(fault, run(&scalar, &y).unwrap_err());
    assert!(matches!(
        fault,
        TypedProgramEvalError::NonFinite {
            check: SolveFiniteCheck::Primal {
                program: 44,
                operation: 7
            },
            element: 0,
            ..
        }
    ));
    assert_eq!(
        run(&map, &[1.0, 4.0, 9.0, 16.0, 25.0, 36.0]).unwrap(),
        run(&scalar, &[1.0, 4.0, 9.0, 16.0, 25.0, 36.0]).unwrap()
    );
}

#[test]
fn no_state_directions_still_execute_the_primal_body() {
    let m = six_states();
    let p = CompactJacobianPlacement::new(vec![2, 3], (0, vec![3, 1]), vec![], 6).unwrap();
    let product = p
        .with_dependency_receipt(span(), |receipt| {
            StateJacobianProduct::construct(
                m.continuous_numerical_source().unwrap(),
                order(&m, false),
                &SolveTargetProfile::current_dense_host(),
                span(),
                |c| {
                    c.store_compact(receipt, grid(), 80, span(), |b| {
                        let minus = b.constant(-1.0, (span(), 2))?;
                        b.unary(SolveUnaryOperator::Sqrt, &minus, (span(), 3))
                    })
                },
            )
        })
        .unwrap()
        .unwrap();
    assert_eq!(product.dependencies().nonzero_upper_bound(), Some(0));
    assert!(matches!(
        run(&product, &[0.0; 6]),
        Err(TypedProgramEvalError::NonFinite {
            check: SolveFiniteCheck::Primal {
                program: 80,
                operation: 3
            },
            element: 0,
            ..
        })
    ));
}

#[test]
fn present_numeric_zero_and_absent_direction_remain_distinct() {
    let m = model();
    let p =
        CompactJacobianPlacement::new(vec![2], (0, vec![1]), vec![(0, vec![0]), (1, vec![0])], 2)
            .unwrap();
    let mut d = grid();
    d.binders.remove(0);
    d.binders[0].upper = 2;
    for cancelled in [false, true] {
        let product = p
            .with_dependency_receipt(span(), |receipt| {
                StateJacobianProduct::construct(
                    m.continuous_numerical_source().unwrap(),
                    order(&m, false),
                    &SolveTargetProfile::current_dense_host(),
                    span(),
                    |c| {
                        c.store_compact(receipt, d.clone(), 0, span(), |b| {
                            let x = b.read(StateJacobianCompactInput::Seed(1), (span(), 0))?;
                            if cancelled {
                                b.binary(SolveBinaryOperator::Subtract, [&x, &x], (span(), 1))
                            } else {
                                b.copy(&x, (span(), 1))
                            }
                        })
                    },
                )
            })
            .unwrap()
            .unwrap();
        let one = if cancelled { 0 } else { 1.0_f64.to_bits() };
        assert_eq!(run(&product, &[2.0, 3.0]).unwrap(), vec![0, one, 0, one]);
        for row in 0..2 {
            assert!(product.dependencies().contains(row, 1));
            assert!(!product.dependencies().contains(row, 0));
        }
    }
}

#[test]
fn parameter_and_time_projection_use_the_same_captured_owner_inputs() {
    let mut m = model();
    m.problem.layout = VarLayout::from_parts_with_shapes_and_spans(
        [
            ("x".into(), scalar_slot_y(0)),
            ("p".into(), scalar_slot_p(0)),
        ]
        .into(),
        [("x".into(), vec![2]), ("p".into(), vec![])].into(),
        [("x".into(), span()), ("p".into(), span())].into(),
        2,
        1,
    )
    .unwrap();
    let p = CompactJacobianPlacement::new(vec![2], (0, vec![1]), vec![], 2).unwrap();
    let mut d = grid();
    d.binders.remove(0);
    d.binders[0].upper = 2;
    let product = p
        .with_dependency_receipt(span(), |receipt| {
            StateJacobianProduct::construct(
                m.continuous_numerical_source().unwrap(),
                order(&m, false),
                &SolveTargetProfile::current_dense_host(),
                span(),
                |c| {
                    c.store_compact(receipt, d, 90, span(), |b| {
                        let p = b.read(StateJacobianCompactInput::Parameter(0), (span(), 0))?;
                        let t = b.read(StateJacobianCompactInput::Time, (span(), 1))?;
                        let difference =
                            b.binary(SolveBinaryOperator::Subtract, [&p, &t], (span(), 2))?;
                        b.unary(SolveUnaryOperator::Sqrt, &difference, (span(), 3))
                    })
                },
            )
        })
        .unwrap()
        .unwrap();
    assert_eq!(product.parameters(), 1);
    let invoke = |parameter: f64, time: f64| {
        let args = product
            .call_site()
            .inputs()
            .iter()
            .zip([2.0, 3.0, parameter, time])
            .map(|(ty, x)| {
                TypedValue::construct(ty.clone(), vec![SolveValueKind::Real64(x.to_bits())])
                    .unwrap()
            })
            .collect::<Vec<_>>();
        eval_pure_call_checked(product.table(), product.call_site().owner(), &args)
    };
    let output = invoke(9.0, 4.0).unwrap();
    assert!(
        output[0]
            .elements()
            .iter()
            .all(|x| matches!(x, SolveValueKind::Real64(0)))
    );
    assert!(matches!(
        invoke(4.0, 9.0),
        Err(TypedProgramEvalError::NonFinite {
            check: SolveFiniteCheck::Primal {
                program: 90,
                operation: 3
            },
            element: 0,
            ..
        })
    ));
    assert_eq!(product.dependencies().nonzero_upper_bound(), Some(0));
}
