//! Compact products consume the exact incidence proof before numeric demand.

use super::zero_products::expression_call;
use super::*;

fn fixture(
    coefficient_shape: &[u32],
    literals: &[f64],
    factor_shape: &[u32],
    operator: dae::BinaryOperator,
    swapped: bool,
) -> dae::Dae {
    dag_fixture(
        coefficient_shape,
        literals,
        factor_shape,
        operator,
        swapped,
        0,
    )
}

fn dag_fixture(
    coefficient_shape: &[u32],
    literals: &[f64],
    factor_shape: &[u32],
    operator: dae::BinaryOperator,
    swapped: bool,
    depth: usize,
) -> dae::Dae {
    let mut sources = SourceMap::new();
    let file = sources.add("partial_product.mo", "coefficient * x");
    let at = dae::DaeProvenance::source(Span::from_offsets(file, 0, 15)).unwrap();
    dae::Dae::construct(sources, |model| {
        let ty = model.types(|types| {
            types.derived(
                dae::ValueType::array(dae::ScalarType::Real, factor_shape.to_vec()),
                at,
            )
        })?;
        let x = model.variables(|variables| {
            variables.algebraic(VarName::new("x"), ty, at, Default::default())
        })?;
        model.expressions(|e| {
            let coefficient = literal_tensor(e, coefficient_shape, literals, at)?;
            let mut factor = e.at(at).coordinate(dae::CoordinateInput::Algebraic(x))?;
            for _ in 0..depth {
                factor = e.at(at).binary(dae::BinaryOperator::Add, factor, factor)?;
            }
            let (lhs, rhs) = if swapped {
                (factor, coefficient)
            } else {
                (coefficient, factor)
            };
            e.at(at).binary(operator, lhs, rhs)?;
            Ok(())
        })
    })
    .unwrap()
}

fn literal_tensor<'dae>(
    e: &mut dae::Expressions<'_, 'dae>,
    dimensions: &[u32],
    values: &[f64],
    at: dae::DaeProvenance,
) -> Result<dae::ExprId<'dae>, dae::DaeConstructionError> {
    if dimensions.is_empty() {
        return e.at(at).literal(dae::DaeLiteral::Real(values[0]));
    }
    let width: usize = dimensions[1..].iter().map(|&n| n as usize).product();
    let mut elements = Vec::new();
    for values in values.chunks(width) {
        elements.push(literal_tensor(e, &dimensions[1..], values, at)?);
    }
    e.at(at).array(elements)
}

fn run(model: &dae::Dae, input: &[f64]) -> Vec<solve::SolveValueKind> {
    let table = expression_call(model, false);
    let owner = &table.owners().last().unwrap();
    let input = rumoca_eval_solve::TypedValue::construct(
        owner.inputs()[0].clone(),
        input
            .iter()
            .map(|x| solve::SolveValueKind::Real64(x.to_bits()))
            .collect(),
    )
    .unwrap();
    rumoca_eval_solve::eval_pure_call_checked(&table, owner.id(), &[input]).unwrap()[0]
        .elements()
        .to_vec()
}

#[test]
fn partial_dot_skips_nan_and_keeps_first_term_negative_zero() {
    let dot = fixture(
        &[2],
        &[0.0, -2.0],
        &[2],
        dae::BinaryOperator::Multiply,
        false,
    );
    assert_eq!(
        run(&dot, &[f64::NAN, 0.0]),
        [solve::SolveValueKind::Real64((-0.0f64).to_bits())]
    );
    let ordered = fixture(
        &[4],
        &[0.0, 1.0, 1.0, 1.0],
        &[4],
        dae::BinaryOperator::Multiply,
        false,
    );
    assert_eq!(
        run(&ordered, &[f64::NAN, 1e16, -1e16, 1.0]),
        [solve::SolveValueKind::Real64(1.0f64.to_bits())]
    );
}

#[test]
fn partial_elementwise_and_both_scalar_scales_skip_nan() {
    for swapped in [false, true] {
        let elementwise = fixture(
            &[2],
            &[0.0, 2.0],
            &[2],
            dae::BinaryOperator::ElementwiseMultiply,
            swapped,
        );
        assert_eq!(
            run(&elementwise, &[f64::NAN, 3.0]),
            [
                solve::SolveValueKind::Real64(0),
                solve::SolveValueKind::Real64(6.0f64.to_bits())
            ]
        );
        let scale = fixture(
            &[2],
            &[0.0, 2.0],
            &[],
            dae::BinaryOperator::Multiply,
            swapped,
        );
        assert_eq!(
            run(&scale, &[3.0]),
            [
                solve::SolveValueKind::Real64(0),
                solve::SolveValueKind::Real64(6.0f64.to_bits())
            ]
        );
    }
}

#[test]
fn partial_matrix_products_keep_each_original_coordinate() {
    for swapped in [false, true] {
        let model = fixture(
            &[2, 2],
            &[0.0, 2.0, 3.0, 0.0],
            &[2],
            dae::BinaryOperator::Multiply,
            swapped,
        );
        let output = run(&model, &[f64::NAN, 4.0]);
        assert_eq!(
            output[0],
            solve::SolveValueKind::Real64(if swapped { 12.0f64 } else { 8.0f64 }.to_bits())
        );
        assert!(
            matches!(output[1], solve::SolveValueKind::Real64(bits) if f64::from_bits(bits).is_nan())
        );
    }
    let model = fixture(
        &[2, 2],
        &[0.0, 2.0, 3.0, 0.0],
        &[2, 2],
        dae::BinaryOperator::Multiply,
        false,
    );
    let output = run(&model, &[f64::NAN, 5.0, 4.0, f64::NAN]);
    assert_eq!(output[0], solve::SolveValueKind::Real64(8.0f64.to_bits()));
    assert_eq!(output[3], solve::SolveValueKind::Real64(15.0f64.to_bits()));
    for index in [1, 2] {
        assert!(
            matches!(output[index], solve::SolveValueKind::Real64(bits) if f64::from_bits(bits).is_nan())
        );
    }
}

fn operation_count(program: &solve::TypedProgram) -> usize {
    program
        .operations()
        .iter()
        .map(|op| {
            1 + match op.operation() {
                solve::SolveOperation::Conditional {
                    if_true, if_false, ..
                } => operation_count(if_true.body()) + operation_count(if_false.body()),
                solve::SolveOperation::Map { body, .. } => operation_count(body.body()),
                solve::SolveOperation::Fold {
                    transition,
                    continuation,
                    ..
                } => {
                    operation_count(transition.body())
                        + continuation
                            .as_ref()
                            .map_or(0, |body| operation_count(body.body()))
                }
                _ => 0,
            }
        })
        .sum()
}

#[test]
fn selected_shared_source_dag_issues_each_arithmetic_node_once() {
    let counts = [10, 20].map(|depth| {
        let model = dag_fixture(
            &[2],
            &[0.0, 2.0],
            &[2],
            dae::BinaryOperator::Multiply,
            false,
            depth,
        );
        let table = expression_call(&model, false);
        assert_eq!(
            run(&model, &[f64::NAN, 3.0]),
            [solve::SolveValueKind::Real64(
                (6.0 * 2.0f64.powi(depth as i32)).to_bits()
            )]
        );
        table_operation_count(&table)
    });
    assert!(
        counts[1] <= 3 * counts[0],
        "shared arithmetic owners must grow with source nodes: {counts:?}"
    );
}

fn compact_fixture(extent: u32) -> dae::Dae {
    let mut sources = SourceMap::new();
    let file = sources.add("compact_partial_product.mo", "[0;p]*x");
    let at = dae::DaeProvenance::source(Span::from_offsets(file, 0, 7)).unwrap();
    dae::Dae::construct(sources, |model| {
        let (wide, one) = model.types(|types| {
            Ok((
                types.derived(dae::ValueType::array(dae::ScalarType::Real, [extent]), at)?,
                types.derived(dae::ValueType::array(dae::ScalarType::Real, [1]), at)?,
            ))
        })?;
        let (p, x) = model.variables(|variables| {
            Ok((
                variables.parameter(
                    VarName::new("p"),
                    wide,
                    at,
                    dae::VariableAttributes {
                        is_tunable: true,
                        ..Default::default()
                    },
                )?,
                variables.algebraic(VarName::new("x"), one, at, Default::default())?,
            ))
        })?;
        model.expressions(|e| {
            let zero = e.at(at).literal(dae::DaeLiteral::Real(0.0))?;
            let p = e.at(at).coordinate(dae::CoordinateInput::Parameter(p))?;
            let coefficient = e
                .at(at)
                .builtin(dae::PureBuiltin::PromotedCat1, [zero, p])?;
            let x = e.at(at).coordinate(dae::CoordinateInput::Algebraic(x))?;
            e.at(at)
                .binary(dae::BinaryOperator::Multiply, coefficient, x)?;
            Ok(())
        })
    })
    .unwrap()
}

#[test]
fn partial_product_metadata_does_not_expand_a_million_lane_extent() {
    let tables = [2, 1_000_000].map(|extent| expression_call(&compact_fixture(extent), false));
    let counts = tables.each_ref().map(table_operation_count);
    assert_eq!(counts[0], counts[1]);
    assert!(
        counts[1] < 200,
        "one map and fold must stay compact: {counts:?}"
    );
    let topology = tables.each_ref().map(|table| {
        let mut value = serde_json::to_value(table).unwrap();
        erase_numbers(&mut value);
        value
    });
    assert_eq!(
        topology[0], topology[1],
        "only fixed-width shape/placement numbers may change"
    );
}

fn erase_numbers(value: &mut serde_json::Value) {
    match value {
        serde_json::Value::Number(_) => *value = serde_json::json!(0),
        serde_json::Value::Array(values) => values.iter_mut().for_each(erase_numbers),
        serde_json::Value::Object(values) => values.values_mut().for_each(erase_numbers),
        _ => {}
    }
}

fn mixed_coefficient() -> dae::Dae {
    let mut sources = SourceMap::new();
    let file = sources.add("mixed_coefficient.mo", "1.0 * {0.0,i+1}");
    let at =
        |offset| dae::DaeProvenance::source(Span::from_offsets(file, offset, offset + 1)).unwrap();
    dae::Dae::construct(sources, |model| {
        let integer = model.types(|types| {
            types.derived(dae::ValueType::scalar(dae::ScalarType::Integer), at(11))
        })?;
        let i = model.variables(|variables| {
            variables.parameter(
                VarName::new("i"),
                integer,
                at(11),
                dae::VariableAttributes {
                    is_tunable: true,
                    ..Default::default()
                },
            )
        })?;
        model.expressions(|e| {
            let i = e
                .at(at(11))
                .coordinate(dae::CoordinateInput::Parameter(i))?;
            let one = e.at(at(13)).literal(dae::DaeLiteral::Integer(1))?;
            let sum = e.at(at(12)).binary(dae::BinaryOperator::Add, i, one)?;
            let zero = e.at(at(7)).literal(dae::DaeLiteral::Real(0.0))?;
            let coefficient = e.at(at(6)).array([zero, sum])?;
            let factor = e.at(at(0)).literal(dae::DaeLiteral::Real(1.0))?;
            e.at(at(4))
                .binary(dae::BinaryOperator::Multiply, factor, coefficient)?;
            Ok(())
        })
    })
    .unwrap()
}

#[test]
fn partial_coefficient_keeps_integer_fault_and_array_real_promotion() {
    let model = mixed_coefficient();
    let candidate = expression_call(&model, false);
    let reference = expression_call(&model, true);
    for value in [1, i64::MAX] {
        let eval = |table: &solve::SolvePureCallTable| {
            let owner = &table.owners().last().unwrap();
            let input = rumoca_eval_solve::TypedValue::construct(
                owner.inputs()[0].clone(),
                vec![
                    solve::SolveValue::integer(arithmetic_profile(), value)
                        .unwrap()
                        .kind(),
                ],
            )
            .unwrap();
            rumoca_eval_solve::eval_pure_call_checked(table, owner.id(), &[input])
        };
        let expected = eval(&reference);
        if value == i64::MAX {
            assert!(matches!(
                expected,
                Err(rumoca_eval_solve::TypedProgramEvalError::IntegerArithmetic { .. })
            ));
            assert_eq!(eval(&candidate).unwrap_err(), expected.unwrap_err());
        } else {
            assert_eq!(eval(&candidate).unwrap(), expected.unwrap());
        }
    }
}

fn indexed_coefficient(row: i64) -> dae::Dae {
    let mut sources = SourceMap::new();
    let file = sources.add("indexed_coefficient.mo", "x * [0;A[row,:]]");
    let at =
        |offset| dae::DaeProvenance::source(Span::from_offsets(file, offset, offset + 1)).unwrap();
    dae::Dae::construct(sources, |model| {
        model.expressions(|e| {
            let matrix = literal_tensor(e, &[2, 2], &[1.0, 2.0, 3.0, 4.0], at(7))?;
            let row = e.at(at(9)).literal(dae::DaeLiteral::Integer(row))?;
            let selected = e.at(at(8)).index(
                matrix,
                [
                    dae::Subscript::Index {
                        expression: row,
                        provenance: at(9),
                    },
                    dae::Subscript::Whole { provenance: at(11) },
                ],
            )?;
            let zero = e.at(at(5)).literal(dae::DaeLiteral::Real(0.0))?;
            let coefficient = e
                .at(at(4))
                .builtin(dae::PureBuiltin::PromotedCat1, [zero, selected])?;
            let x = e.at(at(0)).literal(dae::DaeLiteral::Real(1.0))?;
            e.at(at(2))
                .binary(dae::BinaryOperator::Multiply, x, coefficient)?;
            Ok(())
        })
    })
    .unwrap()
}

#[test]
fn selected_coefficient_retains_original_index_validation() {
    for row in [0, 3] {
        let model = indexed_coefficient(row);
        let evaluate = |reference| {
            let table = expression_call(&model, reference);
            rumoca_eval_solve::eval_pure_call_checked(
                &table,
                table.owners().last().unwrap().id(),
                &[],
            )
            .unwrap_err()
        };
        let reference = evaluate(true);
        assert!(
            matches!(reference,rumoca_eval_solve::TypedProgramEvalError::InvalidCheckedProgram {operation,..} if operation == if row==0 {"resolve aggregate view index"} else {"project aggregate view"})
        );
        assert_eq!(evaluate(false), reference);
    }
}

fn transformed_fixture(transpose: bool) -> dae::Dae {
    let mut sources = SourceMap::new();
    let file = sources.add("transformed_product.mo", "transpose(A)*x or A[1,:]*x");
    let at = dae::DaeProvenance::source(Span::from_offsets(file, 0, 25)).unwrap();
    dae::Dae::construct(sources, |model| {
        let vector = model
            .types(|types| types.derived(dae::ValueType::array(dae::ScalarType::Real, [2]), at))?;
        let x = model.variables(|variables| {
            variables.algebraic(VarName::new("x"), vector, at, Default::default())
        })?;
        model.expressions(|e| {
            let matrix = literal_tensor(e, &[2, 2], &[0.0, 2.0, 3.0, 0.0], at)?;
            let coefficient = if transpose {
                e.at(at).builtin(dae::PureBuiltin::Transpose, [matrix])?
            } else {
                let one = e.at(at).literal(dae::DaeLiteral::Integer(1))?;
                e.at(at).index(
                    matrix,
                    [
                        dae::Subscript::Index {
                            expression: one,
                            provenance: at,
                        },
                        dae::Subscript::Whole { provenance: at },
                    ],
                )?
            };
            let x = e.at(at).coordinate(dae::CoordinateInput::Algebraic(x))?;
            e.at(at)
                .binary(dae::BinaryOperator::Multiply, coefficient, x)?;
            Ok(())
        })
    })
    .unwrap()
}

#[test]
fn compact_transpose_and_literal_view_keep_mask_and_value_axes_together() {
    assert_eq!(
        run(&transformed_fixture(false), &[f64::NAN, 4.0]),
        [solve::SolveValueKind::Real64(8.0f64.to_bits())]
    );
    let output = run(&transformed_fixture(true), &[f64::NAN, 4.0]);
    assert_eq!(output[0], solve::SolveValueKind::Real64(12.0f64.to_bits()));
    assert!(
        matches!(output[1],solve::SolveValueKind::Real64(bits) if f64::from_bits(bits).is_nan())
    );
}

#[test]
fn compact_promoted_concatenation_selects_its_original_numeric_rows() {
    let table = expression_call(&compact_fixture(2), false);
    let owner = &table.owners().last().unwrap();
    let inputs = [vec![2.0f64, 3.0], vec![4.0]]
        .into_iter()
        .zip(owner.inputs())
        .map(|(values, ty)| {
            rumoca_eval_solve::TypedValue::construct(
                ty.clone(),
                values
                    .into_iter()
                    .map(|v| solve::SolveValueKind::Real64(v.to_bits()))
                    .collect(),
            )
            .unwrap()
        })
        .collect::<Vec<_>>();
    assert_eq!(
        rumoca_eval_solve::eval_pure_call_checked(&table, owner.id(), &inputs).unwrap()[0]
            .elements(),
        [
            solve::SolveValueKind::Real64(0),
            solve::SolveValueKind::Real64(8.0f64.to_bits()),
            solve::SolveValueKind::Real64(12.0f64.to_bits())
        ]
    );
}

fn array_dag(depth: usize) -> dae::Dae {
    let mut sources = SourceMap::new();
    let file = sources.add("shared_array_dag.mo", "{zero,one} .* {left,right}");
    let at = dae::DaeProvenance::source(Span::from_offsets(file, 0, 25)).unwrap();
    dae::Dae::construct(sources, |model| {
        let real = model
            .types(|types| types.derived(dae::ValueType::scalar(dae::ScalarType::Real), at))?;
        let (x, y) = model.variables(|v| {
            Ok((
                v.algebraic(VarName::new("x"), real, at, Default::default())?,
                v.algebraic(VarName::new("y"), real, at, Default::default())?,
            ))
        })?;
        model.expressions(|e| {
            let x = e.at(at).coordinate(dae::CoordinateInput::Algebraic(x))?;
            let y = e.at(at).coordinate(dae::CoordinateInput::Algebraic(y))?;
            let mut left = e.at(at).array([x, y])?;
            let mut right = e.at(at).array([y, x])?;
            for _ in 1..depth {
                (left, right) = (
                    e.at(at).array([left, right])?,
                    e.at(at).array([right, left])?,
                );
            }
            let mut zero = e.at(at).literal(dae::DaeLiteral::Real(0.0))?;
            let mut one = e.at(at).literal(dae::DaeLiteral::Real(1.0))?;
            for _ in 1..depth {
                zero = e.at(at).array([zero, zero])?;
                one = e.at(at).array([one, one])?;
            }
            let coefficient = e.at(at).array([zero, one])?;
            e.at(at)
                .binary(dae::BinaryOperator::ElementwiseMultiply, coefficient, left)?;
            Ok(())
        })
    })
    .unwrap()
}

#[test]
fn selected_array_dag_metadata_tracks_shared_source_not_expanded_lanes() {
    let counts =
        [4, 8].map(|depth| table_operation_count(&expression_call(&array_dag(depth), false)));
    eprintln!("selected Array DAG depth4/depth8 all-owner operation counts: {counts:?}");
    assert!(
        counts[1] <= 3 * counts[0],
        "doubling the source DAG must not expand its 16x lane count: {counts:?}"
    );
}

fn table_operation_count(table: &solve::SolvePureCallTable) -> usize {
    table
        .owners()
        .iter()
        .map(|owner| operation_count(owner.body()))
        .sum()
}

#[test]
fn shared_empty_source_has_no_selected_point_owner() {
    let mut sources = SourceMap::new();
    let file = sources.add("shared_empty.mo", "{empty,x} .* {-x,-x}");
    let at = dae::DaeProvenance::source(Span::from_offsets(file, 0, 20)).unwrap();
    let model = dae::Dae::construct(sources, |model| {
        let empty = model
            .types(|types| types.derived(dae::ValueType::array(dae::ScalarType::Real, [0]), at))?;
        let x =
            model.variables(|v| v.algebraic(VarName::new("x"), empty, at, Default::default()))?;
        model.expressions(|e| {
            let empty = e.at(at).empty_array(empty)?;
            let x = e.at(at).coordinate(dae::CoordinateInput::Algebraic(x))?;
            let coefficient = e.at(at).array([empty, x])?;
            let negative = e.at(at).unary(dae::UnaryOperator::Negate, x)?;
            let factor = e.at(at).array([negative, negative])?;
            e.at(at).binary(
                dae::BinaryOperator::ElementwiseMultiply,
                coefficient,
                factor,
            )?;
            Ok(())
        })
    })
    .unwrap();
    model.inspect(|view| {
        let root = view.expression_id(view.expression_count() - 1).unwrap();
        let mut registry = PureCallRegistry::new();
        crate::lower::typed_functions::zero_products::register_selections(
            &mut registry.table,
            view,
            [root],
            &mut registry.identities,
        )
        .unwrap();
        assert!(
            registry.finish().owners().is_empty(),
            "an empty domain has no point helper"
        );
    });
}

#[test]
fn selected_array_dag_helpers_evaluate_the_original_masked_points() {
    let depth = 9;
    let table = expression_call(&array_dag(depth), false);
    let encoded = serde_json::to_vec(&table).unwrap();
    let table: solve::SolvePureCallTable = serde_json::from_slice(&encoded).unwrap();
    assert_eq!(
        serde_json::to_vec(&table).unwrap(),
        encoded,
        "checked wire replay retains the exact owner graph"
    );
    assert!(
        table.owners().len() <= 4 * depth,
        "only retained source nodes may own a helper"
    );
    let owner = table.owners().last().unwrap();
    for (x, y) in [(1.25f64, -0.0), (f64::NAN, 3.0)] {
        let inputs = [x, y]
            .into_iter()
            .zip(owner.inputs())
            .map(|(value, ty)| {
                rumoca_eval_solve::TypedValue::construct(
                    ty.clone(),
                    vec![solve::SolveValueKind::Real64(value.to_bits())],
                )
                .unwrap()
            })
            .collect::<Vec<_>>();
        let result =
            rumoca_eval_solve::eval_pure_call_checked(&table, owner.id(), &inputs).unwrap();
        let extent = 1usize << depth;
        assert_eq!(result[0].elements().len(), extent);
        for (index, actual) in result[0].elements().iter().enumerate() {
            let expected = if index < extent / 2 {
                0.0
            } else if index.count_ones() % 2 == 0 {
                x
            } else {
                y
            };
            if expected.is_nan() {
                assert!(
                    matches!(actual,solve::SolveValueKind::Real64(bits) if f64::from_bits(*bits).is_nan())
                );
            } else {
                assert_eq!(
                    *actual,
                    solve::SolveValueKind::Real64(expected.to_bits()),
                    "original source point {index}"
                );
            }
        }
    }
}
