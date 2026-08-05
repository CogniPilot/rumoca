use rumoca_core::{SourceMap, Span, VarName};

use super::*;

#[test]
fn identity_element_is_integer_and_diagonal_by_index_equality() {
    let diagonal =
        lower_identity_element(&[gast::Expression::Integer(2), gast::Expression::Integer(2)]);
    assert_eq!(diagonal.scalar_type, gast::ScalarType::Integer);
    assert_eq!(diagonal.expression, gast::Expression::Integer(1));

    let off_diagonal =
        lower_identity_element(&[gast::Expression::Integer(1), gast::Expression::Integer(2)]);
    assert_eq!(off_diagonal.expression, gast::Expression::Integer(0));
}

#[test]
fn vector_projection_preserves_the_unique_non_unit_dimension() {
    let index = gast::Expression::Integer(2);
    assert_eq!(
        vector_operand_projection(&[1, 3, 1], std::slice::from_ref(&index)),
        [
            gast::Expression::Integer(1),
            index,
            gast::Expression::Integer(1)
        ]
    );
    assert!(vector_operand_projection(&[], &[gast::Expression::Integer(1)]).is_empty());
}

#[test]
fn static_integer_index_arithmetic_folds_without_overflow() {
    let expression = gast::Expression::binary(
        gast::BinaryOp::Add,
        gast::Expression::Integer(1),
        gast::Expression::binary(
            gast::BinaryOp::Mul,
            gast::Expression::Integer(2),
            gast::Expression::Integer(3),
        ),
    );
    assert_eq!(constant_integer(&expression), Some(7));
    assert_eq!(
        constant_integer(&gast::Expression::binary(
            gast::BinaryOp::Add,
            gast::Expression::Integer(i64::MAX),
            gast::Expression::Integer(1),
        )),
        None
    );
}

#[test]
fn causally_defined_output_remains_an_interface_and_gets_an_assignment() {
    let mut sources = SourceMap::new();
    let text = "input Real u; output Real y; equation y = u;";
    let source = sources.add("output-definition.mo", text);
    let span = Span::from_offsets(source, 0, text.len());
    let provenance = dae::DaeProvenance::source(span).unwrap();
    let model = dae::Dae::construct(sources, |dae| {
        let real = dae.types(|types| {
            types.derived(dae::ValueType::scalar(dae::ScalarType::Real), provenance)
        })?;
        let (input, output) = dae.variables(|variables| {
            Ok((
                variables.input(
                    VarName::new("u"),
                    real,
                    dae::InputVariability::Continuous,
                    provenance,
                    dae::VariableAttributes {
                        causality: dae::VariableCausality::Input,
                        ..Default::default()
                    },
                )?,
                variables.output(
                    VarName::new("y"),
                    real,
                    provenance,
                    dae::VariableAttributes {
                        causality: dae::VariableCausality::Output,
                        ..Default::default()
                    },
                )?,
            ))
        })?;
        let residual = dae.expressions(|expressions| {
            let input = expressions
                .at(provenance)
                .coordinate(dae::CoordinateInput::Input(input))?;
            let output = expressions
                .at(provenance)
                .coordinate(dae::CoordinateInput::Algebraic(output))?;
            expressions
                .at(provenance)
                .binary(dae::BinaryOperator::Subtract, output, input)
        })?;
        dae.continuous(|continuous| {
            continuous.equation(provenance, |equation| equation.residual(residual))?;
            Ok(())
        })
    })
    .unwrap();
    model.inspect(|view| {
        let classified = classify_variables(view).unwrap();
        assert!(classified.iter().any(|variable| {
            variable.class == VariableClass::Output && variable.variable.name().as_str() == "y"
        }));
        let by_id = classified
            .iter()
            .map(|variable| (variable.id.index(), variable.clone()))
            .collect::<HashMap<_, _>>();
        let mut statements = Vec::new();
        append_causal_output_assignments(
            view,
            &classified,
            &by_id,
            &HashMap::new(),
            &mut statements,
        )
        .unwrap();
        assert_eq!(statements.len(), 1);
        assert!(matches!(
            &statements[0].node,
            gast::Statement::Assignment { target, value }
                if matches!(target, gast::Reference::State(parts)
                    if parts.len() == 1 && parts[0].name.lexeme() == "y")
                    && matches!(value, gast::Expression::Ref(gast::Reference::State(parts))
                        if parts.len() == 1 && parts[0].name.lexeme() == "u")
        ));
    });
}

#[test]
fn function_assertion_is_detected_before_expression_inlining() {
    let mut sources = SourceMap::new();
    let text = "function f output Real y; algorithm assert(true, \"valid\"); y := 0.0; end f;";
    let source = sources.add("assertion.mo", text);
    let span = Span::from_offsets(source, 0, text.len());
    let provenance = dae::DaeProvenance::source(span).unwrap();
    let model = dae::Dae::construct(sources, |dae| {
        let real = dae.types(|types| {
            types.derived(dae::ValueType::scalar(dae::ScalarType::Real), provenance)
        })?;
        dae.function(
            dae::FunctionSignature::new(VarName::new("f"), [], [real], provenance),
            |dae, reservation| {
                let output = dae.functions(|functions| {
                    functions.output(&reservation, VarName::new("y"), 0, provenance)
                })?;
                let mut body =
                    dae.functions(|functions| functions.begin(reservation, provenance))?;
                let condition = dae.expressions(|expressions| {
                    expressions
                        .at(provenance)
                        .literal(dae::DaeLiteral::Boolean(true))
                })?;
                let message = dae.expressions(|expressions| {
                    expressions
                        .at(provenance)
                        .literal(dae::DaeLiteral::String("valid".to_owned()))
                })?;
                dae.functions(|functions| {
                    functions.assertion(&mut body, condition, message, provenance)
                })?;
                let zero = dae.expressions(|expressions| {
                    expressions
                        .at(provenance)
                        .literal(dae::DaeLiteral::Real(0.0))
                })?;
                dae.functions(|functions| functions.assign(&mut body, output, zero, provenance))?;
                dae.functions(|functions| functions.define(body, provenance))
            },
        )?;
        Ok(())
    })
    .unwrap();
    model.inspect(|view| {
        let function = view.function(view.function_id(0).unwrap()).unwrap();
        assert_eq!(first_function_assertion(function.statements()), Some(span));
    });
}

#[test]
fn sum_reduction_projects_the_tensor_in_row_major_order() {
    let mut sources = SourceMap::new();
    let text = "sum({1.0, 2.0, 3.0})";
    let source = sources.add("sum.mo", text);
    let span = Span::from_offsets(source, 0, text.len());
    let provenance = dae::DaeProvenance::source(span).unwrap();
    let model = dae::Dae::construct(sources, |dae| {
        dae.expressions(|expressions| {
            let values = [1.0, 2.0, 3.0]
                .into_iter()
                .map(|value| {
                    expressions
                        .at(provenance)
                        .literal(dae::DaeLiteral::Real(value))
                })
                .collect::<Result<Vec<_>, _>>()?;
            let array = expressions.at(provenance).array(values)?;
            expressions
                .at(provenance)
                .builtin(dae::PureBuiltin::Sum, [array])?;
            Ok(())
        })
    })
    .unwrap();
    model.inspect(|view| {
        let sum = (0..view.expression_count())
            .filter_map(|index| view.expression_id(index))
            .find(|id| {
                matches!(
                    view.expression(*id).unwrap().operation(),
                    dae::ExpressionOperation::Builtin {
                        builtin: dae::PureBuiltin::Sum,
                        ..
                    }
                )
            })
            .unwrap();
        let variables = HashMap::new();
        let previous = HashMap::new();
        let mut lowerer = ExpressionLowerer::new(view, &variables, &previous);
        let expected = gast::Expression::binary(
            gast::BinaryOp::Add,
            gast::Expression::binary(
                gast::BinaryOp::Add,
                gast::Expression::Real(1.0),
                gast::Expression::Real(2.0),
            ),
            gast::Expression::Real(3.0),
        );
        assert_eq!(lowerer.lower(sum).unwrap().expression, expected);
    });
}

#[test]
fn array_update_projects_updated_and_historical_elements() {
    let mut sources = SourceMap::new();
    let source = sources.add("array-update.mo", "x[2] := 9.0");
    let span = Span::from_offsets(source, 0, 11);
    let provenance = dae::DaeProvenance::source(span).unwrap();
    let model = dae::Dae::construct(sources, |dae| {
        dae.expressions(|expressions| {
            let one = expressions
                .at(provenance)
                .literal(dae::DaeLiteral::Real(1.0))?;
            let two = expressions
                .at(provenance)
                .literal(dae::DaeLiteral::Real(2.0))?;
            let three = expressions
                .at(provenance)
                .literal(dae::DaeLiteral::Real(3.0))?;
            let values = [one, two, three];
            let base = expressions.at(provenance).array(values)?;
            let index = expressions
                .at(provenance)
                .literal(dae::DaeLiteral::Integer(2))?;
            let value = expressions
                .at(provenance)
                .literal(dae::DaeLiteral::Real(9.0))?;
            expressions.at(provenance).array_update(
                base,
                value,
                [dae::Subscript::Index {
                    expression: index,
                    provenance,
                }],
            )?;
            Ok(())
        })
    })
    .unwrap();
    model.inspect(|view| {
        let update = (0..view.expression_count())
            .filter_map(|index| view.expression_id(index))
            .find(|id| {
                matches!(
                    view.expression(*id).unwrap().operation(),
                    dae::ExpressionOperation::ArrayUpdate { .. }
                )
            })
            .unwrap();
        let variables = HashMap::new();
        let previous = HashMap::new();
        let mut lowerer = ExpressionLowerer::new(view, &variables, &previous);
        assert_eq!(
            lowerer.lower_element(update, &[1]).unwrap().expression,
            gast::Expression::Real(1.0)
        );
        assert_eq!(
            lowerer.lower_element(update, &[2]).unwrap().expression,
            gast::Expression::Real(9.0)
        );
        assert_eq!(
            lowerer.lower_element(update, &[3]).unwrap().expression,
            gast::Expression::Real(3.0)
        );
    });
}

#[test]
fn comprehension_projects_checked_binder_values() {
    let mut sources = SourceMap::new();
    let text = "{i for i in -1:2:3}";
    let source = sources.add("comprehension.mo", text);
    let span = Span::from_offsets(source, 0, text.len());
    let provenance = dae::DaeProvenance::source(span).unwrap();
    let model = dae::Dae::construct(sources, |dae| {
        let domain = dae.domains(|domains| {
            domains.structured(
                rumoca_core::StructuredIndexDomain {
                    binders: vec![rumoca_core::StructuredIndexBinder {
                        id: 0,
                        display_name: "i".to_owned(),
                        lower: -1,
                        upper: 3,
                        step: 2,
                    }],
                },
                provenance,
            )
        })?;
        let binder = dae.domains(|domains| domains.binder(domain, 0, provenance))?;
        let body = dae.expressions(|expressions| expressions.at(provenance).binder(binder))?;
        dae.expressions(|expressions| {
            expressions.at(provenance).comprehension(domain, body)?;
            Ok(())
        })
    })
    .unwrap();
    model.inspect(|view| {
        let comprehension = (0..view.expression_count())
            .filter_map(|index| view.expression_id(index))
            .find(|id| {
                matches!(
                    view.expression(*id).unwrap().operation(),
                    dae::ExpressionOperation::Comprehension { .. }
                )
            })
            .unwrap();
        let variables = HashMap::new();
        let previous = HashMap::new();
        let mut lowerer = ExpressionLowerer::new(view, &variables, &previous);
        for (ordinal, expected) in [(1, -1), (2, 1), (3, 3)] {
            assert_eq!(
                lowerer
                    .lower_element(comprehension, &[ordinal])
                    .unwrap()
                    .expression,
                gast::Expression::Integer(expected)
            );
        }
    });
}
