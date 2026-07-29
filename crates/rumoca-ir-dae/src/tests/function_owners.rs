use super::*;

#[test]
fn functions_conditions_and_generated_runtime_nodes_use_the_same_arena() {
    let source =
        TestSource::new("function f input Real u; output Real y; end f; when x > 0 then end when;");
    let function_declaration = source.source("function f", 0);
    let literal_span = source.source("0", 0);
    let condition_owner = source.source("x > 0", 0);
    let clock_generated = DaeProvenance::generated(
        DaeGeneration::ClockLowering,
        source.span("when x > 0 then end when", 0),
    )
    .unwrap();
    let delay_generated =
        DaeProvenance::generated(DaeGeneration::DelayLowering, source.span("x > 0", 0)).unwrap();

    let dae = Dae::construct(source.map, |dae| {
        let real = dae.types(|types| {
            types.intern(
                TypeId::new(0),
                ValueType::scalar(ScalarType::Real),
                function_declaration,
            )
        })?;
        let literal =
            dae.expressions(|expr| expr.at(literal_span).literal(DaeLiteral::Real(0.0)))?;
        let (function, ()) = dae.function(
            FunctionSignature::new(VarName::new("f"), [real], [real], function_declaration),
            |dae, reservation| {
                let parameter = dae.functions(|functions| {
                    functions.parameter(&reservation, VarName::new("u"), 0, function_declaration)
                })?;
                let output = dae.functions(|functions| {
                    functions.output(&reservation, VarName::new("y"), 0, function_declaration)
                })?;
                let parameter_value = dae.expressions(|expr| {
                    expr.at(function_declaration).function_parameter(parameter)
                })?;
                let mut body =
                    dae.functions(|functions| functions.begin(reservation, function_declaration))?;
                dae.functions(|functions| {
                    functions.assign(&mut body, output, parameter_value, function_declaration)
                })?;
                dae.functions(|functions| functions.define(body, function_declaration))
            },
        )?;

        let condition = dae.conditions(|conditions| conditions.reserve(condition_owner))?;
        let condition_value =
            dae.expressions(|expr| expr.at(condition_owner).literal(DaeLiteral::Boolean(true)))?;
        dae.conditions(|conditions| {
            conditions.define(
                condition,
                ConditionInput::Discrete(condition_value),
                condition_owner,
            )
        })?;
        let delay_time =
            dae.expressions(|expr| expr.at(delay_generated).literal(DaeLiteral::Real(1.0)))?;
        let delay = dae.temporal(|temporal| {
            let positive = temporal.positive_parameter(delay_time, 1.0, delay_generated)?;
            temporal.delay(literal, positive, delay_generated, delay_generated)
        })?;

        dae.expressions(|expr| {
            let _call = expr.at(function_declaration).call(function, 0, [literal])?;
            let _condition = expr
                .at(condition_owner)
                .coordinate(CoordinateInput::Condition(condition))?;
            let _clock = expr.at(clock_generated).coordinate(CoordinateInput::Time)?;
            let _delay = delay.expression();
            Ok(())
        })
    })
    .expect("all owners share one arena");

    dae.inspect(assert_function_runtime_arena);

    let encoded = serde_json::to_string(&dae).unwrap();
    let decoded: Dae = serde_json::from_str(&encoded).unwrap();
    decoded.inspect(|view| {
        assert_eq!(view.expression_count(), 8);
        assert_eq!(view.variable_count(), 0);
    });
}

fn assert_function_runtime_arena(view: DaeView<'_>) {
    assert_eq!(view.expression_count(), 8);
    let function = view.function(view.function_id(0).unwrap()).unwrap();
    assert_eq!(function.name().as_str(), "f");
    assert_eq!(function.parameter_types().len(), 1);
    assert_eq!(function.result_types().len(), 1);
    assert_eq!(function.result_values().len(), 1);
    let definition = function.result_values().get(0).unwrap();
    assert_eq!(
        view.function_definition(definition.id()).unwrap().rhs(),
        definition.rhs()
    );
    assert_eq!(
        function.result_values().iter().next().unwrap().id(),
        definition.id()
    );
    assert_eq!(
        function.result_values().rhs_iter().next(),
        Some(definition.rhs())
    );
    let result = view.expression(definition.rhs()).unwrap();
    assert_eq!(result.function_scope(), view.function_id(0));
    let condition = view.condition(view.condition_id(0).unwrap()).unwrap();
    assert!(matches!(
        condition.operation(),
        ConditionOperation::Discrete(_)
    ));
    let delay = view.delay(view.delay_id(0).unwrap()).unwrap();
    assert!(matches!(
        delay.operation(),
        DelayOperation::ParameterDelay { delay_time } if delay_time.value() == 1.0
    ));
    let expressions = (0..view.expression_count())
        .filter_map(|index| view.expression_id(index))
        .filter_map(|id| view.expression(id))
        .collect::<Vec<_>>();
    let clock = expressions
        .iter()
        .find(|expression| {
            matches!(
                expression.operation(),
                ExpressionOperation::Coordinate(CoordinateView::Time)
            )
        })
        .expect("clock coordinate survives");
    assert_eq!(
        clock.provenance().origin(),
        DaeProvenanceOrigin::Generated(DaeGeneration::ClockLowering)
    );
    let delay = expressions
        .iter()
        .find(|expression| {
            matches!(
                expression.operation(),
                ExpressionOperation::Coordinate(CoordinateView::Delay(_))
            )
        })
        .expect("delay coordinate survives");
    assert_eq!(
        delay.provenance().origin(),
        DaeProvenanceOrigin::Generated(DaeGeneration::DelayLowering)
    );
}

#[test]
fn function_parameters_cannot_cross_or_escape_semantic_owners() {
    let source = TestSource::new("function f end f; function g end g;");
    let f_at = source.source("function f", 0);
    let g_at = source.source("function g", 0);
    let result = Dae::construct(source.map, |dae| {
        let real = dae.types(|types| types.derived(ValueType::scalar(ScalarType::Real), f_at))?;
        let (_, (f_output, f_value, escaped)) = dae.function(
            FunctionSignature::new(VarName::new("f"), [real], [real], f_at),
            |dae, reservation| {
                let parameter = dae.functions(|functions| {
                    functions.parameter(&reservation, VarName::new("u"), 0, f_at)
                })?;
                let output = dae.functions(|functions| {
                    functions.output(&reservation, VarName::new("y"), 0, f_at)
                })?;
                let value = dae.expressions(|expr| expr.at(f_at).function_parameter(parameter))?;
                let mut body = dae.functions(|functions| functions.begin(reservation, f_at))?;
                dae.functions(|functions| functions.assign(&mut body, output, value, f_at))?;
                let escaped =
                    dae.functions(|functions| functions.current_definition(&body, output, f_at))?;
                dae.functions(|functions| functions.define(body, f_at))?;
                Ok((output, value, escaped))
            },
        )?;
        dae.function(
            FunctionSignature::new(VarName::new("g"), [real], [real], g_at),
            |dae, reservation| {
                let parameter = dae.functions(|functions| {
                    functions.parameter(&reservation, VarName::new("u"), 0, g_at)
                })?;
                let output = dae.functions(|functions| {
                    functions.output(&reservation, VarName::new("y"), 0, g_at)
                })?;
                let mut body = dae.functions(|functions| functions.begin(reservation, g_at))?;
                let g_value =
                    dae.expressions(|expr| expr.at(g_at).function_parameter(parameter))?;
                let rejected =
                    dae.functions(|functions| functions.current_definition(&body, f_output, g_at));
                assert!(matches!(
                    rejected,
                    Err(DaeConstructionError::InvalidFunctionScope {
                        expected_function: Some(_),
                        ..
                    })
                ));
                let error = dae.expressions(|expr| {
                    expr.at(g_at).binary(BinaryOperator::Add, f_value, g_value)
                });
                assert!(matches!(
                    error,
                    Err(DaeConstructionError::InvalidFunctionScope {
                        expected_function: Some(_),
                        ..
                    })
                ));
                dae.functions(|functions| functions.assign(&mut body, output, g_value, g_at))?;
                dae.functions(|functions| functions.define(body, g_at))
            },
        )?;
        dae.continuous(|continuous| continuous.value_equation(f_at, escaped))
    });
    assert!(matches!(
        result,
        Err(DaeConstructionError::InvalidFunctionScope {
            expected_function: None,
            ..
        })
    ));
}

#[test]
fn function_assignment_rejects_a_foreign_target_before_insertion() {
    let source = TestSource::new(
        "function f output Real y; algorithm y := 0; end f; function g output Real y; algorithm y := 0; end g;",
    );
    let f_at = source.source("function f", 0);
    let g_at = source.source("function g", 0);
    let rejected_at = source.source("y := 0", 0);
    let accepted_at = source.source("y := 0", 1);
    let zero_at = source.source("0", 0);

    let dae = Dae::construct(source.map, |dae| {
        let real = dae.types(|types| types.derived(ValueType::scalar(ScalarType::Real), f_at))?;
        let zero =
            dae.expressions(|expressions| expressions.at(zero_at).literal(DaeLiteral::Real(0.0)))?;
        let (g, g_output) = dae.function(
            FunctionSignature::new(VarName::new("g"), [], [real], g_at),
            |dae, reservation| {
                let output = dae.functions(|functions| {
                    functions.output(&reservation, VarName::new("y"), 0, g_at)
                })?;
                let mut body = dae.functions(|functions| functions.begin(reservation, g_at))?;
                dae.functions(|functions| functions.assign(&mut body, output, zero, accepted_at))?;
                dae.functions(|functions| functions.define(body, g_at))?;
                Ok(output)
            },
        )?;
        dae.function(
            FunctionSignature::new(VarName::new("f"), [], [real], f_at),
            |dae, reservation| {
                let output = dae.functions(|functions| {
                    functions.output(&reservation, VarName::new("y"), 0, f_at)
                })?;
                let f = reservation.function();
                let mut body = dae.functions(|functions| functions.begin(reservation, f_at))?;
                let rejected = dae.functions(|functions| {
                    functions.assign(&mut body, g_output, zero, rejected_at)
                });
                assert!(matches!(
                    rejected,
                    Err(DaeConstructionError::InvalidFunctionScope {
                        expected_function: Some(expected),
                        found_function,
                        span,
                    }) if expected == f.index()
                        && found_function == g.index()
                        && span == rejected_at.span()
                ));
                dae.functions(|functions| functions.assign(&mut body, output, zero, accepted_at))?;
                dae.functions(|functions| functions.define(body, f_at))
            },
        )
        .map(|_| ())
    })
    .expect("foreign-target rejection leaves both function bodies usable");

    dae.inspect(|view| {
        assert_eq!(
            view.function(view.function_id(0).unwrap())
                .unwrap()
                .definition_count(),
            1
        );
        assert_eq!(
            view.function(view.function_id(1).unwrap())
                .unwrap()
                .definition_count(),
            1
        );
    });
}

#[test]
fn function_assignment_rejects_a_wrong_typed_rhs_before_insertion() {
    let source = TestSource::new("function f output Real y; algorithm y := true; y := 0; end f;");
    let function_at = source.source("function f", 0);
    let rejected_at = source.source("y := true", 0);
    let accepted_at = source.source("y := 0", 0);
    let true_at = source.source("true", 0);
    let zero_at = source.source("0", 0);

    let dae = Dae::construct(source.map, |dae| {
        let real =
            dae.types(|types| types.derived(ValueType::scalar(ScalarType::Real), function_at))?;
        dae.function(
            FunctionSignature::new(VarName::new("f"), [], [real], function_at),
            |dae, reservation| {
                let output = dae.functions(|functions| {
                    functions.output(&reservation, VarName::new("y"), 0, function_at)
                })?;
                let mut body =
                    dae.functions(|functions| functions.begin(reservation, function_at))?;
                let (wrong, zero) = dae.expressions(|expressions| {
                    Ok((
                        expressions.at(true_at).literal(DaeLiteral::Boolean(true))?,
                        expressions.at(zero_at).literal(DaeLiteral::Real(0.0))?,
                    ))
                })?;

                let rejected = dae
                    .functions(|functions| functions.assign(&mut body, output, wrong, rejected_at));
                assert!(matches!(
                    rejected,
                    Err(DaeConstructionError::ShapeMismatch { span })
                        if span == rejected_at.span()
                ));

                dae.functions(|functions| functions.assign(&mut body, output, zero, accepted_at))?;
                dae.functions(|functions| functions.define(body, function_at))
            },
        )
        .map(|_| ())
    })
    .expect("wrong-type rejection leaves the function body usable");

    dae.inspect(|view| {
        assert_eq!(
            view.function(view.function_id(0).unwrap())
                .unwrap()
                .definition_count(),
            1
        );
    });
}

#[test]
fn pure_functions_reject_model_runtime_coordinates_at_the_exact_use_site() {
    let source = TestSource::new(
        "function f output Real y; algorithm y := state_x; y := time; y := delay(state_x, 1); y := 0; end f;",
    );
    let function_at = source.source("function f", 0);
    let output_at = source.source("output Real y", 0);
    let state_at = source.source("state_x", 0);
    let time_at = source.source("time", 0);
    let delay_at = source.source("delay(state_x, 1)", 0);
    let delayed_state_at = source.source("state_x", 1);
    let one_at = source.source("1", 0);
    let zero_at = source.source("0", 0);
    let assignment_at = source.source("y := 0", 0);
    let dae = Dae::construct(source.map, |dae| {
        let real =
            dae.types(|types| types.derived(ValueType::scalar(ScalarType::Real), function_at))?;
        let state = dae.variables(|variables| {
            variables.state(
                VarName::new("state_x"),
                real,
                state_at,
                VariableAttributes::default(),
            )
        })?;
        dae.function(
            FunctionSignature::new(VarName::new("f"), [], [real], function_at),
            |dae, reservation| {
                let output = dae.functions(|functions| {
                    functions.output(&reservation, VarName::new("y"), 0, output_at)
                })?;
                let mut body =
                    dae.functions(|functions| functions.begin(reservation, function_at))?;
                let state_use = dae.expressions(|expressions| {
                    expressions
                        .at(state_at)
                        .coordinate(CoordinateInput::State(state))
                })?;
                let rejected = dae.functions(|functions| {
                    functions.assign(&mut body, output, state_use, assignment_at)
                });
                assert!(matches!(
                    rejected,
                    Err(DaeConstructionError::InvalidFunctionCoordinate {
                        coordinate: "state",
                        span,
                    }) if span == state_at.span()
                ));
                let time = dae.expressions(|expressions| {
                    expressions.at(time_at).coordinate(CoordinateInput::Time)
                })?;
                let rejected = dae.functions(|functions| {
                    functions.assign(&mut body, output, time, assignment_at)
                });
                assert!(matches!(
                    rejected,
                    Err(DaeConstructionError::InvalidFunctionCoordinate {
                        coordinate: "time",
                        span,
                    }) if span == time_at.span()
                ));
                let (delayed_state, one) = dae.expressions(|expressions| {
                    Ok((
                        expressions
                            .at(delayed_state_at)
                            .coordinate(CoordinateInput::State(state))?,
                        expressions.at(one_at).literal(DaeLiteral::Real(1.0))?,
                    ))
                })?;
                let delay = dae.temporal(|temporal| {
                    let positive = temporal.positive_parameter(one, 1.0, one_at)?;
                    temporal.delay(delayed_state, positive, delay_at, delay_at)
                })?;
                let rejected = dae.functions(|functions| {
                    functions.assign(&mut body, output, delay.expression(), assignment_at)
                });
                assert!(matches!(
                    rejected,
                    Err(DaeConstructionError::InvalidFunctionCoordinate {
                        coordinate: "delay",
                        span,
                    }) if span == delay_at.span()
                ));
                let zero = dae.expressions(|expressions| {
                    expressions.at(zero_at).literal(DaeLiteral::Real(0.0))
                })?;
                dae.functions(|functions| {
                    functions.assign(&mut body, output, zero, assignment_at)
                })?;
                dae.functions(|functions| functions.define(body, function_at))
            },
        )
        .map(|_| ())
    })
    .expect("rejected assignments do not mutate the function environment");
    dae.inspect(|view| {
        let function = view.function(view.function_id(0).unwrap()).unwrap();
        assert_eq!(function.statements().count(), 1);
    });
}

#[test]
fn function_locals_keep_ordered_statements_and_exact_use_provenance() {
    let source = TestSource::new(
        "function f\n input Real u;\n output Real y;\n protected Real z;\nalgorithm\n z := u + 1;\n y := z * 2;\nend f;",
    );
    let function_at = source.source("function f", 0);
    let parameter_at = source.source("input Real u", 0);
    let output_at = source.source("output Real y", 0);
    let local_at = source.source("Real z", 0);
    let first_assignment = source.source("z := u + 1", 0);
    let second_assignment = source.source("y := z * 2", 0);
    let parameter_use = source.source("u", 1);
    let one_at = source.source("1", 0);
    let first_rhs = source.source("u + 1", 0);
    let local_use = source.source("z", 2);
    let two_at = source.source("2", 0);
    let second_rhs = source.source("z * 2", 0);

    let dae = Dae::construct(source.map, |dae| {
        let real =
            dae.types(|types| types.derived(ValueType::scalar(ScalarType::Real), function_at))?;
        dae.function(
            FunctionSignature::new(VarName::new("f"), [real], [real], function_at),
            |dae, reservation| {
                let parameter = dae.functions(|functions| {
                    functions.parameter(&reservation, VarName::new("u"), 0, parameter_at)
                })?;
                let output = dae.functions(|functions| {
                    functions.output(&reservation, VarName::new("y"), 0, output_at)
                })?;
                let local = dae.functions(|functions| {
                    functions.local(&reservation, VarName::new("z"), real, local_at)
                })?;
                let mut body =
                    dae.functions(|functions| functions.begin(reservation, function_at))?;
                let parameter = dae.expressions(|expressions| {
                    expressions.at(parameter_use).function_parameter(parameter)
                })?;
                let one = dae.expressions(|expressions| {
                    expressions.at(one_at).literal(DaeLiteral::Real(1.0))
                })?;
                let local_definition = dae.expressions(|expressions| {
                    expressions
                        .at(first_rhs)
                        .binary(BinaryOperator::Add, parameter, one)
                })?;
                dae.functions(|functions| {
                    functions.assign(&mut body, local, local_definition, first_assignment)
                })?;
                let local_value =
                    dae.functions(|functions| functions.read(&body, local, local_use))?;
                let two = dae.expressions(|expressions| {
                    expressions.at(two_at).literal(DaeLiteral::Real(2.0))
                })?;
                let output_definition = dae.expressions(|expressions| {
                    expressions
                        .at(second_rhs)
                        .binary(BinaryOperator::Multiply, local_value, two)
                })?;
                dae.functions(|functions| {
                    functions.assign(&mut body, output, output_definition, second_assignment)
                })?;
                dae.functions(|functions| functions.define(body, function_at))
            },
        )
        .map(|_| ())
    })
    .expect("ordered local definitions construct a complete function");

    dae.inspect(|view| assert_function_local_body(view, local_use));
    let encoded = serde_json::to_string(&dae).unwrap();
    let decoded: Dae = serde_json::from_str(&encoded).unwrap();
    decoded.inspect(|view| assert_function_local_body(view, local_use));
    let mut forged: serde_json::Value = serde_json::from_str(&encoded).unwrap();
    let nodes = forged["storage"]["expressions"]["nodes"]
        .as_array_mut()
        .unwrap();
    let local_read = nodes
        .iter_mut()
        .find_map(|node| node.get_mut("function_value"))
        .expect("fixture contains a function-value read");
    local_read["definition_ordinal"] = serde_json::json!(1);
    let error = serde_json::from_value::<Dae>(forged).unwrap_err();
    assert!(
        error
            .to_string()
            .contains("function value 1 reads definition 1, expected Some(0)"),
        "wire reconstruction must reject forged function snapshots: {error}"
    );
}

fn assert_function_local_body(view: DaeView<'_>, local_use: DaeProvenance) {
    let function = view.function(view.function_id(0).unwrap()).unwrap();
    let parameters = function.parameters().collect::<Vec<_>>();
    assert_eq!(parameters.len(), 1);
    assert_eq!(parameters[0].name().as_str(), "u");
    assert_eq!(
        view.source_text(parameters[0].declaration()),
        Some("input Real u"),
        "function inputs retain declaration provenance"
    );
    let values = function.values().collect::<Vec<_>>();
    assert_eq!(values.len(), 2);
    assert_eq!(values[0].name().as_str(), "y");
    assert_eq!(values[0].role(), FunctionValueRole::Output);
    assert_eq!(values[1].name().as_str(), "z");
    assert_eq!(values[1].role(), FunctionValueRole::Local);
    let statements = function.statements().collect::<Vec<_>>();
    assert_eq!(statements.len(), 2);
    let result = view
        .expression(function.result_values().rhs(0).unwrap())
        .unwrap();
    let ExpressionOperation::Binary { lhs, .. } = result.operation() else {
        panic!("final function output must retain its checked expression");
    };
    let local = view.expression(lhs).unwrap();
    assert_eq!(local.provenance(), local_use);
    let ExpressionOperation::FunctionValue { value, definition } = local.operation() else {
        panic!("local occurrence must retain its declaration and snapshot definition");
    };
    assert_eq!(value.function(), function.id());
    assert_eq!(value.ordinal(), 1);
    assert_eq!(
        view.source_text(local.provenance()),
        Some("z"),
        "the local read keeps its source occurrence"
    );
    assert!(view.expression(definition.rhs()).is_some());
}

#[test]
fn zeros_is_a_provenance_bearing_checked_array_operation() {
    let source = TestSource::new("Real z[2]; algorithm z := zeros(2);");
    let extent_at = source.source("2", 1);
    let zeros_at = source.source("zeros(2)", 0);
    let dae = Dae::construct(source.map, |dae| {
        let extent = dae
            .expressions(|expressions| expressions.at(extent_at).literal(DaeLiteral::Integer(2)))?;
        dae.expressions(|expressions| {
            expressions
                .at(zeros_at)
                .builtin(PureBuiltin::Zeros, [extent])
        })?;
        Ok(())
    })
    .expect("literal zeros dimensions construct a checked array");

    let assert_zeros = |view: DaeView<'_>| {
        let expression = view.expression(view.expression_id(1).unwrap()).unwrap();
        assert_eq!(expression.value_type().scalar_type(), ScalarType::Real);
        assert_eq!(expression.value_type().dimensions(), &[2]);
        assert_eq!(view.source_text(expression.provenance()), Some("zeros(2)"));
        assert!(matches!(
            expression.operation(),
            ExpressionOperation::Builtin {
                builtin: PureBuiltin::Zeros,
                ..
            }
        ));
    };
    dae.inspect(assert_zeros);
    let encoded = serde_json::to_string(&dae).unwrap();
    let decoded: Dae = serde_json::from_str(&encoded).unwrap();
    decoded.inspect(assert_zeros);

    let source = TestSource::new("zeros(-1)");
    let negative_at = source.source("-1", 0);
    let invalid_zeros_at = source.source("zeros(-1)", 0);
    let error = Dae::construct(source.map, |dae| {
        let extent = dae.expressions(|expressions| {
            expressions.at(negative_at).literal(DaeLiteral::Integer(-1))
        })?;
        dae.expressions(|expressions| {
            expressions
                .at(invalid_zeros_at)
                .builtin(PureBuiltin::Zeros, [extent])
        })?;
        Ok(())
    });
    assert!(matches!(
        error,
        Err(DaeConstructionError::InvalidArrayExtent { .. })
    ));
}

#[test]
fn ones_and_fill_are_compact_typed_array_operations() {
    let source = TestSource::new("ones(2, 2); fill(0.5, 3)");
    let ones_at = source.source("ones(2, 2)", 0);
    let fill_at = source.source("fill(0.5, 3)", 0);
    let first_two_at = source.source("2", 0);
    let second_two_at = source.source("2", 1);
    let value_at = source.source("0.5", 0);
    let extent_at = source.source("3", 0);
    let dae = Dae::construct(source.map, |dae| {
        let first_two = dae.expressions(|expressions| {
            expressions.at(first_two_at).literal(DaeLiteral::Integer(2))
        })?;
        let second_two = dae.expressions(|expressions| {
            expressions
                .at(second_two_at)
                .literal(DaeLiteral::Integer(2))
        })?;
        dae.expressions(|expressions| {
            expressions
                .at(ones_at)
                .builtin(PureBuiltin::Ones, [first_two, second_two])
        })?;
        let value =
            dae.expressions(|expressions| expressions.at(value_at).literal(DaeLiteral::Real(0.5)))?;
        let extent = dae
            .expressions(|expressions| expressions.at(extent_at).literal(DaeLiteral::Integer(3)))?;
        dae.expressions(|expressions| {
            expressions
                .at(fill_at)
                .builtin(PureBuiltin::Fill, [value, extent])
        })?;
        Ok(())
    })
    .unwrap();

    let encoded = serde_json::to_string(&dae).unwrap();
    let decoded: Dae = serde_json::from_str(&encoded).unwrap();
    decoded.inspect(|view| {
        for (index, builtin, dimensions, text) in [
            (2, PureBuiltin::Ones, &[2, 2][..], "ones(2, 2)"),
            (5, PureBuiltin::Fill, &[3][..], "fill(0.5, 3)"),
        ] {
            let expression = view.expression(view.expression_id(index).unwrap()).unwrap();
            assert_eq!(expression.value_type().scalar_type(), ScalarType::Real);
            assert_eq!(expression.value_type().dimensions(), dimensions);
            assert_eq!(view.source_text(expression.provenance()), Some(text));
            assert!(matches!(
                expression.operation(),
                ExpressionOperation::Builtin { builtin: found, .. } if found == builtin
            ));
        }
    });
}

#[test]
fn linspace_and_cross_are_checked_compact_vector_operations() {
    let source = TestSource::new("linspace(2.0, 4.0, 3); cross({1.0,2.0,3.0},{4.0,5.0,6.0})");
    let linspace_at = source.source("linspace(2.0, 4.0, 3)", 0);
    let cross_at = source.source("cross({1.0,2.0,3.0},{4.0,5.0,6.0})", 0);
    let provenances = [
        source.source("2.0", 0),
        source.source("4.0", 0),
        source.source("3", 0),
        source.source("1.0", 0),
        source.source("2.0", 1),
        source.source("3.0", 0),
        source.source("4.0", 1),
        source.source("5.0", 0),
        source.source("6.0", 0),
    ];
    let dae = Dae::construct(source.map, |dae| {
        dae.expressions(|expressions| {
            let start = expressions
                .at(provenances[0])
                .literal(DaeLiteral::Real(2.0))?;
            let stop = expressions
                .at(provenances[1])
                .literal(DaeLiteral::Real(4.0))?;
            let count = expressions
                .at(provenances[2])
                .literal(DaeLiteral::Integer(3))?;
            expressions
                .at(linspace_at)
                .builtin(PureBuiltin::Linspace, [start, stop, count])?;
            let lhs = provenances[3..6]
                .iter()
                .zip([1.0, 2.0, 3.0])
                .map(|(at, value)| expressions.at(*at).literal(DaeLiteral::Real(value)))
                .collect::<Result<Vec<_>, _>>()?;
            let lhs = expressions.at(cross_at).array(lhs)?;
            let rhs = provenances[6..]
                .iter()
                .zip([4.0, 5.0, 6.0])
                .map(|(at, value)| expressions.at(*at).literal(DaeLiteral::Real(value)))
                .collect::<Result<Vec<_>, _>>()?;
            let rhs = expressions.at(cross_at).array(rhs)?;
            expressions
                .at(cross_at)
                .builtin(PureBuiltin::Cross, [lhs, rhs])?;
            Ok(())
        })
    })
    .unwrap();

    let encoded = serde_json::to_string(&dae).unwrap();
    let decoded: Dae = serde_json::from_str(&encoded).unwrap();
    decoded.inspect(|view| {
        for (index, builtin, text) in [
            (3, PureBuiltin::Linspace, "linspace(2.0, 4.0, 3)"),
            (12, PureBuiltin::Cross, "cross({1.0,2.0,3.0},{4.0,5.0,6.0})"),
        ] {
            let expression = view.expression(view.expression_id(index).unwrap()).unwrap();
            assert_eq!(expression.value_type().dimensions(), &[3]);
            assert_eq!(view.source_text(expression.provenance()), Some(text));
            assert!(matches!(
                expression.operation(),
                ExpressionOperation::Builtin { builtin: found, .. } if found == builtin
            ));
        }
    });
}

#[test]
fn enumeration_literals_are_canonical_checked_integers_and_round_trip() {
    let source = TestSource::new("E.a");
    let literal_at = source.source("E.a", 0);
    let dae = Dae::construct(source.map, |dae| {
        dae.expressions(|expressions| expressions.at(literal_at).enumeration_literal(1))?;
        Ok(())
    })
    .expect("positive enumeration ordinals construct");

    let encoded = serde_json::to_string(&dae).unwrap();
    let decoded: Dae = serde_json::from_str(&encoded).unwrap();
    decoded.inspect(|view| {
        let expression = view.expression(view.expression_id(0).unwrap()).unwrap();
        assert_eq!(expression.value_type().scalar_type(), ScalarType::Integer);
        assert!(expression.value_type().dimensions().is_empty());
        assert_eq!(view.source_text(expression.provenance()), Some("E.a"));
        assert!(matches!(
            expression.operation(),
            ExpressionOperation::Literal(DaeLiteral::Enumeration(1))
        ));
    });

    let source = TestSource::new("E.invalid");
    let invalid_at = source.source("E.invalid", 0);
    let error = Dae::construct(source.map, |dae| {
        dae.expressions(|expressions| expressions.at(invalid_at).enumeration_literal(0))?;
        Ok(())
    });
    assert!(matches!(
        error,
        Err(DaeConstructionError::InvalidEnumerationOrdinal { ordinal: 0, .. })
    ));
}

#[test]
fn function_for_loop_is_a_compact_checked_transition() {
    let source = TestSource::new(
        "function sum3\n output Real y;\n protected Real scratch;\nalgorithm\n y := 0;\n for k in 1:3 loop\n  y := y + k;\n end for;\nend sum3;",
    );
    let function_at = source.source("function sum3", 0);
    let output_at = source.source("output Real y", 0);
    let scratch_at = source.source("Real scratch", 0);
    let initial_at = source.source("y := 0", 0);
    let zero_at = source.source("0", 0);
    let loop_at = source.source("for k in 1:3 loop", 0);
    let update_at = source.source("y := y + k", 0);
    let y_use_at = source.source("y", 3);
    let k_use_at = source.source("k", 1);
    let update_value_at = source.source("y + k", 0);
    let dae = Dae::construct(source.map, |dae| {
        let real =
            dae.types(|types| types.derived(ValueType::scalar(ScalarType::Real), function_at))?;
        dae.function(
            FunctionSignature::new(VarName::new("sum3"), [], [real], function_at),
            |dae, reservation| {
                let output = dae.functions(|functions| {
                    functions.output(&reservation, VarName::new("y"), 0, output_at)
                })?;
                let scratch = dae.functions(|functions| {
                    functions.local(&reservation, VarName::new("scratch"), real, scratch_at)
                })?;
                let mut body =
                    dae.functions(|functions| functions.begin(reservation, function_at))?;
                let zero = dae.expressions(|expressions| {
                    expressions.at(zero_at).literal(DaeLiteral::Real(0.0))
                })?;
                dae.functions(|functions| functions.assign(&mut body, output, zero, initial_at))?;
                let domain = dae.domains(|domains| {
                    domains.structured(
                        StructuredIndexDomain {
                            binders: vec![StructuredIndexBinder {
                                id: 0,
                                display_name: "k".to_string(),
                                lower: 1,
                                upper: 3,
                                step: 1,
                            }],
                        },
                        loop_at,
                    )
                })?;
                let binder = DomainBinderId::from_raw(domain.index(), 0);
                let mut loop_body = dae
                    .functions(|functions| functions.begin_loop(body, domain, [output], loop_at))?;
                let current =
                    dae.functions(|functions| functions.read(loop_body.body(), output, y_use_at))?;
                let k = dae.expressions(|expressions| expressions.at(k_use_at).binder(binder))?;
                let update = dae.expressions(|expressions| {
                    expressions
                        .at(update_value_at)
                        .binary(BinaryOperator::Add, current, k)
                })?;
                let rejected = dae.functions(|functions| {
                    functions.assign_loop(&mut loop_body, scratch, update, update_at)
                });
                assert!(matches!(
                    rejected,
                    Err(DaeConstructionError::IncompleteDefinition {
                        kind: "function loop target",
                        ..
                    })
                ));
                dae.functions(|functions| {
                    functions.assign_loop(&mut loop_body, output, update, update_at)
                })?;
                let body = dae.functions(|functions| functions.finish_loop(loop_body, loop_at))?;
                dae.functions(|functions| functions.define(body, function_at))
            },
        )
        .map(|_| ())
    })
    .expect("loop-carried function state constructs as a checked fold");

    dae.inspect(assert_sum3_loop);
    let encoded = serde_json::to_string(&dae).unwrap();
    let decoded: Dae = serde_json::from_str(&encoded).unwrap();
    decoded.inspect(assert_sum3_loop);
    assert_invalid_function_loop_wires(&encoded);
}

fn assert_invalid_function_loop_wires(encoded: &str) {
    let mut missing_parameter: serde_json::Value = serde_json::from_str(encoded).unwrap();
    missing_parameter["storage"]["functions"][0]["statements"][1]["for"]["targets"] =
        serde_json::json!([]);
    assert!(
        serde_json::from_value::<Dae>(missing_parameter).is_err(),
        "wire reconstruction rejects a loop operation inconsistent with generated parameters"
    );

    let mut open_initial: serde_json::Value = serde_json::from_str(encoded).unwrap();
    open_initial["storage"]["functions"][0]["statements"][1]["for"]["targets"][0] =
        serde_json::json!(1);
    assert!(
        serde_json::from_value::<Dae>(open_initial).is_err(),
        "wire reconstruction rejects an uninitialized loop-carried local"
    );

    let mut nested_fold: serde_json::Value = serde_json::from_str(encoded).unwrap();
    let outer = nested_fold["storage"]["functions"][0]["statements"][1].clone();
    nested_fold["storage"]["functions"][0]["statements"][1]["for"]["statements"] =
        serde_json::json!([outer]);
    let error = serde_json::from_value::<Dae>(nested_fold).unwrap_err();
    assert!(
        error.to_string().contains("functions.statements.nesting"),
        "wire reconstruction rejects a nested fold that normal construction cannot express: {error}"
    );
}

fn assert_sum3_loop(view: DaeView<'_>) {
    let function = view.function(view.function_id(0).unwrap()).unwrap();
    let statements = function.statements().collect::<Vec<_>>();
    assert_eq!(statements.len(), 2);
    let FunctionStatementView::For {
        fold,
        statements,
        provenance,
    } = statements[1].clone()
    else {
        panic!("second function statement is the compact source loop");
    };
    assert_eq!(view.source_text(provenance), Some("for k in 1:3 loop"));
    assert_eq!(statements.count(), 1);
    let fold = view.function_fold(fold).unwrap();
    assert_eq!(fold.targets().count(), 1);
    assert_eq!(fold.initial_values().len(), 1);
    assert_eq!(fold.update_values().len(), 1);
    assert_eq!(
        view.expression(function.result_values().rhs(0).unwrap())
            .unwrap()
            .kind(),
        ExpressionKind::FunctionFoldOutput
    );
    let update = view
        .expression(fold.update_values().rhs(0).unwrap())
        .unwrap();
    assert_eq!(view.source_text(update.provenance()), Some("y + k"));
}

#[test]
fn function_loop_rejects_duplicate_carried_targets() {
    let source =
        TestSource::new("function f output Real x; algorithm x := 0; for k in 1:2 loop end for;");
    let function_at = source.source("function f", 0);
    let output_at = source.source("output Real x", 0);
    let assignment_at = source.source("x := 0", 0);
    let zero_at = source.source("0", 0);
    let loop_at = source.source("for k in 1:2 loop", 0);
    let error = Dae::construct(source.map, |dae| {
        let real =
            dae.types(|types| types.derived(ValueType::scalar(ScalarType::Real), function_at))?;
        dae.function(
            FunctionSignature::new(VarName::new("f"), [], [real], function_at),
            |dae, reservation| {
                let output = dae.functions(|functions| {
                    functions.output(&reservation, VarName::new("x"), 0, output_at)
                })?;
                let mut body =
                    dae.functions(|functions| functions.begin(reservation, function_at))?;
                let zero = dae.expressions(|expressions| {
                    expressions.at(zero_at).literal(DaeLiteral::Real(0.0))
                })?;
                dae.functions(|functions| {
                    functions.assign(&mut body, output, zero, assignment_at)
                })?;
                let domain = dae.domains(|domains| {
                    domains.structured(
                        StructuredIndexDomain {
                            binders: vec![StructuredIndexBinder {
                                id: 0,
                                display_name: "k".to_string(),
                                lower: 1,
                                upper: 2,
                                step: 1,
                            }],
                        },
                        loop_at,
                    )
                })?;
                let _ = dae.functions(|functions| {
                    functions.begin_loop(body, domain, [output, output], loop_at)
                })?;
                Ok(())
            },
        )
        .map(|_| ())
    })
    .unwrap_err();
    assert!(matches!(
        error,
        DaeConstructionError::DuplicateDefinition {
            kind: "function loop target",
            ..
        }
    ));
}
