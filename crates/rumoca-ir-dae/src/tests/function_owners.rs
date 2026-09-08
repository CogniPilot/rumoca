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
        let positive =
            dae.temporal(|temporal| temporal.positive_parameter(delay_time, 1.0, delay_generated))?;
        let delay = dae.expressions(|expr| {
            expr.at(delay_generated)
                .delay(literal, positive, delay_generated)
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
fn multi_result_call_issues_one_owner_and_replays_its_projections() {
    let source = TestSource::new(
        "function pair input Real u; output Real a; output Real b; end pair; pair(1.0);",
    );
    let declaration = source.source("function pair", 0);
    let call_at = source.source("pair(1.0)", 0);
    let literal_at = source.source("1.0", 0);
    let dae = Dae::construct(source.map, |dae| {
        let real =
            dae.types(|types| types.derived(ValueType::scalar(ScalarType::Real), declaration))?;
        let (function, ()) = dae.function(
            FunctionSignature::new(VarName::new("pair"), [real], [real, real], declaration),
            |dae, reservation| {
                let parameter = dae.functions(|functions| {
                    functions.parameter(&reservation, VarName::new("u"), 0, declaration)
                })?;
                let first = dae.functions(|functions| {
                    functions.output(&reservation, VarName::new("a"), 0, declaration)
                })?;
                let second = dae.functions(|functions| {
                    functions.output(&reservation, VarName::new("b"), 1, declaration)
                })?;
                let value = dae.expressions(|expressions| {
                    expressions.at(declaration).function_parameter(parameter)
                })?;
                let mut body =
                    dae.functions(|functions| functions.begin(reservation, declaration))?;
                dae.functions(|functions| {
                    functions.assign(&mut body, first, value, declaration)?;
                    functions.assign(&mut body, second, value, declaration)?;
                    functions.define(body, declaration)
                })
            },
        )?;
        let argument = dae
            .expressions(|expressions| expressions.at(literal_at).literal(DaeLiteral::Real(1.0)))?;
        dae.expressions(|expressions| {
            expressions
                .at(call_at)
                .call_results(function, [0, 1], [argument])
                .map(|_| ())
        })
    })
    .expect("one multi-result call owner should construct");

    let assert_owner = |view: DaeView<'_>| {
        let calls = (0..view.expression_count())
            .filter_map(|index| view.expression_id(index))
            .filter_map(|id| {
                let operation = view.expression(id)?.operation();
                match operation {
                    ExpressionOperation::Call { owner, output, .. } => Some((id, owner, output)),
                    _ => None,
                }
            })
            .collect::<Vec<_>>();
        assert_eq!(calls.len(), 2);
        assert_eq!(calls[0].0, calls[0].1);
        assert_eq!(calls[1].1, calls[0].1);
        assert_eq!([calls[0].2, calls[1].2], [0, 1]);
    };
    dae.inspect(assert_owner);

    let encoded = serde_json::to_string(&dae).expect("call owner should serialize");
    let decoded: Dae = serde_json::from_str(&encoded).expect("call owner should replay");
    decoded.inspect(assert_owner);
}

#[test]
fn acyclic_function_rejects_self_call_at_the_exact_occurrence() {
    let source = TestSource::new("function f output Real y; algorithm y := f(); end f;");
    let declaration = source.source("function f", 0);
    let output_at = source.source("output Real y", 0);
    let call_at = source.source("f()", 0);
    let assignment_at = source.source("y := f()", 0);
    let call_span = call_at.span();
    let error = Dae::construct(source.map, |dae| {
        let real =
            dae.types(|types| types.derived(ValueType::scalar(ScalarType::Real), declaration))?;
        dae.function(
            FunctionSignature::new(VarName::new("f"), [], [real], declaration),
            |dae, reservation| {
                let function = reservation.function();
                let output = dae.functions(|functions| {
                    functions.output(&reservation, VarName::new("y"), 0, output_at)
                })?;
                let value =
                    dae.expressions(|expressions| expressions.at(call_at).call(function, 0, []))?;
                let mut body =
                    dae.functions(|functions| functions.begin(reservation, declaration))?;
                dae.functions(|functions| {
                    functions.assign(&mut body, output, value, assignment_at)
                })?;
                dae.functions(|functions| functions.define(body, declaration))
            },
        )
        .map(|_| ())
    })
    .unwrap_err();

    assert_eq!(
        error,
        DaeConstructionError::InvalidFunctionDependency {
            function: 0,
            target: 0,
            span: call_span,
        }
    );
}

#[test]
fn recursive_group_rejects_disconnected_headers() {
    let source = TestSource::new("function f end f; function g end g;");
    let f_at = source.source("function f", 0);
    let g_at = source.source("function g", 0);
    let error = Dae::construct(source.map, |dae| {
        let real = dae.types(|types| types.derived(ValueType::scalar(ScalarType::Real), f_at))?;
        let first = FunctionSignature::new(VarName::new("f"), [], [real], f_at);
        let second = FunctionSignature::new(VarName::new("g"), [], [real], g_at);
        dae.recursive_functions(first, [second], |dae, reservations| {
            for (reservation, provenance) in reservations.into_iter().zip([f_at, g_at]) {
                define_constant_result(dae, reservation, provenance)?;
            }
            Ok(())
        })
        .map(|_| ())
    })
    .unwrap_err();

    assert_eq!(
        error,
        DaeConstructionError::InvalidRecursiveFunctionGroup { span: f_at.span() }
    );
}

fn define_constant_result<'dae>(
    dae: &mut DaeConstruction<'dae>,
    reservation: FunctionReservation<'_, 'dae>,
    provenance: DaeProvenance,
) -> Result<(), DaeConstructionError> {
    let output = dae
        .functions(|functions| functions.output(&reservation, VarName::new("y"), 0, provenance))?;
    let zero =
        dae.expressions(|expressions| expressions.at(provenance).literal(DaeLiteral::Real(0.0)))?;
    let mut body = dae.functions(|functions| functions.begin(reservation, provenance))?;
    dae.functions(|functions| functions.assign(&mut body, output, zero, provenance))?;
    dae.functions(|functions| functions.define(body, provenance))
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
                rumoca_core::InstanceId::new(1),
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
                let positive =
                    dae.temporal(|temporal| temporal.positive_parameter(one, 1.0, one_at))?;
                let delay = dae.expressions(|expressions| {
                    expressions
                        .at(delay_at)
                        .delay(delayed_state, positive, delay_at)
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
    assert_function_statement_count(&dae, 1);
}

fn assert_function_statement_count(dae: &Dae, expected: usize) {
    dae.inspect(|view| {
        let function = view.function(view.function_id(0).unwrap()).unwrap();
        assert_eq!(function.statements().count(), expected);
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
fn enumeration_literals_are_canonical_checked_values_and_round_trip() {
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
        assert_eq!(
            expression.value_type().scalar_type(),
            ScalarType::Enumeration
        );
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
fn function_assertion_is_a_checked_call_scoped_statement_and_round_trips() {
    let source = TestSource::new(
        "function f\n output Real y;\nalgorithm\n assert(true, \"valid call\");\n y := 0;\nend f;",
    );
    let function_at = source.source("function f", 0);
    let output_at = source.source("output Real y", 0);
    let assertion_at = source.source("assert(true, \"valid call\")", 0);
    let condition_at = source.source("true", 0);
    let message_at = source.source("\"valid call\"", 0);
    let assignment_at = source.source("y := 0", 0);
    let zero_at = source.source("0", 0);
    let dae = Dae::construct(source.map, |dae| {
        let real =
            dae.types(|types| types.derived(ValueType::scalar(ScalarType::Real), function_at))?;
        dae.function(
            FunctionSignature::new(VarName::new("f"), [], [real], function_at),
            |dae, reservation| {
                let output = dae.functions(|functions| {
                    functions.output(&reservation, VarName::new("y"), 0, output_at)
                })?;
                let mut body =
                    dae.functions(|functions| functions.begin(reservation, function_at))?;
                let condition = dae.expressions(|expressions| {
                    expressions
                        .at(condition_at)
                        .literal(DaeLiteral::Boolean(true))
                })?;
                let message = dae.expressions(|expressions| {
                    expressions
                        .at(message_at)
                        .literal(DaeLiteral::String("valid call".to_owned()))
                })?;
                dae.functions(|functions| {
                    functions.assertion(&mut body, condition, message, assertion_at)
                })?;
                let zero = dae.expressions(|expressions| {
                    expressions.at(zero_at).literal(DaeLiteral::Real(0.0))
                })?;
                dae.functions(|functions| {
                    functions.assign(&mut body, output, zero, assignment_at)
                })?;
                dae.functions(|functions| functions.define(body, function_at))
            },
        )?;
        Ok(())
    })
    .expect("a typed top-level function assertion has a checked owner");

    let assert_owner = |view: DaeView<'_>| {
        let function = view.function(view.function_id(0).unwrap()).unwrap();
        let statements = function.statements().collect::<Vec<_>>();
        assert_eq!(statements.len(), 2);
        let FunctionStatementView::Assertion {
            condition,
            message,
            provenance,
        } = statements[0].clone()
        else {
            panic!("the first function statement retains the assertion action");
        };
        assert_eq!(
            view.source_text(provenance),
            Some("assert(true, \"valid call\")")
        );
        assert_eq!(
            view.expression(condition)
                .unwrap()
                .value_type()
                .scalar_type(),
            ScalarType::Boolean
        );
        assert_eq!(
            view.expression(message).unwrap().value_type().scalar_type(),
            ScalarType::String
        );
    };
    dae.inspect(assert_owner);
    let encoded = serde_json::to_string(&dae).unwrap();
    let decoded: Dae = serde_json::from_str(&encoded).unwrap();
    decoded.inspect(assert_owner);
}

#[test]
fn function_for_loop_is_a_compact_checked_transition() {
    let source = TestSource::new(
        "function sum3\n output Real y;\n protected Real scratch;\nalgorithm\n y := 0;\n for k in 1:3 loop\n  assert(k > 0, \"positive index\");\n  y := y + k;\n end for;\nend sum3;",
    );
    let function_at = source.source("function sum3", 0);
    let output_at = source.source("output Real y", 0);
    let scratch_at = source.source("Real scratch", 0);
    let initial_at = source.source("y := 0", 0);
    let zero_at = source.source("0", 0);
    let loop_at = source.source("for k in 1:3 loop", 0);
    let assertion_at = source.source("assert(k > 0, \"positive index\")", 0);
    let condition_at = source.source("k > 0", 0);
    let assertion_zero_at = source.source("0", 1);
    let message_at = source.source("\"positive index\"", 0);
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
                                id: rumoca_core::StructuredIndexBinderId::new(0),
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
                let assertion_zero = dae.expressions(|expressions| {
                    expressions
                        .at(assertion_zero_at)
                        .literal(DaeLiteral::Integer(0))
                })?;
                let condition = dae.expressions(|expressions| {
                    expressions
                        .at(condition_at)
                        .binary(BinaryOperator::Greater, k, assertion_zero)
                })?;
                let message = dae.expressions(|expressions| {
                    expressions
                        .at(message_at)
                        .literal(DaeLiteral::String("positive index".to_owned()))
                })?;
                dae.functions(|functions| {
                    functions.assertion_loop(&mut loop_body, condition, message, assertion_at)
                })?;
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

    assert_sum3_loop_roundtrip(dae);
}

fn assert_sum3_loop_roundtrip(dae: Dae) {
    dae.inspect(assert_sum3_loop);
    let encoded = serde_json::to_string(&dae).unwrap();
    let decoded: Dae = serde_json::from_str(&encoded).unwrap();
    decoded.inspect(assert_sum3_loop);
    assert_invalid_function_loop_wires(&encoded);
}

#[test]
fn nested_function_loops_round_trip_as_nested_compact_folds() {
    let source = TestSource::new(
        "function nestedSum output Integer y; algorithm y := 0; for i in 1:2 loop for j in 1:2 loop y := y + i + j; end for; end for; end nestedSum;",
    );
    let function_at = source.source("function nestedSum", 0);
    let output_at = source.source("output Integer y", 0);
    let initial_at = source.source("y := 0", 0);
    let zero_at = source.source("0", 0);
    let outer_at = source.source("for i in 1:2 loop", 0);
    let inner_at = source.source("for j in 1:2 loop", 0);
    let update_at = source.source("y := y + i + j", 0);
    let update_value_at = source.source("y + i + j", 0);
    let y_at = update_value_at;
    let i_at = update_value_at;
    let j_at = update_value_at;
    let dae = Dae::construct(source.map, |dae| {
        let integer =
            dae.types(|types| types.derived(ValueType::scalar(ScalarType::Integer), function_at))?;
        dae.function(
            FunctionSignature::new(VarName::new("nestedSum"), [], [integer], function_at),
            |dae, reservation| {
                let output = dae.functions(|functions| {
                    functions.output(&reservation, VarName::new("y"), 0, output_at)
                })?;
                let mut body =
                    dae.functions(|functions| functions.begin(reservation, function_at))?;
                let zero = dae.expressions(|expressions| {
                    expressions.at(zero_at).literal(DaeLiteral::Integer(0))
                })?;
                dae.functions(|functions| functions.assign(&mut body, output, zero, initial_at))?;
                let (outer_domain, inner_domain) = nested_loop_domains(dae, outer_at, inner_at)?;
                let outer = dae.functions(|functions| {
                    functions.begin_loop(body, outer_domain, [output], outer_at)
                })?;
                let mut inner = dae.functions(|functions| {
                    functions.begin_nested_loop(outer, inner_domain, [output], inner_at)
                })?;
                let current =
                    dae.functions(|functions| functions.read(inner.body(), output, y_at))?;
                let (i, j) = dae.expressions(|expressions| {
                    Ok((
                        expressions
                            .at(i_at)
                            .binder(DomainBinderId::from_raw(outer_domain.index(), 0))?,
                        expressions
                            .at(j_at)
                            .binder(DomainBinderId::from_raw(inner_domain.index(), 0))?,
                    ))
                })?;
                let update = dae.expressions(|expressions| {
                    let outer_sum =
                        expressions
                            .at(update_value_at)
                            .binary(BinaryOperator::Add, current, i)?;
                    expressions
                        .at(update_value_at)
                        .binary(BinaryOperator::Add, outer_sum, j)
                })?;
                dae.functions(|functions| {
                    functions.assign_loop(&mut inner, output, update, update_at)
                })?;
                let outer =
                    dae.functions(|functions| functions.finish_nested_loop(inner, inner_at))?;
                let body = dae.functions(|functions| functions.finish_loop(outer, outer_at))?;
                dae.functions(|functions| functions.define(body, function_at))
            },
        )?;
        Ok(())
    })
    .expect("nested function loops construct as a lexical fold stack");

    dae.inspect(assert_nested_folds);
    let encoded = serde_json::to_string(&dae).unwrap();
    let replayed: Dae = serde_json::from_str(&encoded).unwrap();
    replayed.inspect(assert_nested_folds);
}

/// Build the `i` domain and the `j` domain nested inside it, which together are
/// the lexical domain stack the two compact folds are carried by.
fn nested_loop_domains<'dae>(
    dae: &mut DaeConstruction<'dae>,
    outer_at: DaeProvenance,
    inner_at: DaeProvenance,
) -> Result<(DomainId<'dae>, DomainId<'dae>), DaeConstructionError> {
    let outer_domain = dae.domains(|domains| {
        domains.structured(
            StructuredIndexDomain {
                binders: vec![StructuredIndexBinder {
                    id: rumoca_core::StructuredIndexBinderId::new(0),
                    display_name: "i".to_string(),
                    lower: 1,
                    upper: 2,
                    step: 1,
                }],
            },
            outer_at,
        )
    })?;
    let inner_domain = dae.domains(|domains| {
        domains.nested(
            outer_domain,
            StructuredIndexDomain {
                binders: vec![StructuredIndexBinder {
                    id: rumoca_core::StructuredIndexBinderId::new(0),
                    display_name: "j".to_string(),
                    lower: 1,
                    upper: 2,
                    step: 1,
                }],
            },
            inner_at,
        )
    })?;
    Ok((outer_domain, inner_domain))
}

fn assert_nested_folds(view: DaeView<'_>) {
    let function = view.function(view.function_id(0).unwrap()).unwrap();
    assert_eq!(function.fold_count(), 2);
    let statements = function.statements().collect::<Vec<_>>();
    let FunctionStatementView::For { statements, .. } = statements[1].clone() else {
        panic!("outer compact fold expected");
    };
    let nested = statements.collect::<Vec<_>>();
    assert!(matches!(
        nested.as_slice(),
        [FunctionStatementView::For { .. }]
    ));
}

fn assert_invalid_function_loop_wires(encoded: &str) {
    let mut missing_parameter: serde_json::Value = serde_json::from_str(encoded).unwrap();
    missing_parameter["storage"]["functions"][0]["body"]["modelica"]["statements"][1]["for"]["targets"] =
        serde_json::json!([]);
    assert!(
        serde_json::from_value::<Dae>(missing_parameter).is_err(),
        "wire reconstruction rejects a loop operation inconsistent with generated parameters"
    );

    let mut open_initial: serde_json::Value = serde_json::from_str(encoded).unwrap();
    open_initial["storage"]["functions"][0]["body"]["modelica"]["statements"][1]["for"]["targets"]
        [0] = serde_json::json!(1);
    assert!(
        serde_json::from_value::<Dae>(open_initial).is_err(),
        "wire reconstruction rejects an uninitialized loop-carried local"
    );

    let mut nested_fold: serde_json::Value = serde_json::from_str(encoded).unwrap();
    let outer = nested_fold["storage"]["functions"][0]["body"]["modelica"]["statements"][1].clone();
    nested_fold["storage"]["functions"][0]["body"]["modelica"]["statements"][1]["for"]["statements"] =
        serde_json::json!([outer]);
    let error = serde_json::from_value::<Dae>(nested_fold).unwrap_err();
    assert!(
        error
            .to_string()
            .contains("missing function fold end definition for identity 0"),
        "wire reconstruction rejects recursive reuse of one fold's generated identities: {error}"
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
    let statements = statements.collect::<Vec<_>>();
    assert_eq!(statements.len(), 2);
    assert!(matches!(
        statements[0],
        FunctionStatementView::Assertion { .. }
    ));
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
                                id: rumoca_core::StructuredIndexBinderId::new(0),
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
