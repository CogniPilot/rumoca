//! Focused construction and execution tests for the shared typed lowerer.

use rumoca_core::{SourceMap, Span, StructuredIndexBinder, StructuredIndexDomain, VarName};

use super::*;

mod fixtures;

use fixtures::{integer_to_real_sibling_folds, lower_root_call, structured_range};

#[test]
#[expect(
    clippy::too_many_lines,
    reason = "valid-by-construction event transaction fixture enumerates its complete typed ownership proof"
)]
fn mixed_event_transaction_retains_aggregate_inputs_and_atomic_targets() {
    let mut sources = SourceMap::new();
    let source = sources.add(
        "event_transaction.mo",
        "discrete Real x[2]; discrete Boolean valid; when Clock() then end when;",
    );
    let at = dae::DaeProvenance::source(Span::from_offsets(source, 0, 71)).unwrap();
    let model = dae::Dae::construct(sources, |model| {
        let (vector, boolean) = model.types(|types| {
            Ok((
                types.intern(
                    rumoca_core::TypeId::new(0),
                    dae::ValueType::array(dae::ScalarType::Real, [2]),
                    at,
                )?,
                types.intern(
                    rumoca_core::TypeId::new(1),
                    dae::ValueType::scalar(dae::ScalarType::Boolean),
                    at,
                )?,
            ))
        })?;
        let (x, valid) = model.variables(|variables| {
            Ok((
                variables.discrete_real(
                    VarName::new("x"),
                    vector,
                    at,
                    dae::VariableAttributes::default(),
                )?,
                variables.discrete_value(
                    VarName::new("valid"),
                    boolean,
                    at,
                    dae::VariableAttributes::default(),
                )?,
            ))
        })?;
        let lattice = rumoca_core::ClockLattice::from_interval_counter(1, 200).unwrap();
        let clock = model.clocks(|clocks| {
            let clock = clocks.periodic(lattice, at)?;
            clocks.own_discrete_real(clock.into(), x, at)?;
            clocks.own_discrete_value(clock.into(), valid, at)?;
            Ok(clock)
        })?;
        let guard = model.conditions(|conditions| conditions.reserve(at))?;
        model.conditions(|conditions| {
            conditions.define(guard, dae::ConditionInput::Clock(clock.into()), at)
        })?;
        let (x_value, valid_value) = model.expressions(|expressions| {
            Ok((
                expressions
                    .at(at)
                    .coordinate(dae::CoordinateInput::PreDiscreteReal(x))?,
                expressions
                    .at(at)
                    .coordinate(dae::CoordinateInput::PreDiscreteValue(valid))?,
            ))
        })?;
        model.discrete(|discrete| {
            discrete.when_real_equation(guard, guard, at, |equation| equation.residual(x_value))
        })?;
        model.b1c([valid], |topology| {
            topology.owner(at, [valid], |owner| {
                owner.when(guard, guard, at, [(valid_value, at)])
            })?;
            Ok(())
        })?;
        model.model_events(|events| {
            events.transaction(
                [
                    dae::ModelEventTarget::DiscreteReal(x),
                    dae::ModelEventTarget::DiscreteValue(valid),
                ],
                [dae::ModelEventStep::new(
                    guard,
                    guard,
                    Some(clock.into()),
                    [
                        dae::ModelEventDefinition::new(
                            dae::ModelEventTarget::DiscreteReal(x),
                            x_value,
                            at,
                        ),
                        dae::ModelEventDefinition::new(
                            dae::ModelEventTarget::DiscreteValue(valid),
                            valid_value,
                            at,
                        ),
                    ],
                    at,
                )],
                at,
            )
        })?;
        Ok(())
    })
    .unwrap();

    model.inspect(|view| {
        let layout = crate::layout::lower_layout(view).unwrap();
        let clocks = crate::lower::clocks::lower_clocks(view, &layout).unwrap();
        let programs = lower_model_event_transactions(view, &layout, &clocks).unwrap();
        let [program] = programs.as_slice() else {
            panic!("one exact periodic event transaction expected")
        };
        let input_widths = program
            .input_types()
            .map(|value_type| value_type.scalar_count())
            .collect::<Vec<_>>();
        // Event-entry values, aligned held fallbacks, then one Boolean clock
        // activation lane.
        assert_eq!(input_widths, [2, 1, 2, 1, 1]);
        let target_widths = program
            .target_types()
            .map(|value_type| value_type.scalar_count())
            .collect::<Vec<_>>();
        assert_eq!(target_widths, [2, 1]);
        assert_eq!(program.statement_count(), 1);
        assert_eq!(program.clock_owners().len(), 1);
        let table = layout.pure_calls.borrow_mut().finish();
        assert_eq!(table.owners().len(), 1);
        assert!(table.matches_site(program.site()));
    });
}

#[test]
fn disjoint_periodic_regions_form_one_lazy_atomic_transaction() {
    let mut sources = SourceMap::new();
    let source = sources.add(
        "mixed_clock_transaction.mo",
        "discrete Real fast; discrete Real slow; when Clock() then end when;",
    );
    let at = dae::DaeProvenance::source(Span::from_offsets(source, 0, 66)).unwrap();
    let fast_lattice = rumoca_core::ClockLattice::from_interval_counter(1, 100).unwrap();
    let slow_lattice = rumoca_core::ClockLattice::from_interval_counter(1, 20).unwrap();
    let model = dae::Dae::construct(sources, |model| {
        let real = model.types(|types| {
            types.intern(
                rumoca_core::TypeId::new(0),
                dae::ValueType::scalar(dae::ScalarType::Real),
                at,
            )
        })?;
        let (fast, slow) = model.variables(|variables| {
            Ok((
                variables.discrete_real(
                    VarName::new("fast"),
                    real,
                    at,
                    dae::VariableAttributes::default(),
                )?,
                variables.discrete_real(
                    VarName::new("slow"),
                    real,
                    at,
                    dae::VariableAttributes::default(),
                )?,
            ))
        })?;
        let (fast_clock, slow_clock) = model.clocks(|clocks| {
            let fast_clock = clocks.periodic(fast_lattice, at)?;
            let slow_clock = clocks.periodic(slow_lattice, at)?;
            clocks.own_discrete_real(fast_clock.into(), fast, at)?;
            clocks.own_discrete_real(slow_clock.into(), slow, at)?;
            Ok((fast_clock, slow_clock))
        })?;
        let (fast_guard, slow_guard) = model
            .conditions(|conditions| Ok((conditions.reserve(at)?, conditions.reserve(at)?)))?;
        model.conditions(|conditions| {
            conditions.define(
                fast_guard,
                dae::ConditionInput::Clock(fast_clock.into()),
                at,
            )?;
            conditions.define(
                slow_guard,
                dae::ConditionInput::Clock(slow_clock.into()),
                at,
            )
        })?;
        let (fast_value, slow_value) = model.expressions(|expressions| {
            Ok((
                expressions
                    .at(at)
                    .coordinate(dae::CoordinateInput::PreDiscreteReal(fast))?,
                expressions
                    .at(at)
                    .coordinate(dae::CoordinateInput::PreDiscreteReal(slow))?,
            ))
        })?;
        model.model_events(|events| {
            events.transaction(
                [
                    dae::ModelEventTarget::DiscreteReal(fast),
                    dae::ModelEventTarget::DiscreteReal(slow),
                ],
                [
                    dae::ModelEventStep::new(
                        fast_guard,
                        fast_guard,
                        Some(fast_clock.into()),
                        [dae::ModelEventDefinition::new(
                            dae::ModelEventTarget::DiscreteReal(fast),
                            fast_value,
                            at,
                        )],
                        at,
                    ),
                    dae::ModelEventStep::new(
                        slow_guard,
                        slow_guard,
                        Some(slow_clock.into()),
                        [dae::ModelEventDefinition::new(
                            dae::ModelEventTarget::DiscreteReal(slow),
                            slow_value,
                            at,
                        )],
                        at,
                    ),
                ],
                at,
            )
        })?;
        Ok(())
    })
    .unwrap();

    model.inspect(|view| {
        let layout = crate::layout::lower_layout(view).unwrap();
        let clocks = crate::lower::clocks::lower_clocks(view, &layout).unwrap();
        let programs = lower_model_event_transactions(view, &layout, &clocks).unwrap();
        let [program] = programs.as_slice() else {
            panic!("one source algorithm must remain one atomic event transaction")
        };
        assert_eq!(program.statement_count(), 2);
        assert_eq!(program.clock_owners().len(), 2);
        assert_eq!(
            program.target_clock_owners().collect::<Vec<_>>(),
            program
                .clock_owners()
                .iter()
                .copied()
                .map(Some)
                .collect::<Vec<_>>()
        );
        assert_eq!(
            program
                .input_types()
                .map(|value_type| value_type.scalar_count())
                .collect::<Vec<_>>(),
            [1, 1, 1, 1, 1, 1]
        );
        let table = layout.pure_calls.borrow_mut().finish();
        let owner = table.owner(program.site().owner()).unwrap();
        let lazy_regions = owner
            .body()
            .operations()
            .iter()
            .filter(|operation| {
                matches!(
                    operation.operation(),
                    solve::SolveOperation::Conditional { .. }
                )
            })
            .count();
        assert_eq!(lazy_regions, 2);
        assert!(table.matches_site(program.site()));
    });
}

#[test]
fn aggregate_function_lowers_once_without_element_operations() {
    let mut sources = SourceMap::new();
    let source = sources.add("typed_function.mo", "function transform");
    let at = dae::DaeProvenance::source(Span::from_offsets(source, 0, 18)).unwrap();
    let model = dae::Dae::construct(sources, |model| {
        let (matrix, vector_two, vector_three) = model.types(|types| {
            Ok((
                types.derived(dae::ValueType::array(dae::ScalarType::Real, [2, 3]), at)?,
                types.derived(dae::ValueType::array(dae::ScalarType::Real, [2]), at)?,
                types.derived(dae::ValueType::array(dae::ScalarType::Real, [3]), at)?,
            ))
        })?;
        let signature = dae::FunctionSignature::new(
            VarName::new("transform"),
            [matrix, vector_two],
            [vector_three],
            at,
        );
        let (function, ()) = model.function(signature, |model, reservation| {
            let matrix = model.functions(|functions| {
                functions.parameter(&reservation, VarName::new("matrix"), 0, at)
            })?;
            let vector = model.functions(|functions| {
                functions.parameter(&reservation, VarName::new("vector"), 1, at)
            })?;
            let output = model.functions(|functions| {
                functions.output(&reservation, VarName::new("result"), 0, at)
            })?;
            let (matrix, vector) = model.expressions(|expressions| {
                Ok((
                    expressions.at(at).function_parameter(matrix)?,
                    expressions.at(at).function_parameter(vector)?,
                ))
            })?;
            let result = model.expressions(|expressions| {
                let transpose = expressions
                    .at(at)
                    .builtin(dae::PureBuiltin::Transpose, [matrix])?;
                expressions
                    .at(at)
                    .binary(dae::BinaryOperator::Multiply, transpose, vector)
            })?;
            let mut body = model.functions(|functions| functions.begin(reservation, at))?;
            model.functions(|functions| functions.assign(&mut body, output, result, at))?;
            model.functions(|functions| functions.define(body, at))
        })?;
        let (matrix, vector) = model.expressions(|expressions| {
            let values = (1..=6)
                .map(|value| {
                    expressions
                        .at(at)
                        .literal(dae::DaeLiteral::Real(f64::from(value)))
                })
                .collect::<Result<Vec<_>, _>>()?;
            let first = expressions.at(at).array(values[..3].iter().copied())?;
            let second = expressions.at(at).array(values[3..].iter().copied())?;
            let matrix = expressions.at(at).array([first, second])?;
            let vector = expressions.at(at).array(values[..2].iter().copied())?;
            Ok((matrix, vector))
        })?;
        model.expressions(|expressions| expressions.at(at).call(function, 0, [matrix, vector]))?;
        Ok(())
    })
    .unwrap();
    let table = lower_root_call(&model);
    let [owner] = table.owners() else {
        panic!("one exact call owner expected");
    };
    assert_eq!(owner.inputs().len(), 2);
    assert_eq!(owner.outputs().len(), 1);
    assert_eq!(owner.body().operations().len(), 5);
    assert!(owner.body().operations().iter().any(|operation| matches!(
        operation.operation(),
        solve::SolveOperation::Transpose { .. }
    )));
    assert!(owner.body().operations().iter().any(|operation| matches!(
        operation.operation(),
        solve::SolveOperation::MatrixMultiply { .. }
    )));
    assert!(owner.body().operations().iter().all(|operation| !matches!(
        operation.operation(),
        solve::SolveOperation::ProjectElement { .. }
            | solve::SolveOperation::ConstructAggregate { .. }
    )));
}

#[test]
fn function_tensor_geometry_builds_checked_compact_matrices() {
    let mut sources = SourceMap::new();
    let source = sources.add("typed_geometry.mo", "outerProduct(v,v); skew(v)");
    let at = dae::DaeProvenance::source(Span::from_offsets(source, 0, 26)).unwrap();
    let model = dae::Dae::construct(sources, |model| {
        let (vector, matrix) = model.types(|types| {
            Ok((
                types.derived(dae::ValueType::array(dae::ScalarType::Real, [3]), at)?,
                types.derived(dae::ValueType::array(dae::ScalarType::Real, [3, 3]), at)?,
            ))
        })?;
        let (geometry, ()) = model.function(
            dae::FunctionSignature::new(VarName::new("geometry"), [vector], [matrix, matrix], at),
            |model, reservation| {
                let input = model.functions(|functions| {
                    functions.parameter(&reservation, VarName::new("v"), 0, at)
                })?;
                let outer = model.functions(|functions| {
                    functions.output(&reservation, VarName::new("outer"), 0, at)
                })?;
                let skew = model.functions(|functions| {
                    functions.output(&reservation, VarName::new("skew"), 1, at)
                })?;
                let input = model
                    .expressions(|expressions| expressions.at(at).function_parameter(input))?;
                let (outer_value, skew_value) = model.expressions(|expressions| {
                    Ok((
                        expressions
                            .at(at)
                            .builtin(dae::PureBuiltin::OuterProduct, [input, input])?,
                        expressions
                            .at(at)
                            .builtin(dae::PureBuiltin::Skew, [input])?,
                    ))
                })?;
                let mut body = model.functions(|functions| functions.begin(reservation, at))?;
                model.functions(|functions| {
                    functions.assign(&mut body, outer, outer_value, at)?;
                    functions.assign(&mut body, skew, skew_value, at)?;
                    functions.define(body, at)
                })
            },
        )?;
        let input = model.expressions(|expressions| {
            let values = [1.0, 2.0, 3.0]
                .into_iter()
                .map(|value| expressions.at(at).literal(dae::DaeLiteral::Real(value)))
                .collect::<Result<Vec<_>, _>>()?;
            expressions.at(at).array(values)
        })?;
        model.expressions(|expressions| expressions.at(at).call(geometry, 0, [input]))?;
        Ok(())
    })
    .unwrap();

    let table = lower_root_call(&model);
    let [owner] = table.owners() else {
        panic!("one exact call owner expected")
    };
    let argument_type =
        solve::SolveValueType::tensor(solve::SolveScalarType::real(arithmetic_profile()), vec![3])
            .unwrap();
    let argument = rumoca_eval_solve::TypedValue::construct(
        argument_type,
        [1.0_f64, 2.0, 3.0]
            .map(|value| solve::SolveValueKind::Real64(value.to_bits()))
            .to_vec(),
    )
    .unwrap();
    let output = rumoca_eval_solve::eval_pure_call(&table, owner.id(), &[argument]).unwrap();
    let values = |index: usize| {
        output[index]
            .elements()
            .iter()
            .copied()
            .map(|value| match value {
                solve::SolveValueKind::Real64(bits) => f64::from_bits(bits),
                _ => panic!("geometry output is Real"),
            })
            .collect::<Vec<_>>()
    };
    assert_eq!(values(0), [1.0, 2.0, 3.0, 2.0, 4.0, 6.0, 3.0, 6.0, 9.0]);
    assert_eq!(values(1), [0.0, -3.0, 2.0, 3.0, 0.0, -1.0, -2.0, 1.0, 0.0]);
}

#[test]
#[expect(
    clippy::too_many_lines,
    reason = "valid-by-construction nested-call fixture enumerates result and assertion ownership together"
)]
fn nested_call_returns_result_and_assertion_predicate_atomically() {
    let mut sources = SourceMap::new();
    let source = sources.add("typed_nested_function.mo", "function guarded outer call");
    let at = dae::DaeProvenance::source(Span::from_offsets(source, 0, 8)).unwrap();
    let model = dae::Dae::construct(sources, |model| {
        let real = model
            .types(|types| types.derived(dae::ValueType::scalar(dae::ScalarType::Real), at))?;
        let guarded = model
            .function(
                dae::FunctionSignature::new(VarName::new("guarded"), [real], [real], at),
                |model, reservation| {
                    let input = model.functions(|functions| {
                        functions.parameter(&reservation, VarName::new("input"), 0, at)
                    })?;
                    let output = model.functions(|functions| {
                        functions.output(&reservation, VarName::new("result"), 0, at)
                    })?;
                    let input = model
                        .expressions(|expressions| expressions.at(at).function_parameter(input))?;
                    let (zero, two, message) = model.expressions(|expressions| {
                        Ok((
                            expressions.at(at).literal(dae::DaeLiteral::Real(0.0))?,
                            expressions.at(at).literal(dae::DaeLiteral::Real(2.0))?,
                            expressions
                                .at(at)
                                .literal(dae::DaeLiteral::String("positive".to_owned()))?,
                        ))
                    })?;
                    let condition = model.expressions(|expressions| {
                        expressions
                            .at(at)
                            .binary(dae::BinaryOperator::Greater, input, zero)
                    })?;
                    let result = model.expressions(|expressions| {
                        expressions
                            .at(at)
                            .binary(dae::BinaryOperator::Multiply, input, two)
                    })?;
                    let mut body = model.functions(|functions| functions.begin(reservation, at))?;
                    model.functions(|functions| {
                        functions.assertion(&mut body, condition, message, at)
                    })?;
                    model.functions(|functions| functions.assign(&mut body, output, result, at))?;
                    model.functions(|functions| functions.define(body, at))
                },
            )?
            .0;
        let outer = model
            .function(
                dae::FunctionSignature::new(VarName::new("outer"), [real], [real], at),
                |model, reservation| {
                    let input = model.functions(|functions| {
                        functions.parameter(&reservation, VarName::new("input"), 0, at)
                    })?;
                    let output = model.functions(|functions| {
                        functions.output(&reservation, VarName::new("result"), 0, at)
                    })?;
                    let input = model
                        .expressions(|expressions| expressions.at(at).function_parameter(input))?;
                    let guarded = model
                        .expressions(|expressions| expressions.at(at).call(guarded, 0, [input]))?;
                    let one = model.expressions(|expressions| {
                        expressions.at(at).literal(dae::DaeLiteral::Real(1.0))
                    })?;
                    let result = model.expressions(|expressions| {
                        expressions
                            .at(at)
                            .binary(dae::BinaryOperator::Add, guarded, one)
                    })?;
                    let mut body = model.functions(|functions| functions.begin(reservation, at))?;
                    model.functions(|functions| functions.assign(&mut body, output, result, at))?;
                    model.functions(|functions| functions.define(body, at))
                },
            )?
            .0;
        let argument = model
            .expressions(|expressions| expressions.at(at).literal(dae::DaeLiteral::Real(3.0)))?;
        model.expressions(|expressions| expressions.at(at).call(outer, 0, [argument]))?;
        Ok(())
    })
    .unwrap();
    let table = lower_root_call(&model);

    assert_eq!(table.owners().len(), 2);
    let inner = &table.owners()[0];
    let outer = &table.owners()[1];
    assert_eq!(inner.outputs().len(), 2);
    assert_eq!(outer.outputs().len(), 2);
    assert_eq!(
        outer
            .body()
            .operations()
            .iter()
            .filter(|operation| matches!(operation.operation(), solve::SolveOperation::Call { .. }))
            .count(),
        1
    );
    let argument = rumoca_eval_solve::TypedValue::construct(
        solve::SolveValueType::scalar(solve::SolveScalarType::real(arithmetic_profile())),
        vec![solve::SolveValueKind::Real64(3.0_f64.to_bits())],
    )
    .unwrap();
    let result = rumoca_eval_solve::eval_pure_call(&table, outer.id(), &[argument]).unwrap();
    assert_eq!(
        result[0].elements(),
        [solve::SolveValueKind::Real64(7.0_f64.to_bits())]
    );
    assert_eq!(result[1].elements(), [solve::SolveValueKind::Boolean(true)]);
}

#[test]
fn nested_multi_result_projections_execute_one_issued_call_owner() {
    let mut sources = SourceMap::new();
    let source = sources.add("typed_multi_result.mo", "function pair outer call");
    let at = dae::DaeProvenance::source(Span::from_offsets(source, 0, 8)).unwrap();
    let model = dae::Dae::construct(sources, |model| {
        let real = model
            .types(|types| types.derived(dae::ValueType::scalar(dae::ScalarType::Real), at))?;
        let pair = model
            .function(
                dae::FunctionSignature::new(VarName::new("pair"), [real], [real, real], at),
                |model, reservation| {
                    let input = model.functions(|functions| {
                        functions.parameter(&reservation, VarName::new("input"), 0, at)
                    })?;
                    let first = model.functions(|functions| {
                        functions.output(&reservation, VarName::new("first"), 0, at)
                    })?;
                    let second = model.functions(|functions| {
                        functions.output(&reservation, VarName::new("second"), 1, at)
                    })?;
                    let input = model
                        .expressions(|expressions| expressions.at(at).function_parameter(input))?;
                    let two = model.expressions(|expressions| {
                        expressions.at(at).literal(dae::DaeLiteral::Real(2.0))
                    })?;
                    let doubled = model.expressions(|expressions| {
                        expressions
                            .at(at)
                            .binary(dae::BinaryOperator::Multiply, input, two)
                    })?;
                    let mut body = model.functions(|functions| functions.begin(reservation, at))?;
                    model.functions(|functions| {
                        functions.assign(&mut body, first, input, at)?;
                        functions.assign(&mut body, second, doubled, at)?;
                        functions.define(body, at)
                    })
                },
            )?
            .0;
        let outer = model
            .function(
                dae::FunctionSignature::new(VarName::new("outer"), [real], [real], at),
                |model, reservation| {
                    let input = model.functions(|functions| {
                        functions.parameter(&reservation, VarName::new("input"), 0, at)
                    })?;
                    let output = model.functions(|functions| {
                        functions.output(&reservation, VarName::new("result"), 0, at)
                    })?;
                    let input = model
                        .expressions(|expressions| expressions.at(at).function_parameter(input))?;
                    let results = model.expressions(|expressions| {
                        expressions.at(at).call_results(pair, [0, 1], [input])
                    })?;
                    let sum = model.expressions(|expressions| {
                        expressions
                            .at(at)
                            .binary(dae::BinaryOperator::Add, results[0], results[1])
                    })?;
                    let mut body = model.functions(|functions| functions.begin(reservation, at))?;
                    model.functions(|functions| functions.assign(&mut body, output, sum, at))?;
                    model.functions(|functions| functions.define(body, at))
                },
            )?
            .0;
        let argument = model
            .expressions(|expressions| expressions.at(at).literal(dae::DaeLiteral::Real(3.0)))?;
        model.expressions(|expressions| expressions.at(at).call(outer, 0, [argument]))?;
        Ok(())
    })
    .unwrap();
    let table = lower_root_call(&model);

    assert_eq!(table.owners().len(), 2);
    let outer = &table.owners()[1];
    assert_eq!(
        outer
            .body()
            .operations()
            .iter()
            .filter(|operation| matches!(operation.operation(), solve::SolveOperation::Call { .. }))
            .count(),
        1
    );
    let argument = rumoca_eval_solve::TypedValue::construct(
        solve::SolveValueType::scalar(solve::SolveScalarType::real(arithmetic_profile())),
        vec![solve::SolveValueKind::Real64(3.0_f64.to_bits())],
    )
    .unwrap();
    let result = rumoca_eval_solve::eval_pure_call(&table, outer.id(), &[argument]).unwrap();
    assert_eq!(
        result[0].elements(),
        [solve::SolveValueKind::Real64(9.0_f64.to_bits())]
    );
}

#[test]
#[expect(
    clippy::too_many_lines,
    reason = "valid-by-construction conditional fixture enumerates every correlated result branch"
)]
fn correlated_assignment_lowers_as_one_lazy_typed_region() {
    let mut sources = SourceMap::new();
    let source = sources.add("typed_conditional.mo", "function choose");
    let at = dae::DaeProvenance::source(Span::from_offsets(source, 0, 15)).unwrap();
    let model = dae::Dae::construct(sources, |model| {
        let (boolean, real) = model.types(|types| {
            Ok((
                types.derived(dae::ValueType::scalar(dae::ScalarType::Boolean), at)?,
                types.derived(dae::ValueType::scalar(dae::ScalarType::Real), at)?,
            ))
        })?;
        let signature = dae::FunctionSignature::new(
            VarName::new("choose"),
            [boolean, boolean],
            [real, real],
            at,
        );
        let (function, ()) = model.function(signature, |model, reservation| {
            let first = model.functions(|functions| {
                functions.parameter(&reservation, VarName::new("first"), 0, at)
            })?;
            let second = model.functions(|functions| {
                functions.parameter(&reservation, VarName::new("second"), 1, at)
            })?;
            let left = model.functions(|functions| {
                functions.output(&reservation, VarName::new("left"), 0, at)
            })?;
            let right = model.functions(|functions| {
                functions.output(&reservation, VarName::new("right"), 1, at)
            })?;
            let conditions = model.expressions(|expressions| {
                Ok([
                    expressions.at(at).function_parameter(first)?,
                    expressions.at(at).function_parameter(second)?,
                ])
            })?;
            let values = model.expressions(|expressions| {
                [1.0, 2.0, 3.0, 4.0, 5.0, 6.0]
                    .into_iter()
                    .map(|value| expressions.at(at).literal(dae::DaeLiteral::Real(value)))
                    .collect::<Result<Vec<_>, _>>()
            })?;
            let mut body = model.functions(|functions| functions.begin(reservation, at))?;
            model.functions(|functions| {
                functions.assign_conditional_all(
                    &mut body,
                    &[left, right],
                    &conditions,
                    &[vec![values[0], values[1]], vec![values[2], values[3]]],
                    &[values[4], values[5]],
                    at,
                )
            })?;
            model.functions(|functions| functions.define(body, at))
        })?;
        let conditions = model.expressions(|expressions| {
            Ok([
                expressions
                    .at(at)
                    .literal(dae::DaeLiteral::Boolean(false))?,
                expressions.at(at).literal(dae::DaeLiteral::Boolean(true))?,
            ])
        })?;
        model.expressions(|expressions| expressions.at(at).call(function, 0, conditions))?;
        Ok(())
    })
    .unwrap();
    let table = lower_root_call(&model);
    let [owner] = table.owners() else {
        panic!("one exact owner expected")
    };
    assert_eq!(
        owner
            .body()
            .operations()
            .iter()
            .filter(|operation| matches!(
                operation.operation(),
                solve::SolveOperation::Conditional { .. }
            ))
            .count(),
        1
    );
    assert!(
        owner.body().operations().iter().all(|operation| !matches!(
            operation.operation(),
            solve::SolveOperation::Select { .. }
        ))
    );
    let condition = |value| {
        rumoca_eval_solve::TypedValue::construct(
            solve::SolveValueType::scalar(solve::SolveScalarType::Boolean),
            vec![solve::SolveValueKind::Boolean(value)],
        )
        .unwrap()
    };
    let result =
        rumoca_eval_solve::eval_pure_call(&table, owner.id(), &[condition(false), condition(true)])
            .unwrap();
    assert_eq!(
        result
            .iter()
            .map(|value| value.elements()[0])
            .collect::<Vec<_>>(),
        [
            solve::SolveValueKind::Real64(3.0_f64.to_bits()),
            solve::SolveValueKind::Real64(4.0_f64.to_bits()),
        ]
    );
}

/// A fold update can demand a member of a correlated assignment group before
/// the ordinary statement walk reaches that group. The demand must issue the
/// complete group once: lowering each joined definition independently clones
/// the shared predecessor and turns a small loop body into combinatorial IR.
#[test]
#[expect(
    clippy::too_many_lines,
    reason = "valid-by-construction fold fixture states the correlated tuple and its reverse demand explicitly"
)]
fn fold_reverse_demand_lowers_one_correlated_assignment_group() {
    let mut sources = SourceMap::new();
    let source = sources.add(
        "typed_fold_conditional.mo",
        "for i in 1:3 loop shared := if c then 2 else 3; if c then x := x + shared; y := y + shared; end if; end for",
    );
    let at = dae::DaeProvenance::source(Span::from_offsets(source, 0, 12)).unwrap();
    let model = dae::Dae::construct(sources, |model| {
        let (boolean, integer) = model.types(|types| {
            Ok((
                types.derived(dae::ValueType::scalar(dae::ScalarType::Boolean), at)?,
                types.derived(dae::ValueType::scalar(dae::ScalarType::Integer), at)?,
            ))
        })?;
        let (function, ()) = model.function(
            dae::FunctionSignature::new(
                VarName::new("fold_conditional"),
                [boolean],
                [integer, integer],
                at,
            ),
            |model, reservation| {
                let condition = model.functions(|functions| {
                    functions.parameter(&reservation, VarName::new("c"), 0, at)
                })?;
                let (x, y) = model.functions(|functions| {
                    Ok((
                        functions.output(&reservation, VarName::new("x"), 0, at)?,
                        functions.output(&reservation, VarName::new("y"), 1, at)?,
                    ))
                })?;
                let shared = model.functions(|functions| {
                    functions.local(&reservation, VarName::new("shared"), integer, at)
                })?;
                let condition = model
                    .expressions(|expressions| expressions.at(at).function_parameter(condition))?;
                let [zero, two, three] = model.expressions(|expressions| {
                    [0, 2, 3]
                        .into_iter()
                        .map(|value| expressions.at(at).literal(dae::DaeLiteral::Integer(value)))
                        .collect::<Result<Vec<_>, _>>()
                        .map(|values| values.try_into().unwrap())
                })?;
                let mut body = model.functions(|functions| functions.begin(reservation, at))?;
                model.functions(|functions| {
                    functions.assign(&mut body, x, zero, at)?;
                    functions.assign(&mut body, y, zero, at)?;
                    functions.assign(&mut body, shared, zero, at)
                })?;
                let domain = model.domains(|domains| {
                    domains.structured(
                        StructuredIndexDomain {
                            binders: vec![StructuredIndexBinder {
                                id: 0,
                                display_name: "i".to_owned(),
                                lower: 1,
                                upper: 3,
                                step: 1,
                            }],
                        },
                        at,
                    )
                })?;
                // Put the correlated results before `shared` in the carried
                // tuple so their demand reaches the shared definition first.
                let mut loop_body = model.functions(|functions| {
                    functions.begin_loop(body, domain, [x, y, shared], at)
                })?;
                let (old_x, old_y) = model.functions(|functions| {
                    Ok((
                        functions.read(loop_body.body(), x, at)?,
                        functions.read(loop_body.body(), y, at)?,
                    ))
                })?;
                let selected = model.expressions(|expressions| {
                    expressions.at(at).conditional([(condition, two)], three)
                })?;
                model.functions(|functions| {
                    functions.assign_loop(&mut loop_body, shared, selected, at)
                })?;
                let shared_value =
                    model.functions(|functions| functions.read(loop_body.body(), shared, at))?;
                let (next_x, next_y) = model.expressions(|expressions| {
                    Ok((
                        expressions
                            .at(at)
                            .binary(dae::BinaryOperator::Add, old_x, shared_value)?,
                        expressions
                            .at(at)
                            .binary(dae::BinaryOperator::Add, old_y, shared_value)?,
                    ))
                })?;
                model.functions(|functions| {
                    functions.assign_conditional_all_loop(
                        &mut loop_body,
                        &[x, y],
                        &[condition],
                        &[vec![next_x, next_y]],
                        &[old_x, old_y],
                        at,
                    )
                })?;
                let body = model.functions(|functions| functions.finish_loop(loop_body, at))?;
                model.functions(|functions| functions.define(body, at))
            },
        )?;
        let enabled = model.expressions(|expressions| {
            expressions.at(at).literal(dae::DaeLiteral::Boolean(true))
        })?;
        model.expressions(|expressions| expressions.at(at).call(function, 0, [enabled]))?;
        Ok(())
    })
    .unwrap();
    let table = lower_root_call(&model);
    let [owner] = table.owners() else {
        panic!("one exact fold owner expected")
    };
    assert_eq!(conditional_count(owner.body()), 2);
    let enabled = rumoca_eval_solve::TypedValue::construct(
        solve::SolveValueType::scalar(solve::SolveScalarType::Boolean),
        vec![solve::SolveValueKind::Boolean(true)],
    )
    .unwrap();
    let output = rumoca_eval_solve::eval_pure_call(&table, owner.id(), &[enabled]).unwrap();
    assert_eq!(output[0].elements(), [solve::SolveValueKind::Integer(6)]);
    assert_eq!(output[1].elements(), [solve::SolveValueKind::Integer(6)]);
}

#[test]
fn function_fold_stays_one_compact_typed_owner() {
    let mut sources = SourceMap::new();
    let source = sources.add("typed_fold.mo", "function sum for i in 1:3 loop");
    let at = dae::DaeProvenance::source(Span::from_offsets(source, 0, 8)).unwrap();
    let model = dae::Dae::construct(sources, |model| {
        let integer = model
            .types(|types| types.derived(dae::ValueType::scalar(dae::ScalarType::Integer), at))?;
        let (function, ()) = model.function(
            dae::FunctionSignature::new(VarName::new("sum"), [], [integer], at),
            |model, reservation| {
                let output = model.functions(|functions| {
                    functions.output(&reservation, VarName::new("result"), 0, at)
                })?;
                let zero = model.expressions(|expressions| {
                    expressions.at(at).literal(dae::DaeLiteral::Integer(0))
                })?;
                let mut body = model.functions(|functions| functions.begin(reservation, at))?;
                model.functions(|functions| functions.assign(&mut body, output, zero, at))?;
                let domain = model.domains(|domains| {
                    domains.structured(
                        StructuredIndexDomain {
                            binders: vec![StructuredIndexBinder {
                                id: 0,
                                display_name: "i".to_owned(),
                                lower: 1,
                                upper: 3,
                                step: 1,
                            }],
                        },
                        at,
                    )
                })?;
                let binder = model.domains(|domains| domains.binder(domain, 0, at))?;
                let mut loop_body = model
                    .functions(|functions| functions.begin_loop(body, domain, [output], at))?;
                let current =
                    model.functions(|functions| functions.read(loop_body.body(), output, at))?;
                let binder = model.expressions(|expressions| expressions.at(at).binder(binder))?;
                let condition = model.expressions(|expressions| {
                    expressions
                        .at(at)
                        .binary(dae::BinaryOperator::Greater, binder, zero)
                })?;
                let message = model.expressions(|expressions| {
                    expressions
                        .at(at)
                        .literal(dae::DaeLiteral::String("positive index".to_owned()))
                })?;
                model.functions(|functions| {
                    functions.assertion_loop(&mut loop_body, condition, message, at)
                })?;
                let update = model.expressions(|expressions| {
                    expressions
                        .at(at)
                        .binary(dae::BinaryOperator::Add, current, binder)
                })?;
                model.functions(|functions| {
                    functions.assign_loop(&mut loop_body, output, update, at)
                })?;
                let body = model.functions(|functions| functions.finish_loop(loop_body, at))?;
                model.functions(|functions| functions.define(body, at))
            },
        )?;
        model.expressions(|expressions| expressions.at(at).call(function, 0, []))?;
        Ok(())
    })
    .unwrap();
    let table = lower_root_call(&model);
    let [owner] = table.owners() else {
        panic!("one exact fold owner expected")
    };
    assert_eq!(
        owner
            .body()
            .operations()
            .iter()
            .filter(|operation| matches!(operation.operation(), solve::SolveOperation::Fold { .. }))
            .count(),
        1
    );
    assert_eq!(
        owner
            .body()
            .operations()
            .iter()
            .filter(|operation| matches!(operation.operation(), solve::SolveOperation::Map { .. }))
            .count(),
        1
    );
    let output = rumoca_eval_solve::eval_pure_call(&table, owner.id(), &[]).unwrap();
    assert_eq!(output[0].elements(), [solve::SolveValueKind::Integer(6)]);
    assert_eq!(output[1].elements(), [solve::SolveValueKind::Boolean(true)]);
}

#[test]
fn function_fold_preserves_sequential_carried_redefinitions() {
    let mut sources = SourceMap::new();
    let source = sources.add(
        "typed_sequential_fold.mo",
        "for i in 1:3 loop first := first + 1; second := first; end for",
    );
    let at = dae::DaeProvenance::source(Span::from_offsets(source, 0, 8)).unwrap();
    let model = dae::Dae::construct(sources, |model| {
        let integer = model
            .types(|types| types.derived(dae::ValueType::scalar(dae::ScalarType::Integer), at))?;
        let (function, ()) = model.function(
            dae::FunctionSignature::new(VarName::new("sequential"), [], [integer, integer], at),
            |model, reservation| {
                let first = model.functions(|functions| {
                    functions.output(&reservation, VarName::new("first"), 0, at)
                })?;
                let second = model.functions(|functions| {
                    functions.output(&reservation, VarName::new("second"), 1, at)
                })?;
                let (zero, one) = model.expressions(|expressions| {
                    Ok((
                        expressions.at(at).literal(dae::DaeLiteral::Integer(0))?,
                        expressions.at(at).literal(dae::DaeLiteral::Integer(1))?,
                    ))
                })?;
                let mut body = model.functions(|functions| functions.begin(reservation, at))?;
                model.functions(|functions| {
                    functions.assign(&mut body, first, zero, at)?;
                    functions.assign(&mut body, second, zero, at)
                })?;
                let domain = model.domains(|domains| {
                    domains.structured(
                        StructuredIndexDomain {
                            binders: vec![StructuredIndexBinder {
                                id: 0,
                                display_name: "i".to_owned(),
                                lower: 1,
                                upper: 3,
                                step: 1,
                            }],
                        },
                        at,
                    )
                })?;
                let mut loop_body = model.functions(|functions| {
                    functions.begin_loop(body, domain, [first, second], at)
                })?;
                let previous_first =
                    model.functions(|functions| functions.read(loop_body.body(), first, at))?;
                let incremented = model.expressions(|expressions| {
                    expressions
                        .at(at)
                        .binary(dae::BinaryOperator::Add, previous_first, one)
                })?;
                model.functions(|functions| {
                    functions.assign_loop(&mut loop_body, first, incremented, at)
                })?;
                let updated_first =
                    model.functions(|functions| functions.read(loop_body.body(), first, at))?;
                model.functions(|functions| {
                    functions.assign_loop(&mut loop_body, second, updated_first, at)
                })?;
                let body = model.functions(|functions| functions.finish_loop(loop_body, at))?;
                model.functions(|functions| functions.define(body, at))
            },
        )?;
        model.expressions(|expressions| expressions.at(at).call(function, 0, []))?;
        Ok(())
    })
    .unwrap();
    let table = lower_root_call(&model);
    let [owner] = table.owners() else {
        panic!("one exact sequential-fold owner expected")
    };
    assert_eq!(
        owner
            .body()
            .operations()
            .iter()
            .filter(|operation| matches!(operation.operation(), solve::SolveOperation::Fold { .. }))
            .count(),
        1
    );
    let output = rumoca_eval_solve::eval_pure_call(&table, owner.id(), &[]).unwrap();
    assert_eq!(output[0].elements(), [solve::SolveValueKind::Integer(3)]);
    assert_eq!(output[1].elements(), [solve::SolveValueKind::Integer(3)]);
}

#[test]
#[expect(
    clippy::too_many_lines,
    reason = "valid-by-construction assertion-loop fixture enumerates map reduction ownership"
)]
fn assertion_only_loop_uses_map_reduction_without_empty_fold() {
    let mut sources = SourceMap::new();
    let source = sources.add("typed_assertion_loop.mo", "for i in 1:3 assert i <= limit");
    let at = dae::DaeProvenance::source(Span::from_offsets(source, 0, 12)).unwrap();
    let model = dae::Dae::construct(sources, |model| {
        let integer = model
            .types(|types| types.derived(dae::ValueType::scalar(dae::ScalarType::Integer), at))?;
        let (function, ()) = model.function(
            dae::FunctionSignature::new(VarName::new("checked"), [integer], [integer], at),
            |model, reservation| {
                let limit = model.functions(|functions| {
                    functions.parameter(&reservation, VarName::new("limit"), 0, at)
                })?;
                let output = model.functions(|functions| {
                    functions.output(&reservation, VarName::new("result"), 0, at)
                })?;
                let seven = model.expressions(|expressions| {
                    expressions.at(at).literal(dae::DaeLiteral::Integer(7))
                })?;
                let mut body = model.functions(|functions| functions.begin(reservation, at))?;
                model.functions(|functions| functions.assign(&mut body, output, seven, at))?;
                let domain = model.domains(|domains| {
                    domains.structured(
                        StructuredIndexDomain {
                            binders: vec![StructuredIndexBinder {
                                id: 0,
                                display_name: "i".to_owned(),
                                lower: 1,
                                upper: 3,
                                step: 1,
                            }],
                        },
                        at,
                    )
                })?;
                let binder = model.domains(|domains| domains.binder(domain, 0, at))?;
                let mut loop_body =
                    model.functions(|functions| functions.begin_loop(body, domain, [], at))?;
                let (binder, limit) = model.expressions(|expressions| {
                    Ok((
                        expressions.at(at).binder(binder)?,
                        expressions.at(at).function_parameter(limit)?,
                    ))
                })?;
                let condition = model.expressions(|expressions| {
                    expressions
                        .at(at)
                        .binary(dae::BinaryOperator::LessEqual, binder, limit)
                })?;
                let message = model.expressions(|expressions| {
                    expressions
                        .at(at)
                        .literal(dae::DaeLiteral::String("bounded".to_owned()))
                })?;
                model.functions(|functions| {
                    functions.assertion_loop(&mut loop_body, condition, message, at)
                })?;
                let body = model.functions(|functions| functions.finish_loop(loop_body, at))?;
                model.functions(|functions| functions.define(body, at))
            },
        )?;
        let three = model
            .expressions(|expressions| expressions.at(at).literal(dae::DaeLiteral::Integer(3)))?;
        model.expressions(|expressions| expressions.at(at).call(function, 0, [three]))?;
        Ok(())
    })
    .unwrap();
    let table = lower_root_call(&model);
    let [owner] = table.owners() else {
        panic!("one exact assertion-loop owner expected")
    };
    assert!(
        owner
            .body()
            .operations()
            .iter()
            .all(|operation| !matches!(operation.operation(), solve::SolveOperation::Fold { .. }))
    );
    assert!(
        owner
            .body()
            .operations()
            .iter()
            .any(|operation| matches!(operation.operation(), solve::SolveOperation::Map { .. }))
    );
    let argument = |value| {
        rumoca_eval_solve::TypedValue::construct(
            owner.inputs()[0].clone(),
            vec![solve::SolveValueKind::Integer(value)],
        )
        .unwrap()
    };
    let accepted = rumoca_eval_solve::eval_pure_call(&table, owner.id(), &[argument(3)]).unwrap();
    let rejected = rumoca_eval_solve::eval_pure_call(&table, owner.id(), &[argument(2)]).unwrap();
    assert_eq!(
        accepted[1].elements(),
        [solve::SolveValueKind::Boolean(true)]
    );
    assert_eq!(
        rejected[1].elements(),
        [solve::SolveValueKind::Boolean(false)]
    );
}

#[test]
fn record_result_is_an_ordered_tuple_of_compact_tensor_leaves() {
    let mut sources = SourceMap::new();
    let source = sources.add("typed_record_function.mo", "function state tensors");
    let at = dae::DaeProvenance::source(Span::from_offsets(source, 0, 16)).unwrap();
    let model = dae::Dae::construct(sources, |model| {
        let (vector, covariance) = model.types(|types| {
            Ok((
                types.derived(dae::ValueType::array(dae::ScalarType::Real, [3]), at)?,
                types.derived(dae::ValueType::array(dae::ScalarType::Real, [15, 15]), at)?,
            ))
        })?;
        let state = model.types(|types| {
            types.record(
                VarName::new("State"),
                [
                    (VarName::new("position"), vector),
                    (VarName::new("covariance"), covariance),
                ],
                at,
            )
        })?;
        let signature =
            dae::FunctionSignature::new(VarName::new("state"), [vector, covariance], [state], at);
        let (function, ()) = model.function(signature, |model, reservation| {
            let position = model.functions(|functions| {
                functions.parameter(&reservation, VarName::new("position"), 0, at)
            })?;
            let covariance = model.functions(|functions| {
                functions.parameter(&reservation, VarName::new("covariance"), 1, at)
            })?;
            let output = model.functions(|functions| {
                functions.output(&reservation, VarName::new("state"), 0, at)
            })?;
            let fields = model.expressions(|expressions| {
                Ok([
                    expressions.at(at).function_parameter(position)?,
                    expressions.at(at).function_parameter(covariance)?,
                ])
            })?;
            let value =
                model.expressions(|expressions| expressions.at(at).record(state, fields))?;
            let mut body = model.functions(|functions| functions.begin(reservation, at))?;
            model.functions(|functions| functions.assign(&mut body, output, value, at))?;
            model.functions(|functions| functions.define(body, at))
        })?;
        let (position, covariance) = model.expressions(|expressions| {
            let zero = expressions.at(at).literal(dae::DaeLiteral::Real(0.0))?;
            let position = expressions.at(at).array([zero, zero, zero])?;
            let row = expressions.at(at).array(std::iter::repeat_n(zero, 15))?;
            let covariance = expressions.at(at).array(std::iter::repeat_n(row, 15))?;
            Ok((position, covariance))
        })?;
        model.expressions(|expressions| {
            expressions.at(at).call(function, 0, [position, covariance])
        })?;
        Ok(())
    })
    .unwrap();

    let table = lower_root_call(&model);

    let [owner] = table.owners() else {
        panic!("one exact call owner expected");
    };
    assert_eq!(owner.inputs().len(), 2);
    assert_eq!(owner.outputs().len(), 2);
    assert_eq!(owner.body().operations().len(), 4);
    assert_eq!(owner.inputs()[0].dimensions(), [3]);
    assert_eq!(owner.inputs()[1].dimensions(), [15, 15]);
    assert_eq!(owner.outputs()[0].value_type().dimensions(), [3]);
    assert_eq!(owner.outputs()[1].value_type().dimensions(), [15, 15]);
    assert!(owner.body().operations().iter().all(|operation| !matches!(
        operation.operation(),
        solve::SolveOperation::ConstructAggregate { .. }
            | solve::SolveOperation::ProjectElement { .. }
    )));
}

#[test]
fn record_constructor_coerces_integer_tensor_field_to_declared_real_type() {
    let mut sources = SourceMap::new();
    let source = sources.add("typed_record_coercion.mo", "R := Orientation(identity(3))");
    let at = dae::DaeProvenance::source(Span::from_offsets(source, 0, 29)).unwrap();
    let model = dae::Dae::construct(sources, |model| {
        let matrix = model.types(|types| {
            types.derived(dae::ValueType::array(dae::ScalarType::Real, [3, 3]), at)
        })?;
        let orientation = model.types(|types| {
            types.record(
                VarName::new("Orientation"),
                [(VarName::new("T"), matrix)],
                at,
            )
        })?;
        let (function, ()) = model.function(
            dae::FunctionSignature::new(VarName::new("nullRotation"), [], [orientation], at),
            |model, reservation| {
                let output = model.functions(|functions| {
                    functions.output(&reservation, VarName::new("R"), 0, at)
                })?;
                let value = model.expressions(|expressions| {
                    let three = expressions.at(at).literal(dae::DaeLiteral::Integer(3))?;
                    let identity = expressions
                        .at(at)
                        .builtin(dae::PureBuiltin::Identity, [three])?;
                    expressions.at(at).record(orientation, [identity])
                })?;
                let mut body = model.functions(|functions| functions.begin(reservation, at))?;
                model.functions(|functions| functions.assign(&mut body, output, value, at))?;
                model.functions(|functions| functions.define(body, at))
            },
        )?;
        model.expressions(|expressions| expressions.at(at).call(function, 0, []))?;
        Ok(())
    })
    .unwrap();

    let table = lower_root_call(&model);
    let [owner] = table.owners() else {
        panic!("one exact call owner expected")
    };
    assert_eq!(owner.outputs()[0].value_type().dimensions(), [3, 3]);
    let output = rumoca_eval_solve::eval_pure_call(&table, owner.id(), &[]).unwrap();
    assert_eq!(output.len(), 1);
    assert!(
        output[0]
            .elements()
            .iter()
            .all(|value| matches!(value, solve::SolveValueKind::Real64(_)))
    );
}

#[test]
fn real_array_constructor_explicitly_widens_integer_elements() {
    let mut sources = SourceMap::new();
    let source = sources.add("typed_array_coercion.mo", "y := {0, 1.0, 2.0}");
    let at = dae::DaeProvenance::source(Span::from_offsets(source, 0, 18)).unwrap();
    let model = dae::Dae::construct(sources, |model| {
        let vector = model
            .types(|types| types.derived(dae::ValueType::array(dae::ScalarType::Real, [3]), at))?;
        let (function, ()) = model.function(
            dae::FunctionSignature::new(VarName::new("mixedRealVector"), [], [vector], at),
            |model, reservation| {
                let output = model.functions(|functions| {
                    functions.output(&reservation, VarName::new("y"), 0, at)
                })?;
                let value = model.expressions(|expressions| {
                    let integer = expressions.at(at).literal(dae::DaeLiteral::Integer(0))?;
                    let one = expressions.at(at).literal(dae::DaeLiteral::Real(1.0))?;
                    let two = expressions.at(at).literal(dae::DaeLiteral::Real(2.0))?;
                    expressions.at(at).array([integer, one, two])
                })?;
                let mut body = model.functions(|functions| functions.begin(reservation, at))?;
                model.functions(|functions| functions.assign(&mut body, output, value, at))?;
                model.functions(|functions| functions.define(body, at))
            },
        )?;
        model.expressions(|expressions| expressions.at(at).call(function, 0, []))?;
        Ok(())
    })
    .unwrap();

    let table = lower_root_call(&model);
    let [owner] = table.owners() else {
        panic!("one exact call owner expected")
    };
    let output = rumoca_eval_solve::eval_pure_call(&table, owner.id(), &[]).unwrap();
    assert_eq!(
        output[0].elements(),
        [
            solve::SolveValueKind::Real64(0.0_f64.to_bits()),
            solve::SolveValueKind::Real64(1.0_f64.to_bits()),
            solve::SolveValueKind::Real64(2.0_f64.to_bits()),
        ]
    );
}

#[test]
fn smooth_in_function_body_preserves_compact_tensor_value() {
    let mut sources = SourceMap::new();
    let source = sources.add("typed_smooth_vector.mo", "y := smooth(0, u)");
    let at = dae::DaeProvenance::source(Span::from_offsets(source, 0, 17)).unwrap();
    let model = dae::Dae::construct(sources, |model| {
        let vector = model
            .types(|types| types.derived(dae::ValueType::array(dae::ScalarType::Real, [3]), at))?;
        let (function, ()) = model.function(
            dae::FunctionSignature::new(VarName::new("smoothVector"), [vector], [vector], at),
            |model, reservation| {
                let input = model.functions(|functions| {
                    functions.parameter(&reservation, VarName::new("u"), 0, at)
                })?;
                let output = model.functions(|functions| {
                    functions.output(&reservation, VarName::new("y"), 0, at)
                })?;
                let value = model.expressions(|expressions| {
                    let order = expressions.at(at).literal(dae::DaeLiteral::Integer(0))?;
                    let input = expressions.at(at).function_parameter(input)?;
                    expressions
                        .at(at)
                        .builtin(dae::PureBuiltin::Smooth, [order, input])
                })?;
                let mut body = model.functions(|functions| functions.begin(reservation, at))?;
                model.functions(|functions| functions.assign(&mut body, output, value, at))?;
                model.functions(|functions| functions.define(body, at))
            },
        )?;
        let input = model.expressions(|expressions| {
            let values = [1.0, 2.0, 3.0]
                .into_iter()
                .map(|value| expressions.at(at).literal(dae::DaeLiteral::Real(value)))
                .collect::<Result<Vec<_>, _>>()?;
            expressions.at(at).array(values)
        })?;
        model.expressions(|expressions| expressions.at(at).call(function, 0, [input]))?;
        Ok(())
    })
    .unwrap();

    let table = lower_root_call(&model);
    let [owner] = table.owners() else {
        panic!("one exact call owner expected")
    };
    assert_eq!(owner.outputs()[0].value_type().dimensions(), [3]);
}

/// Two sibling loops in one enclosing loop, where the second reads a value the
/// first completed.
///
/// The enclosing fold carries `beta` before `alpha`, so its first tuple member
/// is the one whose value depends on the other sibling. Lowering that member
/// demands the second sibling loop first, and the read of `alpha` inside it
/// names the definition the first sibling issued - not the enclosing loop's
/// entry value. Resolving the named definition is what makes the demand order
/// topological, so the numbers below are the only proof that matters here.
#[test]
fn sibling_nested_folds_read_the_completed_value_of_the_earlier_sibling() {
    let model = fixtures::sibling_folds();
    let table = lower_root_call(&model);
    let [owner] = table.owners() else {
        panic!("one exact sibling-fold owner expected")
    };
    assert_eq!(
        owner
            .body()
            .operations()
            .iter()
            .filter(|operation| matches!(operation.operation(), solve::SolveOperation::Fold { .. }))
            .count(),
        1
    );
    let output = rumoca_eval_solve::eval_pure_call(&table, owner.id(), &[]).unwrap();
    // Two enclosing iterations: alpha reaches 3 then 6, and beta accumulates
    // the completed alpha twice per iteration: 2 * 3 + 2 * 6 = 18. Reading the
    // enclosing loop's entry value instead would yield 6.
    assert_eq!(output[0].elements(), [solve::SolveValueKind::Integer(6)]);
    assert_eq!(output[1].elements(), [solve::SolveValueKind::Integer(18)]);
}

/// A Real target assigned an Integer value, read by a later sibling loop
/// before the assignment that writes it has been lowered.
///
/// `alpha` is declared Real and assigned the Integer `count`, which DAE
/// assignment compatibility admits. The enclosing fold carries `beta` first,
/// so the sibling that reads `alpha` is lowered before `alpha`'s own
/// statement and the read is answered by the capture path instead. Every
/// consumer reads that definition at the Real type its target declares, so
/// the value stored for it must already be Real: the numbers below are what
/// proves the conversion happened, not the register count.
#[test]
fn reverse_demand_capture_coerces_an_integer_definition_to_its_real_target() {
    let model = integer_to_real_sibling_folds();
    let table = lower_root_call(&model);
    let [owner] = table.owners() else {
        panic!("one exact sibling-fold owner expected")
    };
    let output = rumoca_eval_solve::eval_pure_call(&table, owner.id(), &[]).unwrap();
    // `count` completes at 3 then 6, `alpha` publishes each as a Real, and
    // `beta` adds the completed `alpha` twice per enclosing iteration:
    // 2 * 3 + 2 * 6 = 18.
    assert_eq!(
        output[0].elements(),
        [solve::SolveValueKind::Real64(18.0_f64.to_bits())]
    );
    assert_eq!(
        output[1].elements(),
        [solve::SolveValueKind::Real64(6.0_f64.to_bits())]
    );
}

/// An ordinary in-order statement assigning an Integer value to a Real target.
///
/// This is the third arm of the definition-value rule: no capture is demanded
/// in reverse and no fold carries the value, so the only producer is the
/// `Assignment` statement arm. The call's result store compares the stored
/// value's type against the declared result type, so dropping the coercion
/// there turns this red.
#[test]
fn ordinary_statement_coerces_an_integer_right_hand_side_to_its_real_target() {
    let mut sources = SourceMap::new();
    let source = sources.add("typed_statement_coercion.mo", "y := 2");
    let at = dae::DaeProvenance::source(Span::from_offsets(source, 0, 6)).unwrap();
    let model = dae::Dae::construct(sources, |model| {
        let real = model
            .types(|types| types.derived(dae::ValueType::scalar(dae::ScalarType::Real), at))?;
        let (function, ()) = model.function(
            dae::FunctionSignature::new(VarName::new("promote"), [], [real], at),
            |model, reservation| {
                let y = model.functions(|functions| {
                    functions.output(&reservation, VarName::new("y"), 0, at)
                })?;
                let two = model.expressions(|expressions| {
                    expressions.at(at).literal(dae::DaeLiteral::Integer(2))
                })?;
                let mut body = model.functions(|functions| functions.begin(reservation, at))?;
                model.functions(|functions| functions.assign(&mut body, y, two, at))?;
                model.functions(|functions| functions.define(body, at))
            },
        )?;
        model.expressions(|expressions| expressions.at(at).call(function, 0, []))?;
        Ok(())
    })
    .unwrap();
    let table = lower_root_call(&model);
    let [owner] = table.owners() else {
        panic!("one exact statement-coercion owner expected")
    };
    let output = rumoca_eval_solve::eval_pure_call(&table, owner.id(), &[]).unwrap();
    assert_eq!(
        output[0].elements(),
        [solve::SolveValueKind::Real64(2.0_f64.to_bits())]
    );
}

/// A fold whose entry value is an Integer right-hand side under a Real target.
///
/// `alpha := 0` is a legal Real definition, so the loop carries a Real. Typing
/// the carried tuple from the entry right-hand side's own inferred type instead
/// makes the carried slot Integer and rejects the Real update.
#[test]
fn fold_carries_each_target_at_the_type_its_declaration_issues() {
    let mut sources = SourceMap::new();
    let source = sources.add(
        "typed_fold_declared_carry.mo",
        "alpha := 0; for i in 1:3 loop alpha := alpha + 1.5; end for",
    );
    let at = dae::DaeProvenance::source(Span::from_offsets(source, 0, 10)).unwrap();
    let model = dae::Dae::construct(sources, |model| {
        let real = model
            .types(|types| types.derived(dae::ValueType::scalar(dae::ScalarType::Real), at))?;
        let (function, ()) = model.function(
            dae::FunctionSignature::new(VarName::new("accumulate"), [], [real], at),
            |model, reservation| {
                let alpha = model.functions(|functions| {
                    functions.output(&reservation, VarName::new("alpha"), 0, at)
                })?;
                let (zero, increment) = model.expressions(|expressions| {
                    Ok((
                        expressions.at(at).literal(dae::DaeLiteral::Integer(0))?,
                        expressions.at(at).literal(dae::DaeLiteral::Real(1.5))?,
                    ))
                })?;
                let mut body = model.functions(|functions| functions.begin(reservation, at))?;
                model.functions(|functions| functions.assign(&mut body, alpha, zero, at))?;
                let domain =
                    model.domains(|domains| domains.structured(structured_range("i", 3), at))?;
                let mut loop_body =
                    model.functions(|functions| functions.begin_loop(body, domain, [alpha], at))?;
                let carried =
                    model.functions(|functions| functions.read(loop_body.body(), alpha, at))?;
                let updated = model.expressions(|expressions| {
                    expressions
                        .at(at)
                        .binary(dae::BinaryOperator::Add, carried, increment)
                })?;
                model.functions(|functions| {
                    functions.assign_loop(&mut loop_body, alpha, updated, at)
                })?;
                let body = model.functions(|functions| functions.finish_loop(loop_body, at))?;
                model.functions(|functions| functions.define(body, at))
            },
        )?;
        model.expressions(|expressions| expressions.at(at).call(function, 0, []))?;
        Ok(())
    })
    .unwrap();
    let table = lower_root_call(&model);
    let [owner] = table.owners() else {
        panic!("one exact declared-carry fold owner expected")
    };
    let output = rumoca_eval_solve::eval_pure_call(&table, owner.id(), &[]).unwrap();
    assert_eq!(
        output[0].elements(),
        [solve::SolveValueKind::Real64(4.5_f64.to_bits())]
    );
}

/// A correlated conditional group whose branches are Integer under Real
/// targets.
///
/// Both the taken branch and the fallback are region outputs, and both must be
/// published at the type each target declares: the region's output slots are
/// typed from those declarations.
#[test]
fn correlated_branches_coerce_integer_values_to_their_real_targets() {
    let mut sources = SourceMap::new();
    let source = sources.add(
        "typed_conditional_coercion.mo",
        "if c then x := 1; y := 2; else x := 3; y := 4; end if",
    );
    let at = dae::DaeProvenance::source(Span::from_offsets(source, 0, 12)).unwrap();
    let model = dae::Dae::construct(sources, |model| {
        let (boolean, real) = model.types(|types| {
            Ok((
                types.derived(dae::ValueType::scalar(dae::ScalarType::Boolean), at)?,
                types.derived(dae::ValueType::scalar(dae::ScalarType::Real), at)?,
            ))
        })?;
        let (function, ()) = model.function(
            dae::FunctionSignature::new(VarName::new("select"), [boolean], [real, real], at),
            |model, reservation| {
                let condition = model.functions(|functions| {
                    functions.parameter(&reservation, VarName::new("c"), 0, at)
                })?;
                let (x, y) = model.functions(|functions| {
                    Ok((
                        functions.output(&reservation, VarName::new("x"), 0, at)?,
                        functions.output(&reservation, VarName::new("y"), 1, at)?,
                    ))
                })?;
                let condition = model
                    .expressions(|expressions| expressions.at(at).function_parameter(condition))?;
                let values = model.expressions(|expressions| {
                    [1, 2, 3, 4]
                        .into_iter()
                        .map(|value| expressions.at(at).literal(dae::DaeLiteral::Integer(value)))
                        .collect::<Result<Vec<_>, _>>()
                })?;
                let mut body = model.functions(|functions| functions.begin(reservation, at))?;
                model.functions(|functions| {
                    functions.assign_conditional_all(
                        &mut body,
                        &[x, y],
                        &[condition],
                        &[vec![values[0], values[1]]],
                        &[values[2], values[3]],
                        at,
                    )
                })?;
                model.functions(|functions| functions.define(body, at))
            },
        )?;
        let taken = model.expressions(|expressions| {
            expressions.at(at).literal(dae::DaeLiteral::Boolean(true))
        })?;
        model.expressions(|expressions| expressions.at(at).call(function, 0, [taken]))?;
        Ok(())
    })
    .unwrap();
    let table = lower_root_call(&model);
    let [owner] = table.owners() else {
        panic!("one exact correlated-coercion owner expected")
    };
    let condition = |value| {
        rumoca_eval_solve::TypedValue::construct(
            solve::SolveValueType::scalar(solve::SolveScalarType::Boolean),
            vec![solve::SolveValueKind::Boolean(value)],
        )
        .unwrap()
    };
    let taken = rumoca_eval_solve::eval_pure_call(&table, owner.id(), &[condition(true)]).unwrap();
    assert_eq!(
        taken
            .iter()
            .map(|value| value.elements()[0])
            .collect::<Vec<_>>(),
        [
            solve::SolveValueKind::Real64(1.0_f64.to_bits()),
            solve::SolveValueKind::Real64(2.0_f64.to_bits()),
        ]
    );
    let fallback =
        rumoca_eval_solve::eval_pure_call(&table, owner.id(), &[condition(false)]).unwrap();
    assert_eq!(
        fallback
            .iter()
            .map(|value| value.elements()[0])
            .collect::<Vec<_>>(),
        [
            solve::SolveValueKind::Real64(3.0_f64.to_bits()),
            solve::SolveValueKind::Real64(4.0_f64.to_bits()),
        ]
    );
}

/// Recursively count the compact loops one typed program holds.
fn fold_count(program: &solve::TypedProgram) -> usize {
    program
        .operations()
        .iter()
        .map(|operation| match operation.operation() {
            solve::SolveOperation::Fold { transition, .. } => 1 + fold_count(transition.body()),
            solve::SolveOperation::Map { body, .. } => fold_count(body.body()),
            solve::SolveOperation::Conditional {
                if_true, if_false, ..
            } => fold_count(if_true.body()) + fold_count(if_false.body()),
            _ => 0,
        })
        .sum()
}

fn conditional_count(program: &solve::TypedProgram) -> usize {
    program
        .operations()
        .iter()
        .map(|operation| match operation.operation() {
            solve::SolveOperation::Fold { transition, .. } => conditional_count(transition.body()),
            solve::SolveOperation::Map { body, .. } => conditional_count(body.body()),
            solve::SolveOperation::Conditional {
                if_true, if_false, ..
            } => 1 + conditional_count(if_true.body()) + conditional_count(if_false.body()),
            _ => 0,
        })
        .sum()
}

/// A loop body reads an enclosing scope's completed value as a capture.
///
/// The DAE issues `alpha := count` in the enclosing loop's body, between the
/// two sibling loops, and the second sibling reads it. Which side of the loop
/// boundary that definition falls on is a fact the DAE issued when it opened
/// the region, and consulting it is what keeps the body compact: the region
/// computes only the definitions it owns and captures the rest.
///
/// Placing the boundary the other way is what this pins. `alpha`'s right-hand
/// side reads `count`, whose reaching definition is the first sibling loop's
/// result, so a body that treated `alpha` as its own would rebuild that whole
/// loop inside the second sibling's transition - once per `k` - and the
/// program would hold four compact loops instead of three.
#[test]
fn a_fold_body_captures_an_enclosing_scope_definition_instead_of_rebuilding_it() {
    let model = integer_to_real_sibling_folds();
    let table = lower_root_call(&model);
    let [owner] = table.owners() else {
        panic!("one exact sibling-fold owner expected")
    };
    assert_eq!(fold_count(owner.body()), 3);
    let output = rumoca_eval_solve::eval_pure_call(&table, owner.id(), &[]).unwrap();
    // Two enclosing iterations: count reaches 3 then 6, alpha publishes each,
    // and beta accumulates the completed alpha twice per iteration.
    assert_eq!(
        output[0].elements(),
        [solve::SolveValueKind::Real64(18.0_f64.to_bits())]
    );
    assert_eq!(
        output[1].elements(),
        [solve::SolveValueKind::Real64(6.0_f64.to_bits())]
    );
}
