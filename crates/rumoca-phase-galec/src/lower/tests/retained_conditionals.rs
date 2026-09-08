use super::*;

/// One scalar call owner reached on every branch of an exhaustive conditional.
fn scalar_conditional_branches_fixture() -> dae::Dae {
    let mut sources = SourceMap::new();
    let text = "function f input Real u; output Real y; algorithm y := u; end f; if true then f(1.0) else f(1.0)";
    let source = sources.add("conditional-scalar-call.mo", text);
    let span = Span::from_offsets(source, 0, text.len());
    let provenance = dae::DaeProvenance::source(span).unwrap();
    dae::Dae::construct(sources, |dae| {
        let real = dae.types(|types| {
            types.derived(dae::ValueType::scalar(dae::ScalarType::Real), provenance)
        })?;
        let (function, ()) = dae.function(
            dae::FunctionSignature::new(VarName::new("f"), [real], [real], provenance),
            |dae, reservation| {
                let input = dae.functions(|functions| {
                    functions.parameter(&reservation, VarName::new("u"), 0, provenance)
                })?;
                let output = dae.functions(|functions| {
                    functions.output(&reservation, VarName::new("y"), 0, provenance)
                })?;
                let mut body =
                    dae.functions(|functions| functions.begin(reservation, provenance))?;
                let value = dae.expressions(|expressions| {
                    expressions.at(provenance).function_parameter(input)
                })?;
                dae.functions(|functions| functions.assign(&mut body, output, value, provenance))?;
                dae.functions(|functions| functions.define(body, provenance))
            },
        )?;
        let (condition, argument) = dae.expressions(|expressions| {
            Ok((
                expressions
                    .at(provenance)
                    .literal(dae::DaeLiteral::Boolean(true))?,
                expressions
                    .at(provenance)
                    .literal(dae::DaeLiteral::Real(1.0))?,
            ))
        })?;
        let call = dae
            .expressions(|expressions| expressions.at(provenance).call(function, 0, [argument]))?;
        dae.expressions(|expressions| expressions.at(provenance).call(function, 0, [argument]))?;
        dae.expressions(|expressions| {
            expressions
                .at(provenance)
                .conditional([(condition, call)], call)
        })?;
        Ok(())
    })
    .unwrap()
}

#[test]
fn exhaustive_scalar_branches_join_one_materialized_result_storage() {
    let model = scalar_conditional_branches_fixture();

    model.inspect(|view| {
        let calls = (0..view.expression_count())
            .filter_map(|index| view.expression_id(index))
            .filter(|id| {
                matches!(
                    view.expression(*id).unwrap().operation(),
                    dae::ExpressionOperation::Call { owner, .. } if owner == *id
                )
            })
            .collect::<Vec<_>>();
        assert_eq!(calls.len(), 2);
        let variables = HashMap::new();
        let previous = HashMap::new();
        let definitions = rumoca_phase_structural::CausalDefinitions::derive(view);
        let mut distinct = ExpressionLowerer::with_do_step_effects(
            view,
            &definitions,
            &variables,
            &previous,
            positive_zero_arithmetic(),
        );
        for call in calls {
            distinct.lower(call).unwrap();
        }
        assert_eq!(
            count_named_multi_calls(&distinct.take_prefix_statements(), "f"),
            2
        );

        let conditional = (0..view.expression_count())
            .filter_map(|index| view.expression_id(index))
            .find(|id| {
                matches!(
                    view.expression(*id).unwrap().operation(),
                    dae::ExpressionOperation::Conditional(_)
                )
            })
            .unwrap();
        let mut lowerer = ExpressionLowerer::with_do_step_effects(
            view,
            &definitions,
            &variables,
            &previous,
            positive_zero_arithmetic(),
        );
        let mut retained_calls = RetainedCallResults::default();
        let prepared = lowerer
            .prepare_emission_group(
                EmissionRegion::CausalAssignment(0),
                &HashSet::new(),
                CrossGroupCallRetention::Unguarded(&mut retained_calls),
                |lowerer, _| lowerer.lower(conditional).map(|_| ()),
            )
            .unwrap();
        let mut prefix = Vec::new();
        let mut actions = Vec::new();
        prepared.commit_into(&mut prefix, &mut actions);
        assert_eq!(
            CommittedCallActionLedger::occurrence_count(&actions),
            2,
            "each exhaustive branch owns one guarded realization"
        );
        CommittedCallActionLedger::construct(view, actions)
            .expect("construction-issued branch activations prove the realizations disjoint");
        let gast::Statement::If(selection) = &prefix[0].node else {
            panic!("the conditional must dominate both function calls")
        };
        let gast::Statement::MultiAssignment {
            targets: branch_targets,
            ..
        } = &selection.branches[0].body[0].node
        else {
            panic!("the selected branch must materialize its call")
        };
        let gast::Statement::MultiAssignment {
            targets: fallback_targets,
            ..
        } = &selection.else_body.as_ref().unwrap()[0].node
        else {
            panic!("the fallback branch must materialize its call")
        };
        assert_eq!(
            branch_targets, fallback_targets,
            "the exhaustive join gives later consumers one retained-result storage"
        );
    });
}

fn conditional_condition_false_path_reuse_fixture() -> dae::Dae {
    let mut sources = SourceMap::new();
    let text = "function id input Boolean u; output Boolean y; algorithm y := u; end id; if id(false) then true else id(false)";
    let source = sources.add("conditional-condition-call.mo", text);
    let at = dae::DaeProvenance::source(Span::from_offsets(source, 0, text.len())).unwrap();
    dae::Dae::construct(sources, |dae| {
        let boolean =
            dae.types(|types| types.derived(dae::ValueType::scalar(dae::ScalarType::Boolean), at))?;
        let (identity, ()) = dae.function(
            dae::FunctionSignature::new(VarName::new("id"), [boolean], [boolean], at),
            |dae, reservation| {
                let (input, output) = dae.functions(|functions| {
                    Ok((
                        functions.parameter(&reservation, VarName::new("u"), 0, at)?,
                        functions.output(&reservation, VarName::new("y"), 0, at)?,
                    ))
                })?;
                let mut body = dae.functions(|functions| functions.begin(reservation, at))?;
                let value =
                    dae.expressions(|expressions| expressions.at(at).function_parameter(input))?;
                dae.functions(|functions| {
                    functions.assign(&mut body, output, value, at)?;
                    functions.define(body, at)
                })
            },
        )?;
        let (false_value, true_value) = dae.expressions(|expressions| {
            Ok((
                expressions
                    .at(at)
                    .literal(dae::DaeLiteral::Boolean(false))?,
                expressions.at(at).literal(dae::DaeLiteral::Boolean(true))?,
            ))
        })?;
        let call =
            dae.expressions(|expressions| expressions.at(at).call(identity, 0, [false_value]))?;
        dae.expressions(|expressions| {
            expressions.at(at).conditional([(call, true_value)], call)?;
            let skipped_condition = expressions
                .at(at)
                .conditional([(true_value, true_value), (call, true_value)], false_value)?;
            expressions
                .at(at)
                .binary(dae::BinaryOperator::And, skipped_condition, call)?;
            Ok(())
        })
    })
    .unwrap()
}

#[test]
fn conditional_condition_call_dominates_its_false_path_reuse() {
    let model = conditional_condition_false_path_reuse_fixture();
    model.inspect(|view| {
        let conditional = (0..view.expression_count())
            .filter_map(|index| view.expression_id(index))
            .find(|expression| {
                matches!(
                    view.expression(*expression).unwrap().operation(),
                    dae::ExpressionOperation::Conditional(_)
                )
            })
            .unwrap();
        let definitions = rumoca_phase_structural::CausalDefinitions::derive(view);
        let variables = HashMap::new();
        let previous = HashMap::new();
        let mut lowerer = ExpressionLowerer::with_do_step_effects(
            view,
            &definitions,
            &variables,
            &previous,
            positive_zero_arithmetic(),
        );
        let mut retained_calls = RetainedCallResults::default();
        let prepared = lowerer
            .prepare_emission_group(
                EmissionRegion::CausalAssignment(0),
                &HashSet::new(),
                CrossGroupCallRetention::Unguarded(&mut retained_calls),
                |lowerer, _| lowerer.lower(conditional).map(|_| ()),
            )
            .unwrap();
        let mut statements = Vec::new();
        let mut actions = Vec::new();
        prepared.commit_into(&mut statements, &mut actions);
        assert_eq!(count_named_multi_calls(&statements, "id"), 1);
        assert_eq!(CommittedCallActionLedger::occurrence_count(&actions), 1);
        CommittedCallActionLedger::construct(view, actions).unwrap();

        let escaped = (0..view.expression_count())
            .filter_map(|index| view.expression_id(index))
            .find(|expression| {
                matches!(
                    view.expression(*expression).unwrap().operation(),
                    dae::ExpressionOperation::Binary {
                        operator: dae::BinaryOperator::And,
                        ..
                    }
                )
            })
            .unwrap();
        let mut lowerer = ExpressionLowerer::with_do_step_effects(
            view,
            &definitions,
            &variables,
            &previous,
            positive_zero_arithmetic(),
        );
        let mut retained_calls = RetainedCallResults::default();
        let prepared = lowerer
            .prepare_emission_group(
                EmissionRegion::CausalAssignment(0),
                &HashSet::new(),
                CrossGroupCallRetention::Unguarded(&mut retained_calls),
                |lowerer, _| lowerer.lower(escaped).map(|_| ()),
            )
            .unwrap();
        let mut statements = Vec::new();
        let mut actions = Vec::new();
        prepared.commit_into(&mut statements, &mut actions);
        assert_eq!(
            count_named_multi_calls(&statements, "id"),
            2,
            "a skipped later condition cannot publish an uninitialized memo"
        );
        assert!(matches!(
            CommittedCallActionLedger::construct(view, actions),
            Err(GalecTargetError::UnsupportedFeature { feature, .. })
                if feature == "repeated-call-owner"
        ));
    });
}

#[test]
fn distinct_dynamic_array_selections_cannot_alias_one_call_activation() {
    let mut sources = SourceMap::new();
    let text = "function id input Real u; output Real y; algorithm y := u; end id; {id(1.0), 0.0}";
    let source = sources.add("dynamic-selection-owner.mo", text);
    let at = dae::DaeProvenance::source(Span::from_offsets(source, 0, text.len())).unwrap();
    let model = dae::Dae::construct(sources, |dae| {
        let real =
            dae.types(|types| types.derived(dae::ValueType::scalar(dae::ScalarType::Real), at))?;
        let identity = define_scalar_passthrough_function(dae, "id", real, at)?;
        let (one, zero) = dae.expressions(|expressions| {
            Ok((
                expressions.at(at).literal(dae::DaeLiteral::Real(1.0))?,
                expressions.at(at).literal(dae::DaeLiteral::Real(0.0))?,
            ))
        })?;
        let call = dae.expressions(|expressions| expressions.at(at).call(identity, 0, [one]))?;
        dae.expressions(|expressions| expressions.at(at).array([call, zero]))?;
        Ok(())
    })
    .unwrap();
    model.inspect(|view| {
        let array = (0..view.expression_count())
            .filter_map(|index| view.expression_id(index))
            .find(|expression| {
                matches!(
                    view.expression(*expression).unwrap().operation(),
                    dae::ExpressionOperation::Array(_)
                )
            })
            .unwrap();
        let definitions = rumoca_phase_structural::CausalDefinitions::derive(view);
        let variables = HashMap::new();
        let previous = HashMap::new();
        let mut lowerer = ExpressionLowerer::with_do_step_effects(
            view,
            &definitions,
            &variables,
            &previous,
            positive_zero_arithmetic(),
        );
        lowerer.loop_index_bounds.extend([
            LoopIndexBound {
                name: gast::Name::ident("i"),
                minimum: 1,
                maximum: 2,
            },
            LoopIndexBound {
                name: gast::Name::ident("j"),
                minimum: 1,
                maximum: 2,
            },
        ]);
        let mut retained_calls = RetainedCallResults::default();
        let prepared = lowerer
            .prepare_emission_group(
                EmissionRegion::CausalAssignment(0),
                &HashSet::new(),
                CrossGroupCallRetention::Unguarded(&mut retained_calls),
                |lowerer, _| {
                    // At runtime `i = 2` skips the call while `j = 1` must
                    // execute it. The projections therefore cannot share an
                    // ArraySelection branch-0 fact merely because their
                    // candidate element ids are equal.
                    lowerer.lower_at(
                        array,
                        &[gast::Expression::Ref(gast::Reference::local(
                            gast::Name::ident("i"),
                        ))],
                    )?;
                    lowerer.lower_at(
                        array,
                        &[gast::Expression::Ref(gast::Reference::local(
                            gast::Name::ident("j"),
                        ))],
                    )?;
                    Ok(())
                },
            )
            .unwrap();
        let mut statements = Vec::new();
        let mut actions = Vec::new();
        prepared.commit_into(&mut statements, &mut actions);
        assert_eq!(
            count_named_multi_calls(&statements, "id"),
            2,
            "a second selector must not memo-hit the first selector's branch local"
        );
        assert!(matches!(
            CommittedCallActionLedger::construct(view, actions),
            Err(GalecTargetError::UnsupportedFeature { feature, .. })
                if feature == "repeated-call-owner"
        ));
    });
}

#[test]
fn atomic_multi_output_call_dominates_consumers_with_additional_guards() {
    let mut sources = SourceMap::new();
    let text = "function pair input Real u; output Real a; output Real b; algorithm a := u; b := u; end pair; function caller input Boolean sourceGuard; input Boolean consumerGuard; output Real x; output Real y; algorithm (x, y) := pair(1.0); end caller";
    let source = sources.add("atomic-multi-output.mo", text);
    let span = Span::from_offsets(source, 0, text.len());
    let provenance = dae::DaeProvenance::source(span).unwrap();
    let model = dae::Dae::construct(sources, |dae| {
        let real = dae.types(|types| {
            types.derived(dae::ValueType::scalar(dae::ScalarType::Real), provenance)
        })?;
        let boolean = dae.types(|types| {
            types.derived(dae::ValueType::scalar(dae::ScalarType::Boolean), provenance)
        })?;
        let pair = define_real_pair_function(dae, real, provenance)?;
        dae.function(
            dae::FunctionSignature::new(
                VarName::new("caller"),
                [boolean, boolean],
                [real, real],
                provenance,
            ),
            |dae, reservation| {
                let source_guard = dae.functions(|functions| {
                    functions.parameter(&reservation, VarName::new("sourceGuard"), 0, provenance)
                })?;
                let consumer_guard = dae.functions(|functions| {
                    functions.parameter(&reservation, VarName::new("consumerGuard"), 1, provenance)
                })?;
                let first = dae.functions(|functions| {
                    functions.output(&reservation, VarName::new("x"), 0, provenance)
                })?;
                let second = dae.functions(|functions| {
                    functions.output(&reservation, VarName::new("y"), 1, provenance)
                })?;
                let mut body =
                    dae.functions(|functions| functions.begin(reservation, provenance))?;
                let (source_guard, consumer_guard, argument, zero) =
                    dae.expressions(|expressions| {
                        Ok((
                            expressions
                                .at(provenance)
                                .function_parameter(source_guard)?,
                            expressions
                                .at(provenance)
                                .function_parameter(consumer_guard)?,
                            expressions
                                .at(provenance)
                                .literal(dae::DaeLiteral::Real(1.0))?,
                            expressions
                                .at(provenance)
                                .literal(dae::DaeLiteral::Real(0.0))?,
                        ))
                    })?;
                let calls = dae.expressions(|expressions| {
                    expressions
                        .at(provenance)
                        .call_results(pair, [0, 1], [argument])
                })?;
                let first_value = dae.expressions(|expressions| {
                    expressions
                        .at(provenance)
                        .conditional([(source_guard, calls[0])], zero)
                })?;
                let guarded_second = dae.expressions(|expressions| {
                    expressions
                        .at(provenance)
                        .conditional([(source_guard, calls[1])], zero)
                })?;
                let consumed_second = dae.expressions(|expressions| {
                    expressions.at(provenance).binary(
                        dae::BinaryOperator::Add,
                        guarded_second,
                        zero,
                    )
                })?;
                let second_value = dae.expressions(|expressions| {
                    expressions
                        .at(provenance)
                        .conditional([(consumer_guard, consumed_second)], zero)
                })?;
                dae.functions(|functions| {
                    functions.assign_all(
                        &mut body,
                        &[(first, first_value), (second, second_value)],
                        provenance,
                    )
                })?;
                dae.functions(|functions| functions.define(body, provenance))
            },
        )?;
        Ok(())
    })
    .unwrap();

    assert_atomic_multi_output_group(&model);
}

fn define_real_pair_function<'dae>(
    dae: &mut dae::DaeConstruction<'dae>,
    real: dae::ValueTypeId<'dae>,
    provenance: dae::DaeProvenance,
) -> Result<dae::FunctionId<'dae>, dae::DaeConstructionError> {
    let (function, ()) = dae.function(
        dae::FunctionSignature::new(VarName::new("pair"), [real], [real, real], provenance),
        |dae, reservation| {
            let input = dae.functions(|functions| {
                functions.parameter(&reservation, VarName::new("u"), 0, provenance)
            })?;
            let first = dae.functions(|functions| {
                functions.output(&reservation, VarName::new("a"), 0, provenance)
            })?;
            let second = dae.functions(|functions| {
                functions.output(&reservation, VarName::new("b"), 1, provenance)
            })?;
            let mut body = dae.functions(|functions| functions.begin(reservation, provenance))?;
            let value = dae
                .expressions(|expressions| expressions.at(provenance).function_parameter(input))?;
            dae.functions(|functions| functions.assign(&mut body, first, value, provenance))?;
            dae.functions(|functions| functions.assign(&mut body, second, value, provenance))?;
            dae.functions(|functions| functions.define(body, provenance))
        },
    )?;
    Ok(function)
}

fn checked_record_call_projection_fixture() -> dae::Dae {
    let mut sources = SourceMap::new();
    let text = "record Pair Real left; Real right; end Pair; function makePair input Real u; output Pair p; algorithm p := Pair(u, u); end makePair; makePair(2.0).right";
    let source = sources.add("record-call.mo", text);
    let span = Span::from_offsets(source, 0, text.len());
    let provenance = dae::DaeProvenance::source(span).unwrap();
    dae::Dae::construct(sources, |dae| {
        let real = dae.types(|types| {
            types.derived(dae::ValueType::scalar(dae::ScalarType::Real), provenance)
        })?;
        let pair = dae.types(|types| {
            types.record(
                VarName::new("Pair"),
                [(VarName::new("left"), real), (VarName::new("right"), real)],
                provenance,
            )
        })?;
        let signature =
            dae::FunctionSignature::new(VarName::new("makePair"), [real], [pair], provenance);
        let (function, ()) = dae.function(signature, |dae, reservation| {
            let parameter = dae.functions(|functions| {
                functions.parameter(&reservation, VarName::new("u"), 0, provenance)
            })?;
            let output = dae.functions(|functions| {
                functions.output(&reservation, VarName::new("p"), 0, provenance)
            })?;
            dae.functions(|functions| {
                functions.local(&reservation, VarName::new("p.left"), real, provenance)
            })?;
            let mut body = dae.functions(|functions| functions.begin(reservation, provenance))?;
            let fields = dae.expressions(|expressions| {
                Ok([
                    expressions.at(provenance).function_parameter(parameter)?,
                    expressions.at(provenance).function_parameter(parameter)?,
                ])
            })?;
            let value =
                dae.expressions(|expressions| expressions.at(provenance).record(pair, fields))?;
            dae.functions(|functions| functions.assign(&mut body, output, value, provenance))?;
            dae.functions(|functions| functions.define(body, provenance))
        })?;
        let argument = dae.expressions(|expressions| {
            expressions
                .at(provenance)
                .literal(dae::DaeLiteral::Real(2.0))
        })?;
        let call = dae
            .expressions(|expressions| expressions.at(provenance).call(function, 0, [argument]))?;
        dae.expressions(|expressions| expressions.at(provenance).field(call, 1))?;
        Ok(())
    })
    .unwrap()
}

#[test]
fn record_field_of_checked_function_call_is_projected_before_scalar_lowering() {
    let model = checked_record_call_projection_fixture();
    model.inspect(|view| {
        let function = view.function_id(0).unwrap();
        let definitions = rumoca_phase_structural::CausalDefinitions::derive(view);
        let lowered_functions = user_functions::lower_reachable(
            view,
            &definitions,
            HashSet::from([function.index()]),
            positive_zero_arithmetic(),
        )
        .unwrap();
        assert_eq!(lowered_functions.len(), 1);
        let field = (0..view.expression_count())
            .filter_map(|index| view.expression_id(index))
            .find(|id| {
                matches!(
                    view.expression(*id).unwrap().operation(),
                    dae::ExpressionOperation::Field { field: 1, .. }
                )
            })
            .unwrap();
        let variables = HashMap::new();
        let previous = HashMap::new();
        let mut entered = ExpressionLowerer::new(
            view,
            &definitions,
            &variables,
            &previous,
            positive_zero_arithmetic(),
        );
        let lowered = entered.lower(field).unwrap();
        assert_eq!(lowered.scalar_type, gast::ScalarType::Real);
        assert_eq!(lowered.expression, gast::Expression::Real(2.0));
        let bindings = entered.take_prefix_statements();
        assert!(
            bindings.is_empty(),
            "an immutable scalar literal needs no runtime binding"
        );

        let mut materialized = ExpressionLowerer::with_do_step_effects(
            view,
            &definitions,
            &variables,
            &previous,
            positive_zero_arithmetic(),
        );
        let selected = materialized.lower(field).unwrap().expression;
        let prefix = materialized.take_prefix_statements();
        assert_eq!(prefix.len(), 1);
        let gast::Statement::MultiAssignment { targets, .. } = &prefix[0].node else {
            panic!("one record call must become one multi-output assignment")
        };
        assert_eq!(targets.len(), 2);
        assert!(matches!(
            selected,
            gast::Expression::Ref(gast::Reference::Local(ref selected))
                if matches!(&targets[1], gast::Reference::Local(target)
                    if target.name.lexeme() == selected.name.lexeme())
        ));
    });
}

#[test]
fn entered_record_call_projection_commits_its_model_owner() {
    let mut sources = SourceMap::new();
    let text = "record Pair Real left; Real right; end Pair; function makePair output Pair p; algorithm p := Pair(1.0, 2.0); end makePair; makePair().right";
    let source = sources.add("entered-record-call-owner.mo", text);
    let span = Span::from_offsets(source, 0, text.len());
    let provenance = dae::DaeProvenance::source(span).unwrap();
    let model = dae::Dae::construct(sources, |dae| {
        let real = dae.types(|types| {
            types.derived(dae::ValueType::scalar(dae::ScalarType::Real), provenance)
        })?;
        let pair = dae.types(|types| {
            types.record(
                VarName::new("Pair"),
                [(VarName::new("left"), real), (VarName::new("right"), real)],
                provenance,
            )
        })?;
        let nested = dae.types(|types| {
            types.record(
                VarName::new("Nested"),
                [(VarName::new("pair"), pair)],
                provenance,
            )
        })?;
        let (function, ()) = dae.function(
            dae::FunctionSignature::new(VarName::new("makePair"), [], [pair], provenance),
            |dae, reservation| {
                let output = dae.functions(|functions| {
                    functions.output(&reservation, VarName::new("p"), 0, provenance)
                })?;
                dae.functions(|functions| {
                    functions.local(&reservation, VarName::new("unused"), nested, provenance)
                })?;
                let mut body =
                    dae.functions(|functions| functions.begin(reservation, provenance))?;
                let fields = dae.expressions(|expressions| {
                    Ok([
                        expressions
                            .at(provenance)
                            .literal(dae::DaeLiteral::Real(1.0))?,
                        expressions
                            .at(provenance)
                            .literal(dae::DaeLiteral::Real(2.0))?,
                    ])
                })?;
                let value =
                    dae.expressions(|expressions| expressions.at(provenance).record(pair, fields))?;
                dae.functions(|functions| {
                    functions.assign(&mut body, output, value, provenance)?;
                    functions.define(body, provenance)
                })
            },
        )?;
        let call =
            dae.expressions(|expressions| expressions.at(provenance).call(function, 0, []))?;
        dae.expressions(|expressions| {
            expressions.at(provenance).field(call, 1)?;
            Ok(())
        })
    })
    .unwrap();

    model.inspect(|view| {
        let function = view.function_id(0).unwrap();
        assert!(
            !user_functions::is_directly_lowerable(view, function),
            "nested local forces the entered-body record projection path"
        );
        let field = (0..view.expression_count())
            .filter_map(|index| view.expression_id(index))
            .find(|id| {
                matches!(
                    view.expression(*id).unwrap().operation(),
                    dae::ExpressionOperation::Field { .. }
                )
            })
            .unwrap();
        let variables = HashMap::new();
        let previous = HashMap::new();
        let definitions = rumoca_phase_structural::CausalDefinitions::derive(view);
        let mut lowerer = ExpressionLowerer::with_do_step_effects(
            view,
            &definitions,
            &variables,
            &previous,
            positive_zero_arithmetic(),
        );
        let mut retained_calls = RetainedCallResults::default();
        let prepared = lowerer
            .prepare_emission_group(
                EmissionRegion::CausalAssignment(0),
                &HashSet::new(),
                CrossGroupCallRetention::Unguarded(&mut retained_calls),
                |lowerer, _| lowerer.lower(field).map(|_| ()),
            )
            .unwrap();
        let mut statements = Vec::new();
        let mut actions = Vec::new();
        prepared.commit_into(&mut statements, &mut actions);
        assert_eq!(CommittedCallActionLedger::occurrence_count(&actions), 1);
        CommittedCallActionLedger::construct(view, actions).unwrap();
    });
}

fn direct_consumer_record_actual_source() -> (SourceMap, dae::DaeProvenance) {
    let mut sources = SourceMap::new();
    let text = "record Pair Real a; Real b; end Pair; function make input Real u; output Pair r; protected Nested scratch; algorithm assert(u > 0.0, \"record actual evaluated\"); r := Pair(u, u + 1.0); end make; function consume input Pair r; output Real y; algorithm y := r.a + r.b; end consume; consume(make(1.0));";
    let source = sources.add("direct-record-actual.mo", text);
    let at = dae::DaeProvenance::source(Span::from_offsets(source, 0, text.len())).unwrap();
    (sources, at)
}

fn direct_consumer_record_actual_model() -> dae::Dae {
    let (sources, at) = direct_consumer_record_actual_source();
    dae::Dae::construct(sources, |dae| {
        let (real, pair, nested) = dae.types(|types| {
            let real = types.derived(dae::ValueType::scalar(dae::ScalarType::Real), at)?;
            let pair = types.record(
                VarName::new("Pair"),
                [(VarName::new("a"), real), (VarName::new("b"), real)],
                at,
            )?;
            let nested = types.record(VarName::new("Nested"), [(VarName::new("p"), pair)], at)?;
            Ok((real, pair, nested))
        })?;
        let (make, ()) = dae.function(
            dae::FunctionSignature::new(VarName::new("make"), [real], [pair], at),
            |dae, reservation| {
                let (input, output) = dae.functions(|functions| {
                    Ok((
                        functions.parameter(&reservation, VarName::new("u"), 0, at)?,
                        functions.output(&reservation, VarName::new("r"), 0, at)?,
                    ))
                })?;
                dae.functions(|functions| {
                    functions.local(&reservation, VarName::new("scratch"), nested, at)
                })?;
                let mut body = dae.functions(|functions| functions.begin(reservation, at))?;
                let input_value =
                    dae.expressions(|expressions| expressions.at(at).function_parameter(input))?;
                let (zero, one, message) = dae.expressions(|expressions| {
                    Ok((
                        expressions.at(at).literal(dae::DaeLiteral::Real(0.0))?,
                        expressions.at(at).literal(dae::DaeLiteral::Real(1.0))?,
                        expressions.at(at).literal(dae::DaeLiteral::String(
                            "record actual evaluated".to_owned(),
                        ))?,
                    ))
                })?;
                let positive = dae.expressions(|expressions| {
                    expressions
                        .at(at)
                        .binary(dae::BinaryOperator::Greater, input_value, zero)
                })?;
                let second = dae.expressions(|expressions| {
                    expressions
                        .at(at)
                        .binary(dae::BinaryOperator::Add, input_value, one)
                })?;
                let value = dae.expressions(|expressions| {
                    expressions.at(at).record(pair, [input_value, second])
                })?;
                dae.functions(|functions| {
                    functions.assertion(&mut body, positive, message, at)?;
                    functions.assign(&mut body, output, value, at)?;
                    functions.define(body, at)
                })
            },
        )?;
        let (consume, ()) = dae.function(
            dae::FunctionSignature::new(VarName::new("consume"), [pair], [real], at),
            |dae, reservation| {
                let (input, output) = dae.functions(|functions| {
                    Ok((
                        functions.parameter(&reservation, VarName::new("r"), 0, at)?,
                        functions.output(&reservation, VarName::new("y"), 0, at)?,
                    ))
                })?;
                let mut body = dae.functions(|functions| functions.begin(reservation, at))?;
                let record =
                    dae.expressions(|expressions| expressions.at(at).function_parameter(input))?;
                let (first, second) = dae.expressions(|expressions| {
                    Ok((
                        expressions.at(at).field(record, 0)?,
                        expressions.at(at).field(record, 1)?,
                    ))
                })?;
                let sum = dae.expressions(|expressions| {
                    expressions
                        .at(at)
                        .binary(dae::BinaryOperator::Add, first, second)
                })?;
                dae.functions(|functions| {
                    functions.assign(&mut body, output, sum, at)?;
                    functions.define(body, at)
                })
            },
        )?;
        let one =
            dae.expressions(|expressions| expressions.at(at).literal(dae::DaeLiteral::Real(1.0)))?;
        let actual = dae.expressions(|expressions| expressions.at(at).call(make, 0, [one]))?;
        dae.expressions(|expressions| {
            expressions.at(at).call(consume, 0, [actual])?;
            Ok(())
        })
    })
    .unwrap()
}

#[test]
fn direct_consumer_prepares_one_entered_record_actual_transaction() {
    let model = direct_consumer_record_actual_model();
    model.inspect(|view| {
        let make = view.function_id(0).unwrap();
        let consume = view.function_id(1).unwrap();
        assert!(!user_functions::is_directly_lowerable(view, make));
        assert!(user_functions::is_directly_lowerable(view, consume));
        let root = (0..view.expression_count())
            .filter_map(|index| view.expression_id(index))
            .find(|expression| {
                matches!(view.expression(*expression).unwrap().operation(),
                    dae::ExpressionOperation::Call { function, .. } if function == consume)
            })
            .unwrap();
        let definitions = rumoca_phase_structural::CausalDefinitions::derive(view);
        let variables = HashMap::new();
        let previous = HashMap::new();
        let mut lowerer = ExpressionLowerer::with_do_step_effects(
            view,
            &definitions,
            &variables,
            &previous,
            positive_zero_arithmetic(),
        );
        let mut retained_calls = RetainedCallResults::default();
        let prepared = lowerer
            .prepare_emission_group(
                EmissionRegion::CausalAssignment(0),
                &HashSet::new(),
                CrossGroupCallRetention::Unguarded(&mut retained_calls),
                |lowerer, _| lowerer.lower(root).map(|_| ()),
            )
            .unwrap();
        let mut statements = Vec::new();
        let mut actions = Vec::new();
        prepared.commit_into(&mut statements, &mut actions);
        assert_eq!(CommittedCallActionLedger::occurrence_count(&actions), 2);
        CommittedCallActionLedger::construct(view, actions).unwrap();
        assert_eq!(count_named_multi_calls(&statements, "consume"), 1);
        assert_eq!(count_named_multi_calls(&statements, "make"), 0);
        let assertions = statements
            .iter()
            .filter(|statement| matches!(statement.node, gast::Statement::If(_)))
            .count();
        assert_eq!(assertions, 1, "the entered record actual asserts once");
    });
}

fn unused_actual_entered_call_fixture() -> dae::Dae {
    let mut sources = SourceMap::new();
    let text = "function leaf input Real u; output Real y; algorithm assert(u > 0.0, \"evaluated\"); y := u; end leaf; function wrapper input Real unused; output Real y; protected Nested local; algorithm y := 1.0; end wrapper; wrapper(leaf(-1.0));";
    let source = sources.add("eager-inline-argument.mo", text);
    let span = Span::from_offsets(source, 0, text.len());
    let provenance = dae::DaeProvenance::source(span).unwrap();
    dae::Dae::construct(sources, |dae| {
        let (real, nested) = dae.types(|types| {
            let real = types.derived(dae::ValueType::scalar(dae::ScalarType::Real), provenance)?;
            let pair = types.record(
                VarName::new("Pair"),
                [(VarName::new("value"), real)],
                provenance,
            )?;
            let nested = types.record(
                VarName::new("Nested"),
                [(VarName::new("pair"), pair)],
                provenance,
            )?;
            Ok((real, nested))
        })?;
        let (leaf, ()) = dae.function(
            dae::FunctionSignature::new(VarName::new("leaf"), [real], [real], provenance),
            |dae, reservation| {
                let (input, output) = dae.functions(|functions| {
                    Ok((
                        functions.parameter(&reservation, VarName::new("u"), 0, provenance)?,
                        functions.output(&reservation, VarName::new("y"), 0, provenance)?,
                    ))
                })?;
                let mut body =
                    dae.functions(|functions| functions.begin(reservation, provenance))?;
                let input_value = dae.expressions(|expressions| {
                    expressions.at(provenance).function_parameter(input)
                })?;
                let (zero, message) = dae.expressions(|expressions| {
                    Ok((
                        expressions
                            .at(provenance)
                            .literal(dae::DaeLiteral::Real(0.0))?,
                        expressions
                            .at(provenance)
                            .literal(dae::DaeLiteral::String("evaluated".to_owned()))?,
                    ))
                })?;
                let condition = dae.expressions(|expressions| {
                    expressions.at(provenance).binary(
                        dae::BinaryOperator::Greater,
                        input_value,
                        zero,
                    )
                })?;
                dae.functions(|functions| {
                    functions.assertion(&mut body, condition, message, provenance)?;
                    functions.assign(&mut body, output, input_value, provenance)?;
                    functions.define(body, provenance)
                })
            },
        )?;
        let (wrapper, ()) = dae.function(
            dae::FunctionSignature::new(VarName::new("wrapper"), [real], [real], provenance),
            |dae, reservation| {
                let output = dae.functions(|functions| {
                    functions.parameter(&reservation, VarName::new("unused"), 0, provenance)?;
                    let output =
                        functions.output(&reservation, VarName::new("y"), 0, provenance)?;
                    functions.local(&reservation, VarName::new("local"), nested, provenance)?;
                    Ok(output)
                })?;
                let mut body =
                    dae.functions(|functions| functions.begin(reservation, provenance))?;
                let one = dae.expressions(|expressions| {
                    expressions
                        .at(provenance)
                        .literal(dae::DaeLiteral::Real(1.0))
                })?;
                dae.functions(|functions| {
                    functions.assign(&mut body, output, one, provenance)?;
                    functions.define(body, provenance)
                })
            },
        )?;
        let minus_one = dae.expressions(|expressions| {
            expressions
                .at(provenance)
                .literal(dae::DaeLiteral::Real(-1.0))
        })?;
        let leaf_call =
            dae.expressions(|expressions| expressions.at(provenance).call(leaf, 0, [minus_one]))?;
        dae.expressions(|expressions| {
            expressions.at(provenance).call(wrapper, 0, [leaf_call])?;
            Ok(())
        })
    })
    .unwrap()
}

#[test]
fn entered_call_evaluates_an_unused_actual_before_the_callee_body() {
    let model = unused_actual_entered_call_fixture();
    model.inspect(|view| {
        let wrapper = view.function_id(1).unwrap();
        assert!(
            !user_functions::is_directly_lowerable(view, wrapper),
            "the nested record local forces entered-body lowering"
        );
        let call = (0..view.expression_count())
            .filter_map(|index| view.expression_id(index))
            .find(|id| {
                matches!(
                    view.expression(*id).unwrap().operation(),
                    dae::ExpressionOperation::Call { function, .. } if function == wrapper
                )
            })
            .unwrap();
        let variables = HashMap::new();
        let previous = HashMap::new();
        let definitions = rumoca_phase_structural::CausalDefinitions::derive(view);
        let mut lowerer =
            ExpressionLowerer::with_do_step_effects(view, &definitions, &variables, &previous, positive_zero_arithmetic());
        let mut retained_calls = RetainedCallResults::default();
        let prepared = lowerer
            .prepare_emission_group(
                EmissionRegion::CausalAssignment(0),
                &HashSet::new(),
                CrossGroupCallRetention::Unguarded(&mut retained_calls),
                |lowerer, _| lowerer.lower(call).map(|_| ()),
            )
            .unwrap();
        let mut statements = Vec::new();
        let mut actions = Vec::new();
        prepared.commit_into(&mut statements, &mut actions);

        assert_eq!(
            CommittedCallActionLedger::occurrence_count(&actions),
            2,
            "both the nested actual and its outer call must commit ownership"
        );
        assert!(
            statements.iter().any(|statement| matches!(
                &statement.node,
                gast::Statement::MultiAssignment { call, .. }
                    if call.function.lexeme() == "leaf"
            )),
            "the unused actual's call must execute before the wrapper result is used: {statements:?}"
        );
        CommittedCallActionLedger::construct(view, actions).unwrap();
    });
}

fn define_asserting_fold_leaf<'dae>(
    dae: &mut dae::DaeConstruction<'dae>,
    real: dae::ValueTypeId<'dae>,
    body_only_local: Option<dae::ValueTypeId<'dae>>,
    at: dae::DaeProvenance,
) -> Result<dae::FunctionId<'dae>, dae::DaeConstructionError> {
    let (leaf, ()) = dae.function(
        dae::FunctionSignature::new(VarName::new("reviewFoldLeaf"), [real], [real], at),
        |dae, reservation| {
            let (input, output) = dae.functions(|functions| {
                let input = functions.parameter(&reservation, VarName::new("u"), 0, at)?;
                let output = functions.output(&reservation, VarName::new("v"), 0, at)?;
                if let Some(value_type) = body_only_local {
                    functions.local(&reservation, VarName::new("scratch"), value_type, at)?;
                }
                Ok((input, output))
            })?;
            let input_value =
                dae.expressions(|expressions| expressions.at(at).function_parameter(input))?;
            let (condition, message) = dae.expressions(|expressions| {
                let zero = expressions.at(at).literal(dae::DaeLiteral::Real(0.0))?;
                let condition =
                    expressions
                        .at(at)
                        .binary(dae::BinaryOperator::Greater, input_value, zero)?;
                let message = expressions
                    .at(at)
                    .literal(dae::DaeLiteral::String("fold leaf evaluated".to_owned()))?;
                Ok((condition, message))
            })?;
            let mut body = dae.functions(|functions| functions.begin(reservation, at))?;
            dae.functions(|functions| {
                functions.assertion(&mut body, condition, message, at)?;
                functions.assign(&mut body, output, input_value, at)?;
                functions.define(body, at)
            })
        },
    )?;
    Ok(leaf)
}

fn define_fold_caller<'dae>(
    dae: &mut dae::DaeConstruction<'dae>,
    types: (
        dae::ValueTypeId<'dae>,
        dae::ValueTypeId<'dae>,
        dae::ValueTypeId<'dae>,
    ),
    leaf: dae::FunctionId<'dae>,
    at: dae::DaeProvenance,
) -> Result<dae::FunctionId<'dae>, dae::DaeConstructionError> {
    let (real, vector, nested) = types;
    let (fold, ()) = dae.function(
        dae::FunctionSignature::new(VarName::new("reviewFold"), [vector], [real], at),
        |dae, reservation| {
            let (input, output) = dae.functions(|functions| {
                let input = functions.parameter(&reservation, VarName::new("x"), 0, at)?;
                let output = functions.output(&reservation, VarName::new("y"), 0, at)?;
                functions.local(&reservation, VarName::new("scratch"), nested, at)?;
                Ok((input, output))
            })?;
            let mut body = dae.functions(|functions| functions.begin(reservation, at))?;
            let zero = dae.expressions(|expressions| {
                expressions.at(at).literal(dae::DaeLiteral::Real(0.0))
            })?;
            dae.functions(|functions| functions.assign(&mut body, output, zero, at))?;
            let domain = dae.domains(|domains| {
                domains.structured(
                    rumoca_core::StructuredIndexDomain {
                        binders: vec![rumoca_core::StructuredIndexBinder {
                            id: rumoca_core::StructuredIndexBinderId::new(0),
                            display_name: "i".to_owned(),
                            lower: 1,
                            upper: 2,
                            step: 1,
                        }],
                    },
                    at,
                )
            })?;
            let binder_id = dae.domains(|domains| domains.binder(domain, 0, at))?;
            let mut loop_body =
                dae.functions(|functions| functions.begin_loop(body, domain, [output], at))?;
            let carried =
                dae.functions(|functions| functions.read(loop_body.body(), output, at))?;
            let input_value =
                dae.expressions(|expressions| expressions.at(at).function_parameter(input))?;
            let binder = dae.expressions(|expressions| expressions.at(at).binder(binder_id))?;
            let selected = dae.expressions(|expressions| {
                expressions.at(at).index(
                    input_value,
                    [dae::Subscript::Index {
                        expression: binder,
                        provenance: at,
                    }],
                )
            })?;
            let leaf_value =
                dae.expressions(|expressions| expressions.at(at).call(leaf, 0, [selected]))?;
            let update = dae.expressions(|expressions| {
                expressions
                    .at(at)
                    .binary(dae::BinaryOperator::Add, carried, leaf_value)
            })?;
            dae.functions(|functions| functions.assign_loop(&mut loop_body, output, update, at))?;
            let body = dae.functions(|functions| functions.finish_loop(loop_body, at))?;
            dae.functions(|functions| functions.define(body, at))
        },
    )?;
    Ok(fold)
}

fn fold_call_memo_fixture(entered_leaf: bool) -> dae::Dae {
    let mut sources = SourceMap::new();
    let text = "function reviewFoldLeaf input Real u; output Real v; algorithm assert(u > 0.0, \"fold leaf evaluated\"); v := u; end reviewFoldLeaf; function reviewFold input Real x[2]; output Real y; protected Pair scratch; algorithm y := 0.0; for i in 1:2 loop y := y + reviewFoldLeaf(x[i]); end for; end reviewFold; reviewFold({1.0, 2.0});";
    let source = sources.add("fold-call-memo.mo", text);
    let at = dae::DaeProvenance::source(Span::from_offsets(source, 0, text.len())).unwrap();
    dae::Dae::construct(sources, |dae| {
        let types = dae.types(|types| {
            let real = types.derived(dae::ValueType::scalar(dae::ScalarType::Real), at)?;
            let vector = types.derived(dae::ValueType::array(dae::ScalarType::Real, [2]), at)?;
            let pair = types.record(VarName::new("Pair"), [(VarName::new("a"), real)], at)?;
            let nested = types.record(VarName::new("Nested"), [(VarName::new("p"), pair)], at)?;
            Ok((real, vector, nested))
        })?;
        let leaf = define_asserting_fold_leaf(dae, types.0, entered_leaf.then_some(types.2), at)?;
        let fold = define_fold_caller(dae, types, leaf, at)?;
        let values = dae.expressions(|expressions| {
            [1.0, 2.0]
                .into_iter()
                .map(|value| expressions.at(at).literal(dae::DaeLiteral::Real(value)))
                .collect::<Result<Vec<_>, _>>()
        })?;
        let argument = dae.expressions(|expressions| expressions.at(at).array(values))?;
        dae.expressions(|expressions| {
            expressions.at(at).call(fold, 0, [argument])?;
            Ok(())
        })
    })
    .unwrap()
}

fn identity_leaf_result(
    expression: &gast::Expression,
    leaf_values: &HashMap<String, f64>,
) -> Option<f64> {
    match expression {
        gast::Expression::Real(value) => Some(*value),
        gast::Expression::Ref(gast::Reference::Local(reference))
            if reference.subscripts.is_empty() =>
        {
            leaf_values.get(reference.name.lexeme()).copied()
        }
        gast::Expression::Paren(value) => identity_leaf_result(value, leaf_values),
        gast::Expression::Binary {
            op: gast::BinaryOp::Add,
            lhs,
            rhs,
        } => {
            Some(identity_leaf_result(lhs, leaf_values)? + identity_leaf_result(rhs, leaf_values)?)
        }
        _ => None,
    }
}

#[test]
fn statically_unrolled_fold_points_do_not_share_call_or_assertion_identity() {
    let model = fold_call_memo_fixture(false);
    model.inspect(|view| {
        let leaf = view.function(view.function_id(0).unwrap()).unwrap();
        assert!(
            first_function_assertion(leaf.statements()).is_some(),
            "every emitted leaf call executes the source assertion"
        );
        assert!(
            !user_functions::is_directly_lowerable(view, view.function_id(1).unwrap()),
            "the nested record local forces entered-body lowering"
        );
        let root = (0..view.expression_count())
            .filter_map(|index| view.expression_id(index))
            .rfind(|expression| {
                matches!(
                    view.expression(*expression).unwrap().operation(),
                    dae::ExpressionOperation::Call { function, .. }
                        if function == view.function_id(1).unwrap()
                )
            })
            .expect("fixture has one outer root call");
        let definitions = rumoca_phase_structural::CausalDefinitions::derive(view);
        let variables = HashMap::new();
        let previous = HashMap::new();
        let mut lowerer = ExpressionLowerer::with_do_step_effects(
            view,
            &definitions,
            &variables,
            &previous,
            positive_zero_arithmetic(),
        );
        let result = lowerer.lower(root).unwrap();
        let statements = lowerer.take_prefix_statements();
        let calls = statements
            .iter()
            .filter_map(|statement| match &statement.node {
                gast::Statement::MultiAssignment { targets, call }
                    if call.function.lexeme() == "reviewFoldLeaf" =>
                {
                    Some((targets, call))
                }
                _ => None,
            })
            .collect::<Vec<_>>();
        assert_eq!(
            calls.len(),
            2,
            "one assertion-bearing call per fold point: {statements:?}"
        );

        let mut values = HashMap::new();
        let mut leaf_results = HashSet::new();
        for statement in &statements {
            if let gast::Statement::Assignment { target, value } = &statement.node
                && let gast::Reference::Local(target) = target
                && target.subscripts.is_empty()
                && let Some(value) = identity_leaf_result(value, &values)
            {
                values.insert(target.name.lexeme().to_owned(), value);
            }
            let gast::Statement::MultiAssignment { targets, call } = &statement.node else {
                continue;
            };
            if call.function.lexeme() != "reviewFoldLeaf" {
                continue;
            }
            let [target] = targets.as_slice() else {
                panic!("scalar leaf call has one result")
            };
            let [gast::Expression::Ref(gast::Reference::Local(argument))] =
                call.arguments.as_slice()
            else {
                panic!("fold leaf receives one projected aggregate argument")
            };
            let [gast::Expression::Integer(index)] = argument.subscripts.as_slice() else {
                panic!("static fold point projects one literal coordinate")
            };
            let gast::Reference::Local(target) = target else {
                panic!("materialized call result is function-local")
            };
            assert!(
                leaf_results.insert(target.name.lexeme().to_owned()),
                "each iteration owns a distinct result temporary"
            );
            values.insert(target.name.lexeme().to_owned(), *index as f64);
        }
        assert_eq!(leaf_results.len(), 2);
        assert_eq!(identity_leaf_result(&result.expression, &values), Some(3.0));
    });
}

#[test]
fn entered_leaf_assertion_is_emitted_once_for_each_static_fold_point() {
    let model = fold_call_memo_fixture(true);
    model.inspect(|view| {
        let leaf = view.function_id(0).unwrap();
        assert!(
            !user_functions::is_directly_lowerable(view, leaf),
            "nested record local forces the leaf through entered-body assertion lowering"
        );
        let fold = view.function_id(1).unwrap();
        let root = (0..view.expression_count())
            .filter_map(|index| view.expression_id(index))
            .rfind(|expression| {
                matches!(
                    view.expression(*expression).unwrap().operation(),
                    dae::ExpressionOperation::Call { function, .. } if function == fold
                )
            })
            .expect("fixture has one outer root call");
        let definitions = rumoca_phase_structural::CausalDefinitions::derive(view);
        let variables = HashMap::new();
        let previous = HashMap::new();
        let mut lowerer = ExpressionLowerer::with_do_step_effects(
            view,
            &definitions,
            &variables,
            &previous,
            positive_zero_arithmetic(),
        );
        lowerer.lower(root).unwrap();
        let statements = lowerer.take_prefix_statements();
        let invalid_argument_guards = statements
            .iter()
            .filter(|statement| {
                let gast::Statement::If(assertion) = &statement.node else {
                    return false;
                };
                matches!(
                    assertion.branches.as_slice(),
                    [branch]
                        if matches!(branch.body.as_slice(), [signal]
                            if matches!(&signal.node, gast::Statement::Signal(signals)
                                if signals.as_slice() == [gast::Identifier::new(
                                    gast::PredefinedSignal::InvalidArgument.name()
                                )]))
                )
            })
            .count();
        assert_eq!(
            invalid_argument_guards, 2,
            "each static fold point owns one entered call-scoped assertion: {statements:?}"
        );
    });
}

#[test]
fn function_identity_assignments_are_not_emitted() {
    let mut sources = SourceMap::new();
    let source = sources.add("identity-assignment.mo", "function makePair");
    let provenance = dae::DaeProvenance::source(Span::from_offsets(source, 0, 17)).unwrap();
    let model = dae::Dae::construct(sources, |dae| {
        let real = dae.types(|types| {
            types.derived(dae::ValueType::scalar(dae::ScalarType::Real), provenance)
        })?;
        let pair = dae.types(|types| {
            types.record(
                VarName::new("Pair"),
                [(VarName::new("left"), real), (VarName::new("right"), real)],
                provenance,
            )
        })?;
        let signature = dae::FunctionSignature::new(
            VarName::new("makePair"),
            std::iter::empty(),
            [pair],
            provenance,
        );
        dae.function(signature, |dae, reservation| {
            let output = dae.functions(|functions| {
                functions.output(&reservation, VarName::new("result"), 0, provenance)
            })?;
            let left = dae.functions(|functions| {
                functions.local(&reservation, VarName::new("result.left"), real, provenance)
            })?;
            let right = dae.functions(|functions| {
                functions.local(&reservation, VarName::new("result.right"), real, provenance)
            })?;
            let mut body = dae.functions(|functions| functions.begin(reservation, provenance))?;
            let values = dae.expressions(|expressions| {
                Ok([
                    expressions
                        .at(provenance)
                        .literal(dae::DaeLiteral::Real(1.0))?,
                    expressions
                        .at(provenance)
                        .literal(dae::DaeLiteral::Real(2.0))?,
                ])
            })?;
            dae.functions(|functions| functions.assign(&mut body, left, values[0], provenance))?;
            dae.functions(|functions| functions.assign(&mut body, right, values[1], provenance))?;
            let fields = dae.functions(|functions| {
                Ok([
                    functions.read(&body, left, provenance)?,
                    functions.read(&body, right, provenance)?,
                ])
            })?;
            let fields =
                dae.expressions(|expressions| expressions.at(provenance).record(pair, fields))?;
            dae.functions(|functions| functions.assign(&mut body, output, fields, provenance))?;
            dae.functions(|functions| functions.define(body, provenance))
        })?;
        Ok(())
    })
    .unwrap();

    model.inspect(|view| {
        let function = view.function_id(0).unwrap();
        let definitions = rumoca_phase_structural::CausalDefinitions::derive(view);
        let lowered = user_functions::lower_reachable(
            view,
            &definitions,
            HashSet::from([function.index()]),
            positive_zero_arithmetic(),
        )
        .expect("identity assignment lowers");
        assert_eq!(lowered.len(), 1);
        assert_eq!(lowered[0].statements.len(), 2);
        assert!(lowered[0].statements.iter().all(|statement| {
            !matches!(
                &statement.node,
                gast::Statement::Assignment {
                    target: gast::Reference::Local(target),
                    value: gast::Expression::Ref(gast::Reference::Local(value)),
                } if target == value
            )
        }));
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
        let definitions = rumoca_phase_structural::CausalDefinitions::derive(view);
        let mut lowerer = ExpressionLowerer::new(
            view,
            &definitions,
            &variables,
            &previous,
            positive_zero_arithmetic(),
        );
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
        let definitions = rumoca_phase_structural::CausalDefinitions::derive(view);
        let mut lowerer = ExpressionLowerer::new(
            view,
            &definitions,
            &variables,
            &previous,
            positive_zero_arithmetic(),
        );
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
                        id: rumoca_core::StructuredIndexBinderId::new(0),
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
        let definitions = rumoca_phase_structural::CausalDefinitions::derive(view);
        let mut lowerer = ExpressionLowerer::new(
            view,
            &definitions,
            &variables,
            &previous,
            positive_zero_arithmetic(),
        );
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

/// The whole-array-move decision needs BOTH conjuncts: subscript identity
/// alone must never flatten a projection whose source is declared with
/// different extents than the target.
///
/// This constructs the situation the front end cannot yet spell — a
/// subscript-identity projection over a shape-mismatched source — directly
/// against `provable_whole_array_move`, because today every reachable
/// identity projection reads an object declared from the target's own type
/// (the would-be offenders, `y := s[1:2]` and leading sub-range
/// comprehensions, arrive with their range arithmetic unfolded and fail the
/// subscript check first). One index-folding improvement changes that, and
/// this test is what bites: delete or weaken the shape conjunct and the
/// mismatched case below collapses into a whole-array assignment between
/// differently-shaped objects.
#[test]
fn whole_array_move_needs_shape_equality_not_just_subscript_identity() {
    let model = dae::Dae::construct(SourceMap::new(), |_| Ok(())).unwrap();
    model.inspect(|view| {
        let definitions = rumoca_phase_structural::CausalDefinitions::derive(view);
        let variables = HashMap::new();
        let previous = HashMap::new();
        let mut lowerer = ExpressionLowerer::with_do_step_effects(
            view,
            &definitions,
            &variables,
            &previous,
            positive_zero_arithmetic(),
        );
        for (name, extent) in [("wide", 5), ("exact", 2)] {
            lowerer.temporary_locals.push(gast::VariableDeclaration {
                ty: gast::TypeRef::Primitive(gast::ScalarType::Real),
                name: gast::Name::ident(name),
                dimensions: vec![gast::Dimension::Expr(gast::Expression::Integer(extent))],
                range: gast::RangeAttributes::default(),
                span: Span::DUMMY,
            });
        }
        let index = gast::Expression::Ref(gast::Reference::local(gast::Name::ident("i0")));
        let projection = |source: &str| {
            gast::Expression::Ref(gast::Reference::Local(gast::RefPart {
                name: gast::Name::ident(source),
                subscripts: vec![index.clone()],
                span: Span::DUMMY,
            }))
        };

        // Identity subscripts, equal declared shapes: the move is provable.
        let proven = whole_array_move::provable_whole_array_move(
            &mut lowerer,
            &projection("exact"),
            std::slice::from_ref(&index),
            &[2],
            gast::ScalarType::Real,
        );
        assert!(
            matches!(
                proven,
                Some(gast::Reference::Local(ref part))
                    if part.name.lexeme() == "exact" && part.subscripts.is_empty()
            ),
            "a shape-equal identity projection must flatten, got {proven:?}"
        );

        // The same identity subscripts over a WIDER source: flattening would
        // write a whole-array assignment between differently-shaped objects,
        // so the loop must stay.
        assert_eq!(
            whole_array_move::provable_whole_array_move(
                &mut lowerer,
                &projection("wide"),
                std::slice::from_ref(&index),
                &[2],
                gast::ScalarType::Real,
            ),
            None,
            "subscript identity without shape equality must keep its loop"
        );

        // Element type is part of the shape: same extents, different scalar.
        assert_eq!(
            whole_array_move::provable_whole_array_move(
                &mut lowerer,
                &projection("exact"),
                std::slice::from_ref(&index),
                &[2],
                gast::ScalarType::Integer,
            ),
            None,
            "an element-type mismatch must keep its loop"
        );
    });
}
