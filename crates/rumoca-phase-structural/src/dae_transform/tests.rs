use rumoca_core::{SourceMap, Span, TypeId, VarName};

use super::*;

fn source_provenance(
    source: rumoca_core::SourceId,
    text: &str,
    needle: &str,
) -> dae::DaeProvenance {
    let start = text.find(needle).expect("fixture snippet exists");
    dae::DaeProvenance::source(Span::from_offsets(source, start, start + needle.len()))
        .expect("fixture span is source-backed")
}

#[derive(Clone, Copy)]
struct FixtureVariables<'dae> {
    p: dae::ParameterId<'dae>,
    x: dae::StateId<'dae>,
    y: dae::StateId<'dae>,
    a: dae::AlgebraicId<'dae>,
    z: Option<dae::AlgebraicId<'dae>>,
    d: Option<dae::DiscreteRealId<'dae>>,
    b: Option<dae::DiscreteValueId<'dae>>,
}

#[derive(Clone, Copy, Default)]
struct FixtureFeatures {
    family: bool,
    discrete: bool,
    events: bool,
    clocks: bool,
    delay: bool,
    bounded_delay: bool,
}

#[derive(Clone, Copy)]
struct FixtureSpans {
    constraint: dae::DaeProvenance,
    derivative_y: dae::DaeProvenance,
    derivative_x: dae::DaeProvenance,
}

fn fixture_types<'dae>(
    model: &mut dae::DaeConstruction<'dae>,
    declaration: dae::DaeProvenance,
) -> Result<
    (
        dae::ValueTypeId<'dae>,
        dae::ValueTypeId<'dae>,
        dae::ValueTypeId<'dae>,
    ),
    dae::DaeConstructionError,
> {
    model.types(|types| {
        Ok((
            types.intern(
                TypeId::new(0),
                dae::ValueType::scalar(dae::ScalarType::Real),
                declaration,
            )?,
            types.intern(
                TypeId::new(1),
                dae::ValueType::array(dae::ScalarType::Real, [2]),
                declaration,
            )?,
            types.intern(
                TypeId::new(2),
                dae::ValueType::scalar(dae::ScalarType::Boolean),
                declaration,
            )?,
        ))
    })
}

fn fixture_domain<'dae>(
    model: &mut dae::DaeConstruction<'dae>,
    owner: Option<dae::DaeProvenance>,
) -> Result<Option<dae::DomainId<'dae>>, dae::DaeConstructionError> {
    owner
        .map(|owner| {
            model.domains(|domains| {
                domains.structured(
                    rumoca_core::StructuredIndexDomain {
                        binders: vec![rumoca_core::StructuredIndexBinder {
                            id: 0,
                            display_name: "i".to_owned(),
                            lower: 1,
                            upper: 2,
                            step: 1,
                        }],
                    },
                    owner,
                )
            })
        })
        .transpose()
}

fn fixture_variables<'dae>(
    model: &mut dae::DaeConstruction<'dae>,
    real: dae::ValueTypeId<'dae>,
    vector: dae::ValueTypeId<'dae>,
    boolean: dae::ValueTypeId<'dae>,
    declaration: dae::DaeProvenance,
    features: FixtureFeatures,
) -> Result<FixtureVariables<'dae>, dae::DaeConstructionError> {
    model.variables(|variables| {
        let z = if features.family {
            Some(variables.algebraic(
                VarName::new("z"),
                vector,
                declaration,
                dae::VariableAttributes::default(),
            )?)
        } else {
            None
        };
        let d = if features.discrete {
            Some(variables.discrete_real(
                VarName::new("d"),
                real,
                declaration,
                dae::VariableAttributes::default(),
            )?)
        } else {
            None
        };
        let b = if features.discrete {
            Some(variables.discrete_value(
                VarName::new("b"),
                boolean,
                declaration,
                dae::VariableAttributes::default(),
            )?)
        } else {
            None
        };
        Ok(FixtureVariables {
            p: variables.parameter(
                VarName::new("p"),
                real,
                declaration,
                dae::VariableAttributes::default(),
            )?,
            x: variables.state(
                VarName::new("x"),
                real,
                declaration,
                dae::VariableAttributes::default(),
            )?,
            y: variables.state(
                VarName::new("y"),
                real,
                declaration,
                dae::VariableAttributes::default(),
            )?,
            a: variables.algebraic(
                VarName::new("a"),
                real,
                declaration,
                dae::VariableAttributes::default(),
            )?,
            z,
            d,
            b,
        })
    })
}

fn fixture_family_residual<'dae>(
    model: &mut dae::DaeConstruction<'dae>,
    domain: Option<dae::DomainId<'dae>>,
    owner: Option<dae::DaeProvenance>,
    z: Option<dae::AlgebraicId<'dae>>,
) -> Result<Option<dae::ExprId<'dae>>, dae::DaeConstructionError> {
    let (Some(domain), Some(owner), Some(z)) = (domain, owner, z) else {
        return Ok(None);
    };
    let binder = model.domains(|domains| domains.binder(domain, 0, owner))?;
    model
        .expressions(|expressions| {
            let aggregate = expressions
                .at(owner)
                .coordinate(dae::CoordinateInput::Algebraic(z))?;
            let index = expressions.at(owner).binder(binder)?;
            let value = expressions.at(owner).index(
                aggregate,
                [dae::Subscript::Index {
                    expression: index,
                    provenance: owner,
                }],
            )?;
            expressions
                .at(owner)
                .binary(dae::BinaryOperator::Subtract, value, index)
        })
        .map(Some)
}

fn insert_fixture_equations<'dae>(
    model: &mut dae::DaeConstruction<'dae>,
    spans: FixtureSpans,
    residuals: [dae::ExprId<'dae>; 3],
    family: Option<(dae::DomainId<'dae>, dae::DaeProvenance, dae::ExprId<'dae>)>,
    discrete: Option<FixtureDiscrete<'dae>>,
) -> Result<(), dae::DaeConstructionError> {
    model.continuous(|continuous| {
        continuous.value_equation(spans.constraint, residuals[0])?;
        continuous.value_equation(spans.derivative_y, residuals[1])?;
        continuous.value_equation(spans.derivative_x, residuals[2])?;
        if let Some((domain, owner, residual)) = family {
            continuous.structured_family(
                owner,
                domain,
                rumoca_core::ComprehensionScalarView::BinderSubstitution,
                |family| family.body(residual),
            )?;
        }
        Ok(())
    })?;
    if let Some((domain, owner, residual)) = family {
        model.initialization(|initialization| {
            initialization.structured_family(
                owner,
                domain,
                rumoca_core::ComprehensionScalarView::BinderSubstitution,
                |family| family.body(residual),
            )?;
            Ok(())
        })?;
    }
    insert_fixture_discrete(model, discrete)?;
    Ok(())
}

fn insert_fixture_discrete<'dae>(
    model: &mut dae::DaeConstruction<'dae>,
    discrete: Option<FixtureDiscrete<'dae>>,
) -> Result<(), dae::DaeConstructionError> {
    let Some(discrete) = discrete else {
        return Ok(());
    };
    model.discrete(|equations| {
        equations.real_equation(discrete.real_owner, |equation| {
            equation.residual(discrete.real_residual)
        })?;
        equations.assignment(discrete.value_owner, discrete.value_target, discrete.value)?;
        Ok(())
    })
}

#[derive(Clone, Copy)]
struct FixtureDiscrete<'dae> {
    real_owner: dae::DaeProvenance,
    real_residual: dae::ExprId<'dae>,
    value_owner: dae::DaeProvenance,
    value_target: dae::DiscreteValueId<'dae>,
    value: dae::ExprId<'dae>,
}

fn fixture_discrete<'dae>(
    model: &mut dae::DaeConstruction<'dae>,
    variables: FixtureVariables<'dae>,
    owners: Option<(dae::DaeProvenance, dae::DaeProvenance)>,
) -> Result<Option<FixtureDiscrete<'dae>>, dae::DaeConstructionError> {
    let (Some(d), Some(b), Some((real_owner, value_owner))) = (variables.d, variables.b, owners)
    else {
        return Ok(None);
    };
    model.expressions(|expressions| {
        let d = expressions
            .at(real_owner)
            .coordinate(dae::CoordinateInput::DiscreteReal(d))?;
        let two = expressions
            .at(real_owner)
            .literal(dae::DaeLiteral::Real(2.0))?;
        let real_residual =
            expressions
                .at(real_owner)
                .binary(dae::BinaryOperator::Subtract, d, two)?;
        let value = expressions
            .at(value_owner)
            .literal(dae::DaeLiteral::Boolean(true))?;
        Ok(Some(FixtureDiscrete {
            real_owner,
            real_residual,
            value_owner,
            value_target: b,
            value,
        }))
    })
}

fn insert_fixture_events<'dae>(
    model: &mut dae::DaeConstruction<'dae>,
    variables: FixtureVariables<'dae>,
    source: rumoca_core::SourceId,
    text: &str,
    enabled: bool,
) -> Result<(), dae::DaeConstructionError> {
    if !enabled {
        return Ok(());
    }
    let relation_at = source_provenance(source, text, "y > 0");
    let reinitialize_at = source_provenance(source, text, "reinit(y, 0)");
    let assert_at = source_provenance(source, text, "assert(y > 0, \"event fired\")");
    let time_at = source_provenance(source, text, "sample(0.5)");
    let condition = model.conditions(|conditions| conditions.reserve(relation_at))?;
    let (relation_expression, value, message) = model.expressions(|expressions| {
        let y = expressions
            .at(relation_at)
            .coordinate(dae::CoordinateInput::State(variables.y))?;
        let zero = expressions
            .at(relation_at)
            .literal(dae::DaeLiteral::Real(0.0))?;
        let relation = expressions
            .at(relation_at)
            .binary(dae::BinaryOperator::Greater, y, zero)?;
        let value = expressions
            .at(reinitialize_at)
            .literal(dae::DaeLiteral::Real(0.0))?;
        let message = expressions
            .at(assert_at)
            .literal(dae::DaeLiteral::String("event fired".to_owned()))?;
        Ok((relation, value, message))
    })?;
    let relation =
        model.conditions(|conditions| conditions.relation(relation_expression, relation_at))?;
    model.conditions(|conditions| {
        conditions.define(
            condition,
            dae::ConditionInput::Relation(relation),
            relation_at,
        )?;
        conditions.root(relation, condition, relation_at)?;
        Ok(())
    })?;
    model.expressions(|expressions| {
        expressions
            .at(relation_at)
            .coordinate(dae::CoordinateInput::Condition(condition))
    })?;
    model.events(|events| {
        events.time_event(
            rumoca_core::ClockRational::new(1, 2)
                .expect("fixture time event has a positive rational instant"),
            time_at,
        )?;
        events.reinitialize(condition, condition, variables.y, value, reinitialize_at)?;
        events.assert(condition, condition, message, assert_at)?;
        Ok(())
    })
}

fn insert_fixture_clock<'dae>(
    model: &mut dae::DaeConstruction<'dae>,
    variables: FixtureVariables<'dae>,
    source: rumoca_core::SourceId,
    text: &str,
    enabled: bool,
) -> Result<(), dae::DaeConstructionError> {
    if !enabled {
        return Ok(());
    }
    let clock_at = source_provenance(source, text, "Clock(1, 10)");
    let previous_at = source_provenance(source, text, "previous(d)");
    let terminal_at = source_provenance(source, text, "terminal()");
    let condition = model.conditions(|conditions| conditions.reserve(clock_at))?;
    let clock = model.clocks(|clocks| {
        let lattice = rumoca_core::ClockLattice::from_interval_counter(1, 10)
            .expect("fixture clock has a positive exact period");
        clocks.periodic(lattice, clock_at)
    })?;
    let d = variables
        .d
        .expect("clock fixture requires a discrete-real variable");
    model.clocks(|clocks| clocks.own_discrete_real(clock, d, clock_at))?;
    let previous =
        model.temporal(|temporal| temporal.previous_discrete_real(clock, d, previous_at))?;
    let terminal = model.temporal(|temporal| temporal.terminal(terminal_at))?;
    model.conditions(|conditions| {
        conditions.define(condition, dae::ConditionInput::Clock(clock), clock_at)
    })?;
    model.expressions(|expressions| {
        expressions
            .at(previous_at)
            .coordinate(dae::CoordinateInput::Previous(previous))?;
        expressions
            .at(terminal_at)
            .coordinate(dae::CoordinateInput::Terminal(terminal))?;
        expressions
            .at(clock_at)
            .coordinate(dae::CoordinateInput::Condition(condition))?;
        Ok(())
    })
}

fn insert_fixture_delay<'dae>(
    model: &mut dae::DaeConstruction<'dae>,
    variables: FixtureVariables<'dae>,
    source: rumoca_core::SourceId,
    text: &str,
    features: FixtureFeatures,
) -> Result<(), dae::DaeConstructionError> {
    if !features.delay && !features.bounded_delay {
        return Ok(());
    }
    let call_text = if features.bounded_delay {
        "delay(y, 0.5, 1.0)"
    } else {
        "delay(y, 0.5)"
    };
    let call_start = text.find(call_text).expect("delay fixture call exists");
    let owner = dae::DaeProvenance::source(Span::from_offsets(
        source,
        call_start,
        call_start + call_text.len(),
    ))
    .expect("delay owner span is source-backed");
    let source_at =
        dae::DaeProvenance::source(Span::from_offsets(source, call_start + 6, call_start + 7))
            .expect("delay source span is source-backed");
    let timing_at =
        dae::DaeProvenance::source(Span::from_offsets(source, call_start + 9, call_start + 12))
            .expect("delay timing span is source-backed");
    let maximum_at = features.bounded_delay.then(|| {
        dae::DaeProvenance::source(Span::from_offsets(source, call_start + 14, call_start + 17))
            .expect("delay maximum span is source-backed")
    });
    let (delayed, timing, maximum) = model.expressions(|expressions| {
        Ok((
            expressions
                .at(source_at)
                .coordinate(dae::CoordinateInput::State(variables.y))?,
            expressions
                .at(timing_at)
                .literal(dae::DaeLiteral::Real(0.5))?,
            maximum_at
                .map(|at| expressions.at(at).literal(dae::DaeLiteral::Real(1.0)))
                .transpose()?,
        ))
    })?;
    model.temporal(|temporal| {
        if let (Some(maximum), Some(maximum_at)) = (maximum, maximum_at) {
            let positive = temporal.positive_parameter(maximum, 1.0, maximum_at)?;
            temporal
                .bounded_delay(delayed, timing, positive, owner, owner)
                .map(|_| ())
        } else {
            let positive = temporal.positive_parameter(timing, 0.5, timing_at)?;
            temporal.delay(delayed, positive, owner, owner).map(|_| ())
        }
    })
}

fn constrained_state_model(
    nonlinear_constraint: bool,
    features: FixtureFeatures,
) -> (dae::Dae, dae::DaeProvenance) {
    let rhs = if nonlinear_constraint {
        "sin(y)"
    } else {
        "p*y"
    };
    let family_declaration = if features.family { " Real z[2];" } else { "" };
    let family_equation = if features.family {
        " for i in 1:2 loop z[i] = i; end for;"
    } else {
        ""
    };
    let discrete_declaration = if features.discrete {
        " discrete Real d; Boolean b;"
    } else {
        ""
    };
    let discrete_equations = if features.discrete {
        " d = 2; b = true;"
    } else {
        ""
    };
    let event_equations = if features.events {
        " when y > 0 then reinit(y, 0); assert(y > 0, \"event fired\"); end when; sample(0.5);"
    } else {
        ""
    };
    let clock_equations = if features.clocks {
        " Clock(1, 10); previous(d); terminal();"
    } else {
        ""
    };
    let delay_equations = if features.bounded_delay {
        " delay(y, 0.5, 1.0);"
    } else if features.delay {
        " delay(y, 0.5);"
    } else {
        ""
    };
    let text = format!(
        "parameter Real p; Real x; Real y; Real a;{family_declaration}{discrete_declaration} equation x = {rhs}; der(y) = a; der(x) = 1;{family_equation}{discrete_equations}{event_equations}{clock_equations}{delay_equations}"
    );
    let mut sources = SourceMap::new();
    let source = sources.add("checked_index_reduction.mo", &text);
    let declaration = source_provenance(source, &text, "parameter Real p");
    let constraint = source_provenance(source, &text, &format!("x = {rhs}"));
    let derivative_y = source_provenance(source, &text, "der(y) = a");
    let derivative_x = source_provenance(source, &text, "der(x) = 1");
    let family_owner = features
        .family
        .then(|| source_provenance(source, &text, "for i in 1:2 loop z[i] = i"));
    let discrete_owners = features.discrete.then(|| {
        (
            source_provenance(source, &text, "d = 2"),
            source_provenance(source, &text, "b = true"),
        )
    });
    let model = dae::Dae::construct(sources, |model| {
        let (real, vector, boolean) = fixture_types(model, declaration)?;
        let domain = fixture_domain(model, family_owner)?;
        let variables = fixture_variables(model, real, vector, boolean, declaration, features)?;
        insert_fixture_clock(model, variables, source, &text, features.clocks)?;
        insert_fixture_delay(model, variables, source, &text, features)?;
        let spans = FixtureSpans {
            constraint,
            derivative_y,
            derivative_x,
        };
        let residuals = model.expressions(|expressions| {
            fixture_residuals(expressions, variables, spans, nonlinear_constraint)
        })?;
        let family_residual = fixture_family_residual(model, domain, family_owner, variables.z)?;
        let family = domain
            .zip(family_owner)
            .zip(family_residual)
            .map(|((domain, owner), residual)| (domain, owner, residual));
        let discrete = fixture_discrete(model, variables, discrete_owners)?;
        insert_fixture_equations(model, spans, residuals, family, discrete)?;
        insert_fixture_events(model, variables, source, &text, features.events)
    })
    .expect("fixture DAE is valid");
    (model, constraint)
}

fn fixture_residuals<'dae>(
    expressions: &mut dae::Expressions<'_, 'dae>,
    variables: FixtureVariables<'dae>,
    spans: FixtureSpans,
    nonlinear_constraint: bool,
) -> Result<[dae::ExprId<'dae>; 3], dae::DaeConstructionError> {
    let x_value = expressions
        .at(spans.constraint)
        .coordinate(dae::CoordinateInput::State(variables.x))?;
    let y_value = expressions
        .at(spans.constraint)
        .coordinate(dae::CoordinateInput::State(variables.y))?;
    let constraint_rhs = fixture_constraint_rhs(
        expressions,
        variables.p,
        y_value,
        spans.constraint,
        nonlinear_constraint,
    )?;
    let constraint = expressions.at(spans.constraint).binary(
        dae::BinaryOperator::Subtract,
        x_value,
        constraint_rhs,
    )?;
    let y_derivative = expressions
        .at(spans.derivative_y)
        .coordinate(dae::CoordinateInput::Derivative(variables.y))?;
    let a_value = expressions
        .at(spans.derivative_y)
        .coordinate(dae::CoordinateInput::Algebraic(variables.a))?;
    let derivative_y = expressions.at(spans.derivative_y).binary(
        dae::BinaryOperator::Subtract,
        y_derivative,
        a_value,
    )?;
    let x_derivative = expressions
        .at(spans.derivative_x)
        .coordinate(dae::CoordinateInput::Derivative(variables.x))?;
    let one = expressions
        .at(spans.derivative_x)
        .literal(dae::DaeLiteral::Real(1.0))?;
    let derivative_x = expressions.at(spans.derivative_x).binary(
        dae::BinaryOperator::Subtract,
        x_derivative,
        one,
    )?;
    Ok([constraint, derivative_y, derivative_x])
}

fn fixture_constraint_rhs<'dae>(
    expressions: &mut dae::Expressions<'_, 'dae>,
    parameter: dae::ParameterId<'dae>,
    state: dae::ExprId<'dae>,
    provenance: dae::DaeProvenance,
    nonlinear: bool,
) -> Result<dae::ExprId<'dae>, dae::DaeConstructionError> {
    if nonlinear {
        return expressions
            .at(provenance)
            .builtin(dae::PureBuiltin::Sin, [state]);
    }
    let parameter = expressions
        .at(provenance)
        .coordinate(dae::CoordinateInput::Parameter(parameter))?;
    expressions
        .at(provenance)
        .binary(dae::BinaryOperator::Multiply, parameter, state)
}

#[test]
fn direct_state_demotion_reconstructs_a_finalized_dae_with_exact_provenance() {
    let (model, constraint) = constrained_state_model(false, FixtureFeatures::default());
    let prepared = prepare_for_solve(&model).expect("index-one constraint is reducible");
    assert!(matches!(prepared, PreparedDae::Transformed { .. }));

    prepared.as_dae().inspect(|view| {
        let x = view
            .variables()
            .find(|(_, variable)| variable.name().as_str() == "x")
            .map(|(_, variable)| variable)
            .expect("x declaration survives reconstruction");
        assert_eq!(x.role(), dae::VariableRole::Algebraic);
        assert!(sort(view).is_ok(), "replacement DAE has a perfect matching");

        let generated = (0..view.expression_count())
            .filter_map(|index| view.expression_id(index))
            .filter_map(|id| view.expression(id))
            .filter(|expression| {
                expression.provenance().origin()
                    == dae::DaeProvenanceOrigin::Generated(dae::DaeGeneration::IndexReduction)
            })
            .collect::<Vec<_>>();
        assert!(!generated.is_empty());
        assert!(
            generated
                .iter()
                .all(|expression| expression.provenance().span() == constraint.span())
        );
        assert!(
            (0..view.expression_count())
                .filter_map(|index| view.expression_id(index))
                .filter_map(|id| view.expression(id))
                .all(|expression| {
                    !matches!(
                        expression.operation(),
                        dae::ExpressionOperation::Coordinate(dae::CoordinateView::Derivative(state))
                            if view
                                .variable(state.into())
                                .is_some_and(|variable| variable.name().as_str() == "x")
                    )
                })
        );
    });
}

#[test]
fn state_demotion_preserves_structured_families_and_binder_provenance() {
    let (model, _) = constrained_state_model(
        false,
        FixtureFeatures {
            family: true,
            ..FixtureFeatures::default()
        },
    );
    let prepared = prepare_for_solve(&model).expect("structured companion family is preserved");
    let transformed = match prepared {
        PreparedDae::Transformed { dae, .. } => dae,
        PreparedDae::Borrowed(_) => panic!("singular fixture requires state demotion"),
    };
    transformed.inspect(|view| {
        assert_eq!(view.domain_count(), 1);
        assert_eq!(view.continuous_family_count(), 1);
        assert_eq!(view.initialization_family_count(), 1);
        let family = view
            .continuous_family(0)
            .expect("structured family survives reconstruction");
        assert_eq!(family.scalar_rows(), 2);
        assert_eq!(
            view.source_text(family.provenance()),
            Some("for i in 1:2 loop z[i] = i")
        );
        let body = family
            .bodies()
            .get(0)
            .expect("structured family retains its body");
        let mut has_binder = false;
        dae::for_each_expression(view, body, |_, expression| {
            has_binder |= matches!(
                expression.operation(),
                dae::ExpressionOperation::Coordinate(dae::CoordinateView::Binder(binder))
                    if binder.domain() == family.domain()
            );
        });
        assert!(has_binder);
        let initial = view
            .initialization_family(0)
            .expect("initialization family survives reconstruction");
        assert_eq!(initial.domain(), family.domain());
        assert_eq!(initial.bodies().len(), family.bodies().len());
        assert_eq!(initial.provenance(), family.provenance());
        assert!(
            sort(view).is_ok(),
            "replacement DAE remains structurally square"
        );
    });
}

#[test]
fn state_demotion_preserves_discrete_equations_and_exact_provenance() {
    let (model, _) = constrained_state_model(
        false,
        FixtureFeatures {
            discrete: true,
            ..FixtureFeatures::default()
        },
    );
    let prepared = prepare_for_solve(&model).expect("discrete companion equations survive");
    let transformed = match prepared {
        PreparedDae::Transformed { dae, .. } => dae,
        PreparedDae::Borrowed(_) => panic!("singular fixture requires state demotion"),
    };
    transformed.inspect(|view| {
        assert_eq!(view.discrete_real_equation_count(), 1);
        assert_eq!(view.discrete_assignment_count(), 1);
        let real = view
            .discrete_real_equation(0)
            .expect("discrete-real equation survives reconstruction");
        assert_eq!(view.source_text(real.provenance()), Some("d = 2"));
        let assignment_id = view
            .discrete_assignment_id(0)
            .expect("discrete assignment identity resolves");
        let assignment = view
            .discrete_assignment(assignment_id)
            .expect("discrete assignment survives reconstruction");
        assert_eq!(view.source_text(assignment.provenance()), Some("b = true"));
        assert!(matches!(
            view.variable(assignment.target().into())
                .expect("assignment target resolves")
                .role(),
            dae::VariableRole::DiscreteValue
        ));
        assert!(
            sort(view).is_ok(),
            "replacement DAE remains structurally square"
        );
    });
}

#[test]
fn state_demotion_preserves_conditions_roots_and_event_owners() {
    let (model, _) = constrained_state_model(
        false,
        FixtureFeatures {
            events: true,
            ..FixtureFeatures::default()
        },
    );
    let prepared = prepare_for_solve(&model).expect("event companion owners survive");
    let transformed = match prepared {
        PreparedDae::Transformed { dae, .. } => dae,
        PreparedDae::Borrowed(_) => panic!("singular fixture requires state demotion"),
    };
    transformed.inspect(|view| {
        assert_eq!(view.relation_count(), 1);
        assert_eq!(view.condition_count(), 1);
        assert_eq!(view.root_count(), 1);
        assert_eq!(view.time_event_count(), 1);
        assert_eq!(view.event_action_count(), 2);
        let relation = view
            .relation(
                view.relation_id(0)
                    .expect("relation identity survives reconstruction"),
            )
            .expect("relation survives reconstruction");
        assert_eq!(view.source_text(relation.provenance()), Some("y > 0"));
        let reinitialize = view
            .event_action(
                view.event_action_id(0)
                    .expect("reinitialization identity survives"),
            )
            .expect("reinitialization survives reconstruction");
        assert_eq!(
            view.source_text(reinitialize.provenance()),
            Some("reinit(y, 0)")
        );
        assert!(matches!(
            reinitialize.operation(),
            dae::EventActionOperation::Reinitialize { state, .. }
                if view
                    .variable(state.into())
                    .is_some_and(|variable| variable.name().as_str() == "y")
        ));
        let condition_coordinates = (0..view.expression_count())
            .filter_map(|index| view.expression_id(index))
            .filter_map(|id| view.expression(id))
            .filter(|expression| {
                matches!(
                    expression.operation(),
                    dae::ExpressionOperation::Coordinate(dae::CoordinateView::Condition(_))
                )
            })
            .count();
        assert_eq!(condition_coordinates, 1);
        assert!(
            sort(view).is_ok(),
            "replacement DAE remains structurally square"
        );
    });
}

#[test]
fn state_demotion_preserves_clock_history_and_terminal_owners() {
    let (model, _) = constrained_state_model(
        false,
        FixtureFeatures {
            discrete: true,
            clocks: true,
            ..FixtureFeatures::default()
        },
    );
    let prepared = prepare_for_solve(&model).expect("clock companion owners survive");
    let transformed = match prepared {
        PreparedDae::Transformed { dae, .. } => dae,
        PreparedDae::Borrowed(_) => panic!("singular fixture requires state demotion"),
    };
    transformed.inspect(|view| {
        assert_eq!(view.clock_count(), 1);
        assert_eq!(view.clock_ownership_count(), 1);
        assert_eq!(view.previous_value_count(), 1);
        assert_eq!(view.terminal_count(), 1);
        let clock_id = view.clock_id(0).expect("clock identity survives");
        let clock = view.clock(clock_id).expect("clock survives reconstruction");
        assert_eq!(view.source_text(clock.provenance()), Some("Clock(1, 10)"));
        assert!(matches!(
            clock.operation(),
            dae::ClockOperation::Periodic(lattice)
                if lattice.period()
                    == rumoca_core::ClockRational::new(1, 10)
                        .expect("expected period is positive")
        ));
        let previous_id = view
            .previous_id(0)
            .expect("previous coordinate identity survives");
        let previous = view
            .previous(previous_id)
            .expect("previous owner survives reconstruction");
        assert_eq!(view.source_text(previous.provenance()), Some("previous(d)"));
        let terminal_id = view
            .terminal_id(0)
            .expect("terminal coordinate identity survives");
        let terminal = view
            .terminal(terminal_id)
            .expect("terminal owner survives reconstruction");
        assert_eq!(view.source_text(terminal.provenance()), Some("terminal()"));
        let temporal_coordinates = (0..view.expression_count())
            .filter_map(|index| view.expression_id(index))
            .filter_map(|id| view.expression(id))
            .filter(|expression| {
                matches!(
                    expression.operation(),
                    dae::ExpressionOperation::Coordinate(
                        dae::CoordinateView::Previous(_) | dae::CoordinateView::Terminal(_)
                    )
                )
            })
            .count();
        assert_eq!(temporal_coordinates, 2);
        assert!(
            sort(view).is_ok(),
            "replacement DAE remains structurally square"
        );
    });
}

#[test]
fn state_demotion_preserves_delay_owner_coordinate_and_timing_provenance() {
    let (model, _) = constrained_state_model(
        false,
        FixtureFeatures {
            delay: true,
            ..FixtureFeatures::default()
        },
    );
    let prepared = prepare_for_solve(&model).expect("delay companion owner survives");
    let transformed = match prepared {
        PreparedDae::Transformed { dae, .. } => dae,
        PreparedDae::Borrowed(_) => panic!("singular fixture requires state demotion"),
    };
    transformed.inspect(|view| {
        assert_eq!(view.delay_count(), 1);
        let delay_id = view.delay_id(0).expect("dense delay identity survives");
        let delay = view.delay(delay_id).expect("delay owner survives");
        assert_eq!(view.source_text(delay.provenance()), Some("delay(y, 0.5)"));
        assert_eq!(
            view.source_text(
                delay
                    .delay_time_evidence()
                    .expect("fixed delay retains positive evidence")
                    .provenance()
            ),
            Some("0.5")
        );
        assert_eq!(
            view.source_text(
                view.expression(delay.source())
                    .expect("delay source resolves")
                    .provenance()
            ),
            Some("y")
        );
        let coordinates = (0..view.expression_count())
            .filter_map(|index| view.expression_id(index))
            .filter_map(|id| view.expression(id))
            .filter(|expression| {
                matches!(
                    expression.operation(),
                    dae::ExpressionOperation::Coordinate(dae::CoordinateView::Delay(id))
                        if id == delay_id
                )
            })
            .collect::<Vec<_>>();
        assert_eq!(coordinates.len(), 1);
        assert_eq!(
            view.source_text(coordinates[0].provenance()),
            Some("delay(y, 0.5)")
        );
        assert!(
            sort(view).is_ok(),
            "replacement DAE remains structurally square"
        );
    });
}

#[test]
fn state_demotion_preserves_bounded_delay_capability_and_provenance() {
    let (model, _) = constrained_state_model(
        false,
        FixtureFeatures {
            bounded_delay: true,
            ..FixtureFeatures::default()
        },
    );
    let prepared = prepare_for_solve(&model).expect("bounded delay owner survives");
    let transformed = match prepared {
        PreparedDae::Transformed { dae, .. } => dae,
        PreparedDae::Borrowed(_) => panic!("singular fixture requires state demotion"),
    };
    transformed.inspect(|view| {
        let delay_id = view.delay_id(0).expect("dense delay identity survives");
        let delay = view.delay(delay_id).expect("bounded delay owner survives");
        assert_eq!(
            view.source_text(delay.provenance()),
            Some("delay(y, 0.5, 1.0)")
        );
        assert!(delay.delay_time_evidence().is_none());
        let maximum = delay
            .delay_max()
            .expect("bounded delay retains maximum capability");
        assert_eq!(maximum.value(), 1.0);
        assert_eq!(view.source_text(maximum.provenance()), Some("1.0"));
        assert_eq!(
            view.source_text(
                view.expression(delay.delay_time())
                    .expect("bounded delay timing resolves")
                    .provenance()
            ),
            Some("0.5")
        );
        let coordinate = (0..view.expression_count())
            .filter_map(|index| view.expression_id(index))
            .filter_map(|id| view.expression(id))
            .find(|expression| {
                matches!(
                    expression.operation(),
                    dae::ExpressionOperation::Coordinate(dae::CoordinateView::Delay(id))
                        if id == delay_id
                )
            })
            .expect("bounded delay coordinate survives");
        assert_eq!(
            view.source_text(coordinate.provenance()),
            Some("delay(y, 0.5, 1.0)")
        );
        assert!(sort(view).is_ok());
    });
}

#[test]
fn unsupported_symbolic_derivative_preserves_the_original_singular_error() {
    let (model, _) = constrained_state_model(true, FixtureFeatures::default());
    let error = match prepare_for_solve(&model) {
        Ok(_) => panic!("unsupported differentiation must not guess a replacement"),
        Err(error) => error,
    };
    assert!(matches!(error, StructuralError::Singular { .. }));
}
