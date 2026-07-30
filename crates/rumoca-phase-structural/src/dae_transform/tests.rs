mod functions;

use rumoca_core::{SourceMap, Span, TypeId, VarName};

use super::*;
use functions::{FixtureFunctionConfig, fixture_function_declarations, insert_fixture_functions};

fn source_provenance(
    source: rumoca_core::SourceId,
    text: &str,
    needle: &str,
) -> dae::DaeProvenance {
    source_provenance_occurrence(source, text, needle, 0)
}

fn source_provenance_occurrence(
    source: rumoca_core::SourceId,
    text: &str,
    needle: &str,
    occurrence: usize,
) -> dae::DaeProvenance {
    let start = text
        .match_indices(needle)
        .nth(occurrence)
        .map(|(start, _)| start)
        .expect("fixture snippet occurrence exists");
    dae::DaeProvenance::source(Span::from_offsets(source, start, start + needle.len()))
        .expect("fixture span is source-backed")
}

fn nested_source_provenance(
    source: rumoca_core::SourceId,
    text: &str,
    outer: &str,
    inner: &str,
    occurrence: usize,
) -> dae::DaeProvenance {
    let outer_start = text.find(outer).expect("fixture owner snippet exists");
    let inner_start = outer
        .match_indices(inner)
        .nth(occurrence)
        .map(|(start, _)| start)
        .expect("fixture nested snippet occurrence exists");
    dae::DaeProvenance::source(Span::from_offsets(
        source,
        outer_start + inner_start,
        outer_start + inner_start + inner.len(),
    ))
    .expect("fixture nested span is source-backed")
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
    record_companion: bool,
    functions: bool,
    holonomic: bool,
    deep_function: bool,
    same_rhs_definitions: bool,
    range: bool,
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

fn fixture_record_type<'dae>(
    model: &mut dae::DaeConstruction<'dae>,
    real: dae::ValueTypeId<'dae>,
    source: rumoca_core::SourceId,
    text: &str,
    enabled: bool,
) -> Result<Option<dae::ValueTypeId<'dae>>, dae::DaeConstructionError> {
    if !enabled {
        return Ok(None);
    }
    let declaration =
        source_provenance(source, text, "record Pair Real left; Real right; end Pair");
    model
        .types(|types| {
            types.record(
                VarName::new("Pair"),
                [(VarName::new("left"), real), (VarName::new("right"), real)],
                declaration,
            )
        })
        .map(Some)
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
        let build = |equation: &mut dae::ResidualEquation<'_, 'dae>| {
            equation.residual(discrete.real_residual)
        };
        match discrete.activation {
            Some(condition) => {
                equations.when_real_equation(condition, condition, discrete.real_owner, build)?;
            }
            None => {
                equations.real_equation(discrete.real_owner, build)?;
            }
        }
        Ok(())
    })?;
    model.b1c([discrete.value_target], |topology| {
        topology.owner(discrete.value_owner, [discrete.value_target], |owner| {
            owner.always(
                discrete.value_owner,
                [(discrete.value, discrete.value_owner)],
            )
        })?;
        Ok(())
    })
}

#[derive(Clone, Copy)]
struct FixtureDiscrete<'dae> {
    real_owner: dae::DaeProvenance,
    real_residual: dae::ExprId<'dae>,
    activation: Option<dae::ConditionId<'dae>>,
    value_owner: dae::DaeProvenance,
    value_target: dae::DiscreteValueId<'dae>,
    value: dae::ExprId<'dae>,
}

fn fixture_discrete<'dae>(
    model: &mut dae::DaeConstruction<'dae>,
    variables: FixtureVariables<'dae>,
    owners: Option<(dae::DaeProvenance, dae::DaeProvenance)>,
    activation: Option<dae::ConditionId<'dae>>,
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
            activation,
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
) -> Result<Option<dae::ConditionId<'dae>>, dae::DaeConstructionError> {
    if !enabled {
        return Ok(None);
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
    })?;
    Ok(Some(condition))
}

fn insert_fixture_record_companion<'dae>(
    model: &mut dae::DaeConstruction<'dae>,
    real: dae::ValueTypeId<'dae>,
    record: Option<dae::ValueTypeId<'dae>>,
    source: rumoca_core::SourceId,
    text: &str,
) -> Result<(), dae::DaeConstructionError> {
    let Some(record) = record else {
        return Ok(());
    };
    let binding_at = source_provenance(source, text, "Pair(2, 3)");
    let start_at = source_provenance(source, text, "Pair(4, 5)");
    let scalar_declaration_text = "parameter Real recordCompanion(start = 4, fixed = true) = 2";
    let scalar_declaration = source_provenance(source, text, scalar_declaration_text);
    let scalar_binding = nested_source_provenance(source, text, scalar_declaration_text, "2", 0);
    let scalar_start = nested_source_provenance(source, text, scalar_declaration_text, "4", 0);
    let projection_at = source_provenance(source, text, "Pair(2, 3).left");
    let use_start = text
        .find("Pair(2, 3).left")
        .expect("record value projection exists");
    let use_at = dae::DaeProvenance::source(Span::from_offsets(
        source,
        use_start,
        use_start + "Pair(2, 3)".len(),
    ))
    .expect("record value use span is source-backed");
    let (binding, start) = model.expressions(|expressions| {
        let binding = expressions
            .at(scalar_binding)
            .literal(dae::DaeLiteral::Real(2.0))?;
        let start = expressions
            .at(scalar_start)
            .literal(dae::DaeLiteral::Real(4.0))?;
        let binding_fields = [
            expressions
                .at(binding_at)
                .literal(dae::DaeLiteral::Integer(2))?,
            expressions
                .at(binding_at)
                .literal(dae::DaeLiteral::Integer(3))?,
        ];
        let start_fields = [
            expressions
                .at(start_at)
                .literal(dae::DaeLiteral::Integer(4))?,
            expressions
                .at(start_at)
                .literal(dae::DaeLiteral::Integer(5))?,
        ];
        expressions.at(binding_at).record(record, binding_fields)?;
        expressions.at(start_at).record(record, start_fields)?;
        Ok((binding, start))
    })?;
    model.variables(|variables| {
        variables
            .parameter(
                VarName::new("recordCompanion"),
                real,
                scalar_declaration,
                dae::VariableAttributes {
                    binding: Some(binding),
                    start: Some(start),
                    fixed: Some(true),
                    description: Some("record companion".to_owned()),
                    ..dae::VariableAttributes::default()
                },
            )
            .map(|_| ())
    })?;
    model.expressions(|expressions| {
        let binding_fields = [
            expressions
                .at(use_at)
                .literal(dae::DaeLiteral::Integer(2))?,
            expressions
                .at(use_at)
                .literal(dae::DaeLiteral::Integer(3))?,
        ];
        let base = expressions.at(use_at).record(record, binding_fields)?;
        expressions.at(projection_at).field(base, 0)?;
        let generated = dae::DaeProvenance::generated(
            dae::DaeGeneration::RecordEquationProjection,
            projection_at.span(),
        )?;
        expressions.at(generated).field(base, 1)?;
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
    let interval_at = source_provenance(source, text, "interval()");
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
    model.clocks(|clocks| clocks.own_discrete_real(clock.into(), d, clock_at))?;
    let previous =
        model.temporal(|temporal| temporal.previous_discrete_real(clock.into(), d, previous_at))?;
    let terminal = model.temporal(|temporal| temporal.terminal(terminal_at))?;
    model.conditions(|conditions| {
        conditions.define(
            condition,
            dae::ConditionInput::Clock(clock.into()),
            clock_at,
        )
    })?;
    model.expressions(|expressions| {
        expressions
            .at(previous_at)
            .coordinate(dae::CoordinateInput::Previous(previous))?;
        expressions
            .at(terminal_at)
            .coordinate(dae::CoordinateInput::Terminal(terminal))?;
        expressions
            .at(interval_at)
            .coordinate(dae::CoordinateInput::ClockInterval(clock))?;
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
    let positive = model.temporal(|temporal| {
        if let (Some(maximum), Some(maximum_at)) = (maximum, maximum_at) {
            temporal.positive_parameter(maximum, 1.0, maximum_at)
        } else {
            temporal.positive_parameter(timing, 0.5, timing_at)
        }
    })?;
    model.expressions(|expressions| {
        if maximum.is_some() {
            expressions
                .at(owner)
                .bounded_delay(delayed, timing, positive, owner)
                .map(|_| ())
        } else {
            expressions
                .at(owner)
                .delay(delayed, positive, owner)
                .map(|_| ())
        }
    })
}

fn constrained_state_model(
    nonlinear_constraint: bool,
    features: FixtureFeatures,
) -> (dae::Dae, dae::DaeProvenance) {
    let text = fixture_source_text(nonlinear_constraint, features);
    let mut sources = SourceMap::new();
    let source = sources.add("checked_index_reduction.mo", &text);
    let spans = fixture_source_spans(source, &text, nonlinear_constraint, features);
    let model = build_constrained_fixture(
        sources,
        source,
        &text,
        nonlinear_constraint,
        features,
        spans,
    );
    (model, spans.equations.constraint)
}

fn fixture_source_text(nonlinear_constraint: bool, features: FixtureFeatures) -> String {
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
    let discrete_equations = if features.discrete && features.events {
        " b = true;"
    } else if features.discrete {
        " d = 2; b = true;"
    } else {
        ""
    };
    let event_equations = if features.events {
        let discrete_real = if features.discrete { " d = 2;" } else { "" };
        format!(
            " when y > 0 then{discrete_real} reinit(y, 0); assert(y > 0, \"event fired\"); end when; sample(0.5);"
        )
    } else {
        String::new()
    };
    let clock_equations = if features.clocks {
        " Clock(1, 10); previous(d); interval(); terminal();"
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
    let record_declarations = if features.record_companion {
        "record Pair Real left; Real right; end Pair; parameter Real recordCompanion(start = 4, fixed = true) = 2 \"record companion\"; Pair(2, 3); Pair(4, 5); Pair(2, 3).left;"
    } else {
        ""
    };
    let function_declarations = fixture_function_declarations(features);
    let range_declaration = if features.range {
        " parameter Integer r[3] = 1:1:3;"
    } else {
        ""
    };
    let derivative_y_rhs = if features.holonomic { "x" } else { "a" };
    format!(
        "{record_declarations}{function_declarations} parameter Real p;{range_declaration} Real x; Real y; Real a;{family_declaration}{discrete_declaration} equation x = {rhs}; der(y) = {derivative_y_rhs}; der(x) = 1;{family_equation}{discrete_equations}{event_equations}{clock_equations}{delay_equations}"
    )
}

#[derive(Clone, Copy)]
struct FixtureSourceSpans {
    declaration: dae::DaeProvenance,
    equations: FixtureSpans,
    family_owner: Option<dae::DaeProvenance>,
    discrete_owners: Option<(dae::DaeProvenance, dae::DaeProvenance)>,
}

fn fixture_source_spans(
    source: rumoca_core::SourceId,
    text: &str,
    nonlinear_constraint: bool,
    features: FixtureFeatures,
) -> FixtureSourceSpans {
    let rhs = if nonlinear_constraint {
        "sin(y)"
    } else {
        "p*y"
    };
    let derivative_y_rhs = if features.holonomic { "x" } else { "a" };
    let declaration = source_provenance(source, text, "parameter Real p");
    let constraint = source_provenance(source, text, &format!("x = {rhs}"));
    let derivative_y = source_provenance(source, text, &format!("der(y) = {derivative_y_rhs}"));
    let derivative_x = source_provenance(source, text, "der(x) = 1");
    let family_owner = features
        .family
        .then(|| source_provenance(source, text, "for i in 1:2 loop z[i] = i"));
    let discrete_owners = features.discrete.then(|| {
        (
            source_provenance(source, text, "d = 2"),
            source_provenance(source, text, "b = true"),
        )
    });
    FixtureSourceSpans {
        declaration,
        equations: FixtureSpans {
            constraint,
            derivative_y,
            derivative_x,
        },
        family_owner,
        discrete_owners,
    }
}

fn build_constrained_fixture(
    sources: SourceMap,
    source: rumoca_core::SourceId,
    text: &str,
    nonlinear_constraint: bool,
    features: FixtureFeatures,
    spans: FixtureSourceSpans,
) -> dae::Dae {
    dae::Dae::construct(sources, |model| {
        let (real, vector, boolean) = fixture_types(model, spans.declaration)?;
        let record = fixture_record_type(model, real, source, text, features.record_companion)?;
        let domain = fixture_domain(model, spans.family_owner)?;
        let variables =
            fixture_variables(model, real, vector, boolean, spans.declaration, features)?;
        insert_fixture_range_parameter(model, source, text, features.range)?;
        insert_fixture_record_companion(model, real, record, source, text)?;
        insert_fixture_functions(
            model,
            FixtureFunctionConfig {
                real,
                record,
                source,
                text,
                features,
            },
        )?;
        insert_fixture_clock(model, variables, source, text, features.clocks)?;
        insert_fixture_delay(model, variables, source, text, features)?;
        let residuals = model.expressions(|expressions| {
            fixture_residuals(
                expressions,
                variables,
                spans.equations,
                nonlinear_constraint,
                features.holonomic,
            )
        })?;
        let family_residual =
            fixture_family_residual(model, domain, spans.family_owner, variables.z)?;
        let family = domain
            .zip(spans.family_owner)
            .zip(family_residual)
            .map(|((domain, owner), residual)| (domain, owner, residual));
        let discrete_activation =
            insert_fixture_events(model, variables, source, text, features.events)?;
        let discrete =
            fixture_discrete(model, variables, spans.discrete_owners, discrete_activation)?;
        insert_fixture_equations(model, spans.equations, residuals, family, discrete)?;
        Ok(())
    })
    .expect("fixture DAE is valid")
}

fn insert_fixture_range_parameter<'dae>(
    model: &mut dae::DaeConstruction<'dae>,
    source: rumoca_core::SourceId,
    text: &str,
    enabled: bool,
) -> Result<(), dae::DaeConstructionError> {
    if !enabled {
        return Ok(());
    }
    let declaration = source_provenance(source, text, "parameter Integer r[3]");
    let owner = source_provenance(source, text, "1:1:3");
    let start = nested_source_provenance(source, text, "1:1:3", "1", 0);
    let step = nested_source_provenance(source, text, "1:1:3", "1", 1);
    let stop = nested_source_provenance(source, text, "1:1:3", "3", 0);
    let integer_array = model.types(|types| {
        types.derived(
            dae::ValueType::array(dae::ScalarType::Integer, [3]),
            declaration,
        )
    })?;
    let binding = model.expressions(|expressions| {
        let start = expressions.at(start).literal(dae::DaeLiteral::Integer(1))?;
        let step = expressions.at(step).literal(dae::DaeLiteral::Integer(1))?;
        let stop = expressions.at(stop).literal(dae::DaeLiteral::Integer(3))?;
        expressions.at(owner).range(start, Some(step), stop)
    })?;
    model.variables(|variables| {
        variables
            .parameter(
                VarName::new("r"),
                integer_array,
                declaration,
                dae::VariableAttributes {
                    binding: Some(binding),
                    ..dae::VariableAttributes::default()
                },
            )
            .map(|_| ())
    })
}

fn fixture_residuals<'dae>(
    expressions: &mut dae::Expressions<'_, 'dae>,
    variables: FixtureVariables<'dae>,
    spans: FixtureSpans,
    nonlinear_constraint: bool,
    holonomic: bool,
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
    let derivative_value = if holonomic {
        expressions
            .at(spans.derivative_y)
            .coordinate(dae::CoordinateInput::State(variables.x))?
    } else {
        expressions
            .at(spans.derivative_y)
            .coordinate(dae::CoordinateInput::Algebraic(variables.a))?
    };
    let derivative_y = expressions.at(spans.derivative_y).binary(
        dae::BinaryOperator::Subtract,
        y_derivative,
        derivative_value,
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
fn state_demotion_preserves_range_bound_identity_and_provenance() {
    let (model, _) = constrained_state_model(
        false,
        FixtureFeatures {
            range: true,
            ..FixtureFeatures::default()
        },
    );
    let prepared = prepare_for_solve(&model).expect("range companion survives state demotion");
    assert!(matches!(prepared, PreparedDae::Transformed { .. }));

    prepared.as_dae().inspect(|view| {
        let variable = view
            .variables()
            .find(|(_, variable)| variable.name().as_str() == "r")
            .map(|(_, variable)| variable)
            .expect("range parameter survives reconstruction");
        let binding = view.expression(variable.binding().unwrap()).unwrap();
        let dae::ExpressionOperation::Range(range) = binding.operation() else {
            unreachable!("reconstructed parameter retains its checked range")
        };
        let step = range.explicit_step().expect("explicit unit step survives");
        assert_eq!(view.source_text(binding.provenance()), Some("1:1:3"));
        assert_eq!(view.source_text(range.start().provenance()), Some("1"));
        assert_eq!(view.source_text(step.provenance()), Some("1"));
        assert_eq!(view.source_text(range.stop().provenance()), Some("3"));
        assert_ne!(range.start().expression(), step.expression());
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
        assert_eq!(view.discrete_value_owner_count(), 1);
        let real = view
            .discrete_real_equation(0)
            .expect("discrete-real equation survives reconstruction");
        assert_eq!(view.source_text(real.provenance()), Some("d = 2"));
        assert!(matches!(
            real.activation(),
            dae::DiscreteRealActivation::Always
        ));
        let owner = view
            .discrete_value_owner(
                view.discrete_value_owner_id(0)
                    .expect("B.1c owner identity resolves"),
            )
            .expect("B.1c owner survives reconstruction");
        assert_eq!(view.source_text(owner.provenance()), Some("b = true"));
        let target = owner.targets().get(0).expect("B.1c target survives");
        assert!(matches!(
            view.variable(target.into())
                .expect("assignment target resolves")
                .role(),
            dae::VariableRole::DiscreteValue
        ));
        let branch = owner.branches().get(0).expect("B.1c branch survives");
        assert!(matches!(
            branch.activation(),
            dae::DiscreteBranchActivation::Always
        ));
        assert_eq!(view.source_text(branch.provenance()), Some("b = true"));
        assert_eq!(
            view.source_text(branch.values().get(0).unwrap().1),
            Some("b = true")
        );
        assert!(
            sort(view).is_ok(),
            "replacement DAE remains structurally square"
        );
    });
}

#[test]
fn state_demotion_preserves_conditional_discrete_real_activation() {
    let (model, _) = constrained_state_model(
        false,
        FixtureFeatures {
            discrete: true,
            events: true,
            ..FixtureFeatures::default()
        },
    );
    let prepared = prepare_for_solve(&model).expect("conditional B.1b equation survives");
    let transformed = match prepared {
        PreparedDae::Transformed { dae, .. } => dae,
        PreparedDae::Borrowed(_) => panic!("singular fixture requires state demotion"),
    };
    transformed.inspect(|view| {
        let equation = view
            .discrete_real_equation(0)
            .expect("conditional discrete-real equation survives reconstruction");
        let dae::DiscreteRealActivation::When { trigger, guard } = equation.activation() else {
            panic!("conditional B.1b activation is not weakened");
        };
        assert_eq!(trigger, guard);
        assert_eq!(view.source_text(equation.provenance()), Some("d = 2"));
        assert!(view.condition(trigger).is_some());
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
                        dae::CoordinateView::Previous(_)
                            | dae::CoordinateView::ClockInterval(_)
                            | dae::CoordinateView::Terminal(_)
                    )
                )
            })
            .count();
        assert_eq!(temporal_coordinates, 3);
        let interval = (0..view.expression_count())
            .filter_map(|index| view.expression_id(index))
            .filter_map(|id| view.expression(id))
            .find(|expression| {
                matches!(
                    expression.operation(),
                    dae::ExpressionOperation::Coordinate(dae::CoordinateView::ClockInterval(_))
                )
            })
            .expect("periodic interval survives reconstruction");
        assert_eq!(view.source_text(interval.provenance()), Some("interval()"));
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
        let dae::DelayOperation::ParameterDelay { delay_time } = delay.operation() else {
            panic!("fixed delay retains parameter timing evidence");
        };
        assert_eq!(view.source_text(delay_time.provenance()), Some("0.5"));
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
        let dae::DelayOperation::BoundedDelay {
            delay_time,
            delay_max: maximum,
        } = delay.operation()
        else {
            panic!("bounded delay retains maximum capability");
        };
        assert_eq!(maximum.value(), 1.0);
        assert_eq!(view.source_text(maximum.provenance()), Some("1.0"));
        assert_eq!(
            view.source_text(
                view.expression(delay_time)
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
fn state_demotion_preserves_record_layout_values_and_use_sites() {
    let (model, _) = constrained_state_model(
        false,
        FixtureFeatures {
            record_companion: true,
            ..FixtureFeatures::default()
        },
    );
    let prepared = prepare_for_solve(&model).expect("unrelated record companion survives");
    let transformed = match prepared {
        PreparedDae::Transformed { dae, .. } => dae,
        PreparedDae::Borrowed(_) => panic!("singular fixture requires state demotion"),
    };
    transformed.inspect(assert_rebuilt_record_companion);
}

fn assert_rebuilt_record_companion(view: dae::DaeView<'_>) {
    let record_id = (0..view.value_type_count())
        .filter_map(|index| view.value_type_id(index))
        .find(|id| {
            view.value_type(*id)
                .and_then(dae::ValueType::record_name)
                .is_some_and(|name| name.as_str() == "Pair")
        })
        .expect("record type survives");
    let record = view
        .value_type(record_id)
        .expect("record identity resolves");
    assert!(record.is_record());
    assert_eq!(record.record_field_count(), 2);
    assert_eq!(view.record_field(record_id, 0).unwrap().0.as_str(), "left");
    assert_eq!(view.record_field(record_id, 1).unwrap().0.as_str(), "right");
    assert_eq!(
        view.source_text(view.value_type_provenance(record_id).unwrap()),
        Some("record Pair Real left; Real right; end Pair")
    );
    assert!(view.effective_flat_type(record_id).is_none());

    assert_primitive_record_companion_attributes(view);
    assert_record_values(view, record_id);
    assert_record_projections(view);
    assert!(sort(view).is_ok());
}

fn assert_primitive_record_companion_attributes(view: dae::DaeView<'_>) {
    let (_, companion) = view
        .variables()
        .find(|(_, variable)| variable.name().as_str() == "recordCompanion")
        .expect("primitive record companion survives");
    assert_eq!(companion.role(), dae::VariableRole::Parameter);
    assert_eq!(companion.fixed(), Some(true));
    assert_eq!(companion.description(), Some("record companion"));
    assert_eq!(
        view.source_text(companion.declaration()),
        Some("parameter Real recordCompanion(start = 4, fixed = true) = 2")
    );
    for (expression, expected_text, expected_value) in [
        (
            companion.binding().expect("primitive binding survives"),
            "2",
            2.0,
        ),
        (
            companion.start().expect("primitive start survives"),
            "4",
            4.0,
        ),
    ] {
        let expression = view
            .expression(expression)
            .expect("attribute value resolves");
        assert_eq!(
            view.source_text(expression.provenance()),
            Some(expected_text)
        );
        assert!(matches!(
            expression.operation(),
            dae::ExpressionOperation::Literal(dae::DaeLiteral::Real(value))
                if *value == expected_value
        ));
    }
}

fn assert_record_values<'dae>(view: dae::DaeView<'dae>, record_id: dae::ValueTypeId<'dae>) {
    for expected_text in ["Pair(2, 3)", "Pair(4, 5)"] {
        let expression = (0..view.expression_count())
            .filter_map(|index| view.expression_id(index))
            .filter_map(|id| view.expression(id))
            .find(|expression| {
                expression.value_type_id() == record_id
                    && view.source_text(expression.provenance()) == Some(expected_text)
            })
            .expect("record value survives");
        assert!(matches!(
            expression.operation(),
            dae::ExpressionOperation::Record(_)
        ));
    }
}

fn assert_record_projections(view: dae::DaeView<'_>) {
    let projection = (0..view.expression_count())
        .filter_map(|index| view.expression_id(index))
        .filter_map(|id| view.expression(id))
        .find(|expression| {
            expression.provenance().origin() == dae::DaeProvenanceOrigin::Source
                && view.source_text(expression.provenance()) == Some("Pair(2, 3).left")
        })
        .expect("record field projection survives");
    let dae::ExpressionOperation::Field { base, field } = projection.operation() else {
        panic!("record use remains a field projection");
    };
    assert_eq!(field, 0);
    let base = view.expression(base).expect("field base resolves");
    assert_eq!(view.source_text(base.provenance()), Some("Pair(2, 3)"));
    assert!(matches!(
        base.operation(),
        dae::ExpressionOperation::Record(_)
    ));
    let generated = (0..view.expression_count())
        .filter_map(|index| view.expression_id(index))
        .filter_map(|id| view.expression(id))
        .find(|expression| {
            expression.provenance().origin()
                == dae::DaeProvenanceOrigin::Generated(dae::DaeGeneration::RecordEquationProjection)
        })
        .expect("generated record projection survives");
    assert_eq!(
        view.source_text(generated.provenance()),
        Some("Pair(2, 3).left")
    );
    assert!(matches!(
        generated.operation(),
        dae::ExpressionOperation::Field { field: 1, .. }
    ));
}
#[test]
fn continuous_record_unknown_fails_during_checked_construction() {
    let text = "record Pair Real left; end Pair; Pair unresolved;";
    let mut sources = SourceMap::new();
    let source = sources.add("continuous_record.mo", text);
    let real_at = source_provenance(source, text, "Real left");
    let record_at = source_provenance(source, text, "record Pair Real left; end Pair");
    let variable_at = source_provenance(source, text, "Pair unresolved");
    let error = dae::Dae::construct(sources, |model| {
        let real = model
            .types(|types| types.derived(dae::ValueType::scalar(dae::ScalarType::Real), real_at))?;
        let record = model.types(|types| {
            types.record(
                VarName::new("Pair"),
                [(VarName::new("left"), real)],
                record_at,
            )
        })?;
        model.variables(|variables| {
            variables
                .algebraic(
                    VarName::new("unresolved"),
                    record,
                    variable_at,
                    dae::VariableAttributes::default(),
                )
                .map(|_| ())
        })
    })
    .expect_err("record coordinates must be projected before checked DAE construction");
    assert!(matches!(
        &error,
        dae::DaeConstructionError::InvalidVariableType {
            name,
            role: dae::VariableRole::Algebraic,
            found: dae::ScalarType::Record,
            span,
        } if name.as_str() == "unresolved" && *span == variable_at.span()
    ));
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

/// One index-one constraint block: `c = p*d`, `der(d) = e`, `der(c) = rate`.
///
/// The block is singular on its own and becomes regular once `c` is demoted,
/// so repeating it with disjoint variables builds a system that needs exactly
/// that many simultaneous demotions.
fn independent_constraint_residuals<'dae>(
    expressions: &mut dae::Expressions<'_, 'dae>,
    parameter: dae::ParameterId<'dae>,
    constrained: dae::StateId<'dae>,
    driver: dae::StateId<'dae>,
    algebraic: dae::AlgebraicId<'dae>,
    rate: f64,
    spans: [dae::DaeProvenance; 3],
) -> Result<[dae::ExprId<'dae>; 3], dae::DaeConstructionError> {
    let [constraint_at, driver_at, constrained_at] = spans;
    let constrained_value = expressions
        .at(constraint_at)
        .coordinate(dae::CoordinateInput::State(constrained))?;
    let driver_value = expressions
        .at(constraint_at)
        .coordinate(dae::CoordinateInput::State(driver))?;
    let parameter_value = expressions
        .at(constraint_at)
        .coordinate(dae::CoordinateInput::Parameter(parameter))?;
    let scaled = expressions.at(constraint_at).binary(
        dae::BinaryOperator::Multiply,
        parameter_value,
        driver_value,
    )?;
    let constraint = expressions.at(constraint_at).binary(
        dae::BinaryOperator::Subtract,
        constrained_value,
        scaled,
    )?;
    let driver_derivative = expressions
        .at(driver_at)
        .coordinate(dae::CoordinateInput::Derivative(driver))?;
    let algebraic_value = expressions
        .at(driver_at)
        .coordinate(dae::CoordinateInput::Algebraic(algebraic))?;
    let driver_definition = expressions.at(driver_at).binary(
        dae::BinaryOperator::Subtract,
        driver_derivative,
        algebraic_value,
    )?;
    let constrained_derivative = expressions
        .at(constrained_at)
        .coordinate(dae::CoordinateInput::Derivative(constrained))?;
    let rate = expressions
        .at(constrained_at)
        .literal(dae::DaeLiteral::Real(rate))?;
    let constrained_definition = expressions.at(constrained_at).binary(
        dae::BinaryOperator::Subtract,
        constrained_derivative,
        rate,
    )?;
    Ok([constraint, driver_definition, constrained_definition])
}

const INDEPENDENT_CONSTRAINT_TEXT: &str = "parameter Real p; Real x; Real y; Real u; Real v; \
     Real a; Real b; Real spare; equation x = p*y; der(y) = a; der(x) = 1; u = p*v; der(v) = b; \
     der(u) = 2;";

/// Build the two-block fixture, optionally adding an algebraic that no equation
/// determines so that the system stays singular after every demotion.
fn independent_constraint_model(spare_unknown: bool) -> dae::Dae {
    let text = INDEPENDENT_CONSTRAINT_TEXT;
    let mut sources = SourceMap::new();
    let source = sources.add("independent_constraints.mo", text);
    let declaration = source_provenance(source, text, "parameter Real p");
    dae::Dae::construct(sources, |model| {
        let real = model.types(|types| {
            types.intern(
                TypeId::new(0),
                dae::ValueType::scalar(dae::ScalarType::Real),
                declaration,
            )
        })?;
        let declare = |name: &str| source_provenance(source, text, name);
        let variables = model.variables(|variables| {
            let parameter = variables.parameter(
                VarName::new("p"),
                real,
                declaration,
                dae::VariableAttributes::default(),
            )?;
            let mut state = |name: &str| {
                variables.state(
                    VarName::new(name),
                    real,
                    declare(&format!("Real {name}")),
                    dae::VariableAttributes::default(),
                )
            };
            let states = [state("x")?, state("y")?, state("u")?, state("v")?];
            let mut algebraic = |name: &str| {
                variables.algebraic(
                    VarName::new(name),
                    real,
                    declare(&format!("Real {name}")),
                    dae::VariableAttributes::default(),
                )
            };
            let algebraics = [algebraic("a")?, algebraic("b")?];
            if spare_unknown {
                algebraic("spare")?;
            }
            Ok((parameter, states, algebraics))
        })?;
        let (parameter, [x, y, u, v], [a, b]) = variables;
        let first_spans = [
            declare("x = p*y"),
            declare("der(y) = a"),
            declare("der(x) = 1"),
        ];
        let second_spans = [
            declare("u = p*v"),
            declare("der(v) = b"),
            declare("der(u) = 2"),
        ];
        let residuals = model.expressions(|expressions| {
            let first = independent_constraint_residuals(
                expressions,
                parameter,
                x,
                y,
                a,
                1.0,
                first_spans,
            )?;
            let second = independent_constraint_residuals(
                expressions,
                parameter,
                u,
                v,
                b,
                2.0,
                second_spans,
            )?;
            Ok([first, second])
        })?;
        let equations = [first_spans, second_spans]
            .into_iter()
            .zip(residuals)
            .flat_map(|(spans, block)| spans.into_iter().zip(block))
            .collect::<Vec<_>>();
        model.continuous(|continuous| {
            for (provenance, residual) in equations {
                continuous.value_equation(provenance, residual)?;
            }
            Ok(())
        })?;
        Ok(())
    })
    .expect("independent constraint fixture is valid")
}

#[test]
fn accumulated_state_demotions_reduce_independent_constraints_together() {
    let model = independent_constraint_model(false);
    assert!(
        matches!(
            model.inspect(|view| sort(view).map(|_| ())),
            Err(StructuralError::Singular { .. })
        ),
        "fixture is singular before preparation"
    );

    let prepared =
        prepare_for_solve(&model).expect("independent constraints demote to a regular system");
    assert!(matches!(prepared, PreparedDae::Transformed { .. }));
    prepared.as_dae().inspect(|view| {
        assert!(
            sort(view).is_ok(),
            "accumulated replacement DAE has a perfect matching"
        );
        let role = |name: &str| {
            view.variables()
                .find(|(_, variable)| variable.name().as_str() == name)
                .map(|(_, variable)| variable.role())
                .expect("fixture declaration survives reconstruction")
        };
        assert_eq!(role("x"), dae::VariableRole::Algebraic);
        assert_eq!(role("u"), dae::VariableRole::Algebraic);
        assert_eq!(role("y"), dae::VariableRole::State);
        assert_eq!(role("v"), dae::VariableRole::State);
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
                                .is_some_and(|variable| matches!(
                                    variable.name().as_str(),
                                    "x" | "u"
                                ))
                    )
                }),
            "no derivative of a demoted state survives"
        );
        let generated = (0..view.expression_count())
            .filter_map(|index| view.expression_id(index))
            .filter_map(|id| view.expression(id))
            .filter(|expression| {
                expression.provenance().origin()
                    == dae::DaeProvenanceOrigin::Generated(dae::DaeGeneration::IndexReduction)
            })
            .count();
        assert!(
            generated >= 2,
            "each accumulated demotion contributes generated derivative expressions"
        );
    });
}

#[test]
fn partial_demotion_progress_still_reports_the_original_singularity() {
    let model = independent_constraint_model(true);
    let Err(StructuralError::Singular {
        n_equations,
        n_unknowns,
        n_matched,
        ..
    }) = model.inspect(|view| sort(view).map(|_| ()))
    else {
        panic!("fixture with a spare unknown is singular")
    };

    let error = match prepare_for_solve(&model) {
        Ok(_) => panic!("an undetermined unknown must not be reported as prepared"),
        Err(error) => error,
    };
    assert!(
        matches!(
            error,
            StructuralError::Singular {
                n_equations: reported_equations,
                n_unknowns: reported_unknowns,
                n_matched: reported_matched,
                ..
            } if (reported_equations, reported_unknowns, reported_matched)
                == (n_equations, n_unknowns, n_matched)
        ),
        "the original singularity is reported, not the partially demoted one"
    );
}

#[test]
fn source_free_reconstruction_failure_has_no_diagnostic_label() {
    use rumoca_core::PhaseError;

    let error = construction_failure(dae::DaeConstructionError::DuplicateTopology {
        kind: "clock ownership",
        span: None,
    });
    assert!(matches!(
        error,
        StructuralError::UnspannedContractViolation { .. }
    ));
    let diagnostic = error.to_diagnostic();
    assert_eq!(
        diagnostic.code.as_deref(),
        Some(crate::diagnostic_codes::ES014_CONTRACT_VIOLATION)
    );
    assert!(diagnostic.labels.is_empty());
}

#[test]
fn source_bearing_reconstruction_failure_retains_its_exact_span() {
    let span = Span::from_offsets(
        rumoca_core::SourceId::from_source_name("reconstruction_failure.mo"),
        17,
        24,
    );
    let error = construction_failure(dae::DaeConstructionError::ShapeMismatch { span });
    assert!(matches!(
        error,
        StructuralError::ContractViolation {
            span: retained,
            ..
        } if retained == span
    ));
}
