use super::*;

#[test]
fn coupled_b1b_residual_fails_closed_at_equation_provenance() {
    let text = "discrete Real z; discrete Real w; z + w = 1.0; sample(0, 1);";
    let mut sources = SourceMap::new();
    let source = sources.add("coupled-real.mo", text);
    let z_declaration = at(source, text, "discrete Real z");
    let w_declaration = at(source, text, "discrete Real w");
    let equation_at = at(source, text, "z + w = 1.0");
    let clock_at = at(source, text, "sample(0, 1)");
    let model = dae::Dae::construct(sources, |dae| {
        let real = dae.types(|types| {
            types.intern(
                TypeId::new(0),
                dae::ValueType::scalar(dae::ScalarType::Real),
                z_declaration,
            )
        })?;
        let (z, w) = dae.variables(|variables| {
            Ok((
                variables.discrete_real(
                    VarName::new("z"),
                    rumoca_core::InstanceId::new(1),
                    real,
                    z_declaration,
                    dae::VariableAttributes::default(),
                )?,
                variables.discrete_real(
                    VarName::new("w"),
                    rumoca_core::InstanceId::new(2),
                    real,
                    w_declaration,
                    dae::VariableAttributes::default(),
                )?,
            ))
        })?;
        let (lhs, rhs) = dae.expressions(|expressions| {
            let z = expressions
                .at(equation_at)
                .coordinate(dae::CoordinateInput::DiscreteReal(z))?;
            let w = expressions
                .at(equation_at)
                .coordinate(dae::CoordinateInput::DiscreteReal(w))?;
            Ok((
                expressions
                    .at(equation_at)
                    .binary(dae::BinaryOperator::Add, z, w)?,
                expressions
                    .at(equation_at)
                    .literal(dae::DaeLiteral::Real(1.0))?,
            ))
        })?;
        periodic_clock(dae, clock_at)?;
        define_real_equation(dae, equation_at, lhs, rhs)?;
        Ok(())
    })
    .expect("checked coupled B.1b fixture");

    let error = project(&model).expect_err("coupled B.1b is not an assignment");
    assert!(matches!(
        error,
        GalecTargetError::UnsupportedFeature {
            feature,
            span: Some(span),
            ..
        } if feature == "coupled-discrete-real-equation" && span == equation_at.span()
    ));
}

#[test]
fn unclocked_conditional_owner_is_rejected_at_its_source_span() {
    let text = "discrete Boolean m; when a then m = true; end when; sample(0, 1);";
    let mut sources = SourceMap::new();
    let source = sources.add("unclocked.mo", text);
    let declaration = at(source, text, "discrete Boolean m");
    let branch = at(source, text, "when a");
    let action = at(source, text, "m = true");
    let clock_at = at(source, text, "sample(0, 1)");
    let model = dae::Dae::construct(sources, |dae| {
        let boolean = dae.types(|types| {
            types.intern(
                TypeId::new(0),
                dae::ValueType::scalar(dae::ScalarType::Boolean),
                declaration,
            )
        })?;
        let m = dae.variables(|variables| {
            variables.discrete_value(
                VarName::new("m"),
                rumoca_core::InstanceId::new(3),
                boolean,
                declaration,
                dae::VariableAttributes::default(),
            )
        })?;
        let (condition_value, assigned_value) = dae.expressions(|expressions| {
            Ok((
                expressions
                    .at(branch)
                    .literal(dae::DaeLiteral::Boolean(true))?,
                expressions
                    .at(action)
                    .literal(dae::DaeLiteral::Boolean(true))?,
            ))
        })?;
        let condition = dae.conditions(|conditions| {
            let condition = conditions.reserve(branch)?;
            conditions.define(
                condition,
                dae::ConditionInput::Discrete(condition_value),
                branch,
            )?;
            Ok(condition)
        })?;
        periodic_clock(dae, clock_at)?;
        dae.b1c([m], |topology| {
            define_when_owner(topology, m, condition, branch, assigned_value, action)?;
            Ok(())
        })
    })
    .expect("checked unclocked B.1c fixture");

    let error = project(&model).expect_err("unclocked condition is outside DoStep");
    assert!(matches!(
        error,
        GalecTargetError::UnsupportedFeature {
            feature,
            span: Some(span),
            ..
        } if feature == "runtime-event-trigger" && span == branch.span()
    ));
}

/// Count `MultiAssignment` call statements naming `function`, at any nesting
/// depth, so a guarded projection counts the same as a bare one.
pub(super) fn multi_assignment_call_sites(
    statements: &[gast::Spanned<gast::Statement>],
    function: &str,
) -> usize {
    statements
        .iter()
        .map(|statement| match &statement.node {
            gast::Statement::MultiAssignment { call, .. } => {
                usize::from(call.function.lexeme() == function)
            }
            gast::Statement::If(branching) => {
                branching
                    .branches
                    .iter()
                    .map(|branch| multi_assignment_call_sites(&branch.body, function))
                    .sum::<usize>()
                    + branching
                        .else_body
                        .as_ref()
                        .map_or(0, |body| multi_assignment_call_sites(body, function))
            }
            _ => 0,
        })
        .sum()
}

pub(super) fn define_scalar_pair<'dae>(
    dae: &mut dae::DaeConstruction<'dae>,
    real: dae::ValueTypeId<'dae>,
    provenance: dae::DaeProvenance,
) -> Result<dae::FunctionId<'dae>, dae::DaeConstructionError> {
    dae.function(
        dae::FunctionSignature::new(VarName::new("pair"), [real], [real, real], provenance),
        |dae, reservation| {
            let input = dae.functions(|functions| {
                functions.parameter(&reservation, VarName::new("input"), 0, provenance)
            })?;
            let first = dae.functions(|functions| {
                functions.output(&reservation, VarName::new("first"), 0, provenance)
            })?;
            let second = dae.functions(|functions| {
                functions.output(&reservation, VarName::new("second"), 1, provenance)
            })?;
            let input = dae
                .expressions(|expressions| expressions.at(provenance).function_parameter(input))?;
            let two = dae.expressions(|expressions| {
                expressions
                    .at(provenance)
                    .literal(dae::DaeLiteral::Real(2.0))
            })?;
            let doubled = dae.expressions(|expressions| {
                expressions
                    .at(provenance)
                    .binary(dae::BinaryOperator::Multiply, input, two)
            })?;
            let mut body = dae.functions(|functions| functions.begin(reservation, provenance))?;
            dae.functions(|functions| {
                functions.assign(&mut body, first, input, provenance)?;
                functions.assign(&mut body, second, doubled, provenance)?;
                functions.define(body, provenance)
            })
        },
    )
    .map(|(function, _)| function)
}

fn multi_output_clocked_fixture() -> dae::Dae {
    let text = "discrete Real a, b, c; when sample(0, 1) then c = 1.0; (a, b) = pair(c); end when;";
    let mut sources = SourceMap::new();
    let source = sources.add("multi-output-clocked.mo", text);
    let declaration = at(source, text, "discrete Real a, b, c");
    let clock_at = at(source, text, "sample(0, 1)");
    let seed_assignment = at(source, text, "c = 1.0");
    let assignment = at(source, text, "(a, b) = pair(c)");
    dae::Dae::construct(sources, |dae| {
        let real = dae.types(|types| {
            types.intern(
                TypeId::new(0),
                dae::ValueType::scalar(dae::ScalarType::Real),
                declaration,
            )
        })?;
        let pair = define_scalar_pair(dae, real, declaration)?;
        let a = dae.variables(|variables| {
            variables.discrete_real(
                VarName::new("a"),
                rumoca_core::InstanceId::new(4),
                real,
                declaration,
                dae::VariableAttributes::default(),
            )
        })?;
        let b = dae.variables(|variables| {
            variables.discrete_real(
                VarName::new("b"),
                rumoca_core::InstanceId::new(5),
                real,
                declaration,
                dae::VariableAttributes::default(),
            )
        })?;
        let c = dae.variables(|variables| {
            variables.discrete_real(
                VarName::new("c"),
                rumoca_core::InstanceId::new(6),
                real,
                declaration,
                dae::VariableAttributes::default(),
            )
        })?;
        let argument = dae.expressions(|expressions| {
            expressions
                .at(assignment)
                .coordinate(dae::CoordinateInput::DiscreteReal(c))
        })?;
        let results = dae.expressions(|expressions| {
            expressions
                .at(assignment)
                .call_results(pair, [0, 1], [argument])
        })?;
        let (a_ref, b_ref) = dae.expressions(|expressions| {
            Ok((
                expressions
                    .at(assignment)
                    .coordinate(dae::CoordinateInput::DiscreteReal(a))?,
                expressions
                    .at(assignment)
                    .coordinate(dae::CoordinateInput::DiscreteReal(b))?,
            ))
        })?;
        let (c_ref, seed) = dae.expressions(|expressions| {
            Ok((
                expressions
                    .at(seed_assignment)
                    .coordinate(dae::CoordinateInput::DiscreteReal(c))?,
                expressions
                    .at(seed_assignment)
                    .literal(dae::DaeLiteral::Real(1.0))?,
            ))
        })?;
        let clock = periodic_clock(dae, clock_at)?;
        dae.clocks(|clocks| {
            clocks.own_discrete_real(clock, a, declaration)?;
            clocks.own_discrete_real(clock, b, declaration)?;
            clocks.own_discrete_real(clock, c, declaration)?;
            Ok(())
        })?;
        let tick = dae.conditions(|conditions| {
            let tick = conditions.reserve(clock_at)?;
            conditions.define(tick, dae::ConditionInput::Clock(clock), clock_at)?;
            Ok(tick)
        })?;
        define_when_real_equation(dae, tick, tick, seed_assignment, c_ref, seed)?;
        define_when_real_equation(dae, tick, tick, assignment, a_ref, results[0])?;
        define_when_real_equation(dae, tick, tick, assignment, b_ref, results[1])?;
        Ok(())
    })
    .expect("checked multi-output clocked fixture")
}

/// DAE-C21: the two result projections of one issued call owner are one
/// invocation. They must reach GALEC as one call binding both results, not as
/// one call per consumed output.
///
/// The call deliberately reads `c`, a discrete target this same `DoStep`
/// writes. That makes it ineligible for the domain-entry preamble hoist (a
/// hoisted call would read the previous tick's `c`), so the only thing that can
/// collapse the two projections is grouping them into one emission group. A
/// fixture whose call reads nothing would be hoisted instead and would stay
/// green with grouping disabled.
#[test]
fn two_projections_of_one_call_owner_emit_one_clocked_call() {
    let model = multi_output_clocked_fixture();
    let lowered = project_all(&model).expect("multi-output clocked definitions project");
    assert_eq!(
        multi_assignment_call_sites(&lowered.statements, "pair"),
        1,
        "two projections of one issued call owner must emit one call"
    );
    assert_eq!(
        CommittedCallActionLedger::occurrence_count(&lowered.call_actions),
        1,
        "memo-hit projections must not record additional committed evaluations"
    );
}

#[test]
fn a_duplicate_committed_aggregate_action_in_one_group_is_rejected() {
    let model = single_result_clocked_fixture(false, false, false, false);
    let call_span = first_call_span(&model);
    let mut lowered = project_all(&model).expect("aggregate call projects once");
    assert_eq!(
        CommittedCallActionLedger::occurrence_count(&lowered.call_actions),
        1
    );
    CommittedCallActionLedger::duplicate_first(&mut lowered.call_actions);
    let error = model.inspect(|view| {
        CommittedCallActionLedger::construct(view, lowered.call_actions)
            .expect_err("one owner cannot commit twice in one emission group")
    });
    assert!(matches!(
        error,
        GalecTargetError::UnsupportedFeature {
            feature,
            span: Some(span),
            ..
        } if feature == "repeated-call-owner" && span == call_span
    ));
}

#[test]
fn an_omitted_reached_call_action_is_rejected() {
    let model = single_result_clocked_fixture(false, false, false, false);
    let call_span = first_call_span(&model);
    let mut lowered = project_all(&model).expect("aggregate call projects once");
    CommittedCallActionLedger::omit_first(&mut lowered.call_actions);
    let error = model.inspect(|view| {
        CommittedCallActionLedger::construct(view, lowered.call_actions)
            .expect_err("a reached source call must retain its emitted action")
    });
    assert!(matches!(
        error,
        GalecTargetError::UnsupportedFeature {
            feature,
            span: Some(span),
            ..
        } if feature == "missing-call-owner" && span == call_span
    ));
}

#[test]
fn a_call_action_moved_to_another_emission_transaction_is_rejected() {
    let model = single_result_clocked_fixture(false, false, false, false);
    let call_span = first_call_span(&model);
    let mut lowered = project_all(&model).expect("aggregate call projects once");
    CommittedCallActionLedger::move_first_to_another_group(&mut lowered.call_actions);
    let error = model.inspect(|view| {
        CommittedCallActionLedger::construct(view, lowered.call_actions)
            .expect_err("a call receipt cannot discharge another transaction's reach")
    });
    assert!(matches!(
        error,
        GalecTargetError::UnsupportedFeature {
            feature,
            span: Some(span),
            ..
        } if feature == "foreign-call-owner" && span == call_span
    ));
}

/// Every state assignment written by `statements`, as
/// `(target name, subscript count, source name)`.
///
/// `source name` is the referenced name for a bare reference and `<computed>`
/// otherwise, which is all these assertions need to tell one storage object
/// from another.
fn state_assignment_shapes(
    statements: &[gast::Spanned<gast::Statement>],
) -> Vec<(String, usize, String)> {
    let mut shapes = Vec::new();
    for statement in statements {
        match &statement.node {
            gast::Statement::Assignment {
                target: gast::Reference::State(parts),
                value,
            } => {
                let part = parts.first().expect("checked state reference is nonempty");
                let source = match value {
                    gast::Expression::Ref(gast::Reference::Local(source)) => {
                        source.name.lexeme().to_owned()
                    }
                    gast::Expression::Ref(gast::Reference::State(source)) => source
                        .first()
                        .expect("checked state source is nonempty")
                        .name
                        .lexeme()
                        .to_owned(),
                    _ => "<computed>".to_owned(),
                };
                shapes.push((part.name.lexeme().to_owned(), part.subscripts.len(), source));
            }
            gast::Statement::If(branching) => {
                for branch in &branching.branches {
                    shapes.extend(state_assignment_shapes(&branch.body));
                }
                if let Some(body) = &branching.else_body {
                    shapes.extend(state_assignment_shapes(body));
                }
            }
            _ => {}
        }
    }
    shapes
}

fn shapes_for<'a>(
    shapes: &'a [(String, usize, String)],
    target: &str,
) -> Vec<&'a (String, usize, String)> {
    shapes.iter().filter(|shape| shape.0 == target).collect()
}

#[derive(Clone, Copy)]
struct WholeArraySpans {
    parameter_declaration: dae::DaeProvenance,
    declaration: dae::DaeProvenance,
    clock: dae::DaeProvenance,
    seed: dae::DaeProvenance,
    pair: dae::DaeProvenance,
    echo: dae::DaeProvenance,
    alias: dae::DaeProvenance,
    widened: dae::DaeProvenance,
}

#[derive(Clone, Copy)]
struct WholeArrayVariables<'dae> {
    seed: dae::DiscreteRealId<'dae>,
    a: dae::DiscreteRealId<'dae>,
    b: dae::DiscreteRealId<'dae>,
    echo: dae::DiscreteRealId<'dae>,
    alias: dae::DiscreteRealId<'dae>,
    widened: dae::DiscreteRealId<'dae>,
    counts: dae::ParameterId<'dae>,
}

#[derive(Clone, Copy)]
struct WholeArrayExpressions<'dae> {
    seed_ref: dae::ExprId<'dae>,
    seed_value: dae::ExprId<'dae>,
    a_ref: dae::ExprId<'dae>,
    a_value: dae::ExprId<'dae>,
    b_ref: dae::ExprId<'dae>,
    b_value: dae::ExprId<'dae>,
    echo_ref: dae::ExprId<'dae>,
    echo_value: dae::ExprId<'dae>,
    alias_ref: dae::ExprId<'dae>,
    alias_value: dae::ExprId<'dae>,
    widened_ref: dae::ExprId<'dae>,
    widened_value: dae::ExprId<'dae>,
}

fn define_array_pair<'dae>(
    dae: &mut dae::DaeConstruction<'dae>,
    vector: dae::ValueTypeId<'dae>,
    provenance: dae::DaeProvenance,
) -> Result<dae::FunctionId<'dae>, dae::DaeConstructionError> {
    dae.function(
        dae::FunctionSignature::new(VarName::new("pair"), [vector], [vector, vector], provenance),
        |dae, reservation| {
            let input = dae.functions(|functions| {
                functions.parameter(&reservation, VarName::new("u"), 0, provenance)
            })?;
            let alpha = dae.functions(|functions| {
                functions.output(&reservation, VarName::new("alpha"), 0, provenance)
            })?;
            let beta = dae.functions(|functions| {
                functions.output(&reservation, VarName::new("beta"), 1, provenance)
            })?;
            let input = dae
                .expressions(|expressions| expressions.at(provenance).function_parameter(input))?;
            let mut body = dae.functions(|functions| functions.begin(reservation, provenance))?;
            dae.functions(|functions| {
                functions.assign(&mut body, alpha, input, provenance)?;
                functions.assign(&mut body, beta, input, provenance)?;
                functions.define(body, provenance)
            })
        },
    )
    .map(|(function, _)| function)
}

fn define_whole_array_variables<'dae>(
    dae: &mut dae::DaeConstruction<'dae>,
    vector: dae::ValueTypeId<'dae>,
    spans: WholeArraySpans,
) -> Result<WholeArrayVariables<'dae>, dae::DaeConstructionError> {
    let mut discrete_real = |name, source_occurrence| {
        dae.variables(|variables| {
            variables.discrete_real(
                VarName::new(name),
                source_occurrence,
                vector,
                spans.declaration,
                dae::VariableAttributes::default(),
            )
        })
    };
    let seed = discrete_real("seed", rumoca_core::InstanceId::new(1001))?;
    let a = discrete_real("a", rumoca_core::InstanceId::new(1002))?;
    let b = discrete_real("b", rumoca_core::InstanceId::new(1003))?;
    let echo = discrete_real("echo", rumoca_core::InstanceId::new(1004))?;
    let alias = discrete_real("alias", rumoca_core::InstanceId::new(1005))?;
    let widened = discrete_real("widened", rumoca_core::InstanceId::new(1006))?;
    let integer_vector = dae.types(|types| {
        types.intern(
            TypeId::new(1),
            dae::ValueType::array(dae::ScalarType::Integer, [2]),
            spans.parameter_declaration,
        )
    })?;
    let counts = dae.variables(|variables| {
        variables.parameter(
            VarName::new("counts"),
            rumoca_core::InstanceId::new(1007),
            integer_vector,
            spans.parameter_declaration,
            dae::VariableAttributes::default(),
        )
    })?;
    Ok(WholeArrayVariables {
        seed,
        a,
        b,
        echo,
        alias,
        widened,
        counts,
    })
}

fn permuted_echo_value<'dae>(
    dae: &mut dae::DaeConstruction<'dae>,
    variable: dae::DiscreteRealId<'dae>,
    provenance: dae::DaeProvenance,
) -> Result<dae::ExprId<'dae>, dae::DaeConstructionError> {
    dae.expressions(|expressions| {
        let read = expressions
            .at(provenance)
            .coordinate(dae::CoordinateInput::DiscreteReal(variable))?;
        let second = expressions
            .at(provenance)
            .literal(dae::DaeLiteral::Integer(2))?;
        let first = expressions
            .at(provenance)
            .literal(dae::DaeLiteral::Integer(1))?;
        let high = expressions.at(provenance).index(
            read,
            [dae::Subscript::Index {
                expression: second,
                provenance,
            }],
        )?;
        let low = expressions.at(provenance).index(
            read,
            [dae::Subscript::Index {
                expression: first,
                provenance,
            }],
        )?;
        expressions.at(provenance).array([high, low])
    })
}

fn define_whole_array_expressions<'dae>(
    dae: &mut dae::DaeConstruction<'dae>,
    pair: dae::FunctionId<'dae>,
    variables: WholeArrayVariables<'dae>,
    spans: WholeArraySpans,
) -> Result<WholeArrayExpressions<'dae>, dae::DaeConstructionError> {
    let seed_read = dae.expressions(|expressions| {
        expressions
            .at(spans.pair)
            .coordinate(dae::CoordinateInput::DiscreteReal(variables.seed))
    })?;
    let results = dae.expressions(|expressions| {
        expressions
            .at(spans.pair)
            .call_results(pair, [0, 1], [seed_read])
    })?;
    let mut discrete_ref = |variable, provenance| {
        dae.expressions(|expressions| {
            expressions
                .at(provenance)
                .coordinate(dae::CoordinateInput::DiscreteReal(variable))
        })
    };
    let seed_ref = discrete_ref(variables.seed, spans.seed)?;
    let a_ref = discrete_ref(variables.a, spans.pair)?;
    let b_ref = discrete_ref(variables.b, spans.pair)?;
    let echo_ref = discrete_ref(variables.echo, spans.echo)?;
    let alias_ref = discrete_ref(variables.alias, spans.alias)?;
    let alias_value = discrete_ref(variables.a, spans.alias)?;
    let widened_ref = discrete_ref(variables.widened, spans.widened)?;
    let widened_value = dae.expressions(|expressions| {
        expressions
            .at(spans.widened)
            .coordinate(dae::CoordinateInput::Parameter(variables.counts))
    })?;
    let seed_value = dae.expressions(|expressions| {
        let one = expressions
            .at(spans.seed)
            .literal(dae::DaeLiteral::Real(1.0))?;
        let two = expressions
            .at(spans.seed)
            .literal(dae::DaeLiteral::Real(2.0))?;
        expressions.at(spans.seed).array([one, two])
    })?;
    Ok(WholeArrayExpressions {
        seed_ref,
        seed_value,
        a_ref,
        a_value: results[0],
        b_ref,
        b_value: results[1],
        echo_ref,
        echo_value: permuted_echo_value(dae, variables.b, spans.echo)?,
        alias_ref,
        alias_value,
        widened_ref,
        widened_value,
    })
}

fn define_whole_array_clock<'dae>(
    dae: &mut dae::DaeConstruction<'dae>,
    variables: WholeArrayVariables<'dae>,
    expressions: WholeArrayExpressions<'dae>,
    spans: WholeArraySpans,
) -> Result<(), dae::DaeConstructionError> {
    let clock = periodic_clock(dae, spans.clock)?;
    dae.clocks(|clocks| {
        for variable in [
            variables.seed,
            variables.a,
            variables.b,
            variables.echo,
            variables.alias,
            variables.widened,
        ] {
            clocks.own_discrete_real(clock, variable, spans.declaration)?;
        }
        Ok(())
    })?;
    let tick = dae.conditions(|conditions| {
        let tick = conditions.reserve(spans.clock)?;
        conditions.define(tick, dae::ConditionInput::Clock(clock), spans.clock)?;
        Ok(tick)
    })?;
    for (provenance, lhs, rhs) in [
        (spans.seed, expressions.seed_ref, expressions.seed_value),
        (spans.pair, expressions.a_ref, expressions.a_value),
        (spans.pair, expressions.b_ref, expressions.b_value),
        (spans.echo, expressions.echo_ref, expressions.echo_value),
        (spans.alias, expressions.alias_ref, expressions.alias_value),
        (
            spans.widened,
            expressions.widened_ref,
            expressions.widened_value,
        ),
    ] {
        define_when_real_equation(dae, tick, tick, provenance, lhs, rhs)?;
    }
    Ok(())
}

/// A clocked definition whose value already denotes one whole array must reach
/// GALEC as ONE array assignment; a definition that only looks array-shaped
/// must keep one assignment per coordinate.
///
/// The fixture is deliberately not the easy case:
///
/// * `a` and `b` are two array results of ONE issued call, so an emitter that
///   collapsed a definition onto "the call's result" without selecting the
///   right output still emits two whole-array assignments and still compiles —
///   the assertion that `a` reads the `alpha` temporary and `b` the `beta` one
///   is what separates "compiles" from "computes the right thing".
/// * `echo` is `{b[2], b[1]}`: the same extent, the same element type and one
///   single source object, exactly like a collapsible copy, but a PERMUTED
///   correspondence. It must stay at one assignment per coordinate.
/// * `seed` is an array constructor of literals — complete and in order, but
///   not a reference to storage, so there is nothing to copy from.
fn whole_array_clocked_fixture() -> dae::Dae {
    let text = "parameter Integer counts[2]; discrete Real seed[2], a[2], b[2], \
                echo[2], alias[2], widened[2]; when sample(0, 1) then \
                seed = {1.0, 2.0}; (a, b) = pair(seed); echo = {b[2], b[1]}; alias = a; \
                widened = counts; end when;";
    let mut sources = SourceMap::new();
    let source = sources.add("whole-array-clocked.mo", text);
    let spans = WholeArraySpans {
        parameter_declaration: at(source, text, "parameter Integer counts[2]"),
        declaration: at(source, text, "discrete Real seed[2], a[2], b[2]"),
        clock: at(source, text, "sample(0, 1)"),
        seed: at(source, text, "seed = {1.0, 2.0}"),
        pair: at(source, text, "(a, b) = pair(seed)"),
        echo: at(source, text, "echo = {b[2], b[1]}"),
        alias: at(source, text, "alias = a"),
        widened: at(source, text, "widened = counts"),
    };
    dae::Dae::construct(sources, |dae| {
        let vector = dae.types(|types| {
            types.intern(
                TypeId::new(0),
                dae::ValueType::array(dae::ScalarType::Real, [2]),
                spans.declaration,
            )
        })?;
        let pair = define_array_pair(dae, vector, spans.declaration)?;
        let variables = define_whole_array_variables(dae, vector, spans)?;
        let expressions = define_whole_array_expressions(dae, pair, variables, spans)?;
        define_whole_array_clock(dae, variables, expressions, spans)
    })
    .expect("checked whole-array clocked fixture")
}

fn assert_whole_array_assignment_shapes(
    statements: &[gast::Spanned<gast::Statement>],
    shapes: &[(String, usize, String)],
) {
    let a_shapes = shapes_for(shapes, "a");
    let b_shapes = shapes_for(shapes, "b");
    assert_eq!(
        a_shapes.len(),
        1,
        "`a` must be one whole-array copy: {shapes:?}"
    );
    assert_eq!(
        b_shapes.len(),
        1,
        "`b` must be one whole-array copy: {shapes:?}"
    );
    assert_eq!(
        a_shapes[0].1, 0,
        "a whole-array target carries no subscript"
    );
    assert_eq!(
        b_shapes[0].1, 0,
        "a whole-array target carries no subscript"
    );
    assert!(
        a_shapes[0].2.contains("alpha") && !a_shapes[0].2.contains("beta"),
        "`a` must copy the `alpha` result temporary: {shapes:?}"
    );
    assert!(
        b_shapes[0].2.contains("beta") && !b_shapes[0].2.contains("alpha"),
        "`b` must copy the `beta` result temporary: {shapes:?}"
    );
    assert_eq!(
        multi_assignment_call_sites(statements, "pair"),
        1,
        "collapsing to whole-array copies must not duplicate the call"
    );

    let echo_shapes = shapes_for(shapes, "echo");
    assert_eq!(
        echo_shapes.len(),
        2,
        "a permuted definition keeps one assignment per coordinate: {shapes:?}"
    );
    assert!(
        echo_shapes.iter().all(|shape| shape.1 == 1),
        "each permuted coordinate is subscripted: {shapes:?}"
    );
    let seed_shapes = shapes_for(shapes, "seed");
    assert_eq!(
        seed_shapes.len(),
        2,
        "an array constructor keeps one assignment per coordinate: {shapes:?}"
    );
    let alias_shapes = shapes_for(shapes, "alias");
    assert_eq!(
        alias_shapes.len(),
        1,
        "`alias` must be one whole-array copy: {shapes:?}"
    );
    assert_eq!(
        alias_shapes[0].1, 0,
        "a whole-array target has no subscript"
    );
    assert_eq!(
        alias_shapes[0].2, "a",
        "`alias` must copy the whole `a` storage: {shapes:?}"
    );
    let widened_shapes = shapes_for(shapes, "widened");
    assert_eq!(
        widened_shapes.len(),
        2,
        "a widening copy keeps one assignment per coordinate: {shapes:?}"
    );
    assert!(
        widened_shapes
            .iter()
            .all(|shape| shape.1 == 1 && shape.2 == "<computed>"),
        "each widened coordinate is a subscripted conversion: {shapes:?}"
    );
}

#[test]
fn whole_array_clocked_definitions_collapse_only_under_proven_correspondence() {
    let model = whole_array_clocked_fixture();
    let statements = project(&model).expect("whole-array clocked definitions project");
    let shapes = state_assignment_shapes(&statements);
    assert_whole_array_assignment_shapes(&statements, &shapes);
}

pub(super) fn define_array_copy<'dae>(
    dae: &mut dae::DaeConstruction<'dae>,
    vector: dae::ValueTypeId<'dae>,
    provenance: dae::DaeProvenance,
) -> Result<dae::FunctionId<'dae>, dae::DaeConstructionError> {
    dae.function(
        dae::FunctionSignature::new(VarName::new("copy"), [vector], [vector], provenance),
        |dae, reservation| {
            let input = dae.functions(|functions| {
                functions.parameter(&reservation, VarName::new("u"), 0, provenance)
            })?;
            let output = dae.functions(|functions| {
                functions.output(&reservation, VarName::new("y"), 0, provenance)
            })?;
            let input = dae
                .expressions(|expressions| expressions.at(provenance).function_parameter(input))?;
            let mut body = dae.functions(|functions| functions.begin(reservation, provenance))?;
            dae.functions(|functions| {
                functions.assign(&mut body, output, input, provenance)?;
                functions.define(body, provenance)
            })
        },
    )
    .map(|(function, _)| function)
}

#[derive(Clone, Copy)]
struct SingleResultConfig {
    shared_result: bool,
    runtime_guard: bool,
    split_clock: bool,
}

#[derive(Clone, Copy)]
struct SingleResultSpans {
    declaration: dae::DaeProvenance,
    clock: dae::DaeProvenance,
    second_clock: Option<dae::DaeProvenance>,
    seed: dae::DaeProvenance,
    call: dae::DaeProvenance,
    second: dae::DaeProvenance,
    output: Option<dae::DaeProvenance>,
}

struct SingleResultVariables<'dae> {
    seed: dae::DiscreteRealId<'dae>,
    a: dae::DiscreteRealId<'dae>,
    b: dae::DiscreteRealId<'dae>,
    output: Option<dae::AlgebraicId<'dae>>,
    copy: dae::FunctionId<'dae>,
}

struct SingleResultExpressions<'dae> {
    seed_ref: dae::ExprId<'dae>,
    a_ref: dae::ExprId<'dae>,
    b_ref: dae::ExprId<'dae>,
    output_ref: Option<dae::ExprId<'dae>>,
    seed_value: dae::ExprId<'dae>,
    call: dae::ExprId<'dae>,
    runtime_predicate: Option<dae::ExprId<'dae>>,
}

fn declare_single_result_variables<'dae>(
    model: &mut dae::DaeConstruction<'dae>,
    spans: SingleResultSpans,
) -> Result<SingleResultVariables<'dae>, dae::DaeConstructionError> {
    let vector = model.types(|types| {
        types.intern(
            TypeId::new(0),
            dae::ValueType::array(dae::ScalarType::Real, [2]),
            spans.declaration,
        )
    })?;
    let real = model.types(|types| {
        types.intern(
            TypeId::new(1),
            dae::ValueType::scalar(dae::ScalarType::Real),
            spans.declaration,
        )
    })?;
    let copy = define_array_copy(model, vector, spans.declaration)?;
    let mut discrete_real = |name, source_occurrence| {
        model.variables(|variables| {
            variables.discrete_real(
                VarName::new(name),
                source_occurrence,
                vector,
                spans.declaration,
                dae::VariableAttributes::default(),
            )
        })
    };
    let seed = discrete_real("seed", rumoca_core::InstanceId::new(1101))?;
    let a = discrete_real("a", rumoca_core::InstanceId::new(1102))?;
    let b = discrete_real("b", rumoca_core::InstanceId::new(1103))?;
    let output = spans
        .output
        .map(|provenance| {
            model.variables(|variables| {
                variables.output(
                    VarName::new("y"),
                    rumoca_core::InstanceId::new(1104),
                    real,
                    provenance,
                    dae::VariableAttributes {
                        causality: dae::VariableCausality::Output,
                        ..Default::default()
                    },
                )
            })
        })
        .transpose()?;
    Ok(SingleResultVariables {
        seed,
        a,
        b,
        output,
        copy,
    })
}

fn construct_single_result_expressions<'dae>(
    model: &mut dae::DaeConstruction<'dae>,
    config: SingleResultConfig,
    spans: SingleResultSpans,
    variables: &SingleResultVariables<'dae>,
) -> Result<SingleResultExpressions<'dae>, dae::DaeConstructionError> {
    model.expressions(|expressions| {
        let seed_ref = expressions
            .at(spans.seed)
            .coordinate(dae::CoordinateInput::DiscreteReal(variables.seed))?;
        let a_ref = expressions
            .at(spans.call)
            .coordinate(dae::CoordinateInput::DiscreteReal(variables.a))?;
        let b_ref = expressions
            .at(spans.second)
            .coordinate(dae::CoordinateInput::DiscreteReal(variables.b))?;
        let output_ref = variables
            .output
            .map(|output| {
                expressions
                    .at(spans.output.expect("causal output has provenance"))
                    .coordinate(dae::CoordinateInput::Algebraic(output))
            })
            .transpose()?;
        let one = expressions
            .at(spans.seed)
            .literal(dae::DaeLiteral::Real(1.0))?;
        let two = expressions
            .at(spans.seed)
            .literal(dae::DaeLiteral::Real(2.0))?;
        let seed_value = expressions.at(spans.seed).array([one, two])?;
        let call_argument = if config.split_clock {
            seed_value
        } else {
            seed_ref
        };
        let call = expressions
            .at(spans.call)
            .call(variables.copy, 0, [call_argument])?;
        let runtime_predicate = config
            .runtime_guard
            .then(|| {
                expressions
                    .at(spans.call)
                    .literal(dae::DaeLiteral::Boolean(false))
            })
            .transpose()?;
        Ok(SingleResultExpressions {
            seed_ref,
            a_ref,
            b_ref,
            output_ref,
            seed_value,
            call,
            runtime_predicate,
        })
    })
}

fn construct_single_result_clocks<'dae>(
    model: &mut dae::DaeConstruction<'dae>,
    spans: SingleResultSpans,
    variables: &SingleResultVariables<'dae>,
    runtime_predicate: Option<dae::ExprId<'dae>>,
) -> Result<
    (
        dae::ConditionId<'dae>,
        dae::ConditionId<'dae>,
        Option<dae::ConditionId<'dae>>,
    ),
    dae::DaeConstructionError,
> {
    let clock = periodic_clock(model, spans.clock)?;
    let second_clock = spans
        .second_clock
        .map(|provenance| {
            model
                .clocks(|clocks| {
                    clocks.periodic(
                        ClockLattice::new(
                            ClockRational::new(2, 1).expect("fixture period is exact"),
                            ClockRational::ZERO,
                        )
                        .expect("fixture lattice is valid"),
                        provenance,
                    )
                })
                .map(Into::into)
        })
        .transpose()?;
    model.clocks(|clocks| {
        clocks.own_discrete_real(clock, variables.seed, spans.declaration)?;
        clocks.own_discrete_real(clock, variables.a, spans.declaration)?;
        clocks.own_discrete_real(
            second_clock.unwrap_or(clock),
            variables.b,
            spans.declaration,
        )?;
        Ok(())
    })?;
    model.conditions(|conditions| {
        let tick = conditions.reserve(spans.clock)?;
        conditions.define(tick, dae::ConditionInput::Clock(clock), spans.clock)?;
        let second_tick = second_clock
            .map(|clock| {
                let provenance = spans.second_clock.expect("second clock has provenance");
                let tick = conditions.reserve(provenance)?;
                conditions.define(tick, dae::ConditionInput::Clock(clock), provenance)?;
                Ok::<_, dae::DaeConstructionError>(tick)
            })
            .transpose()?;
        let guarded_tick = guard_clock_tick(conditions, tick, runtime_predicate, spans.call)?;
        Ok((tick, guarded_tick, second_tick))
    })
}

fn guard_clock_tick<'dae>(
    conditions: &mut dae::Conditions<'_, 'dae>,
    tick: dae::ConditionId<'dae>,
    predicate: Option<dae::ExprId<'dae>>,
    provenance: dae::DaeProvenance,
) -> Result<dae::ConditionId<'dae>, dae::DaeConstructionError> {
    let Some(predicate) = predicate else {
        return Ok(tick);
    };
    let predicate_condition = conditions.reserve(provenance)?;
    conditions.define(
        predicate_condition,
        dae::ConditionInput::Discrete(predicate),
        provenance,
    )?;
    let guarded = conditions.reserve(provenance)?;
    conditions.define(
        guarded,
        dae::ConditionInput::And(tick, predicate_condition),
        provenance,
    )?;
    Ok(guarded)
}

fn add_single_result_causal_output<'dae>(
    model: &mut dae::DaeConstruction<'dae>,
    spans: SingleResultSpans,
    output: Option<dae::ExprId<'dae>>,
    call: dae::ExprId<'dae>,
) -> Result<(), dae::DaeConstructionError> {
    let Some(output) = output else {
        return Ok(());
    };
    let provenance = spans.output.expect("causal output has provenance");
    let residual = model.expressions(|expressions| {
        let one = expressions
            .at(provenance)
            .literal(dae::DaeLiteral::Integer(1))?;
        let selected = expressions.at(provenance).index(
            call,
            [dae::Subscript::Index {
                expression: one,
                provenance,
            }],
        )?;
        expressions
            .at(provenance)
            .binary(dae::BinaryOperator::Subtract, output, selected)
    })?;
    model.continuous(|continuous| {
        continuous.equation(provenance, |equation| equation.residual(residual))?;
        Ok(())
    })
}

fn construct_single_result_model<'dae>(
    model: &mut dae::DaeConstruction<'dae>,
    config: SingleResultConfig,
    spans: SingleResultSpans,
) -> Result<(), dae::DaeConstructionError> {
    let variables = declare_single_result_variables(model, spans)?;
    let expressions = construct_single_result_expressions(model, config, spans, &variables)?;
    let (tick, guarded_tick, second_tick) =
        construct_single_result_clocks(model, spans, &variables, expressions.runtime_predicate)?;
    define_when_real_equation(
        model,
        tick,
        tick,
        spans.seed,
        expressions.seed_ref,
        expressions.seed_value,
    )?;
    define_when_real_equation(
        model,
        tick,
        guarded_tick,
        spans.call,
        expressions.a_ref,
        expressions.call,
    )?;
    if config.shared_result {
        let b_tick = second_tick.unwrap_or(guarded_tick);
        define_when_real_equation(
            model,
            b_tick,
            b_tick,
            spans.second,
            expressions.b_ref,
            expressions.call,
        )?;
    }
    add_single_result_causal_output(model, spans, expressions.output_ref, expressions.call)
}

fn single_result_clocked_fixture(
    shared_result: bool,
    runtime_guard: bool,
    causal_output: bool,
    split_clock: bool,
) -> dae::Dae {
    let text = if split_clock {
        assert!(shared_result && !runtime_guard && !causal_output);
        "discrete Real seed[2], a[2], b[2]; when sample(0, 1) then \
         seed = {1.0, 2.0}; a = copy(seed); end when; when sample(0, 2) then \
         b = copy(seed); end when;"
    } else {
        match (runtime_guard, causal_output) {
            (true, false) => {
                "discrete Real seed[2], a[2], b[2]; when sample(0, 1) and false then \
             seed = {1.0, 2.0}; a = copy(seed); b = copy(seed); end when;"
            }
            (false, true) => {
                "discrete Real seed[2], a[2], b[2]; output Real y; \
             equation y = copy(seed)[1]; when sample(0, 1) then seed = {1.0, 2.0}; \
             a = copy(seed); b = copy(seed); end when;"
            }
            (false, false) => {
                "discrete Real seed[2], a[2], b[2]; when sample(0, 1) then \
             seed = {1.0, 2.0}; a = copy(seed); b = copy(seed); end when;"
            }
            (true, true) => unreachable!("fixture needs only one competing use"),
        }
    };
    let mut sources = SourceMap::new();
    let source = sources.add("single-result-clocked.mo", text);
    let spans = SingleResultSpans {
        declaration: at(source, text, "discrete Real seed[2], a[2], b[2]"),
        clock: at(source, text, "sample(0, 1)"),
        second_clock: split_clock.then(|| at(source, text, "sample(0, 2)")),
        seed: at(source, text, "seed = {1.0, 2.0}"),
        call: at(source, text, "a = copy(seed)"),
        second: at(source, text, "b = copy(seed)"),
        output: causal_output.then(|| at(source, text, "y = copy(seed)[1]")),
    };
    let config = SingleResultConfig {
        shared_result,
        runtime_guard,
        split_clock,
    };
    dae::Dae::construct(sources, |model| {
        construct_single_result_model(model, config, spans)
    })
    .expect("checked single-result clocked fixture")
}

fn named_call_targets(
    statements: &[gast::Spanned<gast::Statement>],
    function: &str,
    depth: usize,
    targets: &mut Vec<(usize, Vec<gast::Reference>)>,
) {
    for statement in statements {
        match &statement.node {
            gast::Statement::MultiAssignment {
                targets: found,
                call,
            } if call.function.lexeme() == function => targets.push((depth, found.clone())),
            gast::Statement::If(branching) => {
                for branch in &branching.branches {
                    named_call_targets(&branch.body, function, depth + 1, targets);
                }
                if let Some(body) = &branching.else_body {
                    named_call_targets(body, function, depth + 1, targets);
                }
            }
            _ => {}
        }
    }
}

fn first_call_span(model: &dae::Dae) -> Span {
    model.inspect(|view| {
        (0..view.expression_count())
            .filter_map(|index| view.expression_id(index))
            .filter_map(|id| view.expression(id))
            .find_map(|expression| {
                matches!(
                    expression.operation(),
                    dae::ExpressionOperation::Call { .. }
                )
                .then(|| expression.provenance().span())
            })
            .expect("fixture has one issued call owner")
    })
}

#[test]
fn one_schedule_use_materializes_an_aggregate_call_before_the_state_copy() {
    let model = single_result_clocked_fixture(false, false, false, false);
    let lowered = project_all(&model).expect("exclusive aggregate call projects");
    let mut calls = Vec::new();
    named_call_targets(&lowered.statements, "copy", 0, &mut calls);
    assert!(matches!(
        calls.as_slice(),
        [(0, targets)] if matches!(targets.as_slice(), [gast::Reference::Local(_)])
    ));
    let shapes = state_assignment_shapes(&lowered.statements);
    assert!(matches!(
        shapes_for(&shapes, "a").as_slice(),
        [(_, 0, source)] if source.contains("call")
    ));
    assert!(
        lowered
            .locals
            .iter()
            .any(|local| local.name.lexeme().contains("call")),
        "without a replay-derived exclusivity claim the checked call result owns a local"
    );
}

#[test]
fn two_state_consumers_of_one_call_owner_keep_one_materialization() {
    let model = single_result_clocked_fixture(true, false, false, false);
    let lowered = project_all(&model).expect("shared aggregate call projects");
    let mut calls = Vec::new();
    named_call_targets(&lowered.statements, "copy", 0, &mut calls);
    assert_eq!(calls.len(), 1, "one issued call owner must execute once");
    assert!(matches!(calls[0].1.as_slice(), [gast::Reference::Local(_)]));
    let shapes = state_assignment_shapes(&lowered.statements);
    assert_eq!(shapes_for(&shapes, "a").len(), 1);
    assert_eq!(shapes_for(&shapes, "b").len(), 1);
}

#[test]
fn runtime_guarded_aggregate_call_keeps_its_lazy_materialization() {
    let model = single_result_clocked_fixture(false, true, false, false);
    let lowered = project_all(&model).expect("guarded aggregate call projects");
    let mut calls = Vec::new();
    named_call_targets(&lowered.statements, "copy", 0, &mut calls);
    assert_eq!(calls.len(), 1, "the guarded call still executes once");
    assert!(
        calls[0].0 > 0 && matches!(calls[0].1.as_slice(), [gast::Reference::Local(_)]),
        "a runtime-guarded call cannot claim an eager State destination"
    );
}

#[test]
fn identical_runtime_guards_share_one_lazy_call_owner() {
    let model = single_result_clocked_fixture(true, true, false, false);
    let lowered = project_all(&model).expect("identically guarded consumers form one owner group");
    let mut calls = Vec::new();
    named_call_targets(&lowered.statements, "copy", 0, &mut calls);
    assert_eq!(calls.len(), 1, "the guarded source call executes once");
    assert!(
        calls[0].0 > 0,
        "the shared call must remain inside its exact runtime guard"
    );
    let shapes = state_assignment_shapes(&lowered.statements);
    assert_eq!(shapes_for(&shapes, "a").len(), 1);
    assert_eq!(shapes_for(&shapes, "b").len(), 1);
}

#[test]
fn causal_output_reuses_the_clock_scheduled_call_result() {
    let model = single_result_clocked_fixture(false, false, true, false);
    validate_all_call_actions(&model).expect("causal store consumes the retained clocked result");
    rumoca_core::with_target_invocation_brand(|brand| {
        let product = crate::lower_to_algorithm_code(
            brand,
            &crate::GalecInput::new(&model, "SharedCausalOutput"),
            &crate::GalecOptions::new(
                rumoca_ir_galec::package::AlgorithmCodeArithmeticProfile::construct(
                    rumoca_ir_galec::package::AlgorithmCodeRealFormat::Binary64,
                    rumoca_ir_galec::package::AlgorithmCodeIntegerFormat::I32,
                    rumoca_core::RealMatrixMultiplySemantics::SeparateMulAddAscendingPositiveZero,
                ),
            ),
        )
        .expect("the public package preserves one call owner");
        let mut calls = Vec::new();
        named_call_targets(
            &product.package().block().do_step.statements,
            "copy",
            0,
            &mut calls,
        );
        assert_eq!(
            calls.len(),
            1,
            "clocked and causal consumers share one call"
        );
    });
}

#[test]
fn one_call_owner_cannot_be_reexecuted_in_two_clock_domains() {
    let model = single_result_clocked_fixture(true, false, false, true);
    let errors = rumoca_core::with_target_invocation_brand(|brand| {
        crate::lower_to_algorithm_code(
            brand,
            &crate::GalecInput::new(&model, "CoincidentSharedCall"),
            &crate::GalecOptions::new(
                rumoca_ir_galec::package::AlgorithmCodeArithmeticProfile::construct(
                    rumoca_ir_galec::package::AlgorithmCodeRealFormat::Binary64,
                    rumoca_ir_galec::package::AlgorithmCodeIntegerFormat::I32,
                    rumoca_core::RealMatrixMultiplySemantics::SeparateMulAddAscendingPositiveZero,
                ),
            ),
        )
        .expect_err("a slower guarded domain has no shared activation capability")
    });
    assert!(matches!(
        errors.as_slice(),
        [GalecTargetError::UnsupportedFeature { feature, .. }]
            if feature == "cross-region-call-owner"
    ));
}
