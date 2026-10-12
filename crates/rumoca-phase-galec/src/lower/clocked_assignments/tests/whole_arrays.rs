use super::*;

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
    let mut discrete_real = |name| {
        dae.variables(|variables| {
            variables.discrete_real(
                VarName::new(name),
                vector,
                spans.declaration,
                dae::VariableAttributes::default(),
            )
        })
    };
    let seed = discrete_real("seed")?;
    let a = discrete_real("a")?;
    let b = discrete_real("b")?;
    let echo = discrete_real("echo")?;
    let alias = discrete_real("alias")?;
    let widened = discrete_real("widened")?;
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
