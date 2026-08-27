use rumoca_core::{SourceMap, Span, VarName};

use crate::lower::*;

fn indexed_real_update<'dae>(
    dae: &mut dae::DaeConstruction<'dae>,
    base: dae::ExprId<'dae>,
    index: i64,
    provenance: dae::DaeProvenance,
) -> Result<dae::ExprId<'dae>, dae::DaeConstructionError> {
    let (subscript, value) = dae.expressions(|expressions| {
        Ok((
            expressions
                .at(provenance)
                .literal(dae::DaeLiteral::Integer(index))?,
            expressions
                .at(provenance)
                .literal(dae::DaeLiteral::Real(index as f64))?,
        ))
    })?;
    dae.expressions(|expressions| {
        expressions.at(provenance).array_update(
            base,
            value,
            [dae::Subscript::Index {
                expression: subscript,
                provenance,
            }],
        )
    })
}

fn real_literal<'dae>(
    dae: &mut dae::DaeConstruction<'dae>,
    value: f64,
    provenance: dae::DaeProvenance,
) -> Result<dae::ExprId<'dae>, dae::DaeConstructionError> {
    dae.expressions(|expressions| {
        expressions
            .at(provenance)
            .literal(dae::DaeLiteral::Real(value))
    })
}

fn integer_literal<'dae>(
    dae: &mut dae::DaeConstruction<'dae>,
    value: i64,
    provenance: dae::DaeProvenance,
) -> Result<dae::ExprId<'dae>, dae::DaeConstructionError> {
    dae.expressions(|expressions| {
        expressions
            .at(provenance)
            .literal(dae::DaeLiteral::Integer(value))
    })
}

fn assign_function_value<'dae>(
    dae: &mut dae::DaeConstruction<'dae>,
    body: &mut dae::FunctionBody<'dae>,
    output: dae::FunctionValueId<'dae>,
    value: dae::ExprId<'dae>,
    provenance: dae::DaeProvenance,
) -> Result<(), dae::DaeConstructionError> {
    dae.functions(|functions| functions.assign(body, output, value, provenance))
}

fn read_function_value<'dae>(
    dae: &mut dae::DaeConstruction<'dae>,
    body: &dae::FunctionBody<'dae>,
    output: dae::FunctionValueId<'dae>,
    provenance: dae::DaeProvenance,
) -> Result<dae::ExprId<'dae>, dae::DaeConstructionError> {
    dae.functions(|functions| functions.read(body, output, provenance))
}

fn define_array_update_function<'dae>(
    dae: &mut dae::DaeConstruction<'dae>,
    name: &'static str,
    vector: dae::ValueTypeId<'dae>,
    provenance: dae::DaeProvenance,
    nested_updates: bool,
) -> Result<(), dae::DaeConstructionError> {
    let _ = dae.function(
        dae::FunctionSignature::new(VarName::new(name), [], [vector], provenance),
        |dae, reservation| {
            let output = dae.functions(|functions| {
                functions.output(&reservation, VarName::new("q"), 0, provenance)
            })?;
            let mut body = dae.functions(|functions| functions.begin(reservation, provenance))?;
            let zero = dae.expressions(|expressions| {
                expressions
                    .at(provenance)
                    .literal(dae::DaeLiteral::Real(0.0))
            })?;
            let zeros = dae.expressions(|expressions| {
                expressions.at(provenance).array([zero, zero, zero, zero])
            })?;
            assign_function_value(dae, &mut body, output, zeros, provenance)?;

            let mut update = read_function_value(dae, &body, output, provenance)?;
            for index in 1..=4 {
                update = indexed_real_update(dae, update, index, provenance)?;
                if !nested_updates {
                    assign_function_value(dae, &mut body, output, update, provenance)?;
                    update = read_function_value(dae, &body, output, provenance)?;
                }
            }
            if nested_updates {
                assign_function_value(dae, &mut body, output, update, provenance)?;
            }
            dae.functions(|functions| functions.define(body, provenance))
        },
    )?;
    Ok(())
}

fn define_fresh_array_update_function<'dae>(
    dae: &mut dae::DaeConstruction<'dae>,
    vector: dae::ValueTypeId<'dae>,
    provenance: dae::DaeProvenance,
) -> Result<(), dae::DaeConstructionError> {
    let _ = dae.function(
        dae::FunctionSignature::new(VarName::new("fresh"), [], [vector], provenance),
        |dae, reservation| {
            let output = dae.functions(|functions| {
                functions.output(&reservation, VarName::new("q"), 0, provenance)
            })?;
            let mut body = dae.functions(|functions| functions.begin(reservation, provenance))?;
            let values = dae.expressions(|expressions| {
                [30.0, 40.0, 50.0, 60.0]
                    .into_iter()
                    .map(|value| {
                        expressions
                            .at(provenance)
                            .literal(dae::DaeLiteral::Real(value))
                    })
                    .collect::<Result<Vec<_>, _>>()
            })?;
            let base = dae.expressions(|expressions| expressions.at(provenance).array(values))?;
            let update = indexed_real_update(dae, base, 1, provenance)?;
            assign_function_value(dae, &mut body, output, update, provenance)?;
            dae.functions(|functions| functions.define(body, provenance))
        },
    )?;
    Ok(())
}

fn define_reaching_definition_update_function<'dae>(
    dae: &mut dae::DaeConstruction<'dae>,
    vector: dae::ValueTypeId<'dae>,
    provenance: dae::DaeProvenance,
) -> Result<(), dae::DaeConstructionError> {
    let _ = dae.function(
        dae::FunctionSignature::new(VarName::new("dependent"), [], [vector], provenance),
        |dae, reservation| {
            let output = dae.functions(|functions| {
                functions.output(&reservation, VarName::new("q"), 0, provenance)
            })?;
            let mut body = dae.functions(|functions| functions.begin(reservation, provenance))?;
            let values = dae.expressions(|expressions| {
                [30.0, 40.0, 50.0, 60.0]
                    .into_iter()
                    .map(|value| {
                        expressions
                            .at(provenance)
                            .literal(dae::DaeLiteral::Real(value))
                    })
                    .collect::<Result<Vec<_>, _>>()
            })?;
            let initial =
                dae.expressions(|expressions| expressions.at(provenance).array(values))?;
            assign_function_value(dae, &mut body, output, initial, provenance)?;
            let base = read_function_value(dae, &body, output, provenance)?;
            let first = indexed_real_update(dae, base, 1, provenance)?;
            let one = dae.expressions(|expressions| {
                expressions
                    .at(provenance)
                    .literal(dae::DaeLiteral::Integer(1))
            })?;
            let prior_first = dae.expressions(|expressions| {
                expressions.at(provenance).index(
                    base,
                    [dae::Subscript::Index {
                        expression: one,
                        provenance,
                    }],
                )
            })?;
            let second = indexed_update_with_value(dae, first, prior_first, 2, provenance)?;
            assign_function_value(dae, &mut body, output, second, provenance)?;
            dae.functions(|functions| functions.define(body, provenance))
        },
    )?;
    Ok(())
}

fn define_conditional_array_update_function<'dae>(
    dae: &mut dae::DaeConstruction<'dae>,
    vector: dae::ValueTypeId<'dae>,
    predicate: dae::FunctionId<'dae>,
    provenance: dae::DaeProvenance,
) -> Result<(), dae::DaeConstructionError> {
    let _ = dae.function(
        dae::FunctionSignature::new(VarName::new("conditional"), [], [vector], provenance),
        |dae, reservation| {
            let output = dae.functions(|functions| {
                functions.output(&reservation, VarName::new("q"), 0, provenance)
            })?;
            let mut body = dae.functions(|functions| functions.begin(reservation, provenance))?;
            let zero = dae.expressions(|expressions| {
                expressions
                    .at(provenance)
                    .literal(dae::DaeLiteral::Real(0.0))
            })?;
            let zeros = dae.expressions(|expressions| {
                expressions.at(provenance).array([zero, zero, zero, zero])
            })?;
            assign_function_value(dae, &mut body, output, zeros, provenance)?;
            let base = read_function_value(dae, &body, output, provenance)?;
            let first = indexed_real_update(dae, base, 1, provenance)?;
            let condition =
                dae.expressions(|expressions| expressions.at(provenance).call(predicate, 0, []))?;
            let conditional = dae.expressions(|expressions| {
                expressions
                    .at(provenance)
                    .conditional([(condition, first)], base)
            })?;
            let second = indexed_real_update(dae, conditional, 2, provenance)?;
            assign_function_value(dae, &mut body, output, second, provenance)?;
            dae.functions(|functions| functions.define(body, provenance))
        },
    )?;
    Ok(())
}

fn define_true_predicate<'dae>(
    dae: &mut dae::DaeConstruction<'dae>,
    boolean: dae::ValueTypeId<'dae>,
    provenance: dae::DaeProvenance,
) -> Result<dae::FunctionId<'dae>, dae::DaeConstructionError> {
    let (predicate, ()) = dae.function(
        dae::FunctionSignature::new(VarName::new("predicate"), [], [boolean], provenance),
        |dae, reservation| {
            let output = dae.functions(|functions| {
                functions.output(&reservation, VarName::new("value"), 0, provenance)
            })?;
            let mut body = dae.functions(|functions| functions.begin(reservation, provenance))?;
            let value = dae.expressions(|expressions| {
                expressions
                    .at(provenance)
                    .literal(dae::DaeLiteral::Boolean(true))
            })?;
            assign_function_value(dae, &mut body, output, value, provenance)?;
            dae.functions(|functions| functions.define(body, provenance))
        },
    )?;
    Ok(predicate)
}

fn define_identity_function<'dae>(
    dae: &mut dae::DaeConstruction<'dae>,
    real: dae::ValueTypeId<'dae>,
    provenance: dae::DaeProvenance,
) -> Result<dae::FunctionId<'dae>, dae::DaeConstructionError> {
    let (function, ()) = dae.function(
        dae::FunctionSignature::new(VarName::new("identity"), [real], [real], provenance),
        |dae, reservation| {
            let input = dae.functions(|functions| {
                functions.parameter(&reservation, VarName::new("u"), 0, provenance)
            })?;
            let output = dae.functions(|functions| {
                functions.output(&reservation, VarName::new("y"), 0, provenance)
            })?;
            let mut body = dae.functions(|functions| functions.begin(reservation, provenance))?;
            let value = dae
                .expressions(|expressions| expressions.at(provenance).function_parameter(input))?;
            assign_function_value(dae, &mut body, output, value, provenance)?;
            dae.functions(|functions| functions.define(body, provenance))
        },
    )?;
    Ok(function)
}

fn indexed_update_with_value<'dae>(
    dae: &mut dae::DaeConstruction<'dae>,
    base: dae::ExprId<'dae>,
    value: dae::ExprId<'dae>,
    index: i64,
    provenance: dae::DaeProvenance,
) -> Result<dae::ExprId<'dae>, dae::DaeConstructionError> {
    let subscript = dae.expressions(|expressions| {
        expressions
            .at(provenance)
            .literal(dae::DaeLiteral::Integer(index))
    })?;
    dae.expressions(|expressions| {
        expressions.at(provenance).array_update(
            base,
            value,
            [dae::Subscript::Index {
                expression: subscript,
                provenance,
            }],
        )
    })
}

fn collect_indexed_q_assignments(
    statements: &[gast::Spanned<gast::Statement>],
    indices: &mut Vec<i64>,
) {
    for statement in statements {
        match &statement.node {
            gast::Statement::Assignment {
                target: gast::Reference::Local(target),
                ..
            } => {
                if let ("q", [gast::Expression::Integer(index)]) =
                    (target.name.lexeme(), target.subscripts.as_slice())
                {
                    indices.push(*index);
                }
            }
            gast::Statement::If(value) => {
                for branch in &value.branches {
                    collect_indexed_q_assignments(&branch.body, indices);
                }
                if let Some(else_body) = &value.else_body {
                    collect_indexed_q_assignments(else_body, indices);
                }
            }
            _ => {}
        }
    }
}

fn indexed_q_assignments(statements: &[gast::Spanned<gast::Statement>]) -> Vec<i64> {
    let mut indices = Vec::new();
    collect_indexed_q_assignments(statements, &mut indices);
    indices
}

/// The one whole-target loop a diverted chain lowers to: `for i in 1:extent`
/// writing `q[i]` as its last body statement.
///
/// The write is checked at the END of the body rather than as the whole body,
/// because a diverted chain may have to materialize per-element work — a
/// guarded call, a nested selection — ahead of the write it feeds.
fn is_whole_target_loop(statement: Option<&gast::Spanned<gast::Statement>>, extent: i64) -> bool {
    let Some(gast::Spanned {
        node: gast::Statement::For(loop_),
        ..
    }) = statement
    else {
        return false;
    };
    matches!(loop_.stop, gast::Expression::Integer(stop) if stop == extent)
        && matches!(
            loop_.body.last().map(|statement| &statement.node),
            Some(gast::Statement::Assignment {
                target: gast::Reference::Local(target),
                ..
            }) if target.name.lexeme() == "q"
        )
}

/// The four update-chain shapes whose lowering is settled, checked through the
/// element writes each one leaves behind.
///
/// A chain of `ArrayUpdate` nodes is peeled whole and replayed as an in-place
/// element sequence only when the root it peels down to is the target's own
/// value or a generated aggregate seed; any other root is a real whole-value
/// assignment that the element writes cannot stand in for, so the chain is
/// diverted to the aggregate path and lowers as one loop over the whole target.
///
/// * `nested` — four writes stacked into ONE definition. Every one must
///   survive; replaying only the outermost is the erasure the chain-peel fix
///   removed.
/// * `sequential` — the same four writes as four definitions. Same result, so
///   the peel does not depend on how the definitions are cut.
/// * `conditional` — a write stacked on a conditional's join value. The join
///   is neither the target's value nor a seed, so the chain diverts: no
///   in-place writes, one whole-target loop.
/// * `fresh` — a write stacked on a fresh array literal. Emitting the write
///   alone would drop the literal, so this diverts too.
#[test]
fn function_array_updates_preserve_nested_writes_without_replaying_prior_definitions() {
    let mut sources = SourceMap::new();
    let source = sources.add("nested-array-updates.mo", "nested and sequential updates");
    let provenance = dae::DaeProvenance::source(Span::from_offsets(source, 0, 29)).unwrap();
    let model = dae::Dae::construct(sources, |dae| {
        let vector = dae.types(|types| {
            types.derived(
                dae::ValueType::array(dae::ScalarType::Real, [4]),
                provenance,
            )
        })?;
        let boolean = dae.types(|types| {
            types.derived(dae::ValueType::scalar(dae::ScalarType::Boolean), provenance)
        })?;
        define_array_update_function(dae, "nested", vector, provenance, true)?;
        define_array_update_function(dae, "sequential", vector, provenance, false)?;
        let predicate = define_true_predicate(dae, boolean, provenance)?;
        define_conditional_array_update_function(dae, vector, predicate, provenance)?;
        define_fresh_array_update_function(dae, vector, provenance)?;
        define_reaching_definition_update_function(dae, vector, provenance)
    })
    .unwrap();

    model.inspect(|view| {
        // Function 2 is the `predicate` helper and function 5 is the
        // reaching-definition shape, which has its own test below.
        for function_index in [0, 1, 3, 4] {
            let function = view
                .function(view.function_id(function_index).unwrap())
                .unwrap();
            let definitions = rumoca_phase_structural::CausalDefinitions::derive(view);
            let lowered = user_functions::lower_reachable(
                view,
                &definitions,
                HashSet::from([function.id().index()]),
                EmissionPolicy::reviewable(),
            )
            .unwrap();
            let name = function.name().as_str();
            let diverted = name == "conditional" || name == "fresh";
            let expected: &[i64] = if diverted { &[] } else { &[1, 2, 3, 4] };
            let function_statements = &lowered
                .iter()
                .find(|lowered| lowered.name.lexeme() == name)
                .unwrap()
                .statements;
            assert_eq!(
                indexed_q_assignments(function_statements),
                expected,
                "{name}"
            );
            if diverted {
                assert!(
                    is_whole_target_loop(function_statements.last(), 4),
                    "`{name}` peels down to a root the element writes may not \
                     stand in for, so it must lower as one complete \
                     four-element loop that materializes root and writes \
                     together"
                );
            }
        }
    });
}

/// A write whose VALUE reads the chain's root aggregate must not be replayed
/// against storage an earlier write of the same chain has already changed.
///
/// `dependent` builds `q := {30, 40, 50, 60}` and then, in ONE definition,
/// `q[1] := 1.0` followed by `q[2] := <the root's element 1>`. The root is the
/// target's own value, so the root guard alone admits an in-place replay — and
/// the `q[2] := q[1]` that replay emits reads 1.0, where the DAE value it came
/// from names the root's element 1, which is 30.0.
///
/// Proving the ROOT is a value the element sequence may run against is not the
/// same as proving no write VALUE reads that root where storage no longer holds
/// it. The second question is now asked too: element 1 is written before it is
/// read, so the chain is handed to the aggregate path, which materializes the
/// root separately from the updates and lowers as one whole-target loop.
///
/// The shape is not reachable from Modelica as far as probing has established —
/// the natural spelling (`t := y[1]; y[1] := u; y[2] := t;`) keeps `t` as its
/// own function local, so the value read is the local and not the aggregate.
/// It is asserted here as correctness rather than as a recorded gap because the
/// divert path it now takes is executed and proven, so the assertion says what
/// the lowering must do and not merely what it happens to do.
#[test]
fn a_write_reading_the_chain_root_diverts_instead_of_replaying_in_place() {
    let mut sources = SourceMap::new();
    let source = sources.add("reaching-definition-update.mo", "root-reading update");
    let provenance = dae::DaeProvenance::source(Span::from_offsets(source, 0, 19)).unwrap();
    let model = dae::Dae::construct(sources, |dae| {
        let vector = dae.types(|types| {
            types.derived(
                dae::ValueType::array(dae::ScalarType::Real, [4]),
                provenance,
            )
        })?;
        define_reaching_definition_update_function(dae, vector, provenance)
    })
    .unwrap();

    model.inspect(|view| {
        let function = view.function(view.function_id(0).unwrap()).unwrap();
        let definitions = rumoca_phase_structural::CausalDefinitions::derive(view);
        let lowered = user_functions::lower_reachable(
            view,
            &definitions,
            HashSet::from([function.id().index()]),
            EmissionPolicy::reviewable(),
        )
        .unwrap();
        let statements = &lowered
            .iter()
            .find(|lowered| lowered.name.lexeme() == "dependent")
            .unwrap()
            .statements;
        assert_eq!(
            indexed_q_assignments(statements),
            [] as [i64; 0],
            "the second write reads element 1 after the first write stored it, \
             so no part of the chain may replay in place:\n{statements:#?}"
        );
        assert!(
            is_whole_target_loop(statements.last(), 4),
            "the diverted chain materializes root and updates together, as one \
             complete four-element loop:\n{statements:#?}"
        );
    });
}

/// Count the materialized calls a statement tree reaches without passing
/// through a conditional branch. A loop does not guard anything, so it is
/// walked at the same guard depth as its enclosing body.
fn count_calls(statements: &[gast::Spanned<gast::Statement>], guarded: bool, want: bool) -> usize {
    statements
        .iter()
        .map(|statement| match &statement.node {
            gast::Statement::MultiAssignment { .. } => usize::from(guarded == want),
            gast::Statement::For(loop_statement) => {
                count_calls(&loop_statement.body, guarded, want)
            }
            gast::Statement::If(conditional) => {
                conditional
                    .branches
                    .iter()
                    .map(|branch| count_calls(&branch.body, true, want))
                    .sum::<usize>()
                    + conditional
                        .else_body
                        .as_ref()
                        .map_or(0, |body| count_calls(body, true, want))
            }
            _ => 0,
        })
        .sum()
}

fn unguarded_calls(statements: &[gast::Spanned<gast::Statement>]) -> usize {
    count_calls(statements, false, false)
}

fn guarded_calls(statements: &[gast::Spanned<gast::Statement>]) -> usize {
    count_calls(statements, false, true)
}

/// Project one element out of `call` at the already-lowered `expression` ordinal.
fn index_element<'dae>(
    expressions: &mut dae::Expressions<'_, 'dae>,
    call: dae::ExprId<'dae>,
    expression: dae::ExprId<'dae>,
    provenance: dae::DaeProvenance,
) -> Result<dae::ExprId<'dae>, dae::DaeConstructionError> {
    expressions.at(provenance).index(
        call,
        [dae::Subscript::Index {
            expression,
            provenance,
        }],
    )
}

/// One vector-valued call feeding three scalar projections, used to check the
/// call is materialized once rather than per projection.
fn aggregate_call_fixture() -> dae::Dae {
    let mut sources = SourceMap::new();
    let source = sources.add(
        "aggregate-call.mo",
        "one vector call feeds three projections",
    );
    let provenance = dae::DaeProvenance::source(Span::from_offsets(source, 0, 38)).unwrap();
    dae::Dae::construct(sources, |dae| {
        let (vector3, vector5) = dae.types(|types| {
            Ok((
                types.derived(
                    dae::ValueType::array(dae::ScalarType::Real, [3]),
                    provenance,
                )?,
                types.derived(
                    dae::ValueType::array(dae::ScalarType::Real, [5]),
                    provenance,
                )?,
            ))
        })?;
        let (producer, ()) = dae.function(
            dae::FunctionSignature::new(VarName::new("producer"), [], [vector3], provenance),
            |dae, reservation| {
                let output = dae.functions(|functions| {
                    functions.output(&reservation, VarName::new("value"), 0, provenance)
                })?;
                let mut body =
                    dae.functions(|functions| functions.begin(reservation, provenance))?;
                let mut values = Vec::new();
                for value in [1.0, 2.0, 3.0] {
                    values.push(real_literal(dae, value, provenance)?);
                }
                let value =
                    dae.expressions(|expressions| expressions.at(provenance).array(values))?;
                assign_function_value(dae, &mut body, output, value, provenance)?;
                dae.functions(|functions| functions.define(body, provenance))
            },
        )?;
        dae.function(
            dae::FunctionSignature::new(VarName::new("consumer"), [], [vector5], provenance),
            |dae, reservation| {
                let output = dae.functions(|functions| {
                    functions.output(&reservation, VarName::new("result"), 0, provenance)
                })?;
                let mut body =
                    dae.functions(|functions| functions.begin(reservation, provenance))?;
                let call = dae
                    .expressions(|expressions| expressions.at(provenance).call(producer, 0, []))?;
                let mut ordinals = Vec::new();
                for index in 1..=3 {
                    ordinals.push(integer_literal(dae, index, provenance)?);
                }
                let projected = dae.expressions(|expressions| {
                    ordinals
                        .into_iter()
                        .map(|expression| index_element(expressions, call, expression, provenance))
                        .collect::<Result<Vec<_>, _>>()
                })?;
                let zero = dae.expressions(|expressions| {
                    expressions
                        .at(provenance)
                        .literal(dae::DaeLiteral::Real(0.0))
                })?;
                let result = dae.expressions(|expressions| {
                    expressions.at(provenance).array(
                        [zero, zero]
                            .into_iter()
                            .chain(projected)
                            .collect::<Vec<_>>(),
                    )
                })?;
                assign_function_value(dae, &mut body, output, result, provenance)?;
                dae.functions(|functions| functions.define(body, provenance))
            },
        )?;
        Ok(())
    })
    .unwrap()
}

#[test]
fn aggregate_call_is_materialized_once_before_scalar_projection() {
    let model = aggregate_call_fixture();

    model.inspect(|view| {
        let consumer = view.function(view.function_id(1).unwrap()).unwrap();
        let definitions = rumoca_phase_structural::CausalDefinitions::derive(view);
        let lowered = user_functions::lower_reachable(
            view,
            &definitions,
            HashSet::from([consumer.id().index()]),
            EmissionPolicy::reviewable(),
        )
        .expect("aggregate projections should lower from one materialized call");
        let statements = &lowered
            .iter()
            .find(|function| function.name.lexeme() == "consumer")
            .unwrap()
            .statements;
        assert_eq!(
            count_calls(statements, false, false),
            1,
            "one eager aggregate call must precede the scalar projection loop: {statements:#?}"
        );
        assert!(matches!(
            statements.first().map(|statement| &statement.node),
            Some(gast::Statement::MultiAssignment { .. })
        ));
    });
}

/// A call materialized under a runtime guard stays under it when the chain
/// that reads it diverts to the aggregate path.
///
/// `guardedUpdate` calls `identity` once and uses that one value both inside a
/// conditional branch and in a write stacked on the join. The chain peels down
/// to the join, which is neither the target's value nor a seed, so it diverts:
/// the whole conditional becomes a per-element selection inside one aggregate
/// loop rather than a statement-level `if`. The call is then materialized
/// inside the selection arms that use it — which is the property that matters,
/// because a materialization hoisted out of its arm would execute on a path
/// the conditional did not select. Note the cost this shape carries: the
/// materialization is per element, so the call is emitted once per arm rather
/// than once for the function.
#[test]
fn a_diverted_conditional_update_keeps_each_materialized_call_inside_its_guard() {
    let mut sources = SourceMap::new();
    let source = sources.add(
        "conditional-update-call.mo",
        "branch-local call and outer update",
    );
    let provenance = dae::DaeProvenance::source(Span::from_offsets(source, 0, 34)).unwrap();
    let model = dae::Dae::construct(sources, |dae| {
        let (real, vector) = dae.types(|types| {
            Ok((
                types.derived(dae::ValueType::scalar(dae::ScalarType::Real), provenance)?,
                types.derived(
                    dae::ValueType::array(dae::ScalarType::Real, [2]),
                    provenance,
                )?,
            ))
        })?;
        let identity = define_identity_function(dae, real, provenance)?;
        let _ = dae.function(
            dae::FunctionSignature::new(VarName::new("guardedUpdate"), [], [vector], provenance),
            |dae, reservation| {
                let output = dae.functions(|functions| {
                    functions.output(&reservation, VarName::new("q"), 0, provenance)
                })?;
                let mut body =
                    dae.functions(|functions| functions.begin(reservation, provenance))?;
                let (zero, argument, condition) = dae.expressions(|expressions| {
                    Ok((
                        expressions
                            .at(provenance)
                            .literal(dae::DaeLiteral::Real(0.0))?,
                        expressions
                            .at(provenance)
                            .literal(dae::DaeLiteral::Real(7.0))?,
                        expressions
                            .at(provenance)
                            .literal(dae::DaeLiteral::Boolean(false))?,
                    ))
                })?;
                let zeros =
                    dae.expressions(|expressions| expressions.at(provenance).array([zero, zero]))?;
                assign_function_value(dae, &mut body, output, zeros, provenance)?;
                let base = read_function_value(dae, &body, output, provenance)?;
                let call = dae.expressions(|expressions| {
                    expressions.at(provenance).call(identity, 0, [argument])
                })?;
                let first = indexed_update_with_value(dae, base, call, 1, provenance)?;
                let conditional = dae.expressions(|expressions| {
                    expressions
                        .at(provenance)
                        .conditional([(condition, first)], base)
                })?;
                let second = indexed_update_with_value(dae, conditional, call, 2, provenance)?;
                assign_function_value(dae, &mut body, output, second, provenance)?;
                dae.functions(|functions| functions.define(body, provenance))
            },
        )?;
        Ok(())
    })
    .unwrap();

    model.inspect(|view| {
        let definitions = rumoca_phase_structural::CausalDefinitions::derive(view);
        let lowered = user_functions::lower_reachable(
            view,
            &definitions,
            HashSet::from([1]),
            EmissionPolicy::reviewable(),
        )
        .unwrap();
        let function = lowered
            .iter()
            .find(|function| function.name.lexeme() == "guardedUpdate")
            .unwrap();
        assert!(
            function
                .statements
                .iter()
                .all(|statement| matches!(statement.node, gast::Statement::For(_))),
            "a chain rooted in a conditional join diverts, so the body is the \
             seed loop and the aggregate loop and nothing else:\n{:#?}",
            function.statements
        );
        assert_eq!(
            unguarded_calls(&function.statements),
            0,
            "every materialized call must sit inside the guard that selects \
             it: a call hoisted out of its guard would run on the path the \
             conditional did not take:\n{:#?}",
            function.statements
        );
        assert!(
            guarded_calls(&function.statements) > 0,
            "the guarded call must survive lowering:\n{:#?}",
            function.statements
        );
    });
}
