//! Structural replay pins for construction-owned dynamic quotients.

use super::*;

pub(super) fn fixture_declarations(enabled: bool) -> &'static str {
    if enabled {
        " function quotient(input Real u, output Real value); algorithm value := mod(u, 2); end quotient; x > 0; mod(x, p); x < 10; y = p;"
    } else {
        ""
    }
}

pub(super) fn insert<'dae>(
    model: &mut dae::DaeConstruction<'dae>,
    config: (
        dae::ValueTypeId<'dae>,
        FixtureVariables<'dae>,
        rumoca_core::SourceId,
        &str,
        bool,
    ),
) -> Result<(), dae::DaeConstructionError> {
    let (real, variables, source, text, enabled) = config;
    if !enabled {
        return Ok(());
    }
    insert_function_owner(model, real, source, text)?;
    let x_at = nested_source_provenance(source, text, "mod(x, p)", "x", 0);
    let p_at = nested_source_provenance(source, text, "mod(x, p)", "p", 0);
    let quotient_at = source_provenance(source, text, "mod(x, p)");
    let (x, p) = model.expressions(|expressions| {
        Ok((
            expressions
                .at(x_at)
                .coordinate(dae::CoordinateInput::State(variables.x))?,
            expressions
                .at(p_at)
                .coordinate(dae::CoordinateInput::Parameter(variables.p))?,
        ))
    })?;
    insert_generic_root(model, variables.x, source, text, "x > 0", true)?;
    model.runtime_quotient(dae::PureBuiltin::Mod, [x, p], quotient_at)?;
    insert_generic_root(model, variables.x, source, text, "x < 10", false)?;
    insert_second_round_constraint(model, variables, source, text)
}

fn insert_second_round_constraint<'dae>(
    model: &mut dae::DaeConstruction<'dae>,
    variables: FixtureVariables<'dae>,
    source: rumoca_core::SourceId,
    text: &str,
) -> Result<(), dae::DaeConstructionError> {
    let equation_at = source_provenance(source, text, "y = p");
    let y_at = nested_source_provenance(source, text, "y = p", "y", 0);
    let p_at = nested_source_provenance(source, text, "y = p", "p", 0);
    let residual = model.expressions(|expressions| {
        let y = expressions
            .at(y_at)
            .coordinate(dae::CoordinateInput::State(variables.y))?;
        let p = expressions
            .at(p_at)
            .coordinate(dae::CoordinateInput::Parameter(variables.p))?;
        expressions
            .at(equation_at)
            .binary(dae::BinaryOperator::Subtract, y, p)
    })?;
    model.continuous(|continuous| continuous.value_equation(equation_at, residual).map(|_| ()))
}

fn insert_function_owner<'dae>(
    model: &mut dae::DaeConstruction<'dae>,
    real: dae::ValueTypeId<'dae>,
    source: rumoca_core::SourceId,
    text: &str,
) -> Result<(), dae::DaeConstructionError> {
    let declaration = source_provenance(source, text, "function quotient");
    let parameter_at = source_provenance(source, text, "input Real u");
    let output_at = source_provenance(source, text, "output Real value");
    let assignment_at = source_provenance(source, text, "value := mod(u, 2)");
    let u_at = nested_source_provenance(source, text, "mod(u, 2)", "u", 0);
    let two_at = nested_source_provenance(source, text, "mod(u, 2)", "2", 0);
    let quotient_at = source_provenance(source, text, "mod(u, 2)");
    model
        .function(
            dae::FunctionSignature::new(VarName::new("quotient"), [real], [real], declaration),
            |model, reservation| {
                let parameter = model.functions(|functions| {
                    functions.parameter(&reservation, VarName::new("u"), 0, parameter_at)
                })?;
                let output = model.functions(|functions| {
                    functions.output(&reservation, VarName::new("value"), 0, output_at)
                })?;
                let mut body =
                    model.functions(|functions| functions.begin(reservation, declaration))?;
                let (u, two) = model.expressions(|expressions| {
                    Ok((
                        expressions.at(u_at).function_parameter(parameter)?,
                        expressions
                            .at(two_at)
                            .literal(dae::DaeLiteral::Integer(2))?,
                    ))
                })?;
                let quotient = model.function_runtime_quotient(
                    &body,
                    dae::PureBuiltin::Mod,
                    [u, two],
                    quotient_at,
                )?;
                model.functions(|functions| {
                    functions.assign(&mut body, output, quotient, assignment_at)?;
                    functions.define(body, declaration)
                })
            },
        )
        .map(|_| ())
}

fn insert_generic_root<'dae>(
    model: &mut dae::DaeConstruction<'dae>,
    x: dae::StateId<'dae>,
    source: rumoca_core::SourceId,
    text: &str,
    snippet: &str,
    greater: bool,
) -> Result<(), dae::DaeConstructionError> {
    let at = source_provenance(source, text, snippet);
    let activation = model.conditions(|conditions| conditions.reserve(at))?;
    let expression = model.expressions(|expressions| {
        let x = expressions
            .at(at)
            .coordinate(dae::CoordinateInput::State(x))?;
        let bound = expressions
            .at(at)
            .literal(dae::DaeLiteral::Real(if greater { 0.0 } else { 10.0 }))?;
        expressions.at(at).binary(
            if greater {
                dae::BinaryOperator::Greater
            } else {
                dae::BinaryOperator::Less
            },
            x,
            bound,
        )
    })?;
    let relation = model.conditions(|conditions| conditions.relation(expression, at))?;
    model.conditions(|conditions| {
        conditions.define(activation, dae::ConditionInput::Relation(relation), at)?;
        conditions.root(relation, activation, at).map(|_| ())
    })
}

#[derive(Debug, PartialEq, Eq)]
struct OwnerSnapshot {
    owners: Vec<(&'static str, dae::PureBuiltin)>,
    owner_expression_counts: Vec<usize>,
    relations: Vec<(dae::DaeProvenanceOrigin, Option<String>)>,
    conditions: Vec<&'static str>,
    roots: Vec<(u32, u32)>,
    discontinuity_expressions: usize,
}

fn owner_snapshot(view: dae::DaeView<'_>) -> OwnerSnapshot {
    let owner_views = (0..view.runtime_quotient_owner_count())
        .map(|index| {
            view.runtime_quotient_owner_at(index)
                .expect("owner ordinal resolves")
        })
        .collect::<Vec<_>>();
    let owners = owner_views
        .iter()
        .copied()
        .map(|owner| {
            let kind = match owner.kind() {
                dae::RuntimeQuotientOwnerKind::FunctionBody { .. } => "function",
                dae::RuntimeQuotientOwnerKind::ModelEvent { generated, .. } => {
                    assert_eq!(generated.len(), 6);
                    "model"
                }
            };
            (kind, owner.builtin())
        })
        .collect();
    let owner_expression_counts = owner_views
        .into_iter()
        .map(|owner| {
            let mut count = 1;
            if let dae::RuntimeQuotientOwnerKind::ModelEvent { generated, .. } = owner.kind() {
                count += generated.len();
            }
            count
        })
        .collect();
    let relations = (0..view.relation_count())
        .map(|index| {
            let relation = view
                .relation(view.relation_id(index).expect("relation id resolves"))
                .expect("relation resolves");
            (
                relation.provenance().origin(),
                view.source_text(relation.provenance()).map(str::to_owned),
            )
        })
        .collect();
    let conditions = (0..view.condition_count())
        .map(|index| {
            let condition = view
                .condition(view.condition_id(index).expect("condition id resolves"))
                .expect("condition resolves");
            match condition.operation() {
                dae::ConditionOperation::Always => "always",
                dae::ConditionOperation::Relation(_) => "relation",
                _ => "other",
            }
        })
        .collect();
    let roots = (0..view.root_count())
        .map(|index| {
            let root = view
                .root(view.root_id(index).expect("root id resolves"))
                .expect("root resolves");
            (root.relation().index(), root.activation().index())
        })
        .collect();
    let discontinuity_expressions = (0..view.expression_count())
        .filter_map(|index| view.expression_id(index))
        .filter_map(|id| view.expression(id))
        .filter(|expression| {
            expression.provenance().origin()
                == dae::DaeProvenanceOrigin::Generated(dae::DaeGeneration::RuntimeDiscontinuity)
        })
        .count();
    OwnerSnapshot {
        owners,
        owner_expression_counts,
        relations,
        conditions,
        roots,
        discontinuity_expressions,
    }
}

fn assert_owner_snapshot(snapshot: &OwnerSnapshot) {
    assert_eq!(
        snapshot.owners,
        [
            ("function", dae::PureBuiltin::Mod),
            ("model", dae::PureBuiltin::Mod),
        ]
    );
    assert_eq!(snapshot.owner_expression_counts, [1, 7]);
    assert_eq!(snapshot.conditions, ["relation", "always", "relation"]);
    assert_eq!(snapshot.roots, [(0, 0), (1, 1), (2, 2)]);
    assert_eq!(snapshot.discontinuity_expressions, 6);
    assert_eq!(snapshot.relations[0].1.as_deref(), Some("x > 0"));
    assert_eq!(snapshot.relations[1].1.as_deref(), Some("mod(x, p)"));
    assert_eq!(snapshot.relations[2].1.as_deref(), Some("x < 10"));
}

#[test]
fn direct_demotion_replays_model_and_function_owners_at_exact_stream_positions() {
    let (model, _) = constrained_state_model(
        false,
        FixtureFeatures {
            runtime_quotient: true,
            ..FixtureFeatures::default()
        },
    );
    let source = model.inspect(owner_snapshot);
    assert_owner_snapshot(&source);
    let candidate = model
        .inspect(direct_state_constraints)
        .admissible
        .into_iter()
        .next()
        .expect("fixture has a direct demotion");
    let first = rebuild_with_state_demotion(&model, candidate)
        .expect("owned quotient replays through direct demotion");
    let second_candidate = first
        .inspect(direct_state_constraints)
        .admissible
        .into_iter()
        .next()
        .expect("the rebuilt fixture has a second direct demotion");
    let second = rebuild_with_state_demotion(&first, second_candidate)
        .expect("a second reconstruction round retains owned quotients");
    assert_eq!(first.inspect(owner_snapshot), source);
    assert_eq!(second.inspect(owner_snapshot), source);
    first.inspect(|view| {
        let model_owner = view
            .runtime_quotient_owner_at(1)
            .expect("model owner survives");
        let expression = view
            .expression(model_owner.quotient())
            .expect("quotient expression survives");
        let dae::ExpressionOperation::Builtin { arguments, .. } = expression.operation() else {
            panic!("owned quotient remains a builtin")
        };
        let dividend = view
            .expression(arguments.get(0).expect("quotient has a dividend"))
            .expect("dividend resolves");
        assert!(matches!(
            dividend.operation(),
            dae::ExpressionOperation::Coordinate(dae::CoordinateView::Algebraic(_))
        ));
    });
    let encoded = serde_json::to_vec(&first).expect("rebuilt DAE serializes");
    let decoded: dae::Dae = serde_json::from_slice(&encoded).expect("rebuilt DAE wire replays");
    assert_eq!(
        encoded,
        serde_json::to_vec(&decoded).expect("replayed DAE serializes identically")
    );
}

#[test]
fn function_owner_rejects_replay_in_a_foreign_active_body() {
    let (model, _) = constrained_state_model(
        false,
        FixtureFeatures {
            runtime_quotient: true,
            ..FixtureFeatures::default()
        },
    );
    model.inspect(|source| {
        let quotient = source
            .runtime_quotient_owner_at(0)
            .expect("fixture starts with its function owner")
            .quotient();
        let error = dae::Dae::construct(model.source_map().clone(), |target| {
            let mut plan =
                super::super::runtime_quotients::RuntimeQuotientReplayPlan::collect(source)?;
            let mut rebuilt = vec![None; source.expression_count()];
            let error = plan
                .replay_function_owner(source, target, None, Some(1), quotient, &mut rebuilt)
                .expect_err("a function owner cannot replay in a foreign body");
            assert!(matches!(
                error,
                dae::DaeConstructionError::InvalidFunctionScope {
                    expected_function: Some(0),
                    found_function: 1,
                    ..
                }
            ));
            Ok(())
        });
        assert!(
            error.is_ok(),
            "the discriminator itself is side-effect-free"
        );
    });
}

#[test]
fn holonomic_reconstruction_retains_one_complete_model_surface() {
    let (model, _) = constrained_state_model(
        false,
        FixtureFeatures {
            holonomic: true,
            runtime_quotient: true,
            ..FixtureFeatures::default()
        },
    );
    let source = model.inspect(owner_snapshot);
    let constraint = model
        .inspect(holonomic_constraints)
        .into_iter()
        .next()
        .expect("fixture has a holonomic constraint");
    let (rebuilt, manifold) = rebuild_holonomic_constraint(&model, &constraint, &[])
        .expect("owned quotient replays through holonomic reconstruction");
    assert_eq!(manifold.len(), 2);
    assert_eq!(rebuilt.inspect(owner_snapshot), source);
    assert_owner_snapshot(&source);
}
