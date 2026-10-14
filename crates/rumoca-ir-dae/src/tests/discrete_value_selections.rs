use super::*;

fn array_variable<'dae>(
    dae: &mut DaeConstruction<'dae>,
    name: &str,
    dimensions: &[u32],
    at: DaeProvenance,
) -> Result<DiscreteValueId<'dae>, DaeConstructionError> {
    let ty = dae.types(|types| {
        types.derived(
            ValueType::array(ScalarType::Boolean, dimensions.to_vec()),
            at,
        )
    })?;
    dae.variables(|variables| {
        variables.discrete_value(VarName::new(name), ty, at, VariableAttributes::default())
    })
}

fn selected_read<'dae>(
    dae: &mut DaeConstruction<'dae>,
    variable: DiscreteValueId<'dae>,
    index: i64,
    at: DaeProvenance,
) -> Result<ExprId<'dae>, DaeConstructionError> {
    dae.expressions(|expressions| {
        let base = expressions
            .at(at)
            .coordinate(CoordinateInput::DiscreteValue(variable))?;
        let index = expressions.at(at).literal(DaeLiteral::Integer(index))?;
        expressions.at(at).index(
            base,
            [Subscript::Index {
                expression: index,
                provenance: at,
            }],
        )
    })
}

#[derive(Clone, Copy)]
enum DependencyCase {
    EarlierElement,
    LaterElement,
    EarlierGuard,
    LaterGuard,
    WholeBeforeCoverage,
    WholeAfterCoverage,
}

fn dependency_dae(case: DependencyCase) -> Result<Dae, DaeConstructionError> {
    let source = TestSource::new("discrete Boolean m[2]; discrete Boolean n[2];");
    let at = source.source("discrete Boolean m[2]", 0);
    Dae::construct(source.map, |dae| {
        let m = array_variable(dae, "m", &[2], at)?;
        let first = dae.discrete_value_element(m, &[1], at)?;
        let second = dae.discrete_value_element(m, &[2], at)?;
        let literal =
            dae.expressions(|expressions| expressions.at(at).literal(DaeLiteral::Boolean(true)))?;
        let earlier = selected_read(dae, m, 1, at)?;
        let later = selected_read(dae, m, 2, at)?;
        let guard = dae.conditions(|conditions| {
            let condition = conditions.reserve(at)?;
            let value = if matches!(case, DependencyCase::LaterGuard) {
                later
            } else {
                earlier
            };
            conditions.define(condition, ConditionInput::Discrete(value), at)?;
            Ok(condition)
        })?;
        if matches!(
            case,
            DependencyCase::WholeBeforeCoverage | DependencyCase::WholeAfterCoverage
        ) {
            let n = array_variable(dae, "n", &[2], at)?;
            let whole = dae.expressions(|expressions| {
                expressions
                    .at(at)
                    .coordinate(CoordinateInput::DiscreteValue(m))
            })?;
            let plan = if matches!(case, DependencyCase::WholeBeforeCoverage) {
                [first, n.into(), second]
            } else {
                [first, second, n.into()]
            };
            return dae.b1c_selected(plan, |topology| {
                whole_dependency_owners(topology, plan, n, whole, literal, at)
            });
        }
        let first_guard = matches!(case, DependencyCase::LaterGuard).then_some(guard);
        let second_guard = matches!(case, DependencyCase::EarlierGuard).then_some(guard);
        let first_value = if matches!(case, DependencyCase::LaterElement) {
            later
        } else {
            literal
        };
        let second_value = if second_guard.is_some() {
            literal
        } else {
            earlier
        };
        dae.b1c_selected([first, second], |topology| {
            define_selection(topology, first, first_value, first_guard, at)?;
            define_selection(topology, second, second_value, second_guard, at)
        })
    })
}

fn define_selection<'dae>(
    topology: &mut DiscreteValueTopology<'_, 'dae>,
    target: DiscreteValueTarget<'dae>,
    value: ExprId<'dae>,
    guard: Option<ConditionId<'dae>>,
    at: DaeProvenance,
) -> Result<(), DaeConstructionError> {
    topology.selected_owner(at, [target], |owner| match guard {
        Some(guard) => owner.when(guard, guard, at, [(value, at)]),
        None => owner.always(at, [(value, at)]),
    })?;
    Ok(())
}

fn whole_dependency_owners<'dae>(
    topology: &mut DiscreteValueTopology<'_, 'dae>,
    plan: [DiscreteValueTarget<'dae>; 3],
    n: DiscreteValueId<'dae>,
    whole: ExprId<'dae>,
    literal: ExprId<'dae>,
    at: DaeProvenance,
) -> Result<(), DaeConstructionError> {
    for target in plan {
        let value = if target.variable() == n {
            whole
        } else {
            literal
        };
        define_selection(topology, target, value, None, at)?;
    }
    Ok(())
}

#[test]
fn selected_dependencies_require_the_exact_prior_element() {
    for case in [
        DependencyCase::EarlierElement,
        DependencyCase::EarlierGuard,
        DependencyCase::WholeAfterCoverage,
    ] {
        dependency_dae(case).expect("the selected dependencies are already issued");
    }
    for case in [
        DependencyCase::LaterElement,
        DependencyCase::LaterGuard,
        DependencyCase::WholeBeforeCoverage,
    ] {
        assert!(matches!(
            dependency_dae(case),
            Err(DaeConstructionError::UnissuedDiscreteDependency { .. })
        ));
    }
}

#[test]
fn selection_checks_rank_bounds_and_row_major_identity() {
    let source = TestSource::new("discrete Boolean m[2,3];");
    let at = source.source("discrete Boolean m[2,3]", 0);
    Dae::construct(source.map, |dae| {
        let m = array_variable(dae, "m", &[2, 3], at)?;
        for indices in [
            vec![],
            vec![1],
            vec![1, 1, 1],
            vec![0, 1],
            vec![1, -1],
            vec![3, 1],
            vec![1, 4],
        ] {
            assert!(matches!(
                dae.discrete_value_element(m, &indices, at),
                Err(DaeConstructionError::InvalidDiscreteElementSelection { .. })
            ));
        }
        assert_eq!(
            dae.discrete_value_element(m, &[2, 1], at)?
                .scalar_selection(),
            Some(3)
        );
        let selected = dae.discrete_value_element(m, &[2, 1], at)?;
        dae.expressions(|expressions| {
            let current = expressions.at(at).discrete_value_target(selected)?;
            let pre = expressions.at(at).pre_discrete_value_target(selected)?;
            assert!(expressions.value_type(current, at)?.is_scalar());
            assert!(expressions.value_type(pre, at)?.is_scalar());
            let whole = expressions.at(at).discrete_value_target(m.into())?;
            assert_eq!(expressions.value_type(whole, at)?.dimensions(), [2, 3]);
            Ok(())
        })?;
        let value = dae.expressions(|expressions| {
            let scalar = expressions.at(at).literal(DaeLiteral::Boolean(true))?;
            let row = expressions.at(at).array([scalar; 3])?;
            expressions.at(at).array([row; 2])
        })?;
        dae.b1c([m], |topology| {
            topology.owner(at, [m], |owner| owner.always(at, [(value, at)]))?;
            Ok(())
        })
    })
    .unwrap();
}

#[test]
fn selection_plan_rejects_partial_duplicate_and_whole_overlap() {
    let source = TestSource::new("discrete Boolean m[2];");
    let at = source.source("discrete Boolean m[2]", 0);
    Dae::construct(source.map, |dae| {
        let m = array_variable(dae, "m", &[2], at)?;
        let a = dae.discrete_value_element(m, &[1], at)?;
        let b = dae.discrete_value_element(m, &[2], at)?;
        for plan in [vec![a], vec![a, a, b], vec![m.into(), b], vec![a, m.into()]] {
            assert!(matches!(
                dae.b1c_selected(plan, |_| panic!("invalid coverage reaches no owner")),
                Err(DaeConstructionError::InvalidDiscreteTopologyPlan { .. })
            ));
        }
        let literal =
            dae.expressions(|expressions| expressions.at(at).literal(DaeLiteral::Boolean(true)))?;
        dae.b1c_selected([a, b], |topology| {
            for target in [a, b] {
                topology.selected_owner(at, [target], |owner| owner.always(at, [(literal, at)]))?;
            }
            Ok(())
        })
    })
    .unwrap();
}

#[test]
fn selected_owner_rejects_aggregate_values_and_recovers_atomically() {
    let source = TestSource::new("discrete Boolean m[2];");
    let at = source.source("discrete Boolean m[2]", 0);
    let dae = Dae::construct(source.map, |dae| {
        let m = array_variable(dae, "m", &[2], at)?;
        let a = dae.discrete_value_element(m, &[1], at)?;
        let b = dae.discrete_value_element(m, &[2], at)?;
        let (scalar, array) = dae.expressions(|expressions| {
            let scalar = expressions.at(at).literal(DaeLiteral::Boolean(true))?;
            Ok((scalar, expressions.at(at).array([scalar; 2])?))
        })?;
        dae.b1c_selected([a, b], |topology| {
            assert!(matches!(
                topology.selected_owner(at, [a], |owner| owner.always(at, [(array, at)])),
                Err(DaeConstructionError::ShapeMismatch { .. })
            ));
            for target in [a, b] {
                topology.selected_owner(at, [target], |owner| owner.always(at, [(scalar, at)]))?;
            }
            Ok(())
        })
    })
    .unwrap();
    dae.inspect(|view| {
        assert_eq!(view.variable_count(), 1);
        assert_eq!(view.discrete_value_owner_count(), 2);
    });
}

#[test]
fn selection_wire_replays_coverage_and_dependencies() {
    let dae = dependency_dae(DependencyCase::EarlierElement).unwrap();
    let json = serde_json::to_string(&dae).unwrap();
    let decoded: Dae = serde_json::from_str(&json).unwrap();
    assert_eq!(serde_json::to_string(&decoded).unwrap(), json);
    let bytes = bincode::serialize(&dae).unwrap();
    let decoded: Dae = bincode::deserialize(&bytes).unwrap();
    assert_eq!(bincode::serialize(&decoded).unwrap(), bytes);
    let base: serde_json::Value = serde_json::from_str(&json).unwrap();
    for selection in [
        serde_json::json!(2),
        serde_json::json!(0),
        serde_json::Value::Null,
    ] {
        let mut forged = base.clone();
        forged["storage"]["discrete_value_owners"][1]["targets"][0]["scalar_selection"] = selection;
        assert!(serde_json::from_value::<Dae>(forged).is_err());
    }
    let mut reordered = base.clone();
    reordered["storage"]["discrete_value_owners"]
        .as_array_mut()
        .unwrap()
        .swap(0, 1);
    assert!(serde_json::from_value::<Dae>(reordered).is_err());
    let mut partial = base;
    partial["storage"]["discrete_value_owners"]
        .as_array_mut()
        .unwrap()
        .pop();
    assert!(serde_json::from_value::<Dae>(partial).is_err());
}

#[test]
fn selected_elements_keep_independent_when_activations_and_hold() {
    let source = TestSource::new(
        "discrete Boolean m[2]; when first then m[1]=true; end when; when second then m[2]=false; end when;",
    );
    let at = source.source("discrete Boolean m[2]", 0);
    let dae = Dae::construct(source.map, |dae| {
        let m = array_variable(dae, "m", &[2], at)?;
        let targets = [
            dae.discrete_value_element(m, &[1], at)?,
            dae.discrete_value_element(m, &[2], at)?,
        ];
        let values = dae.expressions(|expressions| {
            Ok([
                expressions.at(at).literal(DaeLiteral::Boolean(true))?,
                expressions.at(at).literal(DaeLiteral::Boolean(false))?,
            ])
        })?;
        let conditions = dae.conditions(|conditions| {
            let first = conditions.reserve(at)?;
            conditions.define(first, ConditionInput::Discrete(values[0]), at)?;
            let second = conditions.reserve(at)?;
            conditions.define(second, ConditionInput::Discrete(values[1]), at)?;
            Ok([first, second])
        })?;
        dae.b1c_selected(targets, |topology| {
            for ((target, value), condition) in targets.into_iter().zip(values).zip(conditions) {
                define_selection(topology, target, value, Some(condition), at)?;
            }
            Ok(())
        })
    })
    .unwrap();
    let json = serde_json::to_string(&dae).unwrap();
    let decoded: Dae = serde_json::from_str(&json).unwrap();
    decoded.inspect(|view| {
        assert_eq!(view.variable_count(), 1);
        for ordinal in 0..2 {
            let owner = view
                .discrete_value_owner(view.discrete_value_owner_id(ordinal).unwrap())
                .unwrap();
            let target = owner.targets().get(0).unwrap();
            assert_eq!(target.variable().index(), 0);
            assert_eq!(target.scalar_selection(), Some(ordinal as u32));
            assert_eq!(owner.branches().len(), 1);
            let DiscreteBranchActivation::When { trigger, guard } =
                owner.branches().get(0).unwrap().activation()
            else {
                panic!("element must hold when its independent when branch is inactive");
            };
            assert_eq!(trigger.index(), ordinal as u32);
            assert_eq!(guard, trigger);
        }
    });
}

#[test]
fn packed_target_view_preserves_selected_and_whole_owner_order_after_wire_replay() {
    let model = dependency_dae(DependencyCase::WholeAfterCoverage).unwrap();
    let json: Dae = serde_json::from_str(&serde_json::to_string(&model).unwrap()).unwrap();
    let binary: Dae = bincode::deserialize(&bincode::serialize(&model).unwrap()).unwrap();
    for model in [&model, &json, &binary] {
        model.inspect(|view| {
            let targets = view.discrete_value_targets();
            assert_eq!(targets.len(), view.discrete_value_definition_count());
            let actual = targets
                .iter()
                .map(|target| (target.index(), target.scalar_selection()))
                .collect::<Vec<_>>();
            assert_eq!(actual, [(0, Some(0)), (0, Some(1)), (1, None)]);
        });
    }
}
