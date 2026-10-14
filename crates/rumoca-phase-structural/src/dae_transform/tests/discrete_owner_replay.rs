//! Whole-root reconstruction retains the compact B.1c domain and observation
//! classification; scalar or foreign-domain constructors still refuse escape.
use super::*;

fn domain<'dae>(
    model: &mut dae::DaeConstruction<'dae>,
    at: dae::DaeProvenance,
) -> Result<dae::DomainId<'dae>, dae::DaeConstructionError> {
    model.domains(|domains| {
        domains.structured(
            rumoca_core::StructuredIndexDomain {
                binders: vec![rumoca_core::StructuredIndexBinder {
                    provenance: rumoca_core::IndexBinderProvenance::Generated,
                    id: 0,
                    display_name: "i".into(),
                    lower: 2,
                    upper: 1,
                    step: -1,
                }],
            },
            at,
        )
    })
}

fn fixture(structured: bool) -> dae::Dae {
    let text = "Boolean moving[2]; Boolean observed; i > 1; time > 0.5;";
    let mut map = SourceMap::new();
    let source = map.add("DiscreteOwnerReplay.mo", text);
    let at = source_provenance(
        source,
        text,
        if structured { "i > 1" } else { "time > 0.5" },
    );
    dae::Dae::construct(map, |model| {
        let ty = model.types(|types| {
            types.derived(
                if structured {
                    dae::ValueType::array(dae::ScalarType::Boolean, [2])
                } else {
                    dae::ValueType::scalar(dae::ScalarType::Boolean)
                },
                at,
            )
        })?;
        let target = model.variables(|variables| {
            variables.discrete_value(
                VarName::new(if structured { "moving" } else { "observed" }),
                ty,
                at,
                dae::VariableAttributes::default(),
            )
        })?;
        let active = structured.then(|| domain(model, at)).transpose()?;
        let foreign = structured.then(|| domain(model, at)).transpose()?;
        let binder = active
            .map(|active| model.domains(|domains| domains.binder(active, 0, at)))
            .transpose()?;
        let value = model.expressions(|expressions| {
            let (lhs, rhs) = match binder {
                Some(binder) => (
                    expressions.at(at).binder(binder)?,
                    expressions.at(at).literal(dae::DaeLiteral::Integer(1))?,
                ),
                None => (
                    expressions.at(at).coordinate(dae::CoordinateInput::Time)?,
                    expressions.at(at).literal(dae::DaeLiteral::Real(0.5))?,
                ),
            };
            expressions
                .at(at)
                .binary(dae::BinaryOperator::Greater, lhs, rhs)
        })?;
        model.b1c([target], |topology| {
            match active {
                Some(active) => {
                    assert!(matches!(
                        topology.owner(at, [target], |owner| owner.always(at, [(value, at)])),
                        Err(dae::DaeConstructionError::InvalidBinderScope {
                            expected_domain: None,
                            ..
                        })
                    ));
                    assert!(matches!(
                        topology.structured_owner(
                            at,
                            foreign.unwrap(),
                            rumoca_core::ComprehensionScalarView::BinderSubstitution,
                            [target],
                            |owner| owner.always(at, [(value, at)])
                        ),
                        Err(dae::DaeConstructionError::InvalidBinderScope {
                            expected_domain: Some(_),
                            ..
                        })
                    ));
                    topology.structured_owner(
                        at,
                        active,
                        rumoca_core::ComprehensionScalarView::BinderSubstitution,
                        [target],
                        |owner| owner.always(at, [(value, at)]),
                    )?;
                }
                None => {
                    topology
                        .observed_owner(at, [target], |owner| owner.always(at, [(value, at)]))?;
                }
            }
            Ok(())
        })
    })
    .expect("constructor-backed owner and negative admission controls")
}

fn replay(model: &dae::Dae) -> dae::Dae {
    let plan = model.inspect(|view| vec![None; view.expression_count()]);
    reconstruction::rebuild_literal_expressions(model, &plan).expect("whole-root owner replay")
}

#[test]
fn structured_discrete_replay_retains_domain_direction_and_wire() {
    let original = fixture(true);
    let rebuilt = replay(&original);
    let expected = serde_json::to_value(&original).unwrap();
    assert_eq!(serde_json::to_value(&rebuilt).unwrap(), expected);
    rebuilt.inspect(|view| {
        let owner = view
            .discrete_value_owner(view.discrete_value_owner_id(0).unwrap())
            .unwrap();
        let structure = owner.structure().expect("compact owner survives");
        assert_eq!(structure.scalar_rows(), 2);
        assert_eq!(
            view.domain(structure.domain())
                .unwrap()
                .structured()
                .binders[0]
                .step,
            -1
        );
        assert_eq!(owner.targets().len(), 1);
        assert!(!owner.observed());
    });
    let bytes = serde_json::to_vec(&rebuilt).unwrap();
    let decoded: dae::Dae = serde_json::from_slice(&bytes).unwrap();
    assert_eq!(serde_json::to_vec(&decoded).unwrap(), bytes);
}

#[test]
fn observed_discrete_replay_retains_output_sampling_classification() {
    let original = fixture(false);
    let rebuilt = replay(&original);
    assert_eq!(
        serde_json::to_value(&rebuilt).unwrap(),
        serde_json::to_value(&original).unwrap()
    );
    rebuilt.inspect(|view| {
        let owner = view
            .discrete_value_owner(view.discrete_value_owner_id(0).unwrap())
            .unwrap();
        assert!(owner.observed());
        assert!(owner.structure().is_none());
        assert_eq!(owner.branches().len(), 1);
    });
}

#[test]
fn selected_discrete_replay_retains_element_identity_activation_and_hold() {
    let text = "Boolean m[2,2]; when active then m[2,1] = true; end when;";
    let mut map = SourceMap::new();
    let source = map.add("SelectedOwnerReplay.mo", text);
    let at = source_provenance(source, text, "Boolean m[2,2]");
    let original = dae::Dae::construct(map, |model| {
        let ty = model.types(|types| {
            types.derived(dae::ValueType::array(dae::ScalarType::Boolean, [2, 2]), at)
        })?;
        let variable = model.variables(|variables| {
            variables.discrete_value(
                VarName::new("m"),
                ty,
                at,
                dae::VariableAttributes::default(),
            )
        })?;
        let targets = [[1, 1], [1, 2], [2, 1], [2, 2]]
            .into_iter()
            .map(|indices| model.discrete_value_element(variable, &indices, at))
            .collect::<Result<Vec<_>, _>>()?;
        let value = model.expressions(|expressions| {
            expressions.at(at).literal(dae::DaeLiteral::Boolean(true))
        })?;
        let active = model.conditions(|conditions| {
            let active = conditions.reserve(at)?;
            conditions.define(active, dae::ConditionInput::Discrete(value), at)?;
            Ok(active)
        })?;
        let held = model.expressions(|expressions| {
            targets
                .iter()
                .map(|target| expressions.at(at).pre_discrete_value_target(*target))
                .collect::<Result<Vec<_>, _>>()
        })?;
        model.b1c_selected(targets.iter().copied(), |topology| {
            append_selected_owners(topology, &targets, &held, active, value, at)
        })
    })
    .unwrap();
    let rebuilt = replay(&original);
    assert_eq!(
        serde_json::to_value(&rebuilt).unwrap(),
        serde_json::to_value(&original).unwrap()
    );
    rebuilt.inspect(|view| {
        assert_eq!(view.variable_count(), 1);
        for ordinal in 0..4 {
            let owner = view
                .discrete_value_owner(view.discrete_value_owner_id(ordinal).unwrap())
                .unwrap();
            let target = owner.targets().get(0).unwrap();
            assert_eq!(target.scalar_selection(), Some(ordinal as u32));
            assert_eq!(
                view.discrete_value_target_indices(target).unwrap(),
                [ordinal as i64 / 2 + 1, ordinal as i64 % 2 + 1]
            );
            assert_eq!(
                matches!(
                    owner.branches().get(0).unwrap().activation(),
                    dae::DiscreteBranchActivation::When { .. }
                ),
                ordinal == 2
            );
        }
    });
}

fn append_selected_owners<'dae>(
    topology: &mut dae::DiscreteValueTopology<'_, 'dae>,
    targets: &[dae::DiscreteValueTarget<'dae>],
    held: &[dae::ExprId<'dae>],
    active: dae::ConditionId<'dae>,
    value: dae::ExprId<'dae>,
    at: dae::DaeProvenance,
) -> Result<(), dae::DaeConstructionError> {
    for (ordinal, (&target, &held)) in targets.iter().zip(held).enumerate() {
        topology.selected_owner(at, [target], |owner| {
            if ordinal == 2 {
                owner.when(active, active, at, [(value, at)])
            } else {
                owner.always(at, [(held, at)])
            }
        })?;
    }
    Ok(())
}
