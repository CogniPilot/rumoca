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
