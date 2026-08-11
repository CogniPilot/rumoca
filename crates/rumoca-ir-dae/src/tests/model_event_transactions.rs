use super::*;

const SOURCE: &str = "discrete Real z; discrete Boolean valid; \
    when sample(0, 1) then z := 1; valid := true; end when;";

fn fixture() -> Dae {
    let source = TestSource::new(SOURCE);
    let z_at = source.source("discrete Real z", 0);
    let valid_at = source.source("discrete Boolean valid", 0);
    let sample_at = source.source("sample(0, 1)", 0);
    let owner_at = source.source("when sample(0, 1) then z := 1; valid := true; end when", 0);
    let z_definition_at = source.source("z := 1", 0);
    let valid_definition_at = source.source("valid := true", 0);
    Dae::construct(source.map, |dae| {
        let real = dae.types(|types| {
            types.intern(TypeId::new(0), ValueType::scalar(ScalarType::Real), z_at)
        })?;
        let boolean = dae.types(|types| {
            types.intern(
                TypeId::new(1),
                ValueType::scalar(ScalarType::Boolean),
                valid_at,
            )
        })?;
        let (z, valid) = dae.variables(|variables| {
            Ok((
                variables.discrete_real(
                    VarName::new("z"),
                    real,
                    z_at,
                    VariableAttributes::default(),
                )?,
                variables.discrete_value(
                    VarName::new("valid"),
                    boolean,
                    valid_at,
                    VariableAttributes::default(),
                )?,
            ))
        })?;
        let clock = dae.clocks(|clocks| {
            let clock = clocks.periodic(
                ClockLattice::new(ClockRational::ONE, ClockRational::ZERO).unwrap(),
                sample_at,
            )?;
            clocks.own_discrete_real(clock.into(), z, z_definition_at)?;
            clocks.own_discrete_value(clock.into(), valid, valid_definition_at)?;
            Ok(clock)
        })?;
        let guard = dae.conditions(|conditions| conditions.reserve(sample_at))?;
        dae.conditions(|conditions| {
            conditions.define(guard, ConditionInput::Clock(clock.into()), sample_at)
        })?;
        let (z_value, valid_value) = dae.expressions(|expressions| {
            Ok((
                expressions
                    .at(z_definition_at)
                    .literal(DaeLiteral::Real(1.0))?,
                expressions
                    .at(valid_definition_at)
                    .literal(DaeLiteral::Boolean(true))?,
            ))
        })?;
        dae.discrete(|discrete| {
            discrete.when_real_equation(guard, guard, z_definition_at, |equation| {
                equation.residual(z_value)
            })
        })?;
        dae.b1c([valid], |topology| {
            topology.owner(owner_at, [valid], |owner| {
                owner.when(
                    guard,
                    guard,
                    sample_at,
                    [(valid_value, valid_definition_at)],
                )
            })?;
            Ok(())
        })?;
        dae.model_events(|events| {
            events.transaction(
                [
                    ModelEventTarget::DiscreteReal(z),
                    ModelEventTarget::DiscreteValue(valid),
                ],
                [ModelEventStep::new(
                    guard,
                    guard,
                    Some(clock.into()),
                    [
                        ModelEventDefinition::new(
                            ModelEventTarget::DiscreteReal(z),
                            z_value,
                            z_definition_at,
                        ),
                        ModelEventDefinition::new(
                            ModelEventTarget::DiscreteValue(valid),
                            valid_value,
                            valid_definition_at,
                        ),
                    ],
                    owner_at,
                )],
                owner_at,
            )
        })?;
        Ok(())
    })
    .expect("one event algorithm remains one checked mixed-role transaction")
}

fn assert_fixture(view: DaeView<'_>) {
    assert_eq!(view.model_event_transaction_count(), 1);
    let transaction = view
        .model_event_transaction(view.model_event_transaction_id(0).unwrap())
        .unwrap();
    assert_eq!(transaction.targets().len(), 2);
    let step = transaction.steps().next().unwrap();
    assert_eq!(step.definitions().len(), 2);
    assert!(step.clock().is_some());
    assert!(matches!(
        step.definitions().next().unwrap().target(),
        ModelEventTarget::DiscreteReal(_)
    ));
}

#[test]
fn mixed_role_transaction_round_trips_through_checked_wire() {
    let dae = fixture();
    dae.inspect(assert_fixture);
    let encoded = serde_json::to_string(&dae).unwrap();
    let decoded: Dae = serde_json::from_str(&encoded).unwrap();
    decoded.inspect(assert_fixture);
}

#[test]
fn wire_replay_rejects_a_forged_target_role() {
    let encoded = serde_json::to_value(fixture()).unwrap();
    let mut forged = encoded;
    forged["storage"]["model_event_transactions"][0]["targets"][0]["kind"] =
        serde_json::json!("discrete_value");
    forged["storage"]["model_event_transactions"][0]["steps"][0]["definitions"][0]["target"]["kind"] =
        serde_json::json!("discrete_value");
    let error = serde_json::from_value::<Dae>(forged).unwrap_err();
    assert!(
        error.to_string().contains("wrong DAE coordinate role"),
        "checked replay rejects a role-forged transaction: {error}"
    );
}

#[test]
fn construction_rejects_an_undeclared_step_target_without_partial_insertion() {
    let dae = fixture();
    dae.inspect(|view| assert_eq!(view.model_event_transaction_count(), 1));
    let mut wire = serde_json::to_value(dae).unwrap();
    wire["storage"]["model_event_transactions"][0]["targets"]
        .as_array_mut()
        .unwrap()
        .pop();
    let error = serde_json::from_value::<Dae>(wire).unwrap_err();
    assert!(
        error.to_string().contains("was not declared by its owner"),
        "step definitions must belong to the transaction target set: {error}"
    );
}

#[test]
fn wire_replay_rejects_a_second_transaction_owner_for_one_target() {
    let mut wire = serde_json::to_value(fixture()).unwrap();
    let duplicate = wire["storage"]["model_event_transactions"][0].clone();
    wire["storage"]["model_event_transactions"]
        .as_array_mut()
        .unwrap()
        .push(duplicate);
    let error = serde_json::from_value::<Dae>(wire).unwrap_err();
    assert!(
        error
            .to_string()
            .contains("model-event transaction target owner"),
        "one mutable target has one transaction owner: {error}"
    );
}
