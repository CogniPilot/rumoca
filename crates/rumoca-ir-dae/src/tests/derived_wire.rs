use super::*;

#[test]
fn wire_omits_constructor_derived_facts_and_round_trips_canonically() {
    let dae = derived_wire_fixture();
    let encoded = serde_json::to_string(&dae).expect("checked DAE serializes");
    let wire: serde_json::Value = serde_json::from_str(&encoded).unwrap();
    let storage = &wire["storage"];
    let expressions = storage["expressions"].as_object().unwrap();
    for field in [
        "value_types",
        "variability",
        "binder_domains",
        "function_scopes",
        "type_anchors",
    ] {
        assert!(
            !expressions.contains_key(field),
            "{field} is constructor-derived"
        );
    }
    let record = nodes_of(&wire, "record").remove(0);
    assert!(
        record["value_type"].is_u64(),
        "a record names the type it was constructed with as an operand of that node"
    );
    let arrays = nodes_of(&wire, "array");
    assert_eq!(
        arrays.len(),
        2,
        "the fixture builds an empty and a full array"
    );
    let (empty, populated): (Vec<_>, Vec<_>) = arrays
        .into_iter()
        .partition(|array| array["operand_count"] == 0);
    assert!(
        empty[0]["value_type"].is_u64(),
        "an empty array carries the element type its operands cannot infer"
    );
    assert!(
        populated[0]["value_type"].is_null(),
        "a populated array infers its type from operands and restates nothing"
    );
    for field in ["extents", "scalar_count"] {
        assert!(
            !storage["domains"][0]
                .as_object()
                .unwrap()
                .contains_key(field)
        );
    }
    assert!(
        !storage["continuous_equation_operations"][0]["structured"]
            .as_object()
            .unwrap()
            .contains_key("scalar_rows")
    );
    assert!(
        !storage["clock_ownerships"][0]
            .as_object()
            .unwrap()
            .contains_key("role")
    );
    for field in ["role", "value_type"] {
        assert!(
            !storage["previous_values"][0]
                .as_object()
                .unwrap()
                .contains_key(field)
        );
    }
    for field in ["value_type", "variability"] {
        assert!(
            !storage["delays"][0]
                .as_object()
                .unwrap()
                .contains_key(field)
        );
    }

    let decoded: Dae = serde_json::from_str(&encoded).expect("derived facts reconstruct");
    assert_eq!(serde_json::to_string(&decoded).unwrap(), encoded);
    let binary = bincode::serialize(&dae).unwrap();
    let decoded: Dae = bincode::deserialize(&binary).expect("binary wire reconstructs");
    assert_eq!(bincode::serialize(&decoded).unwrap(), binary);
}

#[test]
fn wire_rejects_removed_derived_fields() {
    let dae = derived_wire_fixture();
    let canonical = serde_json::to_value(&dae).unwrap();
    for (path, field, value) in [
        (
            &["storage", "expressions"][..],
            "value_types",
            serde_json::json!([]),
        ),
        (
            &["storage", "expressions"][..],
            "variability",
            serde_json::json!([]),
        ),
        (
            &["storage", "expressions"][..],
            "binder_domains",
            serde_json::json!([]),
        ),
        (
            &["storage", "expressions"][..],
            "function_scopes",
            serde_json::json!([]),
        ),
        (
            &["storage", "expressions"][..],
            "type_anchors",
            serde_json::json!([]),
        ),
        (
            &["storage", "domains", "0"][..],
            "extents",
            serde_json::json!([]),
        ),
        (
            &["storage", "domains", "0"][..],
            "scalar_count",
            serde_json::json!(1),
        ),
        (
            &[
                "storage",
                "continuous_equation_operations",
                "0",
                "structured",
            ][..],
            "scalar_rows",
            serde_json::json!(1),
        ),
        (
            &["storage", "clock_ownerships", "0"][..],
            "role",
            serde_json::json!("discrete_real"),
        ),
        (
            &["storage", "previous_values", "0"][..],
            "role",
            serde_json::json!("discrete_real"),
        ),
        (
            &["storage", "previous_values", "0"][..],
            "value_type",
            serde_json::json!(0),
        ),
        (
            &["storage", "delays", "0"][..],
            "value_type",
            serde_json::json!(0),
        ),
        (
            &["storage", "delays", "0"][..],
            "variability",
            serde_json::json!("constant"),
        ),
    ] {
        let mut forged = canonical.clone();
        object_at_mut(&mut forged, path).insert(field.to_owned(), value);
        assert!(
            serde_json::from_value::<Dae>(forged).is_err(),
            "removed field {field} must be rejected"
        );
    }
    for removed in [
        "continuous_equations",
        "initialization_equations",
        "continuous_families",
        "initialization_families",
        "continuous_equation_owners",
        "initialization_equation_owners",
        "equation_family_bodies",
    ] {
        let mut forged = canonical.clone();
        forged["storage"]
            .as_object_mut()
            .unwrap()
            .insert(removed.to_owned(), serde_json::json!([]));
        assert!(
            serde_json::from_value::<Dae>(forged).is_err(),
            "removed equation mirror {removed} must be rejected"
        );
    }
}

#[test]
fn wire_rejects_the_superseded_type_anchor_column() {
    let dae = derived_wire_fixture();
    let canonical = serde_json::to_value(&dae).unwrap();

    let mut side_table = canonical.clone();
    side_table["storage"]["expressions"]
        .as_object_mut()
        .unwrap()
        .insert(
            "type_anchors".to_owned(),
            serde_json::json!([{"expression": 2, "value_type": 1}]),
        );
    assert!(
        serde_json::from_value::<Dae>(side_table).is_err(),
        "the removed positional anchor table must not decode alongside node operands"
    );

    let mut anchored_literal = canonical;
    node_mut(&mut anchored_literal, "literal")
        .as_object_mut()
        .unwrap()
        .insert("value_type".to_owned(), serde_json::json!(0));
    assert!(
        serde_json::from_value::<Dae>(anchored_literal).is_err(),
        "a node that infers its own type cannot carry a type operand"
    );
}

#[test]
fn wire_rejects_forged_and_missing_node_type_operands() {
    let dae = derived_wire_fixture();
    let canonical = serde_json::to_value(&dae).unwrap();

    let mut missing_record_type = canonical.clone();
    node_mut(&mut missing_record_type, "record")
        .as_object_mut()
        .unwrap()
        .remove("value_type")
        .expect("a record states its constructed type");
    assert!(
        serde_json::from_value::<Dae>(missing_record_type).is_err(),
        "a record cannot omit the type operand its construction requires"
    );

    let mut forged_record_type = canonical.clone();
    node_mut(&mut forged_record_type, "record")["value_type"] = serde_json::json!(0);
    assert!(
        serde_json::from_value::<Dae>(forged_record_type).is_err(),
        "a record cannot claim a type its operands do not build"
    );

    let mut unknown_record_type = canonical.clone();
    node_mut(&mut unknown_record_type, "record")["value_type"] = u32::MAX.into();
    assert!(
        serde_json::from_value::<Dae>(unknown_record_type).is_err(),
        "a record type operand must name a constructed value type"
    );

    let mut empty_without_type = canonical.clone();
    let empty = array_node_mut(&mut empty_without_type, true);
    empty["value_type"] = serde_json::Value::Null;
    assert!(
        serde_json::from_value::<Dae>(empty_without_type).is_err(),
        "an empty array cannot drop the element type nothing else supplies"
    );

    let mut restated_type = canonical;
    let populated = array_node_mut(&mut restated_type, false);
    populated["value_type"] = serde_json::json!(0);
    assert!(
        serde_json::from_value::<Dae>(restated_type).is_err(),
        "a populated array cannot restate a type its operands already prove"
    );
}

#[test]
fn wire_replays_variable_role_type_checks() {
    let dae = derived_wire_fixture();
    let mut forged = serde_json::to_value(&dae).unwrap();
    forged["storage"]["value_types"][0]["scalar"] = serde_json::json!("boolean");

    let error = serde_json::from_value::<Dae>(forged)
        .expect_err("wire cannot forge a Boolean discrete-Real coordinate")
        .to_string();
    assert!(
        error.contains("variable `z` of type Boolean cannot be a DiscreteReal DAE coordinate"),
        "unexpected checked-wire failure: {error}"
    );
}

fn nodes_of<'value>(wire: &'value serde_json::Value, kind: &str) -> Vec<&'value serde_json::Value> {
    wire["storage"]["expressions"]["nodes"]
        .as_array()
        .unwrap()
        .iter()
        .filter_map(|node| node.get(kind))
        .collect()
}

fn node_mut<'value>(
    wire: &'value mut serde_json::Value,
    kind: &str,
) -> &'value mut serde_json::Value {
    wire["storage"]["expressions"]["nodes"]
        .as_array_mut()
        .unwrap()
        .iter_mut()
        .find_map(|node| node.get_mut(kind))
        .expect("fixture contains the requested node kind")
}

fn array_node_mut(wire: &mut serde_json::Value, empty: bool) -> &mut serde_json::Value {
    wire["storage"]["expressions"]["nodes"]
        .as_array_mut()
        .unwrap()
        .iter_mut()
        .filter_map(|node| node.get_mut("array"))
        .find(|array| (array["operand_count"] == 0) == empty)
        .expect("fixture contains both an empty and a populated array")
}

fn object_at_mut<'value>(
    value: &'value mut serde_json::Value,
    path: &[&str],
) -> &'value mut serde_json::Map<String, serde_json::Value> {
    let mut current = value;
    for component in path {
        current = if let Ok(index) = component.parse::<usize>() {
            &mut current[index]
        } else {
            &mut current[*component]
        };
    }
    current.as_object_mut().unwrap()
}

fn derived_wire_fixture() -> Dae {
    let source = TestSource::new(
        "record Pair Real a; Real b; end Pair; Pair(1,2); Real empty[0]={}; \
         discrete Real z; previous(z); delay(1,1); for k in 1:2 loop k + 0.0 = 0; end for;",
    );
    let owner = source.source("record Pair", 0);
    let pair_at = source.source("Pair(1,2)", 0);
    let empty_at = source.source("{}", 0);
    let z_at = source.source("discrete Real z", 0);
    let delay_at = source.source("delay(1,1)", 0);
    let loop_at = source.source("for k in 1:2 loop", 0);
    Dae::construct(source.map, |dae| {
        let real = dae.types(|types| types.derived(ValueType::scalar(ScalarType::Real), owner))?;
        let pair = dae.types(|types| {
            types.record(
                VarName::new("Pair"),
                [(VarName::new("a"), real), (VarName::new("b"), real)],
                owner,
            )
        })?;
        let empty =
            dae.types(|types| types.derived(ValueType::array(ScalarType::Real, [0]), empty_at))?;
        let z = dae.variables(|variables| {
            variables.discrete_real(VarName::new("z"), real, z_at, VariableAttributes::default())
        })?;
        let clock = dae.clocks(|clocks| {
            let clock = clocks.periodic(
                ClockLattice::new(ClockRational::ONE, ClockRational::ZERO).unwrap(),
                z_at,
            )?;
            clocks.own_discrete_real(clock.into(), z, z_at)?;
            Ok(clock)
        })?;
        dae.temporal(|temporal| temporal.previous_discrete_real(clock.into(), z, z_at))?;
        let (zero, one) = dae.expressions(|expressions| {
            let zero = expressions.at(pair_at).literal(DaeLiteral::Real(0.0))?;
            let one = expressions.at(pair_at).literal(DaeLiteral::Real(1.0))?;
            expressions.at(pair_at).record(pair, [one, one])?;
            expressions.at(empty_at).empty_array(empty)?;
            expressions.at(pair_at).array([one, one])?;
            Ok((zero, one))
        })?;
        let positive = dae.temporal(|temporal| temporal.positive_parameter(one, 1.0, delay_at))?;
        dae.expressions(|expressions| expressions.at(delay_at).delay(one, positive, delay_at))?;
        let domain = dae.domains(|domains| {
            domains.structured(
                StructuredIndexDomain {
                    binders: vec![StructuredIndexBinder {
                        id: 0,
                        display_name: "k".to_owned(),
                        lower: 1,
                        upper: 2,
                        step: 1,
                    }],
                },
                loop_at,
            )
        })?;
        let binder = dae.domains(|domains| domains.binder(domain, 0, loop_at))?;
        let body = dae.expressions(|expressions| {
            let binder = expressions.at(loop_at).binder(binder)?;
            expressions
                .at(loop_at)
                .binary(BinaryOperator::Add, binder, zero)
        })?;
        dae.continuous(|continuous| {
            continuous.structured_family(
                loop_at,
                domain,
                rumoca_core::ComprehensionScalarView::BinderSubstitution,
                |family| family.body(body),
            )
        })?;
        Ok(())
    })
    .expect("derived-wire fixture constructs")
}
