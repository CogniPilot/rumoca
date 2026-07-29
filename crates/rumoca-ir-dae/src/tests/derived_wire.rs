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
    ] {
        assert!(
            !expressions.contains_key(field),
            "{field} is constructor-derived"
        );
    }
    assert_eq!(expressions["type_anchors"].as_array().unwrap().len(), 2);
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
fn wire_rejects_malformed_type_anchors() {
    let dae = derived_wire_fixture();
    let canonical = serde_json::to_value(&dae).unwrap();

    let anchors = canonical["storage"]["expressions"]["type_anchors"]
        .as_array()
        .unwrap();
    let record_anchor = anchors[0].clone();
    let mut missing = canonical.clone();
    missing["storage"]["expressions"]["type_anchors"]
        .as_array_mut()
        .unwrap()
        .remove(0);
    assert!(serde_json::from_value::<Dae>(missing).is_err());

    let mut wrong_type = canonical.clone();
    wrong_type["storage"]["expressions"]["type_anchors"][0]["value_type"] = serde_json::json!(0);
    assert!(serde_json::from_value::<Dae>(wrong_type).is_err());

    let mut extraneous = canonical;
    let mut literal_anchor = record_anchor;
    literal_anchor["expression"] = serde_json::json!(0);
    extraneous["storage"]["expressions"]["type_anchors"]
        .as_array_mut()
        .unwrap()
        .insert(0, literal_anchor);
    assert!(serde_json::from_value::<Dae>(extraneous).is_err());
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
            clocks.own_discrete_real(clock, z, z_at)?;
            Ok(clock)
        })?;
        dae.temporal(|temporal| temporal.previous_discrete_real(clock, z, z_at))?;
        let (zero, one) = dae.expressions(|expressions| {
            let zero = expressions.at(pair_at).literal(DaeLiteral::Real(0.0))?;
            let one = expressions.at(pair_at).literal(DaeLiteral::Real(1.0))?;
            expressions.at(pair_at).record(pair, [one, one])?;
            expressions.at(empty_at).empty_array(empty)?;
            Ok((zero, one))
        })?;
        dae.temporal(|temporal| {
            let positive = temporal.positive_parameter(one, 1.0, delay_at)?;
            temporal.delay(one, positive, delay_at, delay_at)
        })?;
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
