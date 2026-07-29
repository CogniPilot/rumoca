use super::*;

#[test]
fn wire_derives_packed_buffer_offsets_from_readable_counts() {
    let dae = packed_buffer_fixture();
    let encoded = serde_json::to_string(&dae).unwrap();
    let decoded: Dae = serde_json::from_str(&encoded).expect("canonical packed counts round trip");
    assert_eq!(serde_json::to_string(&decoded).unwrap(), encoded);
    let binary = bincode::serialize(&dae).unwrap();
    let decoded: Dae = bincode::deserialize(&binary).expect("binary packed counts replay");
    assert_eq!(bincode::serialize(&decoded).unwrap(), binary);

    let canonical: serde_json::Value = serde_json::from_str(&encoded).unwrap();
    let arrays = canonical["storage"]["expressions"]["nodes"]
        .as_array()
        .unwrap()
        .iter()
        .filter_map(|node| node.get("array"))
        .collect::<Vec<_>>();
    assert_eq!(arrays[0]["operand_count"], 2);
    assert_eq!(arrays[1]["operand_count"], 2);
    let indices = canonical["storage"]["expressions"]["nodes"]
        .as_array()
        .unwrap()
        .iter()
        .filter_map(|node| node.get("index"))
        .collect::<Vec<_>>();
    assert_eq!(indices[0]["subscript_count"], 1);
    assert_eq!(indices[1]["subscript_count"], 1);

    assert_malformed_packed_counts_rejected(canonical);
}

fn packed_buffer_fixture() -> Dae {
    let source = TestSource::new("Real a[2] = {1,2}; Real b[2] = {1,2}; a[:]; a[:];");
    let one_at = source.source("1", 0);
    let two_at = source.source("2", 0);
    let first_array_at = source.source("{1,2}", 0);
    let second_array_at = source.source("{1,2}", 1);
    let first_index_at = source.source("a[:]", 0);
    let second_index_at = source.source("a[:]", 1);
    Dae::construct(source.map, |dae| {
        dae.expressions(|expressions| {
            let one = expressions.at(one_at).literal(DaeLiteral::Real(1.0))?;
            let two = expressions.at(two_at).literal(DaeLiteral::Real(2.0))?;
            let first = expressions.at(first_array_at).array([one, two])?;
            expressions.at(second_array_at).array([one, two])?;
            expressions.at(first_index_at).index(
                first,
                [Subscript::Whole {
                    provenance: first_index_at,
                }],
            )?;
            expressions.at(second_index_at).index(
                first,
                [Subscript::Whole {
                    provenance: second_index_at,
                }],
            )?;
            Ok(())
        })
    })
    .expect("packed-buffer fixture constructs")
}

fn assert_malformed_packed_counts_rejected(canonical: serde_json::Value) {
    let mut old_name = canonical.clone();
    let first_array = old_name["storage"]["expressions"]["nodes"]
        .as_array_mut()
        .unwrap()
        .iter_mut()
        .find_map(|node| node.get_mut("array"))
        .unwrap()
        .as_object_mut()
        .unwrap();
    first_array.remove("operand_count");
    first_array.insert(
        "operands".to_owned(),
        serde_json::json!({"start": 0, "len": 2}),
    );
    assert!(serde_json::from_value::<Dae>(old_name).is_err());

    let mut old_name = canonical.clone();
    let first_index = old_name["storage"]["expressions"]["nodes"]
        .as_array_mut()
        .unwrap()
        .iter_mut()
        .find_map(|node| node.get_mut("index"))
        .unwrap()
        .as_object_mut()
        .unwrap();
    first_index.remove("subscript_count");
    first_index.insert(
        "subscripts".to_owned(),
        serde_json::json!({"start": 0, "len": 1}),
    );
    assert!(serde_json::from_value::<Dae>(old_name).is_err());

    let mut old_range = canonical.clone();
    let first_array = old_range["storage"]["expressions"]["nodes"]
        .as_array_mut()
        .unwrap()
        .iter_mut()
        .find_map(|node| node.get_mut("array"))
        .unwrap();
    first_array["operand_count"] = serde_json::json!({"start": 0, "len": 2});
    assert!(
        serde_json::from_value::<Dae>(old_range).is_err(),
        "wire-v11 rejects the removed packed-range representation"
    );

    let mut oversized = canonical.clone();
    let first_array = oversized["storage"]["expressions"]["nodes"]
        .as_array_mut()
        .unwrap()
        .iter_mut()
        .find_map(|node| node.get_mut("array"))
        .unwrap();
    first_array["operand_count"] = u32::MAX.into();
    assert!(
        serde_json::from_value::<Dae>(oversized).is_err(),
        "an operand count cannot exceed the remaining packed buffer"
    );

    let mut oversized = canonical.clone();
    let first_index = oversized["storage"]["expressions"]["nodes"]
        .as_array_mut()
        .unwrap()
        .iter_mut()
        .find_map(|node| node.get_mut("index"))
        .unwrap();
    first_index["subscript_count"] = 2.into();
    assert!(
        serde_json::from_value::<Dae>(oversized).is_err(),
        "a subscript count cannot consume a later operation's payload"
    );

    let mut shortened = canonical.clone();
    let second_array = shortened["storage"]["expressions"]["nodes"]
        .as_array_mut()
        .unwrap()
        .iter_mut()
        .filter_map(|node| node.get_mut("array"))
        .nth(1)
        .unwrap();
    second_array["operand_count"] = 1.into();
    assert!(
        serde_json::from_value::<Dae>(shortened).is_err(),
        "short counts cannot leave packed payload unconsumed"
    );

    let mut trailing = canonical;
    trailing["storage"]["expressions"]["operands"]
        .as_array_mut()
        .unwrap()
        .push(0.into());
    assert!(
        serde_json::from_value::<Dae>(trailing).is_err(),
        "every packed operand must be consumed exactly once"
    );
}

#[test]
fn wire_replays_ordered_equation_owner_operations_without_global_body_state() {
    let source = TestSource::new(
        "for i in 1:2 loop x = {0,0}; end for; initial equation x = {0,0}; x = {0,0};",
    );
    let continuous_at = source.source("for i in 1:2 loop", 0);
    let initialization_at = source.source("initial equation", 0);
    let zero_at = source.source("0", 0);
    let dae = Dae::construct(source.map, |dae| {
        let domain = dae.domains(|domains| {
            domains.structured(
                StructuredIndexDomain {
                    binders: vec![StructuredIndexBinder {
                        id: 0,
                        display_name: "i".to_owned(),
                        lower: 1,
                        upper: 2,
                        step: 1,
                    }],
                },
                continuous_at,
            )
        })?;
        let body = dae.expressions(|expressions| {
            let zero = expressions.at(zero_at).literal(DaeLiteral::Real(0.0))?;
            expressions.at(continuous_at).array([zero, zero])
        })?;
        dae.continuous(|continuous| {
            continuous.structured_family(
                continuous_at,
                domain,
                rumoca_core::ComprehensionScalarView::RowMajorProjection,
                |family| family.body(body),
            )
        })?;
        dae.initialization(|initialization| {
            initialization.structured_family(
                initialization_at,
                domain,
                rumoca_core::ComprehensionScalarView::RowMajorProjection,
                |family| family.body(body),
            )
        })?;
        dae.continuous(|continuous| {
            continuous.structured_family(
                continuous_at,
                domain,
                rumoca_core::ComprehensionScalarView::RowMajorProjection,
                |family| family.body(body),
            )
        })?;
        Ok(())
    })
    .expect("cross-system family insertion constructs");

    let encoded = serde_json::to_string(&dae).unwrap();
    let decoded: Dae =
        serde_json::from_str(&encoded).expect("ordered equation operations round trip");
    assert_eq!(serde_json::to_string(&decoded).unwrap(), encoded);
    let binary = bincode::serialize(&dae).unwrap();
    let decoded: Dae = bincode::deserialize(&binary).expect("binary equation operations replay");
    assert_eq!(bincode::serialize(&decoded).unwrap(), binary);

    let canonical: serde_json::Value = serde_json::from_str(&encoded).unwrap();
    let continuous = canonical["storage"]["continuous_equation_operations"]
        .as_array()
        .unwrap();
    let initialization = canonical["storage"]["initialization_equation_operations"]
        .as_array()
        .unwrap();
    assert_eq!(continuous.len(), 2);
    assert_eq!(initialization.len(), 1);
    assert!(
        continuous
            .iter()
            .all(|operation| operation.get("structured").is_some()),
        "continuous owner insertion order is explicit"
    );

    let mut unknown_body = canonical;
    unknown_body["storage"]["initialization_equation_operations"][0]["structured"]["bodies"][0] =
        u32::MAX.into();
    assert!(
        serde_json::from_value::<Dae>(unknown_body).is_err(),
        "structured operation bodies must name constructed expressions"
    );
}
