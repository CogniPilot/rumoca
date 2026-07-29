use super::*;

#[test]
fn wire_rejects_repeated_expression_buffer_ranges() {
    let source = TestSource::new("Real a[2] = {1,2}; Real b[2] = {1,2}; a[:]; a[:];");
    let one_at = source.source("1", 0);
    let two_at = source.source("2", 0);
    let first_array_at = source.source("{1,2}", 0);
    let second_array_at = source.source("{1,2}", 1);
    let first_index_at = source.source("a[:]", 0);
    let second_index_at = source.source("a[:]", 1);
    let dae = Dae::construct(source.map, |dae| {
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
    .expect("packed-buffer fixture constructs");

    let encoded = serde_json::to_string(&dae).unwrap();
    let decoded: Dae = serde_json::from_str(&encoded).expect("canonical packed ranges round trip");
    assert_eq!(serde_json::to_string(&decoded).unwrap(), encoded);

    let mut repeated_operands: serde_json::Value = serde_json::from_str(&encoded).unwrap();
    let mut arrays = repeated_operands["storage"]["expressions"]["nodes"]
        .as_array_mut()
        .unwrap()
        .iter_mut()
        .filter_map(|node| node.get_mut("array"))
        .collect::<Vec<_>>();
    let first_operand_start = arrays[0]["operands"]["start"].clone();
    arrays[1]["operands"]["start"] = first_operand_start;
    assert!(
        serde_json::from_value::<Dae>(repeated_operands).is_err(),
        "an operand range cannot replay an already-consumed packed segment"
    );

    let mut repeated_subscripts: serde_json::Value = serde_json::from_str(&encoded).unwrap();
    let mut indices = repeated_subscripts["storage"]["expressions"]["nodes"]
        .as_array_mut()
        .unwrap()
        .iter_mut()
        .filter_map(|node| node.get_mut("index"))
        .collect::<Vec<_>>();
    let first_subscript_start = indices[0]["subscripts"]["start"].clone();
    indices[1]["subscripts"]["start"] = first_subscript_start;
    assert!(
        serde_json::from_value::<Dae>(repeated_subscripts).is_err(),
        "a subscript range cannot replay an already-consumed packed segment"
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
