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
fn wire_replays_interleaved_equation_family_owners_by_global_body_cursor() {
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
        serde_json::from_str(&encoded).expect("global family-body insertion order round trips");
    assert_eq!(serde_json::to_string(&decoded).unwrap(), encoded);

    let mut repeated: serde_json::Value = serde_json::from_str(&encoded).unwrap();
    repeated["storage"]["initialization_families"][0]["bodies"]["start"] = 0.into();
    assert!(
        serde_json::from_value::<Dae>(repeated).is_err(),
        "a family cannot reuse a body segment owned by an earlier system"
    );

    let mut trailing: serde_json::Value = serde_json::from_str(&encoded).unwrap();
    trailing["storage"]["equation_family_bodies"]
        .as_array_mut()
        .unwrap()
        .push(1.into());
    assert!(
        serde_json::from_value::<Dae>(trailing).is_err(),
        "every packed family body must be consumed exactly once"
    );
}
