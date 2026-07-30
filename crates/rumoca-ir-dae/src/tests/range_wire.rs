use super::*;

#[test]
fn ranges_retain_bound_occurrences_and_explicit_step_syntax() {
    let (dae, omitted_at, explicit_at, generated_at) = range_fixture();
    dae.inspect(|view| {
        let ranges = (0..view.expression_count())
            .filter_map(|index| view.expression_id(index))
            .filter_map(|id| view.expression(id))
            .filter(|expression| expression.kind() == ExpressionKind::Range)
            .collect::<Vec<_>>();
        assert_eq!(ranges.len(), 3);

        let ExpressionOperation::Range(omitted) = ranges[0].operation() else {
            unreachable!("selected expression is a range")
        };
        assert_eq!(omitted.start().value(), 1);
        assert_eq!(omitted.stop().value(), 1);
        assert_eq!(omitted.explicit_step().map(|step| step.value()), None);
        assert_eq!(omitted.effective_step(), 1);
        assert_eq!(omitted.start().provenance(), omitted_at.0);
        assert_eq!(omitted.stop().provenance(), omitted_at.1);
        assert_eq!(ranges[0].provenance(), omitted_at.2);
        assert_ne!(
            omitted.start().expression(),
            omitted.stop().expression(),
            "equal-valued bounds remain distinct source occurrences"
        );

        let ExpressionOperation::Range(explicit) = ranges[1].operation() else {
            unreachable!("selected expression is a range")
        };
        let step = explicit.explicit_step().expect("source wrote a step");
        assert_eq!(step.value(), 1);
        assert_eq!(explicit.effective_step(), 1);
        assert_eq!(explicit.start().provenance(), explicit_at.0);
        assert_eq!(step.provenance(), explicit_at.1);
        assert_eq!(explicit.stop().provenance(), explicit_at.2);
        assert_eq!(ranges[1].provenance(), explicit_at.3);

        let ExpressionOperation::Range(generated) = ranges[2].operation() else {
            unreachable!("selected expression is a range")
        };
        let generated_step = generated
            .explicit_step()
            .expect("the generated range retains its explicit step");
        assert_eq!(generated_step.value(), 2);
        for provenance in [
            generated.start().provenance(),
            generated_step.provenance(),
            generated.stop().provenance(),
            ranges[2].provenance(),
        ] {
            assert_eq!(
                provenance.origin(),
                DaeProvenanceOrigin::Generated(DaeGeneration::IndexReduction)
            );
            assert_eq!(provenance.span(), generated_at.span());
        }
    });
}

#[test]
fn range_wire_round_trips_and_rejects_noncanonical_shapes() {
    let (dae, _, _, _) = range_fixture();
    let json = serde_json::to_string(&dae).unwrap();
    let decoded: Dae = serde_json::from_str(&json).expect("range JSON replays construction");
    assert_eq!(serde_json::to_string(&decoded).unwrap(), json);

    let binary = bincode::serialize(&dae).expect("range bincode serializes");
    let decoded: Dae = bincode::deserialize(&binary).expect("range bincode replays construction");
    assert_eq!(bincode::serialize(&decoded).unwrap(), binary);

    let canonical: serde_json::Value = serde_json::from_str(&json).unwrap();
    let omitted = range_node(&canonical, 0);
    assert_eq!(omitted["start_expression"], 0);
    assert!(omitted["explicit_step_expression"].is_null());
    assert_eq!(omitted["stop_expression"], 1);
    for removed in ["start", "step", "stop"] {
        assert!(omitted.get(removed).is_none());
    }

    let mut missing_step_field = canonical.clone();
    range_node_mut(&mut missing_step_field, 0)
        .as_object_mut()
        .unwrap()
        .remove("explicit_step_expression");
    assert!(
        serde_json::from_value::<Dae>(missing_step_field).is_err(),
        "an omitted step is encoded as required explicit null"
    );

    let mut removed_raw_shape = canonical.clone();
    *range_node_mut(&mut removed_raw_shape, 0) = serde_json::json!({
        "start": 1,
        "step": 1,
        "stop": 3,
    });
    assert!(
        serde_json::from_value::<Dae>(removed_raw_shape).is_err(),
        "wire-v11 rejects the removed raw-value range shape"
    );

    let mut future_bound = canonical;
    range_node_mut(&mut future_bound, 0)["start_expression"] = 2.into();
    assert!(
        serde_json::from_value::<Dae>(future_bound).is_err(),
        "wire replay rejects a range bound that is not already constructed"
    );

    let mut non_integer_bound: serde_json::Value = serde_json::from_str(&json).unwrap();
    range_node_mut(&mut non_integer_bound, 1)["start_expression"] = 2.into();
    assert!(
        serde_json::from_value::<Dae>(non_integer_bound).is_err(),
        "wire replay rejects an already-mapped expression that is not an Integer literal"
    );
}

#[test]
fn range_construction_reports_the_exact_bad_bound_occurrence() {
    let source = TestSource::new("1:0:3; 1.0:3;");
    let zero_at = source.source("0", 0);
    let zero_range_at = source.source("1:0:3", 0);
    let real_at = source.source("1.0", 0);
    let invalid_range_at = source.source("1.0:3", 0);
    let one_at = source.source("1", 0);
    let first_stop_at = source.source("3", 0);
    let second_stop_at = source.source("3", 1);
    let dae = Dae::construct(source.map, |dae| {
        dae.expressions(|expressions| {
            let one = expressions.at(one_at).literal(DaeLiteral::Integer(1))?;
            let zero = expressions.at(zero_at).literal(DaeLiteral::Integer(0))?;
            let first_stop = expressions
                .at(first_stop_at)
                .literal(DaeLiteral::Integer(3))?;
            let error = expressions
                .at(zero_range_at)
                .range(one, Some(zero), first_stop)
                .expect_err("a zero step is rejected before parent insertion");
            assert_eq!(
                error,
                DaeConstructionError::ZeroRangeStep {
                    span: zero_at.span()
                }
            );

            let real = expressions.at(real_at).literal(DaeLiteral::Real(1.0))?;
            let second_stop = expressions
                .at(second_stop_at)
                .literal(DaeLiteral::Integer(3))?;
            let error = expressions
                .at(invalid_range_at)
                .range(real, None, second_stop)
                .expect_err("a range cannot accept a merely numeric expression");
            assert_eq!(
                error,
                DaeConstructionError::InvalidRangeBound {
                    span: real_at.span()
                }
            );
            Ok(())
        })
    })
    .expect("rejected ranges leave their checked child occurrences available");
    dae.inspect(|view| assert_eq!(view.expression_count(), 5));
}

type OmittedProvenance = (DaeProvenance, DaeProvenance, DaeProvenance);
type ExplicitProvenance = (DaeProvenance, DaeProvenance, DaeProvenance, DaeProvenance);

fn range_fixture() -> (Dae, OmittedProvenance, ExplicitProvenance, DaeProvenance) {
    let source = TestSource::new("1:1; 4:1:6; generated 7:2:9;");
    let omitted = (
        source.source("1", 0),
        source.source("1", 1),
        source.source("1:1", 0),
    );
    let explicit = (
        source.source("4", 0),
        source.source("1", 2),
        source.source("6", 0),
        source.source("4:1:6", 0),
    );
    let generated =
        DaeProvenance::generated(DaeGeneration::IndexReduction, source.span("7:2:9", 0)).unwrap();
    let dae = Dae::construct(source.map, |dae| {
        dae.expressions(|expressions| {
            let start = expressions.at(omitted.0).literal(DaeLiteral::Integer(1))?;
            let stop = expressions.at(omitted.1).literal(DaeLiteral::Integer(1))?;
            expressions.at(omitted.2).range(start, None, stop)?;

            let start = expressions.at(explicit.0).literal(DaeLiteral::Integer(4))?;
            let step = expressions.at(explicit.1).literal(DaeLiteral::Integer(1))?;
            let stop = expressions.at(explicit.2).literal(DaeLiteral::Integer(6))?;
            expressions.at(explicit.3).range(start, Some(step), stop)?;

            let start = expressions.at(generated).literal(DaeLiteral::Integer(7))?;
            let step = expressions.at(generated).literal(DaeLiteral::Integer(2))?;
            let stop = expressions.at(generated).literal(DaeLiteral::Integer(9))?;
            expressions.at(generated).range(start, Some(step), stop)?;
            Ok(())
        })
    })
    .unwrap();
    (dae, omitted, explicit, generated)
}

fn range_node(wire: &serde_json::Value, ordinal: usize) -> &serde_json::Value {
    wire["storage"]["expressions"]["nodes"]
        .as_array()
        .unwrap()
        .iter()
        .filter_map(|node| node.get("range"))
        .nth(ordinal)
        .unwrap()
}

fn range_node_mut(wire: &mut serde_json::Value, ordinal: usize) -> &mut serde_json::Value {
    wire["storage"]["expressions"]["nodes"]
        .as_array_mut()
        .unwrap()
        .iter_mut()
        .filter_map(|node| node.get_mut("range"))
        .nth(ordinal)
        .unwrap()
}
