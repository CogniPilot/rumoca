use super::*;

#[test]
fn json_roundtrip_preserves_finite_channel_bits() {
    let values = vec![
        -0.0,
        f64::from_bits(1),
        -f64::MIN_POSITIVE,
        1e-300,
        0.1_f64.next_down(),
        -1.0 / 3.0,
        f64::MAX,
    ];
    let original = trace(
        "numeric_samples",
        (0..values.len()).map(|i| i as f64).collect(),
        vec!["value"],
        vec![values],
    );
    let encoded = serde_json::to_string(&original).unwrap();
    let decoded: SimTrace = serde_json::from_str(&encoded).unwrap();
    let bits = |trace: &SimTrace| {
        trace.data[0]
            .iter()
            .map(|value| value.unwrap().to_bits())
            .collect::<Vec<_>>()
    };
    assert_eq!(bits(&decoded), bits(&original));
}

#[test]
fn json_roundtrip_preserves_distinct_event_coordinates() {
    for event in [0.1_f64, 0.5, 1.0] {
        let original = trace(
            "event_coordinates",
            vec![0.0, event.next_down(), event, 2.0],
            vec!["voltage"],
            vec![vec![0.0, 0.0, 100.0, 100.0]],
        );
        let encoded = serde_json::to_string(&original).unwrap();
        let decoded: SimTrace = serde_json::from_str(&encoded).unwrap();
        assert_eq!(
            decoded
                .times
                .iter()
                .map(|t| t.to_bits())
                .collect::<Vec<_>>(),
            original
                .times
                .iter()
                .map(|t| t.to_bits())
                .collect::<Vec<_>>(),
            "JSON transport must preserve the left limit at {event}"
        );
    }
}

#[test]
fn json_roundtrip_does_not_manufacture_a_continuous_event_ramp() {
    let event = 0.1_f64;
    let original = trace(
        "event_voltage",
        vec![0.0, event.next_down(), event, 1.0],
        vec!["voltage"],
        vec![vec![70.0, 70.0, 0.0, 0.0]],
    );
    let encoded = serde_json::to_string(&original).unwrap();
    let decoded: SimTrace = serde_json::from_str(&encoded).unwrap();
    let reference_times = [0.0, event, event + 1.1e-10, event + 1.1e-10, 1.0];
    let reference_values = [Some(70.0), Some(70.0), Some(70.0), Some(0.0), Some(0.0)];
    let compare = |candidate: &SimTrace| {
        compare_channel(
            "voltage",
            ChannelSeries::new(&candidate.times, &candidate.data[0]),
            ChannelSeries::new(&reference_times, &reference_values),
            ChannelRule::pointwise(false),
        )
        .unwrap()
    };
    let original_metric = compare(&original);
    assert!(original_metric.integral_abs_error < 1e-8);
    assert_eq!(
        compare(&decoded).integral_abs_error,
        original_metric.integral_abs_error
    );
}

/// A room-like pair of traces: a flat channel at `rumoca_flat` and
/// `omc_flat`, and a CO2 concentration both tools agree on.
fn room_traces(rumoca_flat: f64, omc_flat: f64) -> (SimTrace, SimTrace) {
    let times = (0..7).map(|i| 600.0 * f64::from(i)).collect::<Vec<_>>();
    let co2 = times.iter().map(|t| 1.5e-3 - 1e-8 * t).collect::<Vec<_>>();
    let side = |flat: f64| {
        trace(
            "RoomCO2",
            times.clone(),
            vec!["m_flow_turbulent", "C"],
            vec![vec![flat; times.len()], co2.clone()],
        )
    };
    (side(rumoca_flat), side(omc_flat))
}

#[test]
fn a_model_metric_stays_finite_when_finite_samples_overflow_the_integral() {
    // MSL RoomCO2 (#755 MSL shard 3): a disagreement of about f64::MAX
    // integrated over the run overflowed; the bounded score was `inf / inf`,
    // the model mean NaN, and its JSON `null` stopped the quality report. The
    // reference side is below the unbounded magnitude, so it is scored.
    let (rumoca, omc) = room_traces(f64::MAX, 1e59);
    let metric = compare_model_traces("RoomCO2", &rumoca, &omc).unwrap();
    let channel = metric
        .worst_variables
        .iter()
        .find(|channel| channel.name == "m_flow_turbulent")
        .unwrap();
    assert_eq!(
        channel.bounded_normalized_l1_error, 1.0,
        "a disagreement too large to integrate is a full deviation, not hidden"
    );
    assert!(metric.mean_channel_bounded_normalized_l1.is_finite());
    let decoded: ModelDeviationMetric =
        serde_json::from_value(serde_json::to_value(&metric).unwrap())
            .expect("the metric of a finite comparison reads back from its JSON");
    assert_eq!(decoded.compared_variables, 2);
}

#[test]
fn a_channel_unbounded_in_both_traces_is_recorded_and_kept_out_of_the_bands() {
    // `Modelica.Constants.inf` is 1.7976931348623157e308 in MSL's
    // ModelicaServices and 1e60 in OpenModelica's: both say unbounded, and
    // there is no finite error to score. RoomCO2's `m_flow_turbulent`
    // made the model near agreement and failed the merge gate (#755).
    let (rumoca, omc) = room_traces(f64::MAX, 1e60);
    let metric = compare_model_traces("RoomCO2", &rumoca, &omc).unwrap();
    // Read through the JSON the reports read, so the same test runs on the
    // code before this field existed.
    let json = serde_json::to_value(&metric).unwrap();
    assert_eq!(
        json["unbounded_channels"],
        serde_json::json!(["m_flow_turbulent"])
    );
    assert_eq!(
        metric.compared_variables, 1,
        "only the CO2 channel is scored"
    );
    assert_eq!(metric.channel_high_count, 1);
    assert_eq!(metric.channel_severe_count, 0);
    assert!(
        metric
            .worst_variables
            .iter()
            .all(|channel| channel.name != "m_flow_turbulent")
    );
    let decoded: ModelDeviationMetric = serde_json::from_value(json).unwrap();
    assert_eq!(decoded.compared_variables, 1);
}

#[test]
fn a_single_instant_metric_stays_finite_when_its_error_overflows_the_scale() {
    // `experiment(StopTime=0)` with a huge disagreement over a tiny reference:
    // f64::MAX over the 1e-6 scale floor overflowed to inf, and the bounded
    // score was `inf / inf`.
    let rumoca = trace("Instant", vec![0.0], vec!["x"], vec![vec![f64::MAX]]);
    let omc = trace("Instant", vec![0.0, 0.0], vec!["x"], vec![vec![0.0, 0.0]]);
    let metric = compare_model_traces("Instant", &rumoca, &omc).unwrap();
    assert_eq!(metric.compared_variables, 1);
    assert_eq!(metric.max_channel_bounded_normalized_l1, 1.0);
    assert!(metric.mean_channel_bounded_normalized_l1.is_finite());
    serde_json::from_value::<ModelDeviationMetric>(serde_json::to_value(&metric).unwrap())
        .expect("the single-instant metric reads back from its JSON");
}

#[test]
fn a_channel_unbounded_in_opposite_directions_is_scored_as_a_full_deviation() {
    // `+inf` against `-inf` is a disagreement, not two spellings of one
    // unbounded value: the channel stays compared and scores 1.0.
    let (rumoca, omc) = room_traces(f64::MAX, -1e60);
    let metric = compare_model_traces("RoomCO2", &rumoca, &omc).unwrap();
    let json = serde_json::to_value(&metric).unwrap();
    assert!(json.get("unbounded_channels").is_none());
    assert_eq!(metric.compared_variables, 2);
    let channel = metric
        .worst_variables
        .iter()
        .find(|channel| channel.name == "m_flow_turbulent")
        .unwrap();
    assert_eq!(channel.bounded_normalized_l1_error, 1.0);
}
