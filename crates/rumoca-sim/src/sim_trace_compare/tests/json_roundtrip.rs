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
            false,
            None,
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
