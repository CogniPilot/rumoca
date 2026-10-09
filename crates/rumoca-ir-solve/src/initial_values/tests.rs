use super::*;

#[test]
fn source_writes_invalidate_only_the_derived_dense_view_and_keep_failed_writes_atomic() {
    let mut values = SolveInitialValues::repeat(-0.0, 12).unwrap();
    assert!(
        values
            .as_slice()
            .iter()
            .all(|v| v.to_bits() == (-0.0f64).to_bits())
    );
    let original = values.clone();
    let before = serde_json::to_value(&values).unwrap();
    assert!(values.replace(12, &vec![1.0].into()).is_err());
    assert_eq!(serde_json::to_value(&values).unwrap(), before);
    assert!(values.has_dense_view());
    values.set(4, 3.0).unwrap();
    assert!(!values.has_dense_view());
    assert_eq!(values.as_slice()[4].to_bits(), 3.0f64.to_bits());
    assert_eq!(original.value(4).unwrap().to_bits(), (-0.0f64).to_bits());
    assert!(!original.has_dense_view());
}

#[test]
fn finite_initialization_admission_reads_source_runs_without_dense_views() {
    for value in [0.0, -0.0, 1.0] {
        let values = SolveInitialValues::repeat(value, 200_000).unwrap();
        assert!(values.require_finite().is_ok());
        assert!(!values.has_dense_view());
    }
    for value in [
        f64::INFINITY,
        f64::NEG_INFINITY,
        f64::from_bits(0x7ff8_0000_0000_1234),
    ] {
        for values in [
            SolveInitialValues::repeat(value, 200_000).unwrap(),
            vec![0.0, value].into(),
        ] {
            assert!(values.require_finite().is_err());
            assert!(!values.has_dense_view());
        }
    }
}

#[test]
fn source_repeat_is_compact_and_dense_views_preserve_all_bits() {
    for bits in [0u64, 1u64 << 63, 0x7ff8_0000_0000_1234] {
        let values = SolveInitialValues::repeat(f64::from_bits(bits), 200_000).unwrap();
        assert_eq!(values.run_count(), 1);
        assert!(!values.has_dense_view());
        let wire = serde_json::to_string(&values).unwrap();
        assert!(wire.len() < 160);
        assert!(!values.has_dense_view());
        let replay: SolveInitialValues = serde_json::from_str(&wire).unwrap();
        assert_eq!(replay.run(0).unwrap().repeated_bits(), Some(bits));
        assert!(!replay.has_dense_view());
        assert!(
            replay
                .as_slice()
                .iter()
                .all(|value| value.to_bits() == bits)
        );
        assert!(!replay.clone().has_dense_view());
    }
}

#[test]
fn checked_source_writes_preserve_prefix_suffix_and_literal_payloads() {
    let mut values = SolveInitialValues::repeat(-0.0, 12).unwrap();
    let source: SolveInitialValues = vec![1.0, f64::from_bits(0x7ff8_0000_0000_0012), -0.0].into();
    values.replace(4, &source).unwrap();
    values.set(0, 3.0).unwrap();
    let expected = [
        3.0,
        -0.0,
        -0.0,
        -0.0,
        1.0,
        f64::from_bits(0x7ff8_0000_0000_0012),
        -0.0,
        -0.0,
        -0.0,
        -0.0,
        -0.0,
        -0.0,
    ];
    assert!(!values.has_dense_view());
    for (index, expected) in expected.iter().enumerate() {
        assert_eq!(values.value(index).unwrap().to_bits(), expected.to_bits());
    }
    let before = serde_json::to_value(&values).unwrap();
    assert!(values.replace(11, &source).is_err());
    assert!(values.set(usize::MAX, 0.0).is_err());
    assert_eq!(serde_json::to_value(&values).unwrap(), before);
    assert!(!values.has_dense_view());
}

#[test]
fn wire_refuses_missing_reordered_duplicated_and_overflowing_runs() {
    let values = SolveInitialValues::concatenate([
        SolveInitialValues::repeat(1.0, 3).unwrap(),
        vec![2.0, -0.0].into(),
    ])
    .unwrap();
    let wire = serde_json::to_value(values).unwrap();
    let mut omitted = wire.clone();
    omitted["runs"].as_array_mut().unwrap().pop();
    let mut reordered = wire.clone();
    reordered["runs"].as_array_mut().unwrap().swap(0, 1);
    let mut duplicated = wire.clone();
    duplicated["runs"]
        .as_array_mut()
        .unwrap()
        .push(wire["runs"][0].clone());
    let mut wrong_capacity = wire.clone();
    wrong_capacity["count"] = 4.into();
    let mut zero = wire.clone();
    zero["runs"][0]["count"] = 0.into();
    for malformed in [omitted, reordered, duplicated, wrong_capacity, zero] {
        assert!(serde_json::from_value::<SolveInitialValues>(malformed).is_err());
    }
    assert!(SolveInitialValues::repeat(0.0, usize::MAX).is_err());
    let empty = SolveInitialValues::repeat(-0.0, 0).unwrap();
    assert_eq!(empty.run_count(), 0);
    let replay: SolveInitialValues =
        serde_json::from_value(serde_json::to_value(empty).unwrap()).unwrap();
    assert!(replay.is_empty());
    assert!(!replay.has_dense_view());
}
