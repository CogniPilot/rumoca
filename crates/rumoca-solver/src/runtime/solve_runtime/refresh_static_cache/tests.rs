use super::*;

#[test]
fn presence_is_distinct_from_every_ieee_number_and_preserves_payload_bits() {
    let bits = [
        0,
        (-0.0f64).to_bits(),
        1,
        0x7ff8_0000_0000_0042,
        f64::INFINITY.to_bits(),
    ];
    let mut values = StaticRefreshValues::default();
    values.reset(bits.len());
    for (index, bits) in bits.into_iter().enumerate() {
        assert!(values.get(index).is_none());
        values.set(index, f64::from_bits(bits));
        assert_eq!(values.get(index).unwrap().to_bits(), bits);
    }
    assert!(values.get(bits.len()).is_none());
}

#[test]
fn parameter_invalidation_clears_presence_without_changing_an_existing_clone() {
    let mut values = StaticRefreshValues::default();
    values.reset(3);
    values.set(1, -0.0);
    let retained = values.clone();
    values.reset(2);
    assert!(values.get(0).is_none());
    assert!(values.get(1).is_none());
    assert!(values.get(2).is_none());
    assert_eq!(retained.get(1).unwrap().to_bits(), (-0.0f64).to_bits());
    values.set(1, 3.0);
    assert_eq!(values.get(1), Some(3.0));
    assert_eq!(retained.get(1).unwrap().to_bits(), (-0.0f64).to_bits());
}
