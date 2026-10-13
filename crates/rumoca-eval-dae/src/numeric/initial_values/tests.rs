use super::*;

#[test]
fn typed_slice_and_numerical_view_preserve_authored_repeat_structure() {
    let values = NumericInitialValues::repeat(super::super::Scalar::Integer(i64::MAX), 120_000);
    let slice = values.slice(100, 100_000).unwrap();
    assert!(matches!(
        slice.runs().next(),
        Some(NumericInitialRun::Repeat {
            value: super::super::Scalar::Integer(i64::MAX),
            count: 100_000
        })
    ));
    assert_eq!(slice.runs().count(), 1);
    assert!(values.slice(120_000, 1).is_none());
    assert_eq!(slice.map(super::super::Scalar::real).runs().count(), 1);
}

#[test]
fn scalar_broadcast_and_source_overrides_retain_complete_ordered_runs() {
    let mut values = NumericInitialValues::<f64>::repeat(-0.0, 1);
    values.broadcast(200_000);
    assert_eq!(values.runs().count(), 1);
    values.apply_source_overrides(&[(2, 2.0), (3, 3.0), (199_999, 4.0)]);
    assert_eq!(values.runs().count(), 4);
    for (index, expected) in [(0, -0.0f64), (2, 2.0), (3, 3.0), (4, -0.0), (199_999, 4.0)] {
        assert_eq!(values.value(index).unwrap().to_bits(), expected.to_bits());
    }
    assert_eq!(values.materialize().len(), 200_000);
}

#[test]
fn authored_equal_literals_stay_literal_and_empty_values_stay_empty() {
    let mut values = NumericInitialValues::<f64>::literal(vec![2.0; 8]);
    values.apply_source_overrides(&[(0, -0.0), (7, 3.0)]);
    assert!(
        matches!(values.runs().next(), Some(NumericInitialRun::Literal(values)) if values.len() == 8)
    );
    assert_eq!(values.value(0).unwrap().to_bits(), (-0.0f64).to_bits());
    let mut empty = NumericInitialValues::repeat(-0.0, 0);
    empty.apply_source_overrides(&[]);
    assert!(empty.is_empty());
    assert_eq!(empty.runs().count(), 0);
}
