use rumoca_ir_dae::{ScalarType, ValueType};

const PRIMITIVE_TYPES: [ScalarType; 5] = [
    ScalarType::Real,
    ScalarType::Integer,
    ScalarType::Enumeration,
    ScalarType::Boolean,
    ScalarType::String,
];

#[test]
fn scalar_count_of_each_primitive_scalar_is_one() {
    for scalar in PRIMITIVE_TYPES {
        assert_eq!(ValueType::scalar(scalar).scalar_count(), Some(1));
    }
}

#[test]
fn scalar_count_multiplies_every_compact_extent() {
    for scalar in PRIMITIVE_TYPES {
        let value_type = ValueType::array(scalar, [2, 3, 5]);
        assert_eq!(value_type.dimensions(), &[2, 3, 5]);
        assert_eq!(value_type.scalar_count(), Some(30));
    }
}

#[test]
fn scalar_count_zero_extent_keeps_the_shape_empty() {
    for dimensions in [[0, u32::MAX, u32::MAX], [2, 0, 5], [2, 3, 0]] {
        assert_eq!(
            ValueType::array(ScalarType::Real, dimensions).scalar_count(),
            Some(0)
        );
    }
}

#[test]
fn scalar_count_refuses_overflow_before_a_later_zero() {
    assert_eq!(
        ValueType::array(ScalarType::Real, [u32::MAX, u32::MAX, 2]).scalar_count(),
        None
    );
    assert_eq!(
        ValueType::array(ScalarType::Real, [u32::MAX, u32::MAX, 2, 0]).scalar_count(),
        None
    );
}

#[test]
fn scalar_count_refuses_records_independently_of_extents() {
    assert_eq!(ValueType::scalar(ScalarType::Record).scalar_count(), None);
    assert_eq!(
        ValueType::array(ScalarType::Record, [2, 3]).scalar_count(),
        None
    );
    assert_eq!(
        ValueType::array(ScalarType::Record, [0]).scalar_count(),
        None
    );
}
