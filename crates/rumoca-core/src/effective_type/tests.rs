use super::*;

#[test]
fn effective_identity_includes_nominal_type_and_shape() {
    let scalar = EffectiveType::new(TypeId::new(1), TypeId::new(1), []).unwrap();
    let array = EffectiveType::new(TypeId::new(1), TypeId::new(1), [5, 5]).unwrap();
    let alias = EffectiveType::new(TypeId::new(8), TypeId::new(1), []).unwrap();

    assert_ne!(scalar, array);
    assert_ne!(scalar, alias);
}

#[test]
fn unresolved_or_negative_descriptors_cannot_be_constructed() {
    assert_eq!(
        EffectiveType::new(TypeId::UNKNOWN, TypeId::new(1), []),
        Err(EffectiveTypeError::UnknownNominalType)
    );
    assert_eq!(
        EffectiveType::new(TypeId::new(1), TypeId::new(1), [-1]),
        Err(EffectiveTypeError::NegativeExtent)
    );
}

#[test]
fn deserialization_replays_checked_construction() {
    let unknown_canonical = serde_json::json!({
        "nominal_type": 1,
        "canonical_type": TypeId::UNKNOWN.index(),
        "dimensions": [],
    });
    let negative_extent = serde_json::json!({
        "nominal_type": 1,
        "canonical_type": 1,
        "dimensions": [-1],
    });

    assert!(serde_json::from_value::<EffectiveType>(unknown_canonical).is_err());
    assert!(serde_json::from_value::<EffectiveType>(negative_extent).is_err());
}
