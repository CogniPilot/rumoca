use super::*;

#[test]
fn effective_identity_is_authoritative_and_derived_types_reuse_the_earliest_layout() {
    let source = TestSource::new(
        "type Width = Real; type WidthAgain = Real; type Height = Real; Real derived;",
    );
    let width_at = source.source("type Width = Real", 0);
    let width_again_at = source.source("type WidthAgain = Real", 0);
    let height_at = source.source("type Height = Real", 0);
    let derived_at = source.source("Real derived", 0);
    let dae = Dae::construct(source.map, |dae| {
        dae.types(|types| {
            let width = types.intern(
                TypeId::new(10),
                ValueType::scalar(ScalarType::Real),
                width_at,
            )?;
            let width_again = types.intern(
                TypeId::new(10),
                ValueType::scalar(ScalarType::Real),
                width_again_at,
            )?;
            let height = types.intern(
                TypeId::new(11),
                ValueType::scalar(ScalarType::Real),
                height_at,
            )?;
            let derived = types.derived(ValueType::scalar(ScalarType::Real), derived_at)?;
            assert_eq!(width.index(), width_again.index());
            assert_eq!(width.index(), derived.index());
            assert_ne!(width.index(), height.index());
            Ok(())
        })
    })
    .expect("equal reuse is idempotent while distinct effective identities stay distinct");

    dae.inspect(|view| {
        let width = view.value_type_id(0).unwrap();
        let height = view.value_type_id(1).unwrap();
        assert_eq!(view.value_type_count(), 2);
        assert_eq!(view.effective_flat_type(width), Some(TypeId::new(10)));
        assert_eq!(view.effective_flat_type(height), Some(TypeId::new(11)));
        assert_eq!(view.value_type_provenance(width), Some(width_at));
        assert_eq!(view.value_type_provenance(height), Some(height_at));
    });

    let json = serde_json::to_string(&dae).unwrap();
    let decoded: Dae = serde_json::from_str(&json).unwrap();
    assert_eq!(serde_json::to_string(&decoded).unwrap(), json);
    let binary = bincode::serialize(&dae).unwrap();
    let decoded: Dae = bincode::deserialize(&binary).unwrap();
    assert_eq!(bincode::serialize(&decoded).unwrap(), binary);
}

#[test]
fn effective_type_never_merges_into_an_earlier_derived_layout() {
    let source = TestSource::new("Real temporary; type Distance = Real; Real reused;");
    let temporary_at = source.source("Real temporary", 0);
    let distance_at = source.source("type Distance = Real", 0);
    let reused_at = source.source("Real reused", 0);
    let dae = Dae::construct(source.map, |dae| {
        dae.types(|types| {
            let temporary = types.derived(ValueType::scalar(ScalarType::Real), temporary_at)?;
            let distance = types.intern(
                TypeId::new(20),
                ValueType::scalar(ScalarType::Real),
                distance_at,
            )?;
            let reused = types.derived(ValueType::scalar(ScalarType::Real), reused_at)?;
            assert_eq!(temporary.index(), reused.index());
            assert_ne!(temporary.index(), distance.index());
            Ok(())
        })
    })
    .unwrap();

    dae.inspect(|view| {
        let temporary = view.value_type_id(0).unwrap();
        let distance = view.value_type_id(1).unwrap();
        assert_eq!(view.value_type_count(), 2);
        assert_eq!(view.effective_flat_type(temporary), None);
        assert_eq!(view.effective_flat_type(distance), Some(TypeId::new(20)));
        assert_eq!(view.value_type_provenance(temporary), Some(temporary_at));
        assert_eq!(view.value_type_provenance(distance), Some(distance_at));
    });
}

#[test]
fn conflicting_effective_layout_reports_both_exact_occurrences() {
    let source = TestSource::new("type Signal = Real; redeclare type Signal = Boolean;");
    let established = source.source("type Signal = Real", 0);
    let attempted = source.source("redeclare type Signal = Boolean", 0);
    let established_type = ValueType::scalar(ScalarType::Real);
    let attempted_type = ValueType::scalar(ScalarType::Boolean);
    let error = Dae::construct(source.map, |dae| {
        dae.types(|types| {
            types.intern(TypeId::new(30), established_type.clone(), established)?;
            types.intern(TypeId::new(30), attempted_type.clone(), attempted)?;
            Ok(())
        })
    })
    .unwrap_err();

    assert_eq!(error.source_span(), Some(attempted.span()));
    assert_eq!(
        error,
        DaeConstructionError::ConflictingEffectiveType {
            type_id: TypeId::new(30),
            established_type: Box::new(established_type),
            attempted_type: Box::new(attempted_type),
            established,
            attempted,
        }
    );
}

#[test]
fn unknown_effective_identity_fails_at_type_construction() {
    let source = TestSource::new("Real unresolved;");
    let at = source.source("Real unresolved", 0);
    let error = Dae::construct(source.map, |dae| {
        dae.types(|types| {
            types.intern(TypeId::UNKNOWN, ValueType::scalar(ScalarType::Real), at)?;
            Ok(())
        })
    })
    .unwrap_err();
    assert_eq!(
        error,
        DaeConstructionError::InvalidEffectiveTypeId {
            type_id: TypeId::UNKNOWN,
            span: at.span(),
        }
    );
}

#[test]
fn wire_replay_rejects_one_effective_identity_with_two_layouts() {
    let source = TestSource::new("type Signal = Real; type Switch = Boolean;");
    let real_at = source.source("type Signal = Real", 0);
    let boolean_at = source.source("type Switch = Boolean", 0);
    let dae = Dae::construct(source.map, |dae| {
        dae.types(|types| {
            types.intern(
                TypeId::new(40),
                ValueType::scalar(ScalarType::Real),
                real_at,
            )?;
            types.intern(
                TypeId::new(41),
                ValueType::scalar(ScalarType::Boolean),
                boolean_at,
            )?;
            Ok(())
        })
    })
    .unwrap();
    let mut forged = serde_json::to_value(dae).unwrap();
    forged["storage"]["flat_type_ids"][1] = forged["storage"]["flat_type_ids"][0].clone();

    let error = serde_json::from_value::<Dae>(forged).unwrap_err();
    assert!(
        error.to_string().contains("has conflicting DAE layouts"),
        "{error}"
    );
}
