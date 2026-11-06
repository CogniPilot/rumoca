use super::*;
use crate::{
    SolveArithmeticProfile, SolvePureCallIdentity, SolvePureCallOutput, SolveScalarType,
    SolveValue, SolveValueType,
};
use rumoca_core::{SourceId, Span};
use std::num::NonZeroU64;

fn declaration(name: &str) -> serde_json::Value {
    serde_json::json!({
        "schema_version": 1,
        "name": name,
        "layout": "dense-host-v1",
        "max_unrolled_passes": MAX_UNROLLED_PASSES,
        "max_unrolled_operations": MAX_UNROLLED_OPERATIONS,
        "admits_nested_map_frames": false,
    })
}

fn profile(name: &str) -> SolveTargetProfile {
    serde_json::from_value(declaration(name)).unwrap()
}

fn table(value: f64) -> SolvePureCallTable {
    let arithmetic = SolveArithmeticProfile::construct(
        crate::SolveRealFormat::Binary64,
        crate::SolveIntegerDomain::construct(i64::MIN, i64::MAX).unwrap(),
    );
    let real = SolveValueType::scalar(SolveScalarType::real(arithmetic));
    let span = Span::from_offsets(SourceId::from_source_name("target.mo"), 1, 2);
    SolvePureCallTable::construct(arithmetic, |table| {
        table.add_owner(
            SolvePureCallIdentity::issued(NonZeroU64::new(1).unwrap()),
            vec![],
            vec![SolvePureCallOutput::result(real)],
            span,
            |builder, _, outputs| {
                let value = builder.constant(SolveValue::real(arithmetic, value), span)?;
                builder.store(outputs[0], value, span)
            },
        )?;
        Ok(())
    })
    .unwrap()
}

#[test]
fn unsupported_profile_choices_refuse_instead_of_using_host_defaults() {
    for (key, value) in [
        ("schema_version", serde_json::json!(2)),
        ("name", serde_json::json!(" ")),
        ("layout", serde_json::json!("sparse")),
        ("max_unrolled_passes", serde_json::json!(33)),
        ("max_unrolled_operations", serde_json::json!(1025)),
        ("wcet_budget", serde_json::json!(100)),
        ("admits_nested_map_frames", serde_json::json!("true")),
    ] {
        let mut declared = declaration("host");
        declared[key] = value;
        assert!(
            serde_json::from_value::<SolveTargetProfile>(declared).is_err(),
            "{key}"
        );
    }
}

#[test]
fn specialization_identity_is_reproducible_and_invalidates_both_inputs() {
    let host = profile("host");
    let other = profile("other-host");
    let first = host.select().bind(&table(1.0)).unwrap();
    assert_eq!(first, host.select().bind(&table(1.0)).unwrap());
    let changed_program = host.select().bind(&table(2.0)).unwrap();
    assert_ne!(first.program(), changed_program.program());
    assert_eq!(first.profile(), changed_program.profile());
    let changed_profile = other.select().bind(&table(1.0)).unwrap();
    assert_eq!(first.program(), changed_profile.program());
    assert_ne!(first.profile(), changed_profile.profile());
    let mut declaration = declaration("host");
    declaration["admits_nested_map_frames"] = serde_json::json!(true);
    let maps: SolveTargetProfile = serde_json::from_value(declaration).unwrap();
    let changed_capability = maps.select().bind(&table(1.0)).unwrap();
    assert_eq!(first.program(), changed_capability.program());
    assert_ne!(first.profile(), changed_capability.profile());
    assert!(maps.select().admits_nested_map_frames());
    assert!(!host.select().admits_nested_map_frames());
    assert!(
        !SolveTargetProfile::current_dense_host()
            .select()
            .admits_nested_map_frames()
    );
}

#[test]
fn checked_profile_round_trip_retains_its_choices() {
    let original = profile("host");
    let replay: SolveTargetProfile =
        serde_json::from_str(&serde_json::to_string(&original).unwrap()).unwrap();
    assert_eq!(original, replay);
    assert_eq!(replay.select().layout(), SolveTargetLayout::DenseHostV1);
    assert!(!replay.select().admits_nested_map_frames());
}
