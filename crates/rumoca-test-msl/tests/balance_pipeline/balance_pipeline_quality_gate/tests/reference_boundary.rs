//! Per-version checks of the reviewed reference-boundary chain.

use super::*;

/// The v15 boundary adds one typed exception (RoomCO2WithControls's noisy
/// OMC reference) for a model that was near, not strict-high, so it earns no
/// strict-high model; the v14 boundary it rests on is returned.
pub(super) fn v14_below_the_v15_boundary(
    reference: MslReferenceBoundaryMigration,
) -> MslReferenceBoundaryMigration {
    assert_eq!(
        reference.metric.strict_high_before,
        reference.metric.strict_high_after
    );
    assert_eq!(
        reference.metric.policy_excluded_after,
        reference.policy_excluded_before + 1
    );
    *reference.previous.expect("reviewed v13-to-v14 boundary")
}

/// A roster addition must name its defect and be in the roster it adds to;
/// the v8 boundary's LogicalSample addition is reviewed and the v9 to v14
/// boundaries add none (SPEC_0050).
#[test]
fn roster_additions_name_their_defect_and_join_the_roster() {
    let baseline =
        load_msl_quality_baseline(&msl_quality_baseline_path()).expect("load checked baseline");
    assert!(validate_unexcepted_non_high_roster(&baseline).is_ok());
    let head = baseline
        .reference_boundary_migration
        .as_ref()
        .expect("checked v15 boundary");
    assert_eq!(
        head.roster_additions
            .iter()
            .map(|addition| addition.model_name.as_str())
            .collect::<Vec<_>>(),
        ["Modelica.Fluid.Examples.AST_BatchPlant.Test.TwoTanks"],
        "v15 moves TwoTanks from a mislabelled exclusion to the roster"
    );
    let v14 = head.previous.as_deref().expect("checked v14 boundary");
    assert!(v14.roster_additions.is_empty());
    let v13 = v14.previous.as_deref().expect("checked v13 boundary");
    assert!(v13.roster_additions.is_empty());
    let v12 = v13.previous.as_deref().expect("checked v12 boundary");
    assert!(v12.roster_additions.is_empty());
    let v11 = v12.previous.as_deref().expect("checked v11 boundary");
    assert!(v11.roster_additions.is_empty());
    let v10 = v11.previous.as_deref().expect("checked v10 boundary");
    assert!(v10.roster_additions.is_empty());
    let v9 = v10.previous.as_deref().expect("checked v9 boundary");
    assert!(v9.roster_additions.is_empty());
    assert_eq!(
        v9.previous
            .as_ref()
            .map(|migration| migration.roster_additions.clone()),
        Some(vec![schema_migrations::logical_sample_roster_addition()])
    );

    let mut unnamed = baseline.clone();
    unnamed
        .reference_boundary_migration
        .as_mut()
        .and_then(|migration| migration.previous.as_mut())
        .and_then(|migration| migration.previous.as_mut())
        .and_then(|migration| migration.previous.as_mut())
        .and_then(|migration| migration.previous.as_mut())
        .and_then(|migration| migration.previous.as_mut())
        .and_then(|migration| migration.previous.as_mut())
        .and_then(|migration| migration.previous.as_mut())
        .unwrap()
        .roster_additions[0]
        .cause = " ".to_string();
    assert!(validate_unexcepted_non_high_roster(&unnamed).is_err());

    // Under the boundary that adds it, LogicalSample must be in the roster.
    let mut outside = baseline;
    outside.reference_boundary_migration = outside
        .reference_boundary_migration
        .and_then(|migration| migration.previous.map(|previous| *previous))
        .and_then(|migration| migration.previous.map(|previous| *previous))
        .and_then(|migration| migration.previous.map(|previous| *previous))
        .and_then(|migration| migration.previous.map(|previous| *previous))
        .and_then(|migration| migration.previous.map(|previous| *previous))
        .and_then(|migration| migration.previous.map(|previous| *previous))
        .and_then(|migration| migration.previous.map(|previous| *previous));
    outside
        .unexcepted_non_high_models
        .shift_remove("Modelica.Clocked.Examples.Elementary.ClockSignals.LogicalSample");
    assert!(validate_unexcepted_non_high_roster(&outside).is_err());
}
