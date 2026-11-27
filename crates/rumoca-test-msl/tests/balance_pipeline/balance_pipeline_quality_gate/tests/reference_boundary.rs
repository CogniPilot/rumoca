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
