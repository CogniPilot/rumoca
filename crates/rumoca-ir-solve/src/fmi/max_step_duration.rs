//! The derived maximum-step-duration local.
//!
//! MLS 3.7 §3.7.2.1 asks a variable-step integrator to respect the current
//! delay times so a delay buffer is never extrapolated. FMI has no operation
//! for that, so a delay-bearing component publishes the bound as ordinary
//! description metadata: one Rumoca-namespaced Float64 local an importer reads
//! with the standard getter it already uses for every other variable.
//!
//! Nothing about the local is supplied from outside. Its presence follows from
//! the checked kernel's delay partition, its attributes are fixed by
//! SPEC_0044 §8, and its value reference is issued by the one inventory it
//! belongs to. Because the component is the sole evaluation owner, the rows it
//! evaluates are the complete current `delay_time_rhs` partition by
//! construction, so no correlation list is stored or storable.

use super::FmiComponentError;
use super::metadata::{
    FmiCausality, FmiInitial, FmiValueBacking, FmiVariability, FmiVariable, FmiWritePolicy,
};
use crate::SolveVariableCatalogEntry;
use crate::SolveVariableValueKind;

/// The name this local carries in Rumoca's namespace.
pub const MAX_STEP_DURATION_NAME: &str = "rumoca.maxStepDuration";

/// The unit the local is expressed in.
pub const MAX_STEP_DURATION_UNIT: &str = "s";

/// The description Rumoca publishes for the local.
pub const MAX_STEP_DURATION_DESCRIPTION: &str = concat!(
    "Largest step duration the current delay times admit; ",
    "the maximum finite Float64 means no delay currently bounds the step"
);

/// The value a component reports when no positive delay time currently bounds
/// a step: the largest finite IEEE 754 binary64 value.
///
/// A reader maps exactly this value to "no bound" and treats every other
/// non-positive or non-finite read as a failure, so the unconstrained case
/// needs no second signalling channel.
pub const MAX_STEP_DURATION_UNCONSTRAINED: f64 = f64::MAX;

/// The inventory entry a delay-bearing kernel publishes.
///
/// Every field is fixed here rather than accepted, so a missing or malformed
/// attribute is unrepresentable and a delay-free kernel simply never reaches
/// this function.
pub(super) fn derived_local(value_reference_fmi3: u32) -> FmiVariable {
    FmiVariable {
        source_id: None,
        name: MAX_STEP_DURATION_NAME.to_string(),
        value_kind: SolveVariableValueKind::Real,
        dimensions: Vec::new(),
        backing: FmiValueBacking::MaxStepDuration,
        // FMI 3.0.2 forbids `start` where `initial="calculated"`, so the entry
        // has none to give rather than an empty or defaulted one.
        start: None,
        minimum: None,
        maximum: None,
        nominal: None,
        unit: Some(MAX_STEP_DURATION_UNIT.to_string()),
        description: Some(MAX_STEP_DURATION_DESCRIPTION.to_string()),
        causality: FmiCausality::Local,
        variability: FmiVariability::Continuous,
        initial: Some(FmiInitial::Calculated),
        write_policy: FmiWritePolicy::ReadOnly,
        tunable: false,
        declaration: None,
        value_reference_fmi3,
    }
}

/// Refuse a source declaration that would take the reserved name from the
/// local a delay-bearing kernel is about to publish.
///
/// Only a delay-bearing kernel publishes the local, so only there is the name
/// genuinely taken twice; the rejection carries the colliding Modelica
/// declaration rather than restating it as text.
pub(super) fn reject_reserved_name(
    input: &SolveVariableCatalogEntry,
    delay_bearing: bool,
) -> Result<(), FmiComponentError> {
    if delay_bearing && input.name() == MAX_STEP_DURATION_NAME {
        return Err(FmiComponentError::ReservedMaxStepDurationName {
            name: MAX_STEP_DURATION_NAME,
            declaration: input.provenance(),
        });
    }
    Ok(())
}
