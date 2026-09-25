//! Consuming C profiles retain the checked component and its indicator order.

use super::{FmiCodegenView, FmiEventFreeCodegenView, FmiMetadata};
use serde::Serialize;
use serde::ser::{SerializeMap, Serializer};
use std::collections::BTreeSet;

#[derive(Debug, thiserror::Error)]
#[error("FMI C profile: {0}")]
pub struct FmiCCodegenError(&'static str);

#[derive(Debug)]
enum Profile {
    EventFree(FmiEventFreeCodegenView),
    StaticAssertions(FmiCodegenView),
}

/// A correlated component admitted by a complete C event-profile check.
///
/// Initialization retains the parameter bindings in dependency order and the
/// settled initialization projection. Static partitions
/// retain their executable predicates and parameter-determined discrete
/// equations without continuous FMI indicators;
/// the admission proof permits no continuously changing predicate, relation
/// memory, clock, scheduled event, or state-dependent discrete update.
#[derive(Debug)]
pub struct FmiCCodegenView {
    profile: Profile,
    /// The dependency order of the parameter-binding programs.
    update_order: Vec<usize>,
    /// The dependency levels of the parameter bindings.
    update_levels: usize,
    /// Inventory indices of the parameters no emitted program reads.
    folded: BTreeSet<usize>,
    /// The evaluation orders of the parameter-determined discrete rows.
    discrete_order: super::static_assertions::DiscreteOrder,
}

impl FmiCodegenView {
    pub fn try_c(self) -> Result<FmiCCodegenView, FmiCCodegenError> {
        validate_variables(&self.metadata)?;
        let (update_order, update_levels) =
            super::parameter_updates::validate(&self.model.problem, &self.model.pure_calls)
                .map_err(FmiCCodegenError)?;
        let folded = folded_parameters(&self.metadata, &self.model.problem);
        if !self.event_indicators.sources().is_empty() {
            return Err(FmiCCodegenError(
                "continuous event indicators require general event support; the C profile supports only event-free models and parameter-determined events",
            ));
        }
        if crate::solve_event_class(&self.model.problem).is_none() {
            return self
                .try_event_free()
                .map(|value| FmiCCodegenView {
                    profile: Profile::EventFree(value),
                    update_order,
                    update_levels,
                    folded,
                    discrete_order: super::static_assertions::DiscreteOrder::default(),
                })
                .map_err(|_| FmiCCodegenError("event-free narrowing failed"));
        }
        let discrete_order =
            super::static_assertions::validate(&self.model).map_err(FmiCCodegenError)?;
        Ok(FmiCCodegenView {
            profile: Profile::StaticAssertions(self),
            update_order,
            update_levels,
            folded,
            discrete_order,
        })
    }
}

impl TryFrom<FmiEventFreeCodegenView> for FmiCCodegenView {
    type Error = FmiCCodegenError;
    fn try_from(value: FmiEventFreeCodegenView) -> Result<Self, Self::Error> {
        validate_variables(&value.metadata)?;
        let (update_order, update_levels) =
            super::parameter_updates::validate(value.problem(), value.pure_calls())
                .map_err(FmiCCodegenError)?;
        let folded = folded_parameters(&value.metadata, value.problem());
        Ok(Self {
            profile: Profile::EventFree(value),
            update_order,
            update_levels,
            folded,
            discrete_order: super::static_assertions::DiscreteOrder::default(),
        })
    }
}

/// Every public variable has one FMI value type the generated component
/// reads and writes through its storage: Real as Float64, Integer and
/// enumeration ordinals as Int32 (FMI 2 Integer), Boolean as Boolean, all
/// held in the numeric storage run; a String parameter or constant as its
/// literal text, which no numeric program reads.
fn validate_variables(metadata: &FmiMetadata) -> Result<(), FmiCCodegenError> {
    use crate::{SolveVariableStorageRole as Role, SolveVariableValueKind as Kind};
    for variable in metadata.variables() {
        match variable.value_kind() {
            Kind::Real => {}
            Kind::Integer | Kind::Boolean | Kind::Enumeration => {
                if variable.role() == Some(Role::State)
                    || variable.variability() == super::FmiVariability::Continuous
                {
                    return Err(FmiCCodegenError(
                        "an Integer, Boolean, or enumeration variable has continuous variability",
                    ));
                }
            }
            Kind::String => {
                if !matches!(variable.role(), Some(Role::Parameter | Role::Constant)) {
                    return Err(FmiCCodegenError(
                        "only String parameters and constants are supported by the C FMI profile",
                    ));
                }
                if variable.text_start().is_none() {
                    return Err(FmiCCodegenError(
                        "a String variable needs a literal start value in the C FMI profile",
                    ));
                }
            }
        }
    }
    Ok(())
}

impl FmiCCodegenView {
    fn model(&self) -> &crate::SolveModel {
        match &self.profile {
            Profile::EventFree(v) => &v.model,
            Profile::StaticAssertions(v) => &v.model,
        }
    }
    fn metadata(&self) -> &FmiMetadata {
        match &self.profile {
            Profile::EventFree(v) => &v.metadata,
            Profile::StaticAssertions(v) => &v.metadata,
        }
    }
    pub fn problem(&self) -> &crate::SolveProblem {
        match &self.profile {
            Profile::EventFree(v) => v.problem(),
            Profile::StaticAssertions(v) => &v.model.problem,
        }
    }
    pub fn artifacts(&self) -> &crate::SolveArtifacts {
        match &self.profile {
            Profile::EventFree(v) => v.artifacts(),
            Profile::StaticAssertions(v) => &v.model.artifacts,
        }
    }
    /// The retained kernel's finite positive characteristic scale of one
    /// solver variable, the same scale its linked algebraic projection uses.
    pub fn solver_variable_scale(&self, index: usize) -> f64 {
        self.model().solver_variable_scale(index)
    }
    pub fn pure_calls(&self) -> &crate::SolvePureCallTable {
        match &self.profile {
            Profile::EventFree(v) => v.pure_calls(),
            Profile::StaticAssertions(v) => &v.model.pure_calls,
        }
    }
}

impl Serialize for FmiCCodegenView {
    fn serialize<S: Serializer>(&self, serializer: S) -> Result<S::Ok, S::Error> {
        let metadata = self.metadata();
        let mut entries = serializer.serialize_map(Some(10))?;
        entries.serialize_entry("initial_y", &self.model().initial_y)?;
        entries.serialize_entry("initial_parameters", &self.model().parameters)?;
        entries.serialize_entry(
            "variables",
            &super::metadata::SerializedFmiVariables::borrowing(metadata.variables())
                .with_constants(&self.folded),
        )?;
        entries.serialize_entry("state_variable_indices", metadata.state_variable_indices())?;
        entries.serialize_entry(
            "derivative_value_reference_base_fmi3",
            &metadata.derivative_value_reference_base_fmi3(),
        )?;
        entries.serialize_entry(
            "assertions",
            &matches!(self.profile, Profile::StaticAssertions(_)),
        )?;
        entries.serialize_entry("event_indicator_roots", &[] as &[usize])?;
        entries.serialize_entry("update_order", &self.update_order)?;
        entries.serialize_entry("discrete_equations", &self.discrete_order.equations)?;
        entries.serialize_entry("discrete_memories", &self.discrete_order.memories)?;
        entries.end()
    }
}

impl FmiCCodegenView {
    /// The dependency levels of the parameter bindings: the simultaneous
    /// sweeps the runtime's `eval_and_apply_update_rows` needs to settle
    /// them from arbitrary values, plus one to observe that nothing changed.
    #[must_use]
    pub const fn parameter_binding_levels(&self) -> usize {
        self.update_levels
    }
}

/// The settable parameters whose values no emitted program reads: the
/// compiler folded them into the programs that use them, so a set could not
/// take effect. The C profile exports them as constants that cannot be set.
fn folded_parameters(metadata: &FmiMetadata, problem: &crate::SolveProblem) -> BTreeSet<usize> {
    let reads = crate::read_parameter_slots(problem);
    metadata
        .variables()
        .iter()
        .enumerate()
        .filter(|(_, variable)| variable.causality() == super::FmiCausality::Parameter)
        .filter(|(_, variable)| {
            variable.storage().is_some_and(|storage| {
                storage.column() == super::metadata::FmiStorageColumn::P
                    && (storage.base()..storage.base() + storage.scalar_count())
                        .all(|index| !reads.contains(&index))
            })
        })
        .map(|(index, _)| index)
        .collect()
}
