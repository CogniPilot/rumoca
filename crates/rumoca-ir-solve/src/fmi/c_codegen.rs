//! Consuming C profiles retain the checked component and its indicator order.

use super::{FmiCodegenView, FmiEventFreeCodegenView, FmiMetadata};
use serde::Serialize;
use serde::ser::{SerializeMap, Serializer};

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
/// Initialization retains only ordered parameter assignments. Static assertions
/// retain their executable predicates without continuous FMI indicators;
/// the admission proof permits no continuously changing predicate, relation
/// memory, clock, scheduled event, or discrete state update.
#[derive(Debug)]
pub struct FmiCCodegenView(Profile);

impl FmiCodegenView {
    pub fn try_c(self) -> Result<FmiCCodegenView, FmiCCodegenError> {
        validate_variables(&self.metadata)?;
        super::parameter_updates::validate(&self.model.problem, &self.model.pure_calls)
            .map_err(FmiCCodegenError)?;
        if crate::solve_event_class(&self.model.problem).is_none() {
            return self
                .try_event_free()
                .map(|value| FmiCCodegenView(Profile::EventFree(value)))
                .map_err(|_| FmiCCodegenError("event-free narrowing failed"));
        }
        super::static_assertions::validate(&self.model).map_err(FmiCCodegenError)?;
        if !self.event_indicators.sources().is_empty() {
            return Err(FmiCCodegenError(
                "static assertions cannot own event indicators",
            ));
        }
        Ok(FmiCCodegenView(Profile::StaticAssertions(self)))
    }
}

impl TryFrom<FmiEventFreeCodegenView> for FmiCCodegenView {
    type Error = FmiCCodegenError;
    fn try_from(value: FmiEventFreeCodegenView) -> Result<Self, Self::Error> {
        validate_variables(&value.metadata)?;
        super::parameter_updates::validate(value.problem(), value.pure_calls())
            .map_err(FmiCCodegenError)?;
        Ok(Self(Profile::EventFree(value)))
    }
}

fn validate_variables(metadata: &FmiMetadata) -> Result<(), FmiCCodegenError> {
    if metadata
        .variables()
        .iter()
        .any(|variable| variable.value_kind() != crate::SolveVariableValueKind::Real)
    {
        return Err(FmiCCodegenError(
            "only Real public variables are supported by the C FMI profile",
        ));
    }
    Ok(())
}

impl FmiCCodegenView {
    fn model(&self) -> &crate::SolveModel {
        match &self.0 {
            Profile::EventFree(v) => &v.model,
            Profile::StaticAssertions(v) => &v.model,
        }
    }
    fn metadata(&self) -> &FmiMetadata {
        match &self.0 {
            Profile::EventFree(v) => &v.metadata,
            Profile::StaticAssertions(v) => &v.metadata,
        }
    }
    pub fn problem(&self) -> &crate::SolveProblem {
        match &self.0 {
            Profile::EventFree(v) => v.problem(),
            Profile::StaticAssertions(v) => &v.model.problem,
        }
    }
    pub fn artifacts(&self) -> &crate::SolveArtifacts {
        match &self.0 {
            Profile::EventFree(v) => v.artifacts(),
            Profile::StaticAssertions(v) => &v.model.artifacts,
        }
    }
    pub fn pure_calls(&self) -> &crate::SolvePureCallTable {
        match &self.0 {
            Profile::EventFree(v) => v.pure_calls(),
            Profile::StaticAssertions(v) => &v.model.pure_calls,
        }
    }
}

impl Serialize for FmiCCodegenView {
    fn serialize<S: Serializer>(&self, serializer: S) -> Result<S::Ok, S::Error> {
        let metadata = self.metadata();
        let mut entries = serializer.serialize_map(Some(7))?;
        entries.serialize_entry("initial_y", &self.model().initial_y)?;
        entries.serialize_entry("initial_parameters", &self.model().parameters)?;
        entries.serialize_entry(
            "variables",
            &super::metadata::SerializedFmiVariables::borrowing(metadata.variables()),
        )?;
        entries.serialize_entry("state_variable_indices", metadata.state_variable_indices())?;
        entries.serialize_entry(
            "derivative_value_reference_base_fmi3",
            &metadata.derivative_value_reference_base_fmi3(),
        )?;
        entries.serialize_entry(
            "assertions",
            &matches!(self.0, Profile::StaticAssertions(_)),
        )?;
        entries.serialize_entry("event_indicator_roots", &[] as &[usize])?;
        entries.end()
    }
}
