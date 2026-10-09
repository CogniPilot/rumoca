//! Consuming C profiles retain the checked component and its indicator order.

use super::{FmiCodegenView, FmiEventFreeCodegenView, FmiMetadata};
use serde::Serialize;
use serde::ser::{SerializeMap, Serializer};

#[derive(Debug, thiserror::Error)]
#[error("FMI C profile: {0}")]
pub struct FmiCCodegenError(pub(super) &'static str);

#[derive(Debug)]
enum Profile {
    EventFree(FmiEventFreeCodegenView),
    StaticAssertions(FmiCodegenView),
    ScalarEvents(
        FmiCodegenView,
        Box<super::scalar_events::ScalarEventProfile>,
    ),
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
    deployment: super::FmiDeploymentCapabilities,
    /// The dependency order of the parameter-binding programs.
    update_order: Vec<usize>,
    /// The dependency levels of the parameter bindings.
    update_levels: usize,
    /// The evaluation orders of the parameter-determined discrete rows.
    discrete_order: super::static_assertions::DiscreteOrder,
}

impl FmiCodegenView {
    pub fn try_c(self) -> Result<FmiCCodegenView, FmiCCodegenError> {
        self.try_deployment(super::FmiDeploymentCapabilities::default())
            .map_err(|error| FmiCCodegenError(error.0))?
            .try_c()
    }

    /// The narrowest profile that executes the component's events: none,
    /// events fixed between parameter changes, or the event iteration.
    fn profile(
        self,
    ) -> Result<(Profile, super::static_assertions::DiscreteOrder), FmiCCodegenError> {
        let no_order = super::static_assertions::DiscreteOrder::default;
        if !self.event_indicators.sources().is_empty() {
            return self.scalar_events().map(|profile| (profile, no_order()));
        }
        if crate::solve_event_class(&self.model.problem).is_none() {
            return self
                .try_event_free()
                .map(|value| (Profile::EventFree(value), no_order()))
                .map_err(|_| FmiCCodegenError("event-free narrowing failed"));
        }
        match super::static_assertions::validate(&self.model) {
            Ok(order) => Ok((Profile::StaticAssertions(self), order)),
            // Events that follow time, states, or inputs, and the discrete
            // rows that follow them, run through the event iteration even
            // when no continuous indicator monitors them.
            Err(super::static_assertions::StaticRefusal::EventIteration(_)) => {
                self.scalar_events().map(|profile| (profile, no_order()))
            }
            Err(refusal) => Err(FmiCCodegenError(refusal.message())),
        }
    }

    fn scalar_events(self) -> Result<Profile, FmiCCodegenError> {
        let events = super::scalar_events::validate(&self.model).map_err(FmiCCodegenError)?;
        Ok(Profile::ScalarEvents(self, Box::new(events)))
    }
}

impl FmiCCodegenView {
    pub(super) fn from_deployment(
        deployment: super::FmiDeploymentView,
    ) -> Result<Self, FmiCCodegenError> {
        let (view, deployment) = deployment.into_parts();
        let (update_order, update_levels) = admit_kernel(&view.model, &view.metadata)?;
        let (profile, discrete_order) = view.profile()?;
        Ok(Self {
            profile,
            deployment,
            update_order,
            update_levels,
            discrete_order,
        })
    }
}

impl TryFrom<FmiEventFreeCodegenView> for FmiCCodegenView {
    type Error = FmiCCodegenError;
    fn try_from(value: FmiEventFreeCodegenView) -> Result<Self, Self::Error> {
        let deployment = super::FmiDeploymentCapabilities::default()
            .admit(&value.metadata)
            .map_err(|error| FmiCCodegenError(error.0))?;
        let (update_order, update_levels) = admit_kernel(&value.model, &value.metadata)?;
        Ok(Self {
            profile: Profile::EventFree(value),
            deployment,
            update_order,
            update_levels,
            discrete_order: super::static_assertions::DiscreteOrder::default(),
        })
    }
}

fn admit_kernel(
    model: &crate::SolveModel,
    metadata: &FmiMetadata,
) -> Result<(Vec<usize>, usize), FmiCCodegenError> {
    validate_initial_values(model)?;
    validate_variables(metadata)?;
    refuse_recursive_groups(&model.pure_calls)?;
    super::parameter_updates::validate(&model.problem, &model.pure_calls).map_err(FmiCCodegenError)
}

fn validate_initial_values(model: &crate::SolveModel) -> Result<(), FmiCCodegenError> {
    for values in [&model.initial_y, &model.parameters] {
        values
            .require_finite()
            .map_err(|_| FmiCCodegenError("non-finite initialization value"))?;
    }
    if model.initial_y.len() != model.problem.layout.y_scalars()
        || model.parameters.len() != model.problem.layout.p_scalars()
    {
        return Err(FmiCCodegenError(
            "initial-value owners disagree with checked storage capacity",
        ));
    }
    Ok(())
}

/// The C profile emits no recursive call or depth-carrying frame, so a
/// SOLVE-C62 recursive owner group is refused before any C is generated.
fn refuse_recursive_groups(table: &crate::SolvePureCallTable) -> Result<(), FmiCCodegenError> {
    if table.recursive_groups().is_empty() {
        Ok(())
    } else {
        Err(FmiCCodegenError(
            "recursive function calls (SOLVE-C62 recursive owner groups) are not supported by the C FMI profile",
        ))
    }
}

/// Every public variable has one FMI value type the generated component
/// reads and writes through its storage: Real as Float64, Integer as Int32
/// (FMI 2 Integer), an enumeration ordinal as Int64 (FMI 2 Integer; the
/// ordinal range is its type's literal count), Boolean as Boolean, all
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
    /// The component's Co-Simulation step rule (ME-LSW-001).
    fn co_simulation(&self) -> &super::CoSimulationStepPlan {
        match &self.profile {
            Profile::EventFree(v) => &v.co_simulation,
            Profile::StaticAssertions(v) | Profile::ScalarEvents(v, _) => &v.co_simulation,
        }
    }

    fn model(&self) -> &crate::SolveModel {
        match &self.profile {
            Profile::EventFree(v) => &v.model,
            Profile::StaticAssertions(v) => &v.model,
            Profile::ScalarEvents(v, _) => &v.model,
        }
    }
    pub(super) fn metadata(&self) -> &FmiMetadata {
        match &self.profile {
            Profile::EventFree(v) => &v.metadata,
            Profile::StaticAssertions(v) => &v.metadata,
            Profile::ScalarEvents(v, _) => &v.metadata,
        }
    }
    pub fn problem(&self) -> &crate::SolveProblem {
        match &self.profile {
            Profile::EventFree(v) => v.problem(),
            Profile::StaticAssertions(v) => &v.model.problem,
            Profile::ScalarEvents(v, _) => &v.model.problem,
        }
    }
    pub fn artifacts(&self) -> &crate::SolveArtifacts {
        match &self.profile {
            Profile::EventFree(v) => v.artifacts(),
            Profile::StaticAssertions(v) => &v.model.artifacts,
            Profile::ScalarEvents(v, _) => &v.model.artifacts,
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
            Profile::ScalarEvents(v, _) => &v.model.pure_calls,
        }
    }
    /// The retained kernel's instantiation point: its initial solver vector
    /// and parameter values, the point at which the linked kernel resolves
    /// each reduced chart's state-binding rows.
    pub fn instantiation_point(&self) -> (&[f64], &[f64]) {
        let model = self.model();
        (&model.initial_y, &model.parameters)
    }
}

/// Named, read-only instantiation buffers of the retained checked kernel.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum FmiInstantiationBuffer {
    Solver,
    Parameters,
}

impl FmiInstantiationBuffer {
    pub const ALL: [Self; 2] = [Self::Solver, Self::Parameters];

    pub const fn name(self) -> &'static str {
        match self {
            Self::Solver => "initial_y",
            Self::Parameters => "initial_parameters",
        }
    }
}

impl FmiCCodegenView {
    pub fn instantiation_values(
        &self,
        buffer: FmiInstantiationBuffer,
    ) -> &crate::SolveInitialValues {
        match buffer {
            FmiInstantiationBuffer::Solver => &self.model().initial_y,
            FmiInstantiationBuffer::Parameters => &self.model().parameters,
        }
    }

    pub fn instantiation_buffer(&self, buffer: FmiInstantiationBuffer) -> &[f64] {
        self.instantiation_values(buffer).as_slice()
    }

    /// The single field inventory for serde and read-only template projections.
    pub fn serialize_entries<M: SerializeMap>(&self, entries: &mut M) -> Result<(), M::Error> {
        let metadata = self.metadata();
        entries.serialize_entry("deployment", &self.deployment)?;
        for buffer in FmiInstantiationBuffer::ALL {
            entries.serialize_entry(buffer.name(), self.instantiation_values(buffer))?;
        }
        entries.serialize_entry(
            "variables",
            &super::metadata::SerializedFmiVariables::borrowing(metadata.variables()),
        )?;
        entries.serialize_entry("enumerations", &metadata.enumerations())?;
        entries.serialize_entry("state_variable_indices", metadata.state_variable_indices())?;
        entries.serialize_entry(
            "derivative_value_reference_base_fmi3",
            &metadata.derivative_value_reference_base_fmi3(),
        )?;
        entries.serialize_entry(
            "assertions",
            &matches!(
                self.profile,
                Profile::StaticAssertions(_) | Profile::ScalarEvents(..)
            ),
        )?;
        entries.serialize_entry("event_indicator_roots", &self.event_indicator_roots())?;
        entries.serialize_entry("co_simulation", &self.co_simulation())?;
        entries.serialize_entry(
            "root_location",
            &match &self.profile {
                Profile::ScalarEvents(view, _) => Some(&view.root_location),
                _ => None,
            },
        )?;
        entries.serialize_entry(
            "scalar_events",
            &match &self.profile {
                Profile::ScalarEvents(_, events) => Some(events.as_ref()),
                _ => None,
            },
        )?;
        entries.serialize_entry("update_order", &self.update_order)?;
        entries.serialize_entry("discrete_equations", &self.discrete_order.equations)?;
        entries.serialize_entry("discrete_memories", &self.discrete_order.memories)?;
        Ok(())
    }
}

impl Serialize for FmiCCodegenView {
    fn serialize<S: Serializer>(&self, serializer: S) -> Result<S::Ok, S::Error> {
        let mut entries = serializer.serialize_map(Some(15))?;
        self.serialize_entries(&mut entries)?;
        entries.end()
    }
}

impl FmiCCodegenView {
    /// The root output each FMI event-indicator position reads, in position
    /// order (the Solve IR indicator table; ME-EVENT-005).
    fn event_indicator_roots(&self) -> Vec<usize> {
        let Profile::ScalarEvents(view, _) = &self.profile else {
            return Vec::new();
        };
        view.event_indicators
            .plan()
            .entries()
            .iter()
            .filter_map(|entry| match entry.reading() {
                super::IndicatorReading::RootValue { index } => Some(index),
                super::IndicatorReading::DeadlineDistance { .. } => None,
            })
            .collect()
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
