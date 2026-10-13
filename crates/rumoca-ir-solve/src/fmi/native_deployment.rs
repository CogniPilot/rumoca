//! Native metadata admission shares the actual kernel getter inventory.

use super::*;
use crate::{
    NativeOutputLane, NativeRefreshAssignmentSchedule, RetainedAssertionRoot,
    RetainedNativePreparation, SolveVariableStorageRole as Role, SolveVariableValueKind as Kind,
};
use serde::ser::SerializeMap;

/// A host contract the package declares (SPEC_0040 SOLVE-C101): the host feeds
/// the value of the output with FMI 3 value reference `output` to the input with
/// value reference `input` before the next evaluation.
#[derive(Clone, Copy, Debug, PartialEq, Eq, serde::Serialize)]
pub struct FmiCarryPair {
    pub output: u32,
    pub input: u32,
}

/// Why a declared carry pair is not admitted.
#[derive(Debug, Clone, PartialEq, Eq, thiserror::Error)]
pub enum NativeCarryError {
    #[error("carried value reference {0} names no published variable")]
    UnknownReference(u32),
    #[error("carried output VR {0} is not a model output")]
    NotOutput(u32),
    #[error("carried input VR {0} is not a model input")]
    NotInput(u32),
    #[error("carried pair (VR {output}, VR {input}) differs in value type or dimensions")]
    Mismatch { output: u32, input: u32 },
    #[error(transparent)]
    Storage(#[from] crate::NativeCarriedRefusal),
}

/// Native deployment refusal: a profile rule, a neutral admission refusal, or
/// the exact variable the native typed inventory leaves uncovered.
#[derive(Debug, thiserror::Error)]
pub enum NativeDeploymentError {
    #[error("FMI deployment profile: {0}")]
    Profile(&'static str),
    #[error(transparent)]
    Admission(#[from] FmiDeploymentError),
    #[error("FMI deployment profile: {0}")]
    TypedInventory(Box<NativeTypedInventoryGap>),
    #[error("FMI carry declaration: {0}")]
    Carry(#[from] NativeCarryError),
}

/// A published variable whose scalars the native typed inventory does not
/// all cover: the exact variable and the lane its kind requires.
#[derive(Debug, Clone, PartialEq, Eq, thiserror::Error)]
#[error(
    "native typed inventory does not cover the requested VR {value_reference} \
     ({name}, {kind:?}, {causality:?}): expected the {expected:?} lane to cover \
     {required} scalars, found {covered}"
)]
pub struct NativeTypedInventoryGap {
    pub value_reference: u32,
    pub name: String,
    pub kind: Kind,
    pub causality: FmiCausality,
    pub expected: NativeOutputLane,
    pub required: usize,
    pub covered: usize,
}

/// Original preparation source admitted for the fixed native CS adapter.
/// No deserialization, uncorrelated metadata or caller capability constructor.
#[derive(Clone, Debug)]
pub struct FmiNativeDeploymentView {
    root: RetainedAssertionRoot,
    capabilities: FmiDeploymentCapabilities,
    carried: Vec<FmiCarryPair>,
}

impl FmiNativeDeploymentView {
    pub fn from_preparation(
        inputs: &RetainedNativePreparation,
    ) -> Result<Self, NativeDeploymentError> {
        let root = inputs.root();
        for variable in root.metadata().variables() {
            check_getter(inputs.schedule(), variable)?;
        }
        // Complete native schedule issuance already refuses state/event obligations.
        // It is not permission to publish synthetic derivative/indicator accessors.
        if !root.metadata().state_variable_indices().is_empty() {
            return Err(NativeDeploymentError::Profile(
                "native publication has no derivative accessor",
            ));
        }
        let capabilities = FmiDeploymentCapabilities {
            interfaces: FmiInterfaceProfile::CoSimulation,
            variable_kinds: vec![Kind::Real, Kind::Integer, Kind::Boolean],
            fmu_state: false,
        }
        .admit(root.metadata())?;
        Ok(Self {
            root: root.clone(),
            capabilities,
            carried: Vec::new(),
        })
    }
    /// This view with the carry `pairs` the package declares, and the carried
    /// state they pair in `schedule`'s storage (SOLVE-C101). Each pair names an
    /// output and an input by value reference with the same value type and
    /// dimensions; the declaration is admitted only when the state
    /// constructs.
    pub fn declaring_carried(
        mut self,
        schedule: &NativeRefreshAssignmentSchedule,
        pairs: &[FmiCarryPair],
    ) -> Result<(Self, crate::NativeCarriedState), NativeDeploymentError> {
        let variable = |reference: u32| {
            self.variables()
                .iter()
                .find(|variable| variable.value_reference_fmi3() == reference)
                .ok_or(NativeCarryError::UnknownReference(reference))
        };
        let mut names = Vec::with_capacity(pairs.len());
        for pair in pairs {
            let (output, input) = (variable(pair.output)?, variable(pair.input)?);
            if output.causality() != FmiCausality::Output {
                return Err(NativeCarryError::NotOutput(pair.output).into());
            }
            if input.causality() != FmiCausality::Input {
                return Err(NativeCarryError::NotInput(pair.input).into());
            }
            if output.value_kind() != input.value_kind()
                || output.dimensions() != input.dimensions()
            {
                return Err(NativeCarryError::Mismatch {
                    output: pair.output,
                    input: pair.input,
                }
                .into());
            }
            names.push((output.name(), input.name()));
        }
        let state =
            crate::NativeCarriedState::declared(&self.root.model().problem, schedule, &names)
                .map_err(NativeCarryError::from)?;
        self.carried = pairs.to_vec();
        Ok((self, state))
    }
    /// The declared host contract pairs, in declaration order.
    pub fn carried(&self) -> &[FmiCarryPair] {
        &self.carried
    }
    pub fn root(&self) -> &RetainedAssertionRoot {
        &self.root
    }
    pub fn variables(&self) -> &[FmiVariable] {
        self.root.metadata().variables()
    }
    pub fn capabilities(&self) -> &FmiDeploymentCapabilities {
        &self.capabilities
    }
    pub fn instantiation_values(
        &self,
        buffer: FmiInstantiationBuffer,
    ) -> &crate::SolveInitialValues {
        match buffer {
            FmiInstantiationBuffer::Solver => &self.root.model().initial_y,
            FmiInstantiationBuffer::Parameters => &self.root.model().parameters,
        }
    }
    pub fn serialize_variable_entries<M: SerializeMap>(
        &self,
        index: usize,
        entries: &mut M,
    ) -> Result<(), M::Error> {
        let variable = self.variables().get(index).ok_or_else(|| {
            <M::Error as serde::ser::Error>::custom("FMI variable ordinal is out of bounds")
        })?;
        super::metadata_projection::SerializedFmiVariable(variable).serialize_entries(entries)
    }
    pub fn serialize_entries<M: SerializeMap>(&self, entries: &mut M) -> Result<(), M::Error> {
        entries.serialize_entry("deployment", &self.capabilities)?;
        entries.serialize_entry("native_logger", &true)?;
        entries.serialize_entry(
            "variables",
            &super::metadata_projection::SerializedFmiVariables::borrowing(self.variables()),
        )?;
        entries.serialize_entry("enumerations", &self.root.metadata().enumerations())?;
        entries.serialize_entry("event_indicator_roots", &[] as &[usize])?;
        entries.serialize_entry("carried", &self.carried)?;
        entries.serialize_entry(
            "derivative_value_reference_base_fmi3",
            &self.root.metadata().derivative_value_reference_base_fmi3(),
        )?;
        Ok(())
    }
}

fn check_getter(
    schedule: &NativeRefreshAssignmentSchedule,
    variable: &FmiVariable,
) -> Result<(), NativeDeploymentError> {
    check_getter_storage(variable)?;
    if matches!(variable.value_kind(), Kind::Integer | Kind::Boolean)
        || variable.role() == Some(Role::DiscreteReal)
    {
        check_native_typed_membership(schedule, variable)?;
    }
    Ok(())
}

pub(super) fn check_getter_storage(variable: &FmiVariable) -> Result<(), NativeDeploymentError> {
    let storage = variable.storage().ok_or(NativeDeploymentError::Profile(
        "published variable has no native getter storage",
    ))?;
    match variable.value_kind() {
        Kind::Enumeration | Kind::String => {
            return Err(NativeDeploymentError::Profile(
                "published variable kind has no native getter",
            ));
        }
        Kind::Integer | Kind::Boolean => {
            if storage.column() != FmiStorageColumn::P {
                return Err(NativeDeploymentError::Profile(
                    "published typed variable has no native P storage",
                ));
            }
        }
        Kind::Real => {}
    }
    Ok(())
}

/// The single schedule membership query used by native typed getters and publication.
/// Callers must already hold the variable from the same retained root inventory.
/// Whether the variable's typed cells are host-written input lanes: external
/// inputs, parameters and constants. Every other typed variable is derived.
pub fn reads_input_lanes(variable: &FmiVariable) -> bool {
    matches!(
        variable.role(),
        Some(Role::ExternalInput | Role::Parameter | Role::Constant)
    )
}

pub fn check_native_typed_membership(
    schedule: &NativeRefreshAssignmentSchedule,
    variable: &FmiVariable,
) -> Result<(), NativeDeploymentError> {
    let storage = variable.storage().ok_or(NativeDeploymentError::Profile(
        "native typed VR has no storage",
    ))?;
    let expected = match variable.value_kind() {
        Kind::Real => NativeOutputLane::Real,
        Kind::Integer => NativeOutputLane::Integer,
        Kind::Boolean => NativeOutputLane::Boolean,
        Kind::Enumeration | Kind::String => {
            return Err(NativeDeploymentError::Profile(
                "native typed VR kind is unsupported",
            ));
        }
    };
    let end = storage.base().checked_add(storage.scalar_count()).ok_or(
        NativeDeploymentError::Profile("native original storage overflow"),
    )?;
    let mut count = 0usize;
    let mut cell = |index: usize, lane: NativeOutputLane, cells: usize| {
        if index >= storage.base() && index < end {
            if lane != expected {
                return Err(NativeDeploymentError::Profile(
                    "native descriptor differs from the original VR type",
                ));
            }
            count = count
                .checked_add(cells)
                .ok_or(NativeDeploymentError::Profile(
                    "native descriptor coverage overflow",
                ))?;
        }
        Ok(())
    };
    if reads_input_lanes(variable) {
        for input in schedule.input_runs() {
            let range = input.p_range();
            let first = range.start.max(storage.base());
            let last = range.end.min(end);
            if first < last {
                cell(first, input.lane(), last - first)?;
            }
        }
    } else {
        for output in schedule.derived_outputs() {
            cell(output.p_index(), output.lane(), 1)?;
        }
    }
    if count != storage.scalar_count() {
        return Err(NativeDeploymentError::TypedInventory(Box::new(
            NativeTypedInventoryGap {
                value_reference: variable.value_reference_fmi3(),
                name: variable.name().to_owned(),
                kind: variable.value_kind(),
                causality: variable.causality(),
                expected,
                required: storage.scalar_count(),
                covered: count,
            },
        )));
    }
    Ok(())
}
