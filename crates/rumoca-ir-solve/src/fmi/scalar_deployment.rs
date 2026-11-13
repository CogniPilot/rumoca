//! Co-Simulation deployment of a stateful (scalar-product) component.

use super::*;
use crate::{
    RetainedAssertionRoot, SolveVariableStorageRole as Role, SolveVariableValueKind as Kind,
};
use serde::ser::SerializeMap;

/// Scalar deployment refusal: the neutral admission refusal, or the exact
/// advertised variable the scalar FMI route has no owner for.
#[derive(Debug, thiserror::Error)]
pub enum ScalarDeploymentError {
    #[error(transparent)]
    Admission(#[from] FmiDeploymentError),
    #[error(
        "the scalar FMI route has no owner for VR {value_reference} ({name}, {kind:?}{storage}); \
         it must be implemented, not hidden"
    )]
    Unowned {
        value_reference: u32,
        name: String,
        kind: Kind,
        storage: &'static str,
    },
    #[error(
        "the scalar FMI route serves a String only as a parameter or constant with a literal \
         start; VR {value_reference} ({name}) is not one"
    )]
    UnservedString { value_reference: u32, name: String },
}

/// The correlated retained root of a stateful component, admitted for the
/// fixed Co-Simulation adapter's scalar product: Co-Simulation only, no FMU
/// state, and the variable kinds the scalar route owns. States stay internal
/// to the component; its
/// complete checked metadata (variables, initial values, model structure) is
/// rendered unchanged. No deserialization or caller capability constructor.
#[derive(Clone, Debug)]
pub struct FmiScalarDeploymentView {
    root: RetainedAssertionRoot,
    capabilities: FmiDeploymentCapabilities,
}

impl FmiScalarDeploymentView {
    /// Admit `root`, refusing any advertised variable the scalar route cannot
    /// serve: a storage-less entry other than the declared maximum step
    /// duration (independent time is not a metadata variable), a non-Real
    /// entry outside P (the typed request owner reads and writes only P
    /// cells; lowering places every discrete value there), or a String that
    /// is not a parameter or constant with a literal start (the generated C
    /// kernel's String profile: String values are never evaluated). The
    /// admitted kinds are Real and those present of Integer, Boolean,
    /// Enumeration and String.
    pub fn from_root(root: &RetainedAssertionRoot) -> Result<Self, ScalarDeploymentError> {
        const OWNED: [Kind; 5] = [
            Kind::Real,
            Kind::Integer,
            Kind::Boolean,
            Kind::Enumeration,
            Kind::String,
        ];
        let metadata = root.metadata();
        let max_step_duration = metadata.max_step_duration();
        for variable in metadata.variables() {
            let storage_less = variable.storage().is_none()
                && !max_step_duration.is_some_and(|declared| std::ptr::eq(declared, variable));
            let typed_outside_p = variable.value_kind() != Kind::Real
                && variable
                    .storage()
                    .is_some_and(|run| run.column() != FmiStorageColumn::P);
            if !OWNED.contains(&variable.value_kind()) || storage_less || typed_outside_p {
                return Err(ScalarDeploymentError::Unowned {
                    value_reference: variable.value_reference_fmi3(),
                    name: variable.name().to_owned(),
                    kind: variable.value_kind(),
                    storage: if storage_less {
                        ", no storage"
                    } else if typed_outside_p {
                        ", outside P"
                    } else {
                        ""
                    },
                });
            }
            if variable.value_kind() == Kind::String
                && (!matches!(variable.role(), Some(Role::Parameter | Role::Constant))
                    || variable.text_start().is_none())
            {
                return Err(ScalarDeploymentError::UnservedString {
                    value_reference: variable.value_reference_fmi3(),
                    name: variable.name().to_owned(),
                });
            }
        }
        let variable_kinds = OWNED
            .into_iter()
            .filter(|&kind| {
                kind == Kind::Real
                    || metadata
                        .variables()
                        .iter()
                        .any(|variable| variable.value_kind() == kind)
            })
            .collect();
        let capabilities = FmiDeploymentCapabilities {
            interfaces: FmiInterfaceProfile::CoSimulation,
            variable_kinds,
            fmu_state: false,
        }
        .admit(metadata)?;
        Ok(Self {
            root: root.clone(),
            capabilities,
        })
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
    /// The same root-only entries the native deployment renders, with no
    /// carried pairs and no event indicators (Co-Simulation hides them).
    pub fn serialize_entries<M: SerializeMap>(&self, entries: &mut M) -> Result<(), M::Error> {
        entries.serialize_entry("deployment", &self.capabilities)?;
        entries.serialize_entry("native_logger", &true)?;
        entries.serialize_entry(
            "variables",
            &super::metadata_projection::SerializedFmiVariables::borrowing(self.variables()),
        )?;
        entries.serialize_entry("enumerations", &self.root.metadata().enumerations())?;
        entries.serialize_entry("event_indicator_roots", &[] as &[usize])?;
        entries.serialize_entry("carried", &[] as &[FmiCarryPair])?;
        entries.serialize_entry(
            "derivative_value_reference_base_fmi3",
            &self.root.metadata().derivative_value_reference_base_fmi3(),
        )?;
        Ok(())
    }
}
