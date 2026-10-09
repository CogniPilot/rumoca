//! Target-neutral capabilities retain the correlated component before execution admission.

use serde::{Deserialize, Serialize};

use super::{FmiCCodegenError, FmiCCodegenView, FmiCodegenView, FmiMetadata};
use crate::SolveVariableValueKind;

/// Interfaces implemented by a storage-backed deployment adapter.
#[derive(Debug, Clone, Copy, Deserialize, Serialize, PartialEq, Eq)]
#[serde(rename_all = "kebab-case")]
pub enum FmiInterfaceProfile {
    ModelExchangeAndCoSimulation,
    CoSimulation,
}

/// Untrusted target-declared deployment capabilities. Admission checks these
/// against the retained component before they become its rendering facts.
#[derive(Debug, Clone, Deserialize, Serialize)]
#[serde(deny_unknown_fields)]
pub struct FmiDeploymentCapabilities {
    pub interfaces: FmiInterfaceProfile,
    pub variable_kinds: Vec<SolveVariableValueKind>,
    pub fmu_state: bool,
}

impl Default for FmiDeploymentCapabilities {
    fn default() -> Self {
        use SolveVariableValueKind::{Boolean, Enumeration, Integer, Real, String};
        Self {
            interfaces: FmiInterfaceProfile::ModelExchangeAndCoSimulation,
            variable_kinds: vec![Real, Integer, Boolean, Enumeration, String],
            fmu_state: true,
        }
    }
}

#[derive(Debug, thiserror::Error)]
#[error("FMI deployment profile: {0}")]
pub struct FmiDeploymentError(pub(super) &'static str);

/// Checked accessor/interface capabilities bound to one correlated component.
/// This proves deployment coverage, not execution-backend coverage.
/// No raw constructor, deserialization or owned component escape is available.
///
/// ```compile_fail
/// use rumoca_ir_solve::fmi::{FmiCodegenView, FmiDeploymentCapabilities, FmiDeploymentView};
/// fn forge(component: FmiCodegenView) -> FmiDeploymentView {
///     FmiDeploymentView { component, capabilities: FmiDeploymentCapabilities::default() }
/// }
/// ```
#[derive(Debug)]
pub struct FmiDeploymentView {
    component: FmiCodegenView,
    capabilities: FmiDeploymentCapabilities,
}

impl FmiCodegenView {
    /// Consume the correlated view without copying or rebuilding its inventory.
    /// Accessor admission is independent of C or native-WASM execution admission.
    pub fn try_deployment(
        self,
        requested: FmiDeploymentCapabilities,
    ) -> Result<FmiDeploymentView, FmiDeploymentError> {
        let capabilities = requested.admit(&self.metadata)?;
        Ok(FmiDeploymentView {
            component: self,
            capabilities,
        })
    }
}

impl FmiDeploymentCapabilities {
    pub(super) fn admit(self, metadata: &FmiMetadata) -> Result<Self, FmiDeploymentError> {
        if !self.variable_kinds.contains(&SolveVariableValueKind::Real) {
            return Err(FmiDeploymentError(
                "deployment must expose the independent Float64 time",
            ));
        }
        for (index, kind) in self.variable_kinds.iter().enumerate() {
            if self.variable_kinds[..index].contains(kind) {
                return Err(FmiDeploymentError(
                    "deployment variable-access kinds are duplicated",
                ));
            }
        }
        for variable in metadata.variables() {
            if !self.variable_kinds.contains(&variable.value_kind()) {
                return Err(unsupported_accessor(variable.value_kind()));
            }
        }
        Ok(self)
    }
}

impl FmiDeploymentView {
    #[must_use]
    pub const fn component(&self) -> &FmiCodegenView {
        &self.component
    }

    #[must_use]
    pub const fn capabilities(&self) -> &FmiDeploymentCapabilities {
        &self.capabilities
    }

    /// Borrow the same runtime root and semantic owners admitted for deployment.
    #[must_use]
    pub fn runtime_view(&self) -> super::FmiRuntimeView<'_> {
        super::FmiRuntimeView {
            model: &self.component.model,
            metadata: &self.component.metadata,
            event_indicators: &self.component.event_indicators,
            root_location: &self.component.root_location,
        }
    }

    #[must_use]
    pub const fn co_simulation(&self) -> &super::CoSimulationStepPlan {
        &self.component.co_simulation
    }

    /// Apply the complete existing C execution profile to this checked product.
    pub fn try_c(self) -> Result<FmiCCodegenView, FmiCCodegenError> {
        FmiCCodegenView::from_deployment(self)
    }

    pub(super) fn into_parts(self) -> (FmiCodegenView, FmiDeploymentCapabilities) {
        (self.component, self.capabilities)
    }
}

fn unsupported_accessor(kind: SolveVariableValueKind) -> FmiDeploymentError {
    FmiDeploymentError(match kind {
        SolveVariableValueKind::Real => "unsupported-feature:fmi-variable-access-real",
        SolveVariableValueKind::Integer => "unsupported-feature:fmi-variable-access-integer",
        SolveVariableValueKind::Boolean => "unsupported-feature:fmi-variable-access-boolean",
        SolveVariableValueKind::Enumeration => {
            "unsupported-feature:fmi-variable-access-enumeration"
        }
        SolveVariableValueKind::String => "unsupported-feature:fmi-variable-access-string",
    })
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{SolveModel, fmi::FmiComponent};

    fn component() -> FmiCodegenView {
        FmiComponent::construct(SolveModel::default(), Vec::new())
            .unwrap()
            .into_codegen_view()
    }

    #[test]
    fn deployment_retains_the_exact_kernel_and_seals_metadata_capabilities() {
        let view = component();
        let kernel = std::ptr::from_ref(view.problem());
        let requested = FmiDeploymentCapabilities {
            interfaces: FmiInterfaceProfile::CoSimulation,
            variable_kinds: vec![SolveVariableValueKind::Real],
            fmu_state: false,
        };
        let view = view.try_deployment(requested).unwrap();
        assert_eq!(std::ptr::from_ref(view.component().problem()), kernel);
        assert_eq!(
            view.capabilities().interfaces,
            FmiInterfaceProfile::CoSimulation
        );
        let runtime = view.runtime_view();
        assert_eq!(std::ptr::from_ref(&runtime.model().problem), kernel);
        let view = view.try_c().unwrap();
        assert_eq!(std::ptr::from_ref(view.problem()), kernel);
        let json = serde_json::to_value(&view).unwrap();
        assert_eq!(json["deployment"]["interfaces"], "co-simulation");
        assert_eq!(json["deployment"]["fmu_state"], false);
        assert_eq!(
            json["deployment"]["variable_kinds"],
            serde_json::json!(["Real"])
        );
    }

    #[test]
    fn deployment_refuses_missing_independent_time_and_duplicate_kinds() {
        let mut requested = FmiDeploymentCapabilities {
            variable_kinds: vec![SolveVariableValueKind::Integer],
            ..FmiDeploymentCapabilities::default()
        };
        assert!(component().try_deployment(requested.clone()).is_err());
        requested.variable_kinds = vec![SolveVariableValueKind::Real; 2];
        assert!(component().try_deployment(requested).is_err());
    }
}
