//! Persist emitted bytes; rederive every control/workspace decision from the root.
use super::*;
use crate::program_payload::ProgramPayload;
use serde::{Deserialize, Serialize};
mod call;
pub use call::{FmiDerivativeCall, FmiDerivativeFailure, FmiKinematicCall};
#[cfg(all(target_arch = "wasm32", target_os = "unknown"))]
mod runtime;
#[cfg(test)]
mod tampering_tests;

#[derive(Clone, Serialize, Deserialize)]
#[serde(deny_unknown_fields)]
pub struct PrecompiledFmiDerivativeOperation {
    version: u32,
    source_key: String,
    #[serde(with = "serde_bytes")]
    bytes: Vec<u8>,
    static_program: Option<ProgramPayload>,
    dynamic_program: Option<ProgramPayload>,
    values: ProgramPayload,
    kinematic: Option<ProgramPayload>,
}

/// Bound bytes still carry no engine admission. The workspace is freshly issued
/// from the same original operation; no persisted frame offsets are trusted.
pub struct BoundFmiDerivativeOperation {
    #[cfg(all(target_arch = "wasm32", target_os = "unknown"))]
    runtime: runtime::DerivativeRuntime,
    bytes: Vec<u8>,
    workspace: FmiDerivativeWorkspace,
    // The refresh helpers are kept to explain a status their call returned,
    // which only the target with an engine can receive.
    #[cfg(all(target_arch = "wasm32", target_os = "unknown"))]
    static_program: Option<ProgramPayload>,
    #[cfg(all(target_arch = "wasm32", target_os = "unknown"))]
    dynamic_program: Option<ProgramPayload>,
    values: ProgramPayload,
    kinematic: Option<ProgramPayload>,
}

impl PrecompiledFmiDerivativeOperation {
    pub fn from_compiled(program: &CompiledFmiDerivativeOperationWasm) -> Self {
        Self {
            version: 2,
            source_key: program.source_key.clone(),
            bytes: program.bytes.clone(),
            static_program: program.static_program.as_ref().map(ProgramPayload::from),
            dynamic_program: program.dynamic_program.as_ref().map(ProgramPayload::from),
            values: ProgramPayload::from(&program.values),
            kinematic: program.kinematic.as_ref().map(ProgramPayload::from),
        }
    }

    /// Bind persisted bytes to `operation`. Every persisted byte, the control
    /// module and each helper's module, scratch and fault tables, is emitted
    /// again from the issued source and must be equal; nothing persisted is
    /// trusted and no part is reused when one differs.
    pub fn bind(
        &self,
        operation: &FmiDerivativeCausalOperation<'_>,
    ) -> Result<BoundFmiDerivativeOperation, WasmCompileError> {
        let source = operation.source();
        let expected =
            rumoca_ir_solve::PreparedKernelSource::new(&source.problem().layout, source.calls())
                .and_then(|key| key.fmi_derivative(operation))
                .map_err(WasmCompileError::Input)?;
        let policy = CheckedNativeWasmCapacityPolicy::default();
        let oversized = std::iter::once(self.bytes.len())
            .chain(
                self.helpers()
                    .map(|(_, p)| p.map_or(0, |p| p.module_bytes().len())),
            )
            .any(|bytes| bytes > (8 << 20))
            || self
                .helpers()
                .filter_map(|(_, p)| p)
                .any(|p| p.scratch_bytes() > policy.public_scratch_bytes());
        if self.version != 2 || self.source_key != expected || oversized {
            return Err(WasmCompileError::Input(
                "precompiled FMI derivative belongs to another source, capability or extent".into(),
            ));
        }
        let issued = compile_fmi_derivative_operation_wasm(operation)?;
        if let Some(mismatch) = self.mismatch(&Self::from_compiled(&issued)) {
            return Err(WasmCompileError::Input(format!(
                "precompiled FMI derivative differs from its issued emission: {mismatch}"
            )));
        }
        self.bound(issued.workspace)
    }

    fn helpers(&self) -> impl Iterator<Item = (FmiDerivativeHelperRole, Option<&ProgramPayload>)> {
        [
            (
                FmiDerivativeHelperRole::StaticRefresh,
                self.static_program.as_ref(),
            ),
            (
                FmiDerivativeHelperRole::DynamicRefresh,
                self.dynamic_program.as_ref(),
            ),
            (FmiDerivativeHelperRole::Values, Some(&self.values)),
            (FmiDerivativeHelperRole::Kinematic, self.kinematic.as_ref()),
        ]
        .into_iter()
    }

    /// The first persisted part that differs from `issued`, in persisted order.
    fn mismatch(&self, issued: &Self) -> Option<FmiDerivativeArtifactMismatch> {
        if self.bytes != issued.bytes {
            return Some(FmiDerivativeArtifactMismatch::Control);
        }
        self.helpers()
            .zip(issued.helpers())
            .find_map(|((helper, persisted), (_, issued))| {
                let part = match (persisted, issued) {
                    (None, None) => return None,
                    (Some(persisted), Some(issued)) => helper_mismatch(persisted, issued)?,
                    _ => FmiHelperMismatch::Presence,
                };
                Some(FmiDerivativeArtifactMismatch::Helper { helper, part })
            })
    }

    fn bound(
        &self,
        workspace: FmiDerivativeWorkspace,
    ) -> Result<BoundFmiDerivativeOperation, WasmCompileError> {
        #[cfg(all(target_arch = "wasm32", target_os = "unknown"))]
        let runtime = runtime::DerivativeRuntime::new(self)?;
        Ok(BoundFmiDerivativeOperation {
            #[cfg(all(target_arch = "wasm32", target_os = "unknown"))]
            runtime,
            bytes: self.bytes.clone(),
            workspace,
            #[cfg(all(target_arch = "wasm32", target_os = "unknown"))]
            static_program: self.static_program.clone(),
            #[cfg(all(target_arch = "wasm32", target_os = "unknown"))]
            dynamic_program: self.dynamic_program.clone(),
            values: self.values.clone(),
            kinematic: self.kinematic.clone(),
        })
    }
}

impl CompiledFmiDerivativeOperationWasm {
    /// Bind code this build has just emitted from its issued source; only
    /// persisted bytes need the re-emission [`PrecompiledFmiDerivativeOperation::bind`]
    /// performs.
    pub fn bind(&self) -> Result<BoundFmiDerivativeOperation, WasmCompileError> {
        PrecompiledFmiDerivativeOperation::from_compiled(self).bound(self.workspace)
    }
}

/// A helper the complete operation links.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub(crate) enum FmiDerivativeHelperRole {
    StaticRefresh,
    DynamicRefresh,
    Values,
    Kinematic,
}

/// What of a persisted helper differs from its issued emission.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub(crate) enum FmiHelperMismatch {
    Presence,
    Module,
    Scratch,
    Faults,
}

/// The first part of a persisted complete operation that differs from what
/// its issued source emits.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub(crate) enum FmiDerivativeArtifactMismatch {
    Control,
    Helper {
        helper: FmiDerivativeHelperRole,
        part: FmiHelperMismatch,
    },
}

impl std::fmt::Display for FmiDerivativeArtifactMismatch {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Control => f.write_str("control module"),
            Self::Helper { helper, part } => write!(f, "{helper:?} helper {part:?}"),
        }
    }
}

fn helper_mismatch(
    persisted: &ProgramPayload,
    issued: &ProgramPayload,
) -> Option<FmiHelperMismatch> {
    if persisted.module_bytes() != issued.module_bytes() {
        Some(FmiHelperMismatch::Module)
    } else if persisted.scratch_bytes() != issued.scratch_bytes() {
        Some(FmiHelperMismatch::Scratch)
    } else if persisted.fault_tables() != issued.fault_tables() {
        Some(FmiHelperMismatch::Faults)
    } else {
        None
    }
}

impl BoundFmiDerivativeOperation {
    pub fn module_bytes(&self) -> &[u8] {
        &self.bytes
    }
    pub fn workspace(&self) -> FmiDerivativeWorkspace {
        self.workspace
    }
    pub fn value_module_bytes(&self) -> &[u8] {
        self.values.module_bytes()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use rumoca_ir_solve::fmi::FmiComponent;

    pub(super) fn build_component(coefficient: usize) -> FmiComponent {
        let mut session = rumoca_compile::Session::default();
        session.add_document("persisted-derivative.mo", &format!("model PersistedDerivative parameter Real p=2; Real x(start=1,fixed=true); Real a; Real b; equation a={coefficient}*p; b=a*x; der(x)=b; end PersistedDerivative;")).unwrap();
        let compiled = session.compile_model("PersistedDerivative").unwrap();
        rumoca_phase_solve::fmi::lower_to_fmi_component(&compiled.dae, &Default::default()).unwrap()
    }

    #[test]
    fn persisted_entry_reissues_workspace_and_exact_original_control() {
        let component = build_component(3);
        let root = component.runtime_view().retain_assertion_root();
        let operation = root
            .try_derivative_operation()
            .unwrap()
            .try_causal_operation()
            .unwrap();
        let compiled = compile_fmi_derivative_operation_wasm(&operation).unwrap();
        let wire = PrecompiledFmiDerivativeOperation::from_compiled(&compiled);
        let json = serde_json::to_value(&wire).unwrap();
        assert!(json.get("workspace").is_none());
        assert!(json.get("plan").is_none());
        let replay: PrecompiledFmiDerivativeOperation =
            serde_json::from_value(json.clone()).unwrap();
        let bound = replay.bind(&operation).unwrap();
        let fresh_component = build_component(3);
        let fresh_root = fresh_component.runtime_view().retain_assertion_root();
        let fresh_operation = fresh_root
            .try_derivative_operation()
            .unwrap()
            .try_causal_operation()
            .unwrap();
        assert!(replay.bind(&fresh_operation).is_ok());
        assert_eq!(bound.module_bytes(), compiled.module_bytes());
        assert_eq!(bound.workspace().regions(), compiled.workspace().regions());
        assert_eq!(bound.value_module_bytes(), compiled.values().module_bytes());
        let mut forged = json;
        forged["workspace"] = serde_json::json!({"bytes":0});
        assert!(serde_json::from_value::<PrecompiledFmiDerivativeOperation>(forged).is_err());
    }

    #[test]
    fn foreign_source_missing_helper_and_modified_control_refuse_before_engine_binding() {
        let component = build_component(3);
        let root = component.runtime_view().retain_assertion_root();
        let operation = root
            .try_derivative_operation()
            .unwrap()
            .try_causal_operation()
            .unwrap();
        assert!(operation.static_schedule().is_some());
        let compiled = compile_fmi_derivative_operation_wasm(&operation).unwrap();
        let wire = PrecompiledFmiDerivativeOperation::from_compiled(&compiled);
        let changed_component = build_component(4);
        let changed_root = changed_component.runtime_view().retain_assertion_root();
        let changed = changed_root
            .try_derivative_operation()
            .unwrap()
            .try_causal_operation()
            .unwrap();
        assert!(wire.bind(&changed).is_err());
        let mut missing = wire.clone();
        missing.static_program = None;
        assert!(missing.bind(&operation).is_err());
        let mut modified = wire.clone();
        modified.bytes[0] ^= 1;
        assert!(modified.bind(&operation).is_err());
        let mut oversized = wire;
        oversized.bytes.resize((8 << 20) + 1, 0);
        assert!(oversized.bind(&operation).is_err());
    }
}
