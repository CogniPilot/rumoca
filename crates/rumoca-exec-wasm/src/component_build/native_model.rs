//! Source-correlated native model emission; FMI assembly remains separate.

use rumoca_ir_solve::{
    RetainedNativePreparation, RetainedNativeScheduleError, fmi::FmiDeploymentView,
};

use crate::{CompiledNativeCallProgramWasm, PreparedNativeWasmCode};
#[cfg(feature = "component-build")]
mod contract;
#[cfg(feature = "component-build")]
mod linkable;
#[cfg(feature = "component-build")]
pub use linkable::NativeLinkableModelLibrary;

/// The existing emitter product and its original checked deployment source.
/// This is preparation, without linked-entry, execution or FMI publication authority.
/// See SPEC_0044 ME-LSW-003 for its assembly contract.
///
/// ```compile_fail
/// use rumoca_exec_wasm::{NativeModelLibrary, PreparedNativeWasmCode};
/// use rumoca_ir_solve::fmi::FmiDeploymentView;
/// fn forge(code: PreparedNativeWasmCode, deployment: FmiDeploymentView) -> NativeModelLibrary {
///     NativeModelLibrary { code, deployment }
/// }
/// ```
pub struct NativeModelLibrary {
    code: PreparedNativeWasmCode,
    deployment: FmiDeploymentView,
}

impl NativeModelLibrary {
    /// Check this actual emitted module's structural shared-library contract.
    /// Success grants no linked entry, allocator, execution or FMI authority.
    #[cfg(feature = "component-build")]
    pub fn check_linkage_contract(&self) -> Result<(), String> {
        contract::check(self.compiled())
    }

    /// Consume already-issued preparation and deployment without re-emission.
    pub fn from_preparation(
        code: PreparedNativeWasmCode,
        deployment: FmiDeploymentView,
    ) -> Result<Self, RetainedNativeScheduleError> {
        let source = deployment.runtime_view().retain_assertion_root();
        let emitted = code.inputs().root();
        if !emitted.shares_root_owner(&source)
            || !std::ptr::eq(emitted.metadata(), source.metadata())
        {
            return Err(RetainedNativeScheduleError::ForeignPreparation);
        }
        Ok(Self { code, deployment })
    }

    /// Borrow the actual emitted module, scratch layout and original fault inventory.
    pub fn compiled(&self) -> &CompiledNativeCallProgramWasm {
        self.code.compiled()
    }

    /// Borrow the original root and its issued native schedule.
    pub fn inputs(&self) -> &RetainedNativePreparation {
        self.code.inputs()
    }

    /// Borrow the same deployment source for canonical compilation orchestration.
    pub fn deployment(&self) -> &FmiDeploymentView {
        &self.deployment
    }
}

#[cfg(test)]
mod tests {
    use std::collections::HashMap;

    use super::NativeModelLibrary;
    use rumoca_ir_solve::fmi::{FmiComponent, FmiDeploymentCapabilities};

    /// The reported Integer-precision source, with its input start spelled as
    /// the checked default the FMI lowering needs.
    const SOURCE: &str = include_str!(concat!(
        env!("CARGO_MANIFEST_DIR"),
        "/../rumoca-compile/src/session/tests/fixtures/reported_bugs/issue_411.mo"
    ));

    fn component() -> FmiComponent {
        let mut session = rumoca_compile::Session::default();
        session
            .add_document(
                "IntegerPrecision.mo",
                &SOURCE.replace("(start = 0);", " = 0;"),
            )
            .unwrap();
        let compiled = session.compile_model("IntegerPrecision").unwrap();
        rumoca_phase_solve::fmi::lower_to_fmi_component(&compiled.dae, &HashMap::new()).unwrap()
    }

    fn library() -> NativeModelLibrary {
        let component = component();
        let root = component.runtime_view().retain_assertion_root();
        let code =
            crate::compile_retained_native_preparation_wasm(root.native_preparation().unwrap())
                .unwrap();
        let deployment = component
            .into_codegen_view()
            .try_deployment(FmiDeploymentCapabilities::default())
            .unwrap();
        NativeModelLibrary::from_preparation(code, deployment).unwrap()
    }

    #[test]
    fn a_library_borrows_the_deployment_over_the_root_it_emitted() {
        let library = library();
        let retained = library.deployment().runtime_view().retain_assertion_root();
        assert!(retained.shares_root_owner(library.inputs().root()));
    }

    #[test]
    fn a_linkable_library_keeps_the_original_it_refines() {
        let library = library();
        let inputs: *const _ = library.inputs();
        let linkable = library.into_linkable().unwrap();
        assert!(std::ptr::eq(linkable.original().inputs(), inputs));
        assert!(!linkable.module_bytes().is_empty());
    }
}
