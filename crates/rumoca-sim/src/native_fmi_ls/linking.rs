//! Closed original-model/data/fixed-runtime graph; no selectable providers.

use super::{
    NativeFmiLsPreparationError, PreparedNativeFmiLsModel, PreparedScalarFmiLsModel, data,
    publication,
};
use rumoca_core::artifact_build::{ArtifactBuildBinding, PreparedBuildRequest};
use rumoca_exec_wasm::{FixedNativeGuestCore, NativeLinkableModelLibrary};

/// Complete checked component and exact publication inventory/slot binding.
/// Only the consuming original pair + fixed-source core issuer constructs it.
/// This is not a raw-byte or imported-symbol execution capability.
pub struct LinkedNativeFmiLsComponent<S> {
    binding: ArtifactBuildBinding<S>,
    bytes: Vec<u8>,
    model: NativeLinkableModelLibrary,
    core: FixedNativeGuestCore,
}

impl<S> LinkedNativeFmiLsComponent<S> {
    pub fn binding(&self) -> &ArtifactBuildBinding<S> {
        &self.binding
    }
    pub fn bytes(&self) -> &[u8] {
        &self.bytes
    }
    pub fn original_model(&self) -> &NativeLinkableModelLibrary {
        &self.model
    }
    pub fn fixed_core(&self) -> &FixedNativeGuestCore {
        &self.core
    }

    /// Consume after packaging has checked the exact original binding. Dropping
    /// the binding releases the inventory before its files are published.
    pub fn into_bytes(self) -> Vec<u8> {
        self.bytes
    }
}

impl PreparedNativeFmiLsModel {
    /// Pair all providers internally, preserving original source ownership.
    /// No arbitrary capsule, token, model body, runtime body or alias is accepted.
    pub fn link_fixed_guest<S>(
        self,
        core: FixedNativeGuestCore,
        request: &PreparedBuildRequest<'_, S>,
    ) -> Result<LinkedNativeFmiLsComponent<S>, NativeFmiLsPreparationError> {
        if core.is_scalar_product() {
            return Err(NativeFmiLsPreparationError::Linkage(
                "a native component links only the native fixed core".into(),
            ));
        }
        let token = publication::token(request).map_err(NativeFmiLsPreparationError::Linkage)?;
        let envelope = rumoca_exec_wasm::encode_native_source_envelope(
            &self.capsule,
            self.library.compiled().capacity_policy(),
        )
        .map_err(NativeFmiLsPreparationError::Linkage)?;
        let data =
            data::library(&envelope, &token).map_err(NativeFmiLsPreparationError::Linkage)?;
        let model = self
            .library
            .into_linkable()
            .map_err(NativeFmiLsPreparationError::Linkage)?;
        let bytes = link(&model, &data, &core).map_err(NativeFmiLsPreparationError::Linkage)?;
        core.check_component_interface(&bytes)
            .map_err(NativeFmiLsPreparationError::Linkage)?;
        Ok(LinkedNativeFmiLsComponent {
            binding: request.binding(),
            bytes,
            model,
            core,
        })
    }
}

fn link(
    model: &NativeLinkableModelLibrary,
    data: &[u8],
    core: &FixedNativeGuestCore,
) -> Result<Vec<u8>, String> {
    // Model target has only its actual checked entry export. Data has only the
    // four issued receiver exports; core validation refuses those five names.
    // Thus linker priority cannot select another equal-signature slot provider.
    wit_component::Linker::default()
        .validate(true)
        .stub_missing_functions(false)
        .use_built_in_libdl(false)
        .library("rumoca-model", model.module_bytes(), false)
        .and_then(|linker| linker.library("rumoca-source", data, false))
        .and_then(|linker| linker.library("rumoca-runtime", core.bytes(), false))
        .and_then(wit_component::Linker::encode)
        .map_err(|error| format!("link original fixed FMI-LS graph: {error:#}"))
}

/// A stateful (scalar-product) component: the source data and the scalar fixed
/// core only. No model-entry library is linked; the core never imports one.
pub struct LinkedScalarFmiLsComponent<S> {
    binding: ArtifactBuildBinding<S>,
    bytes: Vec<u8>,
    core: FixedNativeGuestCore,
}

impl<S> LinkedScalarFmiLsComponent<S> {
    pub fn binding(&self) -> &ArtifactBuildBinding<S> {
        &self.binding
    }
    pub fn bytes(&self) -> &[u8] {
        &self.bytes
    }
    pub fn fixed_core(&self) -> &FixedNativeGuestCore {
        &self.core
    }

    /// Consume after packaging has checked the exact original binding.
    pub fn into_bytes(self) -> Vec<u8> {
        self.bytes
    }
}

impl PreparedScalarFmiLsModel {
    /// Pair the scalar core with this component's own source data. The core
    /// must have been built for the scalar product; the scalar receiver then
    /// refuses a component of the other kind at instantiation.
    pub fn link_fixed_guest<S>(
        self,
        core: FixedNativeGuestCore,
        request: &PreparedBuildRequest<'_, S>,
    ) -> Result<LinkedScalarFmiLsComponent<S>, NativeFmiLsPreparationError> {
        if !core.is_scalar_product() {
            return Err(NativeFmiLsPreparationError::Linkage(
                "a scalar component links only the scalar-product fixed core".into(),
            ));
        }
        let token = publication::token(request).map_err(NativeFmiLsPreparationError::Linkage)?;
        // The scalar receiver decodes and ignores the native capacity policy;
        // the canonical default keeps the envelope format unchanged.
        let envelope = rumoca_exec_wasm::encode_native_source_envelope(
            &self.capsule,
            rumoca_ir_solve::CheckedNativeWasmCapacityPolicy::default(),
        )
        .map_err(NativeFmiLsPreparationError::Linkage)?;
        let data =
            data::library(&envelope, &token).map_err(NativeFmiLsPreparationError::Linkage)?;
        let bytes = wit_component::Linker::default()
            .validate(true)
            .stub_missing_functions(false)
            .use_built_in_libdl(false)
            .library("rumoca-source", &data, false)
            .and_then(|linker| linker.library("rumoca-runtime", core.bytes(), false))
            .and_then(wit_component::Linker::encode)
            .map_err(|error| {
                NativeFmiLsPreparationError::Linkage(format!(
                    "link scalar fixed FMI-LS graph: {error:#}"
                ))
            })?;
        core.check_component_interface(&bytes)
            .map_err(NativeFmiLsPreparationError::Linkage)?;
        Ok(LinkedScalarFmiLsComponent {
            binding: request.binding(),
            bytes,
            core,
        })
    }
}
