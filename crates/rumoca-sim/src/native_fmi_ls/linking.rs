//! Closed original-model/data/fixed-runtime graph; no selectable providers.

use super::{
    NativeFmiLsPreparationError, PreparedNativeFmiLsModel, PreparedScalarFmiLsModel, data,
    publication,
};
use rumoca_core::artifact_build::{ArtifactBuildBinding, PreparedBuildRequest};
use rumoca_exec_wasm::{
    FixedNativeGuestCore, LINKED_DISPATCH_EXPORT, LinkedEntryId, LinkedProduct,
    NativeLinkableModelLibrary,
};
use std::collections::BTreeMap;

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
/// core, and for the scalar-linked product also the issued linked entry modules
/// and their one dispatch. No model-entry library is linked; the core never
/// imports one.
pub struct LinkedScalarFmiLsComponent<S> {
    binding: ArtifactBuildBinding<S>,
    bytes: Vec<u8>,
    core: FixedNativeGuestCore,
    /// The issued linked entries this component links, in plan order; empty
    /// for the interpreted scalar product.
    linked_entries: Vec<(LinkedEntryId, [u64; 4])>,
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
    /// The issued linked entries linked into this component, in plan order.
    pub fn linked_entries(&self) -> &[(LinkedEntryId, [u64; 4])] {
        &self.linked_entries
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
            linked_entries: Vec::new(),
        })
    }

    /// Pair the scalar-linked core with this component's source data, the
    /// issued linked entry modules of `product` and their one dispatch.
    ///
    /// `plan` is the encoded linked entry plan the guest reads through its
    /// plan slots. Every symbol must have exactly one provider among the
    /// source data, the entry modules, the dispatch and the core; the linker
    /// stubs nothing, and the result must expose exactly the checked component
    /// interface, so the dispatch and plan slots stay internal.
    pub fn link_linked_guest<S>(
        self,
        core: FixedNativeGuestCore,
        product: &LinkedProduct,
        plan: &[u8],
        request: &PreparedBuildRequest<'_, S>,
    ) -> Result<LinkedScalarFmiLsComponent<S>, NativeFmiLsPreparationError> {
        let linkage = NativeFmiLsPreparationError::Linkage;
        if !core.is_scalar_linked_product() {
            return Err(linkage(
                "a scalar-linked component links only the scalar-linked fixed core".into(),
            ));
        }
        let token = publication::token(request).map_err(linkage)?;
        let envelope = rumoca_exec_wasm::encode_native_source_envelope(
            &self.capsule,
            rumoca_ir_solve::CheckedNativeWasmCapacityPolicy::default(),
        )
        .map_err(linkage)?;
        let data = data::linked_library(&envelope, &token, plan).map_err(linkage)?;
        let dispatch = product.plan().dispatch();
        check_single_providers(&core, product).map_err(linkage)?;
        let bytes = link_linked(&data, product, &dispatch, &core).map_err(linkage)?;
        core.check_component_interface(&bytes).map_err(linkage)?;
        Ok(LinkedScalarFmiLsComponent {
            binding: request.binding(),
            bytes,
            core,
            linked_entries: product.plan().entries().collect(),
        })
    }
}

/// The source data, the issued entry modules in their link order (helpers
/// first), the dispatch, then the runtime core, with no stubs and no libdl.
fn link_linked(
    data: &[u8],
    product: &LinkedProduct,
    dispatch: &[u8],
    core: &FixedNativeGuestCore,
) -> Result<Vec<u8>, String> {
    fn error(error: impl std::fmt::Display) -> String {
        format!("link scalar-linked fixed FMI-LS graph: {error:#}")
    }
    let mut linker = wit_component::Linker::default()
        .validate(true)
        .stub_missing_functions(false)
        .use_built_in_libdl(false)
        .library("rumoca-source", data, false)
        .map_err(error)?;
    for (index, module) in product.modules().iter().enumerate() {
        linker = linker
            .library(
                &format!("rumoca-core-{index}"),
                module.module_bytes(),
                false,
            )
            .map_err(error)?;
    }
    linker
        .library("rumoca-dispatch", dispatch, false)
        .and_then(|linker| linker.library("rumoca-runtime", core.bytes(), false))
        .and_then(wit_component::Linker::encode)
        .map_err(error)
}

/// Refuse a graph in which two libraries export the same name, so linker
/// priority can never choose between providers of one symbol.
fn check_single_providers(
    core: &FixedNativeGuestCore,
    product: &LinkedProduct,
) -> Result<(), String> {
    let source = [
        "rumoca_source_capsule_len",
        "rumoca_source_capsule_copy",
        "rumoca_publication_token_len",
        "rumoca_publication_token_copy",
        "rumoca_linked_plan_len",
        "rumoca_linked_plan_copy",
    ]
    .map(String::from);
    let mut providers: Vec<(String, Vec<String>)> = vec![("rumoca-source".into(), source.to_vec())];
    for (index, module) in product.modules().iter().enumerate() {
        providers.push((format!("rumoca-core-{index}"), module.exports().to_vec()));
    }
    providers.push((
        "rumoca-dispatch".into(),
        vec![LINKED_DISPATCH_EXPORT.to_owned()],
    ));
    providers.push((
        "rumoca-runtime".into(),
        core.exports().map(String::from).collect(),
    ));
    let mut seen: BTreeMap<String, String> = BTreeMap::new();
    for (library, exports) in providers {
        for name in exports {
            if let Some(previous) = seen.insert(name.clone(), library.clone()) {
                return Err(format!(
                    "linked symbol {name} is exported by both {previous} and {library}"
                ));
            }
        }
    }
    Ok(())
}
