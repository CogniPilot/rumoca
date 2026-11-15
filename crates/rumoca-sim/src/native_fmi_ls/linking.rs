//! Closed original-model/data/fixed-runtime graph; no selectable providers.

use super::{
    NativeFmiLsPreparationError, PreparedNativeFmiLsModel, PreparedScalarFmiLsModel, data,
    publication,
};
use rumoca_core::artifact_build::{ArtifactBuildBinding, PreparedBuildRequest};
use rumoca_exec_wasm::{
    FixedNativeGuestCore, LinkedArtifact, LinkedKernelModule, NativeLinkableModelLibrary,
    check_embedded_core_modules, link_single_provider_graph,
};
use serde::Deserialize;

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
        if !core.is_native_product() {
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
/// core, and for the scalar-linked product also the issued linked modules and
/// their one dispatch. No model-entry library is linked; the core never
/// imports one.
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
    /// Refuse, before any guest is built, unless the component `saved`
    /// carries is this component: the same check `link_linked_guest` makes
    /// at linkage, so a mismatch costs no fixed-core build.
    pub fn check_saved_product(&self, saved: &[u8]) -> Result<(), NativeFmiLsPreparationError> {
        check_saved_component(saved, &self.capsule).map_err(NativeFmiLsPreparationError::Linkage)
    }

    /// The comparison `check_saved_product` makes once the saved product's
    /// component is decoded: `saved` must be this component in the canonical
    /// wire, and a refusal names the first difference.
    pub fn check_component(
        &self,
        saved: &rumoca_ir_solve::fmi::FmiComponent,
    ) -> Result<(), NativeFmiLsPreparationError> {
        check_component(saved, &self.capsule).map_err(NativeFmiLsPreparationError::Linkage)
    }

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

    /// Pair the scalar-linked core with this component's source data and the
    /// linked product one SDK export issued: `artifact` and the `saved`
    /// product it is bound to. The linked modules and the dispatch are read
    /// only from the decoded artifact, so the linked bodies are exactly the
    /// ones the guest certifies; the data library serves both byte strings
    /// to it. Every symbol must have exactly one provider among the source
    /// data, the linked modules, the dispatch and the core; the linker stubs
    /// nothing, and the result must expose exactly the checked component
    /// interface, so the dispatch and data slots stay internal.
    pub fn link_linked_guest<S>(
        self,
        core: FixedNativeGuestCore,
        artifact: &[u8],
        saved: &[u8],
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
        let linked = decode_artifact(artifact).map_err(linkage)?;
        if !linked.is_bound_to(saved) {
            return Err(linkage(
                "the linked artifact is bound to another saved product".into(),
            ));
        }
        check_saved_component(saved, &self.capsule).map_err(linkage)?;
        let data = data::linked_library(&envelope, &token, saved, artifact).map_err(linkage)?;
        let bodies: Vec<&[u8]> = linked
            .modules()
            .iter()
            .map(LinkedKernelModule::module_bytes)
            .chain([linked.dispatch_module()])
            .collect();
        let bytes = link_linked(&data, &bodies, core.bytes()).map_err(linkage)?;
        check_embedded_core_modules(&bytes, &bodies).map_err(linkage)?;
        core.check_component_interface(&bytes).map_err(linkage)?;
        Ok(LinkedScalarFmiLsComponent {
            binding: request.binding(),
            bytes,
            core,
        })
    }
}

/// The saved product wire version this packager reads.
const SAVED_PRODUCT_VERSION: u32 = 3;

/// Refuse unless the component the saved product carries, which the linked
/// guest replays and runs, is the component this FMU packages, from which its
/// capsule and model description come. Both pass through the same checked
/// replay and are compared in the capsule's canonical wire.
fn check_saved_component(saved: &[u8], capsule: &[u8]) -> Result<(), String> {
    #[derive(Deserialize)]
    struct SavedProduct {
        version: u32,
        #[serde(deserialize_with = "rumoca_eval_solve::fmi_wire::deserialize_fmi_component")]
        component: rumoca_ir_solve::fmi::FmiComponent,
    }
    let mut reader = rmp_serde::Deserializer::new(std::io::Cursor::new(saved));
    let product = SavedProduct::deserialize(&mut reader)
        .map_err(|error| format!("decode saved product: {error}"))?;
    if reader.get_ref().position() != saved.len() as u64 {
        return Err("trailing saved product bytes".into());
    }
    if product.version != SAVED_PRODUCT_VERSION {
        return Err(format!(
            "saved product wire version {} is not {SAVED_PRODUCT_VERSION}",
            product.version
        ));
    }
    check_component(&product.component, capsule)
}

/// Refuse unless `saved`, the component a saved product carries, is the
/// capsule's component, compared in the canonical wire; a refusal names the
/// first difference.
fn check_component(
    saved: &rumoca_ir_solve::fmi::FmiComponent,
    capsule: &[u8],
) -> Result<(), String> {
    // Read as the guest receiver replays the capsule: no recursion limit,
    // nothing after the component.
    let mut capsule_reader = serde_json::Deserializer::from_slice(capsule);
    capsule_reader.disable_recursion_limit();
    let packaged = rumoca_eval_solve::fmi_wire::deserialize_fmi_component(&mut capsule_reader)
        .map_err(|error| format!("decode packaged capsule: {error}"))?;
    capsule_reader
        .end()
        .map_err(|error| format!("decode packaged capsule: {error}"))?;
    let saved_wire = canonical_wire(saved)?;
    let packaged_wire = canonical_wire(&packaged)?;
    if saved_wire != packaged_wire {
        return Err(format!(
            "the saved product's component differs from the component this FMU packages; \
             first difference {}",
            rumoca_eval_solve::fmi_wire::first_wire_difference(&saved_wire, &packaged_wire)
        ));
    }
    Ok(())
}

fn canonical_wire(component: &rumoca_ir_solve::fmi::FmiComponent) -> Result<Vec<u8>, String> {
    let root = component.runtime_view().retain_assertion_root();
    let wire = rumoca_eval_solve::fmi_wire::retained_fmi_component_wire(&root)
        .map_err(|error| error.to_string())?;
    serde_json::to_vec(&wire).map_err(|error| error.to_string())
}

/// The artifact on the export's named-map wire, with no trailing bytes.
fn decode_artifact(bytes: &[u8]) -> Result<LinkedArtifact, String> {
    let mut reader = rmp_serde::Deserializer::new(std::io::Cursor::new(bytes));
    let artifact = LinkedArtifact::deserialize(&mut reader)
        .map_err(|error| format!("decode linked artifact: {error}"))?;
    if reader.get_ref().position() != bytes.len() as u64 {
        return Err("trailing linked artifact bytes".into());
    }
    Ok(artifact)
}

/// The source data, the linked modules in their link order (helpers first),
/// the dispatch last among `bodies`, then the runtime core: validated, with
/// no stubs, no libdl and one actual provider per exported name.
fn link_linked(data: &[u8], bodies: &[&[u8]], runtime: &[u8]) -> Result<Vec<u8>, String> {
    let (dispatch, modules) = bodies
        .split_last()
        .ok_or("a scalar-linked graph has a dispatch")?;
    let names: Vec<String> = (0..modules.len())
        .map(|index| format!("rumoca-core-{index}"))
        .collect();
    let libraries: Vec<(&str, &[u8])> = [("rumoca-source", data)]
        .into_iter()
        .chain(
            names
                .iter()
                .map(String::as_str)
                .zip(modules.iter().copied()),
        )
        .chain([("rumoca-dispatch", *dispatch), ("rumoca-runtime", runtime)])
        .collect();
    link_single_provider_graph(&libraries)
        .map_err(|error| format!("link scalar-linked fixed FMI-LS graph: {error}"))
}
