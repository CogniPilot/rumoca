//! Current-build full-runtime transport over the common checked FMI/ME owner.
mod container;
use super::{InteractiveOptions, configured_options, prebuilt};
use crate::WasmError;
use serde::{Deserialize, Serialize};
use sha2::{Digest, Sha256};
use wasm_bindgen::prelude::*;

#[derive(Serialize, Deserialize, PartialEq, Eq)]
#[serde(deny_unknown_fields)]
struct Key {
    schema: u32,
    model: prebuilt::ArtifactKey,
    options_sha256: String,
}

fn digest(bytes: impl AsRef<[u8]>) -> String {
    format!("{:x}", Sha256::digest(bytes.as_ref()))
}
fn serialize(value: &impl Serialize) -> Result<String, WasmError> {
    serde_json::to_string(value)
        .map_err(|error| WasmError::new(format!("Precompiled runtime serialization: {error}")))
}
fn key(
    source: &str,
    model: &str,
    sources: &str,
    options: &InteractiveOptions,
) -> Result<Key, WasmError> {
    Ok(Key {
        schema: 2,
        model: prebuilt::key(source, model, sources)?,
        options_sha256: digest(serialize(options)?),
    })
}

/// Identity of the exact compiler/model/source set and checked runtime options.
#[wasm_bindgen]
pub fn precompiled_runtime_key(
    source: &str,
    model: &str,
    sources: &str,
    options_json: &str,
) -> Result<String, WasmError> {
    let options = InteractiveOptions::parse(options_json)?;
    serialize(&key(source, model, sources, &options)?)
}

/// Build-time export. This executes the common preparation once and records
/// the original emitted kernels; the page later loads them without emission.
/// These are trusted build outputs, not signatures or an authenticity proof.
#[wasm_bindgen]
pub fn export_precompiled_runtime_artifact(
    source: &str,
    model: &str,
    sources: &str,
    options_json: &str,
) -> Result<Vec<u8>, WasmError> {
    let options = InteractiveOptions::parse(options_json)?;
    let key = key(source, model, sources, &options)?;
    let compiled = prebuilt::compile_payload(source, model, sources)?;
    let (base, _) = compiled
        .experiment
        .simulation_options(0.0, options.dt, &options.solver);
    let opts = configured_options(base, &options, options.initial_inputs.clone());
    let runtime = rumoca_sim::export_precompiled_runtime(&compiled.dae, opts, compiled.experiment)
        .map_err(|error| WasmError::new(format!("Precompiled runtime export: {error}")))?;
    container::encode(serialize(&key)?.as_bytes(), &runtime).map_err(WasmError::new)
}

pub(super) fn load(
    artifact: &[u8],
    source: &str,
    model: &str,
    sources: &str,
    options_json: &str,
) -> Result<rumoca_sim::SimulationSessionLease, WasmError> {
    let options = InteractiveOptions::parse(options_json)?;
    let identity = rumoca_core::browser_profile::Span::new("binary_header_identity_and_hash");
    let (encoded_key, payload) = container::decode(artifact).map_err(WasmError::new)?;
    let artifact_key: Key = serde_json::from_slice(encoded_key)
        .map_err(|error| WasmError::new(format!("Invalid runtime key: {error}")))?;
    if artifact_key != key(source, model, sources, &options)? {
        return Err(WasmError::new(
            "Precompiled runtime source/model/compiler/options mismatch",
        ));
    }
    drop(identity);
    rumoca_sim::load_precompiled_runtime(
        payload,
        |experiment: crate::simulation_api::experiment::SimulationExperiment| {
            let (base, _) = experiment.simulation_options(0.0, options.dt, &options.solver);
            configured_options(base, &options, options.initial_inputs.clone())
        },
    )
    .map_err(|error| WasmError::new(format!("Precompiled runtime load: {error}")))
}
/// Enable/reset diagnostic clocks in an explicitly instrumented build.
#[wasm_bindgen]
pub fn browser_profile_enable(enabled: bool) {
    rumoca_core::browser_profile::enable(enabled);
}
/// Inclusive/exclusive wall clocks; per-call instrumentation adds overhead.
#[wasm_bindgen]
pub fn browser_profile_snapshot() -> String {
    rumoca_core::browser_profile::snapshot()
}
