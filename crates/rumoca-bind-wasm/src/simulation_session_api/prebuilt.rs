//! Current-build DAE transport. Checked IR replay remains owned by DAE.

mod sources;

use std::sync::Arc;

use rumoca_compile::{Session, compile::Dae};
use serde::{Deserialize, Serialize};
use sha2::{Digest, Sha256};
use wasm_bindgen::prelude::*;

use crate::simulation_api::experiment::SimulationExperiment;
use crate::{WasmError, compile_requested_model, qualify_input_model_name};

const ARTIFACT_VERSION: u32 = 1;

#[derive(Debug, PartialEq, Eq, Serialize, Deserialize)]
#[serde(deny_unknown_fields)]
pub(super) struct ArtifactKey {
    version: u32,
    compiler_version: String,
    compiler_identity: String,
    model_name: String,
    source_sha256: String,
    sources_sha256: String,
}

#[derive(Serialize, Deserialize)]
#[serde(deny_unknown_fields)]
struct Envelope {
    key: ArtifactKey,
    payload_sha256: String,
    // Keeping the payload opaque prevents JavaScript from rounding u64 IDs.
    payload_json: String,
}

#[derive(Serialize, Deserialize)]
#[serde(deny_unknown_fields)]
pub(super) struct Payload {
    pub(super) dae: Arc<Dae>,
    pub(super) experiment: SimulationExperiment,
}

fn digest(bytes: impl AsRef<[u8]>) -> String {
    format!("{:x}", Sha256::digest(bytes.as_ref()))
}

fn serialize(value: &impl Serialize) -> Result<String, WasmError> {
    serde_json::to_string(value)
        .map_err(|error| WasmError::new(format!("Prebuilt artifact serialization: {error}")))
}

pub(super) fn key(
    source: &str,
    model_name: &str,
    sources_json: &str,
) -> Result<ArtifactKey, WasmError> {
    let sources = sources::SourceSet::parse(source, sources_json)?;
    let identity = rumoca_tool_build_info::build_identity()
        .filter(|identity| !identity.ends_with("-dirty"))
        .ok_or_else(|| {
            WasmError::new("Prebuilt artifacts require a known clean compiler identity")
        })?;
    Ok(ArtifactKey {
        version: ARTIFACT_VERSION,
        compiler_version: env!("CARGO_PKG_VERSION").into(),
        compiler_identity: identity.into(),
        model_name: model_name.into(),
        source_sha256: digest(source),
        sources_sha256: digest(serialize(&sources.hashes())?),
    })
}

/// Deterministic key for the exact source set, selected class and this compiler.
/// Companion keys are canonical relative paths; `input.mo` is reserved for source.
/// Retain the returned string alongside the opaque artifact at site build time.
#[wasm_bindgen]
pub fn prebuilt_artifact_key(
    source: &str,
    model_name: &str,
    workspace_sources_json: &str,
) -> Result<String, WasmError> {
    serialize(&key(source, model_name, workspace_sources_json)?)
}

/// Compile the explicit complete source set in an isolated session and export
/// a DAE plus experiment defaults. It skips source compilation on later load;
/// Solve lowering, kernel generation and runtime initialization still run.
/// Artifacts are trusted build outputs, not signatures or an authenticity proof.
#[wasm_bindgen]
pub fn export_prebuilt_artifact(
    source: &str,
    model_name: &str,
    workspace_sources_json: &str,
) -> Result<String, WasmError> {
    let key = key(source, model_name, workspace_sources_json)?;
    let payload_json = serialize(&compile_payload(
        source,
        model_name,
        workspace_sources_json,
    )?)?;
    serialize(&Envelope {
        key,
        payload_sha256: digest(&payload_json),
        payload_json,
    })
}

pub(super) fn compile_payload(
    source: &str,
    model_name: &str,
    workspace_sources_json: &str,
) -> Result<Payload, WasmError> {
    let sources = sources::SourceSet::parse(source, workspace_sources_json)?;
    let mut session = Session::default();
    sources.load(&mut session)?;
    let requested = qualify_input_model_name(&session, model_name);
    let compiled = compile_requested_model(&mut session, &requested)?;
    let experiment = SimulationExperiment::from(&compiled);
    Ok(Payload {
        dae: compiled.dae,
        experiment,
    })
}

pub(super) fn load(
    artifact_json: &str,
    source: &str,
    model_name: &str,
    workspace_sources_json: &str,
) -> Result<Payload, WasmError> {
    let artifact: Envelope = serde_json::from_str(artifact_json)
        .map_err(|error| WasmError::new(format!("Invalid prebuilt envelope: {error}")))?;
    if artifact.key.version != ARTIFACT_VERSION {
        return Err(WasmError::new("Unsupported prebuilt artifact version"));
    }
    let expected = key(source, model_name, workspace_sources_json)?;
    if artifact.key != expected {
        return Err(WasmError::new(
            "Prebuilt artifact source/model/compiler key mismatch",
        ));
    }
    if digest(&artifact.payload_json) != artifact.payload_sha256 {
        return Err(WasmError::new("Prebuilt artifact payload hash mismatch"));
    }
    // Dae::Deserialize replays its current wire through Dae::construct.
    serde_json::from_str(&artifact.payload_json)
        .map_err(|error| WasmError::new(format!("Invalid prebuilt model payload: {error}")))
}
