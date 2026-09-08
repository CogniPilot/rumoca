//! Strict current-format sidecars, issued only after successful Rust compilation.

#[cfg(test)]
mod tests;

use std::collections::{BTreeMap, BTreeSet};
use std::fs::File;
use std::io::{BufReader, Write};
use std::path::{Path, PathBuf};
use std::sync::Arc;

use rustc_hir::def::DefKind;
use rustc_hir::def_id::{CrateNum, DefPathHash, LOCAL_CRATE};
use rustc_middle::ty::TyCtxt;
use rustc_session::config::{CrateType, OutFileName, OutputType};
use rustc_span::{SourceFileHash, SourceFileHashAlgorithm};
use rustc_target::json::ToJson;
use serde::{Deserialize, Serialize};

use crate::collection::{Collection, CollectionError};
use crate::session::TransportError;
use crate::{ClosureRegionFacts, RegionPart, RegionSlot, RequiredOutlives};

pub(crate) const SCHEMA: u32 = 1;

#[derive(Debug, PartialEq, Eq, Serialize, Deserialize)]
#[serde(deny_unknown_fields)]
struct Identity {
    schema: u32,
    compiler: String,
    driver_digest: String,
    target_digest: String,
    stable_crate_id: u64,
    crate_hash: String,
}

impl Identity {
    fn current(tcx: TyCtxt<'_>, krate: CrateNum, driver_digest: &str) -> Self {
        Self {
            schema: SCHEMA,
            compiler: tcx.sess.cfg_version.to_owned(),
            driver_digest: driver_digest.to_owned(),
            target_digest: SourceFileHash::new_in_memory(
                SourceFileHashAlgorithm::Sha256,
                tcx.sess.target.to_json().to_string(),
            )
            .to_string(),
            stable_crate_id: tcx.stable_crate_id(krate).as_u64(),
            crate_hash: tcx.crate_hash(krate).to_string(),
        }
    }
}

#[derive(Serialize, Deserialize)]
#[serde(deny_unknown_fields)]
struct FactData {
    slots: Vec<RegionSlot>,
    required_outlives: Vec<RequiredOutlives>,
}

impl FactData {
    fn validate(&self) -> Result<(), TransportError> {
        let mut expected = Vec::new();
        for part in [
            RegionPart::ParentArguments,
            RegionPart::Signature,
            RegionPart::Captures,
        ] {
            let count = self.slots.iter().filter(|slot| slot.part() == part).count();
            expected.extend((0..count).map(|occurrence| RegionSlot { part, occurrence }));
        }
        if self.slots != expected {
            return Err(TransportError::InvalidArtifact("noncanonical region slots"));
        }
        let slots: BTreeSet<_> = self.slots.iter().copied().collect();
        let mut seen = BTreeSet::new();
        for relation in &self.required_outlives {
            if !slots.contains(&relation.longer())
                || !slots.contains(&relation.shorter())
                || !seen.insert(*relation)
            {
                return Err(TransportError::InvalidArtifact("invalid required relation"));
            }
        }
        Ok(())
    }
}

#[derive(Serialize, Deserialize)]
#[serde(deny_unknown_fields)]
struct Record {
    owner: u64,
    facts: Result<FactData, CollectionError>,
}

#[derive(Serialize, Deserialize)]
#[serde(deny_unknown_fields)]
struct Artifact {
    identity: Identity,
    artifact_digest: String,
    records: Vec<Record>,
}

pub(crate) fn digest_file(path: &Path) -> Result<String, TransportError> {
    Ok(SourceFileHash::new(SourceFileHashAlgorithm::Sha256, File::open(path)?)?.to_string())
}

#[cfg(target_os = "linux")]
pub(crate) fn executing_driver_digest() -> Result<String, TransportError> {
    // Open the executing inode, not a pathname a concurrent build can replace.
    digest_file(Path::new("/proc/self/exe"))
}

#[cfg(not(target_os = "linux"))]
pub(crate) fn executing_driver_digest() -> Result<String, TransportError> {
    Err(TransportError::InvalidArtifact(
        "executing-image identity is not implemented on this platform",
    ))
}

fn sidecar_path(artifact: &Path) -> PathBuf {
    let mut path = artifact.as_os_str().to_owned();
    path.push(".charon-closure-regions.json");
    path.into()
}

fn emitted_artifacts(tcx: TyCtxt<'_>) -> Result<Vec<PathBuf>, TransportError> {
    let outputs = tcx.output_filenames(());
    let mut paths = Vec::new();
    if outputs.outputs.contains_key(&OutputType::Metadata) {
        paths.push(rustc_session::output::filename_for_metadata(
            tcx.sess, outputs,
        ));
    }
    if outputs.outputs.contains_key(&OutputType::Exe) && !tcx.sess.opts.unstable_opts.no_codegen {
        for &kind in tcx.crate_types() {
            if matches!(kind, CrateType::Rlib | CrateType::Dylib) {
                paths.push(rustc_session::output::out_filename(
                    tcx.sess,
                    kind,
                    outputs,
                    tcx.crate_name(LOCAL_CRATE),
                ));
            }
        }
    }
    paths
        .into_iter()
        .map(|path| match path {
            OutFileName::Real(path) => Ok(path),
            OutFileName::Stdout => Err(TransportError::InvalidArtifact(
                "metadata emitted to stdout",
            )),
        })
        .collect()
}

/// This type is private to the session implementation; analysis alone cannot
/// give an external callback authority to publish a successful-compilation file.
pub(crate) struct PendingArtifact {
    identity: Identity,
    records: Vec<Record>,
    paths: Vec<PathBuf>,
}

impl PendingArtifact {
    pub(crate) fn capture(
        tcx: TyCtxt<'_>,
        collection: &Collection,
        driver_digest: &str,
    ) -> Result<Self, TransportError> {
        if tcx.dcx().has_errors().is_some() {
            return Err(TransportError::RustcRejected);
        }
        let mut records = Vec::new();
        for owner in tcx.hir_body_owners() {
            if tcx.def_kind(owner) != DefKind::Closure {
                continue;
            }
            records.push(Record {
                owner: tcx.def_path_hash(owner.to_def_id()).local_hash().as_u64(),
                facts: collection.local(tcx, owner).map(|facts| FactData {
                    slots: facts.slots().to_vec(),
                    required_outlives: facts.required_outlives().to_vec(),
                }),
            });
        }
        records.sort_by_key(|record| record.owner);
        Ok(Self {
            identity: Identity::current(tcx, LOCAL_CRATE, driver_digest),
            records,
            paths: emitted_artifacts(tcx)?,
        })
    }

    pub(crate) fn publish(self) -> Result<(), TransportError> {
        for path in self.paths {
            let sidecar = sidecar_path(&path);
            let parent = sidecar
                .parent()
                .ok_or(TransportError::InvalidArtifact("no output parent"))?;
            let mut temporary = tempfile::NamedTempFile::new_in(parent)?;
            // Hash finalized bytes, not the pre-codegen file seen by a callback.
            let artifact = ArtifactRef {
                identity: &self.identity,
                artifact_digest: digest_file(&path)?,
                records: &self.records,
            };
            serde_json::to_writer(&mut temporary, &artifact)?;
            temporary.flush()?;
            temporary
                .persist(sidecar)
                .map_err(|error| TransportError::Io(error.error))?;
        }
        Ok(())
    }
}

#[derive(Serialize)]
struct ArtifactRef<'a> {
    identity: &'a Identity,
    artifact_digest: String,
    records: &'a [Record],
}

pub(crate) struct LoadedArtifact {
    records: BTreeMap<u64, Result<Arc<ClosureRegionFacts>, CollectionError>>,
}

impl LoadedArtifact {
    pub(crate) fn load(
        tcx: TyCtxt<'_>,
        krate: CrateNum,
        driver_digest: &str,
    ) -> Result<Self, TransportError> {
        let source = tcx.used_crate_source(krate);
        let path = source
            .rmeta
            .as_ref()
            .or(source.rlib.as_ref())
            .or(source.dylib.as_ref())
            .ok_or(TransportError::InvalidArtifact(
                "no resolved dependency artifact",
            ))?;
        let sidecar = File::open(sidecar_path(path)).map_err(|error| {
            if error.kind() == std::io::ErrorKind::NotFound {
                TransportError::DependencyNotBuiltByCollector(path.clone())
            } else {
                TransportError::Io(error)
            }
        })?;
        let artifact: Artifact = serde_json::from_reader(BufReader::new(sidecar))?;
        if artifact.identity != Identity::current(tcx, krate, driver_digest)
            || artifact.artifact_digest != digest_file(path)?
        {
            return Err(TransportError::InvalidArtifact(
                "stale or mismatched dependency facts",
            ));
        }
        let mut records = BTreeMap::new();
        for record in artifact.records {
            let facts = match record.facts {
                Ok(facts) => {
                    facts.validate()?;
                    let owner = DefPathHash::new(
                        tcx.stable_crate_id(krate),
                        rustc_hashes::Hash64::new(record.owner),
                    );
                    Ok(Arc::new(ClosureRegionFacts {
                        owner,
                        slots: facts.slots,
                        required_outlives: facts.required_outlives,
                    }))
                }
                Err(error) => Err(error),
            };
            if records.insert(record.owner, facts).is_some() {
                return Err(TransportError::InvalidArtifact(
                    "duplicate declaration record",
                ));
            }
        }
        Ok(Self { records })
    }

    pub(crate) fn get(
        &self,
        owner: DefPathHash,
    ) -> Result<Arc<ClosureRegionFacts>, TransportError> {
        Ok(self
            .records
            .get(&owner.local_hash().as_u64())
            .ok_or(TransportError::MissingDeclaration)?
            .clone()?)
    }
}
