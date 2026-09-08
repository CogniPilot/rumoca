mod admission;
mod aggregate;
#[cfg(test)]
mod tests;

use std::collections::BTreeSet;
use std::fs;
use std::path::{Component, Path, PathBuf};

use anyhow::{Context as _, Result, ensure};
use serde::{Deserialize, Serialize};
use sha2::{Digest as _, Sha256};
use time::{OffsetDateTime, format_description::well_known::Rfc3339};

pub use admission::{
    CandidateOnlyRegistryAdmission, CheckedCandidateCapture, ReviewedAdmissionNotImplemented,
    check_candidate_capture, check_candidate_only_registry,
};
pub use aggregate::{
    AggregateDenominatorHistory, CandidateOnlyAggregateReport, candidate_only_aggregate_report,
    checked_aggregate_denominator_history, parse_candidate_only_aggregate_denominator,
};

#[derive(Clone, Debug, Deserialize, Serialize, PartialEq, Eq)]
#[serde(deny_unknown_fields)]
pub struct CoverageTest {
    pub package: String,
    pub test_target: String,
    pub features: Vec<String>,
    pub test_name: String,
}

#[derive(Clone, Deserialize)]
#[serde(deny_unknown_fields)]
pub struct GoldenModelRecord {
    pub id: String,
    pub profile: GoldenProfile,
    pub claim: ModelClaim,
}

#[derive(Clone, Copy, Deserialize)]
#[serde(rename_all = "kebab-case")]
pub enum GoldenProfile {
    UnitDerivativeV1,
    ParameterDecayV1,
}

#[derive(Clone, Deserialize)]
#[serde(
    tag = "kind",
    content = "details",
    rename_all = "kebab-case",
    deny_unknown_fields
)]
pub enum ModelClaim {
    Candidate(CandidateClaim),
    Reviewed(ReviewedClaim),
}

/// A candidate may carry the coverage footprint of its last capture. The
/// footprint file is bound by digest so the record names exact bytes, but its
/// per-file digests are not held against the working tree, because a
/// candidate asserts no currency of its evidence. It contributes no
/// coordinates to the aggregate.
#[derive(Clone, Deserialize)]
#[serde(deny_unknown_fields)]
pub struct CandidateClaim {
    pub reason: String,
    pub last_compliance_check: String,
    pub capture: CapturePlan,
    pub coverage_footprint: Option<ContentBinding>,
}

/// Reserved wire shape for a future reviewed claim. Registry schema 2 checks
/// these common bindings and then returns `ReviewedAdmissionNotImplemented`:
/// coverage execution is not semantic evidence, and no non-self-referential
/// production snapshot checker exists yet.
#[derive(Clone, Deserialize)]
#[serde(deny_unknown_fields)]
pub struct ReviewedClaim {
    pub reason: String,
    pub last_compliance_check: String,
    pub capture: CapturePlan,
    pub coverage_footprint: ContentBinding,
    pub outstanding: Vec<OutstandingReceipt>,
}

#[derive(Clone, Deserialize)]
#[serde(deny_unknown_fields)]
pub struct OutstandingReceipt {
    pub endpoint: Endpoint,
    pub reason: String,
}

#[derive(Clone, Deserialize)]
#[serde(deny_unknown_fields)]
pub struct CapturePlan {
    pub source: ContentBinding,
    pub review_record: ContentBinding,
    pub cone: Vec<CandidateConeNode>,
    pub scenarios: Vec<CaptureScenario>,
}

#[derive(Clone, Deserialize)]
#[serde(deny_unknown_fields)]
pub struct ContentBinding {
    pub path: String,
    pub sha256: String,
}

#[derive(Clone, Deserialize)]
#[serde(deny_unknown_fields)]
pub struct CaptureScenario {
    pub id: String,
    pub test: CoverageTest,
    pub requests: Vec<EndpointRequest>,
}

#[derive(Clone, Deserialize)]
#[serde(deny_unknown_fields)]
pub struct CandidateConeNode {
    pub owner: OwnerId,
    pub predecessors: Vec<OwnerId>,
}

#[derive(Clone, Deserialize)]
#[serde(deny_unknown_fields)]
pub struct EndpointRequest {
    pub endpoint: Endpoint,
    pub disposition: TerminalDisposition,
}

#[derive(Clone, Copy, Debug, Deserialize, PartialEq, Eq, PartialOrd, Ord)]
#[serde(rename_all = "kebab-case")]
pub enum OwnerId {
    CanonicalSource,
    StrictParse,
    ResolvedAst,
    FlatIr,
    DaeIr,
    PreparedDae,
    SolveIr,
    DaeSolveVariableRefinement,
    DaeSolveEquationRefinement,
    RuntimeFmiComponent,
    NativeMeHost,
    NativeTrace,
    Fmi3TargetAdmission,
    Fmi3Component,
    Fmi3PreparedEmission,
    Fmi3ModelDescription,
    Fmi3BuildDescription,
    Fmi3ModelSource,
    Fmi3Archive,
    Fmi3MeLifecycle,
    Fmi3CsLifecycle,
    DirectGalecRefusal,
    RegisteredGalecRefusal,
    RegisteredEfmuRefusal,
}

#[derive(Clone, Copy, Debug, Deserialize, PartialEq, Eq, PartialOrd, Ord)]
#[serde(rename_all = "kebab-case")]
pub enum Endpoint {
    DaeSolveProductionRefinement,
    NativeInterpreterRkTrace,
    Fmi3ModelDescriptionXml,
    Fmi3BuildDescriptionXml,
    Fmi3ModelSourceCc,
    Fmi3ArchiveMembership,
    Fmi3MeDerivativeStateLifecycle,
    Fmi3CsDoStepLifecycle,
    DirectGalecContinuousRefusal,
    RegisteredGalecContinuousRefusal,
    RegisteredEfmuContinuousRefusal,
}

#[derive(Clone, Copy, Debug, Deserialize, PartialEq, Eq)]
#[serde(rename_all = "kebab-case")]
pub enum TerminalDisposition {
    Observed,
    TypedRefusal,
    Blocked,
    Unmet,
}

pub struct GoldenRegistry {
    models: Vec<GoldenModelRecord>,
}

#[derive(Deserialize)]
#[serde(deny_unknown_fields)]
struct RawGoldenRegistry {
    schema_version: u32,
    models: Vec<GoldenModelRecord>,
}

impl GoldenRegistry {
    pub fn models(&self) -> &[GoldenModelRecord] {
        &self.models
    }
}

pub fn parse_golden_registry(source: &str, location: &str) -> Result<GoldenRegistry> {
    let registry: RawGoldenRegistry =
        toml::from_str(source).with_context(|| format!("failed to parse {location}"))?;
    ensure!(
        registry.schema_version == 2,
        "unsupported golden-model schema {}; expected exactly schema 2",
        registry.schema_version
    );
    ensure!(
        !registry.models.is_empty(),
        "golden-model registry is empty"
    );
    validate_unique_model_ids_and_sources(&registry.models)?;
    Ok(GoldenRegistry {
        models: registry.models,
    })
}

fn validate_unique_model_ids_and_sources(models: &[GoldenModelRecord]) -> Result<()> {
    let mut ids = BTreeSet::new();
    let mut sources = BTreeSet::new();
    for model in models {
        ensure!(ids.insert(&model.id), "duplicate model id `{}`", model.id);
        let source = match &model.claim {
            ModelClaim::Candidate(candidate) => &candidate.capture.source.path,
            ModelClaim::Reviewed(reviewed) => &reviewed.capture.source.path,
        };
        ensure!(
            sources.insert(source),
            "canonical source `{source}` is assigned to multiple records"
        );
    }
    Ok(())
}

fn validate_compliance_instant(value: &str) -> Result<()> {
    OffsetDateTime::parse(value, &Rfc3339)
        .with_context(|| format!("compliance-review instant `{value}` is not RFC 3339"))?;
    Ok(())
}

pub fn scenario_identity(test: &CoverageTest) -> Result<String> {
    let bytes = serde_json::to_vec(test)?;
    Ok(format!("scenario-{:x}", Sha256::digest(bytes)))
}

pub fn ensure_safe_id(id: &str) -> Result<()> {
    ensure!(
        !id.is_empty()
            && id
                .bytes()
                .all(|byte| byte.is_ascii_alphanumeric() || matches!(byte, b'-' | b'_')),
        "unsafe golden-model id `{id}`"
    );
    Ok(())
}

fn ensure_safe_atom(kind: &str, value: &str) -> Result<()> {
    ensure!(
        !value.is_empty()
            && value
                .bytes()
                .all(|byte| byte.is_ascii_alphanumeric() || matches!(byte, b'-' | b'_')),
        "unsafe {kind} `{value}`"
    );
    Ok(())
}

pub fn ensure_exact_test_identity(identity: &str) -> Result<()> {
    let segments: Vec<&str> = identity.split("::").collect();
    ensure!(
        !segments.is_empty()
            && segments.iter().all(|segment| {
                !segment.is_empty()
                    && segment
                        .bytes()
                        .all(|byte| byte.is_ascii_alphanumeric() || byte == b'_')
            }),
        "invalid exact libtest identity `{identity}`"
    );
    Ok(())
}

fn checked_repo_path(root: &Path, relative: &str) -> Result<PathBuf> {
    let relative = Path::new(relative);
    ensure!(!relative.is_absolute(), "registry path must be relative");
    ensure!(
        relative
            .components()
            .all(|component| matches!(component, Component::Normal(_))),
        "registry path contains a non-normal component"
    );
    let path = root.join(relative);
    ensure!(
        path.symlink_metadata()
            .is_ok_and(|metadata| metadata.is_file()),
        "registry artifact does not exist: {}",
        path.display()
    );
    let canonical_root = root.canonicalize()?;
    let canonical_path = path.canonicalize()?;
    ensure!(
        canonical_path.starts_with(&canonical_root),
        "registry artifact escapes the repository: {}",
        path.display()
    );
    Ok(path)
}

pub fn sha256_file(path: &Path) -> Result<String> {
    let bytes = fs::read(path).with_context(|| format!("failed to read {}", path.display()))?;
    Ok(format!("{:x}", Sha256::digest(bytes)))
}
