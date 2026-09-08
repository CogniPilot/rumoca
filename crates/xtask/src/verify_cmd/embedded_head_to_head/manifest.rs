//! Strict checked input for the embedded head-to-head gate.

use anyhow::{Context, Result, ensure};
use serde::{Deserialize, Serialize};
use sha2::{Digest, Sha256};
use std::collections::BTreeSet;
use std::fs;
use std::io::Read;
use std::path::{Component, Path, PathBuf};
use std::process::Output;

use super::typed_path::{AuthenticatedExecutable, GitExecutable};

/// Require a current-manifest optional key while accepting its explicit `null`
/// representation as `None`.
fn required_option<'de, D, T>(deserializer: D) -> Result<Option<T>, D::Error>
where
    D: serde::Deserializer<'de>,
    T: Deserialize<'de>,
{
    Option::<T>::deserialize(deserializer)
}

pub(super) const SCHEMA_VERSION: u32 = 9;
pub(super) const MANIFEST_PATH: &str = "infra/verification/embedded-head-to-head.json";
#[cfg(test)]
const EFMI_ROOT_MEMBER: &str = "__content.xml";
#[cfg(test)]
const EFMI_ALGORITHM_MANIFEST_MEMBER: &str = "AlgorithmCode/manifest.xml";
#[cfg(test)]
const EFMI_ALGORITHM_SOURCE_MEMBER: &str = "AlgorithmCode/model.alg";
#[cfg(test)]
const EFMI_PRODUCTION_MANIFEST_MEMBER: &str = "ProductionCode/manifest.xml";
#[cfg(test)]
const EFMI_PRODUCTION_C_MEMBER: &str = "ProductionCode/sources/production.c";
#[cfg(test)]
const EFMI_PRODUCTION_H_MEMBER: &str = "ProductionCode/sources/production.h";
const REQUIRED_CASE_IDS: [&str; 4] = [
    "current-closed-closed",
    "tiny-all-series",
    "middle-coefficient-series",
    "different-dense-coupling",
];
const ECM_003_ID: &str = "exp-mixed-efmu-production-c-casadi-3-7-2-cortex-m7";

#[derive(Clone, Debug, Deserialize, Serialize, PartialEq, Eq)]
#[serde(deny_unknown_fields)]
pub(super) struct Manifest {
    pub(super) schema_version: u32,
    pub(super) ratchet_policy: String,
    pub(super) suite_implementation_history: SuiteImplementationHistory,
    pub(super) measured_tools: MeasuredToolPins,
    pub(super) comparator_outputs: ComparatorOutputPins,
    pub(super) entries: Vec<Entry>,
}

/// Append-only audit history for exact trusted-runner byte closures. This is
/// deliberately separate from immutable benchmark protocol and row identity:
/// a replacement can update the implementation that executes an unchanged
/// protocol, but cannot authorize any benchmark-semantic migration.
#[derive(Clone, Debug, Deserialize, Serialize, PartialEq, Eq)]
#[serde(deny_unknown_fields)]
pub(super) struct SuiteImplementationHistory {
    initial_sha256: String,
    replacements: Vec<SuiteImplementationReplacement>,
}

#[derive(Clone, Debug, Deserialize, Serialize, PartialEq, Eq)]
#[serde(deny_unknown_fields)]
struct SuiteImplementationReplacement {
    implementation_sha256: String,
    rationale: String,
}

impl SuiteImplementationHistory {
    #[cfg(test)]
    pub(super) fn initial(implementation_sha256: String) -> Self {
        Self {
            initial_sha256: implementation_sha256,
            replacements: Vec::new(),
        }
    }

    pub(super) fn active_sha256(&self) -> &str {
        self.replacements
            .last()
            .map_or(&self.initial_sha256, |replacement| {
                &replacement.implementation_sha256
            })
    }

    pub(super) fn digest(&self) -> Result<String> {
        Ok(format!("{:x}", Sha256::digest(serde_json::to_vec(self)?)))
    }

    fn appended_after<'a>(
        &'a self,
        prior: &SuiteImplementationHistory,
    ) -> Result<&'a [SuiteImplementationReplacement]> {
        ensure!(
            self.initial_sha256 == prior.initial_sha256
                && self.replacements.starts_with(&prior.replacements),
            "trusted suite implementation history was rewritten instead of appended"
        );
        Ok(&self.replacements[prior.replacements.len()..])
    }
}

#[derive(Clone, Debug, Deserialize, Serialize, PartialEq, Eq)]
#[serde(deny_unknown_fields)]
pub(super) struct MeasuredToolPins {
    pub(super) arm_gcc_sha256: String,
    pub(super) arm_nm_sha256: String,
    pub(super) qemu_sha256: String,
    pub(super) python_sha256: String,
    pub(super) prlimit_sha256: String,
    pub(super) prlimit_version: String,
    pub(super) git_sha256: String,
    pub(super) git_version: String,
    pub(super) cargo_sha256: String,
    pub(super) cargo_version: String,
    pub(super) rustc_sha256: String,
    pub(super) rustc_version: String,
    pub(super) compiler_dependencies_sha256: String,
    pub(super) execution_tool_closure_sha256: String,
    /// Reserved for the linked-text metric. Absence is mandatory until that
    /// metric is introduced, so a second tool cannot enter execution silently.
    #[serde(deserialize_with = "required_option")]
    pub(super) arm_size_sha256: Option<String>,
}

#[derive(Clone, Debug, Deserialize, Serialize, PartialEq, Eq)]
#[serde(deny_unknown_fields)]
pub(super) struct ComparatorOutputPins {
    pub(super) casadi_c_sha256: String,
    pub(super) casadi_h_sha256: String,
}

#[derive(Clone, Debug, Deserialize, Serialize, PartialEq, Eq)]
#[serde(deny_unknown_fields)]
pub(super) struct Entry {
    pub(super) id: String,
    pub(super) fixture: String,
    pub(super) fixture_sha256: String,
    pub(super) model: String,
    pub(super) target: Target,
    pub(super) rumoca_profile: RumocaProfile,
    pub(super) comparator: Comparator,
    pub(super) comparator_generator: String,
    pub(super) comparator_generator_sha256: String,
    pub(super) comparator_wrapper: String,
    pub(super) comparator_wrapper_sha256: String,
    pub(super) cross_profile: CrossProfile,
    pub(super) normalized_profile_sha256: String,
    pub(super) input_header: String,
    pub(super) input_sha256: String,
    pub(super) harness_sha256: String,
    pub(super) correctness_cases: Vec<CorrectnessCase>,
    pub(super) metric: Metric,
    pub(super) evidence_state: RowEvidenceState,
    #[serde(deserialize_with = "required_option")]
    pub(super) max_delta: Option<i64>,
    pub(super) match_or_beat_target_delta: i64,
    pub(super) measured: BaselineMeasurement,
    pub(super) why: String,
    pub(super) rumoca_artifact_history: Vec<RumocaArtifactRevision>,
}

/// One immutable, authenticated eFMU Production-C artifact and its exact
/// metric. History order is the only predecessor relation: no editable record
/// may nominate the revision that supposedly authorized it.
#[derive(Clone, Debug, Deserialize, Serialize, PartialEq, Eq)]
#[serde(deny_unknown_fields)]
pub(super) struct RumocaArtifactRevision {
    pub(super) rumoca_metric: u64,
    pub(super) rationale: String,
    pub(super) artifact_session: ArtifactSessionInputs,
    pub(super) efmu_package_sha256: String,
    pub(super) archive_members: Vec<EfmuArchiveMember>,
    pub(super) production_c: EfmuProductionMember,
    pub(super) production_h: EfmuProductionMember,
    pub(super) checksum_web_membership: Vec<ChecksumWebEdge>,
    pub(super) oracle_success_receipt: OracleSuccessReceipt,
}

#[derive(Clone, Debug, Deserialize, Serialize, PartialEq, Eq)]
#[serde(deny_unknown_fields)]
pub(super) struct EfmuArchiveMember {
    pub(super) archive_path: String,
    pub(super) sha256: String,
}

#[derive(Clone, Debug, Deserialize, Serialize, PartialEq, Eq)]
#[serde(deny_unknown_fields)]
pub(super) struct ArtifactSessionInputs {
    pub(super) generation_instant: String,
    pub(super) identity_seed: String,
}

#[derive(Clone, Debug, Deserialize, Serialize, PartialEq, Eq)]
#[serde(deny_unknown_fields)]
pub(super) struct EfmuProductionMember {
    pub(super) archive_path: String,
    pub(super) sha256: String,
}

#[derive(Clone, Debug, Deserialize, Serialize, PartialEq, Eq)]
#[serde(deny_unknown_fields)]
pub(super) struct ChecksumWebEdge {
    pub(super) producer_archive_path: String,
    pub(super) producer_sha1: String,
    pub(super) consumer_archive_path: String,
}

#[derive(Clone, Debug, Deserialize, Serialize, PartialEq, Eq)]
#[serde(deny_unknown_fields)]
pub(super) struct OracleSuccessReceipt {
    pub(super) receipt_sha256: String,
    pub(super) efmu_package_sha256: String,
    pub(super) correctness_cases_sha256: String,
}

/// Schema 9 can represent only the pending replacement row. Claim eligibility
/// and an accepted baseline remain unrepresentable until the whole-scope eFMU
/// artifact and raw-byte receipt chain are complete.
#[derive(Clone, Copy, Debug, Deserialize, Serialize, PartialEq, Eq)]
#[serde(tag = "state", rename_all = "kebab-case", deny_unknown_fields)]
pub(super) enum RowEvidenceState {
    PendingWholeScopeEvidence,
    #[cfg(test)]
    TestExactPin,
}

impl RowEvidenceState {
    pub(super) const fn label(&self) -> &'static str {
        match self {
            Self::PendingWholeScopeEvidence => "pending-whole-scope-evidence",
            #[cfg(test)]
            Self::TestExactPin => "test-exact-pin",
        }
    }

    pub(super) const fn is_pending(self) -> bool {
        matches!(self, Self::PendingWholeScopeEvidence)
    }
}

pub(super) struct GenerationBindings {
    pub(super) model: ModelArg,
    pub(super) target: TargetArg,
    pub(super) comparator_version: ComparatorVersionArg,
}

macro_rules! generation_args {
    ($($name:ident),+ $(,)?) => {
        $(
            pub(super) struct $name(String);
            impl $name {
                pub(super) fn into_string(self) -> String {
                    self.0
                }
            }
        )+
    };
}

generation_args! {
    ModelArg,
    TargetArg,
    ComparatorVersionArg,
}

impl Entry {
    pub(super) fn generation_bindings(&self) -> Result<GenerationBindings> {
        Ok(GenerationBindings {
            model: ModelArg(self.model.clone()),
            target: TargetArg(self.target.as_str().to_string()),
            comparator_version: ComparatorVersionArg(
                self.comparator.expected_version().to_string(),
            ),
        })
    }
}

/// The sole current product the harness can generate or authenticate.
#[derive(Clone, Copy, Debug, Deserialize, Serialize, PartialEq, Eq)]
#[serde(rename_all = "kebab-case")]
pub(super) enum Target {
    Efmu,
}

impl Target {
    pub(super) const fn as_str(self) -> &'static str {
        match self {
            Self::Efmu => "efmu",
        }
    }
}

#[derive(Clone, Copy, Debug, Deserialize, Serialize, PartialEq, Eq)]
#[serde(rename_all = "kebab-case")]
pub(super) enum RumocaProfile {
    EfmuFirstProductRetainCalls,
}

#[derive(Clone, Copy, Debug, Deserialize, Serialize, PartialEq, Eq)]
#[serde(rename_all = "kebab-case")]
pub(super) enum Comparator {
    Casadi372FullF32,
}

impl Comparator {
    pub(super) const fn expected_version(self) -> &'static str {
        match self {
            Self::Casadi372FullF32 => "3.7.2",
        }
    }
}

#[derive(Clone, Copy, Debug, Deserialize, Serialize, PartialEq, Eq)]
#[serde(rename_all = "kebab-case")]
pub(super) enum CrossProfile {
    Gcc15CortexM7O3LtoNoFpContractHotAuto700Single700ExactV3,
    #[cfg(test)]
    TestAlternateNoFpContract,
}

#[derive(Clone, Copy, Debug, Deserialize, Serialize, PartialEq, Eq)]
#[serde(rename_all = "kebab-case")]
pub(super) enum Metric {
    GuestInstructions,
    #[cfg(test)]
    TestAlternate,
}

#[derive(Clone, Debug, Deserialize, Serialize, PartialEq, Eq)]
#[serde(deny_unknown_fields)]
pub(super) struct Measured {
    pub(super) rumoca: u64,
    pub(super) comparator: u64,
    pub(super) comment: String,
}

#[derive(Clone, Debug, Deserialize, Serialize, PartialEq, Eq)]
#[serde(untagged)]
pub(super) enum BaselineMeasurement {
    Accepted(Measured),
    Pending(()),
}

impl BaselineMeasurement {
    pub(super) const fn accepted(&self) -> Option<&Measured> {
        match self {
            Self::Accepted(measured) => Some(measured),
            Self::Pending(()) => None,
        }
    }
}

#[derive(Clone, Debug, Deserialize, Serialize, PartialEq, Eq)]
#[serde(deny_unknown_fields)]
pub(super) struct CorrectnessCase {
    pub(super) id: String,
    pub(super) expected_output_bits: Vec<String>,
}

pub(super) fn path(root: &Path) -> PathBuf {
    root.join(MANIFEST_PATH)
}

#[cfg(test)]
pub(super) fn load(path: &Path) -> Result<Manifest> {
    load_authenticated(path).map(CheckedCurrentManifest::into_manifest)
}

#[derive(Debug)]
pub(super) struct CheckedCurrentManifest {
    manifest: Manifest,
    sha256: String,
}

impl CheckedCurrentManifest {
    pub(super) const fn manifest(&self) -> &Manifest {
        &self.manifest
    }

    pub(super) fn sha256(&self) -> &str {
        &self.sha256
    }

    #[cfg(test)]
    pub(super) fn for_test(manifest: Manifest) -> Self {
        validate(&manifest).expect("test current manifest must validate");
        let bytes = serde_json::to_vec_pretty(&manifest).expect("test manifest must serialize");
        Self {
            manifest,
            sha256: format!("{:x}", Sha256::digest(bytes)),
        }
    }

    #[cfg(test)]
    fn into_manifest(self) -> Manifest {
        self.manifest
    }
}

pub(super) fn load_authenticated(path: &Path) -> Result<CheckedCurrentManifest> {
    let bytes = fs::read(path).with_context(|| format!("failed to read {}", path.display()))?;
    let sha256 = format!("{:x}", Sha256::digest(&bytes));
    let text =
        std::str::from_utf8(&bytes).with_context(|| format!("{} is not UTF-8", path.display()))?;
    Ok(CheckedCurrentManifest {
        manifest: parse(text, &path.display().to_string())?,
        sha256,
    })
}

/// Load the reviewed manifest from an authenticated Git commit.
///
/// Bootstrap requires proof that no strict ancestor of `HEAD` ever carried
/// the manifest. The only permitted path-history entry is `HEAD` itself,
/// which is the committed first-introduction case. Invalid, shallow, or
/// malformed history fails closed.
#[derive(Clone, Debug, Serialize)]
#[serde(tag = "kind", rename_all = "kebab-case")]
pub(super) enum HistoryEvidence {
    Bootstrap {
        predecessor_commit: String,
        head_commit: String,
    },
    AuthenticatedPredecessor {
        predecessor_commit: String,
        head_commit: String,
        predecessor_manifest_sha256: String,
    },
    AuthenticatedPredecessorWithRunnerReplacement {
        predecessor_commit: String,
        head_commit: String,
        predecessor_manifest_sha256: String,
        previous_implementation_sha256: String,
        current_implementation_sha256: String,
    },
}

impl HistoryEvidence {
    pub(super) const fn is_bootstrap(&self) -> bool {
        matches!(self, Self::Bootstrap { .. })
    }

    pub(super) fn predecessor_commit(&self) -> &str {
        match self {
            Self::Bootstrap {
                predecessor_commit, ..
            }
            | Self::AuthenticatedPredecessor {
                predecessor_commit, ..
            }
            | Self::AuthenticatedPredecessorWithRunnerReplacement {
                predecessor_commit, ..
            } => predecessor_commit,
        }
    }

    #[cfg(test)]
    pub(super) fn predecessor_manifest_sha256(&self) -> Option<&str> {
        match self {
            Self::Bootstrap { .. } => None,
            Self::AuthenticatedPredecessor {
                predecessor_manifest_sha256,
                ..
            }
            | Self::AuthenticatedPredecessorWithRunnerReplacement {
                predecessor_manifest_sha256,
                ..
            } => Some(predecessor_manifest_sha256),
        }
    }
}

/// Construction proof that the checked manifest was compared with an exact
/// authenticated ancestor of the compiler source revision.
pub(super) struct AuthenticatedHistory {
    evidence: HistoryEvidence,
    commands: Vec<super::process::CommandReceipt>,
}

impl AuthenticatedHistory {
    pub(super) fn evidence(&self) -> &HistoryEvidence {
        &self.evidence
    }

    pub(super) fn commands(&self) -> &[super::process::CommandReceipt] {
        &self.commands
    }
}

#[derive(Debug)]
pub(super) struct HistoricalManifest {
    manifest: Manifest,
}

pub(super) fn authenticate_git_history(
    current: &CheckedCurrentManifest,
    root: &Path,
    git: &AuthenticatedExecutable<GitExecutable>,
    revision: &str,
) -> Result<AuthenticatedHistory> {
    let git_directory = resolve_git_directory(root)?;
    let mut commands = Vec::new();
    match authenticate_git_history_inner(
        current,
        root,
        &git_directory,
        git,
        revision,
        &mut commands,
    ) {
        Ok(evidence) => Ok(AuthenticatedHistory { evidence, commands }),
        Err(error) => Err(super::process::attach_prior_receipts(error, commands)),
    }
}

fn authenticate_git_history_inner(
    current: &CheckedCurrentManifest,
    root: &Path,
    git_directory: &Path,
    git: &AuthenticatedExecutable<GitExecutable>,
    revision: &str,
    commands: &mut Vec<super::process::CommandReceipt>,
) -> Result<HistoryEvidence> {
    let predecessor_commit = resolve_commit(
        root,
        git_directory,
        git,
        revision,
        "authenticate embedded head-to-head baseline commit",
        commands,
    )?;
    let head_commit = resolve_commit(
        root,
        git_directory,
        git,
        "HEAD",
        "authenticate compiler HEAD commit",
        commands,
    )?;
    prove_ancestor(
        root,
        git_directory,
        git,
        &predecessor_commit,
        &head_commit,
        commands,
    )?;
    let manifest_history = manifest_history(root, git_directory, git, &head_commit, commands)?;
    if let Some(evidence) = authenticate_bootstrap(
        BootstrapInputs {
            current,
            root,
            git_directory,
            git,
            predecessor_commit: &predecessor_commit,
            head_commit: &head_commit,
            manifest_history: &manifest_history,
        },
        commands,
    )? {
        return Ok(evidence);
    }
    let Some(object) = manifest_blob(root, git_directory, git, &predecessor_commit, commands)?
    else {
        anyhow::bail!(
            "selected predecessor {predecessor_commit} has no embedded head-to-head manifest, but authenticated strict-ancestor history does"
        );
    };
    let output = git_output(
        root,
        git_directory,
        git,
        ["cat-file", "blob", object.as_str()],
        commands,
    )?;
    ensure_git_success(&output, "read embedded head-to-head baseline blob")?;
    let sha256 = format!("{:x}", Sha256::digest(&output.stdout));
    let text = String::from_utf8(output.stdout)
        .context("embedded head-to-head baseline blob is not UTF-8")?;
    let historical = parse_historical(&text, &format!("{predecessor_commit}:{MANIFEST_PATH}"))?;
    let transition = validate_historical_non_relaxation(current, &historical)?;
    match transition.appended_runner_replacements {
        [] => Ok(HistoryEvidence::AuthenticatedPredecessor {
            predecessor_commit,
            head_commit,
            predecessor_manifest_sha256: sha256,
        }),
        [replacement] => Ok(
            HistoryEvidence::AuthenticatedPredecessorWithRunnerReplacement {
                predecessor_commit,
                head_commit,
                predecessor_manifest_sha256: sha256,
                previous_implementation_sha256: historical
                    .manifest
                    .suite_implementation_history
                    .active_sha256()
                    .to_owned(),
                current_implementation_sha256: replacement.implementation_sha256.clone(),
            },
        ),
        _ => anyhow::bail!(
            "one authenticated comparison cannot append multiple suite implementation replacements"
        ),
    }
}

struct BootstrapInputs<'a> {
    current: &'a CheckedCurrentManifest,
    root: &'a Path,
    git_directory: &'a Path,
    git: &'a AuthenticatedExecutable<GitExecutable>,
    predecessor_commit: &'a str,
    head_commit: &'a str,
    manifest_history: &'a [String],
}

fn authenticate_bootstrap(
    inputs: BootstrapInputs<'_>,
    commands: &mut Vec<super::process::CommandReceipt>,
) -> Result<Option<HistoryEvidence>> {
    let BootstrapInputs {
        current,
        root,
        git_directory,
        git,
        predecessor_commit,
        head_commit,
        manifest_history,
    } = inputs;
    let uncommitted_introduction = manifest_history.is_empty();
    let head_introduction = manifest_history == [head_commit];
    if !uncommitted_introduction && !head_introduction {
        return Ok(None);
    }
    ensure!(
        current
            .manifest
            .suite_implementation_history
            .replacements
            .is_empty(),
        "first-introduction bootstrap cannot claim prior suite implementation replacements"
    );
    ensure!(
        current
            .manifest
            .entries
            .iter()
            .all(|entry| entry.rumoca_artifact_history.is_empty()),
        "schema-9 bootstrap requires pending rows to have no artifact revision"
    );
    if head_introduction {
        let object = manifest_blob(root, git_directory, git, head_commit, commands)?
            .context("HEAD manifest history exists without an exact tree blob")?;
        let output = git_output(
            root,
            git_directory,
            git,
            ["cat-file", "blob", object.as_str()],
            commands,
        )?;
        ensure_git_success(&output, "read first-introduction manifest blob")?;
        let head_manifest_sha256 = format!("{:x}", Sha256::digest(&output.stdout));
        ensure!(
            head_manifest_sha256 == current.sha256,
            "HEAD first-introduction manifest bytes differ from the checked current manifest"
        );
    }
    Ok(Some(HistoryEvidence::Bootstrap {
        predecessor_commit: predecessor_commit.to_owned(),
        head_commit: head_commit.to_owned(),
    }))
}

fn manifest_history(
    root: &Path,
    git_directory: &Path,
    git: &AuthenticatedExecutable<GitExecutable>,
    head: &str,
    commands: &mut Vec<super::process::CommandReceipt>,
) -> Result<Vec<String>> {
    let shallow = git_output(
        root,
        git_directory,
        git,
        ["rev-parse", "--is-shallow-repository"],
        commands,
    )?;
    ensure_git_success(&shallow, "authenticate repository history depth")?;
    ensure!(
        shallow.stdout == b"false\n" || shallow.stdout == b"false\r\n",
        "bootstrap and predecessor authentication require complete Git history"
    );

    let output = git_output(
        root,
        git_directory,
        git,
        ["rev-list", "--full-history", head, "--", MANIFEST_PATH],
        commands,
    )?;
    ensure_git_success(
        &output,
        "authenticate embedded head-to-head manifest history",
    )?;
    let text =
        std::str::from_utf8(&output.stdout).context("Git returned non-UTF-8 manifest history")?;
    text.lines()
        .map(|commit| {
            ensure!(
                (commit.len() == 40 || commit.len() == 64)
                    && commit
                        .bytes()
                        .all(|byte| { byte.is_ascii_digit() || (b'a'..=b'f').contains(&byte) }),
                "Git returned malformed manifest-history commit `{commit}`"
            );
            Ok(commit.to_owned())
        })
        .collect()
}

fn prove_ancestor(
    root: &Path,
    git_directory: &Path,
    git: &AuthenticatedExecutable<GitExecutable>,
    predecessor: &str,
    head: &str,
    commands: &mut Vec<super::process::CommandReceipt>,
) -> Result<()> {
    let output = git_output(
        root,
        git_directory,
        git,
        ["merge-base", "--is-ancestor", predecessor, head],
        commands,
    )?;
    ensure!(
        output.status.success(),
        "authenticated commit {predecessor} is not an ancestor of {head}"
    );
    Ok(())
}

pub(super) fn git_control_output<const N: usize>(
    root: &Path,
    git: &AuthenticatedExecutable<GitExecutable>,
    args: [&str; N],
) -> Result<(Output, super::process::CommandReceipt)> {
    let git_directory = resolve_git_directory(root)?;
    let mut commands = Vec::new();
    let output = git_output(root, &git_directory, git, args, &mut commands)?;
    let receipt = commands
        .pop()
        .context("Git command returned no execution receipt")?;
    ensure!(
        commands.is_empty(),
        "one Git command produced multiple receipts"
    );
    Ok((output, receipt))
}

fn parse(text: &str, source: &str) -> Result<Manifest> {
    let manifest: Manifest =
        serde_json::from_str(text).with_context(|| format!("failed to parse {source}"))?;
    validate(&manifest)?;
    Ok(manifest)
}

pub(super) fn parse_historical(text: &str, source: &str) -> Result<HistoricalManifest> {
    let value: serde_json::Value =
        serde_json::from_str(text).with_context(|| format!("failed to parse {source}"))?;
    let schema = value
        .get("schema_version")
        .and_then(serde_json::Value::as_u64)
        .context("historical manifest has no integer schema_version")?;
    let schema = u32::try_from(schema)?;
    ensure!(
        schema == SCHEMA_VERSION,
        "historical embedded head-to-head schema {schema} is unsupported; expected {SCHEMA_VERSION}"
    );
    let manifest: Manifest = serde_json::from_value(value)
        .with_context(|| format!("failed to parse schema-{SCHEMA_VERSION} {source}"))?;
    validate_manifest_wire(&manifest, SuiteBinding::Historical)?;
    Ok(HistoricalManifest { manifest })
}

fn resolve_commit(
    root: &Path,
    git_directory: &Path,
    git: &AuthenticatedExecutable<GitExecutable>,
    revision: &str,
    purpose: &str,
    commands: &mut Vec<super::process::CommandReceipt>,
) -> Result<String> {
    ensure!(
        !revision.trim().is_empty(),
        "baseline Git revision is empty"
    );
    let peeled = format!("{revision}^{{commit}}");
    let output = git_output(
        root,
        git_directory,
        git,
        ["rev-parse", "--verify", "--end-of-options", peeled.as_str()],
        commands,
    )?;
    ensure_git_success(&output, purpose)?;
    let commit = String::from_utf8(output.stdout)
        .context("Git returned a non-UTF-8 baseline commit identity")?;
    let commit = commit.trim_end_matches(['\r', '\n']);
    ensure!(
        (commit.len() == 40 || commit.len() == 64)
            && commit
                .bytes()
                .all(|byte| { byte.is_ascii_digit() || (b'a'..=b'f').contains(&byte) }),
        "Git returned malformed baseline commit identity `{commit}`"
    );
    Ok(commit.to_owned())
}

fn manifest_blob(
    root: &Path,
    git_directory: &Path,
    git: &AuthenticatedExecutable<GitExecutable>,
    commit: &str,
    commands: &mut Vec<super::process::CommandReceipt>,
) -> Result<Option<String>> {
    let output = git_output(
        root,
        git_directory,
        git,
        ["ls-tree", "-z", "--full-tree", commit, "--", MANIFEST_PATH],
        commands,
    )?;
    ensure_git_success(&output, "query embedded head-to-head baseline tree")?;
    parse_manifest_tree_entry(&output.stdout)
}

fn parse_manifest_tree_entry(bytes: &[u8]) -> Result<Option<String>> {
    if bytes.is_empty() {
        return Ok(None);
    }
    ensure!(
        bytes.last() == Some(&0),
        "baseline tree query returned a non-NUL-terminated entry"
    );
    let entries = bytes[..bytes.len() - 1]
        .split(|byte| *byte == 0)
        .collect::<Vec<_>>();
    ensure!(
        entries.len() == 1,
        "baseline tree query returned {} entries for exact path {MANIFEST_PATH}",
        entries.len()
    );
    let separator = entries[0]
        .iter()
        .position(|byte| *byte == b'\t')
        .context("baseline tree entry has no path separator")?;
    let (metadata, path_with_separator) = entries[0].split_at(separator);
    let path = &path_with_separator[1..];
    ensure!(
        path == MANIFEST_PATH.as_bytes(),
        "baseline tree query returned an unexpected path"
    );
    let fields = metadata.split(|byte| *byte == b' ').collect::<Vec<_>>();
    ensure!(
        fields.len() == 3,
        "baseline tree entry has malformed metadata"
    );
    ensure!(
        fields[0] == b"100644" && fields[1] == b"blob",
        "baseline manifest must be one regular non-executable blob"
    );
    let object =
        std::str::from_utf8(fields[2]).context("baseline tree object identity is not UTF-8")?;
    ensure!(
        (object.len() == 40 || object.len() == 64)
            && object
                .bytes()
                .all(|byte| { byte.is_ascii_digit() || (b'a'..=b'f').contains(&byte) }),
        "baseline tree object identity is malformed"
    );
    Ok(Some(object.to_owned()))
}

fn git_output<const N: usize>(
    root: &Path,
    git_directory: &Path,
    git: &AuthenticatedExecutable<GitExecutable>,
    args: [&str; N],
    commands: &mut Vec<super::process::CommandReceipt>,
) -> Result<Output> {
    let mut command = super::process::HermeticCommand::new(git.for_execution()?);
    command
        .git_control_environment()
        .arg("--no-replace-objects")
        .arg("--git-dir")
        .arg(git_directory)
        .arg("--work-tree")
        .arg(root)
        .args(["-c", "core.hooksPath=/dev/null"])
        .args(["-c", "core.fsmonitor=false"])
        .args(["-c", "credential.helper="])
        .args(args);
    let (output, receipt) = super::process::output(&mut command, super::process::Limit::Probe)?;
    commands.push(receipt);
    Ok(output)
}

fn resolve_git_directory(root: &Path) -> Result<PathBuf> {
    let dot_git = root.join(".git");
    if dot_git.is_dir() {
        return dot_git
            .canonicalize()
            .with_context(|| format!("failed to resolve {}", dot_git.display()));
    }
    ensure!(
        dot_git.is_file(),
        "workspace has neither a .git directory nor a worktree .git file"
    );
    let mut file = fs::File::open(&dot_git)?;
    ensure!(
        file.metadata()?.len() <= 4096,
        "worktree .git file exceeds 4096 bytes"
    );
    let mut text = String::new();
    file.read_to_string(&mut text)?;
    let value = text
        .strip_prefix("gitdir:")
        .map(str::trim)
        .context("worktree .git file is not a single gitdir directive")?;
    ensure!(
        !value.is_empty() && !value.contains('\r') && !value.contains('\n'),
        "worktree .git file has a malformed gitdir directive"
    );
    let candidate = Path::new(value);
    let candidate = if candidate.is_absolute() {
        candidate.to_path_buf()
    } else {
        dot_git
            .parent()
            .context(".git file has no parent")?
            .join(candidate)
    };
    let resolved = candidate.canonicalize().with_context(|| {
        format!(
            "failed to resolve worktree Git directory {}",
            candidate.display()
        )
    })?;
    ensure!(
        resolved.is_dir(),
        "resolved worktree Git directory is not a directory"
    );
    Ok(resolved)
}

fn ensure_git_success(output: &Output, purpose: &str) -> Result<()> {
    ensure!(
        output.status.success(),
        "{purpose} failed ({})\n{}",
        output.status,
        super::process::tail(&super::process::combined(output))
    );
    Ok(())
}

pub(super) fn validate(manifest: &Manifest) -> Result<()> {
    validate_manifest_wire(manifest, SuiteBinding::Current)
}

#[derive(Clone, Copy)]
enum SuiteBinding {
    Current,
    Historical,
}

fn validate_manifest_wire(manifest: &Manifest, suite_binding: SuiteBinding) -> Result<()> {
    ensure!(
        manifest.schema_version == SCHEMA_VERSION,
        "embedded head-to-head manifest schema {} is unsupported; expected {SCHEMA_VERSION}",
        manifest.schema_version
    );
    ensure!(
        !manifest.ratchet_policy.trim().is_empty(),
        "ratchet_policy must explain the gate"
    );
    validate_suite_implementation_history(&manifest.suite_implementation_history, suite_binding)?;
    validate_tool_pins(&manifest.measured_tools)?;
    validate_comparator_output_pins(&manifest.comparator_outputs)?;
    ensure!(
        !manifest.entries.is_empty(),
        "an empty head-to-head manifest would measure no comparator"
    );
    if manifest
        .entries
        .iter()
        .any(|entry| entry.evidence_state.is_pending())
    {
        ensure!(
            manifest.entries.len() == 1 && manifest.entries[0].id == ECM_003_ID,
            "schema 9 requires pending ECM-003 as its sole catalog row"
        );
    }
    let mut ids = BTreeSet::new();
    for entry in &manifest.entries {
        validate_entry_wire(entry)?;
        ensure!(ids.insert(&entry.id), "duplicate entry id `{}`", entry.id);
    }
    Ok(())
}

/// Prove that a proposed manifest did not weaken a reviewed predecessor.
///
/// The predecessor comes from Git history in CI, rather than from another
/// editable field in the proposed manifest. Schema 9 has one pending
/// replacement row and no accepted baseline. Its
/// protocol bytes are immutable until a later schema introduces the complete
/// raw-byte-authenticated evidence chain.
#[cfg(test)]
pub(super) fn validate_non_relaxation(current: &Manifest, prior: &Manifest) -> Result<()> {
    validate(current).context("current manifest is invalid")?;
    validate_manifest_wire(prior, SuiteBinding::Historical).context("prior manifest is invalid")?;
    validate_ordinary_non_relaxation(current, prior).map(|_| ())
}

#[derive(Debug)]
pub(super) struct ValidatedNonRelaxation<'a> {
    appended_runner_replacements: &'a [SuiteImplementationReplacement],
}

pub(super) fn validate_historical_non_relaxation<'a>(
    current: &'a CheckedCurrentManifest,
    prior: &HistoricalManifest,
) -> Result<ValidatedNonRelaxation<'a>> {
    validate_ordinary_non_relaxation(&current.manifest, &prior.manifest)
}

fn validate_ordinary_non_relaxation<'a>(
    current: &'a Manifest,
    prior: &Manifest,
) -> Result<ValidatedNonRelaxation<'a>> {
    let appended_runner_replacements = current
        .suite_implementation_history
        .appended_after(&prior.suite_implementation_history)?;
    ensure!(
        current.ratchet_policy == prior.ratchet_policy,
        "reviewed ratchet policy changed; schema 9 has no migration authority"
    );
    ensure!(
        current.measured_tools == prior.measured_tools,
        "reviewed measured-tool SHA-256 pins changed; schema 9 has no migration authority"
    );
    ensure!(
        current.comparator_outputs == prior.comparator_outputs,
        "reviewed comparator-output SHA-256 pins changed; schema 9 has no migration authority"
    );
    for prior_entry in &prior.entries {
        let current_entry = current
            .entries
            .iter()
            .find(|entry| entry.id == prior_entry.id)
            .with_context(|| format!("reviewed benchmark row `{}` was removed", prior_entry.id))?;
        ensure!(
            same_benchmark_identity(current_entry, prior_entry),
            "{}: benchmark identity changed under an existing row id; add a new row instead",
            prior_entry.id
        );
        ensure!(
            current_entry == prior_entry,
            "{}: pending schema-9 row changed before its evidence migration",
            prior_entry.id
        );
    }
    for current_entry in &current.entries {
        if prior
            .entries
            .iter()
            .any(|prior_entry| prior_entry.id == current_entry.id)
        {
            continue;
        }
        anyhow::bail!(
            "{}: schema 9 has no authority to add another competitor row",
            current_entry.id
        );
    }
    Ok(ValidatedNonRelaxation {
        appended_runner_replacements,
    })
}

fn validate_suite_implementation_history(
    history: &SuiteImplementationHistory,
    suite_binding: SuiteBinding,
) -> Result<()> {
    ensure!(
        is_sha256(&history.initial_sha256),
        "initial suite implementation identity must be a SHA-256"
    );
    let mut implementations = BTreeSet::from([history.initial_sha256.as_str()]);
    for replacement in &history.replacements {
        ensure!(
            is_sha256(&replacement.implementation_sha256),
            "replacement suite implementation identity must be a SHA-256"
        );
        ensure!(
            implementations.insert(&replacement.implementation_sha256),
            "suite implementation history repeats or reverts to an earlier identity"
        );
        ensure!(
            !replacement.rationale.trim().is_empty()
                && !replacement.rationale.contains(['\r', '\n']),
            "suite implementation replacement rationale must be one non-empty line"
        );
    }
    if matches!(suite_binding, SuiteBinding::Current) {
        let observed = crate::verify_cmd::embedded_head_to_head::suite_identity::sha256();
        ensure!(
            history.active_sha256() == observed,
            "trusted benchmark implementation identity changed: observed {observed}, expected {}; append one authenticated implementation replacement without changing benchmark semantics",
            history.active_sha256(),
        );
    }
    Ok(())
}

fn validate_tool_pins(pins: &MeasuredToolPins) -> Result<()> {
    for (name, digest) in [
        ("measured_tools.arm_gcc_sha256", &pins.arm_gcc_sha256),
        ("measured_tools.arm_nm_sha256", &pins.arm_nm_sha256),
        ("measured_tools.qemu_sha256", &pins.qemu_sha256),
        ("measured_tools.python_sha256", &pins.python_sha256),
        ("measured_tools.prlimit_sha256", &pins.prlimit_sha256),
        ("measured_tools.git_sha256", &pins.git_sha256),
        ("measured_tools.cargo_sha256", &pins.cargo_sha256),
        ("measured_tools.rustc_sha256", &pins.rustc_sha256),
        (
            "measured_tools.compiler_dependencies_sha256",
            &pins.compiler_dependencies_sha256,
        ),
        (
            "measured_tools.execution_tool_closure_sha256",
            &pins.execution_tool_closure_sha256,
        ),
    ] {
        ensure!(
            is_sha256(digest),
            "{name} must be 64 lowercase hexadecimal digits"
        );
    }
    ensure!(
        !pins.prlimit_version.trim().is_empty(),
        "measured_tools.prlimit_version must be exact and nonempty"
    );
    ensure!(
        !pins.git_version.trim().is_empty(),
        "measured_tools.git_version must be exact and nonempty"
    );
    ensure!(
        !pins.cargo_version.trim().is_empty(),
        "measured_tools.cargo_version must be exact and nonempty"
    );
    ensure!(
        !pins.rustc_version.trim().is_empty(),
        "measured_tools.rustc_version must be exact and nonempty"
    );
    if let Some(digest) = &pins.arm_size_sha256 {
        ensure!(
            is_sha256(digest),
            "measured_tools.arm_size_sha256 must be 64 lowercase hexadecimal digits"
        );
    }
    ensure!(
        pins.arm_size_sha256.is_none(),
        "measured_tools.arm_size_sha256 is reserved until a linked-text metric is implemented"
    );
    Ok(())
}

fn validate_comparator_output_pins(pins: &ComparatorOutputPins) -> Result<()> {
    for (name, digest) in [
        ("comparator_outputs.casadi_c_sha256", &pins.casadi_c_sha256),
        ("comparator_outputs.casadi_h_sha256", &pins.casadi_h_sha256),
    ] {
        ensure!(
            is_sha256(digest),
            "{name} must be 64 lowercase hexadecimal digits"
        );
    }
    Ok(())
}

fn same_benchmark_identity(current: &Entry, prior: &Entry) -> bool {
    let mut current = current.clone();
    let mut prior = prior.clone();
    for entry in [&mut current, &mut prior] {
        entry.why.clear();
    }
    current == prior
}

fn validate_entry_wire(entry: &Entry) -> Result<()> {
    ensure!(
        safe_id(&entry.id),
        "entry id `{}` must be one lowercase ASCII filename component containing only letters, digits, and interior hyphens",
        entry.id
    );
    ensure!(
        !entry.model.trim().is_empty(),
        "{}: model is empty",
        entry.id
    );
    ensure!(!entry.why.trim().is_empty(), "{}: why is empty", entry.id);
    if entry.evidence_state.is_pending() {
        ensure!(
            entry.max_delta.is_none() && entry.measured.accepted().is_none(),
            "{}: a pending schema-9 row cannot carry an accepted baseline or delta",
            entry.id
        );
    } else {
        #[cfg(test)]
        validate_test_baseline(entry)?;
    }
    for path in [
        &entry.fixture,
        &entry.comparator_generator,
        &entry.comparator_wrapper,
        &entry.input_header,
    ] {
        ensure_relative(path, &entry.id)?;
    }
    for (name, digest) in [
        ("fixture_sha256", &entry.fixture_sha256),
        (
            "comparator_generator_sha256",
            &entry.comparator_generator_sha256,
        ),
        (
            "comparator_wrapper_sha256",
            &entry.comparator_wrapper_sha256,
        ),
        ("input_sha256", &entry.input_sha256),
        ("harness_sha256", &entry.harness_sha256),
        (
            "normalized_profile_sha256",
            &entry.normalized_profile_sha256,
        ),
    ] {
        ensure!(
            is_sha256(digest),
            "{}: {name} must be 64 lowercase hexadecimal digits",
            entry.id
        );
    }
    let case_ids = entry
        .correctness_cases
        .iter()
        .map(|case| case.id.as_str())
        .collect::<Vec<_>>();
    ensure!(
        case_ids == REQUIRED_CASE_IDS,
        "{}: correctness cases must be the closed ordered cohort {REQUIRED_CASE_IDS:?}",
        entry.id
    );
    for case in &entry.correctness_cases {
        ensure!(
            case.expected_output_bits.len() == 10,
            "{}: case {} must carry exactly ten output words",
            entry.id,
            case.id
        );
        ensure!(
            case.expected_output_bits.iter().all(|word| {
                word.len() == 8
                    && word
                        .bytes()
                        .all(|byte| byte.is_ascii_digit() || (b'a'..=b'f').contains(&byte))
            }),
            "{}: case {} output words must be eight lowercase hexadecimal digits",
            entry.id,
            case.id
        );
    }
    ensure!(
        entry.match_or_beat_target_delta == 0,
        "{}: match-or-beat target must be zero",
        entry.id
    );
    validate_artifact_history(entry)?;
    Ok(())
}

fn validate_artifact_history(entry: &Entry) -> Result<()> {
    if entry.evidence_state.is_pending() {
        ensure!(
            entry.rumoca_artifact_history.is_empty(),
            "{}: a pending schema-9 row cannot carry artifact history before the raw-byte receipt migration",
            entry.id
        );
    }
    Ok(())
}

#[cfg(test)]
fn validate_test_baseline(entry: &Entry) -> Result<()> {
    let measured = entry
        .measured
        .accepted()
        .context("test exact-pin row lacks measured values")?;
    let max_delta = entry
        .max_delta
        .context("test exact-pin row lacks max_delta")?;
    ensure!(
        !measured.comment.trim().is_empty(),
        "{}: measured.comment is empty",
        entry.id
    );
    let measured_delta = i128::from(measured.rumoca) - i128::from(measured.comparator);
    ensure!(
        measured_delta == i128::from(max_delta),
        "{}: max_delta {max_delta} does not equal recorded delta {measured_delta}",
        entry.id
    );
    let case_digest = correctness_cases_sha256(&entry.correctness_cases);
    for revision in &entry.rumoca_artifact_history {
        validate_artifact_revision(&entry.id, revision, &case_digest)?;
    }
    Ok(())
}

#[cfg(test)]
fn validate_artifact_revision(
    id: &str,
    revision: &RumocaArtifactRevision,
    case_digest: &str,
) -> Result<()> {
    ensure!(
        !revision.rationale.is_empty()
            && revision.rationale.trim() == revision.rationale
            && !revision.rationale.contains(['\r', '\n']),
        "{id}: artifact rationale must be one canonical non-empty line"
    );
    ensure!(
        revision.rumoca_metric > 0,
        "{id}: artifact metric must be nonzero"
    );
    validate_artifact_session(id, &revision.artifact_session)?;
    ensure!(
        is_sha256(&revision.efmu_package_sha256),
        "{id}: eFMU package identity must be a SHA-256"
    );
    validate_production_member(id, &revision.production_c, EFMI_PRODUCTION_C_MEMBER)?;
    validate_production_member(id, &revision.production_h, EFMI_PRODUCTION_H_MEMBER)?;
    ensure!(
        revision.archive_members.len() == super::efmu_artifact::canonical_members().len(),
        "{id}: eFMU archive-member authentication inventory must contain the exact canonical target inventory"
    );
    for (member, expected_path) in revision
        .archive_members
        .iter()
        .zip(super::efmu_artifact::canonical_members())
    {
        ensure_archive_member(&member.archive_path, id)?;
        ensure!(
            member.archive_path == *expected_path,
            "{id}: eFMU archive-member inventory path/order differs from the canonical target inventory"
        );
        ensure!(
            is_sha256(&member.sha256),
            "{id}: eFMU archive member identity must be a SHA-256"
        );
    }
    for production in [&revision.production_c, &revision.production_h] {
        ensure!(
            revision.archive_members.iter().any(|member| {
                member.archive_path == production.archive_path && member.sha256 == production.sha256
            }),
            "{id}: Production member pin must equal its full archive-inventory pin"
        );
    }
    validate_checksum_web(id, revision)?;
    ensure!(
        is_sha256(&revision.oracle_success_receipt.receipt_sha256),
        "{id}: oracle-success receipt identity must be a SHA-256"
    );
    ensure!(
        revision.oracle_success_receipt.efmu_package_sha256 == revision.efmu_package_sha256,
        "{id}: oracle-success receipt is bound to a different eFMU package"
    );
    ensure!(
        revision.oracle_success_receipt.correctness_cases_sha256 == case_digest,
        "{id}: oracle-success receipt is bound to a different correctness cohort"
    );
    Ok(())
}

pub(super) fn validate_artifact_session(id: &str, session: &ArtifactSessionInputs) -> Result<()> {
    ensure!(
        canonical_utc_second(&session.generation_instant),
        "{id}: artifact generation instant must be canonical UTC-second YYYY-MM-DDTHH:MM:SSZ"
    );
    ensure!(
        canonical_identity_seed(&session.identity_seed),
        "{id}: artifact identity seed must be a canonical lowercase hyphenated UUID"
    );
    Ok(())
}

#[cfg(test)]
fn validate_production_member(
    id: &str,
    member: &EfmuProductionMember,
    expected_path: &str,
) -> Result<()> {
    ensure_archive_member(&member.archive_path, id)?;
    ensure!(
        member.archive_path == expected_path,
        "{id}: Production member path must be exactly `{expected_path}`"
    );
    ensure!(
        is_sha256(&member.sha256),
        "{id}: Production member identity must be a SHA-256"
    );
    Ok(())
}

#[cfg(test)]
fn validate_checksum_web(id: &str, revision: &RumocaArtifactRevision) -> Result<()> {
    let expected = [
        (EFMI_ALGORITHM_SOURCE_MEMBER, EFMI_ALGORITHM_MANIFEST_MEMBER),
        (
            revision.production_c.archive_path.as_str(),
            EFMI_PRODUCTION_MANIFEST_MEMBER,
        ),
        (
            revision.production_h.archive_path.as_str(),
            EFMI_PRODUCTION_MANIFEST_MEMBER,
        ),
        (
            EFMI_ALGORITHM_MANIFEST_MEMBER,
            EFMI_PRODUCTION_MANIFEST_MEMBER,
        ),
        (EFMI_ALGORITHM_MANIFEST_MEMBER, EFMI_ROOT_MEMBER),
        (EFMI_PRODUCTION_MANIFEST_MEMBER, EFMI_ROOT_MEMBER),
    ];
    ensure!(
        revision.checksum_web_membership.len() == expected.len(),
        "{id}: checksum-web receipt must contain the exact six AC/PC membership edges"
    );
    for (edge, (producer, consumer)) in revision.checksum_web_membership.iter().zip(expected) {
        ensure_archive_member(&edge.producer_archive_path, id)?;
        ensure_archive_member(&edge.consumer_archive_path, id)?;
        ensure!(
            edge.producer_archive_path == producer && edge.consumer_archive_path == consumer,
            "{id}: checksum-web membership edge order or identity changed"
        );
        ensure!(
            is_sha1(&edge.producer_sha1),
            "{id}: checksum-web producer identity must be a SHA-1"
        );
    }
    Ok(())
}

#[cfg(test)]
pub(super) fn correctness_cases_sha256(cases: &[CorrectnessCase]) -> String {
    let mut digest = Sha256::new();
    digest.update(b"embedded-head-to-head-oracle-cases-v1\0");
    for case in cases {
        digest.update((case.id.len() as u64).to_le_bytes());
        digest.update(case.id.as_bytes());
        digest.update((case.expected_output_bits.len() as u64).to_le_bytes());
        for word in &case.expected_output_bits {
            digest.update((word.len() as u64).to_le_bytes());
            digest.update(word.as_bytes());
        }
    }
    format!("{:x}", digest.finalize())
}

#[cfg(test)]
fn ensure_archive_member(value: &str, id: &str) -> Result<()> {
    ensure!(
        !value.is_empty()
            && value.len() <= 255
            && value.bytes().all(|byte| {
                byte.is_ascii_alphanumeric() || matches!(byte, b'/' | b'.' | b'_' | b'-')
            })
            && value
                .split('/')
                .all(|component| !component.is_empty() && component != "." && component != ".."),
        "{id}: `{value}` is not a portable exact eFMU archive member path"
    );
    Ok(())
}

fn canonical_utc_second(value: &str) -> bool {
    let bytes = value.as_bytes();
    if bytes.len() != 20
        || bytes[4] != b'-'
        || bytes[7] != b'-'
        || bytes[10] != b'T'
        || bytes[13] != b':'
        || bytes[16] != b':'
        || bytes[19] != b'Z'
        || bytes.iter().enumerate().any(|(index, byte)| {
            !matches!(index, 4 | 7 | 10 | 13 | 16 | 19) && !byte.is_ascii_digit()
        })
    {
        return false;
    }
    let year = decimal(&bytes[0..4]);
    let month = decimal(&bytes[5..7]);
    let day = decimal(&bytes[8..10]);
    let hour = decimal(&bytes[11..13]);
    let minute = decimal(&bytes[14..16]);
    let second = decimal(&bytes[17..19]);
    year > 0
        && (1..=12).contains(&month)
        && (1..=days_in_month(year, month)).contains(&day)
        && hour < 24
        && minute < 60
        && second < 60
}

fn canonical_identity_seed(value: &str) -> bool {
    value.len() == 36
        && value.bytes().enumerate().all(|(index, byte)| {
            if matches!(index, 8 | 13 | 18 | 23) {
                byte == b'-'
            } else {
                byte.is_ascii_digit() || (b'a'..=b'f').contains(&byte)
            }
        })
}

fn decimal(bytes: &[u8]) -> u32 {
    bytes
        .iter()
        .fold(0, |value, byte| value * 10 + u32::from(byte - b'0'))
}

const fn days_in_month(year: u32, month: u32) -> u32 {
    match month {
        2 if year.is_multiple_of(400) || (year.is_multiple_of(4) && !year.is_multiple_of(100)) => {
            29
        }
        2 => 28,
        4 | 6 | 9 | 11 => 30,
        _ => 31,
    }
}

fn safe_id(value: &str) -> bool {
    let bytes = value.as_bytes();
    let alphanumeric = |byte: &u8| byte.is_ascii_lowercase() || byte.is_ascii_digit();
    !bytes.is_empty()
        && bytes.len() <= 96
        && bytes.first().is_some_and(alphanumeric)
        && bytes.last().is_some_and(alphanumeric)
        && bytes.iter().all(|byte| alphanumeric(byte) || *byte == b'-')
}

fn ensure_relative(value: &str, id: &str) -> Result<()> {
    let path = Path::new(value);
    ensure!(
        !path.is_absolute()
            && !path
                .components()
                .any(|component| matches!(component, Component::ParentDir)),
        "{id}: `{value}` must be a workspace-relative path without `..`"
    );
    Ok(())
}

fn is_sha256(value: &str) -> bool {
    value.len() == 64
        && value
            .bytes()
            .all(|byte| byte.is_ascii_digit() || (b'a'..=b'f').contains(&byte))
}

#[cfg(test)]
fn is_sha1(value: &str) -> bool {
    value.len() == 40
        && value
            .bytes()
            .all(|byte| byte.is_ascii_digit() || (b'a'..=b'f').contains(&byte))
}
