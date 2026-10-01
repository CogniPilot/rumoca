use anyhow::{Context, Result, bail};
use rumoca_core::{msl_cache_dir_from_manifest, workspace_root_from_manifest_dir};
use serde::Serialize;
use serde_json::Value;
use std::collections::HashSet;
use std::io::{self, Read, Write};
use std::path::{Path, PathBuf};
use std::process::{Command, Stdio};
use std::thread;
use std::thread::JoinHandle;
use std::time::{Duration, Instant, SystemTime, UNIX_EPOCH};

pub const MSL_VERSION: &str = "4.1.0";
// Keep OMC simulation isolated per model with a strict timeout budget.
pub const BATCH_SIZE_OMC_SIMULATION_DEFAULT: usize = 1;
// Per-model OMC simulate() wall budget. Shared with the rumoca sim worker
// (`rumoca_worker::MSL_SIM_TIMEOUT_SECS`) so both tools get the same time to
// simulate a model — a model is only fairly comparable under an equal budget.
pub const BATCH_TIMEOUT_SECONDS_DEFAULT: u64 = rumoca_worker::MSL_SIM_TIMEOUT_SECS as u64;
pub const AUTO_WORKERS_DEFAULT: usize = 0;
pub const OMC_THREADS_DEFAULT: usize = 1;
pub const SIM_STOP_TIME_DEFAULT: f64 = 1.0;
pub const OMC_BATCH_TIMEOUT_POLL: Duration = Duration::from_millis(25);

#[derive(Debug, Clone, Copy)]
struct MslPackageSpec {
    package_name: &'static str,
    candidates: &'static [&'static str],
}

const MSL_PACKAGE_SPECS: &[MslPackageSpec] = &[
    MslPackageSpec {
        package_name: "Complex",
        candidates: &["Complex.mo"],
    },
    MslPackageSpec {
        package_name: "Modelica",
        candidates: &["Modelica 4.1.0/package.mo"],
    },
    MslPackageSpec {
        package_name: "ModelicaTest",
        candidates: &["ModelicaTest 4.1.0/package.mo", "ModelicaTest/package.mo"],
    },
    MslPackageSpec {
        package_name: "ModelicaReference",
        candidates: &["ModelicaReference 4.1.0/package.mo"],
    },
    MslPackageSpec {
        package_name: "ModelicaTestOverdetermined",
        candidates: &["ModelicaTestOverdetermined.mo"],
    },
];

#[derive(Debug, Clone)]
pub struct MslPaths {
    pub repo_root: PathBuf,
    pub msl_dir: PathBuf,
    pub results_dir: PathBuf,
    pub flat_dir: PathBuf,
    pub work_dir: PathBuf,
    pub sim_work_dir: PathBuf,
    pub omc_trace_dir: PathBuf,
    pub rumoca_trace_dir: PathBuf,
}

impl MslPaths {
    pub fn from_manifest_dir(manifest_dir: &str) -> Self {
        let repo_root = workspace_root_from_manifest_dir(manifest_dir);
        let cache_dir = msl_cache_dir_from_manifest(manifest_dir);
        let msl_dir = cache_dir.join(format!("ModelicaStandardLibrary-{MSL_VERSION}"));
        let results_dir = cache_dir.join("results");
        let flat_dir = results_dir.join("omc_flat");
        let work_dir = results_dir.join("omc_work");
        let sim_work_dir = results_dir.join("omc_sim_work");
        let trace_root_dir = results_dir.join("sim_traces");
        let omc_trace_dir = trace_root_dir.join("omc");
        let rumoca_trace_dir = trace_root_dir.join("rumoca");
        Self {
            repo_root,
            msl_dir,
            results_dir,
            flat_dir,
            work_dir,
            sim_work_dir,
            omc_trace_dir,
            rumoca_trace_dir,
        }
    }

    pub fn current() -> Self {
        // Relocatable: a prebuilt (Nix/crane) binary's compile-time
        // CARGO_MANIFEST_DIR points at the build sandbox (`/build/...`), which is
        // gone at runtime. Resolve the crate manifest dir from the workspace root
        // walked up from the CWD, so the report subcommands (compatibility-report,
        // pr-comment, modelica-test-catalog) find `target/msl/...` in the consuming
        // job's checkout rather than a nonexistent sandbox path. Falls back to the
        // compile-time manifest dir for a normal in-tree run (same result).
        let manifest_dir = crate::repo_root().join("crates/rumoca-test-msl");
        Self::from_manifest_dir(&manifest_dir.to_string_lossy())
    }

    pub fn with_results_dir(mut self, results_dir: &Path) -> Self {
        let results_dir = if results_dir.is_absolute() {
            results_dir.to_path_buf()
        } else {
            self.repo_root.join(results_dir)
        };
        let trace_root_dir = results_dir.join("sim_traces");
        self.flat_dir = results_dir.join("omc_flat");
        self.work_dir = results_dir.join("omc_work");
        self.sim_work_dir = results_dir.join("omc_sim_work");
        self.omc_trace_dir = trace_root_dir.join("omc");
        self.rumoca_trace_dir = trace_root_dir.join("rumoca");
        self.results_dir = results_dir;
        self
    }
}

#[derive(Debug, Clone, Serialize)]
pub struct BatchTimingDetail {
    pub batch_idx: usize,
    pub requested_models: usize,
    pub parsed_models: usize,
    pub elapsed_seconds: f64,
    pub timed_out: bool,
    pub skipped: bool,
}

#[derive(Debug, Clone, Serialize)]
pub struct BatchElapsedStats {
    pub min: f64,
    pub median: f64,
    pub mean: f64,
    pub max: f64,
}

#[derive(Debug, Clone)]
pub struct CommandRunOutput {
    pub stdout: String,
    pub stderr: String,
    pub timed_out: bool,
}

pub fn choose_effective_batch_size(
    total_models: usize,
    requested_batch_size: usize,
    workers: usize,
) -> Result<usize> {
    if requested_batch_size == 0 {
        bail!("--batch-size must be > 0");
    }
    if total_models == 0 || workers <= 1 {
        return Ok(requested_batch_size);
    }
    let per_worker = total_models.div_ceil(workers);
    Ok(requested_batch_size.min(per_worker).max(1))
}

pub fn summarize_batch_timings(batch_timings: &[BatchTimingDetail]) -> Option<BatchElapsedStats> {
    let mut elapsed: Vec<f64> = batch_timings
        .iter()
        .filter(|batch| !batch.skipped)
        .map(|batch| batch.elapsed_seconds)
        .filter(|value| value.is_finite())
        .collect();
    if elapsed.is_empty() {
        return None;
    }
    elapsed.sort_by(|a, b| a.partial_cmp(b).unwrap_or(std::cmp::Ordering::Equal));
    let len = elapsed.len();
    let median = if len.is_multiple_of(2) {
        (elapsed[len / 2 - 1] + elapsed[len / 2]) / 2.0
    } else {
        elapsed[len / 2]
    };
    let mean = elapsed.iter().sum::<f64>() / elapsed.len() as f64;
    Some(BatchElapsedStats {
        min: round3(*elapsed.first().unwrap_or(&0.0)),
        median: round3(median),
        mean: round3(mean),
        max: round3(*elapsed.last().unwrap_or(&0.0)),
    })
}

fn resolve_msl_packages(paths: &MslPaths) -> Vec<(&'static str, PathBuf)> {
    MSL_PACKAGE_SPECS
        .iter()
        .filter_map(|spec| {
            spec.candidates
                .iter()
                .map(|candidate| paths.msl_dir.join(candidate))
                .find(|path| path.exists())
                .map(|path| (spec.package_name, path))
        })
        .collect()
}

pub fn msl_load_lines(paths: &MslPaths) -> Vec<String> {
    // OMC supplies its own ModelicaServices. Preloading the generic MSL
    // implementation replaces URI resolution with fullPathName(uri).
    resolve_msl_packages(paths)
        .into_iter()
        .map(|(_, path)| format!("loadFile(\"{}\");", path.display()))
        .collect()
}

pub fn get_omc_version() -> String {
    let mut command = Command::new("omc");
    command.arg("--version");
    let timeout = Duration::from_secs(10);
    let output = run_command_with_timeout(&mut command, timeout);
    match output {
        Ok(output) if !output.stdout.trim().is_empty() => output.stdout.trim().to_string(),
        _ => "unknown".to_string(),
    }
}

pub fn get_git_commit(repo_root: &Path) -> String {
    let mut command = Command::new("git");
    command.arg("rev-parse").arg("HEAD").current_dir(repo_root);
    let timeout = Duration::from_secs(10);
    let output = run_command_with_timeout(&mut command, timeout);
    match output {
        Ok(output) if !output.stdout.trim().is_empty() => output.stdout.trim().to_string(),
        _ => "unknown".to_string(),
    }
}

/// Whether the working tree carried uncommitted changes when an artifact was
/// written.
///
/// A commit alone is not provenance for a locally-produced artifact: a dirty
/// tree means the numbers came from code that is not at that commit, so a later
/// reader cannot reproduce them from the commit alone. Recording the flag makes
/// that checkable instead of assumed.
pub fn git_worktree_is_dirty(repo_root: &Path) -> bool {
    let mut command = Command::new("git");
    command
        .arg("status")
        .arg("--porcelain")
        .current_dir(repo_root);
    match run_command_with_timeout(&mut command, Duration::from_secs(10)) {
        Ok(output) => !output.stdout.trim().is_empty(),
        // An unreadable tree cannot be shown clean; say dirty rather than
        // stamping an artifact with a cleanliness nobody verified.
        Err(_) => true,
    }
}

/// A content digest of the working tree's uncommitted changes, or `None` when
/// the tree is clean.
///
/// [`git_worktree_is_dirty`] says *that* an artifact came from uncommitted
/// code; it cannot say *which* uncommitted code. Two runs of one commit with
/// different working-tree content — exactly how a change is iterated before it
/// lands — produce artifacts that are indistinguishable by commit and dirty
/// flag alone, so a reader cannot tell which run a table describes, nor order
/// two of them. Hashing `git diff HEAD` together with the untracked-file roster
/// makes each working-tree state self-identifying.
///
/// An unreadable tree yields a sentinel rather than `None`: "clean" is a claim,
/// and a claim nobody verified must not be stamped on an artifact.
pub fn git_worktree_content_digest(repo_root: &Path) -> Option<String> {
    let mut diff = Command::new("git");
    diff.arg("diff")
        .arg("HEAD")
        .arg("--binary")
        .current_dir(repo_root);
    let mut untracked = Command::new("git");
    untracked
        .arg("ls-files")
        .arg("--others")
        .arg("--exclude-standard")
        .current_dir(repo_root);
    let timeout = Duration::from_secs(30);
    let (Ok(diff), Ok(untracked)) = (
        run_command_with_timeout(&mut diff, timeout),
        run_command_with_timeout(&mut untracked, timeout),
    ) else {
        return Some("unverified".to_string());
    };
    if diff.stdout.trim().is_empty() && untracked.stdout.trim().is_empty() {
        return None;
    }
    let mut hasher = blake3::Hasher::new();
    hasher.update(diff.stdout.as_bytes());
    hasher.update(b"\0untracked\0");
    hasher.update(untracked.stdout.as_bytes());
    Some(hasher.finalize().to_hex().to_string())
}

pub fn run_command_with_timeout(
    command: &mut Command,
    timeout: Duration,
) -> std::io::Result<CommandRunOutput> {
    command.stdout(Stdio::piped()).stderr(Stdio::piped());
    let mut child = command.spawn()?;
    let stdout_reader = spawn_pipe_reader(
        child
            .stdout
            .take()
            .ok_or_else(|| io::Error::other("child stdout pipe was not available"))?,
    );
    let stderr_reader = spawn_pipe_reader(
        child
            .stderr
            .take()
            .ok_or_else(|| io::Error::other("child stderr pipe was not available"))?,
    );
    let deadline = Instant::now() + timeout;
    let timed_out = loop {
        if child.try_wait()?.is_some() {
            break false;
        }
        if Instant::now() >= deadline {
            let _ = child.kill();
            break true;
        }
        thread::sleep(OMC_BATCH_TIMEOUT_POLL);
    };
    child.wait()?;
    let stdout = join_pipe_reader(stdout_reader, "stdout")?;
    let stderr = join_pipe_reader(stderr_reader, "stderr")?;
    Ok(CommandRunOutput {
        stdout: String::from_utf8_lossy(&stdout).into_owned(),
        stderr: String::from_utf8_lossy(&stderr).into_owned(),
        timed_out,
    })
}

fn spawn_pipe_reader(mut pipe: impl Read + Send + 'static) -> JoinHandle<io::Result<Vec<u8>>> {
    thread::spawn(move || {
        let mut bytes = Vec::new();
        pipe.read_to_end(&mut bytes)?;
        Ok(bytes)
    })
}

fn join_pipe_reader(
    reader: JoinHandle<io::Result<Vec<u8>>>,
    stream_name: &str,
) -> io::Result<Vec<u8>> {
    reader
        .join()
        .map_err(|_| io::Error::other(format!("child {stream_name} reader panicked")))?
}

pub fn apply_omc_thread_env(command: &mut Command, omc_threads: usize) {
    let threads = omc_threads.max(1).to_string();
    command.arg(format!("--numProcs={threads}"));
    command.env("OMP_NUM_THREADS", &threads);
    command.env("OPENBLAS_NUM_THREADS", &threads);
    command.env("MKL_NUM_THREADS", &threads);
    command.env("NUMEXPR_NUM_THREADS", &threads);
}

pub fn has_fatal_omc_error(error_text: &str) -> bool {
    error_text.lines().any(is_fatal_error_line)
}

pub fn summarize_omc_error(error_text: &str, result_text: &str) -> String {
    let text = if error_text.trim().is_empty() {
        result_text
    } else {
        error_text
    };
    let summary = text
        .lines()
        .map(str::trim)
        .filter(|line| !line.is_empty())
        .filter(|line| !line.starts_with("Notification: Automatically loaded package"))
        .filter(|line| !line.starts_with("Warning: Requested package"))
        .collect::<Vec<_>>()
        .join("\n");
    let compact = summary.trim();
    if compact.is_empty() {
        return "empty result".to_string();
    }
    truncate_utf8(compact, 500).to_string()
}

pub fn load_target_models(path: &Path) -> Result<Vec<String>> {
    let payload: Value = serde_json::from_str(
        &std::fs::read_to_string(path)
            .with_context(|| format!("failed to read target models file '{}'", path.display()))?,
    )
    .with_context(|| format!("failed to parse target models JSON '{}'", path.display()))?;
    let raw = match payload {
        Value::Array(list) => list,
        Value::Object(map) => map
            .get("model_names")
            .and_then(Value::as_array)
            .cloned()
            .context("target models object missing array field 'model_names'")?,
        _ => bail!("target models JSON must be an array or object with model_names"),
    };
    let mut seen = HashSet::new();
    let mut names = Vec::new();
    for item in raw {
        let Some(name) = item.as_str().map(str::trim) else {
            continue;
        };
        if name.is_empty() || !seen.insert(name.to_string()) {
            continue;
        }
        names.push(name.to_string());
    }
    Ok(names)
}

/// Repository-relative path of the tracked trace-exception list.
///
/// Owned here rather than by either consumer because two of them read it: the
/// comparator (which skips the model) and the band table (which records *why* a
/// cohort model is absent). A second copy of the path would let the two drift.
pub const TRACE_EXCLUSIONS_FILE_REL: &str =
    "crates/rumoca-test-msl/tests/msl_tests/msl_trace_compare_exclusions.json";

/// Schema tag of the exception file, so a foreign or free-form list is
/// rejected rather than read as "nothing is excepted".
pub const TRACE_EXCLUSIONS_SCHEMA: &str = "msl_trace_exceptions_v2";

/// Why a model that simulates is not held to strict-high pointwise parity
/// (SPEC_0033 typed trace exceptions). There is no other kind: a model that
/// simulates without one of these must reach the strict-high band.
#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Serialize)]
#[serde(rename_all = "snake_case")]
pub enum TraceExceptionKind {
    /// A random or impure source: traces differ by construction, so the
    /// obligation is statistical, not pointwise.
    ImpureSource,
    /// The OMC reference does not resolve the model: it is not converged or
    /// not accurate enough for the observables compared.
    ReferenceFailure,
    /// A documented property of the model, such as chaos, a discontinuity
    /// wrapped in `noEvent` by design, or an observable the model leaves
    /// underdetermined, makes one trajectory non-identifying.
    ModelIssue,
    /// A reviewed proof that pointwise comparison cannot identify the model.
    Nonidentifiable,
    /// The comparator, not the model or the reference, cannot identify the
    /// channels; the row names the comparator improvement that retires it.
    ComparatorLimitation,
}

impl TraceExceptionKind {
    fn parse(value: &str) -> Option<Self> {
        Some(match value {
            "impure_source" => Self::ImpureSource,
            "reference_failure" => Self::ReferenceFailure,
            "model_issue" => Self::ModelIssue,
            "nonidentifiable" => Self::Nonidentifiable,
            "comparator_limitation" => Self::ComparatorLimitation,
            _ => return None,
        })
    }

    #[must_use]
    pub const fn as_str(self) -> &'static str {
        match self {
            Self::ImpureSource => "impure_source",
            Self::ReferenceFailure => "reference_failure",
            Self::ModelIssue => "model_issue",
            Self::Nonidentifiable => "nonidentifiable",
            Self::ComparatorLimitation => "comparator_limitation",
        }
    }
}

/// The comparator improvement that would retire a `comparator_limitation`
/// row. The end state is fewer exceptions through a better comparator.
#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Serialize)]
#[serde(rename_all = "snake_case")]
pub enum ComparatorImprovement {
    /// Normalize channels whose reference is zero in exact arithmetic by a
    /// physical scale instead of the reference's own near-zero spread.
    NearZeroChannelScaling,
    /// Align samples at an event instant by event side, so pre- and
    /// post-event values recorded at shifted coordinates compare.
    EventInstantAlignment,
    /// Compare period aggregates (Mean, RMS) at the model's annotation
    /// tolerance rather than at the reference's integration residue.
    AggregateToleranceAtAnnotationScale,
    /// Compare angle channels on the circle, so pi and -pi across the atan2
    /// branch cut agree, and treat the angle of an exactly zero phasor as
    /// undefined rather than as whichever of 0, pi or -pi its signed zeros
    /// select.
    AngleBranchAwareComparison,
}

impl ComparatorImprovement {
    fn parse(value: &str) -> Option<Self> {
        Some(match value {
            "near_zero_channel_scaling" => Self::NearZeroChannelScaling,
            "event_instant_alignment" => Self::EventInstantAlignment,
            "aggregate_tolerance_at_annotation_scale" => Self::AggregateToleranceAtAnnotationScale,
            "angle_branch_aware_comparison" => Self::AngleBranchAwareComparison,
            _ => return None,
        })
    }
}

/// The checkable evidence behind one exception: the facts the review
/// measured (channel counts, tolerances, event times, reference values), and
/// where recorded, the artifact and the commit that recorded it. Documentation
/// shipped with the pinned MSL (`msl:<class>`) is its own record and needs no
/// commit.
#[derive(Clone, Debug, PartialEq, Eq, Serialize)]
pub struct TraceExceptionEvidence {
    pub facts: Vec<String>,
    pub commit: Option<String>,
    pub artifact: Option<String>,
}

/// One typed trace exception.
#[derive(Clone, Debug, PartialEq, Eq, Serialize)]
pub struct TraceException {
    pub kind: TraceExceptionKind,
    pub reason: String,
    pub evidence: TraceExceptionEvidence,
    /// Present exactly for [`TraceExceptionKind::ComparatorLimitation`].
    pub retired_by: Option<ComparatorImprovement>,
}

/// Read the tracked exception list: model name -> its typed exception.
///
/// # Acceptance contract
///
/// Accepts exactly an object carrying [`TRACE_EXCLUSIONS_SCHEMA`] and an
/// `exceptions` array whose every entry has a non-empty `model_name`, a known
/// `kind`, a non-empty `reason`, and `evidence` with at least one non-empty
/// fact. An evidence `commit` must be hexadecimal with at least seven digits;
/// a repository `artifact` needs the commit that recorded it, and only
/// `msl:<class>` documentation is its own record. A `comparator_limitation`
/// row names a known `retired_by` improvement and no other kind carries one.
/// It rejects the free-form v1 list, an entry missing any of these, and a
/// duplicated model: nothing reaches the gate unreviewed.
pub fn load_trace_exclusions_file(
    path: &Path,
) -> Result<std::collections::BTreeMap<String, TraceException>> {
    let raw = std::fs::read_to_string(path)
        .with_context(|| format!("failed to read trace exceptions '{}'", path.display()))?;
    let payload: Value = serde_json::from_str(&raw)
        .with_context(|| format!("failed to parse trace exceptions '{}'", path.display()))?;
    parse_trace_exclusions(&payload)
        .with_context(|| format!("invalid trace exceptions '{}'", path.display()))
}

/// Each exception as the reason text the comparator and band table record:
/// its kind, then its reason, so every artifact quoting it names the kind.
#[must_use]
pub fn typed_exception_reasons(
    exceptions: std::collections::BTreeMap<String, TraceException>,
) -> std::collections::BTreeMap<String, String> {
    exceptions
        .into_iter()
        .map(|(model, exception)| {
            let reason = format!("{}: {}", exception.kind.as_str(), exception.reason);
            (model, reason)
        })
        .collect()
}

fn parse_trace_exclusions(
    payload: &Value,
) -> Result<std::collections::BTreeMap<String, TraceException>> {
    let Some(object) = payload.as_object() else {
        bail!("trace exceptions must be an object with '{TRACE_EXCLUSIONS_SCHEMA}' and typed rows");
    };
    let schema = object.get("schema").and_then(Value::as_str).unwrap_or("");
    if schema != TRACE_EXCLUSIONS_SCHEMA {
        bail!("trace exceptions schema is '{schema}', expected '{TRACE_EXCLUSIONS_SCHEMA}'");
    }
    let Some(entries) = object.get("exceptions").and_then(Value::as_array) else {
        bail!("trace exceptions object is missing the `exceptions` array");
    };
    let mut exceptions = std::collections::BTreeMap::new();
    for (index, entry) in entries.iter().enumerate() {
        let (model_name, exception) = parse_trace_exception(index, entry)?;
        if exceptions.insert(model_name.clone(), exception).is_some() {
            bail!("exceptions list '{model_name}' more than once");
        }
    }
    Ok(exceptions)
}

fn nonempty_str<'a>(value: &'a Value, key: &str) -> Option<&'a str> {
    value
        .get(key)
        .and_then(Value::as_str)
        .map(str::trim)
        .filter(|text| !text.is_empty())
}

fn parse_trace_exception(index: usize, entry: &Value) -> Result<(String, TraceException)> {
    let model_name = nonempty_str(entry, "model_name")
        .with_context(|| format!("exceptions[{index}] has no `model_name`"))?;
    let kind = nonempty_str(entry, "kind")
        .and_then(TraceExceptionKind::parse)
        .with_context(|| {
            format!(
                "exception '{model_name}' has no known `kind` (impure_source, reference_failure, \
                 model_issue, nonidentifiable, comparator_limitation)"
            )
        })?;
    let reason = nonempty_str(entry, "reason")
        .with_context(|| format!("exception '{model_name}' has no `reason`"))?;
    let evidence = entry
        .get("evidence")
        .with_context(|| format!("exception '{model_name}' has no `evidence`"))?;
    let evidence = parse_trace_exception_evidence(model_name, evidence)?;
    let retired_by = parse_retired_by(model_name, kind, entry)?;
    Ok((
        model_name.to_string(),
        TraceException {
            kind,
            reason: reason.to_string(),
            evidence,
            retired_by,
        },
    ))
}

fn parse_trace_exception_evidence(
    model_name: &str,
    evidence: &Value,
) -> Result<TraceExceptionEvidence> {
    let facts = evidence
        .get("facts")
        .and_then(Value::as_array)
        .map(|facts| {
            facts
                .iter()
                .map(|fact| fact.as_str().map(str::trim).filter(|fact| !fact.is_empty()))
                .collect::<Option<Vec<_>>>()
        })
        .with_context(|| format!("exception '{model_name}' evidence has no `facts` array"))?
        .with_context(|| format!("exception '{model_name}' evidence has an empty fact"))?;
    if facts.is_empty() {
        bail!("exception '{model_name}' evidence states no checkable fact");
    }
    let commit = nonempty_str(evidence, "commit");
    if let Some(commit) = commit
        && (commit.len() < 7 || !commit.chars().all(|c| c.is_ascii_hexdigit()))
    {
        bail!("exception '{model_name}' evidence commit '{commit}' is not a commit hash");
    }
    let artifact = nonempty_str(evidence, "artifact");
    if let Some(artifact) = artifact
        && commit.is_none()
        && !artifact.starts_with("msl:")
    {
        bail!(
            "exception '{model_name}' evidence needs the commit that recorded '{artifact}'; only \
             pinned MSL documentation (`msl:<class>`) is its own record"
        );
    }
    Ok(TraceExceptionEvidence {
        facts: facts.into_iter().map(str::to_string).collect(),
        commit: commit.map(str::to_string),
        artifact: artifact.map(str::to_string),
    })
}

fn parse_retired_by(
    model_name: &str,
    kind: TraceExceptionKind,
    entry: &Value,
) -> Result<Option<ComparatorImprovement>> {
    let retired_by = nonempty_str(entry, "retired_by");
    match (kind, retired_by) {
        (TraceExceptionKind::ComparatorLimitation, Some(value)) => {
            ComparatorImprovement::parse(value)
                .map(Some)
                .with_context(|| {
                    format!(
                        "exception '{model_name}' names no known `retired_by` improvement \
                     (near_zero_channel_scaling, event_instant_alignment, \
                     aggregate_tolerance_at_annotation_scale, angle_branch_aware_comparison)"
                    )
                })
        }
        (TraceExceptionKind::ComparatorLimitation, None) => bail!(
            "exception '{model_name}' is a comparator_limitation without the `retired_by` \
             improvement that retires it"
        ),
        (
            TraceExceptionKind::ImpureSource
            | TraceExceptionKind::ReferenceFailure
            | TraceExceptionKind::ModelIssue
            | TraceExceptionKind::Nonidentifiable,
            Some(_),
        ) => bail!(
            "exception '{model_name}' carries `retired_by` but is not a comparator_limitation"
        ),
        (
            TraceExceptionKind::ImpureSource
            | TraceExceptionKind::ReferenceFailure
            | TraceExceptionKind::ModelIssue
            | TraceExceptionKind::Nonidentifiable,
            None,
        ) => Ok(None),
    }
}

pub fn write_pretty_json<T: Serialize>(path: &Path, value: &T) -> Result<()> {
    let parent = path
        .parent()
        .filter(|parent| !parent.as_os_str().is_empty())
        .unwrap_or_else(|| Path::new("."));
    std::fs::create_dir_all(parent)
        .with_context(|| format!("failed to create '{}'", parent.display()))?;
    let payload = serde_json::to_string_pretty(value).context("failed to serialize JSON")?;
    // Cache materialization hard-links traces across runs. Replace this path
    // only after the new payload is complete, preserving the other links.
    let mut replacement = tempfile::NamedTempFile::new_in(parent)
        .with_context(|| format!("failed to create replacement for '{}'", path.display()))?;
    replacement
        .write_all(payload.as_bytes())
        .with_context(|| format!("failed to write '{}'", path.display()))?;
    replacement
        .persist(path)
        .with_context(|| format!("failed to replace '{}'", path.display()))?;
    Ok(())
}

pub fn unix_timestamp_seconds() -> i64 {
    let now = SystemTime::now();
    let Ok(duration) = now.duration_since(UNIX_EPOCH) else {
        return 0;
    };
    i64::try_from(duration.as_secs()).unwrap_or(0)
}

pub fn round3(value: f64) -> f64 {
    (value * 1000.0).round() / 1000.0
}

fn is_fatal_error_line(line: &str) -> bool {
    let trimmed = line.trim_start();
    let without_bracket = if trimmed.starts_with('[') {
        trimmed
            .find(']')
            .map(|idx| trimmed[idx + 1..].trim_start())
            .unwrap_or(trimmed)
    } else {
        trimmed
    };
    without_bracket.starts_with("Error:")
}

fn truncate_utf8(text: &str, max_bytes: usize) -> &str {
    if text.len() <= max_bytes {
        return text;
    }
    let mut end = max_bytes;
    while end > 0 && !text.is_char_boundary(end) {
        end -= 1;
    }
    &text[..end]
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::fs;

    fn test_paths(msl_dir: PathBuf) -> MslPaths {
        MslPaths {
            repo_root: PathBuf::from("/tmp/repo"),
            msl_dir,
            results_dir: PathBuf::from("/tmp/results"),
            flat_dir: PathBuf::from("/tmp/results/omc_flat"),
            work_dir: PathBuf::from("/tmp/results/omc_work"),
            sim_work_dir: PathBuf::from("/tmp/results/omc_sim_work"),
            omc_trace_dir: PathBuf::from("/tmp/results/sim_traces/omc"),
            rumoca_trace_dir: PathBuf::from("/tmp/results/sim_traces/rumoca"),
        }
    }

    #[test]
    fn command_timeout_reader_drains_output_larger_than_a_pipe_buffer() {
        let mut command = Command::new("sh");
        command
            .arg("-c")
            .arg("head -c 1048576 /dev/zero | tr '\\000' x; printf stderr-sentinel >&2");

        let output = run_command_with_timeout(&mut command, Duration::from_secs(5))
            .expect("large child output should be drained while the process runs");

        assert!(
            !output.timed_out,
            "a finite producer must not deadlock on its pipe"
        );
        assert_eq!(output.stdout.len(), 1_048_576);
        assert_eq!(output.stderr, "stderr-sentinel");
    }

    #[test]
    fn apply_omc_thread_env_sets_process_and_library_caps() {
        let mut command = Command::new("omc");
        apply_omc_thread_env(&mut command, 3);

        let args = command
            .get_args()
            .map(|arg| arg.to_string_lossy().into_owned())
            .collect::<Vec<_>>();
        assert_eq!(args, vec!["--numProcs=3"]);
        assert_eq!(
            command
                .get_envs()
                .find(|(key, _)| *key == std::ffi::OsStr::new("OMP_NUM_THREADS"))
                .and_then(|(_, value)| value),
            Some(std::ffi::OsStr::new("3"))
        );
        assert_eq!(
            command
                .get_envs()
                .find(|(key, _)| *key == std::ffi::OsStr::new("OPENBLAS_NUM_THREADS"))
                .and_then(|(_, value)| value),
            Some(std::ffi::OsStr::new("3"))
        );
    }

    #[test]
    fn msl_load_lines_supports_release_zip_layout() {
        let temp = tempfile::tempdir().expect("tempdir");
        let msl_dir = temp.path();
        std::fs::write(msl_dir.join("Complex.mo"), "").expect("write Complex.mo");
        std::fs::create_dir_all(msl_dir.join("Modelica 4.1.0")).expect("create Modelica dir");
        std::fs::write(msl_dir.join("Modelica 4.1.0/package.mo"), "")
            .expect("write Modelica/package.mo");
        std::fs::create_dir_all(msl_dir.join("ModelicaServices 4.1.0"))
            .expect("create ModelicaServices dir");
        std::fs::write(msl_dir.join("ModelicaServices 4.1.0/package.mo"), "")
            .expect("write ModelicaServices/package.mo");
        std::fs::create_dir_all(msl_dir.join("ModelicaTest 4.1.0"))
            .expect("create ModelicaTest dir");
        std::fs::write(msl_dir.join("ModelicaTest 4.1.0/package.mo"), "")
            .expect("write ModelicaTest/package.mo");

        let paths = test_paths(msl_dir.to_path_buf());
        let lines = msl_load_lines(&paths);
        assert_eq!(lines.len(), 3);
        assert!(
            lines.iter().any(|line| line.contains("Complex.mo")),
            "expected Complex.mo loadFile entry"
        );
        assert!(
            lines
                .iter()
                .any(|line| line.contains("Modelica 4.1.0/package.mo")),
            "expected Modelica release-layout package load"
        );
        assert!(
            !lines.iter().any(|line| line.contains("ModelicaServices")),
            "OMC must load its own tool-specific services"
        );
        assert!(
            lines
                .iter()
                .any(|line| line.contains("ModelicaTest 4.1.0/package.mo")),
            "expected ModelicaTest release-layout package load"
        );
    }

    #[test]
    fn msl_load_lines_supports_source_tree_modelica_test_layout() {
        let temp = tempfile::tempdir().expect("tempdir");
        let msl_dir = temp.path();
        std::fs::create_dir_all(msl_dir.join("ModelicaTest")).expect("create ModelicaTest dir");
        std::fs::write(msl_dir.join("ModelicaTest/package.mo"), "")
            .expect("write ModelicaTest/package.mo");

        let paths = test_paths(msl_dir.to_path_buf());
        let lines = msl_load_lines(&paths);
        assert_eq!(lines.len(), 1);
        assert!(
            lines
                .iter()
                .any(|line| line.contains("ModelicaTest/package.mo")),
            "expected source-tree ModelicaTest package load"
        );
    }

    #[test]
    fn replacing_json_preserves_hard_linked_reference_history() {
        let temp = tempfile::tempdir().expect("tempdir");
        let historical = temp.path().join("historical.json");
        let current = temp.path().join("current.json");
        let old = serde_json::json!({"value": 1.0});
        let new = serde_json::json!({"value": 2.0});
        write_pretty_json(&historical, &old).expect("write original reference");
        std::fs::hard_link(&historical, &current).expect("materialize cache reference");

        write_pretty_json(&current, &new).expect("regenerate current reference");

        let read = |path: &Path| {
            serde_json::from_slice::<Value>(&std::fs::read(path).expect("read reference"))
                .expect("parse reference")
        };
        assert_eq!(read(&current), new);
        assert_eq!(read(&historical), old);
    }

    #[test]
    fn fatal_omc_error_detection_ignores_warning_lines() {
        assert!(!has_fatal_omc_error("Warning: Requested package Modelica"));
        assert!(has_fatal_omc_error(
            "[/tmp/file.mo:1:1-1:10:writable] Error: Illegal to instantiate partial class Demo."
        ));
    }

    #[test]
    fn summarize_omc_error_prefers_non_empty_diagnostics() {
        let error = "Notification: Automatically loaded package X\nError: Broken";
        let summary = summarize_omc_error(error, "Check failed");
        assert_eq!(summary, "Error: Broken");
    }

    #[test]
    fn load_target_models_supports_object_and_list_formats() {
        let temp = tempfile::tempdir().expect("tempdir");
        let object_path = temp.path().join("targets_object.json");
        fs::write(
            &object_path,
            r#"{"model_names":["A.B","A.B"," C.D ", "", 7]}"#,
        )
        .expect("write object");
        let object_names = load_target_models(&object_path).expect("load object");
        assert_eq!(object_names, vec!["A.B".to_string(), "C.D".to_string()]);

        let list_path = temp.path().join("targets_list.json");
        fs::write(&list_path, r#"["Modelica.A","Modelica.B"]"#).expect("write list");
        let list_names = load_target_models(&list_path).expect("load list");
        assert_eq!(
            list_names,
            vec!["Modelica.A".to_string(), "Modelica.B".to_string()]
        );
    }

    fn write_exceptions(dir: &Path, name: &str, rows: &str) -> PathBuf {
        let path = dir.join(name);
        fs::write(
            &path,
            format!(r#"{{"schema":"{TRACE_EXCLUSIONS_SCHEMA}","exceptions":[{rows}]}}"#),
        )
        .expect("write exceptions");
        path
    }

    /// Each row keeps its own kind, reason, evidence, and retirement path.
    #[test]
    fn trace_exceptions_are_typed_rows_with_evidence() {
        let temp = tempfile::tempdir().expect("tempdir");
        let path = write_exceptions(
            temp.path(),
            "ok.json",
            r#"{"model_name":"A.Noise","kind":"impure_source","reason":"random input",
                "evidence":{"facts":["seeded xorshift source"],"commit":"abcdef1","artifact":"docs/noise.md"}},
               {"model_name":"B.Chaos","kind":"model_issue","reason":"chaotic",
                "evidence":{"facts":["StopTime=50000 s"],"artifact":"msl:B.Chaos"}},
               {"model_name":"C.Node","kind":"comparator_limitation","reason":"zero node",
                "evidence":{"facts":["374/391 channels high"]},"retired_by":"near_zero_channel_scaling"}"#,
        );
        let exceptions = load_trace_exclusions_file(&path).expect("load exceptions");
        assert_eq!(exceptions.len(), 3);
        let noise = &exceptions["A.Noise"];
        assert_eq!(noise.kind, TraceExceptionKind::ImpureSource);
        assert_eq!(noise.evidence.commit.as_deref(), Some("abcdef1"));
        assert_eq!(noise.retired_by, None);
        assert_eq!(
            exceptions["C.Node"].retired_by,
            Some(ComparatorImprovement::NearZeroChannelScaling)
        );
        assert_eq!(
            typed_exception_reasons(exceptions)["B.Chaos"],
            "model_issue: chaotic"
        );
    }

    /// Nothing free-form reaches the gate: the v1 list, a missing or unknown
    /// kind, missing facts, an uncommitted artifact, and a comparator row
    /// without its retirement path are all refused.
    #[test]
    fn trace_exceptions_refuse_untyped_or_unevidenced_rows() {
        let temp = tempfile::tempdir().expect("tempdir");
        let v1 = temp.path().join("v1.json");
        fs::write(
            &v1,
            r#"{"schema":"msl_trace_compare_exclusions","exclusions":[{"model_name":"A","reason":"why"}]}"#,
        )
        .expect("write v1");
        assert!(load_trace_exclusions_file(&v1).is_err());
        for (name, row, needle) in [
            (
                "nokind",
                r#"{"model_name":"A","reason":"r","evidence":{"facts":["f"]}}"#,
                "`kind`",
            ),
            (
                "badkind",
                r#"{"model_name":"A","kind":"allowance","reason":"r","evidence":{"facts":["f"]}}"#,
                "`kind`",
            ),
            (
                "noevidence",
                r#"{"model_name":"A","kind":"model_issue","reason":"r"}"#,
                "no `evidence`",
            ),
            (
                "nofacts",
                r#"{"model_name":"A","kind":"model_issue","reason":"r","evidence":{"facts":[]}}"#,
                "no checkable fact",
            ),
            (
                "nocommit",
                r#"{"model_name":"A","kind":"reference_failure","reason":"r","evidence":{"facts":["f"],"artifact":"docs/x.md"}}"#,
                "needs the commit",
            ),
            (
                "badcommit",
                r#"{"model_name":"A","kind":"reference_failure","reason":"r","evidence":{"facts":["f"],"commit":"main"}}"#,
                "not a commit hash",
            ),
            (
                "noretire",
                r#"{"model_name":"A","kind":"comparator_limitation","reason":"r","evidence":{"facts":["f"]}}"#,
                "without the `retired_by`",
            ),
            (
                "badretire",
                r#"{"model_name":"A","kind":"comparator_limitation","reason":"r","evidence":{"facts":["f"]},"retired_by":"wider_tolerance"}"#,
                "no known `retired_by`",
            ),
            (
                "strayretire",
                r#"{"model_name":"A","kind":"model_issue","reason":"r","evidence":{"facts":["f"]},"retired_by":"event_instant_alignment"}"#,
                "not a comparator_limitation",
            ),
        ] {
            let path = write_exceptions(temp.path(), name, row);
            let error = format!("{:#}", load_trace_exclusions_file(&path).expect_err(name));
            assert!(error.contains(needle), "{name}: {error}");
        }
    }

    /// Every tracked row is typed and evidenced, and every artifact it cites
    /// is in the repository or the pinned MSL (SPEC_0033 typed trace
    /// exceptions).
    #[test]
    fn tracked_exceptions_are_typed_and_evidenced() {
        let root = workspace_root_from_manifest_dir(env!("CARGO_MANIFEST_DIR"));
        let exceptions = load_trace_exclusions_file(&root.join(TRACE_EXCLUSIONS_FILE_REL))
            .expect("tracked exceptions must parse");
        assert_eq!(exceptions.len(), 36);
        let count = |kind| {
            exceptions
                .values()
                .filter(|exception| exception.kind == kind)
                .count()
        };
        assert_eq!(count(TraceExceptionKind::ReferenceFailure), 12);
        assert_eq!(count(TraceExceptionKind::ModelIssue), 6);
        assert_eq!(count(TraceExceptionKind::ComparatorLimitation), 18);
        for (model, exception) in &exceptions {
            if let Some(artifact) = &exception.evidence.artifact {
                assert!(
                    artifact.starts_with("msl:") || root.join(artifact).is_file(),
                    "{model}: evidence artifact {artifact} is not in the repository"
                );
            }
        }
    }
}
