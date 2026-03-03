use anyhow::{Context, Result, bail};
use rumoca_core::{msl_cache_dir_from_manifest, workspace_root_from_manifest_dir};
use serde::Serialize;
use serde_json::Value;
use std::collections::{HashSet, VecDeque};
use std::path::{Path, PathBuf};
use std::process::{Command, Stdio};
use std::sync::{Arc, Mutex, mpsc};
use std::thread;
use std::time::{Duration, Instant, SystemTime, UNIX_EPOCH};

pub(crate) const MSL_VERSION: &str = "4.1.0";
// Keep OMC compile/reference isolated per model by default so a single hung
// model cannot poison an entire batch.
pub(crate) const BATCH_SIZE_OMC_REFERENCE_DEFAULT: usize = 1;
// Keep OMC simulation isolated per model with a strict timeout budget.
pub(crate) const BATCH_SIZE_OMC_SIMULATION_DEFAULT: usize = 1;
// OMC startup + MSL loading can consume multiple seconds per invocation.
// Keep the default high enough that per-model process isolation does not force
// all models into batch timeouts before simulation work starts.
pub(crate) const BATCH_TIMEOUT_SECONDS_DEFAULT: u64 = 30;
pub(crate) const AUTO_WORKERS_DEFAULT: usize = 0;
pub(crate) const AUTO_WORKERS_RESERVED_CPUS: usize = 3;
pub(crate) const OMC_THREADS_DEFAULT: usize = 1;
pub(crate) const SIM_STOP_TIME_DEFAULT: f64 = 1.0;
pub(crate) const OMC_BATCH_TIMEOUT_POLL: Duration = Duration::from_millis(25);

pub(crate) const MSL_PACKAGES: [&str; 5] = [
    "Complex.mo",
    "ModelicaServices/package.mo",
    "Modelica/package.mo",
    "ModelicaTest/package.mo",
    "ModelicaTestOverdetermined.mo",
];

pub(crate) const MSL_TOP_PACKAGES: [&str; 5] = [
    "Complex",
    "ModelicaServices",
    "Modelica",
    "ModelicaTest",
    "ModelicaTestOverdetermined",
];

pub(crate) const EXCLUDE_PREFIXES: [&str; 3] = [
    "ObsoleteModelica4",
    "ModelicaTestConversion4",
    "ModelicaReference",
];

#[derive(Debug, Clone)]
pub(crate) struct MslPaths {
    pub(crate) repo_root: PathBuf,
    pub(crate) msl_dir: PathBuf,
    pub(crate) results_dir: PathBuf,
    pub(crate) flat_dir: PathBuf,
    pub(crate) work_dir: PathBuf,
    pub(crate) sim_work_dir: PathBuf,
    pub(crate) omc_trace_dir: PathBuf,
    pub(crate) rumoca_trace_dir: PathBuf,
}

impl MslPaths {
    pub(crate) fn from_manifest_dir(manifest_dir: &str) -> Self {
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

    pub(crate) fn current() -> Self {
        Self::from_manifest_dir(env!("CARGO_MANIFEST_DIR"))
    }

    pub(crate) fn work_dir_for_run_id(&self, run_id: Option<&str>) -> PathBuf {
        let Some(run_id) = run_id else {
            return self.work_dir.clone();
        };
        if run_id.is_empty() {
            self.work_dir.clone()
        } else {
            self.work_dir.join(run_id)
        }
    }
}

#[derive(Debug, Clone, Serialize)]
pub(crate) struct BatchTimingDetail {
    pub(crate) batch_idx: usize,
    pub(crate) requested_models: usize,
    pub(crate) parsed_models: usize,
    pub(crate) elapsed_seconds: f64,
    pub(crate) timed_out: bool,
    pub(crate) skipped: bool,
}

#[derive(Debug, Clone)]
pub(crate) struct PendingBatch {
    pub(crate) batch_idx: usize,
    pub(crate) start_idx: usize,
    pub(crate) end_idx: usize,
    pub(crate) models: Vec<String>,
}

#[derive(Debug, Clone, Serialize)]
pub(crate) struct BatchElapsedStats {
    pub(crate) min: f64,
    pub(crate) median: f64,
    pub(crate) mean: f64,
    pub(crate) max: f64,
}

#[derive(Debug, Clone)]
pub(crate) struct CommandRunOutput {
    pub(crate) stdout: String,
    pub(crate) stderr: String,
    pub(crate) timed_out: bool,
}

pub(crate) fn resolve_worker_count(workers_requested: usize) -> Result<usize> {
    if workers_requested == 0 {
        let cpu_count = std::thread::available_parallelism()
            .map(usize::from)
            .unwrap_or(1);
        return Ok(cpu_count.saturating_sub(AUTO_WORKERS_RESERVED_CPUS).max(1));
    }
    Ok(workers_requested.max(1))
}

pub(crate) fn choose_effective_batch_size(
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

pub(crate) fn summarize_batch_timings(
    batch_timings: &[BatchTimingDetail],
) -> Option<BatchElapsedStats> {
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

pub(crate) fn msl_load_lines(paths: &MslPaths) -> Vec<String> {
    MSL_PACKAGES
        .iter()
        .map(|pkg| paths.msl_dir.join(pkg))
        .filter(|path| path.exists())
        .map(|path| format!("loadFile(\"{}\");", path.display()))
        .collect()
}

pub(crate) fn get_omc_version() -> String {
    let mut command = Command::new("omc");
    command.arg("--version");
    let timeout = Duration::from_secs(10);
    let output = run_command_with_timeout(&mut command, timeout);
    match output {
        Ok(output) if !output.stdout.trim().is_empty() => output.stdout.trim().to_string(),
        _ => "unknown".to_string(),
    }
}

pub(crate) fn get_git_commit(repo_root: &Path) -> String {
    let mut command = Command::new("git");
    command.arg("rev-parse").arg("HEAD").current_dir(repo_root);
    let timeout = Duration::from_secs(10);
    let output = run_command_with_timeout(&mut command, timeout);
    match output {
        Ok(output) if !output.stdout.trim().is_empty() => output.stdout.trim().to_string(),
        _ => "unknown".to_string(),
    }
}

pub(crate) fn run_command_with_timeout(
    command: &mut Command,
    timeout: Duration,
) -> std::io::Result<CommandRunOutput> {
    command.stdout(Stdio::piped()).stderr(Stdio::piped());
    let mut child = command.spawn()?;
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
    let output = child.wait_with_output()?;
    Ok(CommandRunOutput {
        stdout: String::from_utf8_lossy(&output.stdout).into_owned(),
        stderr: String::from_utf8_lossy(&output.stderr).into_owned(),
        timed_out,
    })
}

pub(crate) fn apply_omc_thread_env(command: &mut Command, omc_threads: usize) {
    let threads = omc_threads.max(1).to_string();
    command.env("OMP_NUM_THREADS", &threads);
    command.env("OPENBLAS_NUM_THREADS", &threads);
    command.env("MKL_NUM_THREADS", &threads);
    command.env("NUMEXPR_NUM_THREADS", &threads);
}

pub(crate) fn run_parallel_batches_with_progress<R, F, P>(
    pending_batches: Vec<PendingBatch>,
    workers: usize,
    task: F,
    mut on_complete: P,
) -> Result<Vec<(PendingBatch, R)>>
where
    R: Send + 'static,
    F: Fn(PendingBatch) -> Result<R> + Send + Sync + 'static,
    P: FnMut(&PendingBatch, &R),
{
    if pending_batches.is_empty() {
        return Ok(Vec::new());
    }
    if workers <= 1 {
        let mut outputs = Vec::with_capacity(pending_batches.len());
        for batch in pending_batches {
            let output = task(batch.clone())?;
            on_complete(&batch, &output);
            outputs.push((batch, output));
        }
        return Ok(outputs);
    }

    let worker_count = workers.min(pending_batches.len()).max(1);
    let queue = Arc::new(Mutex::new(VecDeque::from(pending_batches)));
    let task = Arc::new(task);
    let (tx, rx) = mpsc::channel::<Result<(PendingBatch, R)>>();
    let mut handles = Vec::with_capacity(worker_count);

    for _ in 0..worker_count {
        let queue = Arc::clone(&queue);
        let task = Arc::clone(&task);
        let tx = tx.clone();
        handles.push(thread::spawn(move || {
            while let Some(batch) = pop_pending_batch(&queue) {
                let result = task(batch.clone()).map(|output| (batch, output));
                let _ = tx.send(result);
            }
        }));
    }
    drop(tx);

    let mut outputs = Vec::new();
    let mut first_error: Option<anyhow::Error> = None;
    for message in rx {
        match message {
            Ok((batch, output)) => {
                on_complete(&batch, &output);
                outputs.push((batch, output));
            }
            Err(error) => {
                if first_error.is_none() {
                    first_error = Some(error);
                }
            }
        }
    }
    for handle in handles {
        let _ = handle.join();
    }
    if let Some(error) = first_error {
        return Err(error);
    }
    Ok(outputs)
}

fn pop_pending_batch(queue: &Arc<Mutex<VecDeque<PendingBatch>>>) -> Option<PendingBatch> {
    queue.lock().ok().and_then(|mut queue| queue.pop_front())
}

pub(crate) fn has_fatal_omc_error(error_text: &str) -> bool {
    error_text.lines().any(is_fatal_error_line)
}

pub(crate) fn summarize_omc_error(error_text: &str, result_text: &str) -> String {
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

pub(crate) fn load_target_models(path: &Path) -> Result<Vec<String>> {
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

pub(crate) fn write_pretty_json<T: Serialize>(path: &Path, value: &T) -> Result<()> {
    if let Some(parent) = path.parent() {
        std::fs::create_dir_all(parent)
            .with_context(|| format!("failed to create '{}'", parent.display()))?;
    }
    let payload = serde_json::to_string_pretty(value).context("failed to serialize JSON")?;
    std::fs::write(path, payload).with_context(|| format!("failed to write '{}'", path.display()))
}

pub(crate) fn unix_timestamp_seconds() -> i64 {
    let now = SystemTime::now();
    let Ok(duration) = now.duration_since(UNIX_EPOCH) else {
        return 0;
    };
    i64::try_from(duration.as_secs()).unwrap_or(0)
}

pub(crate) fn round3(value: f64) -> f64 {
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
}
