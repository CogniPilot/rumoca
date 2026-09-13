//! Persistent OMC interactive (ZeroMQ) session.
//!
//! This is the OMC analogue of rumoca's `ModelWorkerDaemon`
//! (`crates/rumoca-worker`): a long-lived `omc` process that loads the MSL
//! once on startup and then services many per-model `simulate(...)` requests
//! over a ZeroMQ REQ/REP channel. A dedicated worker thread owns one session
//! and reuses it across the models it pulls from the shared work queue. When a
//! request exceeds its budget the REQ socket is left in an unusable state, so
//! the caller kills and respawns the session (reloading the MSL) exactly like
//! the rumoca daemon restarts on a hung model.
//!
//! Compared to cold per-model `omc <script.mos>` invocations this amortizes the
//! multi-second MSL parse across every model a worker handles, which is the
//! same win the rumoca warm worker provides.

use anyhow::{Context, Result, anyhow};
use std::path::{Path, PathBuf};
use std::process::{Child, Command, Stdio};
use std::time::{Duration, Instant};

use super::super::common::apply_omc_thread_env;

/// Poll interval while waiting for the OMC ZeroMQ port file to appear.
const PORT_FILE_POLL: Duration = Duration::from_millis(20);

/// Per-model timing self-reported by OMC's `SimulationResult` record. These are
/// OMC's own internal phase timers, so they are independent of scheduling jitter
/// and directly comparable to rumoca's per-phase seconds. The report derives
/// OMC compile time as `time_total - time_simulation`.
#[derive(Debug, Clone, Default, PartialEq)]
pub(super) struct OmcSimTiming {
    pub(super) time_simulation: Option<f64>,
    pub(super) time_total: Option<f64>,
}

/// Outcome of a single `simulate(...)` request inside a session.
#[derive(Debug, Clone)]
pub(super) struct OmcSimOutcome {
    pub(super) result_file: Option<String>,
    pub(super) messages: String,
    pub(super) error: String,
    pub(super) timing: OmcSimTiming,
}

/// Why an evaluation did not return a usable reply.
#[derive(Debug)]
pub(super) enum OmcEvalError {
    /// The reply did not arrive within the budget; the socket is now unusable
    /// and the session must be killed and respawned.
    Timeout,
    /// Transport-level failure (socket closed, process died, encoding error).
    Io(anyhow::Error),
}

impl std::fmt::Display for OmcEvalError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            OmcEvalError::Timeout => write!(f, "omc request timed out"),
            OmcEvalError::Io(error) => write!(f, "omc request failed: {error}"),
        }
    }
}

/// A warm, reusable OMC interactive session.
pub(super) struct OmcSession {
    child: Child,
    socket: zmq::Socket,
    // The context must outlive the socket; keep it owned by the session.
    _ctx: zmq::Context,
    port_file: PathBuf,
}

impl OmcSession {
    /// Spawn an `omc --interactive=zmq` process, connect to it, and load the MSL
    /// once. `msl_load_exprs` are evaluated in order (e.g. `loadFile("...")`).
    pub(super) fn spawn(
        work_dir: &Path,
        msl_load_exprs: &[String],
        omc_threads: usize,
        startup_timeout: Duration,
        load_timeout: Duration,
    ) -> Result<Self> {
        let suffix = unique_session_suffix();
        std::fs::create_dir_all(work_dir).with_context(|| {
            format!(
                "failed to create omc session work dir '{}'",
                work_dir.display()
            )
        })?;

        let mut command = Command::new("omc");
        command
            .arg("--interactive=zmq")
            .arg(format!("-z={suffix}"))
            .arg("--locale=C");
        apply_omc_thread_env(&mut command, omc_threads);
        // Keep the port file and any scratch output inside our work dir so
        // concurrent worker sessions never collide on the default $TMPDIR file.
        command
            .env("TMPDIR", work_dir)
            .current_dir(work_dir)
            .stdin(Stdio::null())
            .stdout(Stdio::null())
            .stderr(Stdio::null());
        // Put omc in its own process group so that on kill we can also reap the
        // separate simulation executables `simulate(...)` spawns — otherwise a
        // model whose integration hangs leaves a grandchild pegging a CPU core
        // after we kill the omc parent.
        #[cfg(unix)]
        std::os::unix::process::CommandExt::process_group(&mut command, 0);
        let ctx = zmq::Context::new();
        let socket = ctx
            .socket(zmq::REQ)
            .context("failed to create omc zmq REQ socket")?;
        // LINGER=0 so dropping a hung socket does not block process teardown.
        socket.set_linger(0)?;
        let mut child = command
            .spawn()
            .context("failed to spawn omc interactive session")?;

        let port_file = match connect_port_file(&socket, work_dir, &suffix, startup_timeout) {
            Ok(path) => path,
            Err(error) => {
                kill_omc_process(&mut child);
                if let Some(path) = find_port_file(work_dir, &format!("port.{suffix}")) {
                    let _ = std::fs::remove_file(path);
                }
                return Err(error);
            }
        };

        let mut session = OmcSession {
            child,
            socket,
            _ctx: ctx,
            port_file,
        };

        for expr in msl_load_exprs {
            let expr = expr.trim().trim_end_matches(';');
            session.eval(expr, load_timeout).map_err(|error| {
                anyhow!("failed to load MSL into omc session ({expr}): {error}")
            })?;
        }
        // Drain any accumulated load-time diagnostics so they do not leak into
        // the first model's error string.
        let _ = session.eval("getErrorString()", load_timeout);
        Ok(session)
    }

    /// Evaluate a single OMC expression, waiting at most `timeout` for the reply.
    pub(super) fn eval(&mut self, expr: &str, timeout: Duration) -> Result<String, OmcEvalError> {
        let millis = i32::try_from(timeout.as_millis().max(1)).unwrap_or(i32::MAX);
        self.socket
            .set_rcvtimeo(millis)
            .map_err(|error| OmcEvalError::Io(anyhow!("set_rcvtimeo failed: {error}")))?;
        self.socket
            .send(expr, 0)
            .map_err(|error| OmcEvalError::Io(anyhow!("send failed: {error}")))?;
        match self.socket.recv_string(0) {
            Ok(Ok(reply)) => Ok(reply),
            Ok(Err(_)) => Err(OmcEvalError::Io(anyhow!("omc reply was not valid utf-8"))),
            Err(zmq::Error::EAGAIN) => Err(OmcEvalError::Timeout),
            Err(error) => Err(OmcEvalError::Io(anyhow!("recv failed: {error}"))),
        }
    }

    /// Simulate one model. Returns the parsed outcome, or `OmcEvalError::Timeout`
    /// if the request exceeded `sim_timeout` (caller should respawn the session).
    pub(super) fn simulate_model(
        &mut self,
        model: &str,
        stop_time: f64,
        use_experiment_stop_time: bool,
        sim_timeout: Duration,
    ) -> Result<OmcSimOutcome, OmcEvalError> {
        let expr = if use_experiment_stop_time {
            format!("simulate({model}, outputFormat=\"csv\", fileNamePrefix=\"{model}\")")
        } else {
            format!(
                "simulate({model}, stopTime={stop_time}, outputFormat=\"csv\", fileNamePrefix=\"{model}\")"
            )
        };
        let record = self.eval(&expr, sim_timeout)?;
        let error = match self.eval("getErrorString()", Duration::from_secs(10)) {
            Ok(text) => unquote_omc_string(text.trim()),
            Err(OmcEvalError::Timeout) => return Err(OmcEvalError::Timeout),
            Err(other) => return Err(other),
        };
        Ok(parse_sim_record(&record, error))
    }

    /// Kill the underlying process. Used before respawning after a hang.
    pub(super) fn kill(&mut self) {
        kill_omc_process(&mut self.child);
        let _ = std::fs::remove_file(&self.port_file);
    }
}

impl Drop for OmcSession {
    fn drop(&mut self) {
        // Best-effort graceful quit, then ensure the process group is gone
        // (omc + any simulation executables it spawned).
        let _ = self.eval("quit()", Duration::from_millis(500));
        self.kill();
    }
}

/// The owned child is also the process-group leader, including during startup.
fn kill_omc_process(child: &mut Child) {
    #[cfg(unix)]
    if let Ok(pid) = i32::try_from(child.id()) {
        let _ = nix::sys::signal::killpg(
            nix::unistd::Pid::from_raw(pid),
            nix::sys::signal::Signal::SIGKILL,
        );
    }
    let _ = child.kill();
    let _ = child.wait();
}

fn connect_port_file(
    socket: &zmq::Socket,
    work_dir: &Path,
    suffix: &str,
    timeout: Duration,
) -> Result<PathBuf> {
    let port_file = wait_for_port_file(work_dir, suffix, timeout).ok_or_else(|| {
        anyhow!(
            "omc session port file for suffix '{suffix}' was not populated within {:.1}s",
            timeout.as_secs_f64()
        )
    })?;
    let endpoint = std::fs::read_to_string(&port_file)
        .with_context(|| format!("failed to read omc port file '{}'", port_file.display()))?;
    let endpoint = endpoint.trim();
    socket
        .connect(endpoint)
        .with_context(|| format!("failed to connect to omc endpoint '{endpoint}'"))?;
    Ok(port_file)
}

fn unique_session_suffix() -> String {
    use std::sync::atomic::{AtomicU64, Ordering};
    static COUNTER: AtomicU64 = AtomicU64::new(0);
    let nanos = std::time::SystemTime::now()
        .duration_since(std::time::UNIX_EPOCH)
        .map(|d| d.as_nanos())
        .unwrap_or(0);
    let seq = COUNTER.fetch_add(1, Ordering::Relaxed);
    format!("rumoca_{}_{seq}_{nanos}", std::process::id())
}

/// OMC writes its port file as `openmodelica.<user>.port.<suffix>` in `$TMPDIR`.
/// We point `$TMPDIR` at `work_dir`, so look there and match on the suffix to
/// avoid depending on the resolved user name.
fn wait_for_port_file(work_dir: &Path, suffix: &str, timeout: Duration) -> Option<PathBuf> {
    let deadline = Instant::now() + timeout;
    let needle = format!("port.{suffix}");
    loop {
        // OMC creates this file before fputs/fclose publishes the endpoint.
        // Existence alone can expose the empty file between those operations.
        if let Some(path) = find_port_file(work_dir, &needle)
            && std::fs::metadata(&path).is_ok_and(|metadata| metadata.len() > 0)
        {
            return Some(path);
        }
        if Instant::now() >= deadline {
            return None;
        }
        std::thread::sleep(PORT_FILE_POLL);
    }
}

fn find_port_file(work_dir: &Path, needle: &str) -> Option<PathBuf> {
    let entries = std::fs::read_dir(work_dir).ok()?;
    entries.flatten().find_map(|entry| {
        let name = entry.file_name();
        let name = name.to_string_lossy();
        (name.starts_with("openmodelica.") && name.ends_with(needle)).then(|| entry.path())
    })
}

/// Strip the surrounding quotes OMC puts around string replies and unescape the
/// common `\n`/`\"` sequences.
fn unquote_omc_string(text: &str) -> String {
    let trimmed = text.trim();
    let inner = trimmed
        .strip_prefix('"')
        .and_then(|rest| rest.strip_suffix('"'))
        .unwrap_or(trimmed);
    inner.replace("\\n", "\n").replace("\\\"", "\"")
}

fn parse_sim_record(record: &str, error: String) -> OmcSimOutcome {
    OmcSimOutcome {
        result_file: extract_record_string(record, "resultFile").filter(|value| !value.is_empty()),
        messages: extract_record_string(record, "messages").unwrap_or_default(),
        error,
        timing: OmcSimTiming {
            time_simulation: extract_record_f64(record, "timeSimulation"),
            time_total: extract_record_f64(record, "timeTotal"),
        },
    }
}

/// Extract `field = "<value>"` from an OMC record reply.
fn extract_record_string(record: &str, field: &str) -> Option<String> {
    let key = format!("{field} =");
    let start = record.find(&key)? + key.len();
    let rest = record[start..].trim_start();
    let rest = rest.strip_prefix('"')?;
    let end = find_unescaped_quote(rest)?;
    Some(rest[..end].replace("\\\"", "\"").replace("\\n", "\n"))
}

fn find_unescaped_quote(text: &str) -> Option<usize> {
    let bytes = text.as_bytes();
    let mut i = 0;
    while i < bytes.len() {
        match bytes[i] {
            b'\\' => i += 2,
            b'"' => return Some(i),
            _ => i += 1,
        }
    }
    None
}

/// Extract `field = <number>` from an OMC record reply.
fn extract_record_f64(record: &str, field: &str) -> Option<f64> {
    let key = format!("{field} =");
    let start = record.find(&key)? + key.len();
    let rest = record[start..].trim_start();
    let token: String = rest
        .chars()
        .take_while(|c| c.is_ascii_digit() || matches!(c, '.' | '-' | '+' | 'e' | 'E'))
        .collect();
    token.parse::<f64>().ok().filter(|v| v.is_finite())
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn empty_port_file_is_not_a_ready_session() {
        let directory = tempfile::tempdir().unwrap();
        let suffix = "empty_endpoint";
        let path = directory
            .path()
            .join(format!("openmodelica.test.port.{suffix}"));
        std::fs::write(&path, "").unwrap();
        assert!(wait_for_port_file(directory.path(), suffix, Duration::ZERO).is_none());
        std::fs::write(&path, "tcp://127.0.0.1:12345").unwrap();
        assert_eq!(
            wait_for_port_file(directory.path(), suffix, Duration::ZERO),
            Some(path)
        );
    }

    #[test]
    fn parse_sim_record_extracts_fields() {
        let record = r#"record SimulationResult
    resultFile = "/tmp/work/First_res.csv",
    simulationOptions = "startTime = 0.0, stopTime = 1.0",
    messages = "",
    timeFrontend = 0.012,
    timeBackend = 0.034,
    timeSimCode = 0.001,
    timeTemplates = 0.002,
    timeCompile = 0.433,
    timeSimulation = 0.0334,
    timeTotal = 0.5739
end SimulationResult;"#;
        let outcome = parse_sim_record(record, "".to_string());
        assert_eq!(
            outcome.result_file.as_deref(),
            Some("/tmp/work/First_res.csv")
        );
        assert_eq!(outcome.timing.time_total, Some(0.5739));
        assert_eq!(outcome.timing.time_simulation, Some(0.0334));
    }

    #[test]
    fn parse_sim_record_handles_failed_empty_result() {
        let record = r#"record SimulationResult
    resultFile = "",
    messages = "Simulation Failed. Model: X does not exist!",
    timeFrontend = 0.0,
    timeTotal = 0.0
end SimulationResult;"#;
        let outcome = parse_sim_record(record, "Error: boom".to_string());
        assert_eq!(outcome.result_file, None);
        assert!(outcome.messages.contains("does not exist"));
        assert_eq!(outcome.error, "Error: boom");
    }

    #[test]
    fn reference_outcome_preserves_runtime_assertion_failure() {
        let record = r#"record SimulationResult
    resultFile = "",
    messages = "Simulation execution failed for model: ResourceUser
LOG_ASSERT | debug | Not possible to open file modelica://Library/data.txt
LOG_ASSERT | info | simulation terminated by an assertion at initialization
",
    timeSimulation = 0.014,
    timeTotal = 0.831
end SimulationResult;"#;
        let outcome = parse_sim_record(
            record,
            "Warning: pure function calls impure function".into(),
        );
        let result = super::super::build_session_model_result(&outcome, 1.0);

        assert_eq!(result.status, "error");
        let error = result.error.expect("runtime failure diagnostic");
        assert!(error.contains("Simulation execution failed"));
        assert!(error.contains("modelica://Library/data.txt"));
        assert_eq!(super::super::omc_assertion_failure_lines(&error).len(), 1);
    }

    #[test]
    fn reference_outcome_requires_a_result_even_without_diagnostics() {
        let outcome = parse_sim_record(
            "record SimulationResult resultFile = \"\", messages = \"\" end SimulationResult;",
            String::new(),
        );
        let result = super::super::build_session_model_result(&outcome, 1.0);

        assert_eq!(result.status, "error");
        assert!(
            result
                .error
                .expect("missing result diagnostic")
                .contains("result file")
        );
    }

    #[test]
    fn reference_outcome_accepts_result_with_nonfatal_warning() {
        let outcome = parse_sim_record(
            r#"record SimulationResult
    resultFile = "result.csv",
    messages = "LOG_ASSERT | warning | [<interactive>:1:68-1:125:writable]
| | | The following assertion has been violated during initialization at time 0.000000
| | | ((x < -1.0)) --> \"expected warning\"
LOG_SUCCESS | info | The simulation finished successfully.
"
end SimulationResult;"#,
            "Warning: harmless diagnostic".into(),
        );
        let result = super::super::build_session_model_result(&outcome, 1.0);

        assert_eq!(result.status, "success");
        assert_eq!(result.result_file.as_deref(), Some("result.csv"));
        assert_eq!(result.error, None);
    }

    #[test]
    fn unquote_strips_and_unescapes() {
        assert_eq!(unquote_omc_string("\"hello\\nworld\""), "hello\nworld");
        assert_eq!(unquote_omc_string("\"\""), "");
        assert_eq!(unquote_omc_string("bare"), "bare");
    }
}
