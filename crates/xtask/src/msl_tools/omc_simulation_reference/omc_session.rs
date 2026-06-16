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
use std::env;
use std::ffi::OsString;
use std::path::{Path, PathBuf};
use std::process::{Child, Command, Stdio};
use std::time::{Duration, Instant};

/// Poll interval while waiting for the OMC ZeroMQ port file to appear.
const PORT_FILE_POLL: Duration = Duration::from_millis(20);
const FALLBACK_DOCKER_OMC_IMAGE: &str = "openmodelica/openmodelica:v1.26.3-minimal";

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
    suffix: String,
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

        let mut command = build_omc_interactive_command(work_dir, &suffix, omc_threads);
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
        let child = command
            .spawn()
            .context("failed to spawn omc interactive session")?;

        let port_file = match wait_for_port_file(work_dir, &suffix, startup_timeout) {
            Some(path) => path,
            None => {
                let mut child = child;
                let _ = child.kill();
                let _ = child.wait();
                return Err(anyhow!(
                    "omc session port file for suffix '{suffix}' did not appear within {:.1}s",
                    startup_timeout.as_secs_f64()
                ));
            }
        };
        let endpoint = std::fs::read_to_string(&port_file)
            .with_context(|| format!("failed to read omc port file '{}'", port_file.display()))?
            .trim()
            .to_string();

        let ctx = zmq::Context::new();
        let socket = ctx
            .socket(zmq::REQ)
            .context("failed to create omc zmq REQ socket")?;
        // LINGER=0 so dropping a hung socket does not block process teardown.
        socket.set_linger(0).ok();
        socket
            .connect(&endpoint)
            .with_context(|| format!("failed to connect to omc endpoint '{endpoint}'"))?;

        let mut session = OmcSession {
            child,
            socket,
            _ctx: ctx,
            port_file,
            suffix,
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
        self.kill_process_group();
        let _ = self.child.kill();
        let _ = self.child.wait();
        self.kill_docker_container_by_suffix();
        let _ = std::fs::remove_file(&self.port_file);
    }

    /// SIGKILL the whole process group (omc plus any simulation executables it
    /// spawned). `omc` is the group leader (see `process_group(0)` at spawn), so
    /// the group id equals its pid. Uses `nix`'s safe `killpg` wrapper.
    fn kill_process_group(&self) {
        #[cfg(unix)]
        if let Ok(pid) = i32::try_from(self.child.id()) {
            let _ = nix::sys::signal::killpg(
                nix::unistd::Pid::from_raw(pid),
                nix::sys::signal::Signal::SIGKILL,
            );
        }
    }

    fn kill_docker_container_by_suffix(&self) {
        let Ok(output) = Command::new("docker")
            .args([
                "ps",
                "--no-trunc",
                "--filter",
                &format!("ancestor={}", docker_omc_image()),
                "--format",
                "{{.ID}} {{.Command}}",
            ])
            .output()
        else {
            return;
        };
        if !output.status.success() {
            return;
        }
        let stdout = String::from_utf8_lossy(&output.stdout);
        for line in stdout.lines() {
            if !line.contains(&self.suffix) {
                continue;
            }
            if let Some(container_id) = line.split_whitespace().next() {
                let _ = Command::new("docker")
                    .arg("kill")
                    .arg(container_id)
                    .stdout(Stdio::null())
                    .stderr(Stdio::null())
                    .status();
            }
        }
    }
}

impl Drop for OmcSession {
    fn drop(&mut self) {
        // Best-effort graceful quit, then ensure the process group is gone
        // (omc + any simulation executables it spawned).
        let _ = self.eval("quit()", Duration::from_millis(500));
        self.kill_process_group();
        let _ = self.child.kill();
        let _ = self.child.wait();
        self.kill_docker_container_by_suffix();
        let _ = std::fs::remove_file(&self.port_file);
    }
}

fn build_omc_interactive_command(work_dir: &Path, suffix: &str, omc_threads: usize) -> Command {
    if omc_command_is_docker_wrapper() {
        return build_docker_omc_interactive_command(work_dir, suffix, omc_threads);
    }
    let mut command = Command::new("omc");
    command
        .arg("--interactive=zmq")
        .arg(format!("-z={suffix}"))
        .arg("--locale=C");
    apply_omc_thread_env_to_native_command(&mut command, omc_threads);
    command
}

fn build_docker_omc_interactive_command(
    work_dir: &Path,
    suffix: &str,
    omc_threads: usize,
) -> Command {
    let threads = omc_threads.max(1).to_string();
    let home = env::var_os("HOME").unwrap_or_else(|| OsString::from("/tmp"));
    let user = docker_user_arg();
    let image = docker_omc_image();
    let mut command = Command::new("docker");
    command
        .arg("run")
        .arg("--rm")
        .arg("-i")
        .arg("--network")
        .arg("host")
        .arg("-v")
        .arg(format!(
            "{}:{}",
            home.to_string_lossy(),
            home.to_string_lossy()
        ))
        .arg("-e")
        .arg(format!("HOME={}", home.to_string_lossy()))
        .arg("-e")
        .arg(format!("TMPDIR={}", work_dir.display()))
        .arg("-e")
        .arg(format!("OMP_NUM_THREADS={threads}"))
        .arg("-e")
        .arg(format!("OPENBLAS_NUM_THREADS={threads}"))
        .arg("-e")
        .arg(format!("MKL_NUM_THREADS={threads}"))
        .arg("-e")
        .arg(format!("NUMEXPR_NUM_THREADS={threads}"))
        .arg("-w")
        .arg(work_dir)
        .arg("--user")
        .arg(user)
        .arg(image)
        .arg("omc")
        .arg("--interactive=zmq")
        .arg(format!("-z={suffix}"))
        .arg("--locale=C")
        .arg(format!("--numProcs={threads}"));
    command
}

fn apply_omc_thread_env_to_native_command(command: &mut Command, omc_threads: usize) {
    let threads = omc_threads.max(1).to_string();
    command.arg(format!("--numProcs={threads}"));
    command.env("OMP_NUM_THREADS", &threads);
    command.env("OPENBLAS_NUM_THREADS", &threads);
    command.env("MKL_NUM_THREADS", &threads);
    command.env("NUMEXPR_NUM_THREADS", &threads);
}

fn docker_user_arg() -> String {
    format!(
        "{}:{}",
        command_stdout("id", &["-u"]),
        command_stdout("id", &["-g"])
    )
}

fn command_stdout(program: &str, args: &[&str]) -> String {
    Command::new(program)
        .args(args)
        .output()
        .ok()
        .filter(|output| output.status.success())
        .map(|output| String::from_utf8_lossy(&output.stdout).trim().to_string())
        .filter(|value| !value.is_empty())
        .unwrap_or_else(|| "0".to_string())
}

fn omc_command_is_docker_wrapper() -> bool {
    let Some(path) = find_executable_on_path("omc") else {
        return false;
    };
    let Ok(text) = std::fs::read_to_string(path) else {
        return false;
    };
    text.contains("docker run") && text.contains("openmodelica/openmodelica:")
}

fn docker_omc_image() -> String {
    if let Ok(image) = env::var("RUMOCA_OMC_DOCKER_IMAGE")
        && !image.trim().is_empty()
    {
        return image.trim().to_string();
    }
    find_executable_on_path("omc")
        .and_then(|path| std::fs::read_to_string(path).ok())
        .and_then(|text| {
            text.split_whitespace()
                .find(|word| word.starts_with("openmodelica/openmodelica:"))
                .map(|word| word.trim_end_matches('\\').to_string())
        })
        .unwrap_or_else(|| FALLBACK_DOCKER_OMC_IMAGE.to_string())
}

fn find_executable_on_path(name: &str) -> Option<PathBuf> {
    let path_var = env::var_os("PATH")?;
    env::split_paths(&path_var)
        .map(|dir| dir.join(name))
        .find(|path| path.is_file())
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
        if let Some(path) = find_port_file(work_dir, &needle) {
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
    fn unquote_strips_and_unescapes() {
        assert_eq!(unquote_omc_string("\"hello\\nworld\""), "hello\nworld");
        assert_eq!(unquote_omc_string("\"\""), "");
        assert_eq!(unquote_omc_string("bare"), "bare");
    }
}
