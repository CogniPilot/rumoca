//! Child-process reporting shared by emission, compilation, and execution.

use super::typed_path::{CargoExecutable, ExecutableRole, RolePath, RustcExecutable};
use anyhow::{Context, Result, anyhow, ensure};
use serde::Serialize;
use sha2::{Digest, Sha256};
use std::ffi::OsStr;
use std::fmt;
use std::fmt::Write as _;
use std::io::Read;
use std::path::Path;
use std::process::{Command, Output, Stdio};
use std::sync::mpsc::{self, Receiver, RecvTimeoutError};
use std::time::{Duration, Instant};

const MAX_CAPTURE_BYTES: usize = 8 * 1024 * 1024;
const READER_DRAIN_TIMEOUT: Duration = Duration::from_secs(1);
pub(super) const HERMETIC_ENVIRONMENT_ID: &str = "inherit-none";
pub(super) const HERMETIC_WORKING_DIRECTORY_ID: &str = "filesystem-root";
pub(super) const COMPILER_RUNTIME_ENVIRONMENT_ID: &str =
    "inherit-none-with-captured-optional-ld-library-path";
pub(super) const COMPILER_FEATURES: &str = "fmu-packaging,scheduled-sim,scenario-config,input-keyboard,transport-udp,transport-zenoh,viewer-web,process-control";
pub(super) const COMPILER_HOST_TARGET: &str = "x86_64-unknown-linux-gnu";
const HERMETIC_WORKING_DIRECTORY: &str = "/";

#[derive(Clone, Copy)]
pub(super) enum Limit {
    Probe,
    WorkspaceBuild,
    Generate,
    CrossBuild,
    Guest,
}

pub(super) struct HermeticCommand(Command);

pub(super) struct CompilerBuildInputs<'a> {
    pub(super) cargo: &'a RolePath<CargoExecutable>,
    pub(super) rustc: &'a RolePath<RustcExecutable>,
    pub(super) source_root: &'a Path,
    pub(super) target_dir: &'a Path,
    pub(super) temporary_dir: &'a Path,
    pub(super) cargo_home: &'a Path,
    pub(super) vendor: &'a Path,
    pub(super) nix_path: &'a OsStr,
}

pub(super) struct CompilerBuildCommand(Command);

pub(super) struct CompilerMetadataInputs<'a> {
    pub(super) cargo: &'a RolePath<CargoExecutable>,
    pub(super) rustc: &'a RolePath<RustcExecutable>,
    pub(super) source_root: &'a Path,
    pub(super) temporary_dir: &'a Path,
    pub(super) cargo_home: &'a Path,
    pub(super) nix_path: &'a OsStr,
    pub(super) filter_platform: Option<&'a str>,
}

pub(super) struct CompilerMetadataCommand(Command);

impl CompilerBuildCommand {
    pub(super) fn rumoca(inputs: CompilerBuildInputs<'_>) -> Result<Self> {
        for (label, path) in [
            ("Cargo", inputs.cargo.as_path()),
            ("rustc", inputs.rustc.as_path()),
            ("compiler source", inputs.source_root),
            ("compiler target", inputs.target_dir),
            ("compiler temporary", inputs.temporary_dir),
            ("Cargo home", inputs.cargo_home),
            ("compiler vendor", inputs.vendor),
        ] {
            ensure!(path.is_absolute(), "{label} path must be absolute");
        }
        let mut command = Command::new(inputs.cargo.as_path());
        command
            .env_clear()
            .current_dir(inputs.source_root)
            .env("PATH", inputs.nix_path)
            .env("RUSTC", inputs.rustc.as_path())
            .env("CARGO_HOME", inputs.cargo_home)
            .env("TMPDIR", inputs.temporary_dir)
            .env("CARGO_BUILD_JOBS", "4")
            .env("RAYON_NUM_THREADS", "4")
            .env("CARGO_INCREMENTAL", "0")
            .env("CARGO_NET_OFFLINE", "true")
            .args(["build", "--frozen", "--offline"])
            .arg("--manifest-path")
            .arg(inputs.source_root.join("Cargo.toml"))
            .args(["--package", "rumoca", "--bin", "rumoca"])
            .args(["--no-default-features", "--features", COMPILER_FEATURES])
            .args(["--target", COMPILER_HOST_TARGET])
            .args(["--config", "net.offline=true"])
            .args([
                "--config",
                "source.crates-io.replace-with=\"vendored-sources\"",
            ])
            .arg("--config")
            .arg(format!(
                "source.vendored-sources.directory=\"{}\"",
                inputs.vendor.display()
            ))
            .arg("--target-dir")
            .arg(inputs.target_dir);
        Ok(Self(command))
    }

    #[cfg(test)]
    pub(super) fn arguments(&self) -> impl Iterator<Item = &OsStr> {
        self.0.get_args()
    }

    #[cfg(test)]
    pub(super) fn environments(&self) -> impl Iterator<Item = (&OsStr, Option<&OsStr>)> {
        self.0.get_envs()
    }

    #[cfg(test)]
    pub(super) fn current_directory(&self) -> Option<&Path> {
        self.0.get_current_dir()
    }
}

impl CompilerMetadataCommand {
    pub(super) fn rumoca(inputs: CompilerMetadataInputs<'_>) -> Result<Self> {
        for (label, path) in [
            ("Cargo", inputs.cargo.as_path()),
            ("rustc", inputs.rustc.as_path()),
            ("compiler source", inputs.source_root),
            ("compiler metadata temporary", inputs.temporary_dir),
            ("Cargo home", inputs.cargo_home),
        ] {
            ensure!(path.is_absolute(), "{label} path must be absolute");
        }
        let features = COMPILER_FEATURES
            .split(',')
            .map(|feature| format!("rumoca/{feature}"))
            .collect::<Vec<_>>()
            .join(",");
        let mut command = Command::new(inputs.cargo.as_path());
        command
            .env_clear()
            .current_dir(inputs.source_root)
            .env("PATH", inputs.nix_path)
            .env("RUSTC", inputs.rustc.as_path())
            .env("CARGO_HOME", inputs.cargo_home)
            .env("TMPDIR", inputs.temporary_dir)
            .env("CARGO_NET_OFFLINE", "true")
            .args(["metadata", "--frozen", "--offline"])
            .args(["--format-version", "1", "--manifest-path"])
            .arg(inputs.source_root.join("Cargo.toml"))
            .arg("--no-default-features")
            .args(["--features", features.as_str()]);
        if let Some(platform) = inputs.filter_platform {
            command.args(["--filter-platform", platform]);
        }
        Ok(Self(command))
    }

    #[cfg(test)]
    pub(super) fn arguments(&self) -> impl Iterator<Item = &OsStr> {
        self.0.get_args()
    }
}

pub(super) struct CompilerRuntimeEnvironment {
    library_path: Option<std::ffi::OsString>,
}

impl CompilerRuntimeEnvironment {
    #[cfg(test)]
    pub(super) fn capture() -> Self {
        Self::authenticated(std::env::var_os("LD_LIBRARY_PATH"))
    }

    pub(super) fn authenticated(library_path: Option<std::ffi::OsString>) -> Self {
        Self { library_path }
    }

    pub(super) fn sha256(&self) -> String {
        let mut digest = Sha256::new();
        digest.update(b"LD_LIBRARY_PATH\0");
        if let Some(value) = &self.library_path {
            digest.update(value.as_encoded_bytes());
        }
        format!("{:x}", digest.finalize())
    }
}

impl HermeticCommand {
    pub(super) fn new<R: ExecutableRole>(program: &RolePath<R>) -> Self {
        let mut command = Command::new(program.as_path());
        command.env_clear().current_dir(HERMETIC_WORKING_DIRECTORY);
        Self(command)
    }

    pub(super) fn arg(&mut self, argument: impl AsRef<OsStr>) -> &mut Self {
        self.0.arg(argument);
        self
    }

    pub(super) fn args<I, S>(&mut self, arguments: I) -> &mut Self
    where
        I: IntoIterator<Item = S>,
        S: AsRef<OsStr>,
    {
        self.0.args(arguments);
        self
    }

    pub(super) fn compiler_runtime_environment(
        &mut self,
        environment: &CompilerRuntimeEnvironment,
    ) -> &mut Self {
        if let Some(value) = &environment.library_path {
            self.0.env("LD_LIBRARY_PATH", value);
        }
        self
    }

    pub(super) fn git_control_environment(&mut self) -> &mut Self {
        self.0
            .env("GIT_NO_REPLACE_OBJECTS", "1")
            .env("GIT_CONFIG_NOSYSTEM", "1")
            .env("GIT_CONFIG_GLOBAL", "/dev/null")
            .env("GIT_CONFIG_SYSTEM", "/dev/null")
            .env("GIT_ATTR_NOSYSTEM", "1");
        self
    }

    pub(super) fn from_nix_store_program(program: &Path) -> Result<Self> {
        ensure!(
            program.is_absolute() && program.starts_with("/nix/store"),
            "bootstrap program invocation is outside the immutable Nix store"
        );
        let canonical = program.canonicalize().with_context(|| {
            format!("failed to resolve Nix-store program {}", program.display())
        })?;
        ensure!(
            canonical.starts_with("/nix/store") && canonical.is_file(),
            "bootstrap program is outside the immutable Nix store"
        );
        // Preserve a Nix multicall program's reviewed invocation name while
        // authenticating that it resolves to an immutable store executable.
        let mut command = Command::new(program);
        command.env_clear().current_dir(HERMETIC_WORKING_DIRECTORY);
        Ok(Self(command))
    }

    #[cfg(test)]
    pub(super) fn get_args(&self) -> impl Iterator<Item = &OsStr> {
        self.0.get_args()
    }

    #[cfg(test)]
    pub(super) fn from_absolute_path(program: &Path) -> Self {
        assert!(program.is_absolute());
        let mut command = Command::new(program);
        command.env_clear().current_dir(HERMETIC_WORKING_DIRECTORY);
        Self(command)
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Serialize)]
pub(super) struct CommandReceipt {
    display_only: String,
    program_encoded_hex: String,
    arguments_encoded_hex: Vec<String>,
    current_directory_encoded_hex: Option<String>,
    environment_inheritance: EnvironmentInheritance,
    environment: Vec<EnvironmentMutation>,
}

#[derive(Debug)]
struct AttemptedCommandFailure {
    receipts: Vec<CommandReceipt>,
    message: String,
}

impl fmt::Display for AttemptedCommandFailure {
    fn fmt(&self, formatter: &mut fmt::Formatter<'_>) -> fmt::Result {
        formatter.write_str(&self.message)
    }
}

impl std::error::Error for AttemptedCommandFailure {}

pub(super) fn attempted_receipts(error: &anyhow::Error) -> &[CommandReceipt] {
    error
        .downcast_ref::<AttemptedCommandFailure>()
        .map_or(&[], |failure| failure.receipts.as_slice())
}

pub(super) fn attach_attempted_receipt(
    error: anyhow::Error,
    receipt: CommandReceipt,
) -> anyhow::Error {
    attach_prior_receipts(error, vec![receipt])
}

pub(super) fn attach_prior_receipts(
    error: anyhow::Error,
    mut prior: Vec<CommandReceipt>,
) -> anyhow::Error {
    prior.extend_from_slice(attempted_receipts(&error));
    if prior.is_empty() {
        return error;
    }
    AttemptedCommandFailure {
        receipts: prior,
        message: format!("{error:#}"),
    }
    .into()
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Serialize)]
#[serde(rename_all = "kebab-case")]
enum EnvironmentInheritance {
    #[cfg(test)]
    InheritParent,
    Clear,
}

#[derive(Clone, Debug, PartialEq, Eq, Serialize)]
struct EnvironmentMutation {
    name_encoded_hex: String,
    value_encoded_hex: Option<String>,
}

impl CommandReceipt {
    fn capture(command: &Command, environment_inheritance: EnvironmentInheritance) -> Self {
        Self {
            display_only: describe(command),
            program_encoded_hex: encoded_hex(command.get_program()),
            arguments_encoded_hex: command.get_args().map(encoded_hex).collect(),
            current_directory_encoded_hex: command
                .get_current_dir()
                .map(|directory| encoded_hex(directory.as_os_str())),
            environment_inheritance,
            environment: command
                .get_envs()
                .map(|(name, value)| EnvironmentMutation {
                    name_encoded_hex: encoded_hex(name),
                    value_encoded_hex: value.map(encoded_hex),
                })
                .collect(),
        }
    }

    #[cfg(test)]
    pub(super) fn display_only(&self) -> &str {
        &self.display_only
    }

    pub(super) fn sha256(&self) -> String {
        let bytes = serde_json::to_vec(self).expect("command receipt serialization cannot fail");
        format!("{:x}", Sha256::digest(bytes))
    }

    #[cfg(test)]
    pub(super) fn is_hermetic(&self) -> bool {
        self.environment_inheritance == EnvironmentInheritance::Clear
            && self.current_directory_encoded_hex.as_deref() == Some("2f")
    }

    #[cfg(test)]
    pub(super) fn has_environment_name(&self, name: &str) -> bool {
        let name = encoded_hex(OsStr::new(name));
        self.environment
            .iter()
            .any(|mutation| mutation.name_encoded_hex == name)
    }
}

#[cfg(test)]
pub(super) fn test_receipt(label: &str) -> CommandReceipt {
    CommandReceipt::capture(&Command::new(label), EnvironmentInheritance::InheritParent)
}

impl fmt::Display for CommandReceipt {
    fn fmt(&self, formatter: &mut fmt::Formatter<'_>) -> fmt::Result {
        formatter.write_str(&self.display_only)
    }
}

fn encoded_hex(value: &OsStr) -> String {
    let bytes = value.as_encoded_bytes();
    let mut encoded = String::with_capacity(bytes.len() * 2);
    for byte in bytes {
        write!(encoded, "{byte:02x}").expect("writing to String cannot fail");
    }
    encoded
}

impl Limit {
    const fn duration(self) -> Duration {
        match self {
            Self::Probe => Duration::from_secs(10),
            Self::WorkspaceBuild => Duration::from_secs(20 * 60),
            Self::Generate | Self::CrossBuild => Duration::from_secs(120),
            Self::Guest => Duration::from_secs(2),
        }
    }
}

pub(super) fn describe(command: &Command) -> String {
    let mut parts = vec![command.get_program().to_string_lossy().into_owned()];
    parts.extend(
        command
            .get_args()
            .map(|argument| argument.to_string_lossy().into_owned()),
    );
    parts.join(" ")
}

pub(super) fn output(
    command: &mut HermeticCommand,
    limit: Limit,
) -> Result<(Output, CommandReceipt)> {
    output_with_timeout_and_environment(
        &mut command.0,
        limit.duration(),
        EnvironmentInheritance::Clear,
    )
}

#[cfg(test)]
fn output_with_timeout(
    command: &mut Command,
    timeout: Duration,
) -> Result<(Output, CommandReceipt)> {
    output_with_timeout_and_environment(command, timeout, EnvironmentInheritance::InheritParent)
}

fn output_with_timeout_and_environment(
    command: &mut Command,
    timeout: Duration,
    environment_inheritance: EnvironmentInheritance,
) -> Result<(Output, CommandReceipt)> {
    let receipt = CommandReceipt::capture(command, environment_inheritance);
    let rendered = receipt.to_string();
    configure_process_group(command);
    command
        .stdin(Stdio::null())
        .stdout(Stdio::piped())
        .stderr(Stdio::piped());
    let mut child = command.spawn().map_err(|error| {
        attach_attempted_receipt(
            anyhow!(error).context(format!("failed to run `{rendered}`")),
            receipt.clone(),
        )
    })?;
    let process_group = child.id();
    let stdout = child.stdout.take().context("child stdout was not piped")?;
    let stderr = child.stderr.take().context("child stderr was not piped")?;
    let stdout_reader = spawn_reader(stdout);
    let stderr_reader = spawn_reader(stderr);
    let started = Instant::now();
    let status = loop {
        match child.try_wait() {
            Ok(Some(status)) => break status,
            Ok(None) => {}
            Err(error) => {
                terminate_process_group(process_group, &mut child);
                return Err(attach_attempted_receipt(
                    anyhow!(error).context(format!("failed to wait for `{rendered}`")),
                    receipt,
                ));
            }
        }
        if started.elapsed() >= timeout {
            terminate_process_group(process_group, &mut child);
            return Err(attach_attempted_receipt(
                anyhow!(
                    "command timed out after {:.3}s: {rendered}",
                    timeout.as_secs_f64()
                ),
                receipt,
            ));
        }
        std::thread::sleep(Duration::from_millis(10));
    };
    let stdout = receive_output(stdout_reader, "stdout", &receipt, process_group, &mut child)?;
    let stderr = receive_output(stderr_reader, "stderr", &receipt, process_group, &mut child)?;
    Ok((
        Output {
            status,
            stdout,
            stderr,
        },
        receipt,
    ))
}

fn spawn_reader(stream: impl Read + Send + 'static) -> Receiver<std::io::Result<Vec<u8>>> {
    let (sender, receiver) = mpsc::channel();
    std::thread::spawn(move || {
        let _receiver_closed = sender.send(read_all(stream));
    });
    receiver
}

fn read_all(mut stream: impl Read) -> std::io::Result<Vec<u8>> {
    let mut bytes = Vec::new();
    let mut chunk = [0_u8; 8192];
    loop {
        let count = stream.read(&mut chunk)?;
        if count == 0 {
            break;
        }
        if bytes.len().saturating_add(count) > MAX_CAPTURE_BYTES {
            return Err(std::io::Error::other(format!(
                "child output exceeded {MAX_CAPTURE_BYTES} bytes"
            )));
        }
        bytes.extend_from_slice(&chunk[..count]);
    }
    Ok(bytes)
}

fn receive_output(
    receiver: Receiver<std::io::Result<Vec<u8>>>,
    stream: &str,
    receipt: &CommandReceipt,
    process_group: u32,
    child: &mut std::process::Child,
) -> Result<Vec<u8>> {
    let rendered = receipt.to_string();
    match receiver.recv_timeout(READER_DRAIN_TIMEOUT) {
        Ok(Ok(bytes)) => Ok(bytes),
        Ok(Err(error)) => {
            terminate_process_group(process_group, child);
            Err(attach_attempted_receipt(
                anyhow!(error).context(format!("failed to capture {stream} for `{rendered}`")),
                receipt.clone(),
            ))
        }
        Err(RecvTimeoutError::Timeout) => {
            terminate_process_group(process_group, child);
            Err(attach_attempted_receipt(
                anyhow!(
                    "{stream} did not close within {:.3}s after `{rendered}` exited; descendant process retained the pipe",
                    READER_DRAIN_TIMEOUT.as_secs_f64()
                ),
                receipt.clone(),
            ))
        }
        Err(RecvTimeoutError::Disconnected) => {
            terminate_process_group(process_group, child);
            Err(attach_attempted_receipt(
                anyhow!("{stream} reader disconnected for `{rendered}`"),
                receipt.clone(),
            ))
        }
    }
}

#[cfg(unix)]
fn configure_process_group(command: &mut Command) {
    std::os::unix::process::CommandExt::process_group(command, 0);
}

#[cfg(not(unix))]
fn configure_process_group(_command: &mut Command) {}

fn terminate_process_group(process_group: u32, child: &mut std::process::Child) {
    #[cfg(unix)]
    if let Ok(process_group) = i32::try_from(process_group) {
        let _already_exited = nix::sys::signal::killpg(
            nix::unistd::Pid::from_raw(process_group),
            nix::sys::signal::Signal::SIGKILL,
        );
    }
    let _already_exited = child.kill();
    let _reap_error = child.wait();
}

pub(super) fn require_success(
    command: &mut HermeticCommand,
    purpose: &str,
    limit: Limit,
) -> Result<CommandReceipt> {
    let (output, receipt) = output(command, limit)?;
    if !output.status.success() {
        return Err(attach_attempted_receipt(
            anyhow!(
                "{purpose} failed ({})\n    command: {receipt}\n{}",
                output.status,
                tail(&combined(&output))
            ),
            receipt,
        ));
    }
    Ok(receipt)
}

pub(super) fn require_workspace_success(
    command: &mut CompilerBuildCommand,
    purpose: &str,
    limit: Limit,
) -> Result<CommandReceipt> {
    let (output, receipt) = output_with_timeout_and_environment(
        &mut command.0,
        limit.duration(),
        EnvironmentInheritance::Clear,
    )?;
    if !output.status.success() {
        return Err(attach_attempted_receipt(
            anyhow!(
                "{purpose} failed ({})\n    command: {receipt}\n{}",
                output.status,
                tail(&combined(&output))
            ),
            receipt,
        ));
    }
    Ok(receipt)
}

pub(super) fn compiler_metadata_output(
    command: &mut CompilerMetadataCommand,
) -> Result<(Output, CommandReceipt)> {
    output_with_timeout_and_environment(
        &mut command.0,
        Limit::WorkspaceBuild.duration(),
        EnvironmentInheritance::Clear,
    )
}

pub(super) fn combined(output: &Output) -> String {
    let mut text = String::from_utf8_lossy(&output.stderr).into_owned();
    text.push_str(&String::from_utf8_lossy(&output.stdout));
    text
}

pub(super) fn tail(text: &str) -> String {
    let lines: Vec<&str> = text
        .lines()
        .filter(|line| !line.trim().is_empty())
        .collect();
    let start = lines.len().saturating_sub(16);
    lines[start..]
        .iter()
        .map(|line| format!("      {line}"))
        .collect::<Vec<_>>()
        .join("\n")
}

#[cfg(all(test, unix))]
mod tests {
    use super::{
        EnvironmentInheritance, HermeticCommand, Limit, attempted_receipts, output,
        output_with_timeout,
    };
    use std::fs;
    use std::path::Path;
    use std::process::{Command, Stdio};
    use std::time::{Duration, Instant};

    #[test]
    fn nonterminating_child_is_killed_and_reported_within_bound() {
        let started = Instant::now();
        let error = output_with_timeout(
            Command::new("/bin/sh").arg("-c").arg("while :; do :; done"),
            Duration::from_millis(50),
        )
        .unwrap_err();
        assert!(format!("{error:#}").contains("timed out"));
        assert_eq!(attempted_receipts(&error).len(), 1);
        assert!(started.elapsed() < Duration::from_secs(2));
    }

    #[test]
    fn spawn_failure_retains_the_exact_attempted_receipt() {
        let error = output_with_timeout(
            &mut Command::new("/definitely/missing/embedded-gate-command"),
            Duration::from_millis(50),
        )
        .unwrap_err();
        let receipts = attempted_receipts(&error);
        assert_eq!(receipts.len(), 1);
        assert_eq!(
            receipts[0].display_only(),
            "/definitely/missing/embedded-gate-command"
        );
    }

    #[test]
    fn runner_closes_stdin_even_when_the_caller_requests_a_pipe() {
        let mut command = Command::new("/bin/sh");
        command
            .arg("-c")
            .arg("if read value; then exit 1; fi")
            .stdin(Stdio::piped());
        let (output, _) = output_with_timeout(&mut command, Duration::from_secs(1)).unwrap();
        assert!(output.status.success());
    }

    #[test]
    fn planned_runner_clears_ambient_environment_and_binds_root_working_directory() {
        let mut command = HermeticCommand::from_absolute_path(Path::new("/bin/sh"));
        command
            .arg("-c")
            .arg("test -z \"${CPATH+x}\" && test \"${PWD:-/}\" = /");
        let (result, receipt) = output(&mut command, Limit::Probe).unwrap();
        assert!(result.status.success());
        assert_eq!(
            receipt.environment_inheritance,
            EnvironmentInheritance::Clear
        );
        assert!(receipt.environment.is_empty());
        assert_eq!(receipt.current_directory_encoded_hex.as_deref(), Some("2f"));
    }

    #[test]
    fn authenticated_prlimit_stops_a_live_writer_at_exact_byte_ceiling() {
        use crate::verify_cmd::embedded_head_to_head::qemu::MAX_TRACE_BYTES;
        use crate::verify_cmd::embedded_head_to_head::typed_path::{
            AuthenticatedExecutable, PrlimitExecutable, RolePath,
        };
        use sha2::{Digest, Sha256};

        let prlimit = std::env::split_paths(&std::env::var_os("PATH").unwrap())
            .map(|directory| directory.join("prlimit"))
            .find(|candidate| candidate.is_file())
            .unwrap()
            .canonicalize()
            .unwrap();
        let sha256 = format!("{:x}", Sha256::digest(fs::read(&prlimit).unwrap()));
        let prlimit = AuthenticatedExecutable::<PrlimitExecutable>::checked(
            RolePath::checked(prlimit).unwrap(),
            &sha256,
        )
        .unwrap();
        let temporary = tempfile::tempdir().unwrap();
        let output_file = temporary.path().join("bounded.trace");
        let mut command = HermeticCommand::new(prlimit.for_execution().unwrap());
        command
            .arg(format!("--fsize={MAX_TRACE_BYTES}:{MAX_TRACE_BYTES}"))
            .args(["--", "/bin/sh", "-c"])
            .arg("while :; do printf 0123456789abcdef; done > \"$1\"")
            .arg("sh")
            .arg(&output_file);
        let (result, receipt) = output(&mut command, Limit::Probe).unwrap();

        assert!(!result.status.success());
        assert_eq!(fs::metadata(&output_file).unwrap().len(), MAX_TRACE_BYTES);
        assert!(receipt.is_hermetic());
        assert!(receipt.display_only().contains(&format!(
            "--fsize={MAX_TRACE_BYTES}:{MAX_TRACE_BYTES} -- /bin/sh"
        )));
    }

    #[test]
    fn stdout_and_stderr_flood_cannot_deadlock_the_timeout() {
        let started = Instant::now();
        let error = output_with_timeout(
            Command::new("/bin/sh")
                .arg("-c")
                .arg("while :; do printf x; printf y >&2; done"),
            Duration::from_millis(50),
        )
        .unwrap_err();
        let message = format!("{error:#}");
        assert!(message.contains("timed out") || message.contains("exceeded"));
        assert!(started.elapsed() < Duration::from_secs(2));
    }

    #[test]
    fn exited_parent_with_pipe_holding_descendant_fails_within_bound() {
        let temporary = tempfile::tempdir().unwrap();
        let pid_file = temporary.path().join("descendant.pid");
        let started = Instant::now();
        let error = output_with_timeout(
            Command::new("/bin/sh")
                .arg("-c")
                .arg("sleep 30 & echo $! > \"$PID_FILE\"")
                .env("PID_FILE", &pid_file),
            Duration::from_secs(5),
        )
        .unwrap_err();
        assert!(format!("{error:#}").contains("descendant process retained the pipe"));
        assert!(started.elapsed() < Duration::from_secs(3));
        assert_pid_gone(&pid_file);
    }

    #[test]
    fn timed_out_descendant_process_group_is_gone() {
        let temporary = tempfile::tempdir().unwrap();
        let pid_file = temporary.path().join("descendant.pid");
        let started = Instant::now();
        let error = output_with_timeout(
            Command::new("/bin/sh")
                .arg("-c")
                .arg("sleep 30 & echo $! > \"$PID_FILE\"; wait")
                .env("PID_FILE", &pid_file),
            Duration::from_millis(100),
        )
        .unwrap_err();
        assert!(format!("{error:#}").contains("timed out"));
        assert!(started.elapsed() < Duration::from_secs(2));
        assert_pid_gone(&pid_file);
    }

    fn assert_pid_gone(pid_file: &Path) {
        let pid = fs::read_to_string(pid_file).unwrap();
        let process = Path::new("/proc").join(pid.trim());
        let deadline = Instant::now() + Duration::from_secs(1);
        while process.exists() && Instant::now() < deadline {
            std::thread::sleep(Duration::from_millis(10));
        }
        assert!(!process.exists(), "descendant {} survived", pid.trim());
    }
}
