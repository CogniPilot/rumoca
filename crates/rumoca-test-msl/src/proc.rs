//! Small process helpers shared by the MSL tooling.

use std::process::{Command, Stdio};

use anyhow::{Context, Result, bail};

/// Platform executable name (`foo` on unix, `foo.exe` on windows).
pub fn exe_name(base: &str) -> String {
    if cfg!(windows) {
        format!("{base}.exe")
    } else {
        base.to_string()
    }
}

/// Run a command to completion, erroring if it exits non-zero.
pub fn run_status(mut command: Command) -> Result<()> {
    let rendered = format!("{command:?}");
    let status = command
        .status()
        .with_context(|| format!("failed to run command: {rendered}"))?;
    if !status.success() {
        bail!("command failed (status={status}): {rendered}");
    }
    Ok(())
}

/// Run a command while capturing output and include it in any failure.
pub fn run_status_quiet(mut command: Command) -> Result<()> {
    let rendered = format!("{command:?}");
    let output = command
        .output()
        .with_context(|| format!("failed to run command: {rendered}"))?;
    if !output.status.success() {
        bail!(
            "command failed (status={}): {}\nstdout:\n{}\nstderr:\n{}",
            output.status,
            rendered,
            String::from_utf8_lossy(&output.stdout),
            String::from_utf8_lossy(&output.stderr)
        );
    }
    Ok(())
}

/// Capture a command's UTF-8-lossy stdout and fail on a non-zero status.
pub fn run_capture(mut command: Command) -> Result<String> {
    let rendered = format!("{command:?}");
    let output = command
        .output()
        .with_context(|| format!("failed to run command: {rendered}"))?;
    if !output.status.success() {
        bail!(
            "command failed (status={}): {}\n{}",
            output.status,
            rendered,
            String::from_utf8_lossy(&output.stderr)
        );
    }
    Ok(String::from_utf8_lossy(&output.stdout).into_owned())
}

/// True if `program --version` runs successfully (a cheap availability probe).
pub fn command_exists(program: &str) -> bool {
    Command::new(program)
        .arg("--version")
        .stdout(Stdio::null())
        .stderr(Stdio::null())
        .status()
        .is_ok_and(|status| status.success())
}
