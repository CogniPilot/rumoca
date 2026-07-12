//! Small process helpers shared by the MSL tooling (copied from `xtask`'s own
//! copies; `xtask` keeps its versions for the light commands that stay there).

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

/// True if `program --version` runs successfully (a cheap availability probe).
pub fn command_exists(program: &str) -> bool {
    Command::new(program)
        .arg("--version")
        .stdout(Stdio::null())
        .stderr(Stdio::null())
        .status()
        .is_ok_and(|status| status.success())
}
