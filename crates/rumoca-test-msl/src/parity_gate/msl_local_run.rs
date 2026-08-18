use anyhow::{Context, Result, ensure};
use serde_json::Value;
use std::io::{BufRead, BufReader};
use std::path::{Path, PathBuf};
use std::process::{Command, Stdio};

use super::MSL_FULL_TEST_FEATURE;

const MODEL_WORKER: &str = "rumoca-worker";
const SIM_WORKER: &str = "rumoca-sim-worker";
const MSL_TOOLS: &str = "rumoca-msl-tools";
const MSL_TESTS: &str = "msl_tests";
pub(super) const MSL_BUILD_PROFILE: &str = "msl-fast";

#[derive(Debug)]
pub(super) struct MslRuntimeArtifacts {
    test_binary: PathBuf,
    model_worker: PathBuf,
    sim_worker: PathBuf,
    msl_tools: PathBuf,
}

impl MslRuntimeArtifacts {
    pub(super) fn binaries(&self) -> MslTestBinaries<'_> {
        MslTestBinaries {
            test_binary: &self.test_binary,
            model_worker: Some(&self.model_worker),
            sim_worker: Some(&self.sim_worker),
            msl_tools: Some(&self.msl_tools),
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub(super) struct MslTestBinaries<'a> {
    pub(super) test_binary: &'a Path,
    pub(super) model_worker: Option<&'a Path>,
    pub(super) sim_worker: Option<&'a Path>,
    pub(super) msl_tools: Option<&'a Path>,
}

pub(super) fn optimized_msl_artifact_build(root: &Path) -> Command {
    let mut command = Command::new("cargo");
    command
        .arg("build")
        .arg("--verbose")
        .arg("--profile")
        .arg(MSL_BUILD_PROFILE)
        .arg("--package")
        .arg(MODEL_WORKER)
        .arg("--package")
        .arg("rumoca-test-msl")
        .arg("--features")
        .arg(format!("rumoca-test-msl/{MSL_FULL_TEST_FEATURE}"))
        .arg("--bin")
        .arg(MODEL_WORKER)
        .arg("--bin")
        .arg(SIM_WORKER)
        .arg("--bin")
        .arg(MSL_TOOLS)
        .arg("--test")
        .arg(MSL_TESTS)
        .arg("--message-format")
        .arg("json-render-diagnostics")
        .current_dir(root);
    command
}

pub(super) fn run_optimized_msl_artifact_build(
    mut command: Command,
) -> Result<MslRuntimeArtifacts> {
    let mut child = command
        .stdout(Stdio::piped())
        .stderr(Stdio::inherit())
        .spawn()
        .context("failed to start the optimized MSL artifact build")?;
    let stdout = child
        .stdout
        .take()
        .context("optimized MSL artifact build stdout was not captured")?;
    let artifacts = parse_cargo_artifacts(BufReader::new(stdout), true);
    let status = child
        .wait()
        .context("failed to wait for the optimized MSL artifact build")?;
    ensure!(
        status.success(),
        "optimized MSL artifact build failed with status {status}"
    );
    artifacts
}

pub(super) fn msl_test_binary_command(
    root: &Path,
    binaries: MslTestBinaries<'_>,
    test_target: &str,
) -> Result<Command> {
    ensure_artifact(binaries.test_binary, MSL_TESTS)?;
    let mut command = Command::new(binaries.test_binary);
    command
        .arg(test_target)
        .arg("--exact")
        .arg("--nocapture")
        .env("RUST_BACKTRACE", "full")
        .current_dir(root);
    if let Some(worker) = binaries.model_worker {
        ensure_artifact(worker, MODEL_WORKER)?;
        command.env("CARGO_BIN_EXE_rumoca-worker", worker);
    }
    if let Some(worker) = binaries.sim_worker {
        ensure_artifact(worker, SIM_WORKER)?;
        command.env("CARGO_BIN_EXE_rumoca-sim-worker", worker);
        command.env("CARGO_BIN_EXE_rumoca_sim_worker", worker);
    }
    if let Some(tools) = binaries.msl_tools {
        ensure_artifact(tools, MSL_TOOLS)?;
        command.env("CARGO_BIN_EXE_rumoca-msl-tools", tools);
        command.env("CARGO_BIN_EXE_rumoca_msl_tools", tools);
    }
    Ok(command)
}

fn ensure_artifact(path: &Path, name: &str) -> Result<()> {
    ensure!(
        path.is_file(),
        "MSL runtime artifact {name} not found at {}",
        path.display()
    );
    Ok(())
}

#[derive(Default)]
struct CargoArtifacts {
    test_binary: Vec<PathBuf>,
    model_worker: Vec<PathBuf>,
    sim_worker: Vec<PathBuf>,
    msl_tools: Vec<PathBuf>,
}

fn parse_cargo_artifacts(
    reader: impl BufRead,
    render_diagnostics: bool,
) -> Result<MslRuntimeArtifacts> {
    let mut artifacts = CargoArtifacts::default();
    for line in reader.lines() {
        let line = line.context("failed to read Cargo JSON output")?;
        let message: Value = serde_json::from_str(&line)
            .with_context(|| format!("invalid Cargo JSON message: {line}"))?;
        render_cargo_diagnostic(&message, render_diagnostics);
        collect_cargo_artifact(&message, &mut artifacts);
    }
    Ok(MslRuntimeArtifacts {
        test_binary: unique_artifact(MSL_TESTS, artifacts.test_binary)?,
        model_worker: unique_artifact(MODEL_WORKER, artifacts.model_worker)?,
        sim_worker: unique_artifact(SIM_WORKER, artifacts.sim_worker)?,
        msl_tools: unique_artifact(MSL_TOOLS, artifacts.msl_tools)?,
    })
}

fn render_cargo_diagnostic(message: &Value, enabled: bool) {
    if !enabled || message.get("reason").and_then(Value::as_str) != Some("compiler-message") {
        return;
    }
    if let Some(rendered) = message.pointer("/message/rendered").and_then(Value::as_str) {
        eprint!("{rendered}");
    }
}

fn collect_cargo_artifact(message: &Value, artifacts: &mut CargoArtifacts) {
    if message.get("reason").and_then(Value::as_str) != Some("compiler-artifact") {
        return;
    }
    let Some(name) = message.pointer("/target/name").and_then(Value::as_str) else {
        return;
    };
    let Some(executable) = message.get("executable").and_then(Value::as_str) else {
        return;
    };
    let mut target_kind = message
        .pointer("/target/kind")
        .and_then(Value::as_array)
        .into_iter()
        .flatten()
        .filter_map(Value::as_str);
    let is_test = target_kind.clone().any(|kind| kind == "test");
    let is_binary = target_kind.any(|kind| kind == "bin");
    match (name, is_test, is_binary) {
        (MSL_TESTS, true, _) => artifacts.test_binary.push(executable.into()),
        (MODEL_WORKER, _, true) => artifacts.model_worker.push(executable.into()),
        (SIM_WORKER, _, true) => artifacts.sim_worker.push(executable.into()),
        (MSL_TOOLS, _, true) => artifacts.msl_tools.push(executable.into()),
        _ => {}
    }
}

fn unique_artifact(name: &str, mut paths: Vec<PathBuf>) -> Result<PathBuf> {
    paths.sort();
    paths.dedup();
    ensure!(
        paths.len() == 1,
        "Cargo reported {} executable artifacts for {name}; expected exactly one",
        paths.len()
    );
    Ok(paths.pop().expect("one artifact was established"))
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::ffi::OsStr;
    use std::io::Cursor;

    fn artifact(name: &str, kind: &str, executable: &str) -> String {
        serde_json::json!({
            "reason": "compiler-artifact",
            "target": { "name": name, "kind": [kind] },
            "executable": executable,
        })
        .to_string()
    }

    fn complete_artifact_json(test_executable: &str) -> String {
        [
            artifact(MODEL_WORKER, "bin", "/target/msl-fast/rumoca-worker"),
            artifact(SIM_WORKER, "bin", "/target/msl-fast/rumoca-sim-worker"),
            artifact(MSL_TOOLS, "bin", "/target/msl-fast/rumoca-msl-tools"),
            artifact(MSL_TESTS, "test", test_executable),
        ]
        .join("\n")
    }

    #[test]
    fn optimized_build_requests_one_complete_cargo_graph() {
        let root = PathBuf::from("/workspace");
        let command = optimized_msl_artifact_build(&root);
        let args = command.get_args().collect::<Vec<_>>();

        assert_eq!(
            args,
            [
                "build",
                "--verbose",
                "--profile",
                "msl-fast",
                "--package",
                "rumoca-worker",
                "--package",
                "rumoca-test-msl",
                "--features",
                "rumoca-test-msl/msl-full-test",
                "--bin",
                "rumoca-worker",
                "--bin",
                "rumoca-sim-worker",
                "--bin",
                "rumoca-msl-tools",
                "--test",
                "msl_tests",
                "--message-format",
                "json-render-diagnostics",
            ]
            .map(OsStr::new)
        );
        assert_eq!(command.get_current_dir(), Some(root.as_path()));
    }

    #[test]
    fn cargo_json_captures_exact_runtime_artifacts() {
        let artifacts = parse_cargo_artifacts(
            Cursor::new(complete_artifact_json("/target/release/deps/msl_tests-abc")),
            false,
        )
        .expect("complete artifact stream");

        assert_eq!(
            artifacts.test_binary,
            PathBuf::from("/target/release/deps/msl_tests-abc")
        );
    }

    #[test]
    fn cargo_json_rejects_missing_test_artifact() {
        let input = complete_artifact_json("/target/release/deps/msl_tests-abc")
            .lines()
            .filter(|line| !line.contains(r#""name":"msl_tests""#))
            .collect::<Vec<_>>()
            .join("\n");
        let error = parse_cargo_artifacts(Cursor::new(input), false).unwrap_err();

        assert!(
            error
                .to_string()
                .contains("0 executable artifacts for msl_tests")
        );
    }

    #[test]
    fn cargo_json_rejects_ambiguous_test_artifacts() {
        let input = format!(
            "{}\n{}",
            complete_artifact_json("/target/release/deps/msl_tests-abc"),
            artifact(MSL_TESTS, "test", "/target/release/deps/msl_tests-def")
        );
        let error = parse_cargo_artifacts(Cursor::new(input), false).unwrap_err();

        assert!(
            error
                .to_string()
                .contains("2 executable artifacts for msl_tests")
        );
    }

    #[test]
    fn direct_test_command_sets_runtime_paths_and_libtest_args() {
        let temp = tempfile::tempdir().expect("tempdir");
        let paths =
            [MSL_TESTS, MODEL_WORKER, SIM_WORKER, MSL_TOOLS].map(|name| temp.path().join(name));
        for path in &paths {
            std::fs::write(path, "").expect("create artifact");
        }
        let binaries = MslTestBinaries {
            test_binary: &paths[0],
            model_worker: Some(&paths[1]),
            sim_worker: Some(&paths[2]),
            msl_tools: Some(&paths[3]),
        };
        let command = msl_test_binary_command(temp.path(), binaries, "suite::test_msl_all")
            .expect("direct test command");
        let envs = command
            .get_envs()
            .map(|(key, value)| (key.to_owned(), value.map(ToOwned::to_owned)))
            .collect::<std::collections::BTreeMap<_, _>>();

        assert_eq!(
            command.get_args().collect::<Vec<_>>(),
            ["suite::test_msl_all", "--exact", "--nocapture"].map(OsStr::new)
        );
        assert_eq!(
            envs.get(OsStr::new("CARGO_BIN_EXE_rumoca-worker")),
            Some(&Some(paths[1].as_os_str().to_owned()))
        );
        for key in [
            "CARGO_BIN_EXE_rumoca-sim-worker",
            "CARGO_BIN_EXE_rumoca_sim_worker",
        ] {
            assert_eq!(
                envs.get(OsStr::new(key)),
                Some(&Some(paths[2].as_os_str().to_owned()))
            );
        }
        for key in [
            "CARGO_BIN_EXE_rumoca-msl-tools",
            "CARGO_BIN_EXE_rumoca_msl_tools",
        ] {
            assert_eq!(
                envs.get(OsStr::new(key)),
                Some(&Some(paths[3].as_os_str().to_owned()))
            );
        }
    }
}
