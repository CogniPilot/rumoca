use std::{fs, process::Command};

use tempfile::Builder;

fn python_modules(backend: &str) -> &'static [&'static str] {
    match backend {
        "CasADi" => &["casadi", "numpy"],
        "SymPy" => &["sympy"],
        "ONNX" => &["onnx", "onnxruntime", "numpy"],
        "JAX" => &["jax", "diffrax", "numpy"],
        _ => panic!("unknown Python template backend: {backend}"),
    }
}

fn probe_python_modules(interpreter: &str, backend: &str, modules: &[&str]) -> Result<(), String> {
    let output = Command::new(interpreter)
        .args(["-c", &format!("import {}", modules.join(", "))])
        .output()
        .map_err(|error| {
            format!("{backend} dependency probe could not run {interpreter}: {error}")
        })?;
    if output.status.success() {
        return Ok(());
    }
    Err(format!(
        "{backend} Python runtime dependency probe failed\ninterpreter: {interpreter}\nrequired modules: {}\nstdout:\n{}\nstderr:\n{}",
        modules.join(", "),
        String::from_utf8_lossy(&output.stdout),
        String::from_utf8_lossy(&output.stderr),
    ))
}

fn resolve_python<'a>(
    candidates: impl IntoIterator<Item = &'a str>,
    backend: &str,
    modules: &[&str],
) -> Result<&'a str, String> {
    let mut failures = Vec::new();
    for candidate in candidates {
        match Command::new(candidate).arg("--version").output() {
            Ok(output) if output.status.success() => {
                match probe_python_modules(candidate, backend, modules) {
                    Ok(()) => return Ok(candidate),
                    Err(error) => failures.push(error),
                }
            }
            Ok(output) => failures.push(format!(
                "interpreter {candidate} --version failed\nstdout:\n{}\nstderr:\n{}",
                String::from_utf8_lossy(&output.stdout),
                String::from_utf8_lossy(&output.stderr),
            )),
            Err(error) => failures.push(format!("interpreter {candidate} unavailable: {error}")),
        }
    }
    Err(format!(
        "{backend} Python runtime resolution failed; required modules: {}\n{}",
        modules.join(", "),
        failures.join("\n---\n")
    ))
}

fn python_command(backend: &str) -> &'static str {
    resolve_python(["python3", "python"], backend, python_modules(backend))
        .unwrap_or_else(|error| panic!("{error}"))
}

pub(super) fn run_python(rendered: &str, driver: &str, backend: &str) -> String {
    let python = python_command(backend);
    let dir = Builder::new()
        .prefix("rumoca_runtime_test_")
        .tempdir()
        .expect("create temp dir");
    let model_path = dir.path().join("model.py");
    let driver_path = dir.path().join("driver.py");
    fs::write(&model_path, rendered).expect("write model.py");
    fs::write(&driver_path, driver).expect("write driver.py");

    let output = Command::new(python)
        .arg(driver_path.to_str().unwrap())
        .output()
        .expect("run Python driver");

    assert!(
        output.status.success(),
        "Python execution failed\nstdout:\n{}\nstderr:\n{}",
        String::from_utf8_lossy(&output.stdout),
        String::from_utf8_lossy(&output.stderr)
    );

    String::from_utf8(output.stdout).expect("stdout is utf8")
}

#[cfg(unix)]
fn probe_executable(dir: &std::path::Path, name: &str, body: &str) -> std::path::PathBuf {
    use std::os::unix::fs::PermissionsExt;

    let path = dir.join(name);
    fs::write(&path, body).expect("write fake Python executable");
    let mut permissions = fs::metadata(&path)
        .expect("fake Python metadata")
        .permissions();
    permissions.set_mode(0o755);
    fs::set_permissions(&path, permissions).expect("make fake Python executable");
    path
}

#[cfg(unix)]
const CANDIDATE_PROBE: &str = r#"#!/bin/sh
dir=${0%/*}
name=${0##*/}
printf '%s\n' "$@" > "$dir/$name.args"
if [ "$1" = --version ]; then exit 0; fi
if [ "$name" = python-capable ]; then exit 0; fi
echo "ModuleNotFoundError: $name lacks ONNX" >&2
exit 7
"#;

#[test]
fn python_runtime_probe_contract_modules_are_backend_specific() {
    assert_eq!(python_modules("CasADi"), &["casadi", "numpy"]);
    assert_eq!(python_modules("SymPy"), &["sympy"]);
    assert_eq!(python_modules("ONNX"), &["onnx", "onnxruntime", "numpy"]);
    assert_eq!(python_modules("JAX"), &["jax", "diffrax", "numpy"]);
}

#[test]
#[cfg(unix)]
fn python_runtime_probe_contract_selects_backend_capable_candidate() {
    let dir = Builder::new()
        .prefix("rumoca_python_candidates_")
        .tempdir()
        .expect("create Python candidate dir");
    let first = probe_executable(dir.path(), "python-first", CANDIDATE_PROBE);
    let second = probe_executable(dir.path(), "python-capable", CANDIDATE_PROBE);
    let candidates = [
        first.to_str().expect("UTF-8 first candidate"),
        second.to_str().expect("UTF-8 second candidate"),
    ];

    assert_eq!(
        resolve_python(candidates, "ONNX", python_modules("ONNX")).expect("resolve Python"),
        candidates[1]
    );
    for candidate in ["python-first", "python-capable"] {
        assert_eq!(
            fs::read_to_string(dir.path().join(format!("{candidate}.args")))
                .expect("read candidate argv"),
            "-c\nimport onnx, onnxruntime, numpy\n"
        );
    }
}

#[test]
#[cfg(unix)]
fn python_runtime_probe_contract_preserves_backend_import_errors() {
    let dir = Builder::new()
        .prefix("rumoca_python_probe_")
        .tempdir()
        .expect("create Python probe dir");
    let first = probe_executable(dir.path(), "python-first", CANDIDATE_PROBE);
    let second = probe_executable(dir.path(), "python-second", CANDIDATE_PROBE);
    let candidates = [
        first.to_str().expect("UTF-8 first candidate"),
        second.to_str().expect("UTF-8 second candidate"),
    ];
    let error = resolve_python(candidates, "ONNX", python_modules("ONNX"))
        .expect_err("missing ONNX dependencies must fail closed");
    for expected in [
        "ONNX",
        "onnx, onnxruntime, numpy",
        candidates[0],
        candidates[1],
        "ModuleNotFoundError: python-first lacks ONNX",
        "ModuleNotFoundError: python-second lacks ONNX",
    ] {
        assert!(error.contains(expected), "missing {expected:?}: {error}");
    }
}
