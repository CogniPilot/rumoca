//! Executable contract test for the pinned, non-normative FMI-LS-Wasm target.

use std::fs;
use std::path::{Path, PathBuf};
use std::process::{Command, Output};

use sha1::{Digest, Sha1};
use tempfile::tempdir;
use walkdir::WalkDir;

const MODEL: &str = "FmiLsDecay";
const SOURCE: &str = r#"
model FmiLsDecay
  input Real u(start = 0.0);
  output Real x(start = 1.0);
equation
  der(x) = -x + u;
end FmiLsDecay;
"#;

fn workspace_root() -> PathBuf {
    Path::new(env!("CARGO_MANIFEST_DIR"))
        .parent()
        .and_then(Path::parent)
        .expect("rumoca crate is two levels below the workspace root")
        .to_path_buf()
}

fn checked_output(command: &mut Command, context: &str) -> Output {
    let output = command
        .output()
        .unwrap_or_else(|error| panic!("start {context}: {error}"));
    assert!(
        output.status.success(),
        "{context} failed\nstdout:\n{}\nstderr:\n{}",
        String::from_utf8_lossy(&output.stdout),
        String::from_utf8_lossy(&output.stderr)
    );
    output
}

fn copy_tree(source: &Path, destination: &Path) {
    for entry in WalkDir::new(source) {
        let entry = entry.expect("walk FMI-LS host fixture");
        let relative = entry
            .path()
            .strip_prefix(source)
            .expect("fixture-relative path");
        let output = destination.join(relative);
        if entry.file_type().is_dir() {
            fs::create_dir_all(&output).expect("create copied fixture directory");
        } else {
            fs::copy(entry.path(), &output).expect("copy fixture file");
        }
    }
}

fn only_wasm(directory: &Path) -> PathBuf {
    let files = fs::read_dir(directory)
        .expect("read generated wasm release directory")
        .filter_map(Result::ok)
        .map(|entry| entry.path())
        .filter(|path| {
            path.extension()
                .is_some_and(|extension| extension == "wasm")
        })
        .collect::<Vec<_>>();
    assert_eq!(
        files.len(),
        1,
        "expected one generated component: {files:?}"
    );
    files[0].clone()
}

fn instantiation_token(source: &str) -> &str {
    source
        .split_once("instantiation_token != \"")
        .and_then(|(_, suffix)| suffix.split_once('"'))
        .map(|(token, _)| token)
        .expect("generated source contains the checked instantiation token")
}

#[test]
fn fmi_ls_wasm_component_validates_and_executes_pinned_lifecycle() {
    let wasm_tools_available = Command::new("wasm-tools").arg("--version").output().is_ok();
    if !super::template_runtime_policy::prerequisites_are_available(
        "FMI-LS-Wasm lifecycle check",
        &[("wasm-tools", wasm_tools_available)],
    ) {
        return;
    }

    let work = tempdir().expect("create FMI-LS-Wasm test directory");
    let result = rumoca::Compiler::new()
        .model(MODEL)
        .compile_str(SOURCE, "FmiLsDecay.mo")
        .expect("compile FMI-LS-Wasm fixture");
    let generated = work.path().join("generated");
    rumoca::compile_packaged_target(&result, MODEL, "fmi-ls-wasm", generated.clone())
        .expect("render complete FMI-LS-Wasm component crate");
    let crate_root = generated.join(MODEL);

    checked_output(
        Command::new("wasm-tools")
            .args(["component", "wit"])
            .arg(crate_root.join("wit")),
        "parse pinned FMI-LS WIT package",
    );
    let component_target = work.path().join("component-target");
    checked_output(
        Command::new("cargo")
            .args(["build", "--release", "--target", "wasm32-wasip2"])
            .arg("--manifest-path")
            .arg(crate_root.join("Cargo.toml"))
            .env("RUSTFLAGS", "-Dwarnings")
            .env("CARGO_TARGET_DIR", &component_target),
        "build generated wasm32-wasip2 component",
    );
    let component = only_wasm(&component_target.join("wasm32-wasip2/release"));
    checked_output(
        Command::new("wasm-tools").arg("validate").arg(&component),
        "validate generated WebAssembly component",
    );

    let source = fs::read_to_string(crate_root.join("src/lib.rs"))
        .expect("read generated FMI-LS implementation");
    let token = instantiation_token(&source);
    let host = work.path().join("host");
    copy_tree(
        &workspace_root().join("crates/rumoca/tests/fixtures/fmi-ls-wasm-host"),
        &host,
    );
    copy_tree(&crate_root.join("wit"), &host.join("wit"));
    checked_output(
        Command::new("cargo")
            .args(["run", "--locked", "--manifest-path"])
            .arg(host.join("Cargo.toml"))
            .args(["--"])
            .arg(&component)
            .arg(token)
            .env("CARGO_TARGET_DIR", work.path().join("host-target")),
        "execute generated FMI-LS component through Wasmtime",
    );
}

#[test]
fn fmi_ls_wasm_vendored_contract_matches_pinned_upstream_bytes() {
    let root = workspace_root().join("crates/rumoca-phase-codegen/src/templates/fmi-ls-wasm");
    let expected = [
        (
            "wit/fmi3-callbacks.wit",
            "3c245a828c438a9ba3629c1fd163f776898cfe2e",
        ),
        (
            "wit/fmi3-co-simulation.wit",
            "41f9753c7614a25d7197c8a78a3724beefbd5250",
        ),
        (
            "wit/fmi3-common.wit",
            "7dbe9aaa3788303237c8b08ab10b0e277df68cb7",
        ),
        (
            "wit/fmi3-model-exchange.wit",
            "0372cacf36db9a717ef094488f233747001fe18a",
        ),
        (
            "wit/fmi3-scheduled-execution.wit",
            "ecb813ced9ec36f6beb55330fc83d19a8d4f34b7",
        ),
        (
            "wit/fmi3-types.wit",
            "3ef57aba19886e110253f103a1ca5e6a0cb0684c",
        ),
        ("wit/world.wit", "4c4f31bdd797bd2e6703b08ba4e7bd56c89be5d7"),
        (
            "upstream/LICENSE.txt",
            "2f6d404a9e3b153b04498beb18c6da1c833e3bbd",
        ),
    ];
    for (path, digest) in expected {
        let bytes = fs::read(root.join(path)).expect("read pinned FMI-LS contract file");
        assert_eq!(
            format!("{:x}", Sha1::digest(bytes)),
            digest,
            "changed {path}"
        );
    }
}
