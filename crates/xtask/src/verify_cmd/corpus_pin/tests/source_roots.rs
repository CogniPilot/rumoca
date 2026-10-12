//! Exercise the real command's immutable input channel without downloading or
//! claiming compiler semantics from the fixture executable.

use super::*;
use sha2::{Digest, Sha256};
use std::os::unix::fs::PermissionsExt;

fn provision_archive(root: &Path) -> PathBuf {
    let cache = root.join("target/verification/fixture-flight");
    fs::create_dir_all(cache.parent().expect("cache parent")).expect("cache directory");
    let archive = cache.with_extension("zip");
    write_container(
        &archive,
        &[(
            "revision/Vehicles/package.mo",
            "package Vehicles end Vehicles;",
        )],
    );
    let digest = format!("{:x}", Sha256::digest(fs::read(archive).expect("archive")));
    let manifest = format!(
        r#"version = 1
[libraries.flight_models]
display_name = "Command input fixture"
version = "fixture-revision"
url = "https://example.invalid/no-network.zip"
sha256 = "{digest}"
cache_dir = "target/verification/fixture-flight"
zip_layout = "auto-strip-common-root"
required_files = ["Vehicles/package.mo"]
"#
    );
    let path = root.join("infra/verification/corpus-dependencies.toml");
    fs::create_dir_all(path.parent().expect("manifest parent")).expect("manifest directory");
    fs::write(path, manifest).expect("dependency manifest");
    cache
}

#[test]
fn corpus_command_qualifies_default_archive_and_rejects_changed_or_explicit_inputs() {
    let temp = tempfile::tempdir().expect("command workspace");
    let root = temp.path();
    let cache = provision_archive(root);
    let mut row = entry(
        compile_check(),
        Expectation::Refused {
            diagnostic: "ED019".to_string(),
        },
    );
    row.corpus = Corpus::FlightModels;
    row.entry_point = "Vehicles/package.mo".to_string();
    let manifest = CorpusManifest {
        schema_version: 2,
        runtime_budget_seconds: 1.0,
        entries: vec![row],
    };
    let manifest_path = root.join("corpus.json");
    fs::write(
        &manifest_path,
        serde_json::to_vec(&manifest).expect("manifest wire"),
    )
    .expect("manifest");
    let compiler = root.join("fixture-compiler");
    fs::write(
        &compiler,
        "#!/bin/sh\nprintf '%s\\n' \"$@\" > invoked\nprintf '[ED019] fixture refusal\\n'\nexit 1\n",
    )
    .expect("fixture compiler");
    fs::set_permissions(&compiler, fs::Permissions::from_mode(0o755)).expect("executable");
    let args = super::super::VerifyCorpusPinArgs {
        manifest: Some(manifest_path),
        msl_root: Some(root.join("unused-msl")),
        rumoca_binary: Some(compiler),
        jobs: Some(1),
        ..Default::default()
    };
    super::super::run(root, &args).expect("complete command with verified default source");
    let entry_point = cache.join("Vehicles/package.mo");
    assert_eq!(
        fs::read_to_string(&entry_point).expect("provisioned source"),
        "package Vehicles end Vehicles;"
    );
    let invoked = root
        .join(super::super::ARTIFACT_DIR)
        .join("artifacts/invoked");
    let invocation = fs::read_to_string(&invoked).expect("worker launched");
    assert!(invocation.contains(entry_point.to_str().expect("entry point")));
    assert!(invocation.contains(cache.to_str().expect("source root")));
    assert!(invocation.contains("--source-root"));
    fs::write(&invoked, "no second worker").expect("launch sentinel");
    fs::write(&entry_point, "changed source").expect("dirty source");
    let error = super::super::run(root, &args).expect_err("dirty default source");
    assert!(
        format!("{error:#}").contains("pinned source differs"),
        "{error:#}"
    );
    assert_eq!(
        fs::read_to_string(&invoked).expect("no worker"),
        "no second worker"
    );
    fs::write(&entry_point, "package Vehicles end Vehicles;").expect("restore fixture");
    let explicit = super::super::VerifyCorpusPinArgs {
        models_root: Some(root.join("wrong-explicit-input")),
        ..args
    };
    let error = super::super::run(root, &explicit).expect_err("explicit root must not fall back");
    assert!(
        format!("{error:#}").contains("pinned source root is missing"),
        "{error:#}"
    );
    assert_eq!(
        fs::read_to_string(&invoked).expect("no worker"),
        "no second worker"
    );
}
