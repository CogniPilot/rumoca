use std::path::{Path, PathBuf};
use std::process::Command;

use super::ParityConfigLock;

/// The parent owns the existing workspace lock until every isolated child and
/// its evidence checks finish; only Cargo's standard target variable varies.
pub(crate) fn in_cargo_target_directory(test_name: &str, observe: impl FnOnce(&Path, &Path)) {
    let config_path = Path::new("target/msl/cargo-target-dir-test.json");
    if config_path.exists() {
        let config = std::fs::read(config_path).expect("parent-owned test config");
        let (root, expected): (PathBuf, PathBuf) =
            serde_json::from_slice(&config).expect("exact test paths");
        observe(&root, &expected);
        return;
    }

    let workspace = tempfile::tempdir().expect("isolated Cargo workspace");
    let root = workspace.path();
    let _lock = ParityConfigLock::acquire(root).expect("exclusive test config owner");
    let absolute = root.join("absolute-build");
    let cases = [
        (None, root.join("target")),
        (
            Some(PathBuf::from("relative-build")),
            root.join("relative-build"),
        ),
        (Some(absolute.clone()), absolute),
    ];
    for (configured, expected) in cases {
        std::fs::write(
            root.join(config_path),
            serde_json::to_vec(&(root, &expected)).unwrap(),
        )
        .expect("write locked test config");
        let mut command = Command::new(std::env::current_exe().unwrap());
        command
            .args(["--exact", test_name, "--nocapture"])
            .current_dir(root)
            .env_remove("CARGO_TARGET_DIR");
        if let Some(configured) = configured {
            command.env("CARGO_TARGET_DIR", configured);
        }
        let output = command.output().expect("isolated environment fixture");
        let stdout = String::from_utf8_lossy(&output.stdout);
        let stderr = String::from_utf8_lossy(&output.stderr);
        assert!(output.status.success(), "{stdout}\n{stderr}");
        assert!(
            stdout.contains("1 passed"),
            "child must execute the exact path assertion: {stdout}"
        );
    }
}
