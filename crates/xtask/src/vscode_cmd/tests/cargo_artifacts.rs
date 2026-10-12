use crate::exe_name;
use crate::verify_cmd::tests::cargo_target_directory::in_cargo_target_directory;
use crate::vscode_cmd::{VscodePackageTarget, stage_vscode_lsp, stage_vscode_release_binaries};
use std::path::Path;

const BINARIES: [&str; 3] = ["rumoca-lsp", "rumoca-lsp-galec", "rumoca"];

fn write_binaries(directory: &Path, contents: &str) {
    std::fs::create_dir_all(directory).unwrap();
    for name in BINARIES {
        std::fs::write(directory.join(exe_name(name)), contents).unwrap();
    }
}

fn assert_staged_binaries(extension: &Path) {
    for name in BINARIES {
        let path = extension.join("bin").join(exe_name(name));
        assert_eq!(
            std::fs::read_to_string(path).unwrap(),
            "selected Cargo artifact"
        );
    }
}

#[test]
fn vscode_host_staging_selects_configured_debug_and_release_artifacts() {
    in_cargo_target_directory(
        "vscode_cmd::tests::cargo_artifacts::vscode_host_staging_selects_configured_debug_and_release_artifacts",
        |root, target| {
            for profile in ["debug", "release"] {
                let selected = target.join(profile);
                let default = root.join("target").join(profile);
                if default != selected {
                    write_binaries(&default, "wrong build root");
                }
                let extension = root.join(format!("extension-{profile}"));
                let error = stage_vscode_lsp(root, &extension, profile).unwrap_err();
                assert!(error.to_string().contains(&selected.display().to_string()));
                write_binaries(&selected, "selected Cargo artifact");
                stage_vscode_lsp(root, &extension, profile).unwrap();
                assert_staged_binaries(&extension);
            }
        },
    );
}

#[test]
fn vscode_cross_staging_preserves_triple_under_configured_cargo_root() {
    in_cargo_target_directory(
        "vscode_cmd::tests::cargo_artifacts::vscode_cross_staging_preserves_triple_under_configured_cargo_root",
        |root, target| {
            let triple = VscodePackageTarget::LinuxX64;
            let selected = target.join(triple.rust_target()).join("release");
            let default = root
                .join("target")
                .join(triple.rust_target())
                .join("release");
            if default != selected {
                write_binaries(&default, "wrong build root");
            }
            let extension = root.join("cross-extension");
            std::fs::create_dir_all(&extension).unwrap();
            let error = stage_vscode_release_binaries(root, &extension, triple).unwrap_err();
            assert_eq!(
                error.to_string(),
                format!(
                    "missing bundled binary: {}",
                    selected.join(exe_name("rumoca-lsp")).display()
                )
            );
            write_binaries(&selected, "selected Cargo artifact");
            stage_vscode_release_binaries(root, &extension, triple).unwrap();
            assert_staged_binaries(&extension);
        },
    );
}
