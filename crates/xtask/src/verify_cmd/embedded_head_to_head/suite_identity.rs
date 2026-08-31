//! Compile-time byte closure for the trusted benchmark implementation.

use sha2::{Digest, Sha256};

const IMPLEMENTATION: &[(&str, &[u8])] = &[
    ("crates/xtask/src/main.rs", include_bytes!("../../main.rs")),
    ("crates/xtask/src/util.rs", include_bytes!("../../util.rs")),
    (
        "crates/xtask/src/verify_cmd.rs",
        include_bytes!("../../verify_cmd.rs"),
    ),
    (
        "embedded_head_to_head.rs",
        include_bytes!("../embedded_head_to_head.rs"),
    ),
    ("artifact_bundle.rs", include_bytes!("artifact_bundle.rs")),
    ("artifact_guard.rs", include_bytes!("artifact_guard.rs")),
    ("compiler_source.rs", include_bytes!("compiler_source.rs")),
    ("compiler_deps.rs", include_bytes!("compiler_deps.rs")),
    ("cross.rs", include_bytes!("cross.rs")),
    ("efmu_artifact.rs", include_bytes!("efmu_artifact.rs")),
    (
        "efmu_artifact/xml_topology.rs",
        include_bytes!("efmu_artifact/xml_topology.rs"),
    ),
    (
        "efmu_artifact/zip_preflight.rs",
        include_bytes!("efmu_artifact/zip_preflight.rs"),
    ),
    ("cross/plan.rs", include_bytes!("cross/plan.rs")),
    (
        "cross/plan/generation.rs",
        include_bytes!("cross/plan/generation.rs"),
    ),
    ("cross/plan/suite.rs", include_bytes!("cross/plan/suite.rs")),
    ("emit.rs", include_bytes!("emit.rs")),
    ("manifest.rs", include_bytes!("manifest.rs")),
    ("process.rs", include_bytes!("process.rs")),
    ("qemu.rs", include_bytes!("qemu.rs")),
    ("snapshot.rs", include_bytes!("snapshot.rs")),
    ("suite.rs", include_bytes!("suite.rs")),
    ("suite_identity.rs", include_bytes!("suite_identity.rs")),
    ("tool_closure.rs", include_bytes!("tool_closure.rs")),
    ("typed_path.rs", include_bytes!("typed_path.rs")),
    (
        "crates/xtask/Cargo.toml",
        include_bytes!("../../../Cargo.toml"),
    ),
    ("Cargo.toml", include_bytes!("../../../../../Cargo.toml")),
    ("Cargo.lock", include_bytes!("../../../../../Cargo.lock")),
    ("flake.nix", include_bytes!("../../../../../flake.nix")),
    ("flake.lock", include_bytes!("../../../../../flake.lock")),
    (
        ".cargo/config.toml",
        include_bytes!("../../../../../.cargo/config.toml"),
    ),
    (
        "rust-toolchain.toml",
        include_bytes!("../../../../../rust-toolchain.toml"),
    ),
    (
        ".github/workflows/ci.yml",
        include_bytes!("../../../../../.github/workflows/ci.yml"),
    ),
    (
        ".github/workflows/nightly.yml",
        include_bytes!("../../../../../.github/workflows/nightly.yml"),
    ),
];

pub(super) fn sha256() -> String {
    digest(IMPLEMENTATION)
}

fn digest(files: &[(&str, &[u8])]) -> String {
    let mut digest = Sha256::new();
    digest.update(b"embedded-head-to-head-trusted-implementation-v1\0");
    for (path, bytes) in files {
        digest.update((path.len() as u64).to_le_bytes());
        digest.update(path.as_bytes());
        digest.update((bytes.len() as u64).to_le_bytes());
        digest.update(bytes);
    }
    format!("{:x}", digest.finalize())
}

#[cfg(test)]
mod tests {
    use super::{IMPLEMENTATION, digest};

    #[test]
    fn semantic_body_mutation_moves_the_trusted_implementation_identity() {
        let expected = digest(IMPLEMENTATION);
        let mut changed = IMPLEMENTATION.to_vec();
        let executor = changed
            .iter()
            .position(|(path, _)| *path == "cross.rs")
            .unwrap();
        changed[executor] = ("cross.rs", b"semantically changed executor body");
        assert_ne!(digest(&changed), expected);
    }

    #[test]
    fn dispatch_mutation_moves_identity_but_excluded_test_bytes_do_not() {
        let expected = digest(IMPLEMENTATION);
        let mut changed = IMPLEMENTATION.to_vec();
        let dispatch = changed
            .iter()
            .position(|(path, _)| *path == "crates/xtask/src/verify_cmd.rs")
            .unwrap();
        changed[dispatch] = (
            "crates/xtask/src/verify_cmd.rs",
            b"changed embedded benchmark dispatch",
        );
        assert_ne!(digest(&changed), expected);

        let mut excluded_test_bytes = include_bytes!("tests.rs").to_vec();
        excluded_test_bytes[0] ^= 1;
        assert_eq!(digest(IMPLEMENTATION), expected);
        assert!(IMPLEMENTATION.iter().all(|(path, _)| *path != "tests.rs"));
    }

    #[test]
    fn entry_root_selection_and_workspace_build_contract_move_identity() {
        let expected = digest(IMPLEMENTATION);
        for path in [
            "crates/xtask/src/main.rs",
            "crates/xtask/src/util.rs",
            "Cargo.toml",
            ".cargo/config.toml",
            "flake.nix",
            "flake.lock",
        ] {
            let mut changed = IMPLEMENTATION.to_vec();
            let index = changed
                .iter()
                .position(|(candidate, _)| *candidate == path)
                .unwrap();
            changed[index] = (path, b"changed production root/build contract");
            assert_ne!(digest(&changed), expected, "{path} must move identity");
        }
    }
}
