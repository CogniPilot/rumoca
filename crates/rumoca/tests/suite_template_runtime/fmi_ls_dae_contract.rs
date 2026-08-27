use sha1::{Digest, Sha1};
use std::path::PathBuf;

fn fixture_root() -> PathBuf {
    PathBuf::from(env!("CARGO_MANIFEST_DIR"))
        .join("tests")
        .join("fixtures")
        .join("fmi-ls-dae-upstream")
}

#[test]
fn fmi_ls_dae_contract_pins_exact_upstream_schema_bytes_without_registering_a_target() {
    let root = fixture_root();
    let expected = [
        (
            "schemas/fmi3LayeredStandardDaeManifest.xsd",
            "4e2955629616c4de613691d0261a18ffa1ac3e72",
        ),
        (
            "schemas/fmi3LayeredStandardManifest.xsd",
            "3bcb471d4b94a082033e8569e88d7640d086bfa5",
        ),
        (
            "schemas/LICENSE.txt",
            "b296dee6f153d4f425e16b47e1785460ac05fc88",
        ),
    ];
    for (relative, digest) in expected {
        let bytes = std::fs::read(root.join(relative)).expect("pinned fixture must be readable");
        assert_eq!(format!("{:x}", Sha1::digest(bytes)), digest, "{relative}");
    }

    let upstream = std::fs::read_to_string(root.join("UPSTREAM.md"))
        .expect("fixture provenance must be readable");
    assert!(upstream.contains("5cd461aba3a00673fb9bffcd2f6565363cced1ce"));
    assert!(
        rumoca_compile::codegen::templates::builtin_target("fmi-ls-dae").is_none(),
        "the incomplete layered DAE profile must not be advertised"
    );
}
